# TrackMan stat calculations and ggplot charts for the TrackMan stats tab.

STRIKE_CALLS <- c("StrikeCalled", "StrikeSwinging", "FoulBall", "FoulBallNotFieldable",
                  "FoulBallFieldable", "InPlay")
SWING_CALLS <- setdiff(STRIKE_CALLS, "StrikeCalled")
HIT_RESULTS <- c("Single", "Double", "Triple", "HomeRun")

# Percent that returns NA when the denominator is 0
pct <- function(num, den) ifelse(den == 0, NA_real_, 100 * num / den)

# Turns -Inf / Inf / NaN from max() or mean() on empty groups into NA
clean_num <- function(x) {
  x[!is.finite(x)] <- NA
  x
}

safe_max <- function(x) clean_num(suppressWarnings(max(x, na.rm = TRUE)))
safe_mean <- function(x) clean_num(mean(x, na.rm = TRUE))

add_flags <- function(d) {
  d |>
    mutate(
      strike = PitchCall %in% STRIKE_CALLS,
      swing = PitchCall %in% SWING_CALLS,
      whiff = PitchCall == "StrikeSwinging" & !is.na(PitchCall),
      csw = PitchCall %in% c("StrikeCalled", "StrikeSwinging"),
      in_zone = !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
        PlateLocSide >= ZONE$x[1] & PlateLocSide <= ZONE$x[2] &
        PlateLocHeight >= ZONE$z[1] & PlateLocHeight <= ZONE$z[2],
      out_zone = !in_zone & !is.na(PlateLocSide) & !is.na(PlateLocHeight),
      chase = swing & out_zone,
      is_k = KorBB %in% "Strikeout",
      is_bb = KorBB %in% "Walk",
      pa_end = is_k | is_bb | PitchCall %in% c("InPlay", "HitByPitch"),
      in_play = PitchCall %in% "InPlay",
      hit = in_play & PlayResult %in% HIT_RESULTS,
      hard_hit = in_play & !is.na(ExitSpeed) & ExitSpeed >= 95
    )
}

# Which rows belong to the chosen view
filter_side <- function(tm, view) {
  switch(view,
    our_p = tm |> filter(PitcherTeam %in% TEAM_CODES),
    our_h = tm |> filter(BatterTeam %in% TEAM_CODES),
    their_p = tm |> filter(!PitcherTeam %in% TEAM_CODES),
    their_h = tm |> filter(!BatterTeam %in% TEAM_CODES),
    tm[0, ]
  )
}

is_pitcher_view <- function(view) view %in% c("our_p", "their_p")

# Players in the order they first appeared
players_in <- function(d, view) {
  col <- if (is_pitcher_view(view)) "Pitcher" else "Batter"
  d <- d |> arrange(PitchNo)
  unique(stats::na.omit(d[[col]]))
}

pitch_colors_for <- function(types) {
  types <- unique(types)
  cols <- PITCH_COLORS[types]
  cols[is.na(cols)] <- PITCH_COLORS[["Other"]]
  setNames(unname(cols), types)
}

# Pitcher view ---------------------------------------------------------------

pitcher_line <- function(d) {
  d <- add_flags(d)
  tibble::tibble(
    Pitches = nrow(d),
    `Strike%` = pct(sum(d$strike), nrow(d)),
    BF = sum(d$pa_end),
    K = sum(d$is_k),
    BB = sum(d$is_bb),
    H = sum(d$hit),
    `Whiff%` = pct(sum(d$whiff), sum(d$swing)),
    `CSW%` = pct(sum(d$csw), nrow(d)),
    `Chase%` = pct(sum(d$chase), sum(d$out_zone)),
    `Hard hit` = sum(d$hard_hit)
  )
}

pitch_mix <- function(d) {
  total <- nrow(d)
  add_flags(d) |>
    group_by(Pitch = PitchType) |>
    summarise(
      `#` = n(),
      `Use%` = round(pct(n(), total)),
      Velo = round(safe_mean(RelSpeed), 1),
      Max = round(safe_max(RelSpeed), 1),
      Spin = round(safe_mean(SpinRate)),
      IVB = round(safe_mean(InducedVertBreak), 1),
      HB = round(safe_mean(HorzBreak), 1),
      `Rel Ht` = round(safe_mean(RelHeight), 1),
      Ext = round(safe_mean(Extension), 1),
      `Strike%` = round(pct(sum(strike), n())),
      `Zone%` = round(pct(sum(in_zone), n())),
      `Whiff%` = round(pct(sum(whiff), sum(swing))),
      .groups = "drop"
    ) |>
    arrange(desc(`#`))
}

pitch_log <- function(d) {
  d |>
    arrange(PitchNo) |>
    transmute(
      `#` = PitchNo, Inn = Inning, Batter,
      Count = paste0(Balls, "-", Strikes),
      Pitch = PitchType, Velo = round(RelSpeed, 1), Spin = round(SpinRate),
      IVB = round(InducedVertBreak, 1), HB = round(HorzBreak, 1),
      Call = PitchCall,
      Result = case_when(
        KorBB %in% c("Strikeout", "Walk") ~ KorBB,
        PitchCall %in% "InPlay" ~ PlayResult,
        TRUE ~ ""
      )
    )
}

chart_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          plot.title = element_text(face = "bold", color = BRAND$navy))
}

movement_plot <- function(d) {
  d <- d |> filter(!is.na(HorzBreak), !is.na(InducedVertBreak))
  # TrackMan reports HorzBreak from the pitcher's point of view, so this is a
  # pitcher-view chart as-is.
  ggplot(d, aes(HorzBreak, InducedVertBreak, color = PitchType)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_vline(xintercept = 0, color = "grey70") +
    geom_point(size = 2.6, alpha = 0.85) +
    scale_color_manual(values = pitch_colors_for(d$PitchType)) +
    coord_equal(xlim = c(-25, 25), ylim = c(-25, 25)) +
    labs(title = "Movement (pitcher view)", x = "Horizontal break (in)", y = "Induced vertical break (in)") +
    chart_theme()
}

# Strike zone and home plate, catcher view
zone_layers <- function() {
  plate <- data.frame(
    x = c(-0.708, 0.708, 0.708, 0, -0.708),
    y = c(0.45, 0.45, 0.3, 0.12, 0.3)
  )
  list(
    annotate("rect", xmin = ZONE$x[1], xmax = ZONE$x[2], ymin = ZONE$z[1], ymax = ZONE$z[2],
             fill = NA, color = "black", linewidth = 0.8),
    geom_polygon(data = plate, aes(x, y), inherit.aes = FALSE, fill = "white", color = "black")
  )
}

location_plot <- function(d) {
  d <- d |> filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
  ggplot(d, aes(PlateLocSide, PlateLocHeight, color = PitchType)) +
    zone_layers() +
    geom_point(size = 2.6, alpha = 0.85) +
    scale_color_manual(values = pitch_colors_for(d$PitchType)) +
    coord_equal(xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
    labs(title = "Location (catcher view)", x = NULL, y = NULL) +
    chart_theme()
}

velo_plot <- function(d) {
  d <- d |> arrange(PitchNo) |> mutate(n = row_number()) |> filter(!is.na(RelSpeed))
  ggplot(d, aes(n, RelSpeed, color = PitchType)) +
    geom_line(aes(group = PitchType), alpha = 0.4) +
    geom_point(size = 2.2) +
    scale_color_manual(values = pitch_colors_for(d$PitchType)) +
    labs(title = "Velocity by pitch number", x = "Pitch number", y = "Velo (mph)") +
    chart_theme()
}

# Hitter view ----------------------------------------------------------------

hitter_summary <- function(d) {
  add_flags(d) |>
    group_by(Batter) |>
    summarise(
      first = clean_num(suppressWarnings(min(PitchNo, na.rm = TRUE))),
      PA = sum(pa_end),
      H = sum(hit),
      K = sum(is_k),
      BB = sum(is_bb),
      Pitches = n(),
      `Swing%` = round(pct(sum(swing), n())),
      `Whiff%` = round(pct(sum(whiff), sum(swing))),
      `Chase%` = round(pct(sum(chase), sum(out_zone))),
      `Avg EV` = round(safe_mean(ExitSpeed[in_play]), 1),
      `Max EV` = round(safe_max(ExitSpeed[in_play]), 1),
      `95+` = sum(hard_hit),
      .groups = "drop"
    ) |>
    arrange(first) |>
    select(-first)
}

OUTCOME_COLORS <- c(
  Whiff = "#D22D49", `In play` = BRAND$navy, Foul = "#FE9D00",
  `Called strike` = "#933F2C", Ball = "#9C8975"
)

pitch_outcome <- function(call) {
  case_when(
    call == "StrikeSwinging" ~ "Whiff",
    call == "InPlay" ~ "In play",
    grepl("^Foul", call) ~ "Foul",
    call == "StrikeCalled" ~ "Called strike",
    TRUE ~ "Ball"
  )
}

zone_outcome_plot <- function(d) {
  d <- d |>
    filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) |>
    mutate(Outcome = factor(pitch_outcome(PitchCall), levels = names(OUTCOME_COLORS)))
  ggplot(d, aes(PlateLocSide, PlateLocHeight, color = Outcome)) +
    zone_layers() +
    geom_point(size = 2.8, alpha = 0.85) +
    scale_color_manual(values = OUTCOME_COLORS, drop = FALSE) +
    coord_equal(xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
    labs(title = "Pitches seen (catcher view)", x = NULL, y = NULL) +
    chart_theme()
}

batted_balls <- function(d) {
  d |>
    filter(PitchCall %in% "InPlay") |>
    arrange(PitchNo) |>
    transmute(
      Inn = Inning, Batter, Pitcher, Pitch = PitchType, Velo = round(RelSpeed, 1),
      Result = PlayResult, EV = round(ExitSpeed, 1), LA = round(Angle), Dist = round(Distance)
    )
}

# UI -------------------------------------------------------------------------

# Compact DT table. Pass page_length to page long tables.
stat_table <- function(df, page_length = NULL) {
  paged <- !is.null(page_length)
  datatable(
    df,
    rownames = FALSE,
    selection = "none",
    class = "compact stripe nowrap",
    options = list(
      dom = if (paged) "tp" else "t",
      paging = paged,
      pageLength = if (paged) page_length else -1,
      scrollX = TRUE,
      ordering = FALSE,
      language = list(emptyTable = "Nothing to show")
    )
  )
}

# DT tables sit in card(fill = FALSE) with height = "auto" so bslib does not
# stretch the card and leave a gap under the table. The card body is also not
# fillable, otherwise DT's 400px flex-basis rule still adds the gap.
table_card <- function(title, id) {
  card(fill = FALSE, card_header(title),
       card_body(fillable = FALSE, DTOutput(id, height = "auto")))
}

plot_card <- function(id) {
  card(fill = FALSE, plotOutput(id, height = "340px"))
}

trackman_ui <- function() {
  tagList(
    conditionalPanel(
      "!output.tm_has_data",
      card(card_body(p(class = "text-muted mb-0",
        "You don't show up in any TrackMan file yet. Your reports appear here after you play.")))
    ),
    conditionalPanel("output.tm_has_data", trackman_body())
  )
}

trackman_body <- function() {
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      open = list(desktop = "open", mobile = "always-above"),
      selectInput("tm_game", "Game", choices = NULL),
      radioButtons("tm_view", "Show", choices = c(
        "Our pitchers" = "our_p", "Our hitters" = "our_h",
        "Their pitchers" = "their_p", "Their hitters" = "their_h"
      )),
      selectInput("tm_player", "Player", choices = NULL)
    ),
    conditionalPanel(
      "input.tm_view == 'our_p' || input.tm_view == 'their_p'",
      card(fill = FALSE, card_header("Stat line"),
           div(class = "table-responsive", tableOutput("tm_line"))),
      table_card("Pitch mix", "tm_mix"),
      layout_column_wrap(
        width = "320px", fill = FALSE,
        plot_card("tm_move"), plot_card("tm_loc"), plot_card("tm_velo")
      ),
      table_card("Pitch by pitch", "tm_log")
    ),
    conditionalPanel(
      "input.tm_view == 'our_h' || input.tm_view == 'their_h'",
      table_card("Hitters", "hit_summary"),
      layout_column_wrap(
        width = "320px", fill = FALSE,
        plot_card("hit_zone")
      ),
      table_card("Batted balls", "hit_bip")
    )
  )
}
