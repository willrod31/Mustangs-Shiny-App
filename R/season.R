# Season stats: running hitting and pitching lines for our team, built from
# every TrackMan file. The same functions build the season table
# (by = "Batter" / "Pitcher") and a player's game log (by = c("GameDate", "Opp")).

FASTBALLS <- c("Fastball", "FourSeamFastBall", "Sinker", "TwoSeamFastBall", "OneSeamFastBall")

# Ratio that returns NA on a zero denominator
rate <- function(num, den) ifelse(den == 0, NA_real_, num / den)

# Adds the columns both stat lines need
season_prep <- function(d) {
  add_flags(d) |>
    mutate(
      GameDate = Date,
      Opp = ifelse(HomeTeam %in% TEAM_CODES, AwayTeam, HomeTeam),
      hbp = PitchCall %in% "HitByPitch",
      sac = in_play & PlayResult %in% "Sacrifice",
      single = in_play & PlayResult %in% "Single",
      double = in_play & PlayResult %in% "Double",
      triple = in_play & PlayResult %in% "Triple",
      hr = in_play & PlayResult %in% "HomeRun",
      ev = ifelse(in_play, ExitSpeed, NA_real_),
      # Outs on the play: OutsOnPlay where tagged, else 1 for an in-play out
      outs_bip = ifelse(
        !is.na(OutsOnPlay), OutsOnPlay,
        as.numeric(in_play & PlayResult %in% c("Out", "Sacrifice", "FieldersChoice"))
      ),
      # A strikeout adds an out only when the play didn't already count one
      outs = outs_bip + as.numeric(is_k & outs_bip == 0),
      runs = ifelse(is.na(RunsScored), 0, RunsScored),
      fb_velo = ifelse(TaggedPitchType %in% FASTBALLS | PitchType %in% FASTBALLS, RelSpeed, NA_real_)
    )
}

hitting_stats <- function(d, by = "Batter") {
  d <- d |> filter(BatterTeam %in% TEAM_CODES)
  season_prep(d) |>
    group_by(across(all_of(by))) |>
    summarise(
      G = n_distinct(GameKey),
      PA = sum(pa_end),
      H = sum(hit),
      `2B` = sum(double),
      `3B` = sum(triple),
      HR = sum(hr),
      BB = sum(is_bb),
      K = sum(is_k),
      HBP = sum(hbp),
      SAC = sum(sac),
      swings = sum(swing), whiffs = sum(whiff), chases = sum(chase), outside = sum(out_zone),
      ev_n = sum(!is.na(ev)), hard = sum(!is.na(ev) & ev >= 95),
      `Avg EV` = round(safe_mean(ev), 1),
      `Max EV` = round(safe_max(ev), 1),
      .groups = "drop"
    ) |>
    mutate(
      AB = PA - BB - HBP - SAC,
      TB = H + `2B` + 2 * `3B` + 3 * HR,
      AVG = rate(H, AB),
      OBP = rate(H + BB + HBP, AB + BB + HBP + SAC),
      SLG = rate(TB, AB),
      OPS = OBP + SLG,
      `K%` = round(pct(K, PA), 1),
      `BB%` = round(pct(BB, PA), 1),
      `Whiff%` = round(pct(whiffs, swings), 1),
      `Chase%` = round(pct(chases, outside), 1),
      `Hard%` = round(pct(hard, ev_n), 1)
    ) |>
    select(all_of(by), G, PA, AB, H, `2B`, `3B`, HR, BB, K, HBP, AVG, OBP, SLG, OPS,
           `K%`, `BB%`, `Whiff%`, `Chase%`, `Avg EV`, `Max EV`, `Hard%`)
}

# 5.2 means 5 innings and 2 outs
format_ip <- function(outs) paste0(outs %/% 3, ".", outs %% 3)

pitching_stats <- function(d, by = "Pitcher") {
  d <- d |> filter(PitcherTeam %in% TEAM_CODES)
  season_prep(d) |>
    group_by(across(all_of(by))) |>
    summarise(
      G = n_distinct(GameKey),
      GS = n_distinct(GameKey[TeamFirstPitch %in% TRUE]),
      outs = sum(outs),
      BF = sum(pa_end),
      H = sum(hit),
      R = sum(runs),
      HR = sum(hr),
      BB = sum(is_bb),
      K = sum(is_k),
      HBP = sum(hbp),
      Pitches = n(),
      strikes = sum(strike), swings = sum(swing), whiffs = sum(whiff), csws = sum(csw),
      `FB Velo` = round(safe_mean(fb_velo), 1),
      `Max Velo` = round(safe_max(RelSpeed), 1),
      .groups = "drop"
    ) |>
    mutate(
      IP = format_ip(outs),
      `R/9` = round(rate(27 * R, outs), 2),
      WHIP = round(rate(3 * (H + BB), outs), 2),
      `K%` = round(pct(K, BF), 1),
      `BB%` = round(pct(BB, BF), 1),
      `K-BB%` = round(`K%` - `BB%`, 1),
      `Strike%` = round(pct(strikes, Pitches), 1),
      `Whiff%` = round(pct(whiffs, swings), 1),
      `CSW%` = round(pct(csws, Pitches), 1)
    ) |>
    select(all_of(by), G, GS, IP, BF, H, R, HR, BB, K, HBP, `R/9`, WHIP, `K%`, `BB%`, `K-BB%`,
           Pitches, `Strike%`, `Whiff%`, `CSW%`, `FB Velo`, `Max Velo`, outs)
}

# Season or game-log table for one side
season_table <- function(d, side, by = NULL) {
  if (side == "pitch") {
    pitching_stats(d, by %||% "Pitcher")
  } else {
    hitting_stats(d, by %||% "Batter")
  }
}

# Sortable DT. For pitching, the hidden outs column sorts IP (a string sort
# would put "10.1" before "5.2").
season_dt <- function(df, selectable = TRUE, searchable = TRUE, page_length = 25) {
  cols <- names(df)
  defs <- list()
  if ("outs" %in% cols) {
    outs_i <- match("outs", cols) - 1
    defs <- list(
      list(targets = outs_i, visible = FALSE, searchable = FALSE),
      list(targets = match("IP", cols) - 1, orderData = outs_i, className = "dt-right")
    )
  }
  dt <- datatable(
    df,
    rownames = FALSE,
    selection = if (selectable) "single" else "none",
    class = "compact stripe hover nowrap",
    options = list(
      dom = paste0(if (searchable) "f", "t", if (nrow(df) > page_length) "p"),
      pageLength = page_length,
      scrollX = TRUE,
      columnDefs = defs,
      language = list(search = "", searchPlaceholder = "Find a player", emptyTable = "No players match")
    )
  )
  rate_cols <- intersect(c("AVG", "OBP", "SLG", "OPS"), cols)
  if (length(rate_cols)) dt <- formatRound(dt, rate_cols, digits = 3)
  dt
}

# Average and max velo (pitchers) or exit velo (hitters) by game
trend_plot <- function(d, side) {
  d <- season_prep(d)
  if (side == "pitch") {
    d <- d |> filter(PitcherTeam %in% TEAM_CODES) |> mutate(v = fb_velo)
    title <- "Fastball velo by outing"
    ylab <- "Velo (mph)"
  } else {
    d <- d |> filter(BatterTeam %in% TEAM_CODES) |> mutate(v = ev)
    title <- "Exit velo by game"
    ylab <- "Exit velo (mph)"
  }
  g <- d |>
    group_by(GameDate) |>
    summarise(Average = safe_mean(v), Max = safe_max(v), .groups = "drop") |>
    velo_long()
  g <- g[!is.na(g$value), ]
  validate(need(nrow(g) > 0, "No readings for this player yet."))
  # Navy lines and average points, steel triangles for the max
  p <- ggplot(g, aes(GameDate, value, linetype = stat)) +
    geom_line(color = BRAND$navy, linewidth = 0.9) +
    geom_point(aes(shape = stat, color = stat), size = 2.8) +
    scale_color_manual(values = c(Average = BRAND$navy, Max = BRAND$steel)) +
    scale_shape_manual(values = c(Average = 16, Max = 17)) +
    scale_linetype_manual(values = c(Average = "solid", Max = "dotted")) +
    labs(title = title, x = NULL, y = ylab) +
    chart_theme()
  if (side != "pitch") p <- p + geom_hline(yintercept = 95, linetype = "dashed", color = "grey50")
  p
}

# Average/Max columns to long rows without adding tidyr as a dependency
velo_long <- function(g) {
  rbind(
    data.frame(GameDate = g$GameDate, stat = "Average", value = g$Average),
    data.frame(GameDate = g$GameDate, stat = "Max", value = g$Max)
  )
}

# UI ---------------------------------------------------------------------------

season_ui <- function() {
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 270,
      open = list(desktop = "open", mobile = "always-above"),
      radioButtons("ss_side", NULL, inline = TRUE, choices = c("Hitting" = "hit", "Pitching" = "pitch")),
      dateRangeInput("ss_dates", "Games between", format = "mm/dd/yy"),
      numericInput("ss_min", "Min PA / BF", value = 0, min = 0, step = 1),
      uiOutput("ss_download_ui")
    ),
    uiOutput("ss_main"),
    uiOutput("ss_player")
  )
}

# Server -----------------------------------------------------------------------

season_server <- function(input, output, session, user, tm) {

  # Full date span of the data, set once per change of span
  observe({
    d <- tm()
    req(!is.null(d), nrow(d) > 0)
    rng <- range(d$Date, na.rm = TRUE)
    updateDateRangeInput(session, "ss_dates", start = rng[1], end = rng[2], min = rng[1], max = rng[2])
  })

  # Only offer the sides this user has data for (a player sees only his own)
  observe({
    d <- tm()
    req(user())
    choices <- c("Hitting" = "hit", "Pitching" = "pitch")
    if (!is_staff(user())) {
      has <- c(hit = any(d$BatterTeam %in% TEAM_CODES), pitch = any(d$PitcherTeam %in% TEAM_CODES))
      choices <- choices[has[choices]]
      req(length(choices) > 0)
    }
    current <- isolate(input$ss_side)
    updateRadioButtons(session, "ss_side", choices = choices, inline = TRUE,
                       selected = if (!is.null(current) && current %in% choices) current else choices[[1]])
  })

  ss_data <- reactive({
    d <- tm()
    req(!is.null(d), nrow(d) > 0)
    dates <- input$ss_dates
    if (length(dates) == 2 && !anyNA(dates)) d <- d |> filter(Date >= dates[1], Date <= dates[2])
    side_col <- if (identical(input$ss_side, "pitch")) "PitcherTeam" else "BatterTeam"
    d[d[[side_col]] %in% TEAM_CODES, ]
  })

  ss_table <- reactive({
    req(input$ss_side)
    t <- season_table(ss_data(), input$ss_side)
    min_n <- suppressWarnings(as.numeric(input$ss_min))
    if (length(min_n) && !is.na(min_n) && min_n > 0) {
      t <- t[(if (input$ss_side == "pitch") t$BF else t$PA) >= min_n, ]
    }
    names(t)[1] <- "Player"
    arrange(t, if (input$ss_side == "pitch") desc(outs) else desc(PA))
  })

  output$ss_main <- renderUI({
    req(user())
    if (is.null(tm())) {
      return(card(card_body(p(class = "text-muted mb-0",
        "You don't show up in any TrackMan file yet. Your stats appear here after you play."))))
    }
    who <- if (is_staff(user())) "Team" else "My"
    side <- if (identical(input$ss_side, "pitch")) "pitching" else "hitting"
    n_games <- n_distinct(ss_data()$GameKey)
    card(
      fill = FALSE,
      card_header(sprintf("%s %s (%d %s with TrackMan)", who, side, n_games, if (n_games == 1) "game" else "games")),
      card_body(
        fillable = FALSE,
        DTOutput("ss_table", height = "auto"),
        p(class = "text-muted small mt-2 mb-0",
          "Stats are built from TrackMan tagging. R/9 counts all runs allowed because TrackMan ",
          "can't separate earned runs. Click a player for his game log and trend.")
      )
    )
  })

  output$ss_table <- renderDT(season_dt(ss_table()))

  ss_selected <- reactive({
    i <- input$ss_table_rows_selected
    t <- ss_table()
    if (length(i) != 1 || i > nrow(t)) return(NULL)
    t$Player[i]
  })

  output$ss_player <- renderUI({
    name <- ss_selected()
    req(name)
    tagList(
      card(fill = FALSE, card_header(paste(name, "game log")),
           card_body(fillable = FALSE, DTOutput("ss_log", height = "auto"))),
      card(fill = FALSE, plotOutput("ss_trend", height = "320px"))
    )
  })

  player_rows <- reactive({
    name <- ss_selected()
    req(name)
    col <- if (input$ss_side == "pitch") "Pitcher" else "Batter"
    d <- ss_data()
    d[d[[col]] %in% name, ]
  })

  output$ss_log <- renderDT({
    log <- season_table(player_rows(), input$ss_side, by = c("GameDate", "Opp")) |>
      arrange(desc(GameDate)) |>
      mutate(GameDate = format(GameDate, "%b %d")) |>
      rename(Date = GameDate)
    season_dt(log, selectable = FALSE, searchable = FALSE)
  })

  output$ss_trend <- renderPlot(trend_plot(player_rows(), input$ss_side), res = 96)

  output$ss_download_ui <- renderUI({
    req(is_staff(user()))
    downloadButton("ss_download", "Download CSV", class = "btn-outline-primary btn-sm")
  })

  output$ss_download <- downloadHandler(
    filename = function() paste0("season_", input$ss_side %||% "hit", "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(is_staff(user()))
      t <- ss_table()
      if ("outs" %in% names(t)) t$outs <- NULL
      readr::write_csv(t, file, na = "")
    }
  )
}
