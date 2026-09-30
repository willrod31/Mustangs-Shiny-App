library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(sodium)

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f, local = TRUE)

ensure_owner()

options(shiny.maxRequestSize = 50 * 1024^2)

# All of the app's CSS. The team colors come from BRAND through the CSS
# variables in :root; the rest is a plain string (no sprintf, so % is safe).
APP_CSS <- paste0(
  sprintf(":root { --navy: %s; --black: %s; --silver: %s; --steel: %s; --light: %s; }",
          BRAND$navy, BRAND$black, BRAND$silver, BRAND$steel, BRAND$light),
  "
/* Top bar: navy with a silver horseshoe stripe */
nav.navbar { border-bottom: 4px solid var(--silver) !important; }
.brand-badge { display: inline-flex; align-items: center; background: #fff; border-radius: 8px;
  padding: 2px 6px; margin-right: .6rem; }
.brand-badge img { height: 34px; width: auto; }
.brand-name { color: #fff; font-weight: 700; }
.navbar .navbar-nav > .nav-item > .nav-link { color: rgba(255, 255, 255, .85); }
.navbar .navbar-nav > .nav-item > .nav-link:hover,
.navbar .navbar-nav > .nav-item > .nav-link:focus,
.navbar .navbar-nav > .nav-item > .nav-link.active,
.navbar .navbar-nav > .nav-item > .nav-link.show {
  color: #fff; box-shadow: inset 0 -3px 0 var(--silver); }
.navbar-toggler { border-color: rgba(255, 255, 255, .5); }
.navbar-toggler-icon { filter: invert(1); }
/* The Account menu is a light dropdown with dark links */
.navbar .dropdown-menu { --bs-dropdown-bg: #fff; --bs-dropdown-color: var(--black);
  --bs-dropdown-link-color: var(--black); --bs-dropdown-link-hover-color: var(--black);
  --bs-dropdown-link-hover-bg: var(--light); --bs-dropdown-link-active-color: #fff;
  --bs-dropdown-link-active-bg: var(--navy); --bs-dropdown-border-color: #D9DADD; }
.navbar .dropdown-menu .dropdown-item { color: var(--black); }
.account-name { color: var(--navy); font-weight: 700; white-space: nowrap; }
@media (min-width: 992px) {
  .navbar .navbar-nav > .nav-item > .nav-link { padding-left: .55rem; padding-right: .55rem;
    font-size: .93rem; }
}
@media (min-width: 992px) and (max-width: 1399.98px) { .brand-name { display: none; } }

/* Login screen */
#login-overlay { position: fixed; inset: 0; z-index: 2000; display: flex; align-items: center;
  justify-content: center; padding: 16px;
  /* The navy gradient, slightly see-through over the Hooker Field photo */
  background: radial-gradient(circle at 50% 30%, rgba(44, 52, 122, .80) 0, rgba(31, 37, 94, .88) 55%,
      rgba(18, 22, 58, .96) 100%),
    url('field.jpg') center / cover no-repeat, var(--navy); }
.login-card { width: 100%; max-width: 380px; border-top: 6px solid var(--silver); }
.login-logo { width: 170px; max-width: 60%; height: auto; }
.login-title { color: var(--navy); font-weight: 700; }

/* Pages, cards and tables */
body, .bslib-page-navbar { background: var(--light); }
.text-navy { color: var(--navy); }
.welcome-logo { width: 64px; height: auto; flex: none; }
.field-banner { position: relative; height: 170px; border-radius: 10px; overflow: hidden;
  margin-top: .5rem; background: url('field.jpg') center 60% / cover no-repeat;
  border-bottom: 4px solid var(--silver); }
.field-banner::after { content: ''; position: absolute; inset: 0;
  background: linear-gradient(180deg, rgba(31, 37, 94, 0) 40%, rgba(31, 37, 94, .85) 100%); }
.field-banner span { position: absolute; left: 16px; bottom: 10px; z-index: 1; color: #fff;
  font-weight: 700; text-shadow: 0 1px 3px rgba(0, 0, 0, .5); }
@media (max-width: 575.98px) {
  .field-banner { height: 110px; }
  .field-banner span { font-size: .8rem; left: 12px; bottom: 8px; }
}
.card-header { background: #fff; color: var(--navy); font-weight: 700;
  border-bottom: 2px solid var(--silver); }
table.dataTable thead th, .table thead th { color: var(--navy); }
table.dataTable tbody tr.selected > * {
  box-shadow: inset 0 0 0 9999px rgba(31, 37, 94, .85) !important; color: #fff !important; }
table.dataTable tbody tr.selected a { color: #fff !important; }
.nav-pills .nav-link.active, .nav-pills .show > .nav-link { background-color: var(--navy); color: #fff; }
.nav-underline .nav-link.active, .nav-underline .show > .nav-link {
  color: var(--navy); border-bottom-color: var(--navy); }
.badge-steel { background-color: var(--steel); color: #fff; }
/* DT widgets built inside renderUI can keep a fixed height and leave a gap */
.datatables.html-widget { height: auto !important; }
"
)

ui <- page_navbar(
  title = tags$span(
    class = "d-inline-flex align-items-center",
    tags$span(class = "brand-badge", tags$img(src = "logo.png", alt = "Mustangs")),
    tags$span(class = "brand-name", APP_TITLE)
  ),
  window_title = APP_TITLE,
  id = "main_nav",
  navbar_options = navbar_options(bg = BRAND$navy, theme = "dark"),
  theme = bs_theme(
    version = 5,
    primary = BRAND$navy, secondary = BRAND$steel, dark = BRAND$black,
    bg = BRAND$white, fg = BRAND$black, "border-color" = "#D9DADD",
    base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"),
    heading_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif")
  ),
  header = tagList(
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
      tags$link(rel = "apple-touch-icon", href = "logo.png"),
      tags$meta(name = "theme-color", content = BRAND$navy),
      tags$style(HTML(APP_CSS))
    ),
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('scroll_to_game', function(msg) {
         if (window.innerWidth < 768) {
           setTimeout(function() {
             var el = document.getElementById('game_page');
             if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'});
           }, 400);
         }
       });"
    )),
    conditionalPanel("!output.logged_in", login_ui())
  ),
  nav_panel("Home", value = "home", uiOutput("home_ui")),
  nav_panel("Schedule", value = "schedule", schedule_ui()),
  nav_panel("TrackMan stats", value = "trackman", trackman_ui()),
  nav_panel("Season stats", value = "season", icon = icon("chart-simple"), season_ui()),
  nav_panel("Scouting library", value = "library", library_ui()),
  nav_spacer(),
  nav_menu(
    title = "Account", icon = icon("circle-user"), align = "right",
    nav_item(uiOutput("user_badge")),
    nav_item(actionLink("logout", "Sign out", class = "dropdown-item"))
  )
)

server <- function(input, output, session) {

  # Login ---------------------------------------------------------------
  user <- reactiveVal(NULL)
  login_msg <- reactiveVal("")

  observeEvent(input$login_btn, {
    u <- check_login(input$login_user, input$login_pass)
    if (is.null(u)) {
      login_msg("Wrong username or password.")
    } else {
      login_msg("")
      user(u)
    }
  })

  output$login_error <- renderText(login_msg())
  output$logged_in <- reactive(!is.null(user()))
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)

  observeEvent(input$logout, session$reload())

  output$user_badge <- renderUI({
    req(user())
    span(class = "dropdown-item-text account-name", paste0(user()$name, " (", user()$role, ")"))
  })
  # It sits in the closed Account menu, so render it before the menu opens
  outputOptions(output, "user_badge", suspendWhenHidden = FALSE)

  # TrackMan stats ---------------------------------------------------------
  tm_signal <- change_signal(session, 10000, trackman_files)
  tm_bump <- reactiveVal(0) # forces a reload right after a TrackMan upload
  tm_all <- settled_reactive(function() {
    req(user())
    tm_signal()
    tm_bump()
    load_trackman()
  })
  # Staff get every pitch. A player gets only his own pitches and plate
  # appearances for our team, or NULL if he isn't in any file. Every tab that
  # shows TrackMan data reads this, so the filter covers them all.
  tm <- reactive({
    d <- tm_all()
    u <- user()
    if (is_staff(u)) return(d)
    keep <- (d$Pitcher %in% u$tm_name & d$PitcherTeam %in% TEAM_CODES) |
      (d$Batter %in% u$tm_name & d$BatterTeam %in% TEAM_CODES)
    if (!any(keep)) return(NULL)
    d[keep, ]
  })
  tm_games <- reactive(game_list(tm() %||% empty_trackman()))

  output$tm_has_data <- reactive(!is.null(tm()))
  outputOptions(output, "tm_has_data", suspendWhenHidden = FALSE)

  # A player only gets the sides he has data for, named for him
  observe({
    req(user(), !is_staff(user()))
    d <- tm()
    req(!is.null(d))
    choices <- c("My pitching" = "our_p", "My hitting" = "our_h")
    choices <- choices[c(any(d$PitcherTeam %in% TEAM_CODES), any(d$BatterTeam %in% TEAM_CODES))]
    current <- isolate(input$tm_view)
    updateRadioButtons(session, "tm_view", choices = choices,
                       selected = if (!is.null(current) && current %in% choices) current else choices[[1]])
  })

  observe({
    g <- tm_games()
    current <- isolate(input$tm_game)
    selected <- if (!is.null(current) && current %in% g$GameKey) current else g$GameKey[1]
    updateSelectInput(session, "tm_game", choices = setNames(g$GameKey, g$label), selected = selected)
  })

  tm_side <- reactive({
    req(user(), !is.null(tm()), input$tm_game, input$tm_view)
    req(is_staff(user()) || input$tm_view %in% c("our_p", "our_h"))
    tm() |> filter(GameKey == input$tm_game) |> filter_side(input$tm_view)
  })

  observe({
    view <- input$tm_view
    req(view)
    players <- players_in(tm_side(), view)
    choices <- if (is_pitcher_view(view) || !is_staff(user())) {
      players
    } else {
      c("Whole lineup" = "__all__", setNames(players, players))
    }
    current <- isolate(input$tm_player)
    selected <- if (!is.null(current) && current %in% choices) current else unname(choices[1])
    updateSelectInput(session, "tm_player", choices = choices, selected = selected)
  })

  tm_sel <- reactive({
    d <- tm_side()
    p <- input$tm_player
    req(p)
    col <- if (is_pitcher_view(input$tm_view)) "Pitcher" else "Batter"
    if (p != "__all__") d <- d[d[[col]] %in% p, ]
    validate(need(nrow(d) > 0, "No pitches for this selection."))
    d
  })

  output$tm_line <- renderTable(pitcher_line(tm_sel()), digits = 0, na = "-", striped = TRUE)
  output$tm_mix <- renderDT(stat_table(pitch_mix(tm_sel())))
  output$tm_move <- renderPlot(movement_plot(tm_sel()), res = 96)
  output$tm_loc <- renderPlot(location_plot(tm_sel()), res = 96)
  output$tm_velo <- renderPlot(velo_plot(tm_sel()), res = 96)
  output$tm_log <- renderDT(stat_table(pitch_log(tm_sel()), page_length = 15))

  output$hit_summary <- renderDT(stat_table(hitter_summary(tm_sel())))
  output$hit_zone <- renderPlot(zone_outcome_plot(tm_sel()), res = 96)
  output$hit_bip <- renderDT(stat_table(batted_balls(tm_sel())))

  # Season stats -----------------------------------------------------------
  season_server(input, output, session, user, tm)

  # Schedule and game pages ----------------------------------------------
  sched_signal <- change_signal(session, 5000, \() SCHEDULE_FILE)
  games_signal <- change_signal(session, 5000, \() list.files(GAMES_DIR, recursive = TRUE, full.names = TRUE))
  files_bump <- reactiveVal(0) # forces a rescan right after a post
  sched_bump <- reactiveVal(0) # forces a reload right after an edit

  sched <- settled_reactive(function() {
    req(user())
    sched_signal()
    sched_bump()
    load_schedule()
  })

  listing <- settled_reactive(function() {
    req(user())
    games_signal()
    files_bump()
    scan_game_files()
  })

  # Schedule with a report status per game (coaches-only files are only
  # counted for coaches)
  sched_status <- reactive({
    s <- sched()
    s$reports <- report_status(s$game_id, user(), listing())
    s
  })

  sched_rows <- reactive({
    s <- sched_status()
    if (identical(input$sched_mode, "full")) {
      s |> arrange(date)
    } else {
      s |> filter(played | reports != "") |> arrange(desc(date))
    }
  })

  selected_game <- reactiveVal(NULL)

  observeEvent(input$sched_table_rows_selected, {
    i <- input$sched_table_rows_selected
    rows <- sched_rows()
    req(length(i) == 1, i <= nrow(rows))
    gid <- rows$game_id[i]
    if (!identical(gid, selected_game())) {
      selected_game(gid)
      session$sendCustomMessage("scroll_to_game", list())
    }
  })

  output$sched_table <- renderDT({
    rows <- sched_rows()
    page_len <- 12
    sel <- match(isolate(selected_game()) %||% NA_character_, rows$game_id)
    sel <- if (is.na(sel)) NULL else sel
    display <- rows |>
      transmute(Date = format(date, "%a %b %d"), Game = matchup, Result = result, Reports = reports)
    datatable(
      display,
      rownames = FALSE,
      selection = list(mode = "single", selected = sel),
      class = "compact hover",
      options = list(
        dom = if (nrow(display) > page_len) "ftp" else "ft",
        pageLength = page_len,
        displayStart = if (is.null(sel)) 0 else (sel - 1) %/% page_len * page_len,
        ordering = FALSE,
        language = list(search = "", searchPlaceholder = "Find a team or date",
                        emptyTable = "No games yet")
      )
    )
  })

  game_row <- reactive({
    gid <- selected_game()
    req(gid)
    g <- sched() |> filter(game_id == gid)
    req(nrow(g) == 1)
    g
  })

  current_files <- reactive({
    req(user(), selected_game())
    game_files(selected_game(), user(), listing())
  })

  # Matched against every game (so doubleheaders pair up right), then kept
  # only if this user can see that game in the TrackMan tab.
  game_tm_key <- reactive({
    g <- game_row()
    key <- match_trackman(g$game_id, g$date, game_list(tm_all()))
    if (!is.null(key) && key %in% tm_games()$GameKey) key else NULL
  })

  MAX_GAME_FILES <- 12
  for (i in seq_len(MAX_GAME_FILES)) {
    local({
      k <- i
      output[[paste0("gdl_", k)]] <- downloadHandler(
        filename = function() current_files()$name[k],
        content = function(file) file.copy(current_files()$path[k], file)
      )
    })
  }

  file_preview <- function(path, name, k) {
    req(user())
    preview_ui(session, path, paste0("game_file_", k))
  }

  file_entry <- function(f, k) {
    div(
      class = "mb-4",
      div(
        class = "d-flex flex-wrap align-items-center gap-2 mb-2",
        strong(f$name),
        if (f$coaches) span(class = "badge badge-steel", f$who),
        if (k <= MAX_GAME_FILES) {
          downloadButton(paste0("gdl_", k), "Download", class = "btn-sm btn-outline-primary ms-auto")
        }
      ),
      file_preview(f$path, f$name, k)
    )
  }

  output$game_page <- renderUI({
    req(user())
    if (is.null(selected_game())) {
      return(card(card_body(p(class = "text-muted mb-0",
        "Pick a game from the list to see its pitcher, hitter and umpire reports."))))
    }
    g <- game_row()
    files <- current_files()
    when <- paste(c(format(g$date, "%A, %B %d, %Y"), g$time, g$location)[c(TRUE, g$time != "", g$location != "")],
                  collapse = " | ")

    header <- card(
      fill = FALSE,
      card_body(
        div(
          class = "d-flex flex-wrap align-items-start gap-2",
          div(
            h3(g$matchup, class = "mb-1 text-navy fw-bold"),
            p(when, class = "text-muted mb-2"),
            if (g$played) div(g$result, class = "fs-2 fw-bold text-navy")
            else p("Not played yet", class = "text-muted mb-0")
          ),
          div(
            class = "ms-auto d-flex flex-wrap gap-2",
            if (!is.null(game_tm_key())) actionButton("open_tm", "TrackMan stats", class = "btn-secondary"),
            if (is_staff(user())) actionButton("add_to_game", "Add files to this game", class = "btn-outline-primary")
          )
        )
      )
    )

    if (!nrow(files)) {
      return(tagList(header, card(card_body(p(class = "text-muted mb-0", "No reports for this game yet.")))))
    }

    types <- intersect(REPORT_TYPES, files$type)
    tabs <- lapply(types, function(t) {
      idx <- which(files$type == t)
      nav_panel(type_tab[[t]], value = t, lapply(idx, function(k) file_entry(files[k, ], k)))
    })
    keep <- intersect(isolate(input$game_tabs), types)
    selected <- if (length(keep)) keep[1] else types[1]
    tagList(header, do.call(navset_card_pill, c(tabs, list(id = "game_tabs", selected = selected))))
  })

  observeEvent(input$open_tm, {
    key <- game_tm_key()
    req(key)
    nav_select("main_nav", "trackman")
    updateSelectInput(session, "tm_game", selected = key)
  })

  observeEvent(input$add_to_game, {
    req(is_staff(user()), selected_game())
    nav_select("main_nav", "manage")
    nav_select("admin_tabs", "game_files")
    updateSelectInput(session, "a_game", selected = selected_game())
  })

  # Scouting library -----------------------------------------------------
  lib_signal <- change_signal(session, 5000, \() REPORTS_IDX)
  lib_bump <- reactiveVal(0)

  lib_all <- reactive({
    req(user())
    lib_signal()
    lib_bump()
    read_index()
  })
  lib_visible <- reactive(visible_reports(lib_all(), user()))

  observe({
    opps <- sort(unique(lib_visible()$opponent))
    opps <- opps[opps != ""]
    current <- isolate(input$lib_opp)
    updateSelectInput(session, "lib_opp", choices = c("All" = "", opps),
                      selected = if (!is.null(current) && current %in% opps) current else "")
  })

  lib_rows <- reactive({
    d <- lib_visible()
    if (nzchar(input$lib_cat %||% "")) d <- d |> filter(category == input$lib_cat)
    if (nzchar(input$lib_opp %||% "")) d <- d |> filter(opponent == input$lib_opp)
    q <- trimws(input$lib_search %||% "")
    if (nzchar(q)) {
      hay <- paste(d$title, d$player, d$notes, d$opponent)
      d <- d[grepl(q, hay, fixed = TRUE, ignore.case = TRUE), ]
    }
    d |> arrange(desc(date), desc(uploaded_at))
  })

  lib_selected <- reactiveVal(NULL)
  observeEvent(input$lib_table_rows_selected, {
    i <- input$lib_table_rows_selected
    rows <- lib_rows()
    req(length(i) == 1, i <= nrow(rows))
    lib_selected(rows$id[i])
  })

  output$lib_table <- renderDT({
    rows <- lib_rows()
    sel <- match(isolate(lib_selected()) %||% NA_character_, rows$id)
    display <- rows |>
      transmute(Date = date, Title = title, Category = category, Opponent = opponent,
                Player = player, Visibility = visibility)
    if (!is_staff(user())) display$Visibility <- NULL
    datatable(
      display,
      rownames = FALSE,
      selection = list(mode = "single", selected = if (is.na(sel)) NULL else sel),
      class = "compact hover",
      options = list(
        dom = if (nrow(display) > 15) "tp" else "t",
        pageLength = 15,
        scrollX = TRUE,
        language = list(emptyTable = "No reports match")
      )
    )
  })

  lib_item <- reactive({
    id <- lib_selected()
    req(id)
    item <- lib_visible() |> filter(id == !!id)
    req(nrow(item) == 1)
    item
  })

  output$lib_dl <- downloadHandler(
    filename = function() sub("^[0-9]+_", "", lib_item()$file),
    content = function(file) file.copy(library_path(lib_item()$file), file)
  )

  output$lib_preview <- renderUI({
    item <- lib_item()
    meta <- c(item$category, item$opponent, item$player, item$date)
    card(
      fill = FALSE,
      card_header(
        class = "d-flex flex-wrap align-items-center gap-2",
        strong(item$title),
        if (item$visibility == "Coaches") span(class = "badge badge-steel", "Coaches only"),
        downloadButton("lib_dl", "Download", class = "btn-sm btn-outline-primary ms-auto")
      ),
      card_body(
        fillable = FALSE,
        p(class = "text-muted mb-1", paste(meta[meta != ""], collapse = " | ")),
        if (nzchar(item$notes)) p(item$notes),
        preview_ui(session, library_path(item$file), "lib_file")
      )
    )
  })

  # Add & manage (coaches and admin). Added after login so players never get
  # it in their page at all.
  observeEvent(user(), {
    if (is_staff(user())) {
      nav_insert("main_nav", nav_panel("Add & manage", value = "manage", icon = icon("pen-to-square"),
                                       manage_ui(admin = is_admin(user()))),
                 target = "library", position = "after")
    }
  }, once = TRUE)

  manage_server(input, output, session, user, sched = sched, sched_bump = sched_bump,
                listing = listing, files_bump = files_bump, tm_all = tm_all, tm_bump = tm_bump,
                lib_all = lib_all, lib_bump = lib_bump)

  # Home ----------------------------------------------------------------
  # Link that sends a value to the server without a round trip through inputs
  open_link <- function(label, input_id, value) {
    tags$a(href = "#", label, onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;", input_id, value
    ))
  }

  output$home_ui <- renderUI({
    req(user())
    games <- sched_status() |>
      filter(played | reports != "") |>
      arrange(desc(date)) |>
      head(5)
    newest <- lib_visible() |> arrange(desc(uploaded_at)) |> head(5)

    games_tbl <- if (nrow(games)) {
      tags$table(
        class = "table table-sm table-hover mb-2",
        tags$thead(tags$tr(tags$th("Date"), tags$th("Game"), tags$th("Result"), tags$th("Reports"))),
        tags$tbody(lapply(seq_len(nrow(games)), function(i) {
          g <- games[i, ]
          tags$tr(tags$td(format(g$date, "%a %b %d")),
                  tags$td(open_link(g$matchup, "home_open_game", g$game_id)),
                  tags$td(g$result), tags$td(g$reports))
        }))
      )
    } else {
      p(class = "text-muted", "No games with results or reports yet.")
    }

    reports_tbl <- if (nrow(newest)) {
      tags$table(
        class = "table table-sm table-hover mb-0",
        tags$thead(tags$tr(tags$th("Date"), tags$th("Title"), tags$th("Category"))),
        tags$tbody(lapply(seq_len(nrow(newest)), function(i) {
          r <- newest[i, ]
          tags$tr(tags$td(r$date), tags$td(open_link(r$title, "home_open_report", r$id)),
                  tags$td(r$category))
        }))
      )
    } else {
      p(class = "text-muted mb-0", "No library reports yet.")
    }

    tagList(
      div(class = "field-banner", role = "img", `aria-label` = "Hooker Field",
          tags$span("Hooker Field, home of the Martinsville Mustangs")),
      div(
        class = "d-flex align-items-center gap-3 mt-3 mb-3",
        tags$img(src = "logo.png", alt = "Mustangs logo", class = "welcome-logo"),
        div(
          h3(paste0("Welcome, ", user()$name), class = "mb-1 text-navy fw-bold"),
          p(class = "mb-0 text-muted",
            "Find any game on the Schedule tab to see its pitcher, hitter and umpire reports.")
        )
      ),
      layout_columns(
        col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
        card(
          fill = FALSE,
          card_header("Latest games"),
          card_body(
            fillable = FALSE,
            div(class = "table-responsive", games_tbl),
            actionLink("home_full_sched", "Full schedule")
          )
        ),
        card(
          fill = FALSE,
          card_header("Newest reports"),
          card_body(fillable = FALSE, div(class = "table-responsive", reports_tbl))
        )
      )
    )
  })

  observeEvent(input$home_full_sched, {
    req(user())
    updateRadioButtons(session, "sched_mode", selected = "full")
    nav_select("main_nav", "schedule")
  })

  observeEvent(input$home_open_game, {
    req(user())
    gid <- input$home_open_game
    req(gid %in% sched()$game_id)
    selected_game(gid)
    nav_select("main_nav", "schedule")
    row <- match(gid, sched_rows()$game_id)
    if (!is.na(row)) selectRows(dataTableProxy("sched_table"), row)
    session$sendCustomMessage("scroll_to_game", list())
  })

  observeEvent(input$home_open_report, {
    req(user())
    id <- input$home_open_report
    req(id %in% lib_visible()$id)
    lib_selected(id)
    nav_select("main_nav", "library")
    row <- match(id, lib_rows()$id)
    if (!is.na(row)) selectRows(dataTableProxy("lib_table"), row)
  })
}

shinyApp(ui, server)
