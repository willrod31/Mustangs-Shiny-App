library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(sodium)

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f, local = TRUE)

ui <- page_navbar(
  title = APP_TITLE,
  id = "main_nav",
  theme = bs_theme(version = 5, primary = COLOR_PRIMARY, secondary = COLOR_SECONDARY),
  header = tagList(
    tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),
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
  nav_spacer(),
  nav_item(uiOutput("user_badge")),
  nav_item(actionLink("sign_out", "Sign out", class = "nav-link"))
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

  observeEvent(input$sign_out, session$reload())

  output$user_badge <- renderUI({
    req(user())
    span(class = "badge bg-light text-dark me-2",
         paste0(user()$name, " (", user()$role, ")"))
  })

  # TrackMan stats ---------------------------------------------------------
  tm_poll <- trackman_poll(session)
  tm <- reactive({
    req(user())
    tm_poll()
  })
  tm_games <- reactive(game_list(tm()))

  observe({
    g <- tm_games()
    current <- isolate(input$tm_game)
    selected <- if (!is.null(current) && current %in% g$GameKey) current else g$GameKey[1]
    updateSelectInput(session, "tm_game", choices = setNames(g$GameKey, g$label), selected = selected)
  })

  tm_side <- reactive({
    req(user(), input$tm_game, input$tm_view)
    tm() |> filter(GameKey == input$tm_game) |> filter_side(input$tm_view)
  })

  observe({
    view <- input$tm_view
    req(view)
    players <- players_in(tm_side(), view)
    choices <- if (is_pitcher_view(view)) players else c("Whole lineup" = "__all__", setNames(players, players))
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

  # Schedule and game pages ----------------------------------------------
  sched_signal <- change_signal(session, 5000, \() SCHEDULE_FILE)
  games_signal <- change_signal(session, 5000, \() list.files(GAMES_DIR, recursive = TRUE, full.names = TRUE))
  files_bump <- reactiveVal(0) # forces a rescan right after a post

  sched <- reactive({
    req(user())
    sched_signal()
    load_schedule()
  })

  listing <- reactive({
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
  wanted_tab <- reactiveVal(NULL) # tab to open after a post

  observeEvent(input$sched_table_rows_selected, {
    i <- input$sched_table_rows_selected
    rows <- sched_rows()
    req(length(i) == 1, i <= nrow(rows))
    gid <- rows$game_id[i]
    if (!identical(gid, selected_game())) {
      selected_game(gid)
      wanted_tab(NULL)
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

  game_tm_key <- reactive({
    g <- game_row()
    match_trackman(g$game_id, g$date, tm_games())
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

  file_preview <- function(path, name, k) NULL

  file_entry <- function(f, k) {
    div(
      class = "mb-4",
      div(
        class = "d-flex flex-wrap align-items-center gap-2 mb-2",
        strong(f$name),
        if (f$coaches) span(class = "badge bg-warning text-dark", "Coaches only"),
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
            h3(g$matchup, class = "mb-1"),
            p(when, class = "text-muted mb-2"),
            if (g$played) div(g$result, class = "fs-2 fw-bold", style = paste0("color:", COLOR_PRIMARY))
            else p("Not played yet", class = "text-muted mb-0")
          ),
          if (!is.null(game_tm_key())) {
            actionButton("open_tm", "TrackMan stats", class = "btn-secondary ms-auto")
          }
        )
      )
    )

    if (!nrow(files)) {
      return(tagList(header, card(card_body(p(class = "text-muted mb-0", "No reports for this game yet.")))))
    }

    types <- intersect(GAME_REPORT_TYPES, files$type)
    tabs <- lapply(types, function(t) {
      idx <- which(files$type == t)
      nav_panel(t, value = t, lapply(idx, function(k) file_entry(files[k, ], k)))
    })
    keep <- intersect(c(isolate(wanted_tab()), isolate(input$game_tabs)), types)
    selected <- if (length(keep)) keep[1] else types[1]
    tagList(header, do.call(navset_card_pill, c(tabs, list(id = "game_tabs", selected = selected))))
  })

  observeEvent(input$open_tm, {
    key <- game_tm_key()
    req(key)
    nav_select("main_nav", "trackman")
    updateSelectInput(session, "tm_game", selected = key)
  })

  # Posting reports (coaches only)
  output$post_card <- renderUI({
    req(is_coach(user()), selected_game())
    files_bump() # clears the file input after a post
    card(
      fill = FALSE,
      card_header("Post a report to this game"),
      card_body(
        fillable = FALSE,
        layout_column_wrap(
          width = "200px", fill = FALSE,
          selectInput("post_type", "Type", choices = GAME_REPORT_TYPES),
          radioButtons("post_vis", "Who can see it",
                       choices = c("Whole team" = "team", "Coaches only" = "coaches"))
        ),
        fileInput("post_file", "File", width = "100%"),
        actionButton("post_btn", "Post report", class = "btn-primary"),
        p(class = "text-muted small mt-2 mb-0",
          "Posting a pitcher, hitter or umpire report replaces the old one of that type.")
      )
    )
  })

  observeEvent(input$post_btn, {
    req(is_coach(user()), selected_game())
    f <- input$post_file
    if (is.null(f)) {
      showNotification("Choose a file first.", type = "warning")
      return()
    }
    type <- input$post_type
    result <- tryCatch(
      save_game_report(selected_game(), type, identical(input$post_vis, "coaches"), f$datapath, f$name),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      showNotification(paste("Could not post:", conditionMessage(result)), type = "error")
      return()
    }
    wanted_tab(type)
    files_bump(files_bump() + 1)
    showNotification(paste("Posted", basename(result)), type = "message")
  })

  # Home ----------------------------------------------------------------
  output$home_ui <- renderUI({
    req(user())
    h3(paste0("Welcome, ", user()$name))
  })
}

shinyApp(ui, server)
