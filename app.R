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
    conditionalPanel("!output.logged_in", login_ui())
  ),
  nav_panel("Home", value = "home", uiOutput("home_ui")),
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

  # Home ----------------------------------------------------------------
  output$home_ui <- renderUI({
    req(user())
    h3(paste0("Welcome, ", user()$name))
  })
}

shinyApp(ui, server)
