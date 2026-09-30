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

  # Home ----------------------------------------------------------------
  output$home_ui <- renderUI({
    req(user())
    h3(paste0("Welcome, ", user()$name))
  })
}

shinyApp(ui, server)
