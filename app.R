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
  theme = bs_theme(version = 5, primary = COLOR_PRIMARY, secondary = COLOR_SECONDARY),
  nav_panel("Home", h3("Coming soon"))
)

server <- function(input, output, session) {}

shinyApp(ui, server)
