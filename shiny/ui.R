library(shiny)

shinyUI(fluidPage(
  titlePanel("Zračne emisije po državah"),
  sidebarPanel(
    selectInput("drzava", label = "Izberi državo:",
                choices = parse_character(izpusti.povrsina$drzava), selected = 'Slovenia')
  ),
  mainPanel(plotOutput("drzava"))
)
)

