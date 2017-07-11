library(shiny)

shinyUI(fluidPage(
  titlePanel("Zračne emisije po državah"),
  sidebarPanel(
    selectInput("drzava", label = "Izberi državo:",
                choices = c("Slovenia", parse_character(izpusti.povrsina$drzava)))
  ),
  mainPanel(plotOutput("drzava"))
)
)

