library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Zračne emisije po državah"),
  
  tabsetPanel(
      tabPanel("leto",
               DT::dataTableOutput("Leta")),
      
      tabPanel("izpusti",
               sidebarPanel(
                  uiOutput("Izpusti po letih")
                ),
               mainPanel(plotOutput("drzava")))
    )
  + plotOutput$drzava
))
