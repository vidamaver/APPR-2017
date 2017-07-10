library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Zračne emisije po državah"),
  
  graf = ggplot(tabela2 %>% filter(drzava) %>%
                group_by(leto, drzava) %>%
                summarise(izpusti = sum(kolicina_v_tonah)) %>%
                inner_join(tabela_povrsin),
              aes(x = leto, y = izpusti/povrsina_v_km2)) + geom_line()
  
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

