library(shiny)

shinyServer(function(input, output) {
  
  output$drzava <- renderPlot({
    ggplot(tabela2 %>% filter(drzava == input$drzava) %>%
             group_by(drzava, leto) %>%
             summarise(izpusti = sum(kolicina_v_tonah)) %>%
             inner_join(tabela_povrsin),
           aes(x = leto, y = izpusti/povrsina_v_km2)) + geom_line(col = "blue") +
      ggtitle("Zračne emisije na površino izbrane države") +
      xlab("leto") + ylab("količina zračnih emisij")
    
    #col = blue v geom_line
  })
})
