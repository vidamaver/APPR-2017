library(shiny)

shinyServer(function(input, output) {
  
  output$drzava <- renderPlot({
    selectInput("drzava", label="Izberi državo",
                choices=c("Vse", levels(izpusti.povrsina$drzava)))
    ggplot(graf, aes(x = leto)) + geom_line(col = "blue") +
      ggtitle("Zračne emisije na površino izbrane države") + xlab("leto") + ylab("količina zračnih emisij")
  })
})
