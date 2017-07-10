library(shiny)

shinyServer(function(input, output) {
  output$drzava <- renderPlot({
    dcast(drzava, value.var = "izpusti.povrsina")
  })
  
  output$drzava <- renderPlot(
    selectInput("drzava", label="Izberi državo",
                choices=c("Vse", levels(izpusti.povrsina$drzava)))
    ggplot(t, aes(x = leto)) + geom_line() +
      ggtitle(Zračne emisije na površino izbrane države) + xlab("leto") + ylab("količina zračnih emisij")
  })
})
