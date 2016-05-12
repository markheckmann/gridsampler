#### Shiny server file ####

shinyServer(function(input, output, session) {
 output$plot1 <- renderPlot({
   ggplot(data = data.frame(x = 1:10, y = 10:1), aes(x = x, y = y)) +
     geom_point()
 })
})
