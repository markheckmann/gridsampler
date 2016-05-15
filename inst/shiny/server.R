#### Shiny server file ####

shinyServer(function(input, output, session) {
  #### Logic for column 1 ####

  # Updating input elements
  observe({
    updateNumericInput(session, "attribute_num", min = input$minimum1, max = input$maximum1)

    if (input$attribute_num < input$minimum1 | input$attribute_num > input$maximum1) {
      updateNumericInput(session, "attribute_num", value = input$minimum1)
    }
  })

  # Plot
  output$plot1 <- renderPlot({
   ggplot(data = data.frame(x = 1:10, y = 10:1), aes(x = x, y = y)) +
     geom_point()
  })

  #### Logic for column 2 ####
  # Updating input elements
  observe({
    updateNumericInput(session, "category", min = input$minimum2, max = input$maximum2)

    if (input$category < input$minimum2 | input$category > input$maximum2) {
      updateNumericInput(session, "category", value = input$minimum2)
    }
  })

  output$plot2 <- renderPlot({
   ggplot(data = data.frame(x = 1:10, y = 1:10), aes(x = x, y = y)) +
     geom_point()
  })

  #### Logic for column 3 ####
  output$plot3_1 <- renderPlot({
   ggplot(data = data.frame(x = 1:10, y = (1:10)^-1), aes(x = x, y = y)) +
     geom_point()
  })

  output$plot3_2 <- renderPlot({
   ggplot(data = data.frame(x = 1:10, y = (1:10)^2), aes(x = x, y = y)) +
     geom_point()
  })
})
