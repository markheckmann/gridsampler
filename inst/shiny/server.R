#### Shiny server file ####

shinyServer(function(input, output, session) {

  #### Logic for column 1 ####

  # Updating input elements
  observe({
    validate(need(!is.na(input$minimum1), "Value must be set!"))
    validate(need(!is.na(input$maximum1), "Value must be set!"))

    updateNumericInput(session, "attribute_num", min = input$minimum1, max = input$maximum1)

    if (input$attribute_num < input$minimum1 | input$attribute_num > input$maximum1) {
      updateNumericInput(session, "attribute_num", value = input$minimum1)
    }

    attributes   <- vector_attributes_final()
    current_prob <- attributes$y[attributes$x == input$attribute_num]
    updateNumericInput(session, "probability1", value = round(current_prob, 3))
  })

  # Creating the vector from preset
  vector_attributes <- reactive({
    input$preset_go1

    attributes_x <- seq(input$minimum1, input$maximum1, by = 1)

    if (input$preset_types1 == "Normal") {
      out_y <- dnorm(attributes_x, mean = input$`1_norm_mean`, sd = input$`1_norm_sd`)
    } else if (input$preset_types1 == "Poisson") {
      out_y <- dpois(attributes_x, lambda = input$`1_pois_lambda`)
    } else if (input$preset_types1 == "Exponential") {
      out_y <- dexp(attributes_x, rate = input$`1_exp_rate`)
    } else if (input$preset_types1 == "Uniform") {
      out_y <- dunif(attributes_x, min = input$minimum1, max = input$maximum1)
    }

    out <- data.frame(x = attributes_x, y = out_y, mark = "No")

    return(out)
  })

  vector_attributes_final <- reactive({
    attributes <- vector_attributes()

    # attributes$y[attributes$x == input$attribute_num] <- isolate(input$probability1)

    return(attributes)
  })


  # Plot
  output$plot1 <- renderPlot({

    data <- vector_attributes_final()
    #data <- attributes
    data$mark[data$x == input$attribute_num] <- "Yes"

    p <- ggplot(data = data, aes(x = x, weight = y, fill = mark)) +
          geom_bar(color = "black") +
          scale_fill_manual(values = c(No = "black", Yes = "red2"), guide = F) +
          labs(x = "Attribute", y = "P(Attribute)") +
      theme_bw() +
      theme(plot.background = element_rect(fill = "#f5f5f5"))

    if (input$plot1_fixy) {
      p <- p + ylim(0, 1)
    }

    return(p)

  })

  #### Logic for column 2 ####
  # Updating input elements
  observe({
    validate(need(!is.na(input$minimum2), "Value must be set!"))
    validate(need(!is.na(input$maximum2), "Value must be set!"))

    updateNumericInput(session, "category", min = input$minimum2, max = input$maximum2)

    if (input$category < input$minimum2 | input$category > input$maximum2) {
      updateNumericInput(session, "category", value = input$minimum2)
    }
  })

  # Creating the vector from preset
  vector_category <- reactive({
    input$preset_go2

    categories_x <- seq(input$minimum2, input$maximum2, by = 1)

    if (input$preset_types2 == "Normal") {
      out_y <- dnorm(categories_x, mean = input$`2_norm_mean`, sd = input$`2_norm_sd`)
    } else if (input$preset_types2 == "Poisson") {
      out_y <- dpois(categories_x, lambda = input$`2_pois_lambda`)
    } else if (input$preset_types2 == "Exponential") {
      out_y <- dexp(categories_x, rate = input$`2_exp_rate`)
    } else if (input$preset_types2 == "Uniform") {
      out_y <- dunif(categories_x, min = input$minimum2, max = input$maximum2)
    }

    out <- data.frame(x = categories_x, y = out_y, mark = "No")

    return(out)
  })

  output$plot2 <- renderPlot({
    data <- vector_category()
    data$mark[data$x == input$category] <- "Yes"

    p <- ggplot(data = data, aes(x = x, weight = y, fill = mark)) +
          geom_bar(color = "black") +
          scale_fill_manual(values = c(No = "black", Yes = "red2"), guide = F) +
          labs(x = "Categories", y = "P(Category)") +
      theme_bw() +
      theme(plot.background = element_rect(fill = "#f5f5f5"))

    if (input$plot2_fixy) {
      p <- p + ylim(0, 1)
    }

    return(p)
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
