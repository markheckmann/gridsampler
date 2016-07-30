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

  })

  # Observer to change attribute properties
  observe({
    # Change number of arributes
    values$attributes_id <- seq(input$minimum1, input$maximum1, by = 1)

    if (input$preset_go1 == 0){
      if (length(values$attributes_id) != length(values$attributes_prob)){
        values$attributes_prob <- dnorm(values$attributes_id, mean = 6, sd = 1)
      }
    }
  })

  # Observer for manual probability adjustments (col 1)
  # TODO: Perform check so that sum of probability approximates 1
  observeEvent(input$probability1, {
    values$attributes_prob[values$attributes_id == input$attribute_num] <- input$probability1
  })

  # Observer for presets in column 1
  observeEvent(input$preset_go1, {
    # Apply presets only if button is pressed
    if (input$preset_go1 != 0){
      cat("pressed preset_go1 \n")
      if (input$preset_types1 == "Normal") {
        values$attributes_prob <- dnorm(values$attributes_id, mean = input$`1_norm_mean`, sd = input$`1_norm_sd`)
      } else if (input$preset_types1 == "Poisson") {
        values$attributes_prob <- dpois(values$attributes_id, lambda = input$`1_pois_lambda`)
      } else if (input$preset_types1 == "Exponential") {
        values$attributes_prob <- dexp(values$attributes_id, rate = input$`1_exp_rate`)
      } else if (input$preset_types1 == "Uniform") {
        values$attributes_prob <- dunif(values$attributes_id, min = input$minimum1 - 1, max = input$maximum1)
      }
    }
  })

  # Plot in column 1
  output$plot1 <- renderPlot({

    data <- data.frame(x = values$attributes_id,
                       y = values$attributes_prob,
                       mark = rep("No", length(values$attributes_id)))

    data$mark[data$x == input$attribute_num] <- "Yes"

    p <- ggplot(data = data, aes(x = x, weight = y, fill = mark)) +
          geom_bar(color = "black") +
          scale_fill_manual(values = c(No = "black", Yes = "red2"), guide = F) +
          labs(x = "Attribute", y = "Probability") +
          scale_x_continuous(breaks = seq(1, 1000, 1)) +
          theme_bw() +
          theme(plot.background = element_rect(fill = "#f5f5f5"),
                panel.background = element_rect(fill = "#f5f5f5"))

    if (input$plot1_fixy) {
      p <- p + ylim(0, 1)
    }

    return(p)

  })

  #### Logic for column 2 ####

  # Updating input elements
  observe({
    validate(need(!is.na(input$maximum2), "Value must be set!"))

    updateNumericInput(session, "category", min = 1, max = input$maximum2)

    if (input$category > input$maximum2) {
      updateNumericInput(session, "category", value = 1)
    }
  })

  # Observer to change category properties
  observe({
    # Change number of arributes
    values$category_id <- seq(1, input$maximum2, by = 1)

    if (input$preset_go2 == 0){
      if (length(values$category_id) != length(values$category_prob)){
        values$category_prob <- dexp(values$category_id, rate = 0.01)
      }
    }
  })

  # Observer for manual probability adjustments (col 2)
  # TODO: Perform check so that sum of probability approximates 1
  observeEvent(input$probability2, {
    values$category_prob[values$category_id == input$category] <- input$probability2
  })

  # Observer for presets in column 1
  observeEvent(input$preset_go2, {
    # Apply presets only if button is pressed
    if (input$preset_types2 == "Normal") {
      values$category_prob <- dnorm(values$category_id, mean = input$`2_norm_mean`, sd = input$`2_norm_sd`)
    } else if (input$preset_types2 == "Poisson") {
      values$category_prob <- dpois(values$category_id, lambda = input$`2_pois_lambda`)
    } else if (input$preset_types2 == "Exponential") {
      values$category_prob <- dexp(values$category_id, rate = input$`2_exp_rate`)
    } else if (input$preset_types2 == "Uniform") {
      values$category_prob <- dunif(values$category_id, min = 1 - 1, max = input$maximum2)
    }
  })

  # Plot for column 2
  output$plot2 <- renderPlot({
    data <- data.frame(x = values$category_id,
                       y = values$category_prob,
                       mark = rep("No", length(values$category_id)))

    data$mark[data$x == input$category] <- "Yes"

    p <- ggplot(data = data, aes(x = x, weight = y, fill = mark)) +
          geom_bar(color = "black") +
          scale_fill_manual(values = c(No = "black", Yes = "red2"), guide = F) +
          labs(x = "Categories", y = "Probability") +
          scale_x_continuous(breaks = seq(1, 1000, 1)) +
          theme_bw() +
          theme(plot.background = element_rect(fill = "#f5f5f5"),
                panel.background = element_rect(fill = "#f5f5f5"))

    if (input$plot2_fixy) {
      p <- p + ylim(0, 1)
    }

    return(p)
  })

  #### Logic for column 3 ####

  # ## col 3 1:
  #
  # - 2 plots
  # random sample `sim_1_persons`: `draw_n_person_sample`
  # run: `expected_frequencies()``
  #
  # ## col 3 2
  #
  # - simulate:
  #   -  in gui versteckt: `handler_btn_redraw`: ggplot
  # -


  output$plot3_1 <- renderPlot({
   ggplot(data = data.frame(x = 1:10, y = (1:10)^-1), aes(x = x, y = y)) +
     geom_point()
  })

  output$plot3_2 <- renderPlot({
   ggplot(data = data.frame(x = 1:10, y = (1:10)^2), aes(x = x, y = y)) +
     geom_point()
  })
})
