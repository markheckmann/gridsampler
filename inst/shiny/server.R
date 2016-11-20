source("global.R")

#### Shiny server file ####
shinyServer(function(input, output, session) {

  #### Logic for column 1 ####

  # Input validation
  observeEvent(input$minimum1, priority = 1, {
    validate(need(!is.na(input$minimum1), "Value must be set!"))
    if (is.na(input$minimum1)) {
      updateNumericInput(session, "minimum1", value = 1)
    }
  })

  # Updating input elements
  observe({
    validate(need(!is.na(input$maximum1), "Value must be set!"))

    # Make sure maximum is not smaller than minumum
    if (!is.na(input$minimum1) & !is.na(input$maximum1)) {
      if (input$maximum1 < input$minimum1) {
        updateNumericInput(session, "maximum1", value = input$maximum1 + 1, max = input$maximum1 + 1)
      }
    }

    # Confine manual selection to limits set before
    updateNumericInput(session, "attribute_num", min = input$minimum1, max = input$maximum1)

    # Set attribute selection input to a value guaranteed in range of set limits (minimum1, maximum1)
    if (!is.na(input$minimum1) & !is.na(input$maximum1)) {
      if (input$attribute_num < input$minimum1 | input$attribute_num > input$maximum1) {
        updateNumericInput(session, "attribute_num", value = input$minimum1)
      }
    }

    # Prevent bug #12 https://github.com/markheckmann/gridsampler/issues/12
    if (!is.na(input$maximum2)) {
      if (length(seq_robust(input$minimum1, input$maximum1)) > input$maximum2) {
        updateNumericInput(session, "maximum1", value = input$maximum1 - 1)
      }
    }

    # Prevent minimum for linear probability distribution to be set to 0, wich would trigger a crash
    # during simulation attempts
    if (!is.na(input$`2_lin_min`)) {
      if (input$`2_lin_min` == 0) {
        updateNumericInput(session, "2_lin_min", value = 0.001)
      }
    }
  })

  # Make sure the probability in column 1 is the current value stored in the reactiveValues object
  observeEvent(input$attribute_num, {
    updateNumericInput(session, "probability1",
                       value = round(values$attributes_prob[values$attributes_id == input$attribute_num], 3))
  })

  # Observer to change attribute properties
  observe({
    # Change number of arributes
    values$attributes_id <- seq_robust(input$minimum1, input$maximum1)

    # If button isn't pressed yet, insert default values
    # Makes sure that the plot shows the correct number of attributes
    if (input$preset_go1 == 0) {
      if (length(values$attributes_id) != length(values$attributes_prob)) {
        values$attributes_prob <- round(dnorm(values$attributes_id,
                                        mean = default_attributes_norm_mean,
                                        sd = default_attributes_norm_sd), 3)
      }
    }
  })

  # Observer for manual probability adjustments (col 1)
  observeEvent(input$probability1, {
    # Prevent probability inputs from going above 1
    if (!is.na(input$probability1)) {
      if (isolate(input$probability1) > 1) {
        updateNumericInput(session, "probability1", value = 1)
      }
    }

    input_prob <- round(input$probability1, 3)

    # Change selected probability
    values$attributes_prob[values$attributes_id == input$attribute_num] <- input_prob

    # Scale probabilities back to 1
    # Different approach: scale non-selected probs to 1 - (selected prob)
    if (round(sum(values$attributes_prob), 4) != 1) {
      other_probs <- values$attributes_prob[values$attributes_id != input$attribute_num]
      other_probs <- other_probs / (sum(other_probs)/(1 - input_prob))

      values$attributes_prob[values$attributes_id != input$attribute_num] <- other_probs
    }
  })

  # Observer for presets in column 1
  observeEvent(input$preset_go1, {
    # Apply presets only if button is pressed
    if (input$preset_go1 != 0) {
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

    # Adjust scaling
    values$attributes_prob <- values$attributes_prob/sum(values$attributes_prob)

    # Update manual probability input
    updateNumericInput(session, "probability1",
                       value = round(values$attributes_prob[values$attributes_id == input$attribute_num], 3))
  })

  # Plot in column 1
  output$plot1 <- renderPlot({

    # Create data.frame from values for use with ggplot2
    data <- data.frame(x = values$attributes_id,
                       y = values$attributes_prob[seq_along(values$attributes_id)],
                       mark = rep("No", length(values$attributes_id)),
                       stringsAsFactors = F)

    # Mark the currently selected element
    data$mark[data$x == input$attribute_num] <- "Yes"

    p <- ggplot(data = data, aes(x = x, weight = y, fill = mark)) +
          geom_bar(width = .1) +
          geom_text(aes(y = y, label = prettify_probs(y, 3)), nudge_y = .02) +
          scale_fill_manual(values = c(No = "black", Yes = "red2"), guide = F) +
          labs(x = "Attribute", y = "Probability") +
          scale_x_continuous(breaks = seq(1, 1000, 1)) +
          theme_bw() +
          theme(plot.background  = element_rect(fill = plot_bg),
                panel.background = element_rect(fill = panel_bg))

    # Vertical adjustment if selected
    if (input$plot1_fixy) {
      p <- p + ylim(0, 1)
    }

    return(p)
  })

  #### Logic for column 2 ####

  # Updating input elements
  observe({
    validate(need(!is.na(input$maximum2), "Value must be set!"))
    if (is.na(input$maximum2)) {
      updateNumericInput(session, "maximum2", value = 15)
    }

    # Confine manual selection to limits set before
    updateNumericInput(session, "category", min = 1, max = input$maximum2)

    if (!is.na(input$maximum2)) {
      if (input$category > input$maximum2) {
        updateNumericInput(session, "category", value = input$category - 1)
      }
    }
  })

  # Make sure the probability in column 2 is the current value stored in the reactiveValues object
  observeEvent(input$category, {
    updateNumericInput(session, "probability2",
                       value = round(values$category_prob[values$category_id == input$category], 3))
  })

  # Observer to change category properties
  observe({
    # Change number of attributes
    values$category_id <- seq_len_robust(input$maximum2)

    # Make sure enough probability values are supplied in default state
    if (input$preset_go2 == 0) {
      if (!is.na(input$maximum2)) {
        if (input$maximum2 != length(values$category_prob)) {
          values$category_prob <- round(dexp(values$category_id,
                                             rate = default_category_exp_rate), 3)
        }
      }
    }
  })

  # Observer for manual probability adjustments (col 2)
  observeEvent(input$probability2, {
    if (!is.na(input$probability2)) {
      if (isolate(input$probability2) > 1) {
        updateNumericInput(session, "probability2", value = 1)
      }
    }

    input_prob <- round(input$probability2, 3)

    # Change selected probability
    values$category_prob[values$category_id == input$category] <- input_prob

    # Scale probs back to 1
    # Different approach: scale non-selected probs to 1 - (selected prob)
    if (round(sum(values$category_prob), 4) != 1) {
      other_probs <- values$category_prob[values$category_id != input$category]
      other_probs <- other_probs / (sum(other_probs)/(1 - input_prob))

      values$category_prob[values$category_id != input$category] <- other_probs
    }
  })

  # Observer for presets in column 2
  observeEvent(input$preset_go2, {
    # Apply presets only if button is pressed
    if (input$preset_types2 == "Exponential") {
      values$category_prob <- dexp(values$category_id, rate = input$`2_exp_rate`)
    } else if (input$preset_types2 == "Uniform") {
      values$category_prob <- dunif(values$category_id, min = 1 - 1, max = input$maximum2)
    } else if (input$preset_types2 == "Linear") {
      values$category_prob <- p_linear(k = length(values$category_id), p_k = input$`2_lin_min`)
    }

    # Adjust scaling
    values$category_prob <- values$category_prob/sum(values$category_prob)

    # Update manual probability input field
    updateNumericInput(session, "probability2",
                       value = round(values$category_prob[values$category_id == input$category], 3))
  })

  # Plot for column 2
  output$plot2 <- renderPlot({

    # Inititate data.frame from reactive values
    data2 <- data.frame(x = values$category_id,
                       y = values$category_prob[seq_along(values$category_id)],
                       mark = rep("No", length(values$category_id)),
                       stringsAsFactors = F)

    # Mark selected element
    data2$mark[data2$x == input$category] <- "Yes"

    # x axis labels depending on number of categories, should be tweaked for real-life use cases
    if (!is.na(input$maximum2)) {
      if (input$maximum2 <= 10) {
        x_breaks <- seq(1, 10, 1)
      } else if (input$maximum2 > 10 & input$maximum2 <= 20) {
        x_breaks <- seq(1, 1000, 2)
      } else {
        x_breaks <- seq(1, 1000, 5)
      }
    } else {
      x_breaks <- seq(1, 10, 1)
    }

    p <- ggplot(data = data2, aes(x = x, weight = y, fill = mark)) +
          geom_bar(width = .1) +
          geom_text(aes(y = y, label = prettify_probs(y, 3)), nudge_y = .005, angle = 40) +
          scale_fill_manual(values = c(No = "black", Yes = "red2"), guide = F) +
          labs(x = "Categories", y = "Probability") +
          scale_x_continuous(breaks = x_breaks) +
          expand_limits(y = max(data2$y + 0.01),
                        x = max(data2$x + 0.9)) +
          theme_bw() +
          theme(plot.background  = element_rect(fill = plot_bg),
                panel.background = element_rect(fill = panel_bg))

    # Adjust y scale if selected
    if (input$plot2_fixy) {
      p <- p + ylim(0, 1)
    }

    return(p)
  })

  #### Logic for column 3 ####

  # Executes chunk if sample_random button is pushed, stores samples in "values" reactiveValues object
  observeEvent(input$sample_random, {
      # Input validation
      n <- isolate(input$sample_size)
      validate(need(is.numeric(n) & length(n) > 0, all(n > 0), message = "Value (N) must be set!"))
      if (n < 0) {n <- abs(n)}

      # Create sample, store in values
      values$sample_plot <- gridsampler::draw_n_person_sample(prob = isolate(values$category_prob),
                                                              n = n,
                                                              a = isolate(values$attributes_id),
                                                              ap = isolate(values$attributes_prob)) +
                              theme_bw() +
                              theme(plot.background  = element_rect(fill = plot_bg),
                                    panel.background = element_rect(fill = panel_bg))
  })

  # Run samples if run_button is pressed
  observeEvent(input$run_button, {

    # Initiate progress bar
    withProgress(value = 0, message = "Running samples...", {

      # Verbatim copy of sim_n_persons_x_times to incorporate progress bar
      # Runs the samples used in further steps
      times <- isolate(input$run_times)
      validate(need(is.numeric(times) & length(times) > 0, all(times > 0), message = "Value (R) must be set!"))
      if (times < 0) {times <- abs(times)}

      r <- plyr::ldply(seq_len_robust(times), function(x){
                                  if (x%%10 == 0) {
                                    # Only update progress bar every 10 iterations to reudce lag
                                    setProgress(session = session, value = x/times,
                                                detail = paste0(x, "/", times))
                                  }

                                  samples <- sim_n_persons(prob = isolate(values$category_prob),
                                                           n = isolate(input$sample_size),
                                                           a = isolate(values$attributes_id),
                                                           ap = isolate(values$attributes_prob))
                                  return(samples)
                                  }, .progress = "none") # No progress bar to R console
    })

    # Create plot from samples, store in values
    values$sample_plot <- expected_frequencies(r) +
                            theme_bw() +
                            theme(plot.background   = element_rect(fill = plot_bg),
                                  panel.background  = element_rect(fill = panel_bg),
                                  legend.background = element_rect(fill = legend_bg))
  })

  # Plot 1 of column 3
  output$plot3_1 <- renderPlot({

    if (input$sample_random == 0 & input$run_button == 0) {
      # Show placeholder plot if no button was pressed yet...
      p_31
    } else {
      # ...otherwise show stored plot from above
      values$sample_plot
    }
  })

  #### Bottom half of column 3 ####
  observeEvent(input$simulate, {

    # Initiate progress bar
    withProgress(message = "Running simulations...", {

      # This is basically a verbatim copy of sim_n_persons_x_times_many_n
      # I extracted it here because for reasons I don't understand it didn't work otherwise
      r <- list()
      n <- text_to_vector(isolate(input$sample_size2))
      # Insert default values in n is nonsense
      if (length(n) == 1){
        if (n == 0) {
          n <- seq(10, 80, 10)
        }
      }

      for (i in seq_along(n)) {
        # Increment progress bar
        incProgress(amount = 1/length(n), detail = paste0(i, "/", length(n)))
        # Do simulation
        r[[i]] <- sim_n_persons_x_times(prob  = values$category_prob,
                                        n     = n[i],
                                        a     = values$attributes_id,
                                        ap    = values$attributes_prob,
                                        times = isolate(input$runs_per_sample),
                                        progress = "none")
      }

      # Store result in values for use in next chunk
      values$simulations <- r
    })
  })

  # Plot 2 of column 3
  output$plot3_2 <- renderPlot({
    # Depend upon redraw button, executes this chunk everytime button is pressed
    input$redraw

    # Don't even attempt to draw a plot if the "simulate" button wasn't pushed
    if (input$redraw == 0 & is.null(values$simulations)) {
      return(NULL)
    }

    # See ?draw_multiple_n_persons_x_times
    # Use isolate() to avoid unwanted re-execution on input change
    N <- text_to_vector(isolate(input$sample_size2))
    # Insert default values in n is nonsense
    if (length(N) == 1){
      if (N == 0) {
        N <- seq(10, 80, 10)
      }
    }

    M <- text_to_vector(isolate(input$mincount_m))
    p <- text_to_vector(isolate(input$proportion_k))

    # Input validation
    validate(need(is.numeric(N) & length(N) > 0, all(N > 0), message = "Value (N) must be set and parseable as valid R!"))
    validate(need(is.numeric(M) & length(M) > 0, all(M > 0), message = "Value (M) must be set and parseable as valid R!"))
    validate(need(is.numeric(p) & length(p) > 0, all(p > 0), message = "Value (C) must be set and parseable as valid R!"))

    # Calculating probabilities & drawing plot
    d <- calc_probabilities(r = values$simulations, n = N, ms = M, min.props = p)

    # Drawing the final plot
    plot_result <- draw_multiple_n_persons_x_times(d) +
                    theme_bw() +
                    theme(plot.background   = element_rect(fill = plot_bg),
                          panel.background  = element_rect(fill = panel_bg),
                          legend.background =  element_rect(fill = legend_bg),
                          legend.key        = element_rect(fill = legend_bg),
                          strip.background  = element_blank(),
                          strip.text        = element_text(size = rel(1.2)),
                          legend.text       = element_text(size = rel(1.1)))
    return(plot_result)
  })
})
