#### Shiny UI definition ####

#### Intro Tour using boostrap-tour library (http://bootstraptour.com/)  ####

# Add a tour to GUI to explain the panels and basis steps.
# The tour is defined in www/tour.js

header <- list(tags$script(src = "bootstrap-tour-0.10.3/js/bootstrap-tour.min.js"),
               tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
               tags$script("var a = $('#ourNavbar  a[data-value=\"Tour\"]');
                                    a.attr('data-toggle', 'noclass');
                                    a.click(function() {
                                      tour.restart();
                                      console.log('Tour started');
                                    });") )

footer <- tags$script(src = "tour.js")  # add tour

#### End Intro Tour components, begin shinyUI  ####

shinyUI(navbarPage(title = gridsampler_version,
                   id = "ourNavbar",
                   inverse = T,
                   theme = shinytheme("flatly"),
                   header = header,
                   footer = footer,

  #### Main tab ####
    tabPanel("Simulate", icon = icon("tasks"),
      wellPanel(
      fluidPage(
        #### Column 1 ####
        column(3,
              h3("1. Number of Attributes"),
              desc_col1,
              # Attribute min / max selection
              fluidRow(
                column(6, numericInput("minimum1", "Minimum",
                                       value = default_attributes_min,
                                       min = 1, max = 100, step = 1, width = "100%")),
                column(6, numericInput("maximum1", "Maximum",
                                       value = default_attributes_max
                                       , min = 1, max = 100, step = 1, width = "100%"))
              ),
              plotOutput("plot1", height = "300px"),
              checkboxInput("plot1_fixy", "Fix y to [0, 1]", value = F),
              # Manual item probability adjustment
              fluidRow(
                column(6, numericInput("attribute_num", "No. of Attributes",
                                    value = default_attributes_min,
                                    min = default_attributes_min, max = default_attributes_max, step = 1, width = "100%")
                ),
                column(6, numericInput("probability1", "Probability",
                                    value = round(default_attributes_probs[1], 3),
                                    min = 0.001, max = 1, step = 0.001, width = "100%")
              )),
              # Preset section of column 1
              wellPanel(
                h4("Probability Presets"),
                fluidRow(
                  # Selection of probability types in column 1
                  column(6, selectInput("preset_types1", "Type",
                               choices = c("Uniform", "Normal", "Poisson", "Exponential"),
                               selected = "Normal", selectize = F)
                         ),
                  column(6,
                         # Show preset arguments depending on distribution selection
                         conditionalPanel("input.preset_types1 == 'Normal'",
                                          numericInput("1_norm_mean", "Mean",
                                                       value = default_attributes_norm_mean,
                                                       step = .1),
                                          numericInput("1_norm_sd", "SD",
                                                       value = default_attributes_norm_sd,
                                                       min = 0.01, step = 0.01)
                                          ),
                         conditionalPanel("input.preset_types1 == 'Poisson'",
                                          numericInput("1_pois_lambda", "Lamda",
                                                       value = default_attributes_lambda)
                                          ),
                         conditionalPanel("input.preset_types1 == 'Exponential'",
                                          numericInput("1_exp_rate", "Rate",
                                                       value = default_attributes_exp_rate,
                                                       step = 0.01)
                                          )
                         )
                ),
                fluidRow(
                  # Action button in column 1
                  column(12, actionButton("preset_go1", "Apply Preset", width = "100%"))
                )
              ) # wellPanel ends here
        ),

        #### Column 2 ####
        column(3,
               h3("2. Probability of Categories"),
               desc_col2,
               # Panel 2 top input controls
               fluidRow(
                 column(12, numericInput("maximum2", "No. of Categories",
                                         value = default_category_count,
                                         min = 1, step = 1, width = "100%"))
               ),
               # Panel 2 plot output
               plotOutput("plot2", height = "300px"),
               checkboxInput("plot2_fixy", "Fix y to [0, 1]", value = F),
               # Panel 2 manual prob adjustment
               fluidRow(
                 column(6, numericInput("category", "Category",
                                        value = 1, min = 1, max = 500,
                                        step = 1, width = "100%")),
                 column(6, numericInput("probability2", "Probability",
                                       value = round(default_category_probs[1], 3),
                                       min = 0, max = 1, step = 0.001, width = "100%"))
                ),
               # Preset section of panel 2
               wellPanel(
                 h4("Probability Presets"),
                 fluidRow(
                 # Selection of probability types in column 2
                 column(6, selectInput("preset_types2", "Type",
                                       choices = c("Uniform", "Exponential", "Linear"),
                                       selected = "Exponential", selectize = F)
                        ),
                 column(6,
                        # Show preset arguments depending on distribution selection
                        conditionalPanel("input.preset_types2 == 'Exponential'",
                                         numericInput("2_exp_rate", "Rate",
                                                      value = default_category_exp_rate,
                                                      step = 0.01)
                        ),
                        conditionalPanel("input.preset_types2 == 'Linear'",
                                         numericInput("2_lin_min", "Minimum",
                                                      value = default_category_lin_min,
                                                      min = 0.001, step = 0.001)
                        )
                 )
                 ),
                 fluidRow(
                   # Action button in column 2
                   column(12, actionButton("preset_go2", "Apply Preset", width = "100%"))
                 )
               ) # wellPanel ends here
         ),

         #### Column 3 ####
         column(6,
               h3("3. Simulation"),
               desc_col3,
               # UI controls in panel 3, top
               fluidRow(
                 column(6, numericInput("sample_size", "Sample Size (N)", value = "100")),
                 column(6, numericInput("run_times", "Simulation Runs (R)", value = "10"))
               ),
               fluidRow(
                 column(6, actionButton("sample_random", "Random Sample", width = "100%")),
                 column(6, actionButton("run_button", "Run", width = "100%"))
               ),
               tags$br(),
               # Plot in panel 1, top
               plotOutput("plot3_1", height = "250px"),
               tags$hr(),
               fluidRow(
                 # Panel 3 bottom input controls, left side
                 column(6,
                 wellPanel(
                   fluidRow(column(12, textInput("sample_size2", "Sample Size (N)",
                                                 value = "10, 20, 30, 40, 50, 60, 70, 80"))),
                   fluidRow(column(12, numericInput("runs_per_sample", "Simulation Runs (R)",
                                                    value = 100, step = 1))),
                   fluidRow(column(12, actionButton("simulate", "Simulate", width = "100%"))
                 ))),
                 # Panel 3 bottom input controls, right side
                 column(6,
                 wellPanel(
                   fluidRow(column(12, textInput("mincount_m", "Minimum Count (M)",
                                                 value = "4, 5, 6"))),
                   fluidRow(column(12, textInput("proportion_k", "Coverage (C)",
                                                 value = "0.9, 0.95, 1"))),
                   fluidRow(column(12, actionButton("redraw", "Redraw", width = "100%"))
                 )))
               ),
               tags$br(),
               # Only show plot if the simulate button has been pressed, show text otherwise
               conditionalPanel("input.simulate == 0",
                                tags$span(class = "help-block", "No simulations run yet!",
                                tags$br(),
                                "A plot will appear here after you press “Simulate“.")),
               conditionalPanel("input.simulate > 0", plotOutput("plot3_2", height = "300px")),
               tags$br()
         )
        #### End of column 3 ####
      ))
    ), # End of first tabPanel, "Simulate" tab

  #### About tab ####
  tabPanel("About", icon = icon("question-circle"),
    fluidPage(
      fluidRow(
        # Use a smaller column size for better readability
        column(10, offset = 1,
          includeHTML("text/index.html")
        )
      )
    )
  ),
  tabPanel("Tour", icon = icon("question-circle")  # trigger intro tour
  )
))
