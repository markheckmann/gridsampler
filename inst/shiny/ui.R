#### Shiny UI definition ####

shinyUI(navbarPage(title = "gridsampler", inverse = T,
  #### Main tab ####
    tabPanel("Simulate", icon = icon("tasks"),
      wellPanel(
      fluidPage(
        #### Column 1 ####
        column(3,
              h3("1. Number of Attributes per Person"),
              helpText("This is a helpful description of this column with adequate amounts of information"),
              fluidRow(
                column(6, numericInput("minimum1", "Minimum",
                                       value = 4, min = 1, max = 100, step = 1, width = "100%")),
                column(6, numericInput("maximum1", "Maximum",
                                       value = 8, min = 1, max = 100, step = 1, width = "100%"))
                # column(12, sliderInput("minmax1", "Minimum & Maximum",
                #                        min = 1, max = 100, step = 1, value = c(1, 20)))
              ),
              plotOutput("plot1", height = "350px"),
              checkboxInput("plot1_fixy", "Fix y to [0, 1]", value = F),
              fluidRow(
                column(6, numericInput("attribute_num", "No. of Attributes",
                                    value = 1, min = 1, max = 100, step = 1, width = "100%")
                ),
                column(6, numericInput("probability1", "Probability",
                                    value = 0.05, min = 0.001, max = 1, step = 0.01, width = "100%")
              )),
              h4("Probability Presets"),
              fluidRow(
                column(6, selectInput("preset_types1", "Type of Probability",
                             choices = c("Uniform", "Normal", "Poisson", "Exponential"),
                             selected = "Normal", selectize = F),
                       actionButton("preset_go1", "Apply Preset", width = "100%")),
                column(6,tags$br(),
                       conditionalPanel("input.preset_types1 == 'Normal'",
                                        fluidRow(
                                        column(6, numericInput("1_norm_mean", "Mean", value = 0)),
                                        column(6, numericInput("1_norm_sd", "SD", value = 1)
                                        ))),
                       conditionalPanel("input.preset_types1 == 'Poisson'",
                                        numericInput("1_pois_lambda", "Lamda", value = 6)
                                        ),
                       conditionalPanel("input.preset_types1 == 'Exponential'",
                                       numericInput("1_exp_rate", "Rate", value = 0.1)
                                        )
                       )
              )
        #  ))
        ),
        #### Column 2 ####
        column(3,
               h3("2. Probability of Each Category"),
               helpText("This is an amusing anecdote about the most dazzling pair of trousers"),
               fluidRow(
                 column(6, numericInput("minimum2", "Minimum", value = 4, min = 1, step = 1, width = "100%")),
                 column(6, numericInput("maximum2", "Maximum", value = 8, min = 1, step = 1, width = "100%"))
               ),
               plotOutput("plot2", height = "350px"),
               checkboxInput("plot2_fixy", "Fix y to [0, 1]", value = F),
               fluidRow(
                 column(6, numericInput("category", "Category",
                                        value = 1, min = 1, max = 100, step = 1, width = "100%")),
                 column(6, numericInput("probability2", "Probability",
                                       value = 0.11, min = 0.001, max = 1, step = 0.01, width = "100%"))
                ),
               h4("Probability Presets"),
               fluidRow(
               column(6, selectInput("preset_types2", "Type of Probability",
                                     choices = c("Uniform", "Normal", "Poisson", "Exponential"),
                                     selected = "Normal", selectize = F),
                      actionButton("preset_go2", "Apply Preset", width = "100%")),
               column(6,tags$br(),
                      conditionalPanel("input.preset_types2 == 'Normal'",
                                       fluidRow(
                                         column(6, numericInput("2_norm_mean", "Mean", value = 0)),
                                         column(6, numericInput("2_norm_sd", "SD", value = 1)
                                         ))),
                      conditionalPanel("input.preset_types2 == 'Poisson'",
                                       numericInput("2_pois_lambda", "Lamda", value = 6)
                      ),
                      conditionalPanel("input.preset_types1 == 'Exponential'",
                                       numericInput("2_exp_rate", "Rate", value = 0.1)
                      )
               )
        )
        ),
        #### Column 3 ####
        column(6,
               h3("3. Simulate"),
               helpText("This is a note about how things happen here"),
               fluidRow(
                 column(6, numericInput("sample_size", "Sample Size", value = "10")),
                 column(6, numericInput("run_times", "Run N times", value = "10"))
               ),
               fluidRow(
                 column(6, actionButton("sample_random", "Random Sample", width = "100%")),
                 column(6, actionButton("run_button", "Run", width = "100%"))
               ),
               tags$br(),
               plotOutput("plot3_1", height = "250px"),
               tags$br(),
               fluidRow(
                 column(6, textInput("sample_size2", "Sample Size (N)", placeholder = "10, 20, 30")),
                 column(6, numericInput("runs_per_sample", "Simulation Runs per Sample", value = 100, step = 1))
               ),
               fluidRow(
                 column(6, textInput("mincount_m", "Minimum Count (M)", placeholder = "4, 5, 6")),
                 column(6, textInput("proportion_k", "Proportion (K)", placeholder = "0.9, 0.95, 1"))
               ),
               fluidRow(
                      column(6, actionButton("simulate", "Simulate", width = "100%")),
                      column(6, actionButton("redraw", "Redraw", width = "100%"))
               ),
               tags$br(),
               plotOutput("plot3_2", height = "250px"),
               tags$br()
        )
        #### End of column 3 ####
      ))
    ),

  #### About tab ####
  tabPanel("About", icon = icon("question-circle"),
           fluidPage(
             wellPanel("This is just a placeholder for the actual about section"),
             #includeHTML(system.file("doc", "index.html", package = "gridsampler")) # messes up layout
             includeMarkdown("text/about.md")
           )
  )
))
