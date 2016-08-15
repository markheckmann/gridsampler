#### Shiny UI definition ####


#------ Intro Tour using boostrap-tour library (http://bootstraptour.com/)  ------#

# Add a tour to GUI to explain the panels and basis steps.
# The tour is defined in www/tour.js
#
header <- list(tags$script(src="bootstrap-tour-0.10.3/js/bootstrap-tour.min.js"),
               tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
               tags$script("var a = $('#ourNavbar  a[data-value=\"Tour\"]');
                                    a.attr('data-toggle', 'noclass');
                                    a.click(function() {
                                      tour.restart();
                                      console.log('Tour started');
                                    });") )

footer <- tags$script(src="tour.js")  # add tour

#------ End Intro Tour components  ------#


shinyUI(navbarPage(title = "gridsampler shiny dev-1.2",
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
              h3("1. Number of Attributes per Person"),
              helpText("This is a helpful description of this column with adequate amounts of information"),
              fluidRow(
                column(6, numericInput("minimum1", "Minimum",
                                       value = 4, min = 1, max = 100, step = 1, width = "100%")),
                column(6, numericInput("maximum1", "Maximum",
                                       value = 8, min = 1, max = 100, step = 1, width = "100%"))
              ),
              plotOutput("plot1", height = "350px"),
              checkboxInput("plot1_fixy", "Fix y to [0, 1]", value = F),
              fluidRow(
                column(6, numericInput("attribute_num", "No. of Attributes",
                                    value = 4, min = 4, max = 8, step = 1, width = "100%")
                ),
                column(6, numericInput("probability1", "Probability",
                                    value = 0.05399097, min = 0.001, max = 1, step = 0.01, width = "100%")
              )),
              h4("Probability Presets"),
              fluidRow(
                column(6, selectInput("preset_types1", "Type of Probability",
                             choices = c("Uniform", "Normal", "Poisson", "Exponential"),
                             selected = "Normal", selectize = F),
                       actionButton("preset_go1", "Apply Preset", width = "100%")),
                column(6, tags$br(),
                       conditionalPanel("input.preset_types1 == 'Normal'",
                                        fluidRow(
                                        column(6, numericInput("1_norm_mean", "Mean", value = 6)),
                                        column(6, numericInput("1_norm_sd", "SD", value = 1, min = 0.01, step = 0.1)
                                        ))),
                       conditionalPanel("input.preset_types1 == 'Poisson'",
                                        numericInput("1_pois_lambda", "Lamda", value = 6)
                                        ),
                       conditionalPanel("input.preset_types1 == 'Exponential'",
                                       numericInput("1_exp_rate", "Rate", value = 0.1, step = 0.1)
                                        ),
                       conditionalPanel("input.preset_types1 == 'Manual'",
                                        textInput("1_manual", label = "Vector of propabilities", placeholder = "0.5, 0.25, 0.25"))
                       )
              )
        ),

        #### Column 2 ####
        column(4,
               h3("2. Probability of Each Category"),
               helpText("This is an amusing anecdote about the most dazzling pair of trousers"),
               fluidRow(
                 column(12, numericInput("maximum2", "No. of Categories", value = 15, min = 1, step = 1, width = "100%"))
               ),
               plotOutput("plot2", height = "350px"),
               checkboxInput("plot2_fixy", "Fix y to [0, 1]", value = F),
               fluidRow(
                 column(6, numericInput("category", "Category",
                                        value = 5, min = 1, max = 100, step = 1, width = "100%")),
                 column(6, numericInput("probability2", "Probability",
                                       value = 0.07085498, min = 0, max = 1, step = 0.001, width = "100%"))
                ),
               h4("Probability Presets"),
               fluidRow(
               column(6, selectInput("preset_types2", "Type of Probability",
                                     choices = c("Uniform", "Normal", "Poisson", "Exponential"),
                                     selected = "Exponential", selectize = F),
                      actionButton("preset_go2", "Apply Preset", width = "100%")),
               column(6, tags$br(),
                      conditionalPanel("input.preset_types2 == 'Normal'",
                                       fluidRow(
                                         column(6, numericInput("2_norm_mean", "Mean", value = 5)),
                                         column(6, numericInput("2_norm_sd", "SD", value = 1, min = 0.01, step = 0.1)
                                         ))),
                      conditionalPanel("input.preset_types2 == 'Poisson'",
                                       numericInput("2_pois_lambda", "Lamda", value = 6)
                      ),
                      conditionalPanel("input.preset_types2 == 'Exponential'",
                                       numericInput("2_exp_rate", "Rate", value = 0.15, step = 0.01)
                      )
               )
        )
        ),

         #### Column 3 ####
        column(5,
               h3("3. Simulate"),
               helpText("This is a note about how things happen here"),
               fluidRow(
                 column(6, numericInput("sample_size", "Sample Size", value = "100")),
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
                 column(6, textInput("sample_size2", "Sample Size (N)", value = "10, 20, 30, 40, 50, 60, 70, 80")),
                 column(6, numericInput("runs_per_sample", "Simulation Runs per Sample", value = 100, step = 1))
               ),
               fluidRow(
                 column(6, textInput("mincount_m", "Minimum Count (M)", value = "4, 5, 6")),
                 column(6, textInput("proportion_k", "Proportion (K)", value = "0.9, 0.95, 1"))
               ),
               fluidRow(
                      column(6, actionButton("simulate", "Simulate", width = "100%")),
                      column(6, actionButton("redraw", "Redraw", width = "100%"))
               ),
               tags$br(),
               conditionalPanel("input.simulate == 0", tags$span(class = "help-block", "No simulations run yet!",
                                                                 tags$br(),
                                                                 "A plot will appear here after you press “simulate“.")),
               conditionalPanel("input.simulate > 0", plotOutput("plot3_2", height = "250px")),
               tags$br()
        )
        #### End of column 3 ####
      ))
    ),

  #### About tab ####
  tabPanel("About", icon = icon("question-circle"),
    fluidPage(
      fluidRow(
        column(10, offset = 1,
          wellPanel("This is just a placeholder for the final about section"),
          #includeHTML(system.file("doc", "index.html", package = "gridsampler")) # messes up layout
          includeHTML("text/index.html")
          #includeMarkdown("text/about.md")
        )
      )
    )
  ),
  tabPanel("Tour", icon = icon("question-circle")  # trigger intro tour
  )
))
