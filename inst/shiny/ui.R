#### Shiny UI definition ####

shinyUI(
  navbarPage(title = "gridsampler", inverse = T,
  #### Main tab ####
    tabPanel("Simulate", icon = icon("tasks"),
      wellPanel(
      fluidPage(
        #### Column 1 ####
        column(4,
          fluidRow(
              h2("1. Number of Attributes per Person"),
              column(6,
                numericInput("minimum", "Minimum", value = 4, min = 1, max = 100, step = 1, width = "100%")
              ),
              column(6,
                numericInput("maximum", "Maximum", value = 8, min = 1, max = 100, step = 1, width = "100%")
              )
          ),
              plotOutput("plot1"),
          fluidRow(
              column(6,
                     numericInput("attribute_num", "No. of Attributes",
                                  value = 1, min = 1, max = 100, step = 1, width = "100%")
              ),
              column(6,
                     numericInput("probability", "Probability",
                                  value = 0.05, min = 0.001, max = 1, step = 0.01, width = "100%")
              ),
              h3("Probability Presets"),
              fluidRow(
                inputPanel(
                  column(12,
                    actionButton("1_uni", "Uniform", width = "100px")
                  )
                ),
                tags$br(),
                inputPanel(
                  fluidRow(
                  column(6, actionButton("1_norm", "Normal", width = "100px")),
                  column(6,
                         numericInput("1_norm_mean", "Mean", value = 0),
                         numericInput("1_norm_sd", "SD", value = 1))
                  )
                ),
                tags$br(),
                inputPanel(
                  column(6, tags$br(), actionButton("1_pois", "Poisson", width = "100px")),
                  column(6, numericInput("1_pois_lambda", "Lamda", value = 6))
                ),
                tags$br(),
                inputPanel(
                  column(6, tags$br(), actionButton("1_exp", "Exponential", width = "100px")),
                  column(6, numericInput("1_exp_rate", "Rate", value = 0.1))
                )
              )
            )
        ),
        #### Column 2 ####
        column(4,
               h2("2. Probability of Each Category")
               ),
        #### Column 3 ####
        column(4,
               h2("3. Simulate")
               )
      ))
    ),
  #### About tab ####
  tabPanel("About", icon = icon("question-circle"))
  )
)
