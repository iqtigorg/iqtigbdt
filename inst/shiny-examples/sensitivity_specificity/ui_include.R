ui <- shinyUI(fluidPage(

  # Application title
  titlePanel("Sensitivity and specificity for a binary quality indicator as a function of the number of cases J"),
  br(),
  fluidRow(
    column(
      2,
      numericInput("ref_value", "Reference value", min = 0, max = 1, value = 0.15, step = 0.01),
      numericInput("theta", "True value \u03B8", min = 0, max = 1, value = 0.15, step = 0.01),
      numericInput("alpha", "Critical value for the classification as outlier (\u03B1)", min = 0, max = 1, value = 0.05, step = 0.01),
      numericInput("omega", "Critical value for the classification as outlier (\u03C9)", min = 0, max = Inf, value = 2, step = .1),
      numericInput("prior_a", "Parameter of the priori distribution Beta(a,b): a",
        min = 0, max = Inf, value = .5, step = .1
      ),
      numericInput("prior_b", "Parameter of the priori distribution Beta(a,b): b",
        min = 0, max = Inf, value = .5, step = .1
      )
    ),
    column(9,
      align = "center",
      plotOutput("plot", height = "650px", width = "1200px")
    )
  )
))
