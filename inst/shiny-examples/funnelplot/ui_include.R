ui <- shinyUI(fluidPage(
  titlePanel("Interactive funnelplot for a binary quality indicator"),
  br(),
  fluidRow(
    column(2,
      numericInput("ref_value", "Reference value", min = 0.00001,
                   max = 1, value = 0.15, step = 0.01),
      numericInput("max_n", "max. number of patients (x-axis)",
                   min = 0, max = 500, value = 100, step = 1),
      style = "background-color: lightgray;",
      numericInput("alpha", "Critical value for the classification as outlier (\u03B1)",
                   min = 0, max = 1, value = 0.05, step = 0.01),
      numericInput("omega", "Critical value for the classification as outlier (\u03C9)",
                   min = 0, max = Inf, value = 2, step = .1),
      numericInput("prior_a", "Parameter of the priori distribution Beta(a,b): a",
                   min = 0, max = Inf, value = .5, step = .1),
      numericInput("prior_b", "Parameter of the priori distribution Beta(a,b): b",
                   min = 0, max = Inf, value = .5, step = .1)
    ),
    column(9,
      align = "center",
      plotOutput("funnel_plot", height = "650px", width = "1500px")
    )
  )
))
