library(shiny)
library(dplyr)
library(ggplot2)
library(purrr)
library(scales)

server <- shinyServer(function(input, output, session) {
  prob_naive <- function(J, theta, R) {
    poss_events <- 0L:J
    outlier <- poss_events[classify_dc(threshold_value = R, pathway = "naive",
                                            dc = (poss_events / J))]
    sum(dbinom(outlier, J, theta))
  }

  prob_report <- function(J, theta, R, alpha, a = .5, b = .5) {
    poss_events <- 0L:J
    outlier <- poss_events[
      classify_dc(threshold_value = alpha,
                       dc = compute_rate_dc(
                         o = poss_events, n = J, t = R,
                         prior_a = a, prior_b = b
                       ))
    ]
    sum(dbinom(outlier, J, theta))
  }

  prob_change <- function(J, theta, R, omega, a = .5, b = .5) {
    poss_events <- 0L:J
    outlier <- poss_events[
      classify_dc(threshold_value = omega, pathway = "change",
                       dc = compute_rate_dc(
                         o = poss_events, n = J, t = R,
                         prior_a = a, prior_b = b,
                         pathway = "change"
                       )
      )
    ]
    sum(dbinom(outlier, J, theta))
  }

  # Setting:
  R <- reactive({
    input$ref_value
  })
  theta <- reactive({
    input$theta
  })
  n_grid <- 1:200

  # Compute results
  res <- reactive({
    data.frame(
      n = n_grid,
      naive = mapply(prob_naive, J = n_grid, R = R(), theta = theta()),
      change = mapply(prob_change,
        J = n_grid, R = R(), theta = theta(),
        omega = input$omega, a = input$prior_a, b = input$prior_b
      ),
      report = mapply(prob_report,
        J = n_grid, R = R(), theta = theta(),
        alpha = input$alpha, a = input$prior_a, b = input$prior_b
      )
    )
  })

  output$plot <- renderPlot({
    ggplot(res()) +
      geom_line(aes(n, naive, col = "a"), lwd = 1.2) +
      geom_line(aes(n, report, col = "b"), lwd = 1.2) +
      geom_line(aes(n, change, col = "c"), lwd = 1.2) +
      xlab("Number of patients (J)") +
      scale_x_continuous(breaks = c(0, 50, 100, 150, 200)) +
      ylab(expression(P(Outlier ~ ~"|" ~ ~theta, J))) +
      scale_color_manual(
        name = "", values = scales::hue_pal()(3),
        labels = c(
          bquote(d[naive]), bquote(d[report] * " (" * alpha * " = " * .(input$alpha * 100) * "%)"),
          bquote(d[change] * " (" * omega * " = " * .(input$omega) * ")")
        )
      ) +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .2), limits = c(0, 1)) +
      theme(
        text = element_text(size = 20),
        legend.key.size = unit(3, "lines")
      )
  })
})
