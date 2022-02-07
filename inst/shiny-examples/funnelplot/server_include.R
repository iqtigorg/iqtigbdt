server <- shinyServer(function(input, output, session) {
  calculate_threshold_change <- function(J, R, omega, a = 0.5, b = 0.5) {
    o_range <- 0L:J
    o_range[classify_dc(threshold_value = omega, pathway = "change",
                             dc = compute_rate_dc(
      o = o_range, n = J, t = R,
      prior_a = a, prior_b = b,
      pathway = "change"
    ))][1L]
  }

  calculate_threshold_report <- function(J, R, alpha, a = 0.5, b = 0.5) {
    o_range <- 0L:J
    o_range[classify_dc(threshold_value = alpha,
                             dc = compute_rate_dc(
                               o = o_range, n = J, t = R,
                               prior_a = a, prior_b = b
                             ))][1L]
  }

  calculate_threshold_naive <- function(J, R) {
    o_range <- 0L:J
    o_range[classify_dc(threshold_value = R, pathway = "naive",
                             dc = (o_range / J))][1L]
  }

  dataset <- reactive({
    if (input$ref_value == 0) stop("Reference value must be > 0")
    data.frame(
      o = rep(0:input$max_n, each = length(0:input$max_n)),
      n = rep(0:input$max_n, length(0:input$max_n))
    ) %>%
      filter(o <= n & n > 0) %>%
      left_join(
        data.frame(
          n = 1:input$max_n
        ) %>%
          mutate(
            reporting = mapply(calculate_threshold_report,
              J = n,
              R = input$ref_value, alpha = input$alpha,
              a = input$prior_a, b = input$prior_b
            ),
            change = mapply(calculate_threshold_change,
              J = n,
              R = input$ref_value, omega = input$omega,
              a = input$prior_a, b = input$prior_b
            ),
            naive = mapply(calculate_threshold_naive,
              J = n,
              R = input$ref_value
            )
          ),
        by = "n"
      )
  })

  output$funnel_plot <- renderPlot({
    if (input$ref_value <= 0) stop("Reference value must be > 0")
    if (input$ref_value >= 1) stop("Reference value must be < 1")
    plot_data <- dataset()
    a_level <- input$alpha * 100
    ggplot(plot_data, aes(n, o / n)) +
      geom_point(alpha = .1, size = 3) +
      geom_line(aes(n, reporting / n, col = "b", lty = "b"), lwd = 1.5) +
      scale_x_continuous("Number of patients (J)", limits = c(0, input$max_n)) +
      scale_y_continuous("Observed result (o/J)", limits = c(0, 1), labels = scales::percent) +
      geom_hline(yintercept = input$ref_value, lwd = 1.5, lty = "dashed", col = "dark red") +
      theme(text = element_text(size = 20)) +
      geom_line(aes(n, change / n, col = "c", lty = "c"), lwd = 1.5) +
      geom_line(aes(n, naive / n, col = "a", lty = "a"), lwd = 1.5) +
      scale_color_manual("",
        values = scales::hue_pal()(3),
        labels = c(
          bquote(d[naive]), bquote(d[report] * " (" * alpha * " = " * .(a_level) * "%)"),
          bquote(d[change] * " (" * omega * " = " * .(input$sd_auff) * ")")
        )
      ) +
      scale_linetype_manual("",
        values = c(1, 1, 1),
        labels = c(
          bquote(d[naive]), bquote(d[report] * " (" * alpha * " = " * .(a_level) * "%)"),
          bquote(d[change] * " (" * omega * " = " * .(input$sd_auff) * ")")
        )
      ) +
      theme(legend.key.size = unit(3, "lines"))
  })
})
