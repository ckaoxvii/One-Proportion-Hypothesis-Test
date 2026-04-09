library(shiny)

library(bslib)

library(reactable)

library(tidyverse)

library(ggplot2)

ui <- page_sidebar(
  title = "One-Proportion Hypothesis Test",
  theme = bs_theme(
    primary = '#A90533',
    "navbar-bg" = "#A90533",
    "card-header-bg" = "#A90533",
    "card-header-color" = "white"
  ),
  tags$head(
    tags$style(
      HTML(
        ".card-header {
          background-color: #A90533 !important;
          color: white !important;
          font-weight: bold;
        }"
      )
    )
  ),
  sidebar = sidebar(
    radioButtons(
      "input_type",
      "Input Type",
      choices = c(
        "Sample Proportion" = "prop",
        "Number of Successes" = "count"
      ),
      selected = "prop"
    ),
    withMathJax(),
    tags$label(HTML("Null Proportion, \\(p_0\\):"), `for` = "p0"),
    numericInput(
      "p0",
      label = NULL,
      value = 0.50,
      min = 0,
      max = 1,
      step = 0.01
    ),
    uiOutput("sample_input_ui"),
    tags$label(HTML("Significance Level, \\(\\alpha\\):"), `for` = "alpha"),
    numericInput(
      "alpha",
      label = NULL,
      value = 0.05,
      min = 0.0001,
      max = 0.5,
      step = 0.001
    ),
    withMathJax(
      radioButtons(
        "alt",
        "Alternative Hypothesis",
        choices = c(
          "\\(p \\ne p_0\\)" = "two.sided",
          "\\(p > p_0\\)" = "greater",
          "\\(p < p_0\\)" = "less"
        )
      )
    )
  ),

  withMathJax(
    layout_columns(
      col_widths = c(6, 6, 12),
      row_heights = c(1, 1, 2),
      card(
        full_screen = TRUE,
        card_header("Hypothesis Test Input Summary"),
        uiOutput("hypothesis_summary")
      ),
      card(
        full_screen = TRUE,
        card_header("Hypothesis Test Results"),
        reactableOutput("results_table")
      ),
      card(
        full_screen = TRUE,
        card_header("\\(p\\)-Value Plot"),
        plotOutput("pvalue_plot")
      )
    )
  )
)

server <- function(input, output, session) {

  fmt <- function(x, d = 4) {
    out <- sprintf(paste0("%.", d, "f"), x)
    sub("^-", "\u2212", out)
  }

  fmtp <- function(p) {
    ifelse(
      p < 1e-4, 
      "< 0.0001",
      formatC(p, format = "f", digits = 6)
    )
  }

  output$sample_input_ui <- renderUI({
    if (input$input_type == "prop") {
      tagList(
        tags$label(
          HTML("Sample Proportion, \\(\\hat{p}\\):"),
          `for` = "p_hat"
        ),
        numericInput(
          "p_hat",
          label = NULL,
          value = 0.55,
          min = 0,
          max = 1,
          step = 0.001
        ),
        tags$label(
          "Sample Size:",
          `for` = "n"
        ),
        numericInput(
          "n",
          label = NULL,
          value = 100,
          min = 1,
          step = 1
        )
      )
    } else {
      tagList(
        tags$label(
          "Number of Successes:",
          `for` = "x"
        ),
        numericInput(
          "x",
          label = NULL,
          value = 55,
          min = 0,
          step = 1
        ),
        tags$label(
          "Sample Size:",
          `for` = "n_count"
        ),
        numericInput(
          "n_count",
          label = NULL,
          value = 100,
          min = 1,
          step = 1
        )
      )
    }
  })

  calc <- reactive({
    req(input$p0, input$alt)

    p0 <- input$p0
    alt <- input$alt

    validate(
      need(p0 >= 0 && p0 <= 1, "Null proportion must be between 0 and 1.")
    )

    if (input$input_type == "prop") {
      req(input$p_hat, input$n)

      validate(
        need(
          input$p_hat >= 0 && input$p_hat <= 1,
          "Sample proportion must be between 0 and 1."
        ),
        need(
          input$n >= 1,
          "Sample size must be at least 1."
        )
      )

      ph <- input$p_hat
      n <- input$n
      x <- round(ph*n)
    } else {
      req(input$x, input$n_count)

      validate(
        need(
          input$x >= 0,
          "Number of successes must be nonnegative."
        ),
        need(
          input$n_count >= 1,
          "Sample size must be at least 1."
        ),
        need(
          input$x <= input$n_count,
          "Number of successes cannot exceed the sample size."
        )
      )

      x <- input$x
      n <- input$n_count
      ph <- x/n
    }

    se <- sqrt(p0*(1 - p0)/n)
    z <- (ph - p0)/se

    p_val <- switch(
      alt,
      "two.sided" = 2*pnorm(-abs(z)),
      "greater"   = 1 - pnorm(z),
      "less"      = pnorm(z)
    )

    list(
      p0 = p0,
      ph = ph,
      x = x,
      n = n,
      se = se,
      z = z,
      p_val = p_val,
      alt = alt
    )
  })

  output$hypothesis_summary <- renderUI({
    cdat <- calc()

    alt_text <- switch(
      cdat$alt,
      "two.sided" = paste0("\\(H_a: p \\ne ", fmt(cdat$p0, 4), "\\)"),
      "greater"   = paste0("\\(H_a: p > ", fmt(cdat$p0, 4), "\\)"),
      "less"      = paste0("\\(H_a: p < ", fmt(cdat$p0, 4), "\\)")
    )

    tagList(
      withMathJax(),
      tags$p(HTML(paste0("\\(H_0: p = ", fmt(cdat$p0, 4), "\\)"))),
      tags$p(HTML(alt_text)),
      tags$p(HTML(paste0("\\(p_0 = ", fmt(cdat$p0, 4), "\\)"))),
      tags$p(HTML(paste0("\\(\\hat{p} = ", fmt(cdat$ph, 4), "\\)"))),
      tags$p(HTML(paste0("\\(n = ", cdat$n, "\\)")))
    )
  })

  mathjax_render <- function(tbl) {
    htmlwidgets::onRender(
      tbl,
      "function(el,x){
        if(window.MathJax){
          if(MathJax.typesetPromise){
            MathJax.typesetPromise([el]);
          } else if(MathJax.Hub && MathJax.Hub.Queue){
            MathJax.Hub.Queue(['Typeset', MathJax.Hub, el]);
          }
        }
      }"
    )
  }

  output$results_table <- renderReactable({
    cdat <- calc()

    mathjax_render(
      reactable(
        tibble(
          "Standard Error" = fmt(cdat$se, 4),
          "\\(z\\)-Statistic" = fmt(cdat$z, 4),
          "\\(p\\)-Value" = fmtp(cdat$p_val)
        ),
        defaultColDef = colDef(align = 'right')
      )
    )
  })

  output$pvalue_plot <- renderPlot({
    cdat <- calc()

    x_grid <- seq(-4.5, 4.5, length.out = 2000)

    curve_df <- tibble(
      x = x_grid,
      y = dnorm(x_grid)
    )

    z_obs <- cdat$z
    z_abs <- abs(z_obs)

    left_tail  <- curve_df %>% filter(x <= -z_abs)
    right_tail <- curve_df %>% filter(x >=  z_abs)
    left_one   <- curve_df %>% filter(x <= z_obs)
    right_one  <- curve_df %>% filter(x >= z_obs)

    p <- ggplot(curve_df, aes(x, y)) +
      geom_line(linewidth = 1.2, color = "black") +
      labs(
        x = "z",
        y = "Density"
      ) +
      coord_cartesian(xlim = c(-4.5, 4.5), ylim = c(0, 0.42)) +
      theme_minimal(base_size = 18) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black")
      )

    if (cdat$alt == "two.sided") {

      left_lab_x  <- max(-3.2, -z_abs - 0.95)
      right_lab_x <- min( 3.2,  z_abs + 0.95)
      tail_lab <- paste0("Area = ", formatC(cdat$p_val / 2, format = "f", digits = 4))

      p <- p +
        geom_area(
          data = left_tail,
          aes(x = x, y = y),
          inherit.aes = FALSE,
          fill = "#CC3366",
          alpha = 0.30
        ) +
        geom_area(
          data = right_tail,
          aes(x = x, y = y),
          inherit.aes = FALSE,
          fill = "#CC3366",
          alpha = 0.30
        ) +
        geom_vline(
          xintercept = c(-z_abs, z_abs),
          color = "#FF5A36",
          linewidth = 1.1,
          linetype = "dashed"
        ) +
        annotate(
          "label",
          x = left_lab_x,
          y = 0.06,
          label = tail_lab,
          fill = "white",
          color = "#CC2244",
          label.size = 0.3,
          size = 6,
          alpha = 0.75
        ) +
        annotate(
          "label",
          x = right_lab_x,
          y = 0.06,
          label = tail_lab,
          fill = "white",
          color = "#CC2244",
          label.size = 0.3,
          size = 6,
          alpha = 0.75
        )

    } else if (cdat$alt == "greater") {

      lab_x <- min(3.2, z_obs + 1.0)

      p <- p +
        geom_area(
          data = right_one,
          aes(x = x, y = y),
          inherit.aes = FALSE,
          fill = "#CC3366",
          alpha = 0.30
        ) +
        geom_vline(
          xintercept = z_obs,
          color = "#FF5A36",
          linewidth = 1.1,
          linetype = "dashed"
        ) +
        annotate(
          "label",
          x = lab_x,
          y = 0.06,
          label = paste0("Area = ", formatC(cdat$p_val, format = "f", digits = 4)),
          fill = "white",
          color = "#CC2244",
          label.size = 0.3,
          size = 6,
          alpha = 0.75
        )

    } else {

      lab_x <- max(-3.2, z_obs - 1.0)

      p <- p +
        geom_area(
          data = left_one,
          aes(x = x, y = y),
          inherit.aes = FALSE,
          fill = "#CC3366",
          alpha = 0.30
        ) +
        geom_vline(
          xintercept = z_obs,
          color = "#FF5A36",
          linewidth = 1.1,
          linetype = "dashed"
        ) +
        annotate(
          "label",
          x = lab_x,
          y = 0.06,
          label = paste0("Area = ", formatC(cdat$p_val, format = "f", digits = 4)),
          fill = "white",
          color = "#CC2244",
          label.size = 0.3,
          size = 6,
          alpha = 0.75
        )
    }

    p
  })
}

shinyApp(ui, server)