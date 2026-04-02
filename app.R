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
        card_header("Hypothesis Test Results")
      ),
      card(
        full_screen = TRUE,
        card_header("\\(p\\)-Value Plot")
      )
    )
  )
)

server <- function(input, output, session) {

  fmt <- function(x, d = 4) {
    sprintf(paste0("%.", d, "f"), x)
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
      "less"      = pnrom(z)
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
    req(input$p0, input$alt)

    alt_text <- switch(
      input$alt,
      "two.sided" = paste0("\\(H_a: p \\ne ", input$p0, "\\)"),
      "greater"   = paste0("\\(H_a: p > ", input$p0, "\\)"),
      "less"      = paste0("\\(H_a: p < ", input$p0, "\\)")
    )

    tagList(
      withMathJax(),
      tags$p(HTML(paste0("\\(H_0: p = ", input$p0, "\\)"))),
      tags$p(HTML(alt_text)),
      tags$p(HTML(paste0("\\(p_0 = ", input$p0, "\\)"))),
      tags$p(HTML(paste0("\\(\\hat{p} = ", input$p_hat, "\\)"))),
      tags$p(HTML(paste0("\\(n = ", input$n, "\\)")))
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
}

shinyApp(ui, server)