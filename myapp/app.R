library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Interaktive Budgetrestriktion und Indifferenzkurve"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("I", "Einkommen (I):", min = 10, max = 300, value = 100, step = 1),
      sliderInput("p1", "Preis von Gut 1 (p1):", min = 0.1, max = 5, value = 1, step = 0.1),
      sliderInput("p2", "Preis von Gut 2 (p2):", min = 0.1, max = 5, value = 0.5, step = 0.1),
      sliderInput("alpha", "Gewichtungsparameter (alpha):", min = 0, max = 1, value = 0.3, step = 0.01)
    ),
    mainPanel(
      plotOutput("mainplot", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  output$mainplot <- renderPlot({
    # Parameter
    I <- input$I
    p1 <- input$p1
    p2 <- input$p2
    alpha <- input$alpha
    
    x1_eq <- alpha * I / p1
    x2_eq <- (1 - alpha) * I / p2
    U_eq <- x1_eq^alpha * x2_eq^(1 - alpha)
    x1 <- 0:100
    
    df <- rbind(
      data.frame(x1, Kurve = "Budget", x2 = I / p2 - p1 / p2 * x1),
      data.frame(x1, Kurve = "Indifferenz", x2 = (U_eq / (x1^alpha))^(1 / (1 - alpha)))
    )
    
    # Filter unendliche/nicht-numerische x2 (z.B. durch Division durch 0) heraus
    df <- df[is.finite(df$x2) & df$x2 >= 0, ]
    
    ggplot(df, aes(x1, x2, color = Kurve)) +
      geom_line(linewidth = 1) +
      annotate("point", x = x1_eq, y = x2_eq, color = "black", size = 2) +
      annotate("segment", x = x1_eq, xend = x1_eq, y = 0, yend = x2_eq, linetype = "dashed", color = "black") +
      annotate("segment", x = 0, xend = x1_eq, y = x2_eq, yend = x2_eq, linetype = "dashed", color = "black") +
      scale_x_continuous(
        limits = c(NA, I / p1 + 10), expand = c(0, 0),
        breaks = round(c(0, .5 * I / p1, I / p1, x1_eq), 2)
      ) +
      scale_y_continuous(
        limits = c(NA, I / p2 + 10), expand = c(0, 0),
        breaks = round(c(0, .5 * I / p2, I / p2, x2_eq), 2)
      ) +
      theme_light(base_size = 15) +
      labs(
        title = "Die Budgetbeschränkung und Indifferenzkurve",
        x = expression(x[1]),
        y = expression(x[2])
      )
  })
}

shinyApp(ui, server)
