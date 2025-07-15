#| file: app.R
#| title: Interaktives Haushaltsoptimum

# Benötigte Pakete laden
library(shiny)
library(ggplot2)

# Definition der Benutzeroberfläche (UI)
ui <- fluidPage(
  # CSS für ein besseres Design hinzufügen
  tags$head(
    tags$style(HTML("
      .shiny-input-container { margin-bottom: 20px; }
      .well { background-color: #f8f9fa; border-color: #dee2e6;}
      h2, h4 { color: #343a40; }
    "))
  ),
  
  # Titel der Anwendung
  titlePanel(h2("Interaktives Haushaltsoptimum (Cobb-Douglas-Nutzenfunktion)", align = "center")),
  
  # Seitenleisten-Layout
  sidebarLayout(
    # Panel für die Seitenleiste mit den Eingabeelementen
    sidebarPanel(
      width = 3,
      h4("Parameter anpassen"),
      # Schieberegler für das Einkommen (I)
      sliderInput("I", "Einkommen (I)", min = 50, max = 200, value = 100, step = 10, pre = "€"),
      # Schieberegler für den Preis von Gut 1 (p1)
      sliderInput("p1", "Preis von Gut 1 (p₁)", min = 0.1, max = 5, value = 1, step = 0.1, pre = "€"),
      # Schieberegler für den Preis von Gut 2 (p2)
      sliderInput("p2", "Preis von Gut 2 (p₂)", min = 0.1, max = 5, value = 0.5, step = 0.1, pre = "€"),
      # Schieberegler für den Präferenzparameter (alpha)
      sliderInput("alpha", "Präferenz (α)", min = 0.05, max = 0.95, value = 0.3, step = 0.05)
    ),
    
    # Hauptpanel zur Anzeige der Grafik
    mainPanel(
      width = 9,
      plotOutput("cobbDouglasPlot", height = "600px")
    )
  )
)

# Definition der Server-Logik
server <- function(input, output) {
  
  # Reaktiv die Berechnungen durchführen, wenn sich ein Input ändert
  reactive_calcs <- reactive({
    # Eingabewerte von den Schiebereglern holen
    I <- input$I
    p1 <- input$p1
    p2 <- input$p2
    alpha <- input$alpha
    
    # Berechnung des optimalen Konsumpunkts (Gleichgewicht)
    x1_eq <- alpha * I / p1
    x2_eq <- (1 - alpha) * I / p2
    
    # Berechnung des Nutzenniveaus im Gleichgewicht
    # Fehlerbehandlung für den Fall, dass x1_eq oder x2_eq null sind
    U_eq <- if (x1_eq > 0 && x2_eq > 0) {
      x1_eq^alpha * x2_eq^(1 - alpha)
    } else {
      0
    }
    
    list(I = I, p1 = p1, p2 = p2, alpha = alpha, x1_eq = x1_eq, x2_eq = x2_eq, U_eq = U_eq)
  })
  
  # Die Grafik rendern
  output$cobbDouglasPlot <- renderPlot({
    
    # Hole die berechneten Werte
    calcs <- reactive_calcs()
    I <- calcs$I
    p1 <- calcs$p1
    p2 <- calcs$p2
    x1_eq <- calcs$x1_eq
    x2_eq <- calcs$x2_eq
    U_eq <- calcs$U_eq
    
    # Maximaler Konsum von Gut 1 (Schnittpunkt mit der x-Achse)
    x1_max <- I / p1
    
    # Erstellen der Daten für die Kurven
    # Wir starten bei einem kleinen Wert, um Division durch Null zu vermeiden
    x1_vals <- seq(0.01, x1_max * 1.1, length.out = 200)
    
    # Daten für die Budgetgerade
    df_budget <- data.frame(
      x1 = seq(0, x1_max, length.out = 200),
      Kurve = "Budgetgerade"
    )
    df_budget$x2 <- I / p2 - (p1 / p2) * df_budget$x1
    
    # Daten für die Indifferenzkurve
    df_indifferenz <- data.frame(
      x1 = x1_vals,
      Kurve = "Indifferenzkurve"
    )
    # Nur berechnen, wenn U_eq > 0 ist
    if (U_eq > 0) {
      df_indifferenz$x2 <- (U_eq / (df_indifferenz$x1^calcs$alpha))^(1 / (1 - calcs$alpha))
    } else {
      df_indifferenz$x2 <- NA
    }
    
    # Kombinieren der Daten
    df <- rbind(df_budget, df_indifferenz)
    # Entferne unmögliche (negative) Werte und Ausreißer
    df <- df[df$x2 >= 0 & df$x2 < (I/p2)*1.5, ]
    
    # Erstellen der Grafik mit ggplot2
    ggplot(df, aes(x = x1, y = x2, color = Kurve)) +
      geom_line(na.rm = TRUE, linewidth = 1.2) +
      
      # Gleichgewichtspunkt und gestrichelte Linien hinzufügen
      annotate("point", x = x1_eq, y = x2_eq, color = "#212529", size = 4, shape = 8, stroke = 1.5) +
      annotate("segment", x = x1_eq, xend = x1_eq, y = 0, yend = x2_eq, linetype = "dashed", color = "black") +
      annotate("segment", x = 0, xend = x1_eq, y = x2_eq, yend = x2_eq, linetype = "dashed", color = "black") +
      
      # Skalen und Achsen anpassen
      scale_x_continuous(
        limits = c(0, 300), 
        expand = c(0, 0),
        breaks = unique(round(c(0, x1_eq, I / p1), 1))
      ) +
      scale_y_continuous(
        limits = c(0, 300), 
        expand = c(0, 0),
        breaks = unique(round(c(0, x2_eq, I / p2), 1))
      ) +
      scale_color_manual(values = c("Budgetgerade" = "red", "Indifferenzkurve" = "green")) +
      
      # Titel, Untertitel und Achsenbeschriftungen
      labs(
        title = "Haushaltsoptimum: Budgetgerade und Indifferenzkurve",
        subtitle = paste0("Optimum bei x₁ = ", round(x1_eq, 2), " und x₂ = ", round(x2_eq, 2)),
        x = expression(Menge~Gut~x[1]),
        y = expression(Menge~Gut~x[2]),
        color = ""
      ) +
      
      # Theme und Design anpassen
      theme_light(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "gray80")
      )
  }, res = 100) # Auflösung für eine schärfere Grafik erhöhen
}

# Die Shiny App erstellen und starten
shinyApp(ui = ui, server = server)
