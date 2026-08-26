library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)

# -------------------------
# LOAD DATA
# -------------------------
cars <- read.csv("car_sales_data.csv")

# Sanity checks — run these once in the console, not in the app,
# to confirm column names / types match what the rest of the code expects
str(cars)
head(cars)
names(cars)

# NOTE: read.csv() auto-converts spaces in header names to dots.
# "Sale Price" -> Sale.Price, "Car Make" -> Car.Make, "Car Model" -> Car.Model,
# "Commission Rate" -> Commission.Rate, etc. Confirm exact names via names(cars).

cars$Date <- as.Date(cars$Date)
cars$Year  <- year(cars$Date)
cars$Month <- month(cars$Date, label = TRUE)

# -------------------------
# UI — minimal, responsive
# -------------------------
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("Car Sales — Minimal Test"),
  
  card(
    card_header("Sales & Revenue Trend"),
    plotOutput("trend_plot_static", height = "400px"),   # <- diagnostic: plain ggplot first
    plotlyOutput("trend_plot", height = "400px")          # <- plotly version, compare after
  )
  
  # -------------------------
  # Everything below is disabled for now — uncomment once
  # the minimal version above is confirmed working
  # -------------------------
  
  # sidebar = sidebar(
  #   selectInput("make", "Car Make:", unique(cars$`Car Make`), multiple = TRUE),
  #   selectInput("model", "Car Model:", unique(cars$`Car Model`), multiple = TRUE),
  #   selectInput("salesperson", "Salesperson:", unique(cars$Salesperson), multiple = TRUE),
  #   dateRangeInput("date", "Date Range:", min = min(cars$Date), max = max(cars$Date),
  #                  start = min(cars$Date), end = max(cars$Date)),
  #   sliderInput("price", "Price Range:", min = min(cars$`Sale Price`), max = max(cars$`Sale Price`),
  #               value = c(min(cars$`Sale Price`), max(cars$`Sale Price`)))
  # ),
  #
  # layout_column_wrap(
  #   width = 1/3,
  #   value_box("Total Sales", textOutput("total_sales"), showcase = icon("car")),
  #   value_box("Total Revenue", textOutput("total_revenue"), showcase = icon("dollar-sign")),
  #   value_box("Avg Price", textOutput("avg_price"), showcase = icon("chart-line"))
  # ),
  #
  # navset_tab(
  #   nav_panel("Market Analysis", ... ),
  #   nav_panel("Pricing", ... ),
  #   nav_panel("Sales Performance", ... ),
  #   nav_panel("Data Explorer", DTOutput("table"))
  # )
)

# -------------------------
# SERVER — minimal
# -------------------------
server <- function(input, output, session) {
  
  # DIAGNOSTIC: pre-compute the aggregated df once, print info to console
  trend_df <- reactive({
    df <- cars %>%
      group_by(Date) %>%
      summarise(
        Sales = n(),
        Revenue = sum(Sale.Price),
        .groups = "drop"
      )
    
    cat("trend_df rows:", nrow(df), "\n")
    cat("NA dates:", sum(is.na(df$Date)), "\n")
    cat("NA revenue:", sum(is.na(df$Revenue)), "\n")
    
    df
  })
  
  # DIAGNOSTIC: plain static ggplot — if this renders but plotly below doesn't,
  # the problem is in ggplotly()/htmlwidgets, not the data
  output$trend_plot_static <- renderPlot({
    df <- trend_df()
    
    ggplot(df, aes(Date, Revenue)) +
      geom_line(color = "steelblue") +
      labs(title = "Revenue Over Time (static)", x = "Date", y = "Revenue") +
      theme_minimal()
  })
  
  output$trend_plot <- renderPlotly({
    
    df <- trend_df()
    
    p <- ggplot(df, aes(Date, Revenue)) +
      geom_line(color = "steelblue") +
      labs(title = "Revenue Over Time", x = "Date", y = "Revenue") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(autosize = TRUE)  # responsive to container width
  })
  
  # -------------------------
  # Everything else disabled — bring back one output at a time
  # once you confirm the above renders correctly
  # -------------------------
  
  # filtered <- reactive({ ... })
  # output$total_sales <- renderText({ ... })
  # output$total_revenue <- renderText({ ... })
  # output$avg_price <- renderText({ ... })
  # output$make_plot <- renderPlotly({ ... })
  # output$model_plot <- renderPlotly({ ... })
  # output$price_hist <- renderPlotly({ ... })
  # output$price_commission <- renderPlotly({ ... })
  # output$salesperson_plot <- renderPlotly({ ... })
  # output$table <- renderDT({ ... })
}

shinyApp(ui, server)