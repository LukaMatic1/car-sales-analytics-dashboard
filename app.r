library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(scales)

# -------------------------
# LOAD DATA
# -------------------------
cars <- read.csv("car_sales_data.csv")

# NOTE: read.csv() auto-converts spaces in header names to dots.
# "Sale Price" -> Sale.Price, "Car Make" -> Car.Make, "Car Model" -> Car.Model,
# "Commission Rate" -> Commission.Rate, etc.

cars$Date <- as.Date(cars$Date)
cars$Sale.Price <- as.numeric(cars$Sale.Price)  # guard against 32-bit int overflow on sum()
cars$Year  <- year(cars$Date)
cars$Month <- month(cars$Date, label = TRUE)

# Precompute filter choices once at startup
make_choices <- sort(unique(cars$Car.Make))
model_choices <- sort(unique(cars$Car.Model))
salesperson_choices <- sort(unique(cars$Salesperson))

date_min <- min(cars$Date)
date_max <- max(cars$Date)
price_min <- min(cars$Sale.Price)
price_max <- max(cars$Sale.Price)

# -------------------------
# UI — sidebar filters added, one plot for now
# -------------------------
ui <- page_sidebar(
  title = "Car Sales — Minimal Test",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    actionButton("reset_filters", "Reset Filters", icon = icon("rotate-left")),
    tags$br(), tags$br(),
    
    selectInput("make", "Car Make:", choices = make_choices, multiple = TRUE),
    selectInput("model", "Car Model:", choices = model_choices, multiple = TRUE),
    selectizeInput("salesperson", "Salesperson:", choices = NULL, multiple = TRUE),
    
    dateRangeInput(
      "date", "Date Range:",
      min = date_min, max = date_max,
      start = date_min, end = date_max
    ),
    
    sliderInput(
      "price", "Price Range:",
      min = price_min, max = price_max,
      value = c(price_min, price_max)
    )
  ),
  
  layout_column_wrap(
    width = 1/3,
    value_box("Total Sales", textOutput("total_sales"), showcase = icon("car")),
    value_box("Total Revenue", textOutput("total_revenue"), showcase = icon("dollar-sign")),
    value_box("Avg Price", textOutput("avg_price"), showcase = icon("chart-line"))
  ),
  
  card(
    card_header("Sales & Revenue Trend"),
    radioButtons("metric", NULL, choices = c("Revenue", "Cars Sold"),
                 selected = "Revenue", inline = TRUE),
    plotlyOutput("trend_plot", height = "400px")
  )
  
  # -------------------------
  # Still disabled — add back one at a time after KPIs are confirmed working
  # -------------------------
  
  # navset_tab(
  #   nav_panel("Market Analysis", ... ),
  #   nav_panel("Pricing", ... ),
  #   nav_panel("Sales Performance", ... ),
  #   nav_panel("Data Explorer", DTOutput("table"))
  # )
)

# -------------------------
# SERVER
# -------------------------
server <- function(input, output, session) {
  
  # Populate salesperson choices server-side — avoids sending a huge
  # option list to the browser upfront, which can crash rendering
  updateSelectizeInput(session, "salesperson", choices = salesperson_choices, server = TRUE)
  
  # Reset all filters back to defaults when the button is clicked
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "make", selected = character(0))
    updateSelectInput(session, "model", selected = character(0))
    updateSelectizeInput(session, "salesperson", selected = character(0), server = TRUE)
    updateDateRangeInput(session, "date", start = date_min, end = date_max)
    updateSliderInput(session, "price", value = c(price_min, price_max))
  })
  
  # Filtered raw data — filters apply per-transaction
  filtered <- reactive({
    df <- cars
    
    if (!is.null(input$make) && length(input$make) > 0)
      df <- df %>% filter(Car.Make %in% input$make)
    
    if (!is.null(input$model) && length(input$model) > 0)
      df <- df %>% filter(Car.Model %in% input$model)
    
    if (!is.null(input$salesperson) && length(input$salesperson) > 0)
      df <- df %>% filter(Salesperson %in% input$salesperson)
    
    df <- df %>%
      filter(
        Date >= input$date[1], Date <= input$date[2],
        Sale.Price >= input$price[1], Sale.Price <= input$price[2]
      )
    
    df
  })
  
  # KPI value boxes — react to the same filtered() data as the chart
  output$total_sales <- renderText({
    comma(nrow(filtered()))
  })
  
  output$total_revenue <- renderText({
    dollar(sum(filtered()$Sale.Price), scale = 1e-6, suffix = "M")
  })
  
  output$avg_price <- renderText({
    dollar(mean(filtered()$Sale.Price))
  })
  
  # Aggregate the filtered data by day (same as yesterday's confirmed-working version)
  trend_df <- reactive({
    df <- filtered() %>%
      group_by(Date) %>%
      summarise(
        Sales = n(),
        Revenue = sum(Sale.Price),
        .groups = "drop"
      )
    
    cat("trend_df rows:", nrow(df), "\n")
    
    df
  })
  
  output$trend_plot <- renderPlotly({
    
    df <- trend_df()
    
    if (input$metric == "Revenue") {
      p <- ggplot(df, aes(Date, Revenue)) +
        geom_line(color = "steelblue") +
        labs(title = "Revenue Over Time", x = "Date", y = "Revenue") +
        theme_minimal()
    } else {
      p <- ggplot(df, aes(Date, Sales)) +
        geom_line(color = "darkorange") +
        labs(title = "Cars Sold Over Time", x = "Date", y = "Cars Sold") +
        theme_minimal()
    }
    
    ggplotly(p) %>%
      layout(autosize = TRUE)  # responsive to container width
  })
  
  # -------------------------
  # Still disabled — bring back one at a time
  # -------------------------
  
  # output$make_plot <- renderPlotly({ ... })
  # output$model_plot <- renderPlotly({ ... })
  # output$price_hist <- renderPlotly({ ... })
  # output$price_commission <- renderPlotly({ ... })
  # output$salesperson_plot <- renderPlotly({ ... })
  # output$table <- renderDT({ ... })
}

shinyApp(ui, server)