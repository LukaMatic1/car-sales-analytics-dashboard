library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(scales)
library(DT)

# -------------------------
# LOAD DATA
# -------------------------
cars <- read.csv("car_sales_data.csv")

# NOTE: read.csv() auto-converts spaces in header names to dots.
# "Sale Price" -> Sale.Price, "Car Make" -> Car.Make, "Car Model" -> Car.Model,
# "Commission Rate" -> Commission.Rate, etc.

cars$Date <- as.Date(cars$Date)
cars$Sale.Price <- as.numeric(cars$Sale.Price)  # guard against 32-bit int overflow on sum()

# Factors speed up group_by()/summarise() on repeated categorical grouping
cars$Car.Make <- as.factor(cars$Car.Make)
cars$Car.Model <- as.factor(cars$Car.Model)
cars$Salesperson <- as.factor(cars$Salesperson)

# Precompute filter choices once at startup
make_choices <- sort(as.character(unique(cars$Car.Make)))
model_choices <- sort(as.character(unique(cars$Car.Model)))
salesperson_choices <- sort(as.character(unique(cars$Salesperson)))

date_min <- min(cars$Date)
date_max <- max(cars$Date)
price_min <- min(cars$Sale.Price)
price_max <- max(cars$Sale.Price)

# Fixed, consistent color per Car Make — reused across every chart that shows
# makes, so "Ford" is always the same color no matter which view you're in
# (Gestalt: Similarity)
make_palette <- c("#2C3E50", "#E67E22", "#27AE60", "#2980B9", "#8E44AD",
                   "#C0392B", "#16A085", "#D35400", "#7F8C8D", "#F39C12")
MAKE_COLORS <- setNames(make_palette[seq_along(make_choices)], make_choices)

# Shared ggplot theme so every chart has consistent typography/spacing
# (Figure-Ground: keep the data itself as the visual focus, minimal clutter)
theme_app <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# -------------------------
# UI — sidebar filters added, one plot for now
# -------------------------
ui <- page_sidebar(
  title = tagList(icon("car-side"), "Car Sales Analytics"),
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    actionButton("reset_filters", "Reset Filters", icon = icon("rotate-left"),
                 class = "btn-outline-secondary w-100"),
    tags$hr(),

    tags$h6("Filter by category", class = "text-muted"),
    selectInput("make", "Car Make:", choices = make_choices, multiple = TRUE),
    selectInput("model", "Car Model:", choices = model_choices, multiple = TRUE),
    selectizeInput("salesperson", "Salesperson:", choices = NULL, multiple = TRUE),

    tags$hr(),
    tags$h6("Filter by range", class = "text-muted"),
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

  navset_tab(
    nav_panel("Overview",
      layout_column_wrap(
        width = 1/3,
        value_box("Total Sales", textOutput("total_sales"), showcase = icon("car"), theme = "primary"),
        value_box("Total Revenue", textOutput("total_revenue"), showcase = icon("dollar-sign"), theme = "success"),
        value_box("Avg Price", textOutput("avg_price"), showcase = icon("chart-line"), theme = "info")
      ),

      card(
        card_header("Sales & Revenue Trend"),
        radioButtons("metric", NULL, choices = c("Revenue", "Cars Sold"),
                     selected = "Revenue", inline = TRUE),
        plotlyOutput("trend_plot", height = "400px")
      )
    ),

    nav_panel("Sales Performance",
      card(
        card_header("Top Salespeople by Revenue"),
        sliderInput("top_n", "Show top N:", min = 5, max = 50, value = 15, step = 5),
        plotlyOutput("salesperson_plot", height = "500px")
      )
    ),

    nav_panel("Market Analysis",
      layout_columns(
        card(
          card_header("Car Make Distribution"),
          plotlyOutput("make_plot", height = "450px")
        ),
        card(
          card_header("Car Model Distribution"),
          plotlyOutput("model_plot", height = "450px")
        )
      ),

      card(
        card_header("Sales Trend by Make (monthly)"),
        plotlyOutput("make_trend_plot", height = "450px")
      )
    ),

    nav_panel("Pricing",
      layout_columns(
        card(
          card_header("Price Distribution"),
          plotlyOutput("price_hist", height = "450px")
        ),
        card(
          card_header("Price vs Commission"),
          plotlyOutput("price_commission", height = "450px")
        )
      )
    ),

    nav_panel("Data Explorer",
      card(
        card_header("Filtered Data"),
        textOutput("table_note"),
        DTOutput("table")
      )
    )
  )
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
    updateSelectizeInput(session, "salesperson", selected = character(0),
                          choices = salesperson_choices, server = TRUE)
    updateDateRangeInput(session, "date", start = date_min, end = date_max)
    updateSliderInput(session, "price", value = c(price_min, price_max))
  })

  # Filtered raw data — filters apply per-transaction
  # Debounce the price slider — it fires continuously while dragging, and
  # every chart in the app depends on filtered(), so this avoids a burst
  # of full recomputations mid-drag
  price_debounced <- debounce(reactive(input$price), 250)

  filtered <- reactive({
    price_range <- price_debounced()

    df <- cars %>%
      filter(
        Date >= input$date[1], Date <= input$date[2],
        Sale.Price >= price_range[1], Sale.Price <= price_range[2]
      )

    if (!is.null(input$make) && length(input$make) > 0)
      df <- df %>% filter(Car.Make %in% input$make)

    if (!is.null(input$model) && length(input$model) > 0)
      df <- df %>% filter(Car.Model %in% input$model)

    if (!is.null(input$salesperson) && length(input$salesperson) > 0)
      df <- df %>% filter(Salesperson %in% input$salesperson)

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
    filtered() %>%
      group_by(Date) %>%
      summarise(
        Sales = n(),
        Revenue = sum(Sale.Price),
        .groups = "drop"
      )
  })

  output$trend_plot <- renderPlotly({

    df <- trend_df()

    if (input$metric == "Revenue") {
      p <- ggplot(df, aes(Date, Revenue)) +
        geom_line(color = "#2980B9", linewidth = 0.8) +
        labs(title = "Revenue Over Time", x = "Date", y = "Revenue") +
        theme_app
    } else {
      p <- ggplot(df, aes(Date, Sales)) +
        geom_line(color = "#E67E22", linewidth = 0.8) +
        labs(title = "Cars Sold Over Time", x = "Date", y = "Cars Sold") +
        theme_app
    }

    ggplotly(p) %>%
      layout(autosize = TRUE)  # responsive to container width
  })

  # Debounce the slider so dragging doesn't trigger a re-render on every pixel
  top_n_debounced <- debounce(reactive(input$top_n), 250)

  # Heavy step: aggregate + sort ALL salespeople — only re-runs when filters change,
  # NOT when the top_n slider moves
  salesperson_summary <- reactive({
    filtered() %>%
      group_by(Salesperson) %>%
      summarise(
        Revenue = sum(Sale.Price),
        Sales = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Revenue))
  })

  # Cheap step: just slice the top N from the already-sorted summary —
  # this is what re-runs when the slider moves, and it's fast
  output$salesperson_plot <- renderPlotly({

    n <- top_n_debounced()
    df <- salesperson_summary() %>% slice_head(n = n)

    p <- ggplot(df, aes(x = reorder(Salesperson, Revenue), y = Revenue)) +
      geom_col(fill = "#8E44AD") +
      coord_flip() +
      labs(title = paste("Top", n, "Salespeople by Revenue"),
           x = NULL, y = "Revenue") +
      theme_app

    ggplotly(p) %>%
      layout(autosize = TRUE)
  })

  # Market Analysis — Car Make distribution (all makes, typically a small set)
  output$make_plot <- renderPlotly({

    df <- filtered() %>%
      group_by(Car.Make) %>%
      summarise(Sales = n(), Revenue = sum(Sale.Price), .groups = "drop") %>%
      arrange(desc(Sales))

    p <- ggplot(df, aes(x = reorder(Car.Make, Sales), y = Sales, fill = Car.Make)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = MAKE_COLORS, guide = "none") +
      labs(title = "Sales by Car Make", x = NULL, y = "Cars Sold") +
      theme_app

    ggplotly(p) %>%
      layout(autosize = TRUE)
  })

  # Car Model distribution — small fixed set (5 models), no need for a top-N slider
  output$model_plot <- renderPlotly({

    df <- filtered() %>%
      group_by(Car.Model) %>%
      summarise(Sales = n(), Revenue = sum(Sale.Price), .groups = "drop") %>%
      arrange(desc(Sales))

    p <- ggplot(df, aes(x = reorder(Car.Model, Sales), y = Sales)) +
      geom_col(fill = "#27AE60") +
      coord_flip() +
      labs(title = "Sales by Car Model", x = NULL, y = "Cars Sold") +
      theme_app

    ggplotly(p) %>%
      layout(autosize = TRUE)
  })

  # Sales trend by make, aggregated monthly — one line per make
  make_trend_df <- reactive({
    filtered() %>%
      mutate(MonthStart = floor_date(Date, "month")) %>%
      group_by(MonthStart, Car.Make) %>%
      summarise(Sales = n(), .groups = "drop")
  })

  output$make_trend_plot <- renderPlotly({
    df <- make_trend_df()

    p <- ggplot(df, aes(MonthStart, Sales, color = Car.Make)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.3) +
      scale_color_manual(values = MAKE_COLORS) +
      labs(title = "Cars Sold per Month by Make", x = "Month", y = "Cars Sold", color = "Make") +
      theme_app

    ggplotly(p) %>%
      layout(autosize = TRUE)
  })

  # Pricing — histogram of sale prices
  output$price_hist <- renderPlotly({
    p <- ggplot(filtered(), aes(Sale.Price)) +
      geom_histogram(bins = 40, fill = "#F39C12", color = "white") +
      labs(title = "Distribution of Sale Prices", x = "Sale Price", y = "Count") +
      theme_app

    ggplotly(p) %>%
      layout(autosize = TRUE)
  })

  # Pricing — price vs commission scatter. With up to 2.5M filtered rows,
  # plotting every point would be slow and unreadable, so we sample.
  output$price_commission <- renderPlotly({
    df <- filtered()

    if (nrow(df) > 5000) {
      df <- df %>% slice_sample(n = 5000)
    }

    p <- ggplot(df, aes(Sale.Price, Commission.Earned)) +
      geom_point(alpha = 0.4, color = "#2980B9") +
      labs(title = "Price vs Commission Earned (sampled)", x = "Sale Price", y = "Commission Earned") +
      theme_app

    ggplotly(p) %>%
      layout(autosize = TRUE)
  })

  # Data Explorer — cap displayed rows since filtered data can still be huge (up to 2.5M)
  TABLE_ROW_CAP <- 5000

  output$table_note <- renderText({
    n <- nrow(filtered())
    if (n > TABLE_ROW_CAP) {
      paste0("Showing first ", comma(TABLE_ROW_CAP), " of ", comma(n),
             " matching rows. Narrow your filters to see a different slice.")
    } else {
      paste0("Showing all ", comma(n), " matching rows.")
    }
  })

  output$table <- renderDT({
    df <- filtered()
    if (nrow(df) > TABLE_ROW_CAP) {
      df <- df %>% slice_head(n = TABLE_ROW_CAP)
    }
    datatable(df, options = list(pageLength = 10), filter = "top")
  })
}

shinyApp(ui, server)
