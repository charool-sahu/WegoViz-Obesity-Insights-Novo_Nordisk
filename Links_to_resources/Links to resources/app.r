# app.R
library(shiny)
library(readxl)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(bslib)

# --- Load Excel Data ---
data <- read_excel("/home/ibab/Book.xlsx")

# Detect numeric and categorical columns automatically
num_cols <- names(data)[sapply(data, is.numeric)]
cat_cols <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]

# --- UI ---
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),  # modern colorful theme
  tags$head(
    tags$style(
      HTML("
        body {
          background-image: url('https://images.unsplash.com/photo-1529101091764-c3526daf38fe');
          background-size: cover;
          background-attachment: fixed;
          background-position: center;
        }
        .well {
          background: rgba(255, 255, 255, 0.8);
        }
        .tab-content {
          background: rgba(255, 255, 255, 0.9);
          padding: 20px;
          border-radius: 10px;
        }
      ")
    )
  ),
  
  titlePanel("Hackathon Dashboard Demo"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Interactive Filters & Plots"),
      
      selectInput("bar_col", "Bar Plot Column (Categorical):", choices = cat_cols, selected = cat_cols[1]),
      selectInput("hist_col", "Histogram Column (Numeric):", choices = num_cols, selected = num_cols[1]),
      selectInput("pie_col", "Pie Chart Column (Categorical):", choices = cat_cols, selected = cat_cols[1]),
      
      selectInput("filter_col", "Filter Column:", choices = c("None", names(data)), selected = "None"),
      uiOutput("filter_ui")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📊 Data Table", DTOutput("table")),
        tabPanel("📈 Summary", verbatimTextOutput("summary")),
        tabPanel("📌 Bar Plot", plotlyOutput("barplot")),
        tabPanel("📌 Histogram", plotlyOutput("histogram")),
        tabPanel("📌 Pie Chart", plotlyOutput("piechart"))
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # Dynamic filter UI
  output$filter_ui <- renderUI({
    req(input$filter_col)
    if (input$filter_col != "None") {
      selectInput("filter_val", paste("Select", input$filter_col),
                  choices = unique(data[[input$filter_col]]),
                  selected = unique(data[[input$filter_col]])[1])
    }
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    df <- data
    if (!is.null(input$filter_val) && input$filter_col != "None") {
      df <- df %>% filter(.data[[input$filter_col]] == input$filter_val)
    }
    df
  })
  
  # Data Table
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  # Summary
  output$summary <- renderPrint({
    summary(filtered_data())
  })
  
  # Bar Plot
  output$barplot <- renderPlotly({
    req(input$bar_col)
    p <- ggplot(filtered_data(), aes_string(x = input$bar_col, fill = input$bar_col)) +
      geom_bar() +
      theme_minimal() +
      labs(title = paste("Bar Plot of", input$bar_col), x = input$bar_col, y = "Count") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(p)
  })
  
  # Histogram
  output$histogram <- renderPlotly({
    req(input$hist_col)
    p <- ggplot(filtered_data(), aes_string(x = input$hist_col)) +
      geom_histogram(bins = 15, color = "black", fill = "steelblue") +
      theme_minimal() +
      labs(title = paste("Histogram of", input$hist_col), x = input$hist_col, y = "Frequency")
    ggplotly(p)
  })
  
  # Pie Chart
  output$piechart <- renderPlotly({
    req(input$pie_col)
    df <- filtered_data() %>%
      count(.data[[input$pie_col]]) %>%
      rename(category = 1, count = n)
    
    plot_ly(df, labels = ~category, values = ~count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            marker = list(colors = RColorBrewer::brewer.pal(n = nrow(df), name = "Set3")))
  })
}

# --- RUN APP ---
shinyApp(ui = ui, server = server)
