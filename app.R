# Budget Tracker Shiny App with Direct MySQL Connection
# File: app.R

# Load environment variables explicitly
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Load required libraries
library(shiny)
library(shinydashboard)
library(DBI)
library(RMySQL)
library(pool)  # For connection pooling
library(DT)
library(plotly)
library(tidyverse)
library(lubridate)

# Budget categories and buyers
BUDGET_CATEGORIES <- c(
  "Housing", "Food", "Clothing", "Education", "Transportation", 
  "Communications", "Health", "Recreation", "Other", "Debt", 
  "Fast Offering", "Tithing", "Income"
)

BUYERS <- c("John", "Kathryn", "Elisa", "Rebekah", "Sarah", "Sophia")

# Database connection pool
# Using pool for better connection management
# Replace the pool creation section with this for better error reporting:
pool <- tryCatch({
  dbPool(
    drv = RMySQL::MySQL(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT"))
  )
}, error = function(e) {
  cat("Database connection error details:\n")
  cat("DB_NAME:", Sys.getenv("DB_NAME"), "\n")
  cat("DB_HOST:", Sys.getenv("DB_HOST"), "\n")
  cat("DB_USER:", Sys.getenv("DB_USER"), "\n")
  cat("DB_PORT:", Sys.getenv("DB_PORT"), "\n")
  cat("Error message:", e$message, "\n")
  return(NULL)
})

# Ensure connection closes when app stops
onStop(function() {
  poolClose(pool)
})

# Function to create table if it doesn't exist
initialize_database <- function() {
  tryCatch({
    dbExecute(pool, "
      CREATE TABLE IF NOT EXISTS budget_transactions (
        id INT AUTO_INCREMENT PRIMARY KEY,
        date DATE NOT NULL,
        description VARCHAR(255) NOT NULL,
        amount DECIMAL(10, 2) NOT NULL,
        vendor VARCHAR(255),
        budget_category VARCHAR(50) NOT NULL,
        buyer VARCHAR(50) NOT NULL,
        notes TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
    return(TRUE)
  }, error = function(e) {
    print(paste("Database initialization error:", e$message))
    return(FALSE)
  })
}

# Initialize database
db_initialized <- initialize_database()

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Budget Tracker"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Transactions", tabName = "transactions", icon = icon("list")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Add Entry", tabName = "add_entry", icon = icon("plus-circle"))
    ),
    
    hr(),
    
    h4("Filters", style = "padding-left: 15px;"),
    
    dateRangeInput(
      "date_range",
      "Date Range:",
      start = floor_date(Sys.Date(), "month"),
      end = Sys.Date(),
      width = "100%"
    ),
    
    selectizeInput(
      "filter_category",
      "Categories:",
      choices = c("All", BUDGET_CATEGORIES),
      selected = "All",
      multiple = TRUE,
      width = "100%"
    ),
    
    selectizeInput(
      "filter_buyer",
      "Buyers:",
      choices = c("All", BUYERS),
      selected = "All",
      multiple = TRUE,
      width = "100%"
    ),
    
    actionButton(
      "refresh_data",
      "Refresh Data",
      icon = icon("refresh"),
      width = "90%",
      style = "margin: 10px;"
    ),
    
    downloadButton(
      "download_data",
      "Download Data",
      width = "90%",
      style = "margin: 10px;"
    )
  ),
  
  dashboardBody(
    # Add custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .small-box {
          border-radius: 5px;
        }
        .info-box {
          min-height: 50px;
        }
      "))
    ),
    
    # Show database connection status
    conditionalPanel(
      condition = "!output.db_connected",
      div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        "Database connection failed. Please check your credentials."
      )
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_income"),
          valueBoxOutput("total_expenses"),
          valueBoxOutput("net_amount")
        ),
        
        fluidRow(
          infoBoxOutput("current_month_income"),
          infoBoxOutput("current_month_expenses"),
          infoBoxOutput("transaction_count")
        ),
        
        fluidRow(
          box(
            title = "Monthly Trend",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("monthly_trend_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Category Breakdown",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("category_pie_chart", height = "350px")
          ),
          box(
            title = "Spending by Buyer",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("buyer_bar_chart", height = "350px")
          )
        )
      ),
      
      # Transactions Tab
      tabItem(
        tabName = "transactions",
        fluidRow(
          box(
            title = "Budget Entries",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("budget_table"),
            br(),
            actionButton(
              "delete_selected",
              "Delete Selected",
              icon = icon("trash"),
              class = "btn-danger"
            )
          )
        )
      ),
      
      # Analysis Tab
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "Monthly Category Totals",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("category_monthly_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Top 10 Vendors",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            DTOutput("top_vendors_table")
          ),
          box(
            title = "Category Summary",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            DTOutput("category_summary_table")
          )
        )
      ),
      
      # Add Entry Tab
      tabItem(
        tabName = "add_entry",
        fluidRow(
          box(
            title = "Add New Entry",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            dateInput(
              "entry_date",
              "Date:",
              value = Sys.Date(),
              width = "100%"
            ),
            
            textInput(
              "description",
              "Description:",
              placeholder = "Enter description",
              width = "100%"
            ),
            
            numericInput(
              "amount",
              "Amount ($):",
              value = 0,
              step = 0.01,
              width = "100%"
            ),
            
            textInput(
              "vendor",
              "Vendor:",
              placeholder = "Enter vendor name",
              width = "100%"
            ),
            
            selectInput(
              "budget_category",
              "Budget Category:",
              choices = BUDGET_CATEGORIES,
              width = "100%"
            ),
            
            selectInput(
              "buyer",
              "Buyer:",
              choices = BUYERS,
              width = "100%"
            ),
            
            textAreaInput(
              "notes",
              "Notes:",
              placeholder = "Additional notes (optional)",
              rows = 3,
              width = "100%"
            ),
            
            actionButton(
              "add_entry",
              "Add Entry",
              icon = icon("plus"),
              class = "btn-success btn-block",
              width = "100%"
            )
          ),
          
          box(
            title = "Recent Entries",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            DTOutput("recent_entries_table")
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Check database connection
  output$db_connected <- reactive({
    db_initialized
  })
  outputOptions(output, "db_connected", suspendWhenHidden = FALSE)
  
  # Reactive values
  values <- reactiveValues(
    data_trigger = 0
  )
  
  # Fetch budget data from database
  budget_data <- reactive({
    values$data_trigger
    
    tryCatch({
      # Build query with filters
      query <- "SELECT * FROM budget_transactions WHERE 1=1"
      params <- list()
      
      # Date range filter
      if (!is.null(input$date_range)) {
        query <- paste0(query, " AND date >= ? AND date <= ?")
        params <- append(params, list(
          as.character(input$date_range[1]),
          as.character(input$date_range[2])
        ))
      }
      
      # Category filter
      if (!"All" %in% input$filter_category && length(input$filter_category) > 0) {
        placeholders <- paste0(rep("?", length(input$filter_category)), collapse = ",")
        query <- paste0(query, " AND budget_category IN (", placeholders, ")")
        params <- append(params, as.list(input$filter_category))
      }
      
      # Buyer filter
      if (!"All" %in% input$filter_buyer && length(input$filter_buyer) > 0) {
        placeholders <- paste0(rep("?", length(input$filter_buyer)), collapse = ",")
        query <- paste0(query, " AND buyer IN (", placeholders, ")")
        params <- append(params, as.list(input$filter_buyer))
      }
      
      query <- paste0(query, " ORDER BY date DESC, id DESC")
      
      # Execute query
      if (length(params) > 0) {
        result <- dbGetQuery(pool, query, params = params)
      } else {
        result <- dbGetQuery(pool, query)
      }
      
      # Convert to tibble and parse dates
      if (nrow(result) > 0) {
        result %>%
          as_tibble() %>%
          mutate(
            date = as.Date(date),
            amount = as.numeric(amount)
          )
      } else {
        tibble()
      }
      
    }, error = function(e) {
      showNotification(paste("Error fetching data:", e$message), type = "error")
      tibble()
    })
  })
  
  # Refresh data
  observeEvent(input$refresh_data, {
    values$data_trigger <- values$data_trigger + 1
    showNotification("Data refreshed!", type = "info", duration = 2)
  })
  
  # Value boxes
  output$total_income <- renderValueBox({
    data <- budget_data()
    income <- if(nrow(data) > 0) sum(data$amount[data$amount > 0], na.rm = TRUE) else 0
    
    valueBox(
      value = sprintf("$%,.2f", income),
      subtitle = "Total Income",
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  output$total_expenses <- renderValueBox({
    data <- budget_data()
    expenses <- if(nrow(data) > 0) abs(sum(data$amount[data$amount < 0], na.rm = TRUE)) else 0
    
    valueBox(
      value = sprintf("$%,.2f", expenses),
      subtitle = "Total Expenses",
      icon = icon("arrow-down"),
      color = "red"
    )
  })
  
  output$net_amount <- renderValueBox({
    data <- budget_data()
    net <- if(nrow(data) > 0) sum(data$amount, na.rm = TRUE) else 0
    
    valueBox(
      value = sprintf("$%,.2f", net),
      subtitle = "Net Amount",
      icon = icon("balance-scale"),
      color = if(net >= 0) "blue" else "yellow"
    )
  })
  
  # Info boxes for current month
  output$current_month_income <- renderInfoBox({
    data <- budget_data()
    current_month <- floor_date(Sys.Date(), "month")
    
    income <- if(nrow(data) > 0) {
      data %>%
        filter(date >= current_month, amount > 0) %>%
        summarise(total = sum(amount, na.rm = TRUE)) %>%
        pull(total)
    } else 0
    
    infoBox(
      "This Month Income",
      sprintf("$%,.2f", income),
      icon = icon("calendar-plus"),
      color = "green",
      fill = TRUE
    )
  })
  
  output$current_month_expenses <- renderInfoBox({
    data <- budget_data()
    current_month <- floor_date(Sys.Date(), "month")
    
    expenses <- if(nrow(data) > 0) {
      data %>%
        filter(date >= current_month, amount < 0) %>%
        summarise(total = abs(sum(amount, na.rm = TRUE))) %>%
        pull(total)
    } else 0
    
    infoBox(
      "This Month Expenses",
      sprintf("$%,.2f", expenses),
      icon = icon("calendar-minus"),
      color = "red",
      fill = TRUE
    )
  })
  
  output$transaction_count <- renderInfoBox({
    data <- budget_data()
    count <- nrow(data)
    
    infoBox(
      "Total Transactions",
      count,
      icon = icon("list"),
      color = "blue",
      fill = TRUE
    )
  })
  
  # Monthly trend plot
  output$monthly_trend_plot <- renderPlotly({
    data <- budget_data()
    
    if (nrow(data) == 0) {
      plot_ly() %>%
        layout(
          title = "No data available",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    } else {
      monthly_data <- data %>%
        mutate(month = floor_date(date, "month")) %>%
        group_by(month) %>%
        summarise(
          Income = sum(amount[amount > 0], na.rm = TRUE),
          Expenses = abs(sum(amount[amount < 0], na.rm = TRUE)),
          Net = sum(amount, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(Income, Expenses, Net), names_to = "Type", values_to = "Amount")
      
      colors <- c("Income" = "#28a745", "Expenses" = "#dc3545", "Net" = "#007bff")
      
      plot_ly(monthly_data, x = ~month, y = ~Amount, color = ~Type, 
              colors = colors, type = 'scatter', mode = 'lines+markers',
              line = list(width = 3), marker = list(size = 8)) %>%
        layout(
          title = "Monthly Income vs Expenses",
          xaxis = list(title = "Month"),
          yaxis = list(title = "Amount ($)"),
          hovermode = "x unified",
          showlegend = TRUE
        )
    }
  })
  
  # Category pie chart
  output$category_pie_chart <- renderPlotly({
    data <- budget_data()
    
    if (nrow(data) == 0) {
      plot_ly() %>%
        layout(title = "No data available")
    } else {
      category_data <- data %>%
        filter(amount < 0) %>%
        mutate(amount = abs(amount)) %>%
        group_by(budget_category) %>%
        summarise(Total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(Total))
      
      plot_ly(category_data, labels = ~budget_category, values = ~Total, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(line = list(color = '#FFFFFF', width = 2))) %>%
        layout(
          title = "Spending by Category",
          showlegend = TRUE
        )
    }
  })
  
  # Buyer bar chart
  output$buyer_bar_chart <- renderPlotly({
    data <- budget_data()
    
    if (nrow(data) == 0) {
      plot_ly() %>%
        layout(title = "No data available")
    } else {
      buyer_data <- data %>%
        filter(amount < 0) %>%
        mutate(amount = abs(amount)) %>%
        group_by(buyer) %>%
        summarise(Total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
        arrange(Total)  # Arrange for horizontal bar chart
      
      plot_ly(buyer_data, x = ~Total, y = ~buyer, type = 'bar', orientation = 'h',
              marker = list(color = '#3498db')) %>%
        layout(
          title = "Spending by Buyer",
          xaxis = list(title = "Total Spent ($)"),
          yaxis = list(title = ""),
          margin = list(l = 100)
        )
    }
  })
  
  # Budget table
  output$budget_table <- renderDT({
    data <- budget_data()
    
    if (nrow(data) > 0) {
      data %>%
        mutate(
          date = format(date, "%Y-%m-%d"),
          amount = sprintf("$%.2f", amount)
        ) %>%
        select(Date = date, Description = description, Amount = amount, 
               Vendor = vendor, Category = budget_category, Buyer = buyer, Notes = notes)
    } else {
      tibble(Message = "No data available")
    }
  }, selection = 'multiple', options = list(
    pageLength = 15,
    lengthMenu = c(10, 15, 25, 50, 100),
    scrollX = TRUE
  ))
  
  # Add new entry
  observeEvent(input$add_entry, {
    req(input$description, input$amount)
    
    if (input$description == "" || input$amount == 0) {
      showNotification("Please fill in description and amount", type = "warning")
      return()
    }
    
    # Make expenses negative
    amount <- input$amount
    if (input$budget_category != "Income" && amount > 0) {
      amount <- -amount
    }
    
    tryCatch({
      # Insert into database
      query <- "INSERT INTO budget_transactions (date, description, amount, vendor, budget_category, buyer, notes) 
                VALUES (?, ?, ?, ?, ?, ?, ?)"
      
      dbExecute(pool, query, params = list(
        as.character(input$entry_date),
        input$description,
        amount,
        ifelse(is.null(input$vendor) || input$vendor == "", NA, input$vendor),
        input$budget_category,
        input$buyer,
        ifelse(is.null(input$notes) || input$notes == "", NA, input$notes)
      ))
      
      showNotification("Entry added successfully!", type = "success", duration = 3)
      
      # Clear form
      updateTextInput(session, "description", value = "")
      updateNumericInput(session, "amount", value = 0)
      updateTextInput(session, "vendor", value = "")
      updateTextAreaInput(session, "notes", value = "")
      
      # Refresh data
      values$data_trigger <- values$data_trigger + 1
      
    }, error = function(e) {
      showNotification(paste("Error adding entry:", e$message), type = "error")
    })
  })
  
  # Delete selected entries
  observeEvent(input$delete_selected, {
    selected_rows <- input$budget_table_rows_selected
    
    if (length(selected_rows) == 0) {
      showNotification("No rows selected", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Are you sure you want to delete", length(selected_rows), "entries?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
  })
  
  # Confirm deletion
  observeEvent(input$confirm_delete, {
    selected_rows <- input$budget_table_rows_selected
    data <- budget_data()
    
    if (nrow(data) > 0 && length(selected_rows) > 0) {
      selected_ids <- data$id[selected_rows]
      
      tryCatch({
        # Delete from database
        placeholders <- paste0(rep("?", length(selected_ids)), collapse = ",")
        query <- paste0("DELETE FROM budget_transactions WHERE id IN (", placeholders, ")")
        
        dbExecute(pool, query, params = as.list(selected_ids))
        
        showNotification(paste("Deleted", length(selected_ids), "entries"), type = "success")
        
        # Refresh data
        values$data_trigger <- values$data_trigger + 1
        
        removeModal()
        
      }, error = function(e) {
        showNotification(paste("Error deleting entries:", e$message), type = "error")
      })
    }
  })
  
  # Monthly category plot
  output$category_monthly_plot <- renderPlotly({
    data <- budget_data()
    
    if (nrow(data) == 0) {
      plot_ly() %>%
        layout(title = "No data available")
    } else {
      monthly_category <- data %>%
        filter(amount < 0) %>%
        mutate(
          amount = abs(amount),
          month = floor_date(date, "month")
        ) %>%
        group_by(month, budget_category) %>%
        summarise(Total = sum(amount, na.rm = TRUE), .groups = "drop")
      
      # Create a complete grid of months and categories
      all_months <- seq(min(monthly_category$month), max(monthly_category$month), by = "month")
      all_categories <- unique(monthly_category$budget_category)
      
      complete_data <- expand_grid(month = all_months, budget_category = all_categories) %>%
        left_join(monthly_category, by = c("month", "budget_category")) %>%
        replace_na(list(Total = 0))
      
      plot_ly(complete_data, x = ~month, y = ~Total, color = ~budget_category, 
              type = 'bar') %>%
        layout(
          title = "Monthly Spending by Category",
          xaxis = list(title = "Month"),
          yaxis = list(title = "Amount ($)"),
          barmode = "stack",
          showlegend = TRUE
        )
    }
  })
  
  # Top vendors table
  output$top_vendors_table <- renderDT({
    data <- budget_data()
    
    if (nrow(data) > 0) {
      vendor_summary <- data %>%
        filter(amount < 0, !is.na(vendor), vendor != "") %>%
        mutate(amount = abs(amount)) %>%
        group_by(Vendor = vendor) %>%
        summarise(
          `Total Spent` = sum(amount, na.rm = TRUE),
          `Transactions` = n(),
          .groups = "drop"
        ) %>%
        arrange(desc(`Total Spent`)) %>%
        head(10) %>%
        mutate(`Total Spent` = sprintf("$%.2f", `Total Spent`))
      
      if (nrow(vendor_summary) > 0) {
        vendor_summary
      } else {
        tibble(Message = "No vendor data available")
      }
    } else {
      tibble(Message = "No data available")
    }
  }, options = list(pageLength = 10, dom = 't'))
  
  # Category summary table
  output$category_summary_table <- renderDT({
    data <- budget_data()
    
    if (nrow(data) > 0) {
      data %>%
        filter(amount < 0) %>%
        mutate(amount = abs(amount)) %>%
        group_by(Category = budget_category) %>%
        summarise(
          `Total` = sum(amount, na.rm = TRUE),
          `Average` = mean(amount, na.rm = TRUE),
          `Count` = n(),
          .groups = "drop"
        ) %>%
        arrange(desc(Total)) %>%
        mutate(
          Total = sprintf("$%.2f", Total),
          Average = sprintf("$%.2f", Average)
        )
    } else {
      tibble(Message = "No data available")
    }
  }, options = list(pageLength = 15, dom = 't'))
  
  # Recent entries table
  output$recent_entries_table <- renderDT({
    data <- budget_data()
    
    if (nrow(data) > 0) {
      data %>%
        arrange(desc(date), desc(id)) %>%
        head(10) %>%
        mutate(
          date = format(date, "%Y-%m-%d"),
          amount = sprintf("$%.2f", amount)
        ) %>%
        select(Date = date, Description = description, Amount = amount, Category = budget_category)
    } else {
      tibble(Message = "No recent entries")
    }
  }, options = list(pageLength = 10, dom = 't'))
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("budget_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- budget_data()
      if (nrow(data) > 0) {
        data %>%
          select(-id, -created_at) %>%
          arrange(desc(date)) %>%
          write_csv(file)
      } else {
        write_csv(tibble(Message = "No data to download"), file)
      }
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)