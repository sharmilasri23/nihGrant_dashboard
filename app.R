

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  pacman, 
  rio,
  tidyverse,
  here,
  janitor,
  scales,
  dplyr,
  ggplot2,
  plotly,
  shiny,
  shinydashboard,
  shinythemes,
  flextable,
  purrr,
  DT)



# Call in data

source("scripts/data_cleaning.R")

# Shiny UI
ui <- fluidPage(
  theme = "bootstrap.css",
  includeCSS("www/styles.css"),
  
  # Panel 1: Funding Overview
  navbarPage(
    "Grant Dashboard",
    id = "main_navbar",
    
    tabPanel(
      "Funding Overview",
      div(
        class = "panel-container",
        style = "overflow-y: auto; max-height: 100vh;",  # Contain the panel within the viewport
        div(
          class = "card-container",
          style = "display: flex; justify-content: space-between; margin-bottom: 20px;",  # Adjust as needed
          div(
            class = "card",
            style = "background-color: #32CD32; flex: 1; min-width: 200px; margin: 0 10px; display: flex; flex-direction: column; justify-content: center;",
            h4("Allocation"),
            uiOutput("card_allocated")
          ),
          div(
            class = "card",
            style = "background-color: #FFC20E; flex: 1; min-width: 200px; margin: 0 10px; display: flex; flex-direction: column; justify-content: center;",
            h4("Expenditure"),
            uiOutput("card_spent")
          ),
          div(
            class = "card",
            style = "background-color: #1C86EE; flex: 1; min-width: 200px; margin: 0 10px; display: flex; flex-direction: column; justify-content: center;",
            h4("Balance"),
            uiOutput("card_left")
          )
        ),
        fluidRow(
          column(
            12,
            selectInput("funding_type", "Select Funding Type",
                        choices = c("Overall", "Cluster", "Institute"),
                        selected = "Overall")
          )
        ),
        fluidRow(
          column(
            12,
            plotOutput("time_series_plot", height = "250px")
          )
        ),
        fluidRow(
          column(
            12,
            DTOutput("data_table")
          )
        )
      )
    ),
    
    # Panel 2: Funding Analysis (updated)
    tabPanel(
      "Funding Analysis",
      fluidRow(
        column(
          4,
          selectInput("funding_entity", "Select Funding Entity",
                      choices = c("Cluster", "Institute"),
                      selected = "Cluster")
        ),
        column(
          4,
          selectInput("plot_type", "Select Plot Type",
                      choices = c("Absolute", "Stacked Percentage"),
                      selected = "Absolute")
        ),
        column(
          4,
          selectInput("plot_year", "Select Year",
                      choices = c("All", years),
                      selected = "All")
        )
      ),
      fluidRow(
        column(
          12,
          plotOutput("bar_plot")
        )
      )
    ),
    
    # Panel 3: Spending Breakdown (updated)
tabPanel(
  "Spending Breakdown",
  fluidRow(
    column(
      4,
      selectInput("table_selector", "Select Table",
                  choices = c("Perjalanan dan Sara Hidup [OS21000]",
                              "Perhubungan Dan Utiliti [OS23000]", 
                              "Sewaan [OS24000]", 
                              "Bahan-bahan Makanan dan Minuman [OS25000]",
                              "Bekalan Bahan Mentah dan Bahan-bahan Untuk penyelidikan [OS26000]",
                              "Bekalan dan Bahan Penyelidikan [OS27000]", 
                              "Selenggaraan Dan Pembaikan Kecil [OS28000]", 
                              "Khidmat Ikhtisas [35000]", 
                              "MySTEP [OS29000]", 
                              " Harta Modal [OS30000]"),
                  selected = "Perjalanan dan Sara Hidup [OS21000]")
    ),
    column(
      4,
      selectInput("funding_entity_selector", "Select Funding Entity",
                  choices = c("Cluster", "Institute"),
                  selected = "Cluster")
    ),
    column(
      4,
      selectInput("plot_year", "Select Year",
                  choices = c("All", years),
                  selected = "All")
    )
  ),
  fluidRow(
    column(
      6, 
      plotOutput("plot_display")
    ),
    column(
      6, 
      uiOutput("table_display")
    )
  )
)
    )
  )

# Shiny Server Side
server <- function(input, output, session) {
  
  source("scripts/panel1_plots.R")
  source("scripts/panel1_text.R")
  source("scripts/panel1_table.R")
  source("scripts/plots.R")
  source("scripts/tables.R")
  
  # Panel 1: Funding Overview - Time Series Plot (continued)
  output$time_series_plot <- renderPlot({
    if (input$funding_type == "Overall") {
      overall_df_plot
    } else if (input$funding_type == "Cluster") {
      kluster_df_plot
    } else if (input$funding_type == "Institute") {
      jabatan_df_plot
    }
  })
  
  # Static cards for funding overview
  output$card_allocated <- renderValueBox({
    valueBox(
      value = dollar(summary_cards$peruntukan, prefix = "MYR"), 
      subtitle = "Amount Allocated",
      icon = icon("money-bill", class = "custom-icon"),
      color = "green"
    )
  })
  
  output$card_spent <- renderValueBox({
    valueBox(
      value = dollar(summary_cards$belanja, prefix = "MYR"),
      subtitle = "Amount Spent",
      icon = icon("money-bill-wave", class = "custom-icon"),
      color = "yellow"
    )
  })
  
  output$card_left <- renderValueBox({
    valueBox(
      value = dollar(summary_cards$baki, prefix = "MYR"),
      subtitle = "Amount Left",
      icon = icon("wallet", class = "custom-icon"),
      color = "blue"
    )
  })
  
  # DT table
  output$data_table <- renderDT({
    datatable(
      merged_df2,
      extensions = 'Buttons',  # Enable extensions, including Buttons
      options = list(
        pageLength = 5,
        autoWidth = FALSE,
        responsive = TRUE,
        dom = 'Blfrtip',  # Layout: Buttons, Length changing, Filtering, Table, Information, Pagination
        buttons = c('csv')  # Define buttons, here only CSV
      ),
      class = 'cell-border stripe hover',  # Adds cell borders, striped rows, and hover effect
      rownames = FALSE  # Removes row names if not needed
    ) %>%
      formatStyle(
        columns = 1:ncol(merged_df2),  # Apply to all columns or specify columns like c('column1', 'column2')
        fontSize = '14px',  # Set font size
        fontWeight = 'normal'  # Set font weight
      )
  })
  
  # Panel 2: Funding Analysis (updated)
  output$bar_plot <- renderPlot({
    plot_data <- data
    
    if (input$funding_entity == "Cluster" && input$plot_type == "Absolute") {
      plot_merged_absolute
      
    } else if (input$funding_entity == "Cluster" && input$plot_type == "Stacked Percentage") {
      plot_merged_percent
      
    } else if (input$funding_entity == "Institute" && input$plot_type == "Absolute") {
      plot_ptj_absolute
      
    } else if (input$funding_entity == "Institute" && input$plot_type == "Stacked Percentage") {
      plot_ptj_percent
    }
  })
  
  # Panel 3: Spending Breakdown (updated)
  # Render your flextable
  output$table_display <- renderUI({
    if (input$funding_entity_selector == "Cluster") {
      if (input$table_selector == "Perjalanan dan Sara Hidup [OS21000]") {
        summarize_table1 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Perhubungan Dan Utiliti [OS23000]") {
        summarize_table2 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Sewaan [OS24000]") {
        summarize_table3 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Bahan-bahan Makanan dan Minuman [OS25000]") {
        summarize_table4 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Bekalan Bahan Mentah dan Bahan-bahan Untuk penyelidikan [OS26000]") {
        summarize_table5 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Bekalan dan Bahan Penyelidikan [OS27000]") {
        summarize_table6 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Selenggaraan Dan Pembaikan Kecil [OS28000]") {
        summarize_table7 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Khidmat Ikhtisas [35000]") {
        summarize_table8 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "MySTEP [OS29000]") {
        summarize_table9 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == " Harta Modal [OS30000]") {
        summarize_table10 %>%
          autofit() %>%
          htmltools_value()
      }
    } else if (input$funding_entity_selector == "Institute") {
      if (input$table_selector == "Perjalanan dan Sara Hidup [OS21000]") {
        ptj_table1 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Perhubungan Dan Utiliti [OS23000]") {
        ptj_table2 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Sewaan [OS24000]") {
        ptj_table3 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Bahan-bahan Makanan dan Minuman [OS25000]") {
        ptj_table4 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Bekalan Bahan Mentah dan Bahan-bahan Untuk penyelidikan [OS26000]") {
        ptj_table5 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Bekalan dan Bahan Penyelidikan [OS27000]") {
        ptj_table6 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Selenggaraan Dan Pembaikan Kecil [OS28000]") {
        ptj_table7 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "Khidmat Ikhtisas [35000]") {
        ptj_table8 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == "MySTEP [OS29000]") {
        ptj_table9 %>%
          autofit() %>%
          htmltools_value()
      } else if (input$table_selector == " Harta Modal [OS30000]") {
        ptj_table10 %>%
          autofit() %>%
          htmltools_value()
      }
    }
  })
  
  output$plot_display <- renderPlot({
    if (input$funding_entity_selector == "Cluster") {
      if (input$table_selector == "Perjalanan dan Sara Hidup [OS21000]") {
        agihan1
      } else if (input$table_selector == "Perhubungan Dan Utiliti [OS23000]") {
        agihan2
      } else if (input$table_selector == "Sewaan [OS24000]") {
        agihan3
      } else if (input$table_selector == "Bahan-bahan Makanan dan Minuman [OS25000]") {
        agihan4
      } else if (input$table_selector == "Bekalan Bahan Mentah dan Bahan-bahan Untuk penyelidikan [OS26000]") {
        agihan5
      } else if (input$table_selector == "Bekalan dan Bahan Penyelidikan [OS27000]") {
        agihan6
      } else if (input$table_selector == "Selenggaraan Dan Pembaikan Kecil [OS28000]") {
        agihan7
      } else if (input$table_selector == "Khidmat Ikhtisas [35000]") {
        agihan8
      } else if (input$table_selector == "MySTEP [OS29000]") {
        agihan9
      } else if (input$table_selector == " Harta Modal [OS30000]") {
        agihan10
      }
    } else if (input$funding_entity_selector == "Institute") {
      if (input$table_selector == "Perjalanan dan Sara Hidup [OS21000]") {
        ptj1
      } else if (input$table_selector == "Perhubungan Dan Utiliti [OS23000]") {
        ptj2
      } else if (input$table_selector == "Sewaan [OS24000]") {
        ptj3
      } else if (input$table_selector == "Bahan-bahan Makanan dan Minuman [OS25000]") {
        ptj4
      } else if (input$table_selector == "Bekalan Bahan Mentah dan Bahan-bahan Untuk penyelidikan [OS26000]") {
        ptj5
      } else if (input$table_selector == "Bekalan dan Bahan Penyelidikan [OS27000]") {
        ptj6
      } else if (input$table_selector == "TSelenggaraan Dan Pembaikan Kecil [OS28000]") {
        ptj7
      } else if (input$table_selector == "Khidmat Ikhtisas [35000]") {
        ptj8
      } else if (input$table_selector == "MySTEP [OS29000]") {
        ptj9
      } else if (input$table_selector == " Harta Modal [OS30000]") {
        ptj10
      }
    }
  })

observeEvent(input$next_page_btn, {
  updateNavbarPage(session, "Dashboard", selected = "Page 2")
})
}

# Run the application
shinyApp(ui = ui, server = server)