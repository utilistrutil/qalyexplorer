###TO DO - 
### Write gets for the data
### Make country drop down responsive
###Code actual math lol

library(shiny)
library(dplyr)

# SET ---------------------------------------------------------------------
username <- "JTate"

# Load or create QALY database

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("QALY Explorer"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("calculator_sections"),
      actionButton("add_calc", "Add Calculation", class = "btn-primary", style = "width: 100%; margin-top: 10px;"),
      actionButton("calc_button", "Calculate QALYs", class = "btn-success", style = "width: 100%; margin-top: 10px;")
    ),
    mainPanel(
      h3("QALY Output", style = "text-align: center; margin-bottom: 20px;"),
      uiOutput("QALY_results")
    )
  )
)

# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  setwd(paste0("C:/Users/", username, "/Global Impact Investing Network/Document Center - RESEARCH/Projects - Performance/Impact Performance Concepts/2023 Fund Level Methodology/Content development/Due diligence and selection/JT Hackathon/QALY/"))
  
  calculations <- reactiveValues(data = list(list()))  # Initialize with one calculator
  
  # Render calculator sections dynamically
  output$calculator_sections <- renderUI({
    tagList(
      lapply(seq_along(calculations$data), function(i) {
        fluidRow(
          column(12, textInput(paste0("Name_", i), "Investment Name:", value = "")),
          column(6, selectInput(paste0("Region_", i), "Investment Region:", choices = unique(qaly_database$region_name))),
          column(6, selectInput(paste0("Country_", i), "Investment Country:", choices = NULL)),
          column(12, selectInput(paste0("Category_", i), "Impact Category:", choices = unique(qaly_database$Impact_category))),
          column(6, numericInput(paste0("Beneficiaries_", i), "Beneficiaries Served:", value = 1, min = 0.1)),
          column(6, selectInput(paste0("Needs_", i), "Percentage of Needs Met:", choices = c(0.25, 0.5, 0.75))),
          column(12, checkboxInput(paste0("Lives_", i), "This Investment Saves Lives", value = FALSE)),
          uiOutput(paste0("Saved_lyg_inputs_", i)),
          tags$hr()
        )
      })
    )
  })
  
  # Add new calculation section
  observeEvent(input$add_calc, {
    calculations$data[[length(calculations$data) + 1]] <- list()
  })
  
  # Dynamic input for Lives Saved and Life-Years Gained (LYG)
  observe({
    lapply(seq_along(calculations$data), function(i) {
      output[[paste0("Saved_lyg_inputs_", i)]] <- renderUI({
        if (input[[paste0("Lives_", i)]]) {
          tagList(
            numericInput(paste0("Saved_", i), "Lives Saved:", value = 0, min = 0),
            selectInput(paste0("LYG_", i), "Increase in Life-Years:", choices = c(1, 5, 10, 20))
          )
        }
      })
    })
  })
  
  # Calculate QALYs
  observeEvent(input$calc_button, {
    results <- lapply(seq_along(calculations$data), function(i) {
      region <- input[[paste0("Region_", i)]]
      country <- input[[paste0("Country_", i)]]
      category <- input[[paste0("Category_", i)]]
      beneficiaries <- input[[paste0("Beneficiaries_", i)]]
      needs_met <- input[[paste0("Needs_", i)]]
      lives_saved <- input[[paste0("Saved_", i)]]
      lifespan <- input[[paste0("LYG_", i)]]
      
      if (is.null(region) || is.null(country) || is.null(category)) {
        return("Missing required inputs")
      }
      
      selected_row <- qaly_database %>%
        filter(region_name == region, country == country, Impact_category == category) %>%
        select(value)
      
      if (nrow(selected_row) > 0) {
        qalys <- as.numeric(selected_row$value) * beneficiaries * needs_met
        if (!is.null(lives_saved) && !is.na(lives_saved)) {
          qalys <- qalys + (lives_saved * lifespan)
        }
        return(paste("QALY Output:", formatC(qalys, format = "f", digits = 2, big.mark = ",")))
      } else {
        return("No matching data found.")
      }
    })
    
    output$QALY_results <- renderUI({
      tagList(
        lapply(results, function(res) {
          div(style = "margin-bottom: 10px;", res)
        })
      )
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)