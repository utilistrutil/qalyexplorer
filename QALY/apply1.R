library(shiny)
library(dplyr)

## Known issues
####Fund names not refreshing
####Long lag times on adding investments (mostly due to issues with presets)
####Code to render UI for fund sums is written but not appearing?
####In general, probably needs much better error handling.

# SET ---------------------------------------------------------------------

username <- "JTate"

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("QALY Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 6,  # Adjust width to 50% of the screen width
      uiOutput("calculator_sections")
    ),
    mainPanel(
      width = 6,
      h3("QALY Output", style = "text-align: center; margin-bottom: 20px;"),
      actionButton("calculate_sums", "Calculate Sums", class = "btn-primary", style = "width: 100%; margin-top: 20px;"),
      uiOutput("fund_total_qalys")
    )
  )
)

server <- function(input, output, session) {
  
  setwd(paste0("C:/Users/", username, "/Global Impact Investing Network/Document Center - RESEARCH/Projects - Performance/Impact Performance Concepts/2023 Fund Level Methodology/Content development/QALY tool/JT Hackathon/QALY/"))
  load("qaly_database.RData")
  
  # Initialize reactive values
  calculations <- reactiveValues(
    data = data.frame(
      Fund = rep(1:2, each = 10),
      Investment = rep(1:10, 2),
      Rendered = rep(0, 20),
      Details = I(vector("list", 20))
    ),
    clicks = c(1, 0)  # Counter for each fund
  )
  
  # Render the fund boxes
  output$calculator_sections <- renderUI({
    tagList(
      lapply(1:2, function(i) {
        div(style = "border: 3px solid #333; background-color: #f2f2f2; padding: 10px; margin-bottom: 20px; border-radius: 5px; height: auto; display: flex; flex-direction: column; justify-content: flex-end;",  # fund box
            fluidRow(
              column(12, textInput(paste0("Fund_Name_", i), "Fund Name:", value = "")),
              actionButton(paste0("add_investment_", i), "Add Investment", class = "btn-primary", style = "width: 100%; margin-top: 10px;")
            ),
            uiOutput(paste0("investment_sections_", i))  # Dynamic investments within each fund
        )
      })
    )
  })
  
  # Dynamically render investments for each fund
  observe({
    lapply(1:2, function(i) {
      output[[paste0("investment_sections_", i)]] <- renderUI({
        tagList(
          lapply(1:calculations$clicks[i], function(j) {
            index <- (i - 1) * 10 + j
            if (calculations$data$Rendered[index] == 1) {
              investment <- calculations$data$Details[[index]]
              div(style = "border: 2px solid #ccc; background-color: #fff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",  # investment box
                  fluidRow(
                    column(12, textInput(paste0("Investment_Name_", i, "_", j), "Investment Name:", value = investment$Investment_Name)),
                    column(12, selectInput(paste0("Country_", i, "_", j), "Investment Country:", choices = unique(qaly_database$country), selected = investment$Country)),
                    column(12, selectInput(paste0("Impact_category_", i, "_", j), "Impact Category:", choices = unique(qaly_database$Impact_category), selected = investment$Impact_category)),
                    column(6, numericInput(paste0("Beneficiaries_", i, "_", j), "Beneficiaries Served:", value = investment$Beneficiaries)),
                    column(6, selectInput(paste0("Needs_", i, "_", j), "Percentage of Needs Met:", choices = c(0.25, 0.5, 0.75), selected = investment$Needs)),
                    column(12, checkboxInput(paste0("Saves_Lives_", i, "_", j), "This investment saves lives", value = investment$Saves_Lives)),
                    conditionalPanel(
                      condition = paste0("input.Saves_Lives_", i, "_", j),
                      column(6, numericInput(paste0("Lives_Saved_", i, "_", j), "Lives Saved:", value = investment$Lives_Saved, min = 0)),
                      column(6, selectInput(paste0("Life_Years_", i, "_", j), "Life Years:", choices = c(1, 5, 25), selected = investment$Life_Years))
                    )
                  )
              )
            }
          })
        )
      })
    })
  })
  
  # Add investment observer
  observe({
    lapply(1:2, function(i) {
      observeEvent(input[[paste0("add_investment_", i)]], {
        if (calculations$clicks[i] < 10) {
          calculations$clicks[i] <- calculations$clicks[i] + 1
          index <- (i - 1) * 10 + calculations$clicks[i]
          calculations$data$Rendered[index] <- 1
        }
      })
    })
  })
  
  # Save input data to reactive values
  observeEvent(input$calculate_sums, {
    lapply(1:2, function(i) {
      lapply(1:10, function(j) {
        index <- (i - 1) * 10 + j
        if (calculations$data$Rendered[index] == 1) {
          investment <- list(
            Investment_Name = input[[paste0("Investment_Name_", i, "_", j)]],
            Country = input[[paste0("Country_", i, "_", j)]],
            Impact_category = input[[paste0("Impact_category_", i, "_", j)]],
            Beneficiaries = as.numeric(input[[paste0("Beneficiaries_", i, "_", j)]]) %||% 0,
            Needs = as.numeric(input[[paste0("Needs_", i, "_", j)]]) %||% 0,
            Saves_Lives = input[[paste0("Saves_Lives_", i, "_", j)]] %||% FALSE,
            Lives_Saved = as.numeric(input[[paste0("Lives_Saved_", i, "_", j)]]) %||% 0,
            Life_Years = as.numeric(input[[paste0("Life_Years_", i, "_", j)]]) %||% 1
          )
          calculations$data$Details[[index]] <- investment
        }
      })
    })
  })
  
  observeEvent(input$calculate_sums, {
    lapply(1:2, function(i) {
      lapply(1:10, function(j) {
        index <- (i - 1) * 10 + j
        if (calculations$data$Rendered[index] == 1) {
          investment <- calculations$data$Details[[index]]
          
          # Ensure Country and Impact_category are set
          avg_qaly_shortfall <- 0
          if (!is.null(investment$Country) && !is.null(investment$Impact_category)) {
            avg_qaly_shortfall <- qaly_database %>%
              filter(country == investment$Country & Impact_category == investment$Impact_category) %>%
              pull(average_qaly_shortfall) %>%
              as.numeric() %||% 0
          }            
          
          qaly_value <- as.numeric(avg_qaly_shortfall) * as.numeric(investment$Beneficiaries) * as.numeric(investment$Needs)
          
          if (as.logical(investment$Saves_Lives)) {
            qaly_value <- qaly_value + (as.numeric(investment$Lives_Saved) * as.numeric(investment$Life_Years))
          }
          
          # Debug statement to check QALY value calculation
          print(paste("Fund:", i, "Investment:", j, "QALY Value:", qaly_value))
          
          # Store the QALY value in the investment details
          calculations$data$Details[[index]] <- modifyList(investment, list(QALY_Value = qaly_value))
        }
      })
    })
  })
  
output$fund_total_qalys <- renderUI({
  cat("Rendering fund_total_qalys\n")
  
  # Iterate over funds
  tagList(
    lapply(1:2, function(i) {
      fund_qaly_value <- sum(sapply(1:calculations$clicks[i], function(j) {
        index <- (i - 1) * 10 + j
        
        # Check if QALY data exists
        if (!is.null(calculations$data$Details[[index]]) && calculations$data$Rendered[index] == 1) {
          qaly_value <- calculations$data$Details[[index]]$QALY_Value
          cat("Fund:", i, "Investment:", j, "QALY Value:", qaly_value, "\n")
          
          if (!is.null(qaly_value) && !is.na(qaly_value)) {
            return(as.numeric(qaly_value))
          }
        }
        return(0)
      }))
      
      cat("Fund:", i, "Total QALYs:", fund_qaly_value, "\n")  # Debugging
      
      div(
        style = "border: 3px solid #333; background-color: #f2f2f2; padding: 10px; margin-top: 20px; border-radius: 5px;",
        h4(paste("Total QALYs for Fund", i, ":"), style = "margin-bottom: 5px;"),
        h3(fund_qaly_value, style = "font-weight: bold; color: #007bff;")
      )
    })
  )
})

observe({
  cat("Current State of calculations$data$Details:\n")
  print(str(calculations$data$Details))
})


}

shinyApp(ui = ui, server = server)