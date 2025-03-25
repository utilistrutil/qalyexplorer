library(shiny)
library(dplyr)
library(plotly)

## Known issues
####Fund names not refreshing
####Long lag times on adding investments (mostly due to issues with presets)
####Code to render UI for fund sums is written but not appearing?
####In general, probably needs much better error handling.

# SET ---------------------------------------------------------------------

username <- "HongyuPan"

# Helper function for NULL handling, suggested by Claude
`%||%` <- function(x, y) if (is.null(x)) y else x

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    .default-btn{
        background-color: #041E42;
        color: white;
        border: none;
        border-radius: 5px;
        padding: 8px 15px;
    }
    .fund-box{
    padding: 10px;
    margin-bottom: 20px; 
    height: auto; 
    display: flex; 
    flex-direction: column; 
    justify-content: flex-end;
    background:transparent;
    border:none;
    }
    .well {
      background-color: transparent;
      border: none;
      box-shadow: none;
    }
    .button-group{
        display: flex;
        gap: 10px;
        margin-top: 10px;
        justify-content: flex-start;
    }
                  "
                    
    ))
  ),
  div(
    style = " display: flex;align-items: center; justify-content: center;margin-top: 20px; margin-left:20px;",
    img(src = "images/GIIN.png", height = "60px", style = "margin-right: 5px;margin-left:5px;"),
    h3("Impact Investing QALY eXplorer", style = "margin: 0 auto;font-weight: bold;padding-right: 80px;")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 6, 
      uiOutput("calculator_sections"),
      actionButton("calculate_sums", "Calculate Sums", class = "default-btn", style = "margin-top: 20px;")
    ),
    mainPanel(
      width = 6,
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
    clicks = c(0, 0),  # Counter for each fund
    calculate_clicked = FALSE,  # Flag to check if calculate button was clicked
    fund_names = c("", "")  # Store fund names
  )
  
  # Render the fund boxes
  output$calculator_sections <- renderUI({
    tagList(
      lapply(1:2, function(i) {
        div(class = "fund-box",  # fund box
            fluidRow(
              column(12, textInput(paste0("Fund_Name_", i), "Fund Name:", value = "")),
            ),
            uiOutput(paste0("investment_sections_", i)),  # Dynamic investments within each fund
            if(calculations$clicks[i] == 0) {
              div(class = "button-group",
                  actionButton(paste0("add_investment_", i), "Add Investment", class = "default-btn")
              )
            } else {
              NULL 
            }
        )
      })
    )
  })
  
  # Dynamically render investments for each fund
  observe({
    lapply(1:2, function(i) {
      output[[paste0("investment_sections_", i)]] <- renderUI({
        if(calculations$clicks[i] == 0) {
          return(NULL)
        }
        tagList(
          lapply(1:calculations$clicks[i], function(j) {
            index <- (i - 1) * 10 + j
            if (calculations$data$Rendered[index] == 1) {
              ## calculations$data$Rendered[index] <- 1 # clause suggestion
              investment <- calculations$data$Details[[index]] %||% list()
              div(class = "investment-box",  # investment box
                  p(paste0("Investment #",j)),
                  fluidRow(
                    
                    column(12, selectInput(paste0("Country_", i, "_", j), "Investment Country:", choices = unique(qaly_database$country), selected = investment$Country)),
                    column(6, selectInput(paste0("Impact_theme_", i, "_", j), "Impact Theme:", choices = unique(qaly_database$Impact_theme), selected = investment$Impact_theme)),
                    column(6, selectInput(paste0("Impact_category_", i, "_", j), "Impact Sub-Theme:", choices = unique(qaly_database$Impact_category), selected = investment$Impact_category)),
                    column(6, numericInput(paste0("Beneficiaries_", i, "_", j), "Beneficiaries Served:", value = investment$Beneficiaries)),
                    column(6, selectInput(paste0("Needs_", i, "_", j), "Percentage of Needs Met:", choices = c(0.25, 0.5, 0.75), selected = investment$Needs)),
                    column(12, checkboxInput(paste0("Saves_Lives_", i, "_", j), "This investment saves lives", value = investment$Saves_Lives)),
                    conditionalPanel(
                      condition = paste0("input.Saves_Lives_", i, "_", j),
                      column(6, numericInput(paste0("Lives_Saved_", i, "_", j), "Lives Saved:", value = investment$Lives_Saved, min = 0)),
                      column(6, selectInput(paste0("Life_Years_", i, "_", j), "Life Years:", choices = c(1, 5, 25), selected = investment$Life_Years))
                    ),
                    if (j == 1 && calculations$clicks[i] >= 1) {
                      column(12,
                             div(class = "button-group",
                                 actionButton(paste0("add_investment_", i, "_after"), "Add Investment", class = "default-btn"),
                                 actionButton(paste0("delete_investment_", i, "_", j), "Delete Investment", class = "default-btn")
                             )
                      )
                    } else {
                      NULL
                    }
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
  # Store fund names
  observe({
    calculations$fund_names[1] <- input$Fund_Name_1 %||% "Fund 1"
    calculations$fund_names[2] <- input$Fund_Name_2 %||% "Fund 2"
  })
  
  # Save input data to reactive values
  observeEvent(input$calculate_sums, {
    calculations$calculate_clicked <- TRUE
    for(i in 1:2) {
      for(j in 1:10) {
        index <- (i - 1) * 10 + j
        if (calculations$data$Rendered[index] == 1) {
          investment <- list(
            #Investment_Name = input[[paste0("Investment_Name_", i, "_", j)]],
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
      }
    }
    
    
    
    for(i in 1:2) {
      for(j in 1:10) {
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
      }
    }
    invalidateLater(0)
  },ignoreInit = TRUE)
  
  
  output$fund_total_qalys <- renderUI({
    div(
      style = "border: 1px solid #eee;box-shadow: 0 2px 4px rgba(0,0,0,0.1);;margin-top: 40px; margin-right:30px;",
      if(!calculations$calculate_clicked){
        div(
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center;margin-top:10px;margin-bottom:10px;",
          tags$img(src = "https://t4.ftcdn.net/jpg/01/17/17/51/360_F_117175168_pW01TQkluE8tFoXlzdSJ3WI6pzqba3dG.jpg", 
                   height = "100px", width = "100px"),
          h4("Fill out the inputs to view QALY impact achieved by different funds", 
             style = "text-align: center; margin-top: 20px; font-weight: bold;max-width: 400px;")
        )
      }else{
        cat("Rendering fund_total_qalys\n")
        
        fund_names <- c(
          input$Fund_Name_1 %||% "Fund 1",
          input$Fund_Name_2 %||% "Fund 2"
        )
        
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
              style = " padding: 10px; margin-top: 20px;",
              h4(paste("Total QALYs for", fund_names[i], ":"), style = "margin-bottom: 5px;"),
              h3(sprintf("%.2f", fund_qaly_value), style = "font-weight: bold; color: #007bff;")
            )
          })
        )
      }
    )
    
  })
  observe({
    cat("Current State of calculations$data$Details:\n")
    print(str(calculations$data$Details))
  })
  
}

shinyApp(ui = ui, server = server)