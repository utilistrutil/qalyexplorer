library(shiny)
library(dplyr)
library(plotly)
library(shinyBS)
library(shinyjs)
## Known issues
####Fund names not refreshing
####Lag times on adding investments (mostly due to issues with presets)
####Category dropdowns are not responsive to IRIS theme selection
####Calculate lag times since we have to do a match to LCA categories -
#####Should harmonize QALY outputs to same labels to reduce lag
####Add delete/clear button



# UI ----------------------------------------------------------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
  ),
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
      uiOutput("fund_total_qalys_container")
    )
  )
)


# Backend -----------------------------------------------------------------



server <- function(input, output, session) {
  
  qaly_database <- get(load("./data/qaly_database.RData"))
  mapping <- get(load("./data/demoqalymap.RData")) %>%
    filter(Applicable != "No")
                 
  # Initialize reactive values
  calculations <- reactiveValues(
    data = data.frame(
      Fund = rep(1:2, each = 10),
      Investment = rep(1:10, 2),
      Rendered = rep(0, 20),
      Details = I(vector("list", 20))
    ),
    clicks = c(0, 0)  # Counter for each fund
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
        if (calculations$clicks[i] == 0) {
          return(div(style = "color: red;", "Click 'add an investment'"))
        }
        
        tagList(
          lapply(1:calculations$clicks[i], function(j) {
            index <- (i - 1) * 10 + j
            if (calculations$data$Rendered[index] == 1) {
              investment <- calculations$data$Details[[index]]

              div(style = "border: 2px solid #ccc; background-color: #fff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",  # investment box
                  fluidRow(
                    column(12, textInput(paste0("Investment_Name_", i, "_", j), "Investment Name:", value = investment$Investment_Name)),
                    column(12, selectInput(paste0("Country_", i, "_", j), "Investment Country:", choices = unique(qaly_database$country), selected = investment$Country)),
                    column(12, selectInput(paste0("IRIS_theme", i, "_", j), "IRIS_theme:", choices = unique(mapping$IRIS_topic), selected = investment$IRIS_theme)),
                    column(12, 
                           div(
                             selectInput(paste0("Impact_category_", i, "_", j), "Impact Category:", choices = unique(mapping$Label), selected = investment$Impact_category),
                             tags$span(id = paste0("Impact_category_info_", i, "_", j), 
                                       tags$i(class = "fa fa-question-circle", style = "margin-left: 5px; cursor: pointer;"))
                           )
                    ),
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
  
# Observe tooltip
  observe({
    lapply(1:2, function(i) {
      lapply(1:10, function(j) {
        select_id <- paste0("Impact_category_", i, "_", j)
        tooltip_id <- paste0("Impact_category_info_", i, "_", j)
        
        observeEvent(input[[select_id]], {
          hover_text <- "Select a category"  # Default text
          
          # Check if user has selected a category and update hover text
          if (!is.null(input[[select_id]]) && input[[select_id]] %in% mapping$Label) {
            hover_text <- mapping$Hover[mapping$Label == input[[select_id]]]
            hover_text <- ifelse(length(hover_text) > 0, hover_text[1], "No description available")
          }
          
          # Log the selected text
          print(paste("Updating hover for", tooltip_id, "with text:", hover_text))
          
          # Update the title attribute with the hover text
          runjs(sprintf("
            var tooltipElement = document.getElementById('%s');
            if (tooltipElement) {
              tooltipElement.title = '%s';  // Set new title for the hover
            }
          ", tooltip_id, hover_text))
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
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
  
  ###add investment save observer
  observe({
    lapply(1:2, function(i) {
      observeEvent(input[[paste0("add_investment_", i)]], {
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
            # Lookup LCA_category based on Impact_category
            lca_category <- mapping$LCA_category[mapping$Label == investment$Impact_category]
            
            # Ensure we have a valid LCA_category before proceeding
            if (length(lca_category) == 1) {
              avg_qaly_shortfall <- qaly_database %>%
                filter(country == investment$Country & Impact_category == lca_category) %>%
                pull(average_qaly_shortfall) %>%
                as.numeric() %||% 0
            }
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
  
  # Conditionally render the fund_total_qalys UI
  output$fund_total_qalys_container <- renderUI({
    if (sum(calculations$clicks) > 0) {
      uiOutput("fund_total_qalys")
    } else {
      NULL
    }
  })
  
  output$fund_total_qalys <- renderUI({
    cat("Rendering fund_total_qalys\n")
    
    # Iterate over funds
    tagList(
      lapply(1:2, function(i) {
        qaly_values <- sapply(1:calculations$clicks[i], function(j) {
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
        })
        
        fund_qaly_value <- sum(qaly_values)
        cat("Fund:", i, "Total QALYs:", fund_qaly_value, "\n")  # Debugging
        
        # Create pie chart for QALY values
        pie_chart <- plot_ly(
          labels = sapply(1:calculations$clicks[i], function(j) {
            index <- (i - 1) * 10 + j
            if (!is.null(calculations$data$Details[[index]]) && calculations$data$Rendered[index] == 1) {
              investment <- calculations$data$Details[[index]]
              return(investment$Investment_Name)
            }
            return(NULL)
          }),
          values = qaly_values,
          type = 'pie',
          textinfo = 'label+percent',
          insidetextorientation = 'radial',
          title = paste("QALY Distribution for Fund", i)
        )
        div(
          style = "border: 3px solid #333; background-color: #f2f2f2; padding: 10px; margin-top: 20px; border-radius: 5px;",
          h4(paste("Total QALYs for Fund", i, ":"), style = "margin-bottom: 5px;"),
          h3(fund_qaly_value, style = "font-weight: bold; color: #007bff;"),
          plotlyOutput(paste("pie_chart_fund", i, sep = "_"))
        )
      })
    )
  })
  
  # Render the pie charts
  observe({
    lapply(1:2, function(i) {
      output[[paste("pie_chart_fund", i, sep = "_")]] <- renderPlotly({
        qaly_values <- sapply(1:calculations$clicks[i], function(j) {
          index <- (i - 1) * 10 + j
          
          if (!is.null(calculations$data$Details[[index]]) && calculations$data$Rendered[index] == 1) {
            qaly_value <- calculations$data$Details[[index]]$QALY_Value
            if (!is.null(qaly_value) && !is.na(qaly_value)) {
              return(as.numeric(qaly_value))
            }
          }
          return(0)
        })
        
        plot_ly(
          labels = sapply(1:calculations$clicks[i], function(j) {
            index <- (i - 1) * 10 + j
            if (!is.null(calculations$data$Details[[index]]) && calculations$data$Rendered[index] == 1) {
              investment <- calculations$data$Details[[index]]
              return(investment$Investment_Name)
            }
            return(NULL)
          }),
          values = qaly_values,
          type = 'pie',
          textinfo = 'label+percent',
          insidetextorientation = 'radial',
          title = paste("QALY Distribution for Fund", i)
        )
      })
    })
  })
}

shinyApp(ui = ui, server = server)