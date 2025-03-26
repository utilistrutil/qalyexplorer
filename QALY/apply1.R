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
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
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
      width = 6,  # Adjust width to 50% of the screen width
      uiOutput("calculator_sections"),
      actionButton("calculate_sums", "Calculate Sums", class = "default-btn", style = "margin-top: 20px;")
    ),
    mainPanel(
      width = 6,
      uiOutput("fund_total_qalys")
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
    clicks = c(0, 0),  # Counter for each fund
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
              investment <- calculations$data$Details[[index]]
              
              div(class = "investment-box",  # investment box
                  p(paste0("Investment #",j)),
                  fluidRow(
                    column(12, selectInput(paste0("Country_", i, "_", j), "Investment Country:", choices = unique(qaly_database$country), selected = investment$Country)),
                    column(6, selectInput(paste0("Impact_theme_", i, "_", j), "Impact Theme:", choices = unique(mapping$IRIS_topic), selected = investment$IRIS_theme)),
                    column(6, 
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
                    ),
                  ),
                  if (j == calculations$clicks[i]) {
                    div(class = "button-group",
                        actionButton(paste0("add_investment_", i), "Add Investment", class = "default-btn"),
                        actionButton(paste0("delete_investment_", i, "_", j), "Delete Investment", class = "default-btn")
                    )
                    
                  } 
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
  # Store fund names
  observe({
    calculations$fund_names[1] <- input$Fund_Name_1 %||% "Fund 1"
    calculations$fund_names[2] <- input$Fund_Name_2 %||% "Fund 2"
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
  
  #delete investments
  #add code here
  
  
  ###add investment save observer
  observe({
    
    lapply(1:2, function(i) {
      observeEvent(input[[paste0("add_investment_", i)]], {
        lapply(1:2, function(i) {
          lapply(1:10, function(j) {
            index <- (i - 1) * 10 + j
            if (calculations$data$Rendered[index] == 1) {
              investment <- list(
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
  
  
  output$fund_total_qalys <- renderUI({
    div(
      style = "border: 1px solid #eee;box-shadow: 0 2px 4px rgba(0,0,0,0.1);;margin-top: 40px; margin-right:30px;",
      if(sum(calculations$clicks)==0 || !input$calculate_sums){
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
            # Aggregate QALY values by impact category
            impact_category_qalys <- lapply(1:calculations$clicks[i], function(j) {
              index <- (i - 1) * 10 + j
              
              # Check if QALY data exists
              if (!is.null(calculations$data$Details[[index]]) && calculations$data$Rendered[index] == 1) {
                investment <- calculations$data$Details[[index]]
                return(data.frame(
                  category = investment$Impact_category, 
                  qaly_value = investment$QALY_Value
                ))
              }
              return(NULL)
            }) 
            
            # Remove NULL values and combine
            impact_category_qalys <- do.call(rbind, Filter(Negate(is.null), impact_category_qalys))
            
            # If we have data, aggregate
            if(nrow(impact_category_qalys) > 0) {
              impact_category_qalys <- impact_category_qalys %>%
                group_by(category) %>%
                summarise(total_qaly = sum(qaly_value, na.rm = TRUE))
              
              fund_qaly_value <- sum(impact_category_qalys$total_qaly)
              cat("Fund:", i, "Total QALYs:", fund_qaly_value, "\n")  # Debugging
              
              div(
                h4(paste("Total QALYs for Fund", i, ":"), style = "margin-bottom: 5px;"),
                h3(sprintf("%.2f", fund_qaly_value), style = "font-weight: bold; color: #007bff;"),
                h4("Fund QALY Composition"),
                plotlyOutput(paste("pie_chart_fund", i, sep = "_"))
                
              )
            } else {
              div(
                h4(paste("No QALY data for Fund", i))
              )
            }
          })
        )
      })
  })
  
  # Update the pie chart rendering observe section
  observe({
    req(input$calculate_sums)
    lapply(1:2, function(i) {
      output[[paste("pie_chart_fund", i, sep = "_")]] <- renderPlotly({
        # Aggregate QALY values by impact category
        impact_category_qalys <- lapply(1:calculations$clicks[i], function(j) {
          index <- (i - 1) * 10 + j
          
          if (!is.null(calculations$data$Details[[index]]) && calculations$data$Rendered[index] == 1) {
            investment <- calculations$data$Details[[index]]
            return(data.frame(
              category = investment$Impact_category, 
              qaly_value = investment$QALY_Value
            ))
          }
          return(NULL)
        })
        
        # Remove NULL values and combine
        impact_category_qalys <- do.call(rbind, Filter(Negate(is.null), impact_category_qalys))
        
        # If we have data, create pie chart
        if(nrow(impact_category_qalys) > 0) {
          # Aggregate QALY values by impact category
          impact_category_qalys <- impact_category_qalys %>%
            group_by(category) %>%
            summarise(total_qaly = sum(qaly_value, na.rm = TRUE))%>%
            mutate(percentage = total_qaly / sum(total_qaly) * 100)
          
          plot_ly(
            labels = impact_category_qalys$category,
            values = impact_category_qalys$total_qaly,
            type = 'pie',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            textposition = 'inside',
            hole = 0.4,
          )%>%
            layout(
              showlegend = TRUE,
              legend = list(
                orientation = "h",
                xanchor = "center",
                x = 0.5,
                y = -0.1
              )
            )
        } else {
          # Fallback plot if no data
          plot_ly(
            labels = "No Data",
            values = 1,
            type = 'pie',
            textinfo = 'label',
            title = paste("No QALY Data for Fund", i)
          )
        }
      })
    })
  })
}

shinyApp(ui = ui, server = server)