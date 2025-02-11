###TO DO - 
###Code actual math lol

library(shiny)
library(dplyr)


# SET ---------------------------------------------------------------------

username <- "JTate"

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("QALY Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 6,  # Adjust width to 50% of the screen width
      uiOutput("calculator_sections"),
      actionButton("add_calc", "Add Fund", class = "btn-primary", style = "width: 100%; margin-top: 10px;"),
    ),
    mainPanel(
      h3("QALY Output", style = "text-align: center; margin-bottom: 20px;"),
      uiOutput("QALY_results")
    )
  )
)

server <- function(input, output, session) {
  calculations <- reactiveValues(data = list(list()))  # Initialize with one fund
  
  # Render the fund boxes
  output$calculator_sections <- renderUI({
    tagList(
      lapply(seq_along(calculations$data), function(i) {
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
  
  observeEvent(input$add_calc, {
    calculations$data[[length(calculations$data) + 1]] <- list()  # Add a new fund
  })
  
  # Dynamically render investments for each fund
  observe({
    lapply(seq_along(calculations$data), function(i) {
      output[[paste0("investment_sections_", i)]] <- renderUI({
        tagList(
          lapply(1:length(calculations$data[[i]]), function(j) {
            div(style = "border: 2px solid #ccc; background-color: #fff; padding: 10px; margin-bottom: 10px; border-radius: 5px;",  # investment box
                fluidRow(
                  column(12, textInput(paste0("Investment_Name_", i, "_", j), "Investment Name:", value = "")),
                  column(12, selectInput(paste0("Country_", i, "_", j), "Investment Country:", choices = c("USA", "Canada", "Mexico"))),
                  column(6, numericInput(paste0("Beneficiaries_", i, "_", j), "Beneficiaries Served:", value = 0, min = 0, step = 1000)),
                  column(6, selectInput(paste0("Needs_", i, "_", j), "Percentage of Needs Met:", choices = c(0.25, 0.5, 0.75))),
                  column(6, actionButton(paste0("remove_investment_", i, "_", j), "Remove Investment", class = "btn-danger", style = "width: 100%; margin-top: 10px;"))
                )
            )
          })
        )
      })
    })
  })
  
  # Add investment observer
  observeEvent(input$add_investment_1, {
    calculations$data[[1]] <- append(calculations$data[[1]], list("investment"))
  })
  
  # Remove investment observer
  observe({
    lapply(seq_along(calculations$data), function(i) {
      lapply(seq_along(calculations$data[[i]]), function(j) {
        observeEvent(input[[paste0("remove_investment_", i, "_", j)]], {
          calculations$data[[i]] <- calculations$data[[i]][-j]  # Remove investment
        })
      })
    })
  })
}

shinyApp(ui = ui, server = server)