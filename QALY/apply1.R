library(shinydashboardPlus)

shinyApp(
  ui = shinydashboard::dashboardPage(title = "My Box Dropdown",
                                     dashboardHeader(),  
                                     dashboardSidebar(),
                                     dashboardBody(
                                       uiOutput("boxes")
                                     )
  ),
  
  server = function(input, output) {
    
    rvs = reactiveValues(boxDropdownItem = list(), observers = list(), tmp=list())
    
    observe({
      for(i in 1:3) {
        rvs$boxDropdownItem[[i]] <-
          box(id = paste0("box",i),
              title = paste("box",i),
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              dropdownMenu = boxDropdown(
                icon = icon("ellipsis-v"),
                boxDropdownItem("Click me", id = paste0("dropdownItem",i), icon = icon("heart")),
                dropdownDivider(),
                boxDropdownItem(id = paste0("del",i), "Delete")
              ),
              paste("My Box",i)
          )
        
      }
      
    })
    
    output$boxes <- renderUI({
      if (length(rvs$tmp)>0){
        rvs$boxDropdownItem[!(rvs$boxDropdownItem %in% rvs$tmp)]
      } else rvs$boxDropdownItem
    })
    
    lapply(1:3, function(i) {
      observeEvent(input[[paste0("del",i)]],{
        rvs$tmp[[i]] <<- rvs$boxDropdownItem[[i]]
      }, ignoreInit = TRUE)
      
      
      observeEvent(input[[paste0("dropdownItem",i)]], {
        showNotification("Hello", duration = i, type = "error")
      })
      
    })
    
  }
)
