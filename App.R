library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

EMP_GAP_Day$AR <- as.factor(EMP_GAP_Day$AR)
EMP_GAP_Day$Manager <- as.factor(EMP_GAP_Day$Manager)
EMP_GAP_MONTH$AR <- as.factor(EMP_GAP_MONTH$AR)
EMP_GAP_MONTH$Manager <- as.factor(EMP_GAP_MONTH$Manager)
MGR_GAP_Day$Manager <- as.factor(MGR_GAP_Day$Manager)
MGR_GAP_MONTH$Manager <- as.factor(MGR_GAP_MONTH$Manager)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Collector by Day", tabName = "collday", icon = icon("dashboard")),
    menuItem("Collector by Month", icon = icon("dashboard"), tabName = "collmonth"),
    menuItem("Manager by Day",tabName="mgrday",icon=icon("dashboard")),
    menuItem("Manager by Month",tabName="mgrmonth",icon=icon("dashboard")),
       dateInput("Day",
              label="Date",
              value=(Sys.Date()-1),
              min="2014-12-31",
              format="m/d/yyyy"),
    selectInput("Month","Month",
                choices=levels(EMP_GAP_MONTH$Month),
                selected= "May 2016"),
    checkboxGroupInput("Department",
                       "Department",
                       choices=levels(df$DEPT),
                       selected=levels(df$DEPT)),
    checkboxGroupInput("Office",
                       "Office",
                       choices=levels(df$off),
                       selected=levels(df$off)),
    downloadButton("downloadData", 'Download Daily Data')
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "collday",
            h2("Collector by Day"),
            DT::dataTableOutput("CollDay")
    ),
    
    tabItem(tabName = "collmonth",
            h2("Collector by Month"),
            DT::dataTableOutput("CollMTH")
    ),
    tabItem(tabName = "mgrday",
            h2("Manager by Day"),
            DT::dataTableOutput("MGRDay")
            ),
    tabItem(tabName = "mgrmonth",
            h2("Manager by Month"),
            DT::dataTableOutput("MGRMTH")
            )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Gap Report"),
  sidebar,
  body
)



server <- function(input, output) {
  
  emp_day <- reactive({
    emp <- subset(EMP_GAP_Day,Date == as.character(input$Day))
    a <- subset(emp,Office %in% c(input$Office[1],
                                  input$Office[2],
                                  input$Office[3],
                                  input$Office[4],
                                  input$Office[5],
                                  input$Office[6]))
    b<- subset(a,Department %in% c(input$Department[1],
                                   input$Department[2],
                                   input$Department[3],
                                   input$Department[4],
                                   input$Department[5],
                                   input$Department[6],
                                   input$Department[7],
                                   input$Department[8]))
    
    b
  })
  
  emp <- reactive({
    emp <- subset(df,DTE == as.character(input$Day))
    a <- subset(emp,off %in% c(input$Office[1],
                                  input$Office[2],
                                  input$Office[3],
                                  input$Office[4],
                                  input$Office[5],
                                  input$Office[6]))
    b<- subset(a,DEPT %in% c(input$Department[1],
                                   input$Department[2],
                                   input$Department[3],
                                   input$Department[4],
                                   input$Department[5],
                                   input$Department[6],
                                   input$Department[7],
                                   input$Department[8]))
    
    b
  })
  
  mgr_day <- reactive({
    emp <- subset(MGR_GAP_Day,Date == as.character(input$Day))
    a <- subset(emp,Office %in% c(input$Office[1],
                                  input$Office[2],
                                  input$Office[3],
                                  input$Office[4],
                                  input$Office[5],
                                  input$Office[6]))
    b<- subset(a,Department %in% c(input$Department[1],
                                   input$Department[2],
                                   input$Department[3],
                                   input$Department[4],
                                   input$Department[5],
                                   input$Department[6],
                                   input$Department[7],
                                   input$Department[8]))
    
    b
  })
  
  emp_mth <- reactive({
    a <- subset(EMP_GAP_MONTH,Office %in% c(input$Office[1],
                                  input$Office[2],
                                  input$Office[3],
                                  input$Office[4],
                                  input$Office[5],
                                  input$Office[6]))
    b<- subset(a,Department %in% c(input$Department[1],
                                   input$Department[2],
                                   input$Department[3],
                                   input$Department[4],
                                   input$Department[5],
                                   input$Department[6],
                                   input$Department[7],
                                   input$Department[8]))
    c <- subset(b,Month == input$Month)
    
    c
  })
  
  mgr_mth <- reactive({
    a <- subset(MGR_GAP_MONTH,Office %in% c(input$Office[1],
                                  input$Office[2],
                                  input$Office[3],
                                  input$Office[4],
                                  input$Office[5],
                                  input$Office[6]))
    b<- subset(a,Department %in% c(input$Department[1],
                                   input$Department[2],
                                   input$Department[3],
                                   input$Department[4],
                                   input$Department[5],
                                   input$Department[6],
                                   input$Department[7],
                                   input$Department[8]))
    c <- subset(b,Month == input$Month)
    
    c
  })
  
  output$CollDay <- DT::renderDataTable({
    datatable(emp_day(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
              options = list(
                searching=TRUE,
                autoWidth=TRUE,
                paging=FALSE,
                "sDom" = 'T<"clear">lfrtip',
                "oTableTools" = list(
                  "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                  "aButtons" = list(
                    "copy",
                    "print",
                    list("sExtends" = "collection",
                         "sButtonText" = "Save",
                         "aButtons" = c("csv","xls"))))))
  })
  
  output$CollMTH <- DT::renderDataTable({
    datatable(emp_mth(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
              options = list(
                searching=TRUE,
                autoWidth=TRUE,
                paging=FALSE,
                "sDom" = 'T<"clear">lfrtip',
                "oTableTools" = list(
                  "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                  "aButtons" = list(
                    "copy",
                    "print",
                    list("sExtends" = "collection",
                         "sButtonText" = "Save",
                         "aButtons" = c("csv","xls"))))))
  })
  
  output$MGRDay <- DT::renderDataTable({
    datatable(mgr_day(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
              options = list(
                searching=TRUE,
                autoWidth=TRUE,
                paging=FALSE,
                "sDom" = 'T<"clear">lfrtip',
                "oTableTools" = list(
                  "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                  "aButtons" = list(
                    "copy",
                    "print",
                    list("sExtends" = "collection",
                         "sButtonText" = "Save",
                         "aButtons" = c("csv","xls"))))))
  })
  
  output$MGRMTH <- DT::renderDataTable({
    datatable(mgr_mth(),extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
              options = list(
                searching=TRUE,
                autoWidth=TRUE,
                paging=FALSE,
                "sDom" = 'T<"clear">lfrtip',
                "oTableTools" = list(
                  "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                  "aButtons" = list(
                    "copy",
                    "print",
                    list("sExtends" = "collection",
                         "sButtonText" = "Save",
                         "aButtons" = c("csv","xls"))))))
  })
  
  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) 
        counter <- -1
      else
        load(file="counter.Rdata")
      counter  <- counter + 1
      save(counter, file="counter.Rdata")     
      paste("You are visitor # ", counter)
      
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(emp(), file)
    }
  )
  
  
  
}

shinyApp(ui, server)
