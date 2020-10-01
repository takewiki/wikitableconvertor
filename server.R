#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#m00


library(shiny)
library(shinydashboard)
library(readxl)
library(openxlsx);
library(stringr);


 shinyServer(function(input, output) {
  
     #GET THE INTIIAL DATA
    csvdata <- reactive({
      if (input$fileType =='Excel')
      {
        inFile <- input$fileExcel
        if (is.null(inFile))
          return(NULL)
        res <-openxlsx::read.xlsx(inFile$datapath,sheet=1,colNames=input$header);
        res <- tbl_as_df(res);
      }
      if (input$fileType == 'CSV')
      {
        inFile <- input$file1
        
        if (is.null(inFile))
          return(NULL)
        
        res <-read.csv(inFile$datapath, header = input$header)    
      }
      res;
       
    })
    
   
  output$content_table <- renderDataTable({
    
 
   
    csvdata()
  },options = list(orderClasses = TRUE,
                   lengthMenu = c(6,10,50,100), 
                   pageLength = 6))
  
  res  <- reactive({
    inFile <- input$fileExcel
   res <-addrPhoneExtractor(inFile$datapath);
   return(res);
  })
  output$dataResult <- renderDataTable({
    
    
    res()
  },options = list(orderClasses = TRUE,
                   lengthMenu = c(10,50,100,1000), 
                   pageLength = 10))
    
  
  
  output$fileTemlate1<- downloadHandler(
    filename = 'template.xlsx',
    content = function(file) {
      writeDataToExcel(iris, file, sheetName = 'sheet1');
    }
  )
  output$fileTemlate2<- downloadHandler(
    filename = 'template.xlsx',
    content = function(file) {
      writeDataToExcel(iris, file, sheetName = 'sheet1');
    }
  )

  output$report_xlsx<- downloadHandler(
    filename = 'wikiTable.txt',
    content = function(file) {
      write.wikiTable(csvdata(), sortable = TRUE,file = file);
    }
  )
   
 }
)
