library(shiny)
options(shiny.maxRequestSize=30*1024^2)
function(input, output) {
   
   ### Remove Blank Columns
   blankColumnsInput <- reactive({
      inFile <- input$blankColumnsFile

      if (is.null(inFile))
         return(NULL)
      
      mydf<- read.csv(inFile$datapath, header=T, sep=',', quote='"')
      mydf[!sapply(mydf, function(x) all(is.na(x)|x==""))] #remove all blank columns
   })
   
   output$blankColumnsContent <- renderTable({
      blankColumnsInput()
   })
   
   output$blankColumnsDownload <- downloadHandler(
      filename = function() { as.character(input$blankColumnsFile) },
      content = function(file) {
         write.csv(blankColumnsInput(), file)
      }
   )
   
   ### CSV Splitter (fast)
   
   ### CSV Splitter (slow)
   
   ### Salesforce Cleaner
   
   ### Filesmasher
   
   ### CSV Merge
   
   ### CSV Converter
}