library(shiny)
options(shiny.maxRequestSize=30*1024^2)
function(input, output) {
  datasetInput <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    mydf<- read.csv(inFile$datapath, header=T, sep=',', quote='"')
    mydf[!sapply(mydf, function(x) all(is.na(x)|x==""))] #remove all of the blank columns
  })
  #output$contents <- renderTable({
    #datasetInput()
  #})
  
  output$downloadData <- downloadHandler(
    filename = function() { as.character(input$file1) },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  ) 
}