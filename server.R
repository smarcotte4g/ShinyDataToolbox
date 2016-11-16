library(shiny)
options(shiny.maxRequestSize=30*1024^2)
function(input, output) {
  datasetInput <- reactive({
    inFile <- input$file1
    print(inFile)
    if (is.null(inFile))
      return(NULL)
    
    mydf<- read.csv(inFile$datapath, header=T, sep=',', quote='"')
    mydf[!sapply(mydf, function(x) all(is.na(x)|x==""))] #remove all blank columns
  })

  output$contents1 <- renderTable({
     datasetInput()
  })

  output$downloadData1 <- downloadHandler(
    filename = function() { as.character(input$file1) },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  ) 
}