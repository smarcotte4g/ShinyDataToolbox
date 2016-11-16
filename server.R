library(shiny)

function(input, output) {
  datasetInput <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=T, sep=',', 
             quote='"')
    
  })
  output$contents <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { as.character(input$file1) },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  ) 
}