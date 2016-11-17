
library(shiny)

fluidPage(
  titlePanel("Uploading Files"),
  #sidebarLayout(
    #sidebarPanel(
    wellPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      downloadButton('downloadData', 'Download')
      # Test
      
    ),
  wellPanel(
    fileInput('file2', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    tags$hr(),
    downloadButton('downloadData2', 'Download')
    # Test
    
  ),
    mainPanel(
      #tableOutput('contents')
    )
  #)
)