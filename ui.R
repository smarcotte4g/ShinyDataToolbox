
library(shiny)

navbarPage("Uploading Files",
   tabPanel("Remove Blank Columns",
      sidebarLayout(
         sidebarPanel(
      fileInput('file1', 'Choose CSV File',
         accept=c('text/csv', 
                  'text/comma-separated-values,text/plain', 
                  '.csv')),
      tags$hr(),
      downloadButton('downloadData1', 'Download')
      ),
      mainPanel(
      tableOutput('contents1')
      ))
   ),
   tabPanel("Salesforce Cleanup",
            sidebarLayout(
               sidebarPanel(
                  fileInput('file2', 'Choose CSV File',
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv')),
                  tags$hr(),
                  downloadButton('downloadData2', 'Download')
               ),
               mainPanel(
                  tableOutput('contents2')
               ))
   )
)