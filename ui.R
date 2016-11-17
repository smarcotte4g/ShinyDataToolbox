
library(shiny)

navbarPage("Uploading Files",
           tabPanel("Remove Blank Columns",
                    sidebarLayout(
                       sidebarPanel(
                          fileInput('blankColumnsFile', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          downloadButton('blankColumnsDownload', 'Download')
                       ),
                       mainPanel(
                          p("This function will remove all blank columns contained in a CSV file."),
                          tableOutput('blankColumnsContent')
                       ))
           ),
           navbarMenu("CSV Splitter",
                      tabPanel("Contains New Lines",
                               sidebarLayout(
                                  sidebarPanel(
                                     fileInput('file2', 'Choose CSV File (Salesforce)',
                                               accept=c('text/csv', 
                                                        'text/comma-separated-values,text/plain', 
                                                        '.csv')),
                                     tags$hr(),
                                     downloadButton('downloadData2', 'Download')
                                  ),
                                  mainPanel(
                                     tableOutput('contents2')
                                  ))
                      ),
                      tabPanel("Doesn't Contain New Lines",
                               sidebarLayout(
                                  sidebarPanel(
                                     fileInput('file3', 'Choose CSV File (Salesforce)',
                                               accept=c('text/csv', 
                                                        'text/comma-separated-values,text/plain', 
                                                        '.csv')),
                                     tags$hr(),
                                     downloadButton('downloadData3', 'Download')
                                  ),
                                  mainPanel(
                                     tableOutput('contents3')
                                  ))
                      )
           ),
           tabPanel("Salesforce Cleaner",
                    sidebarLayout(
                       sidebarPanel(
                          fileInput('file4', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          downloadButton('downloadData4', 'Download')
                       ),
                       mainPanel(
                          tableOutput('contents4')
                       ))
           ),
           tabPanel("Filesmasher",
                    sidebarLayout(
                       sidebarPanel(
                          fileInput('file5', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          downloadButton('downloadData5', 'Download')
                       ),
                       mainPanel(
                          tableOutput('contents5')
                       ))
           ),
           tabPanel("CSV Merge",
                    sidebarLayout(
                       sidebarPanel(
                          fileInput('file6', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          downloadButton('downloadData6', 'Download')
                       ),
                       mainPanel(
                          tableOutput('contents6')
                       ))
           ),
           tabPanel("CSV Converter",
                    sidebarLayout(
                       sidebarPanel(
                          fileInput('file7', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          downloadButton('downloadData7', 'Download')
                       ),
                       mainPanel(
                          tableOutput('contents7')
                       ))
           )
)