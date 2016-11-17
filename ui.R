
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
                                     fileInput('slowSplitFile', 'Choose CSV File (Salesforce)',
                                               accept=c('text/csv', 
                                                        'text/comma-separated-values,text/plain', 
                                                        '.csv')),
                                     tags$hr(),
                                     downloadButton('slowSplitDownload', 'Download')
                                  ),
                                  mainPanel(
                                     tableOutput('slowSplitContent')
                                  ))
                      ),
                      tabPanel("Doesn't Contain New Lines",
                               sidebarLayout(
                                  sidebarPanel(
                                     fileInput('fastSplitFile', 'Choose CSV File (Salesforce)',
                                               accept=c('text/csv', 
                                                        'text/comma-separated-values,text/plain', 
                                                        '.csv')),
                                     tags$hr(),
                                     downloadButton('fastSplitDownload', 'Download')
                                  ),
                                  mainPanel(
                                     tableOutput('fastSplitContent')
                                  ))
                      )
           ),
           tabPanel("Salesforce Cleaner",
                    sidebarLayout(
                       sidebarPanel(
                          fileInput('salesforceFile', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          downloadButton('salesforceDownload', 'Download')
                       ),
                       mainPanel(
                          tableOutput('salesforceContent')
                       ))
           ),
           tabPanel("Filesmasher",
                    sidebarLayout(
                       sidebarPanel(
                          fileInput('filesmasherFile', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          downloadButton('filesmasherDownload', 'Download')
                       ),
                       mainPanel(
                          tableOutput('filesmasherContent')
                       ))
           ),
           tabPanel("CSV Merge",
                    sidebarLayout(
                       sidebarPanel(
                          fileInput('csvMergeFile', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          downloadButton('csvMergeDownload', 'Download')
                       ),
                       mainPanel(
                          tableOutput('csvMergeContent')
                       ))
           ),
           tabPanel("CSV Converter",
                    sidebarLayout(
                       sidebarPanel(
                          fileInput('csvConverterFile', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          downloadButton('csvConverterDownload', 'Download')
                       ),
                       mainPanel(
                          tableOutput('csvConverterContent')
                       ))
           )
)