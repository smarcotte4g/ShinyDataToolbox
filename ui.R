library(shiny)

navbarPage("Uploading Files",
           tabPanel("Remove Blank Columns",
                    sidebarLayout(
                       sidebarPanel(
                          p("This function will remove all blank columns contained in a CSV file.")
                       ),
                       mainPanel(
                          fileInput('blankColumnsFile', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv'), multiple = T),
                          tags$hr(),
                          downloadButton('blankColumnsDownload', 'Download'),
                          tableOutput('blankColumnsContent')
                       ))
           ),
           navbarMenu("CSV Splitter",
                      tabPanel("Contains New Lines",
                               sidebarLayout(
                                  sidebarPanel(
                                     p("This function will split a single CSV file into multiple smaller CSVs under 10 MBs each."),
                                     br(),
                                     p("WARNING: This is slow. Only use this if cells contain new lines.")
                                  ),
                                  mainPanel(
                                     fileInput('slowSplitFile', 'Choose CSV File (Salesforce)',
                                               accept=c('text/csv', 
                                                        'text/comma-separated-values,text/plain', 
                                                        '.csv')),
                                     tags$hr(),
                                     downloadButton('slowSplitDownload', 'Download'),
                                     tableOutput('slowSplitContent')
                                  ))
                      ),
                      tabPanel("Doesn't Contain New Lines",
                               sidebarLayout(
                                  sidebarPanel(
                                     p("This function will split a single CSV file into multiple smaller CSVs under 10 MBs each.")
                                  ),
                                  mainPanel(
                                     fileInput('fastSplitFile', 'Choose CSV File (Salesforce)',
                                               accept=c('text/csv', 
                                                        'text/comma-separated-values,text/plain', 
                                                        '.csv')),
                                     tags$hr(),
                                     downloadButton('fastSplitDownload', 'Download'),
                                     tableOutput('fastSplitContent')
                                  ))
                      )
           ),
           tabPanel("Salesforce Cleaner",
                    sidebarLayout(
                       sidebarPanel(
                          p("This function will remove all blank columns contained in a CSV file.")
                       ),
                       mainPanel(
                          fileInput('salesforceFile', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          downloadButton('salesforceDownload', 'Download'),
                          tableOutput('salesforceContent')
                       ))
           ),
           tabPanel("Filesmasher",
                    sidebarLayout(
                       sidebarPanel(
                          p("This function will smash together all files selected.")
                       ),
                       mainPanel(
                          fileInput('filesmasherFile', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv'), multiple = T),
                          tags$hr(),
                          downloadButton('filesmasherDownload', 'Download'),
                          tableOutput('filesmasherContent')
                       ))
           ),
           tabPanel("CSV Merge",
                    sidebarLayout(
                       sidebarPanel(
                          p("This function will remove all blank columns contained in a CSV file.")
                       ),
                       mainPanel(
                          fileInput('csvMergeFile', 'Choose CSV File (Remove)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          downloadButton('csvMergeDownload', 'Download'),
                          tableOutput('csvMergeContent')
                       ))
           ),
           tabPanel("CSV Converter",
                    sidebarLayout(
                       sidebarPanel(
                          p("This function will convert an XML, JSON, or YAML file to CSV for importing.")
                       ),
                       mainPanel(
                          fileInput('csvConverterFile', 'Choose XML, JSON, or YAML File (Remove)',
                                    accept=c('.xml',
                                             '.json',
                                             '.yaml',
                                             '.yml')),
                          tags$hr(),
                          downloadButton('csvConverterDownload', 'Download'),
                          tableOutput('csvConverterContent')
                       ))
           )
)