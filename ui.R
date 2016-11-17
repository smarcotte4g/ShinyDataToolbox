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
                                            '.csv')),
                         tags$hr(),
                         downloadButton('blankColumnsDownload', 'Download'),
                          tableOutput('blankColumnsContent')
                       ))
           ),
           navbarMenu("CSV Splitter",
                      tabPanel("Contains New Lines",
                               sidebarLayout(
                                  sidebarPanel(
                                     p("Change this text to be directions for the function.")
                                  ),
                                  mainPanel(
                                    fileInput('file2', 'Choose CSV File (Salesforce)',
                                              accept=c('text/csv', 
                                                       'text/comma-separated-values,text/plain', 
                                                       '.csv')),
                                    tags$hr(),
                                    downloadButton('downloadData2', 'Download'),
                                     tableOutput('contents2')
                                  ))
                      ),
                      tabPanel("Doesn't Contain New Lines",
                               sidebarLayout(
                                  sidebarPanel(
                                    p("Change this text to be directions for the function.")
                                  ),
                                  mainPanel(
                                    fileInput('file3', 'Choose CSV File (Salesforce)',
                                              accept=c('text/csv', 
                                                       'text/comma-separated-values,text/plain', 
                                                       '.csv')),
                                    tags$hr(),
                                    downloadButton('downloadData3', 'Download'),
                                     tableOutput('contents3')
                                  ))
                      )
           ),
           tabPanel("Salesforce Cleaner",
                    sidebarLayout(
                       sidebarPanel(
                         p("Change this text to be directions for the function.")
                       ),
                       mainPanel(
                         fileInput('file4', 'Choose CSV File (Remove)',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         tags$hr(),
                         downloadButton('downloadData4', 'Download'),
                          tableOutput('contents4')
                       ))
           ),
           tabPanel("Filesmasher",
                    sidebarLayout(
                       sidebarPanel(
                         p("Change this text to be directions for the function.")
                       ),
                       mainPanel(
                         fileInput('file5', 'Choose CSV File (Remove)',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         tags$hr(),
                         downloadButton('downloadData5', 'Download'),
                          tableOutput('contents5')
                       ))
           ),
           tabPanel("CSV Merge",
                    sidebarLayout(
                       sidebarPanel(
                         p("Change this text to be directions for the function.")
                       ),
                       mainPanel(
                         fileInput('file6', 'Choose CSV File (Remove)',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         tags$hr(),
                         downloadButton('downloadData6', 'Download'),
                          tableOutput('contents6')
                       ))
           ),
           tabPanel("CSV Converter",
                    sidebarLayout(
                       sidebarPanel(
                         p("Change this text to be directions for the function.")
                       ),
                       mainPanel(
                         fileInput('file7', 'Choose CSV File (Remove)',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         tags$hr(),
                         downloadButton('downloadData7', 'Download'),
                          tableOutput('contents7')
                       ))
           )
)