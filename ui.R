library(shiny)
shinyUI(
navbarPage(theme = "bootstrap.css","Infusionsoft Data Toolbox",
           tabPanel("Remove Blank Columns",
                    sidebarLayout(
                       sidebarPanel(
                          p("This function will remove all blank columns contained in a CSV file. If multiple files are selected, they will be outputted in a ZIP file."),
                          br(),
                          p("")
                       ),
                       mainPanel(
                          fileInput('blankColumnsFile', 'Choose CSV File',
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
                          p("This function will take the SalesForce files and output them in a clean format ready for import into Infusionsoft."),
                          br(),
                          p("Current supported files must be named exactly the same as below(to case):"),
                          tags$li("Contact.csv"),
                          tags$li("Account.csv"),
                          tags$li("Lead.csv"),
                          tags$li("Opportunity.csv"),
                          tags$li("Task.csv"),
                          tags$li("Note.csv"),
                          tags$li("User.csv"),
                          p("Note: If you want the user added to each file, you must select the User.csv during file selection.")
                       ),
                       mainPanel(
                          fileInput('salesforceFile', 'Choose SalesForce CSV Files(CTRL + Click)',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv'), multiple = T),
                          
                          tags$hr(),
                          downloadButton('salesforceDownload', 'Download'),
                          tableOutput('salesforceContent')
                       ))
           ),
           tabPanel("Filesmasher",
                    sidebarLayout(
                       sidebarPanel(
                          p("This function will smash together all files selected. If the files do not have the same column names, they will be appended to the end.")
                       ),
                       mainPanel(
                          fileInput('filesmasherFile', 'Choose 2 or More CSV Files',
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
                          p("This function will take the unique Id from 2 files and merge them together like a VLOOKUP in Excel. Instead of one column being merged, the entire files will be.")
                       ),
                       mainPanel(
                         #p("Enter the name of the unique column that you want to merge by, they must be the same. Only select 2 files."),
                         textInput("uniqueId", "Enter the Unique Id Column Name"),
                          fileInput('csvMergeFile', 'Choose 2 CSV Files',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv'), multiple = T),
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
                          fileInput('csvConverterFile', 'Choose XML, JSON, or YAML File',
                                    accept=c('.xml',
                                             '.json',
                                             '.yaml',
                                             '.yml',
                                             '.txt')),
                          tags$hr(),
                          downloadButton('csvConverterDownload', 'Download'),
                          tableOutput('csvConverterContent')
                       ))
           )
)
)