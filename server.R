library(shiny)
library(tools)

#set maximum upload filesize, in MBs
options(shiny.maxRequestSize=200*1024^2)
function(input, output) {
   
   ### Remove Blank Columns --- DONE
   
   #Returns the file input computation
   blankColumnsInput <- reactive({
      
      #gets input data and ensures it's not null
      inFile <- input$blankColumnsFile

      if (is.null(inFile))
         return(NULL)

      #reads the list of files to data frame and stores it in a list
      fileList<- lapply(inFile$datapath, read.csv, header=T)
      
      #removes the blank columns from each of the dataframes in the list
      lapply(fileList, function(df) {df[!sapply(df, function(x) {all(is.na(x)|x=="")})]})
   })
   
   #renders the output of the reactive to a table on the UI
   output$blankColumnsContent <- renderTable({
      blankColumnsInput()
   })
   
   #download button handler
   output$blankColumnsDownload <- downloadHandler(
      
      #manually set filename
      filename = "output.zip",
      
      #zip list of dataframes as folder of csvs
      content = function(filename) {
         
         #create empty list of files to pass to zipper
         files <- c()
         
         #create and set working directory to a temp directory
         tempdir <- tempdir()
         setwd(tempdir())
         
         #get input for looping through list of files
         inFile <- input$blankColumnsFile
         
         #store the results, so we don't have to call the reactive multiple times
         results <- blankColumnsInput()
         
         #loop through each file in the input
         for (i in 1:nrow(inFile)) {
            
            #the resulting filename is the same as the original filename
            path <- inFile[i,]$name
            
            #add current file to list of files to zip
            files <- c(files, path)
            
            #write current dataframe to csv
            write.csv(results[[i]], path, row.names = F)
         }
         
         #zip all csvs and return as content
         zip(zipfile=filename, files=files)
      },
      
      #ensure the resulting filetype is zip
      contentType = "application/zip"
   )
   
   ### CSV Splitter (slow) --- DONE
   
   #returns input file as dataframe
   slowSplitInput <- reactive({
      
      #get file input info and ensure it's not null
      inFile <- input$slowSplitFile

      if (is.null(inFile))
         return(NULL)
      
      #read the input csv file to dataframe and return
      read.csv(inFile$datapath,header=T)
   })
   
   #displays input files data frame on UI
   output$slowSplitContent <- renderTable({
      slowSplitInput()
   })
   
   #download button handler
   output$slowSplitDownload <- downloadHandler(
      
      #filename is same as the original, but file extension is zip
      filename = function() { paste(as.character(input$slowSplitFile$name),"zip",sep=".") },
      
      #zip list of data frames as folder of csvs
      content = function(filename) {
         
         #create list of files for zipping
         files <- c()
         
         #create and set working directory to temp directory
         tempdir <- tempdir()
         setwd(tempdir())
         
         #get input info for filename
         inFile <- input$slowSplitFile
         
         #store dataframe from reactive
         results <- slowSplitInput()
         
         #get filename without extension for incrementing through and creating new filenames
         #uses "tools" package
         fname <- file_path_sans_ext(inFile$name)
         
         #indicate desired maximum filesize per file
         maxfilesize <- 9 * 1024 * 1024
         
         #start the file number at one
         filenum <- 1
         
         #add first file to the list of files for zipping
         files <- c(files,paste(fname,filenum,".csv",sep=""))
         
         #write the header to the first file
         write.csv(results[0,],paste(fname,filenum,".csv",sep=""),na='',row.names=F)
         
         #loop through each row of the original data frame
         for (row in 1:nrow(results)) {
            
            #check to see if the desired filesize has been reached
            #on the current file
            if(file.info(paste(fname,filenum,".csv",sep=""))$size >= maxfilesize) {
               
               #if filesize has been reached, increment the file number
               filenum <- filenum + 1
               
               #add new file to list of files to zip
               files <- c(files,paste(fname,filenum,".csv",sep=""))
               
               #add header to the new file
               write.csv(results[0,],paste(fname,filenum,".csv",sep=""),na='',row.names=F)
            }
            
            #write current line to the bottom of the current file
            write.table(results[row,],paste(fname,filenum,".csv",sep=""),na='',sep=',',row.names=F,col.names=F,append=T)
         }
         
         #zip all files and return as content
         zip(zipfile=filename, files=files)
      },
      #ensure filetype is zip
      contentType = "application/zip"
   )
   
   ### CSV Splitter (fast)
   
   #placeholder for potential output, currently returns "hello"
   fastSplitInput <- reactive({
      
      #get input file info and ensure it's not null
      inFile <- input$fastSplitFile

      if (is.null(inFile))
         return(NULL)
      
      "hello"
   })
   
   #display reactive data in UI
   output$fastSplitContent <- renderTable({
      fastSplitInput()
   })
   
   #download button handler
   output$fastSplitDownload <- downloadHandler(
      
      #output filename is the same, but with a zip file extension
      filename = function() { paste(as.character(input$fastSplitFile$name),"zip",sep=".") },
      
      #zip list of csvs
      content = function(filename) {
         
         #create list of files for zipping
         files <- c()
         
         #create and set working directory to temp directory
         tempdir <- tempdir()
         setwd(tempdir())
         
         #get input file info for filename
         inFile <- input$fastSplitFile
         
         #get filename without the extension for incrementing
         fname <- file_path_sans_ext(inFile$name)
         
         #set total to 0
         #this is used to store the total number of characters stored in the current file
         #this is used to approximate current filesize
         total <- 0
         
         #set filenum for incrementing files
         filenum <- 1
         
         #instantiate firstLine to store header for use in each file
         firstLine <- NA
         
         #indicate desired maximum filesize
         maxfilesize <- 9.5 * 1024 * 1024
         
         #read each line of uploaded file
         lines <- readLines(inFile$datapath)
         
         #add first file to list of files
         files <- c(files,paste(fname,filenum,".csv",sep=""))
         
         #open stream for writing lines to first file
         sink(paste(fname,filenum,'.csv',sep=''))
         
         #loop through each line in the uploaded file
         for(line in lines) {
            
            #in the first pass, sets the first line in the file as the header line
            if(is.na(firstLine))
               firstLine <- line
            
            #gets the number of characters of the current line
            length <- nchar(line)
            
            #if the current line would put the file over the max filesize,
            #move to the next file
            if(total + length >= maxfilesize) {
               
               #increment the file number
               filenum <- filenum + 1
               
               #reset current character total
               total <- 0
               
               #close current file connection
               sink()
               
               #add new file to list of files to zip
               files <- c(files,paste(fname,filenum,".csv",sep=""))
               
               #open stream to next file
               sink(paste(fname,filenum,'.csv',sep=''))
               
               #write the header to the new file
               writeLines(firstLine)
               
               #add the length of the header line to the current line
               length <- length + nchar(firstLine)
            }
            
            #write current line to current file
            writeLines(line)
            
            #add length of current line to the total (the plus 2 handles the new line)
            total <- total + length + 2
         }
         
         #close the final file
         sink()
         
         #zip all csvs and return as content to download
         zip(zipfile=filename, files=files)
      },
      
      #ensure filetype is zip
      contentType = "application/zip"
   )
   
   ### Salesforce Cleaner
   
   salesforceInput <- reactive({
      inFile <- input$salesforceFile
      
      if (is.null(inFile))
         return(NULL)
      
      # <CODE FOR CLEANING UP SALESFORCE>
      # <RETURN LIST OF DATA FRAMES, BASED ON CHECKBOXES>
   })
   
   output$salesforceContent <- renderTable({
      salesforceInput()
   })
   
   output$salesforceDownload <- downloadHandler(
      filename = function() { as.character(input$salesforceFile) },
      content = function(file) {
         write.csv(salesforceInput(), file, row.names = F)
      }
   )
   
   ### Filesmasher
   
   filesmasherInput <- reactive({
     library(plyr)
     #get input file info and ensure it's not null
      inFile <- input$filesmasherFile
      
      if (is.null(inFile))
         return(NULL)
      
      
      #reads the list of files to data frame and stores it in a list
      smashedFiles<- lapply(inFile$datapath, read.csv, header = T, stringsAsFactors = F)
      rbind.fill(smashedFiles)
      
   })
   
   #renders the output of the reactive to a table on the UI
   output$filesmasherContent <- renderTable({
      filesmasherInput()
   })
   
   #download button handler
   output$filesmasherDownload <- downloadHandler(
     
     #name the file it will be
     filename = "AllFilesTogether2.csv",
     
      content = function(filename){
        smashed <- filesmasherInput()
        write.csv(smashed, filename, na="", row.names = F)
   })
   
   ### CSV Merge
   
   csvMergeInput <- reactive({
      inFile <- input$csvMergeFile
      
      if (is.null(inFile))
         return(NULL)
      
      # <CODE FOR MERGING CSVS BY ID NUMBER>
      # <RETURN A DATA FRAME>
   })
   
   output$csvMergeContent <- renderTable({
      csvMergeInput()
   })
   
   output$csvMergeDownload <- downloadHandler(
      filename = function() { as.character(input$csvMergeFile) },
      content = function(file) {
         write.csv(csvMergeInput(), file, row.names = F)
      }
   )
   
   ### CSV Converter
   
   csvConverterInput <- reactive({
      inFile <- input$csvConverterFile
      
      if (is.null(inFile))
         return(NULL)
      
      # <CODE FOR CONVERTING FILES TO CSV, HANDLING JSON, XML, AND YAML>
      # <RETURN A DATA FRAME>
   })
   
   output$csvConverterContent <- renderTable({
      csvConverterInput()
   })
   
   output$csvConverterDownload <- downloadHandler(
      filename = function() { as.character(input$csvConverterFile) },
      content = function(file) {
         write.csv(csvConverterInput(), file, row.names = F)
      }
   )
}