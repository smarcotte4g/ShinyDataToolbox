
library(shiny)
library(tools)

options(shiny.maxRequestSize=200*1024^2)
function(input, output) {
   
   ### Remove Blank Columns --- DONE
   
   blankColumnsInput <- reactive({
      inFile <- input$blankColumnsFile

      if (is.null(inFile))
         return(NULL)

      fileList<- lapply(inFile$datapath, read.csv, header=T)
      lapply(fileList, function(mydf) {mydf[!sapply(mydf, function(x) {all(is.na(x)|x=="")})]})
   })
   
   output$blankColumnsContent <- renderTable({
      blankColumnsInput()
   })
   
   output$blankColumnsDownload <- downloadHandler(
      filename = "output.zip",
      content = function(filename) {
        files <- c()
        tempdir <- tempdir()
        setwd(tempdir())
        inFile <- input$blankColumnsFile
        results <- blankColumnsInput()
        for (i in 1:nrow(inFile)) {
          path <- inFile[i,]$name
          files <- c(files, path)
          write.csv(results[[i]], path, row.names = F)
        }
        zip(zipfile=filename, files=files)
      },
      contentType = "application/zip"
   )
   
   ### CSV Splitter (slow) --- DONE
   
   slowSplitInput <- reactive({
      inFile <- input$slowSplitFile

      if (is.null(inFile))
         return(NULL)
      
      read.csv(inFile$datapath,header=T)
   })
   
   output$slowSplitContent <- renderTable({
      slowSplitInput()
   })
   
   output$slowSplitDownload <- downloadHandler(
      filename = function() { paste(as.character(input$slowSplitFile$name),"zip",sep=".") },
      content = function(filename) {
         files <- c()
         tempdir <- tempdir()
         setwd(tempdir())
         inFile <- input$slowSplitFile
         results <- slowSplitInput()
         
         fname <- file_path_sans_ext(inFile$name)
         maxfilesize <- 9 * 1024 * 1024
         filenum <- 1
         files <- c(files,paste(fname,filenum,".csv",sep=""))
         
         write.csv(results[0,],paste(fname,filenum,".csv",sep=""),na='',row.names=F)
         for (row in 1:nrow(results)) {
            
            if(file.info(paste(fname,filenum,".csv",sep=""))$size >= maxfilesize) {
               filenum <- filenum + 1
               files <- c(files,paste(fname,filenum,".csv",sep=""))
               write.csv(results[0,],paste(fname,filenum,".csv",sep=""),na='',row.names=F)
            }
            
            write.table(results[row,],paste(fname,filenum,".csv",sep=""),na='',sep=',',row.names=F,col.names=F,append=T)
         }
         zip(zipfile=filename, files=files)
      },
      contentType = "application/zip"
   )
   
   ### CSV Splitter (fast)
   
   fastSplitInput <- reactive({
      inFile <- input$fastSplitFile

      if (is.null(inFile))
         return(NULL)
      
      "hello"
   })
   
   output$fastSplitContent <- renderTable({
      fastSplitInput()
   })
   
   output$fastSplitDownload <- downloadHandler(
      filename = function() { paste(as.character(input$fastSplitFile$name),"zip",sep=".") },
      content = function(filename) {
         files <- c()
         tempdir <- tempdir()
         setwd(tempdir())
         inFile <- input$fastSplitFile
         
         fname <- file_path_sans_ext(inFile$name)
         total <- 0
         filenum <- 1
         firstLine <- NA
         maxfilesize <- 9.5 * 1024 * 1024
         lines <- readLines(inFile$datapath)
         
         files <- c(files,paste(fname,filenum,".csv",sep=""))
         
         sink(paste(fname,filenum,'.csv',sep=''))
         
         for(line in lines) {
            
            if(is.na(firstLine))
               firstLine <- line
            length <- nchar(line)
            
            if(total + length >= maxfilesize) {
               filenum <- filenum + 1
               total <- 0
               sink()
               files <- c(files,paste(fname,filenum,".csv",sep=""))
               sink(paste(fname,filenum,'.csv',sep=''))
               writeLines(firstLine)
               length <- length + nchar(firstLine)
            }
            
            writeLines(line)
            total <- total + length + 2
         }
         sink()
         
         zip(zipfile=filename, files=files)
      },
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
      inFile <- input$filesmasherFile
      
      if (is.null(inFile))
         return(NULL)
      
      # <CODE FOR SMASHING CSVS TOGETHER (ROW BIND)>
      # <RETURN A DATA FRAME>
   })
   
   output$filesmasherContent <- renderTable({
      filesmasherInput()
   })
   
   output$filesmasherDownload <- downloadHandler(
      filename = function() { as.character(input$filesmasherFile) },
      content = function(file) {
         write.csv(filesmasherInput(), file, row.names = F)
      }
   )
   
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