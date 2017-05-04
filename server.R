library(shiny)
library(tools)

#set maximum upload filesize, in MBs
options(shiny.maxRequestSize=200*1024^2)
function(input, output, session) {
  session$onSessionEnded(stopApp)
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
    
    #set the file name based on if one or more files are selected
    filename = function(){if(length(blankColumnsInput()) > 1) {
      "output.zip"
    }else {
      paste("clean_",as.character(input$blankColumnsFile), sep="")
    }},
    
    #zip list of dataframes as folder of csvs
    content = function(filename) {
      #if more than 1 file is selected zip
      if(length(blankColumnsInput()) > 1){
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
      }
      #if one file is selected write csv
      else{
        #get input data
        inFile <- input$blankColumnsFile
        
        #get the file name
        fname <- file_path_sans_ext(inFile$name)
        
        #run function to remove blank columns
        blank <- blankColumnsInput()
        
        #write to csv and output
        write.csv(blank, filename, na="", row.names = F, fileEncoding = "UTF-8")
      }
    }
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
  
  ### Salesforce Cleaner --- Done * Need to Comment *
  
  salesforceInput <- reactive({
    inFile <- input$salesforceFile
    
    if (is.null(inFile))
      return(NULL)
    
  })
  
  output$salesforceContent <- renderTable({
    salesforceInput()
  })
  
  output$salesforceDownload <- downloadHandler(
    
    filename = "salesforceoutput.zip",
    content = function(filename) {
      files <- c()
      inFile <- input$salesforceFile
      fileList <- inFile$name
      source("SalesforceMaster.R")
      #create and set working directory to temp directory
      tempdir <- tempdir()
      setwd(tempdir())
      if ((is.element("Contact.csv",fileList)) & ! (is.element("Account.csv",fileList))) {
        if (is.element("User.csv",fileList))
        {
          #fCcAncAnUy function
          contact <- read.csv(inFile[inFile$name=="Contact.csv",]$datapath, header = T)
          user <- read.csv(inFile[inFile$name=="User.csv",]$datapath, header = T)
          contactc <- fCcAncAnUy(contact,user)
          #write current dataframe to csv
          write.csv(contactc, "Contact.csv", na="", row.names = F)
          files <- c(files,"Contact.csv")
        }
        else
        {
          #fCcAncAnUn function
          contact <- read.csv(inFile[inFile$name=="Contact.csv",]$datapath, header = T)
          contactc <- fCcAncAnUn(contact)
          #write current dataframe to csv
          write.csv(contactc, "Contact.csv", na="", row.names = F)
          files <- c(files,"Contact.csv")
        }
      }
      if ((is.element("Account.csv",fileList)) & ! (is.element("Contact.csv",fileList)))
      {
        if (is.element("User.csv",fileList))
        {
          #AcCncUy function
          account <- read.csv(inFile[inFile$name=="Account.csv",]$datapath, header = T)
          user <- read.csv(inFile[inFile$name=="User.csv",]$datapath, header = T)
          accountc <- AcCncUy(account,user)
          #write current dataframe to csv
          write.csv(accountc, "Account.csv", na="", row.names = F)
          files <- c(files,"Account.csv")
        }
        else
        {
          #AcCncUn function
          account <- read.csv(inFile[inFile$name=="Account.csv",]$datapath, header = T)
          accountc <- AcCncUn(account)
          #write current dataframe to csv
          write.csv(accountc, "Account.csv", na="", row.names = F)
          files <- c(files,"Account.csv")
        }
      }
      else if ((is.element("Contact.csv",fileList)) & (is.element("Account.csv",fileList)))
      {
        if (is.element("User.csv",fileList))
        {
          #CcAcUy function
          contact <- read.csv(inFile[inFile$name=="Contact.csv",]$datapath, header = T)
          account <- read.csv(inFile[inFile$name=="Account.csv",]$datapath, header = T)
          user <- read.csv(inFile[inFile$name=="User.csv",]$datapath, header = T)
          
          #add output to list since it returns 2 objects
          acctcontList <- CcAcUy(contact, account, user)
          
          #write current dataframe to csv
          write.csv(acctcontList$account, "Account.csv", na="", row.names = F)
          write.csv(acctcontList$contact, "Contact.csv", na="", row.names = F)
          files <- c(files,"Account.csv","Contact.csv")
        }
        else
        {
          #CcAcUn function
          contact <- read.csv(inFile[inFile$name=="Contact.csv",]$datapath, header = T)
          account <- read.csv(inFile[inFile$name=="Account.csv",]$datapath, header = T)
          acctcontList <- CcAcUn(contact, account)
          #write current dataframe to csv
          write.csv(acctcontList$account, "Account.csv", na="", row.names = F)
          write.csv(acctcontList$contact, "Contact.csv", na="", row.names = F)
          files <- c(files,"Account.csv","Contact.csv")
        }
      }
      if (is.element("Lead.csv",fileList))
      {
        if (is.element("User.csv",fileList))
        {
          #LeadUy function
          lead <- read.csv(inFile[inFile$name=="Lead.csv",]$datapath, header = T)
          user <- read.csv(inFile[inFile$name=="User.csv",]$datapath, header = T)
          leadc <- LeadUy(lead, user)
          #write current dataframe to csv
          write.csv(leadc, "Lead.csv", na="", row.names = F)
          files <- c(files,"Lead.csv")
        }
        else
        {
          #LeadUn function
          lead <- read.csv(inFile[inFile$name=="Lead.csv",]$datapath, header = T)
          leadc <- LeadUn(lead)
          #write current dataframe to csv
          write.csv(leadc, "Lead.csv", na="", row.names = F)
          files <- c(files,"Lead.csv")
        }
      }
      if (is.element("Opportunity.csv",fileList))
      {
        if (is.element("User.csv",fileList))
        {
          #OpportunitiesUy function
          user <- read.csv(inFile[inFile$name=="User.csv",]$datapath, header = T)
          opportunity <- read.csv(inFile[inFile$name=="Opportunity.csv",]$datapath, header = T)
          opportunityc <- OpportunitiesUy(opportunity,user)
          write.csv(opportunityc, "Opportunity.csv", na="", row.names = F)
          files <- c(files,"Opportunity.csv")
        }
        else
        {
          #OpportunitiesUn function
          opportunity <- read.csv(inFile[inFile$name=="Opportunity.csv",]$datapath, header = T)
          opportunityc <- OpportunitiesUn(opportunity)
          write.csv(opportunityc, "Opportunity.csv", na="", row.names = F)
          files <- c(files,"Opportunity.csv")
        }
      }
      if (is.element("Note.csv",fileList))
      {
        if (is.element("User.csv",fileList))
        {
          #NoteUy function
          user <- read.csv(inFile[inFile$name=="User.csv",]$datapath, header = T)
          note <- read.csv(inFile[inFile$name=="Note.csv",]$datapath, header = T)
          notec <- NoteUy(note,user)
          write.csv(notec, "Note.csv", na="", row.names = F)
          files <- c(files,"Note.csv")
        }
        else
        {
          #NoteUn function
          note <- read.csv(inFile[inFile$name=="Note.csv",]$datapath, header = T)
          notec <- NoteUn(note)
          write.csv(notec, "Note.csv", na="", row.names = F)
          files <- c(files,"Note.csv")
        }
      }
      if (is.element("Task.csv",fileList))
      {
        if (is.element("User.csv",fileList))
        {
          #TaskUy function
          user <- read.csv(inFile[inFile$name=="User.csv",]$datapath, header = T)
          task <- read.csv(inFile[inFile$name=="Task.csv",]$datapath, header = T)
          taskc <- TaskUy(task,user)
          write.csv(taskc, "Task.csv", na="", row.names = F)
          files <- c(files,"Task.csv")
        }
        else
        {
          #TaskUn function
          task <- read.csv(inFile[inFile$name=="Task.csv",]$datapath, header = T)
          taskc <- TaskUn(task)
          write.csv(taskc, "Task.csv", na="", row.names = F)
          files <- c(files,"Task.csv")
        }
      }
      #zip all files and return as content
      zip(zipfile=filename, files=files)
    
    #ensure filetype is zip
    contentType = "application/zip"
    
    #removes all saved variables and functions ---- Does not work
    #remove('AcCncUn')
    #rm(list=ls(all=T))
    }
    )
  
  ### Filesmasher --- DONE
  
  filesmasherInput <- reactive({
    #library(plyr)
     library(dplyr)
     library(tools)
    #get input file info and ensure it's not null
    inFile <- input$filesmasherFile
    
    if (is.null(inFile))
      return(NULL)
    
    df <- data.frame()
    
    for(i in 1:length(inFile$datapath)) {
       
       file <- read.csv(inFile$datapath[i],header=T)
       
       file$filename <- file_path_sans_ext(inFile$name[i])
       
       file <- as.data.frame(sapply(file,as.factor))
       
       df <- bind_rows(df,file)
    }
    
    df
    #reads the list of files to data frame and stores it in a list
    #smashedFiles<- lapply(inFile$datapath, read.csv, header = T, stringsAsFactors = F)
    #rbind.fill(smashedFiles)
    
    
  })
  
  #renders the output of the reactive to a table on the UI
  output$filesmasherContent <- renderTable({
    filesmasherInput()
  })
  
  #download button handler
  output$filesmasherDownload <- downloadHandler(
    
    #name the file it will be
    filename = "output.csv",
    
    content = function(filename){
      
      #call the file smasher function
      smashed <- filesmasherInput()
      
      #write to csv and output
      write.csv(smashed, filename, na="", row.names = F)
    })
  
  ### CSV Merge --- DONE
  
  csvMergeInput <- reactive({
    inFile <- input$csvMergeFile
    
    if (is.null(inFile))
      return(NULL)
    #UID <- renderText({input$uniqueId})
    UID <- as.character(input$uniqueId)
    
    #load the 1st data frame
    mergeFile1 <- read.csv(inFile$datapath[1], header = T)
    
    #load the 2nd data frame
    mergeFile2 <- read.csv(inFile$datapath[2], header = T)
    
    #use merge function to merge the 2 files together
    merge(mergeFile1, mergeFile2, by.x = UID, by.y = UID, all.x = TRUE)
  })
  
  output$csvMergeContent <- renderTable({
    csvMergeInput()
  })
  
  output$csvMergeDownload <- downloadHandler(
    
    #name the file it will be
    filename = "output.csv",
    
    content = function(filename){
      
      #call the merge function
      csvMerged <- csvMergeInput()
      
      #write to csv and then output file
      write.csv(csvMerged, filename, na="", row.names = F)
    })
  
  ### CSV Converter
  
  csvConverterInput <- reactive({
    #bring in the libraries to parse the different filetypes
    library(jsonlite)
    library(XML)
    library(yaml)
    
    #store input file data
    inFile <- input$csvConverterFile
    
    #ensure file is uploaded
    if (is.null(inFile))
      return(NULL)
    
    #get the file extension
    ext <- file_ext(inFile)
    
    #create empty list to add parsed data into
    parsedList <- list()
    
    #checks the file extension, and runs different methods based on the file extension
    if(ext=="xml") {
      #parse XML
      xml <- xmlParse(inFile$datapath,useInternalNodes = T,options=NOCDATA)
      
      #convert XML to list (which the flatten function needs)
      parsedList <- xmlToList(xml)
    } else if(ext=="json") {
      
      #parse JSON
      parsedList <- fromJSON(inFile$datapath,simplifyDataFrame = F)
      
      #convert any NULL values to NA
      parsedList <- lapply(parsedList, function(x) {
        x[sapply(x, is.null)] <- NA
        return(x)
      })
    } else if(ext=="yml"|ext=="yaml"|ext=="txt") {
      #parse YAML
      parsedList <- yaml.load_file(inFile$datapath)
    }
    
    #=======================================================================================
    #List Flattener functions
    #source: http://stackoverflow.com/questions/26177565/converting-nested-list-to-dataframe
    
    tl <- function(e) { if (is.null(e)) return(NULL); ret <- typeof(e); if (ret == 'list' && !is.null(names(e))) ret <- list(type='namedlist') else ret <- list(type=ret,len=length(e)); ret; };
    mkcsv <- function(v) paste0(collapse=',',v);
    keyListToStr <- function(keyList) paste0(collapse='','/',sapply(keyList,function(key) if (is.null(key)) '*' else paste0(collapse=',',key)));
    
    extractLevelColumns <- function(
      nodes, ## current level node selection
      ..., ## additional arguments to data.frame()
      keyList=list(), ## current key path under main list
      sep=NULL, ## optional string separator on which to join multi-element vectors; if NULL, will leave as separate columns
      mkname=function(keyList,maxLen) paste0(collapse='.',if (is.null(sep) && maxLen == 1L) keyList[-length(keyList)] else keyList) ## name builder from current keyList and character vector max length across node level; default to dot-separated keys, and remove last index component for scalars
    ) {
      cat(sprintf('extractLevelColumns(): %s\n',keyListToStr(keyList)));
      if (length(nodes) == 0L) return(list()); ## handle corner case of empty main list
      tlList <- lapply(nodes,tl);
      typeList <- do.call(c,lapply(tlList,`[[`,'type'));
      if (length(unique(typeList)) != 1L) stop(sprintf('error: inconsistent types (%s) at %s.',mkcsv(typeList),keyListToStr(keyList)));
      type <- typeList[1L];
      if (type == 'namedlist') { ## hash; recurse
        allKeys <- unique(do.call(c,lapply(nodes,names)));
        ret <- do.call(c,lapply(allKeys,function(key) extractLevelColumns(lapply(nodes,`[[`,key),...,keyList=c(keyList,key),sep=sep,mkname=mkname)));
      } else if (type == 'list') { ## array; recurse
        lenList <- do.call(c,lapply(tlList,`[[`,'len'));
        maxLen <- max(lenList,na.rm=T);
        allIndexes <- seq_len(maxLen);
        ret <- do.call(c,lapply(allIndexes,function(index) extractLevelColumns(lapply(nodes,function(node) if (length(node) < index) NULL else node[[index]]),...,keyList=c(keyList,index),sep=sep,mkname=mkname))); ## must be careful to translate out-of-bounds to NULL; happens automatically with string keys, but not with integer indexes
      } else if (type%in%c('raw','logical','integer','double','complex','character')) { ## atomic leaf node; build column
        lenList <- do.call(c,lapply(tlList,`[[`,'len'));
        maxLen <- max(lenList,na.rm=T);
        if (is.null(sep)) {
          ret <- lapply(seq_len(maxLen),function(i) setNames(data.frame(sapply(nodes,function(node) if (length(node) < i) NA else node[[i]]),...),mkname(c(keyList,i),maxLen)));
        } else {
          ## keep original type if maxLen is 1, IOW don't stringify
          ret <- list(setNames(data.frame(sapply(nodes,function(node) if (length(node) == 0L) NA else if (maxLen == 1L) node else paste(collapse=sep,node)),...),mkname(keyList,maxLen)));
        }; ## end if
      } else stop(sprintf('error: unsupported type %s at %s.',type,keyListToStr(keyList)));
      if (is.null(ret)) ret <- list(); ## handle corner case of exclusively empty sublists
      ret;
    }; ## end extractLevelColumns()
    ## simple interface function
    flattenList <- function(mainList,...) do.call(cbind,extractLevelColumns(mainList,...));
    #=======================================================================================
    
    #call flatten function and return the data frame
    flattenList(parsedList)
  })
  
  #display data frame that's flattened
  output$csvConverterContent <- renderTable({
    csvConverterInput()
  })
  
  output$csvConverterDownload <- downloadHandler(
    #sets the filename based on the input file
    filename = function() { paste(file_path_sans_ext(as.character(input$csvConverterFile)),".csv",sep="") },
    content = function(file) {
      write.csv(csvConverterInput(), file, row.names = F, na="")
    }
  )
}