# SFMaster

### Naming Convention for functions ###
#f = function
#C = Contacts
#A = Accounts
#c = checked
#n = not
#y = Yes
#U = User

createDirec <- function(folderChanged){
  setwd(folderChanged)
  dir.create(file.path(folderChanged,'/Import'))
}

###################################################
# Function to rearrange 
# Example: columns lead <- arrange.vars(lead, c("OwnerName"=2))
arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

userCleaner <- function(user){
  ################################################## Load User File 
  # Load in the user File to the Environment
  #user <- read.csv("User.csv", header = TRUE)
  # Concatnate 'FirstName' and 'LastName' for User
  # Extract FirstName from User frame as variable
  ufname <- user[,c("FirstName")]
  # Extract LastName from User frame as variable
  ulname <- user[,c("LastName")]
  # Extract Id from User frame as variable
  uid <- user[,c("Id")]
  # Combine FirstName and LastName
  flnamecnotrim <- paste(ufname,ulname, sep = ' ')
  # Trim the white space for owner name
  OwnerName <- trimws(flnamecnotrim)
  # Create Data Frame for user file to use with VLOOKUP
  userc <- data.frame(uid,OwnerName)
  # Remove variables from environment
  remove('ufname','ulname','uid','flnamecnotrim','OwnerName','user')
  return(userc)
}


fCcAncAyUy <- function(folderChanged){
  ################################################## Begin fCcAncAyUy File CleanUp
  # Call UserClean Function
  userc <- userCleaner()
  account <- read.csv("Account.csv", header = TRUE)
  # Extract Id from User frame as variable
  accId <- account[,c("Id")]
  # Extract Name from User frame as variable
  Company <- account[,c("Name")]
  # Create Data Frame for user file to use with VLOOKUP
  acctc <- data.frame(accId,Company)
  # Remove variables from environment
  remove('accId','Company','account')
  
  # Load in Contact file 
  contact <- read.csv("Contact.csv", header = TRUE)
  
  # VLOOKUP Contact File accountname
  contact <- merge(contact, acctc, by.x = "AccountId", by.y = "accId", all.x = TRUE)
  # VLOOKUP Contact File for username
  contact <- merge(contact, userc, by.x = "OwnerId", by.y = "uid", all.x = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  contact$DateCreated <- strptime(as.character(contact$CreatedDate), "%Y-%m-%d")
  contact$DateCreated <- format(contact$DateCreated, "%m/%d/%Y")
  
  # If 'HasOptedOutOfEmail' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfEmail) {
    drop <- c("HasOptedOutOfEmail")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfEmail Column")
  
  # If 'DoNotCall' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$DoNotCall) {
    drop <- c("DoNotCall")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete DoNotCall Column")
  
  # If 'HasOptedOutOfFax' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfFax) {
    drop <- c("HasOptedOutOfFax")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfFax Column")
  
  
  # Rename Titles to match Infusionsoft
  names(contact)[names(contact)=="Title"] <- "JobTitle"
  names(contact)[names(contact)=="Salutation"] <- "Title"
  names(contact)[names(contact)=="Id"] <- "contactId"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","IsConverted","ConvertedAccountId","ConvertedContactId","ConvertedOpportunityId",
            "IsUnreadByOwner","CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp","LastActivityDate",
            "LastTransferDate","Jigsaw","JigsawContactId","EmailBouncedReason","EmailBouncedDate","CreatedDate","OwnerId",
            "LastCURequestDate","LastCUUpdateDate")
  contact <- contact[ , !(names(contact) %in% drop)]
  remove(drop)
  # Rearrange Columns
  contact <- arrange.vars(contact, c("contactId"=1,"FirstName"=2,"LastName"=3,"Title"=4,
                                     "OwnerName"=5,"AccountId"=6,"Company"=7,"DateCreated"=8,
                                     "JobTitle"=9))
  # Delete all columns with no data
  contact <- contact[!sapply(contact, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(contact, file = file.path(folderChanged, "Import/Contact.csv"), na="", row.names = F)
  
  remove(contact, acctc, userc)
  ############################################## End of fCcAncAyUy Cleaning
}

fCcAncAnUy <- function(folderChanged){
  ################################################## Begin fCcAncAnUy File CleanUp
  # Call UserClean Function
  userc <- userCleaner()
  
  # Load in Contact file 
  contact <- read.csv("Contact.csv", header = TRUE)
  
  # VLOOKUP Contact File for username
  contact <- merge(contact, userc, by.x = "OwnerId", by.y = "uid", all.x = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  contact$DateCreated <- strptime(as.character(contact$CreatedDate), "%Y-%m-%d")
  contact$DateCreated <- format(contact$DateCreated, "%m/%d/%Y")
  
  # If 'HasOptedOutOfEmail' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfEmail) {
    drop <- c("HasOptedOutOfEmail")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfEmail Column")
  
  # If 'DoNotCall' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$DoNotCall) {
    drop <- c("DoNotCall")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete DoNotCall Column")
  
  # If 'HasOptedOutOfFax' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfFax) {
    drop <- c("HasOptedOutOfFax")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfFax Column")
  
  
  # Rename Titles to match Infusionsoft
  names(contact)[names(contact)=="Title"] <- "JobTitle"
  names(contact)[names(contact)=="Salutation"] <- "Title"
  names(contact)[names(contact)=="Id"] <- "contactId"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","IsConverted","ConvertedAccountId","ConvertedContactId","ConvertedOpportunityId",
            "IsUnreadByOwner","CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp","LastActivityDate",
            "LastTransferDate","Jigsaw","JigsawContactId","EmailBouncedReason","EmailBouncedDate","CreatedDate","OwnerId",
            "LastCURequestDate","LastCUUpdateDate")
  contact <- contact[ , !(names(contact) %in% drop)]
  remove(drop)
  # Rearrange Columns
  contact <- arrange.vars(contact, c("contactId"=1,"FirstName"=2,"LastName"=3,"Title"=4,
                                     "OwnerName"=5,"AccountId"=6,"Company"=7,"DateCreated"=8,
                                     "JobTitle"=9))
  # Delete all columns with no data
  contact <- contact[!sapply(contact, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(contact, file = file.path(folderChanged, "Import/Contact.csv"), na="", row.names = F)
  
  remove(contact, userc)
  ############################################## End of fCcAncAnUy Cleaning
}

fCcAncAyUn <- function(folderChanged){
  ################################################## Begin fCcAncAyUn File CleanUp
  account <- read.csv("Account.csv", header = TRUE)
  # Extract Id from User frame as variable
  accId <- account[,c("Id")]
  # Extract Name from User frame as variable
  Company <- account[,c("Name")]
  # Create Data Frame for user file to use with VLOOKUP
  acctc <- data.frame(accId,Company)
  # Remove variables from environment
  remove('accId','Company','account')
  
  # Load in Contact file 
  contact <- read.csv("Contact.csv", header = TRUE)
  
  # VLOOKUP Contact File accountname
  contact <- merge(contact, acctc, by.x = "AccountId", by.y = "accId", all.x = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  contact$DateCreated <- strptime(as.character(contact$CreatedDate), "%Y-%m-%d")
  contact$DateCreated <- format(contact$DateCreated, "%m/%d/%Y")
  
  # If 'HasOptedOutOfEmail' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfEmail) {
    drop <- c("HasOptedOutOfEmail")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfEmail Column")
  
  # If 'DoNotCall' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$DoNotCall) {
    drop <- c("DoNotCall")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete DoNotCall Column")
  
  # If 'HasOptedOutOfFax' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfFax) {
    drop <- c("HasOptedOutOfFax")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfFax Column")
  
  
  # Rename Titles to match Infusionsoft
  names(contact)[names(contact)=="Title"] <- "JobTitle"
  names(contact)[names(contact)=="Salutation"] <- "Title"
  names(contact)[names(contact)=="Id"] <- "contactId"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","IsConverted","ConvertedAccountId","ConvertedContactId","ConvertedOpportunityId",
            "IsUnreadByOwner","CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp","LastActivityDate",
            "LastTransferDate","Jigsaw","JigsawContactId","EmailBouncedReason","EmailBouncedDate","CreatedDate",
            "LastCURequestDate","LastCUUpdateDate")
  contact <- contact[ , !(names(contact) %in% drop)]
  remove(drop)
  # Rearrange Columns
  contact <- arrange.vars(contact, c("contactId"=1,"FirstName"=2,"LastName"=3,"Title"=4,"AccountId"=5,"Company"=6,"DateCreated"=7,
                                     "JobTitle"=8))
  # Delete all columns with no data
  contact <- contact[!sapply(contact, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(contact, file = file.path(folderChanged, "Import/Contact.csv"), na="", row.names = F)
  
  remove(contact, acctc)
  ############################################## End of fCcAncAyUn Cleaning
}



fCcAncAnUn <- function(folderChanged){
  ################################################## Begin fCcAncAnUn File CleanUp
  
  # Load in Contact file 
  contact <- read.csv("Contact.csv", header = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  contact$DateCreated <- strptime(as.character(contact$CreatedDate), "%Y-%m-%d")
  contact$DateCreated <- format(contact$DateCreated, "%m/%d/%Y")
  
  # If 'HasOptedOutOfEmail' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfEmail) {
    drop <- c("HasOptedOutOfEmail")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfEmail Column")
  
  # If 'DoNotCall' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$DoNotCall) {
    drop <- c("DoNotCall")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete DoNotCall Column")
  
  # If 'HasOptedOutOfFax' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfFax) {
    drop <- c("HasOptedOutOfFax")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfFax Column")
  
  
  # Rename Titles to match Infusionsoft
  names(contact)[names(contact)=="Title"] <- "JobTitle"
  names(contact)[names(contact)=="Salutation"] <- "Title"
  names(contact)[names(contact)=="Id"] <- "contactId"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","IsConverted","ConvertedAccountId","ConvertedContactId","ConvertedOpportunityId",
            "IsUnreadByOwner","CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp","LastActivityDate",
            "LastTransferDate","Jigsaw","JigsawContactId","EmailBouncedReason","EmailBouncedDate","CreatedDate",
            "LastCURequestDate","LastCUUpdateDate")
  contact <- contact[ , !(names(contact) %in% drop)]
  remove(drop)
  # Rearrange Columns
  contact <- arrange.vars(contact, c("contactId"=1,"FirstName"=2,"LastName"=3,"Title"=4,
                                     "OwnerId"=5,"AccountId"=6,"Company"=7,"DateCreated"=8,
                                     "JobTitle"=9))
  # Delete all columns with no data
  contact <- contact[!sapply(contact, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(contact, file = file.path(folderChanged, "Import/Contact.csv"), na="", row.names = F)
  
  remove(contact)
  ############################################## End of fCcAncAnUn Cleaning
}

################################################## Begin AcCncUy File CleanUp
AcCncUy <- function(folderChanged){
  # Call UserClean Function
  userc <- userCleaner()
  account <- read.csv("Account.csv", header = TRUE)
  
  # VLOOKUP User File for username
  account <- merge(account, userc, by.x = "OwnerId", by.y = "uid", all.x = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  account$DateCreated <- strptime(as.character(account$CreatedDate), "%Y-%m-%d")
  account$DateCreated <- format(account$DateCreated, "%m/%d/%Y")
  
  
  # Rename Titles to match Infusionsoft
  names(account)[names(account)=="Id"] <- "CompanyId"
  names(account)[names(account)=="Name"] <- "Company"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","MasterRecordId","ParentId","TickerSymbol","CreatedById","LastModifiedDate",
            "LastModifiedById","SystemModstamp","LastActivityDate","Jigsaw","JigsawCompanyId","OwnerId")
  account <- account[ , !(names(account) %in% drop)]
  remove(drop)
  
  # Rearrange Columns
  account <- arrange.vars(account, c("CompanyId"=1,"Company"=2, "DateCreated"=3))
  
  # Delete all columns with no data
  account <- account[!sapply(account, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(account, file = file.path(folderChanged, "Import/Account.csv"), na="", row.names = F)
  remove(account, userc)
}
############################################## End of AcCncUy Cleaning

################################################## Begin AcCncUn File CleanUp
AcCncUn <- function(folderChanged){
  account <- read.csv("Account.csv", header = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  account$DateCreated <- strptime(as.character(account$CreatedDate), "%Y-%m-%d")
  account$DateCreated <- format(account$DateCreated, "%m/%d/%Y")
  
  
  # Rename Titles to match Infusionsoft
  names(account)[names(account)=="Id"] <- "CompanyId"
  names(account)[names(account)=="Name"] <- "Company"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","MasterRecordId","ParentId","TickerSymbol","CreatedById","LastModifiedDate",
            "LastModifiedById","SystemModstamp","LastActivityDate","Jigsaw","JigsawCompanyId")
  account <- account[ , !(names(account) %in% drop)]
  remove(drop)
  
  # Rearrange Columns
  account <- arrange.vars(account, c("CompanyId"=1,"Company"=2, "DateCreated"=3))
  
  # Delete all columns with no data
  account <- account[!sapply(account, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(account, file = file.path(folderChanged, "Import/Account.csv"), na="", row.names = F)
  remove(account)
}
############################################## End of AcCncUn Cleaning


################################################## Start CcAcUy File CleanUp
CcAcUy <- function(folderChanged){
  # Call UserClean Function
  userc <- userCleaner()
  
  account <- read.csv("Account.csv", header = TRUE)
  # Extract Id from User frame as variable
  accId <- account[,c("Id")]
  # Extract Name from User frame as variable
  Company <- account[,c("Name")]
  # Create Data Frame for user file to use with VLOOKUP
  acctc <- data.frame(accId,Company)
  # Remove variables from environment
  remove('accId','Company','account')
  
  # Load in Contact file 
  contact <- read.csv("Contact.csv", header = TRUE)
  
  # VLOOKUP Contact File accountname
  contact <- merge(contact, acctc, by.x = "AccountId", by.y = "accId", all.x = TRUE)
  # VLOOKUP Contact File for username
  contact <- merge(contact, userc, by.x = "OwnerId", by.y = "uid", all.x = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  contact$DateCreated <- strptime(as.character(contact$CreatedDate), "%Y-%m-%d")
  contact$DateCreated <- format(contact$DateCreated, "%m/%d/%Y")
  
  # If 'HasOptedOutOfEmail' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfEmail) {
    drop <- c("HasOptedOutOfEmail")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfEmail Column")
  
  # If 'DoNotCall' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$DoNotCall) {
    drop <- c("DoNotCall")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete DoNotCall Column")
  
  # If 'HasOptedOutOfFax' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfFax) {
    drop <- c("HasOptedOutOfFax")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfFax Column")
  
  
  # Rename Titles to match Infusionsoft
  names(contact)[names(contact)=="Title"] <- "JobTitle"
  names(contact)[names(contact)=="Salutation"] <- "Title"
  names(contact)[names(contact)=="Id"] <- "contactId"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","IsConverted","ConvertedAccountId","ConvertedContactId","ConvertedOpportunityId",
            "IsUnreadByOwner","CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp","LastActivityDate",
            "LastTransferDate","Jigsaw","JigsawContactId","EmailBouncedReason","EmailBouncedDate","CreatedDate","OwnerId",
            "LastCURequestDate","LastCUUpdateDate")
  contact <- contact[ , !(names(contact) %in% drop)]
  remove(drop)
  # Rearrange Columns
  contact <- arrange.vars(contact, c("contactId"=1,"FirstName"=2,"LastName"=3,"Title"=4,
                                     "OwnerName"=5,"AccountId"=6,"Company"=7,"DateCreated"=8,
                                     "JobTitle"=9))
  # Delete all columns with no data
  contact <- contact[!sapply(contact, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(contact, file = file.path(folderChanged, "Import/Contact.csv"), na="", row.names = F)
  remove(contact, acctc)
  
  ################################################## Begin Account File CleanUp
  account <- read.csv("Account.csv", header = TRUE)
  
  # VLOOKUP User File for username
  account <- merge(account, userc, by.x = "OwnerId", by.y = "uid", all.x = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  account$DateCreated <- strptime(as.character(account$CreatedDate), "%Y-%m-%d")
  account$DateCreated <- format(account$DateCreated, "%m/%d/%Y")
  
  
  # Rename Titles to match Infusionsoft
  names(account)[names(account)=="Id"] <- "CompanyId"
  names(account)[names(account)=="Name"] <- "Company"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","MasterRecordId","ParentId","TickerSymbol","CreatedById","LastModifiedDate",
            "LastModifiedById","SystemModstamp","LastActivityDate","Jigsaw","JigsawCompanyId","OwnerId")
  account <- account[ , !(names(account) %in% drop)]
  remove(drop)
  
  # Rearrange Columns
  account <- arrange.vars(account, c("CompanyId"=1,"Company"=2, "DateCreated"=3))
  
  # Delete all columns with no data
  account <- account[!sapply(account, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(account, file = file.path(folderChanged, "Import/Account.csv"), na="", row.names = F)
  
  remove(account, userc)
}
################################################## End CcAcUy File CleanUp

################################################## Start CcAcUn File CleanUp
CcAcUn <- function(folderChanged){
  
  account <- read.csv("Account.csv", header = TRUE)
  # Extract Id from User frame as variable
  accId <- account[,c("Id")]
  # Extract Name from User frame as variable
  Company <- account[,c("Name")]
  # Create Data Frame for user file to use with VLOOKUP
  acctc <- data.frame(accId,Company)
  # Remove variables from environment
  remove('accId','Company','account')
  
  # Load in Contact file 
  contact <- read.csv("Contact.csv", header = TRUE)
  
  # VLOOKUP Contact File accountname
  contact <- merge(contact, acctc, by.x = "AccountId", by.y = "accId", all.x = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  contact$DateCreated <- strptime(as.character(contact$CreatedDate), "%Y-%m-%d")
  contact$DateCreated <- format(contact$DateCreated, "%m/%d/%Y")
  
  # If 'HasOptedOutOfEmail' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfEmail) {
    drop <- c("HasOptedOutOfEmail")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfEmail Column")
  
  # If 'DoNotCall' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$DoNotCall) {
    drop <- c("DoNotCall")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete DoNotCall Column")
  
  # If 'HasOptedOutOfFax' does not have a 1 then delete column
  if (FALSE == '1' %in% contact$HasOptedOutOfFax) {
    drop <- c("HasOptedOutOfFax")
    contact <- contact[ , !(names(contact) %in% drop)]
    remove(drop)
  } else print("Contact:No Need to Delete HasOptedOutOfFax Column")
  
  
  # Rename Titles to match Infusionsoft
  names(contact)[names(contact)=="Title"] <- "JobTitle"
  names(contact)[names(contact)=="Salutation"] <- "Title"
  names(contact)[names(contact)=="Id"] <- "contactId"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","IsConverted","ConvertedAccountId","ConvertedContactId","ConvertedOpportunityId",
            "IsUnreadByOwner","CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp","LastActivityDate",
            "LastTransferDate","Jigsaw","JigsawContactId","EmailBouncedReason","EmailBouncedDate","CreatedDate",
            "LastCURequestDate","LastCUUpdateDate")
  contact <- contact[ , !(names(contact) %in% drop)]
  remove(drop)
  # Rearrange Columns
  contact <- arrange.vars(contact, c("contactId"=1,"FirstName"=2,"LastName"=3,"Title"=4,
                                     "OwnerId"=5,"AccountId"=6,"Company"=7,"DateCreated"=8,
                                     "JobTitle"=9))
  # Delete all columns with no data
  contact <- contact[!sapply(contact, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(contact, file = file.path(folderChanged, "Import/Contact.csv"), na="", row.names = F)
  remove(contact, acctc)
  
  ################################################## Begin Account File CleanUp
  account <- read.csv("Account.csv", header = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  account$DateCreated <- strptime(as.character(account$CreatedDate), "%Y-%m-%d")
  account$DateCreated <- format(account$DateCreated, "%m/%d/%Y")
  
  
  # Rename Titles to match Infusionsoft
  names(account)[names(account)=="Id"] <- "CompanyId"
  names(account)[names(account)=="Name"] <- "Company"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","MasterRecordId","ParentId","TickerSymbol","CreatedById","LastModifiedDate",
            "LastModifiedById","SystemModstamp","LastActivityDate","Jigsaw","JigsawCompanyId")
  account <- account[ , !(names(account) %in% drop)]
  remove(drop)
  
  # Rearrange Columns
  account <- arrange.vars(account, c("CompanyId"=1,"Company"=2, "DateCreated"=3))
  
  # Delete all columns with no data
  account <- account[!sapply(account, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(account, file = file.path(folderChanged, "Import/Account.csv"), na="", row.names = F)
  
  remove(account)
}
################################################## End CcAcUn File CleanUp


################################################## Begin LeadUy File CleanUp
LeadUy <- function(lead, user){ #folderChanged
  
  # Call UserClean Function
  userc <- userCleaner(user)
  
  # Load in Lead file
  #lead <- read.csv("Lead.csv", header = TRUE)
  # VLOOKUP Lead File 
  lead <- merge(lead, userc, by.x = "OwnerId", by.y = "uid", all.x = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  lead$DateCreated <- strptime(as.character(lead$CreatedDate), "%Y-%m-%d")
  lead$DateCreated <- format(lead$DateCreated, "%m/%d/%Y")
  
  # If 'HasOptedOutOfEmail' does not have a 1 then delete column
  if (FALSE == '1' %in% lead$HasOptedOutOfEmail) {
    drop <- c("HasOptedOutOfEmail")
    lead <- lead[ , !(names(lead) %in% drop)]
    remove(drop)
  } else print("Lead:No Need to Delete HasOptedOutOfEmail Column")
  
  # If 'DoNotCall' does not have a 1 then delete column
  if (FALSE == '1' %in% lead$DoNotCall) {
    drop <- c("DoNotCall")
    lead <- lead[ , !(names(lead) %in% drop)]
    remove(drop)
  } else print("Lead:No Need to Delete DoNotCall Column")
  
  # If 'HasOptedOutOfFax' does not have a 1 then delete column
  if (FALSE == '1' %in% lead$HasOptedOutOfFax) {
    drop <- c("HasOptedOutOfFax")
    lead <- lead[ , !(names(lead) %in% drop)]
    remove(drop)
  } else print("Lead:No Need to Delete HasOptedOutOfFax Column")
  
  # Rename Titles to match Infusionsoft
  names(lead)[names(lead)=="Title"] <- "JobTitle"
  names(lead)[names(lead)=="Salutation"] <- "Title"
  names(lead)[names(lead)=="Id"] <- "LeadId"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","IsConverted","ConvertedAccountId","ConvertedContactId",
            "ConvertedOpportunityId","IsUnreadByOwner","CreatedById","LastModifiedDate",
            "LastModifiedById","SystemModstamp","LastActivityDate","LastTransferDate",
            "Jigsaw","JigsawContactId","EmailBouncedReason","EmailBouncedDate","CreatedDate",
            "OwnerId")
  lead <- lead[ , !(names(lead) %in% drop)]
  remove(drop)
  
  # Rearrange Columns
  lead <- arrange.vars(lead, c("OwnerName"=2,"DateCreated"=3))
  
  # Delete all columns with no data
  lead <- lead[!sapply(lead, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  #write.csv(lead, file = file.path(folderChanged, "Import/Lead.csv"), na="", row.names = F)
  return(lead)
  #remove(lead, userc)
}
############################################## End of LeadUy Cleaning


################################################## Begin LeadUn File CleanUp
LeadUn <- function(folderChanged){
  # Load in Lead file
  lead <- read.csv("Lead.csv", header = TRUE)
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  lead$DateCreated <- strptime(as.character(lead$CreatedDate), "%Y-%m-%d")
  lead$DateCreated <- format(lead$DateCreated, "%m/%d/%Y")
  
  # If 'HasOptedOutOfEmail' does not have a 1 then delete column
  if (FALSE == '1' %in% lead$HasOptedOutOfEmail) {
    drop <- c("HasOptedOutOfEmail")
    lead <- lead[ , !(names(lead) %in% drop)]
    remove(drop)
  } else print("Lead:No Need to Delete HasOptedOutOfEmail Column")
  
  # If 'DoNotCall' does not have a 1 then delete column
  if (FALSE == '1' %in% lead$DoNotCall) {
    drop <- c("DoNotCall")
    lead <- lead[ , !(names(lead) %in% drop)]
    remove(drop)
  } else print("Lead:No Need to Delete DoNotCall Column")
  
  # If 'HasOptedOutOfFax' does not have a 1 then delete column
  if (FALSE == '1' %in% lead$HasOptedOutOfFax) {
    drop <- c("HasOptedOutOfFax")
    lead <- lead[ , !(names(lead) %in% drop)]
    remove(drop)
  } else print("Lead:No Need to Delete HasOptedOutOfFax Column")
  
  # Rename Titles to match Infusionsoft
  names(lead)[names(lead)=="Title"] <- "JobTitle"
  names(lead)[names(lead)=="Salutation"] <- "Title"
  names(lead)[names(lead)=="Id"] <- "LeadId"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","IsConverted","ConvertedAccountId","ConvertedContactId",
            "ConvertedOpportunityId","IsUnreadByOwner","CreatedById","LastModifiedDate",
            "LastModifiedById","SystemModstamp","LastActivityDate","LastTransferDate",
            "Jigsaw","JigsawContactId","EmailBouncedReason","EmailBouncedDate","CreatedDate")
  lead <- lead[ , !(names(lead) %in% drop)]
  remove(drop)
  
  # Rearrange Columns
  lead <- arrange.vars(lead, c("OwnerId"=2,"DateCreated"=3))
  
  # Delete all columns with no data
  lead <- lead[!sapply(lead, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(lead, file = file.path(folderChanged, "Import/Lead.csv"), na="", row.names = F)
  
  remove(lead)
}
############################################## End of LeadUn Cleaning File CleanUp


############################################## Start of OpportunitiesUy Cleaning
OpportunitiesUy <- function(folderChanged){
  
  # Call UserClean Function
  userc <- userCleaner()
  
  # Read the opportunity file and create a data frame
  opportunity <- read.csv("Opportunity.csv", header = TRUE)
  # VLOOKUP Opportunity File for username
  opportunity <- merge(opportunity, userc, by.x = "OwnerId", by.y = "uid", all.x = TRUE)
  
  # Rename Titles to match Infusionsoft
  names(opportunity)[names(opportunity)=="Probability"] <- "PercentChance"
  names(opportunity)[names(opportunity)=="Name"] <- "Title"
  names(opportunity)[names(opportunity)=="ExpectedRevenue"] <- "ProjectedRevenueHigh"
  names(opportunity)[names(opportunity)=="Amount"] <- "ProjectedRevenueLow"
  names(opportunity)[names(opportunity)=="Description"] <- "Notes"
  names(opportunity)[names(opportunity)=="StageName"] <- "StageId"
  names(opportunity)[names(opportunity)=="CloseDate"] <- "NextActionDate"
  names(opportunity)[names(opportunity)=="Id"] <- "OpportunityId"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","IsPrivate","ForecastCategory","ForecastCategoryName","CampaignId",
            "CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp","FiscalYear",
            "FiscalQuarter","OwnerId")
  opportunity <- opportunity[ , !(names(opportunity) %in% drop)]
  remove(drop)
  
  # Rearrange Columns
  opportunity <- arrange.vars(opportunity, c("OpportunityId"=1,"AccountId"=2, "Title"=3, 
                                             "StageId"=4, "NextActionDate"=5, "Notes"=6,
                                             "PercentChance"=7, "ProjectedRevenueLow"=8,
                                             "ProjectedRevenueHigh"=9, "OwnerName"=10))
  
  # Delete all columns with no data
  opportunity <- opportunity[!sapply(opportunity, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(opportunity, file = file.path(folderChanged, "Import/Opportunity.csv"), na="", row.names = F)
  remove(opportunity, userc)
}
############################################## End of OpportunitiesUy Cleaning


############################################## Start of OpportunitiesUn Cleaning
OpportunitiesUn <- function(folderChanged){
  # Read the opportunity file and create a data frame
  opportunity <- read.csv("Opportunity.csv", header = TRUE)
  
  # Rename Titles to match Infusionsoft
  names(opportunity)[names(opportunity)=="Probability"] <- "PercentChance"
  names(opportunity)[names(opportunity)=="Name"] <- "Title"
  names(opportunity)[names(opportunity)=="ExpectedRevenue"] <- "ProjectedRevenueHigh"
  names(opportunity)[names(opportunity)=="Amount"] <- "ProjectedRevenueLow"
  names(opportunity)[names(opportunity)=="Description"] <- "Notes"
  names(opportunity)[names(opportunity)=="StageName"] <- "StageId"
  names(opportunity)[names(opportunity)=="CloseDate"] <- "NextActionDate"
  names(opportunity)[names(opportunity)=="Id"] <- "OpportunityId"
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","IsPrivate","ForecastCategory","ForecastCategoryName","CampaignId",
            "CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp","FiscalYear",
            "FiscalQuarter")
  opportunity <- opportunity[ , !(names(opportunity) %in% drop)]
  remove(drop)
  
  # Rearrange Columns
  opportunity <- arrange.vars(opportunity, c("OpportunityId"=1,"AccountId"=2, "Title"=3, 
                                             "StageId"=4, "NextActionDate"=5, "Notes"=6,
                                             "PercentChance"=7, "ProjectedRevenueLow"=8,
                                             "ProjectedRevenueHigh"=9, "OwnerId"=10))
  
  # Delete all columns with no data
  opportunity <- opportunity[!sapply(opportunity, function (x) all(is.na(x) | x == ""))]
  
  # Output File ready for import
  write.csv(opportunity, file = file.path(folderChanged, "Import/Opportunity.csv"), na="", row.names = F)
  remove(opportunity)
}
############################################## End of OpportunitiesUn Cleaning


############################################## Start of NoteUy Cleaning
NoteUy <- function(folderChanged){
  
  # Call UserClean Function
  userc <- userCleaner()
  
  # Read the note file and create a data frame
  note <- read.csv("Note.csv", header = TRUE)
  # VLOOKUP Opportunity File for username
  note <- merge(note, userc, by.x = "OwnerId", by.y = "uid", all.x = TRUE)
  
  names(note)[names(note)=="Body"] <- "Notes"
  names(note)[names(note)=="Title"] <- "Description"
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  note$Date <- strptime(as.character(note$CreatedDate), "%Y-%m-%d")
  note$Date <- format(note$Date, "%m/%d/%Y")
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp",
            "IsPrivate","CreatedDate","OwnerId")
  note <- note[ , !(names(note) %in% drop)]
  remove(drop)
  
  # Output File ready for import
  write.csv(note, file = file.path(folderChanged, "Import/Note.csv"), na="", row.names = F)
  remove(note, userc)
}
############################################## End of NoteUy Cleaning


############################################## Start of NoteUn Cleaning
NoteUn <- function(folderChanged){
  # Read the note file and create a data frame
  note <- read.csv("Note.csv", header = TRUE)
  
  names(note)[names(note)=="Body"] <- "Notes"
  names(note)[names(note)=="Title"] <- "Description"
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  note$Date <- strptime(as.character(note$CreatedDate), "%Y-%m-%d")
  note$Date <- format(note$Date, "%m/%d/%Y")
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp",
            "IsPrivate","CreatedDate")
  note <- note[ , !(names(note) %in% drop)]
  remove(drop)
  
  # Output File ready for import
  write.csv(note, file = file.path(folderChanged, "Import/Note.csv"), na="", row.names = F)
  remove(note)
}
############################################## End of NoteUn Cleaning


############################################## Start of TaskUy Cleaning
TaskUy <- function(folderChanged){
  
  # Call UserClean Function
  userc <- userCleaner()
  
  # Read the task file and create a data frame
  task <- read.csv("Task.csv", header = TRUE)
  # VLOOKUP Opportunity File for username
  task <- merge(task, userc, by.x = "OwnerId", by.y = "uid", all.x = TRUE)
  
  names(task)[names(task)=="Description"] <- "Notes"
  names(task)[names(task)=="Subject"] <- "Description"
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  #task$ActivityDate <- strptime(as.character(task$ActivityDate), "%Y-%m-%d %H:%M:%S")
  #task$ActivityDate <- format(task$ActivityDate, "%Y-%m-%d %H:%M:%S")
  
  #if(task$Status == "Completed"){
  #  task$CompletionDate <- as.POSIXct(task$ActivityDate, format = "%m/%d/%Y")
  #} else {task$CompletionDate <- ""}
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp",
            "CreatedDate","OwnerId","IsClosed","IsArchived","EmailMessageId","ActivityOriginType",
            "CallDurationInSeconds","ReminderDateTime","IsReminderSet","RecurrenceActivityId",
            "IsRecurrence","RecurrenceStartDateOnly","RecurrenceEndDateOnly","RecurrenceTimeZoneSidKey",
            "RecurrenceType","RecurrenceInterval","RecurrenceDayOfWeekMask","WhoCount","WhatCount")
  task <- task[ , !(names(task) %in% drop)]
  
  # Delete all columns with no data
  task <- task[!sapply(task, function (x) all(is.na(x) | x == ""))]
  
  remove(drop)
  # Rearrange Columns
  task <- arrange.vars(task, c("Id"=1,"WhoId"=2, "WhatId"=3, 
                               "AccountId"=4, "Description"=5, "Notes"=6,
                               "OwnerName"=7, "Priority"=8,
                               "ActivityDate"=9, "Status"=10))
  
  # Output File ready for import
  write.csv(task, file = file.path(folderChanged, "Import/Task.csv"), na="", row.names = F)
  remove(task, userc)
}
############################################## End of TaskUy Cleaning

############################################## Start of TaskUn Cleaning
TaskUn <- function(folderChanged){
  # Read the task file and create a data frame
  task <- read.csv("Task.csv", header = TRUE)
  
  names(task)[names(task)=="Description"] <- "Notes"
  names(task)[names(task)=="Subject"] <- "Description"
  
  # Change format from YYYY-MM-DD HH:MM:SS to MM/DD/YYYY
  #task$ActivityDate <- strptime(as.character(task$ActivityDate), "%Y-%m-%d %H:%M:%S")
  #task$ActivityDate <- format(task$ActivityDate, "%Y-%m-%d %H:%M:%S")
  
  #if(task$Status == "Completed"){
  #  task$CompletionDate <- as.POSIXct(task$ActivityDate, format = "%m/%d/%Y")
  #} else {task$CompletionDate <- ""}
  
  # Delete Columns that we do not normally import
  drop <- c("IsDeleted","CreatedById","LastModifiedDate","LastModifiedById","SystemModstamp",
            "CreatedDate","IsClosed","IsArchived","EmailMessageId","ActivityOriginType",
            "CallDurationInSeconds","ReminderDateTime","IsReminderSet","RecurrenceActivityId",
            "IsRecurrence","RecurrenceStartDateOnly","RecurrenceEndDateOnly","RecurrenceTimeZoneSidKey",
            "RecurrenceType","RecurrenceInterval","RecurrenceDayOfWeekMask","WhoCount","WhatCount")
  task <- task[ , !(names(task) %in% drop)]
  
  # Delete all columns with no data
  task <- task[!sapply(task, function (x) all(is.na(x) | x == ""))]
  
  remove(drop)
  # Rearrange Columns
  task <- arrange.vars(task, c("Id"=1,"WhoId"=2, "WhatId"=3, 
                               "AccountId"=4, "Description"=5, "Notes"=6,
                               "OwnerId"=7, "Priority"=8,
                               "ActivityDate"=9, "Status"=10))
  
  # Output File ready for import
  write.csv(task, file = file.path(folderChanged, "Import/Task.csv"), na="", row.names = F)
  remove(task)
}
############################################## End of TaskUn Cleaning