require(rvest)
require(lubridate)
require(dplyr)

# Spiegel.local
# Init global variables
spiegel.local.debug                             <- FALSE
spiegel.local.general.rootDirName               <- "data"
spiegel.local.general.rawHeadlineDirName        <- "rawHeadlineFiles"
spiegel.local.general.rawArticleDirName         <- "rawArticleFiles"
spiegel.local.general.articleFileName           <- "article"
spiegel.local.general.prefixForDownloadDir      <- "forDate"
spiegel.local.general.processedHeadlineFilename <- "headlineData.csv"
spiegel.local.general.timezone                  <- "CET"
spiegel.local.general.observedStartDate         <- as_date("2017-06-25")
spiegel.local.general.observedEndDate           <- NULL
spiegel.local.download.startDownloadInDate      <- today()
spiegel.local.general.TimeFormatForFileName     <- "%Y-%m-%d %H-%M-%S %Z"
spiegel.local.general.TimeFormatForImport       <- "%Y-%m-%d %H:%M:%S %Z"

spiegel.local.general.getObservedDates <- function(startDate = spiegel.local.general.getObservedStartDate(), endDate = spiegel.local.general.getObservedEndDate()) {
   if(is.null(endDate) || endDate >= today()) {
      endDate <- today() - days(1)
   }
   
   forDates = startDate + days(0:(endDate - startDate))
   forDates
}

spiegel.local.general.getObservedStartDate <- function() {
   spiegel.local.general.observedStartDate
}

spiegel.local.general.setObservedStartDate <- function(date) {
   spiegel.local.general.observedStartDate <- date
}

spiegel.local.general.getObservedEndDate  <- function() {
   spiegel.local.general.observedEndDate
}

spiegel.local.general.setObservedEndDate  <- function(name) {
   spiegel.local.general.observedEndDate <- name
}


spiegel.local.general.getArticleFileName <- function() {
   spiegel.local.general.articleFileName
}

spiegel.local.general.setArticleFileName <- function(name) {
   spiegel.local.general.articleFileName <- name
}

spiegel.local.general.getProcessedHeadlineFilename <- function() {
   spiegel.local.general.processedHeadlineFilename
}

spiegel.local.general.setprocessedHeadlineFilename <- function(name) {
   spiegel.local.general.headlineFilename <- name
}

spiegel.local.general.getRawHeadlineDirName <- function() {
   spiegel.local.general.rawHeadlineDirName
}

spiegel.local.general.getPrefixForDownloadDir <- function() {
   spiegel.local.general.prefixForDownloadDir
}

spiegel.local.createRootDir <- function() {
   if (spiegel.local.isRootDirAvailable() == FALSE) {
      dir.create(spiegel.local.general.rootDirName)
   }
}

spiegel.local.download.updateStartDownloadInDate <- function() {
   spiegel.local.download.startDownloadInDate <- today()
}

spiegel.local.download.getDownloadInDate <- function() {
   spiegel.local.download.downloadInDate
}

spiegel.local.download.getDownloadDirsFullPath <- function(vecForDates) {
   vecForDates <- format(vecForDates, format = "%Y-%m-%d")
   
   vecPaths <- paste0(spiegel.local.general.rootDirName, 
                     "/", 
                     spiegel.local.general.getPrefixForDownloadDir(), 
                     "-", 
                     vecForDates)
   
   vecPaths
}

# spiegel.local.download.createDownloadDir
spiegel.local.download.createDownloadStructure <- function(vecForDate) {
   spiegel.local.createRootDir()
   
   if(length(vecForDate)) {
      # Create download dirs
      vecDownloadDirsFullPath <- spiegel.local.download.getDownloadDirsFullPath(vecForDate)
      
      for(downloadDirFullPath in vecDownloadDirsFullPath) {
         if (!file.exists(downloadDirFullPath)) {
            dir.create(downloadDirFullPath)
         }
      }
      
      
      # Create raw headline dirs
      vecRawHeadlineDirsFullPath <- paste0(vecDownloadDirsFullPath,
                                       "/", 
                                       spiegel.local.general.getRawHeadlineDirName())
      
      for(rawHeadlineDirFullPath in vecRawHeadlineDirsFullPath) {
         if (file.exists(rawHeadlineDirFullPath) == FALSE) {
            dir.create(rawHeadlineDirFullPath)
         }
      }
      
      # Create article  dirs
      vecRawArticleDirsFullPath <- paste0(vecDownloadDirsFullPath,
                                       "/", 
                                       spiegel.local.general.getRawHeadlineDirName())
      
      for(rawArticleDirsFullPath in vecRawArticleDirsFullPath) {
         if (file.exists(rawArticleDirsFullPath) == FALSE) {
            dir.create(rawArticleDirsFullPath)
         }
      }
   }
}

spiegel.local.general.setRootDirName <- function(name) {
   spiegel.local.general.rootDirName <- name
}

spiegel.local.general.getRootDirName <- function() {
   spiegel.local.general.rootDirName
}

spiegel.local.general.getRootFullPath <- function() {
   spiegel.local.general.rootDirName
}

spiegel.local.headlineOverview.downloadPages <- function(startDate = spiegel.local.general.getObservedStartDate(), endDate = spiegel.local.general.getObservedEndDate(), addUpdates = FALSE) {
   if(is.null(endDate) || endDate >= today()) {
      endDate <- today() - days(1)
   }
   
   spiegel.local.download.updateStartDownloadInDate()
   
   vecForDates <- spiegel.local.general.getObservedDates(startDate, endDate)
   
   if (addUpdates == FALSE) {
      dfDownloadDirs <- spiegel.local.headlineOverview.listDownloadDirs()
      
      if(nrow(dfDownloadDirs) > 0){
         dfList <- spiegel.local.headlineOverview.listHeadlineFiles(spiegel.local.headlineOverview.listDownloadDirs())
         if(nrow(dfList) > 0) {
            vecAvailableForDates <- (dfList %>% select(forDate) %>% distinct())$forDate
            vecForDates <- vecForDates[!(vecForDates %in% vecAvailableForDates)]
         }
      }
   }
   
   spiegel.local.download.createDownloadStructure(vecForDates)
   
   vecURLs <- spiegel.online.archive.url.getURL(vecForDates)
   
   vecFilepaths <- paste0(spiegel.local.download.getDownloadDirsFullPath(vecForDates),
                      "/", 
                      spiegel.local.general.rawHeadlineDirName, 
                      "/", 
                      format(as_datetime(now()), 
                             format = spiegel.local.general.TimeFormatForFileName, 
                             tz=spiegel.local.general.timezone),
                      ".html")
   
   vecURLs.len <- length(vecURLs)
   vecFilepaths.len <- length(vecFilepaths)
   
   if(vecURLs.len > 0 && vecURLs.len == vecFilepaths.len) {
      for(i in 1:vecURLs.len) {
         if (!file.exists(vecFilepaths[i])) {
            download.file(vecURLs[i], vecFilepaths[i], method="curl")
         }
      } 
   }
}

spiegel.local.isRootDirAvailable <- function() {
   if (file.exists(spiegel.local.general.rootDirName)) {
      TRUE
   } else {
      FALSE
   }
}

spiegel.local.headlineOverview.listDownloadDirs <- function() {
   dirs <- list.dirs(path = spiegel.local.general.getRootFullPath(), 
                     full.names = TRUE, 
                     recursive = FALSE)
   
   pattern <- paste0("^",
                    spiegel.local.general.getRootFullPath(), 
                    "/", 
                    spiegel.local.general.getPrefixForDownloadDir(),
                    "-[1-9]{1}[0-9]{3}-[0-9]{1,2}-[0-9]{1,2}")
   
   result <- data_frame(downloadDirFullPath = dirs[grep(pattern, dirs)])
   
   pattern <- paste0("[1-9]{1}[0-9]{3}-[0-9]{1,2}-[0-9]{1,2}")
   reg.out <- regexpr(pattern, result$downloadDirFullPath)
   
   forDate <- data_frame(forDate = as_date(substr(result$downloadDirFullPath, reg.out, reg.out + attr(reg.out,"match.length")-1)))
   df <- cbind(forDate, result)
   
   
   if(spiegel.local.debug) {
      print("spiegel.local.headlineOverview.listDownloadDirs Result:")
      print(df)
   }
   
   df
}

spiegel.local.headlineOverview.haveDownloadDirsRawDir <- function(vecDownloadDirs) {
   vecRawDirPath <- paste0(vecDownloadDirs, "/", spiegel.local.general.getRawHeadlineDirName())
   
   vecResult <- file.exists(vecRawDirPath) == TRUE
   
   if(spiegel.local.debug) {
      print("spiegel.local.headlineOverview.haveDownloadDirsRawDir Result:")
      print(head(result))
   }
   
   vecResult
}


## spiegel.local.headlineOverview.listHeadlineFiles
spiegel.local.headlineOverview.listHeadlineFiles <- function(dfDownloadDirs = spiegel.local.headlineOverview.listDownloadDirs()) {
   if(spiegel.local.debug) {
      print("spiegel.local.headlineOverview.listHeadlineFiles Parameter dfDownloadDirs: ")
      print(dfDownloadDirs)
   }
   
   # take only dirs with raw dir over
   # im download einarbeiten
   dfDownloadDirs <- dfDownloadDirs[spiegel.local.headlineOverview.haveDownloadDirsRawDir(dfDownloadDirs$downloadDirFullPath), ] 
   
   df <- data_frame()
   len <- nrow(dfDownloadDirs)
   
   if(len > 0) {
      for(i in 1:len){
         vecRawDir <- paste0(dfDownloadDirs[i,]$downloadDirFullPath, "/", spiegel.local.general.getRawHeadlineDirName())
         
         pattern <- "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2} [A-Z]{1,4}.html"
         
         vecFileNames         <- list.files(path = vecRawDir, 
                                            pattern = pattern,
                                            full.names = FALSE)
         if(length(vecFileNames) > 0) {
            vecFullFilePath     <- list.files(path = vecRawDir, 
                                              pattern = pattern, 
                                              full.names = TRUE)
            
            len <- sapply(vecFileNames, nchar)
            vecStrDatetime <- substr(vecFileNames, 1, len-5)
            
            vecInDatetime <- ymd_hms(vecStrDatetime)
            
            df <- rbind(df,
                        data_frame(forDate          = dfDownloadDirs[i,]$forDate,
                                    inDate           = date(vecInDatetime),
                                    inTime           = vecInDatetime,
                                    fullDownloadDir  = dfDownloadDirs[i,]$downloadDirFullPath,
                                    fullRawDirPath   = vecRawDir,
                                    fullFilePath     = vecFullFilePath,
                                    fileName         = vecFileNames)
                        )
         }
      }
   }
   
   if(spiegel.local.debug) {
      print("spiegel.local.headlineOverview.listHeadlineFiles Result:")
      print(head(df))
   }
   
   df
}


spiegel.local.articel.download <- function(URLs , downloadDir) {
   spiegel.local.download.updateStartDownloadInDate()
   spiegel.local.download.createACTDownloadDir()
   
   for (download in 1:nrow(URLs)) {
      filepath <- paste0(spiegel.local.download.getDownloadDirsFullPath(),
                         "/", 
                         spiegel.local.general.rawArticleDirName, 
                         "/", 
                         URLs[download, 1],
                         ".html")
      
      if (!file.exists(filepath)){
         download.file(URLs[download, 2], filepath, method="curl")
      } 
   }  
}


spiegel.local.headlineOverview.storeHeadlineData <- function(data, dir, overwrite = FALSE) {
   filepath <- paste0(dir, "/", spiegel.local.general.getProcessedHeadlineFilename())
   
   if (!file.exists(filepath) || overwrite == TRUE){
      write.csv2(data, file = filepath)
   } 
}



#spiegel.local.headlineOverview.downloadPages <- function() {
#   spiegel.local.headlineOverview.downloadPages
#}