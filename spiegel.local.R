require(rvest)
require(lubridate)

# Spiegel.local
# Init global variables
spiegel.local.debug                             <- FALSE
spiegel.local.general.rootDirName               <- "data"
spiegel.local.general.rawHeadlineDirName        <- "rawHeadlineFiles"
spiegel.local.general.rawArticleDirName         <- "rawArticleFiles"
spiegel.local.general.articleFileName           <- "article"
spiegel.local.general.prefixForDownloadDir      <- "forDate"
spiegel.local.general.processedHeadlineFilename <- "headlineData.csv"
spiegel.local.download.downloadInDate           <- today()
spiegel.local.general.TimeFormatForFileName     <- "%Y-%m-%d %H-%M-%S %Z"
spiegel.local.general.TimeFormatForImport       <- "%Y-%m-%d %H:%M:%S %Z"

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

#spiegel.local.download.updateACTDownloadStartDate
spiegel.local.download.updateDownloadInDate <- function() {
   spiegel.local.download.downloadInDate <- today()
}


spiegel.local.download.getACTDownloadStartDate <- function() {
   spiegel.local.download.downloadInDate
}

#spiegel.local.download.getACTDownloadStartDatePath
spiegel.local.download.getDownloadDirFullPath <- function(date) {
   paste0(spiegel.local.general.rootDirName, 
          "/", 
          spiegel.local.general.getPrefixForDownloadDir(), 
          "-", 
          date)
}

# spiegel.local.download.createDownloadDir
spiegel.local.download.createDownloadStructure <- function(forDate) {
   spiegel.local.createRootDir()
   
   downloadDirFullPath <- spiegel.local.download.getDownloadDirFullPath(forDate)
   
   if (!file.exists(downloadDirFullPath)) {
      dir.create(downloadDirFullPath)
   }
   
   rawHeadlineDirFullPath <- paste0(downloadDirFullPath,
                                    "/", 
                                    spiegel.local.general.getRawHeadlineDirName())
   
   if (file.exists(rawHeadlineDirFullPath) == FALSE) {
      dir.create(rawHeadlineDirFullPath)
   }
   
   rawArticleDirFullPath <- paste0(downloadDirFullPath,
                                    "/", 
                                    spiegel.local.general.getRawHeadlineDirName())
   
   if (file.exists(rawArticleDirFullPath) == FALSE) {
      dir.create(rawArticleDirFullPath)
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

spiegel.local.headlineOverview.downloadPage <- function(forDate) {
   
   spiegel.local.download.createDownloadStructure(forDate)
   
   URL <- spiegel.online.archive.url.getURL(forDate)
   
   len <- length(URL)
   
   if (len > 0) {
      filepath <- paste0(spiegel.local.download.getDownloadDirFullPath(forDate),
                         "/", 
                         spiegel.local.general.rawHeadlineDirName, 
                         "/", 
                         format(as_datetime(now()), format = spiegel.local.general.TimeFormatForFileName),
                         ".html")
      
      if (!file.exists(filepath)){
         download.file(URL, filepath, method="curl")
      } 
   }
} 

#spiegel.local.headlineOverview.download
spiegel.local.headlineOverview.downloadPages <- function(startDate = spiegel.online.archive.url.getStartDate(), endDate = spiegel.online.archive.url.getEndDate()) {
   spiegel.online.archive.url.setPeriod(as_date(startDate), as_date(endDate))
   
   spiegel.local.download.updateDownloadInDate()
   
   forDates <- spiegel.online.archive.url.getDatesOfPeriod()
   
   len <- length(forDates)
   
   if (len > 0) {
      for (i in 1:len) {
         spiegel.local.headlineOverview.downloadPage(forDates[i])
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
   
   result <- data.frame(downloadDirFullPath = dirs[grep(pattern, dirs)],
                        stringsAsFactors = FALSE)
   
   pattern <- paste0("[1-9]{1}[0-9]{3}-[0-9]{1,2}-[0-9]{1,2}")
   reg.out <- regexpr(pattern, result$downloadDirFullPath)
   
   forDate <- data.frame(forDate = as_date(substr(result$downloadDirFullPath, reg.out, reg.out + attr(reg.out,"match.length")-1)),
                        stringsAsFactors = FALSE)
   result <- cbind(forDate, result)
   
   
   if(spiegel.local.debug) {
      print("spiegel.local.headlineOverview.listDownloadDirs Result:")
      print(result)
   }
   
   result
}

spiegel.local.headlineOverview.haveDownloadDirsRawDir <- function(downloadDirs) {
   rawDirPath <- paste0(downloadDirs, "/", spiegel.local.general.getRawHeadlineDirName())
   
   result <- file.exists(rawDirPath) == TRUE
   
   if(spiegel.local.debug) {
      print("spiegel.local.headlineOverview.haveDownloadDirsRawDir Result:")
      print(head(result))
   }
   
   result
}


spiegel.local.headlineOverview.listHeadlineFiles <- function(downloadDirs = spiegel.local.headlineOverview.listDownloadDirs()) {
   if(spiegel.local.debug) {
      print("spiegel.local.headlineOverview.listHeadlineFiles Parameter downloadDirs: ")
      print(downloadDirs)
   }
   
   # take only dirs with raw dir over
   # TODO: spiegel.local.headlineOverview.haveDownloadDirsRawDir
   downloadDirs <- downloadDirs[spiegel.local.headlineOverview.haveDownloadDirsRawDir(downloadDirs$downloadDirFullPath), ] 
   
   df <- data.frame()
   #downloadDirFullPath
   rows <- nrow(downloadDirs)
   if (rows > 0) {
      for(i in 1:rows){
         #all path to raw dirs
         rawDir <- paste0(downloadDirs$downloadDirFullPath[i], "/", spiegel.local.general.getRawHeadlineDirName())
         
         if(spiegel.local.debug) {
            print("raw dirs: ")
            print(head(rawDir))
         }
         
         #forDate <- as_date(substring(list.files(path = rawDir, 
         #                                      full.names = FALSE, 
         #                                      recursive = FALSE), 
         #                           1, 10))
         
         if(spiegel.local.debug) {
            print("for dates: ")
            print(forDate)
         }
         
         fileName         = list.files(path = rawDir, 
                                       full.names = FALSE)
         
         pattern <- "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}"
         reg.out <- regexpr(pattern, fileName)
         strDatetime <- substr(fileName, reg.out, reg.out + attr(reg.out,"match.length")-1)
         
         pattern <- " [A-Z]{1,4}.html"
         reg.out <- regexpr(pattern, fileName)
         strTimezone <- substr(fileName, reg.out+1, reg.out + attr(reg.out,"match.length")-6)
         
         #Timezone sind doppelt
         inDatetime <- as_datetime(ymd_hms(strDatetime, tz = strTimezone[1]))
         
         df <- rbind(df, 
                     data.frame(forDate          = downloadDirs$forDate[i],
                                inDate           = date(inDatetime), 
                                inTime           = inDatetime,
                                fullDownloadDir  = downloadDirs$downloadDirFullPath[i],
                                fullRawDirPath   = rawDir,
                                fullFilePath     = list.files(path = rawDir, 
                                                               full.names = TRUE),
                                fileName         = fileName,
                                stringsAsFactors = FALSE)
                     )
      }
   }

   if(spiegel.local.debug) {
      print("spiegel.local.headlineOverview.listHeadlineFiles Result:")
      print(head(df))
   }
   
   df
}


spiegel.local.articel.download <- function(URLs , downloadDir) {
   spiegel.local.download.updateDownloadInDate()
   spiegel.local.download.createACTDownloadDir()
   
   for (download in 1:nrow(URLs)) {
      filepath <- paste0(spiegel.local.download.getDownloadDirFullPath(),
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