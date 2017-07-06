source("spiegel.dt.R")
require(rvest)
require(lubridate)
require(dplyr)

# Spiegel.local
# Init global variables
spiegel.local.debug                             <- FALSE
spiegel.local.general.rootDirName               <- "../ObservingMagazine-Data"
spiegel.local.general.rawHeadlineDirName        <- "rawHF"
spiegel.local.general.rawArticleDirName         <- "rawAF"
spiegel.local.general.articleFileName           <- "article"
spiegel.local.general.prefixForDownloadDir      <- "forDate"
spiegel.local.general.processedHeadlineFilename <- "headlineData.csv"
spiegel.local.general.timezone                  <- "CET"
spiegel.local.general.observedStartDate         <- as_date("2016-07-01")
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

spiegel.local.general.getRawArticleDirName <- function() {
   spiegel.local.general.rawArticleDirName
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
      vecRawArticleDirsFullPath <- paste0(vecRawHeadlineDirsFullPath,
                                       "/", 
                                       spiegel.local.general.getRawArticleDirName())
      
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

spiegel.local.headlineOverview.downloadPages <- function(startDate = spiegel.local.general.getObservedStartDate(), 
                                                         endDate = spiegel.local.general.getObservedEndDate(), 
                                                         addUpdates = FALSE) {
   if(is.null(endDate) || endDate >= today()) {
      endDate <- today() - days(1)
   }
   
   spiegel.local.download.updateStartDownloadInDate()
   
   vecForDates <- spiegel.local.general.getObservedDates(startDate, endDate)
   
   if (addUpdates == FALSE) {
      spiegel.dt.lsFileStructur <- spiegel.local.headlineOverview.listDownloadDirs()
      dtDlDir <- spiegel.dt.lsFileStructur$dtDlDir
      
      if(dtDlDir[, (length(id))] > 0){
         dfList <- spiegel.local.headlineOverview.listHeadlineFiles(dtDlDir)
         if(dfList[, length(id)] > 0) {
            vecAvailableForDates <- dfList[, unique(forDate)]
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

spiegel.local.headlineOverview.listDownloadDirs <- function(lsFileStructur = spiegel.dt.lsFileStructur()) {
   lsFileStructur = spiegel.dt.lsFileStructur()
   
   pattern <- paste0("^",
                 spiegel.local.general.getPrefixForDownloadDir(),
                 "-[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$")
   
   vecDlDir.fllPath <- dir(path = spiegel.local.general.getRootFullPath(), 
                        pattern = pattern,
                        full.names = TRUE,
                        recursive = FALSE)
   vecDlDir         <- dir(path = spiegel.local.general.getRootFullPath(), 
                        pattern = pattern,
                        full.names = FALSE,
                        recursive = FALSE)

   
   pattern <- paste0("[1-9]{1}[0-9]{3}-[0-9]{1,2}-[0-9]{1,2}")
   reg.out <- regexpr(pattern, vecDlDir)
   vecForDate <- as_date(substr(vecDlDir, reg.out, reg.out + attr(reg.out,"match.length")-1))
   
   dtDlDir    <- spiegel.dt.dtDlDir()
   vecId      <- 1:length(vecDlDir)
   
   if(length(vecDlDir) > 0) {
      dtDlDir <- rbindlist(list(dtDlDir,
                                spiegel.dt.dtDlDir(id            = vecId,
                                                   forDate       = vecForDate,
                                                   dlDir         = vecDlDir, 
                                                   dlDir.fllPath = vecDlDir.fllPath)))
      
      
      if(spiegel.local.debug) {
         print("spiegel.local.headlineOverview.listDownloadDirs Result:")
         print(lsDlDir)
      }
   }
   lsFileStructur$dtDlDir <- dtDlDir
   lsFileStructur$dtHeadlineFile <- spiegel.dt.dtHeadlineFile()
   lsFileStructur$dtHeadline <- spiegel.dt.dtHeadline()
   
   lsFileStructur
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

#### hier
spiegel.local.headlineOverview.listHeadlineFiles <- function(lsFileStructur = spiegel.local.headlineOverview.listDownloadDirs()
                                                             ) {
   if(spiegel.local.debug) {
      print("spiegel.local.headlineOverview.listHeadlineFiles Parameter dfDownloadDirs: ")
      print(lsFileStructur)
   }
   
   dtDlDir <- lsFileStructur$dtDlDir
   
   # take only dirs with raw dir over
   dtDlDir <- dtDlDir[spiegel.local.headlineOverview.haveDownloadDirsRawDir(dtDlDir[, dlDir.fllPath]), ]
   
   # number of row for dtDlDir
   dtDlDir.len <- dtDlDir[, length(id)]
   # init empty dtHeadlineFile
   dtHeadlineFile  <- spiegel.dt.dtHeadlineFile()
   # init startID
   startId <- 1
   
   # Loop for each dtDlDir item / downloadDir
   if(dtDlDir.len > 0) {
      for(i in 1:dtDlDir.len){
         vecRawDir <- paste0(dtDlDir[i, dlDir.fllPath], "/", spiegel.local.general.getRawHeadlineDirName())
         
         pattern <- "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2} [A-Z]{1,4}.html"
         
         vecFileNames         <- list.files(path = vecRawDir, 
                                            pattern = pattern,
                                            full.names = FALSE)
         if(length(vecFileNames) > 0) {
            vecId <- startId:(length(vecFileNames)+startId-1)
            vecFullFilePath     <- list.files(path = vecRawDir, 
                                              pattern = pattern, 
                                              full.names = TRUE)
            
            vecFileNames.len <- sapply(vecFileNames, nchar)
            vecStrDatetime <- substr(vecFileNames, 1, vecFileNames.len-5)
            
            vecInDatetime <- ymd_hms(vecStrDatetime)
            
            
            dtHeadlineFile <- rbindlist(list(dtHeadlineFile,
                                    spiegel.dt.dtHeadlineFile(id               = vecId,
                                                              idDlDir          = dtDlDir[i, id],
                                                              forDate          = dtDlDir[i, forDate],
                                                              inDate           = as_date(vecInDatetime),
                                                              inTime           = vecInDatetime,
                                                              dlDir            = dtDlDir[i, dlDir],
                                                              dlDir.fllPath    = dtDlDir[i, dlDir.fllPath],
                                                              rawDir           = spiegel.local.general.getRawHeadlineDirName(),
                                                              rawDir.fllPath   = vecRawDir,
                                                              file.fllPath     = vecFullFilePath,
                                                              file             = vecFileNames))
            )
            # dtHeadlineFile <- rbind(dtHeadlineFile,
            #                         spiegel.dt.dtHeadlineFile(id               = vecId,
            #                                                   idDlDir          = dtDlDir[i, id],
            #                                                   forDate          = dtDlDir[i, forDate],
            #                                                   inDate           = as_date(vecInDatetime),
            #                                                   inTime           = vecInDatetime,
            #                                                   dlDir            = dtDlDir[i, dlDir],
            #                                                   dlDir.fllPath    = dtDlDir[i, dlDir.fllPath],
            #                                                   rawDir           = spiegel.local.general.getRawHeadlineDirName(),
            #                                                   rawDir.fllPath   = vecRawDir,
            #                                                   file.fllPath     = vecFullFilePath,
            #                                                   file             = vecFileNames)
            #             )
         }
         startId <- dtHeadlineFile[, length(id)] + 1
      }
   }
   
   if(spiegel.local.debug) {
      print("spiegel.local.headlineOverview.listHeadlineFiles Result:")
      print(head(df))
   }
   
   lsFileStructur$dtHeadlineFile <- dtHeadlineFile
   
   lsFileStructur
}

spiegel.local.articel.download <- function(lsFileStructur = spiegel.local.getHeadlines()) {
   dtHeadlineFile <- lsFileStructur$dtHeadlineFile
   dtHeadline     <- lsFileStructur$dtHeadline
   
   spiegel.local.download.createDownloadStructure(dtHeadline[, unique(forDate)])
   
   dtHeadline.len <- dtHeadline[, length(id)]
   
   if(dtHeadline.len > 0) {
      for (i in 1:dtHeadline.len) {
         headline <- dtHeadline[i, ]
         headlineFile <- dtHeadlineFile[id == headline[, idHeadlineFile], ]
         
         
         filepath <- paste0(headlineFile[ , rawDir.fllPath],
                            "/", 
                            spiegel.local.general.getRawArticleDirName(), 
                            "/", 
                            substr(headlineFile[, file] ,1 , nchar(headlineFile[, file])-5),
                            " -",
                            headline[, idHeadline],
                            "- ",
                            format(as_datetime(now()), 
                                   format = spiegel.local.general.TimeFormatForFileName, 
                                   tz=spiegel.local.general.timezone),
                            ".html")
         
         if (!file.exists(filepath)){
            download.file(headline[, link], filepath, method="curl")
         } 
      }
   }
}


spiegel.local.headlineOverview.storeHeadlineData <- function(data, dir, overwrite = FALSE) {
   filepath <- paste0(dir, "/", spiegel.local.general.getProcessedHeadlineFilename())
   
   if (!file.exists(filepath) || overwrite == TRUE){
      write.csv2(data, file = filepath)
   } 
}


spiegel.local.getHeadlines <- function (lsFileStructur = spiegel.local.headlineOverview.listHeadlineFiles(), 
                                        lessInfo = FALSE) {
   dtHeadlineFiles <- lsFileStructur$dtHeadlineFile
   
   # number of row for dtHeadlineFiles
   dtHeadlineFiles.len <- dtHeadlineFiles[, length(id)]
   # init empty dtHeadline
   dtHeadline  <- spiegel.dt.dtHeadline()
   # init startID
   startId <- 1
   
   if (dtHeadlineFiles.len > 0) {
      for(i in 1:dtHeadlineFiles.len) {
         dtHeadline <- rbindlist(list(dtHeadline, 
                             spiegel.algo.parseHeadlineFile(dtHeadlineFiles[i,], 
                                                            startID = dtHeadline[, length(id)]+1, 
                                                            lessInfo=lessInfo)[, idHeadlineFile := i]))
         
         # dtHeadline <- rbind(dtHeadline, 
         #                    spiegel.algo.parseHeadlineFile(dtHeadlineFiles[i,], lessInfo=lessInfo))
      }
   }
   
   if(spiegel.online.debug) {
      print("spiegel.extractDataFromHeadlineFiles Parameter headlineFile:")
      print(dtHeadline)
   }
   
   lsFileStructur$dtHeadline <- dtHeadline
   lsFileStructur
}