source("spiegel.dt.R")
source("spiegel.online.R")
source("spiegel.local.R")
source("spiegel.algo.R")

spiegel.debug <- FALSE

test.download <- function() {
   
   startTime <- now()
   spiegel.local.headlineOverview.downloadPages(addUpdates = TRUE)
   startTime <- now()
   print(startTime)
   lsFileStructur <- spiegel.dt.lsFileStructur()
   lsFileStructur <- spiegel.local.headlineOverview.listDownloadDirs(lsFileStructur)
   lsFileStructur <- spiegel.local.headlineOverview.listHeadlineFiles(lsFileStructur)
   lsFileStructur <- spiegel.local.getHeadlines(lsFileStructur, lessInfo=TRUE)
   print("URLs extracted")
   print(now())
   spiegel.local.articel.download(lsFileStructur)
   endtime <- now()
   print(endtime)
   diff1 <- endtime - startTime
   print(diff1)
   # spiegel.local.headlineOverview.download(as_date("2017-06-20"))
}

test.download2 <- function() {
   spiegel.local.headlineOverview.downloadPages(as_date("2017-06-20"), as_date("2017-06-30"))
}


test.download3 <- function() {
   startTime <- now()
   spiegel.local.headlineOverview.downloadPages(as_date("2016-07-01"), as_date("2017-06-30"))
   endtime <- now()
   diff <- endtime - startTime
   diff
}

test.extractHeadlineData <- function() {
   files <- spiegel.local.headlineOverview.listHeadlineFiles(spiegel.local.headlineOverview.listDownloadDirs())
   
   startTime <- now()
   data <- spiegel.extractDataFromHeadlineFiles(files, lessInfo=TRUE)
   endtime <- now()
   diff <- endtime - startTime
   diff
   
   View(data)
}


test.extractHeadlineDataCmp <- function() {
   files <- spiegel.local.headlineOverview.listHeadlineFiles(spiegel.local.headlineOverview.listDownloadDirs())
   
   startTime <- now()
   data <- spiegel.local.getHeadlines(files)
   endtime <- now()
   diff1 <- endtime - startTime
   print(diff1)
   
   
   startTime <- now()
   data <- spiegel.extractDataFromHeadlineFiles(files, lessInfo = TRUE)
   endtime <- now()
   diff2 <- endtime - startTime
   print(diff2)
   
   View(data)
}

spiegel.observe <- function() {
   spiegel.local.headlineOverview.downloadPages()
}

