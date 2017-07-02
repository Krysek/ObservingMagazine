source("spiegel.online.R")
source("spiegel.local.R")
source("spiegel.algo.R")

spiegel.debug <- FALSE

test.download <- function() {
   spiegel.local.headlineOverview.downloadPages(as_date("2017-06-20"), as_date("2017-06-29"))
   # spiegel.local.headlineOverview.download(as_date("2017-06-20"))
}

test.download2 <- function() {
   spiegel.local.headlineOverview.download(spiegel.online.archive.url.getURLs(), as_date("2017-06-20"), as_date("2017-06-30"))
}


test.download3 <- function() {
   startTime <- now()
   spiegel.local.headlineOverview.download(spiegel.online.archive.url.getURLs(), as_date("2016-07-01"), as_date("2017-06-30"))
   endtime <- now()
   diff <- endtime - startTime
   diff
}

test.openHeadlineFiles <- function() {
   spiegel.local.headlineOverview.listHeadlineFiles(spiegel.local.headlineOverview.listDownloadDirs())
}

test.openHeadlineFiles <- function() {
   spiegel.local.headlineOverview.listHeadlineFiles(spiegel.local.headlineOverview.listDownloadDirs())
}

test.extractHeadlineData <- function() {
   files <- spiegel.local.headlineOverview.listHeadlineFiles(spiegel.local.headlineOverview.listDownloadDirs())
   
   data <- spiegel.extractDataFromHeadlineFiles(files)
   #data <- spiegel.extractDataFromHeadlineFile(files[127,]) #127
   
   View(data)
}

#startTime <- now()
#test <- test.extractHeadlineData()
#head(test)
#endtime <- now()
#diff <- endtime - startTime