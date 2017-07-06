require(dplyr)
require(lubridate)
require(data.table)

spiegel.dt.dtHeadline <- function (id                   = numeric(),
                                   idHeadline           = numeric(),
                                   idHeadlineFile       = numeric(),
                                   forDate              = as_date(character()),
                                   forTime              = as_datetime(character()),
                                   isBentoLink          = logical(),
                                   isSpiegelPlusLink    = logical(),
                                   intro                = character(), 
                                   title                = character(), 
                                   section              = character(), 
                                   link                 = character()) {
   dfHeadline <- data.table(  id                = id,
                              idHeadline        = idHeadline,
                              idHeadlineFile    = idHeadlineFile,
                              forDate           = forDate,
                              forTime           = forTime,
                              isBentoLink       = isBentoLink,
                              isSpiegelPlusLink = isSpiegelPlusLink,
                              intro             = intro, 
                              title             = title, 
                              section           = section, 
                              link              = link)
   dfHeadline
}


spiegel.dt.dtHeadlineFile <- function (  id                   = numeric(),
                                         idDlDir              = numeric(),
                                         forDate              = as_date(character()),
                                         inDate               = as_date(character()),
                                         inTime               = as_datetime(character()),
                                         dlDir                = character(),
                                         dlDir.fllPath        = character(),
                                         rawDir               = character(),
                                         rawDir.fllPath       = character(),
                                         file                 = character(),
                                         file.fllPath         = character()) {
   dtHeadlineFile <- data.table( id             = id,
                                 idDlDir        = idDlDir,
                                 forDate        = forDate,
                                 inDate         = inDate,
                                 inTime         = inTime,
                                 dlDir          = dlDir,
                                 dlDir.fllPath  = dlDir.fllPath,
                                 rawDir         = rawDir,
                                 rawDir.fllPath = rawDir.fllPath,
                                 file           = file,
                                 file.fllPath   = file.fllPath)
   
   dtHeadlineFile
}



spiegel.dt.dtDlDir <- function (id                   = numeric(),
                                forDate              = as_date(character()),
                                dlDir                  = character(),
                                dlDir.fllPath          = character()) {
   dtDlDir <- data.table(id             = id,
                         forDate       = forDate,
                         dlDir         = dlDir,
                         dlDir.fllPath = dlDir.fllPath)

   dtDlDir
}


spiegel.dt.lsFileStructur <- function (dtDlDir        = spiegel.dt.dtDlDir(),
                                       dtHeadlineFile = spiegel.dt.dtHeadlineFile(),
                                       dtHeadline     = spiegel.dt.dtHeadline()) {
   lsFileStructur <- list( dtDlDir        = dtDlDir,
                           dtHeadlineFile = dtHeadlineFile,
                           dtHeadline     = dtHeadline)
   
   lsFileStructur
}