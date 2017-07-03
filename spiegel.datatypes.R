#spiegel.datatypes


#root
   #headlineFile
      #headline


spiegel.dt.listHeadlines <- list(headline = spiegel.datatypes.headline)

spiegel.dt.dfHeadlineFile <- list(id = numeric(), title = character(), url = character())


spiegel.dt.dfHeadlineFile <- function (  forDate              = as_date(character()),
                                         inDate               = as_date(character()),
                                         inTime               = as_datetime(character()),
                                         dlFullPath           = character(),
                                         rawDirFullPath       = character(),
                                         rawFileName          = character(),
                                         headlineFileFullPath = character(),
                                         headline             = spiegel.datatypes.dfHeadline()) {
   dfHeadlineFile <- list(forDate,
                          inDate,
                          inTime,
                          dlFullPath,
                          rawDirFullPath,
                          rawFileName,
                          headlineFileFullPath,
                          headline)
}

spiegel.dt.dfHeadline <- function (id                   = numeric(),
                                   forDate              = as_date(character()),
                                   forTime              = as_datetime(character()),
                                   isBentoLink          = logical(),
                                   isSpiegelPlusLink    = logical(),
                                   intro                = character(), 
                                   title                = character(), 
                                   section              = character(), 
                                   link                 = character()) {
   dfHeadline <- data_frame(  id,
                              forDate,
                              forTime,
                              isBentoLink,
                              isSpiegelPlusLink,
                              intro, 
                              title, 
                              section, 
                              link)
   dfHeadline
}
   
  