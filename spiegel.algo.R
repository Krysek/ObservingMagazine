require(XML)
require(gdata)

spiegel.online.debug <- FALSE

spiegel.get<- function (url) {
   page <- read_html(url)
   
}

spiegel.extractDataFromHeadlineFiles <- function (headlineFiles) {
   df <- data.frame()
   
   rows <- nrow(headlineFiles)
   
   if (rows > 0) {
      for(i in 1:nrow(headlineFiles)) {
         df <- rbind(df, spiegel.extractDataFromHeadlineFile(headlineFiles[i,]))
      }
   }
   
   if(spiegel.online.debug) {
      print("spiegel.extractDataFromHeadlineFiles Parameter headlineFile:")
      print(df)
   }
   
   df
}

spiegel.extractDataFromHeadlineFile <- function (headlineFile, startID = 1) {
   if(spiegel.online.debug) {
      print("spiegel.extractDataFromHeadlineFile Parameter headlineFile:")
      print(headlineFile)
   }
   
   htmlCode <- read_html(as.character(headlineFile$fullFilePath))
   
   content        <- html_nodes(htmlCode, css = ".column-wide")
   headlines      <- html_nodes(content, "li")
   
   df <- data.frame()
   id <- startID
   
   rows <- length(headlines)
   
   if (rows > 0) {
      for(i in 1:rows) {
      headline <- headlines[i]
      
      #Extract Links
      link             <- html_nodes(headline, "a") %>% 
                                       html_attr("href") %>% 
                                       ifelse(identical(., character(0)), NA, .)
      class             <- html_nodes(headline, "a") %>%
                                       html_attr("class")  %>% 
                                       ifelse(identical(., character(0)), NA, .)
      intro             <- html_nodes(headline, css = ".news-archive-headline-intro") %>% 
                                       html_text(trim = TRUE) %>% 
                                       ifelse(identical(., character(0)), NA, .)
      title             <- html_nodes(headline, css = ".news-archive-headline") %>% 
                                       html_text(trim = TRUE) %>% 
                                       ifelse(identical(., character(0)), NA, .)
      sectionAndTime    <- html_nodes(headline, css = ".headline-date") %>% 
                                       html_text(trim = TRUE) %>% 
                                       ifelse(identical(., character(0)), NA, .)
      
      isBentoLink       <- regexpr("bento", class) > 0
      isBentoLink[is.na(isBentoLink)]             <- FALSE
      isSpiegelPlusLink <- regexpr("spiegelplus", class) > 0
      isSpiegelPlusLink[is.na(isSpiegelPlusLink)] <- FALSE
      
      if (isBentoLink == FALSE) {
         pattern <- "^http"
         if (regexpr(pattern, sectionAndTime) < 0){
            link <- paste0(spiegel.online.getURL() , link)
         }
         
      }
      
      pattern <- "[0-9]{1,2}:[0-9]{1,2})$"
      reg.out <- regexpr(pattern, sectionAndTime)
      forTime <- paste(substr(sectionAndTime, reg.out, reg.out + attr(reg.out,"match.length")-2), 
                       spiegel.online.timezone)
      forDatetime <- ymd_hm(paste0(headlineFile$forDate, " ", forTime))
      
      pattern <- ", [[0-9]{1,2}:[0-9]{1,2})$"
      reg.out <- regexpr(pattern, sectionAndTime)
      section <- substr(sectionAndTime, 2, reg.out-1)
      
      df <- rbind(df,
                  data.frame( id                   = id,
                              forDate              = headlineFile$forDate,
                              forTime              = forDatetime,
                              inDate               = headlineFile$inDate,
                              inTime               = headlineFile$inTime,
                              dlFullPath           = headlineFile$fullDownloadDir,
                              rawDirFullPath       = headlineFile$fullRawDirPath,
                              rawFileName          = headlineFile$fileName,
                              headlineFileFullPath = headlineFile$fullFilePath,
                              isBentoLink          = isBentoLink,
                              isSpiegelPlusLink    = isSpiegelPlusLink,
                              intro                = intro, 
                              title                = title, 
                              section              = section, 
                              link                 = link, 
                              stringsAsFactors  = FALSE))
      
      id = id + 1
      }
   }
   
   if(spiegel.online.debug) {
      print("spiegel.extractDataFromHeadlineFile Parameter headlineFile:")
      print(head(df))
   }
   
   df
}