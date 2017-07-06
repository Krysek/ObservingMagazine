source("spiegel.dt.R")
require(XML)
require(gdata)

spiegel.online.debug <- FALSE

spiegel.get<- function (url) {
   page <- read_html(url)
   
}

# spiegel.extractDataFromHeadlineFiles <- function (headlineFiles) {
#    df <- data_frame()
#    
#    rows <- nrow(headlineFiles)
#    
#    if (rows > 0) {
#       for(i in 1:nrow(headlineFiles)) {
#          df <- rbind(df, spiegel.extractDataFromHeadlineFile(headlineFiles[i,]))
#       }
#    }
#    
#    if(spiegel.online.debug) {
#       print("spiegel.extractDataFromHeadlineFiles Parameter headlineFile:")
#       print(df)
#    }
#    
#    df
# }


spiegel.algo.parseHeadlineFile <- function (headlineFile, startID = 1, lessInfo = FALSE) {
   if(spiegel.online.debug) {
      print("spiegel.extractDataFromHeadlineFile Parameter headlineFile:")
      print(headlineFile)
   }
   
   htmlCode <- read_html(as.character(headlineFile[, file.fllPath]))
   
   content        <- html_nodes(htmlCode, css = ".column-wide")
   headlines      <- html_nodes(content, "li")
   headlines.rows <- length(headlines)
   
   dtHeadline <- spiegel.dt.dtHeadline()
   id <- startID
   
   
   if (headlines.rows > 0) {
      for(i in 1:headlines.rows) {
         headline <- headlines[i]
         
         #Extract Links
         link             <- html_nodes(headline, "a") %>% 
                                          html_attr("href") %>% 
                                          ifelse(identical(., character(0)), NA, .)
         class             <- html_nodes(headline, "a") %>%
                                          html_attr("class")  %>% 
                                          ifelse(identical(., character(0)), NA, .)
         sectionAndTime    <- html_nodes(headline, css = ".headline-date") %>% 
                                          html_text(trim = TRUE) %>% 
                                          ifelse(identical(., character(0)), NA, .)
         
         isBentoLink                                 <- regexpr("bento", class) > 0
         isBentoLink[is.na(isBentoLink)]             <- FALSE
         isSpiegelPlusLink                           <- regexpr("spiegelplus", class) > 0
         isSpiegelPlusLink[is.na(isSpiegelPlusLink)] <- FALSE
         
         if (isBentoLink == FALSE) {
            pattern <- "^http"
            if (regexpr(pattern, link) < 0){
               link <- paste0(spiegel.online.getURL() , link)
            }
         }
         
         pattern <- "[0-9]{1,2}:[0-9]{1,2})$"
         reg.out <- regexpr(pattern, sectionAndTime)
         forTime <- paste(substr(sectionAndTime, reg.out, reg.out + attr(reg.out,"match.length")-2), 
                          spiegel.online.timezone)
         forDatetime <- ymd_hm(paste0(headlineFile[, forDate], " ", forTime))
         
         if(lessInfo == FALSE) {
            intro             <- html_nodes(headline, css = ".news-archive-headline-intro") %>% 
                                             html_text(trim = TRUE) %>% 
                                             ifelse(identical(., character(0)), NA, .)
            title             <- html_nodes(headline, css = ".news-archive-headline") %>% 
                                             html_text(trim = TRUE) %>% 
                                             ifelse(identical(., character(0)), NA, .)
            
            
            pattern <- ", [[0-9]{1,2}:[0-9]{1,2})$"
            reg.out <- regexpr(pattern, sectionAndTime)
            section <- substr(sectionAndTime, 2, reg.out-1)
            
            # dtHeadline <- rbind(dtHeadline,
            #                     spiegel.dt.dtHeadline(id                   = id,
            #                                           forDate              = headlineFile[, forDate],
            #                                           forTime              = forDatetime,
            #                                           isBentoLink          = isBentoLink,
            #                                           isSpiegelPlusLink    = isSpiegelPlusLink,
            #                                           intro                = intro, 
            #                                           title                = title, 
            #                                           section              = section, 
            #                                           link                 = link))
            dtHeadline <- rbindlist(list(dtHeadline,
                                spiegel.dt.dtHeadline(id                   = id,
                                                      idHeadline           = i,
                                                      idHeadlineFile       = NA,
                                                      forDate              = headlineFile[, forDate],
                                                      forTime              = forDatetime,
                                                      isBentoLink          = isBentoLink,
                                                      isSpiegelPlusLink    = isSpiegelPlusLink,
                                                      intro                = intro, 
                                                      title                = title, 
                                                      section              = section, 
                                                      link                 = link)))
         } else {   ###  short Headfile edit
            dtHeadline <- rbindlist(list(dtHeadline,
                                spiegel.dt.dtHeadline(id                   = id,
                                                      idHeadline           = i,
                                                      idHeadlineFile       = NA,
                                                      forDate              = headlineFile[, forDate],
                                                      forTime              = forDatetime,
                                                      isBentoLink          = isBentoLink,
                                                      isSpiegelPlusLink    = isSpiegelPlusLink,
                                                      intro                = "", 
                                                      title                = "", 
                                                      section              = "", 
                                                      link                 = link)))
            # dtHeadline <- rbind(dtHeadline,
            #                     spiegel.dt.dtHeadline(id                   = id,
            #                                           forDate              = headlineFile[, forDate],
            #                                           forTime              = forDatetime,
            #                                           isBentoLink          = isBentoLink,
            #                                           isSpiegelPlusLink    = isSpiegelPlusLink,
            #                                           intro                = "", 
            #                                           title                = "", 
            #                                           section              = "", 
            #                                           link                 = link))
         }
         id = id + 1
      }
   }
   
   if(spiegel.online.debug) {
      print("spiegel.extractDataFromHeadlineFile Parameter headlineFile:")
      print(head(df))
   }
   
   dtHeadline
}