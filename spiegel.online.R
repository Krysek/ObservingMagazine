require(rvest)
require(lubridate)

# spiegel.online
# Init global variables
spiegel.online.debug                         <- FALSE
spiegel.online.url                           <- "http://spiegel.de"
spiegel.online.headlines.url                 <- "http://spiegel.de/schlagzeilen"
spiegel.online.archive.url.prefix            <- c("http://www.spiegel.de/nachrichtenarchiv/artikel-")
spiegel.online.archive.url.dates.startDate   <- as_date(today())
spiegel.online.archive.url.dates.endDate     <- as_date(today())
spiegel.online.archive.url.suffix            <- c(".html")
spiegel.online.timezone                      <- "CET"


# spiegel.online.url.archive
spiegel.online.getURL <- function() {
   spiegel.online.url
}

spiegel.online.setURL <- function(url) {
   spiegel.online.url <- url
}

# spiegel.online.archive.url.setPeriod <- function (startDate, endDate) {
#    spiegel.online.archive.url.setStartDate(as_date(startDate))
#    spiegel.online.archive.url.setEndDate(as_date(endDate))
# }

#spiegel.online.archive.url.getDatesOfPeriod <- function() {
#   spiegel.online.archive.url.dates.startDate + days(0:(spiegel.online.archive.url.dates.endDate - spiegel.online.archive.url.dates.startDate))
#}

spiegel.online.archive.url.setStartDate <- function (date) {
   spiegel.online.archive.url.dates.startDate <<- as_date(date)
}

spiegel.online.archive.url.getStartDate <- function () {
   spiegel.online.archive.url.dates.startDate
}

spiegel.online.archive.url.setEndDate <- function (date) {
   spiegel.online.archive.url.dates.endDate <<- as_date(date)
}

spiegel.online.archive.url.getEndDate <- function () {
   spiegel.online.archive.url.dates.endDate
}

spiegel.online.archive.url.getURL <- function (vecForDates) {
   vecForDates <- format(vecForDates, format="%d.%m.%Y")
   
   if(length(vecForDates) > 0) {
      urls <- paste0(spiegel.online.archive.url.prefix,
                     vecForDates,
                     ".html")
   } else {
      urls <- NULL
   }
   print(urls)
   urls
}

# #spiegel.online.archive.url.getURLs <- function (dates = spiegel.local.general.getObservedDates()) {
# #   rows <- nrow(dates)
#    
# #   df <- data_frame()
#    
#    if(rows > 0) {
#       for(i in 1:rows) {
#          df <- rbind(df, spiegel.online.archive.url.getURL(dates[i,]))
#       }
#    }
#    
#    df
# }