library(rvest)
library(lubridate)
#library(XML)


# spiegel



### Test
### 
spiegel.local.HeadlineOverview.download(spiegel.online.archive.url.getURLs(), "20.06.2017", "29.06.2017")

## BLUB
   
   URLs <- spiegel.online.archive.url.getURLs()
   page <- read_html(URLs)
function()
page <- read_html(url)