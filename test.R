source("spiegel.online.R")
source("spiegel.local.R")


test <- function() {
   spiegel.local.HeadlineOverview.download(spiegel.online.archive.url.getURLs(), as_date("2017-06-20"), as_date("2017-06-29"))
}

test()