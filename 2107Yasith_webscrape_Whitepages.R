############
#Engagement Name: Visy (Supplier Validation Testing)
#Purpose: This code will scrape the number of search results for each vendor from whitepages.com
#Author: Yasith Kariyawasam
#Contact details: Yasith.Kariyawasam@au.ey.com
#Version Number: 1.4
#Notes: Need to install Google Chrome and also download this 
#Google Chrome add-on https://sites.google.com/a/chromium.org/chromedriver/downloads/ and save to your working directory
##############

### Pre-Work

library(RSelenium)
library(rvest)
library(tidyverse)
library(dplyr)
library(formattable)


#import list/file of vendor names
# vendorlist <- read.csv("#filename", sep = ",", quote = "\"", header = F)
vendorlist <- read.csv("VendorNamesToScrape_v2_27-04-2018.csv", sep = "\t", quote = "", header = T, stringsAsFactors = F)
vendorlist$VendorName <- as.character(vendorlist$VendorName)
vendorlist$VendorNumber <- as.character(vendorlist$VendorNumber)

listOfSleepTimes <- seq(from = 10, to = 15, by = 0.02)
listOfBrowsers <- c("firefox", "chrome", "internet explorer")

#Specify file output path to save error log
errorlogpath <-  "C:\\Users\\kariyya\\Documents\\Data Scrapping\\error_log.csv"
#specify file output path to save scraped results until error hits
result2date <-  "C:\\Users\\kariyya\\Documents\\Data Scrapping\\results2date.csv"
#samplesize to scape 
batch <- 2
#############################################################################

scrape_wp <- function(name){
  randomBrowser <- sample(listOfBrowsers, size = 1, replace = TRUE)
  randomSleepTime <- sample(listOfSleepTimes, size = 1, replace = TRUE)
  typeof(as.character(randomSleepTime))

  #print the current search time & and Sys.sleep()
  print(paste0("finding the url for ", name, 
             " with a sleep time of ", randomSleepTime, 
             " using ", randomBrowser,
             " Current time is: ", Sys.time()
           ))
  
  Sys.sleep(randomSleepTime)
  
  #create URL by combining URLhead + vendor_name,
  url <- paste0("https://www.whitepages.com.au/business/results?name=", url_encode(name), 
         "&location=Nationally&expand=true")
  
  driver<- rsDriver(browser=c(randomBrowser))

  remDr <- driver[["client"]]
  remDr$navigate(url)
  t <- remDr$findElements(using = 'css',".searchResultsContainer_originalTotalResults-expanded")    
  tweets=list()
  tweets=sapply(t, function(x){x$getElementText()})
  remDr$close()
  remDr$errorDetails()
  as.character(tweets)
}

for (i in 1:batch){

  Vendor_Number <- vendorlist$VendorNumber[i]
  Vendor_Name <- vendorlist$VendorNameToSearch[i]
  
  df <- data.frame(Vendor_Number = Vendor_Number, Vendor_Name = Vendor_Name, stringsAsFactors = F)
  
  result <- mutate(df, result = tryCatch({map(Vendor_Name, scrape_wp)
  }, error = function(err){
    errorSleepTime <- sample(seq(from = 900, to = 1200 , by = 60), size = 1, replace = TRUE)
    
    print(paste("The error message:  ", err))
    print(paste("Going to Sleep for: ", errorSleepTime, " seconds. Current time:", Sys.time()))
    error_message <- paste("The error message:", err, "- Error time: ", Sys.time())
    # write.table(error_message, file = errorlogpath)
    # Sys.sleep(errorSleepTime)
    # print(paste("Waking up. Current time: ", Sys.time()))
    return('error')
  })
  )
  
  NumberOfWhitePagesHits <- as.numeric(gsub(" results found nationally| result found nationally", "", result$result))
  printableResult <- data.frame(result$Vendor_Number, result$Vendor_Name, NumberOfWhitePagesHits)
  errorvendor <- filter(result, result == 'error' )
  filename = paste(result2date, 'result_part_append_', format(Sys.time(), "%Y-%m-%d"),'.csv')
  write.table(printableResult, file = filename, row.names = FALSE, col.names = FALSE, append = TRUE, sep = "," )
  Sys.sleep(sample(seq(from = 3, to = 4, by = .01),1))
  
}
