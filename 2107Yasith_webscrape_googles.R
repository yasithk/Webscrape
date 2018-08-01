library(dplyr)
library(tidyverse)
library(rvest)
library(urltools)

#import list/file of vendornames 
vendorlist <- read.csv("VendorNamesToScrape_v2_27-04-2018.csv", sep = "\t", quote = "", header = T, stringsAsFactors = F)
vendorlist$VendorName <- as.character(vendorlist$VendorNameToSearch)
vendorlist$VendorNumber <- as.character(vendorlist$VendorNumber)
# vendorlist <- read.csv("##vendorlist_filename.csv", sep = "\t", quote = "", header = F)
# trial_200 <- list(vendorlist[1:4 # nrow(vendorlist) 
#                              ,3 ])

#sample size to scrape
batch <- 4

trial_200 <- as.vector(vendorlist$VendorNameToSearch[1:batch])
df <- data.frame(trial_200 = trial_200, stringsAsFactors = FALSE)


listOfSleepTimes <- seq(from = 10, to = 15, by = 0.02)
listOfBrowsers <- c("firefox", "chrome", "internet explorer")

##### Scrape Number of Search results 
NoSearchResults<- function(name1){
  # randomBrowser <- sample(listOfBrowsers, size = 1, replace = TRUE)
  # randomSleepTime <- sample(listOfSleepTimes, size = 1, replace = TRUE)
  # typeof(as.character(randomSleepTime))
  # Sys.sleep(randomSleepTime)
  # #print the current search time & and Sys.sleep()
  # print(paste0("finding the url for ", name1,
  #              " with a sleep time of ", randomSleepTime,
  #              " using ", randomBrowser,
  #              " Current time is: ", Sys.time()
  # ))
  url <- paste0("https://www.google.com/search?q=", url_encode(name1))
  
  ##Number of search results
  NoResults <- 
    read_html(url) %>%
    html_nodes(css= "#resultStats") %>%
    html_text() %>% 
    str_replace("About", "") %>%
    str_replace("results", "")

}

##### Scrape Address 
Address <- function(name2){
  url <- paste0("https://www.google.com/search?q=", url_encode(name2))
  #Phone and address
  yyy <- read_html(url) %>%
    html_nodes(css= "#rhs_block div") %>% html_text() 
  address <- as.character(grep("Address", yyy, value = TRUE)[4]) %>%
    str_replace("Address:", "") 

}

##### Scrape Phone Number
PhoneNo <- function(name3){
  url <- paste0("https://www.google.com/search?q=", url_encode(name3))
  #Phone and address
  yyy <- read_html(url) %>%
    html_nodes(css= "#rhs_block div") %>% html_text() 

  phoneno <- as.character(grep("Phone", yyy, value = TRUE)[4]) %>%
    str_replace("Phone: ", "")
  
}

##### Function to scrape above functions in parralel
multi.sapply <- function(...) {
  arglist <- match.call(expand.dots = FALSE)$...
  var.names <- sapply(arglist, deparse)
  has.name <- (names(arglist) != "")
  var.names[has.name] <- names(arglist)[has.name]
  arglist <- lapply(arglist, eval.parent, n = 2)
  x <- arglist[[1]]
  arglist[[1]] <- NULL
  result <- sapply(arglist, function (FUN, x) sapply(x, FUN), x)
  colnames(result) <- var.names[-1]
  return(result)
}

 
result <-  tryCatch(multi.sapply(trial_200, NoSearchResults, Address, PhoneNo)
                                      , error = function(err){errorSleepTime <-
                                        sample(seq(from = 900, to = 1200 , by = 60), size = 1, replace = TRUE)
                                  
                                      print(paste("The error message:  ", err))
                                      print(paste("Going to Sleep for: ", errorSleepTime, " seconds. Current time:", Sys.time()))
                                      error_message <- paste("The error message:", err, "- Error time: ", Sys.time())
                                      # write.table(error_message,file = "C:\\Clients\\Visy 2018\\Vendor Supplier Testing\\Visy_error_log.csv", row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
                                      Sys.sleep(errorSleepTime)
                                      print(paste("Waking up. Current time: ", Sys.time()))
                                      return(123456789)
                                      })
mergedf  <- cbind(rownames(result), as.data.frame(result, row.names = F)) %>% 
  left_join( df, by = c("rownames(result)"= "trial_200"))

pattern <- c(".com|.gov|.au" )
#### Srape the top 5 URLs
scrapeURL1 <- function(URL1){
  url <- paste0("https://www.google.com/search?q=", url_encode(URL1))
  
  Sys.sleep(5)
  xxx <- read_html(url) %>%
    html_nodes(css= ".s div") %>%
    html_text()
  xxx <- array(grep(pattern, xxx, value = TRUE))
  # xxx[1]
  #returns 1st URL
  return(xxx[1])
  }

#### Scrape 2nd URL 
scrapeURL2 <- function(URL2){
  url <- paste0("https://www.google.com/search?q=", url_encode(URL2))
  Sys.sleep(5)
  xxx <- read_html(url) %>%
    html_nodes(css= ".s div") %>%
    html_text()
  xxx <- array(grep(pattern, xxx, value = TRUE))
  # xxx[1]
  #returns 1st URL
  return(xxx[2])
}

#### Scrape the 3rd
scrapeURL3 <- function(URL3){
  url <- paste0("https://www.google.com/search?q=", url_encode(URL3))
  
  Sys.sleep(5)
  xxx <- read_html(url) %>%
    html_nodes(css= ".s div") %>%
    html_text()
  xxx <- array(grep(pattern, xxx, value = TRUE))
  # xxx[1]
  #returns 1st URL
  return(xxx[3])
}

#### Scrape the 4th URL
scrapeURL4 <- function(URL4){
  url <- paste0("https://www.google.com/search?q=", url_encode(URL4))
  
  Sys.sleep(5)
  xxx <- read_html(url) %>%
    html_nodes(css= ".s div") %>%
    html_text()
  xxx <- array(grep(pattern, xxx, value = TRUE))
  # xxx[1]
  #returns 1st URL
  return(xxx[4])
}

#### Scrape the 5th URL
scrapeURL5 <- function(URL5){
  url <- paste0("https://www.google.com/search?q=", url_encode(URL5))
    Sys.sleep(5)
    xxx <- read_html(url) %>%
    html_nodes(css= ".s div") %>%
    html_text()
  xxx <- array(grep(pattern, xxx, value = TRUE))
  # xxx[1]
  #returns 1st URL
  return(xxx[5])
}

URLresult <-  tryCatch(multi.sapply(trial_200, scrapeURL1, scrapeURL2, scrapeURL3, scrapeURL4, scrapeURL5)
                    , error = function(err){errorSleepTime <-
                      sample(seq(from = 900, to = 1200 , by = 60), size = 1, replace = TRUE)
                    
                    print(paste("The error message:  ", err))
                    print(paste("Going to Sleep for: ", errorSleepTime, " seconds. Current time:", Sys.time()))
                    error_message <- paste("The error message:", err, "- Error time: ", Sys.time())
                    # write.table(error_message,file = "C:\\Clients\\Visy 2018\\Vendor Supplier Testing\\Visy_error_log.csv", row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
                    Sys.sleep(errorSleepTime)
                    print(paste("Waking up. Current time: ", Sys.time()))
                    return(123456789)
                    })
mergedURL  <- cbind(rownames(URLresult), as.data.frame(URLresult, row.names = F)) %>% 
  left_join( df, by = c("rownames(URLresult)"= "trial_200"))





vendorlistdf <- vendorlist[1:batch,]
resultsdf <- left_join(vendorlistdf, mergedf, by = c("VendorNameToSearch" = "rownames(result)"))
resultsdf <- left_join(resultsdf, mergedURL, by = c("VendorNameToSearch" = "rownames(URLresult)"))
filename = paste0('googleresults',  format(Sys.time(), "%Y-%m-%d"),'.csv')
write.csv(resultsdf, file = filename , row.names = F)
