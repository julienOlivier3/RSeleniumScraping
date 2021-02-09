# Setup -------------------------------------------------------------------
setwd("C:\\Users\\301971\\Privat\\Datengetriebene_Kundenakquise")
#rm(list = ls())

# Steering
slow_down <- 1    # scales the number of scraping iterations down (keeping a low profile for test runs); 1 for iteration over all pages
min_iter <- 0     # the minimum of iterations (needs to be >=1)
reverse <- FALSE  # if TRUE: scrape from last review to first review

## Functions ==============================================================
# Package installer and loader
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# String cleaner
my_strClean <- function(str){
  str %>% 
    str_remove_all(pattern = regex("\\n", ignore_case = FALSE)) %>% 
    str_remove_all(pattern = regex("\\t", ignore_case = FALSE)) %>% 
    str_remove_all(pattern = regex("\\r", ignore_case = FALSE)) %>% 
    str_trim(side = "both") %>% # remove trailing and leading whitespaces
    str_squish()                # reduces repeated whitespaces
}


## Packages ===============================================================
package_list <- c("tidyverse",
                  "rvest",
                  "stringr",
                  "RSelenium",
                  "wdman",
                  "beepr",
                  "lubridate")

ipak(package_list)





# Scraping ----------------------------------------------------------------



### Scraper ###############################################################
# Combine RSelenium and rvest in the following scraper and Scrape company evaluations

# Initialize variables
DATE <- tibble()
REVIEW <- tibble()
RATING <- tibble()


# Define landing page first
landing <- "https://www.ekomi.de//bewertungen-easycredit.html"


# Initialize Selenium server
remDr <- rsDriver(
  port = 4448L,
  browser = "firefox"
)

Sys.sleep(runif(n = 1, min = 1, max = 5)) # sleeping for a couple of seconds (low profile)


# Navigate on landing page
remDr$client$navigate(landing)

Sys.sleep(8) # sleeping for a couple of seconds (low profile)


# Define end button (fix)
try(endButton <- remDr$client$findElement(using = "css selector", "div.pageNav li[rel^=nofollow]"))


# Save all html elements in R list
html_full <- read_html(landing)


# Get last page of reviews for subsequent loop
loop_end <- html_full %>% 
  html_nodes(css = "div.pageNav li[rel^=nofollow]") %>% 
  html_attr("onclick") %>% 
  parse_number()


# Go on last review page if reverse is TRUE
if(reverse){
  try(endButton$clickElement()) # Click
}

# Looping through pages
for (i in 1:max(min_iter, round(loop_end*slow_down))){
  
  #Wait until specific element can be found (i.e. wait until new page has loaded)
  checkElement <- NULL
  while (is.null(checkElement)) {
    checkElement <- tryCatch({remDr$client$findElement(using = "css selector", paste0("div.pageNav li.pagination-item[onclick*='=", i+1, "&']"))},
                             error = function(e){NULL})
  }
  
  
  site <- read_html(remDr$client$getPageSource()[[1]])

  
  
  # Define buttons
  Sys.sleep(5) # sleeping for a couple of seconds (low profile)
  # right click button (the second child in class pagination). Note: this is only true if one scrapes in reverse order. So only use
  # leftButton when scraping from last review to first review
  if(reverse){
    try(leftButton <- remDr$client$findElement(using = "css selector", "div.pageNav ol.pagination li:nth-child(2)")) 
  }
  # active button
  try(activeButton <- remDr$client$findElement(using = "css selector", "div.pageNav ol li.pagination-item.active"))            
  # right click button (following the active button immediately)
  if(!reverse){
    try(rightButton <- remDr$client$findElement(using = "css selector", "div.pageNav ol li.pagination-item.active + li"))
  }
  
  
  
  Sys.sleep(runif(n = 1, min = 2, max = 10)) # sleeping for a couple of seconds (low profile)  
  
  
  site %>% html_text() -> service_check
  
  
  
  
  #If there is a 503 error, go back
  if(grepl("503 Service", service_check)){ 
    
    remDr$goBack()
    
  }
  else
  {
    
    html_review <- site %>% 
      html_nodes(css = "div.reviews_content") %>% 
      html_nodes(css = "table.review-item")
    
    a <- html_review %>% 
      html_nodes(css = "td.review-data") %>% 
      html_nodes(css = "div.review-item-header") %>% 
      html_nodes(css = "span.datetime") %>% 
      html_text() %>% 
      tibble(DATE = .) %>% 
      mutate(DATE = dmy_hm(DATE)) # etract date and time
    DATE <- DATE %>% 
      bind_rows(a)
    
    b <- html_review %>% 
      html_nodes(css = "td.review-data") %>% 
      html_nodes(css = "div.review-item-body.review-coments") %>% 
      html_text() %>% 
      my_strClean(.) %>% 
      tibble(REVIEW = .) 
    REVIEW <- REVIEW %>% 
      bind_rows(b)
    
    c <- html_review %>% 
      html_nodes(css = "td.review-data") %>% 
      html_nodes(css = "div.review-item-footer.review-item-stars-adj") %>% 
      html_text() %>% 
      str_remove_all(pattern = regex("translate", ignore_case = TRUE)) %>% 
      my_strClean(.) %>% 
      tibble(RATING = .)
    RATING <- RATING %>% 
      bind_rows(c)
    
    
  }
  
  
  if(!reverse){
    try(rightButton$clickElement()) # Click
  }
  if(reverse){
    try(leftButton$clickElement()) # Click
  }
  print(i)
  
  
}

remDr$server$stop()

beep(sound = 2)

# Data processing ---------------------------------------------------------
df_scraping <- bind_cols(DATE, REVIEW, RATING)

df_scraping %>% View()
df_scraping3<- bind_cols(DATE, REVIEW, RATING)
save(df_scraping, file = file.path(getwd(), "Results", "df_scraping.RData"))
