library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(beepr)

#library(tidyft) # for UTF-8 encoding issue

cat("\014")

scrape_page <- 30
etnet_news_tbl <- tibble()

for (i in 1:scrape_page) {
   cat("... ", i, "\n", sep = "")
   
  url <- paste("https://www.etnet.com.hk/www/tc/news/categorized_news.php?page=", i, "&category=latest", sep = "")
  html<-read_html(url)
  Sys.sleep(1)

  containers <- html %>% html_elements(css = ".DivArticleList.dotLine")
  if(i==1) containers %>% html_elements(css = "a") %>% length() %>% print()

  datetime_list <- containers %>% map(html_element, css = ".date") %>% map(html_text2) %>% dmy_hm()        %>% unlist() %>% na.omit()
  newsheader_list <- containers %>% map(html_element, css = "a") %>% map(html_text2)        %>% unlist() %>% na.omit()
  newslink_list   <- containers %>% map(html_element, css = "a") %>% map(html_attr, "href") %>% unlist() %>% na.omit()

  df <- tibble(
     datetime = datetime_list,
     page = i,
     news = newsheader_list,
     link = newslink_list
  )

  etnet_news_tbl <- bind_rows(etnet_news_tbl, df)
}

etnet_news_tbl <- etnet_news_tbl %>% mutate(
 #link = paste("https://www.etnet.com.hk/www/tc/news/", link, sep = "")
 link = xml2::url_absolute(link, "https://www.etnet.com.hk/www/tc/news/")
)
View(etnet_news_tbl %>% select(-c("link"))) #%>% View()
