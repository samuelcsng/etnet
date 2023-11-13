library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(beepr)

#library(tidyft) # for UTF-8 encoding issue

cat("\014")


# scrape page -------------------------------------------------------------

scrape_page <- 10
etnet_news_tbl <- tibble()

# first page --------------------------------------------------------------

cat("... ", 1, "\n", sep = "")
#url <- "https://www.etnet.com.hk/www/tc/news/categorized_news.php"
url <- "https://www.etnet.com.hk/www/tc/news/categorized_news.php?page=1&category=latest"
html<-read_html(url)

containers <- html %>% html_elements(css = ".DivArticleList.dotLine")

datetime_list <- containers %>% 
   html_elements(css = ".date") %>% html_text2() %>% dmy_hm()

newsheader_list <- containers %>% 
   html_elements(css = "a") %>% html_text2() %>% .[c(-2)]
   #html_elements(css = "a") %>% html_text2() %>% .[c(-2,-4)]
   #html_elements(css = "a") %>% html_text2() #%>% .[c()]
   #html_elements(css = "a") %>% .[1] %>% html_text2() #%>% .[c(-2,-4)]

newslink_list <- containers %>% 
   html_elements(css = "a") %>% html_attr("href") %>% .[c(-2)]
   #html_elements(css = "a") %>% html_attr("href") %>% .[c(-2,-4)]
   #html_elements(css = "a") %>% html_attr("href") #%>% .[c()]
   #html_elements(css = "a") %>% .[1] %>% html_attr("href") #%>% .[c(-2,-4)]

#newsheader_list2 <- containers %>% html_element(css = "a") %>% html_text2() %>% na.omit()

#, a.ArticleHdr

#newsheader_list %>% print()

df <- tibble(
   datetime = datetime_list,
   page = 1,
   news = newsheader_list,
   link = newslink_list
)

#df %>% print() # UTF-8 code
#df %>% print.listof() # OK but ...
#df %>% as.matrix %>% print() # only ok in R-console
#df %>% view()

etnet_news_tbl <- bind_rows(etnet_news_tbl, df)



# other pages -------------------------------------------------------------

for (i in 2:scrape_page) {
   Sys.sleep(1)
   cat("... ", i, "\n", sep = "")
   
#url <- "https://www.etnet.com.hk/www/tc/news/categorized_news.php?page=2&category=latest"
url <- paste("https://www.etnet.com.hk/www/tc/news/categorized_news.php?page=", i, "&category=latest", sep = "")
html<-read_html(url)

containers <- html %>% html_elements(css = ".DivArticleList.dotLine")

datetime_list <- containers %>% 
   html_elements(css = ".date") %>% html_text2() %>% dmy_hm()

newsheader_list <- containers %>% 
   html_elements(css = "a") %>% html_text2() #%>% .[-2]

newslink_list <- containers %>% 
   html_elements(css = "a") %>% html_attr("href") #%>% .[-2]

#newsheader_list2 <- containers %>% html_element(css = "a") %>% html_text2() %>% na.omit()

#, a.ArticleHdr

#newsheader_list %>% print()

df <- tibble(
   datetime = datetime_list,
   page = i,
   news = newsheader_list,
   link = newslink_list
)

#df %>% print() # UTF-8 code
#df %>% print.listof() # OK but ...
#df %>% as.matrix %>% print() # only ok in R-console
#df %>% view()
etnet_news_tbl <- bind_rows(etnet_news_tbl, df)

}



etnet_news_tbl <- etnet_news_tbl %>% mutate(
 #link = paste("https://www.etnet.com.hk/www/tc/news/", link, sep = "")
 link = xml2::url_absolute(link, "https://www.etnet.com.hk/www/tc/news/")
)
#etnet_news_tbl %>% View()

