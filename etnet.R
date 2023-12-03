library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

#hkf_month <- 202312
#url<-paste("http://www.etnet.com.hk/www/eng/futures/index.php?subtype=HSI&month=", hkf_month, "&tab=interval#tab",sep="")
#html<-read_html(url)
table_list<-
   html %>% 
   html_nodes("table")

etnet_df1<-
   table_list%>%
   .[1]%>%
   html_table()%>%
   data.frame()%>%
   .[-c(1,2),]%>%
   transmute(
      #Date=tradeday,
      Time=X1,
      Open=as.numeric(str_replace_all(X2,",","")),
      High=as.numeric(str_replace_all(X3,",","")),
      Low=as.numeric(str_replace_all(X4,",","")),
      Last=as.numeric(str_replace_all(X6,",","")),
      Chg=as.numeric(str_replace_all(X7,",","")),
      "%Chg"=as.numeric(str_replace_all(X8,"%","")),
      Prem=as.numeric(str_replace_all(X9,",","")),
      Vol=as.numeric(str_replace_all(X10,",","")),
      Tic=as.numeric(str_replace_all(X11,",","")),
      "Vol/T"=round(as.numeric(str_replace_all(X12,",","")), digits = 3)
   )
rownames(etnet_df1)<-NULL
#View(etnet_df1)
bind_rows(etnet_df1, etnet_df1[c(2,1),])[-c(1, 2),] %>% as_tibble() %>%  print(n=Inf)




   







