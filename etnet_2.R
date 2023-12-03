library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

#hkf_month <- 202311

url<-paste("http://www.etnet.com.hk/www/eng/futures/index.php?subtype=HSI&month=", hkf_month, "&tab=data#tab",sep="")
html<-read_html(url)
table_list<-
   html %>% 
   html_nodes("table")

etnet_df2<-
   table_list%>%
   .[1]%>%
   html_table()%>%
   data.frame()%>%
   .[c(2:3),]%>%
   transmute(
      #Date=tradeday,
      Session=str_replace_all(X1," Futures Volume",""),
      Volume=as.numeric(str_replace_all(X2,",","")),
      Tic=as.numeric(str_replace_all(X4,",","")),
      "Vol/T"=as.numeric(str_replace_all(X6,",",""))
   )
rownames(etnet_df2)<-NULL
#View(etnet_df2)
cat("\n")
etnet_df2 %>% as_tibble() %>% print()

cat("\n")
etnet_df3<-
   table_list%>%
   .[1]%>%
   html_table()%>%
   data.frame()%>%
   .[4,]%>%
   transmute(
      #Date=tradeday,
      GOI=as.numeric(str_replace_all(X2,",","")),
      NOI=as.numeric(str_replace_all(X4,",","")),
      "Expiry Date"=X6
   )
rownames(etnet_df3)<-NULL
#View(etnet_df3)
etnet_df3 %>% as_tibble() %>%  print()




   







