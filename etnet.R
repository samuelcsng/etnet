library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

url<-paste("http://www.etnet.com.hk/www/eng/futures/index.php?subtype=HSI&month=", hkf_month, "&tab=interval#tab",sep="")
html<-read_html(url)
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
      Date=tradeday,
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
      Date=tradeday,
      Session=str_replace_all(X1," Futures Volume",""),
      Volume=as.numeric(str_replace_all(X2,",","")),
      Tic=as.numeric(str_replace_all(X4,",","")),
      "Vol/T"=as.numeric(str_replace_all(X6,",",""))
   )
rownames(etnet_df2)<-NULL
#View(etnet_df2)



etnet_df3<-
   table_list%>%
   .[1]%>%
   html_table()%>%
   data.frame()%>%
   .[4,]%>%
   transmute(
      Date=tradeday,
      GOI=as.numeric(str_replace_all(X2,",","")),
      NOI=as.numeric(str_replace_all(X4,",","")),
      "Expiry Date"=X6
   )
rownames(etnet_df3)<-NULL
#View(etnet_df3)



#url<-paste("http://www.etnet.com.hk/www/eng/futures/index.php?subtype=HSI&month=", hkf_month, "&tab=data#tab",sep="")
url<-"http://www.etnet.com.hk/www/eng/stocks/indexes_detail.php?subtype=HSI&column=1"
html<-read_html(url)
table_list<-
   html %>% 
   html_nodes("table")

etnet_df4<-
   table_list%>%
   .[3]%>%
   html_table()%>%
   data.frame()%>%
   .[-c(1),]%>%
   transmute(
      Date=tradeday,
      Code=X1,
      Name=X2,
      Nominal=as.numeric(X4),
      Change=as.numeric(X5),
      "%Change"=as.numeric(str_replace_all(X6,"%","")),
      Turnover=as.numeric(str_sub(X7,1,-2))*case_when(str_sub(X7,-1,-1)=="M" ~ 1000000,
                                                      str_sub(X7,-1,-1)=="B" ~ 1000000000),
      #Unit=if_else(str_sub(X7,-1,-1)=="M",1000000,1000000000),
      Currency=X8
   )
rownames(etnet_df4)<-NULL
#print(etnet_df4)




url<-"http://www.etnet.com.hk/www/eng/stocks/indexes_detail.php?subtype=CEI"
html<-read_html(url)
table_list<-
   html %>% 
   html_nodes("table")

etnet_df5<-
   table_list%>%
   .[3]%>%
   html_table()%>%
   data.frame()%>%
   .[-c(1),]%>%
   transmute(
      Date=tradeday,
      Code=X1,
      Name=X2,
      Nominal=as.numeric(X4),
      Change=as.numeric(X5),
      "%Change"=as.numeric(str_replace_all(X6,"%","")),
      Turnover=as.numeric(str_sub(X7,1,-2))*case_when(str_sub(X7,-1,-1)=="M" ~ 1000000,
                                                      str_sub(X7,-1,-1)=="B" ~ 1000000000),
      #Unit=if_else(str_sub(X7,-1,-1)=="M",1000000,1000000000),
      Currency=X8
   )
rownames(etnet_df5)<-NULL
#print(etnet_df5)







url<-"http://www.etnet.com.hk/www/eng/stocks/indexes_main.php"
html<-read_html(url)
table_list<-
   html %>% 
   html_nodes("table")

etnet_df6<-
   table_list%>%
   .[2]%>%
   html_table()%>%
   data.frame()%>%
   mutate(Date=tradeday)
rownames(etnet_df6)<-NULL
#print(etnet_df6)









load("etnet_.Rdata")

etnet_df_1 <-
   etnet_df_1 %>%
   filter(Date<tradeday)%>%
   bind_rows(etnet_df1)

etnet_df_2 <-
   etnet_df_2 %>%
   filter(Date<tradeday)%>%
   bind_rows(etnet_df2)

etnet_df_3 <-
   etnet_df_3 %>%
   filter(Date<tradeday)%>%
   bind_rows(etnet_df3)

etnet_df_4 <-
   etnet_df_4 %>%
   filter(Date<tradeday)%>%
   bind_rows(etnet_df4)

etnet_df_5 <-
   etnet_df_5 %>%
   filter(Date<tradeday)%>%
   bind_rows(etnet_df5)

etnet_df_6 <-
   etnet_df_6 %>%
   filter(Date<tradeday)%>%
   bind_rows(etnet_df6)

save(list=c("etnet_df_1","etnet_df_2", "etnet_df_3", "etnet_df_4", "etnet_df_5", "etnet_df_6"), file = "etnet_.Rdata")



old <- options(
   pillar.sigfig = 6,
   pillar.print_max = 5,
   pillar.print_min = 5,
   pillar.advice = FALSE
)

print(etnet_df1)
last_time <- etnet_df1 %>% pull(Time) %>% last() #%>% print()
#check_row <- etnet_df1 %>% pull(Time) %>% last() %>% str_sub(-2, -1) %>% as.integer()
check_row <- last_time %>% str_sub(-2, -1) %>% as.integer()

nrow_adj <- case_when(
   check_row %in% c(0, 15, 30, 45) ~ nrow(etnet_df1) - 2,
   TRUE ~ nrow(etnet_df1) - 1
   )
#etnet_df1 %>% nrow() %>% print()

#etnet_df_1%>%
#   filter(Date>=tradeday-5)%>%
#   select(Date, Time, Vol, Tic, "Vol/T")%>%
#   pivot_wider(names_from = Date,
#               values_from = c(Vol, Tic, "Vol/T"),
#               names_glue = "{Date}_{.value}")%>%
#   arrange(Time)%>%
#   print(n=Inf)

tradedays <- etnet_df_1 %>%
   select(Date) %>%
   distinct(Date)
#print(tradedays %>% tail(5) %>% as.list())

etnet_df_1_filtered <-
   etnet_df_1 %>%
   filter( Date %in% (tradedays %>% tail(5) %>% .$Date %>% as.list()) ) %>%
   select(Date, Time, Vol, Tic, "Vol/T") %>%
   pivot_wider(names_from = Date,
               values_from = c(Vol, Tic, "Vol/T"),
               names_glue = "{Date}_{.value}")%>%
   arrange(Time)

etnet_df_1_filtered %>%
   filter(Time != "Prv Day") %>%
   bind_rows(
      etnet_df_1_filtered %>%
         filter(Time == "Prv Day")
      ) %>%
   print(n=Inf)

#etnet_df_1_filtered %>% head(nrow_adj) %>% print(n=Inf)
etnet_df_1_filtered %>% 
   head(nrow_adj) %>% select(2:11) %>%
   summarise_if(
      .predicate = is.numeric,
      .funs = sum,
      na.rm = TRUE
   ) %>% mutate(Time = last_time) %>% select(Time, everything()) %>%
   print(n = Inf) 

etnet_df_2%>%
   filter(between(Date, month_start, tradeday)) %>%
   as_tibble() %>%
   arrange(desc(Session)) %>%
   pivot_wider(names_from = Session,
               values_from = c(Volume, Tic, "Vol/T"),
               names_glue = "{Session}_{.value}",
               names_vary = "slowest") %>%
   left_join(
      etnet_df_3%>%
         filter(between(Date, month_start, tradeday)) %>%
         as_tibble() ) %>%
   print(n=Inf)

options(old)




   
#View(etnet_df_3)
#View(etnet_df_2)
#View(etnet_df_1)





   







