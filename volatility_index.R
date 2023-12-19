library(tidyverse)
library(rvest)

url <- "https://www.etnet.com.hk/www/eng/stocks/indexes_main.php"
pre <- 20
while (1) {
  html <- url %>% read_html()
  
  volatility <- html %>%
    html_elements(".DivFigureContent") %>%
    .[1] %>%
    .[1] %>%
    html_table() %>%
    .[[1]] %>%
    filter(X1 |> str_detect("Volat")) #%>%
  
  
  colnames(volatility) <-
    c("index",
      "last",
      "change",
      "pctChange",
      "prvCls",
      "Open",
      "High",
      "Low")
  volatility <- volatility %>% select(1:8) %>% .[-1, ]
  volatility <- volatility %>%
    mutate(
      last = as.numeric(last),
      change = as.numeric(change),
      pctChange = parse_number(pctChange),
      prvCls = as.numeric(prvCls),
      Open = as.numeric(Open),
      High = as.numeric(High),
      Low = as.numeric(Low),
      r = (High - Low)|> round(2),
      PG = ((last - Low) / r) |> round(2)
    )
  #volatility %>% print(n = Inf)
  
  #Sys.sleep(15)
  last <- volatility[1,2]
  updn <- case_when(
    last > pre ~ "UP",
    last < pre ~ "DN",
    .default = "--"
  )
  
  textStr<-str_glue("VHSI: {volatility[1,2]} {updn} {volatility[1,3]} {volatility[1,7]} {volatility[1,8]} {volatility[1,9]} {volatility[1,10]}")
  cat(textStr, "\n")
  
  pre <- last
  
  Sys.sleep(15)
  
}