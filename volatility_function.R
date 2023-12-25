library(tidyverse)
library(rvest)

volatility_fun <- function(){
  url <- "https://www.etnet.com.hk/www/eng/stocks/indexes_main.php"
  html <- url %>% read_html()
  Sys.sleep(1)
  volatility <- html %>%
    html_elements(".DivFigureContent") %>%
    .[1] %>%
    .[1] %>%
    html_table() %>%
    .[[1]] %>%
    filter(X1 |> str_detect("Volat"))
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
  return(volatility)
}

