library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)
library(beepr)

options(width=270)

#cat("\014")

hkf_month <- 202311
url<-paste("http://www.etnet.com.hk/www/eng/futures/index.php?subtype=HSI&month=", hkf_month, "&tab=interval#tab",sep="")

reference_list <- c()
last_record <- 17293 #17156 #17260

n <- 19
pg_ratios <- c(0, 0.1, 1/8, 1/6, 0.2, 0.25, 0.3, 1/3, 0.4, 0.5, 0.6, 2/3, 0.7, 0.75, 0.8, 5/6, 7/8, 0.9, 1)

while (1) {
   today<-format(today(),"%Y%m%d")
   now<-format(now(),"%H:%M:%S")
   #now <- force_tz(now(), tzone = "hongkong")
   
   html<-read_html(url)
   
   containers <- html %>% html_elements(css = ".FuturesQuoteContent")
   hsif_regular_record_html <- containers[[1]]
   hsif_at_record_html <- containers[[2]]
   hsi_record_html <- containers[[3]]

# regular -----------------------------------------------------------------

   hsif_regular_record <- hsif_regular_record_html %>%
      html_element(css = ".FuturesQuoteNominal") %>% html_text2() %>%
      parse_number() %>% as.integer()
   regular_c <- hsif_regular_record_html %>% 
      html_element(css =".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2() %>% 
      str_split("O") %>% .[[1]] %>% .[1] %>% parse_number()
   regular_o <- hsif_regular_record_html %>% 
      html_element(css =".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2() %>% 
      str_split("O") %>% .[[1]] %>% .[2] %>% parse_number()
   regular_h <- hsif_regular_record_html %>% 
      html_element(css =".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2() %>% 
      str_split("/") %>% .[[1]] %>% .[2] %>% parse_number()
   regular_l <- hsif_regular_record_html %>% 
      html_element(css =".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2() %>% 
      str_split("/") %>% .[[1]] %>% .[3] %>% parse_number()
   regular_r <- regular_h - regular_l
   regular_pg <- ( (hsif_regular_record - regular_l) / regular_r ) |> round(3)
   regular_pg_list <- ( regular_l + regular_r * pg_ratios ) %>% round() %>% as.integer()
   regular_references <- c(regular_c, regular_o, regular_pg_list) #%>% unique() %>% sort()
   regular_tbl <- tibble(
      session = "regular",
      c = regular_c,
      o = regular_o,
      r = regular_r,
      `0.00` = regular_pg_list[1],
      `0.10` = regular_pg_list[2],
      `1/8`  = regular_pg_list[3],
      `1/6`  = regular_pg_list[4],
      `0.20` = regular_pg_list[5],
      `0.25` = regular_pg_list[6],
      `0.30` = regular_pg_list[7],
      `1/3`  = regular_pg_list[8],
      `0.40` = regular_pg_list[9],
      `0.50` = regular_pg_list[10],
      `0.60` = regular_pg_list[11],
      `2/3`  = regular_pg_list[12],
      `0.70` = regular_pg_list[13],
      `0.75` = regular_pg_list[14],
      `0.80` = regular_pg_list[15],
      `5/6`  = regular_pg_list[16],
      `7/8`  = regular_pg_list[17],
      `0.90` = regular_pg_list[18],
      `1.00` = regular_pg_list[19]
   )
   #regular_tbl %>% print(n = Inf)
   #cat(regular_references, "\n")

# at ----------------------------------------------------------------------

   hsif_at_record      <- hsif_at_record_html %>% 
      html_element(css = ".FuturesQuoteNominal") %>% html_text2() %>% 
      parse_number() %>% as.integer()
   at_c <- hsif_at_record_html %>% 
      html_element(css =".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2() %>% 
      str_split("O") %>% .[[1]] %>% .[1] %>% parse_number()
   at_o <- hsif_at_record_html %>% 
      html_element(css =".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2() %>% 
      str_split("O") %>% .[[1]] %>% .[2] %>% parse_number()
   at_h <- hsif_at_record_html %>% 
      html_element(css =".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2() %>% 
      str_split("/") %>% .[[1]] %>% .[2] %>% parse_number()
   at_l <- hsif_at_record_html %>% 
      html_element(css =".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2() %>% 
      str_split("/") %>% .[[1]] %>% .[3] %>% parse_number() 
   at_r <- at_h - at_l
   at_pg <- ( (hsif_at_record - at_l) / at_r ) |> round(3)
   at_pg_list <- ( at_l + at_r * pg_ratios ) %>% round() %>% as.integer()
   at_references <- c(at_c, at_o, at_pg_list) #%>% unique() %>% sort()
   at_tbl <- tibble(
      session = "at",
      c = at_c,
      o = at_o,
      r = at_r,
      `0.00` = at_pg_list[1],
      `0.10` = at_pg_list[2],
      `1/8`  = at_pg_list[3],
      `1/6`  = at_pg_list[4],
      `0.20` = at_pg_list[5],
      `0.25` = at_pg_list[6],
      `0.30` = at_pg_list[7],
      `1/3`  = at_pg_list[8],
      `0.40` = at_pg_list[9],
      `0.50` = at_pg_list[10],
      `0.60` = at_pg_list[11],
      `2/3`  = at_pg_list[12],
      `0.70` = at_pg_list[13],
      `0.75` = at_pg_list[14],
      `0.80` = at_pg_list[15],
      `5/6`  = at_pg_list[16],
      `7/8`  = at_pg_list[17],
      `0.90` = at_pg_list[18],
      `1.00` = at_pg_list[19]
   )
   #at_tbl %>% print(n = Inf)
   #cat(at_references, "\n")
   
   #reference_tbl <- bind_rows(regular_tbl, at_tbl)
   #reference_tbl %>% print(n = Inf)


# hsi ---------------------------------------------------------------------

   hsi_record <- hsi_record_html %>%
      html_element(css =".FuturesQuoteNominal2") %>% html_text2() %>%
      parse_number() %>% trunc() #%>% floor()
   hsi_c <- hsi_record_html %>% 
      html_element(css = ".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2()%>% 
      str_split("O") %>% .[[1]] %>% .[1] %>% parse_number() %>% trunc() #%>% floor()
   hsi_o <- hsi_record_html %>% 
      html_element(css = ".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2()%>% 
      str_split("O") %>% .[[1]] %>% .[2] %>% parse_number() %>% trunc() #%>% floor()
   hsi_h <- hsi_record_html %>% 
      html_element(css =".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2() %>% 
      str_split("/") %>% .[[1]] %>% .[2] %>% parse_number() %>% trunc() #%>% floor()
   hsi_l <- hsi_record_html %>% 
      html_element(css =".FuturesQuoteBlock .FuturesQuoteOthers") %>% html_text2() %>% 
      str_split("/") %>% .[[1]] %>% .[3] %>% parse_number() %>% trunc() #%>% floor()
   hsi_r <- hsi_h - hsi_l
   hsi_pg <- ( (hsi_record - hsi_l) / hsi_r ) |> round(3)
   hsi_pg_list <- ( hsi_l + hsi_r * pg_ratios ) %>% trunc() #%>% floor() #%>% round(2)
   
   hsi_tbl <- tibble(
     session = "hsi",
     c = hsi_c,
     o = hsi_o,
     r = hsi_r,
     `0.00` = hsi_pg_list[1],
     `0.10` = hsi_pg_list[2],
     `1/8`  = hsi_pg_list[3],
     `1/6`  = hsi_pg_list[4],
     `0.20` = hsi_pg_list[5],
     `0.25` = hsi_pg_list[6],
     `0.30` = hsi_pg_list[7],
     `1/3`  = hsi_pg_list[8],
     `0.40` = hsi_pg_list[9],
     `0.50` = hsi_pg_list[10],
     `0.60` = hsi_pg_list[11],
     `2/3`  = hsi_pg_list[12],
     `0.70` = hsi_pg_list[13],
     `0.75` = hsi_pg_list[14],
     `0.80` = hsi_pg_list[15],
     `5/6`  = hsi_pg_list[16],
     `7/8`  = hsi_pg_list[17],
     `0.90` = hsi_pg_list[18],
     `1.00` = hsi_pg_list[19]
   )
   
   reference_tbl <- bind_rows(regular_tbl, at_tbl, hsi_tbl)
   
   #cat(regular_c, regular_o, regular_h, regular_l, hsif_regular_record, "\n", sep = " ")
   #cat(at_c, at_o, at_h, at_l, hsif_at_record, "\n", sep = " ")
   #cat(hsi_c, hsi_o, hsi_h, hsi_l, hsi_record, "\n", sep = " ")


# presentation ------------------------------------------------------------

   if ( (now >= "09:15:00") & (now < "17:15:00") ) {
      hsif_record <- hsif_regular_record
      hsif_references <- regular_references %>% unique() %>% sort()
      hsif_pg <- regular_pg
   } else {
      hsif_record <- hsif_at_record
      hsif_references <- c(regular_references, at_references) %>% unique() %>% sort()
      hsif_pg <- at_pg
   }
   
   n <- n + 1
   if (n >= 20) {
      cat("\n")
      reference_tbl %>% print(n = Inf)
      cat("\n")
      # read 15min analysis
      n <- 0
   }
   
   #cat(if_else(n >= 10, "", " "), n, " ", today, " ", now, " : ", hsif_record, " ", hsif_pg, " ( - ", hsi_record, " = ", round(hsif_record - hsi_record), " )", " --- ",
   cat(if_else(n >= 10, "", " "), n, " ",
       today, " ", now, " : ",
       "(", hsi_record, ", ", hsi_pg, ")", " ",
       "(", hsif_record, ", ", hsif_pg, ", ", round(hsif_record - hsi_record), ")", 
       " --- ",
      case_when(
          hsif_record > last_record ~ "UP",
          hsif_record < last_record ~ "DN",
          .default = ""
       ),
       sep = "")
   
   beep_up <- FALSE
   beep_dn <- FALSE
   for (reference in hsif_references) {
     if ( (last_record <= reference) & (hsif_record > reference) ) { cat(" ", reference, sep = ""); beep_up <- TRUE } #beep(7)} # beep(1) beep(7) beep(10)
     if ( (last_record >= reference) & (hsif_record < reference) ) { cat(" ", reference, sep = ""); beep_dn <- TRUE } #beep(11)} # beep(2) beep(9) beep(11)
     
   }
   if (beep_up) beep(7)
   if (beep_dn) beep(11)
   
   cat("\n")
   
   last_record <- hsif_record
   
   Sys.sleep(14.5)   
   #beep()
}
