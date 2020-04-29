library(RSelenium)
library(tidyverse)

driver <- rsDriver(browser=c("firefox"))
remote_driver <- driver[["client"]]
#remote_driver$open()

library(xml2)
library(rvest)
library(stringr)

URL <- "https://stooq.pl/q/i/?s=^spx&i"

pg <- read_html(URL)

html<-html_attr(html_nodes(pg, "a"), "href")

detected <- str_detect(html_attr(html_nodes(pg, "a"), "href"), "q/.s=")
html_detected <- html[detected]
html_selected <- html_detected[which(str_detect(html_detected, "mv.us"))-1]
html_selected_all<-html_selected

for(i in 2:11){
  
    
  URL <- paste0("https://stooq.pl/q/i/?s=^spx&l=", i, "&i")
    
  pg <- read_html(URL)
  
  html<-html_attr(html_nodes(pg, "a"), "href")
  
  detected <- str_detect(html_attr(html_nodes(pg, "a"), "href"), "q/.s=")
  html_detected <- html[detected]
  html_selected <- html_detected[which(str_detect(html_detected, "mv.us"))-1]
  html_selected_all<-c(html_selected_all, html_selected)

  
}

html_selected_all[99]
############################################################################
#https://stooq.pl/q/d/?s=a.us

for(i in 442:503){
  
  to_click1<-paste0('https://stooq.pl/q/d/?s=', substr(html_selected_all[i], 6, nchar(html_selected_all[i])))
  remote_driver$navigate(to_click1)
  
  Sys.sleep(10)
  
  to_click2<-paste0('//a[@href = "q/d/l/?s=', substr(html_selected_all[i], 6, nchar(html_selected_all[i])), '&i=d"]')
  webElem <- remote_driver$findElement(value = to_click2)
  webElem$getElementAttribute("href")
  webElem$clickElement()
  
  print(paste(i, substr(html_selected_all[i], 6, nchar(html_selected_all[i])), sep = ":"))
  Sys.sleep(10)
  
}



  
  
remote_driver$close()
driver[["server"]]$stop()
