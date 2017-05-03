link <- "https://en.wikipedia.org/wiki/List_of_European_countries_by_area"
stran <- html_session(link) %>% read_html()
tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']")%>%
  .[[1]] %>% html_table(dec = ".")

library(rvest)
library(gsubfn)
library(readr)
library(dplyr)

colnames(tabela) <- c("stevnik", "drzava", "povrsina(km^2)", "opis")
sl <- locale("sl", decimal_mark = ".")
for (col in c("povrsina(km^2)")) {
  tabela[[col]] <- parse_number(tabela[[col]], na = "-", locale = sl)
}

tabela$stevnik <- NULL
tabela$opis <- NULL