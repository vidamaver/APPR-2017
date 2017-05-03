link <- "https://en.wikipedia.org/wiki/List_of_European_countries_by_area"
stran <- html_session(link) %>% read_html()
tabela_povrsin <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']")%>%
  .[[1]] %>% html_table(dec = ".")

library(rvest)
library(gsubfn)
library(readr)
library(dplyr)

colnames(tabela_povrsin) <- c("stevnik", "drzava", "povrsina_v_km2", "opis")
sl <- locale("sl", decimal_mark = ".")
for (col in c("povrsina_v_km2")) {
  tabela_povrsin[[col]] <- parse_number(tabela_povrsin[[col]], na = "-", locale = sl)
}

tabela_povrsin$stevnik <- NULL
tabela_povrsin$opis <- NULL

tabela_povrsin <- tabela_povrsin[-c(49), ]

#View(tabela_povrsin)
#summary(tabela_povrsin)

#===================================================================

colnames(Zracne_emisije_po_drzavah) <- c("leto", "drzava", "tip_izpusta", "podrocje_industrije", "kolicina_v_tonah")


View(Zracne_emisije_po_drzavah)
#summary(Zracne_emisije_po_drzavah)

