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

# Funkcija, ki uvozi podatke iz csv datotek v mapi "podatki"

sl <- locale("sl", decimal_mark = ".", grouping_mark = ",")

tabela2 <- read_csv(file = "podatki/Zracne_emisije_po_drzavah.csv", locale = sl, na = ":") %>%
  filter(GEO != "European Union (28 countries)") %>%
  filter(NACE_R2 != "Total - all NACE activities")

colnames(tabela2) <- c("leto", "drzava", "tip_izpusta", "podrocje_industrije","enota", "kolicina_v_tonah")
tabela2$enota <- NULL

#tabela2 <- tabela2 %>% filter(podrocje_industrije != "Total - all NACE activities")

#View(tabela2)
#summary(tabela2)

#dodani manjkajoči površini
tabela_povrsin <- rbind(tabela_povrsin, data.frame(drzava = c("Turkey", "Cyprus"),
                                                   povrsina_v_km2 = c(783562.00, 9251.00)))

#odstranim države z zvezdico

tabela_povrsin <- tabela_povrsin %>% filter(! grepl("[*]", drzava))

