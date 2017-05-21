# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip",
                             "OB/OB", encoding = "Windows-1250")
levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
{ gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels = levels(obcine$obcina))
zemljevid <- pretvori.zemljevid(zemljevid)

# Izračunamo povprečno velikost družine
povprecja <- druzine %>% group_by(obcina) %>%
  summarise(povprecje = sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))

library(ggplot2)
library(dplyr)

#--------------------------------------------------------------------
zdruzena_tabela <- rbind(tabela2, tabela_povrsin)

#evropske_drzave <- uvozi.zemljevid() %>% pretvori.zemljevid()

#ggplot() + geom_polygon(data = evropske_drzave, aes(x = long, y = lat, 
#                                                    group = group, fill = id)) + 
#  guides(fill = FALSE)
#--------------------------------------------------------------------

g2 = ggplot(tabela2) + aes(x = leto, y = kolicina_v_tonah, group = drzava, color = drzava) + geom_line()

g2 + xlab("leto") + ylab("kolicina_v_tonah") + ggtitle("Skupno_kolicinsko_spreminjanje_zracnih_emisij_po_drzavah")

#--------------------------------------------------------------------

g3 = ggplot(tabela2) + aes(x = leto, y = kolicina_v_tonah, group = tip_izpusta, color = tip_izpusta) + geom_line()

#g3 = ggplot(tabela2 %>% filter(tip_izpusta == "Nitrogen oxides")) + aes(x = leto, y = kolicina_v_tonah) + geom_line()

g3 + xlab("leto") + ylab("kolicina_v_tonah") + ggtitle("Kolicinsko_spreminjanje_tipov_zracnih_emisij_po_letih")

#---------------------------------------------------------------------

#povprecna_kolicina_v_posamezni_panogi <- tabela2 %>% group_by(drzava) %>% summarise(povprecje = sum(kolicina_v_tonah)/ ???)

g4 = ggplot(tabela2) + aes(x = podrocje_industrije, y = povprecna_kolicina_v_posamezni_panogi, group = podrocje_industrije) + geom_bar()

#---------------------------------------------------------------------

