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

evropa <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                  "ne_50m_admin_0_countries", encoding = "UTF-8") %>%
  pretvori.zemljevid() %>% filter(continent == "Europe" | sovereignt %in% c("Turkey", "Cyprus"),
                                  long > -30)

ggplot() + geom_polygon(data = evropa, aes(x = long, y = lat, group = group)) + 
  coord_map(xlim = c(-25, 40), ylim = c(32, 72))


tabela2$drzava <- gsub("Germany.*$", "Germany", tabela2$drzava) #moji podatki
evropa$name_sort <- gsub("^Slovak Republic$", "Slovakia", evropa$name_sort) %>% factor() #zemljevid

izpusti.povrsina <- tabela2 %>% group_by(drzava) %>%
  summarise(kolicina = sum(kolicina_v_tonah)) %>% inner_join(tabela_povrsin) %>%
  transmute(drzava = factor(drzava, levels = levels(evropa$name_sort)),
            izpusti = kolicina / povrsina_v_km2)

ggplot() + geom_polygon(data = left_join(evropa, izpusti.povrsina,
                                         by = c("name_sort" = "drzava")),
                        aes(x = long, y = lat, group = group, fill = izpusti)) +
  coord_map(xlim = c(-25, 40), ylim = c(32, 72))
#-----------------------------------------------------------------------------------------------------

g2 = ggplot(tabela2 %>% filter(drzava == "Slovenia") %>% group_by(leto) %>%
              summarise(izpusti = sum(kolicina_v_tonah)),
            aes(x = leto, y = izpusti)) + geom_line()

g2 + xlab("leto") + ylab("kolicina_v_tonah") + ggtitle("Skupno_kolicinsko_spreminjanje_zracnih_emisij_po_drzavah")

#--------------------------------------------------------------------

#g3 = ggplot(tabela2) + aes(x = leto, y = kolicina_v_tonah, group = tip_izpusta, color = tip_izpusta) + geom_line()
#g3 = ggplot(tabela2 %>% filter(tip_izpusta == "Nitrogen oxides")) + aes(x = leto, y = kolicina_v_tonah) + geom_line()

#graf za ogljikov dioksid prikazi posebaj (g31)
#preostali izpusti na enem grafu

g32 = ggplot(tabela2 %>% filter(tip_izpusta != "Carbon dioxide") %>% group_by(leto, tip_izpusta) %>%
               summarise(izpusti = sum(kolicina_v_tonah, na.rm = TRUE)),
             aes(x = leto, y = izpusti, color = tip_izpusta)) + geom_line()

g3 + xlab("leto") + ylab("kolicina_v_tonah") + ggtitle("Kolicinsko_spreminjanje_tipov_zracnih_emisij_po_letih")

#---------------------------------------------------------------------

#povprecna_kolicina_v_posamezni_panogi <- tabela2 %>% group_by(drzava) %>% summarise(povprecje = sum(kolicina_v_tonah)/ ???)

g4 = ggplot(tabela2 %>% group_by(leto, podrocje_industrije) %>%
              summarise(izpusti = sum(kolicina_v_tonah, na.rm = TRUE)) %>%
              group_by(podrocje_industrije) %>%
              summarise(povprecni_izpusti = mean(izpusti), na.rm = TRUE),
            aes(x = podrocje_industrije, y = povprecni_izpusti)) + geom_bar(stat = "identity")
            
#---------------------------------------------------------------------

