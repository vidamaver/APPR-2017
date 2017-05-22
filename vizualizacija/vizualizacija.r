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
library(readr)
library(tibble)

#--------------------------------------------------------------------
zdruzena_tabela <- rbind(tabela2, tabela_povrsin)

evropa <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                  "ne_50m_admin_0_countries", encoding = "UTF-8") %>%
  pretvori.zemljevid() %>% filter(continent == "Europe" | sovereignt %in% c("Turkey", "Cyprus"),
                                  long > -30)

#kako izpustim Rusijo (podatkov nimam in je ne rabim)?

#prikaz zemljevida brez podatkov
ggplot() + geom_polygon(data = evropa, aes(x = long, y = lat, group = group)) + 
  coord_map(xlim = c(-25, 40), ylim = c(32, 72))

#poskrbim, da se imena držav (oziroma po čemer združujem), ujemajo
tabela2$drzava <- gsub("Germany.*$", "Germany", tabela2$drzava) #moji podatki
evropa$name_sort <- gsub("^Slovak Republic$", "Slovakia", evropa$name_sort) %>% factor() #zemljevid

#poračun skupnih izpustov na površino
izpusti.povrsina <- tabela2 %>% group_by(drzava) %>%
  summarise(kolicina = sum(kolicina_v_tonah)) %>% inner_join(tabela_povrsin) %>%
  transmute(drzava = factor(drzava, levels = levels(evropa$name_sort)),
            izpusti = kolicina / povrsina_v_km2)

#dodaj podatka o površini (a je OK?)

tabela_povrsin <- rbind(tabela_povrsin, "Turkey" = 783562.00)
tabela_povrsin <- rbind(tabela_povrsin, "Cyprus" = 9251.00)

#vsote čez vse panoge za vsako leto (a je OK?)

tabela2 %>% group_by(leto, podrocje_industrije) %>%
         summarise(izpusti = sum(kolicina_v_tonah, na.rm = TRUE)) %>%
         group_by(podrocje_industrije) %>%
         summarise(povprecni_izpusti = mean(izpusti), na.rm = TRUE)

#izris zemljevida
ggplot() + geom_polygon(data = left_join(evropa, izpusti.povrsina,
                                         by = c("name_sort" = "drzava")),
                        aes(x = long, y = lat, group = group, fill = izpusti)) +
  coord_map(xlim = c(-25, 40), ylim = c(32, 72))

#Malta ima največji količnik med izpusti in površino - izločim jo iz podatkov (a je OK?)
tabela2 %>% filter(GEO != "Malta")

#-----------------------------------------------------------------------------------------------------

#spreminjanje količin izpustov skozi leta za posamezno državo (po vseh tipih in vseh industrijah)
g2 = ggplot(tabela2 %>% filter(drzava == "Slovenia") %>% group_by(leto) %>%
              summarise(izpusti = sum(kolicina_v_tonah)),
            aes(x = leto, y = izpusti)) + geom_line()

g2 + xlab("leto") + ylab("kolicina_v_tonah") + ggtitle("Skupno_kolicinsko_spreminjanje_zracnih_emisij_po_drzavah")

#--------------------------------------------------------------------
#spreminjanje količine posameznega tipa izpusta po vseh država in vseh panogah skupaj, po letih

#izpustov ogljikovega dioksida je neprimerno več od ostalih izpustov, zato ga prikažem posebaj (a je OK?)
g31 = ggplot(tabela2 %>% filter(tip_izpusta = "Carbon dioxide")) + aes(x = leto, y = kolicina_v_tonah) + geom_line()

#preostali izpusti prikazani na enem grafu
g32 = ggplot(tabela2 %>% filter(tip_izpusta != "Carbon dioxide") %>% group_by(leto, tip_izpusta) %>%
               summarise(izpusti = sum(kolicina_v_tonah, na.rm = TRUE)),
             aes(x = leto, y = izpusti, color = tip_izpusta)) + geom_line()
#pri uporabi zgornjega pazi, da so podatki še vedno primerljivi -
# - (če bi kje manjkal podatek o kakšni državi, ki znatno vpliva na izpuste)

g32 + xlab("leto") + ylab("kolicina_v_tonah") + ggtitle("Kolicinsko_spreminjanje_tipov_zracnih_emisij_po_letih (brez CO2)")

#---------------------------------------------------------------------
#primerjava izpustov po panogah

g4 = ggplot(tabela2 %>% group_by(leto, podrocje_industrije) %>%
              summarise(izpusti = sum(kolicina_v_tonah, na.rm = TRUE)) %>%
              group_by(podrocje_industrije) %>%
              summarise(povprecni_izpusti = mean(izpusti), na.rm = TRUE),
            aes(x = podrocje_industrije, y = povprecni_izpusti)) + geom_bar(stat = "identity")

#problem: veliko panog, dolga imena - legenda neberljiva
#naredi več stolpčnih grafov, kjer prikažeš panoge z bolj primerljivimi izpusti - podatke pripravi vnaprej, -
# - s filter izberi želene panoge
#za spremembo prikazanih imen, grafu prištejem scale_x_discrete(labels = oznake) 
# oznake = vektor želenih oznak (v slovenščini)
# če bi oznake zarotirala prištejem še theme(axis.text.x = element_text(angle = 90, hjust = 1))










#---------------------------------------------------------------------

