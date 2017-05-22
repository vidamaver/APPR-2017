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

evropa <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                  "ne_50m_admin_0_countries", encoding = "UTF-8") %>%
  pretvori.zemljevid() %>% filter(continent == "Europe" | sovereignt %in% c("Turkey", "Cyprus"),
                                  long > -30, sovereignt != "Russia")


#prikaz zemljevida brez podatkov
ggplot() + geom_polygon(data = evropa, aes(x = long, y = lat, group = group)) + 
  coord_map(xlim = c(-25, 40), ylim = c(32, 72))

#poskrbim, da se imena držav (oziroma po čemer združujem), ujemajo
tabela2$drzava <- gsub("Germany.*$", "Germany", tabela2$drzava) #moji podatki
evropa$name_sort <- gsub("^Slovak Republic$", "Slovakia", evropa$name_sort) %>% factor() #zemljevid

tabela_povrsin <- rbind(tabela_povrsin, data.frame(drzava = c("Turkey", "Cyprus"),
                                                   povrsina_v_km2 = c(783562.00, 9251.00)))



#poračun skupnih izpustov na površino
izpusti.povrsina <- tabela2 %>% group_by(drzava, leto) %>%
  summarise(kolicina = sum(kolicina_v_tonah)) %>%
  group_by(drzava) %>% summarise(kolicina = mean(kolicina, na.rm = TRUE)) %>% inner_join(tabela_povrsin) %>%
  transmute(drzava = factor(drzava, levels = levels(evropa$name_sort)),
            izpusti = kolicina / povrsina_v_km2)

#vsote čez vse panoge za vsako leto

izpusti.podrocje <- tabela2 %>% group_by(leto, podrocje_industrije) %>%
   summarise(izpusti = sum(kolicina_v_tonah)) %>%
   group_by(podrocje_industrije) %>%
   summarise(povprecni_izpusti = mean(izpusti, na.rm = TRUE))

#izris zemljevida
#Malta ima največji količnik med izpusti in površino - izločim jo iz podatkov
ggplot() + geom_polygon(data = left_join(evropa, izpusti.povrsina,
                                         by = c("name_sort" = "drzava")) %>% filter(name_sort != "Malta"),
                        aes(x = long, y = lat, group = group, fill = izpusti)) +
  coord_map(xlim = c(-25, 40), ylim = c(32, 72))


#-----------------------------------------------------------------------------------------------------

#spreminjanje količin izpustov skozi leta za posamezno državo (po vseh tipih in vseh industrijah)
g2 = ggplot(tabela2 %>% filter(drzava %in% c("Slovenia", "Croatia")) %>% group_by(leto, drzava) %>%
              summarise(izpusti = sum(kolicina_v_tonah)),
            aes(x = leto, y = izpusti, color = drzava)) + geom_line()

g2 + xlab("leto") + ylab("kolicina_v_tonah") + ggtitle("Skupno_kolicinsko_spreminjanje_zracnih_emisij_po_drzavah")

#g2 sedaj samo za SLO - rada bi za vse?

#--------------------------------------------------------------------
#spreminjanje količine posameznega tipa izpusta po vseh državah in vseh panogah skupaj, po letih

#izpustov ogljikovega dioksida je neprimerno več od ostalih izpustov, zato ga prikažem posebaj (a je OK?)
g31 = ggplot(tabela2 %>% filter(tip_izpusta == "Carbon dioxide") %>%
               group_by(leto) %>% summarise(kolicina = sum(kolicina_v_tonah, na.rm = TRUE))) +
  aes(x = leto, y = kolicina) + geom_line()

#g31 ne dela kul

#preostali izpusti prikazani na enem grafu
g32 = ggplot(tabela2 %>% filter(tip_izpusta != "Carbon dioxide") %>% group_by(leto, tip_izpusta) %>%
               summarise(izpusti = sum(kolicina_v_tonah, na.rm = TRUE)),
             aes(x = leto, y = izpusti, color = tip_izpusta)) + geom_line()
#pri uporabi zgornjega pazi, da so podatki še vedno primerljivi -
# - (če bi kje manjkal podatek o kakšni državi, ki znatno vpliva na izpuste)

#dušikovi oksidi ful nizko, dam posebaj?
#---------------------------------------------------------------------
#primerjava izpustov po panogah

panoge <- tabela2 %>% group_by(leto, podrocje_industrije) %>%
  summarise(izpusti = sum(kolicina_v_tonah, na.rm = TRUE)) %>%
  group_by(podrocje_industrije) %>%
  summarise(povprecni_izpusti = mean(izpusti)) %>% arrange(desc(povprecni_izpusti))

g41 = ggplot(panoge[1:3, ],
            aes(x = podrocje_industrije, y = povprecni_izpusti)) + geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Oskrba z elektriko, bencinom, paro", "Proizvodnja", "Transport in skladiščenje")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#oznake <- c("Kmetijstvo, gozdarstvo, ribolov", "Veleprodaja, prodaja na drobno, popravila motornih vozil", "Rudarstvo, kamnolom",
#           "Gradbeništvo", "Oskrba z vodo, kanalizacija", "Javna uprava, obramba", "Dejavnosti socialnega dela in varstva za človekove pravice",
#           "Administracija, podporne storitve", "Znanstvene in tehnične aktivnosti", "Izobraževanje", "Nastanitve, oskrba s hrano","Druge storitve", "Komunikacija",
#           "Aktivnosti povezane z nepremičninami", "Finančne in zavarovalne aktivnosti", "Umetnost, zabava, rekreacija",
#           "Dejavnosti gospodinjstev za lastno rabo", "Dejavnosti ekstrateritorialnih organizacij")

g42 = ggplot(panoge[4:16, ],
             aes(x = reorder(podrocje_industrije, -povprecni_izpusti), y = povprecni_izpusti)) + geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Kmetijstvo, gozdarstvo, ribolov", "Veleprodaja, prodaja na drobno, popravila motornih vozil", 
                              "Rudarstvo, kamnolom", "Gradbeništvo", "Oskrba z vodo, kanalizacija", "Javna uprava, obramba", 
                              "Dejavnosti socialnega dela in varstva za človekove pravice", "Administracija, podporne storitve", 
                              "Znanstvene in tehnične aktivnosti", "Izobraževanje", "Nastanitve, oskrba s hrano","Druge storitve", "Komunikacija")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

g43 = ggplot(panoge[17:21, ],
             aes(x = reorder(podrocje_industrije, -povprecni_izpusti), y = povprecni_izpusti)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Aktivnosti povezane z nepremičninami", "Finančne in zavarovalne aktivnosti", 
                              "Umetnost, zabava, rekreacija", "Dejavnosti gospodinjstev za lastno rabo", 
                              "Dejavnosti ekstrateritorialnih organizacij")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#---------------------------------------------------------------------

