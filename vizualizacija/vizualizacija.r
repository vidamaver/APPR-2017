# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(dplyr)
library(readr)
library(tibble)
library(shiny)

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

#poračun skupnih izpustov na površino
izpusti.povrsina <- tabela2 %>% group_by(drzava, leto) %>%
  summarise(kolicina = sum(kolicina_v_tonah)) %>%
  group_by(drzava) %>% summarise(kolicina = mean(kolicina, na.rm = TRUE)) %>% inner_join(tabela_povrsin) %>%
  transmute(drzava = factor(drzava, levels = levels(evropa$name_sort)),
            izpusti = kolicina / povrsina_v_km2)


#vsote čez vse panoge za vsako leto, nato izračunano povprečje teh vsot(ignoriram vrednosti NA)

izpusti.podrocje <- tabela2 %>% group_by(leto, podrocje_industrije) %>%
   summarise(izpusti = sum(kolicina_v_tonah)) %>%
   group_by(podrocje_industrije) %>%
   summarise(povprecni_izpusti = mean(izpusti, na.rm = TRUE))

#izris zemljevida
#Malta ima največji količnik med izpusti in površino - izločim jo iz podatkov

z = ggplot() + geom_polygon(data = left_join(evropa, izpusti.povrsina,
                                         by = c("name_sort" = "drzava")) %>% filter(name_sort != "Malta"),
                        aes(x = long, y = lat, group = group, fill = izpusti)) +
  coord_map(xlim = c(-25, 40), ylim = c(32, 72))


#-----------------------------------------------------------------------------------------------------
#spreminjanje količin izpustov skozi leta v Malti (po vseh tipih in vseh industrijah)
g1 = ggplot(tabela2 %>% filter(drzava %in% c("Malta")) %>%
              group_by(leto, drzava) %>%
              summarise(izpusti = sum(kolicina_v_tonah)) %>%
              inner_join(tabela_povrsin),
            aes(x = leto, y = izpusti/povrsina_v_km2)) + geom_line(col = "green")


#spreminjanje količin izpustov skozi leta za posamezno državo (po vseh tipih in vseh industrijah)

#slovar za preimenovanje v slovenščino

slovar1 <- c("Slovenia" = "Slovenija",
             "Netherlands" = "Nizozemska",
             "Luxembourg" = "Luksemburg",
             "Belgium" = "Belgija",
             "Germany" = "Nemčija",
             "Denmark" = "Danska",
             "United Kingdom" = "Velika Britanija")


g2 = ggplot(tabela2 %>% filter(drzava %in% c("Slovenia", "Netherlands", "Luxembourg", "Belgium", "Germany", "Denmark", "United Kingdom")) %>% 
              group_by(leto, drzava) %>%
              summarise(izpusti = sum(kolicina_v_tonah)) %>%
              inner_join(tabela_povrsin),
            aes(x = leto, y = izpusti / povrsina_v_km2, color = slovar1[drzava])) + geom_line() +
  guides(color = guide_legend(title = "Država")) + ylab("izpusti v tonah")


#--------------------------------------------------------------------
#spreminjanje količine posameznega tipa izpusta po vseh državah in vseh panogah skupaj, po letih

#izpustov ogljikovega dioksida je neprimerno več od ostalih izpustov, zato ga prikažem posebaj
g31 = ggplot(tabela2 %>% filter(tip_izpusta == "Carbon dioxide") %>%
               group_by(leto) %>% summarise(kolicina = sum(kolicina_v_tonah, na.rm = TRUE))) +
  aes(x = leto, y = kolicina / 1e9) + geom_line(col = "orange")

#preostali izpusti prikazani na enem grafu

#preimenovanje v slovenščino 

slovar2 <- c("Methane" = "Metan",
            "Nitrogen oxides" = "Dušikov monoksid",
            "Nitrous oxide" = "Didušikov oksid")

g32 = ggplot(tabela2 %>% filter(tip_izpusta != "Carbon dioxide") %>% 
               group_by(leto, tip_izpusta) %>%
               summarise(izpusti = sum(kolicina_v_tonah, na.rm = TRUE)),
             aes(x = leto, y = izpusti / 1e6, color = slovar2[tip_izpusta])) + geom_line() +
  guides(color = guide_legend(title = "Tip izpusta")) + ylab("izpusti v milijonih ton")

#---------------------------------------------------------------------
#primerjava izpustov po panogah

panoge <- tabela2 %>% group_by(leto, podrocje_industrije) %>%
  summarise(izpusti = sum(kolicina_v_tonah, na.rm = TRUE)) %>%
  group_by(podrocje_industrije) %>%
  summarise(povprecni_izpusti = mean(izpusti)) %>% arrange(desc(povprecni_izpusti))

g41 = ggplot(panoge[1:3, ],
            aes(x = podrocje_industrije, y = povprecni_izpusti/ 1e9)) + geom_bar(stat = "identity", fill = "purple") +
  scale_x_discrete(labels = c("Elektrika, bencin, para", "Proizvodnja", "Transport in skladiščenje")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#oznake <- c("Kmetijstvo, gozdarstvo, ribolov", "Veleprodaja, prodaja na drobno,\n popravila motornih vozil", "Rudarstvo, kamnolom",
#           "Gradbeništvo", "Oskrba z vodo, kanalizacija", "Javna uprava, obramba", "Dejavnosti socialnega dela\n in varstva za človekove pravice",
#           "Administracija, podporne storitve", "Znanstvene in tehnične aktivnosti", "Izobraževanje", "Nastanitve, oskrba s hrano","Druge storitve", "Komunikacija",
#           "Aktivnosti povezane z\n nepremičninami", "Finančne in zavarovalne aktivnosti", "Umetnost, zabava,\n rekreacija",
#           "Dejavnosti gospodinjstev za\n lastno rabo", "Dejavnosti ekstrateritorialnih\n organizacij")

g42 = ggplot(panoge[4:16, ],
             aes(x = reorder(podrocje_industrije, -povprecni_izpusti), y = povprecni_izpusti / 1e6)) + geom_bar(stat = "identity", fill = "darkblue") +
  scale_x_discrete(labels = c("Kmetijstvo, gozdarstvo, ribolov", "Veleprodaja, prodaja na drobno,\n popravila motornih vozil", 
                              "Rudarstvo, kamnolom", "Gradbeništvo", "Oskrba z vodo, kanalizacija", "Javna uprava, obramba", 
                              "Dejavnosti socialnega dela in\n varstva za človekove pravice", "Administracija, podporne storitve", 
                              "Znanstvene in tehnične aktivnosti", "Izobraževanje", "Nastanitve, oskrba s hrano","Druge storitve", "Komunikacija")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

g43 = ggplot(panoge[17:21, ],
             aes(x = reorder(podrocje_industrije, -povprecni_izpusti), y = povprecni_izpusti / 1e6)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  scale_x_discrete(labels = c("Aktivnosti povezane z\n nepremičninami", "Finančne in zavarovalne\n aktivnosti", 
                              "Umetnost, zabava, rekreacija", "Dejavnosti gospodinjstev za\n lastno rabo", 
                              "Dejavnosti ekstrateritorialnih\n organizacij")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#---------------------------------------------------------------------

