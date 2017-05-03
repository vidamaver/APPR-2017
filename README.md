# Analiza podatkov s programom R, 2016/17
Avtor: Vida Maver

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2016/17.

## Analiza zračnih emisij v evropi.

Analizirala bom količine zračnih emisij posameznih evropskih držav med letoma 2008 in 2014. Primerjala bom količino izpustov ogljikovega dioksida, didušikovega oksida, dušikovega monoksida in metana po posameznih državah. Analizirala bom, v kateri državi je teh izpustov največ ter katera država ima največjo količino izpustov plinov v ozračje glede na njeno površino.

Podatke bom pridobila iz Eurostata (http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=env_ac_ainah_r2&lang=en) v obliki .csv in iz Wikipedie (https://en.wikipedia.org/wiki/List_of_European_countries_by_area) v obliki HTML.

Pridobljene podatke bom predstavila v dveh tabelah in sicer:

Tabela 1:

•	1. stolpec: država

•	2. stolpec: leto

•	3. stolpec: tip izpusta

•	4. stolpec: količina izpusta glede na površino države

V stolpcu tip izpusta bo možna vrednost ogljikov dioksid, didušikov oksid, dušikov monoksid ali metan.

Tabela 2:

•	1. stolpec: država

•	2. stolpec: površina

Moj cilj je ugotoviti, katera od evropskih držav ima največje količine zračnih emisij, ali se je količina le-teh glede na to, da se veliko ukvarjamo z okoljsko problematiko, kaj zmanjšala, kje se je količina zračnih emisij zmanjšala najbolj in če se je kje morebiti tudi povečala.

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
