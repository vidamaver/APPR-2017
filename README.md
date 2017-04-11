# Analiza podatkov s programom R, 2016/17
Avtor: Vida Maver

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2016/17.

## Analiza zračnih emisij v evropskih državah.

Analizirala bom količine zračnih emisij posameznih evropskih držav med letoma 2008 in 2014. Analizirala bom, kako je s količino izpustov ogljikovega dioksida, dušikovih oksidov in metana po posameznih državah in primerjala, v kateri državi je posameznih izpustov največ ter v kateri državi je vse skupaj največja količina izpuščenih zračnih emisij.

Podatke bom večinoma pridobila iz Eurostata (http://ec.europa.eu/eurostat/data/database) in nekaj iz Wikipedie. Datoteke bodo oblike .csv in HTML.

Pridobljene podatke bom predstavila v tabeli, kjer bo v 1. stolpcu navedena država, v 2. stolpcu leto, v 3. količina izpustov ogljikovega dioksida, v 4. količina izpustov dušikovih oksidov in v 5. stolpcu količina izpustov metana.

Moj cilj je ugotoviti, katera od evropskih držav ima največje količine zračnih emisij.

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
