#' # Data Wrangling I: Labels & factor {#wrang1}
# Labels aus anderen Programmen in R
library(tidyverse)
pend_kap5 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                             col_select = c("pnr","welle", "zpsex", "PSM0100","azges1","palter")) %>% 
  filter(welle == 8, palter > 0,azges1 > 0)

#' labels sind als `attributes()` Variablen zugeordnet:
attributes(pend_kap5$zpsex)
attributes(pend_kap5$zpsex)$label
attributes(pend_kap5$zpsex)$labels

#' ...leider machen die `attributes()` immer wieder Probleme:
library(ggplot2)
ggplot(data = pend_kap5, aes(x = palter, y = azges1, color = zpsex )) + 
  geom_point()


# Werte oder Labels in neue Variable übernehmen -------
# labels als werte übernehmen
pend_kap5$zpsex_fct <- as_factor(pend_kap5$zpsex)
# nur die zahl übernehmen
pend_kap5$zpsex_num <- as.numeric(pend_kap5$zpsex)

# ansehen:
pend_kap5 %>% select(contains("zpsex")) %>% head()

levels(pend_kap5$zpsex_fct)

# `factor` selbst erstellen oder bearbeiten ----
pend_kap5$zpsex_fct2 <- factor(pend_kap5$zpsex_num,
                               levels = c(1,2),
                               labels = c("Männer","Frauen"))
pend_kap5$zpsex_fct3 <- factor(pend_kap5$zpsex_num,
                               levels = c(2),
                               labels = c("Frauen"))
pend_kap5$zpsex_fct4 <- factor(pend_kap5$zpsex_num,
                               levels = c(4,2),
                               labels = c("Männer","Frauen"))

# ansehen:
pend_kap5 %>% select(contains("zpsex")) %>% head()

table(pend_kap5$zpsex)
levels(pend_kap5$zpsex_fct)
levels(pend_kap5$zpsex_fct2)
levels(pend_kap5$zpsex_fct3)
levels(pend_kap5$zpsex_fct4)

#' Außerdem wird aus allen nicht angegebenen `levels` automatisch `NA`:



attributes(pend_kap5$PSM0100)
pend_kap5 %>% select(PSM0100) %>% head()

pend_kap5$PSM0100_fct <- 
  factor(pend_kap5$PSM0100, 
         levels = 1:2, 
         labels = c("Nutzt soziale Netzwerke","Nutzt keine soziale Netzwerke"))

# Ergebnis ansehen:
pend_kap5 %>% select(contains("PSM0100")) %>% head()


# Factoren bearbeiten ------
#' Oder wir nutzen die Funktionen aus `{forcats}` zur recodierung eines `factors`.
#' `{forcats}` ist Teil des `{tidyverse}`.
#' Mit `fct_recode()` können wir die `levels` verändern:

levels(pend_kap5$PSM0100_fct)

pend_kap5$PSM0100_fct2 <- 
  fct_recode(pend_kap5$PSM0100_fct,
  `Ja, nutzt soziale Netzwerke` = "Nutzt soziale Netzwerke",   # bei Leerzeichen `` um die Wörter
  )

pend_kap5 %>% select(contains("PSM0100")) %>% head()
 
#' ## Übung


# Labels für Stata exportieren -----
#' Wenn wir aber beispielsweise einen Datensatz für Stata labeln wollen, hilft uns  `{labelled}`:
# fdz_install("labelled") #  falls noch nicht installiert
library(labelled)
pend_kap5$zpsex_num2 <- as.numeric(pend_kap5$zpsex)
attributes(pend_kap5$zpsex_num2)
val_labels(pend_kap5$zpsex_num2) <- c("Männer"=1,"Frauen"=2)
attributes(pend_kap5$zpsex_num2)
pend_kap5 %>% count(zpsex_num2)

# Datensatz als dta exportieren:
pend_kap5 %>%
 haven::write_dta(.,path = "./data/pend_kap5.dta")

#' ...in *Stata*:
# use "./data/pend_kap5.dta"
# tab zpsex_num2

