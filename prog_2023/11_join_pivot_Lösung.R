# ------------------- #
# Kapitel 11: Schleifen/Funktionen
# Lösung
# ------------------- #
library(tidyverse)



# Übung 1 ------
# Verknüpfen Sie die ausgewählten Beobachtungen des PASS CF mit der Haushalts-Information, in welcher Region (`region`) die Befragten wohnen.
# Mergen Sie dazu die `hnr` aus `p_register_cf_W13.dta` basiernd auf der `pnr` und mergen dann die `region` basierend auf der `hnr` und `welle`.

pend_ue11 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                             col_select = c("pnr","welle","hnr")) %>%
  slice(1:10)

pend_ue11

hh_dat <- haven::read_dta("./orig/HHENDDAT_cf_W13.dta", col_select = c("hnr","welle","region"))

pend_ue11 %>% left_join(hh_dat, by = c("hnr","welle"))

### Übung 2 ----------
pend_ue11b <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                              col_select = c("pnr","welle","famstand")) %>%
  slice(200:210) %>% 
  filter(welle %in% 2:3)


pend_ue11b

# Bringen Sie `pend_ue11b` in das wide shape:
pend_ue11b %>% pivot_wider(names_from = welle, values_from = famstand)

# Tipp: mit `,names_prefix = "wave"` in `pivot_wider()` können wir ein Präfix angeben:
pend_ue11b %>% 
  pivot_wider(names_from = welle, 
              values_from = famstand,
              names_prefix = "wave")



