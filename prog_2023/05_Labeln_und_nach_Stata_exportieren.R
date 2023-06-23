# Wie kann ich labels bearbeiten und in einem Stata-Datensatz ablegen?
library(tidyverse)
library(haven)

# Daten importieren und labels löschen
pend_bsp5 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                             col_select = c("pnr","welle", "zpsex", "PSM0100","azges1","palter")) %>% 
  haven::zap_labels() %>% 
  haven::zap_label()
# Ergebnis: "roher" Datensatz mit einfachen Zahlenwerten:
pend_bsp5
attributes(pend_bsp5$PSM0100) # keine labels mehr

# dazu kann ich aber alle Labels (Variable- und Value-Labels) laden, in dem ich 
# mit n_max nur die erste Zeile einlese - das geht schneller
lab_df <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                                  col_select = c("pnr","welle", "zpsex", "PSM0100","azges1","palter"),
                                  n_max = 1) 

# hier sind alle labels drin - auch wenn ja nur ein Wert geladen ist:
attributes(lab_df$PSM0100)$labels

# diese Attribute können wir übertagen:
attributes(pend_bsp5$PSM0100)$labels <- attributes(lab_df$PSM0100)$labels

# alternativ kann ich auch variable labels selbst vergeben
attributes(pend_bsp5$PSM0100)$label <- "Soziale Netzwerke"

# Value labels mit val_labels aus labelled vergeben:
fdz_install("labelled")
library(labelled)

val_labels(lab_df$zpsex_num2) <- c("Männer"=1,"Frauen"=2)
lab_df %>% count(zpsex_num2)

write_dta(pend_bsp5 %>% slice(1:5),path = "./data/Label_test.dta")

## ...in stata erscheinen die Variable- und Value-Labels 
