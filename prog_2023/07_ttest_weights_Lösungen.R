# ------------------- #
# Kapitel 7: t-Test, Gewichtung 
# Lösung
# ------------------- #
library(tidyverse)
pend_ue7 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                            col_select = c("pnr","welle","zpsex","palter","netges")) %>% 
  mutate(across(everything(), ~ifelse(.x < 0,NA,.x))) # missings  mit NA überschreiben

# Übung 1 ---------
# Unterschied in der Arbeitszeit (`az`) zwischen Männern und Frauen besteht (`S1`)
t.test(pend_ue7$palter~pend_ue7$zpsex,alternative = "two.sided")

test1 <-  t.test(pend_ue7$palter~pend_ue7$zpsex,alternative = "two.sided")
View(test1)

cor.test(pend_ue7$palter,pend_ue7$netges,method = "pearson")


# Übung 2 ----------
pend_ue7 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                          col_select = c("pnr","welle","zpsex","palter","netges")) %>% 
  mutate(across(everything(), ~ifelse(.x < 0,NA,.x))) # missings  mit NA überschreiben
wgt_df <- haven::read_dta("./orig/pweights_cf_W13.dta")
head(wgt_df)

pend_ue7 <- pend_ue7 %>% left_join(wgt_df, by = join_by(pnr,welle))

library(survey)
pend_kap7_wgt <- svydesign(id      = ~pnr,
                           weights = ~wqp,
                           data    = pend_ue7)

svymean(~netges , pend_kap7_wgt, na.rm = TRUE)
mean(pend_ue7$netges , na.rm = TRUE) # zum Vergleich
mean(pend_kap7_wgt$variables$netges , na.rm = TRUE) # zum Vergleich aus dem pend_kap7_wgt objekt

# Bonus: Kreuztabelle mit Gewichtung
svytable(~welle+zpsex,pend_kap7_wgt) # syntax wie xtabs()
table(pend_ue7$welle,pend_ue7$zpsex) # zum Vergleich
