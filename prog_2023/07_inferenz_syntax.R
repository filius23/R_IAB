library(tidyverse)
pend_kap7 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                         col_select = c("pnr","welle","palter","zpsex","azges1","statakt")) %>% 
  mutate(across(everything(), ~ifelse(.x < 0,NA,.x)) )

#' Für die grundlegenden inferenzstatistischen Aufgaben helfen uns im wesentlichen 3 Funktionen:
#' 
#' + `t.test()` zur Berechnung von t-Tests
#' + `cor.test()` zur Berechnung von Korrelationen und weiterer Zusammenhangsmaße
#' + `{survey}` zur Gewichtung
#' 

# fdz_install("survey")  # Gewichtung (weiter unten)

# t-Tests -------

#' Eines der zentralen Werkzeuge der grundlegenden Inferenzstatistik ist der t-Test.
#' In R steht uns dafür `t.test()` zur Verfügung.
#' Mit der Option `mu =` können wir einen Wert für die $H_{A}$ angeben:

t.test(pend_kap7$azges1, mu = 35)  

#' Ein weiterer typischer Anwendungsfall für t-Tests ist der Gruppenvergleich, dazu geben wir in `t.test()` die zu testende Variable und und nach einer `~`[^tilde2] die Gruppenvariable an. 
#' Wir testen hier also auf Altersunterschiede zwischen Männern (`zpsex`=1, daher group1) und Frauen (`zpsex`=2, daher group2).

t.test(pend_kap7$azges1~pend_kap7$zpsex)

ttest <- t.test(pend_kap7$azges1~pend_kap7$zpsex)

View(ttest)

ttest$estimate

#' Standardmäßig bekommen wir einen beidseitigen Test (`"two.sided"`), wir können aber auch einen links- (`"less"`) oder rechtsseitigen (`"greater"`) Test anfordern [mehr dazu](#hypt):
t.test(pend_kap7$palter~pend_kap7$zpsex,alternative = "two.sided")
t.test(pend_kap7$palter~pend_kap7$zpsex,alternative = "less")
t.test(pend_kap7$palter~pend_kap7$zpsex,alternative = "greater")

#' Den Korrelationskoeffizienten können wir in R mit `cor.test()` berechnen:
cor.test(pend_kap7$palter,pend_kap7$azges1,method = "pearson")
#' Für den Spearman-Rangkorrelationskoeffizienten können wir `method = "spearman"` nutzen:
cor.test(pend_kap7$palter,pend_kap7$azges1,method = "spearman")

#' ### [Übung](#ttestue)

## ergebnisse speichern ----
rang_corr <- cor.test(pend_kap7$palter,pend_kap7$azges1,method = "spearman")
ttest <- t.test(pend_kap7$azges1~pend_kap7$zpsex)

## als RData speichern:
save(rang_corr,ttest,file = "./data/Korrelationen.RData")
rm(rang_corr,ttest)
ttest # weg

# wieder laden:
load(file = "./data/Korrelationen.RData")
ttest # wieder da als wäre er gerade ausgeführt worden


# Gewichtung  -------
# gewichte laden
wgt_df <- haven::read_dta("./orig/pweights_cf_W13.dta")
head(wgt_df)
# ranspielen/mergen --> mehr in Kap11
pend_kap7w <- pend_kap7 %>% left_join(wgt_df, by = join_by(pnr,welle))

#' Die einfachste Variante für eine Gewichtung ist die Option  `wt=` in `count()`:
pend_kap7w %>% 
  count(zpsex,statakt)
pend_kap7w %>% 
  count(zpsex,statakt,wt = wqp)


pend_kap7w %>% summarise(weighted_mean = weighted.mean(palter,na.rm = T,w = wqp),
                         mean = mean(palter,na.rm = T))

#' Zunächst verwenden wir `svydesign()`, um die Gewichtung festzulegen. 
fdz_install("survey")
library(survey)
pend_kap7_wgt <- svydesign(id      = ~pnr,
                           weights = ~wqp,
                           data    = pend_kap7w)

svymean(~palter, pend_kap7_wgt, na.rm = TRUE)
mean(pend_kap7$palter, na.rm = TRUE)
# "normaler" mean() funktioniert nur wenn man explizit die Variables anspricht:
mean(pend_kap7_wgt$variables$palter, na.rm = TRUE) 


#' Für Tabellen gibt es in `{survey}` auch eine Funktion: 
svytable(~zpsex+statakt,pend_kap7_wgt)
table(pend_kap7$zpsex,pend_kap7$statakt)

#' Für [Regressionsmodelle](#reg) gibt es bspw. `survey::svyglm()`

#' ### [Übung](#weight)
