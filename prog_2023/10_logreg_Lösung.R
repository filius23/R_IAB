# ------------------- #
# Kapitel 7: logistische Regression / AME
# Lösung
# ------------------- #
library(tidyverse)
library(marginaleffects)


## Daten einlesen ----------
pend10 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                          col_select = c("pnr","welle","zpsex","palter","PAS0400")) %>% 
  mutate(across(everything(),~ifelse(.x<0,NA,.x)),
         zpsex_fct = factor(zpsex,levels = 1:2, labels = c("Männer","Frauen")),
         search = 2- PAS0400) %>% 
  filter(!is.na(search))


pend10 %>% count(search,PAS0400)

## auszählen zur Kontrolle ----------
pend10 %>% count(search,PAS0400)

## Modell schätzen ----------
log_mod1 <- glm(search ~  palter,family = "binomial" ,pend10)
summary(log_mod1)

## AME berechnen ----------
library(marginaleffects)
ame_mod1 <- avg_slopes(log_mod1)

##AME anzeigen lassen ----------
ame_mod1


# Bonus adjusted predictions ----
# margins, at(....)
predictions(log_mod1, newdata = datagrid(palter  = 18:65)) %>% # vorhergesagte Werte
  data.frame() %>% 
  ggplot(aes(y = estimate , x = palter)) + 
  geom_errorbar(aes(ymin = conf.low, ymax= conf.high),width = .1,color ="lightskyblue4") + # konfidenzintervalle
  geom_point(color ="orange") + # punktschätzer
  theme_minimal()



## erweitertes Modell mit Geschlecht
log_mod2 <- glm(search ~  palter + zpsex_fct,family = "binomial" ,pend10)
summary(log_mod2)
avg_slopes(log_mod2)
ame_mod2 <- avg_slopes(log_mod2)



## Bonus: plot ------------
ame_mod2 %>% 
  data.frame() %>% 
  ggplot(aes(x = estimate, y = term)) + 
  geom_vline(aes(xintercept = 0 ), linetype = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax= conf.high), color = "orange",height = .1) + 
  geom_point(color = "lightskyblue4") +
  labs(x = "Average marginal effect")



# adjusted predictions ----
# margins, at(....)
predictions(log_mod2, newdata = datagrid(palter  = 18:65, zpsex_fct = c("Männer","Frauen"))) %>% # vorhergesagte Werte
  data.frame() %>% 
  ggplot(aes(y = estimate , x = palter,color = zpsex_fct)) + 
  geom_errorbar(aes(ymin = conf.low, ymax= conf.high),width = .1) + # konfidenzintervalle
  geom_point() + # punktschätzer
  theme_minimal()
