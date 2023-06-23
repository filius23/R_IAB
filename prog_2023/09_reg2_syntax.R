#' # Regressionsmodelle: Erweiterungen
library(tidyverse)
library(fixest)
library(marginaleffects)
library(modelsummary)

#' Nach den basics für Regressionsmodelle sehen wir uns in diesem Kapitel einige hilfreiche Erweiterungen von Regressionsmodellen in R an.
library(tidyverse)

dat1 <- data.frame(id   = 1:8,
                   var1 = c(2,1,2,5,7, 8, 9,5),
                   var2 = c(2,2,1,9,7, 4,25,3),
                   educ = c(3,1,2,2,1, 3, 2,-1),
                   gend = c(2,1,1,2,1,2,1,2),
                   x    = c(2,1,2,4,1,NA,NA,NA) )
dat1$ed_fct <- factor(dat1$educ, levels = 1:3,
                        labels = c("basic","medium","high"))
dat1

# Nur vollständige Zeilen behalten {#complcses} ----- 
#' Wenn wir die beiden Modelle `m1` und `m3` vergleichen, sehen wir unterschiedliche Fallzahlen:
m1 <- lm(var2~ var1, data = dat1)  
m4 <- lm(var2 ~ ed_fct  + var1, dat1)
modelsummary(list("m1"=m1,"m4"=m4),gof_omit = "IC|RM|Log|F")

dat1
#' Die Angabe für `ed_fct` fehlt in für `id` = 8.

## `complete.cases()`
dat1 %>% select(var1,var2) %>% complete.cases(.) 
dat1 %>% select(var1,var2,ed_fct) %>% complete.cases(.) 

dat1$compl_m1 <- dat1 %>% select(var1,var2) %>% complete.cases(.) 
dat1$compl_m4 <- dat1 %>% select(var1,var2,ed_fct) %>% complete.cases(.)
dat1

#' So sieht das dann im Datensatz aus:
dat1

## Fälle mit missings finden
dat1 %>% filter(compl_m1 == T & compl_m4 == F) 

## Modelle nur mit vollständigen Fällen berechnen

m1_m4vars <- lm(var2 ~ var1     , data = dat1 %>% filter(compl_m4 == T))

modelsummary(list("m1"=m1,"m1 mit m4vars"=m1_m4vars,"m4"=m4),gof_omit = "IC|RM|Log|F")
#' Jetzt haben wir also in `m1 mit m4vars` und `m4` die gleiche Fallzahl und können so die Ergebnisse direkt miteinander vergleichen.


# Interaktionen -----
dat1$g_fct <- factor(dat1$gend,levels = 1:2,
                     labels = c("women","men"))
m5 <- lm(var2 ~ var1 * g_fct, dat1)
summary(m5)

## Visualisierung mit ggeffects -----
fdz_install("ggeffects")
library(ggeffects)
ggpredict(m5, terms = c("var1","g_fct[women,men]")) %>% plot()
# oder nebeneinander:
ggpredict(m5, terms = c("var1","g_fct[women,men]")) %>% plot(facet=TRUE)

# anpassen:
ggpredict(m5, terms = c("var1","g_fct[women,men]")) %>% plot() + 
  scale_color_manual(values = c("orange","lightskyblue3"),breaks = c("women","men"),labels=c("Frauen","Männer")) +
  scale_fill_manual(values = c("orange","lightskyblue3"),breaks = c("women","men"),labels=c("Frauen","Männer")) +
  labs(title = "Vorhergesagte Werte für var2",
       color = "Gender",
       x = "Werte für var1",
       y = "Vorhergesagte Werte für var1")


# Quadratische Terme & Polynome ----
dat1 %>% filter(id != 7) %>% 
  ggplot(aes(x = var1, y = var2)) + 
  geom_smooth(method = "lm", color = "darkblue" ,formula = "y~x+I(x^2)", se=F, size = .65) +
  geom_point()


m6 <- lm(var2 ~ var1 + I(var1^2), dat1 %>% filter(id != 7))
summary(m6)
ggpredict(m6, terms = c("var1")) %>% plot()


# Gewichtetes Regressionsmodell ------
library(survey)
pend <- haven::read_dta("./orig/PENDDAT_cf_W13.dta") %>% filter(netges > 0 , palter > 0, azges1 > 0) %>% 
  mutate(zpsex_fct = factor(zpsex, levels = 1:2, labels = c("M","W")))
wgt_df <- haven::read_dta("./orig/pweights_cf_W13.dta")
pend_wgt <- pend %>% left_join(wgt_df, by = join_by(pnr,welle))


modx <- lm(netges ~ palter + I(palter^2),data=pend)

pend_weighted <- svydesign(id      = ~pnr,
                           weights = ~wqp,
                           data    = pend_wgt)

# family = gaussian() bekommen wir ein lineares Regressionsmodell, wie bei lm() - mit gewichtet
survey_modx <- svyglm(netges ~ palter + I(palter^2), 
                    family = gaussian(), data = etb18,design = pend_weighted)

modelsummary(list("lm()"=modx,"svyglm()"= survey_modx),gof_omit = "RM|IC|Log")

# "Robuste" Standardfehler ----
#' Wir können sog. *heteroskedasticity-consistent* (HC) "robuste" Standardfehler mit der `vcov`-Option `HC` in `modelsummary()` anfordern. 
mod1 <- lm(netges ~ palter + I(palter^2),data=pend) 

library(modelsummary)
modelsummary(list(mod1,mod1,mod1,mod1),vcov = c("classical","HC","HC2","stata"),gof_omit = "RM|IC|Log")

#' Für geclusterste SE geben wir `~clustervariable` an;
modelsummary(mod1, vcov = c("classical",~pnr), stars = T,gof_omit = "RM|IC|Log|F")

# Fixed effects Modelle mit `{fixest}`  -------
fdz_install("fixest")
library(fixest)
fe_mod1 <- feols(netges ~ palter + I(palter^2) | pnr, data = pend) # pnr als FE-Variable
fe_mod1

#' `{fixest}` clustert automatisch die Standardfehler entlang der FE-Variable (hier also `pnr`).
#' Wenn wir das mal nicht möchten, können wir mit der `se`-Option `= "standard"` ungeclusterte SE anfordern:
summary(fe_mod1)
summary(fe_mod1, se = 'standard')
summary(fe_mod1, cluster = ~pnr)

#' `{modelsummary}` zeigt die geclusterten SE:
modelsummary(fe_mod1,gof_omit = "R|IC|Log|F",stars = T)

# Mehrebenenmodelle mit `{lme4}` -------
library(lme4)
ml_m3 <- lmer(netges ~ palter + I(palter^2) + ( 1 | pnr), data=pend) # random intercept für pnr
modelsummary(list(ml_m3),gof_omit = "R|IC|Log|F",output = "flextable")

# Anhang: Predictions mit `marginaleffects` und "manuelle" Darstellung -----
#' Wir hatten im Kapitel [Interaktionen](#interaktionen) folgendes Modell geschätzt:

summary(m5)

#' Mit `predictions()` aus `{marginaleffects}` können wir basierend auf unserem Modell vorhergesagte Werte für bestimmte Werte berechnen.
#' Dazu geben wir die die gewünschten mit einem `expand.grid()` an.

# Kombinationen aller Werte erstellen
datagrid(var1 = 1:5, 
            g_fct =  c("women","men"),model = m5) # var2 auf mean

#' Diese Werte geben wir dann in `predictions()` als `newdata =` an:

library(marginaleffects)
p <- predictions(m5, 
                 newdata = datagrid(var1 = 1:5, 
                                    g_fct =  c("women","men")) ) # model = m5 nicht nötig weil schon angegeben
                 
head(data.frame(p))

#' Für den `ggplot()` verwenden wir dann `geom_line()` zusammen mit 
#' + `geom_errorbar()` für eine Darstellung mit Konfidenzintervallen als Error Bars 
#' + mit `geom_ribbon()` erhalten wir die Konfidenzintervalle als Konfidenzbänder (hier müssen wir mit `alpha = ` die Deckkraft der etwas heruntersetzen und die Farbe mit `fill=` angeben um den Bereich einzufärben). 
pred_plt <- 
    ggplot(data= data.frame(p),
           aes(x = var1, 
               y =  estimate, 
               ymin = conf.low,ymax = conf.high,
               color = g_fct)) + 
      geom_line() + 
      scale_color_manual(values = c("orange","lightskyblue3"),breaks = c("women","men"),labels=c("Frauen","Männer")) +
      labs(title = "Vorhergesagte Werte für var2",
           color = "Gender",
           x = "Werte für var1",
           y = "Vorhergesagte Werte für var1") +
      theme_minimal()

# Konfidenzintervalle
pred_plt + 
  geom_point(size = 2.75) +
  geom_errorbar(width = .1)

# mit Konfidenzbändern
pred_plt + 
  geom_ribbon(alpha = .1, color = NA,aes(fill = g_fct)) +
  scale_fill_manual(values = c("orange","lightskyblue3"),breaks = c("women","men"),labels=c("Frauen","Männer")) +
  labs(fill = "Gender")
