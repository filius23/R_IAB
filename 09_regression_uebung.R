# 09 Regression

if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4") 


library(tidyverse)
library(modelsummary)
library(ggeffects)

etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta") 

# Übung 1 -------------
etb_reg1 <- etb18 %>% filter(az <90, F518_SUF < 99990,intnr< 200000)

## Regressionsmodell ----

mod1 <-lm(F518_SUF ~ az, etb_reg1)
summary(mod1)

modelsummary(mod1)

## Visualisierung ----
ggplot(etb_reg1,aes(x= az,y = F518_SUF)) + 
  geom_point(fill = "navajowhite", shape = 21, color = "grey25") + 
  geom_smooth(method = "lm", se = F, color = "lightskyblue2") +
  geom_text(data = etb_reg1 %>% filter(F518_SUF>20000),
            aes(label = intnr, x= az - 10), color = "dodgerblue4") +
  theme_minimal()


# Übung 2 ---------

##  nur auf den Beobachtungen mit einem Monatseinkommen unter 20.000 EUR -------

mod2a <-lm(F518_SUF ~ az, etb_reg1 %>% filter(F518_SUF<20000))
summary(mod2a)


etb_reg1_u20k <- etb_reg1 %>% filter(F518_SUF<20000)
mod2b <- lm(F518_SUF ~ az, etb_reg1_u20k)
summary(mod2b)

#  Erstellen Sie eine Regressionstabelle, welche diese neue Modell `m3` neben das Modell `m2` aus Übung 1 stellt.
modelsummary(list(mod1,mod2a,mod2b))

#  Erstellen Sie auch eine grafische Gegenüberstellung der beiden Modelle.

ggplot(etb_reg1,aes(x= az,y = F518_SUF)) + 
  geom_smooth(method = "lm", se = F, color = "royalblue3") +
  geom_smooth(data = etb_reg1 %>% filter(F518_SUF<20000), 
    method = "lm", se = F, color = "midnightblue") +
  geom_point(fill = "navajowhite", shape = 21, color = "grey25") + 
  geom_text(data = etb_reg1 %>% filter(F518_SUF>20000),
            aes(label = intnr, x= az + 7), color = "dodgerblue4") +
  theme_minimal()

# oder:
ggplot(etb_reg1,aes(x= az,y = F518_SUF)) + 
  geom_smooth(method = "lm", se = F, color = "royalblue3") +
  geom_smooth(data = etb_reg1_u20k, method = "lm", se = F, color = "midnightblue") + ##einfach gefilterten Datensatz angeben
  geom_point(fill = "navajowhite", shape = 21, color = "grey25") + 
  geom_text(data = etb_reg1 %>% filter(F518_SUF>20000),
            aes(label = intnr, x= az + 7), color = "dodgerblue4") +
  theme_minimal()


# Übung 3 kat. UV --------

## m1202 als factor -----

etb_reg1$m1202_fct <-  factor(etb_reg1$m1202,levels = 1:4, labels = c("ohne","dual","Aufstieg","FH/Uni"))

mod3 <- lm(F518_SUF ~ m1202_fct, etb_reg1)
summary(mod3)

# Übung 4  --------

mod4 <- lm(F518_SUF ~ az + m1202_fct, etb_reg1)
modelsummary(list(mod1,mod4),stars = T)


# Übung 5 --------



# Übung 6 --------
# etb_reg1$az <- as.numeric(etb_reg1$az)
mod6 <- lm(F518_SUF ~ az + I(az^2),etb_reg1)
summary(mod6)
ggpredict(mod6, terms = c("az")) %>% plot()

etb_reg1$S1_fct <- factor(etb_reg1$S1,levels = 1:2,labels =c("m","w"))
etb_reg1$m1202_fct <-  factor(etb_reg1$m1202,levels = 1:4, labels = c("ohne","dual","Aufstieg","FH/Uni"))
mod6b <- lm(F518_SUF ~ m1202_fct*az + I(az^2) +m1202_fct ,etb_reg1)
summary(mod6b)
ggpredict(mod6b, terms = c("az","m1202_fct[ohne,dual,Aufstieg,FH/Uni]")) %>% plot()