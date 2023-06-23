# ------------------- #
# Kapitel 8: Regression
# Lösung
# ------------------- #
library(tidyverse)

# Verwenden Sie folgenden Subdatensatz der ETB2018:
pend_ue08 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta") %>% 
  filter(welle == 13, netges > 0, azges1 > 0, schul2 > 0, palter > 0)

# Übung 1 -----
# Erstellen Sie ein Objekt mod1 mit einem linearen Regressionsmodell (lm) 
# mit netges (Monatsbrutto in EUR) als abhängiger und azges1 (Arbeitszeit in Stunden) als unabhängiger Variable! (siehe hier)
lm(netges ~ azges1, pend_ue08)
m2 <- lm(netges ~ azges1, pend_ue08)
summary(m2)

# Betrachten Sie Ergebnisse mod1 - was können Sie zum Zusammenhang zwischen netges und azges1 erkennen?

# Visualisieren Sie Ihr Regressionsmodell mit {ggplot2}.
ggplot(data = pend_ue08,aes(x= azges1, y = netges)) +
  geom_point()
ggplot(data = pend_ue08,aes(x= azges1, y = netges)) +
  geom_point() +
  geom_smooth(method = "lm")

# formula explizit angeben:
ggplot(data = pend_ue08,aes(x= azges1, y = netges)) +
  geom_point() +
  geom_smooth(method = "lm",formula = "y~x")

ggplot(data = pend_ue08,aes(x= azges1, y = netges)) +
  geom_point() +
  geom_smooth(method = "lm",formula = "y~x+I(x^2)")
# Sehen Sie Ausreißer im Scatterplot? Markieren Sie diese mit Hilfe der Variable pnr und geom_text().


ggplot(data = pend_ue08,aes(x= azges1, y = netges)) +
  geom_point() +
  geom_smooth(method = "lm",formula = "y~x") + 
  geom_text(data = . %>% filter(netges > 20000),
            aes(label = pnr, x= azges1 + 10 ),
            color = "orange")


ggplot(data = pend_ue08,aes(x= azges1, y = netges)) +
  geom_point() +
  geom_smooth(method = "lm",formula = "y~x") + 
  geom_text(data = . %>% filter(netges > 20000),
            aes(label = pnr, x = azges1 -8),
            color = "orange")

# Übung 2 ----
# ausgangsmodell
m2 <- lm(netges ~ azges1, pend_ue08)

# zwei möglichkeiten, nur fälle unter 20.000EUR zu analysieren:
##subdatensatz
pend_ue08_u20k <- pend_ue08 %>% filter(netges < 20000)
m3a <- lm(netges ~ azges1, pend_ue08_u20k)
## filter im data-Argument
m3b <- lm(netges ~ azges1, pend_ue08 %>% filter(netges < 20000))

library(modelsummary)
modelsummary(list("Modell mit allen"=m2,
                  "Modell u20k ver1"=m3a,
                  "Modell u20k ver2"=m3b),
             stars = T,gof_omit = "IC|RM|Log")

ggplot(data = pend_ue08,aes(x= azges1, y = netges)) +
  geom_point() +
  geom_smooth(method = "lm",formula = "y~x") + 
  geom_smooth(data= . %>% filter(netges < 20000),
              method = "lm", color = "orange", fill = "coral")

# reinzoomen in unteren bereich:
ggplot(data = pend_ue08,aes(x= azges1, y = netges)) +
  geom_point() +
  geom_smooth(method = "lm",formula = "y~x") + 
  geom_smooth(data= . %>% filter(netges < 20000),
              method = "lm", color = "orange", fill = "coral") +
  coord_cartesian(ylim = c(0,10000))

## nicht das gleiche: hier werden Werte außerhalb ausgeschlossen/gefiltert
ggplot(data = pend_ue08,aes(x= azges1, y = netges)) +
  geom_point() +
  geom_smooth(method = "lm",formula = "y~x") + 
  geom_smooth(data= . %>% filter(netges < 20000),
              method = "lm", color = "orange", fill = "coral") +
  ylim(0,10000)

# Übung 3 ------

## kategoriale unabhängige Variable ------
pend_ue08$schul2_fct <-  
  factor(pend_ue08$schul2,levels = 2:7, 
         labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi"))

m4 <- lm(netges ~ schul2_fct,data = pend_ue08)
summary(m4)

## Aufstiegsfort. als Referenz -----
pend_ue08$schul2_fct <-  relevel(pend_ue08$schul2_fct,ref = "Hauptschule")

m4.2 <- lm(netges ~ schul2_fct,data = pend_ue08)
summary(m4.2)


# Modellsummary
modelsummary(list("m4"=m4),output = "markdown")
modelsummary(list("m4"=m4,"m4.2"=m4.2),output = "markdown")

## mehre UVs -----
pend_ue08$schul2_fct <-  relevel(pend_ue08$schul2_fct,ref = "ohne")
m5 <- lm(netges ~ schul2_fct + azges1,data = pend_ue08)
summary(m5)
modelsummary(list("m4"=m4,"m5"=m5),output = "markdown")

modelplot(m4) + theme_bw()
modelplot(list(m4,m5))
modelplot(list("model1" = m4, "model2"= m5),
          coef_map = c("schul2_fctAbi"="Abi",
                       "schul2_fctFOS/BOS"="FOS/BOS"))

modelplot(list(m4,m5)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey40") +  # 0-Linie einfügen
  scale_color_manual(values = c("orange","navy")) +
  theme_grey(base_size = 15,base_family = "mono") 


## beide Modelle nebeneinander ---
modelplot(list(m4,m5)) + facet_grid(~model)

modelplot(list(m4,m5)) + facet_grid(~model) +
  scale_color_manual(values = c("orange","navy")) 


## facet illustration 
pend_w1112 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta") %>% 
  filter(welle %in% 11:12, netges > 0, azges1 > 0, schul2 > 0, palter > 0)
pend_w1112 %>% 
  ggplot(aes(x=netges, y = azges1 )) +
  geom_point() +
  facet_grid(~welle)




## individuell mit tidy()
library(broom) 
tidy(m5, conf.int = TRUE) # ergebnis ist ein data.frame


broom::tidy(m5, conf.int = TRUE) # ergebnis ist ein data.frame

tidy(m5, conf.int = TRUE) %>% 
  mutate(term = str_replace(term, "schul2_fct", "Education: "))


tidy(m5, conf.int = TRUE) %>% 
  mutate(term = str_replace(term, "schul2_fct", "Education: ")) %>% 
  ggplot(aes(y = term, x = estimate)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "navy") +
  geom_errorbarh(aes(xmin = conf.low, xmax  = conf.high), height = .1) + 
  geom_point(color = "navy", shape = 18,size = 8) +
  theme_minimal(base_size = 16)


