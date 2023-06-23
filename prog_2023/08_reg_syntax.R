#' # Regressionsmodelle {#reg}
library(tidyverse)

dat1 <- data.frame(id   = 1:8,
                   var1 = c(2,1,2,5,7, 8, 9,5),
                   var2 = c(2,2,1,9,7, 4,25,3),
                   educ = c(3,1,2,2,1, 3, 2,-1),
                   gend = c(2,1,1,2,1,2,1,2),
                   x    = c(2,1,2,4,1,NA,NA,NA) )
dat1

# Regressionsmodelle mit `lm()` -----
lm(var2 ~ var1, data = dat1)
m1 <- lm(var2~ var1, data = dat1)  

#' Mit `summary()` bekommen wir dann eine Regressionstabelle:
summary(m1)

#' `m1` enthält alle Informationen zum Modell, besonders hilfreich ist `$coefficients`:
m1$coefficients
summary(m1)$coefficients

#' Wir können uns die einzelnen Werte mit `View(m1)` ansehen: 
View(m1) 

#' Bspw. finden sich unter `fitted.values` die vorhergesagten Werte für jeden Fall.

# Regressionsgerade und Daten visualisieren  -----
#' Mit `geom_smooth(method = "lm")` können wir Regressionsgeraden auch in `{ggplot2}` darstellen:
ggplot(dat1, aes(x = var1, y = var2)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm")  

ggplot(dat1, aes(x = var1, y = var2)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm")  +
  geom_text(data = . %>% filter(var2 > 20), 
            aes(x = var1-.3, label = id), color = "sienna1",size = 6)

#' ### [Übung](#reg1) 

# Modelle nur für manche Fälle berechnen  -----

#' Wenn wir jetzt das Modell nochmal berechnen wollen, haben wir zwei Möglichkeiten:
## Neuen data.frame erstellen----
dat1_u20 <- dat1 %>% filter(var2<20)
m2a <- lm(var2~ var1, data = dat1_u20)
summary(m2a)

## Direkt in `lm()` filtern  -----
m2b <- lm(var2~ var1, data = dat1 %>% filter(var2<20))
summary(m2b)


# ...übrigens: wir können auch in `geom_smooth()` filtern: -->
 ggplot(dat1, aes(x = var1, y = var2)) + 
   geom_smooth(method = "lm", color = "darkblue" , fill = "lightskyblue", size = .65)  +
   geom_smooth(data = dat1 %>% filter(var2<20),
     method = "lm", color = "sienna1" , fill = "sienna2", size = .65)  +
   geom_point(size = 2) 


# Regressionstabellen -----
fdz_install("modelsummary")
library(modelsummary)
modelsummary(list(m1,m2a,m2b))

#' + mit `stars = T` können wir uns die Signifikanz mit den gebräuchlichen Sternchen-Codes anzeigen lassen (`*`: p < .05 usw.)
#' + mit `gof_omit = "IC|RM|Log"` können wir die Goodness of Fit Statistiken ausblenden die `IC`, `RM` oder `Log` im Namen haben (also `AIC`, `BIC`, `RMSE` und die `LogLikelihood`)
#' + mit `"Name" =` in `list()` können wir Namen angeben:

modelsummary(list("m1"=m1,"m2a"=m2a,"m2b"=m2b),stars = T,gof_omit = "IC|RM|Log")


#' ### [Übung](#reg2)


# Kategoriale unabhängige Variablen -----
dat1
m3 <- lm(var2~factor(educ), dat1)
summary(m3)

#' Noch schöner ist das aber natürlich, wenn wir `educ` vorher labeln:
dat1$ed_fct <- factor(dat1$educ, 
                      levels = 1:3,
                      labels = c("basic","medium","high"))
dat1

#' Dann verwenden den `factor` im Regressionsbefehl:

m3 <- lm(var2 ~ ed_fct, dat1)
summary(m3)
 

dat1$ed_fct <- relevel(dat1$ed_fct,ref = "medium")
levels(dat1$ed_fct)
m3b <- lm(var2 ~ ed_fct, dat1)
summary(m3b)

#' ### [Übung](#reg3)

# Mehre unabhängige Variablen -----
 

m4 <- lm(var2 ~ ed_fct  + var1, dat1)
summary(m4)


# Koeffizientenplots {#modelplot} -----

#' Neben Regressionstabellen stellt [`{modelsummary}`](https://vincentarelbundock.github.io/modelsummary/articles/modelplot.html) auch die Funktion `modelplot()` zur Verfügung, mit denen einfach ein Koeffizientenplot aus einem oder mehreren Modellen erstellt werden kann:
modelplot(m4)

modelplot(list("Modell 1"=m1,
               "Modell 4"=m4))

#' Mit `coef_map` können wir Labels für die Koeffizienten vergeben (`(Intercept)` bekommt keinen Namen und wird daher weggelassen:
modelplot(list("Modell 1"=m1,
               "Modell 4"=m4),
          coef_map = c("var1" = "Name für var1",
                       "ed_fcthigh"  = "Höhere Bildung",
                       "ed_fctbasic" = "Grundlegende Bildung"
                          ))

#' Außerdem können wir mit den üblichen `{ggplot2}`-Befehlen die Grafik weiter anpassen:
modelplot(list("Modell 1"=m1,
               "Modell 4"=m4),
          coef_map = c("var1" = "Name für var1",
                       "ed_fcthigh"  = "Höhere Bildung",
                       "ed_fctbasic" = "Grundlegende\nBildung")) + # \n fügt einen Zeilenumbruch ein
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey40") +  # 0-Linie einfügen
  scale_color_manual(values = c("orange","navy")) +
  theme_minimal(base_size = 15,base_family = "mono") 

#' ### [Übung](#reg4)



# Koeffizientenplots selber bauen mit broom -------
#' `modelplot()` bietet eine schnelle Art, Koeffizientenplots zu erstellen, allerdings verwende ich häufig [`{broom}` ](https://broom.tidyverse.org/).
#' Mit `broom::tidy(..., conf.int = TRUE)` bekommen wir einen `data.frame` mit den Ergebnissen des Regressionsmodells, die wir bspw. in einem `{ggplot2}` weiterverarbeiten können - wenn uns die Standardlösung von [`modelplot()`](09_reg.qmd#modelplot) nicht weiterhilft/gefällt:

library(broom) ## schon geladen als Teil des tidyverse
tidy(m3, conf.int = TRUE)

tidy(m3, conf.int = TRUE) %>% 
  mutate(term = str_replace(term, "ed_fct", "Education: ")) %>% 
  ggplot(aes(y = term, x = estimate)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "navy") +
  geom_errorbarh(aes(xmin = conf.low, xmax  = conf.high), height = .1) + 
  geom_point(color = "orange", shape = 18,size = 7) +
  theme_minimal(base_size = 16)

#' 
