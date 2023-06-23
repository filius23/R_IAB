 # Data Wrangling II {#wrang2}
library(tidyverse)

# Wir gehen nochmal zurück zum Uni-Datensatz vom Anfang:
dat1 <- data.frame(studs = c(19173,5333,15643), 
                   profs = c(322,67,210),
                   gegr  = c(1971,1830,1973),
                   prom_recht = rep(TRUE,3),
                   uni = c("Uni Bremen","Uni Vechta", "Uni Oldenburg"))
dat2 <- data.frame(studs = c(14954,47269 ,23659,9415 ,38079), 
                   profs = c(250,553,438 ,150,636),
                   prom_recht = c(FALSE,TRUE,TRUE,TRUE,FALSE),
                   gegr  = c(1971,1870,1457,1818,1995),
                   uni = c("FH Aachen","RWTH Aachen","Uni Freiburg","Uni Bonn","FH Bonn-Rhein-Sieg"))
dat1
dat2

# data.frames appenden ----- 
# Mit `bind_rows()` aus `{dplyr}` können wir die beiden `data.frame`s zusammensetzen:

dat3 <- bind_rows(dat1,dat2)
dat3

# Variablen erstellen ------
## 1 base R: `...$newvar <-` ------
dat3$studs_to_mean  <- dat3$studs - mean(dat3$studs)
dat3

# Mit `<- NULL` können Variablen auch gelöscht werden:
dat3$studs_to_mean  <-  NULL
dat3

## 2 {dplyr}: `mutate(neue_var= )`-------
# Eine alternative Möglichkeit, Variablen zu erstellen ist `mutate(neu_variable = )` aus `{dplyr}` (`{tidyverse}`):
dat3 %>% mutate(studs_to_mean = studs-mean(studs))

# Wir können auch mehrere Variablen innerhalb eines `mutate()`-Befehls erstellen:
dat3 %>% mutate(studs_to_mean = studs-mean(studs),
                profs_to_mean = profs-mean(profs))

# Oder Variablen können innerhalb von `mutate()` weiterverwendet werden:
dat3 %>% mutate(rel_to_mean = studs-mean(studs),
                above_mean = rel_to_mean > 0)
 
# Der Ausgangsdatensatz bleibt aber unverändert:
dat3
# Wenn wir die Ergebnisse behalten wollen, müssen wir das Ergebnis in einem Objekt ablegen:
dat4 <-
  dat3 %>% 
  mutate(rel_to_mean = studs-mean(studs),
         above_mean = rel_to_mean > 0
         ) 

dat4

# Dummyvariablen ------
# Wenn wir logische Variablen mit `as.numeric()` in numerische Variablen umformatieren erhalten wir eine Dummy-Codierung:

dat3 %>% 
  mutate(prom_dummy = as.numeric(prom_recht ),
         over10k    = as.numeric(studs > 10000) # Abgleich direkt in numeric
         )


# ### [Übung](#mutate1) {#ue06_1}

# Gruppierung mit `group_by()` oder `.by=` -------

dat5 <- dat3 %>% 
  select(-uni,-gegr) # nur dass alles zu sehen ist


# Wenn wir einen Datensatz mit `group_by()` entlang den Werten einer Variablen gruppieren, 
# dann werden alle weiteren `mutate()` Berechnungen nur innerhalb dieser Gruppen ausgeführt:
dat5 %>%
  mutate(m_studs = mean(studs),
         m_profs = mean(profs)) %>% 
  group_by(prom_recht) %>%
  mutate(m_studs2 = mean(studs),
         m_profs2 = mean(profs))

# Verwenden wir `group_by()`, können (sollten!) wir mit `ungroup()` die Gruppierung wieder aufheben, sobald wir sie nicht mehr benötigen:
dat5 %>%
  mutate(m_studs = mean(studs),
         m_profs = mean(profs)) %>%
  group_by(prom_recht) %>%
  mutate(m_studs2 = mean(studs)) %>%
  ungroup() %>%
  mutate(m_profs2 = mean(profs))

# Seit `{dplyr}`-Version 1.1.1 können wir direkt in `mutate()` mit dem Argument `.by=` eine Gruppierung angeben.
# Diese Gruppierung `.by=` gilt dabei nur für die unmittelbaren Berechnungen innerhalb `mutate()` - wir sparen uns das `ungroup()`.

dat5 %>%
  mutate(m_studs = mean(studs),
         m_profs = mean(profs)) %>% 
  mutate(m_studs2 = mean(studs),
         .by = prom_recht) %>% 
  mutate(m_profs2 = mean(profs))

# Mit `summarise()` statt `mutate()` erhalten wir eine Übersicht:
dat5 %>%
  summarise(m_studs = mean(studs),.by = prom_recht)


### [Übung](#grpue) {#ue06_2}


# `across()`: Mehrere Variablen bearbeiten ------

dat3 %>%
  summarise(studs = mean(studs),
            profs = mean(profs))

# Hier ist `across()` deutlich kürzer -
dat3 %>%
  summarise(across(.cols = c("studs","profs"),.fns = ~ mean(.x) ))

#  für die Variablenauswahl können wir die `?select_helpers` verwenden - z.B. `matches()`:
dat3 %>%
  summarise(across(.cols = contains("stu"),.fns = ~mean(.x)))
dat3 %>%
  summarise(across(.cols = matches("stu|profs"),.fns = ~mean(.x)))

# Natürlich ist das auch kombinierbar mit `group_by()` / `.by=`:
dat3 %>%
  summarise(across(.cols = matches("stu|profs"),.fns = ~mean(.x)), .by = prom_recht)


dat3 %>%
  group_by(prom_recht) %>%
  summarise(across(matches("studs|profs"), ~mean(.x)))

## mehrere Funktionen anwenden mit across ------
# Wir können auch mehrere Funktionen durchführen, dafür müssen wir sie in einer `list()` angeben:

dat3 %>%
  summarise(across(matches("studs|profs"), list(mean = ~mean(.x), standardabw = ~sd(.x)) ))

# Diese `list()`auch vorab ablegen und dann verwenden:

wert_liste <- list(mean = ~mean(.x), sd = ~sd(.x), max = ~max(.x,na.rm = T))

dat3 %>%
  group_by(prom_recht) %>%
  summarise(across(matches("studs|profs"), wert_liste))

## .names - bearbeitete Variablen umbenennen  -----
# Mit dem `.names()`-Argument können wir auch die Benennung der Spalten steuern. 
# `{.fn}` steht dabei als Platzhalter für die angewendete Funktion, `{.col}` für den Namen der bearbeiteten Variable.
dat3 %>%
  summarise(
    across(matches("studs|profs"), 
           list(mean = ~mean(.x), sd = ~sd(.x)),
           .names = "{.fn}_{.col}")
            )

# Alle gezeigten Funktionen funktionieren natürlich auch mit `mutate()`:
dat3 %>%
  mutate(across(matches("studs|profs"), ~mean(.x), .names = "m_{.col}"))

# [Mehr Beispiele in der Hilfe zu across](https://dplyr.tidyverse.org/reference/across.html)
## [Übung](#across_ue) {#ue06_3}

 
# Eigene Funktionen  ------
 
# Woher kommt aber die `~`[^tilde] in `across()`?
# Dazu sehen wir uns einmal die Grundlagen von Funktionen in R an. 

pend <- haven::read_dta("./orig/PENDDAT_cf_W13.dta")

sat_small <- 
  pend %>% 
    filter(welle == 1) %>% 
    select(matches("PEO0300(a|b|c)")) %>% 
    slice(12:16) %>% 
    haven::zap_labels() %>% haven::zap_label() # labels entfernen
sat_small

sat_small <- sat_small %>% mutate(across(.cols = everything(),.fns = ~as.numeric(.x)))
sat_small %>% 
  mutate(dmean_PEO0300a = PEO0300a - mean(PEO0300a,na.rm = T),
         dmean_PEO0300c = PEO0300c - mean(PEO0300c,na.rm = T))

# wiederholte, gleiche Berechnung --> function() definieren

# function() definieren
dtomean <- function(x){
  d_x <- x - mean(x,na.rm = T)
  return(d_x)
}

# geht genauso: UU statt x, außerdem können wir das return() weglassen - dann wird einfach das letzte Ergebnis ausgegeben
dtomean2 <- function(UU){   
  UU - mean(UU,na.rm = T)
}

variable1 <- c(1,6,3,7,8,1,5)
mean(variable1)
dtomean(variable1)
dtomean2(variable1)

# Wie können wir unsere Funktion `dtomean()` jetzt auf die Variablen aus unserem `sat_small` anwenden? 
# Grundsätzlich haben wir ganz [zu Beginn](#02_intro)  gesehen, dass ein `data.frame` lediglich zusammengefügte Sammlung von Vektoren (den Variablen) ist. 
# Dementsprechend können wir jetzt unsere `dtomean()` auf eine Variable (einen Vektor) anwenden, indem wir ihn mit `data.frame$variablename` aufrufen:

dtomean(sat_small$PEO0300a)

# Um unsere Funktion jetzt auf jede Variable eines `data.frame` anzuwenden, können wir `lapply()` verwenden - der Output ist dann eine Liste, deren Elemente nach den Variablennamen benannt werden:
lapply(sat_small,FUN = dtomean)
res <- lapply(sat_small,FUN = dtomean)
class(res)

# `map()` aus `{purrr}` ist eine Alternative zu `lapply`:
lapply(sat_small,FUN = dtomean)
sat_small %>% map(~dtomean(.x))

# Diese *formula syntax* Schreibweise findet sich dann auch in `across()` wieder - zusätzlich haben wir hier direkt über `.names =` die Möglichkeit, die Variablennamen für die Ergebnisse zu bearbeiten:

sat_small %>% 
  mutate(across(matches("PEO0300"),~dtomean(.x),.names = "dmean_{.col}"))

# `ifelse()` und `case_when()` ----------------
 

dat3
# `ifelse()` ist eine große Hilfe für alle Umcodierungen: wir formulieren darin eine Bedingung und wenn diese zutrifft wird der erste Wert eingesetzt, wenn nicht wird der zweite Wert eingesetzt. Hier fragen wir also ab, ob `studs-mean(studs)` größer `0` ist - dann wird `darüber` eingesetzt, ansonsten eine `darunter`:
dat3$studs[4] <- NA

dat3 %>% mutate(rel_to_mean = studs-mean(studs,na.rm=T),
                ab_mean_lab = ifelse(rel_to_mean > 0,"darüber","darunter")) %>% 
  tibble()

 
# `case_when()` (`{dplyr}`) erweitert dieses Prinzip, sodass wir mehr als zwei Optionen angeben können. 
# Die Syntax ist aber etwas anders: hier geben wir erst die Bedingung an, dann nach einer `~`[^tilde] die einzusetzenden Werte: 

dat3 %>% mutate(alter = case_when(gegr < 1500 ~ "sehr ALT",
                                  gegr < 1900 ~ "alt"))

 
# Mit `TRUE` können alle Fälle angesprochen werden, die bis dahin keiner Bedingung entsprochen haben:
dat3$gegr[4] <- NA
dat3 %>% mutate(alter = case_when(gegr < 1500 ~ "sehr alt",
                                  gegr < 1900 ~ "alt",
                                  TRUE ~ "relativ neu"))

# wie nur non-missing werte mit "relativ neu" ersetzen?
dat3 %>% mutate(alter = case_when(gegr < 1500 ~ "sehr alt",
                                  gegr < 1900 ~ "alt",
                                  !is.na(gegr) ~ "relativ neu"))

# Das muss sich nicht auf eine Variable beschränken:
dat3 %>% mutate(alter = case_when(gegr < 1500 & prom_recht == T ~ "sehr alte Uni",
                                  gegr < 1900 & prom_recht == T ~ "alte Uni",
                                  gegr > 1900 & prom_recht == T ~ "junge Uni",
                                  gegr < 1900 & prom_recht == F ~ "alte Hochschule",
                                  gegr > 1900 & prom_recht == F ~ "junge Hochschule"))


# ### [Übung](#ifcase) {#ue06_5}
 
# Variablen umbenennen ---------
 
# Um Variablen umzubenennen gibt es `rename(neuer_name = alter_name)`
sat_small %>% rename(neuername =PEO0300a)

# 
# Für fortgeschrittene Veränderungen empfiehlt sich ein Blick in `rename_with()`. Damit können wir [Regular Expressions](https://raw.githubusercontent.com/rstudio/cheatsheets/main/regex.pdf), bspw. aus [{stringr}](https://raw.githubusercontent.com/rstudio/cheatsheets/main/strings.pdf) verwenden. Hier nur ein Beispiel:
# 

sat_small %>% rename_with(~tolower(.))
sat_small %>% rename_with(~str_remove(.x,"PEO0300"))
sat_small %>% rename_with(~str_replace(.x,"PEO0300","Beruf_"))
