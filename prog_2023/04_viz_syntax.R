
library(tidyverse)
library(haven)
pend <- read_dta("./orig/PENDDAT_cf_W13.dta")
pend$palter[pend$palter>100] <- NA
pend$casmin[pend$casmin<0] <- NA

library(haven)
pend %>%
  filter(welle==13) %>% 
  mutate(zpsex_fct = factor(zpsex, levels = 1:2, labels = c("Männer","Frauen")),
         palter = ifelse(palter < 0,NA,palter),
         azges1  = ifelse(azges1 < 0,NA,azges1)
         ) %>%
ggplot(aes(x = palter, y = azges1)) +
  geom_point(aes(color = zpsex_fct)) +
  # facet_grid(~zpsex_fct) +
  theme_minimal() +
  labs(color = "Geschlecht", y = "Arbeitszeit/Woche",
       x = "Alter") +
  scale_color_manual(values = c("lightskyblue4","navy"))

#' 
#' Datengrundlage für unsere Graphik ist die pend mit den Angaben zur Arbeitszeit sowie dem Geschlecht und Alter der Befragten:
#' Um die Grafik nicht zu groß zu machen, verwenden wir nur die Beobachtungen aus Welle 13:
pend_small <- pend %>% filter(welle==13)

pend_small %>% select(azges1,zpsex,palter) %>% head()

# missings mit NA überschreiben (wir sehen später noch in Kap6 wie das schneller geht)
pend_small$palter[pend_small$palter>100] <- NA 
pend_small$casmin[pend_small$casmin<0] <- NA
pend_small$PAS0100[pend_small$PAS0100<0] <- NA
pend_small$azges1[pend_small$azges1<0] <- NA


## `data =` -----
#' In `data = ` geben die den `data.frame` an, aus dem die darzustellenden Informationen kommen. Wir starten unseren ggplot also mit:

ggplot(data = pend_small)

#' ### `aes` 
#' Diese Werte wollen wir also in einem Scatterplot darstellen, sodass das Alter auf der x-Achse und auf der y-Achse die Wochenarbeitszeit abgetragen ist:
#| warning: false
ggplot(data = pend_small, aes(x = palter, y = azges1))

#' 
## `geom` ----
#' Wenn wir nur diese Angaben machen, bekommen wir lediglich ein leeres Koordinatensystem - warum? Weil wir noch nicht angegeben haben, welche *Form* der Darstellung wir gerne möchten. Dazu muss wir ein `geom_` angeben, für Säulendiagramme ist das `geom_col()`, diese hängen wir an den `ggplot`-Befehl mit `+` an:

ggplot(data = pend_small, aes(x = palter, y = azges1)) + 
  geom_point()

#' Mit `color =` können wir den Punkten auch eine andere Farbe geben:

ggplot(data = pend_small, aes(x = palter, y = azges1)) + 
  geom_point(color = "orange")

# Farbe nach Geschlecht ---------

#' Eine numerische Variable für `color =` ergibt einen Farbverlauf, eine `factor`/`character`-Variable ergibt eine diskrete Farbskala:
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.numeric(zpsex))) + 
  geom_point()
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.factor(zpsex))) + 
  geom_point()
ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.character(zpsex))) + 
  geom_point()

#' Außerdem können wir mit `scale_color_manual` selbst Farben angeben, 
#' eine Liste möglicher Farben findet sich [**hier**](http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf).

ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.factor(zpsex))) + 
  geom_point() + 
  scale_color_manual(values = c("orange","navy"))

# Beschriftungen -------
#' Wir können mit den Optionen `breaks` und `labels` zudem auch die Beschriftung der Legende bearbeiten.
#' Dazu geben wir zunächst in `breaks`  die Ausprägungen der Variable Geschlecht an und dann in der gleichen Reihenfolge die zu vergebenden Labels:

ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.factor(zpsex))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") )


pend_small$geschl <- as_factor(pend_small$zpsex)
ggplot(data = pend_small, 
       aes(x = palter, y = azges1, color = geschl)) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy") )

#' + `title`: Überschrift für die Graphik
#' + `subtitle`:  Unterzeile zur Überschrift
#' + `caption`: Anmerkung unterhalb der Graphik
#' + `x`: x-Achsenbeschriftung
#' + `y`: y-Achsenbeschriftung
#' + `fill`: Beschriftung für die Legende, wenn `fill` in `aes()` angegeben wurde
#' + `color`: Beschriftung für die Legende, wenn `color` in `aes()` angegeben wurde

ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.factor(zpsex))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) +
  labs(color = "Geschlecht", y = "Arbeitszeit/Woche",
       x = "Alter",
       title = "Arbeitszeit und Alter",
       subtitle = "Nach Geschlecht",
       caption = "Quelle: PASS CampusFile"
       ) 

# Grafiken speichern: `ggsave()` -----

plot_objekt1 <- 
  
  ggplot(data = pend_small, aes(x = palter, y = azges1, color = as.factor(zpsex))) + 
  geom_point() + 
  scale_color_manual(values = c("lightskyblue4","navy"),
                     breaks = c(1,2), labels = c("Männer", "Frauen") ) +
  labs(color = "Geschlecht", y = "Arbeitszeit/Woche",
       x = "Alter",
       title = "Arbeitszeit und Alter",
       subtitle = "Nach Geschlecht",
       caption = "Quelle: PASS CampusFile"
  ) 


ggsave(plot = plot_objekt1,
       filename = "./results/plot1.png",
       dpi = 800, # auflösung
       width = 25, height = 11, units = "cm" # falls angepasst werden soll
       )

# Verteilungen visualieren -----
## Boxplot -----

# vertikal
ggplot(data = pend_small, aes(y = palter)) + 
  geom_boxplot()

# horizontal
ggplot(data = pend_small, aes(x = palter)) + 
  geom_boxplot()


# ohne NAs -> führt neuerdings(?) zu Problemen?
ggplot(data = pend_small %>% filter(!is.na(azges1)), 
       aes(y = azges1)) + 
  geom_boxplot()


#' So können wir einen Boxplot erstellen, der die Werte für Männer und Frauen getrennt darstellt: 
#' --> x-achse angeben
ggplot(data = pend_small, 
       aes(y = palter, x = as_factor(zpsex))) + 
  geom_boxplot()

# verschiedene Farben angeben:
ggplot(data = pend_small, 
       aes(y = palter, x = as_factor(zpsex),
           color = as_factor(zpsex))) + 
  geom_boxplot()


## Histogram  ----
ggplot(data = pend_small, aes(x = palter)) + 
  geom_histogram()  

#' Wenn wir hier die Farbe ändern möchten, dann ist `fill =` anstelle von `color =` die richtige Option:
ggplot(data = pend_small, aes(x = palter)) + 
  geom_histogram(fill = "sienna1")  

## nach geschlecht ---
ggplot(data = pend_small, 
       aes(x = palter, fill = factor(zpsex))) + 
  geom_histogram() 

### nebeneinander -----
ggplot(data = pend_small, 
       aes(x = palter, fill = factor(zpsex))) + 
  geom_histogram(position = position_dodge()) 

### hintereinander und transparent ----
# .--> 2 geoms anlegen und mit alpha = die Deckkraft heruntersetzen
ggplot() + 
  geom_histogram(data = pend_small %>% filter(zpsex == 1), 
                 aes(x = palter, fill = factor(zpsex)), alpha = .5,
                 position = position_dodge()) +
  geom_histogram(data = pend_small %>% filter(zpsex == 2), 
                 aes(x = palter, fill = factor(zpsex)), alpha = .5,
                 position = position_dodge()) 

#' Auch hier funktionieren natürlich wieder die `scale_...manual` Befehle, allerdings hier als `scale_fill_manual` anstelle `scale_color_manual` von oben:
ggplot(data = pend_small, aes(x = palter, fill = factor(zpsex))) + 
  geom_histogram(position = position_dodge()) +
  scale_fill_manual(values = c("sienna1","dodgerblue4"),
                    breaks = 1:2, labels = c("Männer","Frauen")) +
  labs(fill = "Geschlecht")

#' ### [Übung](#pltue2) {#ue4_02}


# Kategoriale Merkmale ----

## Säulendiagramm ----
pend_small %>% 
  filter(!is.na(PD0400),PD0400 > 0) %>% 
  ggplot(data = ., aes(x = as_factor(PD0400))) +
  geom_bar() 

# auch hier können wir nach Geschlecht splitten
pend_small %>% 
  filter(!is.na(PD0400),PD0400 > 0) %>% 
  ggplot(data = ., aes(x = as_factor(PD0400), fill = as_factor(zpsex),
                       y = after_stat(count) # after_stat(count) ---> Häufigkeitsauszählung, muss aebr nicht explizit angegeben werden weil das der default ist
                       )) +
  geom_bar(position=position_dodge()) 


#' Wie kommen wir jetzt an die relativen Häufigkeiten? Wir passen unsere `aes` auf `y = after_stat(count)/sum(after_stat(count)) ` an.
pend_small %>% 
  filter(!is.na(PD0400), PD0400 > 0 ) %>% 
  ggplot(data = ., aes(x = as_factor(PD0400), fill = factor(zpsex),
                       y = after_stat(count)/sum(after_stat(count)) )) +
  geom_bar(position=position_dodge()) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) 



#' Um jetzt ein Balken- statt einem Säulendiagramm zu erhalten, tauschen wir einfach `x` und `y` sowie die Prozentbeschriftung auf `scale_x_continuous`:
pend_small %>% 
  filter(!is.na(PD0400), PD0400 > 0) %>% 
  ggplot(data = ., aes(y = as_factor(PD0400), fill = factor(zpsex),
                       x = after_stat(count)/sum(after_stat(count)) )) +
  geom_bar(position=position_dodge()) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) 

#' Auch diese Grafiken können wir dann wieder mit `scale_...` anpassen und mit `labs()` ausführlich labeln - alle Optionen sind konsistent über alle Darstellungsformen hinweg.
#' Außerdem können wir die Kategorien mit `breaks =` und `labels =` auch selbst labeln, wenn uns die definierten Labels nicht gefallen:

pend_small %>% 
  filter(!is.na(PD0400)) %>% 
  ggplot(data = ., aes(y = as_factor(PD0400), fill = factor(zpsex),
                       x = (..count..)/sum(..count..) )) +
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values = c("navajowhite","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Religiösität nach Geschlecht",
       subtitle = "Relative Häufigkeiten",
       caption = "Quelle: PASS-CF 0619",
       y = "Religiösität",
       x = "Relative Häufigkeit",
       fill = "Geschlecht" ) 


pend_small %>% 
  filter(!is.na(PD0400),PD0400>0) %>% 
  ggplot(data = ., aes(y = PD0400, fill = factor(zpsex),
                       x = (..count..)/sum(..count..) )) +
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values = c("navajowhite","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(breaks = 1:4, 
                     labels = c("Überhaupt nicht",
                                "Eher nicht",
                                "Eher schon",
                                "Sehr")) +
  labs(title = "Religiösität nach Geschlecht",
       subtitle = "Relative Häufigkeiten",
       caption = "Quelle: PASS-CF 0619",
       y = "Religiösität",
       x = "Relative Häufigkeit",
       fill = "Geschlecht" ) 
