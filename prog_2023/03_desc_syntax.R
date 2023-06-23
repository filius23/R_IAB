#' # Einen Überblick erhalten {#tab}
#' Nachdem wir Datensätze importiert haben, wollen wir nun einen Überblick erhalten. Jede statistische Auswertung startet mit einer Beschreibung der Variablen. In dieser Session werden wir sehen, wie wir uns mit Tabellen einen Überblick über die Informationen in einem Datensatz verschaffen können. Wir werden auch in dieser Session mit dem ETB2018 arbeiten. Wir starten also mit dem Einlesen der Daten:

library(haven) # datenimport für stata-datensätze
library(tidyverse) # tidyverse
pend <- read_dta("./orig/PENDDAT_cf_W13.dta")

# Häufigkeitsauszählungen --------
table(pend$statakt)

#' Mit `as_factor()` aus dem Paket `{haven}` können wir die Labels aus dem Datensatz abrufen und die numerischen Werte mit den Labels überschreiben. 
#' Der `table()` zeigt dann die Labels als Beschriftungen an:
table(as_factor(pend$statakt))

t1 <- table(pend$statakt)
t2 <- table(as_factor(pend$statakt))

# alternative:
pend %>% count(statakt)

#' Wir können auch Tabellen unter einem frei wählbaren Namen ablegen und später wieder aufrufen:
t1 <- table(pend$statakt)
t2 <- pend %>% count(statakt)

#' Wir sehen hier, dass die Tabelle mit `table()` eine neue Objektform ist, ein table. Mit `count()` wird hingegen ein `data.frame` erstellt.

class(t1)
class(t2)

# Missings in R: `NA` {#NA03} ------------
#' Um die Werte wie `-5` auch in R als fehlende Angabe zu kennzeichnen, müssen wir sie in `pend` auf `NA` setzen. 
pend$statakt[pend$statakt == -5] # nur statakt = -5 aufrufen
pend$statakt[pend$statakt == -5]  <- NA # überschreiben

#' `NA` ist in der R der Code für fehlende Angaben, sie werden dann in `table()` nicht aufgeführt:
table(pend$statakt)
table(pend$statakt,exclude = NULL)

pend %>% count(statakt)
pend %>% count(statakt) %>% filter(!is.na(statakt))

#' Wir können aber mit der Option `exclude = NULL` die Auszählung von `NA` explizit anfordern:
table(pend$statakt,exclude = NULL)

#' Allerdings haben wir ja jetzt noch nicht alle negativen Werte überschrieben, die `-10` und `-9` fehlen noch:
pend$statakt[pend$statakt %in% c(-9,-10)]  <- NA # -9 & -10 auswählen und überschreiben
pend$statakt[pend$statakt < 0 ]  <- NA # alternative: negative Werte auswählen und überschreiben

#' Damit sind wir für `statakt` am Ziel:
table(pend$statakt)
table(pend$statakt,exclude = NULL)

#' In `count()` wird `NA` auch mit ausgezählt:
pend %>% count(statakt)

#' Möchten wir die NA ausblenden, nehmen wir wieder `filter()` zu Hilfe - mit `is.na()` können wir `NA` identifizieren
#' Durch Voranstellen von `!` können wir damit anfordern, dass alle nicht-`NA`-Werte mit `TRUE` behalten werden:
pend %>% filter(!is.na(statakt)) %>% count(statakt)

## [Übung](#descue1) {#ue3_1}

# Andere Tabellenwerte -----
#' Mit Hilfe weiterer Funktionen können wir die Häufigkeitstabellen jeweils anpassen:
#' + `prop.table()`: relative Werte/Anteile
prop.table(table(pend$statakt)) 

table(pend$statakt) %>% prop.table(.) 

#' + `cumsum()`: kumulierte Werte
table(pend$statakt) %>% cumsum(.)

#' + `prop.table()` mit `cumsum()`: kumulierte relative Häufigkeiten
table(pend$statakt) %>% prop.table() %>% cumsum()

# Kontingenztabellen -----
table(pend$zpsex, pend$statakt)
#' Mit `addmargins()` können wir die Tabelle um die Summenwerte erweitern:
table(pend$zpsex, pend$statakt) %>% addmargins()

#' Möchten wir jetzt die relativen Häufigkeiten, dann wenden wir wieder `prop.table()` an:
table(pend$zpsex, pend$statakt) %>% prop.table()
table(pend$zpsex, pend$statakt) %>% prop.table() %>% addmargins()
#' Für Zeilenprozente benötigen wir die zusätzliche Option `margin = 1`:
table(pend$zpsex, pend$statakt) %>% prop.table(margin = 1)

#' Für Spaltenprozente dann `margin = 2`:
table(pend$zpsex, pend$statakt) %>% prop.table(margin = 2)

#' Übrigens funktioniert auch hier `addmargins()`:
table(pend$zpsex, pend$statakt) %>% prop.table(margin = 2) %>% addmargins()

#' Für eine Kontingenztabelle mit `count()` geben wir einfach die Variablen in `count()` an. Das Ergebnis wird immer im "long shape" Format ausgegeben:
pend %>% count(zpsex,statakt)


#' tab im stata-style -----
#' selbstgeschriebene Funktion, die tab aus Stata imitiert
#' mehr in Tag03 recapt.R
tab <- function(x){
  tabx <- table(x) %>% data.frame()
  if(!is.null(attributes(x)$labels))  tabx <- table(as_factor(x)) %>% data.frame()
  tabx$percent <- prop.table(tabx$Freq)
  tabx$cum <- cumsum(tabx$percent)
  return(tabx)
}

tab(pend_recap$zpsex)
tab(pend_recap$PSM0100)
tab(pend_recap$PD0400_fct2)



## [Übung](#descue2) {#ue3_2} -----


#  Lage- & Konzentrationsmaße  -----

summary(pend$netges)
attributes(pend$netges)$labels

#' Um aussagekräftige Werte zu bekommen, müssen wir negative Werte mit `NA` überschreiben:
pend$netges[pend$netges < 0 ] <- NA # missings überschreiben
summary(pend$netges)

#' Wir können aber auch bestimmte Kennzahlen anfordern sehen uns die Bruttoverdienste der Befragten zu beschreiben:
#' + Minimum und Maximum: `min()`, `max()`
#' + arithm. Mittel: `mean()`
#' + Median: `median()`
#' + Quantile: `quantile()`
#' + Varianz: `var()`
#' + Standardabweichung: `sd()`
#' + Gini-Koeffizient: `Gini` aus dem Paket `{ineq}`

#' Wenn eine Variable `NA` enthält, müssen diese explizit ignoriert werden - ansonsten wird nur `NA` ausgegeben:
mean(pend$netges)

#' Deshalb müssen wir die Option `na.rm = T` angeben:
mean(pend$netges,na.rm = T)

#' Ein Quantil einer Verteilung trennt die Daten so in zwei Teile, dass `x`\% der Daten darunter und 100-`x`\% darüber liegen. Mit `quantile()`wir durch Angabe in der Option `probs =` beliebige Quantilgrenzen anfordern, zB. für die 40%-Quantilgrenze:  
quantile(pend$netges,probs = .4, na.rm = T)

## Kennzahlentabelle mit `summarise` ----
#' 
#' Mit Hilfe von `summarise()` aus `{dplyr}` können wir ein eigenes `summary()` bauen:
#' 

pend %>% summarise(Minimum = min(netges,na.rm = T),
                    Median = median(netges,na.rm = T),
                    Mittelwert = mean(netges,na.rm = T),
                    Maximum = max(netges,na.rm = T))

#' 
#' 
#' Der Vorteil des Ganzen wird im nächsten Schritt klarer.
#' 
## Lage- und Streuungsmaße vergleichen ----
#' Häufig werden diese Kennzahlen erst im Vergleich richtig spannend.   
#' Dafür hilft uns das Argument `.by =` in `summarise()`:
pend %>% summarise(Minimum = min(netges,na.rm = T),
                    Median = median(netges,na.rm = T),
                    Mittelwert = mean(netges,na.rm = T),
                    Maximum = max(netges,na.rm = T),
                   .by = welle)


pend %>% 
  group_by(welle) %>% 
  summarise(Minimum = min(netges,na.rm = T),
                   Median = median(netges,na.rm = T),
                   Mittelwert = mean(netges,na.rm = T),
                   Maximum = max(netges,na.rm = T))


#' Hier stört aber die Sortierung der Welle (R übernimmt die Sortierung aus den Daten).
#' Also hängen wir ein `arrange()` an, um die Sortierung nach `welle` anzufordern:
#' 

pend %>% 
  summarise(Minimum = min(netges,na.rm = T),
                    Median = median(netges,na.rm = T),
                    Mittelwert = mean(netges,na.rm = T),
                    Maximum = max(netges,na.rm = T),
            .by = welle) %>% 
  arrange(welle)

pend %>% 
  arrange(welle) %>% 
  summarise(Minimum = min(netges,na.rm = T),
                    Median = median(netges,na.rm = T),
                    Mittelwert = mean(netges,na.rm = T),
                    Maximum = max(netges,na.rm = T),
            .by = welle)

#' Was aber wenn wir nur Welle 1 und 10 vergleichen wollen?
#' --> Wir schalten einen `filter()` vor:

pend %>% 
  filter(welle %in% c(1,10)) %>% 
  summarise(Minimum = min(netges,na.rm = T),
                    Median = median(netges,na.rm = T),
                    Mittelwert = mean(netges,na.rm = T),
                    Maximum = max(netges,na.rm = T),
                   .by = welle)


