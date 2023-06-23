# Tabellenexport
# Eure Zeit ist zu wertvoll, um Tabellen per Hand zu erstellen! 
library(janitor) # kreuztabellen
library(modelsummary) # Tabellen vorbereiten
library(janitor) # kreuztabellen
library(flextable) # Formatierung der Tabelle für Word
library(officer) # eigentlicher Word-Export
library(tidyverse)

# Zu diesen Variablen sollen folgende deskriptiven Übersichtstabellen erstellt und als Word-Dokument exportiert werden:
# Wir starten mit einem Ausschnitt des PASS CF:
pend14 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",col_select = c("pnr","welle","netges","azges1","zpsex","schul2")) %>% 
  filter(welle == 13,schul2 > 1) %>% 
  mutate(across(everything(),~ifelse(.x<0, NA, .x))) %>% 
  na.omit() # alle Zeilen mit (mind.) 1 NA löschen

# `{flextable}`  ------
df1 <- data.frame(x1= c(2,2), y1 = c(0,1))
df1

# `{flextable}` stellt uns eine Reihe an Funktionen zur Formatierung zur Verfügung, um die Darstellung des `data.frame` zu anzupassen:
df1 %>% 
  flextable() %>% 
  border_remove() %>% 
  hline_top(border = fp_border(color = "orange")) %>%
  hline(i=1,border = fp_border(color = "blue",style = "dotted")) %>% 
  set_header_labels(x1 = "Anderes Label") %>% 
  add_header_row(values = c("Überschrift",""),colwidths = c(1,1)) %>% 
  autofit()

# [Hier](https://ardata-fr.github.io/flextable-book/) finden sich weitere Infos zu `flextable`, u.a. können bspw. die Trennlinien dünner gemacht werden oder eine andere Farbe angegeben werden.  [Hier](https://davidgohel.github.io/flextable/reference/index.html) finden sich alle vefügbaren Funktionen.

# Deskription -------
 
## Verteilungstabellen für metrische Variablen -----
# 
# Für die metrischen Merkmale kennen wir ja das `summary()`:

summary(pend14$netges)
summary(pend14$azges1)

# Eine einfach Möglichkeit, diese `summary()` untereinander anzuordnen, ist `summarise` in Kombination mit `pivot_longer()` zu verwenden:
pend14 %>% 
  select(azges1,netges) %>% 
  pivot_longer(cols = everything(), names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(min = min(value,na.rm = T),
            mean = mean(value,na.rm = T),
            max = max(value,na.rm = T))


pend14 %>% 
  select(azges1,netges) %>% 
  pivot_longer(cols = everything(), names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(Min  = min(value,na.rm = T),
            Mean = mean(value,na.rm = T),
            Max  = mean(value,na.rm = T)) %>% 
  flextable() %>% 
  colformat_num(decimal.mark = ",",big.mark = " ") %>% 
  autofit()

# 

met_ft <- 
  pend14 %>% 
  select(azges1,netges) %>% 
  pivot_longer(cols = everything(), names_to = "variable") %>% 
  group_by(variable) %>% 
  summarise(Min  = min(value,na.rm = T),
            Mean = mean(value,na.rm = T),
            Max  = mean(value,na.rm = T)) %>% 
  flextable() %>% 
  autofit()

# Der eigentliche Export ist dann mit `save_as_docx`, wo wir eine Überschrift und mit `path` die Zieldatei  angeben können:
save_as_docx("Metrische unab. Variablen" = met_ft, 
             path = "./results/Met_UVs_Tabelle.docx")


## Häufigkeitsauszählungen  -------
pend14 %>%  
  mutate(schul2_fct = factor(schul2,levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi")),
         zpsex_fct = factor(zpsex,levels = 1:2,labels =c("Männer","Frauen"))) %>% 
  select(zpsex_fct,schul2_fct) %>% 
  pivot_longer(everything(),names_to = "variable") %>% 
  count(variable,value)

häufigkeiten <- 
  pend14 %>%  
  mutate(schul2_fct = factor(schul2,levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi")),
         zpsex_fct = factor(zpsex,levels = 1:2,labels =c("Männer","Frauen"))) %>% 
  select(zpsex_fct,schul2_fct) %>% 
  pivot_longer(everything(),names_to = "variable") %>% 
  count(variable,value)

# relative Häufigkeiten einfügen
häufigkeiten <-  häufigkeiten %>% mutate(pct = prop.table(n),.by=variable)

# als flextable formatieren
häufigkeiten %>% 
  flextable()

# alles auf einmal
kat_ft <- 
  pend14 %>%  
  mutate(schul2_fct = factor(schul2,levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi")),
         zpsex_fct = factor(zpsex,levels = 1:2,labels =c("Männer","Frauen"))) %>% # labeln
  select(zpsex_fct,schul2_fct) %>% # nur interessierende Variablen behalten
  pivot_longer(everything(),names_to = "variable") %>% # ins long shape
  count(variable,value)  %>%  # auszählen welchge Kombination aus Variablen & Ausprägungen es gibt
  mutate(pct = prop.table(n),.by=variable) %>% # relative Häufigkeiten
  flextable() %>% autofit()

 
# Für den Export können wir dann wieder `save_as_docx()` verwenden:
save_as_docx("Kategoriale unab. Variablen" = kat_ft, path = "./results/Kat_UVs_Tabelle.docx")


# Kreuztabellen -----
library(janitor)
pend14 %>%  
  mutate(zpsex_fct = factor(zpsex,levels = 1:2, labels = c("Männer","Frauen"))) %>% 
  mutate(schul2_fct = factor(schul2, levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi"))) %>% 
  tabyl(.,schul2_fct,zpsex_fct) %>%
  adorn_totals(where = c("row","col")) %>% 
  flextable() %>%
  border_remove() %>%
  hline(i=6) %>%
  hline_top() %>%
  set_header_labels(schul2 = "Ausbildung") %>% 
  add_header_row(values = c("","Geschlecht",""),colwidths = c(1,2,1))

# Hier ist die Herausforderung, einen `data.frame()` für `{flextable}` vorzubereiten: `xtabs()` gibt keinen `data.frame` aus und  meistens ist der long shape Output von `count()` auch nicht das was wir wollen:

tab1 <- xtabs(~zpsex+schul2,pend14)
class(tab1)

pend14 %>% 
  mutate(zpsex_fct = factor(zpsex,levels = 1:2, labels = c("Männer","Frauen"))) %>% # zahlenwerte in zpsex mit labels überschreiben 
  mutate(schul2_fct = factor(schul2, levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi"))) %>% # auch für schul2
  count(zpsex_fct,schul2_fct) 

# [`tabyl()`](https://sfirke.github.io/janitor/articles/tabyls.html) aus {janitor}
library(janitor)
pend14 %>% 
  mutate(zpsex_fct = factor(zpsex,levels = 1:2, labels = c("Männer","Frauen"))) %>% # zahlenwerte in zpsex mit labels überschreiben 
  mutate(schul2_fct = factor(schul2, levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi"))) %>% # auch für schul2
  tabyl(schul2_fct,zpsex_fct) %>%
  adorn_totals(where = c("row","col")) 


pend14 %>% 
  mutate(zpsex_fct = factor(zpsex,levels = 1:2, labels = c("Männer","Frauen"))) %>% # zahlenwerte in zpsex mit labels überschreiben 
  mutate(schul2_fct = factor(schul2, levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi"))) %>% # auch für schul2
  tabyl(schul2_fct,zpsex_fct) %>%
  adorn_totals(where = c("row","col"))  %>%
  flextable() %>%
  border_remove() %>% # linien raus
  hline(i=6) %>% # in zeile 4 eine Linie einfügen
  hline_top() %>% # linie oben
  set_header_labels(schul2_fct = "Ausbildung") %>%  # kopf-label links
  add_header_row(values = c("","Geschlecht",""),colwidths = c(1,2,1)) # label oben


cross_tab <- 
  pend14 %>% 
  mutate(zpsex_fct = factor(zpsex,levels = 1:2, labels = c("Männer","Frauen"))) %>% # zahlenwerte in zpsex mit labels überschreiben 
  mutate(schul2_fct = factor(schul2, levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi"))) %>% # auch für schul2
  tabyl(schul2_fct,zpsex_fct) %>%
  adorn_totals(where = c("row","col"))  %>%
  flextable() %>%
  border_remove() %>% # linien raus
  hline(i=6) %>% # in zeile 4 eine Linie einfügen
  hline_top() %>% # linie oben
  set_header_labels(schul2_fct = "Ausbildung") %>%  # kopf-label links
  add_header_row(values = c("","Geschlecht",""),colwidths = c(1,2,1)) # label oben

save_as_docx("Kreuztabelle" = cross_tab, path = "./results/Kreuztabelle.docx")

# ### [Übung](#tabue1) 


# Regressionstabellen -------------
# 
# Für Regressionstabellen können wir mit `{modelsummary}` eine `{flextable}`-Tabelle erstellen:
# 

pend14_reg_df <- 
  pend14 %>%
  mutate(zpsex_fct = factor(zpsex,levels = 1:2, labels = c("Männer","Frauen")),
         schul2_fct = factor(schul2,levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi"))) %>% 
  na.omit()

m1 <- lm(netges ~ azges1 + zpsex_fct, data = pend14_reg_df)
m2 <- lm(netges ~ azges1 + zpsex_fct + schul2_fct, data = pend14_reg_df)
modelsummary(list("Modell 1"=m1,"Modell 2"=m2),
                                output = "flextable",gof_omit = "IC|Log|RMS",
                           coef_rename = c("(Intercept)"="Intercept",
                                           "azges1" = "Arbeitszeit (h)",
                                           "zpsex_fctFrauen" = "Frauen",
                                           "schul2_fctFörderschule" = "Förderschulabschluss",
                                           "schul2_fctHauptschule" = "Hauptschulabschluss",
                                           "schul2_fctMittlere Reife" = "Mittlere Reife",
                                           "schul2_fctFOS/BOS" = "Fachhochschulreife",
                                           "schul2_fctAbi" = "Abitur"),
                           stars = T,fmt =2)


 
## Referenzkategorien einfügen {#refcats} -----
# 
# Um die Referenzkategorie für kategoriale Variablen kenntlich zu machen, können wir den Hinweis *ref.* mitaufanzeigen. 
# 
# Dazu können wir mit Hilfe des Arguments `add_rows` eine zusätzliche Zeile für die Referenzkategorie der Variable `zpsex` einfügen. 
# Zunächst erstellen wir einen `data.frame`, welcher neben den Modellnamen die Koeffizientennamen sowie die einzufügenden Werte enthält. Mit `tribble` aus dem Paket `tibble` lässt sich das einfach bewerkstelligen: wir können die Zeilen und Spalten gleich so aufschreiben, wie wir sie haben möchten:
# 

library(tibble)
ref_rows <- tribble( ~ term,    ~ "Modell 1",  ~ "Modell 2",
                     "Männer",    'ref.',   'ref.')
attr(ref_rows, 'position') <- 5 # Zeile angeben 

modelsummary(
  list("Modell 1" = m1, "Modell 2" = m2),
  output = "flextable",
  gof_omit = "IC|Log|RMS",
  coef_rename = c("(Intercept)"="Intercept",
                                           "azges1" = "Arbeitszeit (h)",
                                           "zpsex_fctFrauen" = "Frauen",
                                           "schul2_fctFörderschule" = "Förderschulabschluss",
                                           "schul2_fctHauptschule" = "Hauptschulabschluss",
                                           "schul2_fctMittlere Reife" = "Mittlere Reife",
                                           "schul2_fctFOS/BOS" = "Fachhochschulreife",
                                           "schul2_fctAbi" = "Abitur"),
  add_rows = ref_rows,
  stars = T,
  fmt = 2
) %>% autofit()

# 
# **Das funktioniert auch für mehrere Referenzkategorien:**

ref_rows2 <- tribble(~term,    ~"Modell 1",  ~"Modell 2",
                "Männer",    'ref.',   'ref.',
                "keine Schulabschluss",    '',   'ref.',
                )
attr(ref_rows2, 'position') <- c(5,8) # Zeile angeben 

modelsummary(
  list("Modell 1" = m1, "Modell 2" = m2),
  output = "flextable",
  gof_omit = "IC|Log|RMS",
  coef_rename = c("(Intercept)"="Intercept",
                                           "azges1" = "Arbeitszeit (h)",
                                           "zpsex_fctFrauen" = "Frauen",
                                           "schul2_fctFörderschule" = "Förderschulabschluss",
                                           "schul2_fctHauptschule" = "Hauptschulabschluss",
                                           "schul2_fctMittlere Reife" = "Mittlere Reife",
                                           "schul2_fctFOS/BOS" = "Fachhochschulreife",
                                           "schul2_fctAbi" = "Abitur"),
  add_rows = ref_rows2,
  stars = T,
  fmt = 2
)

# # Tipparbeit beim Umbenennen sparen mit `coef_rename =`
rename_function <- function(old_names) {
  new_names <- 
    gsub("schul2_fct", "", old_names) %>% 
    gsub("zpsex_fct", "",.) %>% 
    gsub("azges1", "Arbeitszeit (h) ",.)
  
  return(setNames(new_names, old_names))
}

## diese Funktion dann in modelsummary verwenden:
modelsummary(list("Modell 1" = m1, "Modell 2" = m2),
             output = "flextable",gof_omit = "IC|Log|RMS", 
             coef_rename = rename_function) # function anwenden

# Auf den mit `{modelsummary}` erstellten `flextable` können wir natürlich auch [alle Funktionen](https://ardata-fr.github.io/flextable-book/) für `flextable` anwenden und dann mit `save_as_docx()` die Tabelle exportieren:

regtab2 <- 
  modelsummary(
    list("Modell 1" = m1, "Modell 2" = m2),
    output = "flextable",
    gof_omit = "IC|Log|RMS",
    coef_rename = c("(Intercept)"="Intercept",
                    "azges1" = "Arbeitszeit (h)",
                    "zpsex_fctFrauen" = "Frauen",
                    "schul2_fctFörderschule" = "Förderschulabschluss",
                    "schul2_fctHauptschule" = "Hauptschulabschluss",
                    "schul2_fctMittlere Reife" = "Mittlere Reife",
                    "schul2_fctFOS/BOS" = "Fachhochschulreife",
                    "schul2_fctAbi" = "Abitur"),
    add_rows = ref_rows2,
    stars = T,
    fmt = 2) %>% 
  autofit() %>% 
  italic(i = ~ `Modell 2` == "ref.",j =2:3)

# 
save_as_docx(regtab2,path = "./results/regressionstabelle.docx")

# Alle Tabellen in eine Datei mit `{officer}`--------------
library(officer)

# Zunnächst lesen wir mit `read_docx()` eine Vorlage ein, welche Einstellungen für das Word-Dokument enthält (Seitenformat,..) und fügen dann mit `body_add_flextable()` die Tabellen ein. Mit `body_add_par(.,"")` können wir leere Absätze einfügen. 
read_docx("Word_Vorlage.docx")

read_docx("Word_Vorlage.docx") %>%
   body_add_par(value = "Metrische Variablen",style = "heading 1") %>% 
   body_add_flextable(., value = met_ft ) %>% # flextable met_ft einfügen
   body_add_par(.,"") %>% # leeren Absatz  einfügen
   body_add_par(value = "Kategoriale Variablen",style = "heading 1") %>% 
   body_add_flextable(., value = kat_ft ) %>% # flextable cat_ft einfügen
   body_add_par(.,"") %>% # leeren Absatz einfügen
   body_add_flextable(., value = regtab2 ) %>% # flextable regtab2 einfügen
   print(target = "./results/Tables.docx")


