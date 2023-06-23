# ------------------- #
# Kapitel 13: Tabellenexport
# Lösung
# ------------------- #
library(tidyverse)
library(flextable)
library(officer)

# daten einlesen -----
pend_ue14 <- 
  haven::read_dta("./orig/PENDDAT_cf_W13.dta",col_select = c("famstand","azges1","palter","schul2"))%>% 
  filter(palter > 0, famstand > 0 , azges1>0, schul2 > 1) %>% 
  mutate(famstand_fct = factor(famstand,levels = 1:5,labels = c("Ledig","Verheiratet", "Verheiratet getr. lebd.", "Geschieden", "Verwitwet")),
         schul2_fct = factor(schul2, levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi"))) 


## metrische Variablen ------
tab_df <- 
  pend_ue14 %>% 
    select(azges1,palter) %>% 
    pivot_longer(cols = everything(), names_to = "variable") %>% 
    summarise(min = min(value,na.rm = T),
              mean = mean(value,na.rm = T),
              max = max(value,na.rm = T),
              .by = variable)


# als flextable formatieren
tab_df %>% 
  flextable() %>% 
  autofit()

tab_ftab <- 
  tab_df %>% 
    flextable() %>% 
    autofit()
save_as_docx(tab_ftab,path = "./results/Tabelle_Übung13.docx")


## Bonus: viele formatierungen
tab_ftab2 <- 
    pend_ue14 %>% 
      haven::zap_labels() %>% 
      select(Arbeitszeit=azges1,Alter=palter) %>% # in select() umbenennen
      pivot_longer(cols = everything(), names_to = "Variable") %>% 
      group_by(Variable) %>% 
      summarise(Min = min(value,na.rm = T),
                Mean = mean(value,na.rm = T),
                Median = median(value,na.rm = T),
                Max = max(value,na.rm = T)) %>% 
      flextable() %>% 
      border_remove() %>% 
      hline_top(border = fp_border(color = "navy",style = "dotted",width = 4)) %>% 
      hline(i=2,j=3:4,border = fp_border(color = "orange",style = "dashed",width = 4)) %>% 
      colformat_double(j=3,big.mark = ".", decimal.mark = ",",digits = 2) %>% 
      colformat_double(j=c(2,4:5),big.mark = ".", decimal.mark = ",",digits = 0) %>% 
      bg(i= 2,j=4,bg = "green") %>% 
      autofit()
save_as_docx(tab_ftab2,path = "./results/Tabelle_Übung13_ver02.docx")


## kategoriale Variablen --------------
tab_kat <- 
  pend_ue14 %>%  
    select(famstand_fct,schul2_fct) %>% 
    pivot_longer(everything(),names_to = "variable") %>% 
    count(variable,value)

# gleich ander Namen für die Variablen
tab_kat <- 
pend_ue14 %>%  
  select(Familienstand = famstand_fct, Schulabschluss= schul2_fct) %>% 
  pivot_longer(everything(),names_to = "variable") %>% 
  count(variable,value)

tab_kat %>% 
  flextable() %>% 
  autofit() %>% 
  border_remove() %>%
  hline_top() %>% # linie oben
  hline(i =c(5)) %>% 
  merge_v(j=1) # gleiche Zeilen in Spalte 1 zusammenfassen

# Bonus flextable befehle
tab_kat %>% 
  flextable() %>% 
  autofit() %>% 
  border_remove() %>%
  hline_top() %>% # linie oben
  hline(i =c(5,11)) %>% 
  set_header_labels(variable ="Variable",
                    value = "Kategorie",
                    n = "Anzahl") %>% 
  merge_v(j = 1)

KAT_FTAB <- 
tab_kat %>% 
  flextable() %>% 
  autofit() %>% 
  border_remove() %>%
  hline_top() %>% # linie oben
  hline(i =c(7,11)) %>% 
  set_header_labels(variable ="Variable",
                    value = "Kategorie",
                    n = "Anzahl") %>% 
  merge_v(j = 1)


save_as_docx(KAT_FTAB ,path = "./results/Übung13_2.docx")



ftab2 <- 
  tab_kat %>% flextable() %>% border_remove()

ftab2 %>% hline(i = ~variable == "famstand_fct")
ftab2 %>% hline(i = ~variable != lead(variable))

ftab2 %>% bg(j=1,i=~n<2000,bg = "orange")

# Regressionstabelle -----------
m1 <- lm(azges1 ~ schul2_fct , pend_ue14)
m2 <- lm(azges1 ~ schul2_fct + palter, pend_ue14)

modelsummary(
  list("Modell 1" = m1, "Modell 2" = m2),
  output = "flextable",
  gof_omit = "IC|Log|RMS")


ref_rows <- tribble( ~ term,    ~ "Modell 1",  ~ "Modell 2",
                     "kein Schulabschluss",    'ref.',   'ref.')
attr(ref_rows, 'position') <- 3 # Zeile angeben

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
# regtab2 <- 
  modelsummary(
    list("Modell 1" = m1, "Modell 2" = m2),
    output = "flextable",
    gof_omit = "IC|Log|RMS",
    coef_rename = c("(Intercept)"="Intercept",
                    "palter" = "Alter",
                    "schul2_fctFörderschule" = "Förderschulabschluss",
                    "schul2_fctHauptschule" = "Hauptschulabschluss",
                    "schul2_fctMittlere Reife" = "Mittlere Reife",
                    "schul2_fctFOS/BOS" = "Fachhochschulreife",
                    "schul2_fctAbi" = "Abitur"),
    add_rows = ref_rows,
    stars = T,
    fmt = 2) %>% 
  autofit() %>% 
  italic(i = ~ `Modell 2` == "ref.",j =2:3)
