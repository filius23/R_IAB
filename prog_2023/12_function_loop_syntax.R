## Schleifen & Funktionen
library(tidyverse)
library(marginaleffects)
library(broom)

pend12 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                         col_select = c("pnr","welle","netges","schul2","azges1","palter","famstand","zpsex","statakt","hnr")) %>% 
  filter(netges > 0, schul2 %in% 2:7, palter > 0, famstand > 0, azges1 > 0 , welle == 13) %>% 
  mutate(
    across(matches("palter|netges|azges1"),~as.numeric(.x)),
    famstand_fct = factor(famstand,levels = 1:5,labels = c("Ledig","Verheiratet", "Verheiratet getr. lebd.", "Geschieden", "Verwitwet")),
    schul2_fct = factor(schul2,levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi")),
    statakt_fct = factor(statakt,levels = 1:3, labels = c("erbwerbstätig","arbeitslos","inaktiv")),
    zpsex_fct = factor(zpsex,levels = 1:2,labels =c("Männer","Frauen")))

#1. wir legen fest, was gemacht werden soll (in einer `function()`)
#2. wir erstellen eine Serie an Werten
#3. und wenden die `function()` auf diese an

# Plot-Serien -----
# : beispielsweise möchten wir folgende Grafik für alle Ausprägungskombinationen von `famstand` und `zpsex` erstellen:
pend12 %>%
  ggplot(aes(x = azges1,  y = netges)) + 
  geom_point() + 
  theme_minimal()

#Wir erstellen zunächst eine `function()`
#Diese geben wir dann in einen `filter()`-Befehl innerhalb der `function()` an und verwenden sie außerdem, um einen Titel zu generieren.
#Das können wir mit `paste0()`, welches fixe Inputs und Vektoren-Inputs zu einem `character` zusammenfügt:
paste0("no",1:5)
#

plot_fun1 <- function(famstat_filter){
  pend12 %>%
    filter(famstand_fct == famstat_filter) %>% 
    ggplot(aes(x = azges1,  y = netges)) + 
    geom_point() + 
    theme_minimal() +
    labs(title = paste0("Familienstatus: ",famstat_filter))
}

#Mit dieser `function()` können wir nur leicht die jeweiligen Plots erstellen:
plot_fun1(famstat_filter = "Ledig")
plot_fun1(famstat_filter = "Verheiratet")

#Wie kommen wir jetzt aber an alle Kombinationen?
levels(pend12$famstand_fct)
fam_plots <- lapply(levels(pend12$famstand_fct), plot_fun1)
class(fam_plots)

fam_plots[[2]] # ersten Plot aus der fam_plots-Liste aufrufen

#Etwas übersichtlicher ist das ganze, wenn wir die Plots in einem `data.frame`( bzw. einem `tibble`) ablegen neben einer Spalte mit den Ausprägungen:
plot_df <- 
  tibble(famstand_fct = levels(pend12$famstand_fct)) %>% 
  mutate(plots = map(famstand_fct,plot_fun1))

head(plot_df)

plot_df %>% filter(famstand_fct ==  "Ledig") %>% pull(plots)
plot_df %>% slice(5) %>% pull(plots)

# nach einander alle zeigen
lapply(1:5,function(x){
  plot_df %>% slice(x) %>% pull(plots)
})

#Was jetzt aber, wenn wir mehrere Variablen berücksichtigen möchten?
#Dazu erstellen wir zunächst eine neue `function()` mit mehreren Argumenten und erweitern auch den `filter()` und `paste0()`-Befehl innerhalb der Funktionen:

plot_fun2 <- function(famstat_filter, zpsex_filter){
  pend12 %>%
    filter(famstand_fct == famstat_filter, zpsex_fct == zpsex_filter) %>% 
    ggplot(aes(x = azges1,  y = netges)) + 
    geom_point() + 
    theme_minimal() +
    labs(title = paste0("Familienstatus: ",famstat_filter, " Geschlecht: ", zpsex_filter))
}

# alle Kombinationen aus famstat und zpsex mit `expand_grid` 
expand_grid(famstand_fct = levels(pend12$famstand_fct),
            zpsex_fct = levels(pend12$zpsex_fct))

plot_df2 <- 
  expand_grid(famstand_fct = levels(pend12$famstand_fct),
              zpsex_fct = levels(pend12$zpsex_fct)) %>% 
    mutate(plots = pmap(list(famstat_filter= famstand_fct,zpsex_filter = zpsex_fct),  # Variablen mit = den Funktions-Argumenten zuweisen
                        plot_fun2))

plot_df2 %>% filter(famstand_fct ==  "Verheiratet",zpsex_fct ==  "Frauen") %>% pull(plots)
plot_df2 %>% filter(famstand_fct ==  "Verheiratet",zpsex_fct ==  "Männer") %>% pull(plots)

## Daumenkino basteln für Viewer rechts unten  ------
map(plot_df2$plots, ~print(.x))

## ggsave ----
#funktioniert nur mit unlist():
ggsave(
  plot = 
  plot_df2 %>% 
  filter(famstand_fct ==  "Verheiratet",zpsex_fct ==  "Männer") %>% 
  pull(plots) %>% # auswählen und aufrufen
    unlist(), # unlist --> plot aufrufen statt listenelement
   filename = "./results/Verheiratete_Männer.png")


# speichern in function() Befehl miteinbauen:
plot_fun2_mit_save <- function(famstat_filter, zpsex_filter){
  
  plot <- 
    pend12 %>%
      filter(famstand_fct == famstat_filter, zpsex_fct == zpsex_filter) %>% 
      ggplot(aes(x = azges1,  y = netges)) + 
      geom_point() + 
      theme_minimal() +
      labs(title = paste0("Familienstatus: ",famstat_filter, " Geschlecht: ", zpsex_filter))
  
  ggsave(plot,filename =  paste0("./results/",famstat_filter,"_",zpsex_filter,"_",Sys.Date(),".png"))
  
  return(plot) # so wird Plot in R nochmal angezeigt
}


plot_fun2_mit_save(famstat_filter = "Ledig",zpsex_filter = "Frauen")


#### [Übung](#manyplts) {#ue12_00}

# Modellserien ----

## Verschiedene Modelle auf die gleichen Daten als `function()` mit `map()` anwenden ----
#+ Modell 1 = `netges ~ azges1`
#+ Modell 2 = `netges ~ azges1 + zpsex_fct`
#+ Modell 3 = `netges ~ azges1 + zpsex_fct + schul2_fct`
#+ Modell 4 = `netges ~ azges1 + zpsex_fct + schul2_fct + palter`

#Natürlich würde so etwas funktionieren, würde aber vier separate Modelle erstellen:
mod1 <- lm(netges ~ azges1, data = pend12)
mod2 <- lm(netges ~ azges1 + zpsex_fct, data = pend12)
mod3 <- lm(netges ~ azges1 + zpsex_fct + schul2_fct, data = pend12)
mod4 <- lm(netges ~ azges1 + zpsex_fct + schul2_fct + palter, data = pend12)

#Wir definieren dann eine `function()`, in der wir angeben, dass das angegebene Argument die Formel für ein `lm()` sein soll - das Ergebnis dieses `lm()` lassen wir uns dann ausgeben.
mod_function <- function(modx){
  mx <- lm(formula = modx,data = pend12)
  return(mx) 
}

#Als Test können wir `mod_function()` jetzt einmal verwenden:
mod_function("netges ~ azges1") %>% summary() # sieht gut aus

#Im nächsten Schritt erstellen wir jetzt einen Vektor mit verschiedenen Modellvarianten:
mdls <- c(
  "Modell 1" = "netges ~ azges1",
  "Modell 2" = "netges ~ azges1 + zpsex_fct",
  "Modell 3" = "netges ~ azges1 + zpsex_fct + schul2_fct",
  "Modell 4" = "netges ~ azges1 + zpsex_fct + schul2_fct + palter"
)

mdls

#Mit `map` wenden wir unsere `mod_function` jetzt auf den Modell-Vektor an:
mods <- map(mdls,mod_function)

summary(mods$`Modell 3`)
mods$`Modell 1`
mods$`Modell 2`

#Außerdem können wir uns alle Modelle auch direkt in `modelsummary` ausgeben lassen:

library(modelsummary)
modelsummary(mods,stars = T,gof_omit = "IC|RM|Log",output = "markdown")

#### [Übung](#manymod) {#ue12_01}


## Das gleiche Modell auf verschiedene Datensets anwenden -----
#Der zweite Fall ist das gleiche Modell auf verschiedene Datensätze anzuwenden - wer kennt es nicht: 

# "Habt ihr das auch mal getrennt für Ost/West gerechnet?"
#...dafür hilft uns `nest(.by =)` - damit können wir den Datensatz entlang einer Variable splitten und zusammenfassen:

hh_dat <- haven::read_dta("./orig/HHENDDAT_cf_W13.dta", col_select = c("hnr","welle","region"))

pend12_ow <- 
  pend12 %>%
    left_join(hh_dat,by = c("hnr","welle")) %>% 
    mutate(east = ifelse(region == 2,"east","west")) %>% 
  select(netges,azges1,schul2_fct,palter,zpsex_fct, east)

pend12_ow_nest <- 
  pend12_ow %>% 
      nest(.by=east)

#In der Spalte `data` sind jetzt also die Datensätze für Ost und West enthalten:
head(pend12_ow_nest$data[[1]],n=3)
head(pend12_ow_nest$data[[2]],n=3)

# so würde es schritt für schritt gehen:
lm("netges ~ palter + zpsex_fct", data = pend12_ow %>% filter(east == "east"))
lm("netges ~ palter + zpsex_fct", data = pend12_ow %>% filter(east == "west"))

#Auf den so gesplitteten Datensatz können wir mit `map()` ein `lm()`-Modell anwenden:
mod_ow <- 
  pend12_ow_nest %>% 
    mutate(model = map(data, function(data) {
      lm("netges ~ palter + zpsex_fct", data = data) # ad-hoc function --> siehe tip)
    })) 

#Das Ergebnis ist ein `data.frame`/`tibble`, welcher in der Spalte `model` die `lm`-Objekte enthält:

mod_ow
mod_ow$model[1]
mod_ow$model[2]

modelsummary::modelplot(mod_ow$model,coef_omit = "Intercept") +
  geom_vline(aes(xintercept = 0), linetype = 2, alpha = .5) +
  scale_color_manual(values = c("orange","navy"), breaks = c("(1)","(2)"),labels = c("west","ost")) 
  
modelsummary(mod_ow$model,stars = T,gof_omit = "IC|RM|Log",output = "flextable")

#### [Übung](#manydata) {#ue12_02) 
#
### Übungen

# Loop mit `for` ----
for(i in 1:8){
  print(i)
}

# loop über Variablen -----
#Möchten wir über Variablen loopen, müssen wir R explizit mitteilen, dass die mitgegebenen strings als Variablen zu verstehen sind: 
for(v in c("schul2_fct","zpsex_fct","statakt_fct")){
  pend12 %>% count(v) %>% print()
}

#Das können wir mit `!!rlang::sym(v)`:
for(v in c("schul2_fct","zpsex_fct","statakt_fct")){
  pend12 %>% count(!!rlang::sym(v)) %>% print()
}