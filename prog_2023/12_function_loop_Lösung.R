# ------------------- #
# Kapitel 12: Schleifen/Funktionen
# Lösung
# ------------------- #
library(tidyverse)

pend_ue12 <- 
  haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                  col_select = c("pnr","welle","zpsex","netges","schul2","bilzeit","azges1","palter","PA0445") ) %>% 
  filter(netges > 0, schul2 %in% 2:7, palter > 0, bilzeit > 0, azges1 > 0, PA0445>0 ) %>% 
  mutate(
    across(matches("palter|netges|azges1|bilzeit|PA0445"),~as.numeric(.x)),
    schul2_fct = factor(schul2,levels = 2:7, labels = c("ohne","Förderschule","Hauptschule","Mittlere Reife","FOS/BOS","Abi")),
    zpsex_fct = factor(zpsex,levels = 1:2,labels =c("Männer","Frauen")))


# Übung 1 plots -------
  ggplot(pend_ue12,aes(x= bilzeit, y = palter)) + geom_point()

plot_ue1 <- function(schul2_filter){
  pend_ue12 %>%
    filter(schul2_fct == schul2_filter) %>% 
    ggplot(aes(x = bilzeit, y = palter)) + 
    geom_point() + 
    theme_minimal() +
    labs(title = paste0("Schulbildung: ",schul2_filter))
}

plot_ue1("ohne")
plot_ue1("Hauptschule")

plt_schul2 <-  expand_grid(schul2_fct = levels(pend_ue12$schul2_fct)) %>% 
  mutate(plots = map(schul2_fct,plot_ue1))

## daumenkino der plots
# nach einander alle zeigen
lapply(1:6,function(x){
  plt_schul2 %>% slice(x) %>% pull(plots)
})



## zusätzlich nach Geschlecht ---

plot_ue2 <- function(schul2_filter,zpsex_filter){
  pend_ue12 %>%
    filter(schul2_fct == schul2_filter, zpsex_fct == zpsex_filter) %>% 
    ggplot(aes(x = bilzeit, y = PA0445)) + 
    geom_point() + 
    theme_minimal() +
    labs(title = paste0("Schulbildung: ",schul2_filter, " Geschlecht: ", zpsex_filter))
}


plt_schul2 <-  expand_grid(schul2_fct = levels(pend_ue12$schul2_fct),
                           zpsex_fct = levels(pend12$zpsex_fct)) %>% 
  mutate(plots = pmap(list(schul2_filter= schul2_fct,zpsex_filter = zpsex_fct),  # Variablen mit = den Funktions-Argumenten zuweisen
                      plot_ue2))
    

plt_schul2
plt_schul2 %>% filter(zpsex_fct == "Frauen",schul2_fct=="Abi") %>% pull(plots)

## daumenkino der plots
# nach einander alle zeigen
lapply(1:nrow(plt_schul2),function(x){
  plt_schul2 %>% slice(x) %>% pull(plots)
})


# Übung 2 Funktion -------

# funktion erstellen
  mo_function <- function(modx){
    mx <- lm(formula = modx,data = pend_ue12)
    return(mx)
  }
# testen 
  mo_function("azges1 ~ zpsex_fct + schul2_fct + palter") %>% summary()
  mo_function("azges1 ~ zpsex_fct + schul2_fct + palter") %>% modelsummary(output = "markdown",stars=T)

# Modell-Vektor
  modls_ue12 <- c(
    modell1 = "azges1 ~ zpsex_fct",
    modell2 = "azges1 ~ zpsex_fct + schul2_fct",
    modell3 = "azges1 ~ zpsex_fct + schul2_fct + palter"
  )
  
## Modell-Vektor anwenden mit map()
  map(modls_ue12,mo_function)  
  map(modls_ue12,mo_function) %>% modelsummary::modelsummary(output = "markdown",stars=TRUE)
  
  
#  Übung 3: Nach Frau/Mann  ------

# nesten
pend_ue12_genestet <- 
  pend_ue12 %>% 
  nest(.by=zpsex_fct)
  
# nesten wieder rückgängig machen:
pend_ue12_genestet %>% unnest(cols = c(data))

mod_u12_2 <- 
  pend_ue12 %>% 
    nest(.by=zpsex_fct) %>% 
    mutate(model = map(data, function(dat1) {
      lm("azges1 ~ schul2_fct + palter", data = dat1)
    }))
  
mod_u12_2

library(modelsummary)
modelsummary(mod_u12_2$model,stars = T)  
modelplot(mod_u12_2$model,coef_omit = "Intercept",
          coef_rename = c("schul2_fctHauptschule" = "Hauptschule"))

