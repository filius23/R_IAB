# Recap Tag 2 (und 1)
library(tidyverse)

# datensatz laden & pipe ---
pend_recap <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                             col_select = c("pnr","welle", "zpsex", "PSM0100",
                                            "azges1","palter","PD0400")) %>% 
  filter(welle == 8, palter > 0,azges1 > 0)

# ggplot2 ----
pend_recap %>% 
  ggplot(aes(x= azges1, y = palter, color = factor(zpsex))) + 
  geom_point() + 
  scale_color_viridis_d(option = "mako",begin = .5,end = .8) + 
  theme_dark() +
  labs(color = "Geschlecht")


# labels & factor -----
attributes(pend_recap$zpsex)

attributes(pend_recap$zpsex)$label
attributes(pend_recap$zpsex)$labels

pend_recap$PD0400_fct2 <- factor(pend_recap$PD0400,
                               levels = 1:4,
                               labels = c("überhaupt nicht", "eher nicht",
                                          "eher", "sehr"))
pend_recap$PD0400_fct3 <- factor(pend_recap$PD0400,
                               levels = c(3,4,2,1),
                               labels = c(
                                          "sehr","eher",
                                          "eher nicht","überhaupt nicht"))


pend_recap %>% select(matches("PD0")) %>%  head()

pend_recap %>% select(matches("PD0")) %>%  head() %>% arrange(PD0400_fct3)
pend_recap %>% select(matches("PD0")) %>%  head() %>% arrange(PD0400_fct2)

levels(pend_recap$PD0400_fct3 )
levels(pend_recap$PD0400_fct2)


# Data wrangling mit dplyr ----
pend_recap %>% 
  summarise(
    across(ends_with("0"), ~mean(.x,na.rm=T), .names = "m_{.col}")
    )

pend_recap %>% 
  summarise(
    across(ends_with("0"), ~mean(.x,na.rm=T), .names = "m_{.col}"),
    .by = zpsex)

pend_recap %>% 
  mutate(
    across(ends_with("0"), ~mean(.x,na.rm=T), .names = "m_{.col}"),
    .by = zpsex)


# function() ------ 
  dtomean <- function(x){
    d_x <- x - mean(x,na.rm = T)
    return(d_x)
  }

pend_recap %>% 
  mutate(across(ends_with("0"), ~dtomean(.x), .names = "dm_{.col}"),.by = zpsex) %>% 
  select(zpsex, ends_with("0"))


# pend_recap %>% as.data.frame() ...

# tabellenfunktion -----

# freq percent cum wie in stata 
table(pend_recap$zpsex)

table(pend_recap$zpsex) %>% data.frame()

attributes(pend_recap$zpsex)$labels

pend_recap$zpsex_num <- as.numeric(pend_recap$zpsex)
attributes(pend_recap$zpsex_num)$labels

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



## source -> ein R Skript durchlaufen lassen
# source("./prog/Tag02_recap.R")

