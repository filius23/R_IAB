# Recap Tag 3
library(tidyverse)
library(modelsummary)

# datensatz laden & pipe ---
pend_recap <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                             col_select = c("pnr","welle", "zpsex", "PSM0100","netges",
                                            "azges1","palter","PD0400")) %>% 
  filter(welle == 8, palter > 0,azges1 > 0) %>% 
  mutate(
    zpsex_fct = factor(zpsex,levels = 1:2, labels = c("M","W")),
    zpsex_num = as.numeric(zpsex-1),
    zpsex_log = as.logical(zpsex-1),
    )

# regressionen
lm(netges ~ zpsex, data = pend_recap)
lm(netges ~ zpsex_fct, data = pend_recap)
lm(netges ~ zpsex_num, data = pend_recap)
lm(netges ~ zpsex_log, data = pend_recap)

lm(netges ~ azges1, pend_recap)

m1 <- lm(netges ~ azges1 + I(azges1^2), pend_recap)
m2 <- lm(netges ~ azges1 + I(azges1^2) + zpsex_fct, pend_recap)
m3 <- lm(netges ~ azges1 + zpsex_fct * palter , pend_recap)

library(modelsummary)
modelsummary(list(m1,m2,m3),stars = T,gof_omit = "IC|Log|RM")
modelplot(list(m1,m2,m3),coef_omit = "Interc")

## adjusted predictions
library(marginaleffects)
predictions(m3,  newdata = datagrid(palter = 20:65, 
                                    zpsex_fct =  c("M","W")) ) %>% 
  data.frame() %>% 
  ggplot(
         aes(x = palter, 
             y =  estimate, 
             ymin = conf.low,ymax = conf.high,
             color = zpsex_fct)) + 
  geom_line() + 
  geom_point(size = 2.75) +
  geom_errorbar(width = .1) +
  scale_color_manual(values = c("dodgerblue2","sienna1"),
                     breaks = c("M","W"),labels=c("Frauen","Männer")) +
  labs(title = "Vorhergesagte Werte für netges",
       color = "Gender",
       x = "Werte für palter",
       y = "Vorhergesagte Werte für netges") +
  theme_minimal()

# reshape / "pivot" -------

pend_ue11b <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                              col_select = c("pnr","welle","famstand")) %>%
  slice(200:210) %>% 
  filter(welle %in% 2:3)


pend_ue11b

# Bringen Sie `pend_ue11b` in das wide shape:
pend_ue11b %>% pivot_wider(names_from = welle, values_from = famstand)

# Tipp: mit `,names_prefix = "wave"` in `pivot_wider()` können wir ein Präfix angeben:
pend_ue11b %>% 
  pivot_wider(names_from = welle, 
              values_from = famstand,
              names_prefix = "wave")
