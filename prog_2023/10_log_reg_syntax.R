#' # Logistische Regressionsmodelle 
library(tidyverse)
library(marginaleffects)

pend10 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                          col_select = c("pnr","welle","zpsex","famstand","palter","PSM0100")) %>% 
  mutate(across(everything(),~ifelse(.x<0,NA,.x)),
         soc_med = 2- PSM0100)

pend10 %>% count(soc_med,PSM0100)

# Das logistische Regressionsmodell {#logmod} ----
#' glm() --> `family="binomial"` ist dabei entscheidend:
m2 <- glm(soc_med ~ palter, family = "binomial", data = pend10)
summary(m2)

library(marginaleffects)

#' AME -----
avg_slopes(m2)

# Predictions {#pred} ---

predictions(m2, newdata = datagrid(palter  = c(18,25,65))) 

predictions(m2, newdata = datagrid(palter  = 18:65)) %>% # vorhergesagte Werte
  data.frame() %>% 
  ggplot(aes(y = estimate , x = palter)) + 
  geom_errorbar(aes(ymin = conf.low, ymax= conf.high), color = "slateblue",width = .1) + # konfidenzintervalle
  geom_point(color = "slateblue") + # punktschätzer
  theme_minimal()

#' ## [Übung 1](#ame1)

## Fixed effects logistische Regression mit `{fixest}` {#feglm} -------------
#' 
#' Mit `feglm()` lassen sich auch logistische FE-Modelle schätzen:
#' 
fdz_install("fixest") 
library(fixest)
feglm(soc_med ~ palter |pnr, data = pend10, family = binomial)

fe_log1 <- feglm(soc_med ~ palter |pnr, data = pend10, family = binomial)
avg_slopes(fe_log1,by = TRUE, variables = "palter")