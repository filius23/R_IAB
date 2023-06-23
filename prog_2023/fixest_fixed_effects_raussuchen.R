# wie kann ich aus einem fixest-FE-modell die fixed effects
# in den Datensatz übernehmen?
library(tidyverse)
library(fixest)
#daten laden
pend <- haven::read_dta("./orig/PENDDAT_cf_W13.dta") %>% 
  filter(netges > 0 , palter > 0, azges1 > 0)

# FE modell schätzen
fe_mod1 <- feols(netges ~ palter + I(palter^2) | pnr, data = pend) # pnr als FE-Variable
fe_mod1

# mit fixef() können die FEs aus einem fixest-Objekt extrahiert werden
pnr_fe <- fixef(fe_mod1)$pnr

#das ergebnis ist ein named vector:
class(pnr_fe)
# Namen entsprechen den Werten der FE-Variablen
names(fixef(fe_mod1)$pnr[1:10]) 
# check:
table(names(fixef(fe_mod1)$pnr[1:10]) %in% pend$pnr)

# das jetzt zu einem data.frame umformtieren --> mit enframe()
enframe(fixef(fe_mod1)$pnr)

# als data.frame objekt ablegen und gleich noch pnr als numeric umformatieren --> für späteren join()
fixed_eff_pnr <- 
  enframe(fixef(fe_mod1)$pnr,
          name = "pnr",
          value = "pnr_fixed_effect") %>% 
  mutate(pnr = as.numeric(pnr))

fixed_eff_pnr
pend %>% 
  select(penr,welle, palter,netges) %>% 
  left_join(fixed_eff_pnr,by = "pnr")
# in pend hinzufügen
pend_mitfe <- 
  pend %>%  
  select(pnr,welle, palter,netges) %>% # nur ein paar variablen aus pend-Datensatz behalten
  left_join(fixed_eff_pnr,by = "pnr")

pend_mitfe

