

siab <- haven::read_dta("P:/Datensaetze/_Endprodukte/SIAB/SIAB_7521_v1/SIAB_7521_v1.dta",
                        n_max = 100)

head(siab)
siab <- siab %>% select(persnr_siab,matches("epi$"))
siab %>% 
  mutate(
  stichtag = between(x = 
                       year(begepi) %>% paste(.,"-06-30") %>% ymd(),
                       # ymd(paste(year(begepi),"-06-30")),
                     begepi,endepi)  
  ) %>%
  slice(50:60)
  
table(is.na(siab$endepi))
table(is.na(siab$begepi))