
pend_ue5 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                            col_select = c("pnr","welle","PD0400")) %>% 
  filter(PD0400>0)


# Übung1 -----

# Überblick gewinnen
pend_ue5 %>% count(PD0400)
pend_ue5 %>% head()

# automatisch die Labels mit as_factor übernehmen
pend_ue5$PD0400_fct <- as_factor(pend_ue5$PD0400)

# factor() manuell erstellen
pend_ue5$PD0400_fct2 <- factor(pend_ue5$PD0400,
                               levels = 1:4, 
                               labels = c("Überhaupt nicht",
                                          "Eher nicht",
                                          "Eher schon",
                                          "Sehr"))


# anderer Fall: character
pend_ue5$PD0400_chr <- as.character(pend_ue5$PD0400_fct)

# Grafik erstellen
pend_ue5 %>% 
  ggplot(aes(y= PD0400_fct, fill = PD0400_fct)) + 
  geom_bar() + 
  scale_fill_viridis_d(option = "mako")

pend_ue5 %>% 
  ggplot(aes(y= PD0400_chr, fill = PD0400_chr)) + 
  geom_bar() + 
  scale_fill_viridis_d(option = "mako") 
  



