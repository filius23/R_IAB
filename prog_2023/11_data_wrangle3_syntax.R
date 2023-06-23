#' # Data Wrangling III {#wrang3}


## Datensätze verbinden ----
ids_df <-  data.frame(pnr = sample(1:9,4),
                       Bula = c(2,1,14,15))

set.seed(90459)
alo_bula <- data.frame(bundesland = seq(1:8),
                       Werte = sample(letters,size = 8) # mit sample() kann eine zufällige Auswahl getroffen werden 
                       )


ids_df
alo_bula
ids_df %>% left_join(alo_bula,by = c("Bula"="bundesland")) # merge -> alle entsprechungen für ids_df
ids_df %>% full_join(alo_bula,by = c("Bula"="bundesland")) # merge -> alle entsprechungen für ids_df
ids_df %>% inner_join(alo_bula,by = c("Bula"="bundesland")) # merge -> alle entsprechungen für ids_df

# anti_join als check:
ids_df %>% 
  anti_join(alo_bula,by = c("Bula"="bundesland"))  # merge -> alle entsprechungen für ids_df
  
# überblickstabelle
ids_df %>% 
  anti_join(alo_bula,by = c("Bula"="bundesland")) %>% 
  count(Bula)


# nur einzigarige kombinationen
pend %>% select(pnr,welle)
pend %>% select(pnr,welle) %>% distinct(pnr)

#' Ein sehr hilfreiche Checkmöglichkeit, die ich häufig verwende:

table(ids_df$Bula %in% alo_bula$bundesland)

#' ### [Übung](#join_ue)


# Reshape: `pivot_longer()` & `pivot_wider()` -------
bsp_df <- 
  data.frame(
    bula    = c("NRW","NDS"),
    alo2018 = c(2,2),
    alo2017 = c(1,1)
    )

bsp_df

##  pivot_longer: ins long shape -----

bsp_df %>% 
  pivot_longer(cols = c("alo2018","alo2017"),
               names_to = "year",
               values_to = "alo")

#' Mit `names_prefix = "alo"` können wir das `alo` direkt löschen lassen:
bsp_df %>% 
  pivot_longer(cols = c("alo2018","alo2017"),
               names_to = "year",
               values_to = "alo",
               names_prefix = "alo") %>% 
  mutate(year = as.numeric(year))

##  pivot_wider: ins wide shape -----
bsp_df2 <- 
  data.frame(land = c("NRW","NDS","NRW","NDS"),
             alo = c(2.1,1.8,2.4,2.2),
             alter = c("age_1825","age_1825","age_2630","age_2630"))
bsp_df2

bsp_df2 %>% 
  pivot_wider(names_from = alter,
              values_from = alo)

