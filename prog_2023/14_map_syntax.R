# Weitere Visualisierungen mit `{ggplot2}` {#viz2}
library(tidyverse)
library(patchwork)
library(sf)

# Karten ("Choroplethen") {#map} ------
# dpath17 <- "C:/Users/FilserA001.IAB/Downloads/vg250_2017-01-01.utm32s.shape.ebenen/vg250_ebenen/"
dpath17 <- "N:/Ablagen/D01700-Quickablage/Filser/R_2023/orig/vg250_01-01.utm32s.shape.ebenen/vg250_2017-01-01.utm32s.shape.ebenen/vg250_ebenen"
kreis <- sf::read_sf(dpath17, layer="VG250_KRS")

ggplot(kreis) + 
    geom_sf(size = .1,aes(fill = as.numeric(AGS)))  +
    scale_fill_viridis_c(option = "mako", name = "AGS") +
    labs(title = "Landkreise nach Gebietsstand 2017") +
    theme_minimal()


#' + `VG250_LAN`: Bundesländer (2-stellige AGS)
#' + `VG250_KRS`: Kreise und kreisfreie Städte (5-stellige AGS)
#' + `VG250_GEM`: Städte und Gemeinden (8-stellige AGS)
#' Wenn wir also die Bundesländer laden möchten gehen wir wie folgt vor:
lan17 <- sf::read_sf("C:/Users/FilserA001.IAB/Downloads/vg250_2017-01-01.utm32s.shape.ebenen/vg250_ebenen/",
                     layer="VG250_LAN")

#' 
#' Das Objekt `lan17` können wir nun für einen `ggplot()`-Befehl verwenden.
#' `lan17` enthält außerdem die Seegebiete, diese können wir mittels eines `filter()`-Befehls auf die Landgebiete einschränken (`GF` = 4):

ggplot(lan17) + 
    geom_sf(size = .1)  +
    labs(title = "Bundesländer nach Gebietsstand 2017") +
    theme_minimal()

ggplot(lan17 %>% filter(GF==4)) + 
    geom_sf(size = .1)  +
    labs(title = "Bundesländer nach Gebietsstand 2017") +
    theme_minimal()




#' Möchten wir nun die Bundesländer bspw. nach der Arbeitslosenquote einfärben, müssen wir diese an die Daten heranspielen. 
#' Der Einfachheit halber simuliere ich hier die Werte:
alo_df <- 
  data.frame(ags = unique(lan17$AGS),
             alq = sample(seq(.03,.095,.001) ,size = 16,replace = T))
head(alo_df)



lan17 %>% 
  filter(GF==4) %>% 
  left_join(alo_df,by = join_by("AGS"=="ags")) %>% 
  select(AGS,GEN,alq) %>% 
  head()

#' Die Syntax für den eigentlichen Plot ist dann wieder wie für jeden anderen `ggplot()` auch - mit `fill =` können wir eine Flächenfarbe angeben und mit `scale_fill_...` eine Farbpalette auswählen:
library(scico)
lan17 %>% 
  filter(GF==4) %>% 
  left_join(alo_df,by = join_by("AGS"=="ags")) %>% 
  ggplot() + 
      geom_sf(size = 5.1, aes(fill = alq))  +
      labs(title = "Arbeitslosenquote für Bundesländer",
           subtitle = "(keine echten Daten)",
           fill = "Arbeitslosen-\nquote") +
      scale_fill_scico(palette = "oslo") + # setzt scico Paket voraus
      theme_minimal()

# ohne koordinatensystem --> theme_void()
lan17 %>% 
  filter(GF==4) %>% 
  left_join(alo_df,by = join_by("AGS"=="ags")) %>% 
  ggplot() + 
      geom_sf(size = 5.1, aes(fill = alq))  +
      labs(title = "Arbeitslosenquote für Bundesländer",
           subtitle = "(keine echten Daten)",
           fill = "Arbeitslosen-\nquote") +
      scale_fill_scico(palette = "oslo") + # setzt scico Paket voraus
      theme_void()


# legend verschieben ----
lan17 %>% 
  filter(GF==4) %>% 
  left_join(alo_df,by = join_by("AGS"=="ags")) %>% 
  ggplot() + 
      geom_sf(size = 5.1, aes(fill = alq))  +
      labs(title = "Arbeitslosenquote für Bundesländer",
           subtitle = "(keine echten Daten)",
           fill = "Arbeitslosen-\nquote") +
      scale_fill_scico(palette = "oslo") + # setzt scico Paket voraus
      theme_void() +
      theme(legend.position="top")
  

# bearbeiten mit den dplyr-funktionen:
lan17_15länder <- 
  lan17 %>% 
  filter(GF==4) %>%
  mutate(sath = case_when(as.numeric(AGS)>13 ~ "17",  TRUE ~ AGS)) %>% # gleiche ID für SN, SA, TH
  summarise(geometry = st_union(geometry),.by = sath)
  


ggplot(lan17_15länder) + 
  geom_sf(size = .1, aes(fill = as.numeric(sath)) )  +
  scale_fill_scico(palette = "oslo") + # setzt scico Paket voraus
  theme_minimal()
