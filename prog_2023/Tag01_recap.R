# Taschenrechner ---
2 + 2
5 * 5
6 * 4


# data.frame -----
dat1 <- data.frame(x= c(1,2,3,5),
                   y= c(1,2,3,5),
                   z = c("a","b","cc","c"))
dat1
dat1$x
dat1[1,]
dat1[,1]


# tidyverse ----
fdz_install("tidyverse")
library(tidyverse)

dat1 %>% filter(y == 1) %>% select(x)

# datensatz einlesen
library(haven)
pend <- read_dta("./orig/PENDDAT_cf_W13.dta") 

# Tabellen ----
table(pend$famstand)
table(pend$famstand) %>% prop.table(.)

pend %>% pull(famstand) %>%  table()

pend %>% count(famstand)

## Überblick mit `summary()` -----
pend$palter[pend$palter<0] <- NA
min(pend$palter)
min(pend$palter, na.rm = T)

# abgleiche mit NA führen immer zu NA als Ergebnis
2  == NA
2  > NA
2  < NA

summary(pend$palter)


pend %>% 
  summarise(
    min    = min(palter, na.rm = T),
    mean   = mean(palter, na.rm = T),
    median = median(palter, na.rm = T),
    max    = max(palter, na.rm = T),
    var    = var(palter, na.rm = T)
  )

## gruppiertes summary
pend %>% 
  summarise(
    min    = min(palter, na.rm = T),
    mean   = mean(palter, na.rm = T),
    median = median(palter, na.rm = T),
    max    = max(palter, na.rm = T),
    var    = var(palter, na.rm = T),
    .by = zpsex
  )


# Grafik -----
pend_u41 <-
  pend %>% 
  filter(welle == 13, bilzeit > 0, PA0445 >0) 

ggplot(data= pend_u41,
       aes(x=bilzeit,y=PA0445, color = as_factor(zpsex))) + 
  geom_point()

pend_u41 %>% 
  ggplot(aes(x=bilzeit,y=PA0445, color = as_factor(zpsex))) + 
  geom_point()


# plot2 <- 
  ggplot(data= pend_u41,
         aes(x=bilzeit,y=PA0445, color = as_factor(zpsex))) + 
  geom_point() +
  labs(title = "sprechender Titel",
       subtitle =  "Scatterplot",
       y = "Arbeitslosigkeitsdauer (in Monaten)",
       x = "Ausbildungsdauer (in Jahren)",
       color = "Geschlecht",
       caption = "Untertitel") +
  scale_color_brewer(palette = "Dark2")