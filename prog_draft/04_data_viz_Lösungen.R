library(tidyverse)
pend <-
  haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                  col_select = c("zpsex","welle","bilzeit","PA0445","PG1270")
  )

# Übung1 ---------
# Scatterplot 

pend_u41 <-
  pend %>% 
  filter(welle == 13, bilzeit > 0, PA0445 >0) 

pend_u41 %>% 
  ggplot(aes(x=bilzeit,y=PA0445, color = as_factor(zpsex))) + geom_point()



# Übung 2-----
# Boxplot / Histogramm
pend_u42 <-
  pend %>% 
  filter(welle == 9, PA0445 >0) 

pend_u42 %>% ggplot(aes(y= PA0445, color = as_factor(zpsex),
                        x = as_factor(zpsex))) + geom_boxplot()

pend_u42 %>% ggplot(aes(x= PA0445, color = as_factor(zpsex))) + geom_histogram()
pend_u42 %>% ggplot(aes(x= PA0445, fill = as_factor(zpsex))) + geom_histogram(position = position_dodge())

# Übung 3 ----
# Säulen-/Balkendiagramm

pend_u43 <-
  pend %>% 
  filter(welle == 11, PEO0400c >0, migration > 0) 

pend_u43 %>% 
  ggplot(aes(x = as_factor(PEO0400c), fill = as_factor(migration))) + 
  geom_bar(position = position_dodge())
pend_u43 %>% 
  ggplot(aes(y = as_factor(PEO0400c), fill = as_factor(migration))) + 
  geom_bar(position = position_dodge())

pend_u43 %>% 
  ggplot(aes(y = as_factor(PEO0400c), fill = as_factor(migration))) + 
  geom_bar(position = position_dodge()) 
