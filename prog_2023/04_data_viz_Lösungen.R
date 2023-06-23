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

ggplot(data= pend_u41,
       aes(x=bilzeit,y=PA0445, color = as_factor(zpsex))) + 
  geom_point()

pend_u41 %>% 
  ggplot(aes(x=bilzeit,y=PA0445, color = as_factor(zpsex))) + 
  geom_point()


plot2 <- 
  ggplot(data= pend_u41,
         aes(x=bilzeit,y=PA0445, color = as_factor(zpsex))) + 
    geom_point() +
    labs(title = "sprechender Titel",
         subtitle =  "Scatterplot",
         y = "Arbeitslosigkeitsdauer (in Monaten)",
         x = "Ausbildungsdauer (in Jahren)",
         caption = "Untertitel") +
    scale_color_brewer(palette = "Greens")

ggsave(plot2,filename = "./results/Übungsplot1.png",width = 16,height = 6)

# Übung 2-----
# Boxplot / Histogramm
pend_u42 <-
  pend %>% 
  filter(welle == 9, PG1270  >0) 

attributes(pend$PG1270)

ggplot(data = pend_u42, aes(y= PG1270)) + geom_boxplot()
pend_u42 %>% ggplot(aes(y= PG1270)) + geom_boxplot()

pend_u42 %>% ggplot(aes(y= PG1270 , color = as_factor(zpsex),
                        x = as_factor(zpsex))) + geom_boxplot()

pend_u42 %>% ggplot(aes(x= PG1270 , color = as_factor(zpsex))) + 
  geom_histogram()

pend_u42 %>% ggplot(aes(x= PG1270 , fill = as_factor(zpsex))) + 
  geom_histogram(position = position_dodge())

pend_u42 %>% ggplot(aes(x= PG1270 , fill = as_factor(zpsex))) + 
  geom_histogram(position = position_dodge()) +
  scale_fill_viridis_d()

pend_u42 %>% ggplot(aes(x= PG1270 , fill = as_factor(zpsex))) + 
  geom_histogram(position = position_dodge()) +
  scale_fill_manual(values = c("#0c7fa7","#ced72d"))

iab <- c("#ced72d","#93c782","#0c7fa7")
pend_u42 %>% ggplot(aes(x= PG1270 , fill = as_factor(zpsex))) + 
  geom_histogram(position = position_dodge()) +
  scale_fill_manual(values = iab)

pend_u42 %>% ggplot(aes(x= PG1270 , fill = as_factor(zpsex))) + 
  geom_histogram(position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2")

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

## Möglichkeit mit count() & geom_col() ----
pend_u43 %>% 
  count(PEO0400c, migration) 

pend_u43 %>% 
  count(PEO0400c, migration) %>% 
  ggplot(aes(y = n, x = PEO0400c, fill = as_factor(migration))) + 
  geom_col(position = position_dodge()) 
