if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4")
if(Sys.getenv("USERNAME") == "filse" ) path <- "D:/oCloud/RFS/"
library(tidyverse)
library(patchwork)
knitr::opts_chunk$set(message = F)
# https://wtoivo.github.io/SGSSS-data-viz-workshop/bar-plots.html
etb18 <- haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta")
etb18$m1202[etb18$m1202<0] <- NA
kt <- xtabs(~S1+m1202, data = etb18) 

etb18$zpalter[etb18$zpalter>100] <- NA

xtabs(~S1+m1202, data = etb18)

tab_df <- data.frame( kt ) %>% 
  left_join(enframe(labelled::val_labels(etb18$m1202)) %>% mutate(value =factor(value)),
            by = c("m1202"="value")) %>% 
  mutate(name = fct_reorder(name,as.numeric(as.character(m1202))))
 

ggplot(data = tab_df , 
       aes(x = str_wrap(name,20), y = Freq, fill = S1 )) + 
  geom_col(  position=position_dodge()) + 
  theme_minimal() + 
  scale_fill_manual(values = c("navajowhite","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen")) +
  labs(title = "Ausbildungsabschlüsse nach Geschlecht",
       subtitle = "Absolute Häufigkeiten",
       caption = "Quelle: ETB 2018",
       x = "Ausbildung",
       y = "Absolute Häufigkeit",
       fill = "Geschlecht" ) 

## ggplot(data = datensatz, aes(x = var1, y = var2, fill = var3)) +
##   geom_col() +
##   labs(title= "Titel", subtitle = "Untertitel") +
##   theme_bw()

etb18 %>% count(S1,m1202) %>% filter(!is.na(m1202))

tab_df <- etb18 %>% count(S1,m1202) %>% filter(!is.na(m1202))

labelstab_df$m1202




ggplot(data = tab_df, aes(x = m1202, y = Freq)) + geom_col() + theme_gray(base_size = 8) + theme(aspect.ratio = 1)

## ggplot(data = tab_df, aes(x = m1202, y = Freq, color= S1)) + geom_col()
## ggplot(data = tab_df, aes(x = m1202, y = Freq, fill = S1)) + geom_col()

step3 <- ggplot(data = tab_df, aes(x = m1202, y = Freq, color = S1)) + geom_col() + theme_gray(base_size = 8)  + theme(aspect.ratio = .75, plot.margin = unit(c(.1,.1,.1,.1), "cm"))
step4 <- ggplot(data = tab_df, aes(x = m1202, y = Freq, fill = S1)) + geom_col() + theme_gray(base_size = 8) + theme(aspect.ratio = .75, plot.margin = unit(c(.1,.1,.1,.1), "cm")) 
step3 + step4
## cowplot::plot_grid(step3,step4,nrow = 1)

## ggplot(data = tab_df, aes(x = m1202, y = Freq, color = sex , fill = S1)) + geom_col()

## ggplot(data = tab_df, aes(x = m1202, y = Freq, fill = S1)) +
##   geom_col(position=position_dodge())
ggplot(data = tab_df, aes(x = m1202, y = Freq, fill = S1)) + geom_col(position=position_dodge()) + 
  theme_gray(base_size = 8) + theme(aspect.ratio = 1)

## ggplot(data = tab_df, aes(x = m1202, y = Freq, fill = S1)) +
##   geom_col(position=position_dodge()) +
##   scale_fill_manual(values = c("navajowhite","navy"))

## ggplot(data = tab_df, aes(x = m1202, y = Freq, fill = S1)) +
##   geom_col(position=position_dodge()) +
##   scale_fill_manual(values = c("navajowhite","navy"),
##                     breaks = c(1,2), labels = c("Männer", "Frauen") )

## out.height= "75%", out.width = "75%",
step5 <- ggplot(data = tab_df, aes(x = m1202, y = Freq, fill = S1)) + 
  geom_col(position=position_dodge()) + theme_gray(base_size = 8) +
  scale_fill_manual(values = c("navajowhite","navy") )  + 
  theme(aspect.ratio = .75, plot.margin = unit(c(.1,.1,.1,.1), "cm")) +
  coord_fixed(ratio=.005)
step6 <-  ggplot(data = tab_df, aes(x = m1202, y = Freq, fill = S1)) + 
  geom_col(position=position_dodge())+ theme_gray(base_size = 8)  +
  scale_fill_manual(values = c("navajowhite","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) + 
  theme(aspect.ratio = .75, plot.margin = unit(c(.1,.1,.1,.1), "cm")) +
  coord_fixed(ratio=.0075)
step5 + step6
## cowplot::plot_grid(step5, step6, nrow = 1)

ggplot(data = etb18, aes(x = m1202, fill = factor(S1))) + 
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values = c("navajowhite","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) +
  labs(title = "Familienstand",
       subtitle = "Absolute Häufigkeiten pro Geschlecht",
       caption = "Quelle: Allbus 2016",
       x = "Familienstand",
       y = "Absolute Häufigkeit",
       fill = "Geschlecht" ) +
  theme_minimal()

ggplot(data = tab_df , aes(x = m1202, y = Freq, fill = S1 )) + 
  geom_point(shape = 21, size = 1.25) + 
  theme_minimal() + 
  scale_fill_manual(values = c("navajowhite","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen")) +
  labs(title = "Familienstand", subtitle = "Absolute Häufigkeiten pro Geschlecht",
       caption = "Quelle: Allbus 2016",x = "Familienstand",
       y = "Absolute Häufigkeit",fill = "Geschlecht" ) 

ggplot(data = tab_df , aes(x = m1202, y = Freq, fill = S1 )) + 
  geom_col(  position=position_dodge()) + 
  theme_minimal() + 
  scale_fill_manual(values = c("navajowhite","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen")) +
  labs(title = "Familienstand",subtitle = "Absolute Häufigkeiten pro Geschlecht",
       caption = "Quelle: Allbus 2016",x = "Familienstand",
       y = "Absolute Häufigkeit",fill = "Geschlecht" ) +
  coord_flip()


ak <- readr::read_delim(paste0(path,"allbus_kumuliert.csv"), delim = ";", col_types = cols(.default = col_double()))

bp_ann_df <- 
  filter(ak,hs16>0) %>% 
  mutate_at(vars(hs16),~ifelse(.<0,NA,.)) %>% 
  summarise(q25 = quantile(hs16,probs = .25),
            q50 = quantile(hs16,probs = .5),
            q75 = quantile(hs16,probs = .75),
            whis1 = q25 - 1.5*(q75-q25) + .5 ,
            whis2 = q75 + 1.5*(q75-q25) - .5) %>% 
  mutate_all(~if_else(. < 0,0,.)) %>% 
  pivot_longer(cols=everything(), values_to = "hs16") %>% 
  mutate(xend1 = ifelse(grepl("whis",name),.015,.4),
         name = case_when(name == "q25" ~ "1. Quartil (25% Grenze)",
                          name == "q50" ~ "Median (50% Grenze)",
                          name == "q75" ~ "3. Quartil (75% Grenze)",
                          name == "whis1" ~ "unterer Whisker",
                          name == "whis2" ~ "oberer Whisker"),
         x = 1)

bp_ann_ausr <- filter(ak,!between(hs16,146,197),hs16>0) %>% select(hs16) %>% mutate(name="Ausreißer",x=1) %>% 
  distinct() %>% group_by(aus=hs16>146) %>% 
  mutate(hs16m=mean(hs16) %>% if_else(.<144,140,.))

  
ggplot(filter(ak,hs16>0), aes(x = 0, y = hs16)) + 
  geom_boxplot() + 
  geom_label(data = bp_ann_df, aes(x = x-.45, y = hs16, label = name), hjust = 0, label.size = .0,fontface="italic", size = 5.25) +
  geom_label(data = bp_ann_ausr, aes(x = x-.45, y = hs16m, label = name), hjust = 0, label.size = .0,fontface="italic", size = 5.25) +
  geom_segment(data = bp_ann_df, aes(x = x-.451, xend = xend1, yend = hs16, y = hs16), color = "#172869",
               lineend = 'butt', linejoin ='bevel',arrow = arrow(length = unit(.025,"npc"), type = "closed")) +
  geom_segment(data = bp_ann_ausr, aes(x = x-.451, xend = .015, yend = hs16, y = hs16m), color = "#172869",
               lineend = 'butt', linejoin ='bevel',arrow = arrow(length = unit(.01,"npc"), type = "closed")) +
  labs(color = "", # legenden-label auf leer
       y = "", # y-Achse labeln
       x="")+ # x-Achse labeln
  theme_void() + 
  theme(axis.text.x = element_blank()) +
  expand_limits(x = c(0,1.1))


## library(ggplot2)
## ggplot(data = etb18, aes(y = az)) + geom_boxplot()

## ggplot(data = etb18, aes(y = az)) + geom_boxplot() +
##   facet_wrap(~S1) + # nach geschlecht aufsplitten
##   theme_minimal()

library(ggplot2)
library(patchwork)

box_plot <- 
  ggplot( etb18, aes(y = az, x = as.character(S1))) + 
  geom_boxplot(aes(color = as.character(S1))) + 
  # facet_wrap(~S1) +
  theme_minimal() +
  labs(color = "",
       y = "Arbeitszeit",
       x="")+
  scale_color_manual(values = c("#8F92A1","#172869"),
                     breaks = c("1","2"),
                     labels = c("M", "F"), guide = F) 

box_zoom <- box_plot +
  coord_cartesian(ylim = c(25,50)) 

box_plot + box_zoom

ggplot(data = etb18, aes(x = az, fill = as.character(S1))) + 
  geom_histogram() + 
  facet_wrap(~S1) + # nach geschlecht aufsplitten
  theme_minimal()
ggplot(data = etb18, aes(x = az)) + 
  geom_histogram(aes(fill = factor(S1)), alpha = .5, color = "grey50") + 
  theme_minimal()

ggplot(data = etb18, aes(x = az)) + 
  geom_histogram(aes(fill = factor(S1)), color = "grey50",position = position_dodge()) + 
  scale_fill_viridis_d(option = "E",labels = c("Männer","Frauen")) +
  theme_minimal()

ggplot(data = etb18, aes(x = az)) + 
  geom_density(aes(fill = factor(S1)), alpha = .5) + 
  scale_fill_viridis_d(option = "E",labels = c("Männer","Frauen")) +
  labs(fill = "") +
  theme_minimal()

## a9410 <- ak[ak$year == jahr1 | ak$year == jahr2,]

## ggplot(data = ..., aes(...) )+
##   geom_boxplot() +
##   facet_wrap(~year+eastwest, nrow = 1) + ...

## library(ggplot2)
## ggplot(a14, aes(x = "", y = di05)) +
##   geom_boxplot(aes(color = as.character(eastwest))) +
##   facet_wrap(~eastwest) +
##   theme_minimal() +
##   labs(color = "",
##        y = "HH- Einkommen",
##        x="")+
##   scale_color_manual(values = c("lightblue3","navy"),
##                      breaks = c(1,2),
##                      labels = c("west", "ost"))

ggplot(data = tab_df, aes(x = m1202, y = Freq, fill = S1)) + 
  geom_col(position=position_dodge()) +
  scale_fill_manual(values = c("dodgerblue4","sienna1"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) +
  labs(title = "Familienstand",subtitle = "Absolute Häufigkeiten pro Geschlecht",
       caption = "Quelle: Allbus 2016",x = "Familienstand",
       y = "Absolute Häufigkeit",fill = "Geschlecht" ) + theme_minimal()

ggplot(data = tab_df, aes(x = m1202, y = Freq, fill = S1)) +
  geom_col(position=position_dodge()) +
  scale_fill_brewer(palette = "YlGnBu",
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) +
  labs(title = "Familienstand",subtitle = "Absolute Häufigkeiten pro Geschlecht",
       caption = "Quelle: Allbus 2016", x = "Familienstand",
       y = "Absolute Häufigkeit",fill = "Geschlecht" ) + theme_minimal()

ggplot(data = tab_df, aes(x = m1202, y = Freq, fill = S1)) + 
  geom_col(position=position_dodge()) +
  scale_fill_manual(values = c("navajowhite","navy"),
                    breaks = c(1,2), labels = c("Männer", "Frauen") ) +
  labs(title = "Familienstand",subtitle = "Absolute Häufigkeiten pro Geschlecht",
       caption = "Quelle: Allbus 2016",x = "Familienstand",
       y = "Absolute Häufigkeit",fill = "Geschlecht" ) + 
  theme_dark()

knitr::include_graphics("./pic/104_decision.png")
