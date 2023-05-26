if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4")
library(tidyverse)
library(marginaleffects)
library(LaCroixColoR)
knitr::opts_chunk$set(collapse = F)

# load etb -------------

etb <- haven::read_dta("D:/Datenspeicher/BIBB_BAuA/BIBBBAuA_2018_suf1.0.dta") 

etb <- etb %>% 
  mutate(
         F518_SUF = ifelse(F518_SUF >= 99998,NA,F518_SUF),
         inc100 = F518_SUF/100,
         across(matches("605|1503|103"),~ ifelse(.x>2,NA,.x)),
         
         outside = 2-F605,
         gender_fct = factor(S1),
         sick = 2-F1503 ,
         schicht = 2-F209,
         wunschb = 2-F103,
         
         
         m1202_fct = factor(m1202,levels = c(-1,1:4), 
                          labels = names(attributes(m1202)$labels) %>% 
                            substr(.,1,5) %>% str_squish()),
         m1202_fct = fct_relevel(m1202_fct,"Ohne"))


etb_small <- etb %>% select(intnr,az,S1,S3,S2_j,zpalter,Stib,Bula,m1202,matches("F209")) %>% as.data.frame()
head(etb_small)
# write_delim(x = etb_small,"./data/BIBBBAuA_2018_small.csv",delim = ";")

etb %>% count(F103)
xtabs(~S3,etb)




etb 




# distinct values ----

etb <- haven::read_dta("D:/Datenspeicher/BIBB_BAuA/BIBBBAuA_2018_suf1.0.dta") 
ndis <- 
  etb %>% summarise(across(everything(), ~length(unique(.x)  )) )  %>% 
  t(.) %>% data.frame(ndis = .) %>% rownames_to_column(.,var = "var") %>% janitor::clean_names() %>% tibble() %>% 
  left_join(
    map_dfr(etb,~attributes(.x)$label) %>% 
      t(.) %>% data.frame("lab"=.) %>% 
      rownames_to_column(.,var = "var") )

ndis %>% filter(ndis %in% 2:4) %>% print(n=Inf)
ndis %>% filter(ndis == 5) %>% print(n=Inf)

ndis %>% slice_max(ndis,n = 12)


etb %>% select(matches("F1450_")) %>% 
  map_dfr(.,~attributes(.x)$label) %>% 
  t(.) %>% data.frame("lab"=.) %>% 
  rownames_to_column(.,var = "var") 



etb %>% count(F1605e,S1)

etb %>% 
  count(Mig,gkpol) %>% 
  ggplot(.,aes(x= gkpol,y= n, fill = factor(Mig))) + 
  geom_col()



ndis %>% filter(grepl("[A,a]lter",lab)) %>% print(n=Inf)



etb %>%   count(F411_01)


table(etb18$F230_02)
etb18 %>% count(F100_kldb2010_BOF) %>% add_tally()
attributes(etb18$F230_02)

etb_reg1 <- etb18 %>% filter(F200 <90, F518_SUF < 99990,intnr< 200000)
ggplot(etb_reg1,aes(x= F200,y = F518_SUF)) + 
  geom_smooth(method = "lm", se = F) +
  geom_smooth(data = etb_reg1 %>% filter(F518_SUF<20000),
              color = "sienna1",
              method = "lm", se = F) +
  geom_point() + 
  geom_text(data = etb_reg1 %>% filter(F518_SUF>60000),
            aes(label = intnr, x= F200+ 5), color = "sienna")
attributes(etb18$F518_SUF)

etb18 %>% filter(F200 <90, F518_SUF < 99990,F518_SUF>60000,intnr< 200000) %>% select(intnr) %>% slice_min(intnr,n = 10)




# model tests ----

m9 <- lm(outside ~ m1202_fct* gender_fct + az,data = etb)
summary(m9)

mfx9 <- marginaleffects(m9,newdata = datagrid(m1202_fct = levels(etb$m1202_fct),
                                              # zpalter = seq(20,60,2)
                                              gender_fct  = factor(c(1,2))
                                              )) 


mfx9 %>% 
  data.frame(.) %>% 
  ggplot(.,aes(x=gender_fct,y=dydx  ,color = m1202_fct)) + 
  geom_point(aes(color = m1202_fct)) +
  scale_color_viridis_d()






summary(etb$az)

m8 <- glm(outside ~ m1202_fct*inc100 + I(inc100^2), family = "binomial", data = etb %>% filter(S1==1) )
summary(m8)

mfx8 <- marginaleffects(m8,newdata = datagrid(m1202_fct = levels(etb$m1202_fct),
                                              # zpalter = seq(20,60,2)
                                              inc100 = seq(5,60,2)
                                              )) 

mfx8 %>% 
  filter(term == "m1202_fct", !grepl("kein",contrast)) %>% 
  ggplot(.,aes(x= inc100, y= dydx,color = contrast)) + 
  geom_point() +
  geom_errorbar(aes(ymin = conf.low,ymax=conf.high)) +
  labs(title = "Wahrscheinlichkeitsdifferenz für outside = 1 vs. ohne Ausb.",
       y = "Marginaler Effekt", x =  "Alter", color = "") +
  scale_color_manual(values = lacroix_palette("PeachPear")[c(1,5,6)])
