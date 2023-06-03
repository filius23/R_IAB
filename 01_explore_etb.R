if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4")
library(tidyverse)
library(marginaleffects)
library(LaCroixColoR)
knitr::opts_chunk$set(collapse = F)

# load etb -------------
pend <- haven::read_dta("./orig/PENDDAT_cf_W13.dta")

# distinct values ----

ndis <- 
    pend %>% 
      summarise(
      across(everything(), ~ length(unique(.x)), .names = "n0_{col}"),
      across(!matches("^n0"), ~ length(unique(.x[.x >= 0])), .names = "n1_{col}")
      ) %>%
      pivot_longer(cols = everything(),values_to = "ndis") %>%
      separate(name, into = c("n","var"),sep = "(?<=(0|1))_") %>% 
      pivot_wider(names_from = n,values_from = ndis) %>% 
      left_join(
        map_dfr(pend,~attributes(.x)$label) %>% 
          t(.) %>% data.frame("lab"=.) %>% 
          rownames_to_column(.,var = "var") )

ndis %>% filter(n0 %in% 3:4) %>% print(n=Inf)
ndis %>% filter(n1 %in% 3:4) %>% print(n=Inf)
ndis %>% filter(grepl("[B,b]ild",lab))

pend %>% count(erwerb)
pend %>% count(statakt)

ndis %>% slice_max(ndis,n = 12)

t1 <- table(pend$statakt)
names(t1)
names(t1) <- names(val_labels(pend$statakt))

table(as_factor(pend$statakt))


min_val <- 
  pend %>% 
    summarise(across(everything(),~min(.x,na.rm = T))) %>% 
    pivot_longer(cols = everything())


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
