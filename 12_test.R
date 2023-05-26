






etbx <-  haven::read_dta("./data/BIBBBAuA_2018_suf1.0.dta",
                         col_select = c("S1","F518_SUF","m1202","az","zpalter","F1605e")) %>% 
  filter(F518_SUF < 99998, m1202 %in% 1:4, zpalter < 9999 ) %>% 
  mutate(ausb = factor(m1202, levels = 1:4, labels = c("ohne","dual/schul.","Aufst.","FH/Uni")),
         S1 = factor(S1,levels = 1:2,labels =c("m","w")))



# modell funktion ------
mod_function <- function(modx){
  mx <- lm(formula = modx,data = etbx)
  return(mx)
}

## modelle erstellen -----
mlst2 <- list(
  Modell1 = "F518_SUF ~ az",
  Modell2 = "F518_SUF ~ az + S1",
  Modell3 = "F518_SUF ~ az + S1 + m1202",
  Modell4 = "F518_SUF ~ az + S1 + m1202 + zpalter"
)

mod_function("F518_SUF ~ az")


mods2 <- lapply(mlst2,mod_function)
mods2$m1
mods2$m2
modelsummary::modelsummary(mods2)



## adhoc function
mods2 <- lapply(mlst2,function(modx){
  mx <- lm(formula = modx,data = etbx)
  return(mx)
})
  

# if in function ------

mod_function2 <- function(modx, tidy = T){
  mx <- lm(formula = modx,data = etbx)
  if(tidy == T) mx <- tidy(mx,conf.int = T)
  return(mx)
}

mod_function2("F518_SUF ~ az")
mod_function2("F518_SUF ~ az",tidy = F)


## Loop mit `for`

for(i in 1:8){
  print(i)
}

for(i in 1:8){
  etbx %>% slice(i) %>% print()
}

c("ausb","S1",)

for(i in 1:8){
  etbx %>% slice(i) %>% print()
}


## run models mit {purrr} -----
mods2 <- map(mlst2,~lm(formula = .x,data = sample_na))

