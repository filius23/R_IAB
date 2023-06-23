# ------------------- #
# Kapitel 6: Data Wrangling 2
# Lösung
# ------------------- #


# Übung 1 mutate/$newvar -------


## Erstellen Sie dat3 wie oben gezeigt aus dat1 und dat2 -----
dat1 <- data.frame(studs = c(19173,5333,15643), 
                   profs = c(322,67,210),
                   gegr  = c(1971,1830,1973),
                   prom_recht = rep(TRUE,3),
                   uni = c("Uni Bremen","Uni Vechta", "Uni Oldenburg"))
dat2 <- data.frame(studs = c(14954,47269 ,23659,9415 ,38079), 
                   profs = c(250,553,438 ,150,636),
                   prom_recht = c(FALSE,TRUE,TRUE,TRUE,FALSE),
                   gegr  = c(1971,1870,1457,1818,1995),
                   uni = c("FH Aachen","RWTH Aachen","Uni Freiburg","Uni Bonn","FH Bonn-Rhein-Sieg"))
dat3 <- bind_rows(dat1,dat2)


## Berechnen Sie das Betreuungsverhältnis (Studierende pro Professur) und legen Sie die Ergebnisse in einer neuen Variable studprofs ab. -----
## Nutzen Sie dazu mutate() oder ...$newvar <- 
dat4 <- 
  dat3 %>% mutate(studprofs = studs/profs)
dat4

dat3$studprofs <- dat3$studs/dat3$profs
dat3

##   Berechnen Sie außerdem das Betreuungsverhältnis an den Hochschulen relativ zum Mittelwert des Betreuungsverhältnisses (rel_studprofs). -----
dat3$studprofs_rel_to_mean <- dat3$studprofs - mean(dat3$studprofs)


dat3 %>% mutate(studprofs = studs/profs,
                studprofs_rel_to_mean = studprofs - mean(studprofs))


## Liegt das Betreuungsverhältnis über oder unter dem Mittelwert? Wie können Sie den Befehl anpassen, sodass die Variable rel_studprofs lediglich TRUE oder FALSE enthält anstelle der Zahlenwerte. -----


dat3 %>% mutate(studprofs = studs/profs,
                studprofs_rel_to_mean = studprofs - mean(studprofs) > 0)

dat3$studprofs_rel_to_mean <- dat3$studprofs - mean(dat3$studprofs) > 0

## Wandeln Sie rel_studprofs in eine Dummy-Variable mit 0/1 als Werten statt TRUE/FALSE -----

dat3$studprofs_rel_to_mean <- as.numeric(dat3$studprofs - mean(dat3$studprofs) > 0)

dat3$studprofs_rel_to_mean <- dat3$studprofs - mean(dat3$studprofs) > 0 %>% as.numeric(.)

dat3 %>% mutate(studprofs = studs/profs,
                studprofs_rel_to_mean = as.numeric(studprofs - mean(studprofs) > 0))

dat3 %>% mutate(studprofs = studs/profs,
                studprofs_rel_to_mean = studprofs - mean(studprofs) > 0 %>% as.numeric(.))

# Übung 2 group_by() -------

# berechnen Sie das Betreuungsverhältnis (studprofs) an den Hochschulen/Unis mit und ohne Promotionsrecht und fügen Sie dieses als neue Spalte ein.

dat3 %>% group_by(prom_recht) %>% mutate(studprofs = studs/profs )
dat3 %>% mutate(studprofs = studs/profs, .by = prom_recht)

dat3g <-  dat3 %>% group_by(prom_recht) %>% mutate(studprofs = studs/profs )
dat3g  %>% mutate(m_profs = mean(profs) )


# Wie müssen Sie ihren Code ändern, wenn die neue Variable das studprof-Verhältnis der Hochschule relativ zum Mittelwert des Betreuungsverhältnisses innerhalb mit und ohne Promotionsrecht wiedergeben soll?
  
dat3 %>% mutate(studprofs = studs/profs - mean(studs/profs), .by = prom_recht)

# mehrere Gruppierungsvariablen:
dat3 %>% mutate(studprofs = studs/profs - mean(studs/profs), .by = c(prom_recht,gegr))


# Übung 3 `across` -------
pend_small <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                              col_select = c("welle","zpsex","PEO0400a","PEO0400b","PEO0400c","PEO0400d")) %>% 
  filter(welle == 2) %>% 
  slice(1:15)

## Mittelwert für PEO0400a-d -----
pend_small

pend_small %>% 
  summarise(across(matches("PEO0400"),~mean(.x)))

pend_small %>% 
  summarise(across(matches("PEO0400"),~mean(.x,na.rm=T)))

# was wenn man nur PEO0400a und PEO0400b bearbeiten möchte? --> regex
pend_small %>% 
  summarise(across(matches("PEO0400(a|b)"),~mean(.x,na.rm=T)))

## nach Geschlecht ----
pend_small %>% 
  group_by(zpsex) %>% 
  summarise(across(matches("PEO0400"),~mean(.x,na.rm=T)))


pend_small %>% 
  summarise(across(matches("PEO0400"),~mean(.x,na.rm=T)), 
            .by = zpsex)


## var, N, und .names  -----
pend_small %>% 
  summarise(across(.cols = matches("PEO0400"),
                   .fns = list(mean = ~mean(.x,na.rm=T),
                               var = ~var(.x,na.rm=T)),
                   .names = "{.fn}.{.col}")
            )



## mean für gefilterten data.frame nachsehen -----

# mean PEO0400a if zpsex == 1

pend_small %>% 
  filter(zpsex == 1) %>% 
  summarise( MEAN =  mean(PEO0400a,na.rm=T) )

pend_small %>% 
  filter(zpsex == 1) %>% 
  pull(PEO0400a) %>% 
  mean()

mean(pend_small$PEO0400a[pend_small$zpsex == 1])


# Übung 4: function -----
pend_small
  
stdize <- function(x){
  stdx <- (x - mean(x,na.rm = T))/sd(x,na.rm = T)
  return(stdx)
}

pend_small %>% 
  mutate(PEO0400a = stdize(PEO0400a))

pend_small %>% 
  mutate(across(matches("PEO0400"),~stdize(.x),.names = "std_{.col}"))

# Übung 5: ifelse/case_when -----
pend_small2 <- haven::read_dta("./orig/PENDDAT_cf_W13.dta",
                               col_select = c("palter","PEO0400a","PEO0400b","PEO0400c","statakt"))  %>% 
  slice(5624:5640)
pend_small2

## ü50/u50 -----
pend_small2 %>% 
  mutate(over_under = ifelse(palter >= 50, "ü50", "u50"))

pend_small2 <- 
pend_small2 %>% 
  mutate(over_under = ifelse(palter >= 50, "ü50", "u50"))

pend_small2$over_under <- NULL

## u40 u55 ü55 ---------------
pend_small2 %>% 
  mutate(age_cat = case_when(palter < 40 ~ "u40",
                             palter < 50 ~ "u50",
                             palter >=  50 ~ "ü50")) 

# Kontrolle:
pend_small2 %>% 
  mutate(age_cat = case_when(palter < 40 ~ "u40",
                             palter < 50 ~ "u50",
                             palter >=  50 ~ "ü50"),
         age_cut = cut(palter,breaks = c(0,40,50,100)),
         age_cut_lab = cut(palter,breaks = c(0,40,50,100),
                           labels = c("u40","u50","ü50"))) %>% 
  count(age_cat,age_cut,age_cut_lab)



## ifelse PEO0400a ----
pend_small2 %>% mutate(PEO0400a = ifelse(PEO0400a<0,NA,PEO0400a))

## ifelse & `across()` auf PEO0400a,PEO0400b PEO0400c und statakt ------
pend_small2 %>% mutate(across(c("PEO0400a","PEO0400b","PEO0400c","statakt"), ~ifelse(.x<0,NA,.x)))  
pend_small2 %>% mutate(across(matches("PEO0400|statakt"), ~ifelse(.x<0,NA,.x)))  

#nicht überschreiben, sondern neue Variable anlegen:
pend_small2 %>% mutate(across(matches("PEO0400|statakt"), ~ifelse(.x<0,NA,.x),.names = "neu_{.col}"))  

# wie spreche ich NA an?
pend_small3 <- 
  pend_small2 %>% mutate(across(matches("PEO0400|statakt"), ~ifelse(.x<0,NA,.x)))  

pend_small3 %>% 
  mutate(across(matches("PEO0400|statakt"), ~ifelse(is.na(.x),99,.x)))  



