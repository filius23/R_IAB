# ------------------- #
# Kapitel 2: data.frame
# Lösung
# ------------------- #

# Übung 1 -------------
dat2 <- data.frame(studs = c(14954,47269 ,23659,9415 ,38079),
                  profs = c(250,553,438 ,150,636),
                  prom_recht = c(FALSE,TRUE,TRUE,TRUE,FALSE),
                  gegr  = c(1971,1870,1457,1818,1995))

dat2

View(dat2)

dat2$uni <- c("FH Aachen","RWTH Aachen","Uni Freiburg","Uni Bonn","FH Bonn-Rhein-Sieg")

dat2

dat2$studs / dat2$profs

dat2
dat2$studs_profs <- dat2$studs / dat2$profs
dat2
# unis mit unter 10000 studierenden
dat2$uni
dat2$uni[dat2$studs < 10000] 
dat2[dat2$studs < 10000, "uni"] 

# Übung 2 -------------

# Installieren Sie die Pakete des tidyverse mit fdz_install("tidyverse")
fdz_install("tidyverse")# nur einmal ausführen
library(tidyverse)


# Verwenden Sie wieder den data.frame dat2 aus Übung 1
# Nutzen Sie filter, um sich nur die Unis mit unter 10000 Studierenden anzeigen zu lassen. 
filter(dat2,studs < 10000)
# Lassen Sie sich nur die dritte Zeile von dat2 anzeigen.
slice(dat2,3)
# Lassen Sie sich nur die Spalte gegr anzeigen.
select(dat2,gegr)
# Lassen Sie sich nur Zeilen der Hochschulen mit Promotionsrecht anzeigen.
filter(dat2,prom_recht == TRUE)
filter(dat2,prom_recht)

filter(dat2,!prom_recht)

dat2 %>% filter(prom_recht)

# c(0,1,1,1,0,0) %>% as.logical()
# Übung 3 ------------

# Lassen Sie sich nur Hochschulen anzeigen, die 1971, 1457 oder 1995 gegründet wurden - 
# und für diese Fälle nur den Namen und das Gründungsjahr.


dat2 %>% filter(gegr %in% c(1971, 1457, 1995)) %>% select(uni,gegr)

# Sortieren Sie den Datensatz entsprechend dieser Reihenfolge:
dat2 %>% arrange(uni)
dat2$uni_fct <- factor(dat2$uni, 
                       levels = c("RWTH Aachen","Uni Freiburg",
                                  "Uni Bonn","FH Aachen",
                                  "FH Bonn-Rhein-Sieg"))

levels(dat2$uni_fct)
dat2
dat2 %>% arrange(uni)
dat2 %>% arrange(uni_fct)


# Übung 4 ------------
pend <- read_dta("./orig/PENDDAT_cf_W13.dta")
head(pend)

nrow(pend)
names(pend)
dim(pend)

# Zeilen für pnr == 1000908201 
pend %>% 
  filter(pnr == 1000908201 )

#  nur Alter und Welle anzeigen --> select
pend %>% 
  filter(pnr == 1000908201 ) %>% 
  select(pnr,palter, welle)

# Wählen Sie alle Befragten aus, die älter als 60 (Alter: palter) sind legen Sie diese Auswahl unter ue_1960 ab.
ue_60 <- pend %>% filter(palter > 60)
# Wie müssen Sie den Befehl anpassen, sodass ue_1960 nur die Variablen pnr, hnr, welle, pintjahr und palter enthält?
ue_60 <- pend %>% 
  filter(palter > 60) %>% 
  select(pnr,hnr, welle, pintjahr, palter )

# Wie viele Spalten hat ue_1960? Wie viele Zeilen?
nrow(ue_60)
ncol(ue_60)


# Übung 5 ----------
# Exportieren Sie den data.frame mit den in der vorherigen Übung erstellten kleineren Datensatz-Version (ue_1960) als .Rdata-Datei.

saveRDS(ue_1960,file = "./data/ue_1960.RData")

# Laden Sie die eben exportierte .Rdata-Datei unter einem beliebigen Namen, bspw. ue_1960_neu.
ue_1960_neu <-  readRDS(file = "./data/ue_1960.RData")

# Hat alles geklappt? Vergleichen Sie das neu geladene mit dem ursprünglichen Objekt: identical(ue_1960,ue_1960_neu) - sind beide Objekte identisch?
identical(ue_1960,ue_1960_neu)  
