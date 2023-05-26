# test skript für paket installation und funktionstest
log <- file("./log/01_install_test.txt")  # hier pfad anpassen

sink(log)
# sink(log, type="message")
Sys.time()
start <- Sys.time()

pakete1 <- c("haven","readxl","tidyverse","labelled","easystats","modelsummary","ggeffects","flextable","pacman","fixest","effects")


inst_funct <- function(px){
  print(paste0(Sys.time(), ": " ,px, " wird installiert"))
  install.packages(px)
}

lapply(pakete1, inst_funct)
invisible(sapply(pakete1,library, character.only = TRUE))

pakete_mit_tidyverse_easystats <- c(pakete1,tidyverse::tidyverse_packages(),names(easystats::show_suggested()))
pkgs <- sapply(pakete_mit_tidyverse_easystats,function(x) {
  # paste0(x, " Version: ", pacman::p_ver(x))
  numeric_version(packageVersion(paste0(x)))
  }) 

pkgs <- lapply(pkgs,function(x) paste(x, collapse = "-"))

# Paket Versionen ----------------
cat("Paket Versionen")
t(data.frame(pkgs) )

# haven test ----------------
print("haven test")
path <- system.file("examples", "iris.dta", package = "haven")
head(read_dta(path))


# readxl test ------
datasets <- readxl::readxl_example("datasets.xlsx")
head(readxl::read_excel(datasets))



# dplyr Test ------------------ 
print("Tests")
mtcars %>% summarise(across(everything(),~mean(.x)))


mtcars %>%
  rowwise() %>%
  mutate(
    sum = sum(c_across(1:5)),
    sd = sd(c_across(1:5))
  )


# forcats test ----
print("forcats")
fctx <- factor(c("apple", "bear", "banana", "dear"))
fct_recode(fctx, fruit = "apple", fruit = "banana")


# purrr & modelsummary Test ------- #####
print("Modeltest")

models <-list(
   mod1='mpg~hp', 
   mod2='mpg~hp+qsec', 
   mod3='mpg~hp+qsec+am'
   )


models %>% map_dfr(.,~broom::tidy(lm(formula=.x,data=mtcars)),.id = "mod") 
print("")
print("")
print("")
print("")

# ggeffects test ------
library(ggeffects)
library(effects)
ggeffect(lm(mpg~hp+qsec,mtcars))

paste("Start: ", start, "Ende: ", Sys.time())
sink()
