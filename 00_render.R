if(Sys.getenv("USERNAME") == "filse" ) .libPaths("D:/R-library4") 


  in1 <- list.files(pattern = "^\\d{2}.+\\.qmd")[-c(1,6,8,10,16,20:22)]
  paste0("./prog_draft/",gsub(x = in1,"qmd","R"))

knitr::purl(input = "12_apply_loop.qmd",output= "./prog_draft/12_function_map.R",documentation = 0)


rstudioapi::navigateToFile("./prog_prelim/11_data_wrangle3.R")

library(tidyverse)
library(officedown)
rmarkdown::render("./prog/14_Markdown_officedown.Rmd",output_format = "rdocx_document")
rmarkdown::render("./prog/14_Markdown_officedown.Rmd",output_format = "rdocx_document")


rmarkdown::render("./prog/14_Markdown_officedown.Rmd",output_format = "rdocx_document")

quarto::quarto_render("prereq/Vorab_Installation.qmd",output_format = "pdf")
rmarkdown::render("prereq/Vorab_Installation.Rmd",output_format = "pdf_document")

rstudioapi::navigateToFile("./prog/14_Markdown_ver02.Rmd")

# https://datasciencebook.ca/
# https://towardsdatascience.com/r-programming-a-study-guide-for-beginners-732de9542aa8?gi=dce1e602d5ab 
# https://datacarpentry.org/r-socialsci/
# https://kdpsingh.lab.medicine.umich.edu/lhs-610

# https://sta199-f22-1.github.io/
# https://github.com/mattansb/Practical-Applications-in-R-for-Psychologists/

# https://github.com/andrewheiss/2021-seacen
# https://crimebythenumbers.com/
# https://hhsievertsen.github.io/applied_econ_with_r/

# count() als Haupt table-funktion (die andern nur kurz zeigen)
# [] am Ende zeigen -> "übrigens"
# summarise auf Grafik einbauen?
# slice()/pull() einbauen ?
# Kennzahlenkapitel eindampfen -> Lagemaße usw nicht groß unterscheiden
# summarise -funktionen aus SMART-Syntax verwenden -> summary selbst bauen
# Variationskoeffizient function selber bauen als Beispiel




# - export vs save
# - factor Beispiel (as.character…)
# - ersetzen durch [ ]
# - Bedingungen Hintergrund: Auswahl mit T/F, dann logische Vektoren, zusammen führen
# - file_path
# <-
#   readClipboard()
# #
# - Histogramm, Boxplot aus https://modernstatisticswithr.com/thebasics.html#plotting-numerical-data
# - Fehlermeldungen aus https://modernstatisticswithr.com/thebasics.html#troubleshooting
# 


# 
# path1 <- "D:/oCloud/RFS/images/"
# path2 <- here::here("pic") %>% paste0(.,"/")
# 
# file.copy(from = paste0(path1,"102_Dateipfad_Win.png"),to = paste0(path2,"102_Dateipfad_Win.png"))
# file.copy(from = "D:/oCloud/RFS/images/rstudio-icon.png",to = paste0(path2,"rstudio-icon.png"))


quarto::quarto_render("01_intro.qmd")
quarto::quarto_render("03_desc.qmd")
quarto::quarto_preview("03_desc.qmd")