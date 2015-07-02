library(rio)
library(magrittr)
library(dplyr)


path <- "/Users/Jake/ownCloud/Engineering Education Research/HEQCO/LOAC Project/APSC 100/TLO 2014-2015"

tlo <- read.csv(paste0(path,"/APSC 100 Mod 3 TLO 2014-2015.csv"))

gender <-import(paste0(path,"/ReportforJK_June5.xlsx")) %>% 
  plyr::rename(c("ID"="student_number"))

matched <- left_join(tlo,gender)