library(magrittr)
library(dplyr)
library(readxl)

read.files <- . %>%
{
  read_excel(paste0(base.dir,"CLA+/Queens CLA+.xlsx"),.,skip = 0)
}


# Read data for Courses ----
base.dir <- "~/ownCloud/Engineering Education Research/HEQCO/LOAC Project/" 


# Read in the CLA+ Master Data ----
cla.sheets <- paste0(base.dir,"CLA+/Queens CLA+.xlsx") %>% 
  excel_sheets

cla.data <-lapply(cla.sheets[1:4],read.files)

cla.data[[1]][61:73] %<>% as.character
cla.data[[2]][61:73] %<>% as.character

cla.data %<>%
  bind_rows()

names(cla.data) %<>% 
  tolower()









