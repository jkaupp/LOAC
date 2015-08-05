library(DBI)
library(RMySQL)
library(xlsx)
library(magrittr)
library(stringr)
library(dplyr)
library(rio)

read.files <- . %>%
{
  read_excel(paste0(base.dir,"CLA+/Queens CLA+.xlsx"),.,skip = 0)
}



# Establish MySQL Connection to LOAC Database ----
con <- dbConnect(MySQL(), group='LOAC')

# Read data for Courses ----
base.dir <- "~/ownCloud/Engineering Education Research/HEQCO/LOAC Project/" 

apsc.100 <- paste0(base.dir,"APSC 100/Course Information/APSC 100 Master List.xlsx") %>% 
  read.xlsx(.,"Master",stringsAsFactors = FALSE)

apsc.200f <- paste0(base.dir,"APSC 200/Course Information/APSC 200 Master List.xlsx") %>% 
  read.xlsx(.,"Master_F", stringsAsFactors = FALSE) 

apsc.200w <- paste0(base.dir,"APSC 200/Course Information/APSC 200 Master List.xlsx") %>% 
  read.xlsx(.,"Master_W", stringsAsFactors = FALSE) 

apsc.480 <- paste0(base.dir,"APSC 480/APSC 480.xlsx") %>% 
  read.xlsx(.,"Master", stringsAsFactors = FALSE)  

civl.471 <- paste0(base.dir,"CIVL 471/CIVL 471.xlsx") %>% 
  read.xlsx(.,"Master", stringsAsFactors = FALSE)  

geoe.447 <- paste0(base.dir,"GEOE 447/GEOE 447.xlsx") %>% 
  read.xlsx(.,"Master", stringsAsFactors = FALSE) 

mech.462 <- paste0(base.dir,"MECH 462/MECH 462.xlsx") %>% 
  read.xlsx(.,"Master", stringsAsFactors = FALSE) 

enph.460 <- paste0(base.dir,"PHYS 460/PHYS 460.xlsx") %>% 
  read.xlsx(.,"Master", stringsAsFactors = FALSE) 

FAS.files <-paste0(base.dir,"FAS") %>% 
  list.files(full.names=TRUE)

FAS.list <-lapply(FAS.files,read.xlsx, sheetName="Master", stringsAsFactors = FALSE)

df.FAS <- rbindlist(FAS.list, fill=TRUE)

master <- rbindlist(list(apsc.100,apsc.200f,apsc.200w,apsc.480,civl.471,geoe.447,mech.462,enph.460,df.FAS), fill=TRUE)

master$email %<>%
  tolower()

dram.400 <-paste0(base.dir,"FAS/DRAM 400.xlsx") %>% 
  import(.,sheet="Master")

dram.400$name %<>%
  str_sub(10,str_length(.))


# Read in the CLA+ Master Data ----
cla.sheets <- paste0(base.dir,"CLA+/Queens CLA+.xlsx") %>% 
  excel_sheets

cla.data <- lapply(cla.sheets[1:4],read.files) %>% 
  rbindlist

names(cla.data) %<>% 
  tolower()



# Write data to the student information table ----
dbWriteTable(con, name = 'student_info', value = dram.400, row.names = FALSE, append = TRUE)

# Write data to the cla+ table ----
dbWriteTable(con, name = 'cla_plus', value = cla.data, append = TRUE, row.names = FALSE)