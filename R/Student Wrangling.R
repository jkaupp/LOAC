library(magrittr)
library(stringr)
library(dplyr)
library(readxl)
library(data.table)

# Read data for Courses ----
base.dir <- "~/ownCloud/Engineering Education Research/HEQCO/LOAC Project/" 

apsc.100 <- paste0(base.dir,"APSC 100/Course Information/APSC 100 Master List.xlsx") %>% 
  read_excel(.,"Master")

apsc.200f <- paste0(base.dir,"APSC 200/Course Information/APSC 200 Master List.xlsx") %>% 
  read_excel(.,"Master_F") %>% 
  mutate(team = as.character(team))

apsc.200w <- paste0(base.dir,"APSC 200/Course Information/APSC 200 Master List.xlsx") %>% 
  read_excel(.,"Master_W") %>% 
  mutate(team = as.character(team))

apsc.480 <- paste0(base.dir,"APSC 480/APSC 480.xlsx") %>% 
  read_excel(.,"Master")  %>% 
  mutate(team = as.character(team))

civl.471 <- paste0(base.dir,"CIVL 471/CIVL 471.xlsx") %>% 
  read_excel(.,"Master")  

geoe.447 <- paste0(base.dir,"GEOE 447/GEOE 447.xlsx") %>% 
  read_excel(.,"Master") %>% 
  mutate(team = as.character(team))

mech.462 <- paste0(base.dir,"MECH 462/MECH 462.xlsx") %>% 
  read_excel(.,"Master") %>% 
  mutate(team = as.character(team))

enph.460 <- paste0(base.dir,"PHYS 460/PHYS 460.xlsx") %>% 
  read_excel(.,"Master") %>% 
  mutate(team = as.character(team))

FAS.files <-paste0(base.dir,"FAS") %>% 
  list.files(full.names=TRUE)

FAS.list <-lapply(FAS.files, read_excel, sheet="Master")

FAS.list[[1]]$program %<>% as.character()
FAS.list[[2]]$program %<>% as.character()
FAS.list[[3]]$program %<>% as.character()
FAS.list[[4]]$program %<>% as.character()

FAS.list[[1]]$plan %<>% as.character()
FAS.list[[2]]$plan %<>% as.character()
FAS.list[[3]]$plan %<>% as.character()
FAS.list[[4]]$plan %<>% as.character()



df.FAS <- bind_rows(FAS.list) %>% 
  mutate(team = as.character(team))

master <- bind_rows(list(apsc.100,apsc.200f,apsc.200w,apsc.480,civl.471,geoe.447,mech.462,enph.460,df.FAS))

master$email %<>%
  tolower()

master %<>%
  data.table %>% 
  .[(subject=="DRAM" & course=="400"), name := str_sub(name, 10, str_length(name))] %>% 
  tbl_df()





  