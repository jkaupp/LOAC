library(DBI)
library(RMySQL)
library(magrittr)
library(dplyr)
library(stringr)
library(rio)
library(tidyr)

TLO <- dbReadTable(con,"TLO")
student_info <- dbReadTable(con,"student_info")
cla_plus <- dbReadTable(con,"cla_plus")

data.dir<-"/Users/Jake/ownCloud/Engineering Education Research/HEQCO/LOAC Project/VALUE/"

fy.value <- paste0(data.dir,"LOAC 1 VALUE.xlsx") %>% 
  import(sheet="Master") %>% 
  rename(studentid = Student_ID)

artifacts <- paste0(data.dir,"LOAC 1 VALUE.xlsx") %>% 
  import(sheet="Artifacts")

fy.r1 <- fy.value %>% 
  select(studentid,course,contains("R1_")) %>% 
  set_colnames(str_replace(colnames(.),"R\\d_","")) %>% 
  gather("dimension","level",-studentid:-course) 

fy.r1$rater <- 1

fy.r1$rubric <- str_sub(fy.r1$dimension,1,2)



# fy.r1$dimension %<>% str_extract("\\d")


fy.r2 <- fy.value %>% 
  select(studentid,course,contains("R2_")) %>% 
  set_colnames(str_replace(colnames(.),"R\\d_","")) %>% 
  gather("dimension","level",-studentid:-course) 

fy.r2$rater <- 2

fy.r2$rubric <- str_sub(fy.r2$dimension,1,2)

m.fy.value <- bind_rows(fy.r1,fy.r2) %>% 
  na.omit

m.fy.value$artifact <- artifacts$artifact[match(m.fy.value$course,artifacts$course)]

m.fy.value$change <- NA

m.fy.value %<>%
  select(studentid,course,artifact,rater,rubric,dimension,level,change)

  