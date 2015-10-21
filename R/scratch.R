library(DBI)
library(RMySQL)
library(magrittr)
library(dplyr)
library(stringr)
library(rio)
library(tidyr)
library(ggplot2)
library(corrplot)

nsse <- dbConnect(MySQL(), group='QUIVER')

con <- dbConnect(MySQL(), group='LOAC')
TLO <- dbReadTable(con,"TLO")
student_info <- dbReadTable(con,"student_info")
CLA <- dbReadTable(con,"cla_plus")
VALUE <- dbReadTable(con,"VALUE")
CAT <- dbReadTable(con,"cat")
NSSE <- dbReadTable(nsse, 'NSSE') %>% 
  rename(studentid = STUDENT_ID)


apsc.CAT <- student_info %>% 
  filter(subject=="APSC", course==101)  %>% 
  filter(test=="CAT") %>% 
  left_join(CAT, by=c("studentid","subject","course")) %>% 
  filter(!is.na(score)) %>% 
  select(studentid,subject,course,score) %>% 
  inner_join(NSSE) %>% 
  write.csv("APSC 100 CAT-NSSE.csv")
  
apsc.CLA <- student_info %>% 
  filter(subject=="APSC", course==101)  %>% 
  filter(test=="CLA+") %>% 
  left_join(CLA, by=c("studentid","year")) %>% 
  select(-sex.y, -email.y) %>% 
  rename(sex = sex.x,
         email = email.x) %>% 
  select(studentid,year,mastery,time_pt:time_sr,pt_aps:score_total,effort_pt:engaging_sr) %>% 
  inner_join(NSSE) %>% 
  write.csv("APSC 100 CLA-NSSE.csv")
 



apsc.VALUE <- student_info %>% 
  filter(subject=="APSC", course==101)  %>% 
  left_join(VALUE, by=c("studentid","subject","course")) %>% 
  filter(!is.na(artifact)) %>% 
  filter(level != 99) %>% 
  select(-program.y) %>% 
  rename(program = program.x) %>% 
  group_by(studentid,subject,course,dimension) %>% 
  summarize(level=trunc(mean(level))) %>% 
  spread(dimension,level)  %>% 
  inner_join(NSSE) %>% 
  write.csv("APSC 100 VALUE-NSSE.csv")
 


