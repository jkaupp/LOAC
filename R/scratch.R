library(DBI)
library(RODBC)
library(magrittr)
library(dplyr)
library(stringr)
library(rio)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(corrplot)


student_info <- sqlFetch(con, "student_info", stringsAsFactors = FALSE)
frozen.data<-sqlQuery(con2, "SELECT CAN_RPT_PERIOD, EMPLID, NAME, ACAD_PLAN, ACAD_PROG, CAN_SEX2 from qarch where CAN_RPT_PERIOD LIKE '21%9-F'", stringsAsFactors = FALSE)
cla <-sqlFetch(con, "cla_plus")
tlo <-sqlFetch(con, "tlo")
value <-sqlFetch(con, "value")
cat <- sqlFetch(con, "cat")

frozen.data %<>%
  rename(studentid = EMPLID)

wt.change<-student_info %>% 
  filter(grepl("William Taylor",name)) %>% 
  mutate(studentid = 6018405)

student_info %>% 
  filter(!grepl("William Taylor",name)) %>% 
  bind_rows(wt.change)

data1<-left_join(student_info,frozen.data) %>% 
  group_by(studentid) %>% 
  top_n(n=1, CAN_RPT_PERIOD) %>% 
  group_by(studentid,year,semester,subject,course) %>% 
  filter(n()==1)

data2<-left_join(student_info,frozen.data) %>% 
  group_by(studentid) %>% 
  top_n(n=1, CAN_RPT_PERIOD) %>% 
  group_by(studentid,year,subject,course,semester) %>% 
  filter(n()>1) %>% 
  filter(!ACAD_PROG %in% c("BED","CIB","BA","BSC","BCMP")) 

temp<-bind_rows(data1,data2) %>% 
  select(-program,-plan,-sex,-CAN_RPT_PERIOD,-name) %>% 
  rename(name = NAME,
         sex = CAN_SEX2,
         plan = ACAD_PLAN,
         program = ACAD_PROG)

leftovers<-anti_join(student_info,temp, by=c("studentid","year","semester","subject","course")) %>% 
  left_join(frozen.data) %>% 
  filter(!is.na(CAN_RPT_PERIOD)) %>% 
  group_by(studentid) %>% 
  top_n(n=1, CAN_RPT_PERIOD) %>% 
  filter(!ACAD_PROG %in% c("BED")) %>% 
  select(-program,-plan,-sex,-CAN_RPT_PERIOD,-name) %>% 
  rename(name = NAME,
         sex = CAN_SEX2,
         plan = ACAD_PLAN,
         program = ACAD_PROG)

left_outs <- anti_join(student_info,temp, by=c("studentid","year","semester","subject","course")) %>% 
  left_join(frozen.data) %>% 
  filter(is.na(CAN_RPT_PERIOD)) %>% 
  select(-program,-plan,-sex,-CAN_RPT_PERIOD,-name) %>% 
  rename(name = NAME,
         sex = CAN_SEX2,
         plan = ACAD_PLAN,
         program = ACAD_PROG)

new_student_info <- bind_rows(temp,leftovers)



psyc.301 <- import("/Users/Jake/ownCloud/Engineering Education Research/HEQCO/LOAC Project/FAS/PSYC 301.xlsx") %>% 
  left_join(frozen.data) %>% 
  group_by(studentid) %>% 
  top_n(n=1, CAN_RPT_PERIOD) %>% 
  filter(!ACAD_PROG %in% c("BED","CIB")) %>% 
  select(-CAN_RPT_PERIOD,-name) %>% 
  rename(name = NAME,
         sex = CAN_SEX2,
         plan = ACAD_PLAN,
         program = ACAD_PROG) %>% 
  mutate(test = ifelse(is.na(test),"DNT",test)) %>% 
  left_join(.,new_student_info, by=c("studentid","name")) %>% 
  select(1:12,team,email) %>% 
  mutate(team = NA) %>% 
  distinct %>% 
  select(studentid, year.x, subject.x, course.x, semester.x, section.x, 
           team, consent.x, email, test.x, name, plan.x, program.x, 
           sex.x)


psyc.301<-set_names(psyc.301,names(new_student_info))




