# detach("package:plyr", unload=TRUE)
library(DBI)
library(RODBC)
library(magrittr)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(scales)
library(grid)
library(gridExtra)
library(gdata)

# Connections and Directories ----
# Connection to LOAC Database 
odbcCloseAll()

con <- odbcConnect("FEAS-HEQCO", uid="AD\\kauppj", pwd="Laurenque5pge!")

student_info <- sqlFetch(con, "student_info")
cla_plus <-sqlFetch(con, "cla_plus")
tlo <-sqlFetch(con, "tlo")
value <-sqlFetch(con, "value")
cat <- sqlFetch(con, "cat")

fy.loac <- student_info %>% 
  filter(semester==1, course!=103) %>% 
  rename(`first year discipline` = plan) %>% 
  distinct(studentid)

sy.loac <- student_info %>% 
  filter(semester==3 | semester==4, subject != "ENPH") %>% 
  rename(`second year discipline` = plan)


full.loac <- inner_join(fy.loac, sy.loac, by="studentid") %>% 
  filter(!is.na(consent.y)|!is.na(consent.x)) %>% 
  select(studentid, name.y, `second year discipline`, subject.x, course.x, test.x, subject.y, course.y, test.y, year.x, year.y, consent.x, consent.y) %>% 
  unite(fy.course, c(subject.x, course.x), sep = " ") %>% 
  unite(sy.course, c(subject.y, course.y), sep = " ") %>% 
  rename(name = name.y,
         discipline =  `second year discipline`,
         fy = test.x,
         sy = test.y,
         fy.consent = consent.x,
         sy.consent = consent.y) %>% 
  unite(`1`, c(fy.course, fy, year.x, fy.consent), sep = "-") %>% 
  unite(`2`, c(sy.course, sy, year.y, sy.consent), sep = "-") %>% 
  gather(project_year, test, c(`1`,`2`)) %>% 
  separate(test,c("course", "test", "year", "consent"),sep = "-") %>% 
  separate(course,c("subject", "course"), sep = " ")

full.loac$course %<>%
  as.numeric

full.loac$year %<>%
  as.numeric


# Merge consitent test taker frame, subset by CAT, join with CAT data, filter by consenting student, and filter out non-duplicate records----
CAT <- full.loac %>% 
  filter(test=="CAT") %>% 
  left_join(cat, by=c("studentid","subject","course")) %>% 
  semi_join(student_info %>% 
              filter(consent>=2), 
            by= c("studentid","subject","course")) %>% 
  filter(!is.na(score)) %>% 
  select(-matches("\\w\\.y"), -test.x) %>% 
  rename(name = name.x,
         year = year.x,
         consent = consent.x) 
# %>% 
#   mutate(discipline = ifelse(is.na(discipline), subject, discipline))
  

# Merge consitent test taker frame, subset by CLA, join with CLA data, filter by consenting student, and filter out non-duplicate records----
CLA <- full.loac %>% 
  filter(test=="CLA+") %>% 
  left_join(cla_plus, by=c("studentid","year")) %>% 
  semi_join(student_info %>% 
              filter(consent>=2), 
            by=c("studentid","year")) %>% 
  filter(!is.na(score_pt)) 


# VALUE Data Prep----
# Create first year data frame for VALUE 
fy.value <- student_info %>% 
  filter(semester== 2 | semester== 1) %>% 
  rename(`first year discipline`= plan) %>% 
  inner_join(value,., by=c("studentid","subject","course")) %>% 
  filter(level!=99) %>% 
  select(studentid, subject, course, artifact, rubric, dimension, level, `first year discipline`, year, semester, consent) %>% 
  unite(dimension1, rubric, dimension) %>% 
  rename(dimension = dimension1) %>% 
  group_by(studentid, subject, course, artifact,`first year discipline`, year, semester, consent, dimension) %>% 
  summarize(level = trunc(mean(level))) %>% 
  spread(dimension,level) %>% 
  mutate(program_year=1) %>% 
  select(studentid, program_year, everything()) %>% 
  rename(project_year = program_year)


# Create second year data frame
sy.value <- student_info %>% 
  filter(semester==3 | semester==4, subject!="ENPH") %>% 
  rename(`second year discipline`= plan) %>% 
  inner_join(value,., by=c("studentid","subject","course")) %>% 
  filter(level!=99) %>% 
  unite(dimension1, rubric, dimension) %>% 
  rename(dimension = dimension1) %>% 
  group_by(studentid, subject, course, artifact,`second year discipline`, year, semester, consent, dimension) %>% 
  summarize(level=trunc(mean(level))) %>% 
  spread(dimension,level) %>% 
  mutate(program_year=2) %>% 
  select(studentid, program_year, subject, course, consent, everything()) %>% 
  rename(project_year = program_year)

#Entire VALUE Data frame

full.VALUE <- bind_rows(fy.value,sy.value) %>% 
  left_join(sy.loac %>% select(studentid,`second year discipline`), by=c("studentid")) %>% 
  rename(discipline = `second year discipline.y`) %>% 
  select(-`second year discipline.x`) %>% 
  gather(.,rubric1, level, 10:ncol(.), -discipline) %>% 
  separate(.,rubric1,c("rubric","dimension"), sep="_") %>% 
  mutate(level = ifelse(is.na(level),99,level))


# Sequenetial VALUE data frame
VALUE <- bind_rows(fy.value[fy.value$studentid %in% sy.value$studentid,],sy.value[sy.value$studentid %in% fy.value$studentid,]) %>% 
  left_join(sy.loac %>% select(studentid,`second year discipline`), by=c("studentid")) %>% 
  rename(discipline = `second year discipline.y`) %>% 
  select(-`second year discipline.x`, -`first year discipline`) %>%
  gather(.,rubric1,level, 9:ncol(.), -discipline) %>% 
  separate(.,rubric1,c("rubric","dimension"), sep="_") %>% 
  mutate(level = ifelse(is.na(level),99,level))
        


TLO <- full.loac %>% 
  unite(course2, c(subject, course), sep=" ") %>% 
  rename(course = course2) %>% 
  mutate(course = ifelse(course=="APSC 101", "APSC 103", course)) %>% 
  inner_join(tlo %>% 
               rename(course = Collector)) %>% 
  filter(consent>1) %>% 
  distinct(studentid, project_year)
  
    


VALUE$level %<>%
  factor(.,levels=c(0,1,2,3,4,99), labels=c("Below Benchmark 1", "Benchmark 1", "Milestone 2", "Milestone 3", "Capstone 4","Not Assessed"))

CLA %<>% 
  mutate(discipline = str_sub(discipline,1,4),
        discipline = ifelse(is.na(discipline), subject, discipline))

CAT %<>% 
  mutate(discipline = str_sub(discipline,1,4),
         discipline = ifelse(is.na(discipline), subject, discipline)) 

VALUE %<>%
  mutate(discipline = str_sub(discipline,1,4),
         discipline = ifelse(is.na(discipline), subject, discipline))

TLO %<>% 
  mutate(discipline = str_sub(discipline,1,4),
         discipline = ifelse(is.na(discipline), str_sub(course,1,4), discipline),
         project_year = as.numeric(project_year))


# Function to dcast and write VALUE rubric data
# VALUE_cast <- function(df){
#   df %>% 
#     distinct %>% 
#     dcast(., studentid + project_year + course + consent + discipline ~ dimension, value.var = "level", fun.aggregate = NULL) %>% 
#     write.csv(file=paste("Full",df$rubric[[1]],"VALUE Rubric Data 20-11-2015.csv",sep=" "))
# }



# CLA %>% 
#   set_colnames(., str_replace(colnames(.), "\\.\\w","")) %>% 
#   write.csv("Full CLA+ Data 20-11-2015.csv")
# 
# CAT %>% 
#   set_colnames(., str_replace(colnames(.), "\\.\\w","")) %>% 
#   write.csv("Full CAT Data 20-11-2015.csv")
# 
# VALUE %>% 
#   unite(course_code, subject, course, sep = " ") %>% 
#   rename(course = course_code) %>% 
#   d_ply(.,.(rubric), VALUE_cast)
#   
# 
# value %>% 
#   semi_join(., subset(student_info, consent>1), by = c("studentid", "subject", "course")) %>% 
#   filter(subject == "APSC", course == "200", rubric == "Critical Thinking", level !=99) %>% 
#   summarize(mean = mean(level))
