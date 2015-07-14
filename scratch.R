library(DBI)
library(RMySQL)
library(xlsx)
library(magrittr)
library(plyr)
library(dplyr)
library(data.table)
library(stringr)
library(readxl)
library(rio)
library(tidyr)

read.files <- . %>%
{
  read_excel(paste0(base.dir,"CLA+/Queens CLA+.xlsx"),.,skip = 0)
}

base.dir <- "~/ownCloud/Engineering Education Research/HEQCO/LOAC Project/" 


apsc.100.group.consent <- ddply(apsc.100,.(team,semester), summarise, consent=min(consent)) %>% 
  filter(consent>1)


TLO <- import(paste0(base.dir,"TLO/TLO Master.csv"))

TLO %>% 
  separate(Collector,c("subject","course")," ")



consenting <- master %>% 
  unite(Course, c(subject, course), sep = " ") %>% 
  rename(course = Course) %>% 
  filter(consent>=2) %>% 
  select(course, studentid, consent) 

            

semi_join(TLO,consenting, by=c("studentid","course")) %>% 
  write.csv('Consenting TLO.csv')


VALUE <- read.csv("/Users/Jake/ownCloud/Engineering Education Research/HEQCO/LOAC Project/VALUE/2014_VALUE_CLA_1-4_all.csv")

apsc100.value.id <- VALUE %>% 
  filter(Program==4) %>% 
  select(Student_ID) %>% 
  unlist %>% 
  as.numeric()



ids <- student.info %>% 
  filter(test=="CLA+", course!='103', consent>=2) %>% 
  tbl_df()


temp<-cla.plus %>% 
  semi_join(., ids, by=c("studentid", "year"))  %>%
  select(-contains("name"),-email) %>% 
  write.csv("CLA+ Data.csv")
  

temp2 <-cla.data %>% 
  anti_join(ids, ., by="studentid") 







## Continous CLA+ departmental breakdowns-----
fy.eng.cla<-apsc.100 %>% 
  filter(consent>=2, test=="CLA+") %>% 
  inner_join(cla, by=c("studentid","year")) %>% 
  select(studentid,year,plan,time_pt:score_total,mastery)

sy.eng.cla <- apsc200 %>% 
  filter(consent>=2, test=="CLA+", year=="2015") %>% 
  inner_join(cla, by=c("studentid","year")) %>% 
  select(studentid,year,plan,time_pt:score_total,mastery)

second.year <- sy.eng.cla[sy.eng.cla$studentid %in% intersect(fy.eng.cla$studentid,sy.eng.cla$studentid),]

first.year <- fy.eng.cla[fy.eng.cla$studentid %in% intersect(fy.eng.cla$studentid,sy.eng.cla$studentid),] %>% 
  distinct

first.year$plan <- second.year[second.year$studentid %in% intersect(first.year$studentid,second.year$studentid), 3]


long.eng.cla <- rbind_all(list(first.year, second.year))

long.eng.cla %>% 
  group_by(plan,year) %>%
  select(studentid,year,plan,score_total) %>% 
  na.omit %>% 
  summarize("Sample"=n(),
            "Total Score" = mean(na.omit(score_total))) 
  
long.eng.cla %>% 
  ggplot(aes(x = year , y = score_total, color = plan)) +
  geom_line(stat = 'summary', fun.y = mean)
  










  
