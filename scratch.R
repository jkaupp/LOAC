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

read.files <- . %>%
{
  read_excel(paste0(base.dir,"CLA+/Queens CLA+.xlsx"),.,skip = 0)
}

apsc.100.group.consent <- ddply(apsc.100,.(team,semester), summarise, consent=min(consent)) %>% 
  filter(consent>1)
  
  


VALUE <- read.csv("/Users/Jake/ownCloud/Engineering Education Research/HEQCO/LOAC Project/VALUE/2014_VALUE_CLA_1-4_all.csv")

apsc100.value.id <- VALUE %>% 
  filter(Program==4) %>% 
  select(Student_ID) %>% 
  unlist %>% 
  as.numeric()



 master %>% 
  filter(program=="BSCE", test=="CLA+", course!='103', consent>=2) %>% 
  unique() %>% 
  select(-name,-email) %>% 
  write.csv("Engineering Student Info.csv")

eng.ids<-master %>% 
  filter(program=="BSCE", test=="CLA+", consent>=2) %>% 
  select(studentid) %>% 
  tbl_df()
  
cla.data %<>% 
  tbl_df()

cla.data %>% 
  semi_join(., eng.ids, by="studentid")  %>%
  select(-contains("name"),-email) %>% 
  write.csv("Engineering CLA+ Data.csv")






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
  

  





  
