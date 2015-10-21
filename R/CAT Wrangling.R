library(magrittr)
library(dplyr)
library(stringr)
library(rio)
library(tidyr)

# Connections and Directories ----
source("Student Wrangling.R")

# Read in Student Info Table
student_info <- master


# Directory for VALUE Rubric Data
data.dir <- "/Users/Jake/ownCloud/Engineering Education Research/HEQCO/LOAC Project/CAT/"

# Read all files in the data dir into a list
cat.data <- list.files(data.dir, full.names = TRUE) %>% 
  lapply(import) %>% 
  plyr::rbind.fill() %>% 
  select(`Student ID`, Subject, Course, `CAT Score`) %>% 
  rename(subject = Subject,
         course = Course,
         studentid = `Student ID`,
         score = `CAT Score`) %>% 
  filter(!is.na(score)) %>%
  left_join(student_info, by=c("studentid","subject","course")) 


 cat.data %>% 
  filter(course==200,subject=="APSC") %>% 
  select(studentid,course,subject,plan,score) %>% 
  mutate(plan=str_sub(plan,1,4)) %>% 
  write.csv("APSC 200 CAT.csv")


  


