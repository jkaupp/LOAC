library(magrittr)
library(dplyr)
library(stringr)
library(rio)
library(tidyr)

# Connections and Directories ----
source("Student Wrangling.R")


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

 
cat_local_code <- data_frame(loc_code = c(2104,3102,4103,1100,2501,4206,2207,3208,1205),
                             subject  = c("DRAM","PSYC","PHYS","APSC","DRAM","PHYS","DRAM","PSYC","APSC"),
                             course   = c(100,100,104,101,400,239,200,203,200))


cat_mapping <- data_frame(question = c("q1f", "q2f", "q3f", "q4f", "q5f", 
                                       "q6f", "q7f", "q8f", "q9f", "q10f", "q11f", "q12f", "q13f", "q14f", 
                                       "q15f", "score","Evaluate and Interpret Information","Problem Solving","Creative Thinking","Effective Communication"),
                          skill = c("Summarize the pattern of results in a graph without making inappropriate inferences",
                                    "Evaluate how strongly correlational-type data supports a hypothesis",
                                    "Provide alternative explanations for a pattern of results that has many possible causes",
                                    "Identify additional information needed to evaluate a hypothesis",
                                    "Evaluate whether spurious information strongly supports a hypothesis",
                                    "Provide alternative explanations for spurious associations",
                                    "Identify additional information needed to evaluate a hypothesis",
                                    "Determine whether an invited inference is supported by scientific information",
                                    "Provide relevant alternative interpretations for a specific set of results",
                                    "Separate relevant from irrelevant infromation when solving a real-world problem",
                                    "Use and apply relevant information to evaluate a problem",
                                    "Use basic mathematical skills to help solve a real-world problem",
                                    "Identify suitable solutions for real-world problem solving using relevant information",
                                    "Identify and explain the best solution for a real-world problem using relevan information",
                                    "Explain how changes in a real-world problem situation might affect the solution",
                                    "Overall Score",
                                    "Evaluate and Interpret Information",
                                    "Problem Solving",
                                    "Creative Thinking",
                                    "Effective Communication"))
#                          `Evaluate and Interpret Information` = c(1,1,0,0,1,0,0,1,0,1,1,0,1,1,0,0),
#                          `Problem Solving` = c(0,0,0,1,0,0,1,0,0,1,1,1,1,1,1,0),
#                          `Creative Thinking` = c(0,0,1,1,0,1,1,0,1,0,0,0,0,0,1,0),
#                          `Effective Communication` = c(0,1,1,1,0,1,1,0,1,0,1,0,0,1,1,0))
  

full_cat <- import(paste0(data.dir, "Queens_Univ_CAT_Data_File.xlsx"), sheet = 1) %>% 
  select(stude1,loc_code, q1f:total) %>% 
  rename(studentid = stude1,
         score_f = total) %>% 
  left_join(.,cat_local_code, by = "loc_code") %>% 
  left_join(cat,.,by = c("studentid","course","subject")) %>% 
  mutate(`Evaluate and Interpret Information (CAT)` = (q1f+q2f+q5f+q8f+q10f+q11f+q13f+q14f),
         `Problem Solving (CAT)` = (q4f+q7f+q10f+q11f+q12f+q13f+q14f+q15f),
         `Creative Thinking (CAT)` = (q3f+q4f+q6f+q7f+q9f+q15f),
         `Effective Communication (CAT)` = (q2f+q3f+q4f+q6f+q7f+q9f+q11f+q14f+q15f))  
  
m.full_cat <- full_cat %>%   
  select(-score) %>% 
  gather(question,score,q1f:`Effective Communication`) %>% 
  left_join(.,cat_mapping, by = "question")



