library(magrittr)
library(dplyr)
library(readxl)
library(purrr)
library(stringr)
library(jkmisc)
library(tidyr)
library(haven)
library(DBI)

loac <- RSQLServer::src_sqlserver("FEAS", database = "HEQCO-LOAC")

files <- list.files("~/ownCloud/Shared/Data/Project_Year3/", full.names = TRUE, pattern = "Queens_CAT_Data_File")

cat_pdf <- list.files("~/ownCloud/Shared/Data/Project_Year3/", full.names = TRUE, pattern = "Queens_CAT_Report_June_2016_New_Version6")

v6_conversion <- tabulizer::extract_tables(cat_pdf,pages = 10) %>% 
  as.data.frame() %>% 
  select(-X3) %>% 
  slice(-1) %>% 
  set_colnames(c("question","coefficient")) %>% 
  mutate_each(funs(as.character)) %>% 
  mutate(coefficient = as.numeric(coefficient))

cat_headers <- loac %>% 
  tbl("cat") %>% 
  colnames


 
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

  

cat_y3 <- read_excel(files) %>% 
  rename(studentid = stude1,
         score_f = total) %>% 
  select(one_of(cat_headers),testversion) %>% 
  mutate(EvaluateandInterpretInformation = (q1f+q2f+q5f+q8f+q10f+q11f+q13f+q14f),
         ProblemSolving = (q4f+q7f+q10f+q11f+q12f+q13f+q14f+q15f),
         CreativeThinking = (q3f+q4f+q6f+q7f+q9f+q15f),
         EffectiveCommunication = (q2f+q3f+q4f+q6f+q7f+q9f+q11f+q14f+q15f),
         term = 2161) 


  mutate(q1f = q1f*v6_conversion[v6_conversion$question == "Q1",2],
         q2f = q2f*v6_conversion[v6_conversion$question == "Q2",2],
         q3f = q3f*v6_conversion[v6_conversion$question == "Q3",2],
         q4f = q4f*v6_conversion[v6_conversion$question == "Q4",2],
         q5f = q5f*v6_conversion[v6_conversion$question == "Q5",2],
         q6f = q6f*v6_conversion[v6_conversion$question == "Q6",2],
         q7f = q7f*v6_conversion[v6_conversion$question == "Q7",2],
         q8f = q8f*v6_conversion[v6_conversion$question == "Q8",2],
         q9f = q9f*v6_conversion[v6_conversion$question == "Q9",2],
         q10f = q10f*v6_conversion[v6_conversion$question == "Q10",2],
         q11f = q11f*v6_conversion[v6_conversion$question == "Q11",2],
         q12f = q12f*v6_conversion[v6_conversion$question == "Q12",2],
         q13f = q13f*v6_conversion[v6_conversion$question == "Q13",2],
         q14f = q14f*v6_conversion[v6_conversion$question == "Q14",2],
         q15f = q15f*v6_conversion[v6_conversion$question == "Q15",2]) %>% 
  mutate(score_f = q1f + q2f + q3f + q4f + q5f + q6f + q7f + q8f + q9f + q10f + q11f + q12f + q13f + q14f + q15f) 
  
         
  

