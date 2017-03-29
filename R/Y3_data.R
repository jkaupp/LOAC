library(tidyverse)
library(readxl)
library(DBI)

loac <- RSQLServer::src_sqlserver("FEAS", database = "HEQCO-LOAC")


student_info <- loac %>% 
  tbl("student_info") %>% 
  filter(term %in% c(2159,2161), consent != 1) %>% 
  collect() 

value <- loac %>% 
  tbl("value") %>% 
  filter(term %in% c(2159,2161)) %>% 
  collect() %>% 
  semi_join(student_info, by = c("studentid","term")) %>% 
  group_by(studentid, term, subject, course, artifact, rubric, dimension) %>% 
  summarize(level = trunc(mean(as.numeric(level), na.rm = TRUE)))

value %>% 
  unite("RD",rubric, dimension) %>% 
  spread(RD, level) %>% 
  write_csv("./data/3rd_year_value_16_11_2016.csv")
  
cat <- loac %>% 
  tbl("cat") %>% 
  filter(term %in% c(2159,2161)) %>% 
  collect() %>% 
  semi_join(student_info) %>% 
  write_csv("./data/3rd_year_cat_16_11_2016.csv")
