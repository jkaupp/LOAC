library(tidyverse)
library(readxl)
library(DBI)

loac <- RSQLServer::src_sqlserver("FEAS", database = "HEQCO-LOAC")

student_info <- loac %>% 
 tbl("student_info") %>% 
  collect()

updated_info <- list.files("./data", full.names = TRUE, pattern = "info") %>% 
  map_df(~read_excel(.x, skip = 1)) 

ui_1 <- updated_info %>% 
  jkmisc::clean_names() %>% 
  rename(studentid = id) %>% 
  mutate_each(funs(as.numeric), studentid, term) %>% 
  group_by(studentid, term, first_name, middle, last) %>% 
  summarize(acad_prog = paste(unique(acad_prog), collapse = "|"),
            acad_plan = paste(unique(acad_plan), collapse = "|")) 


list <- ui_1 %>% 
  group_by(studentid) %>% 
  top_n(1, term)

updated <- student_info %>% 
  distinct(studentid) %>% 
  select(studentid) %>% 
  left_join(list)

heighten <- list.files("./data", full.names = TRUE, pattern = "HEI") %>% 
  read_excel() %>% 
  jkmisc::clean_names() %>% 
  mutate(term = 2161) %>% 
  select(term, studentid = student_id, everything())

dbWriteTable(loac$con, "heighen", heighten)



