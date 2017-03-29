library(tidyverse)
library(readxl)
library(DBI)

loac <- RSQLServer::src_sqlserver("FEAS", database = "HEQCO-LOAC")

student_info <- loac %>% 
  tbl("student_info") %>% 
  filter(consent != 1) %>% 
  collect() %>% 
  mutate(project_year = case_when(semester %in% c(1,2) ~ "Year 1",
                                  semester %in% c(3,4) ~ "Year 2",
                                  semester %in% c(5,6) ~ "Year 3")) %>% 
  filter(!is.na(project_year)) %>% 
  select(studentid, project_year, term, semester, consent, name, email, acad_prog, acad_plan, subject, course)

cla_plus <- loac %>% 
  tbl("cla_plus") %>% 
  collect() %>% 
  select(studentid, term, test_date, time_pt:mastery,transfer,class,sex,race, parent_edu,dob,effort_pt:engaging_sr,ls_q1:ls_r9)

value <- loac %>% 
  tbl("value") %>% 
  collect() %>% 
  mutate_each(funs(as.numeric), level)


summarized_value <- value %>% 
  mutate(level = ifelse(level == 99, NA, level)) %>% 
  group_by(term, studentid, subject, course, artifact, dimension) %>% 
  summarize(level = trunc(mean(level, na.rm = TRUE))) %>% 
  spread(dimension, level) %>% 
  filter(!(subject == "APSC" & course == 101)) %>% 
  ungroup %>% 
  mutate(course = if_else(course == 302, 301, course))

cat <- loac %>% 
  tbl("cat") %>% 
  collect() 

full_table <- student_info %>% 
  left_join(summarized_value) %>% 
  left_join(cat) %>% 
  left_join(cla_plus %>% select(-sex)) %>% 
  mutate(course = if_else(course == 301 & artifact == 'Research Ethics Proposal', 302, course)) %>% 
  split(.$project_year) 
                        
                      
openxlsx::write.xlsx(full_table, "MASTER LOAC Project File 22-03-2017.xlsx")
