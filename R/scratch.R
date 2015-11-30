library(DBI)
library(RODBC)
library(magrittr)
library(dplyr)
library(stringr)
library(rio)
library(tidyr)
library(ggplot2)
library(corrplot)


con <- odbcConnect("FEAS-HEQCO", uid="AD\\kauppj", pwd="Laurenque5pge!")

student_info <- sqlFetch(con, "student_info")
cla <-sqlFetch(con, "cla_plus")
tlo <-sqlFetch(con, "tlo")
value <-sqlFetch(con, "value")
cat <- sqlFetch(con, "cat")


student_info %>% 
  mutate(project_year = ifelse(semester %in% c(1,2), 1, ifelse(semester %in% c(3,4), 2, 4))) %>% 
  mutate(consent = ifelse(consent == 1,"Non-consenting", "Consenting")) %>% 
  crosstab(., row.vars = c("project_year","consent"), col.vars = "subject", type = c("f", "c"), addmargins = FALSE) 

student_info %>% 
  mutate(project_year = ifelse(semester %in% c(1,2), 1, ifelse(semester %in% c(3,4), 2, 4))) %>% 
  mutate(consent = ifelse(consent == 1,"Non-consenting", "Consenting")) %>% 
  crosstab(., row.vars = c("consent"), col.vars = "project_year", type = c("f", "c"), addmargins = FALSE)

render_loac_report <- function(dept,title) 
{
  rmarkdown::render(
    "./R/LOAC Dept Report.Rmd", output_dir = "./reports/", output_file = paste(dept, "LOAC", "Report.pdf", sep = "_"),
    params = list(dept = dept,
                  dept_title = title
    )
)
}

render_loac_report("MECH","Department of Mechanical and Materials Engineering")
render_loac_report("ECE","Department of Electrical and Computer Engineering")
render_loac_report("PSYC","Department of Pyschology")

report_list <- data_frame(dept = c("MECH","ECE","PSYC"),
                          title = c("Department of Mechanical and Materials Engineering",
                                    "Department of Electrical and Computer Engineering",
                                    "Department of Psychology"))

sapply(report_list, render_loac_report, title = report_list$title)
