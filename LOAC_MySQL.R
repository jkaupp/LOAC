library(DBI)
library(RMySQL)
library(xlsx)
library(magrittr)

# Establish MySQL Connection to LOAC Database
con <- dbConnect(MySQL(), group='LOAC')

# Read data for APSC 100 & 200
base.dir <- "~/ownCloud/Engineering Education Research/HEQCO/LOAC Project/" 

apsc.100 <- paste0(base.dir,"APSC 100/Course Information/APSC 100 Master List.xlsx") %>% 
  read.xlsx(.,"Master Data",stringsAsFactors = FALSE)

apsc.200f <- paste0(base.dir,"APSC 200/Course Information/APSC 200 Master List.xlsx") %>% 
  read.xlsx(.,"Master APSC 200 Fall 2014", stringsAsFactors = FALSE) 

apsc.200w <- paste0(base.dir,"APSC 200/Course Information/APSC 200 Master List.xlsx") %>% 
  read.xlsx(.,"Master APSC 200 Winter 2015", stringsAsFactors = FALSE) 

apsc.200 <- rbind(apsc.200f,apsc.200w)  
  
# Write data to the student information table
dbWriteTable(con, name = 'student_info', value=apsc.100, row.names = FALSE, append = TRUE)
dbWriteTable(con, name = 'student_info', value=apsc.200, row.names = FALSE, append = TRUE)
