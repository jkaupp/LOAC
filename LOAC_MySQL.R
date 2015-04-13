library(DBI)
library(RMySQL)
library(xlsx)
library(magrittr)
library(dplyr)
library(data.table)

# Establish MySQL Connection to LOAC Database ----
con <- dbConnect(MySQL(), group='LOAC')

# Read data for Courses ----
base.dir <- "~/ownCloud/Engineering Education Research/HEQCO/LOAC Project/" 

apsc.100 <- paste0(base.dir,"APSC 100/Course Information/APSC 100 Master List.xlsx") %>% 
  read.xlsx(.,"Master",stringsAsFactors = FALSE)

apsc.200f <- paste0(base.dir,"APSC 200/Course Information/APSC 200 Master List.xlsx") %>% 
  read.xlsx(.,"Master_F", stringsAsFactors = FALSE) 

apsc.200w <- paste0(base.dir,"APSC 200/Course Information/APSC 200 Master List.xlsx") %>% 
  read.xlsx(.,"Master_W", stringsAsFactors = FALSE) 

apsc.480 <- paste0(base.dir,"APSC 480/APSC 480.xlsx") %>% 
  read.xlsx(.,"Master", stringsAsFactors = FALSE)  

civl.471 <- paste0(base.dir,"CIVL 471/CIVL 471.xlsx") %>% 
  read.xlsx(.,"Master", stringsAsFactors = FALSE)  

geoe.447 <- paste0(base.dir,"GEOE 447/GEOE 447.xlsx") %>% 
  read.xlsx(.,"Master", stringsAsFactors = FALSE) 

mech.462 <- paste0(base.dir,"MECH 462/MECH 462.xlsx") %>% 
  read.xlsx(.,"Master", stringsAsFactors = FALSE) 

enph.460 <- paste0(base.dir,"PHYS 460/PHYS 460.xlsx") %>% 
  read.xlsx(.,"Master", stringsAsFactors = FALSE) 

FAS.files <-paste0(base.dir,"FAS") %>% 
  list.files(full.names=TRUE)

FAS.list <-lapply(FAS.files,read.xlsx,sheetName="Master", stringsAsFactors = FALSE)

df.FAS <- rbindlist(FAS.list)

master <- rbind(apsc.100,apsc.200f,apsc.200w,apsc.480,civl.471,geoe.447,mech.462,enph.460,df.FAS)

master$email %<>%
  tolower()

# Read in the CLA+ Master Data ----
cla.data <- paste0(base.dir,"CLA+/Queens CLA+.xlsx") %>% 
  read.xlsx(.,"Master", stringsAsFactors = FALSE)


names(cla.data) %<>% 
  tolower()



# Write data to the student information table ----
dbWriteTable(con, name = 'student_info', value = master, row.names = FALSE, overwrite = TRUE)

# Write data to the cla+ table ----
dbWriteTable(con, name = 'cla+', value = cla.data, append = TRUE, row.names = FALSE)


consenting <- filter(master,course!=103) %>% 
  group_by(consent) %>% 
  tally()

(consenting$n[1]+consenting$n[3])/sum(consenting$n)

consenting.first <- filter(master, year == 2013, consent>=2, semester==1)
consenting.second <-filter(master, year >= 2014, consent>=2)

n.consenting.first <- filter(master, year == 2013, consent==1, semester==1)
n.consenting.second <-filter(master, year >= 2014, consent==1)

no.to.yes <- n.consenting.first[n.consenting.first$studentid %in% consenting.second$studentid,]
yes.to.no <- n.consenting.second[consenting.first$studentid %in% n.consenting.second$studentid,]

