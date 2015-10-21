library(DBI)
library(RMySQL)
library(magrittr)
library(dplyr)
library(stringr)
library(rio)
library(tidyr)

# Connections and Directories ----


# Read in Student Info Table
student_info <- master

# Directory for VALUE Rubric Data
data.dir <- "/Users/Jake/ownCloud/Engineering Education Research/HEQCO/LOAC Project/VALUE/"

## ALL FIRST YEAR LOAC PROJECT VALUE Scoring ----

# Read in the VALUE Data from Year 1
fy.value <- paste0(data.dir,"LOAC 1 VALUE.xlsx") %>% 
  import(sheet="Master") %>% 
  rename(studentid = Student_ID)

# Read in the artifacts 
artifacts <- paste0(data.dir,"LOAC 1 VALUE.xlsx") %>% 
  import(sheet="Artifacts")

# Process data for rater 1
# rename columns and gather into long-form
fy.r1 <- fy.value %>% 
  select(studentid,course,contains("R1_")) %>% 
  set_colnames(str_replace(colnames(.),"R\\d_","")) %>% 
  gather("dimension","level",-studentid:-course) %>% 
  mutate(rater = 1,
         rubric = str_sub(dimension,1,2),
         artifact = artifacts$artifact[match(course,artifacts$course)],
         change = NA,
         program = NA)


# Process data for rater 2
# rename columns and gather into long-form
fy.r2 <- fy.value %>% 
  select(studentid,course,contains("R2_")) %>% 
  set_colnames(str_replace(colnames(.),"R\\d_","")) %>% 
  gather("dimension","level",-studentid:-course) %>% 
  mutate(rater = 2,
         rubric = str_sub(dimension,1,2),
         artifact = artifacts$artifact[match(course,artifacts$course)],
         change = NA,
         program = NA)


# Bind the rater rubrics together
m.fy.LOAC <- bind_rows(fy.r1,fy.r2) %>% 
  select(studentid,course,program,artifact,rater,rubric,dimension,level,change)

m.fy.LOAC$level %<>%
  as.character()

# LOAC 2nd year: Additional Phase 4 Value Rubric Data APSC 103 ----
## Read in the additional APSC P4 VALUE Data
apsc103.extra.value <- paste0(data.dir,"APSC100 P4 VALUE Extra.xlsx") %>% 
  import(sheet=1) %>% 
  slice(1:18) %>% 
  left_join(., student_info %>% 
              filter(subject=="APSC", course=="103")
            , by = "team") %>% 
  select(studentid,course.x,artifact, matches("[A-Z]+\\d-\\d")) %>% 
  rename(course = course.x)

## Set up value rubric extract
apsc103.extra.R1 <- apsc103.extra.value %>% 
  select(studentid, course, artifact, matches("^[A-Z]+\\d-1")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>%
  gather("dimension","level",-studentid:-artifact)

apsc103.extra.R2 <- apsc103.extra.value %>% 
  select(studentid, course, artifact, matches("^[A-Z]+\\d-2")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>%
  gather("dimension","level",-studentid:-artifact) 

## Extract change scores into key-value pairs
apsc103.extra.C1 <- apsc103.extra.value %>% 
  select(studentid, course, artifact, matches("Change-[A-Z]+\\d-1")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>% 
  gather("dimension","change",-studentid:-artifact) %>% 
  mutate(dimension = str_replace(dimension,"Change-", ""))

apsc103.extra.C2 <- apsc103.extra.value %>%
  select(studentid, course, artifact, matches("Change-[A-Z]+\\d-2")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>% 
  gather("dimension","change",-studentid:-artifact) %>% 
  mutate(dimension = str_replace(dimension,"Change-", ""))

## join the ratings and change scores
apsc103.extra.R1 <- left_join(apsc103.extra.R1,apsc103.extra.C1) %>% 
  mutate(rater = 1,
         rubric = str_sub(dimension,1,2),
         program = NA) %>% 
  select(studentid,course,program,artifact,rater,rubric,dimension,level,change)
  
apsc103.extra.R2 <- left_join(apsc103.extra.R2,apsc103.extra.C2) %>% 
  mutate(rater = 2,
         rubric = str_sub(dimension,1,2),
         program = NA) %>% 
  select(studentid,course,program,artifact,rater,rubric,dimension,level,change)  

## Bind both raters into a single data frame
m.apsc103.extra <- bind_rows(apsc103.extra.R1,apsc103.extra.R2)

# LOAC 2nd Year: DRAM 200 VALUE Rubric Scoring ----
dram200.value <- paste0(data.dir,"DRAM 200 VALUE.xlsx") %>% 
  import(sheet=1) 

## Set up value rubric extract
dram200.R1 <- dram200.value %>% 
  select(studentid, course, artifact, matches("^[A-Z]+\\d-1")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>%
  gather("dimension","level",-studentid:-artifact)

dram200.R2 <- dram200.value %>% 
  select(studentid, course, artifact, matches("^[A-Z]+\\d-2")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>%
  gather("dimension","level",-studentid:-artifact) 

## Extract change scores into key-value pairs
dram200.C1 <- dram200.value %>%
  select(studentid, course, artifact, matches("Change-[A-Z]+\\d-1")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>% 
  gather("dimension","change",-studentid:-artifact) %>% 
  mutate(dimension = str_replace(dimension,"Change-", ""))

dram200.C2 <- dram200.value %>%
  select(studentid, course, artifact, matches("Change-[A-Z]+\\d-2")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>% 
  gather("dimension","change",-studentid:-artifact) %>% 
  mutate(dimension = str_replace(dimension,"Change-", ""))

## join the ratings and change scores
dram200.R1 <- left_join(dram200.R1,dram200.C1) %>% 
  mutate(rater = 1,
         rubric = str_sub(dimension,1,2),
         program = NA) %>% 
  select(studentid,course,program,artifact,rater,rubric,dimension,level,change)

dram200.R2 <- left_join(dram200.R2,dram200.C2) %>% 
  mutate(rater = 2,
         rubric = str_sub(dimension,1,2),
         program = NA) %>% 
  select(studentid,course,program,artifact,rater,rubric,dimension,level,change)  

## Bind both raters into a single data frame
m.dram200 <- bind_rows(dram200.R1,dram200.R2)

# LOAC 2nd Year: PSYC 203 VALUE Rubric Scoring ----
psyc203.value <- paste0(data.dir,"PSYC 203 VALUE.xlsx") %>% 
  import(sheet=1) 

## Set up value rubric extract
psyc203.R1 <- psyc203.value %>% 
  select(studentid, course, artifact, matches("^[A-Z]+\\d-1")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>%
  gather("dimension","level",-studentid:-artifact)

psyc203.R2 <- psyc203.value %>% 
  select(studentid, course, artifact, matches("^[A-Z]+\\d-2")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>%
  gather("dimension","level",-studentid:-artifact) 

## Extract change scores into key-value pairs
psyc203.C1 <- psyc203.value %>%
  select(studentid, course, artifact, matches("Change-[A-Z]+\\d-1")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>% 
  gather("dimension","change",-studentid:-artifact) %>% 
  mutate(dimension = str_replace(dimension,"Change-", ""))

psyc203.C2 <- psyc203.value %>%
  select(studentid, course, artifact, matches("Change-[A-Z]+\\d-2")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>% 
  gather("dimension","change",-studentid:-artifact) %>% 
  mutate(dimension = str_replace(dimension,"Change-", ""))

## join the ratings and change scores
psyc203.R1 <- left_join(psyc203.R1,psyc203.C1) %>% 
  mutate(rater = 1,
         rubric = str_sub(dimension,1,2),
         program = NA) %>% 
  select(studentid,course,program,artifact,rater,rubric,dimension,level,change)

psyc203.R2 <- left_join(psyc203.R2,psyc203.C2) %>% 
  mutate(rater = 2,
         rubric = str_sub(dimension,1,2),
         program = NA) %>% 
  select(studentid,course,program,artifact,rater,rubric,dimension,level,change)  

## Bind both raters into a single data frame
m.psyc203 <- bind_rows(psyc203.R1,psyc203.R2)

# LOAC 2nd Year: APSC 200 VALUE Rubric Scoring ----
apsc200.value <- paste0(data.dir,"APSC 200 VALUE.xlsx") %>% 
  import(sheet=1) %>% 
  slice(1:76) %>% 
  mutate(team=as.character(team)) %>% 
  left_join(., student_info %>% 
              filter(subject=="APSC", course=="200"),
              by = c("semester","section","team")) %>% 
  select(studentid, course.x, plan, artifact, matches("[A-Z]+\\d-\\d")) %>% 
  rename(course = course.x,
         program = plan)

## Set up value rubric extract
apsc200.R1 <- apsc200.value %>% 
  select(studentid, course, program, artifact, matches("^[A-Z]+\\d-1")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>%
  gather("dimension","level",-studentid:-artifact)

apsc200.R2 <- apsc200.value %>% 
  select(studentid, course, program, artifact, matches("^[A-Z]+\\d-2")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>%
  gather("dimension","level",-studentid:-artifact) 

## Extract change scores into key-value pairs
apsc200.C1 <- apsc200.value %>% 
  select(studentid, course, program, artifact, matches("Change-[A-Z]+\\d-1")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>% 
  gather("dimension","change",-studentid:-artifact) %>% 
  mutate(dimension = str_replace(dimension,"Change-", ""))

apsc200.C2 <- apsc200.value %>%
  select(studentid, course, program, artifact, matches("Change-[A-Z]+\\d-2")) %>% 
  set_colnames(str_replace(colnames(.),"-\\d","")) %>% 
  gather("dimension","change",-studentid:-artifact) %>% 
  mutate(dimension = str_replace(dimension,"Change-", ""))

## join the ratings and change scores
apsc200.R1 <- left_join(apsc200.R1,apsc200.C1) %>% 
  mutate(rater = 1,
         rubric = str_sub(dimension,1,2),
         program = NA) %>% 
  select(studentid,course,program,artifact,rater,rubric,dimension,level,change)

apsc200.R2 <- left_join(apsc200.R2,apsc200.C2) %>% 
  mutate(rater = 2,
         rubric = str_sub(dimension,1,2),
         program = NA) %>% 
  select(studentid,course,program,artifact,rater,rubric,dimension,level,change)  

## Bind both raters into a single data frame
m.apsc200 <- bind_rows(apsc200.R1,apsc200.R2)


## Creating master data frame ----
m.full.value <- bind_rows(mget(ls(pattern="m\\."))) %>% 
  as.data.frame

# Factor the rubric column
m.full.value$rubric %<>% 
  factor(c("PS","CT","WC"),c("Problem Solving","Critical Thinking","Written Communication"))

# Factor the dimension column
m.full.value$dimension %<>%
  factor(names(
    table(.)),c(
      "Explanation of issues",
      "Evidence",
      "Context and assumptions",
      "Student' position",
      "Conclusions and outcomes",
      "Define problem",
      "Identify strategies",
      "Solution/hypothoses",
      "Evaluate solution",
      "Implement solution",
      "Evaluate outcomes",
      "Context and purpose",
      "Content development",
      "Genre and conventions",
      "Sources of evidence",
      "Control of syntax and mechanics"
    )
  )

# Convert change to lowercase
m.full.value$change %<>%
  tolower()

# Normalize the change scores (0 = No Change, 1 = Change, 99 = Not Assessed) and factor
m.full.value$change[is.na(m.full.value$change)] <- 99
m.full.value$change[m.full.value$change=="na"] <- 99
m.full.value$change[m.full.value$change=="n/a"] <- 99


m.full.value$change[m.full.value$change==1] <- "0"
m.full.value$change[m.full.value$change=="n"] <- "0"
m.full.value$change[m.full.value$change=="no"] <- "0"

m.full.value$change[m.full.value$change==2] <- "1"
m.full.value$change[m.full.value$change=="y"] <- "1"
m.full.value$change[m.full.value$change=="yed"] <- "1"
m.full.value$change[m.full.value$change=="yes"] <- "1"


m.full.value$change %<>%
  factor(names(
    table(.)), c("No Change", "Changed", "Not Assessed"))
  
# Normalized the level scores 
# 0 = Below benchmark 1,
# 1 = Benchmark 1
# 2 = Milestone 2
# 3 = Milestone 3
# 4 = Capstone 4
# 99 = Not Assessed

m.full.value$level[m.full.value$level=="NA"] <- 99
m.full.value$level[m.full.value$level=="N/A"] <- 99
m.full.value$level[is.na(m.full.value$level)] <- 99

# m.full.value$level %<>%
#   factor(names(
#     table(.)), c("Below Benchmark 1", "Benchmark 1", "Milestone 2", "Milestone 3", "Capstone 4", "Not Assessed"))

m.full.value %<>%
  separate(course,c("subject","course"), sep=" ")


  