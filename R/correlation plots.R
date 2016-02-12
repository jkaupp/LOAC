library(DBI)
library(RODBC)
library(magrittr)
library(dplyr)
library(stringr)
library(rio)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(viridis)

## Correlation plots for engineering

total_value <- VALUE %>% 
#   filter(subject == "APSC", course != 480, course != 101) %>%
#   mutate(course = ifelse(course == 103, 101, course)) %>% 
  filter(subject == "APSC") %>% 
  filter(level != 99, !is.na(level)) %>% 
  group_by(studentid,project_year,subject,course,rubric) %>% 
  summarize(level = mean(level)) %>% 
  rename(dimension = rubric) %>% 
  {
    
    df<-VALUE %>%
#       filter(subject == "APSC", course != 480,course != 101) %>%
#       mutate(course = ifelse(course == 103, 101, course)) %>% 
      filter(subject == "APSC") %>% 
      filter(level != 99,!is.na(level)) %>%
      select(studentid,project_year,subject,course,rubric,dimension,level)
    
    bind_rows(.,df) %>% 
      select(-rubric) %>%
      spread(dimension, level) } %>% 
  rename(`Critical Thinking (VALUE)` = `Critical Thinking`,
         `Written Communication (VALUE)` = `Written Communication`,
         `Problem Solving (VALUE)` = `Problem Solving`) %>% 
  mutate(VALUE = "VALUE",
         project_year = as.character(project_year)) 

  


correlation_data <- full.loac %>% 
  filter(subject=="APSC", consent > 1, test != "MSLQ") %>% 
  left_join(.,cla_plus %>% select(studentid,year,pt_aps:score_total), by = c("studentid","year")) %>% 
  left_join(., cat %>% select(studentid,subject,course,q1f:`EffectiveCommunication`), by = c("studentid","subject","course")) %>% 
  left_join(., total_value, by = c("studentid","subject","course")) %>% 
  select(-project_year.y) %>% 
  rename( project_year = project_year.x,
         `Evaluate and Interpret Information (CAT)` = EvaluateandInterpretInformation,
         `Problem Solving (CAT)` = ProblemSolving,
         `Creative Thinking (CAT)` = CreativeThinking,
         `Effective Communication (CAT)` = EffectiveCommunication,
         `Total Score (CAT)` = score_f,
         `Analytic Reasoning and Problem Solving (CLA+)` = pt_aps,
         `Writing Effectiveness (CLA+)` = pt_we,
         `Writing Mechanics (CLA+)` = pt_wm,
         `Performance Task Score (CLA+)` = score_pt,
         `Scientific & Quantitative Reasoning (CLA+)` = sr_sqr,
         `Critical Reading & Evaluation (CLA+)` = sr_cre,
         `Critique an Argument (CLA+)` = sr_ca,
         `Selected Response Score (CLA+)` = score_sr,
         `Total Score (CLA+)` = score_total,
         `Explanation of issues (VALUE)` = `Explanation of issues`,
         `Evidence (VALUE)` = `Evidence`,
         `Context and assumptions (VALUE)` = `Context and assumptions`,
         `Student's position (VALUE)` = `Student' position`,
         `Conclusions and outcomes (VALUE)` = `Conclusions and outcomes`,
         `Define problem (VALUE)` = `Define problem`,
         `Evaluate outcomes (VALUE)` = `Evaluate outcomes`,
         `Evaluate solution (VALUE)` = `Evaluate solution`,
         `Identify strategies (VALUE)` = `Identify strategies`,
         `Implement solution (VALUE)` = `Implement solution`,
         `Solution/hypothesis (VALUE)` = `Solution/hypothoses`,
         `Content development (VALUE)` = `Content development`,
         `Context and purpose (VALUE)` = `Context and purpose`,
         `Control of syntax and mechanics (VALUE)` = `Control of syntax and mechanics`,
         `Genre and conventions (VALUE)` = `Genre and conventions`,
         `Sources of evidence (VALUE)` = `Sources of evidence`
         ) %>% 
  select(project_year,
         `Analytic Reasoning and Problem Solving (CLA+)`,
         `Writing Effectiveness (CLA+)`,
         `Writing Mechanics (CLA+)`,
         `Performance Task Score (CLA+)`,
         `Scientific & Quantitative Reasoning (CLA+)`,
         `Critical Reading & Evaluation (CLA+)`,
         `Critique an Argument (CLA+)`,
         `Selected Response Score (CLA+)`,
         `Total Score (CLA+)`,
         `Evaluate and Interpret Information (CAT)`,
         `Problem Solving (CAT)`,
         `Creative Thinking (CAT)`,
         `Effective Communication (CAT)`,
         `Total Score (CAT)`,
         `Explanation of issues (VALUE)`,
         `Evidence (VALUE)`,
         `Context and assumptions (VALUE)`,
         `Student's position (VALUE)`,
         `Conclusions and outcomes (VALUE)`,
         `Critical Thinking (VALUE)`,
         `Define problem (VALUE)`,
         `Identify strategies (VALUE)`,
         `Solution/hypothesis (VALUE)`,
         `Evaluate solution (VALUE)`,
         `Implement solution (VALUE)`,
         `Evaluate outcomes (VALUE)`,
         `Problem Solving (VALUE)`,
         `Context and purpose (VALUE)`,
         `Content development (VALUE)`,
         `Genre and conventions (VALUE)`,
         `Sources of evidence (VALUE)`,
         `Control of syntax and mechanics (VALUE)`,
         `Written Communication (VALUE)`) 
  
    
  
## CLA & VALUE ---
# p<-correlation_data %>% 
#   #filter(project_year==1) %>% 
#   select(-contains("(CAT)"), - project_year) %>% 
#   psych::corr.test() %>% 
#   .$r

# Function to plot correlations, and output to pdf
# df: data frame of items to correlate with grouping in brackets by test e.g. "Total Score (CAT)", project_year is required
# x:  string of the first group to correlate against
# y:  string of the second group to correlate against
# py: project year to correlate against, "All" for all years
loac_cor <- function(df,x,y,py,title)
{
  
  cairo_pdf(filename = paste0(title,".pdf"), width = 11, height = 8.5)
  if (py != "All") {
    df %>%
      select(contains(paste0("(", x ,")")),contains(paste0("(", y ,")")), project_year) %>%
      filter(project_year == py) %>%
      select(-project_year) %>%
      cor(use = "pairwise.complete.obs") %>%
      corrplot(
        method = "color",
        addgrid.col = "white",
        diag = TRUE,
        title = title,
        type = "lower",
        #            p.mat = p,
        #            sig.level = 0.05,
        #            insig = "blank",
        col = viridis(10),
        tl.cex = 0.6,
        tl.col = "grey20",
        cl.cex = 0.6,
        mar=c(0,0,1,0)
      )
  } else {
    df %>%
      select(contains(paste0("(", x ,")")),contains(paste0("(", y ,")")), project_year) %>%
      select(-project_year) %>%
      cor(use = "pairwise.complete.obs") %>%
      corrplot(
        method = "color",
        diag = TRUE,
        type = "lower",
        title = title,
        addgrid.col = "white",
        #            p.mat = p,
        #            sig.level = 0.05,
        #            insig = "blank",
        col = viridis(10),
        tl.cex = 0.6,
        tl.col = "grey20",
        cl.cex = 0.6,
        mar=c(0,0,1,0)
      )
  }
  dev.off()
}



loac_cor(correlation_data,"CLA+","VALUE",1,"CLA+ - VALUE Correlations - First Year")
loac_cor(correlation_data,"CLA+","VALUE",2,"CLA+ - VALUE Correlations - Second Year")
loac_cor(correlation_data,"CLA+","VALUE","All","CLA+ - VALUE Correlations - Overall")

loac_cor(correlation_data,"CAT","VALUE",2,"CAT - VALUE Correlations - First Year")
loac_cor(correlation_data,"CAT","VALUE",2,"CAT - VALUE Correlations - Second Year")
loac_cor(correlation_data,"CAT","VALUE","All","CAT - VALUE Correlations - Overall")

## Produce the data frame with no missing test information.  Numbers based on only those consenting and WITH a test score.
sub_with_data<-full.loac %>% 
  filter(subject=="APSC", consent > 1) %>% 
  left_join(.,cla_plus %>% select(studentid,year,pt_aps:score_total) %>% filter(!is.na(score_total)), by = c("studentid","year")) %>% 
  left_join(., cat %>% select(studentid,subject,course,q1f:`EffectiveCommunication`), by = c("studentid","subject","course")) %>%
  filter(!is.na(score_total) | !is.na(score_f) | !is.na(VALUE)) %>% 
  group_by(subject,project_year,VALUE) %>% 
  tally %>% 
  tally

#cohen's d function


# VALUE rubric frame for engineering over two years
temp<-full.loac %>% 
  filter(subject=="APSC", consent > 1) %>% 
  inner_join(.,total_value, by = c("studentid","subject","course","project_year")) %>% 
#   rename(project_year = project_year.x) %>% 
#   select(-project_year.y) %>% 
  mutate(project_year = as.numeric(project_year)) %>% 
  mutate(discipline = str_sub(discipline,1,4)) %>% 
  mutate(discipline = ifelse(discipline == "ENCH", "CHEE", discipline)) %>% 
  mutate(discipline = ifelse(discipline %in% c("CMPE","ELEC"), "ECE", discipline)) %>% 
  group_by(project_year, discipline) %>% 
  filter(discipline != "CSCI", discipline != "MATH") %>% 
  tally %>% 
  tally


summarize_each(funs(mean,sd), contains("(VALUE)")) %>% 
  transmute(average = (`Critical Thinking (VALUE)`+`Problem Solving (VALUE)` + `Written Communication (VALUE)`)/3)