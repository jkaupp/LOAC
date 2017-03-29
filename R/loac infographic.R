library(magrittr)
library(dplyr)
library(purrr)
library(stringr)
library(grid)
library(jkmisc)
library(ggplot2)
library(jpeg)
library(tidyr)
library(GGally)
library(gridExtra)
library(showtext)
library(Cairo)
library(readxl)


students <- read_excel(list.files("./data", pattern = "report", full.names = TRUE))

# Database Connection----
loac <- RSQLServer::src_sqlserver("FEAS", database = "HEQCO-LOAC")

# df <- list(studentid = 10089368, name = "Jake", subject = "Engineering")
# Construct data frame for CAT and VALUE means

national_table <- data.frame(year = c("First Year","Second Year","Third Year","Fourth Year","First Year", "Fourth Year"),
                             instrument = c(rep("CAT", 4), rep("CLA+", 2)),
                             value = c(14.8,15.1,17,19.4, 1039, 1128),
                             group = "International Average")

# Construct project value tables----
value <- loac %>% 
  tbl("value") %>% 
  collect() %>% 
  filter(level != "99", !is.na(level), course < 400)

summary_value <- value %>% 
  group_by(studentid, subject, term, rubric) %>% 
  summarize(value = mean(as.numeric(level))) %>% 
  ungroup %>% 
  mutate(instrument = "VALUE") %>% 
  mutate(subject = ifelse(subject %in% c("APSC","GEOE","MECH","ELEC","CMPE","MINE","MTHE","ENPH","CIVL","CHEE"), "Engineering",
                          ifelse(subject == "PSYC", "Psychology",
                                 ifelse(subject == "PHYS", "Physics",
                                        ifelse(subject == "DRAM", "Drama", subject)))))

# Construct project CAT Tables----
cat <- loac %>% 
  tbl("cat") %>% 
  filter(!is.na(score_f)) %>% 
  collect() 
  
summary_cat_v5 <- cat %>% 
  filter(testversion == 5) %>% 
  select(term, studentid, score_f) %>% 
  mutate(instrument = "CAT") %>% 
  rename(value = score_f) 

summary_cat <- cat %>% 
  filter(testversion == 6) %>% 
  mutate(q1f = q1f*1,
         q2f = q2f*1.11,
         q3f = q3f*1.29,
         q4f = q4f*1.56,
         q5f = q5f*0.98,
         q6f = q6f*1.04,
         q7f = q7f*0.95,
         q8f = q8f*1.03,
         q9f = q9f*1.10,
         q10f = q10f*1.05,
         q11f = q11f*1.48,
         q12f = q12f*1.21,
         q13f = q13f*0.79,
         q14f = q14f*1.21,
         q15f = q15f*1.62) %>% 
  mutate(score_f = q1f + q2f + q3f + q4f + q5f + q6f + q7f + q8f + q9f + q10f + q11f + q12f + q13f + q14f + q15f) %>% 
  select(term, studentid, score_f) %>% 
  mutate(instrument = "CAT") %>% 
  rename(value = score_f) %>% 
  bind_rows(summary_cat_v5)
  

#  mutate(ability = "Problem Solving|Critical Thinking")

summary_cat_dimensions <- cat %>% 
  select(term, studentid, matches("Evaluate|Problem|Creative|Effective")) %>% 
  gather(sub_score, value, -studentid, -term) %>% 
  mutate(instrument = "CAT") %>% 
  mutate(sub_score = ifelse(grepl("Evaluate", sub_score),  "Evaluate and interpret information",
                               ifelse(grepl("Problem", sub_score), "Problem solving",
                               ifelse(grepl("Creative", sub_score), "Creative thinking",
                               ifelse(grepl("Effective", sub_score), "Effective communication", sub_score)))))



# Construct project CLA Tables----
cla <- loac %>% 
  tbl("cla_plus") %>% 
  filter(!is.na(score_total)) %>% 
  collect() 

summary_cla <- cla %>% 
  select(term, studentid, score_total) %>% 
  mutate(instrument = "CLA+") %>%
  rename(value = score_total)
  


  #mutate(ability = "Problem Solving|Critical Thinking|Written Communication")

summary_cla_dimensions <- cla %>% 
  select(term,studentid,pt_aps,pt_we,pt_wm,sr_sqr,sr_cre,sr_ca) %>% 
  gather(sub_score, value, -studentid, -term) %>% 
  mutate(instrument = "CLA+") %>% 
  mutate(sub_score = ifelse(sub_score == "pt_aps", "Analysis & problem solving",
                               ifelse(sub_score == "pt_wm", "Writing mechanics",
                               ifelse(sub_score == "pt_we", "Writing effectiveness",
                               ifelse(sub_score == "sr_ca", "Critique an argument",
                               ifelse(sub_score == "sr_cre", "Critical reading & evaluation",
                               ifelse(sub_score == "sr_sqr", "Scientific & quantitative reasoning", sub_score)))))))


# Bind all project tables together
full_results <- plyr::rbind.fill(summary_value, summary_cat, summary_cla) %>% 
  mutate(year = ifelse(term %in% c(2139,2141), "First Year",
                          ifelse(term %in% c(2149,2151), "Second Year",
                          ifelse(term %in% c(2159,2161),"Third Year", NA))))

full_dimension_results <- plyr::rbind.fill(summary_cat_dimensions, summary_cla_dimensions) %>% 
  mutate(year = ifelse(term %in% c(2139,2141), "First Year",
                       ifelse(term %in% c(2149,2151), "Second Year",
                              ifelse(term %in% c(2159,2161),"Third Year", NA))))


# Build Summary Data ----

discipline_value <- full_results %>% 
  filter(!is.na(subject)) %>% 
  group_by(year, subject, rubric, instrument) %>% 
  summarize(value = mean(value)) %>% 
  ungroup %>% 
  mutate(group = "Discipline Average")

summary_results <- full_results %>% 
  group_by(year, instrument, rubric) %>% 
  summarize(value = mean(value)) %>% 
  ungroup %>% 
  mutate(group = "Queen's Average") %>% 
  plyr::rbind.fill(national_table) %>% 
  plyr::rbind.fill(discipline_value) 


summary_dimension_results <- full_dimension_results %>% 
  group_by(year, instrument, sub_score) %>% 
  summarize(value = mean(value)) %>% 
  ungroup %>% 
  mutate(group = "Queen's Average")



# Code to run reports ----
results <- loac %>% 
  tbl("student_info") %>% 
  filter(studentid %in% students$studentid) %>% 
  collect() %>% 
  mutate(subject = ifelse(subject %in% c("APSC","GEOE","MECH","ELEC","CMPE","MINE","MTHE","ENPH","CIVL","CHEE"), "Engineering",
                          ifelse(subject == "PSYC", "Psychology",
                                 ifelse(subject == "PHYS", "Physics",
                                        ifelse(subject == "DRAM", "Drama", subject))))) %>% 
  distinct(studentid, name, email, subject) %>% 
  group_by(studentid, name, email, subject) %>% 
  do(success = loac_student_report(.))



# Function to generate reports----
loac_student_report <- function(df) {
  
  vplayout <- function(x, y) {
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  
  
# Filter to student results from full results----
student_results <- filter(full_results, studentid == df$studentid) %>% 
  group_by(year, instrument, rubric) %>% 
  summarize(value = mean(value)) %>% 
  ungroup %>% 
  mutate(group = "Your Score")

student_dimension_results <- filter(full_dimension_results, studentid == df$studentid) %>% 
  group_by(year, instrument, sub_score) %>% 
  summarize(value = mean(value)) %>% 
  ungroup %>% 
  mutate(group = "Your Score")  

# Avoid plotting students that don't have data----
 if (nrow(student_results) > 0) {
# Build Plot Data----
plot_data <- bind_rows(summary_results, student_results) %>% 
  complete(year, nesting(instrument, rubric, group, subject), fill =  list(value = NA)) %>% 
  filter(!(year %in% c("Second Year","Third Year") & instrument == "CLA+" & group == "International Average")) %>% 
  filter(!(group == "Your Score" & is.na(value)))

dimension_plot_data <- bind_rows(summary_dimension_results, student_dimension_results) %>% 
  complete(year = c("First Year", "Second Year", "Third Year", "Fourth Year"), nesting(instrument, sub_score, group), fill = list(value = NA))


# Construct cla plots----
cla_plot <- ggplot(filter(plot_data, instrument == "CLA+"), aes(x = year, y = value)) +
  geom_line(aes(color = group, group = group), show.legend = FALSE) +
  geom_point(aes(color = group), size = 3) +
  scale_y_continuous(limits = c(800,1650), breaks = c(962,1098,1222,1369,1604), labels = c("Below Basic","Basic","Proficient","Accomplished", "Advanced"), expand = c(0,0)) +
  scale_x_discrete(limits = c("First Year","Second Year","Third Year", "Fourth Year")) +
  scale_color_manual(NULL, values = c("Queen's Average" = "#eebd31", "International Average" =  "#11335d", "Your Score" = "#9d1939")) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL ) +
  theme_jk(grid = "Y", plot_title_family = "Calibri", axis_title_family = "Palatino Linotype", strip_text_family = "Calibri", subtitle_family = "Palatino Linotype", base_family = "Palatino Linotype", base_size = 8) +
  theme(legend.position = "bottom")

cla_pt_dimension_plot <- ggplot(filter(dimension_plot_data, instrument == "CLA+", sub_score %in% c("Analysis & problem solving","Writing mechanics","Writing effectiveness")), aes(x = year, y = value)) +
  geom_line(aes(color = group, group = group), show.legend = FALSE) +
  geom_point(aes(color = group), size = 3) +
  facet_wrap(~ sub_score) +
  scale_x_discrete(limits = c("First Year","Second Year","Third Year", "Fourth Year"), labels = function(x) str_wrap(x, 6)) +
  scale_y_continuous(limits = c(1,6), breaks = c(1:6)) +
  scale_color_manual(NULL, values = c("Queen's Average" = "#eebd31", "Your Score" = "#9d1939")) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
  theme_jk(grid = "Y", plot_title_family = "Calibri", axis_title_family = "Palatino Linotype", strip_text_family = "Calibri", subtitle_family = "Palatino Linotype", base_family = "Palatino Linotype", base_size = 8, strip_text_size = 8) +
  theme(legend.position = "none",
        axis.text.x = element_blank())

cla_sr_dimension_plot <- ggplot(filter(dimension_plot_data, instrument == "CLA+",  !sub_score %in% c("Analysis & problem solving","Writing mechanics","Writing effectiveness")), aes(x = year, y = value)) +
  geom_line(aes(color = group, group = group), show.legend = FALSE) +
  geom_point(aes(color = group), size = 3) +
  facet_wrap(~ sub_score) +
  scale_x_discrete(limits = c("First Year","Second Year","Third Year", "Fourth Year"), labels = function(x) str_wrap(x, 6)) +
  scale_y_continuous(limits = c(200,800), expand = c(0.1,0)) +
  scale_color_manual(NULL, values = c("Queen's Average" = "#eebd31", "Your Score" = "#9d1939")) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
  theme_jk(grid = "Y", plot_title_family = "Calibri", axis_title_family = "Palatino Linotype", strip_text_family = "Calibri", subtitle_family = "Palatino Linotype", base_family = "Palatino Linotype", base_size = 8, strip_text_size = 8) +
  theme(legend.position = "none")

# Construct cat plot----
cat_plot <- ggplot(filter(plot_data, instrument == "CAT"), aes(x = year, y = value)) +
  geom_line(aes(color = group, group = group), show.legend = FALSE) +
  geom_point(aes(color = group), size = 3) +
  scale_y_continuous(limits = c(0,40), expand = c(0.1, 0)) +
  scale_x_discrete(limits = c("First Year","Second Year","Third Year", "Fourth Year")) +
  scale_color_manual(NULL, values = c("Queen's Average" = "#eebd31", "International Average" =  "#11335d", "Your Score" = "#9d1939")) +
  labs(x = NULL, y = NULL, subtitle = NULL, title = NULL) +
  theme_jk(grid = "Y", plot_title_family = "Calibri", axis_title_family = "Palatino Linotype", strip_text_family = "Calibri", subtitle_family = "Palatino Linotype", base_family = "Palatino Linotype", base_size = 8) +
  theme(legend.position = "bottom")

cat_dimension_plot <- ggplot(filter(dimension_plot_data, instrument == "CAT"), aes(x = year, y = value)) +
  geom_line(aes(color = group, group = group), show.legend = FALSE) +
  geom_point(aes(color = group), size = 3) +
  facet_wrap(~sub_score, nrow = 2) +
  scale_x_discrete(limits = c("First Year","Second Year","Third Year", "Fourth Year"), labels = function(x) str_wrap(x, 6)) +
  scale_y_continuous(limits = c(0,30), expand = c(0.2,1)) +
  scale_color_manual(NULL, values = c("Queen's Average" = "#eebd31", "Your Score" = "#9d1939")) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
  theme_jk(grid = "Y", plot_title_family = "Calibri", axis_title_family = "Palatino Linotype", strip_text_family = "Calibri", subtitle_family = "Palatino Linotype", base_family = "Palatino Linotype", base_size = 8, strip_text_size = 8) +
  theme(legend.position = "none",
        axis.text = element_text(size = 6))



# Construct value plot----

value_plot <- ggplot(filter(plot_data, instrument == "VALUE", subject == df$subject | is.na(subject)), aes(x = year, y = value)) +
  geom_line(aes(color = group, group = group), show.legend = FALSE) +
  geom_point(aes(color = group), size = 3) +
  facet_wrap(~ rubric, nrow = 1) +
  scale_x_discrete(limits = c("First Year","Second Year","Third Year", "Fourth Year")) +
  scale_y_continuous(limits = c(0,4), labels = c("Below Benchmark 0", "Benchmark 1", "Milestone 2", "Milestone 3", "Capstone 4")) +
  scale_color_manual(NULL, values = c("Queen's Average" = "#eebd31", "Discipline Average" =  "#11335d", "Your Score" = "#9d1939")) +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL) +
  theme_jk(grid = "Y", plot_title_family = "Calibri", axis_title_family = "Palatino Linotype", strip_text_family = "Calibri", subtitle_family = "Palatino Linotype", base_family = "Palatino Linotype", subtitle_size = 8, plot_title_size = 10, base_size = 8, strip_text_size = 8) +
  theme(legend.position = "bottom")


# Read in banner and divider----
banner <- readJPEG(list.files("./graphics", full.name = TRUE, pattern = "lop_banner.jpg"))
divider <- readJPEG(list.files("./graphics", full.name = TRUE, pattern = "divider.jpg"))


# Build Infographic Text----
cla_text <- "The CLA+ assesses critical thinking, problem solving, and written communication using short-answer and multiple choice questions.\nThe scores on the vertical axis are the CLA+ mastery levels, descriptions available at: http://www.queensu.ca/qloa"

cat_text <- "The CAT assesses critical thinking and problem solving using short-answer questions.  The vertical axis shows the scores out of a possible maximum of 38."

value_text <- "The VALUE rubrics assess critical thinking, problem solving, and written communication exhibited in student submitted coursework.\n  The scores on the vertical axis are the performance levels in the rubric, descriptions available at: http://www.queensu.ca/qloa"

text <- str_wrap("For the past four years, you have participated in the Queen's Learning Outcomes Project, an educational research project that has been determining approaches to assessing students abilities in Critical Thinking, Problem Solving and Written Communication. The charts below present your assessment results relative to Queen's, Disciplinary or International results.  These illustrate how you and your peers developed these skills during your time at Queen's, and would be a valuable resource to show potential employers.  You can find more information about the project, including detailed descriptions of the assessment approaches at http://www.queensu.ca/qloa", 150)

# Build Student Text----
student_text <- sprintf("%i %s", df$studentid, df$name)
#dummy_text <- sprintf("1000000 Minion, Ima")

file_name_1 <- sprintf("%s_Page_1.pdf", df$studentid)
file_name_2 <- sprintf("%s_Page_2.pdf", df$studentid)
merged_name <- sprintf("%s_Learning_Outcomes_Project_Report.pdf", df$studentid)


file_path_1 <- normalizePath(file.path(".", "student_reports", file_name_1))
file_path_2 <- normalizePath(file.path(".", "student_reports", file_name_2))
merged_path <- normalizePath(file.path(".", "student_reports", merged_name))

# Build Infographic page 1----
font.add("Calibri","Calibri.ttf")

CairoPDF(file_path_1, width = 11, height = 8.5)
grid.newpage()
showtext.begin()
pushViewport(viewport(layout = grid.layout(8, 4, heights = c(1,0.75,0.2,0.2,0.4,1,1,1))))
# Red header and banner
grid.rect(width = 1, gp = gpar(col = "#9d1939", fill = "#9d1939"),  vp = vplayout(1, 1:4))
grid.raster(banner,  vp = vplayout(1, 1:4))
# Project text
grid.text(text, vp = vplayout(2, 1:4), gp = gpar(fontfamily = "Palatino Linotype", fontsize = 10))
grid.raster(divider,  vp = vplayout(3, 1:4))
# Red middle header
grid.rect(width = 1, gp = gpar(col = "#9d1939", fill = "#9d1939"),  vp = vplayout(4, 1:4))
grid.text("Valid Assessment of Learning in Undergraduate Education (VALUE) Rubrics", vp = vplayout(4, 1:4), gp = gpar(fontfamily = "Calibri", fontsize = 14, col = "white"))
grid.text(value_text, vp = vplayout(5, 1:4), gp = gpar(fontfamily = "Calibri", fontsize = 10, col = "black"))
# Value plot
print(value_plot, vp = vplayout(6:8, 1:4))
showtext.end()
dev.off()

# Build Infographic page 2----
CairoPDF(file_path_2, width = 11, height = 8.5)
grid.newpage()
showtext.begin()
pushViewport(viewport(layout = grid.layout(15, 4, heights = c(0.2,0.3,0.5,0.5,0.5,0.5,0.5,0.5,0.2,0.2,0.5,0.5,0.5,0.5,0.2))))
# Top red header
grid.rect(gp = gpar(col = "#9d1939", fill = "#9d1939"),  vp = vplayout(1, 1:4))
grid.text("Collegiate Learning Assessment (CLA+) Scores", vp = vplayout(1, 1:2), gp = gpar(fontfamily = "Calibri", fontsize = 14, col = "white"))
grid.text("CLA+ Sub-scores", vp = vplayout(1, 3:4), gp = gpar(fontfamily = "Calibri", fontsize = 14, col = "white"))
grid.text(cla_text, vp = vplayout(2, 1:4), gp = gpar(fontfamily = "Calibri", fontsize = 10, col = "black"))
# CLA plots
print(cla_plot, vp = vplayout(3:8, 1:2))
print(cla_pt_dimension_plot, vp = vplayout(3:5, 3:4))
print(cla_sr_dimension_plot, vp = vplayout(6:8, 3:4))
# Middle red header
grid.rect(gp = gpar(col = "#9d1939", fill = "#9d1939"),  vp = vplayout(9, 1:4))
grid.text("Critical Thinking Assessment Test (CAT) Scores", vp = vplayout(9, 1:2), gp = gpar(fontfamily = "Calibri", fontsize = 14, col = "white"))
grid.text("CAT Sub-scores", vp = vplayout(9, 3:4), gp = gpar(fontfamily = "Calibri", fontsize = 14, col = "white"))
# Cat plots
grid.text(cat_text, vp = vplayout(10, 1:4), gp = gpar(fontfamily = "Calibri", fontsize = 10, col = "black"))
print(cat_plot, vp = vplayout(11:14, 1:2))
print(cat_dimension_plot , vp = vplayout(11:14, 3:4))
# Bottom red header
grid.rect(gp = gpar(col = "#9d1939", fill = "#9d1939"),  vp = vplayout(15, 1:4))
grid.text(student_text, vp = vplayout(15, 1:4), gp = gpar(fontfamily = "Calibri", fontsize = 14, col = "white"))
showtext.end()
dev.off()

plotflow:::mergePDF(
  in.file = paste(file_path_1, file_path_2, collapse = " "),
  file = merged_path
)

file.remove(file_path_1, file_path_2)

}

return(TRUE)
}

