library(ggplot2)
library(scales)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggthemes)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(wesanderson)


data <- VALUE %>%
  filter(subject == "APSC", course %in% c("101","103","200"), rubric == "Written Communication") %>%
  # group_by(studentid) %>%
  # filter(n()>10) %>%
  mutate(semester = ifelse(course == "101", 1, ifelse(
    course == "103", 2, ifelse(
      course == "200" &
        discipline %in% c("CIVL","MINE","ELEC","CMPE","ENPH","MTHE"), 3, 4
    )
  ))) 
  
# 
# APSC200F <- data %>% 
#   filter(semester != 4) %>% 
#   group_by(studentid) %>%
#   filter(n()>10) %>%
#   group_by(semester, rubric, dimension) %>% 
#   summarize(mean = mean(as.numeric(level)),
#             median = median(as.numeric(level))) %>% 
#   build_slopegraph(x = "semester", y = "mean", group = "dimension", method = "tufte", min.space = 0.05) %>% 
#   mutate(y = round(y,1),
#          x = factor(x,labels = c("First year\nFall Semester", "First year\nWinter Semester", "Second Year\nFall Semester"))
#   ) %>%  
#   plot_slopegraph(4) +
#   ggtitle("APSC 200 Fall Semester Courses") +
#   scale_x_discrete(expand = c(0,0.1)) +
#   theme_tufte(base_size=16, ticks=F) + 
#   theme(axis.title = element_blank(),
#         plot.title = element_text(hjust = -0.1)) 
# 
# APSC200W <- data %>% 
#   filter(semester != 3) %>% 
#   group_by(studentid) %>%
#   filter(n()>10) %>%
#   group_by(semester, rubric, dimension) %>% 
#   summarize(mean = mean(as.numeric(level)),
#             median = median(as.numeric(level))) %>% 
#   build_slopegraph(x = "semester", y = "mean", group = "dimension", method = "tufte", min.space = 0.05) %>% 
#   mutate(y = round(y,1),
#          x = factor(x,labels = c("First year\nFall Semester", "First year\nWinter Semester", "Second Year\nWinter Semester"))
#   ) %>%  
#   plot_slopegraph(4) +
#   ggtitle("APSC 200 Winter Semester Courses") +
#   scale_x_discrete(expand = c(0,0.1)) +
#   theme_tufte(base_size=16, ticks=F) + 
#   theme(axis.title = element_blank(),
#         plot.title = element_text(hjust = -0.1)) 

aggregate.FEAS <- data %>% 
  mutate(semester = ifelse(semester == 4, 3, semester)) %>% 
  group_by(studentid) %>%
  filter(n()>10) %>%
  group_by(semester, rubric, dimension) %>% 
  summarize(mean = mean(as.numeric(level)),
            median = median(as.numeric(level))) %>% 
  build_slopegraph(x = "semester", y = "mean", group = "dimension", method = "tufte", min.space = 0.05) %>% 
  mutate(y = round(y,1),
         x = factor(x,labels = c("First year\nFall Semester", "First year\nWinter Semester", "Second Year")),
         group = str_wrap(group, 20)
  ) %>%  
  plot_slopegraph(6) +
  ggtitle("VALUE Rubric Assessment - Written Communication") +
  scale_x_discrete(expand = c(0,0.1)) +
  theme_tufte(base_size = 24, ticks = F) + 
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = -0.1)) 



# 
# ggsave(plot = aggregate.FEAS, dpi = 300, width = 16, height = 10, filename = "VALUE WC - APSC First & Second Years.pdf")
# 
# pdf(file = "APSC 200 Contrast.pdf", width = 26, height = 10)
# apsc200_contrast <- grid.arrange(grobs = list(APSC200F, APSC200W), ncol = 2)
# dev.off()


# Critical Thinking Small Multiples Histogram
value.hist<-full.VALUE %>% 
  filter(subject=="APSC", rubric=="Critical Thinking", course %in% c("103","200")) %>% 
  mutate(discipline = str_sub(discipline,1,4)) %>% 
  mutate(project_year = factor(project_year, labels = c("First\nYear","Second\nYear")),
         discipline = ifelse(discipline == "ENCH", "CHEE", discipline),
         discipline = ifelse(discipline == "CMPE" | discipline == "ELEC", "ECE", discipline)
  ) %>%
  filter(!is.na(discipline), level != 99) %>% 
  group_by(studentid, project_year, discipline, rubric) %>% 
  summarize(mean = mean(level, na.rm = TRUE))
  
means <- full.VALUE %>% 
  filter(subject=="APSC", rubric=="Critical Thinking", course %in% c("103","200")) %>% 
  mutate(discipline = str_sub(discipline,1,4)) %>% 
  mutate(project_year = factor(project_year, labels = c("First Year","Second Year")),
         discipline = ifelse(discipline == "ENCH", "CHEE", discipline),
         discipline = ifelse(discipline == "CMPE" | discipline == "ELEC", "ECE", discipline)
  ) %>%
  filter(!is.na(discipline), level != 99) %>% 
  group_by(project_year, rubric) %>% 
  summarize(mean = mean(level)) %>% 
  mutate(mean = round(mean,1))


# Slopegraphs
devtools::source_url("https://raw.githubusercontent.com/jkaupp/r-slopegraph/master/slopegraph.r")

value.slope<-full.VALUE %>% 
    filter(subject=="APSC", rubric=="Critical Thinking", course %in% c("103","200")) %>% 
    mutate(discipline = str_sub(discipline,1,4)) %>% 
    mutate(project_year = factor(project_year, labels = c("First Year","Second Year")),
           discipline = ifelse(discipline == "ENCH", "CHEE", discipline),
           discipline = ifelse(discipline == "CMPE" | discipline == "ELEC", "ECE", discipline)
    ) %>%
    filter(!is.na(discipline), level != 99) %>% 
    group_by(project_year, discipline, rubric) %>% 
    summarize(mean = mean(level, na.rm = TRUE)) %>% 
    ungroup %>% 
    mutate(mean = round(mean,1),
      discipline = paste("Department", as.character(as.numeric(factor(discipline)))))


value.slope %>% 
  group_by(project_year, rubric) %>% 
  summarize(mean = mean(mean, na.rm = TRUE)) %>% 
  mutate(discipline = "") %>% 
  bind_rows(value.slope,.) %>% 
  build_slopegraph(., x="project_year", y="mean", group="discipline", method="tufte", min.space=0.05) %>% 
  mutate(y = round(y,1),
         colour = ifelse(group == "", "blue", "grey80"),
         label.colour = ifelse(group == "", "blue", "black")) %>% 
  plot_slopegraph(6) +
  theme_tufte(base_size=20, ticks=F) + theme(axis.title=element_blank())  +
  scale_x_discrete(expand = c(0.1,0))
 
# ggplot small multiple
  
ggplot(value.slope) +
  geom_path(aes(y = mean, x = project_year, group = discipline, width = 0.5, alpha = 1)) +
  geom_path(aes(y = mean, x = project_year, group = 1), width = 0.5, alpha = 1, color = "blue", data = means) +
  geom_point(aes(y = mean, x = project_year, group = discipline), size = 10, color = "white") +
  geom_point(aes(y = mean, x = project_year, group =1), size = 10, color = "white", data = means) +
  geom_text(aes(y = mean, x = project_year, group = discipline, label = mean), size = 4) +
  geom_text(aes(y = mean, x = project_year, group = 1, label = mean), color = "blue", size = 4, data = means) +
  scale_x_discrete(expand =  c(0.1,0), labels = function(x) str_wrap(x,4)) +
  facet_wrap(~discipline, nrow = 1) +
  labs(x = NULL, y = NULL, title = NULL)+
  theme_tufte(base_size = 14) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none",
      panel.border = element_rect(color = "grey80", fill = NA)
    )
  
  ggsave("CT Growth Small Multiple.pdf", width = 4.5, height = 1, scale = 4)
  

# Stars!
full.VALUE %>% 
  filter(subject=="APSC", rubric=="Critical Thinking", course %in% c("103","200")) %>% 
  mutate(discipline = ifelse(discipline == "ENCH", "CHEE", discipline),
         discipline = ifelse(discipline == "CMPE" | discipline == "ELEC", "ECE", discipline)
  ) %>%
  group_by(project_year, discipline, dimension) %>% 
  mutate(level = relevel(level, "Not Assessed"),
         score = as.numeric(level)-2,
         score = ifelse(score<0,0,score)) %>% 
  summarize(mean = mean(score)/4) %>% 
  spread(dimension, mean) %>% 
  filter(project_year==2) %>% 
  {
    names <- .$discipline
    labels <- colnames(.[3:7])
    
    select(.,-project_year,-discipline) %>% 
      stars( radius = TRUE, 
             key.labels = labels, labels = names, scale = FALSE, key.loc = NULL, nrow = 1 )
  } 


VALUE %>% 
  filter(subject=="APSC", project_year == 1, course == "101")


VALUE.CTstar <- function(df,year,key) {
  df %>% 
  filter(subject=="APSC", rubric=="Critical Thinking", course %in% c("103","200")) %>% 
  mutate(discipline = ifelse(discipline == "ENCH", "CHEE", discipline),
         discipline = ifelse(discipline == "CMPE" | discipline == "ELEC", "ECE", discipline)
  ) %>%
  group_by(project_year, discipline, dimension) %>% 
  mutate(level = relevel(level, "Not Assessed"),
         score = as.numeric(level)-2,
         score = ifelse(score<0,0,score)) %>% 
  summarize(mean = mean(score)/4) %>% 
  spread(dimension, mean) %>% 
  filter(project_year==year) %>% 
  {
    names <- .$discipline
    labels <- colnames(.[3:7])
    
    select(.,-project_year,-discipline) %>% 
      stars(
        radius = TRUE,
        key.labels = labels,
        labels = names,
        scale = FALSE,
        key.loc = key,
        nrow = 1,
        len = 1.5,
        main = ifelse(year==1,"First Year","Second Year"),
        lwd = 2,
        flip.labels = FALSE
      )
  } 
}

layout(rbind(c(1,1),c(2,2)))
par(mar = c(3, 3, 3, 3))
VALUE.CTstar(VALUE,2,NULL)
VALUE.CTstar(VALUE,1,c(5,5))


grid.newpage() # Open a new page on grid device
pushViewport(viewport(layout = grid.layout(3, 2)))
print(p3, vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2))
print(p4, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1)) 
print(p5, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 2:2))


# geom_tile for value rubric ----

value.combinations <- expand.grid(
  project_year = levels(value.tile$project_year),
  discipline = unique(value.tile$discipline),
  rubric = unique(value.tile$rubric),
  dimension = unique(value.tile$dimension),
  level = unique(value.tile$level)
) 

value.tile <- full.VALUE %>% 
  filter(subject=="APSC", rubric=="Critical Thinking", course %in% c("103","200")) %>% 
  mutate(discipline = str_sub(discipline,1,4)) %>% 
  mutate(project_year = factor(project_year, labels = c("First Year","Second Year")),
         discipline = ifelse(discipline == "ENCH", "CHEE", discipline),
         discipline = ifelse(discipline == "CMPE" | discipline == "ELEC", "ECE", discipline)
  ) %>%
  filter(!is.na(discipline), level != 99) %>% 
  group_by(project_year, discipline, rubric, dimension,level) %>% 
  tally %>% 
  right_join(.,value.combinations) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         density = ifelse(n==0, 0, n/sum(n))) %>% 
  ungroup %>% 
  mutate(discipline = paste("Department", as.character(as.numeric(factor(discipline)))))

myPalette <- colorRampPalette(rev(brewer.pal(6, "Spectral")), space="Lab")

ggplot(value.tile) +
  geom_tile(aes(x = level, y = dimension, fill = n/sum(n)), color = "black") +
  facet_grid(project_year~discipline, as.table = TRUE) +
  scale_x_discrete(limits = c(0:4), breaks= c(0,1,2,3,4), labels=c("Below Benchmark 1", "Benchmark 1", "Milestone 2", "Milestone 3", "Capstone 4")) +
  scale_fill_gradientn(name = "Number of students", colours = myPalette(100)) +
  labs(x = "\nPerformance Level", y = NULL) +
  coord_fixed(ratio=1) +
  theme_tufte(base_size = 16) +
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle = 0),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1))
  
ggsave("VALUE Heatmap.pdf", width = 4.5, height = 1, scale = 6)

ggsave("CT Growth Small Multiple.pdf", width = 4.5, height = 1, scale = 4)


# CLA distributions
data <- CLA %>%
  group_by(studentid) %>% 
  filter(!is.na(score_total), n()>1) %>% 
  ungroup %>% 
  #filter(discipline %in% p.select) %>% 
  #mutate(project_year = factor(project_year,c(1,2), c("2013-2014","2014-2015"))) %>% 
  select(discipline, project_year,effort_pt,effort_sr) %>%
  rename(`Performance Task Effort` = effort_pt,
         `Selected Response Effort` = effort_sr) %>% 
  gather(Task,Value, -project_year, -discipline)  %>% 
  filter(!is.na(Value)) %>% 
  as.data.frame


means <- data %>%
  group_by(project_year, Task) %>% 
  summarise(mean = mean(Value)) %>% 
  mutate(p = "p < 0.001",
         x = ifelse(Task == "Performance Task Effort", 3.64, 2.88))

ggplot(data) + 
  geom_density(aes(x = Value, fill = project_year, y = ..count..), adjust = 2, alpha = 0.5) +
  geom_segment(data = means, aes(x = mean, xend = mean, y = 0, yend = 75, color = project_year), size = 1.5, show_guide = FALSE) +
  geom_path(data = means, aes(x = round(mean,2), y = 75), color = "#d53e4f", arrow = arrow(length = unit(0.15, "inches"), type = "closed"), size = 1.5) +
  geom_text(data = subset(means, project_year==1), aes(x = mean, y = 80, label = paste("Mean =", round(mean,2)), color = project_year), hjust = -0.5, show_guide = FALSE) +
  geom_text(data = subset(means, project_year==2), aes(x = mean, y = 80, label = paste("Mean =", round(mean,2)), color = project_year), hjust = 1.5, show_guide = FALSE) +
  geom_text(data = subset(means, project_year==1), aes(x = x, y = 80, label = p, fontface = "italic")) +
  facet_wrap(~Task) +
  labs(x = "Self-rated Effort Score", y = "Frequency", title = NULL) +
  scale_color_manual(name = "Year", values = c("#d53e4f", "#3288bd"), labels = c("First Year", "Second Year")) +
  scale_fill_manual(name = "Year", values = c("#d53e4f", "#3288bd"), labels = c("First Year", "Second Year")) +
  theme_tufte(base_size = 18) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = NA, color = "grey50"))
  
ggsave("Effort Differences.png", width = 6, height = 4, scale = 2)


# t.test(subset(data, project_year == 1 & Task == "Selected Response Effort", select="Value"), subset(data, project_year == 2 & Task == "Selected Response Effort", select = "Value"))
# 
# 
devtools::source_gist("https://gist.github.com/kdauria/524eade46135f6348140")

# CLA effort scatterplot
effort<-CLA %>% 
  group_by(studentid) %>% 
  filter(!is.na(score_total), n()>1) %>% 
  ungroup %>% 
  mutate(project_year = factor(project_year,c(1,2), c("First Year","Second Year"))) %>% 
  mutate(time_total = time_pt + time_sr,
         effort_total = trunc((effort_pt + effort_sr)/2)) %>% 
  ggplot(.,aes(x = effort_total, y = score_total)) +
  geom_point(position = position_jitter(w=0.1), size = 4, alpha = 0.5) +
  stat_smooth(method = "lm") +
  stat_smooth_func(geom="text", method="lm", hjust = 0, vjust = -2, parse=TRUE, color = "blue", size = 8) +
  geom_rangeframe() +
  labs(x = "Combined self-rated effort", y = str_wrap("CLA+ score",4), title = NULL) +
  facet_wrap(~project_year) +
  theme_tufte(base_size = 24) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = NA, color = "grey50"),
        axis.title.y = element_text(angle = 0))
        

time<-CLA %>% 
  group_by(studentid) %>% 
  filter(!is.na(score_total), n()>1) %>% 
  ungroup %>% 
  mutate(project_year = factor(project_year,c(1,2), c("First Year","Second Year"))) %>% 
  mutate(time_total = time_pt + time_sr,
         effort_total = (effort_pt + effort_sr)/2) %>% 
  ggplot(., aes(x = time_total, y = score_total)) +
  geom_point(size = 4, alpha = 0.5) +
  stat_smooth(aes(x= time_total, y = score_total), method = "lm", color = "blue") +
  stat_smooth_func(geom="text", method="lm", hjust = 0, vjust = -2, parse = TRUE, color = "blue", size = 8) +
  geom_rangeframe() +
  facet_wrap(~project_year) +
  labs(x = "Total time", y = str_wrap("CLA+ score",4), title = NULL) +
  facet_wrap(~project_year) +
  theme_tufte(base_size = 24) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title.y = element_text(angle = 0))

layout <- rbind(c(1,2,2,2,2,2,2,2,2),
                    c(3,4,4,4,4,4,4,4,4))

png("Score Effort and Time.png", width = 1600, height = 1000)

grid.arrange(textGrob("Effort", y=0.5, gp = gpar(fontsize = 24, fontface = "bold")), effort, textGrob("Time", y=0.56, gp = gpar(fontsize = 24, fontface = "bold")), time, layout_matrix = layout)

dev.off()


full.loac %>% 
  filter(grepl("BSE",discipline), consent>1, test!="MSLQ") %>% 
  mutate(discipline = str_sub(discipline,1,4)) %>% 
  mutate(discipline = ifelse(discipline == "ENCH", "CHEE", discipline)) %>% 
  mutate(discipline = ifelse(discipline %in% c("CMPE","ELEC"), "ECE", discipline)) %>% 
  group_by(discipline,project_year,test) %>% 
  tally() %>% 
  ungroup %>% 
  ggplot(aes(x = project_year, y = n)) +
  geom_path(aes(group = test, color = test), size = 2) +
  geom_path(aes(group = 1), stat = "summary", fun.y = "sum", size = 2, color = "grey50") +
  geom_label(aes(label = n, color = test), size = 6, show.legend = FALSE) +
  geom_label(aes(label = ..y..), stat = "summary", fun.y = "sum", size = 6, show.legend = FALSE, color = "grey50") +
  geom_segment(aes(y = Inf, yend = Inf, x = -Inf, xend = Inf), size = 0.5, alpha = 0.5, color = "grey80") +
  facet_wrap(~discipline, nrow = 2) +
  labs(x="",y="",title="Number of Consenting Students by Test and Total") +
  scale_color_tableau(name = "") +
  scale_x_discrete(labels = c("1"="First Year", "2"="Second Year")) +
  theme_tufte(base_size = 20) +
  theme(text = element_text(color = "grey20"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(color = "grey20")) 

full.loac %>% 
  filter(!grepl("BSE",discipline), consent>1, test %in% c("CAT", "CLA+")) %>% 
  group_by(subject,project_year,test) %>% 
  tally() %>% 
  ungroup %>% 
  ggplot(aes(x = project_year, y = n)) +
  geom_path(aes(group = test, color = test), size = 2) +
  geom_path(aes(group = 1), stat = "summary", fun.y = "sum", size = 2, color = "grey50") +
  geom_label(aes(label = n, color = test), size = 6, show.legend = FALSE) +
  geom_label(aes(label = ..y..), stat = "summary", fun.y = "sum", size = 6, show.legend = FALSE, color = "grey50") +
  geom_segment(aes(y = Inf, yend = Inf, x = -Inf, xend = Inf), size = 0.5, alpha = 0.5, color = "grey80") +
  facet_wrap(~subject, nrow = 1) +
  labs(x="",y="",title="Number of Consenting Students by Test and Total") +
  scale_color_tableau(name = "") +
  scale_x_discrete(labels = c("1"="First Year", "2"="Second Year")) +
  theme_tufte(base_size = 20) +
  theme(text = element_text(color = "grey20"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(color = "grey20")) 

ggsave("FAS Test Counts.png", dpi = 300, scale = 2)

