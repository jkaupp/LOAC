library(DBI)
library(RMySQL)
library(magrittr)
library(dplyr)
library(stringr)
library(rio)
library(tidyr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

# Connections and Directories ----
# Connection to LOAC Database 
con <- dbConnect(MySQL(), group='LOAC')
student_info <- dbReadTable(con,"student_info")
cla_plus <- dbReadTable(con,"cla_plus")
cat <- dbReadTable(con,"cat")
value <- dbReadTable(con,"VALUE")

fy.loac <- student_info %>% 
  filter(semester==1, course!=103) %>% 
  rename(`first year discipline` = plan) %>% 
  distinct(studentid)

sy.loac <- student_info %>% 
  filter(semester==3 | semester==4, subject != "ENPH") %>% 
  rename(`second year discipline` = plan)


full.loac <- inner_join(fy.loac, sy.loac, by="studentid") %>% 
  filter(!is.na(consent.y)|!is.na(consent.x)) %>% 
  select(studentid, name.y, `second year discipline`, subject.x, course.x, test.x, subject.y, course.y, test.y, year.x, year.y, consent.x, consent.y) %>% 
  unite(fy.course, c(subject.x, course.x), sep = " ") %>% 
  unite(sy.course, c(subject.y, course.y), sep = " ") %>% 
  rename(name = name.y,
         discipline =  `second year discipline`,
         fy = test.x,
         sy = test.y,
         fy.consent = consent.x,
         sy.consent = consent.y) %>% 
  unite(`1`, c(fy.course, fy, year.x, fy.consent), sep = "-") %>% 
  unite(`2`, c(sy.course, sy, year.y, sy.consent), sep = "-") %>% 
  gather(project_year, test, c(`1`,`2`)) %>% 
  separate(test,c("course", "test", "year", "consent"),sep = "-") %>% 
  separate(course,c("subject", "course"), sep = " ")

full.loac$course %<>%
  as.numeric

full.loac$year %<>%
  as.numeric


# Merge consitent test taker frame, subset by CAT, join with CAT data, filter by consenting student, and filter out non-duplicate records----
CAT <- full.loac %>% 
  filter(test=="CAT") %>% 
  left_join(cat, by=c("studentid","subject","course")) %>% 
#   semi_join(student_info %>% 
#               filter(consent>=2), 
#             by= c("studentid","subject","course")) %>% 
  filter(!is.na(score)) %>% 
  group_by(studentid) %>% 
  filter(n()>1) %>% 
  write.csv("LOAC CAT Paired.csv")

# Merge consitent test taker frame, subset by CLA, join with CAT data, filter by consenting student, and filter out non-duplicate records----
CLA <- full.loac %>% 
  filter(test=="CLA+") %>% 
  left_join(cla_plus, by=c("studentid","year")) %>% 
#   semi_join(student_info %>% 
#               filter(consent>=2), 
#             by=c("studentid","year")) %>% 
  filter(!is.na(score_pt)) %>% 
  group_by(studentid) %>% 
  filter(n()>1) %>% 
  write.csv("LOAC CLA Paired.csv")



# VALUE Data Prep----
# Create first year eng data frame for VALUE 
fy.value <- student_info %>% 
  filter(semester==2 | semester==1, course!="101") %>% 
  rename(`first year discipline`= plan) %>% 
  inner_join(value,., by=c("studentid","subject","course")) %>% 
  filter(level!=99) %>% 
  unite(dimension1, rubric, dimension) %>% 
  rename(dimension = dimension1) %>% 
  group_by(studentid, consent, subject, course, dimension) %>% 
  summarize(level=trunc(mean(level))) %>% 
  spread(dimension,level) %>% 
  mutate(program_year=1) %>% 
  select(studentid, program_year, subject, course, consent, everything())
  
  

# Create second year eng data frame
sy.value <- student_info %>% 
  filter(semester==3 | semester==4, subject!="ENPH") %>% 
  rename(`second year discipline`= plan) %>% 
  inner_join(value,., by=c("studentid","subject","course")) %>% 
  filter(level!=99) %>% 
  unite(dimension1, rubric, dimension) %>% 
  rename(dimension = dimension1) %>% 
  mutate(program_year=1) %>% 
  group_by(studentid, consent, subject, course, dimension) %>% 
  summarize(level=trunc(mean(level))) %>% 
  spread(dimension,level) %>% 
  mutate(program_year=2) %>% 
  select(studentid, program_year, subject, course, consent, everything())


VALUE <- bind_rows(fy.value[fy.value$studentid %in% sy.value$studentid,],sy.value[sy.value$studentid %in% fy.value$studentid,]) %>% 
  write.csv("LOAC VALUE.csv")

bind_rows(fy.value,sy.value) %>% 
  write.csv("LOAC VALUE Unpaired.csv")







VALUE$level %<>%
  factor(names(
  table(.)), c("Below Benchmark 1", "Benchmark 1", "Milestone 2", "Milestone 3", "Capstone 4", "Not Assessed"))

# CLA Plots ----
cla.pt <- . %>% {
  mutate(.,project_year = as.numeric(project_year)) %>% 
    filter(time_pt>5 | effort_pt>2) %>% 
    ggplot(aes(x = project_year, y = score_pt)) +
    geom_line(aes(group = studentid), alpha = 0.1) +
    stat_summary(aes(group=1), size=2, color = "blue" ,geom="line", fun.y="mean") +
    annotate("segment", x = 1, xend = 2, y = 0, yend = 0, colour = "grey50", lwd = 0.5) +
    annotate("segment", x = 1, xend = 1, y = min(.$score_pt), yend = max(.$score_pt), colour = "grey50", lwd = 0.5) +
    scale_x_continuous(limits=c(1,2), breaks=c(1,2)) +
    scale_y_continuous(limits=c(0, max(.$score_pt)), breaks=c(min(.$score_pt),max(.$score_pt))) +
    facet_wrap(~discipline, scales = "free") +
    ggtitle("Engineering Student CLA+ Performance by Discipline\n") +
    xlab("\nYear of Project") +
    ylab("Performance Task Score\n") +
    theme(
      text = element_text(family = "Gill Sans MT", size = 22),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text.y = element_text(size = 18, angle = 0),
      strip.text.x = element_text(size = 18),
      axis.text.x = element_text(size = 18, color = "black"),
      axis.text.y = element_text(size = 18, color = "black"),
      panel.background = element_rect(fill = "#FFFFF3"),
      strip.background = element_rect(fill = "#FFFFF3"),
      plot.background = element_rect(fill = "#FFFFF3")
    ) 
} 

#CAT Plot----
CAT %>% {
  mutate(.,project_year = as.numeric(project_year)) %>% 
    ggplot(aes(x = project_year, y = score)) +
    geom_line(aes(group = studentid), alpha = 0.1) +
    stat_summary(aes(group=1), size=1, color = "blue" ,geom="line", fun.y="mean") +
    annotate("segment", x = 1, xend = 2, y = 0, yend = 0, colour = "grey50", lwd = 0.5) +
    annotate("segment", x = 1, xend = 1, y = min(.$score), yend = max(.$score), colour = "grey50", lwd = 0.5) +
    scale_x_continuous(limits=c(1,2), breaks=c(1,2)) +
    scale_y_continuous(limits=c(0, max(.$score)), breaks=c(min(.$score),max(.$score))) +
    facet_wrap(~discipline, scales = "free") +
    ggtitle("Engineering Student CAT Performance by Discipline\n") +
    xlab("\nYear of Project") +
    ylab("CAT Score\n") +
    theme(
      text = element_text(family = "Gill Sans MT", size = 22),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text.y = element_text(size = 18, angle = 0),
      strip.text.x = element_text(size = 18),
      axis.text.x = element_text(size = 18, color = "black"),
      axis.text.y = element_text(size = 18, color = "black"),
      panel.background = element_rect(fill = "#FFFFF3"),
      strip.background = element_rect(fill = "#FFFFF3"),
      plot.background = element_rect(fill = "#FFFFF3")
    ) 
} %>% 
  ggsave("CAT Panel.png",  plot = ., scale = 1, width = 16, height = 14)

#VALUE Plots----
VALUE %>%
{
  mutate(.,project_year = as.numeric(project_year)) %>%
    mutate(discipline = str_sub(discipline,1,4)) %>%
    filter(!is.na(discipline)) %>%
    filter(rubric == "Critical Thinking") %>%
    ggplot(aes(x = project_year, y = level)) +
    geom_point(aes(group = studentid), alpha = 0.2, position = position_jitter(w = 0.1, h = 0.1)) +
    stat_summary(
      aes(group = 1), size = 4, color = "blue" ,geom = "point", fun.y = "mean"
    ) +
    stat_summary(
      aes(group = 1), size = 1, color = "blue" ,geom = "line", fun.y = "mean"
    ) +
    scale_x_continuous(breaks = c(1,2)) +
    scale_y_discrete(
      limits = c(
        "Not Assessed","Below Benchmark 1", "Benchmark 1", "Milestone 2", "Milestone 3", "Capstone 4"
      )
    ) +
    facet_grid(discipline ~ dimension) +
    ggtitle("Engineering Student Critical Thinking by Discipline\n") +
    xlab("\nYear of Project") +
    ylab("Performance Level\n") +
    theme(
      text = element_text(family = "Gill Sans MT", size = 22),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text.y = element_text(size = 14, angle = 0),
      strip.text.x = element_text(size = 14),
      axis.text.x = element_text(size = 14, color = "black"),
      axis.text.y = element_text(size = 14, color = "black"),
      panel.background = element_rect(fill = "#FFFFF3"),
      strip.background = element_rect(fill = "#FFFFF3"),
      plot.background = element_rect(fill = "#FFFFF3")
    ) +
    annotate(
      "segment", x = Inf, xend = -Inf, y = Inf, yend = Inf, colour = "grey50", lwd = 0.5
    )
} %>%
  ggsave(
    "VALUE CT Panel.png", plot = ., scale = 1, width = 16, height = 14
  )
  

CLA %<>% 
  mutate(discipline = str_sub(discipline,1,4)) %>%
  filter(!is.na(discipline)) 

CAT %<>% 
  mutate(discipline = str_sub(discipline,1,4)) %>%
  filter(!is.na(discipline)) 

VALUE %<>%
  mutate(discipline = str_sub(discipline,1,4)) %>%
  filter(!is.na(discipline)) 


## Program Composite Graphic----


LOAC_composite <- function(df.cla, df.cat, df.value, dept) {
  df.cla %<>% filter(discipline == dept)
  df.cat %<>% filter(discipline == dept)
  df.value %<>% filter(discipline == dept)
  
  
  m.df <- gather(df.cla, item, score, pt_aps:score_total)
  
  m.df$item %<>%
    factor(
      names(table(.)),c(
        "Analytic Reasoning & Problem Solving",
        "Writing Effectiveness",
        "Writing Mechanics",
        "Performance Task Score",
        "Scientific and Quantitative Reasoning",
        "Critical Reading & Evaluation",
        "Critque an Argument",
        "Selected Response Questions Score",
        "Total CLA+ Score"
      )
    )
  
  
  if (m.df %>% count <= 0) {
    cla.total <- data.frame() %>%
      ggplot() +
      geom_point() +
      scale_x_continuous(limits = c(1,2), breaks = c(1,2)) +
      scale_y_continuous(limits = c(400,1800)) +
      ggtitle("CLA+") +
      ylab("CLA+ Score") +
      xlab("") +
      theme(
        text = element_text(family = "Gill Sans MT", size = 14),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.text.y = element_text(size = 12, angle = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.background = element_rect(fill = "#FFFFFF"),
        strip.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF")
      ) +
      annotate(
        "segment", x = Inf, xend = -Inf, y = Inf, yend = Inf, colour = "grey50", lwd = 0.5
      )
  } else {
    cla.total <- m.df %>%
      mutate(.,project_year = as.numeric(project_year)) %>%
      filter(item == "Total CLA+ Score") %>%
      ggplot(aes(x = project_year, y = score)) +
      geom_line(aes(group = studentid), alpha = 0.2) +
      stat_summary(
        aes(group = 1), size = 2, color = "blue" ,geom = "line", fun.y = "mean"
      ) +
      annotate(
        "segment", x = 1, xend = 2, y = 400, yend = 400, colour = "grey50", lwd = 0.5
      ) +
      annotate(
        "segment", x = 1, xend = 1, y = 400, yend = 1800, colour = "grey50", lwd = 0.5
      ) +
      scale_x_continuous(limits = c(1,2), breaks = c(1,2)) +
      scale_y_continuous(limits = c(400,1800)) +
      ggtitle("CLA+") +
      ylab("CLA+ Score") +
      xlab("") +
      theme(
        text = element_text(family = "Gill Sans MT", size = 14),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.text.y = element_text(size = 12, angle = 0),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        panel.background = element_rect(fill = "#FFFFFF"),
        strip.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF")
      ) +
      annotate(
        "segment", x = Inf, xend = -Inf, y = Inf, yend = Inf, colour = "grey50", lwd = 0.5
      )
  }
  
  cat.total <- df.cat %>%
    mutate(.,project_year = as.numeric(project_year)) %>%
    ggplot(aes(x = project_year, y = score)) +
    geom_line(aes(group = studentid), alpha = 0.2) +
    stat_summary(
      aes(group = 1), size = 2, color = "blue" ,geom = "line", fun.y = "mean"
    ) +
    annotate(
      "segment", x = 1, xend = 2, y = 0, yend = 0, colour = "grey50", lwd = 0.5
    ) +
    annotate(
      "segment", x = 1, xend = 1, y = 0, yend = 36, colour = "grey50", lwd = 0.5
    ) +
    scale_x_continuous(limits = c(1,2), breaks = c(1,2)) +
    scale_y_continuous(limits = c(0, 36)) +
    ggtitle("CAT") +
    ylab("CAT Score") +
    xlab("") +
    theme(
      text = element_text(family = "Gill Sans MT", size = 14),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text.y = element_text(size = 12, angle = 0),
      strip.text.x = element_text(size = 12),
      axis.text.x = element_text(size = 12, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      panel.background = element_rect(fill = "#FFFFFF"),
      strip.background = element_rect(fill = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF")
    ) +
    annotate(
      "segment", x = Inf, xend = -Inf, y = Inf, yend = Inf, colour = "grey50", lwd = 0.5
    )
  
  value.ct <- df.value %>%
    mutate(.,project_year = as.numeric(project_year)) %>%
    filter(rubric == "Critical Thinking") %>%
    ggplot(aes(x = project_year, y = level)) +
    geom_point(aes(group = studentid), alpha = 0.2, position = position_jitter(w = 0.1, h = 0.1)) +
    stat_summary(
      aes(group = 1), size = 4, color = "blue" ,geom = "point", fun.y = "mean"
    ) +
    stat_summary(
      aes(group = 1), size = 1, color = "blue" ,geom = "line", fun.y = "mean"
    ) +
    scale_x_continuous(breaks = c(1,2)) +
    scale_y_discrete(
      limits = c(
        "Not Assessed","Below Benchmark 1", "Benchmark 1", "Milestone 2", "Milestone 3", "Capstone 4"
      ), labels = function(x)
        str_wrap(x, width = 11)
    ) +
    facet_grid( ~ dimension, labeller = label_wrap_gen(width = 5)) +
    ggtitle("Critical Thinking") +
    xlab("") +
    ylab("Performance Level") +
    theme(
      text = element_text(family = "Gill Sans MT", size = 12),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text.y = element_text(size = 10, angle = 0),
      strip.text.x = element_text(size = 10),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.text.y = element_text(size = 10, color = "black"),
      panel.background = element_rect(fill = "#FFFFFF"),
      strip.background = element_rect(fill = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF")
    ) +
    annotate(
      "segment", x = Inf, xend = -Inf, y = Inf, yend = Inf, colour = "grey50", lwd = 0.5
    )
  
  value.ps <- df.value %>%
    mutate(.,project_year = as.numeric(project_year)) %>%
    filter(rubric == "Problem Solving") %>%
    ggplot(aes(x = project_year, y = level)) +
    geom_point(aes(group = studentid), alpha = 0.2, position = position_jitter(w = 0.1, h = 0.1)) +
    stat_summary(
      aes(group = 1), size = 4, color = "blue" ,geom = "point", fun.y = "mean"
    ) +
    stat_summary(
      aes(group = 1), size = 1, color = "blue" ,geom = "line", fun.y = "mean"
    ) +
    scale_x_continuous(breaks = c(1,2)) +
    scale_y_discrete(
      limits = c(
        "Not Assessed","Below Benchmark 1", "Benchmark 1", "Milestone 2", "Milestone 3", "Capstone 4"
      ), labels = function(x)
        str_wrap(x, width = 5)
    ) +
    facet_grid( ~ dimension, labeller = label_wrap_gen(width = 11)) +
    ggtitle("Problem Solving") +
    xlab("") +
    ylab("Performance Level") +
    theme(
      text = element_text(family = "Gill Sans MT", size = 12),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text.y = element_text(size = 10, angle = 0),
      strip.text.x = element_text(size = 10),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.text.y = element_text(size = 10, color = "black"),
      panel.background = element_rect(fill = "#FFFFFF"),
      strip.background = element_rect(fill = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF")
    ) +
    annotate(
      "segment", x = Inf, xend = -Inf, y = Inf, yend = Inf, colour = "grey50", lwd = 0.5
    )
  
  value.wc <- df.value %>%
    mutate(.,project_year = as.numeric(project_year)) %>%
    filter(!is.na(discipline)) %>%
    filter(rubric == "Written Communication") %>%
    ggplot(aes(x = project_year, y = level)) +
    geom_point(aes(group = studentid), alpha = 0.2, position = position_jitter(w = 0.1, h = 0.1)) +
    stat_summary(
      aes(group = 1), size = 4, color = "blue" ,geom = "point", fun.y = "mean"
    ) +
    stat_summary(
      aes(group = 1), size = 1, color = "blue" ,geom = "line", fun.y = "mean"
    ) +
    scale_x_continuous(breaks = c(1,2)) +
    scale_y_discrete(
      limits = c(
        "Not Assessed","Below Benchmark 1", "Benchmark 1", "Milestone 2", "Milestone 3", "Capstone 4"
      ), labels = function(x)
        str_wrap(x, width = 5)
    ) +
    facet_grid( ~ dimension, labeller = label_wrap_gen(width = 11)) +
    ggtitle("Written Communication") +
    xlab("") +
    ylab("Performance Level") +
    theme(
      text = element_text(family = "Gill Sans MT", size = 12),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text.y = element_text(size = 10, angle = 0),
      strip.text.x = element_text(size = 10),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.text.y = element_text(size = 10, color = "black"),
      panel.background = element_rect(fill = "#FFFFFF"),
      strip.background = element_rect(fill = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF")
    ) +
    annotate(
      "segment", x = Inf, xend = -Inf, y = Inf, yend = Inf, colour = "grey50", lwd = 0.5
    )
  
  layout <- rbind(c(1,1,2,2),
                  c(3,3,3,3),
                  c(4,4,4,4),
                  c(5,5,5,5))
  
  
  title <- paste(dept, "LOAC Results.png", sep = " ")
  
  png(title, width = 1600, height = 1000)
  
  grid.arrange(
    cla.total,cat.total,value.ct,value.ps,value.wc, layout_matrix = layout, top = textGrob(paste0(dept," Engineering LOAC Results"), gp =
                                                                                             gpar(fontsize = 20))
  )
  
  dev.off()
  
}

dept.list <- VALUE$discipline %>% unique %>% unlist

lapply(dept.list, LOAC_composite, df.cla = CLA, df.cat = CAT, df.value = VALUE)



