library(DBI)
library(RMySQL)
library(ggplot2)
library(grid)
library(scales)
library(magrittr)
library(dplyr)
library(stringr)
library(rio)
library(tidyr)

TLO <- dbReadTable(con,"TLO")
data <- TLO %>% 
  filter(subject=="APSC",course=="101") %>% 
  select(-id:-course) %>% 
  gather("scale item","value", -studentid, na.rm = TRUE) %>% 
  filter(!grepl("5",`scale item`) & !grepl("6",`scale item`)) %>% 
  mutate(scale=str_extract(`scale item`,"[A-Z]+"))

data$value %<>% as.integer
data$scale %<>% factor(levels=c("LB","O","OM","SE","SR","TR"), labels=c("Learning Belief","Organization","Outcome Motivation","Self Efficacy","Self Regulation","Transfer"))

dot<-data %>% 
  group_by(scale) %>% 
  summarize(mean=mean(value), sd=sd(value)) %>% 
  ggplot(aes(y=mean, x=scale)) +
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd, color=scale) , size=1) +
  coord_flip() +
  scale_y_continuous(limits=c(1,5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  xlab("Scale Item\n") +
  ylab("\nRating") +
  theme(
    text = element_text(family = "Gill Sans MT", size = 22),
    panel.grid.major.y = element_line(colour = "grey50", size = 0.05),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    strip.text.y = element_text(size = 18, angle = 0),
    strip.text.x = element_text(size = 18),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 18,color = "black"),
    panel.background = element_rect(fill = "#FFFFFF"),
    strip.background = element_rect(fill = "#FFFFFF"),
    plot.background = element_rect(fill = "#FFFFFF")) + 
  scale_color_manual(values=viridis(6))

box<-data %>% 
  ggplot(aes(y=value, x=scale)) +
  geom_boxplot(aes(fill=scale)) +
  coord_flip() +
  scale_y_continuous(limits=c(1,5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  xlab("Scale Item\n") +
  ylab("\nRating") +
  theme(
    text = element_text(family = "Gill Sans MT", size = 22),
    panel.grid.major.y = element_line(colour = "grey50", size = 0.05),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    strip.text.y = element_text(size = 18, angle = 0),
    strip.text.x = element_text(size = 18),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 18,color = "black"),
    panel.background = element_rect(fill = "#FFFFFF"),
    strip.background = element_rect(fill = "#FFFFFF"),
    plot.background = element_rect(fill = "#FFFFFF")) + 
  scale_fill_manual(values=viridis(6))

grid.arrange(dot,box,ncol=2)


#   cla.pt <- m.df %>% 
#   {
#     mutate(.,project_year = as.numeric(project_year)) %>% 
#       filter(item=="Performance Task Score") %>% 
#       ggplot(aes(x = project_year, y = score)) +
#       geom_line(aes(group = studentid), alpha = 0.3) +
#       stat_summary(aes(group=1), size=2, color = "blue" ,geom="line", fun.y="mean") +
#       annotate("segment", x = 1, xend = 2, y = 400, yend = 400, colour = "grey50", lwd = 0.5) +
#       annotate("segment", x = 1, xend = 1, y = 400, yend = 1800, colour = "grey50", lwd = 0.5) +
#       scale_x_continuous(limits=c(1,2), breaks=c(1,2)) +
#       scale_y_continuous(limits=c(400,1800)) +
#       xlab("Year of Project") +
#       ylab("Score") +
#       theme(
#         text = element_text(family = "Gill Sans MT", size = 22),
#         panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position = "none",
#         strip.text.y = element_text(size = 14, angle = 0),
#         strip.text.x = element_text(size = 14),
#         axis.text.x = element_text(size = 14, color = "black"),
#         axis.text.y = element_text(size = 14, color = "black"),
#         panel.background = element_rect(fill = "#FFFFF3"),
#         strip.background = element_rect(fill = "#FFFFF3"),
#         plot.background = element_rect(fill = "#FFFFF3")
#       )
#   }
#   
#   cla.sr <- m.df %>% 
#   {
#     mutate(.,project_year = as.numeric(project_year)) %>% 
#       filter(item=="Selected Response Questions Score") %>% 
#       ggplot(aes(x = project_year, y = score)) +
#       geom_line(aes(group = studentid), alpha = 0.3) +
#       stat_summary(aes(group=1), size=2, color = "blue" ,geom="line", fun.y="mean") +
#       annotate("segment", x = 1, xend = 2, y = 400, yend = 400, colour = "grey50", lwd = 0.5) +
#       annotate("segment", x = 1, xend = 1, y = 400, yend = 1800, colour = "grey50", lwd = 0.5) +
#       scale_x_continuous(limits=c(1,2), breaks=c(1,2)) +
#       scale_y_continuous(limits=c(400,1800)) +
#       xlab("Year of Project") +
#       ylab("Score") +
#       theme(
#         text = element_text(family = "Gill Sans MT", size = 22),
#         panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position = "none",
#         strip.text.y = element_text(size = 14, angle = 0),
#         strip.text.x = element_text(size = 14),
#         axis.text.x = element_text(size = 14, color = "black"),
#         axis.text.y = element_text(size = 14, color = "black"),
#         panel.background = element_rect(fill = "#FFFFF3"),
#         strip.background = element_rect(fill = "#FFFFF3"),
#         plot.background = element_rect(fill = "#FFFFF3")
#       )
#   }


#   cla.pt_sub_scores <- m.df %>% 
#   {
#     mutate(.,project_year = as.numeric(project_year)) %>% 
#       filter(item %in% c("Analytic Reasoning & Problem Solving",
#              "Writing Effectiveness",
#              "Writing Mechanics")) %>%
#       ggplot(aes(x = project_year, y = score)) +
#       geom_point(aes(group = studentid), alpha = 0.2, position = position_jitter(w = 0.1, h = 0.1)) +
#       stat_summary(
#         aes(group = 1), size = 4, color = "blue" ,geom = "point", fun.y = "mean"
#       ) +
#       stat_summary(
#         aes(group = 1), size = 1, color = "blue" ,geom = "line", fun.y = "mean"
#       ) +
#       scale_x_continuous(breaks = c(1,2)) +
#       scale_y_continuous(limits = c(1,6), breaks=seq(1:6)) +
#       facet_grid( ~ item, labeller = label_wrap_gen(width = 5)) +
#       xlab("Year of Project") +
#       ylab("Score") +
#       theme(
#         text = element_text(family = "Gill Sans MT", size = 22),
#         panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position = "none",
#         strip.text.y = element_text(size = 14, angle = 0),
#         strip.text.x = element_text(size = 14),
#         axis.text.x = element_text(size = 14, color = "black"),
#         axis.text.y = element_text(size = 14, color = "black"),
#         panel.background = element_rect(fill = "#FFFFF3"),
#         strip.background = element_rect(fill = "#FFFFF3"),
#         plot.background = element_rect(fill = "#FFFFF3")
#       )
#   }
#       
#   cla.sr_sub_scores <- m.df %>% 
#   {
#     mutate(.,project_year = as.numeric(project_year)) %>% 
#       filter(item %in% c("Scientific and Quantitative Reasoning",
#              "Critical Reading & Evaluation",
#              "Critque an Argument")) %>% 
#       ggplot(aes(x = project_year, y = score)) +
#       geom_line(aes(group = studentid), alpha = 0.3) +
#       stat_summary(aes(group=1), size=2, color = "blue" ,geom="line", fun.y="mean") +
#       annotate("segment", x = 1, xend = 2, y = 200, yend = 200, colour = "grey50", lwd = 0.5) +
#       annotate("segment", x = 1, xend = 1, y = 200, yend = 800, colour = "grey50", lwd = 0.5) +
#       scale_x_continuous(limits=c(1,2), breaks=c(1,2)) +
#       scale_y_continuous(limits=c(200,800)) +
#       xlab("Year of Project") +
#       ylab("Score") +
#       facet_grid(~item, labeller = label_wrap_gen(width = 5)) +
#       theme(
#         text = element_text(family = "Gill Sans MT", size = 22),
#         panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position = "none",
#         strip.text.y = element_text(size = 14, angle = 0),
#         strip.text.x = element_text(size = 14),
#         axis.text.x = element_text(size = 14, color = "black"),
#         axis.text.y = element_text(size = 14, color = "black"),
#         panel.background = element_rect(fill = "#FFFFF3"),
#         strip.background = element_rect(fill = "#FFFFF3"),
#         plot.background = element_rect(fill = "#FFFFF3")
#       )
#   }
#   


