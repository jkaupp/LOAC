library(magrittr)
library(dplyr)
library(tidyr)
library(rio)
library(ggplot2)
library(ggthemes)
library(scales)
library(grid)
library(gridExtra)

ORIP.dir <- "/Users/Jake/ownCloud/FEAS/QUQAP/OIRP Data/"

admissions.data <-
  import(paste0(ORIP.dir, "06-SF-Student Learning Experience-Queen's First Entry Admission Average-OUR (Mar 2015).xlsx"), sheet=1, skip = 1) %>%
  set_names(c("Faculty","2008",	"2009",	"2010",	"2011",	"2012",	"2013",	"2014",	"2015",	"2016","drop1","drop2")) %>% 
  select(-contains("drop")) %>% 
  filter(!is.na(Faculty)) %>% 
  gather(year, average, -Faculty) %>% 
  filter(year %in% c(2008,2009,2010,2011,2012,2013,2014), !grepl("Target", Faculty)) %>% 
  mutate(color = ifelse(grepl("Engineering", Faculty),"blue","black"),
         color = ifelse(grepl("Overall", Faculty),"red", color),
         alpha = ifelse(grepl("Engineering", Faculty) | grepl("Overall", Faculty), 1, 0.2))

ggplot(admissions.data) +
  geom_line(aes(x = year, y = average, group = Faculty, color = color, alpha = alpha)) +
  geom_point(aes(x = year, y = average, group = Faculty, color = color, alpha = alpha)) +
  geom_text(aes(x = year, y = average, label = percent(round(average,3)), color = color), data = subset(admissions.data, year==2014 & grepl("Engineering", Faculty) | year==2014 & grepl("Overall", Faculty)), hjust = -0.5, show_guide = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(x = "\nYear", y = "Admission\nAverage", title = NULL) +
  scale_color_identity(guide = "legend", labels =c("Other Queen's Faculties", "FEAS", "Queen's Average"), name =  "") +
  scale_alpha_identity() +
  theme_tufte(base_size = 20) +
  theme(
    axis.title.y = element_text(angle = 0),
    legend.position = "bottom"
  )


  
completion.data <-
    import(
      paste0(
        ORIP.dir,"06-SF-Student Learning Experience-ENG Degree Completion (Mar 2015).xlsx"
      ),sheet = 1, skip = 4
    )  %>%
    set_names(
      c(
        "Faculty","2006","2007","2008","2009","2010","2011","2012","2013","2014","Fac","drop"
      )
    ) %>%
    select(-drop,-Fac) %>%
    slice(1:3) %>%
    gather(year, average,-Faculty) %>%
    mutate(
      average = as.numeric(average) / 100,
      color = ifelse(grepl("Engineering", Faculty),"blue","black"),
      color = ifelse(grepl("Queen's", Faculty),"red", color),
      alpha = ifelse(
        grepl("Engineering", Faculty) |
          grepl("Queen's", Faculty), 1, 0.2
      )
    ) %>%
    {
      ggplot(.) +
        geom_line(aes(
          x = year, y = average, group = Faculty, color = color, alpha = alpha
        )) +
        geom_point(aes(
          x = year, y = average, group = Faculty, color = color, alpha = alpha
        )) +
        geom_text(
          aes(
            x = year, y = average, label = percent(average), color = color
          ), data = subset(., year == 2014), hjust = -0.5, show_guide = FALSE
        ) +
        scale_y_continuous(labels = percent) +
        labs(x = "\nYear", y = "Completion\nAverage", title = NULL) +
        scale_color_identity(
          guide = "legend", labels = c("Queen's Target", "FEAS", "Queen's"), name =  ""
        ) +
        scale_alpha_identity() +
        theme_tufte(base_size = 20) +
        theme(axis.title.y = element_text(angle = 0),
              legend.position = "bottom")
    }

continuation.data <-
    import(
      paste0(
        ORIP.dir,"06-SF-Student Learning Experience-ENG Continuation Yr 1 to Yr 2-CSRDE (Mar 2015).xlsx"
      ), sheet = 1, skip = 2
    )  %>%
    set_names(c(
      "drop","Faculty","2008","2009","2010","2011","2012","2013","2014","2015"
    )) %>%
    select(-drop) %>%
    slice(1:3) %>%
    mutate(Faculty = ifelse(is.na(Faculty), "Queen's", Faculty)) %>%
    gather(year, average,-Faculty) %>%
    filter(year != "2014" & year != "2015") %>%
    mutate(
      average = as.numeric(average),
      color = ifelse(grepl("BSCE", Faculty),"blue","black"),
      color = ifelse(grepl("Queen's", Faculty),"red", color),
      alpha = ifelse(grepl("BSCE", Faculty) |
                       grepl("Queen's", Faculty), 1, 0.2)
    ) %>%
    {
      ggplot(.) +
        geom_line(aes(
          x = year, y = average, group = Faculty, color = color, alpha = alpha
        )) +
        geom_point(aes(
          x = year, y = average, group = Faculty, color = color, alpha = alpha
        )) +
        geom_text(
          aes(
            x = year, y = average, label = percent(average), color = color
          ), data = subset(., year == 2013), hjust = -0.5, show_guide = FALSE
        ) +
        scale_y_continuous(labels = percent) +
        labs(x = "\nYear", y = "Continuation\nAverage", title = NULL) +
        scale_color_identity(
          guide = "legend", labels = c("Queen's Target", "FEAS", "Queen's"), name =  ""
        ) +
        scale_alpha_identity() +
        theme_tufte(base_size = 20) +
        theme(axis.title.y = element_text(angle = 0),
              legend.position = "none")
    }

png(file="FEAS Retention.png", res = 300, width = 6000, height = 4000)
grid.arrange(continuation.data, completion.data)
dev.off()

