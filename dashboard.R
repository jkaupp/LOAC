library(shiny)
library(shinydashboard)
library(shinythemes)
library(htmlwidgets)
library(dygraphs)
library(DBI)
library(RMySQL)
library(xts)
library(lubridate)
library(magrittr)
library(dplyr)

# Establish R connection
con <- dbConnect(MySQL(), group='LOAC')

# Pull in tables
student_info <- dbReadTable(con,'student_info')
cla_plus <- dbReadTable(con,'cla_plus')

cla_plus$test_date %<>% 
  strptime(format = "%Y-%m-%d")

xts.cla <- cla_plus %>% 
  select(c(score_pt,score_sr,score_total)) %>% 
  xts(order.by = cla_plus$test_date)


dygraph(xts.cla, main = "CLA+ Data") %>%
  dyRangeSelector()
 
data <- cla_plus %>% 
  filter(year==2014,window=="Fall") %>% 
  select(time_pt,time_sr) %>% 
  summary %>% xtable

cla_plus %>% 
  filter(year==2014,window=="Fall") %>% 
  select(sr_sqr,sr_cre,sr_ca) %>% 
  na.omit %>% 
  ggvis(~sr_sqr) %>% 
  add_axis("x", title="Analytic Reasoning & Problem Solving") %>%
  layer_histograms(width=100)
  

cla_plus$year %<>%
  as.numeric()

data<- student_info %>% 
  filter(consent>=2) %>% 
  semi_join(cla_plus,., by=c("studentid","year"))
