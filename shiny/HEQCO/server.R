library(shiny)
library(plyr)
library(dplyr)
library(magrittr)
library(reshape2)
library(ggvis)
 
cla_plus <- read.csv("cla_plus.csv")

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  cohort <- reactive({
    
    year <- input$year
    
    if(year=="First"){
      t.year <- 2013
      t.window <- "Fall"
    } else if(year=="Second"){
      t.year <- 2014
      t.window <- "Fall"
    }
    else
    {
      t.year <- 2014
      t.window <- "Spring"
    }
    
    data <- cla_plus %>% 
      filter(year==t.year,window==t.window) %>% 
      select(score_pt,score_sr,score_total)
    #%>% 
      #melt(na.rm=TRUE)
    
#     mean_data <- cla_plus %>% 
#        filter(year==t.year,window==t.window) %>% 
#        select(score_pt,score_sr,score_total) %>% 
#        na.omit %>% 
#        summarise(score_pt=mean(score_pt),score_sr=mean(score_sr),score_total=mean(score_total)) %>% 
#        melt() %>% 
#        set_names(c("variable","mean"))
#     
# 
#      data <- join(data,mean_data,by = "variable", type = "left")
    
    data
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  
  cohort %>% 
    select(score_pt) %>%
    ggvis(~score_pt) %>% 
    layer_histograms(width=50) %>%
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>% 
    bind_shiny("plot1")
  
  cohort %>% 
    select(score_sr) %>% 
    ggvis(~score_sr) %>% 
    layer_histograms(width=50) %>% 
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>% 
    bind_shiny("plot2")
  
  cohort %>% 
    select(score_total) %>% 
    ggvis(~score_total) %>% 
    layer_histograms(width=50) %>% 
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>% 
    bind_shiny("plot3")
  
  output$n_students <- renderText({ nrow(cohort()) })
  
  # Generate a summary of the data
#   output$summary <- renderPrint({
#     summary(data())
#   })
#   
#   # Generate an HTML table view of the data
#   output$table <- renderTable({
#     data.frame(x=data())
#   })
#   
})