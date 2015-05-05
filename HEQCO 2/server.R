library(shiny)
library(plyr)
library(dplyr)
library(xtable)
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
      filter(year==t.year,window==t.window)
    
    data
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  
  #PT Score Figure
  cohort %>% 
    select(score_pt) %>%
    ggvis(~score_pt) %>% 
    layer_histograms(width=50) %>%
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>% 
    add_axis("x",title = "Performance Task Score") %>% 
    bind_shiny("plot1")
  
  #SR Figure
  cohort %>% 
    select(score_sr) %>% 
    ggvis(~score_sr) %>% 
    layer_histograms(width=50) %>% 
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>% 
    add_axis("x",title = "Selected Response Score") %>% 
    bind_shiny("plot2")
  
  #Total figure
  cohort %>% 
    select(score_total) %>% 
    ggvis(~score_total) %>% 
    layer_histograms(width=50) %>% 
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>%
    add_axis("x",title = "Total CLA+ Score") %>% 
    bind_shiny("plot3")
  
  #PR Figures
  ## PT_APS
  cohort %>% 
    select(pt_aps,pt_we,pt_wm) %>% 
    na.omit %>% 
    ggvis(~pt_aps) %>% 
    add_axis("x", title="Analytic Reasoning & Problem Solving", subdivide = 0, values = 1:6) %>%
    layer_histograms(width=1) %>% 
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>% 
    set_options(width=180, height=95, keep_aspect=TRUE, padding = padding()) %>% 
    bind_shiny("aps_plot")
  
  ## PT-WE
  cohort %>% 
    select(pt_aps,pt_we,pt_wm) %>% 
    na.omit %>% 
    ggvis(~pt_we) %>% 
    add_axis("x", title="Writing Effectiveness", subdivide = 0, values = 1:6) %>%
    layer_histograms(width=1) %>% 
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>% 
    set_options(width=180, height=95, keep_aspect=TRUE, padding = padding()) %>% 
    bind_shiny("we_plot")
  
  ## PT-WM
  cohort %>% 
    select(pt_aps,pt_we,pt_wm) %>% 
    na.omit %>% 
    ggvis(~pt_wm) %>% 
    add_axis("x", title="Writing Mechanics", subdivide = 0, values = 1:6) %>%
    layer_histograms(width=1) %>% 
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>% 
    set_options(width=180, height=95, keep_aspect=TRUE, padding = padding()) %>%  
    bind_shiny("wm_plot")
  
  #SR Figures
  ## SR_SQR
  cohort %>% 
    select(sr_sqr,sr_cre,sr_ca) %>% 
    na.omit %>% 
    ggvis(~sr_sqr) %>% 
    add_axis("x", title="Scientific & Quantitative Reasoning") %>%
    layer_histograms(width=100) %>% 
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>% 
    set_options(width=180, height=95, keep_aspect=TRUE, padding = padding()) %>% 
    bind_shiny("sqr_plot")
  
  ## SR_CRE
  cohort %>% 
    select(sr_sqr,sr_cre,sr_ca) %>%
    na.omit %>% 
    ggvis(~sr_cre) %>% 
    add_axis("x", title="Critical Reading & Evaluation") %>%
    layer_histograms(width=100) %>% 
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>% 
    set_options(width=180, height=95, keep_aspect=TRUE, padding = padding()) %>% 
    bind_shiny("cre_plot")
  
  ## SR_CA
  cohort %>% 
    select(sr_sqr,sr_cre,sr_ca) %>% 
    na.omit %>% 
    ggvis(~sr_ca) %>% 
    add_axis("x", title="Critique an Argument") %>%
    layer_histograms(width=100) %>% 
    add_tooltip(function(df) paste0("count:",(df$stack_upr_ - df$stack_lwr_))) %>% 
    set_options(width=180, height=95, keep_aspect=TRUE, padding = padding()) %>%  
    bind_shiny("ca_plot")
  
  
  output$time_summary <- renderTable({
    cohort() %>% 
      select(time_pt, time_sr) %>% 
      mutate(time_total=time_pt+time_sr) %>% 
      plyr::rename(c("time_pt" = "Performance Task",
                     "time_sr" = "Selected Response Items", 
                     "time_total" = "Total Time")) %>% 
      na.omit() %>% 
      summary %>% 
      xtable()
  }, include.rownames = FALSE)
  
  output$data_summary <- renderTable({
    cohort() %>% 
      select(score_total, pr_across_total) %>% 
      plyr::rename(c("score_total" = "Total Score",
                     "pr_across_total" = "Percentile Rankings")) %>% 
      na.omit() %>% 
      summary %>% 
      xtable()
  }, include.rownames = FALSE)
  
  output$n_students <- renderText({ paste("Number of students:",nrow(cohort()), sep="  ") })
  
})