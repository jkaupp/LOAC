library(shiny)
library(shinythemes)
library(ggvis)
library(xtable)

# Define UI for random distribution application 
shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Queen's LOAC Project Explorer"),
  
  sidebarLayout(
    
    sidebarPanel( 
      fluidRow(
        selectInput("year", label = "Year of the study", 
                    choices = c("First", "Second", "Fourth"))),
      hr(),
      fluidRow(h4(textOutput("n_students")))
    ,width=3),
    
    mainPanel(
      navbarPage("CLA+:",
                 
                 tabPanel("Overall Score",
                          fluidPage(
                            fluidRow(column(width = 8, h1("Total CLA+ Score"))),
                            fluidRow(column(width = 8, ggvisOutput("plot3"))),
                            hr(),
                            fluidRow(column(width = 6, h4("Time on task"),
                                            tableOutput("time_summary")),
                                     column(width = 6, h4("Data Summary"),
                                            tableOutput("data_summary"))
                                     )
                          )
                 ),
                 
                 tabPanel("Performance Task",
                          fluidPage(
                            fluidRow(column(width = 12, h1("Performance Task Score"))),
                            fluidRow(column(width = 12, ggvisOutput("plot1"))),
                            hr(),
                            h3("Performance Task Subscores"),
                            fluidRow(column(width = 3, ggvisOutput("aps_plot"), offset=1),
                                     column(width = 3, ggvisOutput("we_plot"), offset=1),
                                     column(width = 3, ggvisOutput("wm_plot"), offset=1)
                            )
                          )
                 ),
                 tabPanel("Selected Response Questions",
                          fluidPage(
                            fluidRow(column(width = 8, h1("Selected Response Score"))),
                            fluidRow(column(width = 8, ggvisOutput("plot2"))),
                            hr(),
                            h3("Selected Response Subscores"),
                            fluidRow(column(width = 3, ggvisOutput("sqr_plot"), offset=1),
                                     column(width = 3, ggvisOutput("cre_plot"), offset=1),
                                     column(width = 3, ggvisOutput("ca_plot"), offset=1)
                            )
                          )
                 )
      )
    )
  )
))