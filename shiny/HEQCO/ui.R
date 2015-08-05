library(shiny)
library(shinythemes)
library(ggvis)

# Define UI for random distribution application 
shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  # Application title
  titlePanel("Queen's HEQOC LOAC Demo: CLA+ Explorer"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      
      selectInput("year", label = "Year of the study", 
                  choices = c("First", "Second", "Fourth"))
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Performance Task", ggvisOutput("plot1")),
                  tabPanel("Selected Response Questions", ggvisOutput("plot2")), 
                  tabPanel("Total Score", ggvisOutput("plot3"))
      ),
      wellPanel(
        span("Number of students:",
             textOutput("n_students")
        )
      )
    )
  )
))

    