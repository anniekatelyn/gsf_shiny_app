library(shiny)
library(ggplot2)

categories = c('Genetic Modification and Testing', 'Electrogenic Human', 'Cloning', 'Modification of Sense of Smell')
description = "This survey provides 4 ways in which humans can use genetic engineering or other technologies to alter the human body for various purposes. 
Not all of these technologies are currently possible, but could be in the future. 
Each scenario is rated with a level of comfort on a scale of 1-3 (1 = opposed, 2 = neutral, and 3 = support). 
The results of this survey analyze conceptions about the ethical dilemmas we face as technological advances break down boundaries between human and machine."

fluidPage(
  tags$head(
    tags$style('#sidebar {margin-top: 4em}',
               'body, label, input, button, select { font-family:Calibri; }'),
    tags$style("h4{ font-weight:bold }"),
    tags$style("h2{ font-weight:bold; margin-left:1em }"),
    tags$style("#plot { margin-top:2em }"),
    tags$style("#demo_plot { margin-top:2em }"),
    tags$style("#qdict { margin-top:2em }")
  ), 
  
  br(),
  
  titlePanel(h2("Survey Results: Technological Modifications of the Human")),
  
  br(),
  
  sidebarPanel(id='sidebar', 
           
    h4("Project Description:"),
    
    textOutput("description"),
    
    br(), br(),
    
    selectInput('cat', h4('Select category to view results:'), categories),
    
    br(),
    
    checkboxInput('count', 'Show Counts'),
    
    checkboxInput('stem', 'Show STEM Breakdown'),
    
    checkboxInput('compare', 'Side-by-Side Comparison'),
    
    htmlOutput("qdict")
    
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Demographics", plotOutput("demo_plot")),
      tabPanel("Take the Survey", htmlOutput("google_form"))
    )
    
  )
)