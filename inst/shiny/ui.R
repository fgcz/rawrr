#
# This is the user-interface definition of a Shiny web application. 

library(shiny)


# Define UI for application that draws a histogram
fluidPage(


    # Application title
    titlePanel("rawrr extract XICs demo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel( 
          list(
              htmlOutput("statistics"),
              htmlOutput("rawfiles"),
              htmlOutput("process"),
              htmlOutput("filter"),
              htmlOutput("tol"),
              htmlOutput("proteins"),
              htmlOutput("peptides"),
              htmlOutput("fragments"))),
      
      # Show a plot of the generated distribution
      mainPanel(
        tagList(
          htmlOutput("tabs")
          )
        #plotOutput("xicPlot", height = 3000)
      )
    )
)
