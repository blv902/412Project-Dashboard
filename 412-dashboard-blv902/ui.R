#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) ; library(edgar) ; library(shinythemes)

# Define UI for application
shinyUI(fluidPage(
  #setup theme
  theme = shinytheme("darkly"),
  #setup page structure and set title
  navbarPage(title = "SEC Filing Dashboard",
    #First tab bar with SEC input
    sidebarPanel(
      tags$h1("Input"),
      textInput("rawLink", "Please enter SEC link", "https://www.sec.gov/edgar/browse/?CIK=320193&owner=exclude"),
    ),
        # Show user what is inputted
        mainPanel(
            h1("Company"),
            verbatimTextOutput("outputLink")
        )
    )
))
