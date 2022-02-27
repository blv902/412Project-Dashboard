#Author: Benjamin Vong
#title: SEC Dashboard
#date: 

library(shiny) ; library(edgar) ; library(shinythemes)

# Define UI for application
shinyUI(fluidPage(
  #setup theme
  theme = shinytheme("darkly"),
  #setup page structure and set title
  navbarPage(title = "SEC Filing Dashboard",
    #First tab bar with SEC input
    tabPanel("Setup",
             #Separates first tab into 4 section
             fluidRow(
               column(4,
                      #Ask for user's email to report to SEC
                      textInput("userEmail", "Please enter email (required by SEC)", "useragent@domain.com"),
                      br(),
                      #Ask if user wants 10-k or 10-Q
                      paste('Please select type of form'),
                      checkboxInput('raw10-k', '10-k', value = TRUE),
                      checkboxInput('raw10-Q', '10-Q', value = FALSE),
               ),
               column(4,
                      #Ask for the SEC link
                      textInput("rawLink", "Please enter SEC link", "https://www.sec.gov/edgar/browse/?CIK=320193&owner=exclude"),
                      br(),
                      textInput("rawYears", "Please enter years (Example:2009,2010,2019,2021)", "2009,2010"),
               ),
               column(4,
                      #Ask what quarter if 10-Q
                      sliderInput("rawQuarter", "Select no. of quarter",1,4,1),
                      h5("Quarter 1: January 1 to March 31"),
                      h5("Quarter 2: April 1 to June 30"),
                      h5("Quarter 3: July 1 to September 30"),
                      h5("Quarter 4: October 1 to December 31"),
               ),
             ),
    #Breaks into next part
    hr(),
    #Display the company the user selected
    h1("Company"),
    verbatimTextOutput("outputLink"),
    paste('Press to start'),
    checkboxInput('rawStart', 'Start?', value = FALSE),
    
    ),
    #Create drop down list
    navbarMenu("Financial Tables",
        #First tab bar
        tabPanel("Balance Sheet",
             mainPanel(
               h1("Balance Sheet Table"),
               tableOutput("outputBalance")
             ),
        ),
        #Second tab bar
        tabPanel("Cash Flow",
             h1("Main")
        ),
        #Third tab bar
        tabPanel("Income Statement",
             h1("Main")
        ),
        # Show user what is inputted
        
    ),
    navbarMenu("Graph",
               #First tab bar
               tabPanel("Balance Sheet",
                        mainPanel(
                          h1("Balance Sheet Table"),
                          
                        ),
               ),
               #Second tab bar
               tabPanel("Cash Flow",
                        h1("Main")
               ),
               #Third tab bar
               tabPanel("Income Statement",
                        h1("Main")
               ),
               # Show user what is inputted
               
    )
  )
))
