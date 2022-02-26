#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#facebook
# https://www.sec.gov/edgar/browse/?CIK=1326801&owner=exclude
#apple
#https://www.sec.gov/edgar/browse/?CIK=320193&owner=exclude

library(shiny); library(dplyr); library(edgar); library(request); library(rvest); library(stringr); library(httr)
#Example: https://www.sec.gov/Archives/edgar/data/1326801/000132680119000069/fb-09302019x10q.htm

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  get_web = function(link) {
    CIK = substring(link, first = 39)
    CIK = gsub("&owner=exclude", "", CIK)
    link = gsub("https://www.sec.gov/", "https://www.sec.gov/cgi-bin/", link)
    link = gsub("edgar/browse/", "browse-edgar", link)
    countCIK = str_count(CIK)
    fullCIK = CIK
    while (countCIK <= 9) {
      fullCIK = paste0("0", fullCIK)
      countCIK = countCIK + 1
    }
    link = paste("https://www.sec.gov/cgi-bin/browse-edgar?CIK=", fullCIK, "&owner=exclude", sep = "")
    secURL <- link
    agentURL <- GET(secURL, add_headers('user-agent' = 'SEC listing scraper'))
    page <- read_html(agentURL)
    companyName = page %>% html_nodes(".companyName ") %>% html_text()
    companySIC = page %>% html_nodes(".identInfo acronym+ a") %>% html_text()
    return(companyName)
  }
  
  observeEvent(input$rawLink, {
    get_web(input$rawLink)
  })
    
  output$outputLink <- renderText({
    paste(get_web(input$rawLink))
    
    })

})
