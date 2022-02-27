#Author: Benjamin Vong
#title: SEC Dashboard
#date: 
library(shiny); library(dplyr); library(edgar); library(request); library(rvest); library(stringr); library(httr)


# Define server logic
shinyServer(function(input, output) {
  #Function will test user link with SEC
  get_web = function(link) {
    #remove excess char in string and save into CIK
    CIK = substring(link, first = 39)
    #isolate CIK from rest of string
    CIK = gsub("&owner=exclude", "", CIK)
    #edits link to function in edgar search
    link = gsub("https://www.sec.gov/", "https://www.sec.gov/cgi-bin/", link)
    link = gsub("edgar/browse/", "browse-edgar", link)
    #counts the numbers of char in CIK
    countCIK = str_count(CIK)
    fullCIK = CIK
    #format CIK into 10 digit to be used in SEC website
    while (countCIK <= 9) {
      fullCIK = paste0("0", fullCIK)
      countCIK = countCIK + 1
    }
    #combines the parts of the link together
    link = paste("https://www.sec.gov/cgi-bin/browse-edgar?CIK=", fullCIK, "&owner=exclude", sep = "")
    secURL <- link
    #sets the user agent to web scrap
    agentURL <- GET(secURL, add_headers('user-agent' = 'blv902@uw.edu'))
    #read the the html
    page <- read_html(agentURL)
    #parse the html for the exact Name and SIC
    companyName = page %>% html_nodes(".companyName ") %>% html_text()
    companySIC = page %>% html_nodes(".identInfo acronym+ a") %>% html_text()
    #bundle the variables into an df
    companyDetail = data.frame(companyName, companySIC, CIK, stringsAsFactors = FALSE)
    #return name, SIC, and CIK
    return(companyDetail)
  }
  #convert string userinput into intergers
  get_Years = function() {
    #counts the commas in the userinput and adds one
    yearsCount = str_count(input$rawYears, ",") + 1
    arrayCount = 0
    #removes the commas
    years = gsub(","," ",input$rawYears)
    #makes an array based on amount of years
    arrayYear = array(data = NA, dim = c(1, yearsCount))
    #loop to add and covert years into the array
    while(arrayCount <= yearsCount) {
      arrayYear[arrayCount] = strtoi(word(years,arrayCount))
      arrayCount = arrayCount + 1
    }
    #returns an array of years
    return(arrayYear)
  }
  #uses converted userinput to get SEC filings
  get_Form = function() {
    #counts userinputted years
    yearsCount = str_count(input$rawYears, ",") + 1
    arrayCount = 0
    #gets CIK from get_web array num 3
    CIK = get_web(input$rawLink)[3]
    #call get_years to get array year
    year = get_Years()
    #runs through the year array to get each year filing
    while (arrayCount <= yearsCount) {
      getFilings(CIK, '10-K', year[arrayCount], downl.permit = "n", useragent = 'blv902@uw.edu')
      arrayCount = arrayCount + 1
    }
  }
  #will update when user changes SEC link
  observeEvent(input$rawLink, {
    get_web(input$rawLink)
  })
  #will update when user changes years
  observeEvent(input$rawYears, {
    get_Years()
  })
  #will update when user start
  observeEvent(input$rawStart, {
    #signal to get form when checked
    if (input$rawStart == TRUE) {
      get_Form()
    }
  })
  #display the company name and CIK so that user can double check
  output$outputLink <- renderText({
    companyName = get_web(input$rawLink)[1]
    paste(companyName)
    
  })
  
  output$outputBalance <- renderTable({
    
  })

})
