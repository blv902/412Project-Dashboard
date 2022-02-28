#Author: Benjamin Vong
#title: SEC Dashboard
#date: 
library(shiny); library(dplyr); library(edgar); library(request); library(rvest); library(stringr); library(httr) ; library(readxl) ; library(readr) 
wd <- getwd()
setwd("~/")
print(getwd())
setwd(paste(wd,"/","Directory", sep = ""))
print(getwd())

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
  get_intYears = function() {
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
  #convert string userinput into intergers
  get_stringYears = function() {
    #counts the commas in the userinput and adds one
    yearsCount = str_count(input$rawYears, ",") + 1
    #removes the commas
    years = gsub(","," ",input$rawYears)
    return(years)
  }
  
  get_Table = function(link) {
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
    tableForm = data.frame()
    for (pageNumber in seq(from = 0, to = 720, by = 40)) {
      link = paste("https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", fullCIK, "&type=&dateb=&owner=exclude&start=",pageNumber,"&count=40", sep = "")
      secURL <- link
      agentURL <- GET(secURL, add_headers('user-agent' = 'blv902@uw.edu'))
      page <- read_html(agentURL)
      filingType = page %>% html_nodes("#seriesDiv td:nth-child(1)") %>% html_text()
      filingDescription = page %>% html_nodes("#seriesDiv td:nth-child(3)") %>% html_text()
      
      filingDate = page %>% html_nodes("#seriesDiv td:nth-child(4)") %>% html_text()
      tableForm = rbind(tableForm, data.frame(filingType,filingDescription,filingDate,stringsAsFactors = FALSE))
    }
    
    form <- subset(tableForm, filingType == "10-K")
    form1 <- form %>% mutate(filingDescription = substring(filingDescription, first = 62))
    form2 <- form1 %>% mutate(filingDescription = substring(filingDescription, first = 2, last = 21))
    form2 <- form2 %>% mutate(filingDate = substring(filingDate, first = 1, last = 4))
    
    yearsCount = str_count(input$rawYears, ",") + 1
    arrayCount = 0
    year = get_stringYears()
    df <- data.frame()
    while(arrayCount <= yearsCount) {
      dfTemp <- subset(form2, filingDate == word(year,arrayCount))
      df = rbind(df,data.frame(dfTemp, stringsAsFactors = FALSE))
      arrayCount = arrayCount + 1
    }
    
    accFull <- df %>% mutate(filingDescription = str_replace_all(filingDescription, "-", ""))
    colnames(accFull)[colnames(accFull) == "filingDescription"] <- "accName"
    
    df_new <- cbind(df, accFull$accName)
    colnames(df_new)[colnames(df_new) == "accFull$accName"] <- "accName"
    
    documentURL <- data.frame()
    arrayCount = 1
    while(arrayCount <= yearsCount) {
      filingLink = paste("https://www.sec.gov/Archives/edgar/data/",CIK,"/",df_new[arrayCount,4],"/",df_new[arrayCount,2],"-index.htm", sep = "")
      fileURL <- filingLink
      agentUrl <- GET(fileURL,add_headers('user-agent' = 'blv902@uw.edu'))
      urlPage <- read_html(agentUrl)
      documentURLtemp = urlPage %>% html_nodes("#formDiv:nth-child(3) tr:nth-child(2) td:nth-child(3)") %>% html_text()
      documentURL = rbind(documentURL, data.frame(documentURLtemp, stringsAsFactors = FALSE))
      arrayCount = arrayCount + 1
    }
    htmlTable <- cbind(df_new, documentURL)
    colnames(htmlTable)[colnames(htmlTable) == "documentURLtemp"] <- "file name"
    
    arrayCount = 1
    while(arrayCount <= yearsCount) {
      htmlTable[arrayCount,5] = gsub("htm.*", "", htmlTable[arrayCount,5])
      htmlTable[arrayCount,5] = paste(htmlTable[arrayCount,5],"htm", sep = "")
      arrayCount = arrayCount + 1
    }
    
    formLink = data.frame()
    arrayCount = 1
    while(arrayCount <= yearsCount) {
      formLinktemp = paste("https://www.sec.gov/Archives/edgar/data/",CIK,"/",htmlTable[arrayCount,4],"/",htmlTable[arrayCount,5],sep = "")
      formLink = rbind(formLink, data.frame(formLinktemp, stringsAsFactors = FALSE))
      arrayCount = arrayCount + 1
    }
    htmlTable <- cbind(htmlTable,formLink)
    colnames(htmlTable)[colnames(htmlTable) == "formLinktemp"] <- "link"
    
    write.csv(htmlTable, "forms")
    return(htmlTable)
  }
  
  sort_Table = function(table) {
    
  }
  
  #will update when user changes SEC link
  observeEvent(input$rawLink, {
    get_web(input$rawLink)
  })
  #will update when user changes years
  observeEvent(input$rawYears, {
    get_intYears()
    get_stringYears()
  })
  #will update when user start
  observeEvent(input$rawStart, {
    #signal to get form when checked
    if (input$rawStart == TRUE) {
      get_Table(input$rawLink)
      
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
