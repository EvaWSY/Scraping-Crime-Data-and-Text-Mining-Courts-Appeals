#crime_library.R
library(httr)
library(doParallel)
library(foreach)
library(tidyverse)
library(lubridate)
library(ROCR)
library(xml2)
library(rvest)
library(jsonlite)
library(stringr)

## FUNCTIONS TO GET THE XML PARSED HTML FROM SITE
get_site_content <- function( url ){
  require( httr )
  # get the site response
  response <- httr::GET( url )
  # extract the content
  content <- httr::content( x = response, as = 'text', encoding = 'utf-8' )
  # return 
  return( content )
}

content_to_parsed_html <- function( content ){
  require( xml2 )
  # parse the html with xml2
  parsed_html <- xml2::read_html( content )
  # return
  return( parsed_html )
}

## CRIME NAMES 
extract_all_crime_names <- function( parsed_html ){
  require( rvest )
  # get all table cells of the appropriate class 
  crime_name_els <- rvest::html_nodes( x = parsed_html, xpath = '//td[contains(@class,"field-name")]' )
  # extract the text from the cell
  crime_names <- crime_name_els %>% rvest::html_text() %>% str_trim() 
  # return
  return( crime_names )
}

## HOURS
extract_all_hours <- function( parsed_html ){
  require( rvest )
  # get all table cells of the appropriate class 
  crime_hours_els <- parsed_html %>% rvest::html_nodes(xpath = '//td[contains(@class,"field-crime-date")]' )
  # extract the text from the cell
  crime_hours <- crime_hours_els %>% rvest::html_text() %>% 
    str_split("-") %>% 
    sapply("[[",2) %>% 
    str_trim() %>% 
    parse_date_time(orders= c("%I:%M %p"),exact = TRUE) %>% 
    hour()
  # return
  return( crime_hours )
}

## LINKS TO HOMICIDE ARTICLES
extract_all_hrefs <- function( parsed_html ){
  require( rvest )
  ## Let's write this together
  
  ## step 1: get the appropriate hyperlink elements (subset with just the element selected)
  # a_els <- ?? (hint: html_nodes( x = ??, xpath = ?? ) )
  a_els <-rvest::html_nodes(x=parsed_html,
                            xpath = '//td[contains(@class,"field-title")]/a')
  
  ## step 2: extract the href attribute value
  hrefs <- rvest::html_attr(x = a_els, name = 'href')
  
  ## step 3: examine and, if needed make absolute
  print( hrefs[1:5] )
  
  #return( ?? )
  hrefs<-paste('https://www.universalhub.com/crime/murder/2018',hrefs, sep='')
  return(hrefs)
}



