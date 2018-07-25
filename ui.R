# Determine if the average star rating of two different restaurants (or any business on yelp) 
# is statistically signficiant
#
# Built with help from
# https://github.com/jennybc/yelpr
# http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
#
# To do:
#  Add genre-specific priors
#  Make pretty

# setup ####
library(shiny)
library(tidyverse)
library(rvest)
library(xml2)
library(httr) #need this?
library(ggplot2)
library(shinythemes)

# Define UI ####
fluidPage(theme = shinytheme("yeti"),
  
   titlePanel("yel.p-value"),
   
   helpText("Is one restaurant *significantly* better than another?"),
   
   sidebarLayout(
      sidebarPanel(
         textInput("res1",
                     "Restaurant 1:",
                    value = "Spago"),
         
         textInput("loc1",
                   "Location 1:",
                   value = "los angeles"),
      
        textInput("res2",
                  "Restaurant 2:",
                  value = "California Pizza Kitchen"),
                  
        textInput("loc2",
                  "Location 2:",
                  value = "los angeles"),
      
        sliderInput("bayes", "Prior strength", min = 0, max = 100, value = 0)
      ),
   
    # Show the generated distribution
    mainPanel(
      tags$style(type='text/css', '#res1 {background-color: rgba(255,255,0,0.40); color: black; font: bold 12px/30px Helvetica, sans serif;}'), 
      tags$style(type='text/css', '#res2 {background-color: rgba(0,0,255,0.10); color: black; font: bold 12px/30px Helvetica, sans serif;}'), 
      tags$style(type='text/css', '#loc1 {background-color: rgba(255,255,0,0.40); color: black; font: bold 12px/30px Helvetica, sans serif;}'), 
      tags$style(type='text/css', '#loc2 {background-color: rgba(0,0,255,0.10); color: black; font: bold 12px/30px Helvetica, sans serif;}'), 
      tags$style(type='text/css', '#diff {background-color: rgba(225,0,50,0.10); color: black; font: bold 12px/30px Helvetica, sans serif;}'), 
      
      verbatimTextOutput("res1", placeholder = TRUE),
      verbatimTextOutput("res2", placeholder = TRUE),
      verbatimTextOutput("diff", placeholder = TRUE),
      #verbatimTextOutput("result")
      plotOutput(outputId = "distPlot")
    )
  )
)
