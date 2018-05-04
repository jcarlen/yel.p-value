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
ui <- fluidPage(theme = shinytheme("yeti"),
  
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
      
        sliderInput("bayes", "How much prior?", min = 0, max = 100, value = 0)
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
# Define server logic ####
server <- function(input, output) {
   
   yelp.1 <- reactive({
     yelp_query(input$res1, input$loc1)
   })
   
   yelp.2 <- reactive({
     yelp_query(input$res2, input$loc2)
   })

   result <- reactive({
     yelp_sample(yelp.1()$stars, yelp.2()$stars, input$bayes)
   })
   
   output$res1 <- renderText(paste0("I think Restaurant 1 is: ", yelp.1()$name, ", ", yelp.1()$location))
   output$res2 <- renderText(paste0("I think Restaurant 2 is: ", yelp.2()$name, ", ", yelp.2()$location))
   output$diff <- renderText(paste0("Estimated star difference: ", 
                                    round(mean(result()$mean[1]) - mean(result()$mean[2]), 2), " (= ",
                                    round(mean(result()$mean[1]), 2), " - ", round(mean(result()$mean[1]), 2),")",
                                    ",\nbased on ", yelp.1()$review_count,
                                    " and ", yelp.2()$review_count," reviews, respectively."))
   
   output$distPlot <- renderPlot({
     if (sign(result()$q[1])==sign(result()$q[3])) {
      #print(result()$q)
      #data 1 - 2 
      if (sign(result()$q[1]) > 0) {caption = paste0(yelp.1()$name, " is significantly better. Go there.")}
      if (sign(result()$q[1]) < 0) {caption = paste0(yelp.2()$name, " is significantly better. Go there.")}
     } else {
       caption = "Meh. Go with your heart."
    }
    ggplot(data = result()$means, aes(x = mean_reviews, group = restaurant, fill = restaurant)) + 
       scale_fill_manual(labels = c(yelp.1()$name, yelp.2()$name), values = c("darkblue", "lightblue")) + 
       geom_histogram(position = "identity", bins = 100) + 
       ggtitle(caption) +
       xlim(min(3, min(result()$means$mean_reviews)),5) + 
       ylab("simulation count") +
       xlab("simulated distribution of restaurant's average review") +
       theme(plot.title = element_text(size=22))
   })
}

# Run the application  ####
shinyApp(ui = ui, server = server)

# Helper Functions  ####

yelp_sample <- function(restaurant1, restaurant2, prior_strength = 0, n = 5000) { #input is a lenth-5 vector
  prior = c(.16, .7, .1, .2, .47) #https://www.yelp.com/factsheet
  total_reviews = sum(restaurant1, restaurant2)
  review_samples1 = rmultinom(n, sum(restaurant1)+prior_strength*total_reviews,
                              restaurant1 + prior*prior_strength*total_reviews)
  mean_reviews1 = apply(review_samples1, 2, weighted.mean, x = 1:5)
  review_samples2 = rmultinom(n, sum(restaurant2)+prior_strength*total_reviews,
                              restaurant2 + prior*prior_strength*total_reviews)
  mean_reviews2 = apply(review_samples2, 2, weighted.mean, x = 1:5)
  q = quantile(mean_reviews1 - mean_reviews2, probs = c(0.025, 0.5, .975))
  return(list(q = q, mean = c(mean(mean_reviews1), mean(mean_reviews2)),
              means = data.frame( restaurant = as.factor(c(rep(1, n), rep(2, n))), 
                                         mean_reviews = c(mean_reviews1, mean_reviews2))
              ))
}
  
yelp_query <- function(keywords, location) {
  
  url <- modify_url("https://api.yelp.com", path = c("v3", "businesses", "search"),
               query = list(term = keywords, location = location, limit = 1))
  res <- GET(url, add_headers('Authorization' = paste("bearer", Sys.getenv("YELP_SECRET")))) #see yelp_personal.R
  ct <- content(res)
  
  url2 = str_extract(ct$businesses[[1]]$url, ".+(?=\\?)")
  name = ct$businesses[[1]]$name
  review_count = ct$businesses[[1]]$review_count
  
  tmp = html_nodes(read_html(url2), "table") %>% .[2] %>% html_table(fill = TRUE) #use label instead of ".[2]"?

  res.stars = rev(tmp[[1]]$X4[!is.na(tmp[[1]]$X4)])

  if (sum(res.stars) != review_count) {
    print(paste0("Something's wrong? Total reviews is different than review count by", 
                 abs(sum(res.stars) - review_count)))
  }
  return(list(stars = res.stars, name = name,
              review_count = review_count,
              location = ct$businesses[[1]]$location$display_address[[1]],
              review_mean = weighted.mean(1:5, w = res.stars))
         )
}

