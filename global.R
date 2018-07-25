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

# Helper Functions  ####

yelp_sample <- function(restaurant1, restaurant2, prior_strength = 0, n = 5000) { #input is a lenth-5 vector
  prior = c(.16, .7, .1, .2, .47) #https://www.yelp.com/factsheet #http://i.imgur.com/PqU30Ma.png
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
  res <- GET(url, add_headers('Authorization' = paste("bearer", "EDIQV9ovkJdjQxnMJs1PNQ8qQ--sn2YQRE8kDR5TQKidVBBU7llYQD3VhDKDUBCnyRF0laQb8w6nfU4UuomjEysepdy6mFQEbQg8LLvTlzNKDgw51WmGpbX4jm3hWnYx"))) #Sys.getenv("YELP_SECRET")
                                                                # Currently set to my key. See README.md for how to get and set YELP API key. 
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

