# yel.p-value
A shiny app that tells you if the difference in average star rating between two businesses on Yelp is statistically significant.

For finished product see: https://jcarlen.shinyapps.io/yel_p_value/ -- Might run out of queries/day. To prevent that you can set it up for personal use. 

For personal use you need to have a regular Yelp account and create an app on Yelp's Developers site: https://www.yelp.com/developers/documentation/v3/authentication

Then in R change the line where `res` is set in the yelp_query function in global.R to use your key.

To Do:

- Add explanation of prior
- Add category-specific prior distributions of star ratings
- Make it pretty
