# yel.p-value
A shiny app that tells you if the difference in average star rating between two businesses on Yelp is statistically significant. Just for funsies/shiny practice.

For personal use you need to have a regular Yelp account and create an app on Yelp's Developers site: https://www.yelp.com/developers/documentation/v3/authentication

Then in R:
> Sys.setenv(YELP_ID = "[Your Client ID, probably don't need]",
YELP_SECRET = "[Your API Key]")
