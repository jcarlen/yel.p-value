# Define server logic ####
function(input, output) {
   
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
                                    round(mean(result()$mean[1]), 2), " - ", round(mean(result()$mean[2]), 2),")",
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