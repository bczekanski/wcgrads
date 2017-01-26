#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#' @import dplyr

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$value <- renderPrint({ input$select })
  output$distPlot <- renderPlot({

   if(input$select == 1){

    a <- allyrs %>%
      filter(grad.year == input$year) %>%
      select(latin.honors, gender) %>%
      group_by(latin.honors, gender) %>%
      summarize(count = n())

    z <- a %>%
      ggplot(aes(latin.honors, count, fill = gender)) +
      geom_bar(stat = "identity", position = "dodge")
      # ggtitle("Latin Honors") +
      # xlab() +
      # ylab() +
      # guides(fill = guide_legend(title = "")) +
      # labs(caption = "")
    z
   }
    else{
      a <- allyrs %>%
      filter(grad.year <= input$year) %>%
      select(latin.honors, gender) %>%
      group_by(latin.honors, gender) %>%
      summarize(count = n())

    z <- a %>%
      ggplot(aes(latin.honors, count, fill = gender)) +
      geom_bar(stat = "identity", position = "dodge")
    # ggtitle("Latin Honors") +
    # xlab() +
    # ylab() +
    # guides(fill = guide_legend(title = "")) +
    # labs(caption = "")
    z}
  })

})
