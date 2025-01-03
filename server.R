function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })


  output$pctable <- renderTable({
    fr<-data.frame(
      a = sprintf("%g", input$pc[1]),
      b = sprintf("%g-%g", input$pc[1], input$pc[2]),
      c = sprintf("%g", input$pc[2]))
    colnames(fr) <- c("No health concern",
                      "Uncertainty",
                      "Health concern")
    fr},
    spacing="m"
  )
  
  # output$he_pr <- renderText({
  #   sprintf("%g", input$pc[1] - input$hd_pr[1])
  # })

  observe({
    updateTextInput(inputId="he_pr", value=sprintf("%g", input$pc[1] - input$hd_pr[1]))
  })
  
  output$t2results <- renderUI({
    withMathJax(
      sprintf("In this example, this ratio is $$\\frac{%g}{%g} = %.2f$$",
            input$hd[1], input$he[1], input$hd[1] / input$he[1])
    )
  })
}

