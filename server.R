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
  
  # observe({
  #   updateTextInput(
  #     inputId="he_pr", value=sprintf("%g", input$pc[1] - input$hd_pr[1]))
  # })
  
  observe({
    conc = input$concern_yn
    lost = if(conc == 1) input$pc[1] else 100 - input$pc[2]
    updateSliderInput(inputId="hd_pr",
                      max=lost-1,
                      label=if(conc == 1) "P(HD < y)" else "P(HD > y)")
    if(input$method == 1) {
      he_pr = lost - input$hd_pr
      updateSliderInput(inputId="he_pr", value=he_pr,
                        max=lost-1,
                        label=if(conc == 1) "P(HE > x)" else "P(HE < x)")
    } else {
      updateSliderInput(inputId="he_pr", max=50,
                        label=if(conc == 1) "P(HE > x)" else "P(HE < x)")
      updateSliderInput(inputId="he", value=input$hd)
    }
  })
  
  observe({
    meth = input$method
    shinyjs::disable(if(meth == 1) "he_pr" else "he")
    shinyjs::enable(if(meth == 1) "he" else "he_pr")
  })
  
  output$t2results <- renderUI({
    if(input$method[1] == 1) {
      rat = input$hd[1] / input$he[1]
      conc = input$concern_yn == 1
      isc = if(conc) "is not" else "is"
      sgn = if(conc) ">" else "<"
      reached = if(conc) rat > 1 else rat < 1
      withMathJax(paste(
        sprintf("Practical certainty is obtained if $$\\frac{x}{y} %s 1$$",
                sgn),
        sprintf("With these numbers, the ratio is $$\\frac{%g}{%g} = %.2f$$",
                      input$hd[1], input$he[1], input$hd[1] / input$he[1]),
        if(reached) sprintf(
          "Practical certainty is reached. The compound %s a health concern.",
          toupper(isc)) else sprintf(paste(
            "Practical certainty is not reached with the probability bounds",
            " analysis using probabilities first, so proceed with a refined",
            " approach to determine if the compound %s a health concern."),
          isc)
        ))
    } else {
      
    }
  })
}

