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
    nhc = input$concern_yn == 1
    lost = if(nhc) input$pc[1] else 100 - input$pc[2]
    label = if(nhc) "P(HD < y)" else "P(HD > y)"
    updateSliderInput(
      inputId = "hd_pr",
      max = lost - 1,
      label = label)
  })

  observe({
    nhc = input$concern_yn == 1
    updateSliderInput(
      inputId="he_pr", 
      label=if(nhc) "P(HE > x)" else "P(HE < x)")
  })

  observe({
    if(input$method == 1) {
      shinyjs::hide("he_pr")
      shinyjs::show("he")
    }else{
      shinyjs::hide("he")
      shinyjs::show("he_pr")
    }
  })
  
  observe({
    nhc = input$concern_yn == 1
    lost = if(nhc) input$pc[1] else 100 - input$pc[2]
    if(input$method == 1) {
      he_pr = lost - input$hd_pr
      est = if(nhc) "conservative" else "liberal"
      sgn = if(nhc) ">" else "<"
      output$he_text <- renderText({ sprintf(paste(
        "Elicit a %s estimate of a High Exposure as the quantile",
        " <i>x</i> such that <i>P</i>(HE %s <i>x</i>) = %g%%.<p>"),
        est, sgn, he_pr)})
    } else {
      updateSliderInput(inputId="he", value=input$hd)
      sgn = if(nhc) "above" else "below"
      output$he_text <- renderText({ sprintf(paste(
        "Ask the experts to judge their probability that a High Exposure is",
        " %s <i>x</i> = %g μg/kg bw per day.<p>"),
        sgn, input$hd)})
    }
  })
  
  output$t2results <- renderUI({
    nhc = input$concern_yn == 1

    list(
      if(input$method == 1) {
        ratio = input$hd / input$he
        reached = nhc == (ratio > 1)
        withMathJax(paste(
          sprintf("Practical certainty is obtained if $$\\frac{x}{y} %s 1$$",
                  {if(nhc) ">" else "<"}),
          sprintf("With these numbers, the ratio is $$\\frac{%g}{%g} = %.2f$$",
                  input$hd, input$he, ratio)
        ))
      } else {
        lhs = 100 - min(100, input$hd_pr + input$he_pr)
        notlost = if(nhc) 100 - input$pc[1] else input$pc[2]
        reached = lhs >= notlost
        withMathJax(paste(
          "Practical certainty is obtained if ",
          sprintf("$$100 - \\min(100, P(HE %s x) + P(HD %s y)) \\geq %g$$",
                  {if(nhc) ">" else "<"}, {if(nhc) "<" else ">"}, notlost),
          "With these numbers, the left hand side is ",
          sprintf("$$100 - \\min(100, %g + %g) = %g$$",
                  input$hd_pr, input$he_pr, lhs)
        ))},
      
      {if(reached) sprintf(
        "Practical certainty is reached. The compound %s a health concern.",
        {if(nhc) "IS NOT" else "IS"}
      ) else sprintf(paste(
        "Practical certainty is not reached with the probability bounds",
        " analysis using probabilities first, so proceed with a refined",
        " approach to determine if the compound %s a health concern."),
        {if(nhc) "is not" else "is"})}
    )
  })
}

