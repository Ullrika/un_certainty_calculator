library(sn)
library(SHELF)

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

  ## Tier 3
  
  rv <- reactiveValues(
    hd = data.frame("Prob" = c(5, 50, 95), "Value" = c(70, 106, 130)),
    he = data.frame("Prob" = c(5, 50, 95), "Value" = c(20, 50, 100))
  )
  
  fitsorted <- function(data){
    d <- data[order(data$Value),]
    SHELF::fitdist(
      vals = unlist(d["Value"]),
      probs = unlist(d["Prob"] / 100),
      lower = 0)}
    
  observe(rv$fit_hd <- fitsorted(rv$hd))
  observe(rv$fit_he <- fitsorted(rv$he))

  qxx <- function(fit, dist, probs){
    if(dist == "skewnormal"){
      snd = fit$Skewnormal
      qsn(probs / 100, snd$location, snd$scale, snd$slant)
    } else if (dist == "lognormal"){
      lnd = fit$Log.normal
      qlnorm(probs / 100, lnd$mean.log.X, lnd$sd.log.X)
    }
  }

  qhd <- reactive({
    qxx(rv$fit_hd, input$hd_dist, rv$hd[,"Prob"])
  })

  observeEvent(input$hd_points, {
    pts = as.integer(input$hd_points)
    probs <- rv$hd[,"Prob"]
    probs <- seq(probs[1], probs[length(probs)], len=pts)
    vals = qxx(rv$fit_hd, input$hd_dist, probs)
    rv$hd <- data.frame("Prob" = probs, "Value" = round(vals, 1))
  })

  observeEvent(input$he_points, {
    pts = as.integer(input$he_points)
    probs <- rv$he[,"Prob"]
    probs <- seq(probs[1], probs[length(probs)], len=pts)
    vals = qxx(rv$fit_he, input$he_dist, probs)
    rv$he <- data.frame("Prob" = probs, "Value" = round(vals, 1))
  })
  
  output$hd_table <- renderDT(
    rv$hd,
    editable=TRUE, class="compact", rownames=FALSE,
    options = list(paging = FALSE, searching = FALSE, info = FALSE))

  output$he_table <- renderDT(
    rv$he,
    editable=TRUE, class="compact", rownames=FALSE,
    options = list(paging = FALSE, searching = FALSE, info = FALSE))
  
  output$t3plot <- renderPlot({
    SHELF::plotfit(
      rv$fit_hd, d=input$hd_dist, xlab="HD", ylab="Probability density",
      returnPlot=TRUE, showPlot=FALSE)

    # cols <- paletteer_d("nbapalettes::thunder")
    # names(cols) = c("HE","HE*fHE" ,"HD","HD*fHD")
    # 
    # 
    # pp1 <- ggplot(data.frame(dose = c(hd,he), quantity = rep(c("HD", "HE"),each = niter)),aes(x=dose, fill = quantity)) +
    #   geom_density(alpha=0.2) +
    #   #scale_fill_paletteer_d("nbapalettes::thunder") +
    #   scale_fill_manual(values = cols) +
    #   scale_x_continuous(trans='log10') +
    #   ggtitle("Uncertainty in High Exposure \n and the Human Dose")
    
        })
    
    
}

