library(sn)
library(SHELF)
library(ggplot2)

function(input, output) {

  # Units
  observe(if(input$hdRangeMin >= input$hdRangeMax) {
    showNotification("HD lower limit must be < upper limit", type="error")
  }else{
    updateSliderInput(inputId="hd", min=input$hdRangeMin,
                      max=input$hdRangeMax)
  })
  observe(if(input$heRangeMin >= input$heRangeMax) {
    showNotification("HE lower limit must be < upper limit", type="error")
  }else{
    updateSliderInput(inputId="he", min=input$heRangeMin,
                      max=input$heRangeMax)
  })
  
  
  # Tier 1

  output$t1text <- renderUI(list(
    withMathJax(paste(
    "Ask the experts to agree how certain they are as a group that",
    "\\(\\frac{HE}{HD} < 1\\)",
    "considering all evidence and all identified (non-standard or ",
    "standard) sources of uncertainty.")),
    HTML(paste("<p style='margin-top: 1em'>If the experts are at least",
    sprintf("%d%% certain of this,", 100 - input$pc[1]),
    "we have obtained practical certainty that the compound is not a",
    "health concern."))
  ))
  
  # Tier 2
  
  output$pctable <- renderTable({
    fr<-data.frame(
      a = c(sprintf("%g", input$pc[1]), sprintf("%g", 100-input$pc[1])),
      b = c(sprintf("%g – %g", input$pc[1], input$pc[2]),
            sprintf("%g – %g", 100-input$pc[2], 100-input$pc[1])),
      c = c(sprintf("%g", input$pc[2]), sprintf("%g", 100-input$pc[2])))
    colnames(fr) <- c("No health concern", "Uncertainty", "Health concern")
    fr},
    spacing="m"
  )
  
  observe({
    nhc = input$concern_yn == 1
    lost = if(nhc) input$pc[1] else 100 - input$pc[2]
    label = sprintf("P(HD %s %g)", (if(nhc) "<" else ">"), input$hd)
    # label = if(nhc) "P(HD < y)" else "P(HD > y)"
    updateSliderInput(inputId = "hd_pr", max = lost - 1, label = label)
  })

  observe({
    nhc = input$concern_yn == 1
    label = sprintf("P(HE %s %g)", (if(nhc) ">" else "<"), input$hd)
    updateSliderInput(inputId="he_pr", label = label)
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
    est = if(nhc) "conservative" else "best-case"
    hdsgn = if(nhc) "<" else ">"
    output$hd_text <- renderText({ sprintf(paste(
      "Elicit a %s estimate of a Humans Dose as a quantile",
      "<i>y</i> and the associated confidence ",
      "<span style='white-space:nowrap'><i>P</i>(HD %s <i>y</i>).</span><p>"),
      est, hdsgn)})
    if(input$method == 1) {
      he_pr = lost - input$hd_pr
      hesgn = if(nhc) ">" else "<"
      output$he_text <- renderText({ sprintf(paste(
        "Elicit a %s estimate of a High Exposure as the quantile",
        "<i>x</i> such that <span style='white-space:nowrap'>",
        "<i>P</i>(HE %s <i>x</i>) = %g%%.</span><p>"),
        est, hesgn, he_pr)})
    } else {
      updateSliderInput(inputId="he", value=input$hd)
      sgn = if(nhc) "above" else "below"
      output$he_text <- renderText({ sprintf(paste(
        "Ask the experts to judge their probability that a High Exposure is",
        "%s <i>x</i> = %g %s per day.<p>"),
        sgn, input$hd, input$units)})
    }
  })
  
  output$t2results <- renderUI({
    nhc = input$concern_yn == 1

    list(
      if(input$method == 1) {
        ratio = input$hd / input$he
        reached = nhc == (ratio > 1)
        lhs = if(nhc) 100 - input$pc[1] else input$pc[2]
        withMathJax(HTML(paste(
          sprintf(
            "Practical certainty is obtained if \\(\\frac{y}{x} %s 1\\).",
            {if(nhc) ">" else "<"}),
          sprintf(
            "<br>With these numbers, the ratio is \\(\\frac{%g}{%g} = %.2f\\).",
            input$hd, input$he, ratio)
        )))
      } else {
        lhs = 100 - min(100, input$hd_pr + input$he_pr)
        notlost = if(nhc) 100 - input$pc[1] else input$pc[2]
        reached = lhs >= notlost
        withMathJax(HTML(paste(
          "Practical certainty is obtained if ",
          sprintf("\\(100 - \\min(100, P(HD %s y) + P(HE %s x)) \\geq %g\\).",
                  {if(nhc) "<" else ">"}, {if(nhc) ">" else "<"}, notlost),
          "<br>With these numbers, the left hand side is ",
          sprintf("\\(100 - \\min(100, %g + %g) = %g\\).",
                  input$hd_pr, input$he_pr, lhs)
        )))},
      
      HTML("<p style='margin-top: 1em'>"),

      if(reached) sprintf(paste(
        "Practical certainty is reached. The experts are at least %d%%",
        "certain that the compound %s a health concern."), lhs,
        {if(nhc) "is not" else "is"}
      ) else sprintf(paste(
        "Practical certainty is not reached. The assessment is inconclusive.",
        "Proceed with a refined approach to determine if the compound %s ",
        "a health concern."),
        {if(nhc) "is not" else "is"})
    )
  })

  ## Tier 3
  MAXPTS = 7
  
  h_shown_f <- function(h) {
    n <- as.numeric(input[[paste0(h, "_points")]])
    c(1, diff(seq((MAXPTS-2)%/%2, by=n-2, len=MAXPTS-1) %/% (MAXPTS-2)), 1)
  }

  rv <- reactiveValues()
  # h_shown <- reactiveValues()
  h_shown <- list()
  
  lapply(
    c("hd", "he"), function(h) {
      h_shown[[h]] <- reactive(h_shown_f(h))

      observe({
        sh <- h_shown[[h]]()
        lapply(which(sh == 1),
               function(x) { shinyjs::show(sprintf("%s_l%d", h, x)) })
        lapply(which(sh == 0),
               function(x) { shinyjs::hide(sprintf("%s_l%d", h, x)) })
      })
      
      observe({
        hmin <- input[[sprintf("%sRangeMin", h)]]
        hmax <- input[[sprintf("%sRangeMin", h)]]
        lapply(1:MAXPTS, function(x) { updateNumericInput(
          inputId=sprintf("%s_%d", h, x), min=hmin, max=hmax)})
      })
      
      observe({
        showns <- which(h_shown[[h]]() == 1)
        probs = sapply(showns,
                       function(x) { input[[sprintf("%s_p%d", h, x)]] })
        vals = sapply(showns,
                      function(x) { input[[sprintf("%s_%d", h, x)]] })
        rv[[h]] <- data.frame("Prob" = probs, "Value" = vals)
      })
      
    })
  

  # observe({
  #   for(h in c("hd", "he")) {
  #     rv[[h]] <- data.frame("Prob" = )
  # }})

  # rv <- reactiveValues(
  #   hd = data.frame("Prob" = c(5, 50, 95), "Value" = c(70, 106, 130)),
  #   he = data.frame("Prob" = c(5, 50, 95), "Value" = c(20, 50, 100))
  # )
  
  # # Render the table and set up editing, for both hd and he
  # lapply(list("hd", "he"), function(h) {
  #   output[[paste0(h, "_table")]] <- renderDT(rv[[paste0(h,"")]],
  #     editable="row", class="compact", rownames=FALSE, selection = 'none',
  #     options = list(paging = FALSE, searching = FALSE, info = FALSE))
  # 
  #     table = paste0(h, "_table")
  #     cell = paste0(table, "_cell_edit")
  #     observeEvent(input[[cell]], {
  #       rv[[h]] <<- editData(rv[[h]], input[[cell]], table, rownames = FALSE)
  #     })
  # })

  fitsorted <- function(data){
    d <- data[order(data$Value),]

    if(is.unsorted(d$Prob, strictly=TRUE) ||
       is.unsorted(d$Value, strictly=TRUE)) {
      stop("Values and probabilities must be sorted the same way")
    }
    SHELF::fitdist(
      vals = unlist(d$Value),
      probs = unlist(d$Prob / 100),
      lower = 0)
  }

  lapply(c("hd", "he"), function(h) {
    observe(tryCatch(
      { rv[[sprintf("fit_%s", h)]] <- fitsorted(rv[[h]]) },
      error=function(e) { showNotification(e$message, type="error") }))
  })
  
  # observe(tryCatch(
  #   { rv$fit_he <- fitsorted(rv$he) },
  #   error=function(e) { showNotification(e$message, type="error") }))

  # Get numbers from the SHELF fit with the given probability distribution
  # and probabilities/quantiles
  probfunc <- function(what, fit, dist, xqp){
    if(dist == "skewnormal"){
      snd = fit$Skewnormal
      if(is.na(snd$location))
        stop("Skew-normal distribution was not fitted")
      func = paste0(what, "sn")
      do.call(func, list(xqp, snd$location, snd$scale, snd$slant))
    } else if (dist == "lognormal"){
      lnd = fit$Log.normal
      if(is.na(lnd$mean.log.X))
        stop("Log-normal distribution was not fitted")
      func = paste0(what, "lnorm")
      do.call(func, list(xqp, lnd$mean.log.X, lnd$sd.log.X))
    } else if (dist == "normal"){
      nd = fit$Normal
      if(is.na(nd$mean))
        stop("Normal distribution was not fitted")
      func = paste0(what, "norm")
      do.call(func, list(xqp, nd$mean, nd$sd))
    } else {
      stop("unknown distribution")
    }
  }

  # for(h in c("hd", "he")) {
  #   for(what in c("q", "p", "d")) {
  #     assign(paste0(what, h),
  #            function(arg){ 
  #              print(sprintf("pf %s %s", h, what))
  #              probfunc(
  #              what, rv[[paste0("fit_", h)]],
  #              input[[paste0(h, "_dist")]], arg)})
  #   }
  # }

  qhd <- function(probs){
    probfunc("q", rv$fit_hd, input$hd_dist, probs)
  }
  qhe <- function(probs){
    probfunc("q", rv$fit_he, input$he_dist, probs)
  }
  dhd <- function(x){
    probfunc("d", rv$fit_hd, input$hd_dist, x)
  }
  dhe <- function(x){
    probfunc("d", rv$fit_he, input$he_dist, x)
  }
  phd <- function(q){
    probfunc("p", rv$fit_hd, input$hd_dist, q)
  }
  phe <- function(q){
    probfunc("p", rv$fit_he, input$he_dist, q)
  }

  # # Resize list of probabilities by interpolation
  # observeEvent(input$hd_points, {
  #   pts = as.integer(input$hd_points)
  #   probs <- rv$hd[,"Prob"]
  #   if(pts != length(probs)){
  #     probs <- seq(probs[1], probs[length(probs)], len=pts)
  #     rv$hd <- data.frame("Prob" = probs, "Value" = round(qhd(probs / 100), 1))
  #   }
  # })
  # 
  # observeEvent(input$he_points, {
  #   pts = as.integer(input$he_points)
  #   probs <- rv$he[,"Prob"]
  #   if(pts != length(probs)){
  #     probs <- seq(probs[1], probs[length(probs)], len=pts)
  #     rv$he <- data.frame("Prob" = probs, "Value" = round(qhe(probs / 100), 1))
  #   }
  # })

  t3compute = function(n = 10000, pskip = 1e-5, reverse = FALSE) {
    minq = min(qhd(pskip), qhe(pskip))
    maxq = max(qhd(1 - pskip), qhe(1 - pskip))
    x = seq(minq, maxq, len=n)
    step = (maxq - minq) / n
    if(reverse) {
      overlap1 = dhd(x) * phe(x)
      overlap2 = dhe(x) * (1 - phd(x))
    } else {
      overlap1 = dhd(x) * (1 - phe(x))
      overlap2 = dhe(x) * phd(x)
    }
    (sum(overlap1) + sum(overlap2)) * .5 * step
  }

  output$t3text <- renderUI({
    prob <- t3compute() * 100
    reached = 
    if(prob <= input$pc[1] || prob >= input$pc[2]) {
      nhc = (prob <= input$pc[1])
      sprintf(paste(
        "Practical certainty is reached. The experts are at least %.2g%%",
        "certain that the compound %s a health concern."),
        {if(nhc) 100 - prob else prob},
        {if(nhc) "is not" else "is"} )
    } else {
      sprintf(paste(
        "Practical certainty is not reached. The assessment is inconclusive.",
        "Proceed with a refined approach to determine if the compound is",
        "a health concern."))
    }
  })
  
  output$t3plot <- renderPlot({
    pskip = 1e-4
    minq = min(qhd(pskip), qhe(pskip))
    maxq = max(qhd(1 - pskip), qhe(1 - pskip))
    x = seq(minq, maxq, len=100)

    overlap1 = dhd(x) * (1 - phe(x))
    overlap2 = dhe(x) * phd(x)
    pldata = data.frame(val=rep(x, 2), density=c(dhd(x), dhe(x)),
                        name=rep(c("HD", "HE"), each=length(x)))
    ggplot(pldata) + 
      geom_area(aes(val, density, fill=name), alpha=0.5,
                outline.type="upper") +
     ggtitle("Uncertainty in High Exposure \n and the Human Dose")

    # geom_area(aes(val, density, fill=name), alpha=0.2, fill="red", color="red",
    #           outline.type="upper") +
    #   geom_area(aes(val, dhe), alpha=0.2, fill="blue", color="blue",
    #             outline.type="upper") +
    #   # 
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

