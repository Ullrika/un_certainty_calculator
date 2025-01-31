library(sn)
library(SHELF)
library(ggplot2)


function(input, output) {

  # Practical certainty & Units

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

  output$pctext <- renderUI({ helpText(paste(
    "We accept that there is no health concern if that claim is made",
    "with at least", sprintf("%g%%", 100 - input$pc[1]), "certainty,",
    "or that there is a health concern if claimed with at least",
    sprintf("%g%%", input$pc[2]), "certainty."
  ))})
    
  observe(if(input$hd_min >= input$hd_max) {
    showNotification("HC lower limit must be < upper limit", type="error")
  }else{
    updateSliderInput(
      inputId="hd", min=input$hd_min, value=input$hd_min, max=input$hd_max)
  })
  observe(if(input$he_min >= input$he_max) {
    showNotification("HE lower limit must be < upper limit", type="error")
  }else{
    updateSliderInput(
      inputId="he", min=input$he_min, value=input$he_max, max=input$he_max)
  })
  
  unitsexport <- function(file) {
    data <- list(
      "Practical certainty (lo)" = input$pc[1],
      "Practical certainty (hi)" = input$pc[2],
      "Units" = input$units,
      "HC min" = input$hd_min,
      "HC max" = input$hd_max,
      "HE min" = input$he_min,
      "HE max" = input$he_max)
    write.csv(data.frame("Parameter" = names(data), "Value" = unlist(data)),
              file=file, row.names=FALSE)
  }
  
  # Tier 1

  output$t1text <- renderUI(list(
    withMathJax(paste(
    "Ask the experts to agree how certain they are as a group that",
    "\\(\\frac{HE}{HC} < 1\\)",
    "considering all evidence and all identified (non-standard or ",
    "standard) sources of uncertainty.")),
    HTML(paste("<p style='margin-top: 1em'>If the experts are at least",
    sprintf("%d%% certain of this,", 100 - input$pc[1]),
    "we have obtained practical certainty that the compound is not a",
    "health concern."))
  ))
  
  # Tier 2

  observe({
    nhc = input$concern_yn == 1
    lost = if(nhc) input$pc[1] else 100 - input$pc[2]
    label = sprintf("P(HC %s %g)", (if(nhc) "<" else ">"), input$hd)
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
    # est = if(nhc) "conservative" else "best-case"
    hdsgn = if(nhc) "<" else ">"
    output$hd_text <- renderText({ sprintf(paste(
      "Elicit a number for the Human Concentration as a quantile",
      "<i>y</i> and the associated probability ",
      "<span style='white-space:nowrap'><i>P</i>(HC %s <i>y</i>).</span><p>"),
      hdsgn)})
    if(input$method == 1) {
      he_pr = lost - input$hd_pr
      hesgn = if(nhc) ">" else "<"
      output$he_text <- renderText({ sprintf(paste(
        "Elicit a number for the High Exposure as the quantile",
        "<i>x</i> such that <span style='white-space:nowrap'>",
        "<i>P</i>(HE %s <i>x</i>) = %g%%.</span><p>"),
        hesgn, he_pr)})
    } else {
      updateSliderInput(inputId="he", value=input$hd)
      sgn = if(nhc) "above" else "below"
      output$he_text <- renderText({ sprintf(paste(
        "Ask the experts to judge their probability that the High Exposure is",
        "%s <i>x</i> = %g %s.<p>"),
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
            "<p>Practical certainty is obtained if \\(\\frac{y}{x} %s 1\\).",
            {if(nhc) ">" else "<"}),
          sprintf(
            "<p>With these numbers, the ratio is \\(\\frac{%g}{%g} = %.2f\\).",
            input$hd, input$he, ratio)
        )))
      } else {
        lhs = 100 - min(100, input$hd_pr + input$he_pr)
        notlost = if(nhc) 100 - input$pc[1] else input$pc[2]
        reached = lhs >= notlost
        withMathJax(HTML(paste(
          "<p>Practical certainty is obtained if ",
          sprintf("\\(100 - \\min(100, P(HC %s y) + P(HE %s x)) \\geq %g\\).",
                  {if(nhc) "<" else ">"}, {if(nhc) ">" else "<"}, notlost),
          "<p>With these numbers, the left hand side is ",
          sprintf("\\(100 - \\min(100, %g + %g) = %g\\).",
                  input$hd_pr, input$he_pr, lhs)
        )))},
      
      HTML("<p>"),
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

  t2export <- function(file) {
    nhc = if(input$concern_yn == 1) "No health concern" else "Health concern"
    meth = if(input$method == 1) "Probabilities first" else "Numbers first"
    data <- list(
      "Tentative conclusion" = nhc,
      "Method" = meth,
      "HC value" = input$hd,
      "HC probability" = input$hd_pr)
    if(input$method == 1) {
      data["HE value"] = input$he
    } else {
      data["HE probability"] = input$he_pr
    }
    write.csv(data.frame("Tier 2" = names(data), " " = unlist(data)),
              file=file, row.names=FALSE)
  }
  
  ## Tier 3
  MAXPTS = 7
  
  h_shown <- reactiveValues()
  h_vals <- reactiveValues()
  h_fit <- reactiveValues()
  h_error <- reactiveValues()
  
  lapply(
    c("hd", "he"), function(h) {

      # Decide what rows to show depending on hx_points
      observe({
        n <- as.numeric(input[[paste0(h, "_points")]])
        h_shown[[h]] <-  c(1, diff(seq(
          (MAXPTS - 2) %/% 2, by=n - 2, len=MAXPTS - 1) %/% (MAXPTS - 2)), 1)
      })

      # Show/hide rows when h_shown$hx changes
      observe({
        sh <- h_shown[[h]]
        lapply(which(sh == 1), \(x) { shinyjs::show(sprintf("%s_l%d", h, x)) })
        lapply(which(sh == 0), \(x) { shinyjs::hide(sprintf("%s_l%d", h, x)) })
      })
      
      # Adjust min/max based on hx_min/hx_max
      observe({
        hmin <- input[[paste0(h, "_min")]]
        lapply(1:MAXPTS, \(x) { updateNumericInput(
          inputId=sprintf("%s_%d", h, x), min=hmin)})
        updateNumericInput(inputId=sprintf("%s_p%d", h, 1), value=1)
        updateNumericInput(inputId=sprintf("%s_%d", h, 1), value=hmin)
      })
      observe({
        hmax <- input[[paste0(h, "_max")]]
        lapply(1:MAXPTS, \(x) { updateNumericInput(
          inputId=sprintf("%s_%d", h, x), max=hmax)})
        updateNumericInput(inputId=sprintf("%s_p%d", h, MAXPTS), value=99)
        updateNumericInput(inputId=sprintf("%s_%d", h, MAXPTS), value=hmax)
      })
      
      # Collect current hx into dataframe
      h_vals[[h]] <- reactive({
        showns <- which(h_shown[[h]] == 1)
        probs = sapply(
          showns, \(x) { input[[sprintf("%s_p%d", h, x)]] })
        vals = sapply(
          showns, \(x) { input[[sprintf("%s_%d", h, x)]] })
        data.frame("Prob" = probs, "Value" = vals)
      })
      
      # Show an hx error message, if any
      h_error[[h]] <- NULL
      output[[paste0(h, "_error")]] <- renderUI(
        if(is.null(h_error[[h]])) "" else p(
          h_error[[h]], class="shiny-output-error")
      )
    })
  

  fitsorted <- function(data, resort=FALSE){
    d <- if(resort) data[order(data$Value),] else data

    if(is.unsorted(d$Prob, strictly=TRUE) ||
       is.unsorted(d$Value, strictly=TRUE)) {
      stop("Probabilities and values must be sorted")
    }
    SHELF::fitdist(
      vals = unlist(d$Value),
      probs = unlist(d$Prob / 100),
      lower = 0)
  }

  lapply(c("hd", "he"), function(h) {
    observe(tryCatch({
      h_fit[[h]] <- fitsorted(h_vals[[h]]())
      h_error[[h]] <- NULL
    },
    error=\(e) { h_error[[h]] <- e$message }))
  })
  
  # Get numbers from the SHELF fit with the given probability distribution
  # and probabilities/quantiles
  # 'what' is q, d or p (as in qsn, dsn, psn etc.)
  probfunc <- function(what, fit, dist, xqp){
    if(dist == "skewnormal"){
      snd = fit$Skewnormal
      if(is.na(snd$location))
        stop("Failed to fit skew-normal distribution")
      func = paste0(what, "sn")
      do.call(func, list(xqp, snd$location, snd$scale, snd$slant))
    } else if (dist == "lognormal"){
      lnd = fit$Log.normal
      if(is.na(lnd$mean.log.X))
        stop("Failed to fit log-normal distribution")
      func = paste0(what, "lnorm")
      do.call(func, list(xqp, lnd$mean.log.X, lnd$sd.log.X))
    } else if (dist == "normal"){
      nd = fit$Normal
      if(is.na(nd$mean))
        stop("Failed to fit normal distribution")
      func = paste0(what, "norm")
      do.call(func, list(xqp, nd$mean, nd$sd))
    } else {
      stop("Unknown distribution")
    }
  }

  qhd <- function(probs){ probfunc("q", h_fit$hd, input$hd_dist, probs) }
  qhe <- function(probs){ probfunc("q", h_fit$he, input$he_dist, probs) }
  dhd <- function(x){ probfunc("d", h_fit$hd, input$hd_dist, x) }
  dhe <- function(x){ probfunc("d", h_fit$he, input$he_dist, x) }
  phd <- function(q){ probfunc("p", h_fit$hd, input$hd_dist, q) }
  phe <- function(q){ probfunc("p", h_fit$he, input$he_dist, q) }

  # Returns list of quantiles, ignoring the outermost tails of hd & he
  h_qrange <- function(n, pskip) {
    minq = min(qhd(pskip), qhe(pskip))
    maxq = max(qhd(1 - pskip), qhe(1 - pskip))
    seq(minq, maxq, len=n)
  }
  
  # Compute the probability of HD < HE
  t3compute = function(n = 10000, pskip = 1e-5) {
    # Suitable points to integrate over
    x = h_qrange(n, pskip)
    step = (x[length(x)] - x[1]) / n
    # Compute the integral in two ways and average 
    overlap1 = dhd(x) * (1 - phe(x))
    overlap2 = dhe(x) * phd(x)
    (sum(overlap1) + sum(overlap2)) * .5 * step
  }

  output$t3text <- renderUI({
    if(!is.null(h_error[["hd"]]) || !is.null(h_error[["he"]]))
      return(NULL)
    prob <- t3compute() * 100
    if(prob < 1) {
      prprob <- "< 1"
    } else if(prob > 99) {
      prprob <- "> 99"
    } else {
      prprob <- sprintf("≈ %d", round(prob))
    }

    list(
      HTML("<p>With this model of the HC and HE distributions,"),
      span(sprintf("P(HC < HE) %s%%", prprob), style="white-space:nowrap"),

      if(prob <= input$pc[1] || prob >= input$pc[2]) {
        nhc = (prob <= input$pc[1])
        probx = if(nhc) 100 - prob else prob
        HTML(sprintf(paste(
          "<p>Practical certainty is reached. The experts are at least %.0f%%",
          "certain that the compound %s a health concern."),
          {floor(probx * 1) / 1},
          {if(nhc) "is not" else "is"} ))
      } else {
        HTML(paste(
        "<p>Practical certainty is not reached. The assessment is inconclusive.",
        "Proceed with a refined approach to determine if the compound is",
        "a health concern."))
      }
    )
  })
  
  output$t3plot <- renderPlot({
    if(!is.null(h_error[["hd"]]) || !is.null(h_error[["he"]]))
      return(NULL)

    x = h_qrange(n=200, pskip=1e-4)
    pldata = data.frame(x = x, HC = dhd(x), HE = dhe(x))
    plp = tidyr::pivot_longer(pldata, c("HC", "HE"))
    
    ggplot(plp, aes(x=x, y=value, fill=name, color=name)) + 
      geom_area(alpha=0.3, outline.type="upper", position="identity") +
      scale_color_manual(values=c("red", "blue")) +
      scale_fill_manual(values=c("red", "blue")) +
      labs(x=sprintf("[%s]", input$units), y="Probability") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12),
            legend.title = element_blank(),
            legend.position = c(0.93, 0.88),
            legend.justification = c(0.93, 0.88),
            legend.key.size = unit(1.1, 'cm'),
            legend.text = element_text(size=12)) +
      ggtitle("Uncertainty in High Exposure and the Human Concentration")
  })

  t3export <- function(file) {
    writeLines("Tier 3", file)
    writeLines(paste("HC distribution", input$hd_dist, sep=","), file)
    write.csv(h_vals$hd(), file=file, row.names=FALSE)
    writeLines(paste("HE distribution", input$he_dist, sep=","), file)
    write.csv(h_vals$he(), file=file, row.names=FALSE)
    p <- tryCatch(sprintf("%.4g", t3compute()), error=\(e) { NA } )
    writeLines(paste("P(HC < HE)", p, sep=","), file)
  }
  
  # Export
  fileexport <- function(file) {
    writeLines(paste("# Exported at", Sys.time()), file)
    unitsexport(file)
    writeLines("", file)
    t2export(file)
    writeLines("", file)
    t3export(file)
  }
  
  output$download <- downloadHandler(
    filename = format(Sys.time(), "uncertainty-%Y-%m-%d_%H%M.csv"),
    content = function(file) {
      fc = file(file, open = "w")
      fileexport(fc)
      close(fc)
    },
    contentType = "text/csv")

}

