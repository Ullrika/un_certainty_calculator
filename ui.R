library(shinyjs)
#library(DT)

MAXPTS <- 7
initprobs <- c(1, 5, 20, 50, 80, 95, 99)
hdrange <- c(.01, 100)
inithd <- c(10, 20, 25, 32, 50, 70, 90)
herange <- c(.01, 40)
inithe <- c(5, 8, 12, 15, 20, 27, 35)

fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
    .unctight div { margin-bottom: .05em }
    .shiny-split-layout > div { overflow: visible }
    "))),

  # Application title
  titlePanel("The (un)certainty calculator"),
  
  navlistPanel(
    "Menu",
    widths = c(2, 10),
    tabPanel(
      "Start",
      p("Welcome to the (un)certainty calculator."),
      p("Copyright Ullrika Sahlin, Lund University."),
      p("This calculator is developed to support elicitation and combination of uncertainty in hazard and exposure of a human health assessment."),
      p("1) Set thresholds for practical certainty (from the decision maker)."),
      p("2) Set unit and possible ranges for the two quantities of interest."),
      p("3) Start with the simplest option, to express overall uncertainty directly (Ref 1). If not conclusive, move on to Ref 2."),
      p("4) In Ref 2, you evaluate uncertainty on hazard and exposure separately and combine by probability bounds analysis. Choose potential outcome and the order to put in judgements,"),
      p("5) Fill in judgement from the group of experts and explore the results from calculations."),
      p("6) If not conclusive, move to Ref 3."),
      p("7) Fill in more judgements, and select probability distribution, and explore the results from probabilistic calculations.")
    ),
    tabPanel(
      "Practical certainty",
      
      verticalLayout(
        sliderInput(
          "pc", label = h3("Practical certainty"), min = 0, max = 100, 
          value = c(10, 90), post="%"),
        tableOutput("pctable"),
        htmlOutput("pctext")
      )
    ),
    
    tabPanel(
      "Units",
      
      verticalLayout(
        textInput(
          "units", label = "Units of measurement", value = "µM"),
        HTML("<label class='control-label'>Human Concentration limits</label>"),
        splitLayout(
          numericInput(
            "hd_min", label = "lowest plausible", min = 0,
            max = 100000, step = 1, value = hdrange[1]),
          numericInput(
            "hd_max", label = "highest plausible", min = 0,
            max = 1000000, step = 1, value = hdrange[2])
        ),
        HTML("<label class='control-label'>High Exposure limits</label>"),
        splitLayout(
          numericInput(
            "he_min", label = "lowest plausible", min = 0,
            max = 100000, step = 1, value = herange[1]),
          numericInput(
            "he_max", label = "highest plausible", min = 0,
            max = 1000000, step = 1, value = herange[2])
        )
      )
    ),
    
    tabPanel(
      "Ref 1",
      htmlOutput("t1text")
    ),
    
    tabPanel(
      "Ref 2",
      
      sidebarLayout(
        sidebarPanel(
          width = 6,
          radioButtons(
            "concern_yn", label = "Tentative conclusion",
            choices = list("No health concern" = 1, "Health concern" = 2),
            selected = 1),

          splitLayout(
            radioButtons(
              "method", label = "Method",
              choices = list("Probabilities first" = 1, "Numbers first" = 2),
              selected = 1),
            
            radioButtons(
              "order", label = "Order",
              choices = list("HC first" = 1, "Exposure first" = 2),
              selected = 1),
          ),
          
          div(id="flexy", style="display: flex; flex-direction: column;",
              div(id="hd_div", style="order: 1",
                  htmlOutput("hd_text"),
                  sliderInput("hd", "Point of Departure for HC: x µM",
                              min = hdrange[1], max = hdrange[2],
                              value = inithd[4], step = 1)),
              
              div(id="hd_pr_div", style="order: 2",
                  htmlOutput("hd_pr_text"),
                  sliderInput("hd_pr", "P(HC < x)",
                              min = 0, max = 50, value = 5, post="%")),
              div(id="he_pr_div", style="order: 3",
                  htmlOutput("he_pr_text", style="order: 3"),
                  sliderInput("he_pr", "P(HE > y)",
                              min = 0, max = 50, value = 5, post="%")),
              div(id="he_div", style="order: 4",
                  htmlOutput("he_text"),
                  sliderInput("he", "High Exposure: y µM",
                              min = herange[1], max = herange[2],
                              value = inithe[4], step = 1))
          ),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          width = 6,
          uiOutput("t2results")
        )
      )
    ),

    tabPanel(
      "Ref 3",

      sidebarLayout(
        sidebarPanel(
          width = 5,

          p(paste("Make judgement about uncertainty in the ",
               "target Human Concentration by adjusting or providing more quantiles:")),
          splitLayout(
            selectInput(
              "hd_dist", label = "HC distribution",
              choices = list("Normal" = "normal", "Log-normal" = "lognormal",
                             "Skew-normal"="skewnormal"),
              selected = "lognormal"),
            selectInput(
              "hd_points", label = "Number of quantiles",
              choices = setNames(
                2:MAXPTS, lapply(2:MAXPTS, function(x){sprintf("%d quantiles", x)})),
              selected = 3)
          ),

          lapply(1:MAXPTS, function(x) {
            labs <- if(x == 1) c("Probability", "Value") else c(NULL, NULL)
            splitLayout(
              id=sprintf("hd_l%d", x),
              numericInput(sprintf("hd_p%d", x), label=labs[1],
                           min=1, max=99, step=1,
                           value=initprobs[x]),
              numericInput(sprintf("hd_%d", x), label=labs[2],
                           min=hdrange[1], max=hdrange[2], step=1,
                           value=inithd[[x]] ),
              class=if(x < MAXPTS) "unctight" else "")
          }),
          htmlOutput("hd_error"),

          p(paste("Make judgement about a ",
                  "High Exposure in the target population by adjusting or providing more quantiles:")),
          
          splitLayout(
            selectInput(
              "he_dist", label = "HE distribution",
              choices = list("Normal" = "normal", "Log-normal" = "lognormal",
                             "Skew-normal"="skewnormal"),
              selected = "lognormal"),
            selectInput(
              "he_points", label = "Number of quantiles",
              choices = setNames(
                2:MAXPTS, lapply(2:MAXPTS, function(x){sprintf("%d quantiles", x)})),
              selected = 3)
          ),

          lapply(1:MAXPTS, function(x) {
            labs <- if(x == 1) c("Probability", "Value") else c(NULL, NULL)
            splitLayout(
              id = sprintf("he_l%d", x),
              numericInput(sprintf("he_p%d", x), label=labs[1],
                           min=1, max=99, step=1,
                           value=initprobs[x]),
              numericInput(sprintf("he_%d", x), label=labs[2],
                           min=herange[1], max=herange[2], step=1,
                           value=inithe[[x]]),
              class="unctight")
          }),
          htmlOutput("he_error")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          width = 7,
          plotOutput("t3plot"),
          htmlOutput("t3text", style="margin-top: .5em")
        )
      )
    ),

    tabPanel(
      "Export",
      helpText("Export all your numbers in a one CSV file"),
      downloadButton("download", "Download CSV")
    )
    
  )
)

