library(shinyjs)
library(DT)

MAXPTS <- 7
initprobs <- c(1, 5, 20, 50, 80, 95, 99)

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
      "Practical certainty",
      
      verticalLayout(
        sliderInput(
          "pc", label = h3("Practical certainty"), min = 0, max = 100, 
          value = c(10, 80), post="%"),
        tableOutput("pctable")
      )
    ),
    
    tabPanel(
      "Units",
      
      verticalLayout(
        textInput(
          "units", label = "Units of measurement", value = "µM"),
        splitLayout(
          numericInput(
            "hd_min", label = "Human Dose low limit", min = 1,
            max = 100000, step = 10, value = 20),
          numericInput(
            "hd_max", label = "high limit", min = 10,
            max = 1000000, step = 10, value = 200)
        ),
        splitLayout(
          numericInput(
            "he_min", label = "High Exposure low limit", min = 1,
            max = 100000, step = 10, value = 10),
          numericInput(
            "he_max", label = "high limit", min = 10,
            max = 1000000, step = 10, value = 130)
        )
      )
    ),
    
    tabPanel(
      "Tier 1",
      htmlOutput("t1text")
    ),
    
    tabPanel(
      "Tier 2",
      
      sidebarLayout(
        sidebarPanel(
          width = 5,
          radioButtons(
            "concern_yn", label = "Tentative conclusion",
            choices = list("No health concern" = 1, "Health concern" = 2),
            selected = 1),

          radioButtons(
            "method", label = "Method",
            choices = list("Probabilities first" = 1, "Numbers first" = 2),
            selected = 1),
          
          htmlOutput("hd_text"),
          sliderInput("hd", "Point of departure (y)",
                      min = 1, max = 200, value = 10),
          sliderInput("hd_pr", "P(HC < y)",
                      min = 1, max = 25, value = 5, post="%"),

          htmlOutput("he_text"),
          sliderInput("he_pr", "P(HE > x)",
                      min = 1, max = 50, value = 5, post="%"),
          sliderInput("he", "High Exposure",
                      min = 1, max = 200, value = 100)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          width = 7,
          uiOutput("t2results")
        )
      )
    ),

    tabPanel(
      "Tier 3",

      sidebarLayout(
        sidebarPanel(
          width = 5,

          p(paste("Elicit expert information about the expected",
               "Human Concentration distribution:")),
          splitLayout(
            selectInput(
              "hd_dist", label = "HC distribution",
              choices = list("Normal" = "normal", "Log-normal" = "lognormal",
                             "Skew-normal"="skewnormal"),
              selected = "lognormal"),
            selectInput(
              "hd_points", label = "Number of points",
              choices = setNames(
                2:MAXPTS, lapply(2:MAXPTS, function(x){sprintf("%d points", x)})),
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
                           min=20, max=200, step=1,
                           value=round(20 + (x - 1) * 180 / (MAXPTS - 1))),
              class=if(x < MAXPTS) "unctight" else "")
          }),
          htmlOutput("hd_error"),

          p(paste("Elicit expert information about the expected",
                  "High Exposure distribution:")),
          
          splitLayout(
            selectInput(
              "he_dist", label = "HE distribution",
              choices = list("Normal" = "normal", "Log-normal" = "lognormal",
                             "Skew-normal"="skewnormal"),
              selected = "lognormal"),
            selectInput(
              "he_points", label = "Number of points",
              choices = setNames(
                2:MAXPTS, lapply(2:MAXPTS, function(x){sprintf("%d points", x)})),
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
                           min=10, max=130, step=1,
                           value=round(10 + (x - 1) * 120 / (MAXPTS - 1))),
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
    )
    
  )
)

