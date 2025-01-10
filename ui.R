library(shinyjs)
library(DT)

fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("The (un)certainty calculator"),
  
  navlistPanel(
    "Menu",
    widths = c(2, 7),
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
      "Tier 2",
      
      sidebarLayout(
        sidebarPanel(
          width = 5,
          radioButtons(
            "concern_yn", label = "Hypothesis",
            choices = list("No health concern" = 1, "Health concern" = 2),
            selected = 1),
          sliderInput("hd",
                      "Point of departure (Human Dose)",
                      min = 1,
                      max = 200,
                      value = 70),
          sliderInput("hd_pr",
                      "P(HD < y)",
                      min = 1,
                      max = 25,
                      value = 5,
                      post="%"),

          helpText("dingo"),
          radioButtons(
            "method", label = "Method",
            choices = list("Probabilities first" = 1, "Numbers first" = 2),
            selected = 1),
          
          htmlOutput("he_text"),
          sliderInput("he_pr",
                      "P(HE > x)",
                      min = 1,
                      max = 50,
                      value = 5,
                      post="%"),
          sliderInput("he",
                      "High Exposure",
                      min = 1,
                      max = 200,
                      value = 100)
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
          width = 6,
          selectInput(
            "hd_points", label = "# of HD percentiles",
            choices = list("min/max" = 2, "min/med/max" = 3,
                           "4 points" = 4, "5 points" = 5),
            selected = 3),

          DTOutput("hd_table"),

          selectInput(
            "hd_dist", label = "HD distribution",
            choices = list("Normal" = "normal", "Log normal" = "lognormal",
                          "Skew normal"="skewnormal"),
            selected = "lognormal"),
          
          selectInput(
            "he_points", label = "# of HE percentiles",
            choices = list("min/max" = 2, "min/med/max" = 3),
            selected = 3),
          
          DTOutput("he_table"),

          selectInput(
            "he_dist", label = "HE distribution",
            choices = list("Normal" = "normal", "Log normal" = "lognormal",
                          "Skew normal"="skewnormal"),
            selected = "lognormal")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          width = 6,
          plotOutput("t3plot")
        )
      )
    ),
      
    tabPanel(
      "Example",
      
      # Sidebar with slider input for thresholds for practical certainty
      
      
      # Sidebar with a slider input for quantiles 
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot")
        )
      )
    )
  )
)

