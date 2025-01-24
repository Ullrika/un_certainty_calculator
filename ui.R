library(shinyjs)
library(DT)

fluidPage(
  useShinyjs(),
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
          "units", label = "Units of measurement", value = "µg / kg bw"),
        splitLayout(
          numericInput(
            "hdRangeMin", label = "Human Dose low limit", min = 1,
            max = 100000, step = 1, value = 10),
          numericInput(
            "hdRangeMax", label = "high limit", min = 10,
            max = 1000000, step = 1, value = 200)
        ),
        splitLayout(
          numericInput(
            "heRangeMin", label = "High Exposure low limit", min = 1,
            max = 100000, step = 1, value = 10),
          numericInput(
            "heRangeMax", label = "high limit", min = 10,
            max = 1000000, step = 1, value = 200)
        )
      )
    ),
    
    tabPanel(
      "Tier 1",
      htmlOutput("t1text"),
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
          width = 5,
          uiOutput("t2results")
        )
      )
    ),

    tabPanel(
      "Tier 3",

      sidebarLayout(
        sidebarPanel(
          width = 4,
          selectInput(
            "hd_points", label = "# of HD percentiles",
            choices = list("2 points" = 2, "3 points" = 3,
                           "4 points" = 4, "5 points" = 5),
            selected = 3),

          # uiOutput("hd_list"),
          DTOutput("hd_table"),
          # shinyjs::hidden(textOutput("hd_error")),

          selectInput(
            "hd_dist", label = "HD distribution",
            choices = list("Normal" = "normal", "Log-normal" = "lognormal",
                          "Skew-normal"="skewnormal"),
            selected = "lognormal"),
          
          selectInput(
            "he_points", label = "# of HE percentiles",
            choices = list("2 points" = 2, "3 points" = 3,
                           "4 points" = 4, "5 points" = 5),
            selected = 3),
          
          DTOutput("he_table"),

          selectInput(
            "he_dist", label = "HE distribution",
            choices = list("Normal" = "normal", "Log-normal" = "lognormal",
                          "Skew-normal"="skewnormal"),
            selected = "lognormal")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          width = 6,
          hidden(plotOutput("t3plot")),
          htmlOutput("t3text")
        )
      )
    )

  )
)

