fluidPage(
  
  # Application title
  titlePanel("The (un)certainty calculator"),
  
  navlistPanel(
    "Menu",
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
          sliderInput("hd",
                      "Point of departure (Human Dose)",
                      min = 1,
                      max = 200,
                      value = 70),
          sliderInput("hd_pr",
                      "P(HD < y)",
                      min = 1,
                      max = 25,
                      value = 5),
          hr(),
          textInput("he_pr",
                    "P(HE > x)"),
          # sliderInput("he_pr",
          #             "P(HE > x)",
          #             min = 1,
          #             max = 25,
          #             value = 5),
          sliderInput("he",
                      "High Exposure",
                      min = 1,
                      max = 200,
                      value = 100)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          uiOutput("t2results")
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

