#
# This is a Shiny web application built to perform probability bound calculations 
# based on quantiles representing uncertainty in the hazardous exposure dose H and 
# uncertainty in an estimated high exposure in the target population. 
#
# Ullrika Sahlin (Lund University)
# Contributors
# Dawei Tang (Unilever)
# Carl Troein (Lund University)
# This work was done as part of RiskHunt3R (EU Horizon 2020 grant No 964537)
# 

#https://github.com/Ullrika/un_certainty_calculator.git

library(shiny)
# library(shinylive)
# library(httpuv)
# library(shinyjs)

# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("The (un)certainty calculator"),
#     
#     # Sidebar with slider input for thresholds for practical certainty
#     
# 
#     # Sidebar with a slider input for quantiles 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

# Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }

# Run the application 
shinyApp(ui = ui, server = server)
