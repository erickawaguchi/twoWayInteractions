#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visualizing Two-Way Interactions:"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("beta0",
                        paste(expression(beta0)),
                        min = -2,
                        max = 2,
                        step = 0.01,
                        value = 0),
            sliderInput("betaG",
                        paste(expression(betaG)),
                        min = -2,
                        max = 2,
                        step = 0.01,
                        value = 0),
            sliderInput("betaE",
                        paste(expression(betaE)),
                        min = -2,
                        max = 2,
                        step = 0.01,
                        value = 1.0),
            sliderInput("betaGxE",
                        paste(expression(betaGxE)),
                        min = -2,
                        max = 2,
                        step = 0.01,
                        value = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           verbatimTextOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      par(mar=c(5, 4, 4, 8), xpd=TRUE)
      
        # generate bins based on input$bins from ui.R
        plot(0, xlim = c(-0.2, 1.2), xaxt = "n", ylim = c(-5, 5), cex = 0,
             xlab = "G", ylab = "y", 
             main = "y = beta0 + G * betaG + E * betaE + (GxE) * betaGxE")
        axis(side = 1, at = c(0, 1), labels = c(0, 1))
        #G = 0;E = 0 
        points(x = 0, y = input$beta0, col = "blue", cex = 1.5, pch = 19)
        #G = 1; E = 0
        points(x = 1, y = input$beta0 + input$betaG, col = "blue", cex = 1.5, pch = 19)
        #G = 0; E = 1
        points(x = 0, y = input$beta0 + input$betaE, col = "red", cex = 1.5, pch = 17)
        #G = 1; E = 1
        points(x = 1, y = input$beta0 + input$betaG + input$betaE + input$betaGxE, col = "red", cex = 1.5, pch = 17)
        segments(x0 = 0, x1 = 1, y0 = input$beta0, y1 = input$beta0 + input$betaG, col = "blue")
        segments(x0 = 0, x1 = 1, y0 = input$beta0 + input$betaE, y1 = input$beta0 + input$betaG + input$betaE + input$betaGxE, col = "red")
        legend("topright", inset=c(-0.3, 0), legend=c("E = 0","E = 1"), pch = c(19, 17), cex = 1.5)
    })
    
    #output$table <- renderTable({
    #  
    #  out <- data.frame(matrix(c(0, 1, 0, 1, 0, 0, 1, 1, 
    #           input$beta0,
    #           input$beta0 + input$betaG,
    #           input$beta0 + input$betaE,
    #           input$beta0 + input$betaG + input$betaE + input$betaGxE), 
    #         nrow = 4))
    #  names(out) <- c("G", "E", "y")
    #  out
    #})
    
    output$table <- renderText({
      paste0(
        "Summary:", "\n",
        "G = 0; E = 0: y = beta0 = ", input$beta0, "\n",
        "G = 1; E = 0: y = beta0 + betaG = ", input$beta0 + input$betaG, "\n",
        "G = 0; E = 1: y = beta0 + betaE = ", input$beta0 + input$betaE, "\n",
        "G = 1; E = 1: y = beta0 + betaG + betaE + betaGxE = ", input$beta0 + input$betaG + input$betaE + input$betaGxE, "\n",
        c("-----------------------------------------------------------"), "\n",
        "G = 0, E = 0 vs. 1: y = betaE = ", input$betaE, "\n",
        "G = 1, E = 0 vs. 1: y = betaE + betaGxE = ", input$betaE + input$betaGxE, "\n",
        "E = 0, G = 0 vs. 1: y = betaG = ", input$betaG, "\n",
        "E = 1, G = 0 vs. 1: y = betaG + betaGxE = ", input$betaG + input$betaGxE, "\n",
        c("-----------------------------------------------------------"), "\n"
      )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
