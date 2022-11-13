library("shiny")
library("tidyverse")
library("corrplot")
library("caret")


data = read.csv("./heart.csv")

my_ui <- fluidPage(
  h1("Analysis and Prediction of Heart Disease"),
  plotOutput(outputId = "correlations")
)

my_server <- function(input, output) {

  output$correlations <- renderPlot ({
    corr = corrplot(cor(heart), method = "shade",
                    title = "Correlation of Heart Data",
                    col = colorRampPalette(c("white","lightsalmon","brown2"))(100),
                    tl.pos = "l", 
                    mar = c(2, 1, 3, 1),
                    tl.cex = 0.75,
                    tl.col = "darkred"
    ) 
    
    corr
  })
  
}

shinyApp(ui = my_ui, server = my_server)