library("shiny")
library("tidyverse")
library("corrplot")
library("caret")

data = read.csv("./heart.csv")

my_ui <- fluidPage(
  h1("Analysis and Prediction of Heart Disease"),
  plotOutput(outputId = "correlations"),
  selectInput(inputId = "var1", label = "Choose a variable to compare:", 
              choices = c("Age", "Sex", "Exercise Induced Angina", 
                          "Number of Major Blood Vessels", 
                          "Type of Chest Pain", "Resting Blood Pressure",
                          "Cholesterol Level", "Fasting Blood Sugar", 
                          "Resting ECG", "Thalassemia Results", 
                          "Maximum Heart Rate", "Slope of Peak Exercise")),
  plotOutput(outputId = "compare")
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
  
  output$compare <- renderPlot ({
    var1 = input$var1
    var2 = input$var2
    
    if (var1 == "Age") {
      var1 = data$age
    } else if (var1 == "Sex") {
      var1 = data$sex
    } else if (var1 == "Exercise Induced Angina") {
      var1 = data$exng
    } else if (var1 == "Number of Major Blood Vessels") {
      var1 = data$majvessel
    } else if (var1 == "Type of Chest Pain") {
      var1 = data$cp
    } else if (var1 == "Resting Blood Pressure") {
      var1 = data$restbps
    } else if (var1 == "Cholesterol Level") {
      var1 = data$chol
    } else if (var1 == "Fasting Blood Sugar") {
      var1 = data$fbs
    } else if (var1 == "Resting ECG") {
      var1 = data$restecg
    } else if (var1 == "Thalassemia Results") {
      var1 = data$thall
    } else if (var1 == "Maximum Heart Rate") {
      var1 = data$maxrate
    } else if (var1 == "Slope of Peak Exercise") {
      var1 = data$slp
    }
    
    baryay = ggplot(data, aes(x=var1, y=output)) + 
      geom_bar(aes(fill = var1), stat='identity') +
      scale_fill_gradient2(mid = "sandybrown",
                           high = "brown2") + 
      ggtitle("Number of People with Heart Diseases by Chosen Variable") +
      xlab(input$var1) +
      ylab("Number of Heart Attacks") +
      theme_bw(base_size = 16)
    
    baryay
  })
  
}

shinyApp(ui = my_ui, server = my_server)