library("shiny")
library("tidyverse")
library("corrplot")
library("caret")
library("randomForest")

# load in heart data & averages for prediction interface
data = read.csv("./heart.csv")
averages = read.csv("./averages.csv")

my_ui <- fluidPage(
  
  # header
  h1("Analysis and Prediction of Heart Disease"),
  
  # tabsetPanel --> enables the storage of each "function" on its own page
  tabsetPanel(
    # <https://shiny.rstudio.com/reference/shiny/1.2.0/insertTab.html> for tabs
  
    # tabPanel --> indicates a new/individual tab
    tabPanel( "Correlation Plot", fluid = TRUE,
              # correlation plot
      plotOutput(outputId = "correlations")
    ),
  
    # tab for the heart problems by variable plots
    tabPanel("Variable Plots", fluid = TRUE,
             # dropdown for choosing variable
    selectInput(inputId = "var1", label = "Choose a variable to compare:", 
              choices = c("Age", "Sex", "Exercise Induced Angina", 
                          "Number of Major Blood Vessels", 
                          "Type of Chest Pain", "Resting Blood Pressure",
                          "Cholesterol Level", "Fasting Blood Sugar", 
                          "Resting ECG", "Thalassemia Results", 
                          "Maximum Heart Rate", "Slope of Peak Exercise")),
    # plot call
    plotOutput(outputId = "compare")
  ),
  
  # tab panel for prediction interface
  tabPanel("Prediction Model", fluid = TRUE,
    sidebarLayout(
      # massive sidebar taking user text inputs for multiple variables
      sidebarPanel(
        textInput(inputId = "age", label = "Enter your age below"),
        textInput(inputId = "sex", label = "Enter your sex below (0: male, 1: female)"),
        textInput(inputId = "cp", label = "Enter your type of chest pain below
                (1: typical angina, 2: atypical angina, 3: non-anginal pain, 
                4: asymptomatic)"),
        textInput(inputId = "rbps", label = "Enter your resting blood pressure below"),
        textInput(inputId = "chol", label = "Enter your cholesterol level below"),
        textInput(inputId = "maxrate", label = "Enter your maximum heart rate below"),
        textInput(inputId = "thals", label = "Enter your thalassemia diagnosis 
                below (2: normal, 1: fixed defect, 3: reversible defect)")
        # style: changes background color to match scheme, and font color to then match box better
      , style="background-color:lightsalmon;color:#4d190f"
      ),
    
      # learned colors/styling from <https://felixluginbuhl.com/textyle/>
      mainPanel(
        # colors <https://www.google.com/search?q=%23FF6666&rlz=1C1GCEU_enUS862US862&oq=%23FF6666&aqs=chrome..69i57.297j0j1&sourceid=chrome&ie=UTF-8>
        # information
        h2(""),
        h4("Please enter at least 5 values before trusting your results.", style="color:#d4571e"),
        h4("Additionally, please understand that there are several more variables 
           that go into this analysis, which are excluded because, unless you've 
           already been admitted for heart disease, you are unlikely to know them 
           off the top of your head, and we're not doctors. Happy heart disease!", 
           style="color: sandybrown"),
        textOutput(outputId = "predDefault"),
        # the actual prediction
        textOutput(outputId = "pred"),
        #learned CSS integration from https://stackoverflow.com/questions/24049159/change-the-color-and-font-of-text-in-shiny-app
        tags$head(tags$style("#predDefault{font-size: 20px;padding-top: 20px;}")),
        tags$head(tags$style("#pred{font-size: 18px;padding-top: 10px; 
                             font-style: italic;}"))
        )
      )
    )
  )
)


my_server <- function(input, output) {

  output$correlations <- renderPlot ({
    # correlation plot: using custom colors to match heart scheme
    # from hazel :D
    corr = corrplot(cor(heart), method = "shade",
                    title = "Correlation of Heart Data",
                    col = colorRampPalette(c("white","lightsalmon","brown2"))(100),
                    tl.pos = "l", # position of table labels
                    mar = c(2, 1, 3, 1),
                    tl.cex = 0.75, # size of labels
                    tl.col = "darkred" # label colors
    ) 
    
    corr
  })
  
  output$compare <- renderPlot ({
    var1 = input$var1
    var2 = input$var2
    
    # if statement takes the chosen variable and assigns that to be the variable used in the produced plot
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
    
    #bar colors from https://www.geeksforgeeks.org/change-color-of-bars-in-barchart-using-ggplot2-in-r/
    baryay = ggplot(data, aes(x=var1, y=output)) + 
      geom_bar(aes(fill = var1), stat='identity') +
      scale_fill_gradient2(mid = "sandybrown",
                           high = "brown2") + # colors for aesthetics
      ggtitle("Number of People with Heart Diseases by Chosen Variable") +
      xlab(input$var1) +
      ylab("Number of Heart Attacks") +
      theme_bw(base_size = 16)
    
    baryay
  })
  
  output$predDefault <- renderText({
    #creating text for static introduction to prediction
    str1 = "Using a Random Forest model (which has a 93% accuracy), we have 
        determined that:"
    
    str1
  })
  
  output$pred <- renderText({
    #creating dynamic text for prediction
    
    #random forest model - from hazel
      hrtmodel <- randomForest(as.factor(output) ~ ., data=data, 
                        importance=TRUE, proximity=TRUE, mtry=1, ntrees=500)
      
    #creating prediction var
      preds = 2
      
    #changing testing data using inputs and predicting on new data
      if (input$age != "") {
        averages["age"] = input$age
        preds = predict(hrtmodel, averages)
      }
      if (input$sex != "") {
        averages["sex"] = input$sex
        preds = predict(hrtmodel, averages)
      }
      if (input$cp != "") {
        averages["cp"] = input$cp
        preds = predict(hrtmodel, averages)
      }
      if (input$rbps != "") {
        averages["restbps"] = input$rbps
        preds = predict(hrtmodel, averages)
      }
      if (input$chol != "") {
        averages["chol"] = input$chol
        preds = predict(hrtmodel, averages)
      }
      if (input$maxrate != "") {
        averages["maxrate"] = input$maxrate
        preds = predict(hrtmodel, averages)
      }
      if (input$thals != "") {
        averages["thal"] = input$thals
        preds = predict(hrtmodel, averages)
        
      }
    
    
      # responses/outputs based off the prediction
    if (preds == 2) {
      str = "A random patient is unlikely to have heart problems."
    } else if (preds == 1) {
      str = "You are likely to have heart problems. CLOSEST HOSPITAL: Kaiser Permanente Bellevue Medical Center (3.8 stars - 335 reviews!)"
    } else if (preds == 0) {
      str = "You are unlikely to have heart problems."
    }
    
    str
    
  })
  
}

shinyApp(ui = my_ui, server = my_server)



