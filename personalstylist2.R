# Install necessary packages
install.packages("rpart")
install.packages("dplyr")
install.packages("caret")
install.packages("shiny")

# Load the packages
library(rpart)
library(dplyr)
library(caret)
library(shiny)
# Continue to load other packages as needed
# Read the data
data <- read.csv("28 virtual_personal_stylist_data.csv")

# Remove User ID as it's not a useful feature
data$User.ID <- NULL

# Convert categorical variables to factors
categorical_cols <- c("Gender", "Body.Shape", "Favorite.Color", "Style.Preference", 
                      "Preferred.Brands", "Season", "Occasion")
data[categorical_cols] <- lapply(data[categorical_cols], factor)

# Check for class imbalance
class_distribution <- table(data$ItemReturned)
print(class_distribution)

# Split the data into training and test sets
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(data$ItemReturned, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train the Decision Tree model
dt_model <- rpart(ItemReturned ~ ., data = trainData, method = "class",
                  cp = 0.01,          # Complexity parameter
                  minsplit = 100,      # Minimum number of observations required to attempt a split
                  maxdepth = 10)       # Maximum depth of any node of the final tree

# Print the model summary
print(dt_model)

# Evaluate the model on the test set
predictions <- predict(dt_model, testData, type = "class")

# Save and load the model as needed
saveRDS(dt_model, "decision_tree_model.rds")
dt_model <- readRDS("decision_tree_model.rds")


# Define the UI
ui <- fluidPage(
  titlePanel("Decision Tree Model Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age", value = 30),
      selectInput("gender", "Gender", choices = c("Male", "Female", "Other")),
      numericInput("height", "Height (cm)", value = 170),
      numericInput("weight", "Weight (kg)", value = 70),
      selectInput("body_shape", "Body Shape", choices = c("Rectangle", "Hourglass", "Oval", "Pear", "Triangle")),
      selectInput("favorite_color", "Favorite Color", choices = c("Black", "White", "Red", "Blue", "Green", "Yellow", "Purple", "Orange")),
      selectInput("style_preference", "Style Preference", choices = c("Boho", "Chic", "Sporty")),
      selectInput("preferred_brands", "Preferred Brands", choices = c("Brand A", "Brand B", "Brand C", "Brand D")), # Replace with actual brand names
      selectInput("season", "Season", choices = c("Spring", "Summer", "Fall", "Winter")),
      selectInput("occasion", "Occasion", choices = c("Casual Outing", "Formal Event", "Work")),
      numericInput("budget", "Budget ($)", value = 100),
      numericInput("past_purchase", "Past Purchase Count", value = 0),
      numericInput("satisfaction_rating", "Satisfaction Rating", value = 3),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      textOutput("prediction")
    )
  )
)



# Define the Server
server <- function(input, output) {
  prediction <- eventReactive(input$predict, {
    # Simplified newdata for testing
    newdata <- data.frame(
    Age = input$age,
    Gender = as.factor(input$gender),
    `Height (cm)` = input$height,
    `Weight (kg)` = input$weight,
    `Body Shape` = as.factor(input$body_shape),
    `Favorite Color` = as.factor(input$favorite_color),
    `Style Preference` = as.factor(input$style_preference),
    `Preferred Brands` = as.factor(input$preferred_brands),
    Season = as.factor(input$season),
    Occasion = as.factor(input$occasion),
    `Budget ($)` = input$budget,
    `Past Purchase Count` = input$past_purchase,
    `Satisfaction Rating` = input$satisfaction_rating
    ) 
    # Debugging statements
    print(head(newdata))
    print(exists("dt_model"))
    
    # Temporarily commented out the prediction code for initial testing
     pred <- predict(dt_model, newdata, type = "class")
    return(pred)
  })
  
  output$prediction <- renderText({
    prediction()
  })
  print(prediction)
}




# Run the application 
shinyApp(ui = ui, server = server)
