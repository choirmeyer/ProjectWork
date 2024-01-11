# NOT YET RELEASED IN GITHUB. DO SO BEFORE SUBMISSION
# OR EARLIER TO EXPLORE AND LEARN MORE

# 742 Katie Meyer - Analysis/Comments
# Code, initial short comments provided by Idina Vadapally

# Planning:         PDF explpaining project
# Data Collection:  300 records generated to mimic users
# Data Preparation: Reviewed data
#                   No records with missing fields
#                   Looked like the data could be categorized as the data was labelled
#                   Further EDA is performed within code below.
#
# Hypothisis Question: How can machine language be used to predict what a user's would prefer to wear?

# Install necessary packages
install.packages("rpart") 
 # CRAN - Classification, regression/, and survival trees 
   # CRAN - Comprehensive R Archie Network (https//r-project.org --> doc --> FAQ --> R-FAQ)
 # Decision Tree: ML algo can do classification & regression tasks
   # Classification Tree: (not used: regression tree, survival tree)
     # Braches rep attributes, leaves are decisions.
     # Process goes from the trunk through the (decision tree), until
     # it reaches an end (leaf)  
       
install.packages("dplyr") # Grammer to data changes,so analysis can occur statistically. Required for ml algorithms
install.packages("caret") # Machine Learning in R - many models
install.packages("shiny") # Open Source R package - framework to build web apps in R

# Load the packages that were just loaded from the libraries installed
library(rpart)
library(dplyr)
library(caret)
library(shiny)
# Continue to load other packages as needed
  # The screen did have an error, and I was hoping
  # it wasn't a single click to install it
  # I was expecting a window to pop up and confirm, so could be noted.
  # This did not occur, but I think there were a couple of adds.

# Read the data into the program
#  I created a free repository in GitHub
#    This was an exercise in learning to do so.
#    There are many resouces within GitHub to learn
data <- read.csv("https://github.com/choirmeyer/ProjectWork/releases/tag/Stylist/personalstylist2.1.R")


# EDA - exploratory data analysis - data preparation for modelling
# Data Preprocessing, Feature Enginnering
# Prepare data for visual inspection
# Remove User ID as it's not a useful feature
  # It was the unique ID used to identify each record
  # User ID has no relation to the result being sought 
data$User.ID <- NULL

# Convert categorical variables to factors
  # Categorial variables: dataset with limited values
  # Factor: How a categorical variable is stored in R as an integer
    # Integers are needed for the machine learning algorithm
  # The code below takes the data from the (columns of) categories below and stores them as the factor categorical_cols:
categorical_cols <- c("Gender", "Body.Shape", "Favorite.Color", "Style.Preference", "Preferred.Brands", "Season", "Occasion")
data[categorical_cols] <- lapply(data[categorical_cols], factor)

# Check for class imbalance - Look at the ata distribution, and what it says
  # Visualize class distribution/arrangement - partition data into training and test datasets
  # RUN AND PROVIDE INSIGHT ON RESULT HERE!
class_distribution <- table(data$ItemReturned)
print(class_distribution)

# Split the data into training and test sets
  # initialize a 'random' number generator
  # This way when the data is split, the ratio remains, but what data is in each set varries for reproducability
  # Define how the dataset is split into 80% test, 20% train
  # Create the variables and assign the data to them.
set.seed(123)
trainIndex <- createDataPartition(data$ItemReturned, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train the Model - Decision Tree
dt_model <- rpart(ItemReturned ~ ., data = trainData, method = "class",
                  cp = 0.01,          # Complexity parameter
                  minsplit = 100,      # Minimum number of observations required to attempt a split
                  maxdepth = 10)       # Maximum depth of any node of the final tree

# Print the model summary
# DESCRIBE WHAT IS SEEN AND WHAT IT REPRESENTS
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




# Run the application - straigt through
shinyApp(ui = ui, server = server)
