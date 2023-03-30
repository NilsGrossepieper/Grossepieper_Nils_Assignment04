# Build a dashboard that allows users to upload an image and receive a
# classification result. The dashboard should be built using the {shiny} package
# in R and the {Dash} package in Python. You can assume that only .jpg images
# will be uploaded.


# Build the app:
library(shiny)
library(tidyverse)
library(OpenImageR)
library(MASS)
library(tidymodels)
library(kknn)
library(randomForest)

# Set up the user interface: 
ui <- fluidPage(
  fluidRow(
    headerPanel("Pizza or Not Pizza that is the question:")
  ),
  fluidRow(
    fileInput("image", "Drag and Drop or Select Files",
              multiple = FALSE,
              accept = c("image/png",
                         "image/jpeg"))),
  fluidRow(
    plotOutput("image")),
  fluidRow(
    verbatimTextOutput("result_end")
  )
)

# Set up the server: 
server <- function(input, output){
  output$image <- renderImage({
    list(src = input$image$datapath,
         width = "50%", height = "50%",
         alt = "Image failed to render")
  }, deleteFile = FALSE)
  
  result <- reactive({
    pipeline <- function(img) {
      img %>%
        resizeImage(200, 200, method = "bilinear") %>%
        rgb_2gray() %>%
        HOG(cells = 10, orientations = 9)
    }
    
    # get the image
    image <- readImage(input$image$datapath)
    # preprocess the image
    preprocessed <- pipeline(image)
    # stack the list in a matrix
    preprocessed_matrix <- matrix(unlist(preprocessed), ncol = 900)
    # turn into format for tidymodels
    preprocessed_df <- preprocessed_matrix %>% as_tibble()
    # add blanks for labels
    preprocessed_df <- bind_cols(NA, preprocessed_df)
    # add the correct column names
    names(preprocessed_df) <- paste0("V", 1:901)
    # load the model
    clf_rf <- readRDS("C:/Users/gross/OneDrive/Dokumente/Grossepieper_Nils_Assignment04/clf_rf_r.rds")
    # make predictions
    result_1 <- clf_rf %>% predict(preprocessed_df) %>% pull()
    return(result_1)
  })
  output$result_end <- renderPrint({
    result()
  })
}

# Run the app locally: 
shinyApp(ui = ui, server = server)