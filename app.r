library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

#load in data
heart_data <- read.csv(url("https://raw.githubusercontent.com/info201b-2022-spring/final-projects-bewitt/main/data/heart_2020_cleaned.csv"))
categorical_data <- select(heart_data, HeartDisease, Smoking, AlcoholDrinking, Stroke, DiffWalking, Sex, AgeCategory, Race, Diabetic, PhysicalActivity, GenHealth, Asthma, KidneyDisease, SkinCancer)
summary_page <- tabPanel(
  "Summary",
  titlePanel("Heart Health Data 2020"),
  p("This website lets you explore Heart Health...")
)

cat_analysis_page <- tabPanel(
  "Participant Analysis",
  sidebarLayout(
    sidebarPanel(
      h3("Control Panel"),
      selectInput(
        inputId = "var",
        label = "Select a variable",
        choices = colnames(categorical_data)
      )
    ),
    mainPanel(
      plotOutput(outputId = "bar")
    )
  )
)

ui <- navbarPage(
  # put stuff here
  title = "Heart Health",
  summary_page,
  cat_analysis_page
)

# Define server logic
server <- function(input, output) {
  
  make_bar_df <- function(var_name) {
    bar_df <- select(categorical_data, var_name)
  }
  
  output$bar <- renderPlot({
    ggplot(make_bar_df(input$var), aes(x = (make_bar_df(input$var)[[as.name(input$var)]]))) +
      geom_bar(fill = "indianred3",
               color = "black") +
      labs(x = input$var,
           y = "Number of Participants",
           title = paste("Participants by", input$var))
  })

}

# Run the application
shinyApp(ui = ui, server = server)