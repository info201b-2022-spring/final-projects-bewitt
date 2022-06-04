library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Load in data
heart_data <- read.csv(url("https://raw.githubusercontent.com/info201b-2022-spring/final-projects-bewitt/main/data/heart_2020_cleaned.csv"))
heart_data$HeartDisease<-ifelse(heart_data$HeartDisease=="Yes", 1, 0)
categorical_data <- select(heart_data, Smoking, AlcoholDrinking, Stroke, DiffWalking, Sex, AgeCategory, Race, Diabetic, PhysicalActivity, GenHealth, Asthma, KidneyDisease, SkinCancer)
summary_page <- tabPanel(
  "Summary",
  titlePanel("Heart Health Data 2020"),
  p("This website lets you explore Heart Health...")
)

rf_analysis_page <- tabPanel(
  "Risk Factor Analysis",
  sidebarLayout(
    sidebarPanel(
      h3("Control Panel"),
      selectInput(
        inputId = "variable",
        label = "Select a variable",
        choices = colnames(categorical_data)
      )
    ),
    mainPanel(
      plotOutput(outputId = "blank"),
      verbatimTextOutput(outputId = "test")
    )
  )
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
  # Put stuff here
  title = "Heart Health",
  summary_page,
  cat_analysis_page,
  rf_analysis_page
)

# Define server logic
server <- function(input, output) {
  
  make_bar_df <- function(var_name) {
    bar_df <- select(categorical_data, var_name)
  }
  
  output$bar <- renderPlot({
    ggplot(make_bar_df(input$var), aes(x = make_bar_df(input$var)[[as.name(input$var)]])) +
      geom_bar(fill = "indianred3",
               color = "black") +
      labs(x = input$var,
           y = "Number of Participants",
           title = paste("Participants by", input$var))
  })

  make_grouped_df <- function(var_name) {
    grouped_df <- group_by(heart_data, .data[[var_name]])
    grouped_df <- summarize(grouped_df, heart_disease_prop = format(sum(.data$HeartDisease) / length(.data$HeartDisease), digits = 2))
  }
  
  output$blank <- renderPlot({
    ggplot(make_grouped_df(input$variable), aes(x = make_grouped_df(input$variable)[[as.name(input$variable)]], y = heart_disease_prop)) +
      geom_bar(stat = "identity",
               fill = "indianred3",
               color = "black") +
      labs(x = input$variable,
           y = "Proportion",
           title = paste("Proportion of Heart Disease by", input$variable))
  })
}

# Run the application
shinyApp(ui = ui, server = server)