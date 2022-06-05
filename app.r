library(dplyr)
library(ggplot2)
library(plotly)

# Load in data
heart_data_og <- read.csv(url("https://raw.githubusercontent.com/info201b-2022-spring/final-projects-bewitt/main/data/heart_2020_cleaned.csv"))
heart_data <- heart_data_og
heart_data$HeartDisease<-ifelse(heart_data$HeartDisease=="Yes", 1, 0)
categorical_data <- select(heart_data, Smoking, AlcoholDrinking, Stroke, DiffWalking, Sex, AgeCategory, Race, Diabetic, PhysicalActivity, GenHealth, Asthma, KidneyDisease, SkinCancer)
continuous_data <- select(heart_data, BMI, PhysicalHealth, MentalHealth, SleepTime)

intro_page <- tabPanel(
  "Introduction",
  titlePanel(strong("Heart Health in Adults in the United States")),
  p(em("Project Presented by Jake Flynn, Belle Witt & Jackson Kamins")),
  img("Heart Image One", src = "image1.jpeg", height="50%", width="50%", align="center"),
  h2("Purpose of the Project"),
  p("For almost a century now, Heart Disease has been the leading cause of death in the United States. Living in a post
    pandemic world, all too many people have been faced with the unfortunate reality of losing a loved one. While there are still
    many mysteries associated with heart disease and prevention measures, we hoped to take on this porject to learn more about the
    factors that may influence the prevalence of Heart Disease amongst the adult population of the United States. We aimed to use this
    project to help people better understand the factors that may put themseleves or their loved ones at risk for this disease. Two of the
    factors we were orginally most interested in seeing the effects of were sex and age, as well as the intersection of the two. Overall,
    we hope our analysis will give everyone a better understanding the risk fatcors associated with this disease."),
  img("Heart Image Two", src = "image2.jpeg", height="50%", width="50%", align="center"),
  h2("About the Dataset"),
  p("This dataset was found on the Kaggle Website, and the data was sourced from the Center for Disease Control (CDC). The dataset contains
    information on 319,796 adults aged 55 years and older. It contains data from adults both with and without heart disease and reports
    factors such as Sex, Age, Race, and BMI, amongst others. This large amount of data allowed our project team to have a high
    level of confidence that our findings could be applied to the greater US adult population. ")
)

summary_page <- tabPanel(
  "Summary",
  titlePanel("Heart Health Data 2020"),
  p("This website lets you explore Heart Health...")
)

cont_analysis_page <- tabPanel(
  "Continuous Risk Factor Analysis",
  sidebarLayout(
    sidebarPanel(
      h3("Control Panel"),
      selectInput(
        inputId = "continuous",
        label = "Select a variable",
        choices = colnames(continuous_data)
      )
    ),
    mainPanel(
      plotOutput(outputId ="hist"),
      verbatimTextOutput(outputId = "testing")
    )
  )
)

rf_analysis_page <- tabPanel(
  "Categorical Risk Factor Analysis",
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
      plotOutput(outputId = "blank")
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
  intro_page,
  cat_analysis_page,
  rf_analysis_page,
  cont_analysis_page,
  summary_page
)

# Define server logic
server <- function(input, output) {
  
  make_bar_df <- function(var_name) {
    bar_df <- select(categorical_data, var_name)
  }
  
  output$bar <- renderPlot({
    ggplot(make_bar_df(input$var), aes(x = make_bar_df(input$var)[[as.name(input$var)]], fill = make_bar_df(input$var)[[as.name(input$var)]])) +
      geom_bar() +
      labs(x = input$var,
           y = "Number of Participants",
           title = paste("Participants by", input$var),
           fill = input$var) +
      geom_text(aes(label = ..count..), stat = "count", vjust = -0.5)
  })

  make_grouped_df <- function(var_name) {
    grouped_df <- group_by(heart_data, .data[[var_name]])
    grouped_df <- summarize(grouped_df, heart_disease_prop = format(sum(.data$HeartDisease) / length(.data$HeartDisease), digits = 2))
  }
  
  output$blank <- renderPlot({
    ggplot(make_grouped_df(input$variable), aes(x = make_grouped_df(input$variable)[[as.name(input$variable)]], y = heart_disease_prop, fill = make_grouped_df(input$variable)[[as.name(input$variable)]])) +
      geom_bar(stat = "identity") +
      labs(x = input$variable,
           y = "Proportion",
           title = paste("Proportion of Heart Disease by", input$variable),
           fill = input$variable) +
      geom_text(
        aes(label = heart_disease_prop, vjust = -1)
      )
  })
  
  output$hist <- renderPlot({
    ggplot(heart_data_og, aes(x = .data[[input$continuous]], color = HeartDisease, fill = HeartDisease)) +
      geom_histogram(bins = 40, position = "identity")
  })
  
  # output$testing <- renderText({
    # paste0(heart_data$HeartDisease)
  # })
  
}

# Run the application
shinyApp(ui = ui, server = server)