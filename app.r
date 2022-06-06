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
  img(src = "image1.jpeg", height="50%", width="50%", align="center"),
  h2("Purpose of the Project"),
  p("For almost a century now, Heart Disease has been the leading cause of death in the United States. Living in a post
    pandemic world, all too many people have been faced with the unfortunate reality of losing a loved one. While there are still
    many mysteries associated with heart disease and prevention measures, we hoped to take on this porject to learn more about the
    factors that may influence the prevalence of Heart Disease amongst the adult population of the United States. We aimed to use this
    project to help people better understand the factors that may put themseleves or their loved ones at risk for this disease. Two of the
    factors we were orginally most interested in seeing the effects of were sex and age, as well as the intersection of the two. Overall,
    we hope our analysis will give everyone a better understanding the risk fatcors associated with this disease."),
  img(src = "image2.jpeg", height="50%", width="50%", align="center"),
  h2("About the Dataset"),
  p("This dataset was found on the Kaggle Website, and the data was sourced from the Center for Disease Control (CDC). The dataset contains
    information on 319,796 adults aged 55 years and older. It contains data from adults both with and without heart disease and reports
    factors such as Sex, Age, Race, and BMI, amongst others. This large amount of data allowed our project team to have a high
    level of confidence that our findings could be applied to the greater US adult population.")
)

summary_page <- tabPanel(
  "Summary",
  titlePanel("Heart Health Data 2020"),
  p("This website lets you explore Heart Health..."),
  img(src = "chart1.jpeg", height="50%", width="50%", align="center"),
  img(src = "chart2.jpeg", height="50%", width="50%", align="center"),
  img(src = "chart3.jpeg", height="50%", width="50%", align="center")
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
      ),
      checkboxGroupInput(
        inputId = "sex",
        label = "Select a Sex",
        choices = unique(heart_data$Sex),
        selected = unique(heart_data$Sex)
      ),
      checkboxGroupInput(
        inputId = "race",
        label = "Select a Race",
        choices = unique(heart_data$Race),
        selected = unique(heart_data$Race)
      ),
      checkboxGroupInput(
        inputId = "age",
        label = "Select an Age Category",
        choices = unique(heart_data$AgeCategory),
        selected = unique(heart_data$AgeCategory)
      )
    ),
    mainPanel(
      plotOutput(outputId ="hist"),
      verbatimTextOutput(outputId = "means_wohd"),
      verbatimTextOutput(outputId = "means_hd"),
      p("Here, we explored similar distributions to the Categorical Risk Factor Analysis Page, but with 
      continuous variables. We wanted to explore these factors and how to Heart Disease. These variables 
      were also self-reported by using a much larger and quantitative scale. BMI represents an individual's 
      Body Mass Index, which is a popular tool used in health diagnostics. The values from Physical Health 
      come from the survey asking, “Now thinking about your physical health, which includes physical illness 
      and injury, for how many days during the past 30 days did you feel like your health was no good?” and 
      participants were asked to report this value in days. The same question was asked to obtain the values 
      for Mental Health, replacing the word physical with mental. SleepTime values were collected through 
      asking “On average, how many hours of sleep do you get in a 24-hour period?” and participants were 
      asked to report their answer in hours.")
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
      ),
      sliderInput(
        inputId = "slider_bmi",
        label = "Select BMI Range",
        min = min(heart_data$BMI),
        max = max(heart_data$BMI),
        value = max(heart_data$BMI)
      ),
      sliderInput(
        inputId = "slider_physical",
        label = "Select Physical Health Range",
        min = min(heart_data$PhysicalHealth),
        max = max(heart_data$PhysicalHealth),
        value = min(heart_data$PhysicalHealth)
      ),
      sliderInput(
        inputId = "slider_mental",
        label = "Select Mental Health Range",
        min = min(heart_data$MentalHealth),
        max = max(heart_data$MentalHealth),
        value = min(heart_data$MentalHealth)
      )
    ),
    mainPanel(
      plotOutput(outputId = "blank"),
      p("The various graphs shown to the right demonstrate the interactions of these categorical variables with 
      individuals who have Heart Disease. As opposed to the Participant Analysis portion, this page takes the 
      total population of people with Heart Disease and returns the proportion of people with Heart Disease who 
      satisfy the other specified categorical variable. As mentioned in the introduction, this data was collected 
      through a survey, therefore variables such as DiffWalking (difficulty walking), Mental Health, Physical 
      Activity, and General Health are self-reported. The variables Sex, Age, and Race were all presented as 
      options for the individual. The rest of the variables that pertain to health conditions are reported as 
      yes or no questions, with the exception of Diabetes which was presented as a scale, where yes means they 
      have been diagnosed with the ailment and no means they have not.")
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
      ),
      sliderInput(
        inputId = "slider_bmi2",
        label = "Select BMI Range",
        min = min(heart_data$BMI),
        max = max(heart_data$BMI),
        value = max(heart_data$BMI)
      ),
      sliderInput(
        inputId = "slider_physical2",
        label = "Select Physical Health Range",
        min = min(heart_data$PhysicalHealth),
        max = max(heart_data$PhysicalHealth),
        value = min(heart_data$PhysicalHealth)
      ),
      sliderInput(
        inputId = "slider_mental2",
        label = "Select Mental Health Range",
        min = min(heart_data$MentalHealth),
        max = max(heart_data$MentalHealth),
        value = min(heart_data$MentalHealth)
      )
    ),
    mainPanel(
      plotOutput(outputId = "bar"),
      p("This page is intended to be used to explore the various characteristics of the participants included 
      in the data set. This ranges from demographic data, such as race, as well as data on their lifestyle choices, 
      such as smoking, alcohol use, and physical health. Please use the control widget above to explore more 
      about the people in this data set and gain a better understanding about the link between some of these 
      factors and the prevalence of Heart Disease.")
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
    filtered <- filter(heart_data, BMI <= input$slider_bmi2)
    filtered <- filter(filtered, PhysicalHealth >= input$slider_physical2)
    filtered <- filter(filtered, MentalHealth >= input$slider_mental2)
    bar_df <- select(filtered, var_name)
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
    filtered <- filter(heart_data, BMI <= input$slider_bmi)
    filtered <- filter(filtered, PhysicalHealth >= input$slider_physical)
    filtered <- filter(filtered, MentalHealth >= input$slider_mental)
    grouped_df <- group_by(filtered, .data[[var_name]])
    grouped_df <- summarize(grouped_df, heart_disease_prop = format(sum(.data$HeartDisease) / length(.data$HeartDisease), digits = 2))
  }
  
  output$blank <- renderPlot({
    ggplot(make_grouped_df(input$variable), aes(x = make_grouped_df(input$variable)[[as.name(input$variable)]], y = heart_disease_prop, fill = make_grouped_df(input$variable)[[as.name(input$variable)]])) +
      geom_bar(stat = "identity") +
      labs(x = input$variable,
           y = "Proportion of Heart Disease",
           title = paste("Proportion of Heart Disease Grouped by", input$variable),
           fill = input$variable) +
      geom_text(
        aes(label = heart_disease_prop, vjust = -1)
      )
  })
  
  output$hist <- renderPlot({
    filtered_cat <- filter(heart_data_og, Sex %in% input$sex)
    filtered_cat <- filter(filtered_cat, AgeCategory %in% input$age)
    filtered_cat <- filter(filtered_cat, Race %in% input$race)
    ggplot(filtered_cat, aes(x = .data[[input$continuous]], color = HeartDisease, fill = HeartDisease)) +
      geom_histogram(bins = 40, position = "identity") +
      labs(title = paste("Participant", input$continuous, "Distribution Grouped by Heart Disease"))
  })
  
 compute_means <- function(inp) {
    filtered_cat <- filter(heart_data, Sex %in% input$sex)
    filtered_cat <- filter(filtered_cat, AgeCategory %in% input$age)
    filtered_cat <- filter(filtered_cat, Race %in% input$race)
    grouped <- group_by(filtered_cat, HeartDisease)
    grouped <- mutate(grouped, means = mean(.data[[inp]]))
    grouped_unique <- unique(select(grouped, means))
  }
  
  output$means_wohd <- renderText({
    paste0("The mean ", input$continuous, " for patients without heart disease is ", round(compute_means(input$continuous)$means[1], digits = 2), ".")
  })
  
  output$means_hd <- renderText({
    paste0("The mean ", input$continuous, " for patients with heart disease is ", round(compute_means(input$continuous)$means[2], digits = 2), ".")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)