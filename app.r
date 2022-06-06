library(dplyr)
library(ggplot2)
library(plotly)

# Load in data
heart_data_og <- read.csv(url("https://raw.githubusercontent.com/info201b-2022-spring/final-projects-bewitt/main/data/heart_2020_cleaned.csv"))
heart_data <- heart_data_og
heart_data$HeartDisease <- ifelse(heart_data$HeartDisease == "Yes", 1, 0)
categorical_data <- select(heart_data, Smoking, AlcoholDrinking, Stroke, DiffWalking, Sex, AgeCategory, Race, Diabetic, PhysicalActivity, GenHealth, Asthma, KidneyDisease, SkinCancer)
continuous_data <- select(heart_data, BMI, PhysicalHealth, MentalHealth, SleepTime)

intro_page <- tabPanel(
  "Introduction",
  titlePanel(strong("Heart Health in Adults in the United States")),
  p(em("Project Presented by Jake Flynn, Belle Witt & Jackson Kamins")),
  img(src = "image1.jpeg", height="75%", width="75%", align="center"),
  h2("Purpose of the Project"),
  p("For almost a century now, heart disease has been the leading cause of death in the United States. Living in a post
    pandemic world, all too many people have been faced with the unfortunate reality of losing a loved one. While there are still
    many mysteries associated with heart disease and prevention measures, we hoped to take on this project to learn more about the
    factors that may influence the prevalence of heart disease amongst the adult population of the United States. We aimed to use this
    project to help people better understand the factors that may put themselves or their loved ones at risk for this disease. Specifically, we
    aim to answer 1) what data our data set consists of, 2) how categorical risk factors correlate with heart disease proportion, and 3) 
    how continuous risk factor distributions correlates with the prevalence of heart disease. Overall, we hope our analysis will give 
    everyone a better understanding of the risk factors associated with this disease."),
  img(src = "image2.jpeg", height="50%", width="50%", align="center"),
  h2("About the Dataset"),
  p("This dataset was found on the Kaggle Website, and the data was sourced from the Center for Disease Control (CDC). The data set contains
    information on 319,796 adults aged 55 years and older. It contains data from adults both with and without heart disease and reports risk 
    factors such as Sex, Age, Race, and BMI, amongst others. This large amount of data allowed our project team to have a high
    level of confidence that our findings could be applied to the greater US adult population.")
)

summary_page <- tabPanel(
  "Summary",
  titlePanel(strong("Heart Health in Adults in the United States")),
  p(em("Graph 1: Race")),
  img(src = "chart1.jpeg", height="100%", width="100%", align="center"),
  p("When doing our preliminary research, we found that many sources reported that the largest number of people with heart disease in the 
    United States were white people. This data set has a white majority which allowed us to further investigate this assertion. As evidenced 
    by the Categorical Risk Factor Analysis page, the proportion of people within the specified racial groups is relatively even with some 
    variation. White people had the second highest proportion of Heart Disease amongst the various groups, with American Indian/Native Alaskan 
    having the highest proportion. However, it should be noted that the samples sizes of the non-white groups are significantly smaller, which 
    makes it more difficult for us to make causal conclusions about the entire US Adult Population with our data."),
  p(em("Graph 2: Age")),
  img(src = "chart2.jpeg", height="100%", width="100%", align="center"),
  p("During the early stages of research, we found that many publications asserted that older age groups, specifically 65 and older, were at 
    higher risk of heart disease. This data set sampled all age categories from 18-80 and allowed us to check the validity of these claims. 
    Almost in perfect proportions, as age groups got older, the proportion of heart disease increased. Thus, the claim from the literature is supported, 
    as those in older age groups have a higher risk of developing heart disease than those in younger age groups."),
  p(em("Graph 3: BMI")),
  img(src = "chart3.jpeg", height="100%", width="100%", align="center"),
  p("A common risk factor associated with heart disease is BMI.  Many publications highlight a high BMI as a leading cause of heart disease, 
    and our data set puts that claim to the test. This data set included the BMI of everyone involved in the study, and we analyzed this claim 
    by cross analyzing the BMIs of those with and without the disease. As seen in the histogram, the mean BMI of those with and without heart 
    disease are within 1 of each another. This reveals that BMI is not a great factor to look upon for one's risk of heart disease, 
    as it fails to account for those who are healthy but with above average BMI. Of course, like our past claims this comes down to a lack of 
    sample size, as those with heart disease amounted for a much smaller population than those without."),
  p(em("Overall Findings")),
  p("Overall, we concluded that some information surrounding risk factors of heart disease have possible falsehoods or inconclusive evidence. 
    For example, the claim that white people are at higher risk of heart disease is somewhat questionable, as Black and American Indian 
    populations have very similar proportions, but were studied from a much smaller sample size. Moreover, the claim that BMI is connected 
    to risk of heart disease turns out to be mostly inconclusive. Of course sampling error again exists, but the skew of the data is relatively 
    similar amongst those both with and without heart disease. However, we also found that many other factors are truly associated with higher 
    risk of heart disease. For example, in the age example, we found that there was an extremely positive correlation between older age and risk 
    of heart disease. However, this just scraped the surface of our findings, as other factors like smoking, diabetes, and kidney disease had positive 
    correlation with heart disease."),
  p(em("Potential Future Research")),
  p("A few findings in our research were quite surprising and could require some deeper research in the future. Alcohol, for example, was not 
    correlated with heart disease. This may seem surprising, as alcohol is obviously unhealthy and seen as harmful to the body in any quantity. 
    We theorized that many drink at younger ages, and thus already are at low risk of developing heart disease, so alcohol consumption does not 
    have an immediate impact on heart disease and there exists a time delay between consuming alcohol and developing heart disease. We believe 
    more research should be conducted to consider the correlation of age and alcohol consumption as well as heart disease. Other possible future 
    research opportunities include studying the development and relationship between other diseases and heart disease. We found a positive 
    correlation between every single disease examined and heart disease, however it was inconclusive whether one caused the other, so studies 
    that unravel this relationship could be beneficial in fighting and identifying risk factors of diseases.")
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
      p(em("How do continuous risk factors correlate with the prevalence of heart disease?")),
      p("Here, we explored similar distributions to the Categorical Risk Factor Analysis Page, but with 
      continuous variables, such as BMI and the number of days out of the last 30 that have been bad for 
      physical and mental health.  We wanted to explore how continuous risk factors correlate with heart disease. 
      SleepTime values were also collected and report the number of hours a participant gets on average per night of
      sleep. Group input boxes are also included to filter the charts by Sex, Race, and Age Category. Please use the 
      control widgets above to explore the continuous risk factors and gain a better understanding about the link 
      between some of these factors and the prevalence of heart disease.")
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
      p(em("How do categorical risk factors correlate with the prevalence of heart disease?")),
      p("The various graphs shown above demonstrate the interactions of categorical variables with 
      individuals who have heart disease. As opposed to the Participant Analysis portion, this page takes the 
      total population of people with heart disease and returns the proportion of people with heart disease who 
      satisfy the other specified categorical variable. As mentioned in the introduction, this data was collected 
      through a survey, therefore variables such as DiffWalking (difficulty walking), Mental Health, Physical 
      Activity, and General Health are self-reported. The variables Sex, Age, and Race were also presented as 
      options for the individual. Sliders are also included to filter the data by BMI and the number of days out of the 
      last 30 that the patient has felt bad physically and mentally. Please use the control widgets above to explore 
      the categorical risk factors and gain a better understanding about the link between some of these factors 
      and the prevalence of heart disease.")
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
      p(em("How is the data set distributed?")),
      p("This page is intended to be used to explore the various characteristics of the participants included 
      in the data set. This ranges from demographic data, such as race, as well as data on their lifestyle choices, 
      such as smoking, alcohol use, and physical health. The data set can also be filtered by adjusting the sliders 
      related to BMI and the number of days out of the last 30 that have been bad for physical and mental health. 
      Please use the control widgets above to explore more about the people in this data set and gain a better 
      understanding about the link between some of these factors and the prevalence of heart disease.")
    )
  )
)

ui <- navbarPage(
  # Pages organization
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
    # filter data based on sliders
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
    # filter data based on sliders
    filtered <- filter(heart_data, BMI <= input$slider_bmi)
    filtered <- filter(filtered, PhysicalHealth >= input$slider_physical)
    filtered <- filter(filtered, MentalHealth >= input$slider_mental)
    # group by input variable
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
    # filter data based on check boxes
    filtered_cat <- filter(heart_data_og, Sex %in% input$sex)
    filtered_cat <- filter(filtered_cat, AgeCategory %in% input$age)
    filtered_cat <- filter(filtered_cat, Race %in% input$race)
    ggplot(filtered_cat, aes(x = .data[[input$continuous]], color = HeartDisease, fill = HeartDisease)) +
      geom_histogram(bins = 40, position = "identity") +
      labs(title = paste("Participant", input$continuous, "Distribution Grouped by Heart Disease"))
  })
  
 compute_means <- function(inp) {
    # filter data based on check boxes
    filtered_cat <- filter(heart_data, Sex %in% input$sex)
    filtered_cat <- filter(filtered_cat, AgeCategory %in% input$age)
    filtered_cat <- filter(filtered_cat, Race %in% input$race)
    # group by having and not having heart disease
    grouped <- group_by(filtered_cat, HeartDisease)
    grouped <- mutate(grouped, means = mean(.data[[inp]]))
    grouped_unique <- unique(select(grouped, means))
  }
  
  output$means_wohd <- renderText({
    # compute the mean for patients without heart disease
    paste0("The mean ", input$continuous, " for patients without heart disease is ", round(compute_means(input$continuous)$means[1], digits = 2), ".")
  })
  
  output$means_hd <- renderText({
    # compute the mean for patients with heart disease
    paste0("The mean ", input$continuous, " for patients with heart disease is ", round(compute_means(input$continuous)$means[2], digits = 2), ".")
  })
}

# Run the application
shinyApp(ui = ui, server = server)