library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(reshape2)

data <- read.csv("C:\\Users\\User\\OneDrive - ump.edu.my\\Degree\\DSP II\\Project\\Health_dataset.csv", header = TRUE)

# Checking for columns with null values
na_values <- mean(is.na(data)) # Ensure 'data' is the correct data frame
na_values

# Removing duplicates
data <- data %>% distinct()

# Displaying the structure of the data frame
str(data)
# Removing the ID variable 
# It is merely a repetition of the indexing column, now also with incorrect values after the drop of duplicates.
data <- data[-1]

# Normalizing the 'BMI_Category' data
# There seems to be a redundancy with 'Normal' and 'Normal Weight' in BMI Category
data$BMI_Category <- as.character(data$BMI_Category)
data$BMI_Category <- trimws(data$BMI_Category)
data$BMI_Category[data$BMI_Category == 'Normal '] <- 'Normal'
data$BMI_Category <- factor(data$BMI_Category)

# Factoring categorical variables
data$Gender <- as.factor(data$Gender)
data$Occupation<-as.factor(data$Occupation)
data$BMI_Category<- factor(data$BMI_Category)
data$Sleep_Disorder<- factor(data$Sleep_Disorder)
levels(data$Sleep_Disorder) <- make.names(levels(data$Sleep_Disorder))

# Converting Blood Pressure to numeric
data$Blood_Pressure_Systolic <- as.numeric(data$Blood_Pressure_Systolic)
data$Blood_Pressure_Diastolic <- as.numeric(data$Blood_Pressure_Diastolic)
# Blood Pressure values can be classified based on 'Systolic' and 'Diastolic' values
data <- data %>%
  mutate(Blood_Pressure_Category = case_when(
    Blood_Pressure_Systolic < 120 & Blood_Pressure_Diastolic < 80 ~ 'Optimal',
    Blood_Pressure_Systolic >= 120 & Blood_Pressure_Systolic < 140 & Blood_Pressure_Diastolic < 90 ~ 'Normal',
    Blood_Pressure_Systolic >= 140 & Blood_Pressure_Diastolic >= 90 | Blood_Pressure_Diastolic >= 80 ~ 'Hypertension',
    TRUE ~ 'Undefined' # default case
  ))

sampled_data <- data %>% sample_n(5)
sampled_data

# Let's check the values in the Occupations column
occupation_counts_before <- table(data$Occupation)
occupation_counts_before 

# Merging similar Occupations
data$Occupation <- recode(data$Occupation,
                          'Sales Representative' = 'Salesperson',
                          'Software Engineer' = 'Engineer')

# Convert Occupation to character before filtering
data$Occupation <- as.character(data$Occupation)

# Deleting the Rows with Scientist and Manager as Occupation
data <- data %>% 
  filter(!(Occupation %in% c('Manager', 'Scientist')))

# Convert Occupation back to factor
data$Occupation <- factor(data$Occupation)

# Checking the values in the Occupations column after the changes
occupation_counts_after <- table(data$Occupation)
occupation_counts_after

# Define custom colors for BMI categories
bmi_colors <- c("Obese" = "firebrick3", "Normal" = "skyblue4", "Normal " = "skyblue4", "Overweight" = "violetred3")

# Calculate statistics for info boxes
max_stress <- max(data$Stress_Level, na.rm = TRUE)
min_stress <- min(data$Stress_Level, na.rm = TRUE)
avg_stress <- mean(data$Stress_Level, na.rm = TRUE)


# Define UI
# Define UI
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "SleepHealth"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Overview", tabName = "overall", icon = icon("list")),
      menuItem("Dataset Review", tabName = "details", icon = icon("info-circle")),
      menuItem("Data Distribution", tabName = "distribution"),
      menuItem("Dashboard 1", tabName = "bar_charts", icon = icon("bar-chart")),
      menuItem("Dashboard 2", tabName = "dashboard1", icon = icon("chart-bar")),
      menuItem("Dashboard 3", tabName = "dashboard3", icon = icon("chart-bar")),
      menuItem("Dashboard 4", tabName = "dashboard4", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        box(
          title = "Welcome to Sleep Health and Lifestyle Analysis",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          "This dashboard provides insights into sleep health and lifestyle factors based on various data points. Navigate through the tabs to explore different aspects of the dataset."
        ),
        fluidRow(
          box(
            title = "Sleep and Health",
            status = "info",
            solidHeader = TRUE,
            width = 10,
            infoBox("Sleep Disorders", length(unique(data$Sleep_Disorder)), icon = icon("bed")),
            infoBox("BMI Categories", length(unique(data$BMI_Category)), icon = icon("heartbeat")),
            infoBox("Blood Pressure", length(unique(data$Blood_Pressure_Category)), icon = icon("heartbeat"))
          ),
          box(
            title = "About the Dataset",
            status = "info",
            solidHeader = TRUE,
            width = 2,
            "The dataset contains information about various factors affecting sleep health"
          ),
          box(
            title = "Lifestyle",
            status = "info",
            solidHeader = TRUE,
            width = 10,
            infoBox("Physical Activity Levels", length(unique(data$Physical_Activity_Level)), icon = icon("running")),
            infoBox("Stress Levels", length(unique(data$Stress_Level)), icon = icon("thermometer-half")),
            infoBox("Daily Steps", length(unique(data$Daily_Steps)), icon = icon("shoe-prints"))
          )
        )
      ),
      tabItem(
        tabName = "overall",
        fluidRow(
          column(3, selectInput("Gender", "Gender:", c("All", unique(as.character(data$Gender))))),
          column(3, selectInput("Age", "Age:", c("All", unique(as.character(data$Age))))),
          column(3, selectInput("Occupation", "Occupation:", c("All", unique(as.character(data$Occupation))))),
          column(3, selectInput("SleepDuration", "Sleep Duration:", c("All", unique(as.character(data$SleepDuration))))),
          column(3, selectInput("QualityOfSleep", "Quality of Sleep:", c("All", unique(as.character(data$QualityOfSleep))))),
          column(3, selectInput("PhysicalActivityLevels", "Physical Activity Levels:", c("All", unique(as.character(data$PhysicalActivityLevels))))),
          column(3, selectInput("StressLevel", "Stress Level:", c("All", unique(as.character(data$StressLevel))))),
          column(3, selectInput("BMICategory", "BMI Category:", c("All", unique(as.character(data$BMICategory))))),
          column(3, selectInput("HeartRate", "Heart Rate:", c("All", unique(as.character(data$HeartRate))))),
          column(3, selectInput("DailySteps", "Daily Steps:", c("All", unique(as.character(data$DailySteps))))),
          column(3, selectInput("SleepDisorders", "Sleep Disorders:", c("All", unique(as.character(data$SleepDisorders)))))
        ),
        dataTableOutput("select_overall")
      ),
      tabItem(
        tabName = "details",
        h2("Details about our dataset"),
        fluidRow(
          column(4,
                 style = "padding: 10px;",
                 box(
                   title = "Gender",
                   "The gender of the person (Male/Female).",
                   background = "yellow", width = 12
                 ),
                 box(
                   title = "Age",
                   "The age of the person in years.",
                   background = "aqua", width = 12
                 ),
                 box(
                   title = "Occupation",
                   "The occupation or profession of the person.",
                   background = "blue", width = 12
                 ),
                 box(
                   title = "Details about Sleep Disorder",
                   HTML("None: The individual does not exhibit any specific sleep disorder.<br>
                        Insomnia: The individual experiences difficulty falling asleep or staying asleep, leading to inadequate or poor-quality sleep.<br>
                        Sleep Apnea: The individual suffers from pauses in breathing during sleep, resulting in disrupted sleep patterns and potential health risks."),
                   background = "maroon", width = 12
                 )
          ),
          column(4,
                 style = "padding: 10px;",
                 box(
                   title = "Sleep Duration (hours)",
                   "The number of hours the person sleeps per day.",
                   background = "light-blue", width = 12
                 ),
                 box(
                   title = "Quality of Sleep",
                   "A subjective rating of the quality of sleep, ranging from 1 to 10 (1 = Very Poor Sleep Quality and 10 = Excellent Sleep Quality)",
                   background = "green", width = 12
                 ),
                 box(
                   title = "Physical Activity Level",
                   "The number of minutes the person engages in physical activity daily.",
                   background = "navy", width = 12
                 ),
                 box(
                   title = "Stress Level",
                   "A subjective rating of the stress level experienced by the person, ranging from 1 to 10 (1 = Very Low Stress Level and 10 = Very High Stress Level)",
                   background = "teal", width = 12
                 )
          ),
          column(4,
                 style = "padding: 10px;",
                 box(
                   title = "BMI Category",
                   "The BMI category of the person (e.g., Underweight, Normal, Overweight).",
                   background = "olive", width = 12
                 ),
                 box(
                   title = "Blood Pressure Category",
                   "The blood pressure category of the person (Normal, Hypertension, Optimal).",
                   background = "lime", width = 12
                 ),
                 box(
                   title = "Daily Steps",
                   "The number of steps the person takes per day.",
                   background = "fuchsia", width = 12
                 ),
                 box(
                   title = "Sleep Disorder",
                   "The presence or absence of a sleep disorder in the person (None, Insomnia, Sleep Apnea).",
                   background = "purple", width = 12
                 )
          )
        )
      ),
      tabItem(
        tabName = "distribution",
        fluidRow(
          column(6, plotOutput("ageHistogram")),
          column(6, plotOutput("occupationBarplot")),
          column(6, plotOutput("Stress_LevelBarplot")),
          column(6, plotOutput("Blood_Pressure_CategoryBarplot"))
        )
      ),
      tabItem(
        tabName = "bar_charts",
        h2("Bar Charts"),
        fluidRow(
          column(8,
                 plotOutput("barChart1"),
                 plotOutput("barChart2")
          ),
          column(4,
                 radioButtons("bmi_category_1", "Select BMI Category for Quality of Sleep by Sleep Disorder:",
                              choices = unique(data$BMI_Category),
                              selected = unique(data$BMI_Category)[1]),
                 radioButtons("bmi_category_2", "Select BMI Category for Stress Level by Sleep Disorder:",
                              choices = unique(data$BMI_Category),
                              selected = unique(data$BMI_Category)[1])
          )
        )
      ),
      tabItem(
        tabName = "dashboard1",
        fluidRow(
          box(
            plotlyOutput("sleep_donut_chart"),
            width = 5,
            title = "Sleep Disorder by Gender"
          ),
          box(
            plotOutput("occupation_pie_chart"),
            width = 5,
            title = "Distribution of Occupation"
          ),
          column(
            width = 2,
            box(
              h4("Sleep Disorder By Gender:"),
              radioButtons("display_choice", "Choose Display Type:", choices = c("Count", "Percentage")),
              radioButtons("gender", "Select gender", choices = c("Male", "Female")),
              width = NULL
            )
          ),
          box(
            plotOutput("occupation_bar_chart"),
            width = 12,
            title = "Average Stress Levels by Occupation"
          ),
          infoBox("Max Stress Level", max_stress, icon = icon("arrow-up"), color = "green"),
          infoBox("Avg Stress Level", round(avg_stress, 2), icon = icon("arrows-alt-h"), color = "blue"),
          infoBox("Min Stress Level", min_stress, icon = icon("arrow-down"), color = "red")
        )
      ),
      tabItem(
        tabName = "dashboard3",
        tabBox(id = "t1", width = 12,
               tabPanel("Boxplot", 
                        h2("Daily Steps by BMI Category"), 
                        plotlyOutput("plot_boxplot")
               ),
               tabPanel("Scatter Plot", 
                        h2("Stress Level vs Quality of Sleep"), 
                        plotlyOutput("plot_scatterplot")
               ),
               tabPanel("Bar Plot", 
                        h2("Average Physical Activity Level by Sleep Disorder"), 
                        plotlyOutput("plot_barplot")
               ),
               tabPanel("Heatmap", 
                        h2("Correlation"), 
                        plotOutput("plot_heatmap")
               )
        )
      ),
      tabItem(
        tabName = "dashboard4",
        fluidRow(
          column(12,
                 selectInput("variable", "Choose a Variable:",
                             choices = c("Quality_of_Sleep", "Stress_Level", "BMI_Category"))),
          column(12, plotOutput("stackedPlot")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$select_overall <- renderDataTable({
    filtered_data <- sleep_data
    
    if (input$Gender != "All") {
      filtered_data <- filtered_data[filtered_data$gender == input$Gender,]
    }
    if (input$Age != "All") {
      filtered_data <- filtered_data[filtered_data$age == input$Age,]
    }
    if (input$Occupation != "All") {
      filtered_data <- filtered_data[filtered_data$occupation == input$Occupation,]
    }
    if (input$SleepDuration != "All") {
      filtered_data <- filtered_data[filtered_data$sleep_duration == input$SleepDuration,]
    }
    if (input$QualityOfSleep != "All") {
      filtered_data <- filtered_data[filtered_data$quality_of_sleep == input$QualityOfSleep,]
    }
    if (input$PhysicalActivityLevels != "All") {
      filtered_data <- filtered_data[filtered_data$physical_activity_levels == input$PhysicalActivityLevels,]
    }
    if (input$StressLevel != "All") {
      filtered_data <- filtered_data[filtered_data$stress_level == input$StressLevel,]
    }
    if (input$BMICategory != "All") {
      filtered_data <- filtered_data[filtered_data$BMI_category == input$BMICategory,]
    }
    if (input$HeartRate != "All") {
      filtered_data <- filtered_data[filtered_data$heart_rate == input$HeartRate,]
    }
    if (input$DailySteps != "All") {
      filtered_data <- filtered_data[filtered_data$daily_steps == input$DailySteps,]
    }
    if (input$SleepDisorders != "All") {
      filtered_data <- filtered_data[filtered_data$sleep_disorders == input$SleepDisorders,]
    }
    
    filtered_data
  })
  
  # Age distribution
  output$ageHistogram <- renderPlot({
    # Plot histogram for age distribution
    hist(data$Age, main = "Age Distribution", xlab = "Age", ylab = "Frequency", col = "#E9C6EF")
  })
  
  # Occupation distribution
  output$occupationBarplot <- renderPlot({
    # Plot bar plot for occupation distribution
    barplot(table(data$Occupation), main = "Occupation Distribution", xlab = "Occupation", ylab = "Frequency", col = "#CD96CD")
  })
  
  # Stress Level distribution
  output$Stress_LevelBarplot <- renderPlot({
    # Plot bar plot for Stress Level distribution
    barplot(table(data$Stress_Level), main = "Stress Level Distribution", xlab = "Stress Level", ylab = "Frequency", col = "#8B4789")
  })
  # Blood Pressure Category distribution
  output$Blood_Pressure_CategoryBarplot <- renderPlot({
    # Plot bar plot for Blood Pressure Category distribution
    barplot(table(data$Blood_Pressure_Category), main = "Blood Pressure Category Distribution", xlab = "Blood Pressure", ylab = "Frequency", col = "#784199")
  })
  
  #Bar chart
  output$barChart1 <- renderPlot({
    filtered_data <- data %>% filter(BMI_Category == input$bmi_category_1)
    
    ggplot(filtered_data, aes(x = Sleep_Disorder, y = Quality_of_Sleep, fill = BMI_Category)) +
      geom_bar(stat = "summary", fun = "sum", color = "black") +
      scale_fill_manual(values = bmi_colors) +
      labs(title = paste("Sum of Quality of Sleep by Sleep Disorder for", input$bmi_category_1, "BMI Category"),
           x = "Sleep Disorder",
           y = "Sum of Quality of Sleep") +
      theme_minimal()
  })
  
  output$barChart2 <- renderPlot({
    filtered_data <- data %>% filter(BMI_Category == input$bmi_category_2)
    
    ggplot(filtered_data, aes(x = Sleep_Disorder, y = Stress_Level, fill = BMI_Category)) +
      geom_bar(stat = "summary", fun = "sum", color = "black") +
      scale_fill_manual(values = bmi_colors) +
      labs(title = paste("Sum of Stress Level by Sleep Disorder for", input$bmi_category_2, "BMI Category"),
           x = "Sleep Disorder",
           y = "Sum of Stress Level") +
      theme_minimal()
  })
  
  # Generate donut chart for sleep disorder by gender
  output$sleep_donut_chart <- renderPlotly({
    if (input$gender == "Female") {
      sleep_data <- subset(data, Gender == "Female")
      sleep <- table(sleep_data$Sleep_Disorder)
    } else {
      sleep_data <- subset(data, Gender == "Male")
      sleep <- table(sleep_data$Sleep_Disorder)
    }
    
    percentage <- round(prop.table(sleep) * 100, 1)
    count <- table(sleep_data$Sleep_Disorder)  # Calculate the count
    
    if (input$display_choice == "Count") {
      labels <- paste(c("No Sleep Disorder", "Insomnia", "Sleep Apnea"), "(", count, ")")
    } else {
      labels <- paste(c("No Sleep Disorder", "Insomnia", "Sleep Apnea"), "(", percentage, "%)")
    }
    
    donut_chart <- plot_ly(labels = labels,
                           values = sleep,
                           type = "pie",
                           hole = 0.6,
                           marker = list(colors = c("#CCDAF7", "#949EC5", "#D5B7B6")),
                           textinfo = "label",
                           textposition = "outside",  # Place labels outside the chart
                           showlegend = FALSE)
    
    donut_chart <- layout(donut_chart, title = paste(input$gender, "Sleep Disorder"))
    
    donut_chart
  })
  
  
  # Calculate the distribution of occupation
  occupation_count <- data %>%
    count(Occupation)
  
  # Calculate total count of responses
  total_responses <- nrow(data)
  
  # Generate pie chart for occupation distribution
  output$occupation_pie_chart <- renderPlot({
    occupation_count$Percentage <- round(occupation_count$n / total_responses * 100, 1)
    labels <- paste(occupation_count$Occupation, " (", occupation_count$Percentage, "%,", occupation_count$n, ")")
    
    # Set the size of the plot (adjust width and height as needed)
    options(repr.plot.width = 12, repr.plot.height = 12)
    
    pie(occupation_count$n,
        labels = labels,
        main = "Distribution of Occupation",
        col = c("#9E4A5F", "#D77372", "#E8A997", "#EBC294" ,"#AEC5B5","#90B1CF","#8198C2"),
        cex = 1.3
    )
  })
  
  output$occupation_bar_chart <- renderPlot({
    # Calculate average stress level by occupation using dplyr
    avg_stress_by_occupation <- data %>%
      group_by(Occupation) %>%
      summarise(Average_Stress_Level = mean(Stress_Level, na.rm = TRUE))
    
    # Plot using ggplot2
    ggplot(avg_stress_by_occupation, aes(x = Occupation, y = Average_Stress_Level, fill = Occupation)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(label = round(Average_Stress_Level, 1)), vjust = -0.5, color = "black") +
      labs(#title = "Average Stress Level by Occupation",
        x = "Occupation",
        y = "Average Stress Level") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  
  output$plot_scatterplot <- renderPlotly({
    p <- plot_ly(
      data,
      x = ~Stress_Level,
      y = ~Quality_of_Sleep,
      type = 'scatter',
      mode = 'markers',
      marker = list(color = '#8B1C62', size = 10),
      text = ~paste("Quality of Sleep:", Quality_of_Sleep, "<br>",
                    "Stress Level:", Stress_Level),
      hoverinfo = 'text'
    ) %>%
      add_trace(
        x = ~Stress_Level,
        y = ~predict(lm(Quality_of_Sleep ~ Stress_Level)),
        mode = 'lines',
        line = list(color = '#FFA500', width = 2),
        name = 'Regression Line',
        showlegend = TRUE  
      ) %>%
      layout(
        xaxis = list(
          title = 'Stress Level',
          showgrid = TRUE  
        ),
        yaxis = list(
          title = 'Quality of Sleep',
          showgrid = TRUE  
        ),
        hovermode = 'closest',
        legend = list(
          x = 0.8,  
          y = 1
        )
      )
    
    p
  })
  
  # Boxplot
  output$plot_boxplot <- renderPlotly({
    p <- ggplot(data, aes(x = BMI_Category, y = Daily_Steps, fill = BMI_Category)) +
      geom_boxplot() +
      labs(x = "BMI Category",
           y = "Daily Steps ") +
      scale_fill_manual(values = c("lightblue", "#90EE90", "#2E8B57")) +  
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank())
    
    ggplotly(p)
  })
  
  # Bar Plot
  output$plot_barplot <- renderPlotly({
    # Calculate the average Physical Activity Level by Sleep Disorder
    activity_by_sleep <- data %>%
      group_by(Sleep_Disorder) %>%
      summarise(avg_Physical_Activity_Level = mean(Physical_Activity_Level))
    
    # Define custom colors
    custom_colors <- c("#AEEEEE", "#87CEEB", "#6959CD")
    
    # Create the grouped bar plot
    p <- ggplot(activity_by_sleep, aes(x = Sleep_Disorder, y = avg_Physical_Activity_Level, fill = Sleep_Disorder)) +
      geom_bar(stat = "identity") +
      labs(x = "Sleep Disorder",
           y = "Physical Activity Level",
           fill = "Sleep Disorder") +
      scale_fill_manual(values = custom_colors) +  
      theme_minimal() +
      theme(panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank())  
    
    ggplotly(p)
  })
  
  # Correlation Matrix Heatmap
  output$plot_heatmap <- renderPlot({
    # Select numerical variables for the correlation matrix
    vars_numeriques <- sleep_data[, sapply(sleep_data, is.numeric)]
    
    # Create the correlation matrix
    correlation_matrix <- cor(vars_numeriques)
    
    # Convert the correlation matrix into a dataframe for ggplot2
    library(reshape2)
    cor_df <- melt(correlation_matrix)
    
    # Create the heatmap with ggplot2 and use the "blues" colour palette
    library(ggplot2)
    ggplot(cor_df, aes(Var2, Var1, fill = value, label = round(value, 2))) +
      geom_tile(color = "white") +
      geom_text(size = 4, color = "black") +
      scale_fill_gradientn(colors = c("white", "#FFE1FF", "#CD69C9")) + 
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),  
        axis.text.y = element_text(size = 12),  
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12)    
      ) +
      coord_fixed() +
      labs(x = NULL, y = NULL) +  
      theme(legend.position = "right") +
      guides(fill = guide_colorbar(title.theme = element_text(size = 12),  
                                   label.theme = element_text(size = 12)))
  })
  
  # Stacked graph by gender
  output$stackedPlot <- renderPlot({
    # Filter the data based on selected variable
    data_selected <- data %>%
      select(Gender, !!sym(input$variable)) %>%
      mutate(across(everything(), as.factor)) # Ensure all variables are factors
    
    # Reshape the data for plotting
    data_long <- pivot_longer(data_selected, cols = all_of(input$variable), names_to = "Variable", values_to = "Value")
    
    # Plot stacked graph
    ggplot(data_long, aes(x = Gender, fill = Value)) +
      geom_bar(position = "stack") +
      labs(title = paste("Stacked Graph of", input$variable, "by Gender"),
           x = "Gender",
           y = "Count",
           fill = input$variable) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui, server)
