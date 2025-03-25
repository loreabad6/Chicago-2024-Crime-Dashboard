# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(shiny)


# Load Chicago Crime Data
Chicago_crime_data <- read.csv("Crimes_-_2001_to_Present_20250203.csv")

# Remove unnecessary columns and rows with empty cells
Chicago_crime_data_clean <- Chicago_crime_data %>%
  select(-c(Case.Number, IUCR, Block, Beat, Ward, X.Coordinate, Y.Coordinate, Year, Updated.On, Location))
Chicago_crime_data_clean <- Chicago_crime_data_clean %>%
  filter(complete.cases(.))

#split the Date column into four columns respectively
Chicago_crime_data_clean$Date <- as.POSIXct(Chicago_crime_data_clean$Date, format = "%m/%d/%Y %I:%M:%S %p")
Chicago_crime_data_clean <- Chicago_crime_data_clean %>%
  mutate(
    # Extract Date
    Date_only = as.Date(Date),
    
    # Extract Time
    Time_only = format(Date, "%H:%M:%S"),
    
    # Extract Day of the Week (e.g., "Monday")
    Day_of_week = weekdays(Date),
    
    # Extract Month (e.g., "January")
    Month = format(as.Date(Date), "%B"),
    
    # Create a Time of Day column (Daytime/Nighttime)
    Time_of_day = ifelse(hour(Date) >= 6 & hour(Date) < 18, "Daytime", "Nighttime"))

# Enrich the Chicago Crime data_clean with a dataset containing names if the 77 chicago communties
Chicago_communities <- read.csv("Chicago communities.csv")

# Rename Area_Numbe to community.area in Chicago_communities, so that the two tables can have an index number with the same name.
Chicago_communities <- Chicago_communities %>%
  rename(Community.Area = AREA_NUMBE)

# Join the two tables, by adding the Community.Area and Community colums from the chicago communities to the chicago_crime_data_clean
Chicago_crime_data_enriched <- Chicago_crime_data_clean %>%
  left_join(
    Chicago_communities %>% select(Community.Area, COMMUNITY),
    by = "Community.Area"
  )


ui <- fluidPage(
  # Dashboard Title
  titlePanel("Chicago 2024 Crime Data Dashboard"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      selectizeInput("community", "Communities:",
                     choices = sort(unique(Chicago_crime_data_enriched$COMMUNITY)),
                     multiple = TRUE),
      selectizeInput("CrimeType", "Crime Type:",
                     choices = sort(unique(Chicago_crime_data_enriched$Primary.Type)),
                     multiple = TRUE)
    ),
    
    # Main panel to display outputs
    mainPanel(
      fluidRow(
        column(4,
               wellPanel(
                 textOutput("total_crime"),
                 style = "background-color: #f8f9fa; padding: 10px; border-radius: 10px;")
        ),
        column(8,
               plotOutput("day_crime_chart", height = "300px")
        )
      ),
      
      fluidRow(
        column(6,
               plotOutput("monthly_crime_chart", height = "300px")
        ),
        column(6,
               plotOutput("arrest_made", height = "300px")
        )
      ),
      
      fluidRow(
        column(6,
               plotOutput("top5_communities", height = "300px")
        ),
        column(6,
               plotOutput("time_chart", height = "300px")
        )
      ),
      
      fluidRow(
        column(12,
               leafletOutput("map_of_crime", height = "500px")
        )
      )
    )
  )
)


server <- function(input, output, session){
  filtered_data <- reactive({
    Chicago_crime_data_enriched %>%
      filter(
        !is.na(Latitude) & !is.na(Longitude),
        COMMUNITY %in% input$community | all(input$community == unique(Chicago_crime_data_enriched$COMMUNITY)),
        Primary.Type %in% input$CrimeType | all(input$CrimeType == unique(Chicago_crime_data_enriched$Primary.Type))
      )
  })
  
  # Display 2024 Total Crime Occurrences
  output$total_crime <- renderText({
    total_crime <- nrow(filtered_data())
    paste("2024 Total Crime Occurences:", total_crime)
  })
  
  # Reorder Day_of-week into it natural order from Monday through Sunday
  Chicago_crime_data_enriched$Day_of_week <- factor(
    Chicago_crime_data_enriched$Day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  )
  
  # Display Cime Distribution by Days of the Week
  output$day_crime_chart <- renderPlot({
    ggplot(filtered_data(), aes(x = Day_of_week)) +
      geom_bar(fill = "yellow") +
      labs(title = "Total crime Occurence by Days of the Week", x = "Day of the Week", y = "Total Crime Count") +
      coord_flip()
  })
  
  # Display Monthly Crime Trend
  output$monthly_crime_chart <- renderPlot({
    month_data <-filtered_data() %>%
      group_by(Month) %>%
      summarise(Month_count = n()) %>%
      mutate(Month = factor(Month, levels = month.name)) %>%
      arrange(Month)
    
    ggplot(na.omit(month_data), aes(x = Month, y = Month_count, group = 1)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "red", size = 2) +
      labs(title = "Monthly Crime Occurrence Trends", x = "Month",  y = "Total Crime Occurrences") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #Arrest Made (Efficiency of Chicago policing)
  output$arrest_made <- renderPlot({
    arrest_percentage <- filtered_data() %>%
      group_by(Arrest) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
    
    ggplot(arrest_percentage, aes(x = "", y = percentage, fill = Arrest)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
      labs(title = "Arrest Made/Policing Efficiency") +
      theme_void()
  })
  
  #Display top 5 Communities with the Highest Crime Occurences
  output$top5_communities <- renderPlot({
    top5_chart <- filtered_data() %>%
      group_by(COMMUNITY) %>%
      summarise(Crime_count = n()) %>%
      arrange(desc(Crime_count)) %>%
      head(5)
    
    ggplot(top5_chart, aes(x = reorder(COMMUNITY, Crime_count), y = Crime_count)) +
      geom_bar(stat = "identity", fill = "blue") +
      geom_text(aes(label = Crime_count), vjust = -0.5, size = 4, color = "black") +
      labs(title = "Top Five (5) Communities with Highest Crime Occurrence", x = "Community", y = "Crime Occurence Count") +
      theme_minimal()
  })
  
  #Display Crime Occurences by Day or Night
  output$time_chart <- renderPlot({
    time_percenatge <- filtered_data() %>%
      group_by(Time_of_day) %>%
      summarise(count = n())  %>%
      mutate(percentage = count / sum(count) * 100)
    
    ggplot(na.omit(time_percenatge), aes(x = "", y = percentage, fill = Time_of_day)) +
      geom_bar(stat = "identity", width = 1 ) +
      coord_polar("y") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
      labs(title = "Crime Occurences by Day or Night") +
      theme_void()
  })
  
  #Display Map of Chicago showing Crime Occurrence
  output$map_of_crime <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 0.25,
        color = ~ifelse(Arrest == "True", "green", "red"),
        popup = ~paste0("<b>Crime Type:</b> ", Primary.Type , "<br>",
                        "<b>Community:</b> ", COMMUNITY, "<br>",
                        "<b>Date:</b> ", Date_only, "<br>",
                        "<b>Police District:</b> ", District, "<br>",
                        "<b>Time of Day:</b> ", Time_of_day, "<br>",
                        "<b>Description:</b> ", Description, "<br>",
                        "<b>Location Description:</b> ", Location.Description),
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("green", "red"),
        labels = c("Arrest Made", "No Arrest"),
        title = "Criminal/Perpetrators Arrest Status")
  })
}

shinyApp(ui, server)
