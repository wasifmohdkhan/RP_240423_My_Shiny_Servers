
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)

# Define UI

# Define UI
ui <- fluidPage(
  titlePanel("Poster Score Summarization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileInput", "Choose Excel Files", multiple = TRUE, accept = ".xlsx"),
      hr(),
      h5("Display Options"),
      radioButtons("displayType", "Display Type", choices = c("Average Scores" = "avg", "Feedback Counts" = "count"))
    ),
    mainPanel(
      plotOutput("plotOutput"),
      hr(),
      h5("Top 5 Entries"),
      tableOutput("topEntries")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Process the uploaded files to compute scores and counts
  processed_data <- reactive({
    req(input$fileInput)
    
    score_cols <- c("ScholarlyContentScientificCreativeContent25Points",
                    "ClearAndEngagingDescriptionexplanationOfProject25Points",
                    "AppropriateUseOfImagesGraphicRepresentationAndorTables10Points",
                    "EffevtiveCommunicationToTheAudience15Points",
                    "OverallImpressionAndSignificanceOfResearch25Points")
    
    average_scores <- data.frame(PosterID = character(), Average_Score = numeric())
    feedback_counts <- data.frame(Name_Last = character(), Count = numeric())
    
    for (file in input$fileInput$datapath) {
      data <- read_excel(file)
      data$Total_Score <- rowSums(data[, score_cols], na.rm = TRUE)
      
      poster_average_score <- mean(data$Total_Score, na.rm = TRUE)
      poster_id <- colnames(data)[1]
      average_scores <- rbind(average_scores, data.frame(PosterID = poster_id, Average_Score = poster_average_score))
      
      unique_names <- unique(data$YourName_Last)
      counts <- table(unique_names)
      feedback_counts <- rbind(feedback_counts, data.frame(Name_Last = names(counts), Count = as.integer(counts)))
    }
    
    average_scores <- average_scores %>%
      group_by(PosterID) %>%
      summarise(Average_Score = mean(Average_Score, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Average_Score))
    
    feedback_counts <- feedback_counts %>%
      group_by(Name_Last) %>%
      summarise(Count = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Count))
    
    list(scores = average_scores, counts = feedback_counts)
  })
  
  # Render the appropriate plot based on user selection
  output$plotOutput <- renderPlot({
    data <- processed_data()
    if (input$displayType == "avg") {
      ggplot(data$scores, aes(x = reorder(PosterID, -Average_Score), y = Average_Score)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_text(aes(label = round(Average_Score, 2)), vjust = -0.3, size = 3) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Poster ID", y = "Average Score", title = "Average Scores for Each Poster")
    } else {
      ggplot(data$counts, aes(x = reorder(Name_Last, -Count), y = Count)) +
        geom_bar(stat = "identity", fill = "coral") +
        geom_text(aes(label = Count), vjust = -0.3, size = 3) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Last Name", y = "Feedback Count", title = "Feedback Counts Per Person")
    }
  })
  
  # Render the top 5 entries table based on user selection
  output$topEntries <- renderTable({
    data <- processed_data()
    if (input$displayType == "avg") {
      head(data$scores, 5)
    } else {
      head(data$counts, 5)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
