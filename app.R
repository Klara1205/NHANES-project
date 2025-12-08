#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

 library(shiny)
 library(dplyr)
 library(ggplot2)
 library(scales)
 
 nhanes <- readRDS("nhanes_clean.rds")
 
 nhanes <- nhanes |>
   mutate(
     AgeGroup = factor(AgeGroup, levels = c("18–39", "40–59", "60+")),
     Sex      = factor(Sex)
   )
 
 
 
 # Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NHANES Depression (PHQ-9) Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        h4("Filters"),
        checkboxGroupInput(
          "sex_filter",
          "Sex:",
          choices  = levels(nhanes$Sex),
          selected = levels(nhanes$Sex)
        ),
        
        hr(),
        h4("Outcome"),
        radioButtons(
          "outcome_type",
          "Choose outcome:",
          choices = c("Depression score (DepScore)" = "score",
                      "Depressed (PHQ-9 ≥ 10)"     = "binary"),
          selected = "score"
        )
      ),

        # Show a plot of the generated distribution
      
      mainPanel(
        plotOutput("mainPlot"),
        br(),
        verbatimTextOutput("summaryText")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    nhanes |>
      filter(Sex %in% input$sex_filter)
  })
  
  # Main plot
  output$mainPlot <- renderPlot({
    dat <- filtered_data()
    req(nrow(dat) > 0)   # make sure there is data
    
    if (input$outcome_type == "score") {
      # Boxplot of DepScore by AgeGroup
      ggplot(dat, aes(x = AgeGroup, y = DepScore)) +
        geom_boxplot() +
        labs(
          x = "Age group",
          y = "PHQ-9 total score (DepScore)",
          title = "Depression scores by age group"
        )
    } else {
      # Bar plot of depression prevalence by AgeGroup
      summary_df <- dat |>
        group_by(AgeGroup) |>
        summarise(
          prevalence = mean(Depressed, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        )
      
      ggplot(summary_df, aes(x = AgeGroup, y = prevalence)) +
        geom_col() +
        scale_y_continuous(labels = percent_format()) +
        labs(
          x = "Age group",
          y = "Depression prevalence (PHQ-9 ≥ 10)",
          title = "Depression prevalence by age group"
        )
    }
  })
  
  # Text summary
  output$summaryText <- renderPrint({
    dat <- filtered_data()
    cat("Number of adults in selected subgroup:", nrow(dat), "\n")
    
    if (nrow(dat) == 0) {
      cat("No data for this combination of filters.\n")
      return(invisible(NULL))
    }
    
    cat("Mean PHQ-9 score (DepScore):",
        round(mean(dat$DepScore, na.rm = TRUE), 2), "\n")
    cat("Depression prevalence (DepScore ≥ 10):",
        round(mean(dat$Depressed, na.rm = TRUE) * 100, 1), "%\n")
  })
}

# 4. Run the app ----------------------------------------------

shinyApp(ui = ui, server = server)
