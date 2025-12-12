library(shiny)
library(tidyverse)
library(ggplot2)
library(modelr)

nhanes_clean <- readRDS("nhanes_clean.rds")

dep2score <- function(x) {
  dplyr::case_match(
    as.integer(x),
    1L ~ 0L,
    2L ~ 1L,
    3L ~ 2L,
    4L ~ 3L,
    .default = NA_integer_
  )
}

nhanes_clean <- nhanes_clean |>
  mutate(
    LittleInterestScore = dep2score(LittleInterest),
    DepressedItemScore  = dep2score(Depressed),
    DepScore            = LittleInterestScore + DepressedItemScore
  )

age_choices    <- c("All", sort(unique(nhanes_clean$AgeGroup)))
sex_choices    <- c("All", sort(unique(as.character(nhanes_clean$Sex))))
race_choices   <- c("All", sort(unique(as.character(nhanes_clean$Race))))
income_choices <- c("All", sort(unique(as.character(nhanes_clean$IncomeGroup))))

ui <- fluidPage(
  titlePanel("NHANES: Sleep, Obesity, and Depression – Regression Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput("age_group", "Age group",
                  choices = age_choices, selected = "All"),
      selectInput("sex", "Sex",
                  choices = sex_choices, selected = "All"),
      selectInput("race", "Race/ethnicity",
                  choices = race_choices, selected = "All"),
      selectInput("income", "Income group",
                  choices = income_choices, selected = "All"),
      
      tags$hr(),
      h4("Model setup"),
      
      selectInput(
        "model_type",
        "Predictors of interest (linear regression on DepScore):",
        choices = c(
          "DepScore ~ SleepHrsNight" =
            "sleep_simple",
          "DepScore ~ SleepHrsNight + Obese" =
            "sleep_obese",
          "DepScore ~ SleepHrsNight + Obese + Age + Sex + Race + Income" =
            "full",
          "DepScore ~ SleepCat * Obese" =
            "interaction",
          "DepScore ~ Age + Sex + Race + Income" = "confound"
        ),
        selected = "sleep_simple"
      )
    ),
    
    mainPanel(
      h4(textOutput("model_title")),
      verbatimTextOutput("model_formula"),
      verbatimTextOutput("model_summary"),
      tags$hr(),
      h4("Visualization"),
      plotOutput("reg_plot", height = "400px"),
      br(),
      textOutput("plot_caption")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    d <- nhanes_clean
    
    if (input$age_group != "All") {
      d <- d %>% filter(AgeGroup == input$age_group)
    }
    if (input$sex != "All") {
      d <- d %>% filter(as.character(Sex) == input$sex)
    }
    if (input$race != "All") {
      d <- d %>% filter(as.character(Race) == input$race)
    }
    if (input$income != "All") {
      d <- d %>% filter(IncomeGroup == input$income)
    }
    
    d
  })
  
  model_formula <- reactive({
    y_var <- "DepScore"
    
    rhs <- switch(
      input$model_type,
      "sleep_simple" = "SleepHrsNight",
      "sleep_obese"  = "SleepHrsNight + Obese",
      "full"         = "SleepHrsNight + Obese + Age + Sex + Race + IncomeGroup",
      "interaction"  = "SleepCat * Obese",
      "confound"     = "Age + Sex + Race + IncomeGroup"
    )
    
    if (is.null(rhs)) rhs <- "SleepHrsNight"
    
    as.formula(paste(y_var, "~", rhs))
  })
  
  model_data <- reactive({
    d <- filtered_data()
    
    base_vars       <- c("DepScore", "SleepHrsNight", "SleepCat", "Obese")
    confounders_all <- c("Age", "Sex", "Race", "IncomeGroup")
    
    needed_vars <- base_vars
    if (input$model_type %in% c("full", "interaction", "confound")) {
      needed_vars <- c(needed_vars, confounders_all)
    }
    
    needed_vars <- intersect(needed_vars, names(d))
    
    if (length(needed_vars) > 0) {
      d <- d %>% tidyr::drop_na(all_of(needed_vars))
    }
    
    d
  })
  
  #Fit the linear model
  model_fit <- reactive({
    d <- model_data()
    if (nrow(d) == 0) {
      return(NULL)
    }
    
    f <- tryCatch(model_formula(), error = function(e) NULL)
    if (is.null(f)) return(NULL)
    
    fit <- tryCatch(lm(f, data = d), error = function(e) NULL)
    fit
  })
  
  output$model_title <- renderText({
    switch(
      input$model_type,
      "sleep_simple" = "Model: DepScore vs Sleep hours (unadjusted)",
      "sleep_obese"  = "Model: DepScore vs Sleep hours + Obesity",
      "full"         = "Model: DepScore vs Sleep, Obesity, and Demographics (adjusted)",
      "interaction"  = "Model: DepScore vs Sleep category × Obesity",
      "confound"     = "Model: DepScore vs Demographic confounders",
      "Model"
    )
  })
  
  output$model_formula <- renderText({
    f <- tryCatch(model_formula(), error = function(e) NULL)
    
    if (is.null(f) || is.null(model_fit())) {
      "R formula: (not available for this subset; try broadening the filters such as Income or Sex.)"
    } else {
      paste("R formula:", deparse(f))
    }
  })
  
  output$model_summary <- renderPrint({
    fit <- model_fit()
    
    if (is.null(fit)) {
      cat(
        "Model summary is not available for this subset of the data.\n",
        "Try broadening the filters (for example, set Sex or Race to 'All')",
        "so that the model has more variation to work with.\n"
      )
      return(invisible(NULL))
    }
    
    print(summary(fit))
  })
  
  #make plot using ggplot2
  output$reg_plot <- renderPlot({
    d <- model_data()
    
    validate(
      need(nrow(d) > 0,
           "No data for the selected filters. Try choosing 'All' for some filters.")
    )
    
    if (input$model_type %in% c("sleep_simple", "sleep_obese", "full")) {
      ggplot(d, aes(x = SleepHrsNight, y = DepScore)) +
        geom_point(alpha = 0.4) +
        geom_smooth(method = "lm", se = TRUE) +
        labs(
          x = "Sleep hours per night",
          y = "DepScore",
          title = "Depression score vs sleep hours"
        )
    } else {
      ggplot(d, aes(x = SleepCat, y = DepScore, fill = Obese)) +
        geom_boxplot(alpha = 0.7) +
        labs(
          x = "Sleep category",
          y = "DepScore",
          fill = "Obese",
          title = "Depression score by sleep category and obesity"
        )
    }
  })
  
  output$plot_caption <- renderText({
    "Compare simpler (unadjusted) models to models that add obesity and demographic variables to see how confounding changes the relationships."
  })
}

shinyApp(ui, server)
