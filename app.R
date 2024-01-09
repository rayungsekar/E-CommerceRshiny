library(shiny)
library(shinydashboard)
library(DT)

Placement = factor(rep(c("Left", "Center", "Right"), each = 10))
Rate = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7,
         3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9,
         3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
AdPlacements = data.frame(
  Placement, Rate
)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Click-Through Rates"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Data", tabName = "input"),
      menuItem("Analysis of Variances", tabName = "analysis"),
      menuItem("Visualizations", tabName = "visualizations")
    )
  ),
  dashboardBody(
    tabItems(
      # Input tab
      tabItem(
        tabName = "input",
        titlePanel("Input Data"),
        fluidPage(
          fluidRow(
            column(4, textInput("leftSidebar", "Left Sidebar", "")),
            column(4, textInput("centerPage", "Center Page", "")),
            column(4, textInput("rightSidebar", "Right Sidebar", "")),
            column(4, actionButton("addLeftData", "Add Left Data")),
            column(4, actionButton("addCenterData", "Add Center Data")),
            column(4, actionButton("addRightData", "Add Right Data"))
          ),
          fluidRow(
            tags$style(type="text/css", ".dataTables_wrapper { margin-top: 20px; }"),
            column(12, DTOutput("dataPreview"))
          ),
          fluidRow(
            column(6, actionButton("removeData", "Remove Selected Data"))
          )
        )
      ),
      # Analysis tab
      tabItem(
        tabName = "analysis",
        titlePanel("Analysis of Variances"),
        fluidPage(
          fluidRow(
            box(
              title = "Null Hypothesis",
              width = 6,
              status = "primary",
              solidHeader = TRUE,
              background = "blue",
              height = 100,
              textOutput("nullHypothesis")
            ),
            box(
              title = "Alternative Hypothesis",
              width = 6,
              status = "primary",
              solidHeader = TRUE,
              background = "blue",
              height = 100,
              textOutput("alternativeHypothesis")
            )
          ),
          verbatimTextOutput("anovaSummary"),
          box(
            title = "Conclusion",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            background = "blue",
            height = 80,  # Adjust the height as needed
            textOutput("conclusionOutput")
          )
        )
      )
      ,
      # Visualizations tab
      tabItem(
        tabName = "visualizations",
        titlePanel("Dataset Visualization"),
        fluidPage(
          box(
            title = "Boxplot",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            background = "blue",
            height = 466,  # Adjust the height as needed
            plotOutput("boxplot")
          ),
          verbatimTextOutput("summaryOutput")
        )
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Initialize a reactiveValues object to store the text input data
  text_input_data <- reactiveValues(data = AdPlacements)  
  
  # Reactive function to add a new row to text input data
  observeEvent(input$addLeftData, {
    new_row <- data.frame(
      Placement = "Left",
      Rate = as.numeric(input$leftSidebar)
    )
    
    if (is.null(text_input_data$data)) {
      text_input_data$data <- new_row
    } else {
      # Ensure column names match before using rbind
      if (all(names(text_input_data$data) == names(new_row))) {
        text_input_data$data <- rbind(text_input_data$data, new_row)
      } else {
        # Handle the case where the columns don't match
        cat("Error: Column names do not match. Adding a new row with matching columns.\n")
        text_input_data$data <- rbind(text_input_data$data, new_row)
      }
    }
    
    # Clear the text input field after adding a new row
    updateTextInput(session, "leftSidebar", value = "")
  })
  
  observeEvent(input$addCenterData, {
    new_row <- data.frame(
      Placement = "Center",
      Rate = as.numeric(input$centerPage)
    )
    
    if (is.null(text_input_data$data)) {
      text_input_data$data <- new_row
    } else {
      # Ensure column names match before using rbind
      if (all(names(text_input_data$data) == names(new_row))) {
        text_input_data$data <- rbind(text_input_data$data, new_row)
      } else {
        # Handle the case where the columns don't match
        cat("Error: Column names do not match. Adding a new row with matching columns.\n")
        text_input_data$data <- rbind(text_input_data$data, new_row)
      }
    }
    
    # Clear the text input field after adding a new row
    updateTextInput(session, "centerPage", value = "")
  })
  
  observeEvent(input$addRightData, {
    new_row <- data.frame(
      Placement = "Right",
      Rate = as.numeric(input$rightSidebar)
    )
    
    if (is.null(text_input_data$data)) {
      text_input_data$data <- new_row
    } else {
      # Ensure column names match before using rbind
      if (all(names(text_input_data$data) == names(new_row))) {
        text_input_data$data <- rbind(text_input_data$data, new_row)
      } else {
        # Handle the case where the columns don't match
        cat("Error: Column names do not match. Adding a new row with matching columns.\n")
        text_input_data$data <- rbind(text_input_data$data, new_row)
      }
    }
    
    # Clear the text input field after adding a new row
    updateTextInput(session, "rightSidebar", value = "")
  })
  
  # Reactive function to remove selected rows
  observeEvent(input$removeData, {
    if (!is.null(input$dataPreview_rows_selected)) {
      text_input_data$data <- text_input_data$data[-input$dataPreview_rows_selected, ]
    }
  })
  
  # Data preview table
  output$dataPreview <- renderDT({
    if (is.null(text_input_data$data)) {
      return(datatable(NULL, options = list(pageLength = 5)))
    } else {
      return(datatable(text_input_data$data, editable = TRUE, selection = "multiple", options = list(pageLength = 5)))
    }
  })
  
  # Null Hypothesis
  output$nullHypothesis <- renderText({
    "Null Hypothesis: The mean click-through rates are equal across all placement groups."
  })
  
  # Alternative Hypothesis
  output$alternativeHypothesis <- renderText({
    "Alternative Hypothesis: There is a significant difference in mean click-through rates between at least two placement groups."
  })
  
  # ANOVA analysis
  output$anovaSummary <- renderPrint({
    if (!is.null(text_input_data$data)) {
      anova_model <- aov(Rate ~ Placement, data = text_input_data$data)
      summary(anova_model)
    }
  })
  
  
  # Conclusion based on p-value
  output$conclusionOutput <- renderText({
    if (!is.null(text_input_data$data)) {
      anova_model <- aov(Rate ~ Placement, data = text_input_data$data)
      p_value <- summary(anova_model)[[1]][["Pr(>F)"]][1]
      
      if (p_value < 0.05) {
        return("With the p-value < 5% significance level, there is enough evidence to reject the null hypothesis. Conclusion: There is a significant difference between groups.")
      } else {
        return("With the p-value > 5% significance level, there is not enough evidence to reject the null hypothesis. Conclusion: There is no significant difference between groups.")
      }
    }
  })
  
  # Visualizations tab
  output$boxplot <- renderPlot({
    if (!is.null(text_input_data$data)) {
      # Specify custom colors for each boxplot
      boxplot(Rate ~ Placement, data = text_input_data$data, main = "Boxplot of Click-Through Rates by Placement",
              col = c("#ddafff", "lightgreen", "lightcoral"))
    }
  })
}

# Run the application
shinyApp(ui, server)
