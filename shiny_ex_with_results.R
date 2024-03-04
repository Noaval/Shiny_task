###Shiny EX###
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

##importing data
data <- readxl::read_xlsx("C:/Users/97252/Desktop/תואר שני/מיומנויות במדעי הנתונים/shiny/shiny reactives/file_a3664f94-0441-4e67-bc94-d4ada374a1db.xlsx", 
                          range = "A19:I39") |> 
  rename(Year = 1) |> 
  mutate(across(.fns = as.numeric)) |> 
  tidyr::pivot_longer(-Year, names_to = "Type", values_to = "Tons") |> 
  mutate(Type = factor(Type))

head(data)

## UI
ui <- dashboardPage(
  dashboardHeader(title = "Recycling In Israel"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plot", tabName = "itm_plot", icon = icon("chart-line")),
      menuItem("Table", tabName = "itm_tab", icon = icon("table")),
      menuItem("Comments", tabName = "itm_observations", icon = icon("eye")) 
    )
  ),
  
  dashboardBody(
    # Inputs
    selectInput("gar_type", "Select type to highlight", choices = unique(data$Type)),
    sliderInput("yr_range", "Year Range", min = min(data$Year), max = max(data$Year), value = range(data$Year)),
    actionButton("update_button", "Update Data"),  
    
    h3("Output"),
    
    # Tabbed outputs 
    tabItems(
      tabItem("itm_plot",
              fluidRow(
                plotOutput("plot_gar")
              )),
      
      tabItem("itm_tab",
              fluidRow(
                tableOutput("tab_summ")
              )),
      # An added dashboardSidebar that allows users to write comments on the output graph and summary statistics
      tabItem("itm_observations",  # New tab for user comments
              h4("Your Observations"),
              textInput("user_comment", "Enter your comment:", value = ""),
              textOutput("display_comment"))
    )
  ), skin = "green"
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
   
  observeEvent(input$update_button, {
    output$plot_gar <- renderPlot({
      data_filter <- data |> 
        mutate(Highlight = ifelse(Type == input$gar_type, "Selected", "Other")) |> 
        drop_na(Year, Tons)
      #A plot with time (x) and tons (y), that react to "gar_type" - highlighting (somehow) the selected type.
      ggplot(data_filter, aes(x = Year, y = Tons, color = Highlight)) +
        geom_line() +
        geom_point() +
        scale_color_manual(values = c("Selected" = "red", "Other" = "grey")) +
        theme_minimal() +
        labs(color = "Type")
    })
  })
  
  # A table of summary statistics for each 'type' of waste, that is reactive to "yr_range"
  output$tab_summ <- renderTable({
    data_filtered <- data |> 
      filter(Year >= input$yr_range[1] & Year <= input$yr_range[2]) |>
      drop_na(Year, Tons)
    summary_table <- data_filtered |> 
      group_by(Type) |>
      summarize(Average = mean(Tons, na.rm = TRUE), Total = sum(Tons, na.rm = TRUE), .groups = 'drop')
    summary_table
  })
  output$display_comment <- renderText({
    paste("Your comment:", input$user_comment)
  })
}

# Run App -----------------------------------------------------------------
shinyApp(ui, server)
