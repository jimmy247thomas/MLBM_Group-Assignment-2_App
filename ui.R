library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Airline Tweet Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = ".csv"),
      actionButton("analyze", "Analyze")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Market Sentiment", plotlyOutput("sentiment_summary_plot")),
        tabPanel("Market word cloud", wordcloud2Output("word_cloud")),
        tabPanel("Company sentiment chart", plotlyOutput("airline_comparison_plot")),
        tabPanel("Company word clouds",
                 selectInput("airline_select", "Select Airline", choices = NULL),
                 wordcloud2Output("airline_word_cloud")
        ),
        tabPanel("Competitor analysis", plotlyOutput("airline_ranking_plot")),
        tabPanel("Raw Data", DTOutput("raw_data"))
      )
    )
  )
)
