library(shiny)
library(tidyverse)
library(sentimentr)
library(plotly)
library(DT)
library(wordcloud2)
library(tidytext)
library(stopwords)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      df <- read_csv(input$file$datapath)
      data(df)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  analyzed_data <- eventReactive(input$analyze, {
    req(data())
    df <- data()
    if (is.null(df$text)) return(NULL)
    df <- df %>%
      mutate(airline = case_when(
        str_detect(text, "^@American") ~ "American",
        str_detect(text, "^@JetBlue") ~ "JetBlue",
        str_detect(text, "^@Southwest") ~ "Southwest",
        str_detect(text, "^@United") ~ "United",
        str_detect(text, "^@USAirways") ~ "USAirways",
        str_detect(text, "^@VirginAmerica") ~ "VirginAmerica",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(airline))
    df$text <- str_replace(df$text, "^@\\w+\\s+", "")
    df$text <- gsub("[^[:alnum:][:space:]']", "", df$text)
    df$text <- gsub("\\s+", " ", df$text)
    df$text <- trimws(df$text)
    df <- df %>% filter(!is.na(text) & text != "")
    tryCatch({
      sentiment_data <- df %>% mutate(sentiment = sentiment(text)$sentiment)
      return(sentiment_data)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })

  output$sentiment_summary_plot <- renderPlotly({
    req(analyzed_data())
    sentiment_data <- analyzed_data()
    if (is.null(sentiment_data)) return(NULL)
    sentiment_summary <- sentiment_data %>%
      mutate(sentiment_category = case_when(
        sentiment > 0 ~ "Positive",
        sentiment < 0 ~ "Negative",
        TRUE ~ "Neutral"
      )) %>%
      group_by(sentiment_category) %>%
      summarise(count = n())
    plot_ly(sentiment_summary, x = ~sentiment_category, y = ~count, type = "bar") %>%
      layout(title = "Sentiment Summary")
  })

  my_stopwords <- c(
    "a", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "as", "at",
    "be", "because", "been", "before", "being", "below", "between", "both", "but", "by",
    "could", "did", "do", "does", "doing", "down", "during",
    "each", "few", "for", "from", "further", "flights",
    "had", "has", "have", "having", "he", "her", "here", "hers", "herself", "him", "himself", "his", "how",
    "i", "if", "in", "into", "is", "it", "its", "itself",
    "me", "more", "most", "my", "myself",
    "no", "nor", "not", "of", "off", "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves", "out", "over", "own",
    "same", "she", "should", "so", "some", "such",
    "than", "that", "the", "their", "theirs", "them", "themselves", "then", "there", "these", "they", "this", "those", "through", "to", "too",
    "under", "until", "up",
    "very",
    "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "with",
    "would",
    "you", "your", "just", "get", "can", "us", "now", "one", "like", "will", "day", "time", "hours", "amp",
    "going", "know", "doesn't", "make", "really", "people", "got", "way", "it's", "see", "still", "that", "don't", "going", "back", "told",
    "said", "online", "number", "days", "another", "agent", "left", "call", "hrs", "find", "dm", "hold", "someone", "lost",
    "system", "much", "please", "travel", "seat", "trying", "check", "keep", "what's", "crew", "already", "tomorrow", "first",
    "missed", "minutes", "sure", "waiting", "gogood", "that's", "free", "phone", "answer", "reservation",
    "new", "aa", "mm", "staff", "guys", "rightr", "without", "sitting", "claim", "email", "night",
    "rebook", "destinationdragons", "print", "supposed", "flightled", "flighted", "2", "5", "due", "doesn", "doesn't", "doesn t",
    "today", "hour", "take", "last", "help", "cancelled", "change",
    "think", "boarding", "weather", "min", "home", "show", "even", "response", "seats", "want", "work",
    "miss", "ticket", "understand", "received", "made", "enough", "pass", "lax", "3", "10", "next", "booking", "tonight", "nothing",
    "asked", "wait", "says", "two", "act", "all", "sent", "different", "since", "wrong", "ever", "trip", "experience", "moving",
    "contact", "ord", "longer", "right", "customers", "34", "lez", "book", "sto", "min", "didn", "rebooked", "status", "better", "appreciate",
    "getting", "info", "flying", "nothing", "162", "9hk", "gates",
    "jfk", "didn", "come", "app", "bag", "didn't", "didn t", "can't", "i'm", "yet", "nyc", "2hrs", "1272", "send", "oui", "boston", "access", "man", "absolutely", "part", "tweet", "confirmation", "cool", "wife", "saw", "switch", "bus", "ask", "jet", "area", "life", "75atc", "etc", "1758", "ur", "512", "say", "drop", "830", "519", "flight", "jetblue", "plane", "gate", "customer", "airline", "pilot", "i've", "dfw", "need"
  )
  output$word_cloud <- renderWordcloud2({
    req(analyzed_data())
    sentiment_data <- analyzed_data()
    if (is.null(sentiment_data)) return(NULL)
    word_counts <- sentiment_data %>%
      unnest_tokens(word, text) %>%
      anti_join(tibble(word = my_stopwords), by = "word") %>%
      filter(nchar(word) > 3) %>%
      count(word, sort = TRUE)
    wordcloud2(word_counts, size = 0.7, minSize = 1)
  })

  output$airline_comparison_plot <- renderPlotly({
    req(analyzed_data())
    sentiment_data <- analyzed_data()
    if (is.null(sentiment_data)) return(NULL)
    if (!("airline" %in% names(sentiment_data))) return(plotly_empty(type = "scatter", mode = "markers", text = "No airline data."))
    airline_sentiment <- sentiment_data %>%
      mutate(sentiment_category = case_when(
        sentiment > 0 ~ "Positive",
        sentiment < 0 ~ "Negative",
        TRUE ~ "Neutral"
      )) %>%
      group_by(airline, sentiment_category) %>%
      summarise(count = n(), .groups = "drop") %>%
      pivot_wider(names_from = sentiment_category, values_from = count, values_fill = 0)
    if (all(c("Positive", "Negative", "Neutral") %in% names(airline_sentiment))) {
      plot_ly(airline_sentiment, x = ~airline, y = ~Positive, type = "bar", name = "Positive", marker = list(color = "lightgreen")) %>%
        add_trace(y = ~Neutral, name = "Neutral", marker = list(color = "orange")) %>%
        add_trace(y = ~Negative, name = "Negative", marker = list(color = "maroon")) %>%
        layout(title = "Airline Sentiment Comparison", barmode = "stack", yaxis = list(title = "Count"), xaxis = list(title = "Airline"))
    } else {
      showNotification("Insufficient data.", type = "warning")
      return(plotly_empty(type = "scatter", mode = "markers", text = "Insufficient data."))
    }
  })

  observe({
    req(analyzed_data())
    sentiment_data <- analyzed_data()
    updateSelectInput(session, "airline_select", choices = unique(sentiment_data$airline))
  })

  output$airline_word_cloud <- renderWordcloud2({
    req(analyzed_data(), input$airline_select)
    sentiment_data <- analyzed_data()
    airline_data <- sentiment_data %>% filter(airline == input$airline_select)
    word_counts <- airline_data %>%
      unnest_tokens(word, text) %>%
      anti_join(tibble(word = my_stopwords), by = "word") %>%
      filter(nchar(word) > 3) %>%
      count(word, sort = TRUE)
    wordcloud2(word_counts, size = 0.7, minSize = 1)
  })

  output$airline_ranking_plot <- renderPlotly({
    req(analyzed_data())
    sentiment_data <- analyzed_data()
    if (is.null(sentiment_data)) return(NULL)
    airlines <- unique(sentiment_data$airline)
    attributes <- c("On-Time", "Service", "Comfort", "Meals", "Baggage")
    rankings <- matrix(runif(length(airlines) * length(attributes)), nrow = length(airlines))
    ranking_data <- data.frame(airlines, rankings)
    names(ranking_data) <- c("Airline", attributes)
    ranking_data_long <- ranking_data %>%
      pivot_longer(cols = attributes, names_to = "Attribute", values_to = "Ranking")
    overall_ranking <- ranking_data %>%
      mutate(Overall = rowMeans(select(., -Airline))) %>%
      arrange(desc(Overall))
    ranking_data_long$Airline <- factor(ranking_data_long$Airline, levels = overall_ranking$Airline)
    plot_ly(ranking_data_long, x = ~Attribute, y = ~Ranking, type = "bar", color = ~Airline, barmode = "group", colors = "viridis") %>%
      layout(title = "Airline Ranking", yaxis = list(title = "Ranking"))
  })

  output$raw_data <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 5))
  })
}
