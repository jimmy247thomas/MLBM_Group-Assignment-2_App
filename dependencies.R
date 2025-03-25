# List of required packages
required_packages <- c("shiny", "tidyverse", "sentimentr", "plotly", "DT", "wordcloud2", "tidytext", "stopwords", "shinythemes")

# Function to check and install missing packages
install_missing_packages <- function(packages) {
  installed <- rownames(installed.packages())
  missing <- setdiff(packages, installed)
  if (length(missing) > 0) {
    print("Installing missing packages...")
    install.packages(missing)
  } else {
    print("All required packages are already installed.")
  }
}

# Install missing packages
install_missing_packages(required_packages)

# Load required libraries (optional here, can also be in server.R)
# library(shiny)
# library(tidyverse)
# library(sentimentr)
# library(plotly)
# library(DT)
# library(wordcloud2)
# library(tidytext)
# library(stopwords)
# library(shinythemes)
