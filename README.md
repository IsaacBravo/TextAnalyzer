# Text Analysis Shiny App Package

The Text Analyzer Shiny App is an interactive tool for analyzing and visualizing text data. It allows users to perform various text analysis tasks, including sentiment analysis, word frequency analysis, and word highlighting. This README provides an overview of the app's features, how to use it, and how to get started.

## Features

-   **Sentiment Analysis:** Analyze the sentiment of the provided text using the sentimentr package. Visualize sentiments at both the overall and sentence levels.

-   **Word Frequency Analysis:** Generate a word frequency table and visualize it using an interactive word cloud. Explore the most common words in the input text.

-   **Word Highlighting:** Highlight positive and negative words in the text with customizable background and text colors for quick identification.

-   **Interactive Visualization:** The Shiny app provides an interactive and user-friendly interface for performing text analysis tasks, allowing users to adjust settings and explore results in real-time.

-   **Topic Analysis (Experimental):** Perform topic modeling using Latent Dirichlet Allocation (LDA). Visualize topics and their associated terms.

## Installation

To install the Text Analysis Shiny App Package, you can use the `devtools` package:

``` r
devtools::install_github("IsaacBravo/TextAnalizer")
```

Replace your_username with your GitHub username and your_package_name with your repository name.

## Usage

Load the package and initiate the Shiny app with the following commands:

``` r
library(TextAnalizer)
TextAnalizer::text_analizer()
```

## Features

The Shiny Text Analyzer App offers the following features:

### Data Tab

-   Data Input: Enter or copy text into the text area for analysis.
-   Word Highlighting: Highlight specific words or phrases based on valence.
-   Topic Exploration: Specify the number of topics present in your text.
-   Sentiment Lexicon: Choose from different sentiment lexicons for analysis.

### Highlight Text

-   Display a summary of the text with highlighted keywords.
-   Visualize the main text with highlighted words based on valence.

### Text Analyzer

-   Explore the text with features like word frequency analysis and visualization.
-   Generate an interactive word cloud to visualize the most common words.
-   Analyze and visualize named entities in the text.

### Sentiment Analyzer

-   Analyze sentiment at both the overall and sentence levels.
-   Highlight positive and negative sentences in the text.
-   Visualize valence trends across the text.

### Topic Analyzer

-   Explore topics within the text using Latent Dirichlet Allocation (LDA).
-   Perform topic modeling with options for testing and analysis.
-   Visualize topics with interactive plots and word clouds.

## Dependencies

This package depends on essential R packages, including:

``` r
# Install required packages
Packages <- c("tidyverse", "magrittr", "tidytext", "gt", "DT", "shiny",
            "shinyBS", "shinyWidgets", "plotly", "ggpol",
            "reshape2", "textdata", "ggnewscale", "shinydashboard",
            "shinycssloaders", "reactable", "tidytext", "tm",
            "wordcloud2", "sentimentr", "RColorBrewer", "stringr",
            "shinythemes", "cleanNLP", "topicmodels", "ggwordcloud")
lapply(Packages, library, character.only = TRUE)
```
"# TextAnalyzer" 
