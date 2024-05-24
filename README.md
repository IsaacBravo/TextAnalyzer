# Text Analysis Shiny App Package

The Text Analyzer Shiny App is an interactive tool for analyzing and visualizing text data. It allows users to perform various text analysis tasks, including sentiment analysis, word frequency analysis, and topic modelling. This README provides an overview of the app's features, how to use it, and how to get started.

## Features

-   **Select data type:** The user can decide whether to copy a text fragment or import a data frame (.xlsx, .csv, .txt, .json).

-   **Data Overview:** Provides a first overview of the main data elements (Frequencies, Graphs, Entity Object Recognition).

-   **Sentiment Analysis:** Explore some common sentiment dictionaries to explore the data (Bing, NRC, Loughran-McDonald & Afinn).

-   **Emotion Analysis:** Check other levels in your data using dictionaries to detect emotions such as NRC & Loughran-McDonald.

-   **Arousal & Valence Analysis:** Explore levels of arousal, valence and dominance in your data.

-   **Topic Modelling:** Run an LDA model evaluating the best number of K for your data, & explore some visualisations.

-   **SeededLDA:** Explore your data in depth by defining your own topics and test them on the dataset.



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
