# Text Analysis Shiny App Package

The Text Analysis Shiny App Package is a comprehensive R package that provides an interactive Shiny web application for analyzing and visualizing text data. This package enables users to perform sentiment analysis, word frequency analysis, and highlight specific words in the input text.

## Features

- **Sentiment Analysis:** Analyze the sentiment of the provided text using the sentimentr package, visualizing sentiments at both the overall and sentence levels.

- **Word Frequency Analysis:** Generate a word frequency table and visualize it using an interactive word cloud, allowing users to explore the most common words in the input text.

- **Word Highlighting:** Highlight positive and negative words in the text with customizable background and text colors for quick identification.

- **Interactive Visualization:** The Shiny app provides an interactive and user-friendly interface for performing text analysis tasks, allowing users to adjust settings and explore results in real-time.

## Installation

To install the Text Analysis Shiny App Package, you can use the `devtools` package:

```r
devtools::install_github("IsaacBravo/TextAnalizer")
```
Replace your_username with your GitHub username and your_package_name with your repository name.

## Usage
Load the package and initiate the Shiny app with the following commands:

```r
library(TextAnalizer)
TextAnalizer::text_analizer()
```
## Dependencies
This package depends on essential R packages, including:

* shiny
* tidyverse
* tidytext
* sentimentr
* wordcloud2




