[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

[![CRAN status](https://www.r-pkg.org/badges/version/TextAnalyzer)](https://CRAN.R-project.org/package=TextAnalyzer)

# Text Analysis Shiny App Package

The Text Analyzer Shiny App is an interactive tool for analyzing and visualizing text data. It allows users to perform various text analysis tasks, including sentiment analysis, word frequency analysis, and topic modelling. This README provides an overview of the app's features, how to use it, and how to get started.

This app focuses on making life easier for researchers looking to explore insights in their text analysis. Equipped to perform descriptive analysis, sentiment, emotion, topic modelling (LDA and seededLDA), it provides an excellent starting point for researchers and the general public with minimal or some programming experience.

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
devtools::install_github("IsaacBravo/TextAnalyzer")
```

Replace your_username with your GitHub username and your_package_name with your repository name.

## Usage

Load the package and initiate the Shiny app with the following commands:

``` r
library(TextAnalyzer)
TextAnalyzer::run_TextAnalyzer()
```

## Citation

Please cite TextAnalyzer if you use it for your publications:

      Isaac Bravo (2024). TextAnalyzer: A Shiny Application for Automatic Text Analysis
      https://github.com/IsaacBravo/TextAnalyzer

A BibTeX entry for LaTeX users is:

      @Manual{,
        title = {TextAnalyzer: A Shiny Application for Automatic Text Analysis},
        author = {Isaac Bravo},
        year = {2024},
        url = {https://github.com/IsaacBravo/TextAnalyzer},
      }
