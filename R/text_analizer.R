# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

text_analizer <- function() {
  # Load packages
  Packages <- c("tidyverse", "magrittr", "tidytext", "gt", "DT", "shiny",
                "shinythemes", "shinyBS", "shinyWidgets", "plotly", "ggpol",
                "reshape2", "textdata", "ggnewscale", "shinydashboard",
                "shinycssloaders", "reactable", "tidytext", "tm",
                "wordcloud2", "sentimentr", "RColorBrewer", "stringr",
                "shinythemes")
  lapply(Packages, library, character.only = TRUE)

  ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Text App"),
    sidebarLayout(
      sidebarPanel(
        textAreaInput("input_text", label = "Enter Text:", width = "100%", height = "200px"),
        tags$style(
          HTML("#input_text { width: 100%; }")
        ),
        textInput("highlight_words", label = "Highlight Words (comma-separated):"),
        br(),
        actionButton("highlight_positive", "Highlight Positive Words"),
        br(),
        br(),
        actionButton("highlight_negative", "Highlight Negative Words"),
        br(),
        br(),
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Highlight Text",
                   h3("Summary Text:"),
                   reactableOutput("summary_table"),
                   h3("Main Text:"),
                   uiOutput("highlighted_text")
          ),
          tabPanel("Text Analyzer",
                   h3("Explore Text:"),
                   align = "center",
                   actionButton("word_freq_button", "Show Word Frequency"),

                   fluidRow(
                     column(6,
                            reactableOutput("word_freq_table"),
                            align = "center"
                     ),
                     column(6,
                            wordcloud2Output("cloud"),
                            align = "center"
                     )
                   )
          ),
          tabPanel("Sentiment Analyzer",
                   h3("Sentiment Exploration:"),
                   actionButton("analyze_sentiment_button", "Analyze Sentiment"),
                   align = "center",
                   fluidRow(
                     column(12,
                            htmlOutput("sentiment_analysis_result"),  # Render the HTML content here
                            align = "center"
                     )
                   ),
                   br(),
                   fluidRow(
                     column(2),
                     column(8,
                            plotOutput("plot_sentiment"),
                            align = "center"
                     ),
                     column(2)
                   )
          )

        )
      )
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$highlight_positive, {
      positive_words <- tidytext::get_sentiments("bing") %>%
        filter(sentiment == "positive") %>%
        select(1) %>%
        pull
      highlight_text(positive_words, "#203864", "#DAE3F3")
    })

    observeEvent(input$highlight_negative, {
      negative_words <- c("bad", "poor", "sad", "negative",
                          "stalemate","abruptly", "disconcerted", "confession", "freaks",   "misdirection", "bruised",  "inessential", "harpy","forbidding", "garbage",  "poverty", "ludicrous","insinuate","ignominious",  "moody", "cave","acrimony", "inveigle", "chronic")  # Add your list of negative words

      highlight_text(negative_words, "#500000", "#FFD9D9")
    })

    observeEvent(input$word_freq_button, {
      req(input$input_text)  # Ensure input is available
      word_frequency()
    })

    observe({
      highlight_text()
    })

    highlight_text <- function(words = NULL, background_color = "#E2F0D9", text_color = "red", append = FALSE) {
      input_text <- input$input_text
      if (is.null(words)) {
        words <- character(0)
        if (!is.null(input$highlight_words)) {
          words <- strsplit(input$highlight_words, ",\\b")[[1]]
        }
      }

      highlighted_text <- input_text
      highlight_count <- 0

      for (word in words) {
        highlighted_text <- gsub(word, paste0("<span style='background-color: ", background_color, ";color:", text_color, "; font-weight: bold'>", word, "</span>"), highlighted_text, ignore.case = TRUE)
        highlight_count <- highlight_count + length(gregexpr(word, input_text, ignore.case = TRUE)[[1]])
      }

      # Add line breaks to the highlighted text
      highlighted_text <- gsub("\n", "<br>", highlighted_text)

      if (!append) {
        output$highlighted_text <- renderUI({
          HTML(highlighted_text)
        })
      }

      # Calculate summary statistics
      word_count <- length(unlist(strsplit(input_text, "\\s+")))
      char_count <- nchar(gsub("\\s+", "", input_text))
      sentence_count <- length(gregexpr("[.!?]", input_text)[[1]]) - 1

      data.frame(
        "Statistic" = c("Word Count", "Character Count", "Sentence Count"),
        "Count" = c(word_count, char_count, sentence_count)
      )
    }

    word_frequency <- function() {
      req(input$input_text)  # Ensure input is available

      input_text <- tolower(input$input_text)

      # Remove punctuation
      input_text <- gsub("[[:punct:]]", "", input_text)

      corpus <- Corpus(VectorSource(input_text))
      corpus <- tm_map(corpus, content_transformer(tolower))
      dtm <- DocumentTermMatrix(corpus)
      freq_matrix <- as.matrix(dtm)
      word_freq <- colSums(freq_matrix)

      all_word_freq_df <- data.frame(
        Word = names(word_freq),
        Frequency = word_freq,
        stringsAsFactors = FALSE
      )

      output$word_freq_table <- renderReactable({
        reactable(all_word_freq_df, showPageInfo = TRUE, searchable = TRUE,
                  filterable = TRUE, showPageSizeOptions = TRUE,
                  rownames = FALSE,
                  pageSizeOptions = c(12, 24),
                  defaultPageSize = 12, bordered = TRUE, striped = TRUE, highlight = TRUE)
      })
    }

    output$cloud <- renderWordcloud2({
      req(input$input_text)
      input_text <- tolower(input$input_text)

      # Remove punctuation
      input_text <- gsub("[[:punct:]]", "", input_text)

      corpus <- Corpus(VectorSource(input_text))
      corpus <- tm_map(corpus, content_transformer(tolower))
      dtm <- DocumentTermMatrix(corpus)
      freq_matrix <- as.matrix(dtm)
      word_freq <- colSums(freq_matrix)

      all_word_freq_df <- data.frame(
        Word = names(word_freq),
        Frequency = word_freq,
        stringsAsFactors = FALSE
      )

      # Filter out words with lower frequencies for the word cloud
      min_freq <- 2
      filtered_word_freq_df <- all_word_freq_df[all_word_freq_df$Frequency >= min_freq, ]

      wordcloud2::wordcloud2(filtered_word_freq_df)

    })

    output$plot_sentiment <- renderPlot({
      req(input$input_text)
      input_text <- tolower(input$input_text)

      input_text %>%
        get_sentences() %>%
        sentiment() %>%
        plot() +
        theme_minimal() +
        labs(title = "Plotting Valence at the Sentence Level") +
        theme(plot.title = element_text(face = "bold"))

    })


    output$summary_table <- renderReactable({
      reactable(highlight_text(), showPageInfo = FALSE, searchable = FALSE, bordered = TRUE, striped = TRUE, highlight = TRUE)
    })


    output$summary_sent_table <- renderReactable({
      req(input$input_text)
      input_text <- tolower(input$input_text)

      input_text %>%
        get_sentences() %>%
        sentiment() %>%
        reactable(
          showPageInfo = FALSE, searchable = FALSE, bordered = TRUE, striped = TRUE, highlight = TRUE)
    })


    output$highlighted_text <- renderUI({
      highlight_text(append = TRUE)
      HTML(highlighted_text)
    })


    output$sentiment_analysis_result <- renderText({
      req(input$analyze_sentiment_button)  # Ensure the button is clicked

      # Call the highlight function to generate the HTML content
      sentiment_highlighted_text <-
        get_sentences(input$input_text) %>%
        sentiment_by() %>%
        highlight()

      # Return the HTML content
      return(sentiment_highlighted_text)
    })

  }

  shinyApp(ui, server)

}
