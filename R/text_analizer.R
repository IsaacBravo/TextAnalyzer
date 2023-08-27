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

  source("setup.R")
  source("ui_sidebar.R")
  source("ui_about.R")

  load_libraries()

  ui <- navbarPage(
    theme = shinythemes::shinytheme("flatly"),

    ###############################################.
    ## Header ----
    ###############################################.

    "Text Analyzer App",

    ###############################################.
    ## Sidebar & Data Panel ----
    ###############################################.

    tabPanel("Data", icon = icon("table"), value = "table",
             sidebar_ui()),

    ###############################################.
    ## About Panel ----
    ###############################################.

    tabPanel("About", icon = icon("info"), value = "info",
             ui_about_part1(),
             ui_about_part2()
    )
  )

  server <- function(input, output, session) {

    ###############################################.
    ## ObserveEvents ----
    ###############################################.

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

    observeEvent(input$plot_freq_button, {
      req(input$input_text)  # Ensure input is available
      plt_table_udipe()
      word_frequency_cloud()
    })

    observeEvent(input$word_freq_button, {
      req(input$input_text)  # Ensure input is available
      word_frequency()
      table_entities_udipe()
    })

    observeEvent(input$input_text, {
      # Get the sentences from the input text
      req(input$input_text)

      sentences_df <-
        sentiment(input$input_text) %>%
        rename("sentence_ID" = "element_id") %>%
        rename("text_ID" = "sentence_id")

      # Output the extracted sentences
      output$table_sentences <- renderReactable({
        reactable(sentences_df, showPageInfo = TRUE, searchable = TRUE,
                  filterable = TRUE, showPageSizeOptions = TRUE,
                  rownames = FALSE,
                  pageSizeOptions = c(12, 24),
                  defaultPageSize = 12, bordered = TRUE, striped = TRUE, highlight = TRUE)
      })
    })

    observeEvent(input$analyze_sentiment_button, {
      req(input$input_text)  # Ensure input is available

      input$input_text %>%
        sentiment_by() %>%
        sentimentr:: highlight()
    })


    observe({
      highlight_text()
    })

    ###############################################.
    ## Server Functions ----
    ###############################################.

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
      sentence_count <- length(gregexpr("[.!?]", input_text)[[1]])

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

    table_entities_udipe <- function() {
      req(input$input_text)  # Ensure input is available

      annotation <- cnlp_annotate(input = input$input_text)

      annotation_df <- annotation$token %>%
        select(id = doc_id, token, lemma, entity = upos)

      output$table_entities <- renderReactable({
        reactable(annotation_df, showPageInfo = TRUE, searchable = TRUE,
                  filterable = TRUE, showPageSizeOptions = TRUE,
                  rownames = FALSE,
                  pageSizeOptions = c(12, 24),
                  defaultPageSize = 12, bordered = TRUE, striped = TRUE, highlight = TRUE)
      })
    }

    plt_table_udipe <- function() {
      req(input$input_text)  # Ensure input is available

      annotation <- cnlp_annotate(input = input$input_text)

      output$plot_entities <- renderPlot({

        annotation$token %>%
          select(id = doc_id, token, lemma, entity = upos) %>%
          group_by(entity) %>%
          summarize(total = n()) %>%
          mutate(entity = fct_reorder(entity, total)) %>%
          ggplot( aes(x=entity, y=total)) +
          geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
          coord_flip() +
          xlab("Entities") +
          xlab("Frequency") +
          theme_bw()
      })
    }

    word_frequency_cloud <- function() {
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

      output$cloud <- renderWordcloud2({wordcloud2::wordcloud2(filtered_word_freq_df)})

    }

    ###############################################.
    ## server Outputs ----
    ###############################################.

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

  }


  shinyApp(ui, server)


}
