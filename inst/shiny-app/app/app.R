pacman::p_load(ggpol, reshape2, ggnewscale, sentimentr, RColorBrewer, topicmodels,
               shiny, reactable, tidytext, DT, seededlda,
               shinythemes, shinyWidgets, tm, fontawesome, htmltools, shinyBS,
               highcharter, cleanNLP, shinydashboard,
               textdata, tidytext, udpipe, quanteda, tidyverse)

cnlp_init_udpipe()

options(scipen = 99999)


sentiment_highlighter <- function(text,
                                  lexicon = c('nrc', 'bing', 'loughran', 'afinn'),
                                  token = 'words',
                                  lexicon_dir = '.') {
  colors <- NULL
  sentiments <- NULL
  if(lexicon[1] == 'nrc') {

    sentiments <- readRDS("./lexicons/df_sentiment.RDS") |>
      rename('sentiment' = 'emotion')

  } else if(lexicon[1] == 'bing') {

    sentiments <- tidytext::get_sentiments("bing")

  } else if(lexicon[1] == 'afinn') {

    sentiments <- readRDS("./lexicons/afinn_sentiment.rds") |>
      mutate(sentiment = ifelse(value < 0, "negative", "positive"))

    } else if(lexicon[1] == 'loughran') {

    sentiments <- readRDS("./lexicons/LoughranMcDonald.rds")

  } else {
    stop(paste0('Unknown lexicon: ', lexicon[1]))
  }

  new_colors <- c(negative = "#CC687F", positive = "#D9F2D0")

  colors_df <- data.frame(sentiment = names(new_colors),
                          color = new_colors,
                          row.names = NULL)

  paragraphs <- strsplit(text, '\n')[[1]]

  tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                       text = paragraphs) |>
    dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
    tidytext::unnest_tokens(token, text,
                            token = 'words',
                            to_lower = FALSE,
                            strip_punct = FALSE) |>
    dplyr::mutate(token_lower = tolower(token)) |>
    dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all') |>
    dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))


  tokens$html <- tokens$token
  sentiment_rows <- !is.na(tokens$color)
  tokens[sentiment_rows,]$html <- paste0("<span style='background-color: ",
                                         tokens[sentiment_rows,]$color,
                                         "; word-wrap: break-word; display: inline; white-space: pre-wrap; color: black' class='tooltip2'>", tokens[sentiment_rows,]$token, "</span>")
  html <- ''
  for(i in seq_len(length(paragraphs))) {
    html <- paste0(html, '<p>',
                   paste0(tokens[tokens$paragraph == i,]$html, collapse = ' '),
                   '</p>')
  }
  html <- paste0(tokens$html, collapse = ' ')
  return(html)
}

#tema automático
thematic::thematic_shiny(font = "auto", bg = "#191919", fg = "#8eccad", accent = "#268053")

# UI
ui <- fluidPage(

  fluidRow(
    column(1,  style = "width: 5px;"),
    column(11,
           br(),
           h3("TextAnalizer Shiny App", icon("file-text"),
              class = "data-main-title"),
           hr(),
           p("This Shiny App is designed to help you import, process, and analyze text data from your file or text efficiently. ", "It provides a user-friendly interface to perform various tasks related to text analysis, guided search, and more."),
           p("If you want access to the repository of this package, see ", a("here.", href = "https://github.com/IsaacBravo/ShinyNews", target = "_blank", class = "here-pop-up", id = "here")),
           HTML(paste("<p>(Made by ", paste(" <a href='https://github.com/IsaacBravo'>@Isaac_Bravo</a>", icon("github"),". Source code <a href='https://github.com/IsaacBravo/ShinyNews'>on GitHub</a>)</p>")))
    ),
    br()
  ),
  fluidRow(
    column(1,  style = "width: 5px;"),
    column(11,
           br(),
           h3("Features:"),
           hr(),
           p("This app focuses on making life easier for researchers looking to explore insights in their text analysis. Equipped to perform descriptive analysis, sentiment, emotion, topic modelling (LDA and seededLDA), it provides an excellent starting point for researchers and the general public with minimal or some programming experience."),
           p(HTML("Explore the key features of the <b>TextAnalizer App</b>:"),
             tags$ul(
               id = "wellPanelId2",
               class = "custom-well-panel-home1",
               br(),
               tags$li(strong("Select data type:"), "The user can decide whether to copy a text fragment or import a data frame (.xlsx, .csv, .txt, .json)."),
               tags$li(strong("Data Overview:"), "Provides a first overview of the main data elements (Frequencies, Graphs, Entity Object Recognition)."),
               tags$li(strong("Sentiment Analysis:"), "Explore some common sentiment dictionaries to explore the data (Bing, NRC, Loughran-McDonald & Afinn)."),
               tags$li(strong("Emotion Analysis:"), "Check other levels in your data using dictionaries to detect emotions such as NRC & Loughran-McDonald."),
               tags$li(strong("Arousal & Valence Analysis:"), "Explore levels of arousal, valence and dominance in your data."),
               tags$li(strong("Topic Modelling:"), "Run an LDA model evaluating the best number of K for your data, & explore some visualisations."),
               tags$li(strong("SeededLDA:"), "Explore your data in depth by defining your own topics and test them on the dataset. "),
               br()
             )),
           p(icon("info-circle"), HTML("<b>Note</b>: For the lexicons NRC (sentiment, emotion, valence, arousal & dominance), this app rely on the lexicons developed by Hipson & Mohammad (2021). <b>Citation:</b> Hipson WE, Mohammad SM (2021) Emotion dynamics in movie dialogues. PLoS ONE 16(9): e0256153. <a href='https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0256153'>https://doi.org/10.1371/journal.pone.0256153</a>")),
           hr()
    )),

  theme = bslib::bs_theme(
    bg = "#191919", fg = "#8eccad", primary = "#268053",
  ),

  tags$head(
    tags$script(src = "https://platform.twitter.com/widgets.js", charset = "utf-8"),
    tags$link(href = "https://fonts.googleapis.com/css?family=Roboto+Mono", rel = "stylesheet"),
    tags$style(HTML('
      * {
        font-family: Roboto Mono;
        font-size: 100%;
      }
      #sidebar {
         background-color: #fff;
         border: 0px;
         width: 50px;
      }
      .rt-th {
        display: none;
      }
      .rt-noData {
        display: none;
      }
      .rt-pagination-nav {
        float: left;
        width: 100%;
      }
      h4 {
        font-weight: bold;
      }
      #custom-hr {
        border: 1px solid #2C3E50;
      }
      .inline-columns {
        display: inline-block;
      }
      .btn-summary {
        color: #000;
        background-color: #00bc8c;
        border-radius: 20px;
      }
      .btn-summary:hover {
        background-color: #025839; /* Green */
        color: white;
      }
      .btn-warning-new {
        color: #000;
        background-color: #f0ad4e;
        border-radius: 20px;
      }
      .btn-warning-new:hover {
        background-color: #995e0d;
        color: #000;
      }
      .btn-file {
        color: #000;
        background-color: #00bc8c;
      }
      .btn-file:hover {
        background-color: #025839; /* Green */
        color: white;
      }
      .progress-bar {
        background-color: #f0ad4e;
      }
      textArea {
       background-color: #dcdcdc !important;
       border: 5px solid #808080 !important;
       color: #000 !important;
      }
      #expr-container .form-control {
        background-color: #dcdcdc;
        border: 5px solid #808080;
        border-radius: 15px;
        color: #000;
      }
    '))),



  sidebarLayout(
    sidebarPanel(

      width = 2,
      selectInput("input_type", "Select Input Data Type:",
                  choices = c("Text", "File"), selected = ""),
      conditionalPanel(
        condition = "input.input_type == 'Text'",
        textAreaInput("input_text", label = "Enter Text:", width = "100%", height = "200px"),
        hr(),
        div(style="text-align: center;", actionButton("reset_button", p("Reset", icon("refresh")), class = "btn-warning-new")),
        hr(),
        tags$style(
          HTML("#input_text { width: 100%; }")
        )),
      conditionalPanel(
        condition = "input.input_type == 'File'",
        fileInput("upload", NULL, buttonLabel = "Upload...", accept = c(".csv", ".xls", ".xlsx", ".json", ".rds", ".rdata")),
        hr(),
        div(style="text-align: center;", actionButton("reset_button_file", p("Reset", icon("refresh")), class = "btn-warning-new"))

      )


    ),
    mainPanel(

      width = 10,

      #------------- File Input Text ------------------------------------------#
      conditionalPanel(
        condition = "input.input_type == 'Text'",
        tabsetPanel(
          tabPanel("General Data View:",
                   br(),
                   h4("Explore Text:"),
                   align = "center",
                   fluidRow(
                     column(12,
                            br(),
                            p(HTML("<b>Select the type of analysis that you want explore, checking out these buttons!</b>")),
                            align = "center")
                   ),
                   hr(),
                   fluidRow(
                     column(1),
                     column(3, actionButton("summary_button", "Show Summary Text", class = "btn-summary"),
                            hr(),
                            downloadButton("download_summary", "Download Summary as Excel")),
                     column(1),
                     column(3, actionButton("word_freq_button", "Show Entities & Frequencies", class = "btn-summary"),
                            hr(),
                            downloadButton("download_annotation", "Download Results as Excel")),
                     column(1),
                     column(3, actionButton("word_plot_button", "Show Interactive Visual", class = "btn-summary")),
                     column(1)
                   ),
                   hr(),
                   uiOutput("highlighted_text"),
                   uiOutput("summary_table"),
                   uiOutput("both_outputs"),
                   uiOutput("both_plots")),

          tabPanel("Sentiment Analysis:",
                   align = "center",
                   fluidRow(
                     column(12,
                            br(),
                            p(HTML("<b>Select the type of analysis that you want explore, checking out these buttons!</b>")),
                            align = "center")
                   ),
                   br(),
                   fluidRow(
                     column(2),
                     column(1, actionButton("bing_button", "Bing", class = "btn-summary"),
                            hr(),
                            downloadButton("download_bing", "Download Results as Excel")),
                     column(1),
                     column(1, actionButton("nrc_button", "NRC", class = "btn-summary"),
                            hr(),
                            downloadButton("download_nrc", "Download Results as Excel")),
                     column(1),
                     column(2, actionButton("loughran_button", "Loughran-McDonald", class = "btn-summary"),
                            hr(),
                            downloadButton("download_loughran", "Download Results as Excel")),
                     column(1),
                     column(1, actionButton("afinn_button", "AFINN", class = "btn-summary"),
                            hr(),
                            downloadButton("download_afinn", "Download Results as Excel")),
                     column(2)
                   ),
                   br(),
                   br(),
                   fluidRow(
                     column(4),
                     column(4,       selectInput('sentiment_lexicon',
                                                 choices = c('Bing binary sentiment' = 'bing',
                                                             'NRC Word-Emotion Association' = 'nrc',
                                                             'Loughran-McDonald Sentiment' = 'loughran',
                                                             'AFINN-111 dataset' = 'afinn'),
                                                 selected = 'bing',
                                                 label = 'Sentiment lexicon:')),
                     column(4)
                   ),
                   hr(),
                   uiOutput("sentiment_text"),
                   hr(),
                   uiOutput("bing_plots"),
                   uiOutput("nrc_plots"),
                   uiOutput("loughran_plots"),
                   uiOutput("afinn_plots")
          ),
          tabPanel("Emotion Analysis:",
                   align = "center",
                   fluidRow(
                     column(12,
                            br(),
                            p(HTML("<b>Select the type of analysis that you want explore, checking out these buttons!</b>")),
                            align = "center")
                   ),
                   br(),
                   fluidRow(
                     column(3),
                     column(2, actionButton("nrc_emotion_button", "NRC", class = "btn-summary"),
                            hr(),
                            downloadButton("download_emotion_nrc", "Download Results as Excel")),
                     column(3),
                     column(2, actionButton("loughran_emotion_button", "Loughran-McDonald", class = "btn-summary"),
                            hr(),
                            downloadButton("download_emotion_loughran", "Download Results as Excel")),
                     column(1)
                   ),
                   br(),
                   hr(),
                   uiOutput("nrc_emotion_plots"),
                   uiOutput("loughran_emotion_plots"),
          ),
          tabPanel("Arousal & Valence Analysis:",
                   align = "center",
                   fluidRow(
                     column(12,
                            br(),
                            p(HTML("<b>Select the type of analysis that you want explore, checking out these buttons!</b>")),
                            align = "center")
                   ),
                   br(),
                   fluidRow(
                     column(2),
                     column(2, actionButton("arousal_button", "Arousal Level", class = "btn-summary"),
                            hr(),
                            downloadButton("download_arousal", "Download Results as Excel")),
                     column(1),
                     column(2, actionButton("valence_button", "Valence Level", class = "btn-summary"),
                            hr(),
                            downloadButton("download_valence", "Download Results as Excel")),
                     column(1),
                     column(2, actionButton("dominance_button", "Dominance Level", class = "btn-summary"),
                            hr(),
                            downloadButton("download_dominance", "Download Results as Excel")),
                     column(1)
                   ),
                   br(),
                   hr(),
                   uiOutput("arousal_plots"),
                   uiOutput("valence_plots"),
                   uiOutput("dominance_plots")
          ),



          tabPanel("Topic Modelling (LDA):",
                   align = "center",
                   fluidRow(
                     column(3, br(),
                            p(HTML("<b>Check the best number of topics for your data:</b>")),
                            br(),
                            align = "center",
                            actionButton("lda_test_button", "Check Number of Topics", class = "btn-summary"),
                            hr(),
                            downloadButton("download_lda_test", "Download Results")),
                     column(3, br(),
                            sliderInput("numTopics",
                                        p(HTML("<b>Select the number of topics that you want to check in your data:</b>")),
                                        min = 2,
                                        max = 10,
                                        value = 3)
                     ),
                     column(3, br(),
                            p(HTML("<b>Run LDA Topic Model:</b>")),
                            align = "center",
                            br(),
                            actionButton("lda_button", "Run LDA Model", class = "btn-summary"),
                            hr(),
                            downloadButton("download_lda_results", "Download Results")),
                     column(3, br(),
                            p(HTML("<b>Check Topic Evaluation:</b>")),
                            align = "center",
                            br(),
                            actionButton("lda_evaluation_button", "Run Evaluation Model", class = "btn-summary"),
                            hr(),
                            downloadButton("download_lda_eval", "Download Results"))
                   ),
                   br(),
                   uiOutput("test_k_plot"),
                   br(),
                   uiOutput("topics"),
                   br(),
                   uiOutput("topics_pie"),
                   br(),
                   uiOutput("topics_evaluation"),
                   br(),
                   uiOutput("topics_pieTop")
          ),

          tabPanel("Seeded (LDA):",
                   align = "center",
                   br(),
                   h5(HTML("<b>Select Dictionary Type:</b>")),
                   br(),
                   # User selection for dictionary configuration
                   radioButtons("dict_type", "",
                                choices = c("Use Default Dictionary" = "default_dictionary",
                                            "Create New Dictionary" = "create_dictionary"),
                                selected = "default_dictionary",
                                inline = F),

                   # Main layout for conditional UI elements
                   fluidRow(
                     # Conditional panel for default dictionary
                     conditionalPanel(
                       condition = "input.dict_type === 'default_dictionary'",
                       column(12, align = "center",
                              actionButton("seededlda_button_file_default", "Run Seeded LDA Model", class = "btn-summary"))
                     ),

                     # Conditional panel for creating a new dictionary
                     # Conditional panel for creating a new dictionary
                     conditionalPanel(
                       condition = "input.dict_type === 'create_dictionary'",
                       # UI for creating a new dictionary
                       fluidRow(
                         br(),
                         fluidRow(
                           column(2),
                           column(3, align = "center", textInput("category_name", "Enter Category Name:")),
                           column(2),
                           column(3, textAreaInput("related_words", "Add Related Words (comma-separated):", height = '100px')),
                           column(2)
                         ),
                         br(),
                         fluidRow(
                           column(4),
                           column(5, align = "center",
                                  br(),
                                  actionButton("create_dict", "Create/Display Dictionary", style = "margin-top: 20px;", class = "btn-summary"),
                                  hr(),
                                  verbatimTextOutput("display_dict"),
                                  hr()),
                           column(3)),
                         br(),
                         fluidRow(
                           column(4),
                           column(5, align = "center", actionButton("seededlda_button_file_custom", "Run Seeded LDA Model", class = "btn-summary")),
                           column(3)
                         )
                       ))
                   ),
                   br(),
                   uiOutput("ldaTable"),
                   br(),
                   uiOutput("ldaTable2")

          )
        )),




      #------------- File Input File ------------------------------------------#

      conditionalPanel(
        condition = "input.input_type == 'File'",
        tabsetPanel(
          tabPanel("General Data View:",
                   br(),
                   h4("Explore File:"),
                   align = "center",
                   fluidRow(
                     column(12,
                            br(),
                            p(HTML("<b>Select the type of analysis that you want explore, checking out these buttons!</b>")),
                            align = "center")
                   ),
                   hr(),
                   fluidRow(
                     column(2, actionButton("show_table_button", "Show Data", class = "btn-summary")),
                     column(1),
                     column(2, actionButton("summary_file_button", "Show Summary Data", class = "btn-summary")),
                     column(1),
                     column(2, actionButton("word_freq_file_button", "Show Entities & Frequencies", class = "btn-summary")),
                     column(1),
                     column(2, actionButton("word_plot_file_button", "Show Interactive Visual", class = "btn-summary")),
                   ),
                   fluidRow(
                     column(2),
                     column(1),
                     column(2),
                     column(1),
                     column(2, br(), tags$div(id = "expr-container", textInput("select_column", "Select Column to Explore"))),
                     column(1),
                     column(2, br(), tags$div(id = "expr-container", textInput("select_column2", "Select Column to Explore")))
                   ),
                   hr(),
                   uiOutput("summary_file_table"),
                   uiOutput("show_data"),
                   uiOutput("both_outputs_data"),
                   uiOutput("both_plots_data")
          ),

          tabPanel("Sentiment Analysis:",
                   align = "center",
                   fluidRow(
                     column(12,
                            br(),
                            p(HTML("<b>Select the column & type of analysis that you want explore, checking out these buttons!</b>")),
                            align = "center")
                   ),
                   br(),
                   fluidRow(
                     column(2),
                     column(1, actionButton("bing_button_file", "Bing", class = "btn-summary")),
                     column(1),
                     column(1, actionButton("nrc_button_file", "NRC", class = "btn-summary")),
                     column(1),
                     column(2, actionButton("loughran_button_file", "Loughran-McDonald", class = "btn-summary")),
                     column(1),
                     column(1, actionButton("afinn_button_file", "AFINN", class = "btn-summary")),
                     column(2)
                   ),
                   fluidRow(
                     column(2),
                     column(1, br(), tags$div(id = "expr-container", textInput("select_column3", ""))),
                     column(1),
                     column(1, br(), tags$div(id = "expr-container", textInput("select_column4", ""))),
                     column(1),
                     column(2, br(), tags$div(id = "expr-container", textInput("select_column5", ""))),
                     column(1),
                     column(1, br(), tags$div(id = "expr-container", textInput("select_column6", ""))),
                     column(2)
                   ),
                   fluidRow(
                     column(2),
                     column(1, hr(), downloadButton("download_bing_file", "Download Results as Excel")),
                     column(1),
                     column(1, hr(), downloadButton("download_nrc_file", "Download Results as Excel")),
                     column(1),
                     column(2, hr(), downloadButton("download_loughran_file", "Download Results as Excel")),
                     column(1),
                     column(1, hr(), downloadButton("download_afinn_file", "Download Results as Excel")),
                     column(2)
                   ),
                   br(),
                   hr(),
                   uiOutput("bing_plots_file"),
                   uiOutput("nrc_plots_file"),
                   uiOutput("loughran_plots_file"),
                   uiOutput("afinn_plots_file")
          ),
          tabPanel("Emotion Analysis:",
                   align = "center",
                   fluidRow(
                     column(12,
                            br(),
                            p(HTML("<b>Select the type of analysis that you want explore, checking out these buttons!</b>")),
                            align = "center")
                   ),
                   br(),
                   fluidRow(
                     column(3),
                     column(1, actionButton("nrc_emotion_button_file", "NRC", class = "btn-summary")),
                     column(3),
                     column(2, actionButton("loughran_emotion_button_file", "Loughran-McDonald", class = "btn-summary")),
                     column(3)
                   ),
                   fluidRow(
                     column(3),
                     column(1, br(), tags$div(id = "expr-container", textInput("select_column7", ""))),
                     column(3),
                     column(2, br(), tags$div(id = "expr-container", textInput("select_column8", ""))),
                     column(3)
                   ),
                   br(),
                   hr(),
                   uiOutput("nrc_emotion_plots_file"),
                   uiOutput("loughran_emotion_plots_file"),
          ),
          tabPanel("Arousal & Valence Analysis:",
                   align = "center",
                   fluidRow(
                     column(12,
                            br(),
                            p(HTML("<b>Select the type of analysis that you want explore, checking out these buttons!</b>")),
                            align = "center")
                   ),
                   br(),
                   fluidRow(
                     column(2),
                     column(1, actionButton("arousal_button_file", "Arousal Level", class = "btn-summary")),
                     column(2),
                     column(1, actionButton("valence_button_file", "Valence Level", class = "btn-summary")),
                     column(2),
                     column(1, actionButton("dominance_button_file", "Dominance Level", class = "btn-summary")),
                     column(2)
                   ),
                   fluidRow(
                     column(2),
                     column(1, br(), tags$div(id = "expr-container", textInput("select_column9", ""))),
                     column(2),
                     column(1, br(), tags$div(id = "expr-container", textInput("select_column10", ""))),
                     column(2),
                     column(1, br(), tags$div(id = "expr-container", textInput("select_column11", ""))),
                     column(2)
                   ),
                   br(),
                   hr(),
                   uiOutput("arousal_plots_file"),
                   uiOutput("valence_plots_file"),
                   uiOutput("dominance_plots_file")
          ),
          tabPanel("Topic Modelling (LDA):",
                   align = "center",
                   fluidRow(
                     column(3, br(), tags$div(id = "expr-container", textInput("select_column12", p(HTML("<b>Select the column text</b>"))))),
                     column(2, br(),
                            p(HTML("<b>Check the best number of topics for your data:</b>")),
                            br(),
                            align = "center",
                            actionButton("lda_test_button_file", "Check Number of Topics", class = "btn-summary"),
                            hr(),
                            downloadButton("download_lda_test_file", "Download Results")),
                     column(3, br(),
                            sliderInput("numTopics2",
                                        p(HTML("<b>Select the number of topics that you want to check in your data:</b>")),
                                        min = 2,
                                        max = 10,
                                        value = 3)
                     ),
                     column(2, br(),
                            p(HTML("<b>Run LDA Topic Model:</b>")),
                            align = "center",
                            br(),
                            actionButton("lda_button_file", "Run LDA Model", class = "btn-summary"),
                            br(),
                            br(),
                            hr(),
                            downloadButton("download_lda_results_file", "Download Results")),
                     column(2, br(),
                            p(HTML("<b>Check Topic Evaluation:</b>")),
                            align = "center",
                            br(),
                            actionButton("lda_evaluation_button_file", "Run Evaluation Model", class = "btn-summary"),
                            hr(),
                            downloadButton("download_lda_eval_file", "Download Results"))
                   ),
                   br(),
                   uiOutput("test_k_plot_file"),
                   br(),
                   uiOutput("topics_file"),
                   br(),
                   uiOutput("topics_pie_file"),
                   br(),
                   uiOutput("topics_evaluation_file"),
                   br(),
                   uiOutput("topics_pieTop_file")

          ),

          tabPanel("Seeded (LDA):",
                   align = "center",
                   br(),
                   h5(HTML("<b>Select the column text:</b>")),
                   tags$div(id = "expr-container", textInput("select_column13", "")),
                   br(),
                   h5(HTML("<b>Select Dictionary Type:</b>")),
                   br(),
                   # User selection for dictionary configuration
                   radioButtons("dict_type2", "",
                                choices = c("Use Default Dictionary" = "default_dictionary",
                                            "Create New Dictionary" = "create_dictionary"),
                                selected = "default_dictionary",
                                inline = F),

                   # Main layout for conditional UI elements
                   fluidRow(
                     # Conditional panel for default dictionary
                     conditionalPanel(
                       condition = "input.dict_type2 === 'default_dictionary'",
                       column(12, align = "center",
                              actionButton("seededlda_button_file_default2", "Run Seeded LDA Model", class = "btn-summary"))
                     ),
                     # Conditional panel for creating a new dictionary
                     conditionalPanel(
                       condition = "input.dict_type2 === 'create_dictionary'",
                       # UI for creating a new dictionary
                       uiOutput("customDictionaryUI2"),

                       # Button to trigger dictionary creation and display
                       actionButton("create_dict2", "Create/Display Dictionary", style = "margin-top: 20px;", class = "btn-summary"),

                       # Output area for displaying the dictionary
                       br(),
                       fluidRow(
                         column(3),
                         column(6, align = "center", br(), verbatimTextOutput("display_dict2")),
                         column(3)),
                       br(),
                       fluidRow(column(12, align = "center",
                                       actionButton("seededlda_button_file_custom2", "Run Seeded LDA Model", class = "btn-summary")))
                     )

                   ),
                   br(),
                   uiOutput("ldaTable3"),
                   br(),
                   uiOutput("ldaTable4")
          )



        ))
    )
  )
)


# Server
server <- function(input, output, session) {

  options(shiny.maxRequestSize = 100*1024^2)

  # Reactive expression to read data
  data <- reactive({
    if (input$input_type == "Text") {
      # Read data from text input
      return(input$input_text)
    }
  })

  data_file <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    data <- switch(ext,
                   "csv" = readr::read_csv(input$upload$datapath),
                   "xls" = readxl::read_excel(input$upload$datapath),
                   "xlsx" = readxl::read_excel(input$upload$datapath),
                   "json" = jsonlite::fromJSON(input$upload$datapath),
                   "rds" = readRDS(input$upload$datapath),
                   "rdata" = {
                     load(input$upload$datapath)
                     get(ls()[1])  # Assuming the first object is the data frame you want to display
                   },
                   stop("Unsupported file format"))
    return(data)  # Return the data
  })

#------------------------------- TEXT INPUT -----------------------------------#

  ####### ----------- TAB 1: SUMMARY OF THE DATA ----------------------- #######

  observeEvent(input$summary_button, {
    req(input$input_text)

    input_text <- input$input_text
    word_count <- length(unlist(strsplit(input_text, "\\s+")))
    char_count <- nchar(gsub("\\s+", "", input_text))
    sentence_count <- length(gregexpr("[.!?]", input_text)[[1]])

    data <- data.frame(
      "Statistic" = c("Word Count", "Character Count", "Sentence Count"),
      "Count" = c(word_count, char_count, sentence_count))

    options(reactable.theme = reactableTheme(
      color = "#8eccad",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ))

    output$x <- renderReactable({
      reactable(
        data,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    output$summary_table <- renderUI({
      fluidRow(
        column(3),
        column(6,
               div(
                 h5("Summary Text:"),
                 hr(),
                 reactableOutput("x"),
                 br()
               )),
        column(3),
        br(),
        hr()
      )
    })

    # Make the data reactive so it can be accessed globally
    summary_data <<- data

  })

  # Summary Download Handler
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("summary_results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(summary_data, file)
    }
  )

  observeEvent(input$word_freq_button, {
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
    ) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$word_freq_table <- renderReactable({
      reactable(
        all_word_freq_df,
        rownames = FALSE,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    annotation <- cnlp_annotate(input = input$input_text)

    annotation_df <- annotation$token %>%
      select(id = doc_id, token, lemma, entity = upos)

    output$table_entities <- renderReactable({
      reactable(
        annotation_df,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    output$both_outputs <- renderUI({

      fluidRow(
        column(6,
               div(
                 h5("Word Frequency:"),
                 hr(),
                 reactableOutput("word_freq_table"),
                 hr()
               )
        ),
        column(6,
               div(
                 h5("Table Entities:"),
                 hr(),
                 reactableOutput("table_entities"),
                 hr()
               )
        )
      )

    })

    # Make the data reactive so it can be accessed globally
    annotation_data <<- list(ANNOTATION = annotation_df,
                             FREQUENCY = all_word_freq_df)

  })

  # Summary Download Handler
  output$download_annotation <- downloadHandler(
    filename = function() {
      paste("annotation_results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(annotation_data, file)
    }
  )


  observeEvent(input$word_plot_button, {
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
    ) |>
      select(Word, Frequency) |>
      arrange(desc(Frequency)) |>
      head(5)

    output$plot_freq_table <- renderHighchart({
      hchart(all_word_freq_df, "column",
             hcaes(x = Word, y = Frequency),
             color = "#b1ddc7", # #8eccad
             borderColor = "#8eccad",
             showInLegend = F,
             name = "") %>%
        hc_xAxis( # Modify appearance of x-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of x-axis title
            )
          ),
          lineColor = "#8eccad", # Set color of x-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on x-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on x-axis
        ) %>%
        hc_yAxis( # Modify appearance of y-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of y-axis title
            )
          ),
          lineColor = "#8eccad", # Set color of y-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on y-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on y-axis
        )
    })

    plt_all_word_freq_data <<- all_word_freq_df

    corpus_clean <- tm_map(corpus,  removeWords, stopwords("english"))
    dtm_clean <- DocumentTermMatrix(corpus_clean)
    freq_matrix_clean <- as.matrix(dtm_clean)
    word_freq_clean <- colSums(freq_matrix_clean)

    all_word_freq_df_clean <- data.frame(
      Word = names(word_freq_clean),
      Frequency = word_freq_clean,
      stringsAsFactors = FALSE
    ) |>
      select(Word, Frequency) |>
      arrange(desc(Frequency)) |>
      head(5)

    output$plot_freq_table_clean <- renderHighchart({
      hchart(all_word_freq_df_clean, "column",
             hcaes(x = Word, y = Frequency),
             color = "#b1ddc7", # #8eccad
             borderColor = "#8eccad",
             showInLegend = F,
             name = "") %>%
        hc_xAxis( # Modify appearance of x-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of x-axis title
            )
          ),
          lineColor = "#8eccad", # Set color of x-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on x-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on x-axis
        ) %>%
        hc_yAxis( # Modify appearance of y-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of y-axis title
            )
          ),
          lineColor = "#8eccad", # Set color of y-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on y-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on y-axis
        )
    })

    plt_all_word_freq_clean_data <<- all_word_freq_df_clean



    annotation <- cnlp_annotate(input = input$input_text)

    annotation_df <- annotation$token %>%
      select(id = doc_id, token, lemma, entity = upos) |>
      group_by(entity) |>
      summarize(total = n()) |>
      mutate(entity = forcats::fct_reorder(entity, total)) |>
      arrange(desc(total)) |>
      head(5) |>
      select(Entity = entity, Total = total)

    output$plot_entities <- renderHighchart({

      hchart(annotation_df, "column",
             hcaes(x = Entity, y = Total),
             color = "#b1ddc7", # #8eccad
             borderColor = "#8eccad",
             showInLegend = F,
             name = "") |>
        hc_xAxis( # Modify appearance of x-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of x-axis title
            )
          ),
          tickColor = "#b1ddc7",
          lineColor = "#8eccad", # Set color of x-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on x-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on x-axis
        ) |>
        hc_yAxis( # Modify appearance of y-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of y-axis title
            )
          ),
          tickColor = "#b1ddc7",
          lineColor = "#8eccad", # Set color of y-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on y-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on y-axis
        )
    })


    output$both_plots <- renderUI({

      fluidRow(
        column(4,
               div(
                 h5("Word Distribution:"),
                 hr(),
                 downloadButton("download_word_plot", "Download Word Distribution Plot"),
                 hr(),
                 highchartOutput("plot_freq_table")
               )
        ),
        column(4,
               div(
                 h5("Word Distribution (Clean Text):"),
                 hr(),
                 downloadButton("download_word_plot_clean", "Download Word Distribution Plot (clean)"),
                 hr(),
                 highchartOutput("plot_freq_table_clean")
               )
        ),
        column(4,
               div(
                 h5("Entities Distribution:"),
                 hr(),
                 downloadButton("download_word_plot_entities", "Download Entity Distribution Plot"),
                 hr(),
                 highchartOutput("plot_entities")
               )
        )
      )
    })

    plt_annotation_data <<- annotation_df

  })

  # Download handler for Word Distribution Plot
  output$download_word_plot <- downloadHandler(
    filename = function() {
      paste("word_distribution_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = ggplot(plt_all_word_freq_data, aes(x = Word, y = Frequency)) +
               geom_col(fill = "#b1ddc7", color = "#8eccad") +
               theme_minimal() +
               labs(title = "Word Distribution", x = "Word", y = "Frequency") +
               theme(axis.title.x = element_text(color = "#8eccad"),
                     axis.title.y = element_text(color = "#8eccad")),
             device = "png")
    }
  )

  # Download handler for Word (Clean) Distribution Plot
  output$download_word_plot_clean <- downloadHandler(
    filename = function() {
      paste("word_distribution_clean_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = ggplot(plt_all_word_freq_clean_data, aes(x = Word, y = Frequency)) +
               geom_col(fill = "#b1ddc7", color = "#8eccad") +
               theme_minimal() +
               labs(title = "Word Distribution", x = "Word", y = "Frequency") +
               theme(axis.title.x = element_text(color = "#8eccad"),
                     axis.title.y = element_text(color = "#8eccad")),
             device = "png")
    }
  )

  # Download handler for Entity Distribution Plot
  output$download_word_plot_entities <- downloadHandler(
    filename = function() {
      paste("word_distribution_entity_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = ggplot(plt_annotation_data, aes(x = Entity, y = Total)) +
               geom_col(fill = "#b1ddc7", color = "#8eccad") +
               theme_minimal() +
               labs(title = "Word Distribution", x = "Entity", y = "Frequency") +
               theme(axis.title.x = element_text(color = "#8eccad"),
                     axis.title.y = element_text(color = "#8eccad")),
             device = "png")
    }
  )

  ####### ----------- TAB 2: SENTIMENT ANALYSIS ------------------------ #######

  # Sentiment Plots
  observeEvent(input$bing_button, {

    sentiments <- tidytext::get_sentiments("bing")

    new_colors <- c(negative = "#CC687F", positive = "#ef8a62")

    colors_df <- data.frame(sentiment = names(new_colors),
                            color = new_colors,
                            row.names = NULL)

    paragraphs <- strsplit(data(), '\n')[[1]]

    tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                         text = paragraphs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all') |>
      dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

    tokens_tab <- table(tokens$sentiment) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1)

    output$b_plot <- renderHighchart({
      hchart(tokens_tab, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_colors(c("#CC687F", "#8eccad")) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))

    })

    output$bing_plots <- renderUI({

      fluidRow(
        column(3),
        column(6,
               div(
                 br(),
                 h5("Bing Dictionary:"),
                 hr(),
                 highchartOutput("b_plot")
               )
        ),
        column(3)
      )

    })

    tokens_results <- tokens |> select(paragraph, word = token_lower, sentiment) |> filter(!is.na(sentiment))

    tokens_tab_bing <<- list(SUMMARY = tokens_tab, DETAIL = tokens_results)

  })

  # Summary Download Handler
  output$download_bing <- downloadHandler(
    filename = function() {
      paste("results_sentiment_bing_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_bing, file)
    }
  )





  observeEvent(input$nrc_button, {

    sentiments <- readRDS("./lexicons/df_sentiment.RDS") |>
      rename('sentiment' = 'emotion')

    new_colors <- c(Negative = "#CC687F", Positive = "#ef8a62")

    colors_df <- data.frame(sentiment = names(new_colors),
                            color = new_colors,
                            row.names = NULL)

    paragraphs <- strsplit(data(), '\n')[[1]]

    tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                         text = paragraphs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all') |>
      dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

    tokens_tab <- table(tokens$sentiment) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1)

    output$nv_plot <- renderHighchart({
      hchart(tokens_tab, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_colors(c("#CC687F", "#8eccad")) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))

    })

    output$nrc_plots <- renderUI({

      fluidRow(
        column(3),
        column(6,
               div(
                 h5("NRC Valence:"),
                 hr(),
                 highchartOutput("nv_plot")
               )),
        column(3)
      )

    })

    tokens_results <- tokens |> select(paragraph, word = token_lower, sentiment) |> filter(!is.na(sentiment))

    tokens_tab_nrc <<- list(SUMMARY = tokens_tab, DETAIL = tokens_results)

  })

  # Summary Download Handler
  output$download_nrc <- downloadHandler(
    filename = function() {
      paste("results_sentiment_nrc_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_nrc, file)
    }
  )


  observeEvent(input$loughran_button, {

    sentiments <- readRDS("./lexicons/LoughranMcDonald.rds") |>
      filter(sentiment %in% c("positive", "negative"))

    new_colors <- c(Negative = "#CC687F", Positive = "#ef8a62")

    colors_df <- data.frame(sentiment = names(new_colors),
                            color = new_colors,
                            row.names = NULL)

    paragraphs <- strsplit(data(), '\n')[[1]]

    tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                         text = paragraphs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all') |>
      dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

    tokens_tab <- table(tokens$sentiment) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1)) |>
      dplyr::filter(Var1 %in% c("Negative", "Positive")) |>
      dplyr::mutate(Sentiment = Var1)

    output$louv_plot <- renderHighchart({
      hchart(tokens_tab, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_colors(c("#CC687F", "#8eccad")) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))

    })

    output$loughran_plots <- renderUI({

      fluidRow(
        column(3),
        column(6,
               div(
                 h5("Loughran Valence:"),
                 hr(),
                 highchartOutput("louv_plot")
               )),
        column(3)
      )

    })

    tokens_results <- tokens |> select(paragraph, word = token_lower, sentiment) |> filter(!is.na(sentiment))

    tokens_tab_loughran <<- list(SUMMARY = tokens_tab, DETAIL = tokens_results)

  })

  # Summary Download Handler
  output$download_loughran <- downloadHandler(
    filename = function() {
      paste("results_sentiment_loughran_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_loughran, file)
    }
  )

  observeEvent(input$afinn_button, {

    sentiments <- readRDS("./lexicons/afinn_sentiment.rds") |>
      mutate(sentiment = ifelse(value < 0, "negative", "positive"))

    new_colors <- c(Negative = "#CC687F", Positive = "#ef8a62")

    colors_df <- data.frame(sentiment = names(new_colors),
                            color = new_colors,
                            row.names = NULL)

    paragraphs <- strsplit(data(), '\n')[[1]]

    tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                         text = paragraphs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all') |>
      dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

    tokens_tab <- table(tokens$sentiment) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1)

    output$av_plot <- renderHighchart({
      hchart(tokens_tab, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_colors(c("#CC687F", "#8eccad")) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))

    })

    output$afinn_plots <- renderUI({

      fluidRow(
        column(3),
        column(6,
               div(
                 h5("Afinn Valence:"),
                 hr(),
                 highchartOutput("av_plot")
               )),
        column(3)
      )

    })

    tokens_results <- tokens |> select(paragraph, word = token_lower, sentiment) |> filter(!is.na(sentiment))

    tokens_tab_afinn <<- list(SUMMARY = tokens_tab, DETAIL = tokens_results)

  })

  # Summary Download Handler
  output$download_afinn <- downloadHandler(
    filename = function() {
      paste("results_sentiment_affin_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_afinn, file)
    }
  )

  # Render the highlighted text
  output$highlighted_text <- renderUI({
    req(data())
    highlighted_text <- data()

    fluidRow(
      column(12,
             div(
               h5("Main Text:"),
               hr(),
               HTML(
                 paste0(
                   "<div style='background-color: #191919; padding: 15px;'>",
                   highlighted_text,
                   "</div>"
                 )),
               hr()
             )
      ))
  })

  ##### Sentiment view of the text
  output$sentiment_text <- shiny::renderText({
    shiny::req(data())

    thetext <- data()


    return(sentiment_highlighter(thetext,
                                 lexicon = input$sentiment_lexicon,
                                 lexicon_dir = '.'))
  })

  ####### ----------- TAB 3: EMOTION ANALYSIS -------------------------- #######

  observeEvent(input$nrc_emotion_button, {

    sentiments <- readRDS("./lexicons/df_emotions.RDS")

    paragraphs <- strsplit(data(), '\n')[[1]]

    tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                         text = paragraphs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all')

    tokens_tab <- table(tokens$emotion) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1)

    output$n_emotion_plot <- renderHighchart({
      hchart(tokens_tab, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))

    })

    df_emotion <- tokens |> filter(!is.na(emotion)) |>
      select(Word = token_lower, Emotion = emotion) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$n_emotion_table <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        df_emotion,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })


    output$nrc_emotion_plots <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("NRC Emotions:"),
                 hr(),
                 highchartOutput("n_emotion_plot")
               )),
        column(1),
        column(4,
               div(
                 h5("NRC Emotions - Table:"),
                 hr(),
                 reactableOutput("n_emotion_table")
               )),
        column(1)
      )

    })

    tokens_results <- tokens |> select(paragraph, word = token_lower, emotion) |> filter(!is.na(emotion))

    tokens_tab_emo_nrc <<- list(SUMMARY = tokens_tab, DETAIL = tokens_results)

  })

  # Summary Download Handler
  output$download_emotion_nrc <- downloadHandler(
    filename = function() {
      paste("results_emotion_nrc_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_emo_nrc, file)
    }
  )

  observeEvent(input$loughran_emotion_button, {

    sentiments <- readRDS("./lexicons/LoughranMcDonald.rds") |>
      filter(!sentiment %in% c("positive", "negative"))

    paragraphs <- strsplit(data(), '\n')[[1]]

    tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                         text = paragraphs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all')

    tokens_tab <- table(tokens$sentiment) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1)

    output$loughran_emotion_plot <- renderHighchart({
      hchart(tokens_tab, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))

    })

    df_loughran_emotion <- tokens |> filter(!is.na(sentiment)) |>
      select(Word = token_lower, Emotion = sentiment) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$loughran_emotion_table <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        df_loughran_emotion,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })


    output$loughran_emotion_plots <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("Loughran McDonald Emotions:"),
                 hr(),
                 highchartOutput("loughran_emotion_plot")
               )),
        column(1),
        column(4,
               div(
                 h5("Loughran McDonald Emotions - Table:"),
                 hr(),
                 reactableOutput("loughran_emotion_table")
               )),
        column(1)
      )

    })

    tokens_results <- tokens |> select(paragraph, word = token_lower, sentiment) |> filter(!is.na(sentiment))

    tokens_tab_emo_loughran <<- list(SUMMARY = tokens_tab, DETAIL = tokens_results)

  })

  # Summary Download Handler
  output$download_emotion_loughran <- downloadHandler(
    filename = function() {
      paste("results_emotion_loughran_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_emo_loughran, file)
    }
  )


  ####### ----------- TAB 4: AROUSAL & VALENCE ------------------------- #######

  observeEvent(input$arousal_button, {

    sentiments <- readRDS("./lexicons/df_arousal.RDS")

    paragraphs <- strsplit(data(), '\n')[[1]]

    # paragraphs <- strsplit(text, '\n')[[1]]

    tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                         text = paragraphs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all')

    tokens_tab <- table(tokens$level) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1) |>
      group_by(Sentiment) |>
      summarize(dist = sum(Freq)) |>
      mutate(percent = round((dist/sum(dist)), 3), label = paste0("Arousal Level: ", Sentiment, " - ", percent*100, "%"))

    output$arousal_plot <- renderHighchart({
      hchart(tokens_tab,
             "pie", hcaes(x = label, y = percent),
             name = "Arousal Distribution")
    })

    df_arousal <- tokens |> filter(!is.na(level)) |>
      select(Word = token_lower, Arousal = level) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$arousal_table <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        df_arousal,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })


    output$arousal_plots <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("Arousal:"),
                 hr(),
                 highchartOutput("arousal_plot")
               )),
        column(1),
        column(4,
               div(
                 h5("Arousal - Table:"),
                 hr(),
                 reactableOutput("arousal_table")
               )),
        column(1)
      )

    })

    tokens_tab_arousal <<- list(SUMMARY = tokens_tab, DETAILS = df_arousal)

  })

  # Summary Download Handler
  output$download_arousal <- downloadHandler(
    filename = function() {
      paste("results_arousal_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_arousal, file)
    }
  )

  observeEvent(input$valence_button, {

    sentiments <- readRDS("./lexicons/df_valence.RDS")

    paragraphs <- strsplit(data(), '\n')[[1]]


    tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                         text = paragraphs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all')

    tokens_tab <- table(tokens$level) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1) |>
      group_by(Sentiment) |>
      summarize(dist = sum(Freq)) |>
      mutate(percent = round((dist/sum(dist)), 3), label = paste0("Valence Level: ", Sentiment, " - ", percent*100, "%"))

    output$valence_plot <- renderHighchart({
      hchart(tokens_tab,
             "pie", hcaes(x = label, y = percent),
             name = "Valence Distribution")
    })

    df_valence <- tokens |> filter(!is.na(level)) |>
      select(Word = token_lower, Valence = level) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$valence_table <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        df_valence,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })


    output$valence_plots <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("Valence:"),
                 hr(),
                 highchartOutput("valence_plot")
               )),
        column(1),
        column(4,
               div(
                 h5("Valence - Table:"),
                 hr(),
                 reactableOutput("valence_table")
               )),
        column(1)
      )

    })

    tokens_tab_valence <<- list(SUMMARY = tokens_tab, DETAILS = df_valence)

  })

  # Summary Download Handler
  output$download_valence <- downloadHandler(
    filename = function() {
      paste("results_valence_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_valence, file)
    }
  )

  observeEvent(input$dominance_button, {

    sentiments <- readRDS("./lexicons/df_dominance.RDS")

    paragraphs <- strsplit(data(), '\n')[[1]]


    tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                         text = paragraphs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all')

    tokens_tab <- table(tokens$level) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1) |>
      group_by(Sentiment) |>
      summarize(dist = sum(Freq)) |>
      mutate(percent = round((dist/sum(dist)), 3), label = paste0("Dominance Level: ", Sentiment, " - ", percent*100, "%"))

    output$dominance_plot <- renderHighchart({
      hchart(tokens_tab,
             "pie", hcaes(x = label, y = percent),
             name = "Dominance Distribution")
    })

    df_dominance <- tokens |> filter(!is.na(level)) |>
      select(Word = token_lower, Dominance = level) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$dominance_table <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        df_dominance,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })


    output$dominance_plots <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("Dominance:"),
                 hr(),
                 highchartOutput("dominance_plot")
               )),
        column(1),
        column(4,
               div(
                 h5("Dominance - Table:"),
                 hr(),
                 reactableOutput("dominance_table")
               )),
        column(1)
      )

    })

    tokens_tab_dominance <<- list(SUMMARY = tokens_tab, DETAILS = df_dominance)

  })

  # Summary Download Handler
  output$download_dominance <- downloadHandler(
    filename = function() {
      paste("results_dominance_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_dominance, file)
    }
  )


  ####### ----------- TAB 5: TOPIC MODELLING LDA ----------------------- #######

  data_lda <- reactive({
    req(data())
    # Read data from text input
    text <- data()

    # Split text into sentences
    # sentences <- str_split(text, pattern = "\\?|\\.|!", simplify = FALSE) |>
    #   unlist() |>
    #   gsub("[[:punct:]]", "", .) %>%
    #   trimws() #%>%
    #   .[. != ""]

        # Split the text into sentences using str_split
    sentences <- str_split(text, pattern = "\\?|\\.|!", simplify = FALSE)

    # Unlist the result
    sentences <- unlist(sentences)

    # Remove punctuation
    sentences <- gsub("[[:punct:]]", "", sentences)

    # Trim whitespace
    sentences <- trimws(sentences)

    # Remove empty strings
    sentences <- sentences[sentences != ""]



    # Prepare for LDA
    corpus <- corpus(sentences)
    toks <- tokens(corpus, remove_punct = TRUE, remove_symbols = TRUE,
                   remove_numbers = TRUE, remove_url = TRUE)
    dfmt <- dfm(toks) |>
      dfm_remove(stopwords("en")) |>
      dfm_remove("*@*") |>
      dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

    # Convert the DFM to a format for topicmodels
    forTM <- convert(dfmt, to = "topicmodels")

    # forTM <- sentences |> as.data.frame() |> rename("text" = "sentences")

    return(forTM)

  })

  observeEvent(input$lda_button, {

    req(input$numTopics)

      myLDA <- LDA(data_lda(), k = input$numTopics, control = list(seed = 1234))

      ap_topics <- tidy(myLDA, matrix = "beta")

      ap_top_terms <- ap_topics %>%
        group_by(topic) %>%
        slice_max(beta, n = 10) %>%
        ungroup() %>%
        arrange(topic, -beta) |>
        group_by(topic) %>%
        slice_max(term, n = 8)


      output$lda_plot <- renderPlot({
        ap_top_terms %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(beta, term, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        scale_y_reordered() +
        theme(axis.text = element_text(size = 16),
              axis.text.y = element_text(face = "bold"),
              strip.text = element_text(size = 18, face = "bold"))
      })

      df_predominant_topic <- tidy(myLDA, matrix = "gamma") |>
        group_by(document, topic) |>
        summarize(total = mean(gamma)) |>
        slice_max(total, n = 1) |>
        group_by(topic) |>
        summarize(dist = mean(total)) |>
        mutate(percent = round((dist/sum(dist)), 3), label = paste0("Topic ", topic, ": ", percent*100, "%"))

      output$lda_plot_pie <- renderHighchart({

        hchart(df_predominant_topic,
          "pie", hcaes(x = label, y = percent),
          name = "Topic Distribution")

      })

      topic_docs <- tidy(myLDA, matrix = "gamma") %>%
        group_by(document, topic) %>%
        summarize(total = mean(gamma), .groups = 'drop') %>%
        ungroup() |>
        group_by(document) %>%
        mutate(total_gamma = sum(total),             # Total gamma per document
               percent = round(total / total_gamma * 100, 2))  %>%  # Calculate percentage
        ungroup()

      plt_sample <- topic_docs |> select(document) |> unique() |> sample_n(5, replace = FALSE) |> pull()

      output$lda_plotTop <- renderPlot({
        ggplot(topic_docs |> filter(document %in% plt_sample), aes(x = document, y = total, fill = as.factor(topic))) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = sprintf("%.1f%%", percent)),   # Format the text labels to one decimal place
                    position = position_stack(vjust = 0.5),   # Center the text in each stack
                    color = "white",                         # Text color
                    size = 7) +
          labs(title = "",
               subtitle = "",
               x = "Document",
               y = "Share of topics [%]",
               fill = "Topic") +
          # theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                axis.text = element_text(size = 16),
                axis.text.y = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold", size = 18),
                strip.text = element_text(size = 18, face = "bold"),
                legend.key.size = unit(1., 'cm'),
                legend.title = element_text(face = "bold", size = 17),
                legend.text = element_text(face = "bold", size = 17))
      })

      plt_lda_1 <- ap_top_terms %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(beta, term, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        scale_y_reordered() +
        theme(axis.text = element_text(size = 16),
              axis.text.y = element_text(face = "bold"),
              strip.text = element_text(size = 18, face = "bold"))

      output$topics <- renderUI({

        fluidRow(
          column(1),
          column(10,
                 div(
                   h5("The terms that are most common within each topic:"),
                   br(),
                   downloadButton("download_lda_plt1", "Download Plot"),
                   hr(),
                   plotOutput("lda_plot") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
                 )),
          column(1)
        )
      })

      # Summary Download Handler
      output$download_lda_plt1 <- downloadHandler(
        filename = function() {
          paste("results_LDA_plt1_", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          ggsave(file, plot = plt_lda_1,
                 device = "png")
        }
      )

    output$topics_pie <- renderUI({

      fluidRow(
        column(1),
        column(10,
               div(
                 br(),
                 h5("Prevalence distribution across Topics in Text:"),
                 hr(),
                 highchartOutput("lda_plot_pie") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )
    })

    output$topics_pieTop <- renderUI({

      fluidRow(
        column(1),
        column(10,
               div(
                 br(),
                 h5("Share of topics [%] by Sentence (from a Random Sample):"),
                 hr(),
                 plotOutput("lda_plotTop") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )
    })

    results_lda <<- list(TOP_TERMS = ap_top_terms, SUMMARY = df_predominant_topic, DISTRIBUTION = topic_docs)


  })


  # Summary Download Handler
  output$download_lda_results <- downloadHandler(
    filename = function() {
      paste("results_LDA_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(results_lda, file)
    }
  )

  observeEvent(input$lda_test_button, {

    FindTopics_result <- ldatuning::FindTopicsNumber(
      data_lda(),
      topics = seq(2, 10, by = 1),
      metrics = c("CaoJuan2009",  "Deveaud2014"),
      method = "Gibbs",
      control = list(seed = 1234),
      verbose = TRUE
    )

    values <- FindTopics_result[!names(FindTopics_result) %in% c("LDA_model")]

    # normalize to [0,1]
    columns <- base::subset(values, select = 2:ncol(values))
    values <- base::data.frame(
      values["topics"],
      base::apply(columns, 2, function(column) {
        scales::rescale(column, to = c(0, 1), from = range(column))
      })
    )

    # melt
    values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)

    # separate max-arg & min-arg metrics
    values$group <- values$variable %in% c("Griffiths2004", "Deveaud2014")
    values$group <- base::factor(
      values$group,
      levels = c(FALSE, TRUE),
      labels = c("Minimize", "Maximize")
    )

    output$test_number_k <- renderPlot({
      p <- ggplot(values, aes_string(x = "topics", y = "value", group = "variable"),
                  height = 500)
      p <- p + geom_line()
      p <- p + geom_point(aes_string(shape = "variable"), size = 3, color = "white")
      p <- p + guides(size = FALSE, shape = guide_legend(title = "Metrics:"))
      p <- p + scale_x_continuous(breaks = values$topics)
      p <- p + labs(x = "Number of Topics", y = NULL)

      # separate in two parts
      p <- p + facet_grid(group ~ .)
      # style
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1),
                     axis.text = element_text(size = 16),
                     axis.text.y = element_text(face = "bold"),
                     axis.title.y = element_text(face = "bold", size = 18),
                     axis.title.x = element_text(face = "bold", size = 18),
                     strip.text = element_text(size = 18, face = "bold"),
                     legend.key.size = unit(1., 'cm'),
                     legend.title = element_text(face = "bold", size = 17),
                     legend.text = element_text(face = "bold", size = 17))
      p
    })

    output$test_k_plot <- renderUI({

      fluidRow(
        column(3),
        column(6,
               div(
                 # style = "height:1000px;",
                 h5("Checking the best number of Topics:"),
                 hr(),
                 plotOutput("test_number_k") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(3)
      )
    })

    p_lda_test <<- ggplot(values, aes_string(x = "topics", y = "value", group = "variable"),
                          height = 500) +
      geom_line() +
      geom_point(aes_string(shape = "variable"), size = 3, color = "white") +
      guides(size = FALSE, shape = guide_legend(title = "Metrics:")) +
      scale_x_continuous(breaks = values$topics) +
      labs(x = "Number of Topics", y = NULL) +
      facet_grid(group ~ .) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.text = element_text(size = 16),
            axis.text.y = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold", size = 18),
            axis.title.x = element_text(face = "bold", size = 18),
            strip.text = element_text(size = 18, face = "bold"),
            legend.key.size = unit(1., 'cm'),
            legend.title = element_text(face = "bold", size = 17),
            legend.text = element_text(face = "bold", size = 17))

  })

  # Download handler for Word Distribution Plot
  output$download_lda_test <- downloadHandler(
    filename = function() {
      paste("LDA_test_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = p_lda_test,
             device = "png")
    }
  )

  observeEvent(input$lda_evaluation_button, {

    req(input$numTopics)

    myLDA <- LDA(data_lda(), k = input$numTopics, control = list(seed = 1234))

    diag_df <- topicdoc::topic_diagnostics(myLDA, data_lda())

    ap_topics <- tidy(myLDA, matrix = "beta")
    ap_top_terms <- ap_topics %>%
      group_by(topic) %>%
      slice_max(beta, n = 5) %>%
      ungroup() %>%
      arrange(topic, -beta) |>
      group_by(topic) |>
      slice_max(term, n = 5) |>
      select(-beta) |>
      summarize(topic_label = paste(term, collapse = ", "), .groups = "drop")

    diag_df <- diag_df |>
      left_join(ap_top_terms, by = c("topic_num" = "topic")) |>
      mutate(
        dist_from_corpus = round(dist_from_corpus, 2),
        tf_df_dist = round(tf_df_dist, 2),
        topic_coherence = round(topic_coherence, 2),
        topic_exclusivity = round(topic_exclusivity, 2)
      )

    output$topic_eval <- renderPlot({
      p <- diag_df %>%
        gather(diagnostic, value, -topic_label, -topic_num) %>%
        ggplot(aes(x = topic_num, y = value,
                   fill = str_wrap(topic_label, 25))) +
        geom_bar(stat = "identity") +
        facet_wrap(~diagnostic, scales = "free", ncol = 7) +
        labs(x = "Topic Number", y = "Diagnostic Value",
             fill = "Topic Label", title = "")

      # style
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1),
                     axis.text = element_text(size = 16),
                     axis.text.y = element_text(face = "bold"),
                     axis.title.y = element_text(face = "bold", size = 18),
                     axis.title.x = element_text(face = "bold", size = 18),
                     strip.text = element_text(size = 18, face = "bold"),
                     legend.position = "bottom",
                     legend.key.size = unit(1., 'cm'),
                     legend.title = element_text(face = "bold", size = 17),
                     legend.text = element_text(face = "bold", size = 17))
      p
    })

    output$topics_evaluation <- renderUI({

      fluidRow(
        column(1),
        column(10,
               div(
                 h5("All Topic Model Diagnostics:"),
                 hr(),
                 plotOutput("topic_eval") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )
    })

    p_lda_evaluation <<- diag_df %>%
      gather(diagnostic, value, -topic_label, -topic_num) %>%
      ggplot(aes(x = topic_num, y = value,
                 fill = str_wrap(topic_label, 25))) +
      geom_bar(stat = "identity") +
      facet_wrap(~diagnostic, scales = "free", ncol = 2) +
      labs(x = "Topic Number", y = "Diagnostic Value",
           fill = "Topic Label", title = "") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.text = element_text(size = 12),
            axis.text.y = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold", size = 12),
            axis.title.x = element_text(face = "bold", size = 12),
            strip.text = element_text(size = 12, face = "bold"),
            legend.position = "bottom",
            legend.key.size = unit(.5, 'cm'),
            legend.title = element_text(face = "bold", size = 12),
            legend.text = element_text(face = "bold", size = 12))
  })

  # Download handler for Word Distribution Plot
  output$download_lda_eval <- downloadHandler(
    filename = function() {
      paste("LDA_evaluation_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = p_lda_evaluation,
             device = "png")
    }
  )

  ####### ----------- TAB 6: SEEDED LDA -------------------------------- #######

  data_seededlda <- reactive({
    req(data())
    # Read data from text input
    text <- data()

    # text <- x

    # Split text into sentences
    sentences <- str_split(text, pattern = "\\?|\\.|!", simplify = FALSE) %>%
      unlist() %>%
      gsub("[[:punct:]]", "", .) %>%
      trimws() %>%
      .[. != ""]

    # Prepare for LDA
    corpus <- corpus(sentences)
    toks <- tokens(corpus, remove_punct = TRUE, remove_symbols = TRUE,
                   remove_numbers = TRUE, remove_url = TRUE)
    dfmt <- dfm(toks) |>
      dfm_remove(stopwords("en")) |>
      dfm_remove("*@*") |>
      dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

    return(dfmt)

  })

  reactive_dict <- reactiveVal(list())

  observeEvent(input$create_dict, {
    # Check if we're adding to the dictionary
    if (input$dict_type == "create_dictionary" && nzchar(input$category_name) && nzchar(input$related_words)) {
      # Split words and trim whitespace
      words <- strsplit(input$related_words, ",\\s*")[[1]]

      # Add or update the category in the dictionary
      new_dict <- reactive_dict()
      new_dict[[input$category_name]] <- words
      reactive_dict(new_dict)

      # Display the updated dictionary
      output$display_dict <- renderText({
        if (length(reactive_dict()) > 0) {
          paste(sapply(names(reactive_dict()), function(cat) {
            sprintf("- [%s]:\n  - %s", cat, paste(reactive_dict()[[cat]], collapse=", "))
          }), collapse="\n")
        } else {
          "No categories defined."
        }
      })
    }
  }, ignoreNULL = FALSE)

  # Add UI elements conditionally
  # output$customDictionaryUI <- renderUI({
  #   if (input$dict_type == "create_dictionary") {
  #     fluidRow(
  #       column(6, textInput("category_name", "Enter Category Name:")),
  #       column(6, textAreaInput("related_words", "Add Related Words (comma-separated):", height = '100px'))
  #     )
  #   }
  # })

  # Listen for changes in the input 'dict_type'
  observeEvent(input$seededlda_button_file_default, {

    req(data_seededlda())

    dfmt <- data_seededlda()

    # Load the dictionary
    dict_topic <- dictionary(file = "./lexicons/dictionary.yml")

    # Run seeded LDA
    lda_seed <- seededlda::textmodel_seededlda(dfmt, dict_topic, batch_size = 0.01, auto_iter = TRUE, verbose = TRUE)

    # Output the terms from lda_seed model
    options(reactable.theme = reactableTheme(
      color = "#8eccad",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ))

    output$ldaResults <- renderReactable({
      reactable(
        as.data.frame(terms(lda_seed)),
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    # assign topics from seeded LDA as a document-level variable to the dfm
    dfmt$topic2 <- topics(lda_seed)

    dominant_topic <- dfmt@docvars |>
      group_by(topic2) |>
      summarize(total = n()) |>
      mutate(percent = round((total/sum(total)), 3),
             label = paste0("Topic ", topic2, ": ", percent*100, "%"))

    seedlda_results_custom <<- list(SUMMARY = dominant_topic, DETAILS = as.data.frame(terms(lda_seed2, n = 20)))

    plt_seedlda_2 <- ggplot(dominant_topic, aes(x = "", y = percent, fill = label)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      labs(title = "Topic Distribution") +
      theme_void() +
      theme(axis.text = element_text(size = 16),
            axis.text.y = element_text(face = "bold"),
            strip.text = element_text(size = 18, face = "bold"))

    output$seededlda_plot_pie <- renderHighchart({

      hchart(dominant_topic,
             "pie", hcaes(x = label, y = percent),
             name = "Topic Distribution")

    })


    seedlda_results <<- list(SUMMARY = dominant_topic, DETAILS = as.data.frame(terms(lda_seed, n = 20)))

    plt_seedlda_1 <- ggplot(dominant_topic, aes(x = "", y = percent, fill = label)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      labs(title = "Topic Distribution") +
      theme_void() +
      theme(axis.text = element_text(size = 16),
            axis.text.y = element_text(face = "bold"),
            strip.text = element_text(size = 18, face = "bold"))


    # Render LDA Results Table
    output$ldaTable <- renderUI({
      fluidRow(
        hr(),
        h5(HTML("<b>Results by Default Dictionary:</b>")),
        column(6,
               div(
                 br(),
                 h5("SeededLDA Results:"),
                 br(),
                 downloadButton("download_seed_table", "Download Results"),
                 hr(),
                 reactableOutput("ldaResults") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(6,
               div(
                 br(),
                 h5("Distribution across Topics in Text:"),
                 br(),
                 downloadButton("download_seed_plt1", "Download Plot"),
                 hr(),
                 highchartOutput("seededlda_plot_pie") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               ))
      )
    })

    # Summary Download Handler
    output$download_seed_table <- downloadHandler(
      filename = function() {
        paste("results_SEEDLDA_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(seedlda_results, file)
      }
    )

    # Summary Download Handler
    output$download_seed_plt1 <- downloadHandler(
      filename = function() {
        paste("results_SEEDLDA_plt1_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plt_seedlda_1,
               device = "png")
      }
    )


  })

  # Listen for changes in the input 'dict_type'
  observeEvent(input$seededlda_button_file_custom, {
    # Ensure the data is available
    req(data_seededlda(), reactive_dict())

    dfmt <- data_seededlda()

    # Load the dictionary
    dict_topic <- reactive_dict()
    dict_topic <- dictionary(dict_topic)

    # Run seeded LDA
    lda_seed2 <- textmodel_seededlda(dfmt, dict_topic, batch_size = 0.01, auto_iter = TRUE, verbose = TRUE)

    # Output the terms from lda_seed model
    options(reactable.theme = reactableTheme(
      color = "#8eccad",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ))

    output$ldaResults2 <- renderReactable({
      reactable(
        as.data.frame(terms(lda_seed2)),
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    # assign topics from seeded LDA as a document-level variable to the dfm
    dfmt$topic2 <- topics(lda_seed2)

    dominant_topic <- dfmt@docvars |>
      group_by(topic2) |>
      summarize(total = n()) |>
      mutate(percent = round((total/sum(total)), 3),
             label = paste0("Topic ", topic2, ": ", percent*100, "%"))

    seedlda_results_custom <<- list(SUMMARY = dominant_topic, DETAILS = as.data.frame(terms(lda_seed2, n = 20)))

    plt_seedlda_2 <- ggplot(dominant_topic, aes(x = "", y = percent, fill = label)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      labs(title = "Topic Distribution") +
      theme_void() +
      theme(axis.text = element_text(size = 16),
            axis.text.y = element_text(face = "bold"),
            strip.text = element_text(size = 18, face = "bold"))

    output$seededlda_plot_pie2 <- renderHighchart({

      hchart(dominant_topic,
             "pie", hcaes(x = label, y = percent),
             name = "Topic Distribution")

    })


    # Render LDA Results Table
    output$ldaTable2 <- renderUI({

      fluidRow(
        hr(),
        h5(HTML("<b>Results by Custom Dictionary:</b>")),
        column(6,
               div(
                 br(),
                 h5("SeededLDA Results:"),
                 br(),
                 downloadButton("download_seed_table_custom", "Download Results"),
                 hr(),
                 reactableOutput("ldaResults2") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(6,
               div(
                 br(),
                 h5("Distribution across Topics in Text:"),
                 br(),
                 downloadButton("download_seed_plt2", "Download Plot"),
                 hr(),
                 highchartOutput("seededlda_plot_pie2") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               ))
      )
    })

    # Summary Download Handler
    output$download_seed_table_custom <- downloadHandler(
      filename = function() {
        paste("results_SEEDLDA_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(seedlda_results_custom, file)
      }
    )

    # Summary Download Handler
    output$download_seed_plt2 <- downloadHandler(
      filename = function() {
        paste("results_SEEDLDA_plt2_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plt_seedlda_2,
               device = "png")
      }
    )

  })


  observeEvent(input$reset_button, {

    session$reload()

  })





#------------------------------- FILE INPUT -----------------------------------#



  ####### ----------- TAB 1: SUMMARY OF THE DATA ----------------------- #######


  observeEvent(input$show_table_button, {

    output$data <- DT::renderDataTable({
      DT::datatable(
        data_file(),
        style = 'bootstrap',
        rownames = FALSE,
        extensions = c('Buttons', 'FixedHeader', 'KeyTable', 'Scroller'),
        plugins = 'natural',
        options = list(
          deferRender = TRUE,
          scrollY = 300,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          pageLength = 3,
          buttons = list(
            list(
              extend = "collection",
              buttons = c('csv', 'excel', 'pdf'),
              text = "Download Current Page",
              filename = "page",
              exportOptions = list(
                modifier = list(page = "current")
              )
            ),
            list(
              extend = "collection",
              buttons = c('csv', 'excel', 'pdf'),
              text = "Download Full Results",
              filename = "data",
              exportOptions = list(
                modifier = list(page = "all")
              )
            ))
        ))
    })

    output$show_data <- renderUI({

      fluidRow(
        column(1),
        column(10,
               div(
                 h5("Data Uploaded:"),
                 hr(),
                 DT::dataTableOutput("data") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )
    })

  })

  observeEvent(input$summary_file_button, {

    # Read the data frame from the uploaded file
    df <- data_file()

    # Calculate the summary statistics
    num_rows <- nrow(df)
    num_cols <- ncol(df)
    # words <- sum(sapply(df, function(x) sum(nchar(as.character(x), type = "chars"))))
    # characters <- sum(sapply(df, function(x) sum(nchar(gsub("\\s", "", as.character(x)), type = "chars"))))
    column_names <- toString(names(df))

    # Create a data frame for summary
    data <- data.frame(
      Statistic = c("Number of Rows/Docs", "Number of Columns", "Column Names"),
      Value = c(num_rows, num_cols, column_names),
      stringsAsFactors = FALSE
    )

    options(reactable.theme = reactableTheme(
      color = "#8eccad",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ))

    output$file_summary <- renderReactable({
      reactable(
        data,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    # Make the data reactive so it can be accessed globally
    summary_data_file <<- data

    output$summary_file_table <- renderUI({
      fluidRow(
        column(3),
        column(6,
               div(
                 h5("Summary File:") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5),
                 hr(),
                 downloadButton("download_summary_file", "Download Summary as Excel"),
                 hr(),
                 reactableOutput("file_summary"),
                 br()
               )
        ),
        column(3),
        br(),
        hr()
      )
    })

    # Summary Download Handler
    output$download_summary_file <- downloadHandler(
      filename = function() {
        paste("summary_results_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(summary_data_file, file)
      }
    )

  })

  observeEvent(input$word_freq_file_button, {

    # Ensure a column is selected
    req(input$select_column)

    # Read the data frame from the uploaded file
    df <- data_file()
    selected_col <- input$select_column

    # Ensure the column exists in the dataframe
    req(selected_col %in% names(df))

    # Access the correct column and prepare the text
    input_text <- tolower(df[[selected_col]])
    input_text <- gsub("[[:punct:]0-9]", "", input_text)

    corpus <- Corpus(VectorSource(input_text))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus_clean <- tm_map(corpus,  removeWords, stopwords("english"))
    dtm <- DocumentTermMatrix(corpus_clean)
    freq_matrix <- as.matrix(dtm)
    word_freq <- colSums(freq_matrix)

    all_word_freq_df <- data.frame(
      Word = names(word_freq),
      Frequency = word_freq,
      stringsAsFactors = FALSE
    ) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$word_freq_table <- renderReactable({
      reactable(
        all_word_freq_df,
        rownames = FALSE,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    annotation <- cnlp_annotate(input = input_text)

    annotation_df <- annotation$token %>%
      select(id = doc_id, token, lemma, entity = upos)

    output$table_entities <- renderReactable({
      reactable(
        annotation_df,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    output$both_outputs_data <- renderUI({

      fluidRow(
        column(6,
               div(
                 h5("Word Frequency Across Data:"),
                 hr(),
                 downloadButton("download_annotation_file", "Download Results as Excel"),
                 hr(),
                 reactableOutput("word_freq_table") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5),
                 hr()
               )
        ),
        column(6,
               div(
                 h5("Table Entities Across Data:"),
                 hr(),
                 downloadButton("download_annotation_file", "Download Results as Excel"),
                 hr(),
                 reactableOutput("table_entities") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5),
                 hr()
               )
        )
      )

    })

    # Make the data reactive so it can be accessed globally
    annotation_data_file <<- list(ANNOTATION = annotation_df,
                                  FREQUENCY = all_word_freq_df)

  })

  # Summary Download Handler
  output$download_annotation_file <- downloadHandler(
    filename = function() {
      paste("annotation_results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(annotation_data_file, file)
    }
  )

  observeEvent(input$word_plot_file_button, {
    # Ensure a column is selected
    req(input$select_column2)

    # Read the data frame from the uploaded file
    df <- data_file()
    selected_col <- input$select_column2

    # Ensure the column exists in the dataframe
    req(selected_col %in% names(df))


    # Access the correct column and prepare the text
    input_text <- tolower(df[[selected_col]])

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
    ) |>
      select(Word, Frequency) |>
      arrange(desc(Frequency)) |>
      head(5)

    plt_all_word_freq_data_file <<- all_word_freq_df

    output$plot_freq_table <- renderHighchart({
      hchart(all_word_freq_df, "column",
             hcaes(x = Word, y = Frequency),
             color = "#b1ddc7", # #8eccad
             borderColor = "#8eccad",
             showInLegend = F,
             name = "") %>%
        hc_xAxis( # Modify appearance of x-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of x-axis title
            )
          ),
          lineColor = "#8eccad", # Set color of x-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on x-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on x-axis
        ) %>%
        hc_yAxis( # Modify appearance of y-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of y-axis title
            )
          ),
          lineColor = "#8eccad", # Set color of y-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on y-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on y-axis
        )
    })

    corpus_clean <- tm_map(corpus,  removeWords, stopwords("english"))
    dtm_clean <- DocumentTermMatrix(corpus_clean)
    freq_matrix_clean <- as.matrix(dtm_clean)
    word_freq_clean <- colSums(freq_matrix_clean)

    all_word_freq_df_clean <- data.frame(
      Word = names(word_freq_clean),
      Frequency = word_freq_clean,
      stringsAsFactors = FALSE
    ) |>
      select(Word, Frequency) |>
      arrange(desc(Frequency)) |>
      head(5)

    output$plot_freq_table_clean <- renderHighchart({
      hchart(all_word_freq_df_clean, "column",
             hcaes(x = Word, y = Frequency),
             color = "#b1ddc7", # #8eccad
             borderColor = "#8eccad",
             showInLegend = F,
             name = "") %>%
        hc_xAxis( # Modify appearance of x-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of x-axis title
            )
          ),
          lineColor = "#8eccad", # Set color of x-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on x-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on x-axis
        ) %>%
        hc_yAxis( # Modify appearance of y-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of y-axis title
            )
          ),
          lineColor = "#8eccad", # Set color of y-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on y-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on y-axis
        )
    })

    plt_all_word_freq_clean_data_file <<- all_word_freq_df_clean

    annotation <- cnlp_annotate(input = input_text)

    annotation_df <- annotation$token %>%
      select(id = doc_id, token, lemma, entity = upos) |>
      group_by(entity) |>
      summarize(total = n()) |>
      mutate(entity = forcats::fct_reorder(entity, total)) |>
      arrange(desc(total)) |>
      head(5) |>
      select(Entity = entity, Total = total)

    output$plot_entities <- renderHighchart({

      hchart(annotation_df, "column",
             hcaes(x = Entity, y = Total),
             color = "#b1ddc7", # #8eccad
             borderColor = "#8eccad",
             showInLegend = F,
             name = "") |>
        hc_xAxis( # Modify appearance of x-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of x-axis title
            )
          ),
          tickColor = "#b1ddc7",
          lineColor = "#8eccad", # Set color of x-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on x-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on x-axis
        ) |>
        hc_yAxis( # Modify appearance of y-axis
          title = list(
            style = list(
              color = "#8eccad" # Set color of y-axis title
            )
          ),
          tickColor = "#b1ddc7",
          lineColor = "#8eccad", # Set color of y-axis line
          minorGridLineColor = "#8eccad", # Set color of minor grid lines on y-axis
          gridLineColor = "#8eccad" # Set color of major grid lines on y-axis
        )
    })

    output$both_plots_data <- renderUI({

      fluidRow(
        column(4,
               div(
                 h5("Word Distribution:"),
                 hr(),
                 downloadButton("download_word_plot_file", "Download Word Distribution Plot"),
                 hr(),
                 highchartOutput("plot_freq_table")
               )
        ),
        column(4,
               div(
                 h5("Word Distribution (Clean Text):"),
                 hr(),
                 downloadButton("download_word_plot_clean_file", "Download Word Distribution Plot (clean)"),
                 hr(),
                 highchartOutput("plot_freq_table_clean")
               )
        ),
        column(4,
               div(
                 h5("Entities Distribution:"),
                 hr(),
                 downloadButton("download_word_plot_entities_file", "Download Entity Distribution Plot"),
                 hr(),
                 highchartOutput("plot_entities")
               )
        )
      )
    })

    plt_annotation_data_file <<- annotation_df

  })


  # Download handler for Word Distribution Plot
  output$download_word_plot_file <- downloadHandler(
    filename = function() {
      paste("word_distribution_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = ggplot(plt_all_word_freq_data_file, aes(x = Word, y = Frequency)) +
               geom_col(fill = "#b1ddc7", color = "#8eccad") +
               theme_minimal() +
               labs(title = "Word Distribution", x = "Word", y = "Frequency") +
               theme(axis.title.x = element_text(color = "#8eccad"),
                     axis.title.y = element_text(color = "#8eccad")),
             device = "png")
    }
  )

  # Download handler for Word Distribution Plot
  output$download_word_plot_clean_file <- downloadHandler(
    filename = function() {
      paste("word_distribution_clean_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = ggplot(plt_all_word_freq_clean_data_file, aes(x = Word, y = Frequency)) +
               geom_col(fill = "#b1ddc7", color = "#8eccad") +
               theme_minimal() +
               labs(title = "Word Distribution", x = "Word", y = "Frequency") +
               theme(axis.title.x = element_text(color = "#8eccad"),
                     axis.title.y = element_text(color = "#8eccad")),
             device = "png")
    }
  )

  # Download handler for Word Distribution Plot
  output$download_word_plot_entities_file <- downloadHandler(
    filename = function() {
      paste("word_distribution_entity_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = ggplot(plt_annotation_data_file, aes(x = Entity, y = Total)) +
               geom_col(fill = "#b1ddc7", color = "#8eccad") +
               theme_minimal() +
               labs(title = "Word Distribution", x = "Entity", y = "Frequency") +
               theme(axis.title.x = element_text(color = "#8eccad"),
                     axis.title.y = element_text(color = "#8eccad")),
             device = "png")
    }
  )

  ####### ----------- TAB 2: SENTIMENT ANALYSIS ------------------------ #######

  observeEvent(input$bing_button_file, {

    sentiments <- tidytext::get_sentiments("bing")

    new_colors <- c(negative = "#CC687F", positive = "#ef8a62")

    colors_df <- data.frame(sentiment = names(new_colors),
                              color = new_colors,
                              row.names = NULL)

    # Ensure a column is selected
    req(input$select_column3)

    # Read the data frame from the uploaded file
    df <- data_file()
    selected_col <- input$select_column3

    # Ensure the column exists in the dataframe
    req(selected_col %in% names(df))

    # Access the correct column and prepare the text
    input_text <- df[[selected_col]]

    input_text <- paste(input_text, collapse = "\n")

    docs <- strsplit(input_text, '\n')[[1]]

    tokens <- data.frame(docs = seq_len(length(docs)),
                         text = docs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all') |>
      dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

    tokens_tab <- table(tokens$sentiment) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1)

    output$b_plot <- renderHighchart({
      hchart(tokens_tab, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_colors(c("#CC687F", "#8eccad")) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))
    })

    tokens_list <- tokens |> filter(!is.na(sentiment)) |>
      select(Word = token_lower, Sentiment = sentiment) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$tokens_list_table <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        tokens_list,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    output$bing_plots_file <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("Bing Dictionary:"),
                 hr(),
                 highchartOutput("b_plot") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1),
        column(4,
               div(
                 h5("Bing Dictionary - Table:"),
                 hr(),
                 reactableOutput("tokens_list_table") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )

    })

    tokens_results_file <- tokens |> select(docs, word = token_lower, sentiment) |> filter(!is.na(sentiment))

    tokens_tab_bing_file <<- list(SUMMARY = tokens_tab, DETAIL = tokens_results_file)

  })

  # Summary Download Handler
  output$download_bing_file <- downloadHandler(
    filename = function() {
      paste("results_sentiment_bing_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_bing_file, file)
    }
  )

  observeEvent(input$nrc_button_file, {

    sentiments <- readRDS("./lexicons/df_sentiment.RDS") |>
      rename('sentiment' = 'emotion')

    new_colors <- c(Negative = "#CC687F", Positive = "#ef8a62")

    colors_df <- data.frame(sentiment = names(new_colors),
                            color = new_colors,
                            row.names = NULL)

    # Ensure a column is selected
    req(input$select_column4)

    # Read the data frame from the uploaded file
    df <- data_file()
    selected_col <- input$select_column4

    # Ensure the column exists in the dataframe
    req(selected_col %in% names(df))

    # Access the correct column and prepare the text
    input_text <- df[[selected_col]]

    input_text <- paste(input_text, collapse = "\n")

    docs <- strsplit(input_text, '\n')[[1]]

    tokens <- data.frame(docs = seq_len(length(docs)),
                         text = docs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all') |>
      dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

    tokens_tab <- table(tokens$sentiment) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1)

    output$nv_plot_file <- renderHighchart({
      hchart(tokens_tab, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_colors(c("#CC687F", "#8eccad")) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))
    })

    tokens_list <- tokens |> filter(!is.na(sentiment)) |>
      select(Word = token_lower, Sentiment = sentiment) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$tokens_list_nrc_table <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        tokens_list,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    output$nrc_plots_file <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("NRC Dictionary:"),
                 hr(),
                 highchartOutput("nv_plot_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1),
        column(4,
               div(
                 h5("NRC Dictionary - Table:"),
                 hr(),
                 reactableOutput("tokens_list_nrc_table") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )

    })

    tokens_results_file <- tokens |> select(docs, word = token_lower, sentiment) |> filter(!is.na(sentiment))

    tokens_tab_nrc_file <<- list(SUMMARY = tokens_tab, DETAIL = tokens_results_file)

  })

  # Summary Download Handler
  output$download_nrc_file <- downloadHandler(
    filename = function() {
      paste("results_sentiment_nrc_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_nrc_file, file)
    }
  )

  observeEvent(input$loughran_button_file, {

    sentiments <- readRDS("./lexicons/LoughranMcDonald.rds") |>
      filter(sentiment %in% c("positive", "negative"))

    new_colors <- c(Negative = "#CC687F", Positive = "#ef8a62")

    colors_df <- data.frame(sentiment = names(new_colors),
                            color = new_colors,
                            row.names = NULL)

    # Ensure a column is selected
    req(input$select_column5)

    # Read the data frame from the uploaded file
    df <- data_file()
    selected_col <- input$select_column5

    # Ensure the column exists in the dataframe
    req(selected_col %in% names(df))

    # Access the correct column and prepare the text
    input_text <- df[[selected_col]]

    input_text <- paste(input_text, collapse = "\n")

    docs <- strsplit(input_text, '\n')[[1]]

    tokens <- data.frame(docs = seq_len(length(docs)),
                         text = docs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all') |>
      dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

    tokens_tab_louv <- table(tokens$sentiment) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1)) |>
      dplyr::filter(Var1 %in% c("Negative", "Positive")) |>
      dplyr::mutate(Sentiment = Var1)

    output$louv_plot <- renderHighchart({
      hchart(tokens_tab_louv, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab_louv$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_colors(c("#CC687F", "#8eccad")) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))

    })


    tokens_list <- tokens |> filter(!is.na(sentiment)) |>
      select(Word = token_lower, Sentiment = sentiment) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$tokens_list_louv_table <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        tokens_list,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    output$loughran_plots_file <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("Loughran Dictionary:"),
                 hr(),
                 highchartOutput("louv_plot") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1),
        column(4,
               div(
                 h5("Loughran Dictionary - Table:"),
                 hr(),
                 reactableOutput("tokens_list_louv_table") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )

    })

    tokens_results_file <- tokens |> select(docs, word = token_lower, sentiment) |> filter(!is.na(sentiment))

    tokens_tab_loughran_file <<- list(SUMMARY = tokens_tab_louv, DETAIL = tokens_results_file)

  })

  # Summary Download Handler
  output$download_loughran_file <- downloadHandler(
    filename = function() {
      paste("results_sentiment_loughran_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_loughran_file, file)
    }
  )

  observeEvent(input$afinn_button_file, {

    sentiments <- readRDS("./lexicons/afinn_sentiment.rds") |>
      mutate(sentiment = ifelse(value < 0, "negative", "positive"))

    new_colors <- c(Negative = "#CC687F", Positive = "#ef8a62")

    colors_df <- data.frame(sentiment = names(new_colors),
                            color = new_colors,
                            row.names = NULL)

    # Ensure a column is selected
    req(input$select_column6)

    # Read the data frame from the uploaded file
    df <- data_file()
    selected_col <- input$select_column6

    # Ensure the column exists in the dataframe
    req(selected_col %in% names(df))

    # Access the correct column and prepare the text
    input_text <- df[[selected_col]]

    input_text <- paste(input_text, collapse = "\n")

    docs <- strsplit(input_text, '\n')[[1]]

    tokens <- data.frame(docs = seq_len(length(docs)),
                         text = docs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all') |>
      dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

    tokens_tab_afinn <- table(tokens$sentiment) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1)

    output$av_plot_file <- renderHighchart({
      hchart(tokens_tab_afinn, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab_afinn$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_colors(c("#CC687F", "#8eccad")) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))

    })

    tokens_list <- tokens |> filter(!is.na(sentiment)) |>
      select(Word = token_lower, Sentiment = sentiment) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$tokens_list_afinn_table <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        tokens_list,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })


    output$afinn_plots_file <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("Afinn Dictionary:"),
                 hr(),
                 highchartOutput("av_plot_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1),
        column(4,
               div(
                 h5("Afinn Dictionary - Table:"),
                 hr(),
                 reactableOutput("tokens_list_afinn_table") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )

    })

    tokens_results_file <- tokens |> select(docs, word = token_lower, sentiment) |> filter(!is.na(sentiment))

    tokens_tab_afinn_file <<- list(SUMMARY = tokens_tab_afinn, DETAIL = tokens_results_file)

  })

  # Summary Download Handler
  output$download_afinn_file <- downloadHandler(
    filename = function() {
      paste("results_sentiment_affin_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_afinn_file, file)
    }
  )

  ####### ----------- TAB 3: EMOTION ANALYSIS -------------------------- #######

  observeEvent(input$nrc_emotion_button_file, {

    sentiments <- readRDS("./lexicons/df_emotions.RDS")

    # Ensure a column is selected
    req(input$select_column7)

    # Read the data frame from the uploaded file
    df <- data_file()
    selected_col <- input$select_column7

    # Ensure the column exists in the dataframe
    req(selected_col %in% names(df))

    # Access the correct column and prepare the text
    input_text <- df[[selected_col]]

    input_text <- paste(input_text, collapse = "\n")

    docs <- strsplit(input_text, '\n')[[1]]

    tokens <- data.frame(docs = seq_len(length(docs)),
                         text = docs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all')

    tokens_tab <- table(tokens$emotion) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1)

    output$n_emotion_plot_file <- renderHighchart({
      hchart(tokens_tab, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))

    })

    df_emotion <- tokens |> filter(!is.na(emotion)) |>
      select(Word = token_lower, Emotion = emotion) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$n_emotion_table_file <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        df_emotion,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    tokens_results <- tokens |> select(docs, word = token_lower, emotion) |> filter(!is.na(emotion))

    tokens_tab_emo_nrc_file <<- list(SUMMARY = tokens_tab, DETAIL = tokens_results)

    output$nrc_emotion_plots_file <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("NRC Emotions:"),
                 hr(),
                 downloadButton("download_emotion_nrc_file", "Download Results as Excel"),
                 hr(),
                 highchartOutput("n_emotion_plot_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1),
        column(4,
               div(
                 h5("NRC Emotions - Table:"),
                 br(),
                 br(),
                 br(),
                 br(),
                 hr(),
                 reactableOutput("n_emotion_table_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )

    })

  })

  # Summary Download Handler
  output$download_emotion_nrc_file <- downloadHandler(
    filename = function() {
      paste("results_emotion_nrc_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_emo_nrc_file, file)
    }
  )

  observeEvent(input$loughran_emotion_button_file, {

    sentiments <- readRDS("./lexicons/LoughranMcDonald.rds") |>
      filter(!sentiment %in% c("positive", "negative"))

    # Ensure a column is selected
    req(input$select_column8)

    # Read the data frame from the uploaded file
    df <- data_file()
    selected_col <- input$select_column8

    # Ensure the column exists in the dataframe
    req(selected_col %in% names(df))

    # Access the correct column and prepare the text
    input_text <- df[[selected_col]]

    input_text <- paste(input_text, collapse = "\n")

    docs <- strsplit(input_text, '\n')[[1]]

    tokens <- data.frame(docs = seq_len(length(docs)),
                         text = docs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all')

    tokens_tab <- table(tokens$sentiment) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1)

    output$loughran_emotion_plot_file <- renderHighchart({
      hchart(tokens_tab, "column", hcaes(x = Sentiment, y = Freq, group = Sentiment)) %>%
        hc_plotOptions(column = list(stacking = "normal")) %>%
        hc_tooltip(pointFormat = "Frequency: {point.y}") %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_xAxis(categories = tokens_tab$Var1) %>%
        hc_legend(enabled = FALSE) %>%
        hc_chart(type = "bar") %>%
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, align = "right", color = "#FFFFFF", format = "{point.y}")))

    })

    df_loughran_emotion <- tokens |> filter(!is.na(sentiment)) |>
      select(Word = token_lower, Emotion = sentiment) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$loughran_emotion_table_file <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        df_loughran_emotion,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    output$loughran_emotion_plots_file <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("Loughran McDonald Emotions:"),
                 hr(),
                 downloadButton("download_emotion_loughran_file", "Download Results as Excel"),
                 hr(),
                 highchartOutput("loughran_emotion_plot_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1),
        column(4,
               div(
                 h5("Loughran McDonald Emotions - Table:"),
                 br(),
                 br(),
                 br(),
                 br(),
                 hr(),
                 reactableOutput("loughran_emotion_table_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )

    })

    tokens_results <- tokens |> select(docs, word = token_lower, sentiment) |> filter(!is.na(sentiment))

    tokens_tab_emo_loughran_file <<- list(SUMMARY = tokens_tab, DETAIL = tokens_results)

  })

  # Summary Download Handler
  output$download_emotion_loughran_file <- downloadHandler(
    filename = function() {
      paste("results_emotion_loughran_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(tokens_tab_emo_loughran_file, file)
    }
  )

  ####### ----------- TAB 4: AROUSAL & VALENCE ------------------------- #######

  observeEvent(input$arousal_button_file, {

    sentiments <- readRDS("./lexicons/df_arousal.RDS")

    # Ensure a column is selected
    req(input$select_column9)

    # Read the data frame from the uploaded file
    df <- data_file()
    selected_col <- input$select_column9

    # Ensure the column exists in the dataframe
    req(selected_col %in% names(df))

    # Access the correct column and prepare the text
    input_text <- df[[selected_col]]

    input_text <- paste(input_text, collapse = "\n")

    docs <- strsplit(input_text, '\n')[[1]]

    tokens <- data.frame(docs = seq_len(length(docs)),
                         text = docs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all')

    tokens_tab <- table(tokens$level) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1) |>
      group_by(Sentiment) |>
      summarize(dist = sum(Freq)) |>
      mutate(percent = round((dist/sum(dist)), 3), label = paste0("Arousal Level: ", Sentiment, " - ", percent*100, "%"))

    output$arousal_plot_file <- renderHighchart({
      hchart(tokens_tab,
             "pie", hcaes(x = label, y = percent),
             name = "Arousal Distribution")
    })

    df_arousal <- tokens |> filter(!is.na(level)) |>
      select(Word = token_lower, Arousal = level) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$arousal_table_file <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        df_arousal,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    tokens_tab_arousal_file <<- list(SUMMARY = tokens_tab, DETAILS = df_arousal)

    output$arousal_plots_file <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("Arousal:"),
                 hr(),
                 downloadButton("download_arousal_file", "Download Results as Excel"),
                 hr(),
                 highchartOutput("arousal_plot_file")
               )),
        column(1),
        column(4,
               div(
                 h5("Arousal - Table:"),
                 br(),
                 br(),
                 br(),
                 br(),
                 hr(),
                 reactableOutput("arousal_table_file")
               )),
        column(1)
      )

    })

    # Summary Download Handler
    output$download_arousal_file <- downloadHandler(
      filename = function() {
        paste("results_arousal_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(tokens_tab_arousal_file, file)
      }
    )

  })

  observeEvent(input$valence_button_file, {

    sentiments <- readRDS("./lexicons/df_valence.RDS")

    # Ensure a column is selected
    req(input$select_column10)

    # Read the data frame from the uploaded file
    df <- data_file()
    selected_col <- input$select_column10

    # Ensure the column exists in the dataframe
    req(selected_col %in% names(df))

    # Access the correct column and prepare the text
    input_text <- df[[selected_col]]

    input_text <- paste(input_text, collapse = "\n")

    docs <- strsplit(input_text, '\n')[[1]]

    tokens <- data.frame(docs = seq_len(length(docs)),
                         text = docs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all')

    tokens_tab <- table(tokens$level) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1) |>
      group_by(Sentiment) |>
      summarize(dist = sum(Freq)) |>
      mutate(percent = round((dist/sum(dist)), 3), label = paste0("Valence Level: ", Sentiment, " - ", percent*100, "%"))

    output$valence_plot_file <- renderHighchart({
      hchart(tokens_tab,
             "pie", hcaes(x = label, y = percent),
             name = "Valence Distribution")
    })

    df_valence <- tokens |> filter(!is.na(level)) |>
      select(Word = token_lower, Valence = level) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$valence_table_file <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        df_valence,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    output$valence_plots_file <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("Valence:"),
                 hr(),
                 downloadButton("download_valence_file", "Download Results as Excel"),
                 hr(),
                 highchartOutput("valence_plot_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1),
        column(4,
               div(
                 h5("Valence - Table:"),
                 br(),
                 br(),
                 br(),
                 br(),
                 hr(),
                 reactableOutput("valence_table_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )

    })

    tokens_tab_valence_file <<- list(SUMMARY = tokens_tab, DETAILS = df_valence)

    # Summary Download Handler
    output$download_valence_file <- downloadHandler(
      filename = function() {
        paste("results_valence_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(tokens_tab_valence_file, file)
      }
    )

  })

  observeEvent(input$dominance_button_file, {

    sentiments <- readRDS("./lexicons/df_dominance.RDS")

    # Ensure a column is selected
    req(input$select_column11)

    # Read the data frame from the uploaded file
    df <- data_file()
    selected_col <- input$select_column11

    # Ensure the column exists in the dataframe
    req(selected_col %in% names(df))

    # Access the correct column and prepare the text
    input_text <- df[[selected_col]]

    input_text <- paste(input_text, collapse = "\n")

    docs <- strsplit(input_text, '\n')[[1]]

    tokens <- data.frame(docs = seq_len(length(docs)),
                         text = docs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all')

    tokens_tab <- table(tokens$level) |> as.data.frame() |>
      dplyr::mutate(Var1 = str_to_title(Var1),
                    Sentiment = Var1) |>
      group_by(Sentiment) |>
      summarize(dist = sum(Freq)) |>
      mutate(percent = round((dist/sum(dist)), 3), label = paste0("Dominance Level: ", Sentiment, " - ", percent*100, "%"))

    output$dominance_plot_file <- renderHighchart({
      hchart(tokens_tab,
             "pie", hcaes(x = label, y = percent),
             name = "Dominance Distribution")
    })

    df_dominance <- tokens |> filter(!is.na(level)) |>
      select(Word = token_lower, Dominance = level) |>
      mutate(id = row_number()) |>
      select(id, everything())

    output$dominance_table_file <- renderReactable({

      options(reactable.theme = reactableTheme(
        color = "#8eccad",
        backgroundColor = "hsl(233, 9%, 19%)",
        borderColor = "hsl(233, 9%, 22%)",
        stripedColor = "hsl(233, 12%, 22%)",
        highlightColor = "hsl(233, 12%, 24%)",
        inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
        pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      ))

      reactable(
        df_dominance,
        rownames = FALSE,
        defaultPageSize = 5,
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    output$dominance_plots_file <- renderUI({

      fluidRow(
        column(1),
        column(5,
               div(
                 h5("Dominance:"),
                 hr(),
                 downloadButton("download_dominance_file", "Download Results as Excel"),
                 hr(),
                 highchartOutput("dominance_plot_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1),
        column(4,
               div(
                 h5("Dominance - Table:"),
                 br(),
                 br(),
                 br(),
                 br(),
                 hr(),
                 reactableOutput("dominance_table_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )

    })

    tokens_tab_dominance_file <<- list(SUMMARY = tokens_tab, DETAILS = df_dominance)


    # Summary Download Handler
    output$download_dominance_file <- downloadHandler(
      filename = function() {
        paste("results_dominance_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(tokens_tab_dominance_file, file)
      }
    )

  })


  ####### ----------- TAB 5: TOPIC MODELLING LDA ----------------------- #######

  data_lda_file <- reactive({
    # Read the data frame from the uploaded file
    df <- data_file()

    # Ensure a column is selected
    req(input$select_column12)

    selected_col <- input$select_column12

    # Access the correct column and prepare the text
    input_text <- df |> select(text = selected_col)

    forTM <- input_text |>
      unnest_tokens(word, text) |>
      anti_join(stop_words) |>
      count(word, sort = TRUE) |>
      mutate(id = row_number()) |>
      select(id, everything()) |>
      cast_dtm(id, word, n)

    return(forTM)

  })

  observeEvent(input$lda_button_file, {

    # Ensure a column is selected
    req(input$numTopics2)

    myLDA <- LDA(data_lda_file(), k = input$numTopics2, control = list(seed = 1234))

    ap_topics <- tidy(myLDA, matrix = "beta")

    ap_top_terms <- ap_topics %>%
      group_by(topic) %>%
      slice_max(beta, n = 10) %>%
      ungroup() %>%
      arrange(topic, -beta) |>
      group_by(topic) %>%
      slice_max(term, n = 8)

    output$lda_plot_file <- renderPlot({
      ap_top_terms %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(beta, term, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        scale_y_reordered() +
        theme(axis.text = element_text(size = 16),
              axis.text.y = element_text(face = "bold"),
              strip.text = element_text(size = 18, face = "bold"))
    })

    df_predominant_topic <-  tidy(myLDA, matrix = "gamma") |>
      group_by(document, topic) |>
      summarize(total = mean(gamma)) |>
      slice_max(total, n = 1) |>
      group_by(topic) |>
      summarize(dist = mean(total)) |>
      mutate(percent = round((dist/sum(dist)), 3), label = paste0("Topic ", topic, ": ", percent*100, "%"))

    output$lda_plot_pie_file <- renderHighchart({

      hchart(df_predominant_topic,
             "pie", hcaes(x = label, y = percent),
             name = "Topic Distribution")

    })

    topic_docs <- tidy(myLDA, matrix = "gamma") %>%
      group_by(document, topic) %>%
      summarize(total = mean(gamma), .groups = 'drop') %>%
      ungroup() |>
      group_by(document) %>%
      mutate(total_gamma = sum(total),             # Total gamma per document
             percent = round(total / total_gamma * 100, 2))  %>%  # Calculate percentage
      ungroup()

    plt_sample <- topic_docs |> select(document) |> unique() |> sample_n(10, replace = FALSE) |> pull()

    output$lda_plotTop_file <- renderPlot({
      ggplot(topic_docs |> filter(document %in% plt_sample), aes(x = document, y = total, fill = as.factor(topic))) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = sprintf("%.1f%%", percent)),   # Format the text labels to one decimal place
                  position = position_stack(vjust = 0.5),   # Center the text in each stack
                  color = "white",                         # Text color
                  size = 3) +
        labs(title = "",
             subtitle = "",
             x = "Document",
             y = "Share of topics [%]",
             fill = "Topic") +
        # theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.text = element_text(size = 16),
              axis.text.y = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold", size = 18),
              strip.text = element_text(size = 18, face = "bold"),
              legend.key.size = unit(1., 'cm'),
              legend.title = element_text(face = "bold", size = 17),
              legend.text = element_text(face = "bold", size = 17))
    })



    output$topics_file <- renderUI({

      fluidRow(
        column(1),
        column(10,
               div(
                 h5("The terms that are most common within each topic:"),
                 hr(),
                 plotOutput("lda_plot_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )
    })

    output$topics_pie_file <- renderUI({

      fluidRow(
        column(1),
        column(10,
               div(
                 br(),
                 h5("Prevalence distribution across Topics in Document:"),
                 hr(),
                 highchartOutput("lda_plot_pie_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )
    })

    output$topics_pieTop_file <- renderUI({

      fluidRow(
        column(1),
        column(10,
               div(
                 br(),
                 h5("Share of topics [%] by Document (from a Random Sample):"),
                 hr(),
                 plotOutput("lda_plotTop_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )
    })

  })

  observeEvent(input$lda_test_button_file, {

    req(data_lda_file())

    FindTopics_result <- ldatuning::FindTopicsNumber(
      data_lda_file(),
      topics = seq(2, 10, by = 1),
      metrics = c("CaoJuan2009",  "Deveaud2014"),
      method = "Gibbs",
      control = list(seed = 1234),
      verbose = TRUE
    )

    values <- FindTopics_result[!names(FindTopics_result) %in% c("LDA_model")]

    # normalize to [0,1]
    columns <- base::subset(values, select = 2:ncol(values))
    values <- base::data.frame(
      values["topics"],
      base::apply(columns, 2, function(column) {
        scales::rescale(column, to = c(0, 1), from = range(column))
      })
    )

    # melt
    values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)

    # separate max-arg & min-arg metrics
    values$group <- values$variable %in% c("Griffiths2004", "Deveaud2014")
    values$group <- base::factor(
      values$group,
      levels = c(FALSE, TRUE),
      labels = c("Minimize", "Maximize")
    )

    output$test_number_k_file <- renderPlot({
      p <- ggplot(values, aes_string(x = "topics", y = "value", group = "variable"),
                  height = 500)
      p <- p + geom_line()
      p <- p + geom_point(aes_string(shape = "variable"), size = 3, color = "white")
      p <- p + guides(size = FALSE, shape = guide_legend(title = "Metrics:"))
      p <- p + scale_x_continuous(breaks = values$topics)
      p <- p + labs(x = "Number of Topics", y = NULL)

      # separate in two parts
      p <- p + facet_grid(group ~ .)
      # style
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1),
                     axis.text = element_text(size = 16),
                     axis.text.y = element_text(face = "bold"),
                     axis.title.y = element_text(face = "bold", size = 18),
                     axis.title.x = element_text(face = "bold", size = 18),
                     strip.text = element_text(size = 18, face = "bold"),
                     legend.key.size = unit(1., 'cm'),
                     legend.title = element_text(face = "bold", size = 17),
                     legend.text = element_text(face = "bold", size = 17))
      p
    })

    output$test_k_plot_file <- renderUI({

      fluidRow(
        column(3),
        column(6,
               div(
                 # style = "height:1000px;",
                 h5("Checking the best number of Topics:"),
                 hr(),
                 plotOutput("test_number_k_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(3)
      )
    })

  })

  observeEvent(input$lda_evaluation_button_file, {

    req(input$numTopics2, data_lda_file())

    myLDA <- LDA(data_lda_file(), k = input$numTopics2, control = list(seed = 1234))

    diag_df <- topicdoc::topic_diagnostics(myLDA, data_lda_file())

    ap_topics <- tidy(myLDA, matrix = "beta")
    ap_top_terms <- ap_topics %>%
      group_by(topic) %>%
      slice_max(beta, n = 5) %>%
      ungroup() %>%
      arrange(topic, -beta) |>
      group_by(topic) |>
      slice_max(term, n = 5) |>
      select(-beta) |>
      summarize(topic_label = paste(term, collapse = ", "), .groups = "drop")

    diag_df <- diag_df |>
      left_join(ap_top_terms, by = c("topic_num" = "topic")) |>
      mutate(
        dist_from_corpus = round(dist_from_corpus, 2),
        tf_df_dist = round(tf_df_dist, 2),
        topic_coherence = round(topic_coherence, 2),
        topic_exclusivity = round(topic_exclusivity, 2)
      )

    output$topic_eval_file <- renderPlot({
      p <- diag_df %>%
        gather(diagnostic, value, -topic_label, -topic_num) %>%
        ggplot(aes(x = topic_num, y = value,
                   fill = str_wrap(topic_label, 25))) +
        geom_bar(stat = "identity") +
        facet_wrap(~diagnostic, scales = "free", ncol = 7) +
        labs(x = "Topic Number", y = "Diagnostic Value",
             fill = "Topic Label", title = "")

      # style
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1),
                     axis.text = element_text(size = 16),
                     axis.text.y = element_text(face = "bold"),
                     axis.title.y = element_text(face = "bold", size = 18),
                     axis.title.x = element_text(face = "bold", size = 18),
                     strip.text = element_text(size = 18, face = "bold"),
                     legend.position = "bottom",
                     legend.key.size = unit(1., 'cm'),
                     legend.title = element_text(face = "bold", size = 17),
                     legend.text = element_text(face = "bold", size = 17))
      p
    })

    output$topics_evaluation_file <- renderUI({

      fluidRow(
        column(1),
        column(10,
               div(
                 h5("All Topic Model Diagnostics:"),
                 hr(),
                 plotOutput("topic_eval_file") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(1)
      )
    })

  })


  ####### ----------- TAB 6: SEEDED LDA -------------------------------- #######

  data_seededlda_file <- reactive({
    req(data_file(), input$select_column13)

    # Read the data frame from the uploaded file
    df <- data_file()

    selected_col <- input$select_column13

    # Access the correct column and prepare the text
    input_text <- df |> select(text = selected_col)

    # Prepare for LDA
    corpus <- corpus(input_text)
    toks <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)
    toks <- tokens_remove(toks, pattern = c(stopwords("en"), "*-time", "updated-*", "gmt", "bst"))
    dfm <- dfm(toks)

    return(dfmt)

  })

  data_seededlda_file_default <- reactive({
    req(data_file(), input$select_column13)

    # Read the data frame from the uploaded file
    df <- data_file()

    selected_col <- input$select_column13

    # Access the correct column and prepare the text
    input_text <- df |> select(text = selected_col)

    # Prepare for LDA
    corpus <- corpus(input_text)
    toks <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)
    toks <- tokens_remove(toks, pattern = c(stopwords("en"), "*-time", "updated-*", "gmt", "bst"))
    dfm <- dfm(toks)

    return(dfmt)

  })

  reactive_dict_file <- reactiveVal(list())

  observeEvent(input$create_dict2, {
    # Check if we're adding to the dictionary
    if (input$dict_type2 == "create_dictionary" && nzchar(input$category_name) && nzchar(input$related_words)) {
      # Split words and trim whitespace
      words <- strsplit(input$related_words, ",\\s*")[[1]]

      # Add or update the category in the dictionary
      new_dict <- reactive_dict_file()
      new_dict[[input$category_name]] <- words
      reactive_dict_file(new_dict)

      # Display the updated dictionary
      output$display_dict2 <- renderText({
        if (length(reactive_dict_file()) > 0) {
          paste(sapply(names(reactive_dict_file()), function(cat) {
            sprintf("- [%s]:\n  - %s", cat, paste(reactive_dict_file()[[cat]], collapse=", "))
          }), collapse="\n")
        } else {
          "No categories defined."
        }
      })
    }
  }, ignoreNULL = FALSE)

  # Add UI elements conditionally
  output$customDictionaryUI2 <- renderUI({
    if (input$dict_type2 == "create_dictionary") {
      fluidRow(
        column(6, textInput("category_name", "Enter Category Name:")),
        column(6, textAreaInput("related_words", "Add Related Words (comma-separated):", height = '100px'))
      )
    }
  })

  # Listen for changes in the input 'dict_type'
  observeEvent(input$seededlda_button_file_default2, {

    req(data_seededlda_file()) # create a new dataset

    dfmt_file <- data_seededlda_file()

    # Load the dictionary
    dict_topic <- dictionary(file = "./lexicons/dictionary.yml")

    # Run seeded LDA
    lda_seed_file <- seededlda::textmodel_seededlda(dfmt_file, dict_topic, batch_size = 0.01, auto_iter = TRUE,
                                                    verbose = TRUE)

    # Output the terms from lda_seed model
    options(reactable.theme = reactableTheme(
      color = "#8eccad",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ))

    output$ldaResults2 <- renderReactable({
      reactable(
        as.data.frame(terms(lda_seed_file)),
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    # assign topics from seeded LDA as a document-level variable to the dfm
    dfmt_file$topic2 <- seededlda::topics(lda_seed_file)

    dominant_topic_file <- dfmt_file@docvars |>
      group_by(topic2) |>
      summarize(total = n()) |>
      mutate(percent = round((total/sum(total)), 3),
             label = paste0("Topic ", topic2, ": ", percent*100, "%"))

    output$seededlda_plot_pie2 <- renderHighchart({

      hchart(dominant_topic_file,
             "pie", hcaes(x = label, y = percent),
             name = "Topic Distribution")

    })


    # Render LDA Results Table
    output$ldaTable3 <- renderUI({
      fluidRow(
        hr(),
        h5(HTML("<b>Results by Default Dictionary:</b>")),
        column(6,
               div(
                 h5("SeededLDA Results:"),
                 br(),
                 reactableOutput("ldaResults2") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(6,
               div(
                 br(),
                 h5("Distribution across Topics in Text:"),
                 br(),
                 highchartOutput("seededlda_plot_pie2") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               ))
      )
    })


  })

  # Listen for changes in the input 'dict_type'
  observeEvent(input$seededlda_button_file_custom2, {
    # Ensure the data is available
    req(data_seededlda_file(), reactive_dict_file())

    dfmt <- data_seededlda_file()

    # Load the dictionary
    dict_topic <- reactive_dict_file()
    dict_topic <- dictionary(dict_topic)

    # Run seeded LDA
    lda_seed2 <- textmodel_seededlda(dfmt, dict_topic, batch_size = 0.01, auto_iter = TRUE, verbose = TRUE)

    # Output the terms from lda_seed model
    options(reactable.theme = reactableTheme(
      color = "#8eccad",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ))

    output$ldaResults_custom <- renderReactable({
      reactable(
        as.data.frame(terms(lda_seed2)),
        filterable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })

    # assign topics from seeded LDA as a document-level variable to the dfm
    dfmt$topic2 <- topics(lda_seed2)

    dominant_topic <- dfmt@docvars |>
      group_by(topic2) |>
      summarize(total = n()) |>
      mutate(percent = round((total/sum(total)), 3),
             label = paste0("Topic ", topic2, ": ", percent*100, "%"))

    output$seededlda_plot_pie_custom <- renderHighchart({

      hchart(dominant_topic,
             "pie", hcaes(x = label, y = percent),
             name = "Topic Distribution")

    })


    # Render LDA Results Table
    output$ldaTable4 <- renderUI({

      fluidRow(
        hr(),
        h5(HTML("<b>Results by Custom Dictionary:</b>")),
        column(6,
               div(
                 br(),
                 h5("SeededLDA Results:"),
                 br(),
                 reactableOutput("ldaResults_custom") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               )),
        column(6,
               div(
                 br(),
                 h5("Distribution across Topics in Text:"),
                 br(),
                 highchartOutput("seededlda_plot_pie_custom") |> shinycssloaders::withSpinner(color="#0dc5c1", type = 5)
               ))
      )
    })
  })


  observeEvent(input$reset_button_file, {

    session$reload()

  })


}

# Run the application
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))


