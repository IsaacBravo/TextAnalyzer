sidebar_ui <- function() {
  # Define sidebar UI
  sidebarLayout(
    sidebarPanel(
      textAreaInput("input_text", label = "Enter Text:", width = "100%", height = "200px"),
      tags$style(
        HTML("#input_text { width: 100%; }")
      ),
      textInput("highlight_words", label = "Highlight Words (comma-separated):"),
      p("Enter the keyword(s) to be highlighted in the text, based on the valence."),
      actionButton("highlight_positive", "Highlight Positive Words"),
      br(),
      br(),
      actionButton("highlight_negative", "Highlight Negative Words"),
      br(),
      br(),
      
    ),
    mainPanel(
      p("This application allows you to copy a text from any type of file to run 
        a basic and exploratory text analysis."),
      p(tags$ul(tags$li("The first tab focuses on displaying the text loaded in the application. 
                        This tab can provides a brief summary of the text loaded, and also allows
                        to the user highlight keywords, or positive and negative words."),
                tags$li("The second set presents a basic text analysis, showing the 
                        words frequency of the text loaded, along with a visualization 
                        of the data."),
                tags$li("The third set presents a sentiment analysis based on the 
                        package sentimentr, where the user can check the valence by
                         sentence, and the tendency of the valence across the text."))),
      hr(),
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
                 br(),
                
                 fluidRow(
                   column(2),
                   column(3, actionButton("word_freq_button", "Show Summary Tables")),
                   column(2),
                   column(3, actionButton("plot_freq_button", "Show Summary Plots")),
                   column(2)
                 ),
                 fluidRow(
                   column(6, 
                          reactableOutput("word_freq_table"),
                          align = "center"
                   ),
                   column(6,
                          wordcloud2Output("cloud"),
                          align = "center"
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(6, 
                          reactableOutput("table_entities"),
                          align = "center"
                   ),
                   column(6,
                          plotOutput("plot_entities"),
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
                          br(),
                          p(HTML("<i>If you wish to see the output from sentiment_by line by line with positive/negative sentences highlighted, check out this button!</i>")),                     
                 align = "center")
                 ),
                 br(),
                 fluidRow(
                   column(2),
                   column(8, 
                          reactableOutput("table_sentences_avg"),
                          align = "center"
                   ),
                   column(2)
                 ),    
                 br(),
                 fluidRow(
                   column(2),
                   column(8, 
                          reactableOutput("table_sentences"),
                          align = "center"
                   ),
                   column(2)
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
}
