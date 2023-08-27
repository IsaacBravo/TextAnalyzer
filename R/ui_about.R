ui_about_part1 <- function() {

           fluidRow(
             column(1,  style = "width: 5px;"),
             column(11,
                    h3("Welcome to the Text Analysis Shiny App Package.",
                       class = "data-main-title"),
                    p("The Text Analysis Shiny App Package is a comprehensive R package that provides an interactive Shiny web application for analyzing and visualizing text data. This package enables users to perform sentiment analysis, word frequency analysis, and highlight specific words in the input text.",
                      "If you want access to the repository of this package, see ",
                      a("here.", href = "https://github.com/IsaacBravo/TextAnalizer/tree/master",
                        target = "_blank",
                        class = "here-pop-up",
                        id = "here",
                        bsPopover(id="here", title = '<span class="pop"><b>GitHub Repository</b></span>',
                                  content = '<span class="pop-content">Check it out!</span>',
                                  trigger = "hover",
                                  placement = "right",
                                  options = list(container = "body"))
                      )),
                    br()
             ))
  
}

ui_about_part2 <- function() {
  
  fluidRow(
    column(1,  style = "width: 5px;"),
    column(11,
           h4("Features"),
           p(tags$ul(tags$li("Sentiment Analysis: Analyze the sentiment of the provided text using the sentimentr package, visualizing sentiments at both the overall and sentence levels."),
                     tags$li("Word Frequency Analysis: Generate a word frequency table and visualize it using an interactive word cloud, allowing users to explore the most common words in the input text."),
                     tags$li("Word Highlighting: Highlight positive and negative words in the text with customizable background and text colors for quick identification."))),
           hr(),
           HTML("<p>(Made by <a href='https://twitter.com/IsaacBr45419303'>@IsaacBr</a>. Source code <a href='https://github.com/IsaacBravo/TextAnalizer'>on GitHub</a>.)</p>")
    ))
  
}