library(shiny)
library(stringr)
library(tidyr)
library(dplyr)

## HELPER FUNCTIONS
# predictNextWord <- function(input_sentence, model, df) {
#     
#     if (model == "bigram") {
#         word_1 <- word(input_sentence, -1)
#         word_1 <- word_1[[1]]
#         word_1 <- str_to_lower(word_1)
#         next_word_df <- df %>%
#             filter(word1 == word_1) %>%
#             filter(bigram_prob == max(bigram_prob))
# 
#         next_word <- next_word_df$word2
# 
#     } else if (model == "trigram") {
#         word_2 <- word(input_sentence, -1)
#         word_2 <- word_2[[1]]
#         word_1 <- word(input_sentence, -2)
#         word_1 <- word_1[[1]]
# 
#         word_1 <- str_to_lower(word_1)
#         word_2 <- str_to_lower(word_2)
# 
#         next_word_df <- df %>%
#             separate(word1_word2, c("word1", "word2"), sep = " ") %>%
#             filter(word1 == word_1, word2 == word_2) %>%
#             filter(trigram_prob == max(trigram_prob))
# 
#         next_word <- next_word_df$word3
# 
#     } else {
#         return ("Incorrect Ngram config.")
#     }
# 
#     return(next_word)
#     
# }


last <- function(x) { return( x[length(x)] ) }
second_last <- function(x) { return( x[length(x)-1]) }

predictNextWord <- function(input_sentence, bigram_prob_df, trigram_prob_df) {
    if (lengths(strsplit(input_sentence, " ")) == 1) {
        word_1 <- strsplit(input_sentence, " ")[[1]][1]
        
        word_1 <- str_to_lower(word_1)
        
        next_word_df <- bigram_prob_df %>% 
            filter(word1 == word_1) %>%
            filter(bigram_prob == max(bigram_prob)) %>%
            head(1)
        
        
        next_word <- next_word_df$word2
    } else {
        word_2 <- last(strsplit(input_sentence, " ")[[1]])
        word_1 <- second_last(strsplit(input_sentence, " ")[[1]])
        
        word_1 <- str_to_lower(word_1)
        word_2 <- str_to_lower(word_2)
        
        
        next_word_bigram <- bigram_prob_df %>% 
            filter(word1 == word_2) %>%
            filter(bigram_prob == max(bigram_prob)) %>%
            head(1)
        
        
        # next_word_trigram <- trigram_prob_df %>% 
        #     separate(word1_word2, c("word1", "word2"), sep = " ") %>%
        #     filter(word1 == word_1, word2 == word_2) %>%
        #     filter(trigram_prob == max(trigram_prob)) %>%
        #     head(1)
        # 
        # if (nrow(next_word_trigram) == 0) {
        #     next_word <- next_word_bigram$word2
        # } else if (nrow(next_word_bigram) == 0) {
        #     next_word <- next_word_trigram$word3
        # } else if (next_word_bigram$bigram_prob > next_word_trigram$trigram_prob) {
        #     next_word <- next_word_bigram$word2
        # } else if (next_word_bigram$bigram_prob < next_word_trigram$trigram_prob) {
        #     next_word <- next_word_trigram$word3   
        # } else {
        #     next_word <- "No prediction. Type a few more words."
        # }
        
        if (nrow(next_word_bigram) == 0) {
            next_word <- "No prediction can be made. Enter more text."
        } else {
            next_word <- next_word_bigram$word2
        }    
        
        return(next_word)   
    }
    
    
}


df_us_bigrams_probs <- readRDS("bigram_probs.rds")
# df_us_trigrams_probs <- readRDS("trigram_probs.rds")

# print(predictNextWord("hello how", bigram_prob = df_us_bigrams_probs, trigram_prob = df_us_trigrams_probs))


#######################################################################################
#######################################################################################
#######################################################################################


# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("TextPredictR"),

    sidebarLayout(
        sidebarPanel(
            p("Text predictor app."),
            p("The model was trained on 0.1% of the total dataset size due to memory and time-to-train constraints."),
            p("Enter the text in the text box and obtain predictions."),
            p("Please wait for a few seconds for the app to initialize."),
            p("Ensure you don't have trailing spaces at the end of the sentence."),
            p("A few examples to try: "),
            p('> "Hi"'),
            p('> "Hello there, Obi Wan Kenobi, How are "'),
            p('> "We need to stop global warming because time is"'),
            p('> "My car is"')
        ),
        mainPanel(
            h2("Enter Text."),
            textInput(inputId = "input_text", label = "Write text to generate predictions."),
            verbatimTextOutput(outputId = "output_text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$output_text <- renderText({
        if (input$input_text == "") {
            return ("")
        } else {
            predictNextWord(input$input_text, bigram_prob = df_us_bigrams_probs, trigram_prob = df_us_trigrams_probs)
        }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
