library(shiny)
library(tidyverse)

ui <- fluidPage(
   
        titlePanel("Text Prediction"),
        mainPanel(
                textInput("text","Write your text below:","I am"),
                submitButton("Click here to submit"),
                helpText("Below are the possible results"),
                tableOutput("result1"),
                tableOutput("result2")
))

server <- function(input, output) {
        ##Load the data and combine into one sample
        
        blogs <- readLines("en_US.blogs.txt",skipNul = T)
        blogsSample <- sample(blogs,1000,replace = F) 
        
        news <- readLines("en_US.news.txt",skipNul = T)
        newsSample <- sample(news,1000,replace = F)
        
        twitter <- readLines("en_US.twitter.txt",skipNul = T)
        twitterSample <- sample(twitter,1000,replace = F)
        
        combineSample <- c(blogsSample,newsSample,twitterSample)
        
        ##Filter profanity
        profane <- "[Ss][Hh][Ii][Tt]|[Pp][Ii][Ss][Ss]|[Ff][Uu][Cc][Kk]|[Cc][Uu][Nn][Tt]|[Cc][Oo][Cc][Kk]|[Tt][Ii][Tt]"
        combineSampleClean <- combineSample[-grep(profane,combineSample)]
        
        #Find common twoGrams and threeGrams
        twoGrams <- data_frame(text =  combineSampleClean) %>% 
                mutate(text = tolower(text)) %>% 
                mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
                mutate(phrase = str_extract_all(text, "[a-z]+\\s+[a-z]+")) %>%
                unnest() %>% 
                count(phrase) %>% 
                arrange(desc(n))
        
        threeGrams <- data_frame(text =  combineSampleClean) %>% 
                mutate(text = tolower(text)) %>% 
                mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
                mutate(phrase = str_extract_all(text, "[a-z]+\\s+[a-z]+\\s+[a-z]+")) %>%
                unnest() %>% 
                count(phrase) %>% 
                arrange(desc(n))
        
        #Analyze the input text
        word1 <- renderText({tolower(word(input$text,-2))})
        word2 <- renderText({tolower(word(input$text,-1))})
        
        #Lastly, present the options
        output$result1 <- renderTable({
                test1 <- grep(paste0("^",word1(),"\\s+",word2(),"\\s+"),threeGrams$phrase)
                if(is.null(test1)){
                        return(NULL)
                }
                analysis1 <- threeGrams[test1,]
                return(analysis1[1:3,1])
        })
        output$result2 <- renderTable({
                test2 <- grep(paste0("^",word2(),"\\s+"),twoGrams$phrase)
                if(is.null(test2)){
                        return(NULL)
                }
                analysis2 <- twoGrams[test2,]
                return(analysis2[1:3,1])
        })
}

shinyApp(ui,server)

