
# This is a Shiny web application. The aim of this application to create a dashboard let viewer access interactive plots, tables and figures from the World Value Study Wave 6 data (2010-2014).
# Using the drop down menu, viewer will be able to select a country participated in the Wave 6, and visualize survey results for the chosen topic namely democracy, news and science.  

#Application can be run by clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#Below packages should be installed and stored in the library.

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(rsconnect)

# The line of code below will be used to read data and sets country codes to factors
df <- read.table(file = 'C:/Users/Pinar/Documents/mwfdata/MWF4/MVF4PU/WV6_Data_csv_v20201117.csv', sep = ';', header = T)
df$C_COW_ALPHA <- as.factor(df$C_COW_ALPHA)

# This code collects the unique country codes
countries <- unique(df$C_COW_ALPHA)
# This code sets unique topics to explore
topics <- c('Democracy', 'Science', 'News')

# This code omits/exclude missing values (values lower than zero (-5 to -1))
#These values will not be used for data visualization.
democracy <- df %>% select(Country = 'C_COW_ALPHA', V228A:V228I)
democracy[democracy < 0 ] <- NA
news <- df %>% select(Country = 'C_COW_ALPHA', V217:V224)
news[news < 0 ] <- 0
science <- df %>% select(Country = 'C_COW_ALPHA', V192:V197)
science[science < 0 ] <- NA

# This code Calculates the country average for each of the questions regarding the different topics.
Science <- science %>% group_by(Country) %>% summarize(V192 = mean(V192, na.rm = TRUE),
                                                             V193 = mean(V193, na.rm = TRUE),
                                                             V194 = mean(V194, na.rm = TRUE),
                                                             V195 = mean(V195, na.rm = TRUE),
                                                             V196 = mean(V196, na.rm = TRUE),
                                                             V197 = mean(V197, na.rm = TRUE))

WVS_science_avg <- science %>% summarize(V192 = mean(V192, na.rm = TRUE),
                                                            V193 = mean(V193, na.rm = TRUE),
                                                            V194 = mean(V194, na.rm = TRUE),
                                                            V195 = mean(V195, na.rm = TRUE),
                                                            V196 = mean(V196, na.rm = TRUE),
                                                            V197 = mean(V197, na.rm = TRUE))

Democracy <- democracy %>% group_by(Country) %>% summarize(V228A = mean(V228A, na.rm = TRUE),
                                                                 V228B = mean(V228B, na.rm = TRUE),
                                                                 V228C = mean(V228C, na.rm = TRUE),
                                                                 V228D = mean(V228D, na.rm = TRUE),
                                                                 V228E = mean(V228E, na.rm = TRUE),
                                                                 V228F = mean(V228F, na.rm = TRUE),
                                                                 V228G = mean(V228G, na.rm = TRUE),
                                                                 V228H = mean(V228H, na.rm = TRUE),
                                                                 V228I = mean(V228I, na.rm = TRUE))

WVS_democracy_avg <- democracy %>% summarize(V228A = mean(V228A, na.rm = TRUE),
                                             V228B = mean(V228B, na.rm = TRUE),
                                             V228C = mean(V228C, na.rm = TRUE),
                                             V228D = mean(V228D, na.rm = TRUE),
                                             V228E = mean(V228E, na.rm = TRUE),
                                             V228F = mean(V228F, na.rm = TRUE),
                                             V228G = mean(V228G, na.rm = TRUE),
                                             V228H = mean(V228H, na.rm = TRUE),
                                             V228I = mean(V228I, na.rm = TRUE))



grouped_news <- news %>%  group_by(Country) 
news_sum <- grouped_news %>% summarise_each(funs(sum))
row_sum <- rowSums(news_sum[, !names(news_sum) %in% c("Country")], na.rm = TRUE)
# This code calculates each country's proportion of news sources
News <- news_sum  %>% summarize(Country = Country,  
                                Newspaper = round(V217/row_sum, digits = 3),
                                Magazines = round(V218/row_sum, digits = 3),
                                TV = round(V219/row_sum, digits = 3),
                                Radio = round(V220/row_sum, digits = 3),
                                Phone = round(V221/row_sum, digits = 3),
                                Email = round(V222/row_sum, digits = 3),
                                Internet = round(V223/row_sum, digits = 3),
                                "Friends & \n Colleagues" = round(V224/row_sum, digits = 3))

# This code filters away infinite values (handling of non responses in survey)
News <- News %>% filter_if(~is.numeric(.), all_vars(!is.infinite(.)))


# This code calculates WVS survey wave 6  data proportions of news sources
WVS_news_avg <- colSums(news[, !names(news) %in% c("Country")], na.rm = TRUE)/sum(colSums(news[, !names(news) %in% c("Country")], na.rm = TRUE))



# This code defines server logic required to compute means and visualize the bar charts.
server <- function(input, output, session) {
    
    # Dynamically makes the into proper bar chart format, based on the country code value from the drop down menu. 
    data <- reactive({
        if (input$Topic == 'Science') {
            df <- gather(Science[Science$Country == input$DropDown,], key = Country)
            colnames(df)[1] <- "Question"
        } else if (input$Topic == 'Democracy') {
            df <- gather(Democracy[Democracy$Country == input$DropDown,], key = Country)
            colnames(df)[1] <- "Question"
        } else if (input$Topic == 'News') {
            df <- gather(News[News$Country == input$DropDown,], key = Country) 
            colnames(df)[1] <- "Question"
        }
        df
    })
    
    # Creates the right formatting of the data for numerical table
    WVS <- reactive({
        if (input$Topic == 'Science') {
            WVS <- gather(WVS_science_avg, key = 'Question')
        } else if (input$Topic == 'Democracy') {
            WVS <- gather(WVS_democracy_avg, key = 'Question')
        } else if (input$Topic == 'News') {
            WVS <- NULL          
        }
    })
    
    # Creates title depending on topic
    plot_title <- reactive({
        if (input$Topic == 'News') {
            plot_title <- ggtitle(paste('Proportion for sources of information of news in', input$DropDown))    
        } else {
            plot_title <- ggtitle(paste('Attitude towards:', input$Topic, 'in', input$DropDown))
        }
    })

    # Dynamically computes the plot, based on the country code value from the drop down menu.
    output$BarChart <- renderPlotly({
        ggplot(data(), aes(x = Question, y = value)) + 
            geom_bar(position="dodge", stat="identity", fill = "#0385a8") +
            # Sets a minimal theme to the plot, giving it a clean appearance
            theme_minimal() +
            # Suppresses the x-axis text
            xlab('') +
            # Suppresses y-axis text
            ylab('Country Average') +
            # Sets title
            plot_title() + 
            # Centers the title and increases font size of text
            theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20))
        
    })
    
    output$values <- renderTable({
        # Renders the numerical table to the left
        if (input$Topic != 'News'){
            df <- data.frame("Question" = data()$Question,  
                             "Country Average" = data()$value, 
                             "WVS Sample Average" = WVS()$value, 
                             # Calculates the difference between Country Average - Survey Average
                             "Difference" = data()$value - WVS()$value, check.names = FALSE)
        } else if (input$Topic == 'News'){
            df <- data.frame("Question" = data()$Question,  
                             "Country Average" = data()$value, 
                             "WVS Sample Average" = WVS_news_avg,
                             # Calculates the difference between Country Average - Survey Average
                             "Difference" = data()$value - WVS_news_avg, check.names = FALSE)
        }
        df
    })
    # Renders a table of questions below the bar chart depending on if the topic is Democracy or Science, if its news it does nothing. 
    # As these have a clear x-axis labeling. 
    output$Questions <- renderTable({
        if (input$Topic == 'Democracy') {
            questions <- data.frame(
            V228A = "How often in country's elections: Votes are counted fairly",
            V228B = "How often in country's elections: Opposition candidates are prevented from running",
            V228C = "How often in country's elections: TV news favors the governing party",
            V228D = "How often in country's elections: Voters are bribed",
            V228E = "How often in country's elections: Journalists provide fair coverage of elections",
            V228F = "How often in country's elections: Election officials are fair",
            V228G = "How often in country's elections: Rich people buy elections",
            V228H = "How often in country's elections: Voters are threatened with violence at the polls",
            V228I = "How often in country's elections: Voters are offered a genuine choice in the elections"
            
        )
        } else if (input$Topic == 'Science'){
            questions <- data.frame(
                V192 = "Science and technology are making our lives healthier, easier, and more comfortable",
                V193 = "Because of science and technology, there will be more opportunities for the next generation",
                V194 = "We depend too much on science and not enough on faith",
                V195 = "One of the bad effects of science is that it breaks down people’s ideas of right and wrong",
                V196 = "It is not important for me to know about science in my daily life",
                V197 = "The world is better off, or worse off, because of science and technology"
            )
        }
        questions
    })
    
    # Renders instructions on how to interpret numerical values in bar chart
    output$text <- renderText({
        if (input$Topic == 'Science'){
            text <- "<em>Higher value indicates higher agreement</em>"
        } else if (input$Topic == 'Democracy'){
            text <- paste("<em>Higher value indicates <b>less</b> often</em>")
        }
        text
    })
    
        
    
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("World Values Survey Wave 6 (2010-2014)"),
    
    fluidRow(
    # Creates the drop down menu for country choice.
    sidebarLayout(
        sidebarPanel(
            tags$h3("Overview"),
            p("Select a country from the drop down menu, and a survey topic to explore to visualize the citizens
              attitudes towards the chosen topic."),
            selectInput('DropDown', "Select country:", choices = countries),
            radioButtons("Topic", "Explore topic:", choices = topics),
            tableOutput("values")
        ),
        
        # Visualize the computed plot
        mainPanel(
            plotlyOutput("BarChart"),
            
            # Creates a table with questions, as long as the topic is not News.
            conditionalPanel(condition = "input.Topic != 'News'",
                tableOutput('Questions')
            ),
            # Visualizes the text of how to interpret bar chart values, as long as the topic is not News.
            conditionalPanel(condition = "input.Topic != 'News'",
                htmlOutput("text")
            ))
        )
    )
)

# Run the application 
shinyApp(ui = ui, server = server)


