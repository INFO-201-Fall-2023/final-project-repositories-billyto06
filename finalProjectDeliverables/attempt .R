library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
#test
library(stringr)
library(dplyr)
df <- read.csv("GamingStudy_data.csv")
df2 <- read.csv("Book1.csv")
df_final <- read.csv("data_cleaned.csv")

rank_map <- c("iron", "bronze", "silver", "gold", "platinum", "diamond", "master", "grandmaster", "challenger")

filtered_df <- filter(df, Game == "League of Legends", !is.na(League))
filtered_df <- head(filtered_df, n = 50)
filtered_df$League <- case_when(
  grepl(paste(rank_map, collapse = "|"), filtered_df$League, ignore.case = TRUE) ~ 
    tolower(sub("^.*(iron|bronze|silver|gold|plat|diamond|master|grandmaster|challenger).*$", "\\1", filtered_df$League, ignore.case = TRUE)),
  TRUE ~ "unranked"
)
df2_avg <- df2 %>%
  group_by(rank) %>%
  summarize(Depression_index = mean(Depression_index), Rounds = mean(Rounds))

filtered_df_avg <- filtered_df %>%
  group_by(League) %>%
  summarize(Narcissism = mean(Narcissism), SPIN14 = mean(SPIN14), GAD_T = mean(GAD_T))

merged_df <- merge(filtered_df_avg, df2_avg, by.x = "League", by.y = "rank")

merged_df$round.index <- merged_df$Depression_index / merged_df$Rounds

merged_df$global_avg_dep_index <- 3.77
merged_df$avg_comparison <- merged_df$Depression_index > merged_df$global_avg_dep_index
write.csv(merged_df, "data_cleaned.csv", row.names = FALSE)
df_summary <- summary(merged_df)
df_summary <- as.data.frame(df_summary)



#-------------------------------------------------------------#


# another dataframe
bars <- data.frame(
  Category = df_final$League,
  Value1 = df_final$Narcissism,
  Value2 = df_final$Depression_index
)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "Mental Health x Gaming",
                  tabPanel("Introduction",
                           fluidRow(
                             column(12, align="center",
                                    p("INTRODUCTION"),
                                    p("In this Shinyapp, we demonstrate our evaluation of the connection between Video games and possible mental health effects on people. We measure based on time and effort given into the video game, and the emotional stability, self image, and life satifiscation in a person. With this evaluation, we hope to find a definitive correlation, in order to better understand the possible risks of prolonged hours in video games, and find the acceptable range. We find mental health to be a serious topic, and although many people use video games as an escape from reality, we don't know if their are any harmful effects to prolonged gaming."),
                
                                     p("Although we had many different games and players to work with, we decided to focus on the game with the largest percent of players: League of Legends. We are analyzing our data with 3 tabs: Outliers, Factors, and Contrasts. We first look at the outliers to find any possible skew of data, or people who are abnormal within our dataset. "),
                                    img(src = "aaaaa.jpeg", width = "500px", height = "500px"),
                             )
                           ),
                           br(),
                  ),
                  tabPanel("Outliers",
                           plotlyOutput("scatter_plot"),  # Display the scatter plot
                           p("This chart shows 48 players who play league from our dataset, and their corresponding hours spent playing in a week. The X axis the assigned player number from the dataset, and the Y axis is the amount of hours they put in. From this chart we can pick out 2 outliers who clock in around 50 hours of playing time. This is significant because these players should not be considered when evaluating play time with possible overall mental health decline, as their high amount of hours sways the total data results. 
Each Dot represents a person who plays League."),
                           p("(This chart is interactive, by clicking and dragging you may zoom in to look closer)"),
                  ),
                  tabPanel("Factors",
                           selectInput("category_select", "Select Category", choices = merged_df$League),
                           plotlyOutput("stacked_bar_chart"),  # Display the stacked bar chart
                           p("This chart shows the compounded mental health comparison, meaning we are comparing to see how 
                           depression and mental health were correlated with one another, we found that when rounded to the nearest tenth
                           all ranks had the same factor score, meaning likely both narcaissim and depression were affected equally
                           regardless of rank
")
                  ),
                  tabPanel("Contrasts", "",
                           plotlyOutput("bar_chart"),
                           p("in this bar graph we are contrasting the depression index between different ranks to give a big picture as to how different ranks fare"),
                           br(),
                           p("if you want to hover your mouse over the bars it will give you their average rating out of 10")
                  ),
                  tabPanel("Summary",fluidRow(
                    column(12, align="center",
                           p("SUMMARY"),
                           br(),
                           p("OUTLIERS"),
                           p("We found that the outliers(player's 114 & 157) had a negative impact on the overall average hours of league Players.Without the outliers there is a more common trend that allows us to measure the mental health based on ranks and not hours put in"),
                           br(),
                           p("FACTORS"),
                           p("Middle rank players, although have more players in those ranks, still have a higher rate in which their mental health is negative. This shows us that rank ultimately isn't the determining factor, but the time and effort that is put in"),
                           br(),
                           p("CONTRASTS"),
                           p("Narcissim and depression had no contrast in terms of mental health score, both were effected
                             evenly when rounded to the nearest tenth"),
                           br(),
                           p("CONCLUSIONS"),
                           p("Overall, we found that there was a correlation between time + effort into playing video games, and a change in mental health"),
                           br(),
                           br(),
                           br(),
                           p("CREATORS"),
                           p("Billy To, Andrew Yu"),
                           
                    )
                  )
                  ),
                )
)

# Define server function
server <- function(input, output) {
  
  output$txtout <- renderText({
    paste(input$txt1, input$txt2, sep = " ")
  })
  
  output$data_table <- renderTable({
    data
  })
  
  output$scatter_plot <- renderPlotly({
    p <- ggplot(filtered_df, aes(x = X, y = Hours)) +
      geom_point() +
      labs(x = "Column 1", y = "Column 2", title = "Scatter Plot")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$stacked_bar_chart <- renderPlotly({
    selected_category <- input$category_select
    
    filtered_bars <- merged_df[merged_df$League == selected_category, ]
    
    p <- ggplot(filtered_bars, aes(x = League, y = Depression_index)) +
      geom_bar(stat = "identity", position = "fill") +
      labs(x = "Rank", y = "Compounded Mental Health", fill = "Variable") +
      ggtitle("Stacked Bar Plot")
      
    
    ggplotly(p)
  })
  
  output$bar_graph <- renderPlot({
    # Subset the dataframe based on the condition
    subset_df <- df_final[df_final$League == "silver", ]
    
    # Remove the first and last column
    subset_df <- subset_df[, -c(1, ncol(subset_df))]
    
    # Create a vector of column names to exclude
    excluded_cols <- c("League", "avg_comparison")
    
    # Remove the excluded columns
    subset_df <- subset_df[, !(colnames(subset_df) %in% excluded_cols)]
    
    # Create the bar graph
    barplot(as.numeric(unlist(subset_df)), names.arg = colnames(subset_df), 
            xlab = "Columns", ylab = "Values", main = "Bar Graph")
    
    ggplotly(p)
  })

  output$bar_chart <- renderPlotly({
    p <- ggplot(merged_df, aes(x = League, y = Depression_index)) +
      geom_bar(stat = "identity") +
      labs(x = "Ranks", y = "Depression Index", title = "Contrasting Ranks")
    
    ggplotly(p)
  })
}
# Create Shiny object
shinyApp(ui = ui, server = server)