#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinythemes)
library(tidytext)

teams <- read.csv("winningteams.csv")
teamstib <- as.tibble(teams)
teamstib <- teamstib %>% mutate(FFBSScore = 0.4*eFG - 0.25*TOV + 0.2*RB + 0.15*FT)
teamstib <- teamstib %>% mutate(Rank = c(2,4,1,3,4,1,2,3,3,4,1,2,4,1,3,2,3,4,2,1,2,4,3,1,2,4,1,3,4,3,2,1,3,4,1,2,3,2,1,4,1,4,2,3,2,1,4,3,2,4,3,1,3,4,1,2,3,1,4,2,3,2,4,1,1,2,4,3,1,3,4,2,1,3,2,4,3,1,4,2,1,3,4,2))
teams <- as.data.frame(teamstib)
champteams <- teamstib %>% filter(Champion == "Yes")
champscores <- teamstib %>% select(Champion, FFBSScore) %>% 
  filter(Champion == "Yes") %>% as.data.frame()
nonchampscores <- teamstib %>% select(Champion, FFBSScore) %>% 
  filter(Champion == "No") %>% as.data.frame()

c<- read_csv("celtics.csv")
tceltics <- c %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())
c_sentiment <- tceltics %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) 
w <- read_csv("warriors.csv")
twarriors <- w %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())
w_sentiment <- twarriors %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) 
ca <- read_csv("cavs.csv")
tcavs <- ca %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())
ca_sentiment <- tcavs %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)  
p <- read_csv("pelicans.csv")
tpelicans <- p %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())
p_sentiment <- tpelicans %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
r <- read_csv("rockets.csv")
trockets <- r %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())
r_sentiment <- trockets %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
j <- read_csv("jazz.csv")
tjazz <- j %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())
j_sentiment <- tjazz %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
s <- read_csv("sixers.csv")
tsixers <- s %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())
s_sentiment <- tsixers %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
ra <- read_csv("raptors.csv")
traptors <- c %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())
ra_sentiment <- traptors %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
sentlist <- list(c_sentiment, ca_sentiment,j_sentiment,p_sentiment,r_sentiment, ra_sentiment, s_sentiment,w_sentiment)
senttib <- tibble(
  Team = c("Boston Celtics", "Cleveland Caveliers", "Utah Jazz", "New Orleans Pelicans", "Houston Rockets", "Toronto Raptors", "Philadelphia 76ers", "Golden State Warriors"),
  Sentiment_Score = c(22252,11870,23089,27238,12769,22252,12369,16981),
  Rank = c(3,8,2,1,6,3,7,5),
  Revrank = c(5,1,7,8,3,5,2,4)
)
current <- read.csv("currentteams.csv")
currenttib <- as.tibble(current)
currenttib <- currenttib %>% mutate(FFBSScore = 0.4*eFG - 0.25*TOV + 0.2*RB + 0.15*FT)
currenttib <- currenttib %>% mutate(Rank = c(8,4,7,5,2,3,6,1))





ui <- dashboardPage(
  dashboardHeader(title = "NBA Playoffs"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Historical NBA Champs",tabName = "historical"),
      menuItem("2018 Current NBA Playoffs",tabName = "current")
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "historical",
              fluidRow(
                mainPanel(HTML('<p><img src="http://blog.logomyway.com/wp-content/uploads/2017/01/nba-logo-design.jpg"/></p>')),
                box("My Analysis begins by looking at NBA Championship teams, as well as the other 3 teams to make it the furthest in the playoffs, from 1996 through 2016. I decided to look at four components from these teams' regular season statistics that Dean Oliver (https://www.basketball-reference.com/about/factors.html) identifies as the 'Four Factors of Basketball Success.",br(),
                    "These four components are Shooting (40%), Turnovers (25%), Rebounding (20%), and Free Throws (15%). I combined these stats with the listed weights to calculate an FFBS Score for each team. The idea was to see if there was a statistical difference betwee the scores of the Championship Teams vs. those of the teams that made it far in the playoffs but did not win.",br(),
                    "I wanted to explore and see if calculating the FFBS Score of teams currently in the 2018 playoffs would provide me with any indication of which team is most likely to win the Championship this year.", width = 11),
                mainPanel(plotOutput("FFBSChamp", height = 300, width = 600)),
                box("Two Sample t-test",br(),
                    
                    "data:  champscores$FFBSScore and nonchampscores$FFBSScore",br(),
                    "t = 0.13865, df = 82, p-value = 0.8901",br(),
                    "alternative hypothesis: true difference in means is not equal to 0",br(),
                    "95 percent confidence interval:",br(),
                    "-0.5678241  0.6529035",br(),
                    "sample estimates:",br(),
                    "mean of x mean of y",br(), 
                    "38.04548  38.00294 "),
                box("The results of the T-test comparing average FFBS Scores for Championship teams versus the FFBS Scores for the 3 other teams to make it the furthest in the playoffs over the last 20 years are shown to the left.",br(),
                    "The results tell us that there is NO statistical difference between these sets of scores, suggesting that FFBS Score may not be the best way to predict whether a playoff team will win the championship."),
                mainPanel(plotOutput("tsplot", height = 250, width = 600)),
                box("The Time series plot of FFBS Scores of Championship teams from 1996 to 2016 shows us that the FFBS Scores seem to have been increasing for championship teams over time. This suggests that winning a championship in the NBA becomes more and more competitive each year.",br(),
                    "The Shooting, Rebounding, and Free Throw percentages of Championship teams are increasing on average while Turnover rates are decreasing.", width = 11)
              )
      ),
      tabItem(tabName = "current",
              fluidRow(
                box("My analysis of the 8 teams currenlty left in the 2018 playoffs started with Twitter. I decided to look at the 1000 most recent tweets mentioning all of these teams' official twitter accounts.",br(),
                    "I conducted a positive/negative sentiment analysis on the tweets and came up with a sentiment score for each team. I wanted to see if more 'postive' or more 'negative' tweets correlated with",br(),
                    "how the teams are currenlty performing in the playoffs as well as how likely they are to win the championship this year.", width = 600),
                box(plotOutput("sentiment",height=250)),
                box(selectInput("team","Choose Team",choice = c("Boston Celtics"="1","Cleveland Caveliers"="2","Utah Jazz"="3","New Orleans Pelicans"="4","Houston Rockets"="5","Toronto Raptors"="6","Philadelphia 76ers"="7","Golden State Warriors"=8))),
                mainPanel(plotOutput("sentbar", width = 650)),
                box("The bar plot above shows the sentiment score for each team that I calculated. The plot below shows the ranking of each team from 1-8 based on sentiment score with 1 being the highest and 8 being the lowest. If one was to base predictions for most likely to win the championship based on sentiment score this plot demonstrates that.",br(),
                    "The issue is that this plot has New Orleans ranked number 1, Utah Jazz number 2, the Caveliers number 8, and the Warriors at number 5. These predictions are not accurate as the Pelicans and Jazz are currently losing their current series and will most likely be eliminated while the Warriors and the Caveliers are favored to win."),
                mainPanel(plotOutput("sentrank", width = 650)),
                box("This plot above demonstrates that there is not a positive correlation between positive sentiment and playoff success. The final plot shown below ranks the 8 current teams based on their FFBS Score. Interestingly enough it is the most accurate even though there was no evidence showing that FFBS Scores of Championship teams were higher than that of their competitors.",br(),
                    "This plot has the Warriors ranked number 1, the Rockets at number 2, and the Caveliers at number 4. Those three teams are the three most favored teams currently in the playoffs according to fans and analysts."),
                mainPanel(plotOutput("FFBSrank", width = 650))
                
                
              )
              
      )
      
    )
    
  )
)


server <- function(input, output) {
  
  
  output$FFBSChamp <- renderPlot({
    ggplot(champteams) + 
      geom_bar(mapping = aes(x = Year, y = FFBSScore, color = Team), stat = "identity") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      ggtitle("FFBSscores of Championship Teams by Year")
  })
  
  output$tsplot <- renderPlot({
    
    ggplot(data = champteams, aes(x = Year, y = FFBSScore, colour = 'red')) +geom_line() + ggtitle("Timeseries")
    
  })
  
  output$sentiment <-renderPlot({
    team <- sentlist[[as.numeric(input$team)]]
    ggplot(team, aes(index, sentiment)) +
      geom_col(show.legend = FALSE,col=5+as.numeric(input$team)) +
      ggtitle("Positive/Negative Sentiment Analysis Visual")
  })
  
  output$sentbar <- renderPlot({
    ggplot(senttib) + 
      geom_bar(mapping = aes(x = Team, y = Sentiment_Score, color = Team, fill = Team), stat = "identity") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      ggtitle("Sentiment Scores of Current NBA Playoff Teams")
  })
  
  output$sentrank <- renderPlot({
    ggplot(senttib) + 
      geom_bar(mapping = aes(x = Team, y = Rank, color = Team, fill = Team), stat = "identity") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      ggtitle("Likelihood of Winning Championship Ranking based on Sentiment Score")
  })
  
  
  output$FFBSrank <- renderPlot({
    ggplot(currenttib) + 
      geom_bar(mapping = aes(x = Team, y = Rank, color = Team, fill = Team), stat = "identity") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      ggtitle("Likelihood of Winning Championship Ranking based on FBSS Score")
  })
  
}

shinyApp(ui = ui, server = server)

