library(tidyverse)
library(devtools)
library(twitteR)
library(tidytext)

teams <- read.csv("winningteams.csv")
teamstib <- as.tibble(teams)

# editing the data set to add two new columns of FFBS SCore and rank
teamstib <- teamstib %>% mutate(FFBSScore = 0.4*eFG - 0.25*TOV + 0.2*RB + 0.15*FT)
teamstib <- teamstib %>% mutate(Rank = c(2,4,1,3,4,1,2,3,3,4,1,2,4,1,3,2,3,4,2,1,2,4,3,1,2,4,1,3,4,3,2,1,3,4,1,2,3,2,1,4,1,4,2,3,2,1,4,3,2,4,3,1,3,4,1,2,3,1,4,2,3,2,4,1,1,2,4,3,1,3,4,2,1,3,2,4,3,1,4,2,1,3,4,2))
teams <- as.data.frame(teamstib)

ranktib <- teamstib %>% select(Champion, Rank) %>% 
  filter(Champion == "Yes") 
rank <- as.data.frame(ranktib) 
mean(rank$Rank)

# filtering out non championship teams
champscores <- teamstib %>% select(Champion, FFBSScore) %>% 
    filter(Champion == "Yes") %>% as.data.frame()
avgchampscores <- mean(champscores$FFBSScore)

#filtering out Championship teams
nonchampscores <- teamstib %>% select(Champion, FFBSScore) %>% 
  filter(Champion == "No") %>% as.data.frame()
avgnonchampscores <- mean(nonchampscores$FFBSScore)

var.test(champscores$FFBSScore, nonchampscores$FFBSScore)
t.test(champscores$FFBSScore, nonchampscores$FFBSScore, var.equal = T)



## timeseries plot of FFBSSCore of championship teams over 20 years 
champteams <- teamstib %>% filter(Champion == "Yes")
ts <- ggplot(data = champteams, aes(x = Year, y = FFBSScore, colour = 'red')) +geom_line() + ggtitle("Timeseries")
ts

FFBSchampplot <- ggplot(champteams) + 
  geom_bar(mapping = aes(x = Year, y = FFBSScore, color = Team), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("FFBSscores of Championship Teams by Year")
FFBSchampplot


### TWITTER ANALYSIS ###

## THE FOLLOWING CODE IS COMMENTED OUT BECAUSE IT IS FOR 
## TWITTER REAL TIME DATA COLLECTION. I HAVE ALREADY SAVED
## THE DATA COLLECTED FROM TWITTER IN CSV'S WHICH ARE  INCLUDED IN THE 
## PROJECT FOLDER

# api_key <- "A39e9EPdw5vVO9zXU5fgSsclD"
# api_secret <- "79o3ei4tr4OQLhah0CtLGUuabCsiWCiVIAQPBu3Lzyjye2SSDk"
# 
# access_token <- "451216855-evLahTJeb2xGIwPky4TNfXCFULF590Xm4FCqsAkM"
# access_token_secret <- "NpbU1k5EF6GuMxR1AiKVopo0vC6voRhV0KLs10LpAJlWp"
# 
# 
# setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
# 
# 
# # gathering Boston Celtics tweets and creating data set 
# celtics <- searchTwitter('@celtics', resultType = "recent", n = 1000)
# celtics.df <- twListToDF(celtics)
# write.csv(celtics.df, "celtics.csv")
# 
# # gathering Golden State Warriors tweets and creating data set
# warriors <- searchTwitter('@warriors', resultType = "recent", n = 1000)
# warriors.df <- twListToDF(warriors)
# write.csv(warriors.df, "warriors.csv")
# 
# # gathering Cleveland Caveliers tweets and creating data set
# cavs <- searchTwitter('@cavs', resultType = "recent", n = 1000)
# cavs.df <- twListToDF(cavs)
# write.csv(cavs.df, "cavs.csv")
# 
# # gathering New Orleans Pelicans tweets and creating data set
# pelicans <- searchTwitter('@PelicansNBA', resultType = "recent", n = 1000)
# pelicans.df <- twListToDF(pelicans)
# write.csv(pelicans.df, "pelicans.csv")
# 
# # gathering Houston Rockets tweets and creating data set
# rockets <- searchTwitter('@HoustonRockets', resultType = "recent", n = 1000)
# rockets.df <- twListToDF(rockets)
# write.csv(rockets.df, "rockets.csv")
# 
# # gathering Utah Jazz tweets and creating data set
# jazz <- searchTwitter('@utahjazz', resultType = "recent", n = 1000)
# jazz.df <- twListToDF(jazz)
# write.csv(jazz.df, "jazz.csv")
# 
# # gathering Philadelphia 76ers tweets and creating data set
# sixers <- searchTwitter('@sixers', resultType = "recent", n = 1000)
# sixers.df <- twListToDF(sixers)
# write.csv(sixers.df, "sixers.csv")
# 
# # gathering Toronto Raptors tweets and creating data set
# raptors <- searchTwitter('@Raptors', resultType = "recent", n = 1000)
# raptors.df <- twListToDF(raptors)
# write.csv(raptors.df, "raptors.csv")


# unnest words in celtics tweets data set
c<- read_csv("celtics.csv")
tceltics <- c %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())

# positive/negative sentiment analysis of celtics tweets 
c_sentiment <- tceltics %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)  

# total sum of sentiments for 1000 most recent celtics tweets
c_sent_score <- as.data.frame(c_sentiment) %>% sum(c_sentiment$sentiment)

# pos/neg sentiment plot for celtics sentiment analysis 
ggplot(c_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE,col=13) 


# unnest words in warriors tweets data set
w <- read_csv("warriors.csv")
twarriors <- w %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())

# positive/negative sentiment analysis of warriors tweets 
w_sentiment <- twarriors %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)  

# total sum of sentiments for 1000 most recent warriors tweets
w_sent_score <- as.data.frame(w_sentiment) %>% sum(w_sentiment$sentiment)

# pos/neg sentiment plot for warriors sentiment analysis 
ggplot(w_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE,col=13)


# unnest words in cavs tweets data set
ca <- read_csv("cavs.csv")
tcavs <- ca %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())

# positive/negative sentiment analysis of cavs tweets 
ca_sentiment <- tcavs %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)  

# total sum of sentiments for 1000 most recent cavs tweets
ca_sent_score <- as.data.frame(ca_sentiment) %>% sum(ca_sentiment$sentiment)

# pos/neg sentiment plot for cavs sentiment analysis 
ggplot(ca_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE,col=13)


# unnest words in pelicans tweets data set
p <- read_csv("pelicans.csv")
tpelicans <- p %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())

# positive/negative sentiment analysis of pelicans tweets 
p_sentiment <- tpelicans %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)  

# total sum of sentiments for 1000 most recent pelicans tweets
p_sent_score <- as.data.frame(p_sentiment) %>% sum(p_sentiment$sentiment)

# pos/neg sentiment plot for pelicans sentiment analysis 
ggplot(p_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE,col=13)


# unnest words in rockets tweets data set
r <- read_csv("rockets.csv")
trockets <- r %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())

# positive/negative sentiment analysis of rockets tweets 
r_sentiment <- trockets %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)  

# total sum of sentiments for 1000 most recent rockets tweets
r_sent_score <- as.data.frame(r_sentiment) %>% sum(r_sentiment$sentiment)

# pos/neg sentiment plot for rockets sentiment analysis 
ggplot(r_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE,col=13)


# unnest words in jazz tweets data set
j <- read_csv("jazz.csv")
tjazz <- j %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())

# positive/negative sentiment analysis of jazz tweets 
j_sentiment <- tjazz %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)  

# total sum of sentiments for 1000 most recent jazz tweets
j_sent_score <- as.data.frame(j_sentiment) %>% sum(j_sentiment$sentiment)

# pos/neg sentiment plot for jazz sentiment analysis 
ggplot(j_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE,col=13)


# unnest words in sixers tweets data set
s <- read_csv("sixers.csv")
tsixers <- s %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())

# positive/negative sentiment analysis of sixers tweets 
s_sentiment <- tsixers %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)  

# total sum of sentiments for 1000 most recent sixers tweets
s_sent_score <- as.data.frame(s_sentiment) %>% sum(s_sentiment$sentiment)

# pos/neg sentiment plot for sixers sentiment analysis 
ggplot(s_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE,col=13)


# unnest words in raptors tweets data set
ra <- read_csv("raptors.csv")
traptors <- c %>% 
  as.tibble() %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(linenumber = row_number())

# positive/negative sentiment analysis of raptors tweets 
ra_sentiment <- traptors %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)  

# total sum of sentiments for 1000 most recent raptors tweets
ra_sent_score <- as.data.frame(ra_sentiment) %>% sum(ra_sentiment$sentiment)

# pos/neg sentiment plot for raptors sentiment analysis 
ggplot(ra_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE,col=13)

# tibble of sentiment scores for current nba playoff teams
senttib <- tibble(
  Team = c("Boston Celtics", "Cleveland Caveliers", "Utah Jazz", "New Orleans Pelicans", "Houston Rockets", "Toronto Raptors", "Philadelphia 76ers", "Golden State Warriors"),
  Sentiment_Score = c(22252,11870,23089,27238,12769,22252,12369,16981),
  Rank = c(3,8,2,1,6,3,7,5),
  Revrank = c(5,1,7,8,3,5,2,4)
)

# bar plot of current teams based on sentiment score
sentplot <- ggplot(senttib) + 
  geom_bar(mapping = aes(x = Team, y = Sentiment_Score, color = Team, fill = Team), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Sentiment Scores of Current NBA Playoff Teams")
sentplot

# bar plot of current teams by ranking based on sentiment score
sentrankplot <- ggplot(senttib) + 
  geom_bar(mapping = aes(x = Team, y = Rank, color = Team, fill = Team), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Likelihood of Winning Championship Ranking based on Sentiment Score")
sentrankplot

# bar plot of current teams by reversing ranking based on sentiment score 
sentrevrankplot <- ggplot(senttib) + 
  geom_bar(mapping = aes(x = Team, y = Revrank, color = Team, fill = Team), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Likelihood of Winning Championship Ranking (reversed) based on Sentiment Score")
sentrevrankplot

### Current NBA Playoff Teams FFBS Score analysis ###
current <- read.csv("currentteams.csv")
currenttib <- as.tibble(current)
currenttib <- currenttib %>% mutate(FFBSScore = 0.4*eFG - 0.25*TOV + 0.2*RB + 0.15*FT)
currenttib <- currenttib %>% mutate(Rank = c(8,4,7,5,2,3,6,1))

# bar plot of current teams based on FFBSScore
FFBSplot <- ggplot(currenttib) + 
  geom_bar(mapping = aes(x = Team, y = FFBSScore, color = Team, fill = Team), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("FFBSScores of Current NBA Playoff Teams") 
FFBSplot

# bar plot of current teams by ranking based on FFBSScore
FFBSrankplot <- ggplot(currenttib) + 
  geom_bar(mapping = aes(x = Team, y = Rank, color = Team, fill = Team), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Likelihood of Winning Championship Ranking based on FBSS Score")
FFBSrankplot



