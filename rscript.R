#Question One
#Fetching tweets from twitter with the hastag humantrafficking and saving it as a csv
library(rtweet)
number1 <- search_tweets("#HumanTrafficking", n=250)
class(number1)
number1 <- as.data.frame(number1)
number1 <- data.frame(lapply(number1, as.character), stringsAsFactors=FALSE)
write.table(number1, file="httweets.csv", row.names=FALSE, na="", sep=",")
vars <- c("user_id","status_id", "created_at", "screen_name", "text", "is_quote", "is_retweet", "favorite_count",
          "retweet_count", "quote_count", "hashtags", "name", "location")
httweets <- number1[vars]
write.csv(httweets, file = 'humantraffickingtweets2.csv', row.names = F)

#Loading the data into memory
sampletweets <- read.csv("sample_tweets.csv", header = T)

#Working with the date column to fomrat it into understandable formats
class(sampletweets$publish_date)

#Loading a library to format date
library(anytime)
sampletweets$publish_date <- anytime(sampletweets$publish_date)

#Obtaining the first date in the date column
earliestdate <- min(sampletweets$publish_date)
earliestdate

#Obtaining information associated with the earliest date
Originaltweets <- sampletweets[which(sampletweets$post_type!="RETWEET" & sampletweets$post_type!="QUOTE_TWEET"),]
earliestdate2 <- min(Originaltweets$publish_date)
earliestdate2
firstorigtweet <- subset(Originaltweets, publish_date=="2014-11-28 +03")
View(firstorigtweet)

first_tweet <- subset(sampletweets, publish_date=="2014-11-27 17:21:00 +03")
View(first_tweet)
first_tweet$tweet_id
first_tweet$article_url
id <- format(first_tweet$tweet_id, scientific = F)
info2 <- lookup_users(first_tweet$author)
View(info2)

#Loading a library that will pull twitter data
library(rtweet)
info <- lookup_statuses(id, parse = T)
info$screen_name
info$name
info$status_url

#Fetching data from twitter using the username and tweet id to obtain more information
id2 <- "538019747355701000"
info3 <- lookup_statuses(id2, parse = T)
View(info3)

#Network visulaisation
library(igraph)
y <- data.frame(table$Group.1, table$Group.2)
j <- data.frame(sampletweets$account_category, sampletweets$author)
net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)
plot(cnet,
     net,
     vertex.size=10,
     vertex.label.cex=0.8)

net2 <- graph.data.frame(j, directed = F)
cnet2 <- cluster_edge_betweenness(net2)
plot(cnet2,
     net2,
     vertex.size=10,
     vertex.label.cex=0.8)

#Plotting
#Loading the plotting library to for create visualisations
library(ggplot2)
Top2$date <- as.Date(Top2$publish_date)
Next2$date <- as.Date(Next2$publish_date)
count <- table(Top2$date)
barplot(count, col="blue", main="Daily Number of Tweets from the Top 2 Categories(RightTroll & LeftTroll)",
        xlab="Dates", ylab = "Number of Tweets")
count2 <- table(Next2$date)
barplot(count2, col="red", main="Daily Number of Tweets from the 3rd & 4th Categories",
        xlab="Dates", ylab = "Number of Tweets")

#Frequencies
class(sampletweets$account_category)
table(sampletweets$account_category)
Top2 <- sampletweets[which(sampletweets$account_category=='RightTroll' | sampletweets$account_category=='LeftTroll'),]
Next2 <- sampletweets[which(sampletweets$account_category=='HashtagGamer' | sampletweets$account_category=='NewsFeed'),]

ctab <- subset(as.data.frame(table(sampletweets$account_category, sampletweets$author))Freq>0)
View(ctab)
View(ftable(ctab))

ctab <- as.data.frame(ctab)
maxbycat <- aggregate(ctab$Freq, by=list(ctab$Var2, ctab$Var1), FUN=max)
View(maxbycat)

table <- aggregate(sampletweets$author, by=list(sampletweets$account_category, sampletweets$author),FUN=length)
View(table)

###Most active by category
hashtaggamers <- table[which(table$Group.1=='HashtagGamer'),]
mahashtag <- max(hashtaggamers$x)
View(mahashtag)
mah <- subset(hashtaggamers, x=="2369")
View(mah)

righttrolls <- table[which(table$Group.1=='RightTroll'),]
mostactiverighttroll <- max(righttrolls$x)
View(mostactiverighttroll)
mostactiverighttroll2 <- subset(righttrolls, x=="8673")
View(mostactiverighttroll2)

lefttrolls <- table[which(table$Group.1=='LeftTroll'),]
mostlefttroll <- max(lefttrolls$x)
View(mostlefttroll)
mostactivelefttroll2 <- subset(lefttrolls, x=="2623")
View(mostactivelefttroll2)

fearmonger <- table[which(table$Group.1=='Fearmonger'),]
mostfm <- max(fearmonger$x)
View(mostfm)
mostactivefm <- subset(fearmonger, x=="29")
View(mostactivefm)

newsfeed <- table[which(table$Group.1=='NewsFeed'),]
mostlefttroll <- max(lefttrolls$x)
View(mostlefttroll)
mostactivelefttroll2 <- subset(lefttrolls, x=="2623")
View(mostactivelefttroll2)

unknown <- table[which(table$Group.1=='Unknown'),]

#Most followers by category
htaggamers <- sampletweets[which(sampletweets$account_category=='HashtagGamer'),]
mshtaggammers <- max(htaggamers$followers)
View(mshtaggammers)
toph <- subset(htaggamers, followers=="23890")
View(toph)

rttrolls <- sampletweets[which(sampletweets$account_category=='RightTroll'),]
mostfollowedrttrolls <- max(rttrolls$followers)
View(mostfollowedrttrolls)
toprt <- subset(rttrolls, followers=="10456")
View(toprt)

ltrolls <- sampletweets[which(sampletweets$account_category=='LeftTroll'),]
mostfolwedlefttroll <- max(ltrolls$followers)
View(mostfolwedlefttroll)
topl <- subset(ltrolls, followers=="4811")
View(topl)

fmonger <- sampletweets[which(sampletweets$account_category=='Fearmonger'),]

nfeed <- sampletweets[which(sampletweets$account_category=='NewsFeed'),]
mostfollowersnfeed <- max(nfeed$followers)
View(mostfollowersnfeed)
topnfeed <- subset(nfeed, followers=="18927")
View(topnfeed)

unown <- sampletweets[which(sampletweets$account_category=='Unknown'),]

library(dplyr)
sampletweets %>%
  group_by(account_category, author) %>%
  summarise(maximum = max(followers))



#Question Four
#subsetting data to get the author of a tweet
author_tweet <- subset(sampletweets, content=="I am here for a purpose and that purpose is to grow into a mountain, not to shrink to a grain of sand. - Mandino #quote via @roxanamjones")
View(author_tweet)
authorstweets <- subset(sampletweets, author=="AMELIEBALDWIN")
View(authorstweets)
write.csv(authorstweets, file = 'ameliebaldwintweets.csv', row.names = F)

#Most active date
sampletweets$dates <- as.Date(sampletweets$publish_date)
table2 <- aggregate(sampletweets$dates, by=list(sampletweets$dates),FUN=length)
View(table2)
highesttweets <- max(table2$x)
View(highesttweets)
mosttweets <- subset(table2, x=="1509")
View(mosttweets)

#Could be a Bot
bottweets <- subset(sampletweets, following=="2001")
bottweets2 <- subset(sampletweets, author=="4MYSQUAD")
bottweets3 <- subset(sampletweets, author=="AMELIEBALDWIN")
View(bottweets3)
table(bottweets3$post_type)