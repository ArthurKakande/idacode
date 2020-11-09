#Sankey diagram
library(networkD3)
nodesdata <- read.csv("nodestest.csv", header = T)
linksdata <- read.csv("linkstest.csv", header = T)
mychart <- sankeyNetwork(Links = linksdata, Nodes = nodesdata, Source = "Source", 
                         Target = "Traget", Value = "Size", NodeID = "Name", fontSize = 11, 
                         nodeWidth = 15, iterations = 0)
mychart

#Loading the datasets into R
library(plyr)
library(dplyr)
library(janitor)
library(reshape2)
library(imputeTS)
library(forecast)
library(ggplot2)
library(viridis)
library(hrbrthemes)
rainfall <- read.csv("rainfall.csv", header = T)
runoff <- read.csv("runoff.csv", header = T)

#Data cleaning and analysis
colnames(rainfall)
rainfall <- clean_names(rainfall)
View(rainfall)
rainfall <- rename(rainfall, c("jan"="january", "feb"="february", "aug"="august", "sept"="september", "oct"="october", "nov"="november", "dec"="december"))
rainfallcleaned <- na_mean(rainfall)
write.csv(rainfallcleaned, file = 'rainfallcleaned.csv', row.names = F)

rainfallt <- t(rainfallcleaned)
rainfallt <- as.data.frame(rainfallt)
rainfallt <- row_to_names(rainfallt,1)
rainfallt <- tibble::rownames_to_column(rainfallt, "month")

df <- melt(rainfallt, value.name="rainfall", variable.name="year")
df$month <- as.factor(df$month)
df$month = factor(df$month,levels=unique(df$month),ordered=TRUE)
write.csv(rainfallt, file = 'rainfalltransposed.csv', row.names = F)
rainfallcleaned2 <- rainfallcleaned[c(2:13)]
rainfallcleaned$annaulaverage <- rowMeans(rainfallcleaned2)
min(rainfallcleaned$annaulaverage)
max(rainfallcleaned$annaulaverage)

monthlyaverage <- colMeans(rainfallcleaned2)
View(monthlyaverage)
min(monthlyaverage)
max(monthlyaverage)

#creating a static heatmap
rainfallheatmap <- ggplot(df, aes(month, year, fill = rainfall)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  theme_ipsum()

#making the heatmap interactive
library(plotly)
library(cowplot)
rainfallheatmap2 <- ggplotly(rainfallheatmap + theme(legend.position = "none"))

#Combining both plots in one visual
cowplot::plot_grid(rainfallheatmap, rainfallheatmap2, labels = "AUTO")
