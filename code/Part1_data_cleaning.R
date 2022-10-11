knitr::opts_chunk$set(echo = TRUE)
library(reshape2)
library(ggplot2)
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(glmnet)
library(randomForest)


#data Cleaning
data = read.csv("BodyFat.csv")
#check data
data
#remove 0 
data = data[-which(data$BODYFAT==0),]
data = data[-39,]
data = data[-216,]
#remove the ID numbers
data$IDNO <- NULL
#Summary of distribution
summary(data)
#check for NA
sum(is.na(data))
write.csv(data,"BodyFat_clean.csv", row.names = FALSE)

#check correlation
#correlation matrix
cormat <- round(cor(data),2)
head(cormat)

melted_cormat <- melt(cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value),label=round(r_if_sig,2)) + 
  geom_tile()+theme(axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation", title="Correlations in BodyFat variables",   subtitle="") + scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0))











                   
                   
                   
                   


