options(scipen = 999)

library(tidyr)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)

## Harvey data load

hharvey <- read.csv("Hurricane_Harvey.csv")

head(hharvey)

names(hharvey)

dim(hharvey)
# [1] 150045      6

hharvey <- hharvey[-1]

# [1] "ID"       "Time"     "Tweet"    "Retweets" "Replies"  "Likes" 


# Start working on HHarvey

summary(hharvey)

head(hharvey$Time)

str(hharvey)

# THere are few observations with a different format containing blanks. Hence we are removing
# those observations

hharvey <- hharvey[!apply(hharvey == "" | is.na(hharvey) , 1, all),]

dim(hharvey)
#[1] 150024      6

anyNA(hharvey)
# TRUE

colnames(hharvey[,sapply(hharvey, function(x) any(is.na(x)))])

table(hharvey$Retweets, useNA = "ifany")

table(hharvey$Replies, useNA = "ifany")

table(hharvey$Likes, useNA = "ifany")

# The total dataset count is 1.5L, and the total number of NAs is just 48. 
# WHich is not even .01% of the total number. Hence we are going to remove NAs totally from this dataset


hharvey <- hharvey[complete.cases(hharvey),]

dim(hharvey)

# [1] 149976      6

anyNA(hharvey)

# [1] FALSE

str(hharvey)

# If we notice the data, we can see the Timestamp is saved as levels and we have 1169 levels
# Hence we wanted to remove them and keep the date alone to see if we can analyze the tweets
# based on the timestamp

write.csv(hharvey, "hharvey.csv")

hharvey <- read.csv("hharvey.csv", stringsAsFactors = FALSE)

str(hharvey)

hharvey <- hharvey %>%
  separate(Time, c("Date","Time") ," ")

table(hharvey$Date)

# 8/25/2017 8/26/2017 
# 50604     99372 

min(hharvey$Time)

# [1] "0:00"

max(hharvey$Time)

# [1] "23:59"

summary(hharvey$Time)

# Day wise time

table(hharvey$Time[hharvey$Date == "8/25/2017"])

# On 25-August, Tweets were taken from 20:05 to 23:59 PM every second

table(hharvey$Time[hharvey$Date == "8/26/2017"])

# On 26-August, Tweets were taken from 00:00 to 09:59 AM every second

# To perform text mining, we have to create corpus using the tweets

# We can create corpus day wise. one for 25-Aug and one for 26-Aug


Tweets25Aug <- hharvey$Tweet[hharvey$Date == "8/25/2017"]

length(Tweets25Aug)  # [1] 50604

Tweets26Aug <- hharvey$Tweet[hharvey$Date == "8/26/2017"]

length(Tweets26Aug)  # [1] 99372
