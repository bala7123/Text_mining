

source(hh_data_load.r)


# Start text mining

Sys.setlocale(locale="C")

# Create a corpus of the data

data25 <- tm::Corpus(VectorSource(Tweets25Aug))

inspect(data25)

data26 <- tm::Corpus(VectorSource(Tweets26Aug))

inspect(data26)

# Formatting corrections - Remove punctuations, transformation to lower,
# remove numbers and website links

data25 <- tm::tm_map(data25, removePunctuation)

data25 <- tm::tm_map(data25, content_transformer(tolower))

data25 <- tm::tm_map(data25, removeNumbers)

data26 <- tm::tm_map(data26, removePunctuation)

data26 <- tm::tm_map(data26, content_transformer(tolower))

data26 <- tm::tm_map(data26, removeNumbers)

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)

data25 <- tm_map(data25, removeURL) 

data26 <- tm_map(data26, removeURL) 

# remove stop words

data25 <- tm_map(data25, removeWords, c("hurricaneharvey","please","make","seen","may","mph",
                                        "now","makes","get","will",stopwords("english")))

data25 <- tm_map(data25, PlainTextDocument)

data25 <- tm_map(data25, stripWhitespace)


data26 <- tm_map(data26, removeWords, c("hurricaneharvey","please","make","seen","may","mph",
                                        "now","makes","get","will",stopwords("english")))

data26 <- tm_map(data26, PlainTextDocument)

data26 <- tm_map(data26, stripWhitespace)

# Build a term document matrix for 25-August

data25 <- Corpus(VectorSource(data25)) # Adding this to avoid error in TDM

data25_dmatrix <- TermDocumentMatrix(data25, control = list(minWordLength = 1))

# Matrix of the top words used in twitter

matrix25 <- as.matrix(data25_dmatrix)

v <- sort(rowSums(matrix25),decreasing=TRUE)

d <- data.frame(word = names(v),freq=v)

head(d, 20)

# Generate word could for 25-August

set.seed(1234)

wordcloud::wordcloud(words = d$word, freq = d$freq, min.freq = 400,
                     max.words = 50000, random.order = FALSE,rot.per = 0.3,
                     colors = brewer.pal(5, "Dark2"))


# Term Document Matrix for 26-Aug

data26 <- Corpus(VectorSource(data26))

data26_matrix <- TermDocumentMatrix(data26, control = list(minWordLength = 1))

# Matrix of the top words used in twitter

matrix26 <- as.matrix(data26_matrix)

v1 <- sort(rowSums(matrix26), decreasing = TRUE)

d1 <- data.frame(word = names(v1), freq = v1)

head(d1, 20)

# Generate word could for 25-August


set.seed(5678)

wordcloud::wordcloud(words = d1$word, freq = d1$freq, min.freq = 800,
                     max.words = 100000, random.order = FALSE,rot.per = 0.3,
                     colors = brewer.pal(5, "Dark2"))


# Frequent association

freq25 <- findFreqTerms(data25_dmatrix, lowfreq = 3000)

freq25

# [1] "category"  "harvey"    "hurricane" "landfall"  "live"      "news"     
# [7] "safe"      "storm"     "texas"     "trump"     "via"       "winds"

# Plot Frequencies using excel

freq26 <- findFreqTerms(data26_matrix, lowfreq = 4000)

freq26

# [1] "category"      "cupola"        "harvey"        "hurricane"    
# [5] "international" "nasa"          "path"          "safe"         
# [9] "space"         "station"       "storm"         "texas"        
# [13] "via"   


