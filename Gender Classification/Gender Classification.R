library(plyr)
library(dplyr)
library(NLP)
library(tm)
library(tmap)
library(RColorBrewer)
library(tidytext)
library(data.table)
library(caret)
#reference dataset
genderact<-read.csv("/Users/praneeththomas/Downloads/School Stuff/FluShot/untitled folder 2/Gender Classification/cleaned_with_gender.csv")
#dataset used for prediction
tweetdata<-read.csv("/Users/praneeththomas/Downloads/School Stuff/FluShot/untitled folder 2/Gender Classification/CommonTweetUser.csv")
#consider only Gender and username to compare to the actual dataset we will be working on
genderact<-genderact[,c(1,5)]
#name the columns for latter use
colnames(genderact)<-c("Gender","Username")
#replacing the value 2( which is used to identify gender F) to 0 for making it more easier to work on and understand
genderact$Gender[(genderact$Gender==2)]<-0
#considering only columns username tweet and description
tweetdata<-tweetdata[,c(2,3,4)]
colnames(tweetdata)<-c("Username","Tweet","Description")

#another way to get atleast 2 tweets from each user
# uni<- unique(tweetdata[,1])
 #uni[1]<-"IDHW"
 #newtweetdata<-tweetdata[c(1,2),]

 
 # q<-1
#for(i in 2:length(uni)){
 # id<-uni[i-1]
  #for(p in q:nrow(tweetdata)){
  #if(id!=tweetdata[p,1])
 # newtweetdata<-rbind(newtweetdata,tweetdata[c(i,i+1),])
  #q<-p
  ##}
#}

#initialize the username id to the first username in the tweetdata dataset
id<-tweetdata[1,1]
counter<-0
newtweetdata<-NULL

#iterating for all rows in the tweetdata set.
#This takes some time, about 10 mins, for obvious reasons that the dataset is way too big to run through
for(i in 1:nrow(tweetdata)){
  #this gets the first 2 tweets of every user and checks if the previous user is as the same as the current one in the loop
  if(counter<2 && id==tweetdata[i,1])
  {
    
    newtweetdata<-rbind(newtweetdata,tweetdata[i,])
    counter<-counter+1
  }
 #changes the username and thereby the 'id' to the next one in the tweetdata set
else{
     
     if(id!=tweetdata[i,1]){
       id<-tweetdata[i,1]
       counter<-0
     }
   }
  
  
}

#Combines both the tweets of a user into a single rows for readability
tweetdata1<-ddply(newtweetdata,.(Username),summarize,
                  text=paste(Tweet,collapse = " "),
                  description=paste(unique(Description),collapse=" ")
                  )

#associating common columns from the datasets genderact and processed tweetdata to add a new gender column
#commonelem is the working data frame now
commonelem<-inner_join(genderact,tweetdata1,by="Username")


#omitting rows that dont have gender
#we could impute these values and see how that works out and test the accuracy(just another approach)
workingdata<-dplyr::filter(commonelem,  !is.na(Gender))
workingdata<-na.omit(workingdata)

#function for cleaning data, filtering stop words and casing the letters
cleaneddata<- function(data){
  data<-gsub("\\W",' ',data )
  data <- trimws(data)
  data <- gsub("\\s+", " ", data)
  data <-gsub("/", " ", data)
  data <- gsub("@", " ", data)
  data <- gsub("\\|", " ", data)
  data <- tolower(data)
  return(data)
}
#calling the function to clean description column and tweet column
workingdata$description <- cleaneddata(workingdata$description)
workingdata$text <- cleaneddata(workingdata$text)

#Splitting data to training data set and test data set
splitdata<-sample(nrow(workingdata),nrow(workingdata)*0.8)
train<-workingdata[splitdata,]
test<-workingdata[-splitdata,]

#merging description and tweet of an user 
train$combi<-paste(train$description,train$text)

#Implementation technique: Natural Language Programming(NLP)
 #a) This method counts the number of times a word has been repeated in a tweet and in all tweets.
 #b) It cross references the count of words within a user of a particular gender.
 #c) The number of occurences of every word occured in a tweet is referenced to a particular gender.
 #d) corpus of the dataset is considered when this function is called
WordFreq <- function(d){
  d = Corpus(VectorSource(d))
  d = tm_map(d, removePunctuation)
  d = tm_map(d, removeNumbers)
  d <- tm_map(d, removeWords,c('https','http',stopwords('english')))
  tdm <- TermDocumentMatrix(d)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
}

male_train <- train[train$Gender == 1,]
female_train <- train[train$Gender == 0,]

#training the train data set
#function Wordfreq is called to calculate the number of words a gender uses in the entire text set
male_words <- WordFreq(male_train$combi)
female_words <- WordFreq(female_train$combi)

#the sum of all words is counted and ordered by all_words
#calculating the probability of male words 
all_words<-merge(x = male_words, y = female_words, by = "word", all = TRUE)
colnames(all_words) <- c("Word", "M_frequency", "F_frequency")
all_words[is.na(all_words)] <- 0
all_words$sum <- all_words$M_frequency + all_words$F_frequency
all_words$male_prob<-all_words$M_frequency/all_words$sum
all_words = all_words[!(all_words$male_prob %in% c(0,1)),]
all_words = all_words[order(-all_words$male_prob),]

# train on the corpus of the entire text and clean it
d <- Corpus(VectorSource(train$combi))
d<- tm_map(d, removePunctuation)
d <- tm_map(d, removeNumbers)
d <- tm_map(d, removeWords,c('https','http',stopwords('english')))
tdm <- TermDocumentMatrix(d)
DF <- tidy(tdm)
DF <- DF[DF$term %in% all_words$Word,]
merged_set <- merge(x = DF, y = all_words, by.x = "term", by.y = "Word")
merged_set$document <- as.numeric(merged_set$document)
merged_set <-merged_set[order(order(merged_set$document)),]
aggr <- aggregate(cbind(M_frequency, sum) ~ document, data = merged_set, sum)
aggr$male_prob <- aggr$M_frequency / aggr$sum
train$ID <- 1:nrow(train)
train_op<- merge(train, aggr, by.x = "ID", by.y = "document", all.x = T)
train_op$gender_predicted <- ifelse(train_op$male_prob >= 0.5, 1, 0)

#training test data
#test$combi = paste(test$description, test$text)
#d = Corpus(VectorSource(test$combi))
#d = tm_map(d, removePunctuation)
#d = tm_map(d, removeNumbers)
#d <- tm_map(d, removeWords,c('https','http',stopwords('english')))
#tdm <- TermDocumentMatrix(d)
#DF <- tidy(tdm)
#DF <- DF[DF$term %in% all_words$word,]
#merged_set <- merge(x = DF, y = all_words, by.x = "term", by.y = "word")
#merged_set$document <- as.numeric(merged_set$document)
#merged_set <- merged_set[order(-merged_set$document),]
#merged_set = merged_set[order(order(merged_set$document)),]
#aggr = aggregate(cbind(freq_m, sum) ~ document, data = merged_set, sum)
#aggr$male_prob = aggr$freq_m / aggr$sum
#test$ID = 1:nrow(test)
#test_op= merge(test, aggr, by.x = "ID", by.y = "document", all.x = T)
#test_op$gender_predicted = ifelse(test_op$male_prob >= 0.5, 1, 0)
#mean(test_op$gender == test_op$gender_predicted, na.rm = T)*100
#sum(is.na(test_op$gender_predicted))
 
#calculating accuracy
accuracy<-mean(train_op$Gender == train_op$gender_predicted, na.rm = T)*100

#accuracy rate of 81.70%

