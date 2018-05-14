library(plyr)
library(dplyr)
library(NLP)
library(tm)
library(tmap)
library(RColorBrewer)
library(tidytext)
library(data.table)
library(caret)
genderact<-read.csv("/Users/praneeththomas/Downloads/School Stuff/FluShot/untitled folder 2/Gender Classification/cleaned_with_gender.csv")
tweetdata<-read.csv("/Users/praneeththomas/Downloads/School Stuff/FluShot/untitled folder 2/Gender Classification/CommonTweetUser.csv")
genderact<-genderact[,c(1,5)]
colnames(genderact)<-c("Gender","Username")
genderact$Gender[(genderact$Gender==2)]<-0
tweetdata<-tweetdata[,c(2,3,4)]
colnames(tweetdata)<-c("Username","Tweet","Description")
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

id<-tweetdata[1,1]
counter<-0
newtweetdata<-NULL

for(i in 1:nrow(tweetdata)){
  
  if(counter<2 && id==tweetdata[i,1])
  {
    
    newtweetdata<-rbind(newtweetdata,tweetdata[i,])
    counter<-counter+1
  }
else{
     
     if(id!=tweetdata[i,1]){
       id<-tweetdata[i,1]
       counter<-0
     }
   }
  
  
}

tweetdata1<-ddply(newtweetdata,.(Username),summarize,
                  text=paste(Tweet,collapse = " "),
                  description=paste(unique(Description),collapse=" ")
                  )

commonelem<-inner_join(genderact,tweetdata1,by="Username")


workingdata<-dplyr::filter(commonelem,  !is.na(Gender))

workingdata<-na.omit(workingdata)

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

workingdata$description <- cleaneddata(workingdata$description)
workingdata$text <- cleaneddata(workingdata$text)

splitdata<-sample(nrow(workingdata),nrow(workingdata)*0.8)
train<-workingdata[splitdata,]
test<-workingdata[-splitdata,]

train$combi<-paste(train$description,train$text)

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
male_words <- WordFreq(male_train$combi)
female_words <- WordFreq(female_train$combi)

all_words<-merge(x = male_words, y = female_words, by = "word", all = TRUE)
colnames(all_words) <- c("Word", "M_frequency", "F_frequency")
all_words[is.na(all_words)] <- 0
all_words$sum <- all_words$M_frequency + all_words$F_frequency
all_words$male_prob<-all_words$M_frequency/all_words$sum
all_words = all_words[!(all_words$male_prob %in% c(0,1)),]
all_words = all_words[order(-all_words$male_prob),]


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
 
mean(train_op$Gender == train_op$gender_predicted, na.rm = T)*100

#accuracy rate of 81.70%

