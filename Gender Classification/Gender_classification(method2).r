library(plyr)
library(dplyr)
library(NLP)
library(tm)
library(tmap)
library(RColorBrewer)
library(tidytext)
library(data.table)
library(caret)
require(tokenizers)
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
#male_train_both<-male_train[,5]
#female_train_both<-female_train[,5]
#training the train data set
#male_words <- WordFreq(male_train$combi)
#female_words <- WordFreq(female_train$combi)
#male_words<-as.data.frame(tokenize_words(male_train_both[1,1]))


total_male_text<- as.vector(aggregate(male_train$both ~ Gender, data=male_train, FUN=paste, collapse=" "))
total_female_text<- aggregate(female_train$both ~ Gender, data=female_train, FUN=paste, collapse=" ")
male_words<-as.data.frame(tokenize_words(total_male_text))
female_words<-as.data.frame(tokenize_words(total_female_text))


#New approach: Probability

workingdata$both<-paste(workingdata$text,workingdata$description)

probability<-function(x){
sampletweet<-workingdata[3,5]

words<-as.data.frame(tokenize_words(sampletweet))


malen<-nrow(male_words)
femlen<-nrow(female_words)
p<-0
for(i in 1:nrow(words))
{
  for(j in 1:nrow(male_words)){
    if(words[i,1]==male_words[j,1])
    {
      p<-p+(male_words[j,2])/malen
      
    } 
    if(words[i,1]==female_words[j,1])
    {
      q<-q+(female_words[j,2])/femlen
   
    } 
  }
  
  if( p > q){
    return p
  } else if( p<q) {
    return q
  }
}

}

for(i in 1:nrow(workingdata)){
  newdf[i,1]<-probability(i)
}

write.csv(newdf,file="results.csv")
