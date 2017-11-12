if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(rattle)) {install.packages("rattle"); library(rattle)}
if (!require(rpart.plot)) {install.packages("rpart.plot"); library(rpart.plot)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
setwd("..path")
# reading main data file
orig_imdb_data <- read.csv("..path/Dataset.csv")
# generating a new column for movie id
for (i in 1:dim(orig_imdb_data)[1]){
  orig_imdb_data[i,"movieId"]=i
}

# selecting a part of data, as kaggle data does not have relese month and date
imdb_data<-orig_imdb_data[5250:10000,]
#head(imdb_data)
imdb_data_final<-imdb_data

CatVars <- names(which(sapply(imdb_data_final,is.factor)))
CatVars
NumVars <- names(which(sapply(imdb_data_final,is.numeric)))
NumVars

stopifnot(length(CatVars)+length(NumVars)==ncol(imdb_data))

# change date formats
imdb_data_final$release_date=as.Date(as.character(imdb_data_final$title_year),format="%m/%d/%y")
imdb_data_final$release_month=format(imdb_data_final$release_date,"%m")
imdb_data_final$release_monthyear=format(imdb_data_final$release_date,"%m-%Y")

# creating release seasons using release month
imdb_data_final$release_season=rep("1Winter",length(imdb_data_final$release_month))
imdb_data_final$release_season[imdb_data_final$release_month %in% c('03','04')]="2Spring"
imdb_data_final$release_season[imdb_data_final$release_month %in% c('05','06','07')]="3Summer"
imdb_data_final$release_season[imdb_data_final$release_month %in% c('08','09','10')]="4Fall"
imdb_data_final$release_season[imdb_data_final$release_month %in% c('11','12')]="5Holiday"


library(DMwR)
vars <- c(NumVars)
knnOutput <- knnImputation(imdb_data_final[, vars])  # perform knn imputation.
anyNA(knnOutput)
actuals <- imdb_data$gross[is.na(imdb_data_final$gross)]
predicteds <- knnOutput[is.na(imdb_data_final$gross), "gross"]
#regr.eval(actuals, predicteds)
knnOutput <- sapply(knnOutput[,-11],round)
imdb_data_final[,NumVars[-11]] <- knnOutput
#uniqueimdb_data_final
genreList<-c()
# each movie had multiple genres so handling that
getEachGenre<-function(str1)
{
  str1<- toString(str1)
  temp<-strsplit(str1,'|',fixed=TRUE)
  return (temp[[1]])
  
}
chosenGenres<-c('Drama','Comedy','Action','Romance','Horror','Animation','Sci-Fi','Sport','Music','Documentary')
genreFrequency<-sort(table(genreList),decreasing=TRUE)
for (i in 1:dim(imdb_data_final)[1]){
  temp<-getEachGenre(imdb_data_final[i,]$genres)
  for (j in chosenGenres)
  {
    if (is.element(j,temp))
    {
      imdb_data_final[i,"final_genres"]=j
    }
  }
}

# summing all actor likes to form a new variable, actor_final_likes
for (i in 1:dim(imdb_data_final)[1]){
  actor_final_likes<-imdb_data_final[i,]$actor_1_facebook_likes+imdb_data_final[i,]$actor_2_facebook_likes+imdb_data_final[i,]$actor_3_facebook_likes
  imdb_data_final[i,"actor_final_likes"]=actor_final_likes
}
#summary(imdb_data_final$final_genres)
#imdb_data_final<-imdb_data_final[!is.na(imdb_data_final$final_genres),]

# converting content rating and genre to factor variables
imdb_data_final$content_rating<-as.factor(imdb_data_final$content_rating)
imdb_data_final$final_genres<-as.factor(imdb_data_final$final_genres)

set.seed(7890)
library(caret)
# selecting needed variables from data set
final_data <- imdb_data_final[,c('budget','content_rating','final_genres','director_facebook_likes','duration','actor_final_likes','release_season')]
final_data$release_season_num=rep(1,length(imdb_data_final$release_month))
final_data$release_season_num[imdb_data_final$release_month %in% c('03','04')]=2
final_data$release_season_num[imdb_data_final$release_month %in% c('05','06','07')]=3
final_data$release_season_num[imdb_data_final$release_month %in% c('08','09','10')]=4
final_data$release_season_num[imdb_data_final$release_month %in% c('11','12')]=5
# partiniong data
final_data<-na.omit(final_data)
s <- createDataPartition(y = final_data$release_season, p = 0.8, list = FALSE)
trainData   <- final_data[s,]
testData    <- final_data[-s,]
stopifnot(nrow(trainData) + nrow(testData) == nrow(final_data))

##################### decision tree to predict release season  ######################

# estimate a tree to predict the release month
ctree = rpart(release_season~final_genres+content_rating+director_facebook_likes+actor_final_likes+duration+budget,data=trainData,  method = "class",parms = list(split="information"))
summary(ctree)
plot(ctree)
text(ctree)
prp(ctree)
fancyRpartPlot(ctree,cex=.7)
summary(ctree)

prediction_decision_tree <- predict(ctree, testData, type = "class")
confusionMatrix(table(prediction_decision_tree,testData$release_season))

# predicting release season using random forest
library(randomForest)
summary(trainData)
x <- subset(trainData, select=-c(release_season_num,release_season))
fit_random_forest<- randomForest(x = x,y=trainData$release_season_num,data=trainData,importance=TRUE,type="class",ntree = 200)
predict_random_forest <- predict(fit_random_forest, testData, type = "response")
final_responses<-c()
for (i in predict_random_forest){
    final_responses<-c(final_responses,round(i,digits=0))
  
    
}
confusionMatrix(final_responses,testData$release_season_num)


################################# starting topic modelling ##################
library(tm)
#library(tmap)
# reading plot keywords file
keywords_df<-read.csv("..path/plotwords.csv",header = FALSE)
# selecting 1000 movies to do topic modelling
keywords_df<-keywords_df[7500:8500,]

# creating corpus
docs <- Corpus(VectorSource(keywords_df$V3))
writeLines(as.character(docs[[29]]))
docs <-tm_map(docs,content_transformer(tolower))

#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "???")
#install.packages("SnowballC")
library(SnowballC)
#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)

writeLines(as.character(docs[[4]]))
#Stem document
docs <- tm_map(docs,stemDocument)

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#convert rownames of term document matrix to movie names
rownames(dtm) <- keywords_df$V1


#load topic models library
library(topicmodels)
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE


#rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
#dtm.new   <- dtm[rowTotals> 0, ] 

# selecting onlt words which occour in more than 20 movies
dtm.new=dtm[,apply(dtm,2,sum)>=20]
# droping movies which contain no words
dtm.new=dtm.new[apply(dtm.new,1,sum)>0,]

# selecting movies which are left after droping movies with no keywords
umovies=imdb_data_final[imdb_data_final$movieId %in% as.integer(rownames(dtm.new)),]



# estimate a series of LDA models
ClusterOUT2 = LDA(dtm.new,2,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT3 = LDA(dtm.new,3,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT4 = LDA(dtm.new,4,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT5 = LDA(dtm.new,5,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT6 = LDA(dtm.new,6,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT7 = LDA(dtm.new,7,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT8 = LDA(dtm.new,8,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT9 = LDA(dtm.new,9,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT10 = LDA(dtm.new,10,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT15 = LDA(dtm.new,15,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
ClusterOUT20 = LDA(dtm.new,20,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))


ntopics=c(2:10,15,20)
llike=rep(NA,length(ntopics))
llike[1]=ClusterOUT2@loglikelihood
llike[2]=ClusterOUT3@loglikelihood
llike[3]=ClusterOUT4@loglikelihood
llike[4]=ClusterOUT5@loglikelihood
llike[5]=ClusterOUT6@loglikelihood
llike[6]=ClusterOUT7@loglikelihood
llike[7]=ClusterOUT8@loglikelihood
llike[8]=ClusterOUT9@loglikelihood
llike[9]=ClusterOUT10@loglikelihood
llike[10]=ClusterOUT15@loglikelihood
llike[11]=ClusterOUT20@loglikelihood



plot(ntopics,llike,ylab="LogLikelihood")
lines(ntopics,llike)


#Number of topics
k <- 12


#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm.new,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.terms <- as.matrix(terms(ldaOut,6))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)


# matrix with probabilities of each term per topic
# this is a matrix whose rows are topics, columns are terms,
# and each element is the probability of a term for a specific topic
ClustTopics = exp(ldaOut@beta)
colnames(ClustTopics)=colnames(dtm.new)
#dim(ClustTopics)   # nrows is the # of topics, and ncols is # of terms
  
if (!require(plyr)) {install.packages("plyr"); library(plyr)}    



# printing 20 most likely terms used for each topic
results=matrix('a',nrow=20,ncol=12)
for (i in 1:12) {
  idxtopterms=order(ClustTopics[i,],decreasing=TRUE)  # find the indices associated with the topic
  topterms=ClustTopics[i,idxtopterms[1:20]]  # identify the terms with the highest probabilities
  results[,i]=names(topterms)                # save the names
}
print(results)



# probability of topic assignments (each movie has its own unique profile)
# rows are movies and columns are topics
ClustAssign = ldaOut@gamma   # this is a matrix with the row as the movie and column as the topic
rownames(ClustAssign)=umovies$movie_title



# find the index associated with our target movie
imovie=(1:nrow(umovies))[umovies$movie_title=="Harry Potter and the Deathly Hallows: Part 2"]
print(umovies[imovie,])

# show the topics associated with a selected movie
barplot(ClustAssign[imovie,],names.arg=1:ncol(ClustAssign),main=paste("Topics Associated with selected movie",umovies$display_name[imovie]))

# visualize the distribution of topics across the movies
boxplot(ClustAssign,xlab="Topic",ylab="Probability of Topic across Movies")

# we can compute the distance between a target movie and all other movies in the "topics" space
topicdist=ClustAssign-matrix(ClustAssign[imovie,],nrow=nrow(ClustAssign),ncol=ncol(ClustAssign),byrow=T)
topicdistss=sqrt(apply(topicdist^2,1,sum))  # taking the root of the sum of square of the distance between movies
augmovies=cbind(umovies,topicdistss)  # add the distance to the original movie data frame
augmovies=augmovies[-imovie,]  # removing Harry Potte from our set
idxorder=order(augmovies$topicdistss)  # get the index of movies by similarity
head(augmovies[idxorder,c("movieId","topicdistss","movie_title","release_date","final_genres","content_rating")])  # most similar movies


cutoff=sort(topicdistss)[10]  # select the 10th smallest sum of square distance
print(paste("Most similar movies to movie #",imovie,":",umovies$movie_title[imovie]))
augmovies[topicdistss<=cutoff,c("movie_title","release_season")]
table(augmovies[topicdistss<=cutoff,c("movie_title","release_season")]$release_season)
