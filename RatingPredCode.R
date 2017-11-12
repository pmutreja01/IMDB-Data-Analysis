library(ggplot2)
library(reshape2)
library(corrgram)
library(dplyr)

imdb_data <- read.csv("..path/Dataset.csv",na.strings=c("", "NA"))
head(imdb_data)
colnames(imdb_data)
str(imdb_data)
genreList<-c()
getEachGenre<-function(str1)
{
  str1<- toString(str1)
  temp<-strsplit(str1,'|',fixed=TRUE)
  return (temp[[1]])
  
}
chosenGenres<-c('Drama','Comedy','Action','Romance','Horror','Animation','Sci-Fi','Sport','Music','Documentary')
genreFrequency<-sort(table(genreList),decreasing=TRUE)
for (i in 1:dim(imdb_data)[1]){
  temp<-getEachGenre(imdb_data[i,]$genres)
  for (j in chosenGenres)
  {
    if (is.element(j,temp))
    {
      imdb_data[i,"final_genres"]=j
    }
  }
}
sapply(imdb_data,class)
imdb_data$final_genres<-as.factor(imdb_data$final_genres)
CatVars <- names(which(sapply(imdb_data,is.factor)))
CatVars
NumVars <- names(which(sapply(imdb_data,is.numeric)))
NumVars

stopifnot(length(CatVars)+length(NumVars)==ncol(imdb_data))



######################################
# calulating age code starts
splittingDate<-function(str1)
{
  str1<- toString(str1)
  temp<-strsplit(str1,'/')[[1]]
  toReturn<-""
  if (length(temp)>1)
  {
    toReturn<- temp[3]
  }
  else
  {
    toReturn<- temp[1]
  }
  return (toReturn)
}

calculatingAge<-function(givenYear)
{
  return (2017-strtoi(givenYear))
}

years<-sapply(imdb_data$title_year,splittingDate)
movie_age<-sapply(years,calculatingAge)
imdb_data$movie_Age<-movie_age
head(imdb_data)

NumVars_ageincl <- names(which(sapply(imdb_data,is.numeric)))
NumVars_ageincl


############PLOTTING CODE STARTS HERE############
summary(imdb_data)
pairs(imdb_data[NumVars_ageincl])

for (i in NumVars_ageincl)
{
  # jpeg(paste(i,'jpeg', sep = "."))
  hist(imdb_data[[i]], xlab = i, main = paste('Histogram for', i))
  boxplot(imdb_data[[i]], main = paste('Boxplot for', i))
  print(ggplot(imdb_data, aes(x = imdb_data[[i]], y = imdb_data$imdb_score)) + geom_point()+labs(x=i,y='score'))
  #dev.off()
}


barplot(table(imdb_data$content_rating), xlab='Content Rating', main = 'Barplot for Content-Rating', las =1 ,col = 'blue' )

barplot(table(imdb_data$final_genres), xlab='Genre', main = 'Barplot for Genre', las =2 ,col = 'blue',horiz = F)

corMat <- cor(na.omit(imdb_data[NumVars_ageincl]))
corMat
library('corrplot')
corrplot(corMat,addCoef.col = TRUE)

col_names <- names(which(colSums(is.na(imdb_data))>0))
col_names

ggplot(data = melt(imdb_data), mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

ggplot(imdb_data, aes(x = num_voted_users, y = imdb_score)) + geom_point() + geom_smooth()

ggplot(aes(x = title_year, y = imdb_score), data = imdb_data) +
  + geom_point() + geom_smooth(method = "auto")

#ggplot(imdb_data, aes(x =factor(title_year), y = imdb_score)) + geom_boxplot() + scale_x_discrete(breaks = pretty(imdb_data$title_year,n=20))

movie_fb_likes_in100million <- imdb_data$movie_facebook_likes/1000000

ggplot(imdb_data, aes(x = movie_fb_likes_in100million, y = imdb_score)) + geom_point() + geom_smooth()

library(plyr)
library(car) 
library(moments)  
library(lattice)  

genres_data <- ddply(imdb_data, .(final_genres), summarize,  gross= mean(gross, na.rm = TRUE) / 1000000, 
                     budget = mean(budget, na.rm = TRUE)/ 1000000)

genres_data[ ,"Profit"] <-   genres_data$gross - genres_data$budget

genres_data <- genres_data[order(-genres_data$Profit),]

genres_data <- genres_data[1:10,]


symbols( genres_data$budget, genres_data$gross, circles=genres_data$Profit, 
         fg="white", bg=colors()[417:446], xlab="Budget in 100 milion", ylab="Gross Income in 100 milion")
title("Profit Distribution across top 10 Genres")
text(genres_data$budget, genres_data$gross, genres_data$final_genres, cex=0.6)

install.packages('readr')
install.packages('ggthemes')
library(readr)
library(ggthemes)

# first we use plyr to calculate the mean rating and SE for each main actor
ratingdat <- ddply(imdb_data, c("actor_1_name"), summarise,
                   M = mean(imdb_score, na.rm=T),
                   SE = sd(imdb_score, na.rm=T)/sqrt(length(na.omit(imdb_score))),
                   N = length(na.omit(imdb_score)))
ratings<-ratingdat[which(ratingdat$N>=15),]

# make actor into an ordered factor, ordering by mean rating:
ratings$actor_1_name <- factor(ratings$actor_1_name)
ratings$actor_1_name <- reorder(ratings$actor_1_name, ratings$M)

ggplot(ratings, aes(x = M, xmin = M-SE, xmax = M+SE, y = actor_1_name )) +
  geom_point() + 
  geom_segment( aes(x = M-SE, xend = M+SE,
                    y = actor_1_name, yend=actor_1_name)) +
  theme(axis.text=element_text(size=8)) +
  xlab("Mean rating") + ylab("First Actor") 

# then we use plyr to calculate the mean rating and SE for each director
ratingdat <- ddply(imdb_data, c("director_name"), summarise,
                   M = mean(imdb_score, na.rm=T),
                   SE = sd(imdb_score, na.rm=T)/sqrt(length(na.omit(imdb_score))),
                   N = length(na.omit(imdb_score)))
ratings<-ratingdat[which(ratingdat$N>=10 & !(ratingdat$director_name=='')),]

# make director into an ordered factor, ordering by mean rating:
ratings$director_name <- factor(ratings$director_name)
ratings$director_name <- reorder(ratings$director_name, ratings$M)

ggplot(ratings, aes(x = M, xmin = M-SE, xmax = M+SE, y = director_name )) +
  geom_point() + 
  geom_segment( aes(x = M-SE, xend = M+SE,
                    y = director_name, yend=director_name)) +
  theme(axis.text=element_text(size=8)) +
  xlab("Mean rating") + ylab("Director") 


############### MISSING VALUE IMPUTATION STARTS####################
library(DMwR)
vars <- c(NumVars_ageincl,'final_genres')
knnOutput <- knnImputation(imdb_data[, vars])  # perform knn imputation.
anyNA(knnOutput)
knnOutput[,NumVars_ageincl[-11]] <- sapply(knnOutput[,NumVars_ageincl[-11]],round)
genre_included<-c(NumVars_ageincl[-11],'final_genres')
imdb_data[,genre_included] <- knnOutput[-11]

############### Transformation for linear regression ########

imdb_data2 <- imdb_data
vars <- subset(NumVars_ageincl, !(NumVars_ageincl %in% c('duration','imdb_score')))
imdb_data2[vars] <- sqrt(sqrt((imdb_data[vars])))


############### Spliting dataset ###############
set.seed(7890)
library(caret)
final_data <- imdb_data2[,c(NumVars_ageincl,'content_rating','final_genres')]
final_data$imdb_score_01 = final_data$imdb_score/10
s <- createDataPartition(y = final_data$imdb_score, p = 0.8, list = FALSE)
trainData   <- final_data[s,]
testData    <- final_data[-s,]
stopifnot(nrow(trainData) + nrow(testData) == nrow(final_data))

############## Model fitting ################
targetVar <- c('imdb_score_01')
xVars <- c(NumVars_ageincl,'content_rating','final_genres')[-11]

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(sprintf("`%s`", targetVar), "~", paste(xVars, collapse = '+ '),sep = ""))
  } else {
    modelForm <- as.formula(paste(sprintf("`%s`", targetVar), "~", paste(xVars, collapse = '+ '), -1,sep = ""))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars,TRUE)



############ Linear Regression [R Sq : 0.4309769404]
linearModel = lm(modelForm, data=trainData)
summary(linearModel)

fbselection <- step(linearModel, scope = list(lower= final_data~1, upper= final_data ~., data=final_data), direction = 'both')
summary(fbselection)
formula(fbselection)

lrpredict <- predict(fbselection, testData)*10


R2Model1 <- 1 - (sum((testData$imdb_score-lrpredict )^2)/sum((testData$imdb_score-mean(testData$imdb_score))^2))
print (R2Model1)
SSEM1 <- sum((testData$imdb_score - lrpredict)^2, na.rm=T)

#SSE : 1528.586038



################# Logistic Regression ############# 
logistic <- glm(modelForm,family=binomial(link='logit'),data=trainData)
predlogistic <- predict(logistic, testData,type="response")*10
#SSE 1664.691142
sum((testData$imdb_score - predlogistic)^2, na.rm=T)
summary(logistic)


#### Null deviance: 466.82195  on 8501  degrees of freedom
#### Residual deviance: 264.60724  on 8478  degrees of freedom
#### AIC: 8317.8817

fblogi <- step(logistic, scope = list(lower= final_data~1, upper= final_data ~., data=final_data), direction = 'both')
formula(fblogi)
summary(fblogi)

R2Model2 <- 1 - (sum((testData$imdb_score-predlogistic )^2)/sum((testData$imdb_score-mean(testData$imdb_score))^2))
#### R Sq : 0.4344054515

######### Scaling ####### 
select_columns = c('imdb_score', 'num_user_for_reviews', 'num_critic_for_reviews', 'duration', 
                   'director_facebook_likes', 'movie_facebook_likes',
                   'actor_1_facebook_likes', 'actor_2_facebook_likes', 'actor_3_facebook_likes',
                   'gross', 'num_voted_users', 'budget','movie_Age','content_rating','final_genres')

movie_data = imdb_data[,select_columns]
head(movie_data)
n_predictors = dim(movie_data[,select_columns[-1]])[2]

library(class)
# first normalize variables to make them in a similar value range (from 0 to 1)
minimum_variables = matrix(0, 1, n_predictors)
maximum_variables = matrix(0, 1, n_predictors)
for (i in seq(n_predictors-2)){
  minimum_variables[i] = min(movie_data[,i], na.rm=T)
  maximum_variables[i] = max(movie_data[,i], na.rm=T)
}
movie_data2 = movie_data
for (i in seq(2,n_predictors-2,1)){
  movie_data2[,i] = (movie_data[,i]-minimum_variables[i])/(maximum_variables[i]-minimum_variables[i])
}
head(movie_data2)
set.seed(7890)
s <- createDataPartition(y = movie_data2$imdb_score, p = 0.8, list = FALSE)
training   <- movie_data2[s,]
test    <- movie_data2[-s,]
stopifnot(nrow(training) + nrow(test) == nrow(movie_data2))


######################## Random Forest ##########
library('randomForest')
movie.rf = randomForest(imdb_score~.,data=training, mtry=9,importance=TRUE, type = "regression")
movie.rf
summary(movie.rf)
predicted.rf = predict(movie.rf,newdata=test)
# calculate the sum of squared error
sum((test$imdb_score - predicted.rf)^2, na.rm=T)
# Sum of squared error is 967.7379641

x = training[,2:11]
y = training[,1]

R2Model5 <- 1 - (sum((test$imdb_score-predicted.rf )^2)/sum((test$imdb_score-mean(test$imdb_score))^2))
# R Sq is 0.6397551701

# Tunning the Random Forest mtry paramter
bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500,doBest = TRUE)


## Show "importance" of variables: higher value mean more important:
round(importance(movie.rf), 2)


##################### Boosting ###############
install.packages('gbm')
library(gbm)
movie.boost=gbm(imdb_score~.,data=training,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(movie.boost)
predicted.boost=predict(movie.boost,newdata=test,n.trees=5000)
# calculate the sum of squared error
sum((test$imdb_score - predicted.boost)^2, na.rm=T)
# Sum of squared error is 1277.528258

R2Model5 <- 1 - (sum((test$imdb_score-predicted.boost )^2)/sum((test$imdb_score-mean(test$imdb_score))^2))

#### R Sq is 0.5244343334

###################### Decision tree ############
library(rpart)
DT <- rpart(imdb_score~.-imdb_score_01,
            data=trainData,
            method="anova")
DT
summary(DT)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(DT)
rsq.rpart(DT)
predicted.dt = predict(DT,newdata=testData)
# calculate the sum of squared error
sum((testData$imdb_score - predicted.dt)^2, na.rm=T)
#SSE is 1800.10969
R2model6 <- 1-sum((testData$imdb_score-predicted.dt)^2)/sum((testData$imdb_score-mean(testData$imdb_score))^2)
#R2 = 0.3299010341
