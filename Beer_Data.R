
#Packages to be used:
#recommenderlab
#ggplot

#In this case study, we will look at :

#Loading recommenderlabs Lib package
library(recommenderlab)
library(tidyverse)
# Loading the dataset
beer_df <- read.csv("beer_data.csv", stringsAsFactors = T)
str(beer_df)
sum(is.na(beer_df))

# Check the total Unique Beer Brands ID
length(unique(beer_df$beer_beerid))

# Summerize the data by checking the count of reviews against each beer ID
count_vector <- beer_df %>%
  group_by(beer_beerid) %>%
  summarize(n())
colnames(count_vector) <- c("Beer_ID", "Count_of_Reviews")

# Checking the Max & Min number of reviews on beer ID
max(count_vector$Count_of_Reviews)
min(count_vector$Count_of_Reviews)

# Check the summary of Count dataframe to know the mean/median
summary(count_vector)

# Plotting the Review count dataframe
ggplot(count_vector, aes(count_vector$Beer_ID, count_vector$Count_of_Reviews))+geom_point()

# Considering the Beer ID which at least have 2 reviews
count_vector <- count_vector[count_vector$Count_of_Reviews > 2, ]
beer_final <- beer_df[beer_df$beer_beerid %in% count_vector$Beer_ID,] 


beer_rrm <- as(beer_final, "realRatingMatrix")
class(beer_rrm)

# get some informtaion
dimnames(beer_rrm)
rowCounts(beer_rrm)
colCounts(beer_rrm)
rowMeans(beer_rrm)

#How similar are the first ten beers are with each other
beer_similar <- similarity(beer_rrm[1:10, ],
                               method = "cosine",
                               which = "beer_beerid")


#Similarity matrix
as.matrix(beer_similar)

#Visualise similarity matrix
image(as.matrix(beer_similar), main = "Beer similarity")

#Inference 
#Users 1, 3, 4 and 9 are similar

#How similar are the first five items are with each other

similar_items <- similarity(beer_rrm[,1:5 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_items)

image(as.matrix(similar_items), main = "Item similarity")

#You can notice more items being similar :Eg items 2 and 6 are similar; 5 and 10 are similar etc.

#--------------------------Understand users and ratings----------#

# Visualizing ratings
library(ggplot2)
qplot(getRatings(beer_rrm), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")

summary(getRatings(beer_rrm)) # Skewed to the right

qplot(getRatings(normalize(beer_rrm, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "Rating") 

summary(getRatings(normalize(beer_rrm, method = "Z-score"))) # seems better


qplot(rowCounts(beer_rrm), binwidth = 10, 
      main = "Movies Rated on average", 
      xlab = "# of users", 
      ylab = "# of Beers rated")
#Most users rate less number of movies.
#Very few users have rated more movies



#--------------------------Recommendation models ----------------#

#List of models available
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)# 9 types of models


#description of recommendation system algorithms/models used
lapply(recommender_models, "[[", "description")

#This gives you different types of recommendation models
#In this case study , let's compare user based and item based
#collaborative filtering

#checking the parameters of these two models
recommender_models$IBCF_realRatingMatrix$parameters

#Divide data into test 
scheme <- evaluationScheme(beer_rrm, method = "split", train = .9,
                           k = 1, given = 1, goodRating = 4)

#--arguments
#train and test
#Here we create an evaluation scheme which splits the users 
#into a training set (90%) and a test set (10%). 

#given 
#For the test set 10 items(given argument) will be given to the
#recommender algorithm and the other items will be held out for computing the error

#With goodRating=4 all items with actual user rating of greater or equal 4 are considered 
#positives in the evaluation process

scheme

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results)

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")

