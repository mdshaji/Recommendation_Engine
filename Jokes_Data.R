library("recommenderlab")
library(caTools)
library(reshape2)

# Jokes Rating Data
library(readxl)
Joke_rate_data <- read_xlsx(file.choose())
head(Joke_rate_data)

# Removing unnecessary column of "Id"
Joke_rate_data <- Joke_rate_data[ ,2:4]
dim(Joke_rate_data)

# Checking NA values
sum(is.na(Joke_rate_data))

# Graphical Representation

# Histogram
hist(as.vector(as.matrix(Joke_rate_data)), main = "Distribution of Joke Ratings",
     col = "yellow", xlab = "Ratings")

# Boxplot
boxplot(as.vector(as.matrix(Joke_rate_data)), col = "blue", main = "Distribution of Jokes Ratings", ylab = "Ratings")

 
# Convert to Matrix format
ratings_matrix <- as.matrix(acast(Joke_rate_data , user_id ~ joke_id, fun.aggregate = mean))
dim(ratings_matrix)

# Recommender lab real rating matrix format
R <- as(ratings_matrix, "realRatingMatrix")

rec1 <- Recommender(R, method="UBCF")# User-based collaberative filtering
rec2 <- Recommender(R, method="IBCF") # Item Based collaberative filtering
rec3 <- Recommender(R, method="SVD")
rec4 <- Recommender(R, method="POPULAR")
rec5 <- Recommender(binarize(R,minRating = 2), method="UBCF")

# Creating recommendations for user_id # 333
uid <- 333
Jokes <- subset(Joke_rate_data, Joke_rate_data$user_id==uid)
print("You have rated:")
Jokes
print("recommendations for you:")
prediction <- predict(rec1, R[uid], n=2) ## we can change the model here
as(prediction, "list")
prediction <- predict(rec2, R[uid], n=2)
as(prediction, "list")
prediction <- predict(rec3, R[uid], n=2)
as(prediction, "list")
prediction <- predict(rec4, R[uid], n=2)
as(prediction, "list")
prediction <- predict(rec5, R[uid], n=2)
as(prediction, "list")


# Creating recommendations for user_id # 4
uid <- 4
Jokes <- subset(Joke_rate_data, Joke_rate_data$user_id==uid)
print("You have rated:")
Jokes
print("recommendations for you:")
prediction <- predict(rec1, R[uid], n=2) ## we can change the model here
as(prediction, "list")
prediction <- predict(rec2, R[uid], n=2)
as(prediction, "list")
prediction <- predict(rec3, R[uid], n=2)
as(prediction, "list")
prediction <- predict(rec4, R[uid], n=2)
as(prediction, "list")
prediction <- predict(rec5, R[uid], n=2)
as(prediction, "list")

# Creating recommendations for user_id # 500
uid <- 500
Jokes <- subset(Joke_rate_data, Joke_rate_data$user_id==uid)
print("You have rated:")
Jokes
print("recommendations for you:")
prediction <- predict(rec1, R[uid], n=2) ## we can change the model here
as(prediction, "list")
prediction <- predict(rec2, R[uid], n=2)
as(prediction, "list")
prediction <- predict(rec3, R[uid], n=2)
as(prediction, "list")
prediction <- predict(rec4, R[uid], n=2)
as(prediction, "list")
prediction <- predict(rec5, R[uid], n=2)
as(prediction, "list")
