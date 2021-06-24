##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

# Added some extra useful libraries to the pre-given code

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(recosystem)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# We are left with to main sets edx set and validation set

rm(dl, ratings, movies, test_index, temp, movielens, removed)

head(edx,3)

##########################################################
# Creation of RMSE function to facilitate RMSE calculation
##########################################################

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

##########################################################
# Some useful data fields creation and cleaning
##########################################################

#extracting movie release year from title and rating year from timestamp

edx <- edx %>% mutate(rating_year = year(as.POSIXlt(timestamp, origin = "1970-01-01")),
                      hour = hour(as.POSIXlt(timestamp, origin = "1970-01-01")),
                      weekday = weekdays(as.POSIXlt(timestamp, origin = "1970-01-01")),
                      movie_year = as.numeric(str_sub(title,-5,-2)),
                      movie_age = (rating_year - movie_year))
                      
validation <- validation %>% mutate(rating_year = year(as.POSIXlt(timestamp, origin = "1970-01-01")),
                                    hour = hour(as.POSIXlt(timestamp, origin = "1970-01-01")),
                                    weekday = weekdays(as.POSIXlt(timestamp, origin = "1970-01-01")),
                                    movie_year = as.numeric(str_sub(title,-5,-2)),
                                    movie_age = (rating_year - movie_year))

#validation set will be parked and only used once to obtain final RMSE after choosing a final model

#separating the multiple genres information could turn useful for exploration or modeling

edx_separated <- edx %>% separate_rows(genres,sep = "\\|")

edx_split <- edx_separated %>% mutate(value = 1) %>% spread(genres, value , fill = 0)

edx %>% select(timestamp, rating_year, weekday, hour) %>% head(3)

edx %>% select(title, rating_year, movie_year, movie_age) %>% head(3)

##########################################################
# Exploratory analysis of the dataset
##########################################################

summary(edx)

#total number of ratings in edx set

n_ratings_total <- nrow(edx)

#total number of unique users and movies

unique_movies <- n_distinct(edx$movieId)
unique_users <- n_distinct(edx$userId)

# there are 20 differnt types of individual genres, of which Drama Comedy and Thriller are the top genres

n_distinct(edx_separated$genres)

mean(edx$rating)

n_ratings_total / unique_users

n_ratings_total / unique_movies

pure_genres_label <- edx_separated %>% group_by(genres) %>% summarize(n=n()) %>% arrange(desc(n))
pure_genres_label <- as.vector(pure_genres_label$genres)

#drama, comedy and thriller among the most used genre tags

edx_separated %>% group_by(genres) %>% summarize(n=n()) %>% filter(genres != 'NA') %>% 
  arrange(desc(n)) %>% ggplot(aes(x=reorder(genres,n), y=n)) + 
  geom_col() + coord_flip() + xlab("Number of ratings") + ylab("Movie (individual) genres")

#top_20 genre labels including "pure" and combined labels

edx %>% group_by(genres) %>% summarize(n = n()) %>% filter(genres != 'NA') %>% 
  top_n(20,n) %>% ggplot(aes(x=reorder(genres,n), y=n)) + 
  geom_col() + coord_flip() + xlab("Number of ratings") + ylab("Movie genres")

#Drama, Comedy, Comedy|Drama and Horror are the most frequently rated genres

#observing ratings distribution: full figure ratings x.0 are more common than x.5 ratings

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_col() + xlab("Rating") + ylab("Number of ratings")

#observing number of ratings per user distribution: 128 ratings per users but some are more active than others

edx %>% group_by(userId) %>%
  summarize(n_ratings = n()) %>%
  ggplot(aes(x=n_ratings, y=..count..)) + 
  geom_histogram() + 
  scale_x_log10() + xlab("Ratings per user") + ylab("Count of users")

#observing number of ratings per movie distribution: 842 ratings per movie but some are more rated than others

edx %>% group_by(movieId) %>%
  summarize(n_ratings = n()) %>%
  ggplot(aes(x=n_ratings, y=..count..)) + 
  geom_histogram() + 
  scale_x_log10()  + xlab("Ratings per movie") + ylab("Count of movies")

#genres contribution by year: based on every pure gender incidence

edx_separated %>% filter(movie_year >= 1930) %>%
  filter(genres %in% pure_genres_label) %>%
  group_by(genres,movie_year) %>% summarize(n_movies = n()) %>%
  ggplot(aes(x = movie_year,y = n_movies, fill = reorder(genres,-n_movies))) +
  geom_bar(position="stack", stat="identity") + xlab("Movie release year") + ylab("Number of ratings") +
  theme(legend.text = element_text(size=5),legend.key.size = unit(0.4, 'cm')) + labs(fill="Genres")

#exploring the average rating by genre

edx %>% group_by(genres) %>% filter(genres != 'NA' & genres != "IMAX") %>% 
  summarize(rating = mean(rating), n = n()) %>% top_n(20,n) %>%
  ggplot(aes(x = reorder(genres,rating), size = n, y = rating)) + geom_point() + coord_flip() +
  xlab("Rating") + ylab("Top 20 genre combinations") + labs(size = "Number of ratings")

#average rating trend through the year the ratings where generated

edx %>% group_by(rating_year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(rating_year, rating)) +
  geom_point() +
  geom_smooth()

#average rating trend through the year the movie was released

edx %>% group_by(movie_year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(movie_year, rating)) +
  geom_point() +
  geom_smooth()

#average rating depending on how old the movie was at time of rating

edx %>% mutate(movie_age = rating_year - movie_year) %>% filter (rating_year >= movie_year) %>%
  group_by(movie_age) %>% summarize(rating = mean(rating), n = n()) %>%
  ggplot(aes(movie_age, rating)) + geom_point() + geom_smooth()

#only in instances were the movies are quite old ratings seem to be clearly higher

#exploring a few other parameters linked to rating time for example weekday or hour

edx %>% group_by(hour) %>% summarize(rating = mean(rating), n = n()) %>%
  ggplot(aes(hour, rating)) + geom_point() + geom_smooth()

#evenings and nighttime seem to generate higher ratings

weekday_avgs <- edx %>% group_by(weekday) %>% summarize(rating = mean(rating), n = n())

weekday_avgs$weekday <- factor(weekday_avgs$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

weekday_avgs %>% ggplot(aes(weekday, rating)) + geom_point()

#rating levels higher on the weekend and decrease over the week

##########################################################
# Preparation of training and test split from edx partition
##########################################################

# Test set will be 10% of edx and train set will be the remining
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)

train_temp <- edx[-test_index,]
test_temp <- edx[test_index,]

# Making sure userId and movieId in test set are also in training set
test_edx <- test_temp %>% semi_join(train_temp, by = 'movieId') %>%
  semi_join(train_temp, by = 'userId')

removed <- anti_join(test_temp, test_edx)
train_edx <- rbind(train_temp, removed)

# We are left with two main sets train_edx set and test_edx

rm(train_temp, test_temp, removed)

##########################################################
# First set of different simple models
##########################################################

#MODEL 0 --> simply using the average rating as a prediction as base model

mu <- mean(train_edx$rating)

model_0_rmse <- RMSE(mu, test_edx$rating)
model_0_rmse
rmse_results <- data_frame(method = "Model 0: Just the average", RMSE = model_0_rmse)

rmse_results %>% knitr::kable()

#we are missing the rating by more than one star

#MODEL 1a --> factoring for the movie effect alone

movie_avgs <- train_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + test_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1a_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_1a_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 1a: Movie Effect alone Model",
                                     RMSE = model_1a_rmse ))

rmse_results %>% knitr::kable()

#results are below one star deviation from the correct rating

#MODEL 1b --> factoring for the user effect alone

user_avgs <- train_edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

user_avgs %>% qplot(b_u, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + test_edx %>% 
  left_join(user_avgs, by='userId') %>%
  .$b_u

model_1b_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_1b_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 1b: User Effect alone Model",
                                     RMSE = model_1b_rmse ))

rmse_results %>% knitr::kable()

#results are below one star difference but looks like movie effect improves better our model

#MODEL 1c --> factoring for the hour of rating effect

hour_avgs <- train_edx %>% 
  group_by(hour) %>% 
  summarize(b_h = mean(rating - mu))

hour_avgs %>% qplot(b_h, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + test_edx %>% 
  left_join(hour_avgs, by='hour') %>%
  .$b_h

model_1c_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_1c_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 1c: Hour Effect alone Model",
                                     RMSE = model_1c_rmse ))

rmse_results %>% knitr::kable()

#MODEL 1d --> factoring for the age of the movie at rating time effect

age_avgs <- train_edx %>% 
  group_by(movie_age) %>% 
  summarize(b_a = mean(rating - mu))

age_avgs %>% qplot(b_a, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + test_edx %>% 
  left_join(age_avgs, by='movie_age') %>%
  .$b_a

model_1d_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_1d_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 1d: Age Effect alone Model",
                                     RMSE = model_1d_rmse ))

rmse_results %>% knitr::kable()

#MODEL 1e --> factoring for the genre effect

genre_avgs <- train_edx %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu))

genre_avgs %>% qplot(b_g, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + test_edx %>% 
  left_join(genre_avgs, by='genres') %>%
  .$b_g

model_1e_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_1e_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 1e: Genres Effect alone Model",
                                     RMSE = model_1e_rmse ))

rmse_results %>% knitr::kable()

#genres effect performs better but far from movie effect, which makes sense because genre effect is contained in movie effect

#MODEL 2a --> factoring for the user effect on top of movie effect

#the user effect is calculated as the remining gap left after the mu and the movie effect

user_avgs <- train_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

user_avgs %>% qplot(b_u, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- test_edx %>% 
  left_join(user_avgs, by='userId') %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2a_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_2a_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 2a: User Effect on top of Movie Effect",
                                     RMSE = model_2a_rmse ))

rmse_results %>% knitr::kable()

#using user effect as the residual from the average minus the movie effect improves substantially the result

test_edx %>% mutate(error = rating - predicted_ratings) %>% 
  group_by(title) %>% summarize(n = n(), error = mean(error)) %>%
  top_n(10,error)

test_edx %>% mutate(error = rating - predicted_ratings) %>% 
  group_by(title) %>% summarize(n = n(), error = mean(error)) %>%
  top_n(-10,error)

#as a strategy to improve this model, we observe that the responsibles for the largest errors are movies with very few ratings

#MODEL 2b --> factoring for the movie effect on top of user effect (reverse order)

#the user effect is calculated as the remaining gap left after the mu and the user effect

user_avgs <- train_edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

movie_avgs <- train_edx %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu - b_u))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- test_edx %>% 
  left_join(user_avgs, by='userId') %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2b_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_2b_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 2b: Movie Effect on top of User Effect",
                                     RMSE = model_2b_rmse ))

rmse_results %>% knitr::kable()

#model 2a performs better using the movie effect as a basis then calculating user effect

#MODEL 2c --> factoring for the year, hour and day effect on top of movie and user effects

#the effects are added in order of performance of the standalone models

movie_avgs <- train_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

user_avgs <- train_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

genre_avgs <- train_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u))

age_avgs <- train_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  group_by(movie_age) %>% 
  summarize(b_a = mean(rating - mu - b_i - b_u - b_g))

hour_avgs <- train_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(age_avgs, by='movie_age') %>%
  group_by(hour) %>% 
  summarize(b_h = mean(rating - mu - b_i - b_u - b_g - b_a))

weekday_avgs <- train_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(age_avgs, by='movie_age') %>%
  left_join(hour_avgs, by='hour') %>%
  group_by(weekday) %>% 
  summarize(b_w = mean(rating - mu - b_i - b_u - b_g - b_a - b_h))

predicted_ratings <- test_edx %>% 
  left_join(user_avgs, by='userId') %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(age_avgs, by='movie_age') %>%
  left_join(hour_avgs, by='hour') %>%
  left_join(weekday_avgs, by='weekday') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_a + b_h + b_w) %>%
  .$pred

RMSE(predicted_ratings, test_edx$rating)

model_2c_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_2c_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 2c: Movie/User/Genre/Age/Hour/Day Effects",
                                     RMSE = model_2c_rmse ))

rmse_results %>% knitr::kable()

#the model marginally improves our previous best model with two effects

rmse_results %>% ggplot(aes(x=reorder(method,RMSE), y=RMSE)) + 
  geom_col() + coord_flip() + xlab("Method (by descending RMSE)")

#we will now build on our best models to improve via regularization techniques

##########################################################
# Applying regularization approach to previous models
##########################################################

test_edx %>% mutate(error = rating - predicted_ratings) %>% 
  group_by(title) %>% summarize(n = n(), error = mean(error)) %>%
  top_n(5,error)

test_edx %>% mutate(error = rating - predicted_ratings) %>% 
  group_by(title) %>% summarize(n = n(), error = mean(error)) %>%
  top_n(-5,error)

#as a strategy to improve last models, we observe that the responsibles for the largest errors are movies with very few reatings

#preparation of cross-validation data partitions to tune regularization parameter

nb_folds <- 5

cv_folds <- createFolds(train_edx$rating, k = nb_folds, returnTrain = TRUE)

#MODEL 3a --> applying regularization to movie effect

l_min <- 1
l_max <- 3
l_steps <- 0.25

rmse_matrix <- matrix(nrow=nb_folds,ncol=(l_max-l_min)/l_steps+1)
lambdas <- seq(l_min, l_max, l_steps)

for(k in 1:nb_folds) {
  train_temp <- train_edx[cv_folds[[k]],]
  test_temp <- train_edx[-cv_folds[[k]],]
  
  test_data <- test_temp %>% semi_join(train_temp, by = 'movieId') %>%
    semi_join(train_temp, by = 'userId')
  
  removed <- anti_join(test_temp, test_data)
  train_data <- rbind(train_temp, removed)
  
  mu <- mean(train_data$rating)
  
  just_the_sum <- train_data %>% group_by(movieId) %>%
    summarize(s = sum(rating - mu), n_i = n())
  
  rmse_matrix[k,] <- sapply(lambdas, function(l){
    predicted_ratings <- test_data %>% 
      left_join(just_the_sum, by='movieId') %>% 
      mutate(b_i = s/(n_i+l)) %>%
      mutate(pred = mu + b_i) %>%
      .$pred
    
  return(RMSE(predicted_ratings, test_data$rating))
  })
}

rmses <- colMeans(rmse_matrix)
lambda_best <- lambdas[which.min(rmses)]

qplot(lambdas, rmses)

lambda_best

#lambda that optimizes the model is 2.25

#build the model and test with edx test partition set

mu <- mean(train_edx$rating)

movie_avgs <- train_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_best), n_i = n())

predicted_ratings <- mu + test_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

movie_avgs_ori <- train_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

data_frame(original = movie_avgs_ori$b_i, 
           regularlized = movie_avgs$b_i, 
           n = movie_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5) + xlab("Original b_i") + ylab("Regularized b_i") + labs(size="n_i")

#the plot shows how the smaller the n_i's the more the b_i's shrink towards zero

model_3a_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_3a_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 3a: Movie Effect alone regularized",
                                     RMSE = model_3a_rmse ))

rmse_results[c(2,10),] %>% knitr::kable()

#initiating a small table to store lambda values

lambda_results <- data_frame(method = "Model 3a: Lambda_i", Lambda = lambda_best)

lambda_results %>% knitr::kable()

#the results improves slightly vs the non-regularizer movie effect model alone

#MODEL 3b --> applying regularization to both movie first then to user effect

l_min <- 4
l_max <- 6
l_steps <- 0.25

rmse_matrix <- matrix(nrow=nb_folds,ncol=(l_max-l_min)/l_steps+1)
lambdas <- seq(l_min, l_max, l_steps)

for(k in 1:nb_folds) {
  
  train_temp <- train_edx[cv_folds[[k]],]
  test_temp <- train_edx[-cv_folds[[k]],]
  
  test_data <- test_temp %>% semi_join(train_temp, by = 'movieId') %>%
    semi_join(train_temp, by = 'userId')
  
  removed <- anti_join(test_temp, test_data)
  train_data <- rbind(train_temp, removed)
  
  mu <- mean(train_data$rating)

  rmse_matrix[k,] <- sapply(lambdas, function(l){
    movie_avgs <- train_data %>% group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    user_avgs <- train_data %>% 
      left_join(movie_avgs, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - mu - b_i)/(n()+l))
    
    predicted_ratings <- test_data %>% 
      left_join(movie_avgs, by='movieId') %>% 
      left_join(user_avgs, by='userId') %>% 
      mutate(pred = mu + b_i + b_u) %>%
      .$pred
    
    return(RMSE(predicted_ratings, test_data$rating))
  })
  
}

rmses <- colMeans(rmse_matrix)

qplot(lambdas, rmses)

lambda_best <- lambdas[which.min(rmses)]
lambda_best

#lambda that optimizes the model is 4.75

#build the model and test with the edx test set

mu <- mean(train_edx$rating)

movie_avgs <- train_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_best))

user_avgs <- train_edx %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda_best))

predicted_ratings <- test_edx %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_3b_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_3b_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 3b: Movie then User Effect regularized",
                                     RMSE = model_3b_rmse ))

rmse_results %>% knitr::kable()

lambda_results <- bind_rows(lambda_results,
                            data_frame(method = "Model 3b: Lambda_i_u", Lambda = lambda_best))

lambda_results %>% knitr::kable()

#the model improves the non regularized model by 0.001

#warning: this code can take a long time to run

#MODEL 4 --> applying different lambdas to movie first then to user effects

l_min <- 4.0
l_max <- 6.0
l_steps <- 0.25

lambda_pairs <- gtools::permutations(length(seq(l_min, l_max, l_steps)), 2, seq(l_min, l_max, l_steps), repeats.allowed=TRUE)

rmse_matrix <- matrix(nrow=nb_folds,ncol=nrow(lambda_pairs))

index <- 1:nrow(lambda_pairs)

for(k in 1:nb_folds) {
  
  train_temp <- train_edx[cv_folds[[k]],]
  test_temp <- train_edx[-cv_folds[[k]],]
  
  test_data <- test_temp %>% semi_join(train_temp, by = 'movieId') %>%
    semi_join(train_temp, by = 'userId')
  
  removed <- anti_join(test_temp, test_data)
  train_data <- rbind(train_temp, removed)
  
  mu <- mean(train_data$rating)
  
  rmse_one_pair <- function(index, l){
    lambda_i <- l[index,1]
    lambda_u <- l[index,2]
    
    movie_avgs <- train_data %>% group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+lambda_i))
    
    user_avgs <- train_data %>% 
      left_join(movie_avgs, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_u))
    
    predicted_ratings <- test_data %>% 
      left_join(movie_avgs, by='movieId') %>% 
      left_join(user_avgs, by='userId') %>% 
      mutate(pred = mu + b_i + b_u) %>%
      .$pred
    
    return(RMSE(predicted_ratings, test_data$rating))
  }

  rmse_matrix[k,] <- sapply(index, rmse_one_pair, l = lambda_pairs)
  
} 

rmses <- colMeans(rmse_matrix)

lambda_best <- lambda_pairs[which.min(rmses),]
lambda_best

lambda_plot <- data.frame(rmse = rmses, lambda_i = lambda_pairs[,1],lambda_u = lambda_pairs[,2])
lambda_plot %>% ggplot(aes(x=lambda_i,y=lambda_u,fill=rmse, z=rmse))  + 
  geom_raster(vjust=0.1,hjust=0.1) + scale_fill_gradient(trans = "log", low = "black", high = "white") + geom_contour()

#lambdas that optimize the model are 4.25 for movies and 5.00 for users, very close to previous 4.75 figure

#build the model and test with edx test set

mu <- mean(train_edx$rating)

movie_avgs <- train_edx %>% group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda_best[1]))

user_avgs <- train_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_best[2]))

predicted_ratings <- test_edx %>% 
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_4_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_4_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 4: Different lambdas to Movie first then to User Effects",
                                     RMSE = model_4_rmse ))

rmse_results[10:12,] %>% knitr::kable()

lambda_results <- bind_rows(lambda_results,
                            data_frame(method = "Model 4: Lambda_i", Lambda = lambda_best[1]))
lambda_results <- bind_rows(lambda_results,
                            data_frame(method = "Model 4: Lambda_u", Lambda = lambda_best[2]))

lambda_results %>% knitr::kable()

#the model does not seem to improve the RMSE vs the single lambda test

#MODEL 5 --> applying regularization to all effects calculated before

l_min <- 4
l_max <- 6
l_steps <- 0.25

rmse_matrix <- matrix(nrow=nb_folds,ncol=(l_max-l_min)/l_steps+1)
lambdas <- seq(l_min, l_max, l_steps)

for(k in 1:nb_folds) {
  train_temp <- train_edx[cv_folds[[k]],]
  test_temp <- train_edx[-cv_folds[[k]],]
  
  test_data <- test_temp %>% semi_join(train_temp, by = 'movieId') %>%
    semi_join(train_temp, by = 'userId')
  
  removed <- anti_join(test_temp, test_data)
  train_data <- rbind(train_temp, removed)
  
  mu <- mean(train_data$rating)
  
  rmse_matrix[k,] <- sapply(lambdas, function(l){
    movie_avgs <- train_data %>% 
      group_by(movieId) %>% 
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    user_avgs <- train_data %>% 
      left_join(movie_avgs, by='movieId') %>%
      group_by(userId) %>% 
      summarize(b_u = sum(rating - mu - b_i)/(n()+l))
    
    genre_avgs <- train_data %>% 
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      group_by(genres) %>% 
      summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
    
    age_avgs <- train_data %>% 
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      left_join(genre_avgs, by='genres') %>%
      group_by(movie_age) %>% 
      summarize(b_a = sum(rating - mu - b_i - b_u - b_g)/(n()+l))
    
    hour_avgs <- train_data %>% 
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      left_join(genre_avgs, by='genres') %>%
      left_join(age_avgs, by='movie_age') %>%
      group_by(hour) %>% 
      summarize(b_h = sum(rating - mu - b_i - b_u - b_g - b_a)/(n()+l))
    
    weekday_avgs <- train_data %>% 
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      left_join(genre_avgs, by='genres') %>%
      left_join(age_avgs, by='movie_age') %>%
      left_join(hour_avgs, by='hour') %>%
      group_by(weekday) %>% 
      summarize(b_w = sum(rating - mu - b_i - b_u - b_g - b_a - b_h)/(n()+l))
    
    predicted_ratings <- test_data %>% 
      left_join(user_avgs, by='userId') %>%
      left_join(movie_avgs, by='movieId') %>%
      left_join(genre_avgs, by='genres') %>%
      left_join(age_avgs, by='movie_age') %>%
      left_join(hour_avgs, by='hour') %>%
      left_join(weekday_avgs, by='weekday') %>%
      mutate(pred = mu + b_i + b_u + b_g + b_a + b_h + b_w) %>%
      .$pred
    
    return(RMSE(predicted_ratings, test_data$rating))
  })
}

rmses <- colMeans(rmse_matrix)

lambda_best <- lambdas[which.min(rmses)]

qplot(lambdas, rmses)

#build the model and test with edx test set

mu <- mean(train_edx$rating)

movie_avgs <- train_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_best))

user_avgs <- train_edx %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda_best))

genre_avgs <- train_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>% 
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda_best))

age_avgs <- train_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  group_by(movie_age) %>% 
  summarize(b_a = sum(rating - mu - b_i - b_u - b_g)/(n()+lambda_best))

hour_avgs <- train_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(age_avgs, by='movie_age') %>%
  group_by(hour) %>% 
  summarize(b_h = sum(rating - mu - b_i - b_u - b_g - b_a)/(n()+lambda_best))

weekday_avgs <- train_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(age_avgs, by='movie_age') %>%
  left_join(hour_avgs, by='hour') %>%
  group_by(weekday) %>% 
  summarize(b_w = sum(rating - mu - b_i - b_u - b_g - b_a - b_h)/(n()+lambda_best))

predicted_ratings <- test_edx %>% 
  left_join(user_avgs, by='userId') %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(age_avgs, by='movie_age') %>%
  left_join(hour_avgs, by='hour') %>%
  left_join(weekday_avgs, by='weekday') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_a + b_h + b_w) %>%
  .$pred

model_5_rmse <- RMSE(predicted_ratings, test_edx$rating)
model_5_rmse
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 5: Movie/User/Genre/Age/Hour/Day Effects regularized",
                                     RMSE = model_5_rmse ))

rmse_results[10:13,] %>% knitr::kable()

lambda_results <- bind_rows(lambda_results,
                            data_frame(method = "Model 5: Lambda_all", Lambda = lambda_best))

lambda_results %>% knitr::kable()

rmse_results %>% ggplot(aes(x=reorder(method,RMSE), y=RMSE)) + 
  geom_col() + coord_flip() + xlab("Method (by descending RMSE)")

#model 5 technique will be our selection as it shows the best comparative performance
lambda_final <- lambda_best

##########################################################
# Aplying the selected model to the final validation test
##########################################################

#building of the final model with the edx set and final lambda

mu <- mean(edx$rating)

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_final))

user_avgs <- edx %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda_final))

genre_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>% 
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda_final))

age_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  group_by(movie_age) %>% 
  summarize(b_a = sum(rating - mu - b_i - b_u - b_g)/(n()+lambda_final))

hour_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(age_avgs, by='movie_age') %>%
  group_by(hour) %>% 
  summarize(b_h = sum(rating - mu - b_i - b_u - b_g - b_a)/(n()+lambda_final))

weekday_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(age_avgs, by='movie_age') %>%
  left_join(hour_avgs, by='hour') %>%
  group_by(weekday) %>% 
  summarize(b_w = sum(rating - mu - b_i - b_u - b_g - b_a - b_h)/(n()+lambda_final))

predicted_ratings <- validation %>% 
  left_join(user_avgs, by='userId') %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(age_avgs, by='movie_age') %>%
  left_join(hour_avgs, by='hour') %>%
  left_join(weekday_avgs, by='weekday') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_a + b_h + b_w) %>%
  .$pred

model_final_rmse <- RMSE(predicted_ratings, validation$rating)
model_final_rmse

rmse_results %>% ggplot(aes(x=reorder(method,RMSE), y=RMSE)) + 
  geom_col() + coord_flip() + xlab("Method (by descending RMSE)") +
  geom_hline(yintercept=model_final_rmse,linetype="dashed", color = "black", size = 1) +
  geom_text(aes(0,model_final_rmse,label = round(model_final_rmse,4),vjust = -1.5, hjust = -0.1))

##########################################################
# Matrix Factorization approach with recosystem
##########################################################

#adapt our train and test data to recosystem input format
train_data <-  with(train_edx, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_data  <-  with(test_edx,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

#create model object
r <-  recosystem::Reco()

#define tuning parameters and train the algorithm
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, niter = 10))

r$train(train_data, opts = c(opts$min, nthread = 4, niter = 20))

#calculate the predicted ratings 
predicted_ratings <-  r$predict(test_data, out_memory())

model_MF_rmse <- RMSE(predicted_ratings, test_edx$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Matrix Factorization with recosystem package",
                                     RMSE = model_MF_rmse ))

rmse_results %>% knitr::kable()

#after a lengthy processing, the RMSE is improving greatly with this technique

#building the model on the edx entire set

edx_data <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))
validation_data  <-  with(validation, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))

r <-  recosystem::Reco()

opts <-  r$tune(edx_data, opts = list(dim = c(10, 20, 30), 
                                      lrate = c(0.1, 0.2),
                                      costp_l2 = c(0.01, 0.1), 
                                      costq_l2 = c(0.01, 0.1),
                                      nthread  = 4, niter = 10))

r$train(edx_data, opts = c(opts$min, nthread = 4, niter = 20))

predicted_ratings <-  r$predict(validation_data, out_memory())

model_finalMF_rmse <- RMSE(predicted_ratings, validation$rating)
model_finalMF_rmse

rmse_results %>% ggplot(aes(x=reorder(method,RMSE), y=RMSE)) + 
  geom_col() + coord_flip() + xlab("Method (by descending RMSE)") +
  geom_hline(yintercept=model_finalMF_rmse,linetype="dotted", color = "black", size = 1) +  
  geom_text(aes(0,model_finalMF_rmse,label = round(model_finalMF_rmse,4),vjust = -1.5, hjust = -0.1))

##########################################################
# End of the full project code
##########################################################
