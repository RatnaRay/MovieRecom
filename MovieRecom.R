## Project: Movielens Recommender Project
## Author: Ratna Ray
## Date: 6/8/2019"
## GitHub Codebase: https://github.com/RatnaRay/Movie


# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")


###################################
# Create edx set and validation set
###################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
dl
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", 
                                  readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
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

#rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Summary of movies
str(movies)
# This has avour 10K movies along with the title and associated genre for each movie

summary(movies)
head(movies)

# Summary of ratings
summary(ratings)

str(ratings)
head(ratings)

# Summary of movielens
str(movielens)
# This has 10M ratings and has been joined with movies to build the movielens dataset

summary(movielens)
head(movielens)
# This has 10M ratings and has been joined with movies to build the movielens dataset

# Summary of edx

summary(edx)
# Summary of the edx dataset confirms no missing values

# First entries of the edx dataset
head(edx)

# Data Analysis of edx
str(edx)
# This consists of 90% of the movielens dataset ~9M

# Checking the edx dataset properties. 
n_distinct(edx$movieId)
n_distinct(edx$genres)
n_distinct(edx$userId)
nrow(edx)
# The total of unique movies and users in the edx subset is about 70,000 unique users 
# and about 10,700 different movies approximately

# Summary of the validation dataset
summary(validation)

# First entries of the validation dataset
head(validation)

# Checking the validation dataset properties
n_distinct(validation$movieId)
n_distinct(validation$genres)
n_distinct(validation$userId)
nrow(validation)

# Load libraries
if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", 
                                        repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", 
                                            repos = "http://cran.us.r-project.org")

# Split Genres into separate rows and save this into a separate dataset (if needed)
dat <- edx %>% separate_rows(genres, sep ="\\|")
head(dat)

# Count the number of movies using movieId in each genre
genre_count_by_movieId <- dat %>% group_by(genres) %>% summarize(n = n())
head(genre_count_by_movieId)
table(genre_count_by_movieId)

# Build a plot for this data
genreCountPlot <- ggplot(dat, aes(x = reorder(genres, genres, function(x) -length(x)))) + geom_bar()

# Fix the axes
genreCountPlot <- genreCountPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
genreCountPlot <- genreCountPlot + ylab("Number of movies") + xlab("Genre")
genreCountPlot <- genreCountPlot

# Plot by genres
print(genreCountPlot)

# Arrange the Genres better by movie ratings
GenreRatingPlot <- dat %>%   group_by(genres) %>%
  summarize(n=n()) %>%   ungroup() %>%
  mutate(sumN = sum(n), percentage = n/sumN) %>%  arrange(-percentage)

# Plot by genres
GenreRatingPlot %>%
  ggplot(aes(reorder(genres, percentage), percentage, fill= percentage)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "Spectral") + labs(y = "Percentage", x = "Genre") +
  ggtitle("Distribution of Genres by Percent Rated")

# Check ratings 
table(edx$rating)

summary(edx$rating)
# Ratings range from 0.5 to 5.0. 
# The difference in median and mean shows that the distribution is skewed towards higher ratings. 

# Rating Distribution plot
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "blue") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  xlab("Rating") +
  ylab("Number of ratings") +
  ggtitle("Rating Distribution")

# To check distribution of ratings by year, load library and add year field

# Get the year from the timestamp
edx <- mutate(edx, year = year(as_datetime(timestamp)))
head(edx)

# Convert to numeric
edx$year <- as.numeric(substr(as.character(edx$title),
                              nchar(as.character(edx$title))-4,
                              nchar(as.character(edx$title))-1))
colnames(edx)

# Work on extracting the premiere date from the movie titles

# Extract the premier date from the movie title and 
# add it as a column and store in edx_upd dataset
year_premier_edx <- stringi::stri_extract(edx$title, 
                                          regex = "(\\d{4})", 
                                          comments = TRUE ) %>% as.numeric()
edx_upd <- edx %>% mutate(premiered = year_premier_edx)
head(edx_upd)

# Checking for possible invalid cases in the edx_upd dataset

# edx
edx_chk1 <- edx_upd %>% filter(premiered > 2018) %>% group_by(movieId, title, premiered) %>% summarize(n = n())
edx_chk1

edx_chk2 <- edx_upd %>% filter(premiered < 1900) %>% group_by(movieId, title, premiered) %>% summarize(n = n())
edx_chk2

# Manually fix the incorrect dates in both datasets for possible use later on
edx_upd[edx_upd$movieId == "27266", "premiered"] <- 2004
edx_upd[edx_upd$movieId == "671", "premiered"] <- 1996
edx_upd[edx_upd$movieId == "2308", "premiered"] <- 1973
edx_upd[edx_upd$movieId == "4159", "premiered"] <- 2001
edx_upd[edx_upd$movieId == "5310", "premiered"] <- 1985
edx_upd[edx_upd$movieId == "8864", "premiered"] <- 2004
edx_upd[edx_upd$movieId == "1422", "premiered"] <- 1997
edx_upd[edx_upd$movieId == "4311", "premiered"] <- 1998
edx_upd[edx_upd$movieId == "5472", "premiered"] <- 1972
edx_upd[edx_upd$movieId == "6290", "premiered"] <- 2003
edx_upd[edx_upd$movieId == "6645", "premiered"] <- 1971
edx_upd[edx_upd$movieId == "8198", "premiered"] <- 1960
edx_upd[edx_upd$movieId == "8905", "premiered"] <- 1992
edx_upd[edx_upd$movieId == "53953", "premiered"] <- 2007

# Calculate and add age of the movie (in years) as column to the updated edx dataset
edx_upd <- edx_upd %>% mutate(age_of_movie = year(date(today())) - premiered)
head(edx_upd)

# Ratings by year plot
plot(table(edx$year),main="Ratings by year",xlab="Year",ylab="Number of ratings", 
     col = "blue")

# A dd the number of movies per user and number of users per movie as new columns to dataset
edx <- edx %>%
  group_by(userId) %>%
  mutate(movies_by_user = n());

edx <- edx %>%
  group_by(movieId) %>%
  mutate(users_by_movie = n()); 

head(edx)

# Distribution of movie ratings based on the updated edx dataset
edx %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "lightblue", color = "grey20", bins = 10) +
  scale_x_log10() + 
  ggtitle("Number of movies ratings")

# Distribution by users
edx %>% group_by(userId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "lightblue", color = "grey20", bins = 10) +
  scale_x_log10() +  
  ggtitle("Number of unique ratings")

# Check average distribution over time
avg_ratings <- edx %>% group_by(year) %>% summarise(avg_rating = mean(rating))
plot(avg_ratings, col = "red")

# To check what affects the movie ratings are affected by 

# Movie rating averages
movie_avg <- edx_upd %>% group_by(movieId) %>% summarize(avg_movie_rating = mean(rating))
user_avg <- edx_upd %>% group_by(userId) %>% summarize(avg_user_rating = mean(rating))
year_avg <- edx_upd%>% group_by(year) %>% summarize(avg_rating_by_year = mean(rating)) #year the movie was rated
age_avg <- edx_upd %>% group_by(age_of_movie) %>% summarize(avg_rating_by_age = mean(rating)) #age of movie

# View sample data and plot 
head(age_avg)
head(user_avg)
age_avg %>% ggplot(aes(age_of_movie, avg_rating_by_age)) +
  geom_point(color = "navy") +  ggtitle("Movie Rating by Age")

# Explore correlations between ratings, users, movieId age of movie and number 
# of ratings for each movie

# Number of movie ratings
movie_ratings <- edx_upd %>% group_by(movieId) %>% summarize(n = n())

# Average movie ating
avg_movie_rating <- edx_upd %>% group_by(movieId) %>% summarize(avg_m_r = mean(rating))

# Create correlation 
correlation <- edx_upd %>% select(rating, movieId, userId, year, age_of_movie, premiered) %>%
  left_join(movie_ratings, by = "movieId") %>%
  left_join(avg_movie_rating, by = 'movieId')
head(correlation)

# Plot the correlation data
plt <- correlation %>% select(one_of("rating", "movieId", "userId", "year", "age_of_movie", "premiered", "n", "avg_m_r")) %>% as.matrix()
graph <- cor(plt, use = "pairwise.complete.obs")

corrplot(graph, order = "hclust", addrect = 2, type = "lower",
         col = brewer.pal(n = 10, name = "PiYG"))


# Number of Ratings per Movie plot
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue", fill = "cadetblue") +
  scale_x_log10() +
  xlab("Number of Ratings") +
  ylab("Number of Movies") +
  ggtitle("Number of Ratings per Movie")

# Movies that have only one rating
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = count) %>%
  slice(1:20) %>%
  knitr::kable()

# Plot number of ratings given by users
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue", fill = "cadetblue") +
  scale_x_log10() +
  xlab("Number of Ratings") + 
  ylab("Number of Users") +
  ggtitle("Number of Ratings given by Users")

# Plot Mean Movie Ratings given by Users
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "blue",fill="cadetblue3") +
  xlab("Mean Rating") +
  ylab("Number of Users") +
  ggtitle("Mean Movie Ratings given by Users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5)))


### Modelling Approach ###

## 1. Average Movie Rating Model ##

# Compute the dataset's mean rating
mu <- mean(edx$rating)
mu

# Test results based on simple prediction
model_1_rmse <- RMSE(validation$rating, mu)
model_1_rmse

# Check results
# Save prediction to data frame
rmse_results <- data_frame(method = "Average movie rating model", RMSE = model_1_rmse)
rmse_results %>% knitr::kable()

## 2. Movie Effect Model ##

# Simple model taking into account the movie effect b_i
# Subtract the rating minus the mean for each rating the movie received
# Plot number of movies with the computed b_i
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("blue"),
                     ylab = "Number of Movies", main = "Number of Movies with Computed b_i")


# Test and save predicted rmse results to data frame
predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_2_rmse ))
# Check results
rmse_results %>% knitr::kable()

## 3. Movie and User Effect Model ##

# Plot penaly term user effect #
user_avgs<- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_avgs%>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("blue"))


user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


# Test and save predicted rmse results to data frame 
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and User Effect Model",  
                                     RMSE = model_3_rmse))

# Check result
rmse_results %>% knitr::kable()

## 4. Regularized Movie and User Effect Model ##

# Lambda is a tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(0, 10, 0.25)


# For each lambda,find b_i & b_u, followed by rating prediction & testing
# Below code might take some time  
model_4_rmse <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})


# Plot rmses vs lambdas to select the optimal lambda                                                             
qplot(lambdas, model_4_rmse)  


# Optimal lambda                                                             
lambda <- lambdas[which.min(model_4_rmse)]
lambda

# Test and save predicted rmse results to data frame                                                             
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie and User Effect Model",  
                                     RMSE = min(model_4_rmse)))

# Check result
rmse_results %>% knitr::kable()

#### Results ####                                                            
# RMSE Results Overview                                                          
rmse_results %>% knitr::kable()
