#This is a prototype of the book recommender system
#Uses cluster 27 from the k=100 k-means cluster analysis
library(tidyverse)
library(stringr)
library(recommenderlab)
set.seed(123)

#Ratings without clustering
ratings <- read_csv('data/ratings.csv')
clusters <- read_csv('data/clusters.csv')

ratings_cluster <- inner_join(ratings,clusters) %>% filter(k_100==27) %>% select(user_id,book_id,rating)
ratings_matrix <- ratings_cluster %>% spread(key=book_id,value=rating)

#Assign training and test books
training_set_size <- floor(0.75*(ncol(ratings_matrix)-1))
train <- sample(seq_len(ncol(ratings_matrix)-1),size=training_set_size)
training_matrix <- ratings_matrix[,c(1,train)]
test_matrix <- ratings_matrix[,-train]

#Add books from test_matrix back to training matrix with all NAs
test_matrix_NAs <- test_matrix[,-1]
test_matrix_NAs[,] <- NA

training_matrix <- cbind(training_matrix,test_matrix_NAs)

#Remove rows of all NAs
training_matrix <- cbind(training_matrix,ratings=apply(training_matrix[,-1],1,function(x) {sum(x,na.rm=TRUE)})) %>%
  filter(ratings!=0) %>% select(-ratings)
test_matrix <- cbind(test_matrix,ratings=apply(test_matrix[,-1],1,function(x) {sum(x,na.rm=TRUE)})) %>%
  filter(ratings!=0) %>% select(-ratings)

training_matrix <- as.data.frame(training_matrix) %>% column_to_rownames(var='user_id')
training_matrix <- as(as.matrix(training_matrix),'realRatingMatrix')

cluster27 <- Recommender(training_matrix, method='UBCF', parameter=list(method='cosine'))
cluster27_recommendations <- predict(cluster27,newdata=training_matrix,n=100)

test_predictions_all <- as(cluster27_recommendations,'matrix') %>% as.data.frame() %>% rownames_to_column() %>% 
  rename(user_id=rowname) %>% mutate(user_id=as.integer(user_id))
test_predictions <- test_predictions_all[,c(1,58:76)]

#Calculate SSE for predictions
fit <- test_matrix[1,-1]
fit[,] <- 0
fit <- data.frame(t(fit)) %>% rownames_to_column(var='book_id') %>% rename(SSE=X1)

for (i in 1:(ncol(test_matrix)-1)) {
  actual_v_predict <- inner_join(test_matrix[,c(1,i+1)],test_predictions[,c(1,i+1)],by=c('user_id'='user_id'))
  fit$SSE[i] <- sum((actual_v_predict[,2]-actual_v_predict[,3])^2,na.rm=TRUE)
  rm(actual_v_predict)
}

#Plot predicted vs. actual for representative books
actual_v_predict <- inner_join(test_matrix[,c(1,2)],test_predictions[,c(1,2)],by=c('user_id'='user_id')) %>%
  filter(!is.na(`2549.x`))
ggplot() + 
  geom_boxplot(data=actual_v_predict,aes(x=as.factor(`2549.x`),y=`2549.y`)) +
  labs(title=str_wrap('Rating Prediction Accuracy for Book 2549 Blue Moon (Anita Blake, Vampire Hunter, #8) by Laurell K. Hamilton',width=70),
       x='Actual User Rating', y='Predicted User Rating') +
  theme(axis.ticks.y=element_blank(),
        panel.background=element_rect(fill='white'),
        panel.grid.major.y=element_line(color='gray'),
        legend.position='none')

actual_v_predict <- inner_join(test_matrix[,c(1,11)],test_predictions[,c(1,11)],by=c('user_id'='user_id')) %>%
  filter(!is.na(`3445.x`))
ggplot() + 
  geom_boxplot(data=actual_v_predict,aes(x=as.factor(`3445.x`),y=`3445.y`)) +
  labs(title=str_wrap('Rating Prediction Accuracy for Book 3445 Succubus Blues (Georgina Kincaid, #1) by Richelle Mead',width=70),
       x='Actual User Rating', y='Predicted User Rating') +
  theme(axis.ticks.y=element_blank(),
        panel.background=element_rect(fill='white'),
        panel.grid.major.y=element_line(color='gray'),
        legend.position='none')

actual_v_predict <- inner_join(test_matrix[,c(1,20)],test_predictions[,c(1,20)],by=c('user_id'='user_id')) %>%
  filter(!is.na(`9593.x`))
ggplot() + 
  geom_boxplot(data=actual_v_predict,aes(x=as.factor(`9593.x`),y=`9593.y`)) +
  labs(title=str_wrap('Rating Prediction Accuracy for Book 9593 The Rapture of Canaan by Sheri Reynolds',width=70),
       x='Actual User Rating', y='Predicted User Rating') +
  theme(axis.ticks.y=element_blank(),
        panel.background=element_rect(fill='white'),
        panel.grid.major.y=element_line(color='gray'),
        legend.position='none')
