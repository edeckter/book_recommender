#This script does k-means clustering (with various k values) for the Goodreads data
library(tidyverse)
set.seed(123)

books <- read_csv('data/books_final.csv')

books_filtered <- books %>% select(book_id,title,first_author,average_rating,ratings_count=work_ratings_count,Art:`Young Adult`)

#Normalize data using min/max normalization
summary <- 
  data.frame(rbind(min=apply(books_filtered[,4:ncol(books_filtered)],2,min),
        max=apply(books_filtered[,4:ncol(books_filtered)],2,max),
        range=apply(books_filtered[,4:ncol(books_filtered)],2,max)-apply(books_filtered[,4:ncol(books_filtered)],2,min)))

for (i in 4:ncol(books_filtered)) {
  j <- i-3
  books_filtered[,i] <- (books_filtered[,i]-summary[1,j])/summary[3,j]
}

#Run k-means clustering algorithm
k_5 <- kmeans(books_filtered[,5:44],centers=5, nstart=5, iter.max=100)
k_10 <- kmeans(books_filtered[,5:44],centers=10, nstart=5, iter.max=100)
k_20 <- kmeans(books_filtered[,5:44],centers=20, nstart=5, iter.max=100)
k_30 <- kmeans(books_filtered[,5:44],centers=30, nstart=5, iter.max=100)
k_40 <- kmeans(books_filtered[,5:44],centers=40, nstart=5, iter.max=100)
k_50 <- kmeans(books_filtered[,5:44],centers=50, nstart=5, iter.max=100)
k_100 <- kmeans(books_filtered[,5:44],centers=100, nstart=5, iter.max=100)
k_150 <- kmeans(books_filtered[,5:44],centers=150, nstart=5, iter.max=100)
k_200 <- kmeans(books_filtered[,5:44],centers=200, nstart=5, iter.max=100)
k_300 <- kmeans(books_filtered[,5:44],centers=300, nstart=5, iter.max=100)

compare_kmeans <- data.frame(k=c(5,10,20,30,40,50,100,150,200,300),
                             `between_SS/total_SS`=c(k_5$betweenss/k_5$totss,
                                                     k_10$betweenss/k_10$totss,
                                                     k_20$betweenss/k_20$totss,
                                                     k_30$betweenss/k_30$totss,
                                                     k_40$betweenss/k_40$totss,
                                                     k_50$betweenss/k_50$totss,
                                                     k_100$betweenss/k_100$totss,
                                                     k_150$betweenss/k_150$totss,
                                                     k_200$betweenss/k_200$totss,
                                                     k_300$betweenss/k_300$totss))

#Plot BCV/WCV
ggplot(data=compare_kmeans,aes(x=k,y=`between_SS.total_SS`)) + 
  geom_point() +
  geom_line() +
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by=0.1),label=scales::percent) +
  scale_x_continuous(limits=c(0,300), breaks=seq(0,300,by=20)) +
  labs(title='Homogeneity of Clusters Increases as Number of Clusters Increases',
       subtitle='But there are diminishing returns as the number of clusters increases',
       x='Number of Clusters (k)',y='Between-Cluster Variation/Within-Cluster Variation') +
  theme(axis.ticks.y=element_blank(),
        panel.background=element_rect(fill='white'),
        panel.grid.major.y=element_line(color='gray'),
        legend.position='none')

#Examine clusters
books_filtered$k_5 <- k_5$cluster
books_filtered$k_10 <- k_10$cluster
books_filtered$k_20 <- k_20$cluster
books_filtered$k_30 <- k_30$cluster
books_filtered$k_40 <- k_40$cluster
books_filtered$k_50 <- k_50$cluster
books_filtered$k_100 <- k_100$cluster
books_filtered$k_150 <- k_150$cluster
books_filtered$k_200 <- k_200$cluster
books_filtered$k_300 <- k_300$cluster

#Centers
k10_centers <- data.frame(cbind(Cluster=row(k_10$centers)[,1],k_10$centers)) %>% gather(key='Variable',value='Center',-Cluster)

ggplot(k10_centers) +
  geom_bar(aes(x=Variable,y=Center),stat='identity') +
  coord_flip() +
  theme_bw() +
  facet_wrap(~Cluster, nrow=10)

k100_centers <- data.frame(cbind(Cluster=row(k_100$centers)[,1],k_100$centers)) %>% gather(key='Variable',value='Center',-Cluster)

k100_centers %>% filter(Cluster==27) %>%
ggplot() +
  geom_bar(aes(x=Variable,y=Center),stat='identity') +
  coord_flip() +
  theme_bw()

cluster_books <- books_filtered %>% filter(k_100==27) %>% top_n(20,ratings_count) %>% 
  mutate(Title=title,Author=first_author,`Average Rating`=average_rating*summary$average_rating[3],`Total User Ratings`=ratings_count*summary$ratings_count[3]) %>%
  select(Title,Author,`Average Rating`,`Total User Ratings`)

#Write list of top 20 books in cluster 27
write_csv(cluster_books,'data/cluster_books.csv')

#Write cluster assignments out to disk
write_csv(select(books_filtered,book_id,k_5:k_200),'data/clusters.csv')

