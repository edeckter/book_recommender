#This script cleans and explores the Goodreads 10k book data set
#Original data location: https://github.com/zygmuntz/goodbooks-10k
library(tidyverse)
library(stringr)

#Read in data
books <- read_csv('data/books.csv')
book_tags <- read_csv('data/book_tags.csv')
tags <- read_csv('data/tags.csv')
genres <- read_csv('data/genres.csv')

#Call fuzzy matching code
#source('fuzzy_match.R')

#Read table of tags to keep back in
keep_tags <- read_csv('data/keep_tags.csv')

#Match kept tags with books
book_genres <- inner_join(book_tags,keep_tags)
book_genres <- book_genres %>% group_by(goodreads_book_id,GenreID=match,Genre) %>% summarize(user_tag_count=sum(count)) %>% arrange(goodreads_book_id,desc(user_tag_count)) %>%
  group_by(goodreads_book_id) %>% top_n(3,user_tag_count)

#Check for books without at least one genre
left_join(books,book_genres) %>% filter(is.na(Genre))

#Join original book lists with new genres
books <- inner_join(books,book_genres) %>% arrange(book_id,GenreID)

books_final <- books %>% select(-GenreID) %>% spread(key='Genre',value=user_tag_count)

#Clean up author names
books_final$first_author <- str_match(books_final$authors,'(^[\\w|\\s|.]*),?')[,2]

#Set NAs to 0 for all genres
for (i in 24:ncol(books_final)) {
  books_final[is.na(books_final[,i]),i] <- 0
}

#Write final clean books table out to disk
#write_csv(books_final,'data/books_final.csv')

#Plot books by genre
books_by_genre <- enframe(apply(books_final[,24:(ncol(books_final)-1)],2,function(x) {sum(x!=0)}))

ggplot(books_by_genre) +
  geom_bar(aes(x=reorder(name,value),y=value),stat='identity',fill='steelblue') +
  coord_flip() +
  scale_y_continuous(limits=c(0,6500),breaks=seq(0,6000,by=1000),expand=c(0,0)) +
  labs(title='Goodread Books Breakdown by Genre',x='',y='Number of Books',
       subtitle='Each book was assigned to its top 3 genres based on user tags') +
  theme(axis.ticks.y=element_blank(),
        panel.background=element_rect(fill='white'),
        panel.grid.major.x=element_line(color='gray'),
        legend.position='none')

#Top 15 authors
books_final %>% group_by(first_author) %>% summarize(books=n()) %>% top_n(15,books) %>%
  ggplot() +
  geom_bar(aes(x=reorder(first_author,books),y=books),stat='identity',fill='steelblue') +
  coord_flip() +
  labs(title='Top 15 Authors by Book Count',x='',y='Number of Books') +
  theme(axis.ticks.y=element_blank(),
        panel.background=element_rect(fill='white'),
        panel.grid.major.x=element_line(color='gray'),
        legend.position='none')


#Most rated books
books_final %>% group_by(book_id) %>% summarize(ratings=sum(work_ratings_count)) %>% top_n(15,ratings) %>%
  inner_join(select(books_final,book_id,title,first_author)) %>% mutate(new_title=str_wrap(paste(title,first_author,sep=' by '),width=30)) %>%
  ggplot() +
  geom_bar(aes(x=reorder(new_title,ratings),y=ratings/1000000),stat='identity',fill='steelblue') +
  coord_flip() +
  scale_y_continuous(limits=c(0,5),breaks=seq(0,5,by=0.5),expand=c(0,0)) +
  labs(title='Top 15 Most Rated Books',x='',y='Number of Ratings (in millions)') +
  theme(axis.ticks.y=element_blank(),
        panel.background=element_rect(fill='white'),
        panel.grid.major.x=element_line(color='gray'),
        legend.position='none')

#Most 5 star ratings
books_final %>% filter(ratings_count>100000) %>% group_by(book_id) %>% summarize(ratings=sum(ratings_5/work_ratings_count)) %>% top_n(15,ratings) %>%
  inner_join(select(books_final,book_id,title,first_author)) %>% mutate(new_title=str_wrap(paste(title,first_author,sep=' by '),width=40)) %>%
  ggplot() +
  geom_bar(aes(x=reorder(new_title,ratings),y=ratings),stat='identity',fill='steelblue') +
  coord_flip() +
  scale_y_continuous(limits=c(0,0.85),breaks=seq(0,0.8,by=0.1),labels=scales::percent,expand=c(0,0)) +
  labs(title='Best Rated Books',subtitle=str_wrap('Books with the most 5-Star ratings as a proportion of total ratings (minimum 100,000 ratings)',width=50),
                                                  x='',y='5-Star Ratings as a Percent of Total Ratings') +
  theme(axis.ticks.y=element_blank(),
        panel.background=element_rect(fill='white'),
        panel.grid.major.x=element_line(color='gray'),
        legend.position='none')

#Most 1 star ratings
books_final %>% filter(ratings_count>100000) %>% group_by(book_id) %>% summarize(ratings=sum(ratings_1/work_ratings_count)) %>% top_n(15,ratings) %>%
  inner_join(select(books_final,book_id,title,first_author)) %>% mutate(new_title=str_wrap(paste(title,first_author,sep=' by '),width=40)) %>%
  ggplot() +
  geom_bar(aes(x=reorder(new_title,ratings),y=ratings),stat='identity',fill='steelblue') +
  coord_flip() +
  scale_y_continuous(limits=c(0,0.20),breaks=seq(0,0.15,by=0.05),labels=scales::percent,expand=c(0,0)) +
  labs(title='Worst Rated Books',subtitle=str_wrap('Books with the most 1-Star ratings as a proportion of total ratings (minimum 100,000 ratings)',width=50),
       x='',y='1-Star Ratings as a Percent of Total Ratings') +
  theme(axis.ticks.y=element_blank(),
        panel.background=element_rect(fill='white'),
        panel.grid.major.x=element_line(color='gray'),
        legend.position='none')
