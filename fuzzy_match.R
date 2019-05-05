#This script does fuzzy matching to assign standardized genres to books based on user-defined tags
library(fuzzywuzzyR)

#Join tag ids with tag names
book_tags_wnames <- inner_join(book_tags,tags)
#Aggregate tags
popular_tags <- book_tags_wnames %>% group_by(tag_id,tag_name) %>% summarize(tag_count=sum(count)) %>% arrange(desc(tag_count))
#Remove foreign language tags
popular_tags <- popular_tags %>% filter(!(tag_id %in% (33309:34244)))

#Initialization of FuzzMatcher class
fuzzy <- FuzzMatcher$new()          

#Define minimum certainty of match
match_threshold <- 85

#Loop to find closest matching tag names
match_tags <- popular_tags
match_tags$match <- NA
match_tags$max_score <- -1

#Match against main genre
for (i in 1:nrow(match_tags)) {
  print(i)
  for (j in 1:nrow(genres)) {
    score <- fuzzy$Token_sort_ratio(string1=match_tags$tag_name[i],string2=genres$Genre[j])
    if (score>match_threshold && score>match_tags$max_score[i]) {
      match_tags$match[i] <- genres$GenreID[j]
      match_tags$max_score[i] <- score
    }
  }
}

#Make table of matched tags to keep
keep_tags <- match_tags %>% filter(!is.na(match))

#Match on first alternate genre label
match_tags <- match_tags %>% filter(is.na(match))
alt1 <- genres %>% filter(!is.na(Alt1))
for (i in 1:nrow(match_tags)) {
  print(i)
  for (j in 1:nrow(alt1)) {
    score <- fuzzy$Token_sort_ratio(string1=match_tags$tag_name[i],string2=alt1$Alt1[j])
    if (score>match_threshold && score>match_tags$max_score[i]) {
      match_tags$match[i] <- alt1$GenreID[j]
      match_tags$max_score[i] <- score
    }
  }
}

#Make table of matched tags to keep
keep_tags <- rbind(keep_tags,match_tags %>% filter(!is.na(match)))

#Match on second alternate genre label
match_tags <- match_tags %>% filter(is.na(match))
alt2 <- genres %>% filter(!is.na(Alt2))
for (i in 1:nrow(match_tags)) {
  print(i)
  for (j in 1:nrow(alt2)) {
    score <- fuzzy$Token_sort_ratio(string1=match_tags$tag_name[i],string2=alt2$Alt2[j])
    if (score>match_threshold && score>match_tags$max_score[i]) {
      match_tags$match[i] <- alt2$GenreID[j]
      match_tags$max_score[i] <- score
    }
  }
}

#Make table of matched tags to keep
keep_tags <- rbind(keep_tags,match_tags %>% filter(!is.na(match)))

#Match final version of keep_tags back to genres table
keep_tags <- inner_join(keep_tags,genres,by=c('match'='GenreID'))

#Manual fixes based on visual inspection
keep_tags$match[keep_tags$tag_id==7436] <- 32
keep_tags$Genre[keep_tags$tag_id==7436] <- 'Science Fiction'
keep_tags$match[keep_tags$tag_id %in% c(21732,21799,26865,26866)] <- 31
keep_tags$Genre[keep_tags$tag_id %in% c(21732,21799,26865,26866)] <- 'Science'

#Write keep_tags table out to disk
write_csv(keep_tags,'data/keep_tags.csv')
