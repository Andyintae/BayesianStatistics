library(ggplot2)
library(dplyr)
library(statsr)
library(BAS)
library(GGally)

load("movies.Rdata")

head(movies)

names(movies)

head(movies)

levels(as.factor(movies$title_type))
levels(as.factor(movies$genre))
levels(as.factor(movies$mpaa_rating))
levels(as.factor(movies$thtr_rel_year))


movies_set <- movies %>% 
    filter(!is.na(runtime)) %>% 
    mutate(feature_film     = if_else(title_type == "Feature Film", "yes", "no"),
           drama            = if_else(genre == "Drama", "yes", "no"),
           mpaa_rating_R    = if_else(mpaa_rating == "R", "yes", "no"),
           oscar_season     = if_else(thtr_rel_month %in% c("10","11","12"), "yes", "no"),
           summer_season    = if_else(thtr_rel_month %in% c("5","6","7","8"), "yes", "no"),
           best_pic_nom     = if_else(best_pic_nom == "no", 0, 1),
           best_pic_win     = if_else(best_pic_win == "no", 0, 1),  
           best_actor_win   = if_else(best_actor_win == "no", 0, 1),
           best_actress_win = if_else(best_actress_win == "no", 0, 1),
           best_dir_win     = if_else(best_dir_win == "no", 0, 1),
           top200_box       = if_else(top200_box == "no", 0, 1),
           award_count      = best_pic_win + best_pic_win + best_actor_win + best_actress_win + best_dir_win + top200_box,
           thtr_rel_century = if_else(thtr_rel_year < 2000, "1900s", "2000s")
           ) %>% 
    select(audience_score, title, feature_film, drama, runtime, mpaa_rating_R, thtr_rel_year, thtr_rel_century,
           oscar_season, summer_season, imdb_rating, imdb_num_votes, critics_score, 
           best_pic_nom, best_pic_win, best_actor_win, best_actress_win, best_dir_win, top200_box, award_count)

glimpse(movies_set)

ggpairs(movies_set, columns = c("audience_score", "runtime", "imdb_rating", "imdb_num_votes", "critics_score", "award_count"))

ggpairs(movies_set, columns = c("audience_score", "feature_film", "drama", "mpaa_rating_R", "thtr_rel_year", "thtr_rel_century",
                                "oscar_season", "summer_season"))

summary(movies_set)

table(movies_set$feature_film)
movies_set %>% ggplot(aes(feature_film, audience_score)) +
  geom_boxplot()

table(movies_set$drama)
movies_set %>% ggplot(aes(drama, audience_score)) +
  geom_boxplot()

table(movies_set$mpaa_rating_R)
movies_set %>% ggplot(aes(mpaa_rating_R, audience_score)) +
  geom_boxplot()

table(movies_set$thtr_rel_year)
movies_set %>% ggplot(aes(as.factor(thtr_rel_year), audience_score)) +
  geom_boxplot()

table(movies_set$thtr_rel_century)
movies_set %>% ggplot(aes(as.factor(thtr_rel_century), audience_score)) +
  geom_boxplot()

table(movies_set$oscar_season)
movies_set %>% ggplot(aes(oscar_season, audience_score)) +
  geom_boxplot()

table(movies_set$summer_season)
movies_set %>% ggplot(aes(summer_season, audience_score)) +
  geom_boxplot()