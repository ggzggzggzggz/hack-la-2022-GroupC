# install.packages("tidyverse")
library("tidyverse")

# import datasets
navigation_events <- read_csv("data/navigation_events.csv")
# additional data
assignments <- read.csv("~/Documents/hack-la-2022-GroupC/data/additional/assignments.csv")
discussion_topics <- read_csv("data/additional/discussion_topics.csv")
discussions <- read_csv("data/additional/discussions.csv")  
enrollments <- read_csv("data/additional/enrollments.csv")
files <- read_csv("data/additional/files.csv")
gradebook <- read_csv("data/additional/gradebook.csv")
module_items <- read_csv("data/additional/module_items.csv")
pages <- read_csv("data/additional/pages.csv")

# data cleaning
discussions <- discussions %>% 
  rename(user_id = actor_id)
discussion_topics <- discussion_topics %>% 
  rename(discussion_topic_title = title)
gradebook <- gradebook[-c(1,2), ]

# add only date and count
discussions_date <- mutate(discussions, date = as.Date(timestamp), count = 1) |>
  select(date,count)
discussions_date <- aggregate(discussions_date$count, by=list(date=discussions_date$date),length)

#count users likes
discussions_users_likes <- aggregate(discussions$count_of_likes, by=list(user_id=discussions$user_id),sum) |>
  arrange(user_id)

#count student grade
gradebook_each_student <- select(gradebook, Student, `Current Score`) |>
  arrange(Student)

# count per user post
discussions_users_post <- mutate(discussions, count = 1) |>
  select(user_id,count)
discussions_users_post <- aggregate(discussions_users_post$count, by=list(user_id=discussions_users_post$user_id),length)

#count users words
discussions_users_words <- aggregate(discussions$post_message_length, by=list(user_id=discussions$user_id),sum) |>
  arrange(user_id)

#count replies
discussions_replies <- mutate(discussions, count = 1) |>
  select(post_parent_id,count)
discussions_replies <- aggregate(discussions_replies$count, by=list(post_id=discussions_replies$post_parent_id),length)
discussions_user_connect_post <- select(discussions, user_id, post_id)
discussions_replies_merge <- merge(x = discussions_replies,y = discussions_user_connect_post,by = "post_id")
discussions_replies_merge <- aggregate(discussions_replies_merge$x, by=list(user_id=discussions_replies_merge$user_id),sum)


# discussions_merge <- merge(x = mutate(discussions_users_post, posts = x),
  #                          y = merge(x = mutate(discussions_users_words, words = x),
  #                                    y = mutate(discussions_users_likes, likes = x),
  #                                    by = "user_id"),by = "user_id") |>
  # select(user_id,posts,likes,words)

gradebook_each_student <- gradebook |> select(Student, `Current Score`)
discussions_merge <- merge(x = mutate(discussions_users_post, posts = x),
             y = merge(x = mutate(discussions_users_words, words = x),
                       y = merge(x = mutate(discussions_users_likes,likes = x),
                                 y = mutate(gradebook_each_student,user_id = Student),by = "user_id"),by = "user_id"),
             by = "user_id") |>
  select(user_id,posts,likes,words,`Current Score`)

# add only date and count
discussions_date <- mutate(discussions, date = as.Date(timestamp), count = 1)
# discussions_date_user <- discussions_date
discussions_date <- aggregate(discussions_date$count, by=list(date=discussions_date$date),length) |>
  rename(post = x)

# add only date users and count
discussions_date_user <- mutate(discussions, date = as.Date(timestamp)) |>
  unite("date_user", date, user_id, remove = FALSE) |>
  select(date_user,date) |>
  distinct(date_user, .keep_all = TRUE) |>
  mutate(count = 1)
discussions_date_user <- aggregate(discussions_date_user$count, by=list(date=discussions_date_user$date),length) |>
  rename(users = x) 
#merge
discusstions_date_merge <- merge(x = discussions_date_user,y = discussions_date,by = "date")

#count # of received replies
discusstion_topic_hot <- mutate(discussions, count = 1)
discusstion_topic_hot <- aggregate(discusstion_topic_hot$count, by=list(topic=discusstion_topic_hot$discussion_topic_title),sum)
myLabel = as.vector(discusstion_topic_hot$topic)