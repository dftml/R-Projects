facebook = read.table("facebook_user.tsv", header = TRUE)

# 1. Which day has the highest number of birthdays ? 

facebook$D.O.B = as.Date(with(facebook, paste(dob_year,dob_month,dob_day, sep = "-")),"%Y-%m-%d")
  
facebook %>% group_by(D.O.B) %>% 
  summarise(Count = n()) %>% filter(Count == max(Count) )

# 2. Who tends to have more friends by average / by median ? Males or Females ? 

facebook %>% group_by(gender) %>% 
  summarise(Average = mean(friend_count), Median = median(friend_count))

# 3. Which set of users have been on facebook for a long time ? 

facebook %>% arrange(desc(tenure))  %>% 
  select(userid,tenure) %>% 
  head(5)

# 4. Who is most active on facebook ? Who is most active on mobile facebook ? on web facebook ? 

facebook %>% filter(likes == max(likes)) %>% 
  select(userid,likes)

facebook %>% filter(mobile_likes == max(mobile_likes)) %>% 
  select(userid,mobile_likes)

facebook %>% filter(www_likes == max(www_likes)) %>% 
  select(userid,www_likes)

# Who is using facebook more on mobile than desktop ?

facebook %>% group_by(userid) %>% 
  filter(mobile_likes > www_likes) %>% 
  select(userid,mobile_likes,www_likes)

# if only one user

facebook %>% filter(mobile_likes>www_likes) %>% 
  filter(mobile_likes == max(mobile_likes)) %>% 
  select(userid,mobile_likes,www_likes)