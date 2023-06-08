# This is the data analysis of Cylistic
library(readr)
cyclistic <- read_csv("Desktop/Case Study 1/202303-cyclistic.csv")

# This is to check each colmn for NULLs and count the number of NULLs
null_check <- c(1:ncol(cyclistic))
  for(i in 1:ncol(cyclistic)){
    null_check[i] <- sum(is.na(cyclistic[,i]))}
null_check <- data.frame(colnames(cyclistic), null_check)

# Determine if there are any duplicate bike ID, how many bikes are available, and ensure there are no typos
#  in bike_id, bike type, and member_casual

num_distinct <- c(1:ncol(cyclistic))
  for(i in 1:ncol(cyclistic) ){
    num_distinct[i] <- count(distinct(cyclistic[i]))}
  num_distinct <- rbind(colnames(cyclistic), num_distinct)

# Next, we need to determine the rental time of each bike_id
cyclistic <- cyclistic %>% mutate(time_used=as.numeric(difftime(cyclistic$ended_at, cyclistic$started_at, units = "secs")))

# Find the mean for time used for each user type
cyclistic %>% filter(member_casual == "casual") %>% summarize(casual_mean_time_min = (mean(time_used)/60),
                                                              casual_max_time_min = (max(time_used)/60),
                                                              casual_meam_time_hr = (max(time_used)/3600))
#   mean and max rental for members
cyclistic %>% filter(member_casual == "member") %>% summarize(casual_mean_time_min = (mean(time_used)/60),
                                                              casual_max_time_min = (max(time_used)/60),
                                                              casual_meam_time_hr = (max(time_used)/3600))

# Find the most popular bike type
#bike_name <- c("electric_bike", "classic_bike", "docked_bike")
#bike_count <- c(sum(cyclistic$rideable_type == 'electric_bike'),
#                sum(cyclistic$rideable_type == 'classic_bike'),
#                sum(cyclistic$rideable_type == 'docked_bike'))
#bike_type_df <- data.frame(bike_name, bike_count)
# Using the new df, we can add the avg and max time use for each bike
cyclistic %>% filter(rideable_type == "electric_bike" & member_casual == "casual") %>% summarize(casual_mean_time_min = (mean(time_used)/60),
                                                              casual_max_time_min = (max(time_used)/60),
                                                              casual_meam_time_hr = (max(time_used)/3600))

cyclistic %>% filter(rideable_type == "classic_bike" & member_casual == "casual") %>% summarize(casual_mean_time_min = (mean(time_used)/60),
                                                                casual_max_time_min = (max(time_used)/60),
                                                                casual_meam_time_hr = (max(time_used)/3600))

cyclistic %>% filter(rideable_type == "docked_bike" & member_casual == "casual") %>% summarize(casual_mean_time_min = (mean(time_used)/60),
                                                                     casual_max_time_min = (max(time_used)/60),
                                                                     casual_meam_time_hr = (max(time_used)/3600))
