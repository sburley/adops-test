library(tidyverse)

data <- read_csv('./adops_data.csv')

# Consider only the rows with country_id = "BDV" (there are 844 such rows). For
# each site_id, we can compute the number of unique user_id's found in these 844
# rows. Which site_id has the largest number of unique users? And what's the
# number?

data %>% 
  filter(country_id == "BDV") %>% 
  distinct(site_id, user_id) %>%
  group_by(site_id) %>% 
  summarize(user_count = n()) %>% 
  filter(user_count == max(user_count))

# Between 2019-02-03 00:00:00 and 2019-02-04 23:59:59, there are four users who
# visited a certain site more than 10 times. Find these four users & which sites
# they (each) visited more than 10 times. (Simply provides four triples in the
# form (user_id, site_id, number of visits) in the box below.)

data %>% 
  filter(ts >= "2019-02-03 00:00:00" & ts <= "2019-02-04 23:59:59") %>%
  group_by(user_id, site_id) %>%
  summarize(visit_count = n()) %>%
  filter(visit_count > 10)

# For each site, compute the unique number of users whose last visit (found in
# the original data set) was to that site. For instance, user "LC3561"'s last
# visit is to "N0OTG" based on timestamp data. Based on this measure, what are
# top three sites? (hint: site "3POLC" is ranked at 5th with 28 users whose last
# visit in the data set was to 3POLC; simply provide three pairs in the form
# (site_id, number of users).)

data %>%
  group_by(user_id) %>%
  filter(ts == max(ts)) %>%
  group_by(site_id) %>%
  summarize(user_count = n()) %>%
  arrange(desc(user_count)) %>%
  top_n(3)

# For each user, determine the first site he/she visited and the last site
# he/she visited based on the timestamp data. Compute the number of users whose
# first/last visits are to the same website. What is the number?

data %>%
  group_by(user_id) %>%
  filter(ts == min(ts)) %>%  # first visit
  left_join(
    data %>%
      group_by(user_id) %>%
      filter(ts == max(ts)),  # last visit
    by = "user_id",
    suffix = c(".first", ".last")
  ) %>%
  filter(site_id.first == site_id.last) %>%
  nrow