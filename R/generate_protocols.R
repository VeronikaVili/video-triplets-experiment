library(tidyverse)

set.seed(789)

videos <- read_table("R/data/triplets_sortedByReference.txt", col_names = F)
# wait till final version
videos <- videos[1:1218,]
videos <- videos %>% mutate(across(.fns = ~sprintf("%03d",.x)))

n_videos <- nrow(videos)
file_names <- 1:n_videos

videos_per_subject <- 87

n_repeats <- 3

n_protocols <- n_repeats*(n_videos / videos_per_subject)

protocols <- 
  videos %>% 
  slice(rep(1:n(), times = 3)) %>% 
  mutate(n_repeat = rep(1:n_repeats,each = n_videos))

protocols %>% 
  group_by(n_repeat) %>% 
  count()



protocols <- protocols %>% 
  group_by(n_repeat) %>% 
  sample_frac(1) %>% 
  ungroup() %>% 
  mutate(prot_id = rep(1:n_protocols, each = videos_per_subject))

protocols %>% 
  group_by(prot_id) %>% 
  count()

protocols %>% jsonlite::write_json("xx.json")
