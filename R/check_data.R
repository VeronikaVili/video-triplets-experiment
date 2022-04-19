library(tidyverse)
library(jsonlite)

p <- jsonlite::read_json("protocol.json",simplifyVector = T)  %>% as_tibble()

df_raw <- jsonlite::read_json("R/data/data2.json",simplifyVector = T) %>% as_tibble()

df <- df_raw %>% 
  unnest(cols = c(stimulus)) %>% 
  filter(trial_type == "video-button-response") %>% 
  separate(stimulus, sep = "/", into = c("dir","stimuli_type","video_id")) %>% 
  filter(stimuli_type!="practice")

df %>% 
  group_by(trial_type,video_id) %>% 
  summarize(n = n()) %>% arrange(-n) %>% 
  head(20)
df %>% filter(stimuli_type=="test") %>% unnest(cols = c(response)) %>% 
  group_by(response) %>% 
  summarize(n = n())
p1 <- p %>% 
  group_by(prot_id) %>% 
  mutate(video_id = paste0(X1,"_",X2,"_",X3)) %>% 
  filter(prot_id == 1) %>% 
  arrange(video_id,.by_group = T) %>% as.data.frame() %>% pull(video_id)

p %>% 
  group_by(prot_id) %>% 
  slice(1)

p2 <- df %>% 
  filter(stimuli_type =="test") %>% arrange(video_id) %>% pull(video_id) %>% str_remove(".mp4")

setdiff(p1,p2)
setdiff(p2,p1)

test_id <- df %>% 
  filter(trial_type == "test") %>% pull(video_id)

catch_id <- df %>% 
  filter(trial_type == "catch") %>% pull(video_id)


practice_id <- df_raw %>% 
  unnest(cols = c(stimulus)) %>% 
  separate(stimulus, sep = "/", into = c("dir","trial_type","video_id")) %>% 
  filter(trial_type == "practice") %>% pull(video_id)

intersect(practice_id, test_id)
intersect(catch_id, test_id)
intersect(catch_id, practice_id)
