library(tidyverse)

library(jsonlite)
library(purrr)

fs <- list.files(here::here("R/data/triplets/EXP1"),full.names = T, recursive = T) %>% 
  as_tibble()

fs <- fs %>% filter(str_detect(value,".json")) %>% 
  mutate(params = str_remove(value,"D:/Documents/git/video-triplets-experiment/R/data/triplets/EXP1/4variants_")) %>% 
  separate(params,into = c("lrate",NA,"lambda",NA), sep = "_") %>% 
  mutate(lrate = str_replace(lrate,"lrate","0.") %>% as.numeric(),
         lambda = str_replace(lambda,"lmbda","0.") %>% as.numeric())

df <- fs %>% 
  rowwise() %>% 
  mutate(train_accuracy = jsonlite::read_json(value)$train_acc,
         val_accuracy = jsonlite::read_json(value)$val_acc,
         val_loss = jsonlite::read_json(value)$val_loss)

df
df %>% 
  filter(train_accuracy>0) %>% 
  ggplot(aes(x = lrate, y = val_accuracy)) + 
  geom_point() + 
  facet_grid(~lambda)

p <- df %>% 
  filter(train_accuracy>0) %>% 
  ggplot(aes(x = lambda, y = val_accuracy)) + 
  geom_point() + 
  facet_grid(~lrate)

plot_dir <- here::here("R/plots")

if(!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}

ggsave(file.path(plot_dir, "lambda_lrate.png"),p,width = 12)  


df %>% 
  filter(train_accuracy>0) %>% 
  ggplot(aes(x = lrate, y = lambda, size = val_accuracy,group = lrate)) + 
  geom_point()+
  geom_segment()
