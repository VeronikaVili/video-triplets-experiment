library(tidyverse)

library(jsonlite)
library(purrr)
library(RcppCNPy)

fs <- list.files(here::here("R/data/triplets/EXP1"),full.names = T, recursive = T) %>% 
  as_tibble()

fs_json <- fs %>% 
  filter(str_detect(value,".json")) %>% 
  filter(str_detect(value,"_ix")) %>% 
  mutate(params = str_remove(value,"C:/git/video-triplets-experiment/R/data/triplets/EXP1/4variants_")) %>% 
  mutate(params = str_remove(params,"/results.json")) %>% 
  separate(params,into = c("lrate",NA,"lambda",NA,"ix"), sep = "_") %>% 
  mutate(lrate = str_replace(lrate,"lrate","0.") %>% as.numeric(),
         lambda = str_replace(lambda,"lmbda","0.") %>% as.numeric())


fs_npy <- fs %>% 
  filter(str_detect(value,".npy")) %>% 
  filter(str_detect(value,"_ix")) %>% 
  mutate(params = str_remove(value,"C:/git/video-triplets-experiment/R/data/triplets/EXP1/4variants_")) %>% 
  mutate(params = str_remove(params,"/weights_sorted.npy")) %>% 
  separate(params,into = c("lrate",NA,"lambda",NA,"ix"), sep = "_") %>% 
  mutate(lrate = str_replace(lrate,"lrate","0.") %>% as.numeric(),
         lambda = str_replace(lambda,"lmbda","0.") %>% as.numeric())

df <- fs_json %>% 
  rowwise() %>% 
  mutate(train_accuracy = jsonlite::read_json(value)$train_acc,
         val_accuracy = jsonlite::read_json(value)$val_acc,
         val_loss = jsonlite::read_json(value)$val_loss)

library(reticulate)
np <- import("numpy")

df_dim <- fs_npy %>% 
  rowwise() %>% 
  mutate(ndim = ncol(np$load(value)))

df <- df %>% 
  left_join(df_dim %>% select(-value), by = c("lrate", "lambda", "ix"))



df
df %>% 
  filter(train_accuracy>0) %>% 
  ggplot(aes(x = lrate, y = val_accuracy)) + 
  stat_summary(fun.data="mean_cl_boot") +
  facet_grid(~lambda)

df %>% 
  filter(train_accuracy>0) %>% 
  ggplot(aes(x = lrate, y = ndim)) + 
  stat_summary(fun.data="mean_cl_boot") +
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



# pca ---------------------------------------------------------------------

read_table()
