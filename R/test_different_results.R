library(tidyverse)

library(jsonlite)
library(purrr)

require("reticulate")

source_python(here::here("R","import_pickle.py"))

#

# VICE --------------------------------------------------------------------

fs <- list.files(here::here("R/data/triplets/VICE/EXP1"),full.names = T, recursive = T) %>% 
  as_tibble()

fs_json <- fs %>% 
  filter(str_detect(value,".json")) %>% 
  filter(str_detect(value,"_ix")) %>% 
  mutate(params = str_remove(value,"D:/Documents/git/video-triplets-experiment/R/data/triplets/VICE/EXP1/4variants_")) %>% 
  mutate(params = str_remove(params,"/results.json")) %>% 
  separate(params,into = c("mixture","eta","spike","slab","pi","ix",NA,NA,NA,NA,NA,NA,NA,NA), sep = "[_/]") %>% 
  mutate(eta = str_replace(eta,"eta","0.") %>% as.numeric(),
         spike = str_replace(spike,"spike","0.") %>% as.numeric(),
         slab = recode(slab,"slab5"="0.5","slab1"="1","slab2"="2") %>% as.numeric(),
         pi = str_replace(pi,"pi","0.") %>% as.numeric())



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

x <- np$load(here::here("R/data/triplets/VICE/EXP1/4variants_mixturegaussian_eta0005_spike125_slab1_pi4_ix2/30d/adam/gaussian/0.125/1.0/0.4/seed42/parameters.npz"))

df
df %>% 
  filter(train_accuracy>0) %>% 
  ggplot(aes(x = spike, y = val_accuracy)) + 
  stat_summary(fun.data="mean_cl_boot") +
  facet_grid(eta~spike)+
  ylim(0,1)

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
