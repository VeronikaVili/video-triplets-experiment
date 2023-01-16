library(tidyverse)

library(jsonlite)
library(purrr)

<<<<<<< HEAD
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

df_weights <- fs_npy %>% 
  rowwise() %>% 
  mutate(weights = list(np$load(value)))
df_weights_non_negtive <- df_weights %>% 
  filter(ncol(weights)>1)
df_weights_non_negtive <- df_weights_non_negtive %>% 
  mutate(weights = list(weights %>% as_tibble() %>% mutate(material_id = 1:30) %>% pivot_longer(cols = -material_id,names_to = "dimensions", values_to = "values"))) 
df_weights_non_negtive <- df_weights_non_negtive %>% 
  unnest(weights)

df_weights_non_negtive_clipped <- df_weights_non_negtive %>% 
  group_by(lrate,lambda,ix) %>% 
  mutate(m = mean(values), sd = sd(values)) %>% 
  summarize(value_2sd = if_else(values < m-2*sd, 0, value)) 

         
df_weights_non_negtive2 <- df_weights_non_negtive %>% 
  group_by(lrate,lambda,ix, dimensions) %>% 
  summarize(nonzero =sum(values < 0.01)) 

df_weights_non_negtive2 %>% 
  mutate(nonzero_p = nonzero/30) %>% 
  group_by(lrate,lambda,ix) %>% 
  summarize(nonzero_p_mean = mean(nonzero_p),ndim = n(),nonzero_ratio = nonzero/ndim) %>% 
  ggplot(aes(x = lrate, y = nonzero_ratio)) + 
  stat_summary(fun.data="mean_cl_boot") +
  facet_grid(~lambda)

df_weights_non_negtive2 %>% 
  mutate(nonzero_p = nonzero/30) %>% 
  group_by(lrate,lambda,ix) %>% 
  summarize(nonzero_p_mean = mean(nonzero_p),ndim = n(),nonzero_ratio = nonzero/ndim) %>% 
  ggplot(aes(x = lrate, y = nonzero_p_mean)) + 
  stat_summary(fun.data="mean_cl_boot") +
  facet_grid(~lambda)


cbind(round(df_weights$weights[[1]],3)[,1:5],round(df_weights$weights[[2]],3)[,1:5])

round(df_weights$weights[[1]]-df_weights$weights[[2]],3) %>% image()
legend()

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

# ep 2000 -----------------------------------------------------------------


fs <- list.files(here::here("R/data/triplets/EXP1_2000"),full.names = T, recursive = T) %>% 
  as_tibble()

fs_json_2000 <- fs %>% 
  filter(str_detect(value,".json")) %>% 
  filter(str_detect(value,"_ix")) %>% 
  mutate(params = str_remove(value,"C:/git/video-triplets-experiment/R/data/triplets/EXP1_2000/4variants_")) %>% 
  mutate(params = str_remove(params,"/results.json")) %>% 
  separate(params,into = c("lrate",NA,"lambda",NA,"ix"), sep = "_") %>% 
  mutate(lrate = str_replace(lrate,"lrate","0.") %>% as.numeric(),
         lambda = str_replace(lambda,"lmbda","0.") %>% as.numeric())


fs_npy_2000 <- fs %>% 
  filter(str_detect(value,".npy")) %>% 
  filter(str_detect(value,"_ix")) %>% 
  mutate(params = str_remove(value,"C:/git/video-triplets-experiment/R/data/triplets/EXP1_2000/4variants_")) %>% 
  mutate(params = str_remove(params,"/weights_sorted.npy")) %>% 
  separate(params,into = c("lrate",NA,"lambda",NA,"ix"), sep = "_") %>% 
  mutate(lrate = str_replace(lrate,"lrate","0.") %>% as.numeric(),
         lambda = str_replace(lambda,"lmbda","0.") %>% as.numeric())

df_2000 <- fs_json_2000 %>% 
  rowwise() %>% 
  mutate(train_accuracy = jsonlite::read_json(value)$train_acc,
         val_accuracy = jsonlite::read_json(value)$val_acc,
         val_loss = jsonlite::read_json(value)$val_loss)

library(reticulate)
np <- import("numpy")

df_dim_2000 <- fs_npy_2000 %>% 
  rowwise() %>% 
  mutate(ndim = ncol(np$load(value)))

df_weights_2000 <- fs_npy_2000 %>% 
  rowwise() %>% 
  mutate(weights = list(np$load(value)))

df_2000 <- df_2000 %>% 
  left_join(df_dim_2000 %>% select(-value), by = c("lrate", "lambda", "ix"))

df_mixed <- rbind(df_2000 %>% mutate(epochs = "2000"),
      df %>% mutate(epochs = "1200"))
  
df_2000%>% 
  filter(train_accuracy>0) %>% 
  ggplot(aes(x = lrate, y = ndim)) + 
  stat_summary(fun.data="mean_cl_boot") +
  facet_grid(~lambda)

