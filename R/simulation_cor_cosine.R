

library(tidyverse)
library(here)

theme_set(theme_minimal(16))
raw <- read_table2(here("R/data/EXP1_rating_embedding_10dims_all_101122.txt"), col_names = F) %>% 
  select(-X31) %>% 
  mutate(dimension = rep(1:10, each = 45)) 
df <- raw %>% 
  pivot_longer(cols = -dimension, names_to = "material_id", values_to = "rating") %>% 
  arrange(material_id) %>% 
  mutate(rater_id = rep(1:45, times = 300)) %>% 
  group_by(rater_id) %>% 
  mutate(rating_scaled = (rating - mean(rating))/sd(rating) )

rating_names <- tibble(dimension = 1:10, dim_name = c("brightness","glossiness","colourfulness","directionality","complexity","contrast","roughness","patchiness/regularity","line elongation","spatial scale"))
df <- df %>% left_join(rating_names)


df_small <- df %>% 
  filter(rater_id < 5, dim_name == "brightness") %>% 
  ungroup()

m <- df_small %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(rater_id), names_from = material_id, values_from = rating_scaled) %>% 
  select(-rater_id) %>% as.matrix() 

m %>% t() %>% cor()
set.seed(223)

m_20perc <- 
  df_small 
m_20perc$rating_scaled[sample(1:120,96)] <- 0

m_20perc %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(rater_id), names_from = material_id, values_from = rating_scaled) %>% 
  select(-rater_id) %>% as.matrix() %>% 
  t() %>% cor()

set.seed(41)
library(mvtnorm)
sigma <- matrix(c(4,2,2,3), ncol=2)
xy <- rmvnorm(n=500, mean=c(1,2), sigma=sigma, method="chol")
x <- xy[,1]
y <- xy[,2]
n <- 500
cx <- NA
cos_s <- NA
ix <- 0:99

for (i in 0:99) {
  x_curr <- x
  y_curr <- y
  x_curr[sample(1:n,(n/100)*i)] <- 0
  y_curr[sample(1:n,(n/100)*i)] <- 0
  cx <- c(cx,cor(x_curr,y_curr))
  cos_s <- c(cos_s,lsa::cosine(x_curr,y_curr))
}
df1 <- tibble(percent = 0:99, pearson = cx[-1],cosine_sim = cos_s[-1])
p1 <- df1 %>% 
  ggplot(aes(x = percent, y = pearson)) + geom_point() +
  theme(aspect.ratio = 1)
p2 <- df1 %>% 
  ggplot(aes(x = percent, y = cosine_sim)) + geom_point() +
  theme(aspect.ratio = 1)
library(patchwork)
p1+p2

plot(0:99,cx[-1])
plot(0:99,cos_s[-1])
