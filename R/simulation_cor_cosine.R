

library(tidyverse)
library(here)
library(patchwork)
library(MASS)


# low correlation
set.seed(41)

sigma<-rbind(c(1,0.6), c(0.6,1))
xy <- mvrnorm(n=500, mu = c(10,22), Sigma = sigma)
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

px1 <- p1+p2 + plot_annotation(
  title = 'Correlated data - r=0.6, means = 10 and 20')


# correlation 0.8 ---------------------------------------------------------

set.seed(41)

sigma<-rbind(c(1,0.8), c(0.8,1))
xy <- mvrnorm(n=500, mu = c(1,2), Sigma = sigma)
x <- xy[,1]
y <- xy[,2]

x <- x-min(x)
y <- y-min(y)
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

px2 <- p1+p2 + plot_annotation(
  title = 'Correlated data - r=0.8, means = 1 and 2 translated to positive number')

# correlation 0.2 ---------------------------------------------------------

# low correlation
set.seed(41)

sigma<-rbind(c(1,0.2), c(0.2,1))
xy <- mvrnorm(n=500, mu = c(1,2), Sigma = sigma)
x <- xy[,1]
y <- xy[,2]

x <- x-min(x)
y <- y-min(y)
cor(x,y)

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

px3 <- p1+p2 + plot_annotation(
  title = 'Correlated data - r=0.2, means = 1 and 2 translated to positive number')



px1/px2/px3
