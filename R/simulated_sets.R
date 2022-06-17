# simulated sets
library(tidyverse)
library(imager)

d <- readRDS(here::here("R", "data", "pilot_56.rds"))

# random data ------------------------------------------------
n <- 30
set.seed(42)
set1 <- tibble(lum = runif(n, min = 0, max = 1))
set2 <- tibble(h = runif(n, min = 0, max = 1), 
               v = runif(n, min = 0, max = 1))
set3 <- tibble(r = runif(n, min = 0, max = 1), 
               g = runif(n, min = 0, max = 1),
               b = runif(n, min = 0, max = 1))

# demo images ------------------------------------------------
w <- 128
h <- 128

dir_sets <- here::here("R", "data", "simulated_sets")

if (!dir.exists(dir_sets)) dir.create(dir_sets)
if (!dir.exists(file.path(dir_sets, "dim1"))) 
  dir.create(file.path(dir_sets, "dim1"))
if (!dir.exists(file.path(dir_sets, "dim2"))) 
  dir.create(file.path(dir_sets, "dim2"))
if (!dir.exists(file.path(dir_sets, "dim3"))) 
  dir.create(file.path(dir_sets, "dim3"))

# dim1 
for (i in 1:n) {
  a <- imager::imfill(w, h, 1, val = c(set1$lum[i], set1$lum[i], set1$lum[i]))
  imager::save.image(a, file.path(dir_sets, "dim1", sprintf("%03d.png", i)))
}
# dim3
for (i in 1:n) {
  a <- imager::imfill(w, h, 1, val = c(set3$r[i], set3$g[i], set3$b[i]))
  imager::save.image(a, file.path(dir_sets, "dim3", sprintf("%03d.png", i)))
}
# dim2
patch <- function(nhor, nver, width = 128, height = 128, bg = 0.5) {
  a <- imager::imfill(width, height, 1, val = c(bg, bg, bg))
  nhor <- round(nhor)
  nver <- round(nver)
  range_hor <- round((1:nhor) * height / (nhor + 1))
  range_ver <- round((1:nver) * width / (nver + 1))
  p <- 
    tibble(type = c(rep("hor", nhor), rep("ver", nver)),
           x = c(range_hor, range_ver)) %>% 
    slice_sample(prop = 1)
  for (i in 1:nrow(p)) {
    if (p$type[i] == "hor") {
      a[Yc(a) == p$x[i]] <- 1
    } else {
      a[Xc(a) == p$x[i]] <- 0
    }
  }
  a
}
for (i in 1:n) {
  a <- patch(set2$h[i] * 16 + 2, set2$v[i] * 16 + 2)
  imager::save.image(a, file.path(dir_sets, "dim2", sprintf("%03d.png", i)))
}
# simulate responses ------------------------------------
L2_distance <- function(i1, i2, set, ...) {
  x1 <- as.numeric(set[i1, ])
  x2 <- as.numeric(set[i2, ])
  m <- rbind(
    matrix(x1, nrow = 1), 
    matrix(x2, nrow = 1)
  )
  as.numeric(dist(m, ...))
}

add_distances <- function(dd, set) {
  dx <- 
    dd %>% 
    select(-response, -selected, -rt) %>% 
    mutate(L2_left = NA_real_, L2_right = NA_real_, 
           response = NA_integer_, selected = NA_character_)
  for (i in 1:nrow(dx)) {
    dx$L2_left[i] <- L2_distance(
      as.numeric(dx$orig[i]), as.numeric(dx$left[i]), set)
    dx$L2_right[i] <- L2_distance(
      as.numeric(dx$orig[i]), as.numeric(dx$right[i]), set)
    dx$response[i] <- ifelse(dx$L2_left[i] < dx$L2_right[i], 0, 1)
    dx$selected[i] <- ifelse(dx$L2_left[i] < dx$L2_right[i], dx$left[i], dx$right[i])
  }
  dx
} 

d1 <- add_distances(d, set1)
d2 <- add_distances(d, set2)
d3 <- add_distances(d, set3)

results <- list(
  dim1_simulated = d1, dim1_values = set1 %>% mutate(sample = 1:n(), .before = everything()),
  dim2_simulated = d2, dim2_values = set2 %>% mutate(sample = 1:n(), .before = everything()),
  dim3_simulated = d3, dim3_values = set3 %>% mutate(sample = 1:n(), .before = everything())
)

writexl::write_xlsx(results, file.path(dir_sets, "simulated_sets.xlsx"))

