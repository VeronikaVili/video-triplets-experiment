# simulated sets
library(tidyverse)
library(imager)

source(here::here("R","utils_simulation.R"))

d <- readRDS(here::here("R", "data", "pilot.rds"))
vd <- d$vd %>% filter(video_group == "test")
n <- length(unique(vd$record))
vdr <- 
  vd %>% 
  mutate(selected = if_else(response == 0, left, right)) %>% 
  mutate(triplet = if_else(as.numeric(left) < as.numeric(right), 
                           str_c(orig, left, right, sep = "_"), 
                           str_c(orig, right, left, sep = "_")))
vdr <- vdr %>% filter(record != 8)
d <- vdr
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


d1 <- add_distances(d, set1)
d2 <- add_distances(d, set2)
d3 <- add_distances(d, set3)

results <- list(
  dim1_simulated = d1, dim1_values = set1 %>% mutate(sample = 1:n(), .before = everything()),
  dim2_simulated = d2, dim2_values = set2 %>% mutate(sample = 1:n(), .before = everything()),
  dim3_simulated = d3, dim3_values = set3 %>% mutate(sample = 1:n(), .before = everything())
)

writexl::write_xlsx(results, file.path(dir_sets, "simulated_sets.xlsx"))

d_full <- complete_responses(d)

d1 <- add_distances(d_full, set1)
d2 <- add_distances(d_full, set2)
d3 <- add_distances(d_full, set3)

results_full <- list(
  dim1_simulated = d1, dim1_values = set1 %>% mutate(sample = 1:n(), .before = everything()),
  dim2_simulated = d2, dim2_values = set2 %>% mutate(sample = 1:n(), .before = everything()),
  dim3_simulated = d3, dim3_values = set3 %>% mutate(sample = 1:n(), .before = everything())
)

writexl::write_xlsx(results_full, file.path(dir_sets, "simulated_sets_full.xlsx"))


# 80% of all data ---------------------------------------------------------

d_full <- complete_responses(d)
d80 <-  sample_frac(d_full, 0.8)

d1 <- add_distances(d80, set1)
d2 <- add_distances(d80, set2)
d3 <- add_distances(d80, set3)

results_full <- list(
  dim1_simulated = d1, dim1_values = set1 %>% mutate(sample = 1:n(), .before = everything()),
  dim2_simulated = d2, dim2_values = set2 %>% mutate(sample = 1:n(), .before = everything()),
  dim3_simulated = d3, dim3_values = set3 %>% mutate(sample = 1:n(), .before = everything())
)

writexl::write_xlsx(results_full, file.path(dir_sets, "simulated_sets_80.xlsx"))


# 60% of all data ---------------------------------------------------------
set.seed(60)
d_full <- complete_responses(d)
d60 <-  sample_frac(d_full, 0.6)

d1 <- add_distances(d60, set1)
d2 <- add_distances(d60, set2)
d3 <- add_distances(d60, set3)

results_full <- list(
  dim1_simulated = d1, dim1_values = set1 %>% mutate(sample = 1:n(), .before = everything()),
  dim2_simulated = d2, dim2_values = set2 %>% mutate(sample = 1:n(), .before = everything()),
  dim3_simulated = d3, dim3_values = set3 %>% mutate(sample = 1:n(), .before = everything())
)

writexl::write_xlsx(results_full, file.path(dir_sets, "simulated_sets_60.xlsx"))

# 40% of all data ---------------------------------------------------------
set.seed(40)

d_full <- complete_responses(d)
d40 <-  sample_frac(d_full, 0.4)

d1 <- add_distances(d40, set1)
d2 <- add_distances(d40, set2)
d3 <- add_distances(d40, set3)

results_full <- list(
  dim1_simulated = d1, dim1_values = set1 %>% mutate(sample = 1:n(), .before = everything()),
  dim2_simulated = d2, dim2_values = set2 %>% mutate(sample = 1:n(), .before = everything()),
  dim3_simulated = d3, dim3_values = set3 %>% mutate(sample = 1:n(), .before = everything())
)

writexl::write_xlsx(results_full, file.path(dir_sets, "simulated_sets_40.xlsx"))

# 20% of all data ---------------------------------------------------------
set.seed(20)
d_full <- complete_responses(d)
d20 <-  sample_frac(d_full, 0.2)

d1 <- add_distances(d20, set1)
d2 <- add_distances(d20, set2)
d3 <- add_distances(d20, set3)

results_full <- list(
  dim1_simulated = d1, dim1_values = set1 %>% mutate(sample = 1:n(), .before = everything()),
  dim2_simulated = d2, dim2_values = set2 %>% mutate(sample = 1:n(), .before = everything()),
  dim3_simulated = d3, dim3_values = set3 %>% mutate(sample = 1:n(), .before = everything())
)

writexl::write_xlsx(results_full, file.path(dir_sets, "simulated_sets_20.xlsx"))
