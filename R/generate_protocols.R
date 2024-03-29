library(tidyverse)

set.seed(789)

videos <- read_table("R/data/triplets_sortedByReference.txt", col_names = F)
# wait till final version
stopifnot(nrow(videos) == 1218)

# format names
videos <- videos %>% 
  mutate(across(.fns = ~sprintf("%03d",.x))) %>% 
  mutate(video_triplet_id = 1:n())

n_videos <- nrow(videos)
file_names <- 1:n_videos

videos_per_subject <- 87

n_repeats <- 2

n_protocols <- n_repeats*(n_videos / videos_per_subject)

protocols <- 
  videos %>% 
  slice(rep(1:n(), times = 2)) %>% 
  mutate(n_repeat = rep(1:n_repeats,each = n_videos))

protocols_A <- protocols %>% 
  group_by(n_repeat) %>% 
  sample_frac(1) %>% 
  ungroup() %>% 
  mutate(prot_id = rep(1:n_protocols, each = videos_per_subject)) %>% 
  mutate(version = "A")

protocols_B <- protocols_A %>% 
  rename(tmp = X2) %>% 
  rename(X2 = X3) %>% 
  rename(X3 = tmp) %>% 
  mutate(version = "B")
protocols <- rbind(protocols_A,protocols_B)

# test

stimuli_list <- readxl::read_excel(here::here("R/data/pilot_wood_list.xlsx"))
protocols2 <- protocols %>% 
  mutate(name = sprintf("%s_%s_%s.mp4", X1, X2, X3))
swapped_names <- stimuli_list %>% filter(type == "swapped") %>% pull(name)
normal_names <- stimuli_list %>% filter(type != "swapped") %>% pull(name)

normal_names_protocols <- protocols2 %>% 
  filter(version == "A") %>% pull(name)
swapped_names_protocols <- protocols2 %>% 
  filter(version == "B") %>% pull(name)

intersect(normal_names, normal_names_protocols)
setdiff(normal_names, normal_names_protocols)
setdiff(normal_names_protocols, normal_names)

intersect(swapped_names, swapped_names_protocols)
setdiff(swapped_names, swapped_names_protocols)
setdiff(swapped_names_protocols, swapped_names)

# export protocols for js
p <- protocols %>% 
  mutate(
    name = sprintf("%s_%s_%s.mp4", X1, X2, X3),
    pid = str_c(str_pad(prot_id, 3, side = "left", pad = "0"), version)
  )
p_list <- p %>% group_split(pid)
p_names <- p_list %>% map_chr(~unique(.$pid))
p_text <- p_list %>% 
  map(~list(video = .$name)) %>% 
  set_names(p_names) %>% 
  jsonlite::toJSON()
final_json <- str_c("const protocols = ", p_text, ";\n")
readr::write_file(final_json, file = "protocols.js")

# some simple sanity checks

# all 87
protocols %>% 
  group_by(prot_id,version) %>% 
  count()

# all fours
protocols %>% 
  group_by(video_triplet_id) %>% 
  count()

# all twos
protocols %>% 
  group_by(video_triplet_id,version) %>% 
  count()

# same triplet never in one protocol twice
protocols %>% 
  group_by(video_triplet_id,prot_id,version) %>% 
  count() %>% pull(n) %>% all(.== 1)

# all 1218
protocols %>% 
  group_by(n_repeat,version) %>% 
  count()
