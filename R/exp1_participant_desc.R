library(tidyverse)
library(here)

# we are selecting only participants with ids used in consistency.RmD
exp1 <- readRDS(here::here("R/data/pilot.rds"))
valid_session_id <- exp1$meta %>% filter(record!=8) %>% pull(SESSION_ID)

df <- read_csv(here("R/data/prolific_export_628360838a332d6bf6adaf27.csv"))

used_data <- df %>% 
  filter(`Submission id` %in% valid_session_id)

stopifnot(nrow(used_data) == 56)

used_data %>% 
  mutate(Age = as.numeric(Age)) %>% 
  summarize(m = mean(Age, na.rm = T),
            sd = sd(Age),
            n_males = sum(Sex == "Male"))
