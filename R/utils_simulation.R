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
    mutate(
      L2_left = NA_real_, L2_right = NA_real_,
      response = NA_integer_, selected = NA_character_
    )
  for (i in 1:nrow(dx)) {
    dx$L2_left[i] <- L2_distance(
      as.numeric(dx$orig[i]), as.numeric(dx$left[i]), set
    )
    dx$L2_right[i] <- L2_distance(
      as.numeric(dx$orig[i]), as.numeric(dx$right[i]), set
    )
    dx$response[i] <- ifelse(dx$L2_left[i] < dx$L2_right[i], 0, 1)
    dx$selected[i] <- ifelse(dx$L2_left[i] < dx$L2_right[i], dx$left[i], dx$right[i])
  }
  dx
}
complete_responses <- function(df) {
  stimuli_id <- df$orig %>% unique()

  d_complete <-
    tibble(
      expand.grid(
        orig = stimuli_id,
        left = stimuli_id,
        right = stimuli_id,
        stringsAsFactors = F
      ),
      response = NA_integer_,
      selected = NA_integer_,
      rt = NA_real_
    )
  d_complete %>% filter(orig != left, orig != right, left != right)
}
