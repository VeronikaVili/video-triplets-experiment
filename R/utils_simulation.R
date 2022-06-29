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

L2_distance_cached <- function(i1, i2, dmat) {
  dmat[i1, i2]
}

L2_distance_vectorized <- function(v1, v2, d_tbl) {
  tibble(x = v1, y = v2) %>% left_join(d_tbl, by = c("x", "y")) %>% pull(d)
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

add_distances_cached <- function(dd, set) {
  dx <-
    dd %>%
    select(-response, -selected, -rt) %>%
    mutate(
      L2_left = NA_real_, L2_right = NA_real_,
      response = NA_integer_, selected = NA_character_
    )
  dmat <- as.matrix(dist(as.matrix(set)))
  for (i in 1:nrow(dx)) {
    dx$L2_left[i] <- L2_distance_cached(
      as.numeric(dx$orig[i]), as.numeric(dx$left[i]), dmat
    )
    dx$L2_right[i] <- L2_distance_cached(
      as.numeric(dx$orig[i]), as.numeric(dx$right[i]), dmat
    )
    dx$response[i] <- ifelse(dx$L2_left[i] < dx$L2_right[i], 0, 1)
    dx$selected[i] <- ifelse(dx$L2_left[i] < dx$L2_right[i], dx$left[i], dx$right[i])
  }
  dx
}

add_distances_vectorized <- function(dd, set) {
  dx <-
    dd %>%
    select(-response, -selected, -rt) %>%
    mutate(
      L2_left = NA_real_, L2_right = NA_real_,
      response = NA_integer_, selected = NA_character_
    )
  dmat <- as.matrix(dist(as.matrix(set)))
  dist_tbl <- crossing(x = 1:nrow(set), y = 1:nrow(set)) %>% 
    mutate(d = as.numeric(dmat))
  dx$L2_left <- L2_distance_vectorized(
    as.numeric(dx$orig), as.numeric(dx$left), dist_tbl
  )
  dx$L2_right <- L2_distance_vectorized(
    as.numeric(dx$orig), as.numeric(dx$right), dist_tbl
  )
  dx$response <- if_else(dx$L2_left < dx$L2_right, 0, 1)
  dx$selected <- if_else(dx$L2_left < dx$L2_right, dx$left, dx$right)
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
