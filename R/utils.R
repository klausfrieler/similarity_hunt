
pair_index <- function(s1, s2, sep = "_"){
  stopifnot(length(s1) == length(s2))
  ret <- vector("character", length(s1))
  ret[s1 <= s2] <- sprintf("%s%s%s", s1[s1 <= s2], sep, s2[s1 <= s2])
  ret[s1 > s2] <- sprintf("%s%s%s", s2[s1 > s2], sep, s1[s1 > s2])
  ret
}


get_ratings_sim_mat <- function(avg_sim_ratings, min_ratings = 10){
  rating_sim_mat_red <- avg_sim_ratings %>%
    filter(n_ratings >= min_ratings) %>%
    mutate(melody1 = sprintf("MEL%04d", target_id),
           melody2 = sprintf("MEL%04d", query_id),
           algorithm = "ratings", full_name = "ratings") %>%
    select(-c(target_id, query_id))
  sim_mat_factory$new(rating_sim_mat_red, paired = T)
}
