get_all_similarities <- function(sim_measures, sim_ratings, mel_list){
  tmp <- sim_ratings %>% distinct(target_id, query_id, pair_id)
  melsim(mel_list[tmp$target_id], mel_list[tmp$query_id], paired = T, sim_measures)
}

cmp_similarities <- function(sim_mat, sim_ratings){
  sim_ratings_avg <- sim_ratings %>%
    group_by(target_id, query_id) %>%
    summarise(sim_rating = mean(similarity_rating, na.rm = T),
              .groups = "drop")
  sim_mat_data <-
    sim_mat$data %>%
    filter(algorithm != "const") %>%
    mutate(target_id = str_remove(melody1, "MEL") %>% as.integer(),
           query_id = str_remove(melody2, "MEL") %>% as.integer()) %>%
    select(algorithm, target_id, query_id, sim)

  joined_data <- sim_mat_data %>%
    left_join(sim_ratings_avg %>%
                select(target_id, query_id, sim_rating), by = c("target_id", "query_id"))

  corr_mat <- joined_data %>%
    select(algorithm, sim, sim_rating) %>%
    group_by(algorithm) %>%
    correlation::correlation()
  list(joined_data, corr_mat)
}

get_ratings_sim_mat <- function(avg_sim_ratings, min_ratings = 10){
  rating_sim_mat_red <- avg_sim_ratings %>%
    filter(n >= min_ratings) %>%
    mutate(melody1 = sprintf("MEL%04d", target_id),
           melody2 = sprintf("MEL%04d", query_id),
           algorithm = "ratings", full_name = "ratings") %>%
    select(-c(target_id, query_id))

  sim_mat_factory$new(rating_sim_mat_red, paired = T)
}
