library(tidyverse)
library(melsim)


setup_workspace <-function(min_ratings = 10){
  logging::loginfo("Reading melody objects...")
  mel_list <- readRDS("data/mel_objects.rds") %>%
    update_melodies()
  assign("mel_list", mel_list, globalenv())

  logging::loginfo("Reading melody list...")
  assign("mel_ids", readRDS("data/mel_list.rds"), globalenv())

  logging::loginfo("Reading similarity ratings...")
  sim_ratings_full <-  readRDS("data/sim_ratings_full.rds")

  sim_ratings <- sim_ratings_full %>%
    filter(sample_type != "catch_trial") %>%
    mutate(pair_id = pair_index(target_id, query_id))

  assign("sim_ratings", sim_ratings,globalenv())

  logging::loginfo("Calculating averages")
  sim_ratings_avg <- sim_ratings %>%
    group_by(target_id, query_id, pair_id, trial_type, sample_type) %>%
    summarise(sim = mean(similarity_rating, na.rm = T)/10,
              n = n(),
              sd = sd(similarity_rating/10, na.rm = T),
              se = sd/sqrt(n),
              .groups = "drop")
  assign("sim_ratings_avg", sim_ratings_avg, globalenv())


  logging::loginfo("Calculating full similarity matrices...")
  rating_sim_mat <- sim_ratings_avg %>%
    mutate(melody1 = sprintf("MEL%d", target_id),
           melody2 = sprintf("MEL%d", query_id),
           algorithm = "ratings", full_name = "ratings") %>%
    select(-c(target_id, query_id))

  rating_sim_mat <- sim_mat_factory$new(rating_sim_mat, paired = T)
  assign("rating_sim_mat", rating_sim_mat, globalenv())

  logging::loginfo("Calculating reduced similarity matrices...")
  rating_sim_mat_red <- sim_ratings_avg %>%
    filter(n >= min_ratings) %>%
    mutate(melody1 = sprintf("MEL%04d", target_id),
           melody2 = sprintf("MEL%04d", query_id),
           algorithm = "ratings", full_name = "ratings") %>%
    select(-c(target_id, query_id))

  rating_sim_mat_red <- sim_mat_factory$new(rating_sim_mat_red, paired = T)
  assign("rating_sim_mat_red", rating_sim_mat_red, globalenv())


}
