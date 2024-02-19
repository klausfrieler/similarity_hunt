library(tidyverse)
library(melsim)

get_all_similarities <- function(sim_measures, sim_ratings, mel_list){
  tmp <- sim_ratings %>% distinct(target_id, query_id, pair_id)
  melsim(mel_list[tmp$target_id], mel_list[tmp$query_id], paired = T, sim_measures)
}

recalc_similarities <- function(min_ratings = setup_min_ratings,
                                sim_measures = c("diffed", "rawed",  "ncdintioi", "ngrtvers", "opti3",
                                                 "ngrukko2", "ngram_ioi_class_dist2",
                                                 "ngrukko1", "ngram_ioi_class_dist1",
                                                 "emd", "dtw")){

  rating_sim_mat_m2m <- get_ratings_sim_mat(sim_ratings_avg %>%
                                              filter(trial_type == "midi_to_midi"),
                                            min_ratings = min_ratings)

  rating_sim_mat_m2a <- get_ratings_sim_mat(sim_ratings_avg %>%
                                              filter(trial_type == "midi_to_audio"),
                                            min_ratings = min_ratings)

  all_sim_m2m <- get_all_similarities(sim_measures,
                                      sim_ratings %>%
                                        filter(n_ratings >= min_ratings,
                                               trial_type == "midi_to_midi"),
                                      mel_list)
  all_sim_m2a <- get_all_similarities(sim_measures,
                                      sim_ratings %>%
                                        filter(n_ratings >= min_ratings,
                                               trial_type == "midi_to_audio"),
                                      mel_list)

  all_sim_m2m$fuse(rating_sim_mat_m2m)$remove_algorithm("const")
  all_sim_m2a$fuse(rating_sim_mat_m2a)$remove_algorithm("const")
  saveRDS(all_sim_m2m, sprintf("data/all_sim_%d_m2m.rds", min_ratings))
  saveRDS(all_sim_m2a, sprintf("data/all_sim_%d_m2a.rds", min_ratings))
  sim_full <- bind_rows(all_sim_m2m$as_wide()   %>%
                          mutate(trial_type = "midi_to_midi"),
                        all_sim_m2a$as_wide() %>%
                          mutate(trial_type = "midi_to_audio")) %>% select(trial_type, where(is.numeric))
  saveRDS(sim_full, sprintf("data/sim_full_%s.rds", min_ratings))
}

add_similarities <- function(min_ratings, sim_measures, sim_ratings = sim_ratings_avg, overwrite = F){
  fname <-  sprintf("data/sim_full_%s.rds", min_ratings)
  if(!file.exists(fname)){
    logging::logerror(sprintf("Could not find '%s'", fname))
    return(NULL)
  }
  sim_full <- readRDS(fname)

  fname <- sprintf("data/all_sim_%d_m2m.rds", min_ratings)
  if(!file.exists(fname)){
    logging::logerror(sprintf("Could not find '%s'", fname))
    return(NULL)
  }
  all_sim_m2m <- readRDS(fname) %>% melsim::update_sim_mat()

  fname <- sprintf("data/all_sim_%d_m2a.rds", min_ratings)
  if(!file.exists(fname)){
    logging::logerror(sprintf("Could not find '%s'", fname))
    return(NULL)
  }
  all_sim_m2a <- readRDS(fname) %>% melsim::update_sim_mat()

  common_algos <- intersect(all_sim_m2a$algorithms, sim_measures)
  if(length(common_algos) > 0){
    if(overwrite){
      logging::logwarn("Some measures already present in old data, overwriting")
    }
    else{
      sim_measures <- setdiff(sim_measures, all_sim_m2a$algorithms)
      if(length(sim_measures) == 0){
        logging::logwarn("All measures already present, use overwrite = T to enforce recalculation.")
        return(NULL)
      }
      else{
        logging::logwarn("Some measures already present in old data, ignoring.")

      }
    }
  }
  new_sim_m2m <- get_all_similarities(sim_measures,
                                      sim_ratings %>%
                                        filter(n_ratings >= min_ratings,
                                               trial_type == "midi_to_midi"),
                                      mel_list)
  new_sim_m2a <- get_all_similarities(sim_measures,
                                      sim_ratings %>%
                                        filter(n_ratings >= min_ratings,
                                               trial_type == "midi_to_audio"),
                                      mel_list)

  all_sim_m2m$fuse(new_sim_m2m, overwrite = overwrite)$remove_algorithm("const")
  all_sim_m2a$fuse(new_sim_m2a, overwrite = overwrite)$remove_algorithm("const")
  saveRDS(all_sim_m2m, sprintf("data/all_sim_%d_m2m.rds", min_ratings))
  saveRDS(all_sim_m2a, sprintf("data/all_sim_%d_m2a.rds", min_ratings))

  sim_full <- bind_rows(all_sim_m2m$as_wide()   %>%
                          mutate(trial_type = "midi_to_midi"),
                        all_sim_m2a$as_wide() %>%
                          mutate(trial_type = "midi_to_audio")) %>%
    select(trial_type, where(is.numeric))
  saveRDS(sim_full, sprintf("data/sim_full_%s.rds", min_ratings))

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

