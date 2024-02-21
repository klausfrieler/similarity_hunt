
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


add_and_collect_features <- function(mel_list, segmentation = NULL){
  if(is.null(segmentation)){
    segmentation <- "global"
  }
  mel_list <- update_melodies(mel_list)
  cli::cli_progress_bar("Calculating features...", total = length(mel_list), .envir = globalenv())
  ret <-
    map_dfr(1:length(mel_list), function(i){
      cli::cli_progress_update(.envir = globalenv())
      mel_list[[i]]$add_basic_features(overwrite = F)
      mel_list[[i]]$add_tonal_features(overwrite = F)
      mel_list[[i]]$add_difficulty_features(overwrite = F)
      ret <- mel_list[[i]]$features[[segmentation]] %>% mutate(id = mel_list[[i]]$meta$id)

    })
  ret <- ret %>% select(-contains(!!segmentation))
  cli::cli_progress_done(.envir = globalenv())
  return(ret)
}

plot_z_values <- function(data, max_z = 4, point_size = 3, base_size = NULL){
  #data <- merge(data, sof, by = "id")
  data$range <- factor(floor(abs(data$value) ))
  data$name <- factor(data$name, levels = rev(unique(data$name)))
  #print(names(data))
  q <- data %>% ggplot(aes(x = name, y = value))

  q <- q + geom_point(size = point_size, aes(colour = range))
  q <- q + geom_segment(aes(x = name, xend = name, y = 0, yend = value))
  q <- q + scale_y_continuous(limits = c(- max_z - .5, max_z + .5), breaks = seq(-max_z, max_z))
  q <- q + coord_flip()
  q <- q + facet_wrap(~id)
  if(is.null(base_size)){
    q <- q + theme_bw()
  }
  else{
    q <- q + theme_bw(base_size = base_size)
  }
  q <- q + theme(axis.ticks = element_blank())
  q <- q + guides(colour = "none")
  q <- q + labs(x = "",  y = "z-value")
  q <- q + geom_hline(yintercept = 0, linetype = "dotted")
  q <- q + geom_hline(yintercept = 1) + geom_hline(yintercept = -1)
  if(max_z >= 2){
    q <- q + geom_hline(yintercept = 2, linetype = "dotted")  + geom_hline(yintercept = -2, linetype = "dotted")
  }
  if (max_z > 3){
    q <- q + geom_hline(yintercept = 3) + geom_hline(yintercept = -3)
  }
  if (max_z >= 4){
    q <- q + geom_hline(yintercept = 4, linetype = "dotted") + geom_hline(yintercept = -4, linetype = "dotted")
  }
  q
}

get_z_diff <- function(features, id1, id2){
  features$id <- NULL
  mean(abs(features[id1, ]- features[id2, ]))
}

select_numeric <- function(df){
  df %>% select(where(is.numeric))
}
add_feature_sim <- function(base_sim, mel_features){
  mel_features <-   mel_features %>%
    select(where(is.numeric)) %>%
    select(-id, -TON.global) %>%
    mutate(across(where(is.numeric), function(x) as.numeric(scale(x)))) %>% select(!contains("abs"))
  fa_features <- suppressWarnings(mel_features %>% psych::fa(8))

  z_features <- fa_features %>%
    predict(data = mel_features) %>%
    as_tibble()

  id_pairs <-
    base_sim$data %>%
    distinct(melody1, melody2) %>%
    mutate(id1 = as.integer(str_extract(melody1, "[0-9]+")),
           id2 = as.integer(str_extract(melody2, "[0-9]+")))

  f_sims <- rowMeans(abs(z_features[id_pairs$id1,] - z_features[id_pairs$id2,]))

  base_sim$fuse(melsim::sim_mat_factory$new(id_pairs %>%
                                            select(-id1, -id2) %>%
                                            mutate(sim = exp(-f_sims),
                                                   algorithm = "feature"),
                                          paired = T))
  base_sim
}
