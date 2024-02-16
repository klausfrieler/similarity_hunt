
pair_index <- function(s1, s2, sep = "_"){
  stopifnot(length(s1) == length(s2))
  ret <- vector("character", length(s1))
  ret[s1 <= s2] <- sprintf("%s%s%s", s1[s1 <= s2], sep, s2[s1 <= s2])
  ret[s1 > s2] <- sprintf("%s%s%s", s2[s1 > s2], sep, s1[s1 > s2])
  ret
}

