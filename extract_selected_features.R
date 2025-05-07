extract_selected_features <- function(table) {
  idx <- find_threshold(table)
  if (idx >= 23) {
    idx <- 14  # manual override for special cases
  }
  selected_features <- table$Removed_Feature[idx:nrow(table)]
  return(selected_features)
}
