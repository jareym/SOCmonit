#' Perform K-fold sampling with spatial neighbor constraints
#'
#' This function performs k-fold sampling while considering spatial autocorrelation.
#'
#' @param x Numeric vector or matrix of data.
#' @param min.dist Numeric value specifying the minimum distance threshold for neighbor selection. Pairs of samples
#'   closer than this threshold are considered neighbors and placed within the same fold.
#' @param nfolds Integer specifying the number of folds for sampling.
#' @param stratified Logical indicating whether to perform stratified sampling based on quantiles of \code{x}.
#'   Stratified sampling ensures that each fold is a good representative of the whole dataset.
#' @param xcoord Numeric vector of x-coordinates corresponding to the spatial locations of the data.
#' @param ycoord Numeric vector of y-coordinates corresponding to the spatial locations of the data.
#'
#' @return A data frame with the sampled data and fold assignments.
#'
#' @examples
#' x <- 1:10
#' xcoord <- runif(10)
#' ycoord <- runif(10)
#' CV.folds(x, min.dist = 0.5, nfolds = 5, stratified = TRUE, xcoord, ycoord)
#' @export
#'
#' @details The function first computes a spatial distance matrix using the provided coordinates. For each pair of
#'   samples, if the distance is below the provided minimum distance threshold, the samples are marked as neighbors.
#'   The function then creates k-folds either randomly or in a stratified manner (if specified). It then iteratively
#'   reassigns the folds of the neighbor pairs in such a way that they belong to the same fold, while ensuring that
#'   each fold has at least one sample. If the function cannot achieve this configuration due to the number of close
#'   neighbors being larger than the fold size, it stops and raises an error. If there are no neighbors below the
#'   minimum distance, the function proceeds with the created folds without reassigning any samples.

CV.folds <- function (x, min.dist, nfolds, stratified = TRUE, xcoord, ycoord) {

  spatial_dist_matrix <- as.matrix(dist(cbind(xcoord, ycoord)))  # spatial distance matrix
  spatial_dist_matrix[(upper.tri(spatial_dist_matrix,diag=FALSE))] <- NA
  neighbor_group <- ifelse(spatial_dist_matrix >0 & spatial_dist_matrix < min.dist, 1, 2)
  index_pair <- which(!is.na(neighbor_group), arr.ind=T)  # select and order pairs
  neighbors <- cbind(neighbor_group[ ! is.na(neighbor_group) ], index_pair)
  colnames(neighbors) <- c("distance_group","loc1","loc2")
  fold_indicators <- as.data.frame(neighbors)
  close_neighbors <- subset(fold_indicators, distance_group==1)  # select pairs of neighbors
  num_samples <- seq(1:nrow(as.data.frame(x)))

  if(isTRUE(stratified)){  # stratified sampling
    StrataBoundaries = quantile(x,  # separate data in quantiles
                                probs = seq(from = 0, to = 1, by = (1 / nfolds)),
                                na.rm = TRUE,
                                type = 7)
    Strata = cut(x,  # assign data to a quantile
                 StrataBoundaries,
                 labels = FALSE,
                 include.lowest = TRUE,
                 right = FALSE,
                 ordered_result = FALSE)

    strata_group <- factor(Strata)
  } else {
    strata_group <- factor(sample(rep(1:nfolds, len = length(num_samples))))  # random sampling
  }

  fold <- rep(NA, nrow(as.data.frame(x)))
  for (nf in levels(strata_group)) {
    num_strata <- sum(selected_strata <- (strata_group == nf))  # number of folds
    fold[selected_strata] <- sample(rep(1:nfolds, length = num_strata),  # assign data for each fold
                                    size = num_strata)
  }

  data_frame <- data.frame(x, num_samples, num_samples, fold)
  neighbor_index <- c(rbind(close_neighbors$loc2,close_neighbors$loc1))

  if (nrow(close_neighbors) > nrow(subset(data_frame, fold == 1)))
    stop("Number of close neighbors is larger than fold size")

  if (nrow(close_neighbors) > 0) {
    # Set a limit on the number of attempts to rearrange the folds
    max_attempts <- 1000
    attempt <- 0

    repeat {
      attempt <- attempt + 1

      neighbor_id <- match(x = data_frame$num_samples, neighbor_index, nomatch = -1)  # neighbors ID
      df_with_id <- data.frame(data_frame, neighbor_id)
      neighbor_subset <- subset(df_with_id, neighbor_id != -1)  # subset of neighbors
      neighbor_df <- data.frame(neighbor_index)
      neighbor_df$num_samples <- neighbor_df$neighbor_index
      neighbor_df$index_order <- 1:nrow(neighbor_df)
      merged_neighbor_df <- merge(neighbor_subset, neighbor_df, by="num_samples", sort = FALSE)
      neighbor_subset <- merged_neighbor_df[order(merged_neighbor_df$index_order),]
      neighbor_subset$group_label <- c("a", "b")
      split_groups <- split(neighbor_subset, neighbor_subset$group_label)
      split_groups <- as.data.frame(split_groups)
      neighbors <- subset(split_groups, select = c(a.num_samples, b.num_samples))
      split_groups$neighbors_same_fold <- ifelse((split_groups$a.fold != split_groups$b.fold), T, F)  # check if neighbors are not in the same fold
      neighbors_diff_fold <- subset(split_groups, neighbors_same_fold == T)

      if (nrow(neighbors_diff_fold) > 0 && attempt <= max_attempts) {  # neighbors rearrangement
        for (q in 1:nrow(neighbors_diff_fold)) {
          samples_same_fold <- subset(data_frame, fold == neighbors_diff_fold[q,]$a.fold)
          if (nrow(samples_same_fold) > 1) {  # ensure there are other samples in the fold
            sample_to_change <- samples_same_fold[sample(nrow(samples_same_fold), size = 1),]
            data_frame$fold[neighbors_diff_fold[q,]$b.num_samples] <- neighbors_diff_fold[q,]$a.fold
            data_frame$fold[sample_to_change$num_samples] <- neighbors_diff_fold[q,]$b.fold
          }
        }
      } else {
        break
      }
    }

    return(subset(data_frame, select = -num_samples))
  } else {
    print("No neighbors below the minimum distance")
    return(subset(data_frame, select = -num_samples))
  }
}
