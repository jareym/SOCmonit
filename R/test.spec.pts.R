#' Test point spectral data
#'
#' Identify and remove outliers from spectral data based on the PCOut function
#'
#'
#' This function calculates outliers and summary statistics for a given set of spectra.
#'
#' @param spectra The input data containing the spectra.
#' @param columns A vector specifying the columns of the spectra to be considered.
#' @param ID.point The column index or name indicating the identifier for each spectrum.
#' @param ID.sample The column index or name indicating the identifier for each sample. Defaults to NULL.
#' @param outliers_by_id_point Logical value indicating whether to calculate outliers separately for each ID.point.
#'                             If TRUE, outliers will be calculated for each ID.point individually.
#'                             If FALSE, general outliers for all spectra will be calculated.
#' @param include_average Logical value indicating whether to include the average of non-outlier spectra by ID.point.
#' @param ... Additional arguments to be passed to the \code{mvoutlier::pcout} function.
#'
#' @return A list containing the calculated results.
#'
#' @details The function calculates outliers using the \code{mvoutlier::pcout} function based on the provided spectra data.
#' If \code{outliers_by_id_point} is set to TRUE, it separates the spectra into individual ID.points and calculates outliers
#' for each ID.point separately. The outliers are stored in the \code{Individual_results} list, which contains two elements:
#' \code{No_outliers} and \code{Outliers}. The \code{No_outliers} element contains the non-outlier spectra for each ID.point,
#' and the \code{Outliers} element contains the outlier spectra for each ID.point.
#'
#' If \code{outliers_by_id_point} is set to FALSE, it calculates general outliers for all spectra. The general outliers are
#' stored in the \code{General_results} list, which contains two elements: \code{No_outliers} and \code{Outliers}. The
#' \code{No_outliers} element contains the non-outlier spectra, and the \code{Outliers} element contains the outlier spectra.
#'
#' Additionally, if \code{include_average} is set to TRUE, it calculates the average of the non-outlier spectra by ID.point
#' and stores it in the \code{Average_no_outliers} element of the output list.
#'
#' @examples
#' # Example with a data frame
#' data <- data.frame(ID.point = c(1, 1, 2, 2, 3, 3),
#'                    ID.sample = c(1, 2, 1, 2, 1, 2),
#'                    spec = c(1.2, 1.3, 1.1, 1.2, 0.9, 1.0))
#' result <- test.spec.pts(data, columns = "spec", ID.point = "ID.point", outliers_by_id_point = TRUE)
#'
#' @export

test.spec.pts <- function(spectra, columns = 1:ncol(spectra),
                          ID.point, ID.sample = NULL, outliers_by_id_point = TRUE,
                          include_average = FALSE, ...) {

  if (is.null(ID.sample)) {
    ID.sample <- seq_len(nrow(spectra))
  }

  spec <- spectra[, columns]
  ID.sample <- spectra[, ID.sample]
  ID.point <- spectra[, ID.point]
  spec.data <- cbind(ID.point, ID.sample, spec)

  general_outliers <- NULL
  individual_results <- list()

  if (!outliers_by_id_point) {
    general_result <- mvoutlier::pcout(spec, ...)
    print(paste0("Total outliers: ", sum(general_result$wfinal01 == 0)))
    print(paste0("Total non-outliers: ", sum(general_result$wfinal01 == 1)))
    if (sum(general_result$wfinal01 == 0) > 0) {
      general_outliers <- spec.data[which(general_result$wfinal01 == 0), ]

       }

    if (include_average) {
      avg_no_outliers <- aggregate(spec.data[, -c(1:2)], by = list(spec.data$ID.point), FUN = mean)
      colnames(avg_no_outliers)[1] <- "ID.point"
      output <- list(General_results = list(No_outliers = spec.data[which(general_result$wfinal01 == 1), ], Outliers = general_outliers),
                     Average_no_outliers = avg_no_outliers)
      return(output)
    }
  }

  if (outliers_by_id_point) {
    unique_points <- unique(ID.point)
    individual_outliers <- list()
    individual_no_outliers <- list()

    for (i in unique_points) {
      spec_subset <- spec.data[ID.point == i, ]
      individual_result <- mvoutlier::pcout(spec_subset[, -c(1:2)], ...)

      if (sum(individual_result$wfinal01 == 0) > 0) {
        individual_outliers[[as.character(i)]] <- spec_subset[which(individual_result$wfinal01 == 0), ]
      } else {
        individual_outliers[[as.character(i)]] <- "no outliers"
      }

      individual_no_outliers[[as.character(i)]] <- spec_subset[which(individual_result$wfinal01 == 1), ]
    }

    individual_results$No_outliers <- do.call(rbind, individual_no_outliers)
    individual_results$Outliers <- do.call(rbind, individual_outliers)
    print(paste0("Total outliers: ", (nrow(spectra)-dim(individual_results$No_outliers)[1])))
    print(paste0("Total non-outliers: ", dim(individual_results$No_outliers)[1]))
    if (include_average) {
      avg_no_outliers <- aggregate(individual_results$No_outliers[, -c(1, 2)], by = list(individual_results$No_outliers$ID.point), FUN = mean)
      colnames(avg_no_outliers)[1] <- "ID.point"
      output <- list(Individual_results = individual_results, Average_no_outliers = avg_no_outliers)
      return(output)
    }
  }

  output <- list()

  if (!outliers_by_id_point && length(general_outliers) > 0) {
    output <- list(General_results = list(No_outliers = spec.data[which(general_result$wfinal01 == 1), ], Outliers = general_outliers))
  }

  if (outliers_by_id_point) {
    output <- list(Individual_results = individual_results)

  }

  return(output)
}
