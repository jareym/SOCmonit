#' Test on-the-go spectral data
#'
#' This function performs outlier detection on spectral data, either in a general or separated by Plot ID.
#'
#' @param spectra An object containing spectral data. It can be either a matrix or an S4 object (e.g., SpatialPointsDataFrame).
#' @param columns A vector specifying the indices or names of the columns in the \code{spectra} object that contain the spectral data.
#' @param ID.plot A vector specifying the indices or names of the columns in the \code{spectra} object that contain the plot IDs (optional).
#' @param ID.otg A vector specifying the indices or names of the columns in the \code{spectra} object that contain the outlier group IDs (optional).
#' @param outliers_by_id_plot A logical value indicating whether to perform outlier detection separately for each ID.plot (default is TRUE).
#' @param ... Additional arguments to be passed to the \code{mvoutlier::pcout} function.
#'
#' @return A list containing the results of outlier detection. The structure of the output depends on the \code{outliers_by_id_plot} argument.
#' If \code{outliers_by_id_plot} is FALSE, the output will contain general results, including a list of no outliers and outliers.
#' The no outliers will be returned as a SpatialPointsDataFrame object with the same coordinate reference system (CRS) as the input \code{spectra} object.
#' The outliers will be returned as a dataframe.
#' If \code{outliers_by_id_plot} is TRUE, the output will contain individual results, including a list of no outliers and outliers for each ID.plot.
#' The no outliers (for general or each ID.plot) will be returned as a SpatialPointsDataFrame object(s) with the same CRS as the input \code{spectra} object.
#' The outliers (for general or each ID.plot) will be returned as a dataframe(s).
#'
#' @examples
#' # Example 1: General outlier detection
#' data <- matrix(rnorm(100), ncol = 10)  # Sample spectral data
#' result <- test.spec.otg(data)  # Perform general outlier detection
#' print(result)
#'
#' # Example 2: Individual outlier detection by ID.plot
#' data <- data.frame(ID.plot = c(1, 1, 2, 2, 2, 3, 3), Spectra = rnorm(7))
#' result <- test.spec.otg(data, ID.plot = "ID.plot", outliers_by_id_plot = TRUE)
#' print(result)
#'
#' @export


test.spec.otg <- function(spectra, columns = 1:ncol(spectra),
                          ID.plot = NULL, ID.otg = NULL, outliers_by_id_plot = TRUE,
                          ...) {

  is_s4 <- isS4(spectra)

  if (is_s4) {
    spec <- spectra@data[, columns]
    if (outliers_by_id_plot && !is.null(ID.plot)) {
      ID.plot <- spectra@data[, ID.plot]
    }
    if (is.null(ID.otg)) {
      ID.otg <- seq_len(nrow(spectra))
    } else {
      ID.otg <- spectra@data[, ID.otg]
    }
  } else {
    spec <- spectra[, columns]
    if (outliers_by_id_plot && !is.null(ID.plot)) {
      ID.plot <- spectra[, ID.plot]
    }
    if (is.null(ID.otg)) {
      ID.otg <- seq_len(nrow(spectra))
    } else {
      ID.otg <- spectra[, ID.otg]
    }
  }

  spec.data <- cbind(ID.otg, spec)

  if (outliers_by_id_plot && !is.null(ID.plot)) {
    spec.data <- cbind(ID.plot, spec.data)
  }

  general_outliers <- NULL
  individual_results <- list()

  if (!outliers_by_id_plot) {
    general_result <- mvoutlier::pcout(spec, ...)
    print(paste0("Total outliers: ", sum(general_result$wfinal01 == 0)))
    print(paste0("Total non-outliers: ", sum(general_result$wfinal01 == 1)))
    if (sum(general_result$wfinal01 == 0) > 0) {
      general_outliers <- spec.data[which(general_result$wfinal01 == 0), ]
    }

    if (is_s4) {
      no_outliers_spatial <- spectra[which(general_result$wfinal01 == 1), ]
      no_outliers_spatial@data <- spec.data[which(general_result$wfinal01 == 1), ]
      no_outliers_spatial@coords <- spectra@coords[which(general_result$wfinal01 == 1), ]
      no_outliers_spatial@proj4string <- spectra@proj4string
      output <- list(General_results = list(No_outliers = no_outliers_spatial, Outliers = general_outliers))
      return(output)
    } else {
      output <- list(General_results = list(No_outliers = spec.data[which(general_result$wfinal01 == 1), ], Outliers = general_outliers))
      return(output)
    }
  }

  if (outliers_by_id_plot) {
    unique_plots <- unique(ID.plot)
    individual_outliers <- list()
    individual_no_outliers <- list()

    for (i in unique_plots) {
      spec_subset <- spec.data[ID.plot == i, ]
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

    if (is_s4) {
      no_outliers_ids <- individual_results$No_outliers$ID.otg
      no_outliers_spatial <- spectra[spectra@data$ID.otg %in% no_outliers_ids, ]
      no_outliers_spatial@data <- individual_results$No_outliers
      no_outliers_spatial@coords <- spectra@coords[match(no_outliers_ids, spectra@data$ID.otg), ]
      no_outliers_spatial@proj4string <- spectra@proj4string
      output <- list(Individual_results = list(No_outliers = no_outliers_spatial, Outliers = individual_results$Outliers))
      return(output)
    } else {
      output <- list(Individual_results = individual_results)
      return(output)
    }
  }

  output <- list()

  if (!outliers_by_id_plot && length(general_outliers) > 0) {
    output <- list(General_results = list(No_outliers = spec.data[which(general_result$wfinal01 == 1), ], Outliers = general_outliers))
  }

  if (outliers_by_id_plot) {
    output <- list(Individual_results = individual_results)
  }

  if (is_s4) {
    output@proj4string <- spectra@proj4string
    return(output)
  } else {
    return(output)
  }
}
