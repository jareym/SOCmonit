#' Preprocess Spectral data
#'
#' Applies preprocessing methods to spectral data using prospectr package see \link[prospectr]{prospectr}
#'
#' @param spectra The input spectral data. Can be a matrix or data frame.
#' @param columns The columns of the spectral data to be processed.
#' @param pp.method The preprocessing method to apply. Available options: "cr" (continuum removal),
#'   "sg" (Savitzky-Golay smoothing), "dt" (detrend), "gd" (gap derivative), "ma" (moving average),
#'   "msc" (multiplicative scatter correction),"snv" (standard normal variate), "b" (binning),
#'   "rs" (resample), "rs2" (resample2), "bs" (block scaling), "bn" (block normalization).
#' @param wav Wavelength vector used for preprocessing methods that require it (cr, dt, rs, rs2).
#' @param type Type of continuum removal method (cr). Available options: "rubberband" or "modpoly".
#' @param interpol Interpolation method for resampling (rs). Available options: "linear" or "spline".
#' @param new.wav Wavelength vector for resampling (rs, rs2).
#' @param sigma Sigma value for block scaling (bs).
#' @param targetnorm Target normalization value for block normalization (bn).
#' @param m Polynomial order for Savitzky-Golay smoothing (sg).
#' @param p Number of points to use for Savitzky-Golay smoothing (sg).
#' @param w Width of the Savitzky-Golay window (sg, ma).
#' @param fwhm Full Width at Half Maximum for resample2 method (rs2).
#' @param bins Number of bins for binning method (b).
#' @param bin.size Size of each bin for binning method (b).
#' @param ... Additional parameters to be passed to the respective prospectr functions.
#'
#' @return Processed spectral data with the specified preprocessing applied.
#'
#' @examples
#' # Example 1: Standard Normal Variate (snv) preprocessing
#' pp.spec(spectra, pp.method = "snv")
#'
#' # Example 2: Savitzky-Golay smoothing
#' pp.spec(spectra, pp.method = "sg", m = 2, p = 1, w = 5)
#'
#' # Example 3: Resample with interpolation
#' pp.spec(spectra, pp.method = "rs", interpol = "linear", new.wav = new_wav, wav = wav)
#'
#' @export


pp.spec <- function(spectra, columns = 1:ncol(spectra), pp.method = "snv",
                    wav = NULL, type = NULL, interpol = NULL, new.wav = NULL, sigma = NULL,
                    targetnorm = NULL, m = 1, p = 1, w = 5, fwhm = NULL, bins = NULL,
                    bin.size = NULL, ...) {

  # Convert spectra to matrix if it's a data frame
  spec <- as.matrix(spectra[, columns])

  preprocess <- switch(pp.method,
                       cr = prospectr::continuumRemoval(spec, wav = wav, type = type, ...),
                       sg = prospectr::savitzkyGolay(spec, m = m, p = p, w = w, ...),
                       dt = prospectr::detrend(spec, wav = wav, ...),
                       gd = prospectr::gapDer(spec, ...),
                       ma = prospectr::movav(spec, w = w, ...),
                       msc = prospectr::msc(spec, ...),
                       snv = prospectr::standardNormalVariate(spec, ...),
                       b = prospectr::binning(spec, bins = bins, bin.size = bin.size, ...),
                       rs = prospectr::resample(spec, new.wav = new.wav, interpol = interpol, wav = wav, ...),
                       rs2 = prospectr::resample2(spec, new.wav = new.wav, fwhm = fwhm, wav = wav, ...),
                       bs = prospectr::blockScale(spec, type = "hard", sigma2 = sigma, ...),
                       bn = prospectr::blockNorm(spec, targetnorm = targetnorm, ...)
  )

  if (is.data.frame(spectra)) {
    spectra[, columns] <- as.data.frame(preprocess)
    return(spectra)
  } else {
    spectra[, columns] <- preprocess
    return(spectra)
  }
}
