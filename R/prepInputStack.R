#' Simple wrapper around `prepInputs` for a stack of raster layers
#'
#' @param ... Arguments passed to \code{\link{prepInputs}} and  \code{\link{postProcess}}.
#'            User should supply several named arguments here, including:
#'            `targetFile`, `archive`, `url`, `alsoExtract`, `destinationPath`, `to`, `fun`, `quick`,
#'            `purge`, `overwrite`, and `useCache`.
#'            See details and examples.
#'
#' @return `RasterStack`
#'
#' @author Tati Micheletti
#' @export
#' @importFrom reproducible prepInputs postProcess
#' @importFrom terra rast 
#'
#' @rdname prepInputStack
prepInputStack <- function(...) {
  message("prepInput a raster stack...")
  stackLayers <- prepInputs(...)
  postProcessedLayers <- lapply(stackLayers, 
                                FUN = function(layer, ...) {
                                  postProcess(layer,...)
                                }, ...)
  postProcessedLayers <- rast(postProcessedLayers)
  names(postProcessedLayers) <- names(stackLayers)
  return(postProcessedLayers)
}
