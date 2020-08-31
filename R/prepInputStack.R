#' Simple wrapper around `prepInputs` for a stack of raster layers
#'
#' @param ... Arguments passed to `fun` (i.e,. user supplied), \code{\link{postProcess}} and
#'            \code{\link[reproducible]{Cache}}.
#'            Since `...` is passed to `postProcess`, these will also be passed into the inner
#'            functions, e.g., \code{\link{cropInputs}}.
#'            User should supply several named arguments here, including:
#'            `targetFile`, `archive`, `url`, `alsoExtract`, `destinationPath`, `fun`, `quick`,
#'            `purge`, `overwrite`, and `useCache`.
#'            See details and examples.
#'
#' @return `RasterStack`
#'
#' @author Tati Micheletti
#' @export
#' @importFrom reproducible prepInputs postProcess
#' @importFrom raster nlayers stack
#'
#' @rdname prepInputStack
prepInputStack <- function(...) {
  dots <- list(...)
  message("prepInput a raster stack...")
  stackLayers <- reproducible::prepInputs(archive = dots$archive,
                                          url = dots$url,
                                          targetFile = dots$targetFile,
                                          alsoExtract = dots$alsoExtract,
                                          destinationPath = dots$destinationPath,
                                          fun = "raster::stack")
  postProcessedLayers <- lapply(X = seq_len(nlayers(stackLayers)), FUN = function(layer) {
    lay <- reproducible::postProcess(stackLayers[[layer]],
                                     studyArea = dots$studyArea,
                                     rasterToMatch = dots$rasterToMatch,
                                     destinationPath = dots$destinationPath,
                                     filename2 = dots$filename2)
    names(lay) <- names(stackLayers[[layer]])
    return(lay)
  })
  postProcessedLayers <- raster::stack(postProcessedLayers)
  names(postProcessedLayers) <- names(stackLayers)

  return(postProcessedLayers)
}
