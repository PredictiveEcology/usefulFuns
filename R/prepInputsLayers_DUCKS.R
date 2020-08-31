#' Prepare DUCKS layer
#'
#' Intended to prepare the DUCKS Unlimited Hybrid Wetland v. 2.1 layer for different purposes.
#' The output is a `RasterLayer` cropped and reprojected to the `studyArea`, and resampled to the
#' `rasterToMatch` if any of these are provided.
#'
#' @param destinationPath Path where to save the downloaded file.
#' @param lccLayer Which year should be used as a base for the vegetation layer? Default to 2005
#' @param url The url from where the layer should be downloaded from.
#'            If \code{NULL}, the default is used.
#' @param archive TODO
#' @param targetFile TODO
#' @param studyArea Study area for which the layer should be cropped to
#' @param rasterToMatch TODO
#' @param overwrite Logical indicating whether to overwrite the previously processed file.
#'
#' @return RasterLayer
#'
#' @export
#' @importFrom crayon green red yellow
#' @importFrom reproducible asPath Cache prepInputs
#' @include classifyWetlands.R
#' @rdname prepInputsLayers_DUCKS
prepInputsLayers_DUCKS <- function(destinationPath, lccLayer = "2005",
                                   url = NULL, archive = NULL,
                                   targetFile = NULL,
                                   studyArea = NULL,
                                   rasterToMatch = NULL,
                                   overwrite = TRUE) {
  if (is.null(url))
    url <- "https://drive.google.com/open?id=1wNpBdLICWDJ-DGwDboPb9wVwRwtGm1go"
  if (is.null(targetFile))
    targetFile <- "HWL_BCR6.tif"
  if (is.null(archive))
    archive <- "HWL_BCR6.zip"

  message(yellow("  Trying to load DUCKS Unlimited Hybrid Wetland v. 2.1 layers..."))
  tryCatch({
      DUCKSlayer <- Cache(prepInputs,
                          targetFile = targetFile,
                          archive = archive,
                          url = url,
                          alsoExtract = "similar",
                          destinationPath = destinationPath,
                          fun = "raster::raster",
                          studyArea = studyArea,
                          rasterToMatch = rasterToMatch,
                          datatype = "INT1U",
                          overwrite = overwrite,
                          userTags =  c("DUCKs", "Hybrid", "Wetland"))

      message(green("  DUCKS Unlimited Hybrid Wetland v. 2.1 layers successfully loaded."))

      DUCKSlayerReclass <- classifyWetlands(LCC = lccLayer, wetLayerInput = DUCKSlayer,
                                            pathData = destinationPath, studyArea = studyArea)

    return(DUCKSlayerReclass)
  }, error = function(e){
    message(red(paste("  Downloading DUCKS Unlimited Hybrid Wetland v. 2.1 layers failed.",
                      "This is probably a restriction access issue.",
                      "A wetlands layer based on LCC05 will be downloaded instead.")))
    url <- "https://drive.google.com/open?id=10RRHsy2vX6xaOLNPQSvz66k-xn_4X7GY"
    targetFile <- "wetlandsLayer.tif"
    archive <- "wetlandsLayer.zip"

    LCCLayer <- Cache(prepInputs,
                      targetFile = targetFile,
                      archive = archive,
                      url = url,
                      alsoExtract = "similar",
                      destinationPath = destinationPath,
                      fun = "raster::raster",
                      studyArea = studyArea,
                      rasterToMatch = rasterToMatch,
                      datatype = "INT1U",
                      overwrite = overwrite,
                      userTags =  c("LCC05wetlands", "Wetland", "Uplands"))
    return(LCCLayer)
  })
}
