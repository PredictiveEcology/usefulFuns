utils::globalVariables(c("PRENAME"))

#' @title
#' Preparing study area for Canadian projects
#'
#' @description
#' Downloads, reprojects, crops and masks to speficic areas in canada such as:
#' boreal, random areas, provinces and territories, or any of the last in the
#' boreal.
#'
#' @param testArea          Logical. Indicates if the test area should
#'                          be anything other than `NULL`. Default is `NULL`.
#' @param specificTestArea  A character string with a province or territory, or 'boreal'.
#'                          if boreal region (following Brandt et al., 2013) is wanted.
#'                          Default is `NULL`.
#' @param mapSubset         If specificTestArea is supplied as 'boreal', this can be set
#'                          as a character string with a province or territory that is contained
#'                          in the boreal or 'Canada' if the whole Canadian boreal is wanted.
#'                          Default is `NULL`.
#' @param destinationFolder Path to where to save downloaded files. Default is `tempdir()`.
#'
#' @author Tati Micheletti
#' @export
#' @importFrom reproducible prepInputs
#' @importFrom SpaDES.tools randomPolygon
#' @rdname defineStudyArea
#'
#' @examples
#' ## rp is the whole North American boreal region
#' rp <- defineStudyArea(testArea = TRUE, specificTestArea = "boreal", mapSubset = NULL)
#'
#' ## Alberta
#' rp <- defineStudyArea(testArea = TRUE, specificTestArea = "Alberta", mapSubset = NULL)
#'
#' \dontrun{
#' ## Alberta inside boreal extension
#' rp <- defineStudyArea(testArea = TRUE, specificTestArea = "boreal", mapSubset = "Alberta")
#' }
defineStudyArea <- function(testArea = NULL, specificTestArea = NULL, mapSubset = NULL,
                            destinationFolder = tempdir()) {
  rP <- NULL
  if (any(is.null(testArea), (!is.null(testArea) &
                              testArea == FALSE))) {
    if (!is.null(specificTestArea)) {
      warning(crayon::yellow(paste(
        "Test area is FALSE or NULL, but specificTestArea is not.",
        "Ignoring 'specificTestArea' and running the analysis without a study area.",
        "To set a study area, use testArea == TRUE.")))
    } else {
      message(crayon::yellow("Test area is FALSE or NULL. Running the analysis without a study area."))
    }
  } else {
    if (is.null(specificTestArea)) {
      polyMatrix <- matrix(c(-79.471273, 48.393518), ncol = 2)
      areaSize <- 10000000
      set.seed(1234)
      rP <- SpaDES.tools::randomPolygon(x = polyMatrix, hectares = areaSize) # Create Random polygon in southern Ontario
      message(crayon::yellow("Test area is TRUE, specificTestArea is 'NULL'. Cropping and masking to an area in south Ontario, Canada."))
    } else {
      if (specificTestArea == "boreal") {
        if (is.null(mapSubset)) {
          message(crayon::yellow("Test area is TRUE, specificTestArea is 'boreal', and mapSubset is NULL. Cropping and masking to the whole Boreal."))
          rP <- reproducible::prepInputs(url = "http://206.167.182.7/BAM/dataset/EnvironmentCanada/Brandt_boreal.zip",
                                         destinationPath = destinationFolder
          )
        }
        if (!is.null(mapSubset) && mapSubset != "Canada") {
          sA <- reproducible::prepInputs(url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip",
                                         targetFile = "gpr_000b11a_e.shp",
                                         alsoExtract = "similar",
                                         destinationPath = destinationFolder) %>%
            raster::subset(PRENAME %in% mapSubset)
          if (nrow(sA@data) == 0) {
            stop(paste0("There is no Canadian Province called ",
                        mapSubset,
                        ". Please provide a Canadian province name in English for subsetMap, ",
                        "or use 'NULL' (does not subset boreal, dangerous when dealing with higher resolution)."))
          }
          rP <- reproducible::prepInputs(url = "https://drive.google.com/open?id=0B_2riEic8l1mYW1SaVphNk9MaUdrRWhLYU1XUHdQcWhyMkxn",
                                         alsoExtract = "similar",
                                         studyArea = sA,
                                         destinationPath = destinationFolder)
          return(rP)
        } else {
          ## TODO: why is this empty?
        }
        if (!is.null(mapSubset) && mapSubset == "Canada") {
          message(crayon::yellow("Test area is TRUE. Cropping and masking to the Canadian Boreal."))
          rP <- reproducible::prepInputs(url = "https://drive.google.com/open?id=0B_2riEic8l1mYW1SaVphNk9MaUdrRWhLYU1XUHdQcWhyMkxn",
                           alsoExtract = "similar",
                           destinationPath = destinationFolder)
          return(rP)
        }
      } else {
        if (!is.null(specificTestArea)) {
          rP <- reproducible::prepInputs(url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip",
                           targetFile = "gpr_000b11a_e.shp",
                           alsoExtract = "similar",
                           destinationPath = destinationFolder) %>%
            raster::subset(PRENAME == specificTestArea)
          if (nrow(rP@data) == 0) {
            stop(paste0("There is no Canadian Province called ",
                        specificTestArea,
                        ". Please provide a Canadian province name in English for specificTestArea, ",
                        "use 'boreal', or use 'NULL' (creates a random area in South Ontario, Canada)."))
          } else {
            message(crayon::yellow(paste0("Test area is TRUE. Cropped and masked to ", specificTestArea)))
            return(rP)
          }
        }
      }
    }
  }
  return(rP)
}
