utils::globalVariables(c("MDC_0", "MDC_m", "pixelID"))

#' Function to create raster stack for climate sensitive models
#'
#' Designed primarily for NWT project, but somewhat flexible.
#'
#' @param pathInputs Default NULL (TODO: description needed)
#'
#' @param variables Character string of the variables to be used, i.e. c("PPT", "Tmax").
#'
#' @param years Character string of the years to use. i.e. c(2011:20100).
#'
#' @param GDriveFolder Character string of the folder in google drive to upload the layers to.
#'                     Handy for shared projects.
#'
#' @param climateFilePath Character string of the path to the climate file in google drive
#'                        (i.e. "https://drive.google.com/open?id=1wcgytGJmfZGaapZZ9M9blfGa-45eLVWE" for
#'                        `Canada3ArcMinute.7z`)
#' @param fileResolution Character string of the for naming purposes (i.e. `3ArcMinute`)
#'
#' @param authEmail Character string of googledrive e.mail for authentication for non-interactive use.
#'
#' @param RCP Character string of RCP to be used (i.e. `45`)
#'
#' @param climateModel Character string of climate mode to be used (i.e. `CanESM2`)
#'
#' @param ensemble Character string of climate ensemble to be used (i.e. `r11i1p1`)
#'
#' @param rasterToMatch RasterLayer template for these layers to match
#'
#' @param studyArea shapefile of study area
#'
#' @param model For naming and shortcut for variables: ie. `birds` or `fireSense`.
#'              If you wanna provide the variables to be produced, don't use birds or fireSense here.
#'
#' @param droughtMonths numeric. Months for fireSense to calculate MonthlyDroughtCode (MDC) i.e. `4:9`.
#'
#' @param returnCalculatedLayersForFireSense Logical. Should it calculate MDC (TRUE) or return the original variables (FALSE)? Default is FALSE.
#'
#' @param yearsWithClimateProjections Numeric. The user can pass the years that have climate projection in the data. Default to 2011:2100.
#'
#' @param overwrite logical. Default to FALSE. Should the layers be overwritten if exist?
#'
#' @param backupFolder character. Default to NULL. If passed, if the unzipped file is not found, the
#'                     function will look for it in the backupFolder and copy over (faster).
#'                     Otherwise, it will follow the normal course (slower) of just unzipping
#'                     the .zip file if available
#'
#' @param overwriteOriginalData logical. Default to FALSE. If changes happen in the original layer
#'                              (the one provided in climateFilePath),
#'                              set this to TRUE to overwrite the zip files downloaded.
#'
#' @return This function returns a list of all years, with each year being the local path for the raster stack that contains all variables
#'
#' @author Tati Micheletti
#' @export
#' @importFrom crayon red green yellow
#' @importFrom data.table data.table
#' @importFrom googledrive drive_auth drive_get drive_ls
#' @importFrom raster crs crs<- getValues raster setValues stack
#' @importFrom reproducible assessDataType basename2 Cache postProcess preProcess
#' @importFrom stats na.omit
#' @importFrom tools file_path_sans_ext
#' @importFrom utils zip
#'
#' @include grepMulti.R
#' @rdname prepareClimateLayersWithBackup
prepareClimateLayersWithBackup <- function(pathInputs = NULL,
                                 variables = NULL,
                                 years = NULL,
                                 GDriveFolder = NULL,
                                 climateFilePath = NULL,
                                 fileResolution = NULL,
                                 authEmail = NULL,
                                 RCP = NULL, # 45
                                 climateModel = NULL, # CCSM ~CanESM2~ ==> On 21stNOV19 changed
                                 # to CCSM due to the "squareness" of CanESM2
                                 ensemble = NULL, # r11i1p1
                                 rasterToMatch = NULL,
                                 studyArea = NULL,
                                 model = NULL, # 'birds', 'fireSense'. If you wanna provide
                                 # other variables, don't use birds or fireSense here.
                                 droughtMonths = 4:9, # Months for fireSense to calculate MonthDoughtCode (MDC)
                                 returnCalculatedLayersForFireSense = FALSE,
                                 yearsWithClimateProjections = 2011:2100,
                                 overwrite = FALSE,
                                 overwriteOriginalData = FALSE,
                                 backupFolder = NULL){ # If TRUE, it returns the
  # calculated MDC layers already, not the original stack with Tmax and PPT


  googledrive::drive_auth(email = authEmail)
  # 1. Make sure it has all defaults
  if (!all(droughtMonths %in% 4:9)){
    stop("Drought calculation for Months other than April to June is not yet supported") # TODO
  }
  if (is.null(model)) {
    stop("Please provide the model for which you are creating the variables (i.e. 'birds', 'fireSense', 'NA')")
  }
  if (is.null(rasterToMatch)) {
    message("rasterToMatch is NULL, no post processing will happen")
  }
  if (is.null(ensemble)) {
    ensemble <- "r11i1p1"
    message("ensemble is NULL, using default ", ensemble)
  }
  if (is.null(RCP)) {
    RCP <- paste0("RCP", 45)
    message("RCP is NULL, using default ", RCP)
  }
  if (is.null(climateModel)) {
    climateModel <- "CanESM2"
    message("climateModel is NULL, using default ", climateModel)
  }
  if (is.null(GDriveFolder)) {
    GDriveFolder <- "https://drive.google.com/open?id=1Ww_GYtxB9ZALGjjJnML_F4Mgl8UR-zl9"
    message("GDriveFolder is NULL, using default folder ", GDriveFolder, ".",
            "Ignore this message if you have already the file locally.")
  }
  if (is.null(pathInputs)) {
    pathInputs <- tempdir()
    message(paste0("pathInputs is NULL, using default temp folder: ", pathInputs))
  }
  if (is.null(climateFilePath)){
    climateFilePath <- "https://drive.google.com/open?id=1wcgytGJmfZGaapZZ9M9blfGa-45eLVWE"
    threeArcMin <- TRUE
    message("This function uses CanESM2 RCP45 resolution 3ArcMinute (~3 x 5Km).",
            "\nFile downloaded from climateFilePath URL: ", climateFilePath,").",
            "\nIf another layer is intended, please provide the URL to the file.",
            "\n(This function does not download the files from ClimateNA)")
  }
  if (is.null(fileResolution)) { # For naming purposes mostly. Should only be passed if user is also passing the path to the complete file
    if (threeArcMin){
      fileResolution <- "3ArcMin"
      message("fileResolution is NULL. Using the original")
    } else {
      stop("Please inform the resolution in fileResolution when passing a climate archive file path")
    }
  }
  if (is.null(years)){
    years <- 2011:2100
    message("Years is NULL. Using the original time series (2011-2100)")
  }
  if (all(is.null(variables), !model %in% c("birds", "fireSense"))){
    variables <- c("AHM", "bFFP", "CMD01", "CMD02", "CMD03", "CMD04", "CMD05",
                   "CMD06", "CMD07", "CMD08", "CMD09", "CMD10", "CMD11", "CMD12",
                   "CMD", "CMD_at", "CMD_sm", "CMD_sp", "CMD_wt", "DD_0_01", "DD_0_02",
                   "DD_0_03", "DD_0_04", "DD_0_05", "DD_0_06", "DD_0_07", "DD_0_08",
                   "DD_0_09", "DD_0_10", "DD_0_11", "DD_0_12", "DD_0", "DD_0_at",
                   "DD_0_sm", "DD_0_sp", "DD_0_wt", "DD_18_01", "DD18_01", "DD_18_02",
                   "DD18_02", "DD_18_03", "DD18_03", "DD_18_04", "DD18_04", "DD_18_05",
                   "DD18_05", "DD_18_06", "DD18_06", "DD_18_07", "DD18_07", "DD_18_08",
                   "DD18_08", "DD_18_09", "DD18_09", "DD_18_10", "DD18_10", "DD_18_11",
                   "DD18_11", "DD_18_12", "DD18_12", "DD_18", "DD18", "DD_18_at",
                   "DD18_at", "DD_18_sm", "DD18_sm", "DD_18_sp", "DD18_sp", "DD_18_wt",
                   "DD18_wt", "DD5_01", "DD5_02", "DD5_03", "DD5_04", "DD5_05",
                   "DD5_06", "DD5_07", "DD5_08", "DD5_09", "DD5_10", "DD5_11", "DD5_12",
                   "DD5", "DD5_at", "DD5_sm", "DD5_sp", "DD5_wt", "eFFP", "EMT",
                   "Eref01", "Eref02", "Eref03", "Eref04", "Eref05", "Eref06", "Eref07",
                   "Eref08", "Eref09", "Eref10", "Eref11", "Eref12", "Eref", "Eref_at",
                   "Eref_sm", "Eref_sp", "Eref_wt", "EXT", "FFP", "MAP", "MAR",
                   "MAT", "MCMT", "MSP", "MWMT", "NFFD01", "NFFD02", "NFFD03", "NFFD04",
                   "NFFD05", "NFFD06", "NFFD07", "NFFD08", "NFFD09", "NFFD10", "NFFD11",
                   "NFFD12", "NFFD", "NFFD_at", "NFFD_sm", "NFFD_sp", "NFFD_wt",
                   "PAS01", "PAS02", "PAS03", "PAS04", "PAS05", "PAS06", "PAS07",
                   "PAS08", "PAS09", "PAS10", "PAS11", "PAS12", "PAS", "PAS_at",
                   "PAS_sm", "PAS_sp", "PAS_wt", "PPT01", "PPT02", "PPT03", "PPT04",
                   "PPT05", "PPT06", "PPT07", "PPT08", "PPT09", "PPT10", "PPT11",
                   "PPT12", "PPT_at", "PPT_sm", "PPT_sp", "PPT_wt", "Rad01", "Rad02",
                   "Rad03", "Rad04", "Rad05", "Rad06", "Rad07", "Rad08", "Rad09",
                   "Rad10", "Rad11", "Rad12", "Rad_at", "Rad_sm", "Rad_sp", "Rad_wt",
                   "RH01", "RH02", "RH03", "RH04", "RH05", "RH06", "RH07", "RH08",
                   "RH09", "RH10", "RH11", "RH12", "RH", "RH_at", "RH_sm", "RH_sp",
                   "RH_wt", "SHM", "Tave01", "Tave02", "Tave03", "Tave04", "Tave05",
                   "Tave06", "Tave07", "Tave08", "Tave09", "Tave10", "Tave11", "Tave12",
                   "Tave_at", "Tave_sm", "Tave_sp", "Tave_wt", "TD", "Tmax01", "Tmax02",
                   "Tmax03", "Tmax04", "Tmax05", "Tmax06", "Tmax07", "Tmax08", "Tmax09",
                   "Tmax10", "Tmax11", "Tmax12", "Tmax_at", "Tmax_sm", "Tmax_sp",
                   "Tmax_wt", "Tmin01", "Tmin02", "Tmin03", "Tmin04", "Tmin05",
                   "Tmin06", "Tmin07", "Tmin08", "Tmin09", "Tmin10", "Tmin11", "Tmin12",
                   "Tmin_at", "Tmin_sm", "Tmin_sp", "Tmin_wt")
    message(crayon::red(paste0("variables is NULL, using all variables ",
                               "available in the climate layers. ",
                               "ATTENTION! This might be very slow!",
                               " Total number of variables loaded: ", length(variables))))
  }
  if (model == "birds"){
    variables <- c("AHM", "bFFP", "CMD", "DD_0", "DD_18",
                   "DD18", "DD5", "eFFP", "EMT", "EXT", "FFP",
                   "MAP", "MAT", "MCMT", "MSP", "MWMT", "NFFD",
                   "PAS", "PPT_sm", "PPT_wt", "SHM", "Tave_sm",
                   "Tave_wt", "TD")
  }
  if (model == "fireSense"){
    variables <- c(paste0("Tmax0", droughtMonths),
                   paste0("PPT0", droughtMonths))
  }

  # 2. Check if we have the years chosen (we should lapply through years)
  yearsList <- lapply(X = years, FUN = function(y) {
    # Check if the year is in the climate projection range. If not, don't
    # even bother, return a raster of NA's
    if (!y %in% 2011:2100) {
      message(red(paste0("The year", y,
                         "does not have climate projections. Returning NULL")))
      return(NULL)
    }
    if (all(model == "fireSense",
            isTRUE(returnCalculatedLayersForFireSense),
            !isTRUE(overwriteOriginalData))){
      fileName <- file.path(pathInputs, paste0(paste(climateModel, RCP, ensemble,
                                                     fileResolution, model,
                                                     "Calc", y, sep = "_"),
                                               ".grd"))
    } else {
      fileName <- file.path(pathInputs, paste0(paste(climateModel, RCP, ensemble,
                                                     fileResolution, model,
                                                     y, sep = "_"), ".grd"))
      dType <- "INT4S"
    }

    if (all(file.exists(fileName), !isTRUE(overwrite))) {
      message(green(paste0(fileName, " exists. Returning the raster stack")))
      # A. If we have the year, return
      return(stack(fileName))
    } else
      # B0. Check if we have the unzipped files in the backupFolder
      if (all(!is.null(backupFolder),
              !isTRUE(overwrite))){
        fileNameBackup <- sub(pattern = dirname(fileName),
                              replacement = backupFolder,
                              x = fileName)
        if (file.exists(fileNameBackup)){
          lapply(c(".grd",".gri"), function(extnt){
            system(paste0("cp ", paste0(file_path_sans_ext(fileNameBackup), extnt),
                          " ", dirname(fileName)))
          })
        }
        if (file.exists(fileName)){ # If the files are  there, return

          return(stack(fileName))
        } # Otherwise, if the files are NOT there (i.e. have been deleted, try the zips)
        message(yellow(paste0(fileName, " unzipped in the backupFolder was not found.",
                              " Trying a second approach...")))
      }
      if (all(file.exists(paste0(file_path_sans_ext(fileName), ".zip")),
                   !isTRUE(overwrite))) {
      message(green(paste0(fileName, " as a zipfile exists locally.",
                                 " Will be unzipped and returned as rasterStack. ")))
      fileNameZip <- paste0(file_path_sans_ext(fileName), ".zip")
      system(paste0("unzip -j ", fileNameZip, " ",
                      paste0("*", tools::file_path_sans_ext(basename2(fileNameZip)),
                             "*"),
                      " -d ", pathInputs))
      return(stack(fileName))
    } else { # If the zipfile doesn't exist or if overwrite
      if (all(!is.null(backupFolder),
              !isTRUE(overwrite))){
        fileNameZip <- paste0(file_path_sans_ext(fileName), ".zip")
        fileNameBackup <- sub(pattern = dirname(fileNameZip),
                              replacement = backupFolder,
                              x = fileNameZip)
        if (file.exists(fileNameBackup)){
          system(paste0("cp ", fileNameBackup, " ", dirname(fileNameZip)))
          system(paste0("unzip -j ", fileNameZip, " ",
                        paste0("*", tools::file_path_sans_ext(basename2(fileNameZip)),
                               "*"),
                        " -d ", pathInputs))
        }
        if (file.exists(fileName)){
          return(stack(fileName))
        }
        message(yellow(paste0(fileName, " zipped in the backupFolder was not found.",
                              " Trying a second approach...")))
      }
      # B. If we don't have the year LOCALLY, see if we have in the cloud
      message(yellow(paste0(fileName, " does not exist locally or should be overwritten. ",
                            "Checking the cloud... ")))
      filesInFolder <- drive_ls(path = as_id(GDriveFolder), recursive = FALSE)
      if (all(paste0(basename2(file_path_sans_ext(fileName)), ".zip") %in% filesInFolder$name,
              !isTRUE(overwrite))){
        rw <- which(filesInFolder$name == paste0(basename2(tools::file_path_sans_ext(fileName)),
                                                 ".zip"))
        # googledrive::drive_download(file = as_id(filesInFolder$id[rw]),
        #                             path = file.path(pathInputs,
        #                             paste0(basename2(tools::file_path_sans_ext(fileName)),
        #                             ".zip")))
        preProcess(url = paste0("https://drive.google.com/open?id=", filesInFolder$id[rw]),
                   alseExtract = "similar",
                   archive = paste0(basename2(file_path_sans_ext(fileName)), ".zip"),
                   destinationPath = pathInputs)
        # Currently not really working. Giving error:
        # Error in grepl(archive, pattern = destinationPathUser) :
        #   object 'destinationPathUser' not found
        message(green(paste0(fileName, " now exists locally. Returning the raster ",
                             "stack with the following variables: ")))
        message(green(paste(variables, collapse = ", ")))

        return(stack(fileName))
      } else {
        # B1. If we don't have it in the cloud, (use the years in file name),
        # make it from the original layer.
        message(yellow(paste0(fileName, " does not exist locally nor in the cloud or needs ",
                              "to be overwritten. Creating layers... ")))
        fullDatasetName <- drive_get(as_id(climateFilePath))$name
        if (any(!file.exists(file.path(pathInputs, fullDatasetName)),
                isTRUE(overwriteOriginalData))) {
          message(red(paste0(fullDatasetName, " does not exist in your pathInputs (", pathInputs,
                             ") or needs to be overwritten. Downloading, unzipping ",
                             "and creating layers... This might take a few hours")))
          preProcess(url = climateFilePath,
                     targetFile = "MAP.asc", # targetFile just to avoid error
                     filename2 = fullDatasetName,
                     destinationPath = pathInputs)
          # 26NOV19: preProcess works, but need to specify targetFile
          # # TEMPORARY SYSTEM CALL WITH THE OPTION x - this assumes you have
          # '7za'. Possibly not necessary as preProcess seems to be working. Testing again.
          # system(paste0("7za x ", file.path(pathInputs, fullDatasetName))) # ==> This hasn't
          # been tested with the full file path. Just guessing it works... done by hand
        }
        datasetsPath <- file.path(pathInputs, file_path_sans_ext(fullDatasetName))
        folders <- setdiff(list.dirs(path = datasetsPath), datasetsPath)
        # excluding original folder from the variable
        currentYearsFolder <- grepMulti(x = folders,
                                        patterns = c(climateModel, RCP,
                                                     ensemble, y))
        currentYearFiles <- list.files(currentYearsFolder)
        filesToLoad <- paste0(variables, ".asc")
        variablesStack <- stack(lapply(X = filesToLoad, FUN = function(variable){
          ras <- raster(x = file.path(currentYearsFolder, variable))
          crs(ras) <- raster::CRS(paste0('+init=epsg:4326 +proj=longlat +ellps=WGS84 ",
                                  "+datum=WGS84 +no_defs +towgs84=0,0,0'))
          if (any(!is.null(rasterToMatch),
                  !is.null(studyArea)))
            ras <- postProcess(x = ras,
                               studyArea = studyArea,
                               rasterToMatch = rasterToMatch,
                               filename2 = NULL)
          return(ras)
        })
        )
        # Fixing the layers for the values that were multiplied by 10 in ClimateNA V6.11
        # The variables to potentially fix are:
        # •	Annual: MAT, MWMT, MCMT, TD, AHM, SHM, EMT, EXT and MAR;
        # •	Seasonal: Tmax, Tmin, Tave and Rad;
        # •	Monthly: Tmax, Tmin, Tave and Rad.'
        variablesStack <- raster::stack(lapply(names(variablesStack), function(lay) {
          if (lay %in% c("MAT", "MWMT", "MCMT", "TD", "AHM", "SHM", "EMT", "EXT", "MAR",
                         paste0("Tmax0", droughtMonths),
                         paste0("Tmin0", droughtMonths),
                         paste0("Tave0", droughtMonths),
                         paste0("Rad0", droughtMonths),
                         paste0("Rad_", c("wt", "sm", "at", "sp")),
                         paste0("Tmax_", c("wt", "sm", "at", "sp")),
                         paste0("Tmin_", c("wt", "sm", "at", "sp")),
                         paste0("Tave_", c("wt", "sm", "at", "sp")))) {
            message(crayon::red(paste0("ClimateNA 6.11 multiplies ", lay, "by 10 for storage.",
                                       "Backtransforming the layer")))
            variablesStack[[lay]] <- variablesStack[[lay]]/10
            return(variablesStack[[lay]])
          } else {
            return(variablesStack[[lay]])
          }
        }))

        # For fireSense, do the calculations already'
        if (all(model == "fireSense", isTRUE(returnCalculatedLayersForFireSense))){
          # Day length adjustement L_f in Drought Code (taken from Van Wagner 1987)
          L_f <- function(Month){
            c('4' = 0.9,
              '5' = 3.8,
              '6' = 5.8,
              '7' = 6.4,
              '8' = 5.0,
              '9' = 2.4)[[as.character(Month)]]
            # TODO [ FIX ] Update for all Months,
            # check latitude problem.
            # Ideally, bring original table in here.
          }

          nDays <- function(Month){
            c('4' = 30,
              '5' = 31,
              '6' = 30,
              '7' = 31,
              '8' = 31,
              '9' = 30)[[as.character(Month)]]
          }

          # remove the variables from rasterStack for faster operations
          dt <- na.omit(data.table(raster::getValues(variablesStack),
                                   pixelID = 1:ncell(variablesStack)))
          dt[,MDC_0 := 0]
          for (Month in droughtMonths){
            dt[, MDC_m := pmax(
              MDC_0 + .25 * nDays(Month) *
                (.36 * eval(parse(
                  text = paste0("Tmax0", Month)
                )) +
                  L_f(Month)) -
                400 * log(1 + 3.937 * .83 *
                            eval(parse(
                              text = paste0("PPT0", Month)
                            )) /
                            (800 * exp(-MDC_0 / 400))) +
                .25 * nDays(Month) * (.36 * eval(parse(
                  text = paste0("Tmax0", Month)
                )) + L_f(Month)),
              0
            )]
            dt[, MDC_0 := pmax((MDC_0 + MDC_m) / 2, 0)]
          }
          # Set new raster variable to raster
          MDC <- merge(data.table(pixelID = 1:ncell(variablesStack)),
                       dt[, c("pixelID", "MDC_0")], by = "pixelID", all.x = TRUE)
          setkey(MDC, pixelID)
          variablesStack <- raster::setValues(x = variablesStack[[1]],
                                              values = MDC$MDC_0)
          names(variablesStack) <- paste("MDC", y)
          dType <- assessDataType(variablesStack)
        }
        writeRaster(variablesStack,
                    filename = fileName,
                    datatype = dType,
                    overwrite = overwrite)
        variablesStack <- stack(fileName)
        filesToUpload <- grepMulti(x = list.files(dirname(fileName), full.names = TRUE),
                                   patterns = basename(tools::file_path_sans_ext(fileName)))
        zip(zipfile = file_path_sans_ext(fileName), files = filesToUpload)
        drive_upload(media = paste0(tools::file_path_sans_ext(fileName), ".zip"),
                     path = googledrive::as_id(GDriveFolder))
        return(raster::stack(fileName))
      }
      writeRaster(variablesStack, filename = fileName,
                  datatype = dType,
                  overwrite = overwrite)
      variablesStack <- stack(fileName)
      filesToUpload <- grepMulti(x = list.files(dirname(fileName),
                                                full.names = TRUE),
                                 patterns = basename(tools::file_path_sans_ext(fileName)))
      zip(zipfile = file_path_sans_ext(fileName),
          files = filesToUpload)
      drive_upload(media = paste0(tools::file_path_sans_ext(fileName),
                                  ".zip"),
                                path = googledrive::as_id(GDriveFolder))
      return(raster::stack(fileName))
    }
  })
  names(yearsList) <- paste0("year", years)
  return(yearsList)
}
