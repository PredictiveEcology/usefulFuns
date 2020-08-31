#' Column binds a list of `data.frame`s or `data.table`s
#'
#' @param lst List of `data.table`s or `data.frame`s to column bind.
#'
#'
#' @author Tati Micheletti
#' @return Returns a merged `data.table` with unique columns
#' @export
#' @importFrom data.table data.table
#' @rdname cbindFromList
cbindFromList <- function(lst) {
  bindedList <- do.call(cbind, args = lst)
  bindedList <- data.table::data.table(bindedList)
  bindedList[, which(duplicated(names(bindedList))) := NULL]
  return(bindedList)
}
