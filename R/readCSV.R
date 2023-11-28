#' A function to read in a csv file
#'
#' @param csv A csv file
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' myRead <- ("SPRUCE.csv")
myRead <- function(csv) {
  obj <- read.csv(csv);
}
