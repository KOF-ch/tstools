#' Read Meta Information from a Swissdata type of yaml file
#' 
#' Read yaml meta information from swissdata type of meta information
#' and return an R list. 
#' 
#' @param path character file path to swissdata yaml file.
#'@importFrom yaml yaml.load_file
#'@export
read_swissdata_meta <- function(path){
  dataset <- gsub("\\.yaml","",basename(path))
  # we had a discussion against yaml.read in favor 
  # of yaml load on swissdata, but that is only the case
  # if we use a text string inside R. In this case we
  # need to read it from a file. However, swissdata should 
  # make sure yaml files are UTF-8.
  y <- yaml.load_file(path)
  attr(y,"dataset") <- dataset
  y
}



