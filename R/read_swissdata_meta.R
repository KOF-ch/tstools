#' Read swissdata style yaml timeseries metadata
#'
#' read_swissdata_meta reads the given .yaml file and converts it into a
#' per-timeseries format.
#' 
#' If as_list is set to TRUE, the function returns a nested list with one
#' element per timeseries, otherwise a data.table with one row per series.
#'
#' @param path Path to the yaml file to be read
#' @param locale Locale in which to read the data (supported are "de", "fr", "it" and "en")
#' @param as_list Should the output be converted to a list?
#' @export
#'
read_swissdata_meta <- function(path, locale = "de", as_list = FALSE) {
  if(!grepl(".yaml$", path)) {
    path <- paste0(path, ".yaml")
  }
  
  set_name <- gsub("(.+)(.yaml)", "\\1", basename(path))
  
  meta <- yaml::read_yaml(path)
  
  dimnames_idx <- match("dimnames", names(meta$labels))
  meta_labels <- meta$labels[-dimnames_idx]
  meta_dimnames <- sapply(meta$labels$dimnames, `[[`, locale)
  n_dims <- length(meta_dimnames)
  
  # Enforce dim.order
  meta_dimorder <- meta$dim.order
  meta_labels <- meta_labels[match(names(meta_labels), meta_dimorder)]
  meta_dimnames <- meta_dimnames[match(names(meta_dimnames), meta_dimorder)]
  
  keychunks <- lapply(meta_labels, names)
  keys <- do.call(paste, c(
    expand.grid(keychunks, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE), # To understand, run expand.grid(list(1:2, 1:5))
    sep = "."
  ))
  keys <- paste(set_name, keys, sep = ".")
  
  # Now this is some serious R-Fu
  labels <- expand.grid(lapply(meta_labels, sapply, `[[`, locale), stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  
  # Work some dark magic to get units into it?
  
  # Is this smert? dimnames could be any old crazy strings
  names(labels) <- meta_dimnames
  
  out <- as.data.table(labels)
  
  per_set_dims <- yaml::yaml.load(
    "
        title:
          de: Datensatz
          fr: Datensatz
          it: Datensatz
          en: Dataset
        source:
          de: Quelle
          fr: Source
          it: La sourca (not correct)
          en: Source
        details:
          de: Details
          fr: Details
          it: Details
          en: Details
        utc.updated:
          de: Aktualisierungszeitpunkt
          fr: Aktualisierungszeitpunkt
          it: Aktualisierungszeitpunkt
          en: Aktualisierungszeitpunkt
    "
  )
  per_set_dims <- sapply(per_set_dims, `[[`, locale)
  
  n_dims <- n_dims + length(per_set_dims)
  
  meta$source <- paste0(meta$source.name[[locale]], " (", meta$source.url, ")")
  
  out[, (per_set_dims) := lapply(meta[names(per_set_dims)], function(x){if(is.list(x)) x[[locale]] else x})]
  
  if(as_list) {
    lapply(split(out, keys), as.list)
  } else {
    out[, ts_key := keys]
    setcolorder(out, c(n_dims + 1, 1:(n_dims)))
    out
  }
}
