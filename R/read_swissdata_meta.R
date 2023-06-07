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
#' @importFrom yaml read_yaml yaml.load
#' @export
read_swissdata_meta <- function(path, locale = "de", as_list = FALSE) {
  # avoid them CRAN NOTEs on data.table
  ts_key <- NULL
  if (grepl("yaml$", path)) {
    if (file.exists(path)) {
      set_name <- gsub(".yaml$", "", basename(path))
      meta <- read_yaml(path)
    } else {
      stop(sprintf("Could not find file %s!", path))
    }
  } else if (grepl("json$", path)) {
    if (file.exists(path)) {
      set_name <- gsub(".json$", "", basename(path))
      meta <- jsonlite::fromJSON(path)
    } else {
      stop(sprintf("Could not find file %s!", path))
    }
  } else {
    set_name <- basename(path)
    meta <- .read_swissdata_meta_unknown_format(path)
  }

  if (length(meta) == 0) {
    stop("No metadata found!")
  }

  dimnames_idx <- match("dimnames", names(meta$labels))
  meta_labels <- meta$labels[-dimnames_idx]
  meta_dimnames <- sapply(meta$labels$dimnames, `[[`, locale)

  # Override column names for which no name is provided
  # (these are likely NULL and won't show up in the output anyway)
  missing_dimnames <- sapply(meta_dimnames, function(x) {
    is.null(x) || nchar(x) == 0 || x == "---" || is.list(x)
  })
  meta_dimnames[missing_dimnames] <- names(meta_dimnames)[missing_dimnames]
  n_dims <- length(meta_dimnames)

  # Enforce dim.order
  meta_dimorder <- meta$dim.order
  meta_labels <- meta_labels[match(names(meta_labels), meta_dimorder)]
  meta_dimnames <- meta_dimnames[match(meta_dimorder, names(meta_dimnames))]

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

  # prettify NULL columns
  null_cols <- sapply(labels, function(x) {
    sum(sapply(x, is.null)) == length(x)
  })
  if (any(null_cols)) {
    null_col_names <- names(null_cols)[null_cols]
    labels[null_col_names] <- NA_character_
  }


  out <- as.data.table(labels)

  if (!is.null(meta$source.url)) {
    meta$source <- paste0(meta$source.name[[locale]], " (", meta$source.url, ")")
  } else {
    meta$source <- meta$source.name[[locale]]
  }

  per_set_dims <- yaml.load(
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
          en: Last update
    "
  )
  per_set_dims <- sapply(per_set_dims, `[[`, locale)

  # Keep only those that appear in the data (e.g. details is optional)
  per_set_dims <- per_set_dims[intersect(names(per_set_dims), names(meta))]

  n_dims <- n_dims + length(per_set_dims)

  out[, (per_set_dims) := lapply(meta[names(per_set_dims)], function(x) {
    if (is.list(x)) x[[locale]] else x
  })]

  if (as_list) {
    lapply(split(out, keys), as.list)
  } else {
    n_dims <- ncol(out)
    out[, ts_key := keys]
    setcolorder(out, c(n_dims + 1, 1:(n_dims)))
    out
  }
}

#' Read Meta Data File w/o File Extension
#'
#' Read a meta file without extension -> unknown format
#' Tries to determine format (yaml, json) and return the metadata
#' path must point to the file without extension e.g. swissdata_wd/set_id/set_id
#' @param path character file path.
#' @importFrom yaml read_yaml
#' @importFrom jsonlite fromJSON
#' @return Meta list if file could be located, empty list otherwise
.read_swissdata_meta_unknown_format <- function(path) {
  set_id <- basename(path)
  meta_formats <- c("yaml", "json")
  existing_meta_files <- file.exists(file.path(dirname(path), sprintf("%s.%s", set_id, meta_formats)))
  names(existing_meta_files) <- meta_formats

  if (existing_meta_files["yaml"]) {
    meta <- read_yaml(paste0(path, ".yaml"))
  } else if (existing_meta_files["json"]) {
    meta <- fromJSON(paste0(path, ".json"))
  } else {
    meta <- list()
  }

  meta
}
