read_meta_json <- function(keys, locale) {
  # read metadata json from DB

  ###################
  # DEV STUFFSES
  ###################
  meta <- yaml::read_yaml("C:/dev/R_repos/swissdata/wd/ch.bfs.besta/ch.bfs.besta.yaml")
  locale <- "de"
  keys_without_prefix <- keys
  
  # Get "set global" metadata
  out <- meta[c("title", "source.name", "source.url", "units", "aggregate", "details", "utc.updated")]
  
  # Helper to extract desired locale and set NULLs to NA
  get_localized <- function(x) {
    if(is.list(x)) {
      if(is.null(x[[1]])) {
        NA
      } else if(locale %in% names(x)) {
        x[[locale]]
      } else {
        x
      }
    } else {
      x
    }
  }
  
  # Map from dimnames to localized labels
  dim_names <- lapply(meta$labels$dimnames, get_localized)
  
  # Split key suffix into chunks for getting metadata
  chunks <- unlist(strsplit(keys_without_prefix, "\\."))
  
  n <- length(chunks)
  
  # Since a given value for chunks can appear in multiple dimensions
  # we need to figure out a mapping that covers all dimensions
  
  # a map of which chunks may belong to each dimension
  map <- lapply(names(meta$hierarchy), function(x) {
    chunks %in% names(meta$labels[[x]])
  })

  # convert map to a labelled matrix
  map <- matrix(unlist(map), nrow = n)
  rownames(map) <- chunks
  colnames(map) <- names(meta$hierarchy)
  map[map == TRUE] <- 1
  
  # Loop over map columnwise, removing items where the column (dimension) has
  # multiple candidate chunks
  for(i in 1:n) {
    rs <- rowSums(map)
    cs <- colSums(map)
    for(j in 1:n) {
      # We need to remove items (no 1-1 mapping yet)
      if(cs[j] > 1) {
        # Find all rows where it's safe to remove 1s from the jth column
        ind <- map[, j] > 0 & rs > 1
        
        # Ensure we keep at least one
        if(sum(ind) > 1) {
          first_one <- min(which(map[, j] == 1))
          ind[first_one] <- FALSE
        }
        
        # Set all to 0
        map[which(ind), j] <- 0
        
        # Update rowsums
        rs <- rowSums(map)
      }
    }
  }
  
  # Assign metadata
  dims <- colnames(map)
  dims_localized <- unlist(dim_names[dims])
  chunks2 <- rownames(map)
  
  for(i in 1:n) {
    out[dims_localized[i]] <- meta$labels[[dims[i]]][chunks2[which(map[,i] > 0)]]
  }

  # Extract the desired locale
  lapply(out, get_localized)
}


####
# DEV STUFF
####


keys <- "ch.bfs.besta.3.59.1"
key_without_prefix <- "3.59.1"
