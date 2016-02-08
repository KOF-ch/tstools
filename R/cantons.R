#' Vector of Swiss cantons
#'
#' @param full logical full names or two letter classification. defaults to FALSE.
#' @export
cantons <- function(full=F){
  if(full){
    c("Aargau",
      "Appenzell Ausserrhoden",
      "Appenzell Innerrhoden",
      "Basel-Land",
      "Basel-Stadt",
      "Bern",
      "Fribourg",
      "Genf",
      "Glarus",
      "Graubünden",
      "Jura",
      "Luzern",
      "Neuenburg",
      "Nidwalden",
      "Obwalden",
      "Schaffhausen",
      "Schwyz",
      "Solothurn",
      "St. Gallen",
      "Thurgau",
      "Tessin",
      "Uri",
      "Wallis",
      "Waadt",
      "Zug",
      "Zürich"
    )
  } else {
    c("AG",
      "AR",
      "AI",
      "BL",
      "BS",
      "BE",
      "FR",
      "GE",
      "GL",
      "GR",
      "JU",
      "LU",
      "NE",
      "NW",
      "OW",
      "SH",
      "SZ",
      "SO",
      "SG",
      "TG",
      "TI",
      "UR",
      "VS",
      "VD",
      "ZG",
      "ZH")
  }
  
}


