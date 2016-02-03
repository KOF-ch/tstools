#' Vector of Swiss cantons
#'
#' @param full logical full names or two letter classification. defaults to FALSE.
#' @export
cantons <- function(full=F){
  if(full){
    c("Argau",
      "Appenzell Ausserrhoden",
      "Appenzell Innerrhoden",
      "Basel-Landschaft",
      "Basel-Stadt",
      "Bern",
      "Fribourg",
      "Geneva",
      "Glarus",
      "Graubünden",
      "Jura",
      "Luzern",
      "Neuchatel",
      "Nidwalden",
      "Obwalden",
      "Schaffhausen",
      "Schwyz",
      "Solothurn",
      "St.Gallen",
      "Thurgau",
      "Ticino",
      "Uri",
      "Valais",
      "Vaud",
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
