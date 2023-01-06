#' Extraction des données
#'
#' @param date_debut La date de début de la période
#' @param date_fin La date de fin de la période
#' @param NomIndice Le nom de l'indice
#'
#' @return les données boursières entre la date de début et la date de fin
#'
#' @examples
#' DJI <- dataFinaExtract(NomIndice = "^DJI",date_debut = "2021-12-01",date_fin = "2022-01-01")
#' colnames(DJI) <- c("open","high","low","close","adj_close","volume")
#'
#' View(DJI)
#'
#' pour la version data frame:
#'
#' df_DJI <- data.frame((dataFinaExtract(NomIndice = "^DJI",date_debut = "2021-01-01",date_fin = "2022-01-01")))
#' colnames(df_DJI) <- c("open","high","low","close","adj_close","volume")
#'
#' View(df_DJI)
#' @export
dataFinaExtract <-function(date_debut,date_fin,NomIndice){
  return(pdfetch::pdfetch_YAHOO(NomIndice, from=as.Date(date_debut), to = as.Date(date_fin)))
}




