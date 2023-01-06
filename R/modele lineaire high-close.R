#' Modelisation lineaire high close
#'
#' @param Ind_data_frame Le nom de l'indice version data frame
#'
#' @return le modele lineaire de l'indice qui predit la variable Close dependant de la variable High
#'
#' @export
modele_lineaire_highclose <- function(Ind_data_frame){
  return(ggplot2::ggplot(Ind_data_frame, ggplot2::aes(x=high,y=close))+
           ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm"))
}
