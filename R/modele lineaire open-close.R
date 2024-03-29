#' Modelisation lineaire open close
#'
#' @param Ind_data_frame Le nom de l'indice version data frame
#'
#' @return le modele lineaire de l'indice qui predit la variable Close dependant de la variable Open
#'
#' @export
modele_lineaire_openclose <- function(Ind_data_frame){
  return(ggplot2::ggplot(Ind_data_frame, ggplot2::aes(x=open,y=close))+
           ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm"))
}
