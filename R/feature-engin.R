#' Feature Engineering
#'
#' @param nomInd Le nom de l'indice
#' @param annee l'année
#'
#' @return Générer les nouvelles variables pour une période donnée
#'
#' @examples
#' resultat_DJI <- feature_engin(DJI,'2021::')
#' colnames(resultat_DJI) <- c("Rendement quotidien",
#'"Rendement hebdomadaire",
#'"Rendement mensuel",
#'"Rendement annuel",

#'"log return quotidien",
#'"log return hebdo",
#'"log return mensuel",
#'"log return annuel",

#'"Prix cloture moyen de la semaine",
#'"Prix cloture min de la semaine",
#'"Prix cloture max de la semaine",
#'"Prix cloture moyen de la semaine",
#'"Médiane du prix de cloture de la semaine",
#'"Volume moyen de la semaine",

#'"Prix cloture moyen du mois",
#'"Prix cloture min du mois",
#'"Prix cloture max du mois",
#'"Prix cloture moyen du mois",
#'"Médiane du prix de cloture du mois",
#'"Volume moyen du mois",

#'"Prix cloture moyen de l'année",
#'"Prix cloture min de l'année",
#'"Prix cloture max de l'année",
#'"Prix cloture moyen de l'année",
#'"Médiane du prix de cloture de l'année",
#'"Volume moyen de l'année")

#' View(resultat_DJI)
#' @export
feature_engin <- function(nomInd,annee){
  cbind(
    quantmod::dailyReturn(quantmod::Cl(nomInd),subset=annee),
    quantmod::weeklyReturn(quantmod::Cl(nomInd),subset=annee),
    quantmod::monthlyReturn(quantmod::Cl(nomInd),subset=annee),
    quantmod::yearlyReturn(quantmod::Cl(nomInd),subset=annee),

    xts::apply.daily(diff(log(quantmod::Cl(nomInd)))[-1], FUN = sum),
    xts::apply.weekly(diff(log(quantmod::Cl(nomInd)))[-1], FUN = sum),
    xts::apply.monthly(diff(log(quantmod::Cl(nomInd)))[-1], FUN = sum),
    xts::apply.yearly(diff(log(quantmod::Cl(nomInd)))[-1], FUN = sum),

    xts::apply.weekly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.weekly(quantmod::Cl(nomInd),FUN = function(x){min(x)}),
    xts::apply.weekly(quantmod::Cl(nomInd),FUN = function(x){max(x)}),
    xts::apply.weekly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.weekly(quantmod::Cl(nomInd),FUN = function(x){median(x)}),
    xts::apply.weekly(quantmod::Vo(nomInd),FUN = function(x){mean(x)}),

    xts::apply.monthly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.monthly(quantmod::Cl(nomInd),FUN = function(x){min(x)}),
    xts::apply.monthly(quantmod::Cl(nomInd),FUN = function(x){max(x)}),
    xts::apply.monthly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.monthly(quantmod::Cl(nomInd),FUN = function(x){median(x)}),
    xts::apply.monthly(quantmod::Vo(nomInd),FUN = function(x){mean(x)}),

    xts::apply.yearly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.yearly(quantmod::Cl(nomInd),FUN = function(x){min(x)}),
    xts::apply.yearly(quantmod::Cl(nomInd),FUN = function(x){max(x)}),
    xts::apply.yearly(quantmod::Cl(nomInd),FUN = function(x){mean(x)}),
    xts::apply.yearly(quantmod::Cl(nomInd),FUN = function(x){median(x)}),
    xts::apply.yearly(quantmod::Vo(nomInd),FUN = function(x){mean(x)}))

}
