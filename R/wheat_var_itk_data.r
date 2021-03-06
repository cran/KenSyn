#' @name wheat_var_itk
#' @title Network of experiment to evaluate Wheat varieties on one single year with different cropping systems.
#' @description
#' The data set consists of measurements of wheat yields from a network of field experiments with different cropping systems
#' designed to evaluate and compare performances of different varieties or varietal mixtures.
#' This extract consists of 5 different experiments corresponding to five different places and 12 varieties or varietal mixtures.
#' ITK2 cropping system : "normal" inputs according to regional recommandations
#' ITK3 cropping system : aims for a lower yield goal with reduced inputs (seeding rates, nitrogen fertilization (-30 uN / ha) and reduced protection against diseases and pests)
#' The experimental designs used are complete random block designs  TODO : CHECK
#' The number of repetitions in each experiment is equal to three.
#' @docType data
#' @usage wheat_var_itk
#' @format a \code{RangedData} instance, 1 row per measurement. annee : year, lieu : place, experimentation : experiment name, variete : variety, bloc : block of the design, rdt : Wheat Yield (ton/hectare)
#' @source Arvalis - institut du vegetal, INRA, Chambres Agricultures et CIVAM, real data, but anonymized (place and variety). 
#' Description of complete database : Felix et al. (2016)
#' @examples
#' summary(wheat_var_itk)
NULL
