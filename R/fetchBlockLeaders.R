#' Fetch the government's lider ID in the Senate 
#' 
#' @param The bloc string 
#' 
#' @importFrom xml2 xml_find_all xml_text xml_find_first
#' @importFrom purrr map_df
#' @importFrom dplyr filter
#' @importFrom RCurl getURL
#' @importFrom stringr str_match
#' 
#' @examples 
#' \dontrun{
#' fetchGovBlocLeader(bloc="Governo")
#' 
#'  fetchGovBlocLeader(bloc="Maioria")
#' 
#' fetchGovBlocLeader(bloc="Minoria")
#' 
#' fetchGovBlocLeader(bloc="Bloco")
#' }
#' 
#' @export
fetchGovBlocLeader <- function(bloc = "Governo"){
suppressMessages(library(tidyverse))
suppressMessages(library(RCurl))
suppressMessages(library(xml2))
suppressMessages(library(stringr))

  xml <- getURL("https://legis.senado.leg.br/dadosabertos/plenario/lista/liderancas") %>% xml2::read_xml()
  lideranca <- xml2::xml_find_all(xml, ".//DadosLiderancas/Lideranca") %>%
    purrr::map_df(function(x) {
    list(
SiglaUnidLideranca = xml_find_first(x, "./SiglaUnidLideranca") %>% xml_text(),
CodigoParlamentar = xml_find_first(x, ".//CodigoParlamentar") %>% xml_text(),
NomeParlamentar = xml_find_first(x, ".//NomeParlamentar") %>% xml_text()
      )
      
})
  
  if(!is.null(bloc)){
    lideranca <- lideranca %>% 
      dplyr::filter(stringr::str_detect(SiglaUnidLideranca, bloc))
  return(lideranca)
  }
  return(lideranca)
}
NULL