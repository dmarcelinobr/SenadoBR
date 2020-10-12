#' Fetch the government's lider ID in the Senate 
#' 
#' @importFrom xml2 xml_find_all xml_text xml_find_first
#' @importFrom purrr map_df
#' @importFrom dplyr filter
#' @importFrom RCurl getURL
#' 
#' @examples 
#' \dontrun{
#' lider = fetchGovBlocLeader()
#' }
#' 
#' @export
fetchGovBlocLeader <- function(){
suppressMessages(library(tidyverse))
suppressMessages(library(RCurl))
suppressMessages(library(xml2))
  
  xml <- getURL("https://legis.senado.leg.br/dadosabertos/plenario/lista/liderancas") %>% xml2::read_xml()
  lideranca <- xml2::xml_find_all(xml, ".//DadosLiderancas/Lideranca") %>%
    purrr::map_df(function(x) {
      list(
SiglaUnidLideranca = xml_find_first(x, "./SiglaUnidLideranca") %>% xml_text(),
CodigoParlamentar = xml_find_first(x, ".//CodigoParlamentar") %>% xml_text(),
NomeParlamentar = xml_find_first(x, ".//NomeParlamentar") %>% xml_text()
      )
      
}) %>% dplyr::filter(SiglaUnidLideranca == "Governo") 
  
return(lideranca)
  
}
NULL