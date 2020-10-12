#' @title Recupera informações das votações nominais do plenário de uma proposição
#' @description A partir do id de uma proposição, recupera dados de
#' votações nominais de plenário que aconteceram na Senado Federal
#' @param bill_id Id da proposição
#' @param year Ano que filtra as proposições. Caso seja null, todas as votações serão retornadas
#' @return Votações de uma proposição em um ano ou todos, caso nenhum ano seja passado como parâmetro
#' @importFrom xml2 xml_find_all read_xml xml_find_first xml_text
#' @importFrom RCurl getURI
#' @importFrom dplyr select mutate
#' @importFrom tibble tribble
#' @examples 
#' \dontrun{
#' fetchRollcallVotes(bill_id = 135251)
#' }
#' @export
fetchRollcallVotes <- function(bill_id, year = NULL) {
    Sys.sleep(.25)
    suppressPackageStartupMessages(library(httr))
    suppressPackageStartupMessages(library(tidyverse))
    suppressPackageStartupMessages(library(RCurl))  
    suppressPackageStartupMessages(library(xml2))
    
    
    url <-
      paste0("http://legis.senado.leg.br/dadosabertos/materia/votacoes/", bill_id)
    
    xml <-
      RCurl::getURI(url, .encoding = 'UTF-8')
    
    
    votacoes <- tryCatch({
      data <-
        xml %>%
        xml2::read_xml(encoding = "UTF-8") %>%
        xml2::xml_find_all(".//Materia/Votacoes/Votacao") %>%
        purrr::map_df(function(x) {
          list(
            session_id =
              xml2::xml_find_first(x, "./SessaoPlenaria/CodigoSessao") %>%
              xml2::xml_text(),
            rollcall_info =
              xml2::xml_find_first(x, "./DescricaoVotacao") %>%
              xml2::xml_text(),
            data =
              xml2::xml_find_first(x, "./SessaoPlenaria/DataSessao") %>%
              xml2::xml_text(),
            hora =
              xml2::xml_find_first(x, "./SessaoPlenaria/HoraInicioSessao") %>%
              xml2::xml_text()
          )
        }) %>%
        dplyr::mutate(datetime =
                 paste0(data,
                        " ",
                        hora) %>%
                 as.POSIXct(),
                 bill_id = bill_id) %>%
        dplyr::select(bill_id,
                      rollcall_info,
               datetime,
               session_id)
      
    #  if (!is.null(year)) {
    #    data <- data %>%
    #      dplyr::filter(lubridate::year(datetime) == year)
    #    return(data)
    #  }
    }, error = function(e) {
      return(tibble::tribble( ~ bill_id,
                      ~ rollcall_info,
                      ~ datetime,
                      ~ session_id))
})
    return(votacoes)
}
NULL
