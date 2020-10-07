#' @title Extrai somente os números de um texto extraído de regex
#' @description A partir de uma expressão regular, extrai o texto desse regex e depois retorna apenas os números existentes
#' @param text Texto onde o regex será aplicado
#' @param text_regex Expressão regular onde o texto será extraído para depois serem retornados apenas os números
#' @return Números existentes em um texto extraído a partir de uma expressão regular
#' @examples
#' \dontrun{
#' extractNumberUsingRegex("p_cod_materia_i=[\\d]*&")
#' }
#' @export
extractNumberUsingRegex <- function(text, text_regex) {
  return(stringr::str_extract(text, text_regex) %>%
           stringr::str_extract("[0-9]+"))
}





#' @title Extrai data no formato "dd/mm/yyyy" de uma tag dentro de um xml_nodeset html
#' @description A partir de um nó xml_nodeset, extrai o texto da tag e recupera uma data no formato "dd/mm/yyyy"
#' @param x xml_nodeset contendo a tag a ser extraída
#' @param tag tag html que possui a data
#' @return Data extraída no formato "yyyy-mm-dd"
#' @examples
#' \dontrun{
#' extractDateUsingRegex(x, "caption")
#' }
#' @export
extractDateUsingRegex <- function(x, tag) {
  rvest::html_nodes(x, tag) %>%
    rvest::html_text() %>%
    stringr::str_extract("[\\d]{2}/[\\d]{2}/[\\d]{4}") %>%
    lubridate::dmy() %>% 
    return()
}
