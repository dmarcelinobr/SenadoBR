#' @title Recupera informações das votações nominais do plenário em um intervalo de tempo
#' @description A partir de uma data de início e uma de fim, recupera dados de
#' votações nominais de plenário que aconteceram na Senado Federal
#' @param initial_date Data inicial do período de votações. Formato: "dd/mm/yyyy"
#' @param end_date Data final do período de votações. Formato: "dd/mm/yyyy"
#' @return Votações da proposição em um intervalo de tempo
#' @importFrom dplyr mutate select
#' @importFrom RCurl getURI
#' @importFrom rvest html_nodes
#' @importFrom xml2 read_html
#' @importFrom stringr str_remove_all
#' 
#' @examples
#' \dontrun{
#' votacoes <- fetchRollcallVotesByInterval(initial_date = "01/02/2019")
#' }
#' @export
fetchRollcallVotesByInterval <-
  function(initial_date = "01/02/2019",
           end_date = format(Sys.Date(), "%d/%m/%Y")) {
    suppressPackageStartupMessages(library(tidyverse))
    suppressPackageStartupMessages(library(RCurl))
    suppressPackageStartupMessages(library(xml2))
    suppressPackageStartupMessages(library(rvest))
    
    url <-
      paste0(
        "https://www25.senado.leg.br/web/atividade/votacoes-nominais/-/v/periodo/",
        initial_date,
        "/a/",
        end_date
      )
    
    votacoes <- tryCatch({
      dados1 <-
        RCurl::getURI(url, .encoding = 'UTF-8') %>%
        xml2::read_html() %>%
        rvest::html_nodes(".table") %>%
        
        purrr::map_df(function(x) {
          rows <-
            rvest::html_nodes(x, "tbody") %>%
            rvest::html_nodes("tr") 
          
          if (length(rows) > 0) {
            dados2 <-
              purrr::map_df(rows, function(y) {
                list(
                  rollcall_info = (rvest::html_nodes(y, "td")[2]) %>%
                    rvest::html_text(),
                  
                  url_votacao =
                    (rvest::html_nodes(y, "td") %>%
                       rvest::html_nodes("a") %>%
                       rvest::html_attr("href")
                    )[1],
                  
                  votacao_secreta = 
                    if_else(
                      str_detect(
                        (rvest::html_nodes(y, "td")[1]) %>%
                          rvest::html_text(), 
                        "votação secreta"),
                      1, 
                      0)
                )
              }) %>%
              dplyr::mutate(
                url_votacao =
                  stringr::str_remove_all(url_votacao,
                                          '&p_order_by=.*'),
                
                bill_id =
                  extractNumberUsingRegex(url_votacao,
                                            "materia.[0-9]+"),
                
                rollcall_id =
                  extractNumberUsingRegex(url_votacao,
                                            "votacao.[0-9]+"),
                
                datetime =
                  extractDateUsingRegex(x, "caption")
              ) %>% 
              dplyr::mutate(link_pdf = paste0("https://rl.senado.gov.br/reports/rwservlet?legis&report=/forms/parlam/vono_r01.RDF&paramform=no&p_cod_materia_i=",
                                      bill_id, "&p_cod_materia_f=", bill_id, "&p_cod_sessao_votacao_i=", rollcall_id, "&p_cod_sessao_votacao_f=", rollcall_id)
              ) %>%
              dplyr::select(bill_id, 
                            rollcall_id, 
                            rollcall_info, 
                     datetime, 
                     votacao_secreta,
                     url_votacao,
                     link_pdf); 
            
            return(dados2)
          }
          
          return(tribble())
          
        })
      
      return(dados1)
      
    }, error = function(e) {
      return(tribble( ~ bill_id,
                      ~ rollcall_id,
                      ~ rollcall_info,
                      ~ datetime,
                      ~ votacao_secreta,
                      ~ url_votacao,
                      ~ link_pdf))
    })
    return(votacoes)
  }
NULL