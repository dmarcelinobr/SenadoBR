#' @title Recupera informações das votações nominais do plenário em um intervalo de tempo
#' @description A partir de uma data de início e uma de fim, recupera dados de
#' votações nominais de plenário que aconteceram na Senado Federal
#' @param initial_date Data inicial do período de votações. Formato: "dd/mm/yyyy"
#' @param end_date Data final do período de votações. Formato: "dd/mm/yyyy"
#' @return Votações da proposição em um intervalo de tempo
#' @examples
#' \dontrun{
#' votacoes <- fetchRollcallVotesByInterval(initial_date = "01/02/2019")
#' }
#' @export
fetchRollcallVotesByInterval <-
  function(initial_date = "01/02/2019",
           end_date = format(Sys.Date(), "%d/%m/%Y")) {
    library(tidyverse)
    library(rvest)
    
    url <-
      paste0(
        "https://www25.senado.leg.br/web/atividade/votacoes-nominais/-/v/periodo/",
        initial_date,
        "/a/",
        end_date
      )
    
    votacoes <- tryCatch({
      dados <-
        RCurl::getURI(url) %>%
        xml2::read_html() %>%
        html_nodes(".table") %>%
        
        purrr::map_df(function(x) {
          rows <-
            html_nodes(x, "tbody") %>%
            html_nodes("tr")
          
          if (length(rows) > 0) {
            data <-
              purrr::map_df(rows, function(y) {
                list(
                  objeto_votacao = (html_nodes(y, "td")[2]) %>%
                    html_text(),
                  
                  url_votacao =
                    (html_nodes(y, "td") %>%
                       html_nodes("a") %>%
                       html_attr("href")
                    )[1],
                  
                  votacao_secreta = 
                    if_else(
                      str_detect(
                        (html_nodes(y, "td")[1]) %>%
                          html_text(), 
                        "votação secreta"),
                      1, 
                      0)
                )
              }) %>%
              mutate(
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
              dplyr::select(bill_id, 
                            rollcall_id, 
                     objeto_votacao, 
                     datetime, 
                     votacao_secreta, 
                    url_votacao); 
            
            return(data)
          }
          
          return(tribble())
          
        })
      
      return(dados)
      
    }, error = function(e) {
      return(tribble( ~ bill_id,
                      ~ rollcall_id,
                      ~ objeto_votacao,
                      ~ datetime,
                      ~ votacao_secreta,
                      ~ url_votacao))
    })
    
    return(votacoes)
  }
NULL

