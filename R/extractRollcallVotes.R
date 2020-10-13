#' @title Extrai informações de votos dos senadores a partir de um conjutno de votações
#' @description A partir de um dataframe de votações, extrai os dados de votos dos senadores.
#' @return Dataframe com informações de votos dos senadores
#' @examples 
#' \dontrun{
#' votacoes = fetchRollcallVotesByInterval(initial_date = "01/02/2019")
#'  
#' rollcalls = extractRollcallVotes(votacoes)
#' }
#' @export
#' 
extractRollcallVotes <- function(votacoes) {
  library(tidyverse)
  
  votacoes <- votacoes %>% 
    filter(votacao_secreta == 0) %>% 
    mutate(year = lubridate::year(datetime))
  
  votos <- 
    tibble::tibble(
      bill_id = votacoes$bill_id,
      rollcall_id = votacoes$rollcall_id,
      rollcall_info = votacoes$rollcall_info,
      datetime = votacoes$datetime,
      year = votacoes$year,
      url = votacoes$link_pdf) %>% 
    mutate(dados = purrr::map(
      url,
      fetchVotesFromRollcallUrl)) %>% 
    unnest(dados) %>% 
    dplyr::filter(legislator_name != '' | is.na(legislator_name)) %>%
    dplyr::select(-url)
  
  return(votos)
}