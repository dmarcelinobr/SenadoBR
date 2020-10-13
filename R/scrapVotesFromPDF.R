#' @title Raspa os dados de votos de um pdf
#' @description A partir do caminho do pdf, raspa as informações referentes aos votos dos senadores
#' @param filepath Caminho + nome do arquivo PDF que será baixado.
#' @return Dataframe com informações de votos dos senadores
#' @importFrom pdftools pdf_text
#' @importFrom stringr str_extract str_split
#' @importFrom purrr map_df
#' @importFrom tibble tribble
#' @importFrom  dplyr slice
#' 
#' @export
scrapVotesFromPDF <- function(filepath) {
  library(tidyverse)
  
  pdf <- pdftools::pdf_text(filepath)
  
  votos <- purrr::map_df(pdf, function(x) {
    content <-
      stringr::str_extract(x, "(.|\n)+?(?=(\\s Legenda|PRESENTES))") %>%
      stringr::str_extract("SENADO.*UF.*VOTO(.|\n)*") %>%
      stringr::str_split("\n")
    
    data <- content[[1]] %>%
      stringr::str_split("\\s{2,}") %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame()
    
    rownames(data) <- NULL
    
    if(ncol(data) > 1) {
      colnames(data) <- c('legislator_name', 'legislator_state', 'legislator_party', 'legislator_vote')
      
      data <- data %>% mutate(legislator_name = trimws(legislator_name, which = "both"),
                              legislator_state =  trimws(legislator_state, which = "both"),
                              legislator_party = trimws(legislator_party, which = "both"),
                              legislator_vote = trimws(legislator_vote, which = "both"))
      return(data %>% 
               dplyr::slice(2:nrow(data)))
      
    } else{
      
      return(tribble(~ legislator_name, ~ legislator_state, ~ legislator_party, ~ legislator_vote))
    }
    
  })
  
  return(votos)
} 
NULL






#' @title Extrai informações de votos dos senadores a partir de uma url
#' @description A partir de uma url, extrai os dados de votos dos senadores.
#' @param url URL da requisição
#' @param keep if FALSE, the pdf file is deleted 
#' @return Dataframe com informações de votos dos senadores
#' @importFrom stringr str_extract str_split
#' @examples 
#' \dontrun{
#' "https://www25.senado.leg.br:443/web/atividade/materias/-/materia/123903/votacoes#votacao_5952"
#' 
#' fetchVotesFromRollcallUrl("https://rl.senado.gov.br/reports/rwservlet?legis&report=/forms/parlam/vono_r01.RDF&paramform=no&p_cod_materia_i=139005&p_cod_materia_f=139005&p_cod_sessao_votacao_i=6042&p_cod_sessao_votacao_f=6042")
#' }
#' @export
fetchVotesFromRollcallUrl <- function(url, keep = FALSE) {
 suppressPackageStartupMessages(library(RCurl))
  suppressPackageStartupMessages(library(rvest))
  suppressPackageStartupMessages(library(xml2))
  
  
  print(paste0("Extracting details from rollcall id ", 
               stringr::str_extract(url, "\\d*$")))
  
  #new_url <- 
  # getURL(url, 
  #       ssl.verifypeer = FALSE) %>% 
  #read_html() %>%
  #html_node('a') %>% 
  #html_attr('href')
  
  pdf_filepath <- here::here("data/votacoes/votacao_senado.pdf")
  
  download_pdf(url, pdf_filepath)
  
  votos <- scrapVotesFromPDF(pdf_filepath)
  
  if(keep == FALSE){
    base::file.remove(pdf_filepath)
  }
  
  return(votos)
}
NULL
