#' Fetch a list of actual senators
#' 
#' @param what a string, if "atual" only the active senators are returned.
#' @query a string for querying data on API, ex: 
#' 
#' @return a data.frame 
#' @importFrom dplyr %>%
#' @importFrom xml2 read_xml xml_find_all 
#' @importFrom tibble tibble
#' @examples
#' \dontrun{
#' dat = fetchSenators()
#' } 
#' 
#' 
#' @export
fetchSenators <- function(what = "atual", query = NULL){
  Sys.sleep(.25)
  suppressPackageStartupMessages(library(httr));
  suppressPackageStartupMessages(library(XML));
  suppressPackageStartupMessages(library(tidyverse));
  suppressPackageStartupMessages(library(xml2));
  
  
  url = "https://legis.senado.leg.br/"
  path = paste0("dadosabertos/senador/lista/",what)
  query = query
  
  if(url_exists(httr::modify_url(url = url, path = path, query = query))==TRUE){
    resp = httr::modify_url(url = url, path = path, query = query)
    
    raw_xml <- xml2::read_xml(resp)
    
    CodigoParlamentar = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/CodigoParlamentar") %>% xml2::xml_text();
    
    CodigoPublicoNaLegAtual = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/CodigoPublicoNaLegAtual") %>% xml2::xml_text();
    
    NomeParlamentar = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/NomeParlamentar") %>% xml2::xml_text();
    
    NomeCompletoParlamentar = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/NomeCompletoParlamentar") %>% xml2::xml_text();
    
    SexoParlamentar = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/SexoParlamentar") %>% xml2::xml_text();
    
    UrlFotoParlamentar = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/UrlFotoParlamentar") %>% xml2::xml_text();
    
    UrlPaginaParlamentar = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/UrlPaginaParlamentar") %>% xml2::xml_text();
    
    UrlPaginaParticular = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/UrlPaginaParticular") %>% xml2::xml_text();
    
    EmailParlamentar = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/EmailParlamentar") %>% xml2::xml_text();
    
    NumeroTelefone = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/Telefones/Telefone/NumeroTelefone") %>% xml2::xml_text();
    
    SiglaPartidoParlamentar = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/SiglaPartidoParlamentar") %>% xml2::xml_text();
    
    UfParlamentar = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/UfParlamentar") %>% xml2::xml_text();
    
    MembroMesa = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/MembroMesa") %>% xml2::xml_text();
    
    MembroLideranca = raw_xml %>% xml2::xml_find_all("//Parlamentares/Parlamentar/IdentificacaoParlamentar/MembroLideranca") %>% xml2::xml_text();
    
    
    data = tibble(idParlamentar = CodigoParlamentar,
                  siglaCasa = "SF",
                  nomeCivil = NomeCompletoParlamentar, 
                  nomeEleitoral = NomeParlamentar,
                  nome = NomeParlamentar,
                  sexo = SexoParlamentar, 
                  siglaUF = UfParlamentar, 
                  siglaPartido = SiglaPartidoParlamentar,
                  email = EmailParlamentar, 
                  # telefone = NumeroTelefone,
                  urlFoto = UrlFotoParlamentar
    )
    
  } else{
    data = tibble()
  }
  return(data)
}
NULL




