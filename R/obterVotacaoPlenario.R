#' Obtem os dados de votações realizada no plenário do Senado
#' 
#' @param  dataInicio string para data de início
#' @param  dataFim string para data fim 
#'  @examples 
#'  \dontrun{
#' dat = obterVotacaoPlenarioSenado(dataInicio = '20190201',   dataFim = '20200831')
#'  }
#' 
obterVotacaoPlenarioSenado <- function(dataInicio = '20190201',   dataFim = '20191231', query = NULL){  
  # http://legis.senado.leg.br/dadosabertos/plenario/lista/votacao/  
  Sys.sleep(.25)
  suppressPackageStartupMessages(library(httr));
  suppressPackageStartupMessages(library(XML));
  suppressPackageStartupMessages(library(tidyverse));
  suppressPackageStartupMessages(library(xml2));
  suppressPackageStartupMessages(library(purrr));
  suppressPackageStartupMessages(library(jsonlite));
  
  # https://legis.senado.leg.br/dadosabertos/plenario/lista/votacao/20160401/20160430
  url = "https://legis.senado.leg.br/"
  path = paste0("dadosabertos/plenario/lista/votacao/", dataInicio,'/', dataFim)
  query = query
  
  if(url_exists(httr::modify_url(url = url, path = path, query = query))==TRUE){
    resp = httr::modify_url(url = url, path = path, query = query)
    
    raw_xml <- xml2::read_xml(resp)
    
    CodigoSessao = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/CodigoSessao") %>% xml2::xml_text();
    SiglaCasa = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/SiglaCasa") %>% xml2::xml_text();
    DataSessao = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/DataSessao") %>% xml2::xml_text();
    HoraInicio = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/HoraInicio") %>% xml2::xml_text();
    CodigoTramitacao = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/CodigoTramitacao") %>% xml2::xml_text();
    CodigoSessaoVotacao = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/CodigoSessaoVotacao") %>% xml2::xml_text();
    SequencialSessao = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/SequencialSessao") %>% xml2::xml_text();
    DescricaoVotacao = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/DescricaoVotacao") %>% xml2::xml_text();
    Resultado = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/Resultado") %>% xml2::xml_text();
    
    TotalVotosSim = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/TotalVotosSim") %>% xml2::xml_text();
    TotalVotosNao = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/TotalVotosNao") %>% xml2::xml_text();
    TotalVotosAbstencao = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/TotalVotosAbstencao") %>% xml2::xml_text();
    CodigoMateria = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/CodigoMateria") %>% xml2::xml_text();
    NumeroMateria = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/NumeroMateria") %>% xml2::xml_text();
    AnoMateria = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/AnoMateria") %>% xml2::xml_text();
    SiglaCasaMateria = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/SiglaCasaMateria") %>% xml2::xml_text();
    DescricaoIdentificacaoMateria = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/DescricaoIdentificacaoMateria") %>% xml2::xml_text();
    CodigoParlamentar = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/Votos/VotoParlamentar/CodigoParlamentar") %>% xml2::xml_text();
    NomeParlamentar = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/Votos/VotoParlamentar/NomeParlamentar") %>% xml2::xml_text();
    SiglaPartido = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/Votos/VotoParlamentar/SiglaPartido") %>% xml2::xml_text();
    SiglaUF = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/Votos/VotoParlamentar/SiglaUF") %>% xml2::xml_text();
    Voto = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/Votos/VotoParlamentar/Voto") %>% xml2::xml_text();
    DescricaoVoto = raw_xml %>% xml2::xml_find_all("//Votacoes/Votacao/Votos/VotoParlamentar/DescricaoVoto/DescricaoVoto") %>% xml2::xml_text();
    
    
    data <- tibble::tibble(DescricaoIdentificacaoMateria = DescricaoIdentificacaoMateria,
                   CodigoParlamentar = CodigoParlamentar ,
                   NomeParlamentar = NomeParlamentar, 
                   SiglaPartido = SiglaPartido,
                   SiglaUF = SiglaUF,
                   Voto = Voto)
    
  } else{
    data = tibble::tibble()
  }
  return(data)
} 
