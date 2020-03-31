if (getRversion() >= "3.0.0")  utils::globalVariables(".")

#' @title Fetch proposals by year from the Senate
#' @description Returns a data frame with few information from a proposal, including id, type, year, name and an indicator if the bill has a final decision or not.
#' @param ano A 4 digits integer as 2019.
#' @return Dataframe with some information of a given propsition
#' @export
#' @examples
#' \dontrun{
#' fetchYearlyProposals(2020, 'SF'); 
#' fetchYearlyProposals(2020, 'CD')
#' }
fetchYearlyProposals <- function(ano, casa) {
  if (casa == "CD") {
    fetchCanaraProposals(ano)
  } else if (casa == "SF") {
    fetchSenadoProposals(ano)
  } else {
    return("Parâmetro 'casa' não identificado.")
  }
}
NULL




#' @title Fetch proposals by year from the Senate
#' @description Returns a dataframe with few information from a proposal, including id, type, year, name and an indicator if the bill has a final decision or not.
#'
#' @param ano A 4 digits integer as 2019.
#' @return Dataframe with few information of a given propsition
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom purrr map
#' @importFrom purrr flatten
#' @importFrom purrr discard
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#' @importFrom stringi stri_trans_general
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate year
#' @export
#' @examples
#' \dontrun{
#' 
#' df = fetchSenadoProposals(2020)
#' }
fetchSenadoProposals <- function(ano){
  require(httr)
  require(data.table)
  require(tidyverse)
  require(jsonlite)
  require(progress)
  require(lubridate)
  require(purrr)
  
  # http://legis.senado.leg.br/dadosabertos/docs/ui/index.html
  if(!is.null(ano)){
    Y <- lubridate::year(today())
    if(ano > Y){
      stop("Você precisa solicitar um ano em formato de número interio válido.")
    }
    url <- paste0("http://legis.senado.gov.br/dadosabertos/materia/pesquisa/lista?ano=", ano);
  }
  
  request <- httr::GET(url)
  request <-  status(request)
  
  if(is.null(request)){
    stop("Não existem dados para essa busca")
  }
  
  Materia <- request$PesquisaBasicaMateria$Materias$Materia
  Autores <- purrr::map(Materia, .null = NA_character_, "AutoresPrincipais") %>% purrr::flatten()
  DadosBasicosMateria <- purrr::map(Materia, .null = NA_character_, "DadosBasicosMateria") %>% purrr::flatten()
  
  dados <- tibble::tibble(
    idProposicao = purrr::flatten(Materia) %>%
      purrr::map_chr(.null =  NA_character_, "CodigoMateria") %>% na_discard(),
    uri = paste0("http://legis.senado.gov.br/dadosabertos/materia/", idProposicao),
    dataApresentacao = purrr::flatten(Materia) %>%
      purrr::map_chr(.null = NA_character_, "DataApresentacao") %>% na_discard(),
    siglaTipo = purrr::flatten(Materia) %>%
      purrr::map_chr(.null =  NA_character_, "SiglaSubtipoMateria") %>% na_discard(),
    codTipo = purrr::flatten(DadosBasicosMateria) %>%
      purrr::map_chr(.null =  NA_character_, "CodigoNatureza") %>% na_discard(),
    numero = purrr::flatten(Materia) %>%
      purrr::map_chr(.null =  NA_character_, "NumeroMateria") %>% na_discard(),
    ano = purrr::flatten(Materia) %>%
      purrr::map_chr(.null =  NA_character_, "AnoMateria") %>% na_discard(),
    nomeProposicao = purrr::flatten(Materia) %>%
      purrr::map_chr(.null =  NA_character_, "DescricaoIdentificacaoMateria") %>% na_discard(),
    indTramitando = purrr::flatten(Materia) %>%
      purrr::map_chr(.null =  NA_character_, "IndicadorTramitando") %>% na_discard()
  )
  return(dados)
}
NULL




#' Fetch proposals from Camara dos Deputados by year 
#' 
#' The function allows for retrieving proposals from Camara dos Deputados API by year. 
#'
#' @param ano The year of the legislative session 
#' 
#'
#'
#' @export
#' @examples 
#' \dontrun{
#' library(purrr)
#' library(progress)
#' library(data.table)
#' 
#' y=2001:2019
#' 
#' barra <- progress_bar$new(total = length(y),
#                             format = "[:bar] :percent eta: :eta")
#'
#' propostas <- map_df(y, ~{
#'         barra$tick() 
#'         fetchCanaraProposals(.x)})
#' }
#'  
fetchCanaraProposals <- function(ano){
  suppressPackageStartupMessages(library(tidyverse));
  suppressPackageStartupMessages(library(httr));
  suppressPackageStartupMessages(library(XML));
  suppressPackageStartupMessages(library(stringr));
  suppressPackageStartupMessages(library(purrr));
  suppressPackageStartupMessages(library(jsonlite));
  
  data <- data.frame()
  i <- 1
  while(TRUE){
    url <- paste0("https://dadosabertos.camara.leg.br/api/v2/proposicoes?ano=",ano,"&itens=100&pagina=",i,"&ordem=ASC&ordenarPor=id&dataInicio=1945-01-01&dataFim=2025-12-31")
    
    sucesso <- FALSE
    while(!sucesso){
      ret <- try(api_prop <- httr::GET(url))
      sucesso <- !grepl("Error", ret) & status_code(api_prop) == 200
    }
    
    api_data <- api_prop %>%
      httr::content("text") %>%
      jsonlite::fromJSON() %>%
      .[["dados"]]
    
    if(is.null(nrow(api_data))){
      break
    } else {
      data <- bind_rows(data, api_data %>%
                          dplyr::rename(idProposicao = id) %>% 
                          dplyr::mutate(nomeProposicao = paste0(siglaTipo," ", numero,"/", ano)) %>% 
                          # dplyr::mutate(indTramitacao = NA_character_) %>%
                          dplyr::select(-ementa)) 
    }
    
    rm(api_data)
    i <- i + 1
  }
  return(data)
} 
NULL









#' @title Baixa dados sobre uma proposição
#' @description Retorna um dataframe contendo dados sobre uma proposição
#' @param id Um ou mais IDs de proposições
#' @return Dataframe
#' @examples
#' @export
#' \dontrun{
#' fetchCamaraProposal(2056568);
#' fetchCamaraProposal(2121442);
#' fetchCamaraProposal(21058);
#'}
#'
fetchCamaraProposal <- function(id){
  suppressPackageStartupMessages(library(tidyverse));
  suppressPackageStartupMessages(library(httr));
  suppressPackageStartupMessages(library(XML));
  suppressPackageStartupMessages(library(stringr));
  suppressPackageStartupMessages(library(magrittr));
  suppressPackageStartupMessages(library(purrr));
  suppressPackageStartupMessages(library(jsonlite));
  
  autor_df <- fetchCamaraAuthors(id)
  if("ultimoStatus.nome" %in% names(autor_df)) {
    autor_df <- autor_df %>%
      dplyr::rename('nome' = 'ultimoStatus.nome')
  }
  
  proposicao <- rcongresso::fetch_proposicao_camara(id) %>%
    dplyr::transmute(idProposicao = as.integer(id),
                     siglaTipo,
                     numero = as.integer(numero),
                     ano = as.integer(ano),
                     nomeProposicao = paste0(siglaTipo," ", numero,"/",ano),
                     ementa = paste(ementa, ementaDetalhada),
                     dataApresentacao = lubridate::ymd_hm(stringr::str_replace(dataApresentacao,'T',' ')),
                     casaOrigem = ifelse(autor_df %>% head(1) %>%
                                           dplyr::select(codTipo) == 40000, "SF",
                                         ifelse(autor_df %>% head(1) %>%
                                                  dplyr::select(codTipo) == 10000, "CD", NA_character_)),
                     nomeAutor = paste(unlist(t(autor_df$nome)),collapse="+"),
                     regimeTramitacao = statusProposicao.regime, 
                     descricaoSituacao = statusProposicao.descricaoSituacao,
                     urlInteiroTeor = urlInteiroTeor) %>% select(-ano, -numero, dataApresentacao, idProposicao, nomeProposicao, nomeAutor, casaOrigem, ementa, regimeTramitacao, descricaoSituacao, urlInteiroTeor)
  return(proposicao)
}
NULL





#' @title Fetches proposition's authors
#' @description Fetches a dataframe containing basic information about the authors of the proposition
#' @param idProposicao Proposition's ID
#' @export
#' \dontrun{
#' fetchCamaraAuthors(2121442);
#' fetchCamaraAuthors(2056568);
#' fetchCamaraAuthors(21058);
#'
#' }
fetchCamaraAuthors <- function(idProposicao = NULL) 
{
  autor_uri <- paste0(.API_CAMARA_PROPOSICOES, "/", idProposicao, 
                      "/autores")
  autor_info <- api_camara(autor_uri)
  if (any(is.na(autor_info$uri))) {
    autores <- api_camara(autor_uri) %>% 
      assert_dataframe_completo(.COLNAMES_CAMARA_AUTORES) %>% 
      coerceTableParams(.COLNAMES_CAMARA_AUTORES)
  }
  else {
    autores <- purrr::map_df(autor_info$uri, ~.auxiliary_fetch_autor_camara(.x)) %>% 
      dplyr::left_join(autor_info %>%
                         dplyr::select(-nome), by = "uri")
  }
  
  # dplyr::rename(idParlamentar = id, uriAutor = uri,
  #                nomeAutor = ultimoStatus.nome,
  #                sexoAutor = sexo,tipoAutor = tipo, 
  #                 siglaPartido = ultimoStatus.siglaPartido,
  #                  siglaUF = ultimoStatus.siglaUf)
  return(autores)
}



#' @title Retrieves details about an author of a proposition
#' @description Fetches a dataframe containing detailed information about the author of the proposition
#' @param uri URL relative to the Deputy url
#' @return A dataframe containing details about the author of a proposition
#' @examples
#' \dontrun{
#' .auxiliary_fetch_autor_camara('https://dadosabertos.camara.leg.br/api/v2/deputados/178854')
#' }
.auxiliary_fetch_autor_camara <- function(uri) {
  strsplit(uri, '/')[[1]] %>% tail(1) %>%
    .fetch_using_id(.API_CAMARA_DEPUTADOS)
}



#' Wraps an access to the camara API given a relative path and query arguments.
#' @param path URL relative to the API base URL
#' @param query Query parameters
#' @export
api_camara <- function(path=NULL, query=NULL, asList = FALSE){
  
  resp <- .get_from_api(.API_CAMARA, path, query)
  obtained_data <- .get_json(resp)$dados
  
  if(!is.data.frame(obtained_data) && !asList){
    obtained_data %>%
      .get_dataframe()
  } else obtained_data
  
}



#' Wraps an access to the senate API given a relative path and query arguments.
#' @param path URL relative to the API base URL
#' @param query Query parameters
#' @param asList If return should be a list or a dataframe
#' @export
api_senado <- function(path=NULL, query=NULL, asList = FALSE){
  
  resp <- .get_from_api(.API_SENADO, path, query)
  obtained_data <- .get_json(resp)
  
  if(!is.data.frame(obtained_data) && !asList){
    obtained_data %>%
      .get_dataframe()
  } else obtained_data
  
}


#' In case of receiving a list, this function converts the list into a dataframe.
#' @param x List
.get_dataframe <- function(x){
  x %>%
    #lapply(.replace_null) %>%
    unlist() %>%
    #.coerce_numeric() %>%
    as.list() %>%
    as.data.frame(stringsAsFactors = FALSE)
}





#' Garantees that the dataframe x has all the columns passed by y.
#' @param x dataframe
#' @param y vector of characters containing the names of columns.
#' @param warning boolean to show the prints
assert_dataframe_completo <- function(x, y, warning = FALSE){
  if(nrow(x) != 0){
    colnames_x <- colnames(x)
    colnames_y <- names(y)
    types_y <- unname(y)
    indexes <- !(colnames_y %in% colnames_x)
    
    if (any(indexes) & warning) {
      .print_warning_and_list("Not found columns:", colnames_y[indexes])
    }
    nao_esperadas = colnames_x[!(colnames_x %in% colnames_y)]
    if (length(nao_esperadas) & warning) {
      .print_warning_and_list("Unexpected columns:", nao_esperadas)
    }
    x[colnames_y[indexes]] <- ifelse(types_y[indexes] == "character", NA_character_, NA_real_)
    x
  } else tibble::tibble()
}





#' Garantees that the dataframe x has all the columns passed by y.
#' @param x dataframe
#' @param y vector of characters containing the names of columns.
#' @param warning boolean to show the prints
assertTableIsValid <- function(x, y, warning = FALSE){
  if(nrow(x) != 0){
    colnames_x <- colnames(x)
    colnames_y <- names(y)
    types_y <- unname(y)
    indexes <- !(colnames_y %in% colnames_x)
    
    if (any(indexes) & warning) {
      printWarningAndList("Not found columns:", colnames_y[indexes])
    }
    nao_esperadas = colnames_x[!(colnames_x %in% colnames_y)]
    if (length(nao_esperadas) & warning) {
      printWarningAndList("Unexpected columns:", nao_esperadas)
    }
    
    x[colnames_y[indexes]] <- ifelse(types_y[indexes] == "character", NA_character_, NA_real_)
    x
  } else tibble::tibble()
}





#' Garantees that the dataframe obj has all the correct types passed by types.
#' @param obj dataframe
#' @param types named vector of the columns names and types
coerceTableParams <- function(obj, types, order_cols=TRUE){
  if(nrow(obj) != 0){
    if (order_cols) {
      obj <- obj[,order(colnames(obj))]
      types <- unname(types[sort(names(types))])
    } else {
      types <- unname(types[names(types)])
    }
    
    length(types) <- length(names(obj))
    types <- replace(types, is.na(types), "character")
    
    out <- lapply(1:length(obj),FUN = function(i){
      dynamic_cast <<-switchTableParams(types[i])
      obj[,i] %>% unlist() %>% dynamic_cast
    })
    names(out) <- colnames(obj)
    as.data.frame(out,stringsAsFactors = FALSE)
  } else tibble::tibble()
}



#' Returns a conversion function given a type name.
#' @param x type name
switchTableParams <- function(x){
  switch(x,
         character = as.character,
         numeric = as.numeric,
         integer = as.integer,
         is.na = NA,
         list = as.list,
         logical = as.logical)
}


#' Prints a warning and a list.
#' @param msg warning message
#' @param l list
printWarningAndList <- function(msg, l) {
  cat(crayon::red("\n", msg, "\n  ", paste(l, collapse="\n   "),"\n"))
}




.get_from_api <- function(api_base=NULL, path=NULL, query=NULL, timeout = 1, tentativa = 0){
  ua <- httr::user_agent(.APROVOMETRO_LINK)
  api_url <- httr::modify_url(api_base, path = path, query = query)
  
  resp <- .get_from_cache(api_url)
  
  if (is.null(resp)) {
    resp_in_cache <- FALSE
    #print(paste("URL:",api_url))
    resp <- httr::GET(api_url, ua, httr::accept_json())
    Sys.sleep(.DEF_POST_REQ_SLEEP_TIME)
  } else {
    resp_in_cache <- TRUE
  }
  
  # Handle errors and retries
  if(httr::status_code(resp) >= .COD_ERRO_CLIENTE &&
     httr::status_code(resp) < .COD_ERRO_SERV){
    if (!resp_in_cache) .put_in_cache(api_url, resp)
    .throw_req_error(httr::status_code(resp), api_url)
  } else if(httr::status_code(resp) >= .COD_ERRO_SERV) {
    if(tentativa < .MAX_TENTATIVAS_REQ){
      .use_backoff_exponencial(api_base, path, query, timeout, tentativa+1)
    } else {
      if (!resp_in_cache) .put_in_cache(api_url, resp)
      .throw_req_error(httr::status_code(resp), api_url)
    }
  }
  
  if (!resp_in_cache) .put_in_cache(api_url, resp)
  
  if (httr::http_type(resp) != "application/json") {
    stop(.ERRO_RETORNO_JSON, call. = FALSE)
  }
  
  return(resp)
}

.get_hrefs <- function(path=NULL, query=NULL) {
  resp <- .get_from_api(.API_CAMARA, path, query)
  .get_json(resp)$links
}




#' Setup cache
#' @param name Name of the cache to use. NULL for none.
#' @export
setup_cache <- function (name) {
  options("rcongresso.cache"=name)
}

#' Save the cache to a file
.save_cache <- function() {
  dir.create(.cache_dir_path, showWarnings = FALSE)
  saveRDS(.HTTP_CACHE, .cache_file_path)
}

#' Stores a value in the cache.
#' @param key Key to store
#' @param value Value to store
.put_in_cache <- function(key, value) {
  if (is.null(getOption("rcongresso.cache"))) return(NULL)
  
  # Sometimes (after package installation) it's a locked list
  if (typeof(.HTTP_CACHE) == "environment") {
    assign(key, value, envir=.HTTP_CACHE)
    .save_cache()
  }
}

#' Gets a value from the cache.
#' @param key Key to get
.get_from_cache <- function(key) {
  if (is.null(getOption("rcongresso.cache"))) return(NULL)
  
  if (length(.HTTP_CACHE) == 0) {
    tryCatch({
      cache <- readRDS(.cache_file_path)
      for( k in names(cache) ) assign(k, cache[[k]], envir=.HTTP_CACHE)
    }, warning = function(w) {
    }, error = function(error_condition) {
    })
  }
  if (typeof(.HTTP_CACHE) == "environment") {
    # Sometimes it's an environment
    value <- tryCatch({
      get(key, envir=.HTTP_CACHE)
    }, error = function(e) {
      NULL
    })
  } else {
    # Sometimes it's a list (when checking package)
    value <- .HTTP_CACHE[[key]]
  }
  
  if (is.null(value)) {
    print(c("Not in cache", .cache_file_path))
  } else {
    print(c("In cache", .cache_file_path))
  }
  return(value)
}


#' Extracts the JSON data from an HTTP response
#' @param response The HTTP response
#' @return The json
.get_json <- function(response){
  httr::content(response, as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE)
}



#' Fetches details from a proposition.
#'
#' @param id Proposition's ID
#' @param API_path API path
#' @param asList If return should be a list or a dataframe
#'
#' @return Dataframe containing information about the proposition.
#'
#' @examples
#' pec241 <- rcongresso:::.fetch_using_id(2088351, API_path = "/api/v2/proposicoes")
#'
#' @export
.fetch_using_id <- function(id, API_path, asList = FALSE){
  tibble::tibble(id) %>%
    dplyr::mutate(path = paste0(API_path, "/", id)) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      api_camara(.$path, asList = asList)
    ) %>%
    dplyr::ungroup()
}



# response status check
status <- function(x){
  if(x$status_code != 200){
    stop("Request failed. Please check the validity of the information you requested.")
  } else{
    xx <- httr::content(x, as = "parsed")
    
  }
  if(is.null(xx)){
    stop("No data matches your search.")
  } else{
    return(xx)
  }
}


status2 <- function(x){
  if(x$status_code != 200){
    stop("GET request failed. Please check the validity of the information you requested.")
  } else{
    xx <- httr::content(x, as = "parsed", encoding = "latin1")
    
  }
  if(is.null(xx)){
    stop("No data matches your search.")
  } else{
    return(xx)
  }
}

'%ni%' <- Negate('%in%')


# depth of list check
depth <- function(x) ifelse(is.list(x), 1L + max(sapply(x, depth)), 0L)


# discard NA in vector
na_discard <- function(x){
  x <- as.character(x) %>% purrr::discard(is.na)
  if(purrr::is_empty(x)){
    x <- NA
  }
  return(x)
}




.API_CAMARA <- "https://dadosabertos.camara.leg.br"
.API_CAMARA_PROPOSICOES <- "/api/v2/proposicoes"
.API_SENADO <- "http://legis.senado.leg.br"
.API_SENADO_TRAMITACAO_PATH <- "/dadosabertos/materia/movimentacoes/"
.API_SENADO_RELATORIA <- "/dadosabertos/materia/relatorias/"
.API_CAMARA_RELATORIA <- "/api/v2/proposicoes"
.API_CAMARA_DEPUTADOS <- "/api/v2/deputados"

# Link do repositório do Aprovometro
.APROVOMETRO_LINK <- "https://github.com/JOTAJornalismo/Aprovometro"


.ERRO_RETORNO_JSON <- "API did not return json"

# Requests
.MAX_ITENS <- 100
.LAST_PAGE_INDEX <- 4
.COD_REQ_SUCCESS_MIN <- 200
.COD_REQ_SUCCESS_MAX <- 300
.COD_ERRO_CLIENTE <- 400
.COD_ERRO_SERV <- 500
.MAX_TENTATIVAS_REQ <- 5
.DEF_POST_REQ_SLEEP_TIME <- 0.05
.MENSAGEM_ERRO_REQ <- "Falha na requisicao da API. Erro %s ao tentar acessar: %s"
.LEGISLATURA_INICIAL <- 40
.LEGISLATURA_ATUAL <- 56


.PROPOSICAO_COLNAMES_CAMARA <- c("idProposicao"="character",
                                 "siglaTipo"="character",
                                 "siglaCasa"="character",
                                 "casaOrigem"="character",
                                 "numero"="integer",
                                 "ano"="integer",
                                 "nomeProposicao"="character",
                                 "dataApresentacao"="character")


# Autores
.COLNAMES_CAMARA_AUTORES <- c("uri"="character",
                              "nome"="character", 
                              "codTipo"="integer",
                              "tipo"="character")


# Deputados
.COLNAMES_DEP_INFO <- c("id"="integer",
                        "uri"="character",
                        "nome"="character",
                        "siglaPartido"="character",
                        "uriPartido"="character",
                        "siglaUf"="character",
                        "idLegislatura"="integer",
                        "urlFoto"="character")

.COLNAMES_DEP_INFO_ID <- c(
  "id"="integer",
  "uri"="character",
  "nomeCivil"="character",
  "ultimoStatus.id"="integer",
  "ultimoStatus.uri"="character",
  "ultimoStatus.nome"="character",
  "ultimoStatus.situacao"="character",
  "ultimoStatus.siglaPartido"="character",
  "ultimoStatus.uriPartido"="character",
  "ultimoStatus.siglaUf"="character",
  "ultimoStatus.idLegislatura"="integer",
  "ultimoStatus.urlFoto"="character",
  "ultimoStatus.data"="character",
  "ultimoStatus.nomeEleitoral"="character",
  "ultimoStatus.email"="character",
  "ultimoStatus.gabinete.email"="character",
  "ultimoStatus.gabinete.telefone"="character",
  "ultimoStatus.gabinete.predio"="character",
  "ultimoStatus.gabinete.sala"="character",
  "ultimoStatus.situacao"="numeric",
  "ultimoStatus.condicaoEleitoral"="character",
  "cpf"="character",
  "sexo"="character",
  "dataNascimento"="character",
  "ufNascimento"="character",
  "municipioNascimento"="character",
  "escolaridade"="character",
  "redeSocial"="character",
  "urlWebsite"="character",
  "dataFalecimento"="character"
  #, "ultimoStatus.descricaoStatus"="character","redeSocial"="list"
)
