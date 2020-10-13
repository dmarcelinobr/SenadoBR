#' @title Baixa um PDF a partir de uma url e um caminho de destino
#' @description A partir de uma url e de o caminho de destino + nome para o pdf, baixa e salva este arquivo
#' @param url URL da requisição
#' @param dest_path Caminho + nome do arquivo PDF que será baixado.
#' @importFrom RCurl getBinaryURL
download_pdf <- function(url, dest_path = "votacao_senado.pdf") {
  pdf <- RCurl::getBinaryURL(url,  
                             ssl.verifypeer=FALSE)
  
  base::writeBin(pdf, dest_path)
}


#' check if a url exists 

url_exists <- function(url){
  tryCatch(
    identical(httr::status_code(HEAD(url)),200L), 
    error = function(e){
      FALSE
    })
}
NULL


