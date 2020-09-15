#' check if a url exists 

url_exists <- function(url){
  tryCatch(
    identical(status_code(HEAD(url)),200L), 
    error = function(e){
      FALSE
    })
}
NULL