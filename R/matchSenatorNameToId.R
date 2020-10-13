#' @title Mapeia um nome eleitoral para id correspondente
#' @description Recebe dois dataframes contendo nome eleitoral e um deles com informação de id
#' @param df Dataframe a receber o id do parlamentar
#' @return Dataframe target_df contendo coluna id
matchSenatorNameToId <- function(df) {
  library(tidyverse)
  
  senadores <- read_csv("data/senadores.csv")
  
  result <- 
    df %>% 
    dplyr::left_join(
      senadores %>%
        dplyr::select(legislator_name, legislator_id), 
      by=c("legislator_name"))
  
  return(result)
}
NULL
