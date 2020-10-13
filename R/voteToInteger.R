#' @title Transforma a string de votos para código numérico
#' @description Recebe um dataframe com coluna voto e converte o valor para um número inteiro
#' @param df Dataframe com a coluna voto
#' @return Dataframe com coluna voto enumerada
#' @examples
#' \dontrun{
#' voteStringToInteger(df)
#' }
#' @export
voteStringToInteger <- function(df) {
  df %>%
    mutate(
      legislator_vote = case_when(
        str_detect(legislator_vote, "Não") ~ -1,
        str_detect(legislator_vote, "Sim") ~ 1,
        str_detect(legislator_vote, "Obstrução|P-OD") ~ 2,
        str_detect(legislator_vote, "Abstenção") ~ 3,
        str_detect(legislator_vote, "Art. 17|art. 51 RISF|Art.17") ~ 4,
        str_detect(legislator_vote, "Liberado") ~ 5,
        #TODO: Tratar caso P-NRV: Presente mas não registrou foto
        TRUE ~ 0
      )
    )
}
NULL






#' @title Recupera descrição do voto a partir do código numérico do voto
#' @description Recebe um valor numérico que representa o código do voto e retorna a descrição do mesmo
#' @param vote Voto para descrição
#' @return Descrição do voto apssado como parâmetro
#' @examples
#' \dontrun{
#' voteIntegerToString(2)
#' }
#' @export
voteIntegerToString <- function(legislator_vote) {
  vote_string <- case_when(
    legislator_vote == -1 ~ "Não",
    legislator_vote == 1 ~ "Sim",
    legislator_vote == 2 ~ "Obstrução",
    legislator_vote == 3 ~ "Abstenção",
    legislator_vote == 4 ~ "Art. 17",
    legislator_vote == 5 ~ "Liberado",
    TRUE ~ "Não votou")
  
  return(vote_string)
}
NULL
