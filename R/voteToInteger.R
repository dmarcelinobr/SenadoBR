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
      voto = case_when(
        str_detect(voto, "Não") ~ -1,
        str_detect(voto, "Sim") ~ 1,
        str_detect(voto, "Obstrução|P-OD") ~ 2,
        str_detect(voto, "Abstenção") ~ 3,
        str_detect(voto, "Art. 17|art. 51 RISF|Art.17") ~ 4,
        str_detect(voto, "Liberado") ~ 5,
        #TODO: Tratar caso P-NRV: Presente mas não registrou foto
        TRUE ~ 0
      )
    )
}
NULL






#' @title Recupera descrição do voto a partir do código numérico do voto
#' @description Recebe um valor numérico que representa o código do voto e retorna a descrição do mesmo
#' @param voto Voto para descrição
#' @return Descrição do voto apssado como parâmetro
#' @examples
#' \dontrun{
#' voteIntegerToString(2)
#' }
#' @export
voteIntegerToString <- function(voto) {
  voto_descricao <- case_when(
    voto == -1 ~ "Não",
    voto == 1 ~ "Sim",
    voto == 2 ~ "Obstrução",
    voto == 3 ~ "Abstenção",
    voto == 4 ~ "Art. 17",
    voto == 5 ~ "Liberado",
    TRUE ~ "Não votou")
  
  return(voto_descricao)
}
NULL
