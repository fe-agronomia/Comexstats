#' Pesquisa informações no sistema Comex Stat
#'
#' @description
#' Consulta estatísticas de importação e exportação disponibilizadas pelo sistema
#' Comex Stat e retorna um data frame com os resultados.
#'
#' @param ano_inicial [int] Ano inicial da consulta. Ex: 2018.
#' @param ano_final [int] Ano final da consulta. Ex: 2018.
#' @param mes_inicial [int] Mês inicial da consulta (1-12).
#' @param mes_final [int] Mês final da consulta (1-12).
#' @param detalha_mes [logical] Se TRUE, detalha informações por mês.
#' @param tipo_op [char] Tipo da operação: `'exp'` (exportação) ou `'imp'` (importação).
#' @param tipo_ord [char] Tipo de ordenamento: `'val'` (valor) ou `'det'` (detalhamento).
#' @param filtros [character] Vetor com filtros. Ex: `c('pais', 'ncm')`.
#' @param filtros_esp [list] Lista com valores para cada filtro.
#'        Ex: `list(c(160,249))`. Consulte \code{\link{Tabelas_Auxiliares}}.
#' @param detalhamentos [character] Vetor com detalhamentos. Ex: `c('pais', 'ncm')`.
#' @param valor_FOB [logical] Se TRUE, inclui valores FOB.
#' @param valor_kg [logical] Se TRUE, inclui valores de peso líquido (kg).
#' @param qtd_est [logical] Se TRUE, inclui quantidade estatística (necessário detalhamento NCM).
#'
#' @return Data frame com os dados retornados pela API, ou `NULL` se não houver resultados.
#' @examples
#' \dontrun{
#' pesquisar_comex_stat(
#'   ano_inicial = 2022, ano_final = 2022,
#'   mes_inicial = 1, mes_final = 12,
#'   detalha_mes = TRUE,
#'   tipo_op = "imp",
#'   filtros = c("country", "state"),
#'   filtros_esp = list(c(105, 107), c(26, 13)),
#'   detalhamentos = c("country", "state", "ncm")
#' )
#' }
#' @export
#'
search_comex <- function(ano_inicial = format(Sys.Date(), "%Y"),
                                 ano_final = format(Sys.Date(), "%Y"),
                                 mes_inicial = 1, mes_final = 12,
                                 detalha_mes = FALSE,
                                 tipo_op = 'exp', tipo_ord = 'val',
                                 filtros = character(),
                                 filtros_esp = list(),
                                 detalhamentos = character(),
                                 valor_FOB = TRUE, valor_kg = TRUE, qtd_est = FALSE) {

  # Garantir pacotes
  if (!requireNamespace("httr", quietly = TRUE)) stop("Pacote 'httr' é necessário.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Pacote 'jsonlite' é necessário.")

  # Conversão tipo de operação e ordenação (se necessário)
  tipo_op_val <- ifelse(tipo_op == "exp", 1, 2)
  tipo_ord_val <- ifelse(tipo_ord == "val", 1, 2)

  # Preparar parâmetros
  mes_inicial <- sprintf("%02d", mes_inicial)
  mes_final <- sprintf("%02d", mes_final)

  # Montar lista de filtros
  lista_filtros <- list()
  if (length(filtros) > 0) {
    for (i in seq_along(filtros)) {
      vals <- as.character(filtros_esp[[i]])
      lista_filtros[[i]] <- list(filter = filtros[i], values = vals)
    }
  }

  # Montar métricas
  lista_metricas <- c()
  if (valor_FOB) lista_metricas <- c(lista_metricas, "metricFOB")
  if (valor_kg) lista_metricas <- c(lista_metricas, "metricKG")
  if (qtd_est) lista_metricas <- c(lista_metricas, "metricStatistic")

  # Corpo da requisição
  corpo <- list(
    flow = ifelse(tipo_op == "exp", "export", "import"),
    monthDetail = detalha_mes,
    period = list(from = paste0(ano_inicial, "-", mes_inicial),
                  to = paste0(ano_final, "-", mes_final)),
    filters = lista_filtros,
    details = detalhamentos,
    metrics = lista_metricas
  )

  # Chamada à API
  url <- "https://api-comexstat.mdic.gov.br/general?language=pt"


  resp <- httr::POST(
    url,
    body = corpo,
    encode = "json",
    config = httr::config(ssl_verifypeer = 0L)
  )

  if (resp$status_code != 200) {
    stop("Erro na API: ", resp$status_code, " - ", httr::content(resp, "text"))
  }

  dados <- httr::content(resp, as = "parsed", simplifyVector = TRUE)

  # Retornar dataframe
  if (!is.null(dados$data$list) && length(dados$data$list) > 0) {
    return(as.data.frame(dados$data$list))
  } else {
    warning("Nenhum dado retornado para os parâmetros informados.")
    return(NULL)
  }
}
