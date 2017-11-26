
#' Mescla datasets
#'
#' Função que captura os datasets por mes e mescla em um único dataset do ano.
#' @param type O tipo do dataset. Valores possiveis: "raw", "wrangled", "spatial", "site"
#' @param crime O crime que se deseja mesclar os dados
#' @param year O ano dos datasets
#' @param months Um vetor de caracteres contendo os meses. Deve conter duas casas ou zero à esquerda
#' @export
#' @examples
#' # Recupera o dataset de furto de veiculos do ano de 2012
#' mescla_datasets("spatial", "furtoveiculo", 2012)
#'
#' # Recupera o dataset de furto de veiculos do ano de 2015 dos meses de Março a Agosto
#' mescla_datasets("spatial", "furtoveiculo", 2015, c('03', '04', '05', '06', '07', '08'))
mescla_datasets <- function(type, crime, year, months = get_months()) {
  default_location <- '/run/media/rmartine/TOSHIBA EXT/big-data-projects/ssp/'

  dataset <- data.frame()

  for(month in months) {
    dataset_month <- read.csv(paste(default_location,
                                    type,
                                    '/',
                                    year,
                                    '/',
                                    crime,
                                    '_',
                                    year,
                                    '_',
                                    month,
                                    '.csv',
                                    sep=''))

    dataset <- bind_rows(dataset, dataset_month)
  }
  rm(dataset_month)

  return(dataset[,-1])
}

#' Get Months
#' Função auxiliar para recuperar um vetor de caracteres de meses,
#' muito útil para utilizar com a função mescla_datasets()
#' @param start O mês inicial (opcional)
#' @param end O mês final (opcional)
get_months <- function(start = 1, end = 12) {
  months <- seq(start, end, 1)
  smonths <- sprintf("%02d", months)

  return(smonths)
}

#' Recupera Datasets
#' Recupera o dataset de um ano especifico.
#' @param type O tipo do dataset
#' @param crime O tipo do crime
#' @param years O ano que se deseja recuperar o dataset
recupera_datasets <- function(type, crime, years) {
  dataset <- NA

  for(year in years) {
    dataset_year <- mescla_datasets(type, crime, year)
    dataset <- rbind(dataset, dataset_year)
  }
  rm(dataset_year)

  return(dataset)
}

numero_nas <- function(dataset) {
  na_count <- map(dataset, ~ sum(is.na(.)))

  return(unlist(na_count))
}

### Tratamento dos dados de coordenadas
trata_latitudes <- function(x) {
  if(x != "[]" & x != "" & !is.na(x)) {
    coords <- str_replace_all(x, "u'", "'")
    coords <- str_replace_all(coords, "'", '"')
    coords <- str_replace_all(coords, ' u"', ' "')
    coords <- str_replace_all(coords, 'd"Avila', 'd Avila')
    coords <- str_replace_all(coords, "\\\\xba", "o.")
    coords <- str_replace_all(coords, "\\\\xc1", "A")
    coords <- str_replace_all(coords, "\\\\xc2", "A")
    coords <- str_replace_all(coords, "\\\\xcd", "I")
    coords <- str_replace_all(coords, "\\\\xe9", "e")
    coords <- str_replace_all(coords, "\\\\xea", "e")
    coords <- str_replace_all(coords, "\\\\xe1", "a")
    coords <- str_replace_all(coords, "\\\\xe2", "a")
    coords <- str_replace_all(coords, "\\\\xe3", "a")
    coords <- str_replace_all(coords, "\\\\xe7", "c")
    coords <- str_replace_all(coords, "\\\\xed", "i")
    coords <- str_replace_all(coords, "\\\\xf3", "o")
    coords <- str_replace_all(coords, "\\\\xf4", "o")
    coords <- str_replace_all(coords, "\\\\xf5", "o")
    coords <- str_replace_all(coords, "\\\\xfa", "u")
    coords <- str_replace_all(coords, "True", '"true"')

    coords <- fromJSON(coords)

    return(coords[1,]$geometry$location$lat)
  } else {
    return(NA)
  }
}

trata_longitudes <- function(x) {
  if(x != "[]" & x != "" & !is.na(x)) {
    coords <- str_replace_all(x, "u'", "'")
    coords <- str_replace_all(coords, "'", '"')
    coords <- str_replace_all(coords, ' u"', ' "')
    coords <- str_replace_all(coords, 'd"Avila', 'd Avila')
    coords <- str_replace_all(coords, "\\\\xba", "o.")
    coords <- str_replace_all(coords, "\\\\xc1", "A")
    coords <- str_replace_all(coords, "\\\\xc2", "A")
    coords <- str_replace_all(coords, "\\\\xcd", "I")
    coords <- str_replace_all(coords, "\\\\xe9", "e")
    coords <- str_replace_all(coords, "\\\\xea", "e")
    coords <- str_replace_all(coords, "\\\\xe1", "a")
    coords <- str_replace_all(coords, "\\\\xe2", "a")
    coords <- str_replace_all(coords, "\\\\xe3", "a")
    coords <- str_replace_all(coords, "\\\\xe7", "c")
    coords <- str_replace_all(coords, "\\\\xed", "i")
    coords <- str_replace_all(coords, "\\\\xf3", "o")
    coords <- str_replace_all(coords, "\\\\xf4", "o")
    coords <- str_replace_all(coords, "\\\\xf5", "o")
    coords <- str_replace_all(coords, "\\\\xfa", "u")
    coords <- str_replace_all(coords, "True", '"true"')


    coords <- fromJSON(coords)

    return(coords[1,]$geometry$location$lng)
  } else {
    return(NA)
  }
}
