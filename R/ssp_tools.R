
#' Mescla datasets
#'
#' Funcao que captura os datasets por mes e mescla em um unico dataset do ano.
#' @param type O tipo do dataset. Valores possiveis: "raw", "wrangled", "spatial", "site"
#' @param crime O crime que se deseja mesclar os dados
#' @param year O ano dos datasets
#' @
mescla_datasets <- function(type, crime, year, months) {
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

get_months <- function(start = 1, end = 12) {
  months <- seq(start, end, 1)
  smonths <- sprintf("%02d", months)

  return(smonths)
}

recupera_datasets <- function(type, crime, years) {
  months <- get_months()
  dataset <- NA

  for(year in years) {
    dataset_year <- mescla_datasets(type, crime, year, months)
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
