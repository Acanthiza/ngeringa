

  library("tidyverse")
  library("readxl")
  library("fs")
  library("sf")
  library("tmap")

  dat <- dir_ls(regexp = "xlsx") %>%
    tibble::enframe(name = NULL, value = "path") %>%
    dplyr::mutate(sheets = map(path,readxl::excel_sheets)) %>%
    tidyr::unnest(cols = c(sheets)) %>%
    dplyr::filter(grepl("\\d{4}",sheets)) %>%
    dplyr::mutate(data = map2(path,sheets,read_excel)) %>%
    tidyr::unnest(cols = c(data)) %>%
    setNames(gsub("[A-Z]-","",names(.))) %>%
    dplyr::filter(!is.na(Latitude))
  
  datGeo <- st_as_sf(dat
                     , coords = c("Longitude","Latitude")
                     , crs = 4326
                     )
  
  tm_shape(datGeo) +
    tm_dots(col = "Species")
  