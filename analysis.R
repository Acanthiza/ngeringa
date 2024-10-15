
  # packages -------
  
  library("tidyverse")
  library("readxl")
  library("fs")
  library("sf")
  library("tmap")
  library("lubridate")
  library("terra")
  
  
  # setup -------
  tmap::tmap_mode("view")
  
  tmap::tmap_options(basemaps = c("OpenStreetMap.Mapnik"
                                  , "Esri.WorldImagery"
                                  )
                     , limits = c(facets.plot = 100)
                     , max.raster = c(plot = 1e7, view = 1e6)
                     , max.categories = 42
                     )
  
  
  # lookups ------
  
  taxa <- rio::import("Ngeringa ALL survey data v3.xlsx"
                     , which = "Species-codes"
                     , setclass = "tibble"
                     , col_names = TRUE
                     )
  
  hab <- rio::import("Ngeringa ALL survey data v3.xlsx"
                     , which = "Habitat Codes"
                     , setclass = "tibble"
                     , col_names = FALSE
                     ) %>%
    dplyr::rename(hab = 1, Habitat = 2) %>%
    dplyr::mutate(Habitat = tolower(gsub("-", "", Habitat))
                  , hab = gsub("Remant", "Remnant", hab)
                  )
  
  epsg <- tibble::tribble(
    ~ year, ~ epsg,
    2019, 28354,
    2020, 28354,
    2021, 28354,
    2023, 3857
    ) %>%
    dplyr::mutate(year = factor(year, ordered = TRUE))
  
  # data -------
  
  dat <- rio::import("Ngeringa ALL survey data v3.xlsx"
                     , which = "ALL DATA"
                     , setclass = "tibble"
                     ) %>%
    dplyr::mutate(strCode = Species
                  , Habitat = tolower(gsub("-", "", Habitat))
                  ) %>%
    dplyr::left_join(taxa) %>%
    dplyr::left_join(hab) %>%
    setNames(gsub("[A-Z]-| .*|\\/.*","",names(.))) %>%
    dplyr::filter(!is.na(Lat)) %>%
    dplyr::mutate(date = ymd(gsub("T.*","",Date))
                  , time = gsub(".*T","",Date)
                  , time = gsub("+10:30|010:30|009:30","",time)
                  , time = hms(time)
                  , year = year(date)
                  , year = factor(year, ordered = TRUE)
                  ) %>%
    dplyr::mutate(use_hab = gsub("\\s.*|\\-.*|\\/.*", "", hab)) %>%
    dplyr::filter(!is.na(Count)) %>%
    dplyr::add_count(Species, name = "spp_n") %>%
    dplyr::add_count(use_hab, name = "hab_n")
  
  dat_sf <- dat %>%
    dplyr::mutate(old_east = East
                  , old_north = North
                  ) %>%
    dplyr::mutate(East = dplyr::if_else(year == 2023, old_north, old_east)
                  , North = dplyr::if_else(year == 2023, old_east, old_north)
                  ) %>%
    tidyr::nest(data = -year) %>%
    dplyr::left_join(epsg) %>%
    dplyr::mutate(data = purrr::map2(data
                                     , epsg
                                     , \(x, y) sf::st_as_sf(x
                                                            , coords = c("East", "North")
                                                            , crs = y
                                                            , remove = FALSE
                                                            ) %>%
                                       sf::st_transform(crs = 7854)
                                     )
                  ) %>%
    tidyr::unnest(cols = data) %>%
    sf::st_sf()
  
  
  # Area -------
  
  hec <- dat_sf %>%
    dplyr::summarise() %>%
    sf::st_convex_hull() %>%
    dplyr::mutate(hec = sf::st_area(geometry)
                  , hec = as.numeric(hec / 10000)
                  )
  
  tm_shape(hec) + tm_polygons(col = "year", alpha = 0.5)
  
  # naive occ -----
  
  occ <- dat %>%
    dplyr::group_by(year, Species, strCommonName) %>%
    dplyr::summarise(occ = sum(Count)) %>%
    dplyr::ungroup() %>%
    dplyr::cross_join(hec %>%
                       sf::st_set_geometry(NULL)
                     ) %>%
    dplyr::mutate(occ = occ / hec) %>%
    tidyr::pivot_wider(values_from = "occ", names_from = "year")
  
  
  rio::export(occ, "naive_occ.csv")
  
  # maps ------
  tmap_mode("view")
  
  map <- dat_sf %>%
    dplyr::group_by(year) %>%
    dplyr::summarise() %>%
    dplyr::ungroup() %>%
    tm_shape() +
    tm_dots(col = "year"
            , palette = "viridis"
            ) +
    tm_shape(hec) +
    tm_polygons(alpha = 0.5)
  
  tmap::tmap_save(map, filename = "map.html")
  
  facets <- dat_sf %>%
    dplyr::group_by(year) %>%
    dplyr::summarise() %>%
    dplyr::ungroup() %>%
    tm_shape() +
    tm_dots() +
    tm_facets(by = "year") +
    tm_shape(hec) +
    tm_polygons(alpha = 0.5)

  tmap_mode("plot")
  
  tmap::tmap_save(facets, filename = "facets.png")
  