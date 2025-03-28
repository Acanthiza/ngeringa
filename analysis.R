
  # packages -------
  
  packages <- c("base"
                
                # tidyverse
                , "dplyr"
                , "tidyr"
                , "purrr"
                , "tibble"
                , "magrittr"
                , "ggplot2"
                , "readxl"
                , "fs"
                , "lubridate"
                , "rio"
                
                # mapping
                , "sf"
                , "tmap"
                , "terra"
                
                # report
                , "knitr"
                , "rmarkdown"
                , "bookdown"
                
                # env
                , "envFunc"
                )
  
  envFunc::check_packages(packages
                          , lib = TRUE
                          , bib = TRUE
                          , file = "packages.bib"
                          )
  
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
    2023, 3857,
    2024, 3857
    ) %>%
    dplyr::mutate(year = factor(year, ordered = TRUE))
  
  # data -------
  
  dat <- rio::import("Ngeringa ALL survey data v3.xlsx"
                     , which = "ALL DATA"
                     , setclass = "tibble"
                     ) %>%
    dplyr::mutate(Species = gsub("CBK", "BKB", Species)
                  , Species = gsub("WPG", "WPH", Species)
                  ) |>
    dplyr::mutate(strCode = Species
                  , Habitat = tolower(gsub("-", "", Habitat))
                  ) %>%
    dplyr::left_join(taxa) %>%
    dplyr::left_join(hab) %>%
    setNames(gsub("[A-Z]-| .*|\\/.*","",names(.))) %>%
    dplyr::filter(!is.na(Lat)) %>%
    dplyr::mutate(date = ymd(gsub("T.*","",Date))
                  , time = gsub(".*T","",Date)
                  , time = gsub("\\+10:30|010:30|009:30","",time)
                  , time = hms(time)
                  , year = lubridate::year(date)
                  , year = factor(year, ordered = TRUE)
                  ) %>%
    dplyr::mutate(use_hab = gsub("\\s.*|\\-.*|\\/.*", "", hab)) %>%
    dplyr::filter(!is.na(Count))
  
  dat_sf <- dat %>%
    dplyr::mutate(old_east = East
                  , old_north = North
                  ) %>%
    dplyr::mutate(East = dplyr::if_else(year %in% c(2023, 2024), old_north, old_east)
                  , North = dplyr::if_else(year %in% c(2023, 2024), old_east, old_north)
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
                                       sf::st_transform(crs = 3857)
                                     )
                  ) %>%
    tidyr::unnest(cols = data) %>%
    sf::st_sf()
  
  
  if(FALSE) {
    
    tm_shape(dat_sf) + tm_dots(col = "year")
    
    
  }
  
  
  # Area -------
  
  hec <- dat_sf %>%
    dplyr::summarise() %>%
    sf::st_convex_hull() %>%
    dplyr::mutate(hec = sf::st_area(geometry)
                  , hec = as.numeric(hec / 10000)
                  )
  
  tm_shape(hec) + tm_polygons(alpha = 0.5)
  
  # raster ------
  
  r <- terra::rast(crs = paste0("epsg:", 3857)
                   , resolution = c(50, 50)
                   , extent = terra::ext(hec)
                   )
  
  r_poly <- r |>
    terra::as.polygons() |>
    sf::st_as_sf()
  
  tm_shape(r_poly) +
    tm_polygons(alpha = 0.03) +
  tm_shape(dat_sf) +
    tm_dots(col = "year")
  
  naive_occ <- dat_sf |>
    tidyr::nest(data = -c(Species, year)) |>
    dplyr::mutate(r = purrr::map(data
                                  , \(x) terra::rasterize(x = terra::vect(x)
                                                          , y = r
                                                          , field = "Count"
                                                          , fun = \(x) sum(x) > 0
                                                          )
                                  )
                  , cells = purrr::map_dbl(r, \(x) as.numeric(terra::global(x, "sum", na.rm = TRUE)))
                  )
  
  tm_shape(r_poly) +
    tm_borders() +
  tm_shape(naive_occ$r[[1]]) +
    tm_raster() +
  tm_shape(dat_sf |>
             dplyr::filter(Species == naive_occ$Species[[1]])
           ) +
    tm_dots(col = "year")
  
  
  # JAG 24/02/2025
  # PUT IN 1 HA GRID and do p/a within grid
  
  # total count -----
  
  total_count <- dat %>%
    dplyr::group_by(year, Species, strCommonName) %>%
    dplyr::summarise(occ = sum(Count)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(values_from = "occ", names_from = "year")
  
  
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
  
  
  # year by count -------
  
  dat |>
    dplyr::filter(hab != "oh") |>
    dplyr::group_by(strCommonName , year) |>
    dplyr::summarise(n = sum(Count)) |>
    dplyr::ungroup() |>
    ggplot(aes(year, n)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ strCommonName)
  
  # year by cell -------
  
  naive_occ |>
    dplyr::left_join(taxa
                     , by = c("Species" = "strCode")
                     ) |>
    ggplot(aes(year, cells)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ strCommonName)
  
  # report -------
  # Clean up previous knits
  
  fs::file_delete(fs::dir_ls(regexp = "_book|_main"))
  
  bookdown::render_book(output_dir = "docs")
  
  envFunc::git_commit_env(paste0("Successful render of report: "
                                 , format(Sys.Date(), "%Y-%m-%d")
                                 )
                          )
  