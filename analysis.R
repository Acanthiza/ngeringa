
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
  
  hab <-  rio::import("Ngeringa ALL survey data v3.xlsx"
                     , which = "Habitat Codes"
                     , setclass = "tibble"
                     , col_names = FALSE
                     ) %>%
    dplyr::rename(hab = 1, Habitat = 2) %>%
    dplyr::mutate(Habitat = tolower(gsub("-", "", Habitat))
                  , hab = gsub("Remant", "Remnant", hab)
                  )
  
  
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
                  ) %>%
    dplyr::mutate(use_hab = gsub("\\s.*|\\-.*|\\/.*", "", hab)) %>%
    dplyr::filter(!is.na(Count)) %>%
    dplyr::add_count(Species, name = "spp_n") %>%
    dplyr::add_count(use_hab, name = "hab_n")
  
  datGeo <- st_as_sf(dat
                     , coords = c("Long", "Lat")
                     , crs = 4326
                     ) %>%
    sf::st_transform(crs = 7845)
  
  tm_shape(datGeo) +
    tm_dots(col = "Species")
  
  
  mod <- rstanarm::stan_glmer(Count ~ year + (year | Species) + (1 | use_hab)
                              , data = dat
                              , family = stats::poisson()
                              )
  
  
  
  #----------Site raster----------
  
  if(FALSE) {
  
    #-------Satellite data---------
  
  siteExtent <- st_bbox(datGeo) %>%
    st_as_sfc() %>%
    st_buffer(50)
  
  ras_dir <- fs::path("H:"
                      , "data"
                      , "raster"
                      , "aligned"
                      , "sa_ibrasub_xn____0__30"
                      )
  
  env_info <- envRaster::parse_env_tif(ras_dir, cube = FALSE) %>%
    dplyr::filter(!is.na(band)) %>%
    dplyr::mutate(name = paste0(band, "__", season))
  
  env <- terra::rast(env_info$path) %>%
    terra::crop(siteExtent)
  
  names(env) <- env_info$name
  
    cell_vals <- datGeo %>%
      dplyr::mutate(cell = terra::cellFromXY(env, sf::st_coordinates(datGeo))) %>%
      st_set_geometry(NULL) %>%
      as_tibble() %>%
      dplyr::count(cell, use_hab, name = "value") %>%
      dplyr::group_by(cell) %>%
      dplyr::filter(value == max(value)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = as.numeric(as.factor(use_hab)))
    
    mod_data <- cell_vals %>%
      dplyr::filter(!is.na(cell)
                    , !is.na(use_hab)
                    , !use_hab %in% c("Over", "Heard")
                    ) %>%
      dplyr::add_count(use_hab, name = "n") %>%
      dplyr::filter(n > 3) %>%
      dplyr::mutate(use_hab = forcats::fct_infreq(use_hab))
    
    env_data <- terra::extract(env, mod_data$cell)
    
    mod <- randomForest::randomForest(x = env_data
                                      , y = mod_data$use_hab
                                      , ntree = 3000
                                      , sampsize = rep(min(table(mod_data$use_hab)), length(table(mod_data$use_hab)))
                                      , mtry = 2
                                      )
    
    pred <- terra::predict(env
                           , mod
                           )
    
    tm_shape(pred) + tm_raster(alpha = 0.8)
    
  }
  