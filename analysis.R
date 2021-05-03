

  library("tidyverse")
  library("readxl")
  library("fs")
  library("sf")
  library("tmap")
  library("lubridate")
  library("raster")

  dat <- dir_ls(regexp = "xlsx") %>%
    tibble::enframe(name = NULL, value = "path") %>%
    dplyr::filter(!grepl("~",path)) %>%
    dplyr::mutate(sheets = map(path,readxl::excel_sheets)) %>%
    tidyr::unnest(cols = c(sheets)) %>%
    dplyr::filter(grepl("ALL",sheets)) %>%
    dplyr::mutate(data = map2(path,sheets,read_excel)) %>%
    tidyr::unnest(cols = c(data)) %>%
    setNames(gsub("[A-Z]-| .*|\\/.*","",names(.))) %>%
    dplyr::filter(!is.na(Latitude)) %>%
    dplyr::mutate(date = ymd(gsub("T.*","",Date))
                  , time = gsub(".*T","",Date)
                  , time = gsub(":\\d{3}.*","",time)
                  , time = hm(time)
                  , hab = substr(Habitat,1,3)
                  )
  
  datGeo <- st_as_sf(dat
                     , coords = c("Longitude","Latitude")
                     , crs = 4326
                     ) %>%
    st_transform(crs = 8059)
  
  tm_shape(datGeo) +
    tm_dots() +
    tm_facets(by = "Species")
  
  
  #-------Sentinel data---------
  
  siteExtent <- st_bbox(datGeo) %>%
    st_as_sfc() %>%
    st_buffer(500)
  
  # Set paths
  dir_create(path("out","sentinel"))
  outDir  <- path("out","sentinel","sen2r_out") # output folder
  safeDir <- path("out","sentinel","sen2r_safe")  # folder to store downloaded SAFE
  
  st_write(siteExtent,path("out","sentinel","extent.geojson"))
  
  sentinelExtent <- path("out","sentinel","extent.geojson")
  
  library(sen2r)
  
  write_scihub_login(Sys.getenv("COPERNICUS_user")
                     , Sys.getenv("COPERNICUS_pwd")
                     )
  
  out_paths_1 <- sen2r(gui = FALSE
                       , preprocess = FALSE
                       , list_prods = c("BOA", "TOA")
                       , list_indices = "NDVI"
                       , step_atmcorr = "auto"
                       , extent = sentinelExtent
                       , extent_name = "ngeringa"
                       , timewindow = c(as.Date("2020-01-01"), as.Date("2020-01-31"))
                       , path_l2a = safeDir
                       , path_out = outDir
                       )
  
  
  #----------Site raster----------
  
  cellVals <- datGeo %>%
    dplyr::mutate(cell = cellFromXY(siteRaster,as_Spatial(datGeo$geometry))) %>%
    st_set_geometry(NULL) %>%
    as_tibble() %>%
    dplyr::filter(!hab %in% c("OH","HO")) %>%
    dplyr::count(cell,hab, name = "value") %>%
    dplyr::group_by(cell) %>%
    dplyr::filter(value == max(value)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = as.numeric(as.factor(hab)))
  
  cellRAT <- tibble(cell = 1:ncell(siteRaster)) %>%
    dplyr::left_join(cellVals)
  
  siteRaster <- setValues(siteRaster,cellRAT$value)
  
  plot(siteRaster)  
  