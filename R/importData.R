#' @title importData: Import GLKN water data package
#'
#' @description This function imports views in GLKN water data packages. The easiest way to work with the data package is to
#' download the published data package from NPS DataStore manually (irma.nps.gov/datastore), or use csvs that match the format
#' of published data packages. The latter option being for importing pre-certified for tasks like performing QC checks. The other option
#' is to download a published data package using the NPSutils package. However, the NPSutils package has a lot of dependencies,
#' including the terra package, which typically fails to install from CRAN. Once NPSutils is installed, the process is less cumbersome.
#' You can import rivers or inland lakes data.
#'
#' Each csv (view) in the data package is added to a GLKN_WQ environment in your workspace, or to your global environment based on
#' whether new_env = TRUE or FALSE. If you want to import both rivers and lakes data, include them in the same importData() function.
#' The views will then be row binded together.
#'
#' @param type Select how to import the data package.
#' \describe{
#' \item{"csv"}{Imports the csv version of the data package views from your local machine. If selected, must provide the
#' filepath for the csvs in the filepath argument. If importing both the rivers and lake data in the same call, data package views
#' must be in separate folders for the two protocols.}
#' \item{"zip"}{Imports the csv version of the data package views from your local machine as a zipped file. If selected, must provide the
#' filepath and the name of the zip file.}
#' }
#'
#' @param filepath Quoted filepath where data package files live if type = "csv" or type = "zip".
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores views in GLKN_WQ
#' environment. If \code{FALSE}, stores views in global environment. If both rivers and lakes data are imported in the same
#' importData() call, they'll be row binded to their respective datasets, and can be queried either by site or site_type.
#'
#'
#' @examples
#' \dontrun{
#'
#' library(waterGLKN)
#'
#' # Download lakes and rivers data package from DataStore
#' devtools::install_github("nationalparkservice/NPSutils") # might take a few tries to install
#' NPSutils::get_data_packages(c("2306516", "2309369"))
#'
#' path_lake = "../data/GLKN_water/2306516"
#' importData(filepath = path_lake)
#'
#' path_river = "../data/GLKN_water/2309369"
#' importData(filepath = path_river)
#'
#' # import both lakes and river data as csvs
#' importData(type = 'csv', filepath = c(path_lake, path_river))
#'
#' # import both lakes and rivers data as zip files
#' river_zip = ("../data/GLKN_water/records-2309369.zip")
#' lake_zip = ("../data/GLKN_water/records-2306516.zip")
#' importData(type = 'zip', filepath = c(river_zip, lake_zip))
#'
#'
#'
#' }
#'
#' @return Assigns water csvs to specified environment
#' @export

importData <- function(type = "csv", filepath = NA, new_env = TRUE){

  #---- Error handling ----
  type <- match.arg(type, c("zip", "csv"))
  stopifnot(class(new_env) == 'logical')

  # check that filepath was specified for non-DSN options
  if(all(is.na(filepath))){stop(paste0("Must specify a filepath to the database"))}

  if(type == 'csv'){
    if(length(filepath) > 1){
    invisible(
      lapply(seq_along(filepath),function(x){
             fp1 = filepath[[x]]
             if(!grepl("/$", fp1)){
               fp <- paste0(fp1, "/")
               filepath[[x]] <<- fp}}))
    } else {if(!grepl("/$", filepath)){filepath <- paste0(filepath, "/")}}
  }

  if(!any(file.exists(filepath))){
    stop(paste0("Specified file path does not exist. ",
                ifelse(any(grepl("sharepoint", filepath)), " Note that file paths from Sharepoint or Teams are not accessible.",
                       "")))}

  # Check if type = 'csv' was specified, but .zip file is filepath
  if(type == 'csv' & any(grepl(".zip", filepath))){stop("Specified a zip file in filepath. Must use type = 'zip' instead of 'csv'.")}

  # Check if type = 'zip' was specified, check that filepath ends in .zip
  if(type == 'zip' & !all(grepl(".zip", filepath))){stop("File path for type = 'zip' requires the name of the zip file in the path.")}

  # Create new environment if new_env = T or set env as Global
  if(new_env == TRUE){GLKN_WQ <<- new.env()}

  env <- if(new_env == TRUE){GLKN_WQ} else {.GlobalEnv}

  # Vector of file names in filepath that end in .csv (ie the data package views)
  wq_views <- c("Characteristics", "HUC", "Locations", "Projects", "Results")

  #---- Import from csvs ----

  if(type == "csv"){
    if(length(filepath) == 1){
    # List csvs in filepath folder
    dp_list <- list.files(filepath, pattern = ".csv")
    # Drop csvs that don't matching names in the wq_views
    dp_list <- dp_list[grepl(paste0(wq_views, collapse = "|"), dp_list)]
    # Drop date stamp (if it exists) from file name if exists in 2 steps
    dp_list_names <- gsub("[[:digit:]]+|.csv", "", dp_list)
    dp_list_names <- gsub("_$","", dp_list_names)

    miss_vws <- setdiff(dp_list_names, wq_views)

    # Check for missing views
    if(length(miss_vws) > 0){stop("Missing the following views from the specified filepath: ",
                                  paste0(miss_vws, collapse = ", "))}

    if(length(dp_list) > length(wq_views)){
      stop(
        "More than one file matching the data package names were detected in the specified filepath
    (e.g. 'Chemistry_Data'). Must specify a filepath that only contains 1 version of each view.")
    }

    # Setup progress bar
    pb <- txtProgressBar(min = 0, max = length(dp_list), style = 3)

    # Import the file names by applying read.csv to the dp_list of file names
    # This will return one list that includes all the datasets as individual elements
    # The na.string = NA and NULL converts them in data to blanks. The check.names = F won't
    # replace invalid characters (eg "+") with "."
    dp_files <- lapply(seq_along(dp_list),
                       function(x){
                         fname = dp_list[[x]]
                         setTxtProgressBar(pb, x)
                         read.csv(paste0(filepath, fname),
                                  na.string = c("NA", "NULL"),
                                  tryLogical = TRUE,
                                  check.names = FALSE)
                       })

    # Set the names of dp_files as the shorter dp_list2 names
    dp_files <- setNames(dp_files, dp_list_names)

    # Takes every element of the dp_files list and saves it to the VIEWS_WQ or global
    # environment as separate, named objects.
    list2env(dp_files, envir = env)

    # Close progress bar
    close(pb)

    } else if(length(filepath == 2)){
      # First data package
      # List csvs in filepath folder
      dp_list1 <- sort(list.files(path = filepath[[1]], pattern = ".csv", full.names = T))
      dp_names1 <- gsub(".csv", "", sort(list.files(path = filepath[[1]], pattern = ".csv")))

      # Drop csvs that don't matching names in the wq_views
      dp_list1 <- dp_list1[grepl(paste0(wq_views, collapse = "|"), dp_list1)]
      dp_names1 <- dp_names1[grepl(paste0(wq_views, collapse = "|"), dp_names1)]

      miss_vws1 <- setdiff(dp_names1, wq_views)

      # Check for missing views
      if(length(miss_vws1) > 0){stop(paste0("Missing the following views from the following file path: ",
                                           filepath[[1]], ": ",
                                    paste0(miss_vws1, collapse = ", "))
      )}

      if(length(dp_list1) > length(wq_views)){
        stop(paste0(
          "More than one file matching the data package view names were detected in the same folder
          (e.g. 'Locations'), for ", filepath[1], ". Must specify a filepath that only contains 1 version of each view.")
        )
      }

      #2nd data package
      dp_list2 <- sort(list.files(path = filepath[[2]], pattern = ".csv", full.names = T))
      dp_names2 <- gsub(".csv", "", sort(list.files(path = filepath[[2]], pattern = ".csv")))

      # Drop csvs that don't matching names in the wq_views
      dp_list2 <- dp_list2[grepl(paste0(wq_views, collapse = "|"), dp_list2)]
      dp_names2 <- dp_names2[grepl(paste0(wq_views, collapse = "|"), dp_names2)]

      miss_vws2 <- setdiff(dp_names2, wq_views)

      # Check for missing views
      if(length(miss_vws2) > 0){stop(paste0("Missing the following views from the following file path: ",
                                           filepath[[2]], ": ",
                                           paste0(miss_vws2, collapse = ", "))
      )}

      if(length(dp_list2) > length(wq_views)){
        stop(paste0(
          "More than one file matching the data package view names were detected in the same folder
          (e.g. 'Locations'), for ", filepath[2], ". Must specify a filepath that only contains 1 version of each view.")
          )
          }

      # Setup progress bar
      pb <- txtProgressBar(min = 0, max = length(dp_list1), style = 3)

      # Import the file names by applying read.csv to the dp_list of file names
      # This will return one list that includes all the datasets as individual elements
      # The na.string = NA converts "NA" in data to blanks. The check.names = F won't
      # replace invalid characters (eg "+") with "."
      dp_files <- lapply(seq_along(wq_views),
                         function(x){
                           fname1 = dp_list1[[x]]
                           fname2 = dp_list2[[x]]
                           setTxtProgressBar(pb, x)
                           rbind(
                             read.csv(fname1, na.string = c("NA", "NULL"), tryLogical = TRUE, check.names = FALSE),
                             read.csv(fname2, na.string = c("NA", "NULL"), tryLogical = TRUE, check.names = FALSE))
                         })

      # Set the names of dp_files as the shorter dp_list2 names
      dp_files <- setNames(dp_files, dp_names1)

      # Takes every element of the dp_files list and saves it to the VIEWS_WQ or global
      # environment as separate, named objects.
      list2env(dp_files, envir = env)
    close(pb)
    }
    }

  #---- import zips ----
  if(type == "zip"){
    if(length(filepath) == 1){
    # Check if can read files within the zip file
    tryCatch(
      {zfiles = utils::unzip(filepath, list = T)$Name
      },
      error = function(e){stop(paste0("Unable to import specified zip file."))})

    z_list = sort(zfiles[grepl(paste0(wq_views, collapse = "|"), zfiles)])

    # Drop date stamp (if it exists) from file name if exists in 2 steps
    z_list_names <- gsub("[[:digit:]]+|.csv", "", z_list)
    z_list_names <- gsub("./", "", z_list_names)
    z_list_names <- gsub("_$","", z_list_names)

    miss_vws <- setdiff(z_list_names, wq_views)

    # Check for missing views
    if(length(miss_vws) > 0){stop("Missing the following views from the specified filepath: ",
                                  paste0(miss_vws, collapse = ", "))}

    if(length(z_list) > length(wq_views)){
      stop(
        "More than one file matching the data package names were detected in the specified filepath
    (e.g. 'Chemistry_Data'). Must specify a filepath that only contains 1 version of each view.")
    }

    # Since the missing test passed, clean up files so only includes names in view_list, but
    # maintain order in files

    # Import views now that all tests passed
    pb <- txtProgressBar(min = 0, max = length(z_list), style = 3)

    wqviews1 <- unzip(filepath, junkpaths = TRUE, exdir = tempdir())
    wqviews <- sort(wqviews1[grepl(".csv", wqviews1)])

    view_import <-
      lapply(seq_along(wqviews), function(x){
        setTxtProgressBar(pb,x)
        read.csv(wqviews[x], na.string = c("NA", "NULL"), check.names = FALSE)})

    view_import <- setNames(view_import, z_list_names)
    list2env(view_import, envir = env)
    # Close progress bar
    close(pb)

    } else if(length(filepath == 2)){

      # Check that both zips can be read
      tryCatch({zfiles1 = utils::unzip(filepath[[1]], list = T)$Name},
                error = function(e){stop(paste0("Unable to import specified zip file: ", filepath[[1]]))})

      tryCatch({zfiles2 = utils::unzip(filepath[[2]], list = T)$Name},
                error = function(e){stop(paste0("Unable to import specified zip file: ", filepath[[2]]))})

      zlist1 = sort(zfiles1[grepl(paste0(wq_views, collapse = "|"), zfiles1)])
      zlist2 = sort(zfiles2[grepl(paste0(wq_views, collapse = "|"), zfiles2)])

      miss_vws1 <- setdiff(gsub(".csv", "", zlist1), wq_views)
      miss_vws2 <- setdiff(gsub(".csv", "", zlist2), wq_views)

      # Check for missing views
      if(length(miss_vws1) > 0){stop(paste0("Missing the following views from the following zip: ",
                                     filepath[[1]], paste0(miss_vws1, collapse = ", ")))}
      if(length(miss_vws2) > 0){stop(paste0("Missing the following views from the following zip: ",
                                            filepath[[2]], paste0(miss_vws2, collapse = ", ")))}

      pb <- txtProgressBar(min = 0, max = length(zlist1), style = 3)
      if(!file.exists(paste0(tempdir(), "\\1"))){dir.create(paste0(tempdir(), "\\1"))}
      if(!file.exists(paste0(tempdir(), "\\2"))){dir.create(paste0(tempdir(), "\\2"))}

      z_names <- gsub("[[:digit:]]+|.csv", "", zlist1)
      z_names <- gsub("./", "", z_names)
      z_names <- gsub("_$","", z_names)

      vw1 <- unzip(filepath[[1]], junkpaths = T, exdir = paste0(tempdir(), "\\1"))
      vw2 <- unzip(filepath[[2]], junkpaths = T, exdir = paste0(tempdir(), "\\2"))

      vw1 <- sort(vw1[grepl(".csv", vw1)])
      vw2 <- sort(vw2[grepl(".csv", vw2)])

      view_import <-
        lapply(seq_along(wq_views),
             function(x){
               fname1 = vw1[[x]]
               fname2 = vw2[[x]]
               setTxtProgressBar(pb, x)
               f1 <- read.csv(fname1, na.string = c("NA", "NULL"), tryLogical = TRUE, check.names = FALSE)
               f2 <- read.csv(fname2, na.string = c("NA", "NULL"), tryLogical = TRUE, check.names = FALSE) 
               col_names <- intersect(names(f1), names(f2))
               rbind(f1[,col_names], f2[,col_names])
             })
      view_import <- setNames(view_import, z_names)
      list2env(view_import, envir = env)
      close(pb)

      } # end of fp 2
  }

  # Add param abbr column to Results view
  #load("./R/sysdata.rda") # for GLKN_WQ_abbreviations
  data(GLKN_WQ_abbreviations, envir = environment())
  # print(environment())
  Results1 <- get("Results", envir = env)
  Results <- merge(GLKN_WQ_abbreviations, Results1, by = c("Characteristic_Name", "Result_Unit"), all.y = T)
  assign("Results", Results, envir = env)

  # Print message in console
  print(ifelse(new_env == TRUE,
               paste0(" Import complete. Views are located in GLKN_WQ environment."),
               paste0(" Import complete. Views are located in global environment.")), quote = FALSE)
}
