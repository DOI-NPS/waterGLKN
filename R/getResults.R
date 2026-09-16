#' @include getLocations.R
#'
#' @title getResults: query GLKN water data
#'
#' @description Queries GLKN water data by site, year, month, parameter, etc.
#'
#' @importFrom dplyr filter full_join left_join
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#'  \describe{
#'  \item{"all"}{Includes all parks in the network}
#'  \item{"APIS"}{Apostle Islands National Lakeshore}
#'  \item{"INDU"}{Indiana Dunes National Park}
#'  \item{"ISRO"}{Isle Royale National Park}
#'  \item{"MISS"}{Mississippi National River and Recreation Area}
#'  \item{"PIRO"}{Pictured Rocks National Lakeshore}
#'  \item{"SLBE"}{Sleeping Bear National Lakeshore}
#'  \item{"SACN"}{St. Croix National Scenic Riverway}
#'  \item{"VOYA"}{Voyageurs National Park}
#'  }
#'
#' @param site Filter on Location_ID. Easiest way to pick a site. Defaults to "all". Accepted sites are below.
#' If new sites are added, need to be added to this function as an accepted site.
#'
#' Rivers: c('INDU_04', 'MISS_UM814', 'MISS_UM822', 'MISS_UM852', 'MISS_UM862', 'MISS_UM868', 'MISS_UM880', 'SACN_APLE_0.5',
#'           'SACN_KINI_2.2', 'SACN_NAKA_4.8', 'SACN_NAKA_41.3', 'SACN_NAKA_74.5', 'SACN_NAKA_84.6',
#'           'SACN_SNKE_0.5', 'SACN_STCR_43.7', 'SACN_STCR_63.8', 'SACN_STCR_89.7' 'SACN_STCR_104.0', 'SACN_STCR_138.9')
#'
#' Lakes: c('APIS_01', 'APIS_02', 'APIS_03', 'APIS_04', 'INDU_01', 'INDU_02', 'INDU_05',
#'           'ISRO_01', 'ISRO_02', 'ISRO_03', 'ISRO_04', 'ISRO_05', 'ISRO_06', 'ISRO_07', 'ISRO_08', 'ISRO_09',
#'           'ISRO_13', 'ISRO_14', 'ISRO_19', 'ISRO_20', 'ISRO_21', 'ISRO_22', 'ISRO_24', 'ISRO_30', 'ISRO_32',
#'           'PIRO_01', 'PIRO_02', 'PIRO_03', 'PIRO_04', 'PIRO_05', 'PIRO_06', 'PIRO_07', 'PIRO_08',
#'           'SACN_PACQ_SP_01', 'SACN_PHIP_SP_01', 'SACN_STCR_2.0', 'SACN_STCR_15.8', 'SACN_STCR_20.0',
#'           'SLBE_01', 'SLBE_02', 'SLBE_03', 'SLBE_04', 'SLBE_05', 'SLBE_07', 'SLBE_08', 'SLBE_09', 'SLBE_10', 'SLBE_11',
#'           'VOYA_01', 'VOYA_02', 'VOYA_03', 'VOYA_04', 'VOYA_05', 'VOYA_06', 'VOYA_07', 'VOYA_08', 'VOYA_09', 'VOYA_10',
#'           'VOYA_11', 'VOYA_12', 'VOYA_13', 'VOYA_14', 'VOYA_15', 'VOYA_16', 'VOYA_17', 'VOYA_18', 'VOYA_19',
#'           'VOYA_20', 'VOYA_21', 'VOYA_22', 'VOYA_23', 'VOYA_24', 'VOYA_25')
#'
#' Impoundments: c('SACN_CLAM_0.7', 'SACN_STCR_53.9', 'SACN_WILO_0.5')
#'
#' @param site_type Quoted string to select either inland lake or stream sites.  Note that impounded sites are not actively
#' monitored sites, so active = F would need to be specified to return data for those sites. Options are:
#' \describe{
#'  \item{"all"}{Includes all location types}
#'  \item{"lake"}{Location_types that = "Lake"}
#'  \item{"river"}{Location_types that = "River/Stream"}
#'  \item{"impound"}{Location_types that = "Riverine Impoundment"}
#'  }
#'
#' @param sample_type Select the sample type. Options available are below Can only choose one option.
#' \describe{
#' \item{"all"}{All possible sampling events.}
#' \item{"VS"}{Default. GLKN Vital Signs monitoring events, which matches all non-QAQC Activity_Types.}
#' \item{"QC"}{Includes only include Quality Control sampling events listed in Activity_Type.}
#' }
#'
#' @param years Numeric. Years to query. Accepted values start at 2007.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Default month range is
#' 4:11, as most non-QAQC sample events occur between the months of April and November.
#'
#' @param active Logical. If TRUE (Default), only returns actively monitored locations. If FALSE, returns all
#' locations that have been monitored at least once since 2007. Active sites are defined as sites that have at
#' least one sampling event between 2014 and 2024. See "./scripts/active_sites.R" for more details.
#'
#' @param parameter Specify the parameter(s) to return. Note if additional parameters are added to the Results view,
#' they need to be added as accepted values in this function. Current accepted values are:
#'    c("Alkalinity_mgL", "Ca_mgL", "ChlA_mgm3", "ChlA_Pheo_pct", "ChlA_ppb", "ChlA_ugL", "Cl_mgL", "Depth_m",
#'      "DO_mgL", "DOC_mgL", "DOsat_pct", "Hg_ngL", "HgMethyl_ngL", "K_mgL", "Mg_mgL", "N_ugL", "NA_mgL",
#'      "NH4_ugL", "NO2+NO3_ugL", "P_ugL", "pH", "SecchiCond", "Secchi_m", "Si_mgL", "SO4_mgL",
#'      "SpecCond_uScm", "TempAir_C", "TempWater_C", "Transp_cm", "TSS_mgL", "Turbidity_NTU", "WaterLevel_m",
#'      "WaterLevelRef_m", "WaveHt_cm", "WaveHt_m",
#'      "WindCond", "WindDir_Deg")
#'
#' @param sample_depth Quoted string. Filters on sample depth. If "all" (default), returns all sample depths.
#' If "surface", only returns data from the uppermost sample.
#'
#' @param include_censored Logical. If TRUE, the value column includes non-censored and censored values
#' using the Lower/Upper Quantification Limits values. Censored values are indicated by censored = TRUE, and
#' are defined as records where the Result_Detection_Condition = Present Above/Below Quantification Limit.
#' If FALSE (Default), only values with Result_Detection_Condition = "Detected and Quantified"
#' are returned in the value column.
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields
#' (output = "short"; default.)
#'
#' @return Data frame of GLKN water data
#'
#' @examples
#' \dontrun{
#' # RUN IMPORT FIRST: import both lakes and rivers data as zip files
#' river_zip = ("../data/GLKN_water/records-2309369.zip")
#' lake_zip = ("../data/GLKN_water/records-2306516.zip")
#' importData(type = 'zip', filepath = c(river_zip, lake_zip))
#'
#' # get all non-QC, non censored samples at all depths
#' res <- getResults()
#'
#' # include censored samples
#' resC <- getResults(include_censored = T)
#'
#' # return samples from 2024 only
#' res24 <- getResults(years = 2024)
#'
#' # Get only pH for SACN sites
#' pH_SACN <- getResults(park = "SACN", parameter = "pH")
#'
#' # Get water temp data for surface measurements of lakes only
#' res_surf <- getResults(sample_depth = "surface", site_type = 'lake')
#'
#'
#' }
#' @export

getResults <- function(park = "all", site = "all", site_type = "all",
                       sample_type = "VS",
                       years = 2007:format(Sys.Date(), "%Y"),
                       months = 4:11,
                       active = TRUE,
                       parameter = "all",
                       sample_depth = 'all',
                       include_censored = FALSE,
                       output = "short"){

  #---- Error handling ----
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APIS", "GRPO", "INDU", "ISRO", "MISS", "PIRO", "SLBE", "SACN", "VOYA"))
  if(any(park == "all")){park = c("APIS", "INDU", "ISRO", "MISS", "PIRO", "SLBE", "SACN", "VOYA")} else {park}

  site_type <- match.arg(site_type, several.ok = TRUE, c("all", "impound", "lake", "river"))
  if(any(site_type == "all")){site_type = c("lake", "river", "impound")}

  sample_type <- match.arg(sample_type, several.ok = T, c("all", "VS", "QC"))

  Rivers <- c('INDU_04', 'MISS_UM814', 'MISS_UM822', 'MISS_UM852', 'MISS_UM862', 'MISS_UM868', 'MISS_UM880', 'SACN_APLE_0.5',
            'SACN_KINI_2.2', 'SACN_NAKA_4.8', 'SACN_NAKA_41.3', 'SACN_NAKA_74.5', 'SACN_NAKA_84.6',
            'SACN_SNKE_0.5', 'SACN_STCR_43.7', 'SACN_STCR_63.8', 'SACN_STCR_89.7', 'SACN_STCR_104.0', 'SACN_STCR_138.9')

  Lakes <- c('APIS_01', 'APIS_02', 'APIS_03', 'APIS_04', 'INDU_01', 'INDU_02', 'INDU_05',
             'ISRO_01', 'ISRO_02', 'ISRO_03', 'ISRO_04', 'ISRO_05', 'ISRO_06', 'ISRO_07', 'ISRO_08', 'ISRO_09',
             'ISRO_13', 'ISRO_14', 'ISRO_19', 'ISRO_20', 'ISRO_21', 'ISRO_22', 'ISRO_24', 'ISRO_30', 'ISRO_32',
             'PIRO_01', 'PIRO_02', 'PIRO_03', 'PIRO_04', 'PIRO_05', 'PIRO_06', 'PIRO_07', 'PIRO_08',
             'SACN_PACQ_SP_01', 'SACN_PHIP_SP_01', 'SACN_STCR_2.0', 'SACN_STCR_15.8', 'SACN_STCR_20.0',
             'SLBE_01', 'SLBE_02', 'SLBE_03', 'SLBE_04', 'SLBE_05', 'SLBE_07', 'SLBE_08', 'SLBE_09', 'SLBE_10', 'SLBE_11',
             'VOYA_01', 'VOYA_02', 'VOYA_03', 'VOYA_04', 'VOYA_05', 'VOYA_06', 'VOYA_07', 'VOYA_08', 'VOYA_09', 'VOYA_10',
             'VOYA_11', 'VOYA_12', 'VOYA_13', 'VOYA_14', 'VOYA_15', 'VOYA_16', 'VOYA_17', 'VOYA_18', 'VOYA_19',
             'VOYA_20', 'VOYA_21', 'VOYA_22', 'VOYA_23', 'VOYA_24', 'VOYA_25')

  Impoundments <- c('SACN_CLAM_0.7', 'SACN_STCR_53.9', 'SACN_WILO_0.5')

  site <- match.arg(site, several.ok = TRUE, c("all", Rivers, Lakes))
  if(any(site == "all")){site = c(Rivers, Lakes, Impoundments)} else {site}

  stopifnot(is.logical(active))

  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2007)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))

  sample_depth <- match.arg(sample_depth, c("surface", "all"))

  params <- c("Alkalinity_mgL", "Ca_mgL", "ChlA_mgm3", "ChlA_Pheo_pct", "ChlA_ppb", "ChlA_ugL", "Cl_mgL", "Depth_m",
              "DO_mgL", "DOC_mgL", "DOsat_pct", "Hg_ngL", "HgMethyl_ngL", "K_mgL", "Mg_mgL", "N_ugL", "Na_mgL",
              "NH4_ugL", "NO2+NO3_ugL", "P_ugL", "pH", "SecchiCond", "Secchi_m", "Si_mgL", "SO4_mgL",
              "SpecCond_uScm", "TempAir_C", "TempWater_C", "Transp_cm", "TSS_mgL", "Turbidity_NTU", "WaterLevel_m",
              "WaterLevelRef_m", "WaveHt_cm", "WaveHt_m", "WindCond", "WindDir_Deg")

  parameter <- match.arg(parameter, several.ok = T, c("all", params))
  if(any(parameter == "all")){parameter = params} else {parameter = parameter}

  stopifnot(is.logical(include_censored))
  output <- match.arg(output, c("short", "verbose"))

  options(scipen = 10) # prevent scientific notation

  #---- compile data ----
  env <- if(exists("GLKN_WQ")){GLKN_WQ} else {.GlobalEnv}

  tryCatch({res <- get("Results", envir = env)},
           error = function(e){stop("GLKN water views not found. Please import data.")}
  )

  loc <- getLocations(park = park, site = site, site_type = site_type, active = active, output = "short")
  loc_evs <- loc$Location_ID

  res1 <- dplyr::filter(res, Location_ID %in% loc_evs)

  res1$Park_Code <- substr(res1$Location_ID, 1, 4)
  res1$sample_date <- format(as.Date(res1$Activity_Start_Date, format = "%Y-%m-%d"), "%Y-%m-%d")
  res1$year <- as.numeric(substr(res1$sample_date, 1, 4)) # faster than using format with date
  res1$month <- as.numeric(substr(res1$sample_date, 6, 7))
  res1$doy <- as.numeric(format(as.Date(res1$sample_date, format = "%Y-%m-%d"), "%j"))
  res1$samp_type <- NA_character_
  res1$samp_type[!grepl("QC|Quality Control", res1$Activity_Type)] <- "VS"
  res1$samp_type[grepl("QC|Quality Control", res1$Activity_Type)] <- "QC"

  # Handle censored values
  res1$value <- suppressWarnings(as.numeric(res1$Result_Text))

  res1$value_cen <- NA_real_

  res1$value_cen[res1$Result_Detection_Condition %in% "Detected and Quantified"] <-
    res1$value[res1$Result_Detection_Condition %in% "Detected and Quantified"]

  res1$value_cen[res1$Result_Detection_Condition %in% "Present Above Quantification Limit"] <-
    res1$Upper_Quantification_Limit[res1$Result_Detection_Condition %in% "Present Above Quantification Limit"]

  res1$value_cen[res1$Result_Detection_Condition %in% "Present Below Quantification Limit"] <-
    res1$Lower_Quantification_Limit[res1$Result_Detection_Condition %in% "Present Below Quantification Limit"]

  res1$censored <- ifelse(res1$Result_Detection_Condition %in% "Detected and Quantified", FALSE, TRUE)

  res1$value <- if(include_censored == TRUE){res1$value_cen} else {res1$value}

  res2 <- if(any(parameter %in% "all")){res1
  } else {filter(res1, param_name %in% parameter)}
  res3 <- filter(res2, year %in% years)
  res4 <- filter(res3, month %in% months)
  res5 <- if(any(sample_type %in% "all")){res4
  } else {filter(res4, samp_type %in% sample_type)}

  res6 <- if(any(sample_depth %in% "all")){res5
  } else {filter(res5, is.na(Activity_Relative_Depth) | Activity_Relative_Depth %in% "Surface")}

  res7 <- if(any(include_censored == TRUE)){res6
  } else {filter(res6, censored == FALSE)}

  res8 <- full_join(loc, res7, by = c("Org_Code", "Location_ID", "Park_Code"))

  res_final <- if(any(output == "verbose")){res8
  } else {res8[,c("Org_Code","Park_Code", "Project_ID", "Location_ID", "Location_Name", "SiteType",
                  "sample_date", "year", "month", "doy",
                  "Activity_Relative_Depth", "Activity_Depth", "Activity_Depth_Unit", "Activity_Type", "samp_type",
                  "Characteristic_Name", "param_name", "Result_Detection_Condition",
                  "Result_Text", "value", "censored", "Result_Unit",
                  "Method_Detection_Limit", "Lower_Quantification_Limit", "Upper_Quantification_Limit", "Result_Comment")]}

  if(nrow(res_final) == 0){stop("Specified arguments returned an empty data frame.")}

  return(data.frame(res_final))

  } # end of fxn



