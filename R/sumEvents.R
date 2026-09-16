#' @include getResults.R
#'
#' @title sumEvents: summarize sample events
#'
#' @description Summarize the number of samples collected at each site by year, month, parameter, and
#' whether measurement was real or censored. Only includes records for non-QC samples.
#'
#' @importFrom dplyr arrange filter first group_by last mutate select summarize
#' @importFrom tidyr pivot_wider
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
#' @param sample_depth Quoted string. Filters on sample depth. If "all" (default), returns all sample depths.
#' If "surface", only returns data from the uppermost sample.
#'
#' @param years Numeric. Years to query. Accepted values start at 2007.
#'
#' @param active Logical. If TRUE (Default), only returns actively monitored locations. If FALSE, returns all
#' locations that have been monitored at least once since 2007. Active sites are defined as sites that have at
#' least one sampling event between 2014 and 2024. See "./scripts/active_sites.R" for more details.
#'
#' @param parameter Specify the parameter(s) to return. Note if additional parameters are added to the Results view,
#' they need to be added as accepted values in this function. Current accepted values are:
#'    c("all" (default), "Alkalinity_mgL", "Ca_mgL", "ChlA_mgm3", "ChlA_Pheo_pct", "ChlA_ppb", "ChlA_ugL", "Cl_mgL", "Depth_m",
#'      "DO_mgL", "DOC_mgL", "DOsat_pct", "Hg_ngL", "HgMethyl_ngL", "K_mgL", "Mg_mgL", "N_ugL", "NA_mgL",
#'      "NH4_ugL", "NO2+NO3_ugL", "P_ugL", "pH", "SecchiCond", "Secchi_m", "Si_mgL", "SO4_mgL",
#'      "SpecCond_uScm", "TempAir_C", "TempWater_C", "Transp_cm", "TSS_mgL", "Turbidity_NTU", "WaterLevel_m",
#'      "WaterLevelRef_m", "WaveHt_cm", "WaveHt_m",
#'      "WindCond", "WindDir_Deg")
#'
#' @return Data frame of summarized event info
#'
#'@examples
#' \dontrun{
#' # RUN IMPORT FIRST: import both lakes and rivers data as zip files
#' library(waterGLKN)
#' river_zip = ("../data/GLKN_water/records-2309369.zip")
#' lake_zip = ("../data/GLKN_water/records-2306516.zip")
#' importData(type = 'zip', filepath = c(river_zip, lake_zip))
#'
#' # get all events for PIRO for all years and active sites
#' piro_ev <- sumEvents(park = "PIRO")
#'
#' # get all events from 2023
#' evs23 <- sumEvents(years = 2023)
#'
#' # get all events for VOYA in July 2023
#' voya23 <- sumEvents(park = "VOYA", years = 2023, months = 7)
#'
#'
#' }
#' @export

sumEvents <- function(park = "all",
                      site = "all",
                      site_type = "all",
                      sample_depth = 'all',
                      parameter = "all",
                      years = 2007:format(Sys.Date(), "%Y"),
                      active = TRUE){

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APIS", "GRPO", "INDU", "ISRO", "MISS", "PIRO", "SLBE", "SACN", "VOYA"))
  if(any(park == "all")){park = c("APIS", "INDU", "ISRO", "MISS", "PIRO", "SLBE", "SACN", "VOYA")} else {park}

  site_type <- match.arg(site_type, several.ok = TRUE, c("all", "impound", "lake", "river"))
  if(any(site_type == "all")){site_type = c("lake", "river", "impound")}

  sample_depth <- match.arg(sample_depth, c("surface", "all"))

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

  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2007)
  stopifnot(class(active) == "logical")

  wdat <-
    getResults(park = park, site = site, site_type = site_type, sample_type = "VS", years = years,
               active = active, parameter = parameter, sample_depth = sample_depth,
               include_censored = TRUE)

  evs <- wdat |>
    select(Park_Code, Location_ID, year) |> unique() |>
    arrange(Location_ID, year) |>
    group_by(Park_Code, Location_ID) |>
    summarize(year_start = first(year),
              year_latest = last(year),
              num_years = sum(!is.na(year)),
              .groups = 'drop')

  wdat$value_type <- ifelse(wdat$censored == TRUE, 'cens', "real")

  wdat2 <- left_join(evs, wdat, by = c("Park_Code", "Location_ID"))
  wdat2$mon <- factor(format(as.Date(wdat2$sample_date, format = c("%Y-%m-%d")), "%b"),
                      levels = month.abb, ordered = T)
  wdat2$mon <- wdat2$mon[,drop = T]

  samp_tab <- wdat2 |> group_by(Park_Code, Location_ID, SiteType, mon, value_type, param_name,
                                year_start, year_latest, num_years) |>
    summarize(num_samples = sum(!is.na(year)), .groups = "drop") |>
    pivot_wider(names_from = c(mon, value_type), values_from = num_samples, values_fill = 0)

  # Add month columns that could be missing (mostly cens)
  all_cols <- c("Park_Code", "Location_ID", "value_type" ,"param_name",
                "year_start", "year_latest", "num_years",
                "Apr_real", "Apr_cens", "May_real", "May_cens", "Jun_real", "Jun_cens",
                "Jul_real", "Jul_cens", "Aug_real", "Aug_cens", "Sep_real", "Sep_cens",
                "Oct_real", "Oct_cens")

  missing <- setdiff(all_cols, names(samp_tab))

  samp_tab[missing] <- 0

  samp_tab2 <- samp_tab[samp_tab$num_years > 0,]

  samp_tab_final <- samp_tab2 |>
    mutate(year_range = paste0(year_start, " \U2013 ", year_latest)) |>
    select(Park_Code, SiteType, Location_ID,  param_name, year_range,
           num_years, Apr = Apr_real, May = May_real, Jun = Jun_real,
           Jul = Jul_real, Aug = Aug_real, Sep = Sep_real, Oct = Oct_real,
           Apr_cens, May_cens, Jun_cens, Jul_cens, Aug_cens, Sep_cens,
           Oct_cens) |>
  arrange(Park_Code, SiteType, Location_ID, param_name)

return(samp_tab_final)
}
