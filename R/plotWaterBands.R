#' @include getResults.R
#' @include theme_WQ.R
#'
#' @importFrom dplyr filter group_by left_join mutate select summarize ungroup
#' @importFrom tidyr pivot_longer pivot_wider
#' @import ggplot2
#'
#' @title plotWaterBands: compare current vs. historic values
#'
#' @description This function produces a plot that summarizes the range of historic data compared with current measurements.
#' Historic measurements are displayed as min-max values ever previously recorded (outermost band), upper and lower 95%
#' distribution and middle 50% distribution (inner quartiles) of values previously recorded (inner bands). The line
#' represents the median value. Sample periods (ie month range on the x-axis) are fixed for each park.
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"APIS"}{Apostle Islands National Lakeshore}
#' \item{"INDU"}{Indiana Dunes National Park}
#' \item{"ISRO"}{Isle Royale National Park}
#' \item{"MISS"}{Mississippi National River and Recreation Area}
#' \item{"PIRO"}{Pictured Rocks National Lakeshore}
#' \item{"SLBE"}{Sleeping Bear National Lakeshore}
#' \item{"SACN"}{St. Croix National Scenic Riverway}
#' \item{"VOYA"}{Voyageurs National Park}
#'}
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
#'           'VOYA_01',
#'           'VOYA_02', 'VOYA_03', 'VOYA_04', 'VOYA_05', 'VOYA_06', 'VOYA_07', 'VOYA_08', 'VOYA_09', 'VOYA_10',
#'           'VOYA_11', 'VOYA_12', 'VOYA_13', 'VOYA_14', 'VOYA_15', 'VOYA_16', 'VOYA_17', 'VOYA_18', 'VOYA_19',
#'           'VOYA_20', 'VOYA_21', 'VOYA_22', 'VOYA_23', 'VOYA_24', 'VOYA_25')
#'
#' Impoundments: c('SACN_CLAM_0.7', 'SACN_STCR_53.9', 'SACN_WILO_0.5')
#'
#' @param site_type Combine all site types, lakes or streams. Not needed if specifying particular sites.
#' \describe{
#' \item{"all"}{Default. Includes all site types, unless site or site_name select specific site types.}
#' \item{"lake"}{Include only lakes.}
#' \item{"stream"}{Include streams only.}
#' }
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
#' @param sample_type Select the sample type. Options available are below. Can only choose one option.
#' \describe{
#' \item{"all"}{All possible sampling events.}
#' \item{"VS"}{Default. GLKN Vital Signs monitoring events, which matches all non-QAQC Activity_Types.}
#' \item{"QC"}{Includes only include Quality Control sampling events listed in Activity_Type.}
#' }
#'
#' @param year_current Year that will be plotted separately. Must be numeric and 4 digits.
#' @param years_historic Years to include in historic range calculations. Default starts at 2007.
#'
#' @param active Logical. If TRUE (Default) only queries actively monitored sites. If FALSE, returns all sites that have been monitored.
#'
#' @param param_name Specify the param_name to return. Can only work with 1 param_name at a time. Current accepted values are:.
#' chemistry: c("ANC_ueqL", "AppColor", "AppColor_PCU", "ChlA_ugL", "Cl_ueqL",
#' "DOC_mgL", "NH3_mgL", "NO2_mgL", "NO2+NO3_mgL", "NO3_ueqL", "pH_Lab", "PO4_ugL", "SO4_ueqL",
#' "TN_mgL", "TotDissN_mgL", "TotDissP_ugL", "TP_ugL")
#' sonde: c("Temp_C", "Temp_F", "SpCond_uScm", "DOsat_pct", "DOsatLoc_pct", "DO_mgL", "pH", "pHmV",
#' "Turbidity_FNU", "ChlA_RFU", "ChlA_ugL", "BP_mmHg").
#' other: c("SDepth_m", "Discharge_cfs", "PenetrationRatio", "WaterLevel_Feet", "WaterLevel_m").
#' Note that "all" is not an accepted value, because there are too many to plot.
#'
#' @param include_censored Logical. If TRUE, the value column includes non-censored and censored values
#' using the Lower/Upper Quantification Limits values. Censored values are indicated by censored = TRUE, and
#' are defined as records where the Result_Detection_Condition = Present Above/Below Quantification Limit.
#' If FALSE (Default), only values with Result_Detection_Condition = "Detected and Quantified"
#' are returned in the value column. Censored values will be plotted as a star compared with filled circles
#' for non-censored values.
#'
#' @param sample_depth Filter on sample depth. If "all", returns all sample depths. If "surface" (Default),
#' only returns records that are either blank or "Surface" in the Activity_Relative_Depth column of the Results view.
#' In general, sample_depth = "surface" is the most appropriate choice.
#'
#' @param threshold Logical. If TRUE (Default), will plot a dashed (upper) or dotted (lower) line if a water quality
#' threshold exists for that param_name and site. If FALSE, no threshold line will be plotted. *NOT CURRENTLY ENABLED*
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @param facet_scales Specify whether facet axes should be "fixed" (all the same) or "free_y", "free_x" or "free" (both).
#'
#' @return Returns a ggplot object of specified current vs historic values
#'
#' @examples
#'
#' # RUN IMPORT FIRST: import both lakes and rivers data as zip files
#' library(waterGLKN)
#' river_zip = ("../data/GLKN_water/records-2309369.zip")
#' lake_zip = ("../data/GLKN_water/records-2306516.zip")
#' importData(type = 'zip', filepath = c(river_zip, lake_zip))
#'
#' # Plot pH in SACN_STCR_20.0 for 2023
#' plotWaterBands(site = "SACN_STCR_20.0", year_curr = 2023, years_historic = 2007:2022,
#'   parameter = "pH", legend_position = 'right')
#'
#' # Plot ChlA in Lake St. Croix sites for 2023, including censored
#' lkst <- c("SACN_STCR_20.0", "SACN_STCR_15.8", "SACN_STCR_2.0")
#' plotWaterBands(site = lkst, year_curr = 2023, years_historic = 2007:2022,
#'   parameter = "ChlA_ugL", legend_position = 'right', include_censored = T)
#'
#' # Plot DO in SLBE  2023
#' plotWaterBands(park = "SLBE", year_curr = 2023, years_historic = 2007:2022,
#'   parameter = "DOsat_pct", legend_position = 'right')
#'
#' # Plot ChlA in Lake St. Croix sites with legend on the bottom
#' lksc <- c('SACN_STCR_2.0', 'SACN_STCR_15.8', 'SACN_STCR_20.0')
#' plotWaterBands(site = lksc, year_curr = 2023, years_historic = 2007:2022,
#'                parameter = "ChlA_ugL", legend_position = 'bottom', gridlines = 'grid_y',
#'                include_censored = T)
#'
#'
#' @export

plotWaterBands <- function(park = "all",
                           site = "all",
                           site_type = "all",
                           year_current = format(Sys.Date(), "%Y"),
                           years_historic = seq(2007, as.numeric(format(Sys.Date(), "%Y")) - 1, 1),
                           active = TRUE,
                           parameter = NA,
                           include_censored = FALSE,
                           sample_depth = "surface",
                           #threshold = TRUE,
                           legend_position = 'none',
                           gridlines = "none",
                           facet_scales = 'fixed'){
  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APIS", "INDU", "ISRO", "PIRO", "SACN", "SLBE", "VOYA"))

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

  site <- match.arg(site, several.ok = TRUE, c("all", Rivers, Lakes, Impoundments))
  if(any(site == "all")){site = c(Rivers, Lakes, Impoundments)} else {site}
  site_type <- match.arg(site_type, several.ok = TRUE, c("all", "impound", "lake", "river"))
  stopifnot(class(years_historic) %in% c("numeric", "integer"), years_historic >= 2007)
  stopifnot(class(year_current) %in% c("numeric", "integer"), year_current >= 2008)
  stopifnot(class(active) == "logical")
  stopifnot(class(include_censored) == "logical")
  sample_depth <- match.arg(sample_depth, c("all", "surface"))
  # stopifnot(class(threshold) == "logical")
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))
  facet_scales <- match.arg(facet_scales, c("fixed", "free", "free_y", "free_x"))

  years <- c(year_current, years_historic)

  params <- c("Alkalinity_mgL", "Ca_mgL", "ChlA_mgm3", "ChlA_Pheo_pct", "ChlA_ugL", "Cl_mgL", "Depth_m",
              "DO_mgL", "DOC_mgL", "DOsat_pct", "Hg_ngL", "HgMethyl_ngL", "K_mgL", "Mg_mgL", "N_ugL",
              "Na_mgL", "NH4_ugL", "NO2+NO3_ugL", "P_ugL", "pH", "Secchi_m", "Si_mgL", "SO4_mgL", "SpecCond_uScm",
              "TempAir_C", "TempWater_C", "Transp_cm", "TSS_mgL", "Turbidity_NTU", "WaterLevel_m",
              "WaveHt_cm", "WaveHt_m", "WindDir_Deg")

  typo <- parameter[(!parameter %in% params)]

  if(any(!parameter %in% params)){
    stop(paste0("The following param_names are not accepted values: ", paste0(typo, collapse = ","), "\n"),
         "Accepted values: ", paste0(params, collapse = ", "))}

  #-- Compile data for plotting --
  wdat1 <- getResults(park = park, site = site, site_type = site_type, sample_type = "VS", years = years,
                     active = active, parameter = parameter, sample_depth = sample_depth,
                     include_censored = include_censored)

  wdat1 <- wdat1[!is.na(wdat1$value),]

  wdat1$param_label <- ifelse(grepl("_", wdat1$param_name),
                              paste0(gsub("_", " (", wdat1$param_name), ")"),
                              paste0(wdat1$param_name))

  wdat1$sample_date <- as.Date(wdat1$sample_date, format = "%Y-%m-%d")

  wdat <- wdat1 |> filter((Park_Code %in% c("APIS", "PIRO", "SLBE", "VOYA") & month %in% 6:9) |
                          (Park_Code == "INDU" & month %in% 5:8) |
                          (Park_Code == "ISRO" & month %in% 6:8) |
                          (Park_Code == "SACN" & month %in% 4:11))

  months <- unique(wdat$month)

  # join wdat with WQ thresholds, stored as a dataset in the package
  # data("GLKN_WQ_thresh")
  xaxis_breaks <- month.abb[months]
  wdat$mon <- factor(format(wdat$sample_date, "%b"), month.abb, ordered = TRUE)
  wdat$mon <- wdat$mon[,drop = T]

  if(nrow(wdat) == 0){stop("Combination of sites and param_names returned a data frame with no records.")}

  # If threshold is >10% higher than max or 10% lower than min in full dataset, don't plot
  wdat_thresh <- wdat |> group_by(Location_ID, Park_Code, param_name) |>
    summarize(max_value = max(value, na.rm = T),
              min_value = min(value, na.rm = T),
              .groups = 'drop') |> ungroup() |> droplevels()

  # Wait until thresholds are defined for GLKN water data
  # wdat2 <- left_join(wdat, wdat_thresh, by = c("Location_ID", "Park_Code", "param_name")) |>
  #   mutate(UpperThreshold_corr =
  #                  ifelse(!is.na(UpperThreshold) & UpperThreshold > 1.1*max_value, NA_real_, UpperThreshold),
  #          LowerThreshold_corr =
  #                  ifelse(!is.na(LowerThreshold) & LowerThreshold < min_value - 0.1*min_value, NA_real_, LowerThreshold))

  wdat_hist <- wdat[wdat$year %in% years_historic, ] |> droplevels()
  wdat_curr <- wdat[wdat$year == year_current, ] |> droplevels()

  wdat_hist$site_fac <-
    if(!any(site == "all")){factor(wdat_hist$Location_ID, levels = site)
    } else {wdat_hist$Location_ID}

  wdat_curr$site_fac <-
    if(!any(site == "all")){factor(wdat_curr$Location_ID, levels = site)
    } else {wdat_curr$Location_ID}

  if(nrow(wdat_curr) == 0){
    stop(paste0("There are no data available to plot for ", year_current, " in: ",
                paste0(site, collapse = ", "), ", and for param_names: ",
                paste0(param_name, collapse = ", "), "."))}

  if(nrow(wdat_hist) == 0){
    stop(paste0("There are no historic data available to plot for years: ",
                years_historic[1], ":", years_historic[2], " in: ",
                paste0(site, collapse = ', '), ", and for param_names: ",
                paste0(param_name, collapse = ', '), "."))}

 # Calc min/max 95% stats
 wdat_sum <- wdat_hist |>
   group_by(Location_ID, Park_Code, month, mon, param_name, param_label, Result_Unit, site_fac) |>
   summarize(num_samps = sum(!is.na(value)),
             median_val = median(value, na.rm = TRUE),
             min_val = min(value, na.rm = TRUE),
             max_val = max(value, na.rm = TRUE),
             lower_100 = ifelse(num_samps >= 3, min(value, na.rm = T), NA),
             upper_100 = ifelse(num_samps >= 3, max(value, na.rm = T), NA),
             lower_95 = ifelse(num_samps >= 3, quantile(value, 0.025, na.rm = T), NA),
             upper_95 = ifelse(num_samps >= 3, quantile(value, 0.975, na.rm = T), NA),
             lower_50 = ifelse(num_samps >= 3, quantile(value, 0.25, na.rm = T), NA),
             upper_50 = ifelse(num_samps >= 3, quantile(value, 0.75, na.rm = T), NA),
             .groups = "drop") |>
  filter(!is.na(lower_50))

  wdat_curr <- wdat_curr |>
    mutate(metric_type = # enable once have WQ thresholds
      # ifelse(!is.na(UpperThreshold_corr) & value > UpperThreshold_corr, "poor value",
      # ifelse(!is.na(LowerThreshold_corr) & value < LowerThreshold_corr, "poor value",
        ifelse(censored == TRUE, "censored", "value"))

  wdat_med <- wdat_sum |> select(Location_ID:Result_Unit, median_val, site_fac) |>
    mutate(metric_type = "median")

  wdat_hist2 <- wdat_sum |>
    select(Location_ID:Result_Unit, site_fac, lower_100, upper_100, lower_95, upper_95, lower_50, upper_50) |>
    pivot_longer(cols = c(lower_100, upper_100, lower_95, upper_95, lower_50, upper_50),
                 names_to = "metric", values_to = "value") |>
    mutate(metric_type = ifelse(grepl("lower", metric), "lower", "upper"),
          distrib = paste0("d", gsub("\\D", "", metric))) |>
    select(-metric) |>
    pivot_wider(values_from = value, names_from = metric_type) |>
    mutate(metric_type = distrib)

  wdat_hist2$metric_type <- factor(wdat_hist2$metric_type, levels = c("d100", "d95", "d50"))

  # xaxis_labels <- lapply(xaxis_breaks, function(x){as.character(lubridate::month(x, label = T))})

  # Set up legend
  fill_values <-
    if(include_censored == TRUE){ c("d100" = "#E4F0F8", "d95" = "#B8D8ED", "d50" = "#7FB9DD")
    } else {c("d100" = "#E4F0F8", "d95" = "#B8D8ED", "d50" = "#7FB9DD")}

  fill_labels <- c("d100" = "Historic range", "d95" = "Hist. 95% range", "d50" = "Hist. 50% range")

  fill_breaks <-  c("d100", "d95", "d50")

  color_values <-
    if(include_censored == TRUE){c("median" = "#1378b5", "value" = "black", 'censored' = 'black')
    } else {c("median" = "#1378b5", "value" = "black")
    }

  color_labels <-
    if(include_censored == TRUE){c("median" = "Hist. median",
                                   "value" = paste0("Current (", year_current, ") WQ value"),
                                   "censored" = "Current censored value")
    } else {c("median" = "Hist. median", "value" = paste0("Current (", year_current, ") WQ value")) }

  color_breaks <- if(include_censored == TRUE){c("median", "value", "censored")
  } else {c("median", "value")}

  shape_values <- if(include_censored == TRUE){c("median" = NA, "value" = 19, "censored" = 8)
  } else {c("median" = NA, "value" = 19)}

  size_values <- if(include_censored == TRUE){c("median" = 1, "value" = 2, "censored" = 3)
    } else {c("median" = 1, "value" = 2)}

  ylab <- unique(wdat_curr$param_label)

  facetsite <- ifelse(length(unique(wdat_curr$site_fac)) > 1, TRUE, FALSE)

  monthly_plot <- suppressWarnings(
    ggplot() + theme_WQ() +
    geom_ribbon(data = wdat_hist2,
                aes(ymin = lower, ymax = upper, x = mon,
                    fill = metric_type,
                    #color = metric_type,
                    group = metric_type#,
                    # tooltip = paste0("Site: ", Location_ID, "<br>", # future use in ggiraph
                    #               "Month: ", mon, "<br>",
                    #               "param_name: ", param_label, "<br>",
                    #               "Distribution: ", as.numeric(gsub("\\D", "", distrib)), "%", "<br>",
                    #               "Historic Upper value: ", round(upper, 1), "<br>",
                    #               "Historic Lower value: ", round(lower, 1), "<br>")
                )) +
    geom_line(data = wdat_med,
              aes(y = median_val, x = mon, group = metric_type, color = metric_type, linewidth = metric_type#,
                  #tooltip = paste0("Site: ", Location_ID, "<br>",
                  #              "Month: ", mon, "<br>",
                  #              "param_name: ", param_label, "<br>",
                  #              "Historic Median: ", round(median_val, 1), "<br>")
                  ), lwd = 0.7) +
    geom_point(data = wdat_curr,
               aes(y = value, x = mon, #color = metric_type,
                   group = metric_type,
                   shape = metric_type, size = metric_type#,
                   #tooltip = paste0("Site: ", Location_ID, "<br>",
                   #                 "Month: ", mon, "<br>",
                   #                 "param_name: ", param_label, "<br>",
                   #                 "Current value: ", round(value, 1), "<br>")
                   )) +
    scale_color_manual(values = color_values,
                       breaks = color_breaks,
                       labels = color_labels,
                       name = NULL) +
    scale_fill_manual(values = fill_values,
                      breaks = fill_breaks,
                      labels = fill_labels,
                      name = NULL) +
    scale_shape_manual(values = shape_values,
                       breaks = color_breaks,
                       labels = color_labels,
                       name = NULL) +
    scale_size_manual(values = size_values,
                      breaks = color_breaks,
                      labels = color_labels,
                      name = NULL) +
    # Facets
    {if(facetsite == TRUE){facet_wrap(~site_fac, scales = facet_scales)}} +
    # Labels/Themes/axes
    scale_x_discrete(breaks = xaxis_breaks, drop = F, expand = c(0.04,0.04)) +
    scale_y_continuous(n.breaks = 8) +
    labs(y = ylab, x = NULL, title = NULL,
         alt = paste0("Distribution of current verses historic values shown as increasingly darkened blue bands that cover",
                      " historic min and max, 95% range, and middle 50% distributions respectively, and current values for each month as points.")) +
    theme(axis.title.y = element_text(size = 10),
          panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4),
          axis.line.x = element_line(color = "#696969", linewidth = 0.4),
          axis.line.y = element_line(color = "#696969", linewidth = 0.4),
          axis.ticks = element_line(color = "#696969", linewidth = 0.4),
          legend.key = element_blank(),
          legend.spacing.y = unit(-0.2, "cm"),
          legend.box = 'vertical',
          legend.position = legend_position) +
    {if(any(gridlines %in% c("grid_y", "both"))){
         theme(panel.grid.major.y = element_line(color = 'grey'),
               panel.grid.minor.y = element_line(color = 'grey'))}} +
    {if(any(gridlines %in% c("grid_x", "both"))){
        theme(panel.grid.major.x = element_line(color = 'grey'),
              panel.grid.minor.x = element_line(color = 'grey'))}} +
    guides(fill = guide_legend(order = 1),
           color = guide_legend(order = 2)
    )
  )

    return(suppressWarnings(monthly_plot))
    }

