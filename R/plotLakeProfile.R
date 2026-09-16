#' @include getResults.R
#' @include theme_WQ.R
#'
#' @title plotLakeProfile: Plot lake profiles
#'
#' @importFrom dplyr arrange group_by lead left_join mutate select slice summarize
#' @importFrom purrr pmap_dfr possibly
#' @import ggplot2
#'
#' @description This function produces a heatmap filtered on park, site, year, month, parameter. The y-axis is 1m bins, with a
#' reversed Y axis, such that the surface = 0 and is at the top, and the lowest sample depth is at the bottom of the y-axis.
#' If multiple samples are taken within the same 1m bin, the median value is used. The width of the profile columns reflects
#' the number of days between sample events, centered on the day sampled. Shorter intervals between events result in narrower bars.
#' You can only specify one parameter at a time. If multiple sites or years are selected, plots will be faceted on those factors.
#' Keep options limited for best plotting results. If you specify a lake x year x parameter combination that doesn't exist
#' (e.g., a year a lake isn't sampled), the function will return an error message instead of an empty plot. Only non-censored
#' and non-QAQC values are plotted. Sample periods (ie month range on the x-axis) are fixed for each park.
#'
#' @param park Select a park to plot. Can only plot one park at a time because of differences in length of sampling period. Valid inputs:
#' \describe{
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
#' @param site Filter on Location_ID. Easiest way to pick a site. Defaults to "all" lake or riverine impounded sites.
#' Accepted sites are below. If new sites are added, need to be added to this function as an accepted site. When
#' multiple sites are specified, they will be faceted in the order specified.
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
#' @param site_type Quoted string to select either inland lake or riverine impounded site (river sites not included).  Note that impounded sites are not actively
#' monitored sites, so active = F would need to be specified to return data for those sites. Options are:
#' \describe{
#'  \item{"all"}{Includes all location types}
#'  \item{"lake"}{Location_types that = "Lake"}
#'  \item{"impound"}{Location_types that = "Riverine Impoundment"}
#'  }
#'
#' @param years Numeric. Years to query. Accepted values start at 2007.
#'
#' @param active Logical. If TRUE (Default), only returns actively monitored locations. If FALSE, returns all
#' locations that have been monitored at least once since 2007. Active sites are defined as sites that have at
#' least one sampling event between 2014 and 2024. See "./scripts/active_sites.R" for more details.
#'
#' @param parameter Specify the parameter to plot. Can only plot one parameter at a time. Can use grid or cowplot
#' packages to create multi-paneled figures. Current accepted values with profile data are:
#'     c("DO_mgL", "DOsat_pct", "pH", "SpecCond_uScm", "TempWater_C", "N_ugL", "P_ugL", "Hg_ngL", "HgMethyl_ngL").
#'
#' @param palette Diverging color palette for plots. Options currently are 'viridis'
#' (yellow - green - blue), 'mako' (light blue grading to black), 'plasma' (yellow to purple), 'rocket' (red grading to black),
#' 'turbo' (rainbow), 'or any built-in continuous color palette available in RColorBrewer. Run RColorBrewer::display.brewer.all()
#' to see the diverging color palettes. Common palettes include "Blues", "GnBu", "RdPu", "Spectral" (Default), "RdYlBu",
#' "RdBu", "PiYg". See https://ggplot2-book.org/scales-colour for more info.
#'
#' @param color_rev Reverse the order of the color pallete. For example change RdYlBu from red - yellow - blue
#' to blue - yellow -red.
#'
#' @param plot_title Logical. If TRUE (default) prints site name at top of figure. If FALSE,
#' does not print site name. Only enabled when one site is selected.
#'
#' @param plot_thermocline Logical. If TRUE (default) plots the depth of the thermocline, calculated by
#' rLakeAnalyzer as the depth/elevation within the water column where the temperature gradient is the steepest
#' and indicates where the upper waters are typically not mixing with deeper waters. Only plots where at least
#' 5 depth measurements for temperature have been collected (may want to increase this threshold). Note that in
#' the rare cases that multiple sampling events occur within a month, both are plotted, but will have a thinner
#' column than other samples that are more spread out. If no thermocline is detected, as defined by
#' `rLakeAnalyzer::thermo.depth()`, no points are plotted.
#'
#' @param legend_position Specify location of legend (default is 'right'). To turn legend off, use legend_position = "none". Other
#' options are "top", "bottom", "left", "right".
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @param facet_scales Specify whether facet axes should be fixed (all the same; default) or "free_y", "free_x" or "free" (both).
#'
#' @examples
#' \dontrun{
#'
#' # RUN IMPORT FIRST: import both lakes and rivers data as zip files
#' river_zip = ("../data/GLKN_water/records-2309369.zip")
#' lake_zip = ("../data/GLKN_water/records-2306516.zip")
#' importData(type = 'zip', filepath = c(river_zip, lake_zip))
#'
#'# Plot pH for ISRO_07 all years with thermocline plotted as black lines.
#' plotLakeProfile(site = "ISRO_07", parameter = "pH")
#'
#'# Plot temperature for VOYA_01 all years with thermocline plotted as black lines.
#' plotLakeProfile(site = "VOYA_01", parameter = "TempWater_C")
#'
#' # Plot Specific Conductance for SLBE_01 all years without theromocline or plot title
#' plotLakeProfile(site = "SLBE_01", parameter = "SpecCond_uScm", plot_thermocline = F, plot_title = F)
#'
#' # Plot water temp using Red-Yellow-Blue palette
#' plotLakeProfile(site = "PIRO_01", parameter = "TempWater_C", palette = "RdYlBu") # PIRO has more months
#'
#' # Plot DO for all sites in PIRO sampled in 2023  with mako palette
#' plotLakeProfile(park = "PIRO", years = 2023, parameter = "DOsat_pct", palette = "mako")
#'
#' # Plot pH for Lake St. Croix Sites in 2023
#' lkst <- c("SACN_STCR_20.0", "SACN_STCR_15.8", "SACN_STCR_2.0")
#' plotLakeProfile(site = lkst, parameter = "pH", years = 2023)
#' }
#'
#' @return Returns a panel of lake profile plots during the growing season for each year
#' in the data frame.
#'
#' @export
#'
plotLakeProfile <- function(park = 'all',
                            site = "all", site_type = 'all',
                            years = 2007:format(Sys.Date(), "%Y"),
                            active = TRUE,
                            parameter = NA,
                            palette = "Spectral",
                            color_rev = FALSE,
                            plot_title = TRUE,
                            plot_thermocline = TRUE,
                            legend_position = 'right',
                            facet_scales = 'fixed',
                            gridlines = "none"){

  #-- Error handling --
  park <- match.arg(park, several.ok = FALSE,
                    c('all', "APIS", "INDU", "ISRO", "PIRO", "SACN", "SLBE", "VOYA"))

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

  site <- match.arg(site, several.ok = TRUE, c("all", Lakes, Impoundments))
  if(any(site == "all")){site = c(Lakes, Impoundments)} else {site}

  site_type <- match.arg(site_type, several.ok = TRUE, c("all", "impound", "lake"))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2007)
  stopifnot(class(active) == "logical")
  stopifnot(class(color_rev) == "logical")
  stopifnot(class(plot_title) == "logical")
  facet_scales <- match.arg(facet_scales, c("fixed", "free", "free_y", "free_x"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  if(length(parameter) > 1){stop("Can only use 1 parameter at a time.")}
  palette <- match.arg(palette, c("viridis", "magma", "mako", "plasma", "rocket", "turbo",
                                  'Blues', 'GnBu', 'RdPu', 'Spectral', 'RdYlBu', 'RdBu', 'PiYg'))

  params <- c("DO_mgL", "DOsat_pct", "pH", "SpecCond_uScm", "TempWater_C",
              "N_ugL", "P_ugL", "Hg_ngL", "HgMethyl_ngL")

  typo <- parameter[(!parameter %in% params)]

  if(any(!parameter %in% params)){
    stop(paste0("The following parameters are not accepted values: ", paste0(typo, collapse = ","), "\n"),
         "Accepted values: ", paste0(params, collapse = ", "))}

  if(!palette %in% "viridis"){
    if(!requireNamespace("RColorBrewer", quietly = TRUE)){
      stop("Package 'RColorBrewer' needed if palette is anything but 'viridis'. Please install it.",
           call. = FALSE)
    }}

  #-- Compile data for plotting --
  # combine sonde and water level data and group depths by 1m or 0.25m bins
  wdat <- getResults(park = park, site = site, site_type = site_type, active = active, sample_type = "VS",
                     include_censored = FALSE, years = years, parameter = parameter, sample_depth = 'all') |>
          select(Park_Code, Location_ID, sample_date, year, month, doy, Activity_Depth, param_name, value) |>
    filter(!is.na(Activity_Depth))

  if(length(unique(wdat$Park_Code)) > 1){stop("More than 1 park specified. Can only plot 1 park at a time.")}

  # Binning by 1m depths
  wdat$depth_bin <- ifelse(wdat$Activity_Depth < 1, 0, round(wdat$Activity_Depth, 0))

  wdat2 <- wdat |> group_by(Park_Code, Location_ID, sample_date, year, month, doy, depth_bin, param_name) |>
                     summarize(value = median(value), .groups = 'drop') |>
                     arrange(Location_ID, month, year, doy)

  if(nrow(wdat2) == 0){stop("Combination of sites, years and parameters returned a data frame with no records.")}

  #-- Set up plotting features --
  # create column of abbreviated months
  wdat2$mon <- factor(format(as.Date(wdat2$sample_date, format = c("%Y-%m-%d")), "%b"), levels = month.abb, ordered = T)
  #wdat2$mon <- factor(format(wdat2$EventDate, "%b"), month.abb, ordered = TRUE)
  wdat2$mon <- wdat2$mon[,drop = T]

  param_label <- unique(ifelse(grepl("_", wdat2$param_name),
                        paste0(gsub("_", " (", wdat2$param_name), ")"),
                        paste0(wdat2$param_name)))

  ylab <- "Sample Depth (m)"

  facet_site <- ifelse(length(unique(wdat2$Location_ID)) > 1, TRUE, FALSE)
  facet_year <- ifelse(length(unique(wdat2$year)) > 1, TRUE, FALSE)

  color_dir <- ifelse(color_rev == FALSE, -1, 1)
  ptitle <- if(length(unique(wdat2$Location_ID)) == 1 & plot_title == TRUE){unique(wdat2$Location_ID)} else {NULL}

  # Figure out width of sampling periods for tile widths, with bookends on the first and last sample day per park
  prof_width1 <- wdat2 |> select(Location_ID, year, doy) |> unique() |> arrange(Location_ID, year, doy)
  prof_width2 <- prof_width1 |> group_by(Location_ID, year) |>
    mutate(lag_doy = lag(doy, 1),
           lead_doy = lead(doy, 1))

  # First day of sampling period for each park
  prof_width2$park <- substr(prof_width2$Location_ID, 1, 4)

  prof_width2$first_day <- NA_integer_
  prof_width2$first_day[prof_width2$park %in% c("APIS", "ISRO", "PIRO", "SLBE", "VOYA")] <- 121 # May 1
  prof_width2$first_day[prof_width2$park %in% c("INDU")] <- 60 # Mar 1
  prof_width2$first_day[prof_width2$park %in% c("SACN")] <- 60 # Mar 1 # makes first band bigger

  prof_width2$last_day <- NA_integer_
  prof_width2$last_day[prof_width2$park %in% c("APIS", "PIRO", "SLBE", "VOYA")] <- 304 # Oct 31
  prof_width2$last_day[prof_width2$park %in% c("ISRO")] <- 258 # Sep 15
  prof_width2$last_day[prof_width2$park %in% c("INDU")] <- 319 # Nov 15
  prof_width2$last_day[prof_width2$park %in% c("SACN")] <- 351 # Dec 15

  # drop measurements outside standard sample period based on last_day
  prof_width <- prof_width2 |> filter(doy <= last_day) |> filter(doy >= first_day)

  # For the first and last lead/lag of each year, need to set a first date/last date
  prof_width$lag_doy[is.na(prof_width$lag_doy)] <- prof_width$first_day[is.na(prof_width$lag_doy)]
  prof_width$lead_doy[is.na(prof_width$lead_doy)] <- prof_width$last_day[is.na(prof_width$lead_doy)]

  # Calculate width of profile columns as half of distance in days between samples on left and right.
  prof_width$lag_dif <- (prof_width$doy - prof_width$lag_doy)/2
  prof_width$lead_dif <- (prof_width$lead_doy - prof_width$doy)/2

  prof_width$col_width <- prof_width$lag_dif + prof_width$lead_dif
  prof_width$doy_plot <- prof_width$doy - prof_width$lag_dif + ((prof_width$lag_dif + prof_width$lead_dif)/2)

  # join prof params with original data
  wdat3 <- left_join(wdat2, prof_width, by = c("Location_ID", "year", "doy"))

  #-- Calculate thermocline --
  if(plot_thermocline == TRUE){
    if(!requireNamespace("rLakeAnalyzer", quietly = TRUE)){
      stop("Package 'rLakeAnalyzer' needed if plot_thermocline = TRUE. Please install it.", call. = FALSE)
    }
  # thermocline calcuated for temperature only
  temp <- force(getResults(park = park, site = site, site_type = site_type, active = active, sample_type = "VS",
                           include_censored = FALSE, years = years, parameter = "TempWater_C",
                           sample_depth = 'all')) |>
    select(Park_Code, Location_ID, sample_date, year, month, doy, Activity_Depth, param_name, value)

  temp$depth_bin <- ifelse(temp$Activity_Depth < 1, 0, round(temp$Activity_Depth, 0))

  temp1 <- temp |>
    group_by(Location_ID, sample_date, year, month, doy,
             depth_bin, param_name) |>
    summarize(value = median(value), .groups = 'drop')

  # Check number of depths measured. If < 5 will be dropped from thermocline calculation
  num_depths <- temp1 |> group_by(Location_ID, year, month, doy) |>
    summarize(num_meas = sum(!is.na(depth_bin)), .groups = 'drop') |>  # this turns into logical, where every depth with a value = 1 and is summed
    filter(num_meas >= 5)

  # Drop temp data from samples with < 5 samples with left join
  temp2 <- left_join(num_depths, temp1, by = c("Location_ID", "year", "month", "doy"))

  # Check for multiple sampling events within a month, and take the first.
  num_evs <- temp2 |> select(Location_ID, year, month, doy) |> unique() |>
    group_by(Location_ID, year, month, doy) |> slice(1)

  # Take only first temp if two samples on the same day
  temp3 <- left_join(num_evs, temp2, by = c("Location_ID", "year", "month", "doy"))

  site_list <- sort(unique(temp3$Location_ID))
  year_list <- sort(unique(temp3$year))
  mon_list <- sort(unique(temp3$month))

  all_params <- unique(data.frame(site = temp3$Location_ID, yr = temp3$year, doy = temp3$doy))
  param_list <- list(all_params[[1]], all_params[[2]], all_params[[3]])

  # calc. thermocline on all site, year, month combinations in dataset
  tcline1 <-
    pmap_dfr(param_list, function(site, yr, day){
      df <- temp3 |> filter(Location_ID == site) |> filter(year == yr) |> filter(doy == day)
      dfmonth <- unique(temp2$month)
      tc <- rLakeAnalyzer::thermo.depth(wtr = df$value, depths = df$depth_bin)
      tcdf <- data.frame(Location_ID = site, year = yr, month = dfmonth, doy = day, value = tc)
      return(tcdf)
    })

  # add thermocline to temp/wl data
  tcline2 <-
       left_join(temp3 |> select(Location_ID, year, month, doy, depth_bin) |> unique(),
                  tcline1, by = c("Location_ID", "year", "month", "doy")) |>
          mutate(mon = factor(month, levels = unique(month),
                 labels = unique(month.abb[month])))

  tcline <- tcline2[!is.na(tcline2$value),]

  # add prof width data to tcline
  prof_unique <- prof_width |> select(Location_ID, year, doy, doy_plot, col_width) |> unique()

  tcline_final <- left_join(tcline, prof_unique, by = c("Location_ID", "year", "doy"))

  tcline_final$site_fac <-
    if(!any(site == "all")){factor(tcline_final$Location_ID, levels = site)
    } else {tcline_final$Location_ID}
  }

  vpals <- c("viridis", "magma", "mako", "plasma", "rocket", "turbo")

  park_doy_min  <- min(c(wdat3$doy_plot - wdat3$col_width, wdat3$first_day), na.rm = T)
  park_doy_max <- max(c(wdat3$doy_plot + wdat3$col_width, wdat3$last_day), na.rm = T)
  park_limits <- c(park_doy_min, park_doy_max)

  axis_labs1 <- data.frame(doy = c(91, 121, 152, 182, 213, 244, 274, 305),
                           label = c("Apr-1", "May-1", "Jun-1", "Jul-1", "Aug-1", "Sep-1", "Oct-1", "Nov-1"))

  axis_labs <- axis_labs1 |> filter(doy <= park_doy_max) |> filter(doy >= park_doy_min)

  wdat3$site_fac <-
  if(!any(site == "all")){factor(wdat3$Location_ID, levels = site)
    } else {wdat3$Location_ID}

  #-- Create plot --
  profplot <-
      ggplot(wdat3 |> droplevels(), aes(x = doy_plot, y = depth_bin)) +
        geom_tile(aes(width = col_width, height = 1, color = value, fill = value)) +
        theme(legend.position = legend_position, axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        theme_WQ() +
        {if(plot_thermocline == TRUE){
          geom_segment(data = tcline_final,
                       aes(x = doy_plot - (col_width/2), xend = doy_plot + (col_width/2),
                           y = value, yend = value), linewidth = 0.7) }} +
        # facets if more than 1 year or site
        {if(facet_site == TRUE & facet_year == TRUE) facet_wrap(~site_fac + year, drop = T, scales = facet_scales)} +
        {if(facet_site == TRUE & facet_year == FALSE) facet_wrap(~site_fac, drop = T, scales = facet_scales)} +
        {if(facet_site == FALSE & facet_year == TRUE) facet_wrap(~year, drop = T, scales = facet_scales)} +
        # color palettes
        {if(palette %in% vpals) scale_fill_viridis_c(direction = color_dir, option = palette,
                                                     aesthetics = c("fill", "color"))} +
        #{if(palette %in% vpals) scale_color_viridis_d(direction = color_dir, option = "magma")} +
        {if(!palette %in% vpals) scale_fill_distiller(palette = palette, direction = color_dir)} +
        {if(!palette %in% vpals) scale_color_distiller(palette = palette, direction = color_dir)} +
        # labels, themes
        labs(x = NULL, y = ylab, color = param_label, fill = param_label, title = ptitle,
             alt = paste0("A heat map of lake profile data for ", paste0(param_label, collapse = ", "), 
                          "by depth in ", paste0(unique(wdat3$SiteName), collapse = ", "))) +
        {if(any(gridlines %in% c("grid_y", "both"))){
          theme(
            panel.grid.major.y = element_line(color = 'grey'),
            panel.grid.minor.y = element_line(color = 'grey'))}} +
        {if(any(gridlines %in% c("grid_x", "both"))){
          theme(
            panel.grid.major.x = element_line(color = 'grey'),
            panel.grid.minor.x = element_line(color = 'grey'))}} +
        scale_y_reverse(breaks = pretty(wdat3$depth_bin, n = 8)) +
        scale_x_continuous(limits = park_limits,
                           breaks = axis_labs$doy,
                           labels = axis_labs$label)


 #return(#suppressWarnings(
   profplot
  # )#)
}


