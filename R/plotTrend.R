#' @include getResults.R
#' @include theme_WQ.R
#'
#' @title plotTrend: Plot WQ trends over time
#'
#' @importFrom dplyr mutate select
#' @import ggplot2
#'
#' @description This function produces a line or smoothed trend plot filtered on park, site, year, month, and parameter.
#' If multiple sites are specified, they will be plotted on the same figure. If multiple parameters are specified,
#' they will be plotted on separate figures. Note that if you specify a site and parameter combination that doesn't
#' exist (e.g., a stream site and a parameter only collected in lakes), the function will return an error message
#' instead of an empty plot.
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
#' @param years Numeric. Years to query. Accepted values start at 2007.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 4 and 11, and these are set as the defaults.
#'
#' @param active Logical. If TRUE (Default), only returns actively monitored locations. If FALSE, returns all
#' locations that have been monitored at least once since 2007. Active sites are defined as sites that have at
#' least one sampling event between 2014 and 2024. See "./scripts/active_sites.R" for more details.
#'
#' @param parameter Specify one or more parameters to plot. Current accepted values are:
#'     c("Alkalinity_mgL", "Ca_mgL", "ChlA_mgm3", "ChlA_Pheo_pct", "ChlA_ugL", "Cl_mgL", "Depth_m",
#'       "DO_mgL", "DOC_mgL", "DOsat_pct", "Hg_ngL", "HgMethyl_ngL", "K_mgL", "Mg_mgL", "N_ugL",
#'       "Na_mgL", "NH4_ugL", "NO2+NO3_ugL", "P_ugL", "pH", "Secchi_m", "Si_mgL", "SO4_mgL", "SpecCond_uScm",
#'       "TempAir_C", "TempWater_C", "Transp_cm", "TSS_mgL", "Turbidity_NTU", "WaterLevel_m",
#'       "WaveHt_cm", "WaveHt_m", "WindDir_Deg").
#'
#' @param sample_depth Filter on sample depth. If "all" (Default), returns all sample depths. If "surface",
#' only returns records that are either blank or "Surface" in the Activity_Relative_Depth column of the Results view.
#' If there are multiple surface samples for a given site, date, parameter, the median value will be used.
#'
#' @param include_censored Logical. If TRUE, the value column includes non-censored and censored values
#' using the Lower/Upper Quantification Limits values. Censored values are indicated by censored = TRUE, and
#' are defined as records where the Result_Detection_Condition = Present Above/Below Quantification Limit.
#' If FALSE (Default), only values with Result_Detection_Condition = "Detected and Quantified"
#' are returned in the value column. Censored values will be plotted as a star compared with filled circles
#' for non-censored values.
#'
#' @param layers Options are "points" and "lines". By default, both will plot.
#'
#' @param point_size If specified, will change the size of the points from ggplot default
#'
#' @param line_size If specified, will change the size of the line from ggplot default
#'
#' @param palette Theme to plot points and lines. Options include 'viridis' (Default- ranges of blue,
#' green and yellow), magma (yellow, red, purple), plasma (brighter version of magma), turbo (rainbow),
#' or specify a vector of colors manually. If fewer colors than parameters are specified, they will be
#' color-ramped to generate enough colors.
#'
#' @param threshold Logical. If TRUE (Default), will plot a dashed (upper) or dotted (lower) line if a water
#' quality threshold exists for that parameter and site. If FALSE, no threshold line will be plotted.
#' *NOT CURRENTLY ENABLED*
#'
#' @param smooth Logical. If TRUE (Default), will plot a loess smoothed line. If FALSE, will plot actual line. Only
#' plots if layers argument includes 'lines'.
#'
#' @param span Numeric. Determines how smoothed the line will be for smooth = TRUE. Default is 0.3. Higher spans (up to 1)
#' cause more smoothing, with 1 being a straight line. Span can range from 0 to 1.
#'
#' @param facet_site Logical. If TRUE (default), will facet on site if multiple sites specified. If FALSE, will plot all sites
#' on the same figure. Only enabled when multiple sites specified.
#'
#' @param facet_scales Specify whether facet axes should be fixed (all the same; default) or "free_y", "free_x" or "free" (both).
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param numcol Specify number of columns in the facet wrap, which is only enabled when either multiple sites are
#'  specified or multiple parks. Default is 2.
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @examples
#' \dontrun{
#'
#' # RUN IMPORT FIRST: import both lakes and rivers data as zip files
#' library(waterGLKN)
#' river_zip = ("../data/GLKN_water/records-2309369.zip")
#' lake_zip = ("../data/GLKN_water/records-2306516.zip")
#' importData(type = 'zip', filepath = c(river_zip, lake_zip))
#'
#' # Plot smoothed surface pH for ISRO_01 for all years
#' plotTrend(site = "ISRO_01", parameter = "pH")
#'
#' # Plot smoothed surface pH for ISRO_01 for all years, removing the legend and using span of 0.75.
#' plotTrend(site = "ISRO_01", parameter = "pH", span = 0.75)
#'
#' # Plot smoothed surface ChlA for VOYA_01 for all years including censored values, with palette and larger points/lines.
#' plotTrend(site = "VOYA_01", parameter = "ChlA_ugL", include_censored = TRUE,
#'   palette = 'navyblue', point_size = 3, line_size = 1.5)
#'
#' # Plot non-smoothed DO in SLBE for all years, including the legend,
#' # different color palette, all sites on the same figure.
#' plotTrend(park = "SLBE", parameter = "DO_mgL", smooth = F, layers = c('points', 'lines'),
#'   palette = c("blue", "green", "gold"), point_size = 3, legend_position = 'bottom', facet_site = F)
#'
#' # Plot smoothed surface SO4 for all VOYA sites all years, including censored, with 0.6 span
#' plotTrend(park = "VOYA", parameter = "SO4_mgL", include_censored = T, legend_position = "right",
#'   span = 0.6, point_size = 2.5)
#'
#' # Plot smoothed surface of multiple Sonde parameters for all PIRO sites over all
#' # years with 0.6 span.
#' params <- c("TempWater_C", "SpecCond_uScm", "DOsat_pct", "pH")
#' plotTrend(park = "PIRO",  facet_site = F,
#'           parameter = params, legend_position = "right", span = 0.6, palette = 'turbo')
#'
#' # Plot all %DO values from ISRO_04, including samples below the surface
#' plotTrend(site = "ISRO_04", sample_depth = "all", parameter = "DOsat_pct")
#'
#'}
#'
#' @return Returns a ggplot object of specified parameter trends.
#'
#' @export
#'
plotTrend <- function(park = "all",
                      site = "all",
                      site_type = "all",
                      sample_type = "VS",
                      years = 2007:format(Sys.Date(), "%Y"),
                      months = 4:11,
                      active = TRUE,
                      parameter = NA,
                      sample_depth = "surface",
                      include_censored = FALSE,
                      layers = c("points", "lines"),
                      point_size = 1,
                      line_size = 1,
                      palette = "viridis", #threshold = FALSE,
                      smooth = TRUE,
                      span = 0.3,
                      facet_site = TRUE,
                      facet_scales = "free_y",
                      legend_position = 'none',
                      numcol = 2,
                      gridlines = "none"){

  # park = 'all'; site = 'all'; site_type = 'all'; years = 2013:2023;
  # parameter = c("ANC", "pH_Lab", "pH", "Temp_C"); ... = NULL

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
  sample_type <- match.arg(sample_type, c("all", "VS", "QC"))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2007)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  sample_depth <- match.arg(sample_depth, c("all", "surface"))
  stopifnot(class(include_censored) == "logical")
  layers <- match.arg(layers, c("points", "lines"), several.ok = TRUE)
  # stopifnot(class(threshold) == "logical")
  stopifnot(class(smooth) == 'logical')
  stopifnot(class(span) == "numeric")
  stopifnot(class(facet_site) == "logical")
  facet_scales <- match.arg(facet_scales, c("fixed", "free_y", "free_x", "free"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  stopifnot(class(numcol) %in% c("numeric", "integer"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))

  params <- c("Alkalinity_mgL", "Ca_mgL", "ChlA_mgm3", "ChlA_Pheo_pct", "ChlA_ugL", "Cl_mgL", "Depth_m",
              "DO_mgL", "DOC_mgL", "DOsat_pct", "Hg_ngL", "HgMethyl_ngL", "K_mgL", "Mg_mgL", "N_ugL",
              "Na_mgL", "NH4_ugL", "NO2+NO3_ugL", "P_ugL", "pH", "Secchi_m", "Si_mgL", "SO4_mgL", "SpecCond_uScm",
              "TempAir_C", "TempWater_C", "Transp_cm", "TSS_mgL", "Turbidity_NTU", "WaterLevel_m",
              "WaveHt_cm", "WaveHt_m", "WindDir_Deg")

  typo <- parameter[(!parameter %in% params)]

  if(any(!parameter %in% params)){
    stop(paste0("The following parameters are not accepted values: ", paste0(typo, collapse = ","), "\n"),
         "Accepted values: ", paste0(params, collapse = ", "))}

  #-- Compile data for plotting --
  wdat <-
    getResults(park = park, site = site, site_type = site_type, sample_type = "VS", years = years,
               months = months, active = active, parameter = parameter, sample_depth = sample_depth,
               include_censored = include_censored) |>
    group_by(Location_ID, year, month, sample_date, doy, Activity_Relative_Depth, param_name, censored) |>
    summarize(value = median(value, na.rm = T), .groups = 'drop')
  # Drop NAs (often from params that only have censored data and censored = F)

  wdat <- wdat[!is.na(wdat$value),]

  wdat$param_label <- ifelse(grepl("_", wdat$param_name),
                             paste0(gsub("_", " (", wdat$param_name), ")"),
                             paste0(wdat$param_name)
  )

  params <- unique(wdat$param_label) # for alt text
  
  # join wdat with WQ thresholds, stored as a dataset in the package
  if(nrow(wdat) == 0){stop("Combination of sites, years, and parameters returned a data frame with no records.")}

  #-- Set up plotting features --
  ylabel <- ifelse(length(unique(wdat$param_label)) == 1, unique(wdat$param_label), "Value")
  #wdat_cens <- wdat |> filter(censored == TRUE)

  wdat$sample_date <- as.Date(wdat$sample_date, format = c("%Y-%m-%d"))

  # Code below is to make an x-axis that can be generalized across a range of years/months
  # to have logical tick placement and labels, and to not include Nov-Apr, when when
  # samples aren't collected

  # May 1 doy = 122; Oct 31 doy = 305
  wdat$doy_norm <- (wdat$doy - 122)/(305-122)
  wdat$x_axis <- wdat$year - years[1] + wdat$doy_norm

  year_len <- length(unique(years))
  mon_len <- length(unique(months))

  wdat$mon <- factor(format(wdat$sample_date, "%b"), month.abb, ordered = TRUE)
  wdat$mon <- wdat$mon[,drop = T]

  # Expand year and months to include missed periods that make x-axis funky
  #data_years <- range(wdat$year)
  time_mat1 <- expand.grid(year = years, month = months) |> arrange(year, month)
  time_mat1$mon <- factor(time_mat1$month, levels = time_mat1$month, labels = month.abb[time_mat1$month], ordered = TRUE)
  time_mat1$mon <- time_mat1$mon[,drop = T]
  time_mat1$date <- as.Date(paste0(time_mat1$year, "-", time_mat1$month, "-", "01"), format = c("%Y-%m-%d"))
  time_mat1$doy <-  as.numeric(strftime(time_mat1$date, format = "%j"))
  time_mat1$doy_norm <- (time_mat1$doy - 122)/(305-122)
  time_mat1$x_axis <- time_mat1$year - years[1] + time_mat1$doy_norm

  time_mat <- time_mat1 |>
    filter(date <= max(wdat$sample_date)) |>
    filter(date >= min(wdat$sample_date))# drop dates not included in wdat
  time_mat$x_label <- if(year_len == 1){as.character(paste0(time_mat$mon, "-01"))
    } else if(year_len %in% c(2, 3, 4)){paste0(time_mat$mon, "-", time_mat$year)
    } else {(time_mat$year)}

  x_row_breaks <- if(year_len == 1){c(1:6)
  } else if(year_len %in% 2){seq(1, year_len * mon_len , 2)
  } else if(year_len %in% c(3:5)){seq(1, year_len * mon_len, 3)
  } else if(year_len %in% c(5:10)){seq(1, year_len * mon_len, mon_len)
          } else if(year_len > 10){seq(1, year_len * mon_len, mon_len*2)}

  xbreaks <- time_mat$x_axis[x_row_breaks]
  xlabs <- time_mat$x_label[x_row_breaks]

  vir_pal = ifelse(palette %in%
                     c("viridis", "magma", "plasma", "turbo", "mako", "rocket", "cividis", "inferno"),
                       "viridis", "colbrew")
  # set up facets
  facet_param <- if(length(unique(parameter)) > 1){TRUE} else {FALSE}


  pal <-
    if(any(vir_pal == "colbrew")){
      colorRampPalette(palette)(length(parameter) * length(unique(wdat$Location_ID)))
      #rep(colorRampPalette(palette)(length(unique(parameter))), times = length(parameter) * length(unique(wdat2$SiteCode)))
    }

  #-- Create plot --
  trendplot <-
    if(include_censored == TRUE){
      ggplot(wdat, aes(x = x_axis, y = value, group = Location_ID, #if(smooth == TRUE){Location_ID} else{year},
                       color = Location_ID, fill = Location_ID)) +
        # layers*
        {if(smooth == TRUE) geom_smooth(method = 'loess', formula = 'y ~ x', se = F, span = span, linewidth = line_size) } +
        {if(smooth == FALSE & any(layers %in% "lines")) geom_line(linewidth = line_size)} +
        {if(any(layers %in% "points")) geom_point(aes(shape = censored), alpha = 0.4, size = point_size)} +
        {if(any(layers %in% "points"))
          scale_shape_manual(values = c(19, 8), labels = c("Real", "Censored"), name = "Legend")} +
        # {if(any(layers %in% "points"))
        #   scale_size_manual(values = c(3,3.5), labels = c("Real", "Censored"), name = "legend")} +
        # {if(threshold == TRUE){geom_hline(aes(yintercept = UpperThreshold, linetype = "Upper WQ Threshold"), lwd = 0.7)}} +
        # {if(threshold == TRUE){geom_hline(aes(yintercept = LowerThreshold, linetype = "Lower WQ Threshold"), lwd = 0.7)}} +
        # {if(threshold == TRUE){scale_linetype_manual(values = c("dotted", "dashed"))}} +
        # facets
        {if(facet_site == TRUE & facet_param == FALSE){facet_wrap(~Location_ID, scales = facet_scales, ncol = numcol)}} +
        {if(facet_site == FALSE & facet_param == TRUE){facet_wrap(~param_label, scales = 'free_y', ncol = numcol)}} +
        {if(facet_site == TRUE & facet_param == TRUE){facet_wrap(~Location_ID + param_label, scales = 'free_y', ncol = numcol)}} +
        # themes
        theme_WQ() +
        theme(legend.position = legend_position,
              legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        {if(any(gridlines %in% c("grid_y", "both"))){
          theme(panel.grid.major.y = element_line(color = 'grey'))}} + #,
        #panel.grid.minor.y = element_line(color = 'grey'))}}+
        {if(any(gridlines %in% c("grid_x", "both"))){
          theme(panel.grid.major.x = element_line(color = 'grey'))}} + #,
        #panel.grid.minor.x = element_line(color = 'grey'))}}+
        # palettes
        {if(any(vir_pal == "viridis")) scale_color_viridis_d(option = palette)} +
        {if(any(vir_pal == "viridis")) scale_fill_viridis_d(option = palette)} +
        {if(any(vir_pal == "colbrew")) scale_fill_manual(values = pal)} +
        {if(any(vir_pal == "colbrew")) scale_color_manual(values = pal)} +
        #axis format
        scale_x_continuous(breaks = xbreaks,
                           labels = xlabs,
                           limits = c(min(wdat$x_axis), max(wdat$x_axis))) +
        scale_y_continuous(n.breaks = 8) +
        # labels
        #labs(x = "Year", y = ylab) +
        labs(x = NULL, y = ylabel, 
             alt = paste0("Trend plot for ", paste0(params, collapse = ", "))) +
        guides(fill = guide_legend(order = 1),
               color = guide_legend(order = 1),
               shape = guide_legend(order = 1))
    } else {
      ggplot(wdat, aes(x = x_axis, y = value, group = Location_ID, #if(smooth == TRUE){Location_ID} else{year},
                        color = Location_ID, fill = Location_ID)) +
        #layers
        {if(smooth == TRUE) geom_smooth(method = 'loess', formula = 'y ~ x', se = F, span = span, linewidth = line_size) } +
        {if(smooth == FALSE & any(layers %in% "lines")) geom_line(linewidth = line_size)} +
        {if(any(layers %in% "points")) geom_point(alpha = 0.4, size = point_size)} +
        # {if(threshold == TRUE){geom_hline(aes(yintercept = UpperThreshold, linetype = "Upper WQ Threshold"), lwd = 0.7)}} +
        # {if(threshold == TRUE){geom_hline(aes(yintercept = LowerThreshold, linetype = "Lower WQ Threshold"), lwd = 0.7)}} +
        # {if(threshold == TRUE){scale_linetype_manual(values = c("dashed", "solid"))}} +
        # facets
        {if(facet_site == TRUE & facet_param == FALSE){facet_wrap(~Location_ID, scales = facet_scales, ncol = numcol)}} +
        {if(facet_site == FALSE & facet_param == TRUE){facet_wrap(~param_label, scales = 'free_y', ncol = numcol)}} +
        {if(facet_site == TRUE & facet_param == TRUE){facet_wrap(~Location_ID + param_label, scales = 'free_y', ncol = numcol)}} +
        # themes
        theme_WQ() +
        theme(legend.position = legend_position,
              legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        {if(any(gridlines %in% c("grid_y", "both"))){
          theme(panel.grid.major.y = element_line(color = 'grey'))}} + #,
        #panel.grid.minor.y = element_line(color = 'grey'))}}+
        {if(any(gridlines %in% c("grid_x", "both"))){
          theme(panel.grid.major.x = element_line(color = 'grey'))}} + #,
        #panel.grid.minor.x = element_line(color = 'grey'))}}+
        # color palettes
        {if(any(vir_pal == "viridis")) scale_color_viridis_d(option = palette)} +
        {if(any(vir_pal == "viridis")) scale_fill_viridis_d(option = palette)} +
        {if(any(vir_pal == "colbrew")) scale_fill_manual(values = pal)} +
        {if(any(vir_pal == "colbrew")) scale_color_manual(values = pal)} +
        #axis format
        scale_x_continuous(breaks = xbreaks,
                           labels = xlabs,
                           limits = c(min(wdat$x_axis), max(wdat$x_axis))) +
        scale_y_continuous(n.breaks = 8) +
        # labels
        #labs(x = "Year", y = ylab) +
        labs(x = NULL, y = ylabel) +
        guides(fill = guide_legend(order = 1),
               color = guide_legend(order = 1),
               shape = guide_legend(order = 1))
    }

  suppressWarnings(trendplot)

}


