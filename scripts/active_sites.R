#---------------------------------------------------------
# Determine sites that aren't active based on not having sample events in the past 10 years
#---------------------------------------------------
library(waterGLKN)
library(dplyr)
library(tidyr)
river_zip = ("../data/records-2309514.zip") # updated 20260916
lake_zip = ("../data/records-2318770.zip") # updated 20260916
importData(type = 'zip', filepath = c(river_zip, lake_zip))

res <- GLKN_WQ$Results
res$Park <- substr(res$Location_ID, 1, 4)
res$Date <- format(as.Date(res$Activity_Start_Date, format = "%Y-%m-%d"), "%Y-%m-%d")
res$Year <- as.numeric(substr(res$Date, 1, 4)) # faster than using format with date
res$Month <- as.numeric(substr(res$Date, 6, 7))
res$doy <- as.numeric(format(as.Date(res$Date, format = "%Y-%m-%d"), "%j"))

table(res$Year, useNA = 'always')
table(res$Month, useNA = 'always')
table(res$Park, res$Month)
table(res$Activity_Group_Type, res$Month)
table(res$Year, res$Month)
table(res$Park, res$Year, useNA = 'always')

site_by_year <- data.frame(table(res$Location_ID, res$Year)) |>
  select(Location_ID = Var1, Var2, Freq) |>
  tidyr::pivot_wider(names_from = Var2, values_from = Freq, names_prefix = "yr")

col_numbs <- (ncol(site_by_year)-10):ncol(site_by_year)
site_by_year$num_samps <- rowSums(site_by_year[,col_numbs])
site_by_year$active <- ifelse(site_by_year$num_samps > 0, TRUE, FALSE)

active_sites <- site_by_year[,c("Location_ID", "active")]
usethis::use_data(active_sites, overwrite = TRUE)
