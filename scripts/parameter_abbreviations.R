#--------------------------------------------------------------------------------------------
# Until this is incorporated in the data package, generating an R package data file with
# parameter abbreviations. If new parameters are added, they will need to be hard coded here.
#--------------------------------------------------------------------------------------------

GLKN_WQ_abbreviations <- data.frame(
  rbind(
    c('Alkalinity, Total (total hydroxide+carbonate+bicarbonate)', 'mg/l', 'Alkalinity_mgL'),
    c('Calcium', 'mg/l', 'Ca_mgL'),
    c('Carbon, organic', 'mg/l', 'DOC_mgL'),
    c('Chloride', 'mg/l', 'Cl_mgL'),
    c('Chlorophyll a, free of pheophytin', 'mg/m3', 'ChlA_mgm3'),
    c('Chlorophyll a, free of pheophytin', 'ppb', 'ChlA_ugL'),
    c('Chlorophyll a, free of pheophytin', 'ug/l', 'ChlA_ugL'),
    c('Chlorophyll/Pheophytin ratio', '%', 'ChlA_Pheo_pct'),
    c('Cloud cover (choice list)', 'NULL', 'Cloud_pct'),
    c('Depth, bottom', 'm', 'Depth_m'),
    c('Depth, Secchi Disk Depth', 'm', 'Secchi_m'),
    c('Dissolved oxygen (DO)', 'mg/l', 'DO_mgL'),
    c('Dissolved oxygen saturation', '%', 'DOsat_pct'),
    c('Flow, stream stage (choice list)', 'NULL', 'Flow'),
    c('General Observation (text)', 'NULL', ''),
    c('Magnesium', 'mg/l', 'Mg_mgL'),
    c('Mercury', 'ng/l', 'Hg_ngL'),
    c('Methylmercury (+1)', 'ng/l', 'HgMethyl_ngL'),
    c('Nitrogen', 'ug/l', 'N_ugL'),
    c('Nitrogen, Ammonium (NH4) as N', 'ug/l', 'NH4_ugL'),
    c('Nitrogen, Nitrite (NO2) + Nitrate (NO3) as N', 'ug/l', 'NO2+NO3_ugL'),
    c('pH', 'None', 'pH'),
    c('Phosphorus as P', 'ug/l', 'P_ugL'),
    c('Potassium', 'mg/l', 'K_mgL'),
    c('Secchi Reading Condition (choice list)', 'None', 'SecchiCond'),
    c('Silicate', 'mg/l', 'Si_mgL'),
    c('Sodium', 'mg/l', 'Na_mgL'),
    c('Solids, Suspended (TSS)', 'mg/l', 'TSS_mgL'),
    c('Specific conductance', 'uS/cm', 'SpecCond_uScm'),
    c('Sulfur, sulfate (SO4) as SO4', 'mg/l', 'SO4_mgL'),
    c('Temperature, air', 'deg C', 'TempAir_C'),
    c('Temperature, water', 'deg C', 'TempWater_C'),
    c('Transparency, tube with disk', 'cm', 'Transp_cm'),
    c('Turbidity', 'NTU', 'Turbidity_NTU'),
    c('Water appearance (text)', 'NULL', 'WaterApp'),
    c('Water level', 'm', 'WaterLevel_m'),
    c('Water level in relation to reference point', 'm', 'WaterLevelRef_m'),
    c('Wave height', 'cm', 'WaveHt_m'),
    c('Wave height', 'm', 'WaveHt_cm'),
    c('Weather Comments (text)', 'NULL', ''),
    c('Wind Condition (choice List)', 'None', 'WindCond'),
    c('Wind Condition (choice List)', 'NULL', ''),
    c('Wind direction (direction from, expressed 0-360 deg)', 'Deg', 'WindDir_Deg')
  ))

colnames(GLKN_WQ_abbreviations) <- c("Characteristic_Name", "Result_Unit", "param_name")

usethis::use_data(GLKN_WQ_abbreviations, overwrite = T)
