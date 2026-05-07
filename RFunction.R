library("amt")
library("ctmm")
library("GPSeqClus")
library("move2")
library("sf")
library("tidyverse")
library("units")

## The parameter "data" is reserved for the data object passed on from the previous app

# R function to generate event notifications based on 6 alert types (note movement has cluster and nsd)
rFunction = function(
    # data input (move2 object)
  data, # move2 data
  # alert class 1 = manufacturer notification of mortality event
  mortality = FALSE, # include a manufacturer mortality notification event field?
  mortality_alias = NULL, # name of variable that tracks mortality status (can be more than one name)
  mortality_value = NULL, # levels of variable that indicate a mortality event
  # alert class 2 = cluster event
  cluster = FALSE, # include cluster analysis to detect events?
  cluster_radius = 50, # search radius in meters when using cluster analysis
  cluster_window = 1, # moving window length when using cluster analysis
  cluster_minlocations = 5, # minimum number of locations when using cluster analysis
  cluster_duration = 5, # minimum number of cluster duration in days to include as event
  # alert class 3 = nsd event
  nsd = FALSE, # include net-squared displacement to detect events?
  nsd_value = 1000, # area in square meters as a minimum threshold based on daily NSD to have an event
  nsd_duration = 5, # number of days to summarize maximum NSD over
  # alert class 4 = collar voltage event
  voltage = FALSE, # check for low voltage levels in collar
  voltage_alias = NULL, # name of voltage field to check (can be more than one name)
  voltage_value = NULL, # minimum voltage to trigger a warning (use 1st quartile if left as NULL)
  # alert class 5 = GPS accuracy check
  gps_accuracy = FALSE, # check if collar is have low accuracy (e.g., high percentage of 2D fixes)
  gps_accuracy_alias = NULL, # can be more than one field (e.g, different collar vendors for same project)
  gps_accuracy_value = NULL, # what levels of the variable indicate low accuracy?
  gps_accuracy_prop = 0.10, # what proportion of low accuracy locations should trigger an event
  # alert class 6 = GPS transmission gap
  gps_transmission = FALSE, # check if collar has a gap in GPS transmissions
  gps_transmission_gap = 10, # number of days between current date and last GPS transmission to trigger an event
  gps_transmission_include_current = FALSE, # add the current system date to the timestamp vector in calculating the time differences
  # alert class 7 = GPS resurrection check
  gps_resurrection = FALSE, # check if a collar has resurrected after a period on non-transmission
  gps_resurrection_duration = 5, # set the number of days that a collar has been active again after a period of non-transmission to trigger alert
  ...){
  # add unique record identifier to data
  data$FID <- 1:nrow(data)
  # add all alert fields to data and set to zero
  data$mortality <- numeric(nrow(data))
  data$cluster <- numeric(nrow(data))
  data$nsd <- numeric(nrow(data))
  data$voltage <- numeric(nrow(data))
  data$gps_accuracy <- numeric(nrow(data))
  data$gps_transmission <- numeric(nrow(data))
  data$gps_resurrection <- numeric(nrow(data))
  # alert class 1 = manufacturer notification of mortality event
  if(mortality){
    # set warning for condition true but missing alias or value
    # this will be replace with logger.warning() using in Moveapps
    if(mortality & is.null(mortality_alias) | mortality & is.null(mortality_value)){
      logger.warn("Must provide mortality alias and mortality value when mortality event is requested")
    }
    # check if mortality alias is in the dataset
    if(isFALSE(all(mortality_alias %in% colnames(data)))){
      alias_not_found <- mortality_alias[which(mortality_alias %in% colnames(data) == FALSE)]
      logger.warn(paste("Mortality alias(es) not found in dataset:",alias_not_found))
    }
    # check if mortality variables are a factor, if not, convert them
    if(isFALSE(all(data |> as.data.frame() |> dplyr::select(all_of(mortality_alias)) |> sapply(is.factor)))){
      check_factor_index <- which(data |> as.data.frame() |> dplyr::select(all_of(mortality_alias)) |> sapply(is.factor) == FALSE)
      data <- data |> mutate(across(mortality_alias[check_factor_index], as.factor))
    }
    # check if mortality values exist in levels of mortality alias variable(s)
    test_levels <- data |> as.data.frame() |> dplyr::select(all_of(mortality_alias)) |> 
      pivot_longer(cols = all_of(mortality_alias),
                   names_to = "test_var",
                   values_to = "test_vals") 
    # now test if mortality_values are in test_vals
    if(any(mortality_value %in% levels(test_levels$test_vals) == FALSE)){
      variable_not_found <- mortality_value[which(mortality_value %in% test_levels$test_vals == FALSE)]
      logger.warn(paste("At least one mortality value not found in levels of mortality status variable(s):",variable_not_found))
    }  
    # use factor() to remove unused levels
    data <- data |> mutate(across(all_of(mortality_alias), factor))
    # check for mortality alerts based on manufacturer 
    mortality_check <- data |> 
      pivot_longer(cols = all_of(mortality_alias), 
                   names_to = "alias", 
                   values_to = "alias_vals") |>
      group_by(.data[[mt_track_id_column(data)]]) |> 
      filter(alias_vals %in% mortality_value) |>
      mutate(mortality_status = alias_vals) |>
      dplyr::select(-alias,-alias_vals) |> 
      ungroup()
    # reset to move2 object
    mortality_check <-mt_as_move2(mortality_check,
                                  sf_column_name = "geometry", time_column = mt_time_column(data),
                                  track_id_column = mt_track_id_column(data))
    # set class of mortality check to class of data
    class(mortality_check) = class(data)
    # remove duplicate records based on FID
    if(any(duplicated(mortality_check$FID))){
      mortality_check <- mortality_check |> slice(-which(duplicated(FID)))
    }
    # add to event list if any mortalities identified
    if(nrow(mortality_check) > 0){
      # now set the records that have a mortality event to 1
      data$mortality[which(data$FID %in% mortality_check$FID)] = 1
    }
  }
  # alert class 2 = cluster event
  # check for cluster and carry out if TRUE
  if(cluster){
    # convert move2 to data frame for cluster analysis
    clust_data <- data |> as.data.frame()
    # add longitude and latitude coordinates to data frame
    clust_data <- cbind(clust_data, st_coordinates(data))
    # set ID, Date, Latitude, and Longitude names
    clust_data <- clust_data |> rename(AID = mt_track_id_column(data),
                                       TelemDate = mt_time_column(data),
                                       Long = X,
                                       Lat = Y)
    # fix sequential cluster algorithm using GPSeq_clus
    clust_out <- suppressWarnings(tryCatch(GPSeq_clus(dat = clust_data,
                                                      search_radius_m = cluster_radius,
                                                      window_days = cluster_window,
                                                      clus_min_locs = cluster_minlocations,                                    
                                                      centroid_calc = "mean",show_plots = c(FALSE, "mean"),                          
                                                      store_plots = FALSE, scale_plot_clus = FALSE,prbar=FALSE),
                                           error = function(e) {NULL}))
    if(isFALSE(is.null(clust_out))){
      clust_out[[2]]$clus_dur_day <- clust_out[[2]]$clus_dur_hr
      units(clust_out[[2]]$clus_dur_day) <- "days"
      # check for minimum number of cluster days
      if(any(clust_out[[2]]$clus_dur_day > cluster_duration)){
        # filter for minimum number of cluster days
        clust_out[[2]] <- clust_out[[2]] |> filter(clus_dur_hr > cluster_duration)
        # save cluster points to appArtefactPath
        write.csv(clust_out[[2]], file = AppArtefactPath("cluster_output.csv"), row.names = FALSE)
        # add cluster ID field to data
        data$clus_ID <- clust_out[[1]]$clus_ID
        # filter data based on cluster IDs in clust_out[[2]]
        cluster_check <- data |> filter(clus_ID %in% clust_out[[2]]$clus_ID) |> 
          dplyr::select(-clus_ID)
        # remove clus_ID from data
        data <- data |> dplyr::select(-clus_ID)
        # now set the records that have a cluster event to 1
        data$cluster[which(data$FID %in% cluster_check$FID)] = 1
      }
    }
  }
  # alert class 3 = NSD event
  if(nsd){
    # get UTM zone for data
    data_centroid <- data |> st_combine() |> st_centroid() |> st_coordinates() |>
      as.vector()
    # determine UTM zone
    zone_number <- floor((data_centroid[1] + 180) / 6) + 1
    utm_crs <- paste("+proj=utm",paste0("+zone=",zone_number),"+datum=WGS84 +units=m +no_defs")
    data_utm <- data |> st_transform(st_crs(utm_crs))
    # create amt dataset
    amt_track <- data_utm |> mutate(x = st_coordinates(data_utm)[,1], y = st_coordinates(data_utm)[,2],
                                    id = mt_track_id(data_utm), t = mt_time(data_utm)) |> 
      st_drop_geometry() |> 
      amt::make_track(.x = x, .y = y, .t = t,
                      id = id,crs = utm_crs)
    # create variable for user-defined number of days
    day_interval <- ifelse(nsd_duration > 1, paste(nsd_duration,"days"), paste(nsd_duration,"day"))
    # need to check for individuals that have a shorter duration of data than the day interval
    amt_track <- amt_track |> group_by(id) |> mutate(date_range = max(timestamp) - min(timestamp)) |> ungroup()
    # now filter out individuals where data_range is less than day_interval
    if(any(as.numeric(amt_track$date_range) <= nsd_duration)){
      amt_track <- amt_track |> filter(as.numeric(date_range) > nsd_duration)
    }
    # create index for group over a user-defined number of days
    amt_track <- amt_track |> mutate(day = lubridate::date(t_)) |> group_by(id) |>
      mutate(day_index = as.factor(ifelse(is.na(as.numeric(cut(day, seq(min(day), max(day), by = day_interval)))),
                                          max(as.numeric(cut(day, seq(min(day), max(day), by = day_interval))),na.rm=TRUE)+1,
                                          as.numeric(cut(day, seq(min(day), max(day), by = day_interval)))))) |> ungroup()
    # split track by id and day index, calculate NSD, and then merge again
    amt_track_daily_nsd <- amt_track |>
      group_split(id,day_index) |>
      lapply(add_nsd) |> mt_stack(.track_combine = "merge")
    # now calculate max NSD by ID and day index
    amt_max_daily_nsd <- amt_track_daily_nsd |> dplyr::group_by(id, day_index) |> # Group by ID and day
      mutate(maxNSD = max(nsd_, na.rm=TRUE)) |> 
      ungroup() 
    # conduct check of nsd minimum area for event
    if(any(amt_max_daily_nsd$maxNSD < nsd_value)){
      # now set the records that have a NSD event to 1
      data$nsd[which(amt_max_daily_nsd$maxNSD < nsd_value)] = 1
    }
  }  
  # alert class 4 = voltage event
  if(voltage){ 
    # set warning for condition true but missing alias or value
    # this will be replace with logger.warning() using in Moveapps
    if(voltage & is.null(voltage_alias)){
      logger.warn("Must provide voltage alias when voltage event is requested")
    }
    # check if voltage alias is in the dataset
    if(isFALSE(all(voltage_alias %in% colnames(data)))){
      alias_not_found <- mortality_alias[which(voltage_alias %in% colnames(data) == FALSE)]
      logger.warn(paste("Voltage alias(es) not found in dataset:",alias_not_found))
    }
    # subset records by user-provided voltage values
    if(isFALSE(is.null(voltage_value))){ 
      # nest these ifelse statements to avoid error
      if(voltage_value >= 1){
        voltage_check <- data |> 
          pivot_longer(cols = all_of(voltage_alias), 
                       names_to = "alias", 
                       values_to = "alias_vals") |> 
          mutate(alias_vals = set_units(alias_vals, mV)) |>
          group_by(mt_track_id_column(data)) |> 
          filter(alias_vals <= set_units(voltage_value, mV)) |>
          mutate(tag_voltage = alias_vals) |> 
          dplyr::select(-alias,-alias_vals) |>
          ungroup()  
      }else
        # use given quantile value if voltage_value < 1  
        if(voltage_value < 1){  # need to fix this to test for NULL
          voltage_check <- data |> 
            pivot_longer(cols = all_of(voltage_alias), 
                         names_to = "alias", 
                         values_to = "alias_vals") |> 
            mutate(alias_vals = set_units(alias_vals, mV)) |>
            group_by(mt_track_id_column(data)) |> 
            filter(alias_vals <= set_units(as.numeric(quantile(alias_vals, probs = voltage_value, na.rm = TRUE)), mV)) |>
            mutate(tag_voltage = alias_vals) |> 
            dplyr::select(-alias,-alias_vals) |>
            ungroup()
        }
    }else
      # subset records by first quantile of voltage values
      if(is.null(voltage_value)){ # need to fix this to test for NULL
        voltage_check <- data |> 
          pivot_longer(cols = all_of(voltage_alias), 
                       names_to = "alias", 
                       values_to = "alias_vals") |> 
          mutate(alias_vals = set_units(alias_vals, mV)) |>
          group_by(mt_track_id_column(data)) |> 
          filter(alias_vals <= set_units(as.numeric(quantile(alias_vals, probs = 0.25, na.rm = TRUE)), mV)) |>
          mutate(tag_voltage = alias_vals) |> 
          dplyr::select(-alias,-alias_vals) |>
          ungroup() 
      }
    if(nrow(voltage_check) > 0){
      # reset to move2 object
      voltage_check <-mt_as_move2(voltage_check,
                                  sf_column_name = "geometry", time_column = mt_time_column(data),
                                  track_id_column = mt_track_id_column(data))
      # set class of voltage check to class of data
      class(voltage_check) = class(data)
      # remove duplicate records based on FID
      if(any(duplicated(voltage_check$FID))){
        voltage_check <- voltage_check |> slice(-which(duplicated(FID)))
      }
      # now set the records that have a voltage event to 1
      data$voltage[which(data$FID %in% voltage_check$FID)] = 1
    }
  }
  # alert class 5 = GPS accuracy event
  if(gps_accuracy){
    # set warning for condition true but missing alias or value
    # this will be replace with logger.warning() using in Moveapps
    if(gps_accuracy & is.null(gps_accuracy_alias) | gps_accuracy & is.null(gps_accuracy_value)){
      logger.warn("Must provide GPS accuracy alias and GPS accuracy value when GPS accuracy event is requested")
    }
    # check if GPS accuracy alias is in the dataset
    if(isFALSE(all(gps_accuracy_alias %in% colnames(data)))){
      alias_not_found <- gps_accuracy_alias[which(gps_accuracy_alias %in% colnames(data) == FALSE)]
      logger.warn(paste("GPS accuracy alias(es) not found in dataset:",alias_not_found))
    }
    # subset records for based on GPS accuracy supplied 
    # check if gps accuracy variables are a factor, if not, convert them
    if(isFALSE(all(data |> as.data.frame() |> dplyr::select(all_of(gps_accuracy_alias)) |> sapply(is.factor)))){
      check_factor_index <- which(data |> as.data.frame() |> dplyr::select(all_of(gps_accuracy_alias)) |> sapply(is.factor) == FALSE)
      data <- data |> mutate(across(gps_accuracy_alias[check_factor_index], as.factor))
    }
    # check if GPS accuracy values exist in levels of GPS accuracy alias variable(s)
    test_levels <- data |> as.data.frame() |> dplyr::select(all_of(gps_accuracy_alias)) |>
      pivot_longer(cols = all_of(gps_accuracy_alias),
                   names_to = "test_var",
                   values_to = "test_vals") 
    # now test if GPS accuracy values are in test_vals
    if(any(gps_accuracy_value %in% levels(test_levels$test_vals) == FALSE)){
      variable_not_found <- gps_accuracy_value[which(gps_accuracy_value %in% test_levels$test_vals == FALSE)]
      logger.warn(paste("At least one GPS accuracy value not found in levels of GPS accuracy variable(s):",variable_not_found))
    } 
    # use factor() to remove unused levels
    data <- data |> mutate(across(all_of(gps_accuracy_alias), factor))
    # conduct GPS accuracy event check
    gps_accuracy_check <- data |> 
      pivot_longer(cols = all_of(gps_accuracy_alias), 
                   names_to = "alias", 
                   values_to = "alias_vals") |>
      group_by(mt_track_id_column(data)) |> 
      filter(alias_vals %in% gps_accuracy_value) |>
      mutate(gps_fix_type = alias_vals) |>
      dplyr::select(-alias,-alias_vals) |> 
      ungroup()
    # create dataset if data contains any poor fixes
    if(nrow(gps_accuracy_check)>0){
      # reset to move2 object
      gps_accuracy_check <-mt_as_move2(gps_accuracy_check,
                                       sf_column_name = "geometry", time_column = mt_time_column(data),
                                       track_id_column = mt_track_id_column(data))
      # set class of GPS accuracy check to class of data
      class(gps_accuracy_check) = class(data)
      # remove duplicate records based on FID
      if(any(duplicated(gps_accuracy_check$FID))){
        gps_accuracy_check <- gps_accuracy_check_check |> slice(-which(duplicated(FID)))
      }
      # calculate counts of bad fixes for each individual
      gps_accuracy_sum <- gps_accuracy_check |> group_by(.data[[mt_track_id_column(data)]]) |>
        summarize(rowCount = n()) |> as.data.frame() |> dplyr::select(-geometry)
      # calculate total row counts for each individual
      gps_total_sum <- data |> filter(.data[[mt_track_id_column(data)]] %in% gps_accuracy_sum[,mt_track_id_column(data)]) |> 
        group_by(.data[[mt_track_id_column(data)]]) |>
        summarize(totalCount = n()) |> as.data.frame() |> dplyr::select(-geometry)
      # now add total count to gps_accuarcy_sum
      gps_accuracy_sum$totalCount <- gps_total_sum$totalCount
      # now summarize proportion of poor locations
      prop_bad_locs <- gps_accuracy_sum |> group_by(.data[[mt_track_id_column(data)]]) |>
        summarise(prop_poor = rowCount/totalCount)
      # check if any prop_poor is above threshold
      if(any(prop_bad_locs$prop_poor>gps_accuracy_prop)){
        prop_bad_ids <- prop_bad_locs |> slice(which(prop_bad_locs$prop_poor>gps_accuracy_prop)) |>
          # dplyr::select(.data[[mt_track_id_column(data)]])
          dplyr::select(all_of(mt_track_id_column(data)))
        # filter data by IDs 
        gps_accuracy_check <- gps_accuracy_check |> filter(.data[[mt_track_id_column(data)]] %in% prop_bad_ids)
        # now set the records that have a gps accuracy event to 1
        data$gps_accuracy[which(data$FID %in% gps_accuracy_check$FID)] = 1
      }
    }
  }
  # alert class 6 = GPS transmission event 
  if(gps_transmission){
    # check for events based on timestamp
    if(gps_transmission_include_current){
      gps_transmission_check <- data |> 
        group_by(.data[[mt_track_id_column(data)]]) |> 
        mutate(time_diff = diff(c(.data[[mt_time_column(data)]],lubridate::with_tz(Sys.time(), "UTC")), units = "days")) |>
        ungroup()
    }else
      if(isFALSE(gps_transmission_include_current)){
        gps_transmission_check <- data |> 
          group_by(.data[[mt_track_id_column(data)]]) |> 
          mutate(time_diff = c(NA,diff(.data[[mt_time_column(data)]], units = "days"))) |>
          ungroup() |> slice(-1)
      }
    # check for time differences greater 
    if(any(gps_transmission_check$time_diff>gps_transmission_gap, na.rm = TRUE)){
      # filter data by IDs 
      gps_transmission_check <- gps_transmission_check |> slice(which(gps_transmission_check$time_diff>=gps_transmission_gap))
      # now set the records that have a gps_transmission event to 1
      data$gps_transmission[which(data$FID %in% gps_transmission_check$FID)] = 1
    }
  }
  # alert class 7 = GPS resurrection event
  if(gps_resurrection){
    # use settings from GPS_tranmission_gap to identify ressurection
    if(gps_transmission_include_current){
      gps_transmission_check <- data |> 
        group_by(.data[[mt_track_id_column(data)]]) |> 
        mutate(time_diff = diff(c(.data[[mt_time_column(data)]],lubridate::with_tz(Sys.time(), "UTC")), units = "days")) |>
        ungroup()
    }else
      if(isFALSE(gps_transmission_include_current)){
        gps_transmission_check <- data |> 
          group_by(.data[[mt_track_id_column(data)]]) |> 
          mutate(time_diff = c(NA,diff(.data[[mt_time_column(data)]], units = "days"))) |>
          ungroup() |> slice(-1)
      }
    # check for time differences greater than gps_transmission_gap
    if(any(gps_transmission_check$time_diff>gps_transmission_gap, na.rm = TRUE)){
      # filter data by IDs 
      gps_resurrection_check <- gps_transmission_check |> slice(which(gps_transmission_check$time_diff>=gps_transmission_gap)) 
      # need to get max indices of transmission gap for those that had events
      max_times_resurrection <- gps_resurrection_check |> group_by(.data[[mt_track_id_column(gps_resurrection_check)]]) |>
        summarize(maxTimes = max(.data[[mt_time_column(gps_resurrection_check)]], na.rm = TRUE),
                  maxFID = max(FID,na.rm=TRUE)) |> as.data.frame()              
      # now get time difference in days between gps_resurrection_check time and max time of each individual in gps_resurrection_check
      max_times_data <- data |> filter(mt_track_id(data) %in% mt_track_id(gps_resurrection_check)) |>
        group_by(.data[[mt_track_id_column(data)]]) |>
        summarize(maxTimes = max(.data[[mt_time_column(data)]], na.rm = TRUE),
                  maxFID = max(FID,na.rm=TRUE)) |> as.data.frame() 
      max_times_resurrection$timediff = as.numeric(difftime( max_times_data$maxTimes,max_times_resurrection$maxTimes, 
                                                             units = "days"))
      if(any(max_times_resurrection$timediff>gps_resurrection_duration, na.rm = TRUE)){
        # filter data by IDs 
        max_times_resurrection <- max_times_resurrection |> slice(which(max_times_resurrection$timediff>gps_resurrection_duration))
        # now loop over individuals to populate resurrection events to 1
        for(i in 1:nrow(max_times_resurrection)){
          data[data$FID %in% c(max_times_resurrection$maxFID[i]:max_times_data$maxFID[i]),]$gps_resurrection = 1
        }
      }
    }
    # end of alert event checks
  }
  # write any aliases and values attributes of the move2 data object
  if(mortality){
    attr(data, "mortality_alias") <- mortality_alias
    attr(data, "mortality_value") <- mortality_value
  }
  if(voltage){
    attr(data, "voltage_alias") <- voltage_alias
    attr(data, "voltage_value") <- voltage_value
  }
  if(gps_accuracy){
    attr(data, "gps_accuracy_alias") <- gps_accuracy_alias
    attr(data, "gps_accuracy_value") <- gps_accuracy_value
  }
  # get index of geometry field
  geometry_index <- which(colnames(data) == "geometry")
  # now organize data set
  data <- data[,c((1:ncol(data))[-geometry_index],geometry_index)]
  # now drop FID field
  data <- data |> dplyr::select(-FID)
  # now return move2 dataset (whether there are events or not)
  return(data)
  # end function
}  
