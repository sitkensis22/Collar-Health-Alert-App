library("amt")
library("GPSeqClus")
library("move2")
library("sf")
library("tidyverse")
library("tools")
library("units")

## The parameter "data" is reserved for the data object passed on from the previous app

# R function to generate event notifications based on 6 alert types (note movement has cluster and nsd)
rFunction = function(
  # data input (move2 object)
  data, # move2 data
  log_event = TRUE, # logical to create event log or not
  log_folder = "log_folder", # create a subdirectory to store the log file for this project
  # alert class 1 = manufacturer notification of mortality event
  mortality = FALSE, # include a manufacturer mortality notification event field?
  mortality_alias = NULL, # name of variable that tracks mortality status (can be more than one name)
  mortality_value = NULL, # levels of variable that indicate a mortality event
  # alert class 2 = movement based event (identify clustered locations?, number of locations in cluster?)
  movement = FALSE, # check for movement an that could indicate a mortality or dropped collar?
  movement_type = c("cluster","nsd"), # use cluster analysis or daily net-squared displacement
  movement_search_radius = 50, # search radius in meters when using cluster analysis
  movement_cluster_window = 1, # moving window length when using cluster analysis
  movement_cluster_minlocations = 5, # minimum number of locations when using cluster analysis
  movement_cluster_mindays = 5, # minimum number of cluster duration in days to include as event
  movement_nsd_value = 1000, # area in square meters as a minimum threshold based on daily NSD to have an event
  movement_nsd_days = 5, # number of days to summarize maximum NSD over
  # alert class 3 = collar voltage event
  voltage = FALSE, # check for low voltage levels in collar
  voltage_alias = NULL, # name of voltage field to check (can be more than one name)
  voltage_value = "", # minimum voltage to trigger a warning (use 1st quartile if left as "")
  # alert class 4 = GPS accuracy check
  gps_accuracy = FALSE, # check if collar is have low accuracy (e.g., high percentage of 2D fixes)
  gps_accuracy_alias = NULL, # can be more than one field (e.g, different collar vendors for same project)
  gps_accuracy_value = NULL, # what levels of the variable indicate low accuracy?
  gps_accuracy_prop = 0.10, # what proportion of low accuracy locations should trigger an event
  # alert class 5 = GPS transmission gap
  gps_transmission = FALSE, # check if collar has a gap in GPS transmissions
  gps_transmission_gap = 3, # number of days between current date and last GPS transmission to trigger an event
  gps_transmission_include_current = FALSE, # add the current system date to the timestamp vector in calculating the time differences
  ...){
  # add unique record identifer to data
  data$FID <- 1:nrow(data)
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
      # create mortality variable for dataset (note that I changed this from using an event list before)
      data$mortality <- FALSE
      # now set the records that have a mortality event to TRUE
      data$mortality[which(data$FID %in% mortality_check$FID)] = TRUE
    }
  }
  # alert class 2 = movement based event (identify clustered locations?, number of locations in cluster?)
  if(movement){
    # check for cluster and carry out if TRUE
    if(any(movement_type == "cluster")){
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
      clust_out <- GPSeq_clus(dat = clust_data,
                              search_radius_m = movement_search_radius,
                              window_days = movement_cluster_window,
                              clus_min_locs = movement_cluster_minlocations,                                    
                              centroid_calc = "mean",show_plots = c(FALSE, "mean"),                          
                              store_plots = FALSE, scale_plot_clus = FALSE,prbar=FALSE)
      clust_out[[2]]$clus_dur_day <- clust_out[[2]]$clus_dur_hr
      units(clust_out[[2]]$clus_dur_day) <- "days"
      # check for minimum number of cluster days
      if(any(clust_out[[2]]$clus_dur_day > movement_cluster_mindays)){
        # filter for minimum number of cluster days
        clust_out[[2]] <- clust_out[[2]] |> filter(clus_dur_hr > movement_cluster_mindays)
        # add cluster ID field to data
        data$clus_ID <- clust_out[[1]]$clus_ID
        # filter data based on cluster IDs in clust_out[[2]]
        cluster_check <- data |> filter(clus_ID %in% clust_out[[2]]$clus_ID) |> 
          dplyr::select(-clus_ID)
        # remove clus_ID from data
        data <- data |> dplyr::select(-clus_ID)
        # create cluster variable for dataset (note that I changed this from using an event list before)
        data$cluster <- FALSE
        # now set the records that have a cluster event to TRUE
        data$cluster[which(data$FID %in% cluster_check$FID)] = TRUE
      }
    }
    if(any(movement_type == "nsd")){
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
      day_interval <- ifelse(movement_nsd_days > 1, paste(movement_nsd_days,"days"), paste(movement_nsd_days,"day"))
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
      if(any(amt_max_daily_nsd $maxNSD < movement_nsd_value)){
        # create NSD variable for dataset (note that I changed this from using an event list before)
        data$nsd <- FALSE
        # now set the records that have a NSD event to TRUE
        data$nsd[which(amt_max_daily_nsd$maxNSD < movement_nsd_value)] = TRUE
      }
    }
  }  
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
    if(voltage_value != "" & voltage_value >= 1){
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
      if(voltage_value != "" & voltage_value < 1){  
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
      }else
        # subset records by first quantile of voltage values
        if(voltage_value == ""){
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
      # create voltage variable for dataset (note that I changed this from using an event list before)
      data$voltage <- FALSE
      # now set the records that have a voltage event to TRUE
      data$voltage[which(data$FID %in% voltage_check$FID)] = TRUE
    }
  }
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
        # create gps accuracy variable for dataset (note that I changed this from using an event list before)
        data$gps_accuracy <- FALSE
        # now set the records that have a gps accuracy event to TRUE
        data$gps_accuracy[which(data$FID %in% gps_accuracy_check$FID)] = TRUE
      }
    }
  }
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
    if(any(gps_transmission_check$time_diff>gps_transmission_gap)){
      # filter data by IDs 
      gps_transmission_check <- gps_transmission_check |> slice(which(gps_transmission_check$time_diff>=gps_transmission_gap))
      # create gps_transmission variable for dataset (note that I changed this from using an event list before)
      data$gps_transmission <- FALSE
      # now set the records that have a gps_transmission event to TRUE
      data$gps_transmission[which(data$FID %in% gps_transmission_check$FID)] = TRUE
    }
  }
  # check data if there are events 
  if(any(names(data) %in% c("mortality","cluster","nsd","voltage","gps_accuracy","gps_transmission"))){
    # store directory for R-related user-specific data in base package
    temp_path <- tools::R_user_dir("base", which = "data")
    # create folder in R-related user-specific data location in base package
    if(isFALSE(dir.exists(paste0(temp_path,"/",paste0("log_",log_folder))))){
      dir.create(paste0(temp_path,"/",paste0("log_",log_folder)))
    }
    # create empty list to hold aliases and values
    alias_list <- list()
    # write any aliases and values to temp .rds file to use in Shiny app
    if(any(colnames(data) == "mortality")){
      alias_list$mortality_alias <- mortality_alias
      alias_list$mortality_value <- mortality_value
    }
    if(any(colnames(data) == "voltage")){
      alias_list$voltage_alias <- voltage_alias
      alias_list$voltage_value <- voltage_value
    }
    if(any(colnames(data) == "gps_accuracy")){
      alias_list$gps_accuracy_alias <- gps_accuracy_alias
      alias_list$gps_accuracy_value <- gps_accuracy_value
    }
    # save to alias list to temp_path folder as .rds
    saveRDS(alias_list, 
            file = paste0(temp_path,"/",paste0("log_",log_folder),"/alias_list.rds"))
    # write alias list as artifact for testing
    saveRDS(alias_list, file = appArtificatPath("alias_list.rds"))
    # create event log if log_event = TRUE
    if(log_event){
      # create empty list to hold unique event logs
      event_list = list()
      # need to build unique ids and notification types
      if(any(colnames(data)=="mortality")){
        event_list$mortality <- unique(data[,c(mt_track_id_column(data),"mortality")]) |> filter(mortality == TRUE) |> 
          as.data.frame() |> dplyr::select(-c(geometry,timestamp)) |> unique()
        names(event_list$mortality)[2] = "notification_type"
        event_list$mortality$notification_type = "mortality"
      }
      if(any(colnames(data)=="cluster")){
        event_list$cluster <- unique(data[,c(mt_track_id_column(data),"cluster")]) |> filter(cluster == TRUE) |>
          as.data.frame() |> dplyr::select(-c(geometry,timestamp)) |> unique()
        names(event_list$cluster)[2] = "notification_type"
        event_list$cluster$notification_type = "cluster"
      }
      if(any(colnames(data)=="nsd")){
        event_list$nsd <- unique(data[,c(mt_track_id_column(data),"nsd")]) |> filter(nsd == TRUE) |> 
          as.data.frame() |> dplyr::select(-c(geometry,timestamp)) |> unique()
        names(event_list$nsd)[2] = "notification_type"
        event_list$nsd$notification_type = "nsd"
      }
      if(any(colnames(data)=="voltage")){
        event_list$voltage <- unique(data[,c(mt_track_id_column(data),"voltage")]) |> filter(voltage == TRUE) |> 
          as.data.frame() |> dplyr::select(-c(geometry,timestamp)) |> unique()
        names(event_list$voltage)[2] = "notification_type"
        event_list$voltage$notification_type = "voltage"
      }
      if(any(colnames(data)=="gps_accuracy")){
        event_list$gps_accuracy <- unique(data[,c(mt_track_id_column(data),"gps_accuracy")]) |> filter(gps_accuracy == TRUE) |> 
          as.data.frame() |> dplyr::select(-c(geometry,timestamp)) |> unique()
        names(event_list$gps_accuracy)[2] = "notification_type"
        event_list$gps_accuracy$notification_type = "gps_accuracy"
      }
      if(any(colnames(data)=="gps_transmission")){
        event_list$gps_transmission <- unique(data[,c(mt_track_id_column(data),"gps_transmission")]) |> filter(gps_transmission== TRUE) |> 
          as.data.frame() |> dplyr::select(-c(geometry,timestamp)) |> unique()
        names(event_list$gps_transmission)[2] = "notification_type"
        event_list$gps_transmission$notification_type = "gps_transmission"
      }
      # create temp data to extract unique ids and notification types
      event_log  <- do.call(rbind, event_list)
      # add current system date
      event_log$logging_data <- as.character(Sys.Date())
      # reset row names
      row.names(event_log) <- 1:nrow(event_log)
      # write just this event log if one doesn't already exist
      if(isFALSE(file.exists(paste0(temp_path,"/",paste0("log_",log_folder),"/event_log.csv")))){
        write.csv(event_log, 
                  file = paste0(temp_path,"/",paste0("log_",log_folder),"/event_log.csv"),
                  row.names = FALSE)
      }else
        # append data and write as .csv file to temporary file
        if(file.exists(paste0(temp_path,"/",paste0("log_",log_folder),"/event_log.csv"))){
          temp_file <- read.csv(paste0(temp_path,"/",paste0("log_",log_folder),"/event_log.csv"))
          write.csv(rbind(temp_file,event_log), paste0(temp_path,"/",paste0("log_",log_folder),"/event_log.csv"),
                    row.names = FALSE)
        }
      # write event log to appArtifactPath to test if they can be retained across runs (can remove this if the solution works)
      write.csv(read.csv(paste0(temp_path,"/",paste0("log_",log_folder),"/event_log.csv")), file = appArtifactPath("event_log.csv"),
                row.names = FALSE)
      # write name of log folder to appArtifactPath
      write(paste0("log_",log_folder), file = appArtificatPath("log_path.txt"))
    }
    # organize and return results
    logger.info("Alerts were triggered for at least one field. The full dataset will be passed along with these alerts")

    # now return all items as a list (for now)
    return(data)
  }else
    if(any(names(data) %in% c("mortality","cluster","nsd","voltage","gps_accuracy","gps_transmission"))==FALSE){
      # return empty list if no events triggered
      logger.info("No events were triggered given the data and parameters that were set. No alerts were added to the data.")
    }
  # end function
}

