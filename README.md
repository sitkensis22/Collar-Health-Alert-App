# Notifcation Alert App

MoveApps

Github repository: *github.com/sitkensis22/Notification-Alert-App* *https://github.com/sitkensis22/Notification-Alert-App*

## Description
Generates and appends field to data for 6 different types of alerts that are were developed to monitor collar health: (1) mortality status, (2) cluster analysis, (3) maximum net-squared displacement, (4) voltage levels, (5) GPS accuracy, and (6) GPS transmission gaps.

## Documentation
This App provides a variety of tools to monitor collar health and generate alerts that are appended to the user's move2 dataset when present. It was developed to address the challenge of monitoring collars from different vendors that send out various alerts and require the user to use multple software platforms to monitor collar status and health. Also, rather than rely on other MoveApps in a workflow to provide fields for alerts (e.g., distanceMoved), the App has built-in functinonality to monitor movement anamolies using cluster analysis and calculating the maximum net-squared displacement over a user-provided duration of time. The main output from this app is the move2 dataset with logical (TRUE/FALSE) fields appended for each alert class where the condition is TRUE for locations that met the alert criteria and FALSE otherwise. For large datasets, the cluster analysis can take considerable time to run, but this function can be switched off using the default setting (cluster = false). If a log_folder name is provided, then an event log is created and stored within the base package in R for use in the Notification Filter App, which provides the abilty to filter alerts from this App by setting specific delays or other custom filters. Note this functionality is in testing right now and may need to be updated. Finally, this App was designed as a precusor step in a workflow for the Notificaiton Shiny App that allows the user to visualize the data in Leaflet basemaps, as well as graphical and tabular form.

### Application scope
#### Generality of App usability
This app was developed for any taxinomic group. 

Besides collar health alerts, the cluster analysis functionality in the App could also be used for predation rate studies, especially when combined with the Notification Shiny App. 

#### Required data properties
The App should work for any kind of (location) data.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
`log_path.txt`: txt-file with name of event log folder.
`alias_list.rds`: rds-file with alias fields and values used for mortality, voltage, and gps_accuracy for use in Notification Shiny App.

### Settings 
'Subdirectory to store event log' (log_folder): If this character string is provided, a subdirectory will be created within R base package that is meant to be used in subsequent steps in a workflow for filtering alerts using the Notification Filter App. The event log is necessary to delay alert notifications in the Notification Filter App, Note that if this input is left as null (the default), then no event log will be created. Ideally, the specific study ID is used as the log_folder name, which will allow a unique folder name to be generated. This functionality is still in testing and may be updated.
'Set mortality alert trigger' (mortality): The logical indicator acts as a switch to turn on mortality event monitoring based on a field or multiple fields provided in the next input. 
'Mortality field name' (mortality_alias): This character string is the field name that tracks mortality status in the dataset. Note that multiple fields may be provided to accomodate datasets that are from multiple collar vendors with different mortality status fields. The field is ignored is the mortality alert is not activated.
'Mortality status value' (mortality_value): This character string is the value that indicates that a mortality has occurred for a given location. Note that multiple fields may be provided to accomodate datasets that are from multiple collar vendors with different mortality status fields. The field is ignored is the mortality alert is not activated.
'Set cluster alert trigger' (cluster)



*Example:* `Radius of resting site` (radius): Defined radius the animal has to stay in for a given duration of time for it to be considered resting site. Unit: `metres`.

### Changes in output data
*Specify here how and if the App modifies the input data. Describe clearly what e.g. each additional column means.*

*Examples:*

The App adds to the input data the columns `Max_dist` and `Avg_dist`. They contain the maximum distance to the provided focal location and the average distance to it over all locations. 

The App filterers the input data as selected by the user. 

The output data is the outcome of the model applied to the input data. 

The input data remains unchanged.

### Most common errors
*Please describe shortly what most common errors of the App can be, how they occur and best ways of solving them.*

### Null or error handling
*Please indicate for each setting as well as the input data which behaviour the App is supposed to show in case of errors or NULL values/input. Please also add notes of possible errors that can happen if settings/parameters are improperly set and any other important information that you find the user should be aware of.*

*Example:* **Setting `radius`:** If no radius AND no duration are given, the input data set is returned with a warning. If no radius is given (NULL), but a duration is defined then a default radius of 1000m = 1km is set. 
