# Notifcation Alert App

MoveApps

Github repository: *github.com/sitkensis22/Notification-Alert-App* (*https://github.com/sitkensis22/Notification-Alert-App*)

## Description
Generates and appends fields to data for 7 different types of alerts that were developed to monitor collar health: (1) mortality status, (2) cluster analysis, (3) maximum net-squared displacement, (4) voltage levels, (5) GPS accuracy, (6) GPS transmission gaps, and (7) the resurrection of a GPS tag or collar.

## Documentation
This App provides a variety of tools to monitor collar health and generate alerts that are appended to the user's move2 dataset when present. It was developed to address the challenge of monitoring collars from different vendors within the same study that send out various alerts and require the user to use multple software platforms to monitor collar status and health. Also, rather than rely on other MoveApps in a workflow to provide fields for alerts (e.g., distanceMoved), the App has built-in functinonality to monitor, for example, movement anomalies using cluster analysis and calculating the maximum net-squared displacement over a user-provided duration of time. For large datasets, the cluster analysis can take considerable time to run, but this function can be switched off using the default setting (`cluster` = FALSE). Finally, this App was designed as a precusor step in a workflow for the Notificaiton Shiny App that allows the user to visualize the data in Leaflet basemaps, as well as graphical and tabular form. This App was recently updated to work with the Email-Alert App to send out email notifications when an alert has been triggered. 

### Application scope
#### Generality of App usability
This app was developed for any taxinomic group. 

Besides collar health alerts, the cluster analysis functionality in the App could also be used for predation rate studies, especially when combined with the Notification Shiny App. 

#### Required data properties
The App should work for any kind of (location) data. However, certain fields will be needed for mortality (e.g., "mortality_status"), voltage (e.g., "tag_voltage"), and GPS accuracy (e.g., "gps_fix_type_raw") monitoring.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
The App generates an artefact called 'cluster_output.csv' when cluster events are triggered, and this output is a .csv file of the mean centroid locations of the identified clusters. 

### Settings 
**Set mortality alert (`mortality`):** This logical input acts as a switch to turn on mortality event monitoring based on a field or multiple fields provided in the next input. 

**Mortality field name (`mortality_alias`):** This character string input is the field name that tracks mortality status in the dataset. Note that multiple fields may be provided to accomodate datasets that are from multiple collar vendors with different mortality status fields. Multiple values must be comma-separated. The field is ignored if the mortality alert is not activated.

**Mortality status value (`mortality_value`):** This character string input is the value that indicates that a mortality has occurred for a given location. Note that multiple values may be provided to accomodate datasets that are from multiple collar vendors with different mortality status fields. Multiple values must be comma-separated. The field is ignored if the mortality alert is not activated.

**Set cluster alert (`cluster`):** This logical input acts as a switch to turn on cluster event monitoring. 

**Cluster search radius (`cluster_radius`):** This numeric input defines the search radius in meters for cluster analysis. Note that the input will only be used when cluster trigger is activated.

**Cluster moving window (`cluster_window`):** This integer input defines the number of days for the moving window analysis in determining clusters. Note that the input will only be used when cluster trigger is activated.

**Minimum cluster locations (`cluster_minlocations`):** This integer input defines the minimum number of locations to form a cluster in cluster analyses. Note that the input will only be used when cluster trigger is activated.

**Minimum cluster duration (`cluster_duration`):** This integer input defines the mimimum duration in days as a threshold to include idenfied cluster events. This is useful when using cluster analysis to detect mortalties when many short duration clusters are present. Note that the input will only be used when cluster trigger is activated.

**Set net-squared displacement alert (`nsd`):** This logical input acts as a switch to turn on maximum net-squared displacement event monitoring.

**Threshold for maximum net-squared displacement (`nsd_value`):** This numeric input defines the threshold in square meters for the maximum net-squared displacement to trigger an event. Note that the input will only be used when net-squared displacement trigger is activated.

**Net-squared displacement duration (`nsd_duration`):** This integer input defines the duration of days to summarize the maximum net-squared displacement over. Note that the input will only be used when net-squared displacement trigger is activated.

**Set voltage alert (`voltage`):** This logical input acts as a switch to turn on voltage condition monitoring.

**Voltage field name (`voltage_alias`):** This character string input is the field name that tracks voltage in the dataset. Note that multiple fields may be provided to accomodate datasets that are from multiple collar vendors with different voltage fields. Multiple values must be comma-separated. The field is ignored if the voltage alert is not activated.

**Minimum voltage threshold (`voltage_value`):** This numeric input is the threshold to trigger a voltage event. If the value is > 0 and < 1, then the quartile function is used to determine the voltage threshold to trigger an event. If the default value is used (i.e., `voltage_value` = NULL), the first quartile (0.25) will be applied. Otherwise an input value > 1 will be the threshold to trigger a voltage event. The units for voltage are assumed to be in millivolts (mV), but some collars or tags may have different units. Thus, it may be advantageous to use the default first quartile setting or set a quantile between 0 and 1 (e.g., 0.10) in these cases. This is also recommended when different collar types are deployed within the same study and voltage is on vastly different scales. The field is ignored if the voltage alert is not activated.

**Set GPS accuracy alert (`gps_accuracy`):** This logical input acts as a switch to turn on GPS accuracy monitoring.

**GPS accuracy field name (`gps_accuracy_alias`):** This character string input is the field name that tracks GPS accuracy in the dataset. Note that multiple fields may be provided to accomodate datasets that are from multiple collar vendors with different GPS accuracy fields. Multiple values must be comma-separated. The field is ignored if the GPS accuracy alert is not activated.

**GPS accuracy value (`gps_accuracy_value`):** This character string input is the value that indicates that a missed location or poor GPS accuracy has occurred. Note that multiple values may be provided to accomodate datasets that are from multiple collar vendors and more than one GPS accuracy value indicates a missed location or poor GPS accuracy has occured. Multiple values must be comma-separated. The field is ignored if the GPS accuracy alert is not activated.

**Minimum proportion threshold (`gps_accuracy_prop`):** This numeric input is for the proportion of missed locations or poor GPS accuracy as a threshold to trigger a GPS accuracy event. The field is ignored if the GPS accuracy alert is not activated.

**Set GPS transmission alert (`GPS transmission`):** This logical input acts as a switch to turn on GPS transmission gap monitoring.

**GPS transmission gap value (`gps_transmission_gap`):** This integer input defines the gap in days to trigger a GPS transmission event. Note that the input will only be used when GPS transmission gap trigger is activated.

**Include current date in timestamp (`gps_transmission_include_current`):** This logical input acts will include the current system date in the timestamp vector in checking for GPS transmission anomalies. Note that the input will only be used when GPS transmission gap trigger is activated.

**Set GPS resurrection alert (`GPS resurrection`):** This logical input acts as a switch to turn on GPS resurrection monitoring. This feature detects when a collar or tag has resurrected after a period of non-transmission, which is indicated by a gap in GPS transmission. Note that this feature will is the input for `gps_transmission_gap` to first identify gaps in GPS transmission and then determine if the collar is resurrected.

**GPS resurrection duration (`gps_resurrection_duration`):** This integer input defines the duration in days to trigger a GPS resurrection event. When the collar or tag has resurrected longer than this duration after a period of non-transmission, the alert will be activated. Note that the input will only be used when GPS resurrection gap trigger is activated.

### Changes in output data
The App adds binary numerical (1/0) fields to the input data for each alert class where the condition is 1 for locations that meet the alert criteria and 0 otherwise. The following fields are added to the data: (1) mortality, (2) cluster, (3) nsd, (4) voltage, (5) gps_accuracy, (6) gps_transmission, and (7) gps_resurrection regardless if event triggers are detected or not. This functionality allows the App to be used in conjunction with the Email-Alert App when all 7 fields are included and the alert condition is set to 1 in the Email-Alert App. Note that these fields are used downstream in other Apps that integrate into a workflow such as the Notification Filter App and Notification Shiny App.The alias and value names provided as input for the App for mortality, voltage, and gps accuracy are stored as attribute data in the move2 data object that is output by the App for use in the Notification Shiny App within a workflow. 

### Most common errors

### Null or error handling
**Setting `mortality_alias`:** If the variable(s) is (or are) not present in the input dataset or not provided when the mortality switch is activated, an error will be returned. If the spelling does not match exactly, an error will be returned. Review available variables in the input dataset to confirm their existence and spelling. 

**Setting `mortality_value`:** If the value given is not in the variable provided for the `mortality_alias` or not provided when the mortality switch is activated, an error will be returned. If the spelling does not match exactly, an error will be returned. Review available variable levels in the input dataset to confirm their existence and spelling.

**Setting `voltage_alias`:** If the variable(s) is (or are) not present in the input dataset or not provided when the voltage switch is activated, an error will be returned. If the spelling does not match exactly, an error will be returned. Review available variables in the input dataset to confirm their existence and spelling. 

**Setting `gps_accuracy_alias`:** If the variable(s) is (or are) not present in the input dataset or not provided when the GPS accuracy switch is activated, an error will be returned. If the spelling does not match exactly, an error will be returned. Review available variables in the input dataset to confirm their existence and spelling. 

**Setting `gps_accuracy_value`:** If the value given is not in the variable provided for the `gps_accuracy_alias` or not provided when the GPS accuracy switch is activated, an error will be returned. If the spelling does not match exactly, an error will be returned. Review available variable levels in the input dataset to confirm their existence and spelling.
