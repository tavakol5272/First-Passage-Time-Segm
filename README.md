# First Passage Time Segmentation

MoveApps

Github repository: https://github.com/movestore/First-Passage-Time-Segm

## Description
This App calculates the first passage time (FPT) for every location of each track. Using a user-defined time threshold, each location is classified as fast movement (FPT below the threshold) or slow movement/resting (FPT equal to or above the threshold). Results are shown per track in a plot and an interactive map, and are added to the data as new columns.

## Documentation
The first passage time (FPT) of a location is the time an animal needs to cross a circle of a given radius around that location. Short FPTs indicate fast, directed movement (e.g. migration), long FPTs indicate slow movement or resting (e.g. stopover sites).

After clicking `Start/Update calculation`, the App calculates the FPT for each location of each track separately, using the radius set by the user. To measure the radius correctly in metres, each track is projected to an azimuthal equidistant projection centred on the middle of that track. The FPT is calculated with the `fpt()` function of the `adehabitatLT` package and is given in the time unit selected by the user.

Each location is then classified with the selected time threshold:
- `fast`: FPT is smaller than the threshold (fast movement);
- `slow`: FPT is equal to or larger than the threshold (slow movement/resting);
- `NA`: no FPT could be calculated for this location.

The App shows:
- a table of all tracks with their first and last timestamp and the number of fast, slow and NA locations. Use the radio buttons in the first column to select the track shown in the plot and map;
- a plot of the FPT values over time for the selected track, with fast locations in red, slow locations in dark green and the threshold as a blue line;
- an interactive map of the selected track with the track line, fast locations (red) and slow locations (dark green). An additional layer `FPT value` (hidden by default, can be switched on in the layer control) colours all locations by their FPT value. Three base maps can be selected: StreetMap, TopoMap and Aerial.

The plot can be downloaded as a png file and the map as an html file.

The plot, map, table and output data always refer to the settings used in the last click on `Start/Update calculation`. After changing a setting, click the button again to update the results.

The FPT value and behaviour class of each location are added to the output data in the columns `fpt_value` and `fpt_behaviour`, so that they can be used in subsequent Apps (e.g. for filtering, or writing csv files or shapefiles).

### Application scope
#### Generality of App usability
This App was developed using data of migratory birds, to distinguish migration from stopover/resting. It can be used for any taxonomic group, as long as the radius and threshold time are adapted to the scale of movement of the study animals.

#### Required data properties
The App should work for any kind of location data. The data should have at least 2 locations per track. The radius should be chosen in relation to the spatial extent and fix rate of the data: if the radius is too large compared with the movement of the animals, most FPT values will be NA; if it is too small compared with the distance between locations, FPT values will be very short.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
none

### Settings
`Radius parameter for First Passage Time (m)` (radius): Radius of the circle that the animal has to cross, used for the FPT calculation. Unit: `metres`. Default: 30000 (30 km).

`Threshold time for migration/resting` (time_thr): Time threshold that splits locations into fast movement (FPT below the threshold) and slow movement/resting (FPT equal to or above the threshold). Unit: as selected in `Unit of the threshold time`. Default: 10.

`Unit of the threshold time` (thr_uni): Time unit of the threshold time and of the calculated FPT values. Options: `seconds`, `hours`, `days`. Default: `days`.

`Start/Update calculation`: click to calculate the FPT with the current settings, and again after changing any of the settings above. A progress bar shows the calculation progress per track.

`Show Plots and Map`: radio buttons in the track table to select the track shown in the plot and map. By default the first track is shown.

`Save Plot as PNG`: click to download the FPT plot of the selected track as png file.

`Save Map as Html`: click to download the map of the selected track as html file.

`Store settings`: click to store the current settings of the App for future Workflow runs.
`Restore default settings`: click to restore to the default setting values of the App.

### Changes in output data
Before `Start/Update calculation` is clicked, the input data remains unchanged and is passed on as it is.

After the calculation, the App adds to the input data the columns:
- `fpt_value`: first passage time of the location, in the selected time unit. `NA` if no FPT could be calculated.
- `fpt_behaviour`: `fast` if `fpt_value` is smaller than the threshold time, `slow` if it is equal to or larger than the threshold time, `NA` if `fpt_value` is `NA`.

No locations are removed, and the order of the locations, the projection and the timestamps of the input data are kept.

### Errors and null handling
**Setting `Radius parameter for First Passage Time (m)` and `Threshold time for migration/resting`:** Both must be numbers larger than 0. If one of them is empty, 0 or negative, the calculation is not started, an error message is shown and the previous result (or the unchanged input data) is kept.

**Setting `Unit of the threshold time`:** Selection of three fixed values with a default; no null or error possibilities.

**Input data:** Tracks with fewer than 2 locations get `NA` for all FPT values. Locations without coordinates (empty locations) are kept in the data and get `NA`.

**Common error:** If no plot, map or table is shown, click `Start/Update calculation`. If many or all locations of a track are `NA`, the radius is probably too large for the data; try a smaller radius.

### Technical details
- The FPT calculation uses `adehabitatLT::as.ltraj()` and `adehabitatLT::fpt()`. The data are processed per track.
- Before the calculation, each track is projected with `sf` to an azimuthal equidistant projection (`+proj=aeqd`) centred on the geographic centroid of the track, so that distances are measured in metres with low distortion near the track centre. For very long tracks (thousands of kilometres), distances far from the centre are still slightly distorted.
- The projection is only used for the calculation; the output data keep the projection of the input data. For the map, the data are transformed to longitude/latitude (EPSG:4326).
- The interactive table uses `DT`, the map `leaflet` and `htmlwidgets`, and the setting tooltips `shinyBS`.
- Runtime increases with the number of locations and tracks.
