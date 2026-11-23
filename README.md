# First Passage Time Segmentation

MoveApps

Github repository: https://github.com/movestore/First-Passage-Time-Segm

## Description
Calculates the first passage time (FPT) for a user-defined radius at each location of all tracks and classifies the locations into fast movement and slow movement/resting by a time threshold. Results are shown in an overview table of all tracks and, for a selected track, in an FPT plot, an interactive map and a plot that helps to choose the radius.

## Documentation
The first passage time (FPT) of a location is the time an animal needs to leave a circle of a given radius around that location (Fauchald & Tveraa 2003). Short FPTs indicate fast, directed movement; long FPTs indicate that the animal stays in an area, e.g. while resting, foraging or at a stopover site.

When the App opens, the FPT is calculated for every location of every track with the radius, time threshold and time unit set in the sidebar (default 30 km, 10 days). Each location is classified as:

- **fast movement**: FPT less than or equal to the threshold time;
- **slow movement**: FPT larger than the threshold time;
- **not classified (NA)**: the FPT cannot be calculated, which is the case at the start and end of a track, where the circle is never left, and for tracks that cannot be analysed (see Errors and null handling).

Changes of the settings take effect when `Apply Changes` is clicked; the FPT is then recalculated for all tracks.

All tracks are listed in an overview table with their first and last timestamp, number of locations, number of fast and slow locations, the percentage of slow locations and a note if the FPT could not be calculated. Clicking a row in the table shows that track in three tabs:

- **FPT plot**: FPT over time, with fast locations in red, slow locations in green and the threshold as a blue line. The legend is shown above the plot.
- **Map**: the track as a line with the fast (red) and slow (green) locations; clicking a location shows its time, FPT value and class. Base maps: Esri topographic map (default), Esri aerial imagery and OpenStreetMap.
- **Choose radius**: the variance of log(FPT) for a range of radii (Fauchald & Tveraa 2003), following the approach in *Analysing and Mapping of Animal Movement in R* (Kranstauber, Safi & Scharf). The radii are spaced evenly on a log scale and the radius axis is logarithmic, so that small scales are resolved as well as large ones. A peak in this curve indicates the spatial scale at which the animal concentrates its activity and can be used to choose the radius. Several peaks can indicate behaviour at several scales. The plot is calculated for the track selected in the table when `Calculate` is clicked. The highest variance (peak) is marked with a red line and its radius is written on the plot ("Peak at … m"); `Use peak as radius`, under the plot, copies it into the radius setting. If the highest variance lies at the smallest or largest radius, it is not a real peak, and a message under the plot asks to widen the range of radii. The radius currently used for the FPT is shown as a dashed blue line; the legend is shown above the plot. Note that for multi-year tracks of migratory animals the peak can reflect the movement within the breeding or wintering range rather than an area-restricted search.

If `Add FPT results to workflow data` is selected (default), the FPT values and classes of all tracks are added to the output data, so that they can be used in subsequent Apps. Otherwise the input data are passed on unchanged.

Fauchald, P. & Tveraa, T. 2003. Using first-passage time in the analysis of area-restricted search and habitat selection. Ecology 84(2): 282-288. https://doi.org/10.1890/0012-9658(2003)084[0282:UFPTIT]2.0.CO;2

### Application scope
#### Generality of App usability
This App was developed using data of migratory birds to identify stopover sites, but it can be used for any taxonomic group and on any spatial scale from metres upwards, as long as the radius and threshold time are adapted to the movement of the animals.

#### Required data properties
Each track needs at least 3 locations with unique timestamps. The App works best with regularly sampled data. The radius should be larger than the typical distance between consecutive locations and the location error; otherwise the FPT mostly reflects the sampling interval rather than behaviour.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
The following files can be downloaded optionally (to disk):

`FPT_LeafletMap.html`: by clicking the `Map (HTML)` button. Interactive map of the selected track with fast (red) and slow (green) locations.

`FPT_Map.png`: by clicking the `Map (PNG)` button. PNG image of the map of the selected track.

`FPT_values.csv`: by clicking the `FPT values (CSV)` button. FPT value and class of every location of all tracks.

`FPT_track_summary.csv`: by clicking the `Track summary (CSV)` button. Overview table of all tracks.

### Settings
**Settings**

`Radius parameter for First Passage Time (m)` (radius): radius of the circle for which the first passage times are calculated. Unit: metres. Default: 30000.

`Threshold time for migration/resting` (time_thr): locations with an FPT up to this time are classified as fast movement, above it as slow movement. Unit: as selected below. Default: 10.

`Unit of the threshold time` (thr_uni): time unit of the threshold and of the FPT values: seconds, hours or days. Default: days.

`Add FPT results to workflow data` (add_fpt_columns): if selected, the columns `fpt_value` and `fpt_behaviour` are added to the output data; if not, the input data are passed on unchanged. Default: selected.

`Apply Changes`: applies the settings above, including `Add FPT results to workflow data`, and recalculates the FPT for all tracks.

**Track table**

Click a row in the table to show that track in the FPT plot, the map and the plot "Choose radius". Default: the first track.

**Choose radius**

`Smallest radius (m)` (vlf_rmin): smallest radius for the variance of log(FPT). Default: 1000.

`Largest radius (m)` (vlf_rmax): largest radius for the variance of log(FPT). Default: 1000000.

`Number of radii` (vlf_n): number of radii, spaced evenly on a log scale between the smallest and largest radius (2 to 150). More radii give a more precise peak but take longer to calculate. Default: 50.

`Calculate`: calculates the variance of log(FPT) for the track selected in the table and shows it in the tab "Choose radius".

`Use peak as radius`: shown under the plot in the tab "Choose radius" after `Calculate`, when the curve has a peak. Copies the radius of the peak into `Radius parameter for First Passage Time (m)`; click `Apply Changes` to recalculate the FPT with it.

### Changes in output data
If `Add FPT results to workflow data` is selected (default), the App adds the columns `fpt_value` (first passage time in the selected unit) and `fpt_behaviour` (`fast`, `slow` or NA) to the input data. All other data remain unchanged. If it is not selected, the output data are the unchanged input data.

### Errors and null handling
**Settings:** invalid values (e.g. a radius of 0) show a red message under the button, and the last result is kept.

**Input data:** tracks with fewer than 3 locations or duplicated timestamps are not analysed; their FPT values are NA and the reason is shown in the table.

**All locations fast or slow:** the threshold does not fit the FPT values. Adapt the threshold or its unit, or choose another radius with "Choose radius".

**No peak in "Choose radius":** if the highest variance of log(FPT) lies at the smallest or largest radius, an orange message under the plot asks to widen the range of radii, and neither the peak value nor `Use peak as radius` is shown. If no variance can be calculated for the range, a red message is shown under the plot.

**Small plot area:** if the window is too small for a plot, the message "The plot area is too small. Please enlarge the window." is shown instead of the plot.

### Technical details
- The FPT is calculated per track with `fpt()` and `varlogfpt()` from the R package `adehabitatLT`.
- Each track is projected to an azimuthal equidistant projection centred on the track, so that the radius is a true distance in metres.
- The PNG download uses `webshot2`, which needs Chrome or Chromium.