# First Passage Time Segmentation

MoveApps

Github repository: https://github.com/movestore/First-Passage-Time-Segm

## Description
Calculates the first passage time (FPT) for a user-defined radius at each location of all tracks and classifies the locations into fast movement and slow movement/resting by a time threshold. Results are shown in an overview table of all tracks and, for a selected track, in an FPT plot, an interactive map and a plot that helps to choose the radius. Optionally, the FPT series of all tracks can be segmented into behavioural phases (e.g. resting, migration), which are shown in a plot, a table and on the map.

## Documentation
The first passage time (FPT) of a location is the time an animal needs to leave a circle of a given radius around that location (Fauchald & Tveraa 2003). Short FPTs indicate fast, directed movement; long FPTs indicate that the animal stays in an area, e.g. while resting, foraging or at a stopover site.

When the App opens, the FPT is calculated for every location of every track with the radius, time threshold and time unit set in the sidebar (default 30 km, 10 days). Each location is classified as:

- **fast movement**: FPT less than or equal to the threshold time;
- **slow movement**: FPT larger than the threshold time;
- **not classified (NA)**: the FPT cannot be calculated, which is the case at the start and end of a track, where the circle is never left, and for tracks that cannot be analysed (see Errors and null handling).

Changes of the settings take effect when `Apply Changes` is clicked; the FPT is then recalculated for all tracks.

All tracks are listed in an overview table with their first and last timestamp, number of locations, number of fast and slow locations, the percentage of slow locations and a note if the FPT could not be calculated. Clicking a row in the table shows that track in four tabs:

- **FPT plot**: FPT over time, with fast locations in red, slow locations in green and the threshold as a blue line. The legend is shown above the plot.
- **Map**: the track as a line with the fast (red) and slow (green) locations; clicking a location shows its time, FPT value and class. Base maps: Esri topographic map (default), Esri aerial imagery and OpenStreetMap. If segments were calculated and `Show segmentation on map` is selected, the layer "segments" shows each segment of the track in its own colour (as in the tab "Segments"); clicking a segment shows its start and end.
- **Choose radius**: the variance of log(FPT) for a range of radii (Fauchald & Tveraa 2003), following the approach in *Analysing and Mapping of Animal Movement in R* (Kranstauber, Safi & Scharf). The radii are spaced evenly on a log scale and the radius axis is logarithmic, so that small scales are resolved as well as large ones. A peak in this curve indicates the spatial scale at which the animal concentrates its activity and can be used to choose the radius. Several peaks can indicate behaviour at several scales. The plot is calculated for the track selected in the table when `Calculate` is clicked, and the tab "Choose radius" opens. The highest variance (peak) is marked with a red line and its radius is written on the plot ("Peak at … m"); `Copy the peak value to radius`, under the plot, copies it into the radius setting, and a message reminds to click `Apply Changes`. If the highest variance lies at the smallest or largest radius, it is not a real peak, and a message under the plot asks to widen the range of radii. The radius currently used for the FPT is shown as a dashed blue line; the legend is shown above the plot. Note that for multi-year tracks of migratory animals the peak can reflect the movement within the breeding or wintering range rather than an area-restricted search.
- **Segments**: the FPT series of each track is divided into phases with a constant typical FPT, following *Analysing and Mapping of Animal Movement in R* (Kranstauber, Safi & Scharf): log10(FPT) at the current radius is segmented with the penalised contrast method of Lavielle (2005), using the R package `segclust2d` (Patin et al. 2020). The segmentation is calculated for all tracks when `Segment` is clicked, and the tab "Segments" opens. For the selected track it shows the FPT over time coloured by segment, with the breaks as dashed lines and the median FPT of each segment as a thick line; the number of segments with the Lavielle criterion D (a value above 0.75 supports that number of segments); and a table with start, end, duration, number of locations, median FPT and class (fast/slow from the median and the threshold) of each segment. The number of segments is chosen automatically for each track by the criterion, or fixed by the user. Phases shorter than the minimum segment length are merged into neighbouring segments. Locations without FPT (at the start and end of a track) are not segmented. The segments refer to the radius used for the FPT; after `Apply Changes` they are removed and `Segment` has to be clicked again.

The FPT values, the classes and (after `Segment`) the segment numbers of all tracks can be added to the output data, so that they can be used in subsequent Apps. Each column is selected with its own checkbox under "Add columns to the return data" (none selected by default); changes take effect immediately. If no column is selected, which is the default, the input data are passed on unchanged.

Fauchald, P. & Tveraa, T. 2003. Using first-passage time in the analysis of area-restricted search and habitat selection. Ecology 84(2): 282-288. https://doi.org/10.1890/0012-9658(2003)084[0282:UFPTIT]2.0.CO;2

Lavielle, M. 2005. Using penalized contrasts for the change-point problem. Signal Processing 85(8): 1501-1510. https://doi.org/10.1016/j.sigpro.2005.01.012

Patin, R., Etienne, M.-P., Lebarbier, E., Chamaillé-Jammes, S. & Benhamou, S. 2020. Identifying stationary phases in multivariate time series for highlighting behavioural modes and home range settlements. Journal of Animal Ecology 89(1): 44-56. https://doi.org/10.1111/1365-2656.13105

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

`FPT_LeafletMap.html`: by clicking the `Map (HTML)` button. Interactive map of the selected track with fast (red) and slow (green) locations, including the segments if they are shown on the map.

`FPT_Map.png`: by clicking the `Map (PNG)` button. PNG image of the map of the selected track, including the segments if they are shown on the map.

`FPT_values.csv`: by clicking the `FPT values (CSV)` button. FPT value, class and segment number (`fpt_segment`, NA if not segmented) of every location of all tracks.

`FPT_track_summary.csv`: by clicking the `Track summary (CSV)` button. Overview table of all tracks.

`FPT_segments.csv`: by clicking the `Segments (CSV)` button. All segments of all tracks with start, end, duration, number of locations, median FPT and class, and the settings used (radius, unit, minimum segment length, number of segments). Before `Segment` was clicked, the file only contains a note.

### Settings
**Settings**

`Radius parameter for First Passage Time (m)` (radius): radius of the circle for which the first passage times are calculated. Unit: metres. Default: 30000.

`Threshold time for migration/resting` (time_thr): locations with an FPT up to this time are classified as fast movement, above it as slow movement. Unit: as selected below. Default: 10.

`Unit of the threshold time` (thr_uni): time unit of the threshold and of the FPT values: seconds, hours or days. Default: days.

`Apply Changes`: applies the settings above and recalculates the FPT for all tracks.

**Track table**

Click a row in the table to show that track in the FPT plot, the map and the plot "Choose radius". Default: the first track.

**Choose radius**

`Smallest radius (m)` (vlf_rmin): smallest radius for the variance of log(FPT). Default: 1000.

`Largest radius (m)` (vlf_rmax): largest radius for the variance of log(FPT). Default: 1000000.

`Number of radii` (vlf_n): number of radii, spaced evenly on a log scale between the smallest and largest radius (2 to 150). More radii give a more precise peak but take longer to calculate. Default: 50.

`Calculate`: calculates the variance of log(FPT) for the track selected in the table and shows it in the tab "Choose radius".

`Copy the peak value to radius`: shown under the plot in the tab "Choose radius" after `Calculate`, when the curve has a peak. Copies the radius of the peak into `Radius parameter for First Passage Time (m)`; a message under the button reminds to click `Apply Changes`, which recalculates the FPT with it.

**Segmentation**

`Number of segments` (seg_k_mode): automatic (chosen for each track by the Lavielle criterion) or fixed. Default: automatic.

`Fixed number of segments` (seg_k): shown if fixed is selected; number of segments of every track (at most the maximum number of segments). Default: 4.

`Maximum number of segments` (seg_kmax): largest number of segments considered (2 to 50). Default: 12.

`Minimum segment length (days)` (seg_lmin_days): shortest phase allowed; converted for each track into a number of locations from its median sampling interval. Shorter phases (e.g. a migration of a few days) are merged into neighbouring segments; lower it to detect short phases. Default: 7.

`Show segmentation on map` (seg_on_map): shows the segments of the selected track as a layer on the map. Default: not selected.

`Segment`: segments the FPT series of all tracks and opens the tab "Segments" (or stays on the map if the map is shown and `Show segmentation on map` is selected).

**Add columns to the return data**

`fpt_value (first passage time)` (add_fpt_value): adds the column `fpt_value` to the output data. Default: not selected.

`fpt_behaviour (fast / slow)` (add_fpt_behaviour): adds the column `fpt_behaviour` to the output data. Default: not selected.

`fpt_segment (segment number)` (add_fpt_segment): adds the column `fpt_segment` to the output data, only after `Segment` was clicked. If no segments exist (before `Segment` was clicked, or after `Apply Changes`), an orange message under the checkbox reminds to click `Segment`. Default: not selected.

The three columns are selected independently of each other, and changes take effect immediately, without clicking `Apply Changes` or `Segment`.

### Changes in output data
By default, the output data are the unchanged input data. Depending on the checkboxes under "Add columns to the return data" (none selected by default), the App adds the columns `fpt_value` (first passage time in the selected unit), `fpt_behaviour` (`fast`, `slow` or NA) and, after `Segment` was clicked, `fpt_segment` (segment number within the track; NA for locations without FPT and for tracks that could not be segmented) to the input data. All other data remain unchanged. If no column is selected, the output data are the unchanged input data.

If the input data already contain columns named `fpt_value`, `fpt_behaviour` or `fpt_segment` (e.g. when this App is used twice in a workflow), they are replaced by the new values; a warning is written to the log.

### Errors and null handling
**Settings:** invalid values (e.g. a radius of 0) show a red message under the button, and the last result is kept.

**Input data:** tracks with fewer than 3 locations or duplicated timestamps are not analysed; their FPT values are NA and the reason is shown in the table.

**All locations fast or slow:** the threshold does not fit the FPT values. Adapt the threshold or its unit, or choose another radius with "Choose radius".

**No peak in "Choose radius":** if the highest variance of log(FPT) lies at the smallest or largest radius, an orange message under the plot asks to widen the range of radii, and neither the peak value nor `Copy the peak value to radius` is shown. If no variance can be calculated for the range, a red message is shown under the plot.

**Segmentation:** a track is not segmented if it has no FPT values, if it is shorter than two minimum segment lengths, or if the segmentation fails; the reason is shown in the table of the tab "Segments" and in the log. Invalid segmentation settings show a red message under `Segment`.

**Small plot area:** if the window is too small for a plot, the message "The plot area is too small. Please enlarge the window." is shown instead of the plot.

### Technical details
- The FPT is calculated per track with `fpt()` and `varlogfpt()` from the R package `adehabitatLT`.
- Each track is projected to an azimuthal equidistant projection centred on the track, so that the radius is a true distance in metres.
- The PNG download uses `webshot2`, which needs Chrome or Chromium.
- The segmentation uses `segmentation()` (with `sameSigma = TRUE`, `subsample = FALSE`), `chooseseg_lavielle()` and `segment()` from the R package `segclust2d`.