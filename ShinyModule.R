library("shiny")
library("move2")
library("sf")
library("leaflet")
library("shinyBS")
library("webshot2")
library(segclust2d)

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

## helper functions ##

### Helper 1
# check the FPT settings; returns a message if they are not valid, otherwise NULL
checkSettings <- function(radius, thr) {
  if (!isTRUE(is.finite(radius) && radius > 0)) return("Please enter a radius larger than 0 m.")
  if (!isTRUE(is.finite(thr) && thr >= 0)) return("Please enter a threshold time of 0 or larger.")
  NULL
}


### Helper 2
# check the range of radii for the variance of log(FPT); returns a message if not valid, otherwise NULL
checkRadii <- function(rmin, rmax, n) {
  if (!isTRUE(is.finite(rmin) && rmin > 0)) return("Please enter a smallest radius larger than 0 m.")
  if (!isTRUE(is.finite(rmax) && rmax > rmin)) return("The largest radius must be larger than the smallest radius.")
  if (!isTRUE(is.finite(n) && n >= 2 && n <= 150)) return("The number of radii must be between 2 and 150.")
  NULL
}

### Helper 3
# fast: FPT <= threshold, slow: FPT > threshold, NA: FPT could not be calculated
classifyFPT <- function(fptv, thr) {
  ifelse(is.na(fptv), NA_character_, ifelse(fptv <= thr, "fast", "slow"))
}

formatTime <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")


### Helper 4 :metric projection for the FPT calculation.
trackLtraj <- function(trk) {
  
  if (is.na(sf::st_crs(trk))) {
    return(list(problem = "no coordinate reference system"))
  }
  
  ok <- which(!sf::st_is_empty(trk))
  
  if (length(ok) < 3) {
    return(list(problem = "fewer than 3 locations"))
  }
  
  tms <- mt_time(trk)[ok]
  
  if (anyDuplicated(tms) > 0) {
    return(list(problem = "duplicated timestamps"))
  }
  
  ok <- ok[order(tms)]
  
  # trk is in longitude/latitude coordinates:
  # either already geographic or transformed to EPSG:4326 in data_ll
  ll <- sf::st_coordinates(trk[ok, ])
  
  # local metric projection centred on this track
  aeqd <- sprintf(
    "+proj=aeqd +lat_0=%.6f +lon_0=%.6f +units=m",
    mean(ll[, 2]),
    mean(ll[, 1])
  )
  
  # convert longitude/latitude to x/y in metres
  trk_m <- sf::st_transform(trk[ok, ], aeqd)
  xy <- sf::st_coordinates(trk_m)
  
  ltr <- adehabitatLT::as.ltraj(
    xy = data.frame(x = xy[, 1], y = xy[, 2]),
    date = as.POSIXct(mt_time(trk)[ok]),
    id = "track",
    typeII = TRUE,
    proj4string = sp::CRS(aeqd)
  )
  
  list(
    ltraj = ltr,
    rows = ok,
    problem = NA_character_
  )
}

### Helper 5
# add base maps and the layer control; osm = FALSE for the map saved as HTML,
# because OpenStreetMap blocks tiles requested from a file opened from disk
addBaseMaps <- function(map, overlayGroups = NULL, osm = TRUE) {
  map <- map %>%
    addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Aerial")
  if (osm) map <- addProviderTiles(map, "OpenStreetMap", group = "OpenStreetMap")
  addLayersControl(map,
                   baseGroups = c("TopoMap", "Aerial", if (osm) "OpenStreetMap"),
                   overlayGroups = overlayGroups,
                   options = layersControlOptions(collapsed = FALSE))
}

### Helper 6
# TRUE if the current plot device is large enough for the margins set with par(mar);
# the browser can report a very small plot size for a moment, e.g. while the window
# is moved to another monitor, and plot() would then fail with "figure margins too large"
plotFits <- function(min_inch = 0.5) {
  mai <- par("mai")
  inner <- par("din") - c(mai[2] + mai[4], mai[1] + mai[3])
  all(inner > min_inch)
}

### Helper 7
# check the segmentation settings; returns a message if they are not valid, otherwise NULL
checkSegSettings <- function(minDays, Kmax, K = NULL) {
  if (!isTRUE(is.finite(minDays) && minDays > 0)) return("Please enter a minimum segment length larger than 0 days.")
  if (!isTRUE(is.finite(Kmax) && Kmax >= 2 && Kmax <= 50)) return("The maximum number of segments must be between 2 and 50.")
  if (!is.null(K) && !isTRUE(is.finite(K) && K >= 1 && K <= Kmax)) return("The fixed number of segments must be between 1 and the maximum number of segments.")
  NULL
}

### Helper 8
# segment the log10 FPT series of one track into phases (Lavielle's penalised contrast, package
# segclust2d), as in "Segmenting first passage time" of Analysing and Mapping of Animal Movement in R;
# returns the segments, or a problem message
segmentTrack <- function(fptv, tms, minDays, Kmax, K = NULL) {
  keep <- which(!is.na(fptv) & fptv > 0)
  if (length(keep) < 2) return(list(problem = "no first passage times"))
  ord <- keep[order(tms[keep])]
  # minimum segment length in locations, from the median sampling interval of this track
  dt <- median(as.numeric(diff(tms[ord]), units = "days"))
  lmin <- max(2, ceiling(minDays / dt))
  Kmax <- min(Kmax, floor(length(ord) / lmin))
  if (Kmax < 2) return(list(problem = paste0("too short for segments of at least ", minDays, " days")))
  seg <- tryCatch(suppressMessages(suppressWarnings(
    segclust2d::segmentation(data.frame(logFpt = log10(fptv[ord])), seg.var = "logFpt",
                             lmin = lmin, Kmax = Kmax, subsample = FALSE, sameSigma = TRUE))),
    error = function(e) e)
  if (inherits(seg, "error")) return(list(problem = paste("segmentation failed:", conditionMessage(seg))))
  D <- segclust2d::chooseseg_lavielle(seg$likelihood$likelihood)$lavielle
  Kauto <- seg$Kopt.lavielle
  nseg <- if (is.null(K)) Kauto else min(K, Kmax)
  # segment() prints "User-selected number of segment" also for the automatic choice: keep it out of the log
  path <- suppressMessages(segclust2d::segment(seg, nseg = nseg))
  list(rows = ord, begin = path$begin, end = path$end, nseg = nseg, Kauto = Kauto, lmin = lmin,
       criterion = data.frame(K = seq_along(D) + 1, D = D), problem = NA_character_)
}

### Helper 9
# colours of the segments, the same in the plot "Segments" and on the map
segColours <- function(n) grDevices::hcl.colors(max(n, 2), "Dark 3")[seq_len(n)]


####### shiny Interface
shinyModuleUserInterface <- function(id, label) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)
  
  tagList(
    # tab names a bit bigger and tabs wider (default: 14px text, 15px padding left and right)
    # (the tabs above the plots are the only tabs of the App; !important overrides the Bootstrap defaults)
    tags$head(tags$style(".nav-tabs > li > a, .nav-tabs .nav-link { font-size: 17px !important; padding: 10px 28px !important; }")),
    titlePanel("First Passage Time Segmentation"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Settings"),
        numericInput(ns("radius"), label = "Radius parameter for First Passage Time (m)", value = 30000, min = 1),
        bsTooltip(id = ns("radius"), title = "Define the radius for which you want to calculate the first passage times (when does an animal pass the radius). Unit = m.", placement = "bottom", trigger = "hover", options = list(container = "body")),
        numericInput(ns("time_thr"), label = "Threshold time for migration/resting", value = 10, min = 0),
        bsTooltip(id = ns("time_thr"), title = "Define the time threshold with which you will split your tracks into fast and slow movement locations. Locations with a first passage time up to this threshold are fast, above it slow (resting). Select your time unit below.", placement = "bottom", trigger = "hover", options = list(container = "body")),
        selectInput(ns("thr_uni"), label = "Unit of the threshold time", choices = c("seconds", "hours", "days"), selected = "days"),
        bsTooltip(id = ns("thr_uni"), title = "Select the time unit of your selected threshold time. First passage times are given in this unit.", placement = "bottom", trigger = "hover", options = list(container = "body")),
        actionButton(ns("goButton"), "Apply Changes", class = "btn-primary btn-block"),
        uiOutput(ns("settings_msg")),
        hr(),
        h4("Choose radius"),
        numericInput(ns("vlf_rmin"), label = "Smallest radius (m)", value = 1000, min = 1),
        numericInput(ns("vlf_rmax"), label = "Largest radius (m)", value = 1000000, min = 1),
        numericInput(ns("vlf_n"), label = "Number of radii", value = 50, min = 2, max = 150),
        bsTooltip(id = ns("vlf_n"), title = "The variance of log(FPT) is calculated for this many radii, spaced evenly on a log scale between the smallest and largest radius, for the track selected in the table. The highest variance (peak) indicates the spatial scale at which the animal concentrates its search effort.", placement = "bottom", trigger = "hover", options = list(container = "body")),
        actionButton(ns("vlf_go"), "Calculate", class = "btn-primary btn-block"),
        uiOutput(ns("vlf_msg")),
        hr(),
        h4("Segmentation"),
        selectInput(ns("seg_k_mode"), "Number of segments", choices = c("automatic", "fixed"), selected = "automatic"),
        bsTooltip(id = ns("seg_k_mode"), title = "automatic: the number of segments is chosen for each track by the criterion of Lavielle (2005). fixed: every track is divided into the number of segments set below.", placement = "bottom", trigger = "hover", options = list(container = "body")),
        conditionalPanel("input.seg_k_mode == 'fixed'", ns = ns,
                         numericInput(ns("seg_k"), "Fixed number of segments", value = 4, min = 1, max = 50)),
        numericInput(ns("seg_kmax"), "Maximum number of segments", value = 12, min = 2, max = 50),
        numericInput(ns("seg_lmin_days"), "Minimum segment length (days)", value = 7, min = 0.01),
        bsTooltip(id = ns("seg_lmin_days"), title = "Shortest phase allowed. It is converted for each track into a number of locations from its median sampling interval.", placement = "bottom", trigger = "hover", options = list(container = "body")),
        checkboxInput(ns("seg_on_map"), "Show segmentation on map", value = FALSE),
        actionButton(ns("seg_go"), "Segment", class = "btn-primary btn-block"),
        uiOutput(ns("seg_msg")),
        hr(),
        h4("Add columns to the return data"),
        # each column on its own; takes effect immediately
        checkboxInput(ns("add_fpt_value"), "fpt_value (first passage time)", value = FALSE),
        checkboxInput(ns("add_fpt_behaviour"), "fpt_behaviour (fast / slow)", value = FALSE),
        checkboxInput(ns("add_fpt_segment"), "fpt_segment (segment number)", value = FALSE),
        uiOutput(ns("seg_col_msg")),   # reminder to click "Segment" while there are no segments
        bsTooltip(id = ns("add_fpt_segment"), title = "Only added after 'Segment' was clicked.", placement = "bottom", trigger = "hover", options = list(container = "body")),
        hr(),
        h4("Download"),
        downloadButton(ns("saveMap"), "Map (HTML)", class = "btn-sm"),
        downloadButton(ns("saveMapPng"), "Map (PNG)", class = "btn-sm"),
        downloadButton(ns("saveCsv"), "FPT values (CSV)", class = "btn-sm"),
        downloadButton(ns("saveSummary"), "Track summary (CSV)", class = "btn-sm"),
        downloadButton(ns("saveSegments"), "Segments (CSV)", class = "btn-sm")
      ),
      mainPanel(
        width = 9,
        h4("Tracks"),
        helpText("Click a row to show that track in the plot and map."),
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("track_table"))),
        tabsetPanel(id = ns("main_tabs"),
          tabPanel("FPT plot", shinycssloaders::withSpinner(plotOutput(ns("fpt"), height = "60vh"))),
          tabPanel("Map", shinycssloaders::withSpinner(leafletOutput(ns("leafmap"), height = "60vh"))),
          tabPanel("Choose radius",
                   shinycssloaders::withSpinner(plotOutput(ns("vlf_plot"), height = "60vh")),
                   # "Copy the peak value to radius" or a message, directly under the plot
                   uiOutput(ns("vlf_peak"))),
          tabPanel("Segments",
                   shinycssloaders::withSpinner(plotOutput(ns("seg_plot"), height = "60vh")),
                   uiOutput(ns("seg_criterion")),
                   DT::dataTableOutput(ns("seg_table")))
        )
      )
    )
  )
}

# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  
  # longitude/latitude working copy;
  # keep the original workflow data and CRS unchanged
  data_ll <- if (
    is.na(sf::st_crs(data)) ||
    isTRUE(sf::st_is_longlat(data))
  ) {
    data
  } else {
    sf::st_transform(data, 4326)
  }
  
  results <- reactiveVal(NULL)     # annotated data and per-track overview of the last calculation
  settingsMsg <- reactiveVal(NULL)
  vlfRes <- reactiveVal(NULL)
  vlfMsg <- reactiveVal(NULL)
  vlfCopied <- reactiveVal(NULL)   # peak copied to the radius setting, until "Apply Changes" is clicked
  segRes <- reactiveVal(NULL)      # segments of all tracks of the last click on "Segment"
  segMsg <- reactiveVal(NULL)
  
  ## FPT calculation: runs once at App start and on every click of "Apply Changes" ##
  observeEvent(input$goButton, {
    msg <- checkSettings(input$radius, input$time_thr)
    if (!is.null(msg)) {
      settingsMsg(msg)
      logger.info(msg)
      return()
    }
    settingsMsg(NULL)
    radius <- input$radius
    thr <- input$time_thr
    unit <- input$thr_uni
    
    trackRows <- split(seq_len(nrow(data)), mt_track_id(data), drop = TRUE)
    fptv <- rep(NA_real_, nrow(data))
    problems <- setNames(rep(NA_character_, length(trackRows)), names(trackRows))
    
    withProgress(message = "Calculating first passage times", value = 0, {
      for (tid in names(trackRows)) {
        rows <- trackRows[[tid]]
        lt <- trackLtraj(data_ll[rows, ])
        if (is.null(lt$ltraj)) {
          problems[tid] <- lt$problem
          logger.warn(paste0("First passage time could not be calculated for track ", tid, ": ", lt$problem, ". Its fpt_value and fpt_behaviour are NA."))
        } else {
          f <- adehabitatLT::fpt(lt$ltraj, radii = radius, units = unit)
          fptv[rows[lt$rows]] <- f[[1]][, 1]
        }
        incProgress(1 / length(trackRows), detail = tid)
      }
    })
    
    annotated <- data
    annotated$fpt_value <- fptv
    annotated$fpt_behaviour <- classifyFPT(fptv, thr)
    
    overview <- do.call(rbind, lapply(names(trackRows), function(tid) {
      rows <- trackRows[[tid]]
      tms <- mt_time(data)[rows]
      cls <- annotated$fpt_behaviour[rows]
      nfast <- sum(cls %in% "fast")
      nslow <- sum(cls %in% "slow")
      data.frame("Track" = tid,
                 "First timestamp (UTC)" = formatTime(min(tms)),
                 "Last timestamp (UTC)" = formatTime(max(tms)),
                 "Locations" = length(rows),
                 "Fast" = nfast,
                 "Slow" = nslow,
                 "% slow" = if (nfast + nslow > 0) round(100 * nslow / (nfast + nslow), 1) else NA,
                 "Note" = ifelse(is.na(problems[tid]), "", paste("not calculated:", problems[tid])),
                 check.names = FALSE)
    }))
    
    logger.info(paste0("First passage times calculated for ", length(trackRows), " track(s) with radius ", radius, " m and threshold ", thr, " ", unit, "."))
    results(list(data = annotated, trackRows = trackRows, overview = overview,
                 radius = radius, thr = thr, unit = unit))
  }, ignoreNULL = FALSE)
  
  output$settings_msg <- renderUI({
    if (!is.null(settingsMsg())) p(settingsMsg(), style = "color: red;")
  })
  
  ## track table and selection ##
  output$track_table <- DT::renderDataTable({
    res <- results()
    req(res)
    # keep a restored or previous selection if it is still valid
    sel <- isolate(input$track_table_rows_selected)
    if (!(length(sel) == 1 && sel >= 1 && sel <= nrow(res$overview))) sel <- 1
    DT::datatable(res$overview, rownames = FALSE,
                  selection = list(mode = "single", selected = sel, target = "row"),
                  options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = 200, scrollCollapse = TRUE))
  })
  
  # index of the selected track; a reactiveVal only invalidates the plots and the map when the
  # value really changes, so the initial row selection sent by the table (row 1, the track
  # already shown) does not draw them a second time
  selectedIdx <- reactiveVal(1)
  observe({
    res <- results()
    req(res)
    i <- input$track_table_rows_selected
    selectedIdx(if (length(i) == 1 && i >= 1 && i <= nrow(res$overview)) i else 1)
  })
  
  ## FPT plot of the selected track ##
  output$fpt <- renderPlot({
    res <- results()
    req(res)
    i <- selectedIdx()
    tid <- res$overview$Track[i]
    trk <- res$data[res$trackRows[[i]], ]
    validate(need(any(!is.na(trk$fpt_value)), paste0("No first passage times could be calculated for track ", tid, ".")))
    ord <- order(mt_time(trk))
    tms <- mt_time(trk)[ord]
    v <- trk$fpt_value[ord]
    cls <- trk$fpt_behaviour[ord]
    par(mar = c(5.1, 4.1, 6, 2.1))   # extra space at the top for the legend
    validate(need(plotFits(), "The plot area is too small. Please enlarge the window."))
    plot(tms, v, type = "l", main = "", ylab = paste0("FPT (", res$unit, ")"), xlab = "time")
    title(main = tid, line = 3.5)    # title above the legend
    points(tms[cls %in% "fast"], v[cls %in% "fast"], cex = 2, pch = 20, col = "red")
    points(tms[cls %in% "slow"], v[cls %in% "slow"], cex = 2, pch = 20, col = "darkgreen")
    abline(h = res$thr, col = "blue", lwd = 2)
    # legend directly above the plot area, between the title and the plot
    usr <- par("usr")
    legend(x = mean(usr[1:2]), y = usr[4], xjust = 0.5, yjust = 0, xpd = TRUE,
           horiz = TRUE, bty = "n", cex = 1.2, text.font = 2,
           legend = c("fast movement", "slow movement", "threshold"),
           col = c("red", "darkgreen", "blue"), pch = c(20, 20, NA), lty = c(NA, NA, 1), lwd = c(NA, NA, 2))
  }, execOnResize = TRUE)   # redraw on resize (e.g. moving to another monitor) instead of replaying
  
  ## map of the selected track ##
  mapLayers <- reactive({
    res <- results()
    req(res)
    i <- selectedIdx()
    tid <- res$overview$Track[i]
    # rows of the selected track with a location, in time order
    keep <- res$trackRows[[i]]
    keep <- keep[!sf::st_is_empty(res$data[keep, ])]
    validate(need(length(keep) > 0, paste0("Track ", tid, " has no locations to show.")))
    keep <- keep[order(mt_time(res$data)[keep])]
    trk_ll <- sf::st_transform(res$data[keep, ], 4326)
    xy <- sf::st_coordinates(trk_ll)
    cls <- trk_ll$fpt_behaviour
    segNo <- if (isTRUE(input$seg_on_map)) segColumn()[keep] else rep(NA_integer_, length(keep))
    popup <- paste0(sprintf("<b>Time (UTC):</b> %s<br><b>FPT:</b> %s %s<br><b>Class:</b> %s",
                            formatTime(mt_time(trk_ll)), round(trk_ll$fpt_value, 2), res$unit,
                            ifelse(is.na(cls), "not classified", cls)),
                    ifelse(is.na(segNo), "", paste0("<br><b>Segment:</b> ", segNo)))
    fast <- which(cls %in% "fast")
    slow <- which(cls %in% "slow")
    bounds <- as.vector(sf::st_bbox(trk_ll))
    
    outl <- leaflet() %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addPolylines(lng = xy[, 1], lat = xy[, 2], color = "blue", weight = 2, group = "lines")
    if (length(fast) > 0) {
      outl <- addCircles(outl, lng = xy[fast, 1], lat = xy[fast, 2], popup = popup[fast],
                         fillOpacity = 0.3, opacity = 0.5, color = "red", group = "fast movement")
    }
    if (length(slow) > 0) {
      outl <- addCircles(outl, lng = xy[slow, 1], lat = xy[slow, 2], popup = popup[slow],
                         fillOpacity = 0.3, opacity = 0.5, color = "darkgreen", group = "slow movement")
    }
    # segments of the selected track as thick lines, one colour per segment (as in the plot "Segments"),
    # only while "Show segmentation on map" is ticked
    sr <- segRes()
    s <- if (!is.null(sr) && isTRUE(input$seg_on_map)) sr$tracks[[i]] else NULL
    if (!is.null(s) && is.na(s$problem)) {
      cols <- segColours(s$nseg)
      for (k in seq_len(s$nseg)) {
        r <- s$rows[s$begin[k]:s$end[k]]            # rows of this segment, in time order
        r <- r[!sf::st_is_empty(res$data[r, ])]
        if (length(r) == 0) next
        g <- sf::st_coordinates(sf::st_transform(res$data[r, ], 4326))
        outl <- addPolylines(outl, lng = g[, 1], lat = g[, 2], color = cols[k], weight = 4, opacity = 0.9,
                             group = "segments",
                             popup = paste0("<b>Segment ", k, "</b><br>", formatTime(min(mt_time(res$data)[r])),
                                            " to ", formatTime(max(mt_time(res$data)[r]))))
      }
      outl <- addLegend(outl, position = "bottomright", colors = cols, labels = paste("segment", seq_len(s$nseg)),
                        opacity = 0.9, title = "segments", group = "segments")
    }
    outl %>%
      addLegend(position = "topright", colors = c("blue", "red", "darkgreen"),
                labels = c("lines", "fast movement", "slow movement"), opacity = 0.7, title = paste("track", tid)) %>%
      addScaleBar(position = "bottomleft",
                  options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE))
  })
  
  
  output$leafmap <- renderLeaflet({
    addBaseMaps(mapLayers(), mapGroups())
  })
  
  ## variance of log(FPT) over a range of radii for the selected track ##
  observeEvent(input$vlf_go, {
    res <- results()
    req(res)
    msg <- checkRadii(input$vlf_rmin, input$vlf_rmax, input$vlf_n)
    if (!is.null(msg)) {
      vlfMsg(msg)
      logger.info(msg)
      return()
    }
    vlfMsg(NULL)
    # show the result: open the tab "Choose radius"
    updateTabsetPanel(session, "main_tabs", selected = "Choose radius")
    i <- selectedIdx()
    tid <- res$overview$Track[i]
    lt <- trackLtraj(data_ll[res$trackRows[[i]], ])
    if (is.null(lt$ltraj)) {
      vlfRes(list(track = tid, problem = lt$problem))
      return()
    }
    # radii evenly spaced on a log scale: more radii at small scales, where the detail is
    radii <- exp(seq(log(input$vlf_rmin), log(input$vlf_rmax), length.out = round(input$vlf_n)))
    withProgress(message = "Calculating variance of log(FPT)", value = 0.5, {
      # the variance of log(FPT) does not depend on the time unit
      f <- adehabitatLT::fpt(lt$ltraj, radii = radii, units = "seconds")
      v <- adehabitatLT::varlogfpt(f, graph = FALSE)
    })
    vv <- as.numeric(v[1, ])
    ok <- which(!is.na(vv))
    peak <- if (length(ok) > 0) which.max(vv) else NA_integer_
    # a maximum at the first or last radius with a value is not a real peak: the range is too narrow
    atEdge <- !is.na(peak) && peak %in% range(ok)
    vlfCopied(NULL)   # a new curve: no copied peak yet
    vlfRes(list(track = tid, radii = radii, var = vv, peak = peak, atEdge = atEdge, problem = NA_character_))
  })

  output$vlf_peak <- renderUI({
    vr <- vlfRes()
    req(vr, is.na(vr$problem))
    if (is.na(vr$peak)) {
      p("No variance of log(FPT) could be calculated for this range of radii.", style = "color: red;")
    } else if (vr$atEdge) {
      p(paste0("The highest variance is at the ", if (vr$peak == min(which(!is.na(vr$var)))) "smallest" else "largest",
               " radius (", format(round(vr$radii[vr$peak]), big.mark = ","), " m). ",
               "This is not a real peak: please widen the range of radii."), style = "color: #d9822b;")
    } else {
      # the value of the peak is written on the plot
      copied <- vlfCopied()
      div(style = "text-align: center;",
          actionButton(ns("vlf_use"), "Copy the peak value to radius", class = "btn-primary btn-sm"),
          if (!is.null(copied)) {
            p(paste0("The peak value (", format(copied, big.mark = ","), " m) was copied to ",
                     "'Radius parameter for First Passage Time (m)'. Click 'Apply Changes' to recalculate ",
                     "the first passage times with this radius."),
              style = "color: #d9822b; margin-top: 0.5em;")
          })
    }
  })

  # copy the peak into the radius setting; "Apply Changes" then recalculates the FPT
  observeEvent(input$vlf_use, {
    vr <- vlfRes()
    req(vr, !is.na(vr$peak), !vr$atEdge)
    value <- round(vr$radii[vr$peak])
    updateNumericInput(session, "radius", value = value)
    vlfCopied(value)   # shows the reminder to click "Apply Changes"
  })
  
  output$vlf_msg <- renderUI({
    if (!is.null(vlfMsg())) p(vlfMsg(), style = "color: red;")
  })
  
  output$vlf_plot <- renderPlot({
    vr <- vlfRes()
    validate(need(!is.null(vr), "Set the range of radii in the sidebar and click 'Calculate' to compute the variance of log(FPT) for the track selected in the table."))
    validate(need(is.na(vr$problem), paste0("The variance of log(FPT) could not be calculated for track ", vr$track, ": ", vr$problem, ".")))
    res <- results()
    par(mar = c(5.1, 4.1, 6, 2.1))   # extra space at the top for the legend
    validate(need(plotFits(), "The plot area is too small. Please enlarge the window."))
    plot(vr$radii, vr$var, type = "b", pch = 20, log = "x", main = "",
         xlab = "radius (m, log scale)", ylab = "variance of log(FPT)")
    title(main = paste("Track", vr$track), line = 3.5)   # title above the legend
    if (!is.na(vr$peak)) {
      abline(v = vr$radii[vr$peak], col = "red", lwd = 2)
      points(vr$radii[vr$peak], vr$var[vr$peak], pch = 19, cex = 2, col = "red")
    }
    if (!is.na(vr$peak) && !vr$atEdge) {
      usr <- par("usr")
      xp <- vr$radii[vr$peak]
      # label at the bottom of the red line; to its right, or to its left if the peak is in the right half
      right <- log10(xp) < mean(usr[1:2])
      text(x = xp, y = usr[3] + 0.04 * diff(usr[3:4]),
           labels = paste0("Peak at ", format(round(xp), big.mark = ","), " m"),
           pos = if (right) 4 else 2, col = "red", font = 2, cex = 1.2)
    }
    if (!is.null(res)) abline(v = res$radius, col = "blue", lty = 2)
    # legend directly above the plot area, between the title and the plot;
    # on the log axis par("usr") holds log10 values, hence 10^
    usr <- par("usr")
    legend(x = 10^mean(usr[1:2]), y = usr[4], xjust = 0.5, yjust = 0, xpd = TRUE,
           horiz = TRUE, bty = "n", cex = 1.2, text.font = 2,
           legend = c("peak", "current radius"), col = c("red", "blue"), lty = c(1, 2), lwd = c(2, 1))
  }, execOnResize = TRUE)   # redraw on resize (e.g. moving to another monitor) instead of replaying

  ## segmentation of the FPT series of all tracks into phases ##
  # segments belong to the FPT values they were calculated from: clear them after "Apply Changes"
  observeEvent(results(), {
    segRes(NULL)
    segMsg(NULL)
    vlfCopied(NULL)   # "Apply Changes" was clicked: the reminder under "Copy the peak value to radius" is done
  })

  observeEvent(input$seg_go, {
    res <- results()
    req(res)
    fixed <- identical(input$seg_k_mode, "fixed")
    msg <- checkSegSettings(input$seg_lmin_days, input$seg_kmax, if (fixed) input$seg_k else NULL)
    if (!is.null(msg)) {
      segMsg(msg)
      logger.info(msg)
      return()
    }
    segMsg(NULL)
    # show the result: open the tab "Segments", unless the user is looking at the map
    # with "Show segmentation on map" ticked
    if (!(identical(input$main_tabs, "Map") && isTRUE(input$seg_on_map))) {
      updateTabsetPanel(session, "main_tabs", selected = "Segments")
    }
    minDays <- input$seg_lmin_days
    Kmax <- round(input$seg_kmax)
    K <- if (fixed) round(input$seg_k) else NULL
    tms <- mt_time(res$data)
    tids <- names(res$trackRows)
    tracks <- vector("list", length(tids))
    withProgress(message = "Segmenting first passage times", value = 0, {
      for (j in seq_along(tids)) {
        rows <- res$trackRows[[j]]
        s <- segmentTrack(res$data$fpt_value[rows], tms[rows], minDays, Kmax, K)
        if (is.na(s$problem)) {
          s$rows <- rows[s$rows]   # back to row numbers of the data
        } else {
          logger.warn(paste0("Track ", tids[j], " was not segmented: ", s$problem, "."))
        }
        tracks[[j]] <- s
        incProgress(1 / length(tids), detail = tids[j])
      }
    })

    # one row per segment; one row with a note for a track that was not segmented
    segRow <- function(tid, i = NA, from = NA, to = NA, n = NA, med = NA, note = "") {
      data.frame("Track" = tid,
                 "Segment" = i,
                 "From (UTC)" = if (is.na(i)) NA else formatTime(from),
                 "To (UTC)" = if (is.na(i)) NA else formatTime(to),
                 "Duration (days)" = if (is.na(i)) NA else round(as.numeric(difftime(to, from, units = "days")), 1),
                 "Locations" = n,
                 "Median FPT" = if (is.na(i)) NA else round(med, 2),
                 "Class" = if (is.na(i)) NA else classifyFPT(med, res$thr),
                 "Note" = note,
                 check.names = FALSE)
    }
    table <- do.call(rbind, lapply(seq_along(tids), function(j) {
      s <- tracks[[j]]
      if (!is.na(s$problem)) return(segRow(tids[j], note = paste("not segmented:", s$problem)))
      do.call(rbind, lapply(seq_len(s$nseg), function(i) {
        r <- s$rows[s$begin[i]:s$end[i]]
        segRow(tids[j], i, min(tms[r]), max(tms[r]), length(r), 10^median(log10(res$data$fpt_value[r])))
      }))
    }))
    names(table)[names(table) == "Median FPT"] <- paste0("Median FPT (", res$unit, ")")

    nok <- sum(vapply(tracks, function(s) is.na(s$problem), logical(1)))
    logger.info(paste0("Segmented ", nok, " of ", length(tids), " track(s) with minimum segment length ", minDays,
                       " days and ", if (fixed) paste(K, "segments") else "automatic number of segments", "."))
    segRes(list(tracks = tracks, table = table, unit = res$unit, radius = res$radius,
                minDays = minDays, mode = if (fixed) paste("fixed:", K) else "automatic"))
  })

  # segment number of every location; NA where the location has no FPT, its track was not
  # segmented or "Segment" was not clicked yet
  segColumn <- reactive({
    sr <- segRes()
    res <- results()
    req(res)
    col <- rep(NA_integer_, nrow(res$data))
    if (!is.null(sr)) for (s in sr$tracks) {
      if (is.na(s$problem)) col[s$rows] <- findInterval(seq_along(s$rows), s$begin)
    }
    col
  })

  # overlay groups of the map; "segments" only if "Show segmentation on map" is ticked
  # and the selected track was segmented
  mapGroups <- reactive({
    sr <- segRes()
    s <- if (!is.null(sr) && isTRUE(input$seg_on_map)) sr$tracks[[selectedIdx()]] else NULL
    c("lines", "fast movement", "slow movement", if (!is.null(s) && is.na(s$problem)) "segments")
  })

  # under the checkbox fpt_segment: it is ticked, but there are no segments (not segmented yet,
  # or removed by "Apply Changes")
  output$seg_col_msg <- renderUI({
    if (isTRUE(input$add_fpt_segment) && is.null(segRes())) {
      p("fpt_segment is not added yet: click 'Segment' first.", style = "color: #d9822b; margin-top: -0.5em;")
    }
  })

  output$seg_msg <- renderUI({
    if (!is.null(segMsg())) p(segMsg(), style = "color: red;")
  })

  output$seg_plot <- renderPlot({
    sr <- segRes()
    validate(need(!is.null(sr), "Set the segmentation settings in the sidebar and click 'Segment' to divide the first passage times of all tracks into phases."))
    res <- results()
    req(res)
    i <- selectedIdx()
    tid <- res$overview$Track[i]
    s <- sr$tracks[[i]]
    validate(need(is.na(s$problem), paste0("Track ", tid, " was not segmented: ", s$problem, ".")))
    tms <- mt_time(res$data)[s$rows]
    v <- res$data$fpt_value[s$rows]
    segOf <- findInterval(seq_along(v), s$begin)   # segment of each location
    cols <- segColours(s$nseg)
    par(mar = c(5.1, 4.1, 6, 2.1))   # extra space at the top for the legend
    validate(need(plotFits(), "The plot area is too small. Please enlarge the window."))
    plot(tms, v, type = "n", log = "y", main = "", xlab = "time", yaxt = "n",
         ylab = paste0("FPT (", sr$unit, ", log scale)"))
    # y axis labels as plain numbers (0.05, 0.5, 5, 50) instead of 5e-02 ...
    at <- axTicks(2)
    axis(2, at = at, labels = format(at, scientific = FALSE, drop0trailing = TRUE, trim = TRUE))
    title(main = tid, line = 3.5)    # title above the legend
    lines(tms, v, col = "grey75")
    points(tms, v, pch = 20, col = cols[segOf])
    for (k in seq_len(s$nseg)) {
      idx <- s$begin[k]:s$end[k]
      med <- 10^median(log10(v[idx]))
      segments(tms[s$begin[k]], med, tms[s$end[k]], med, col = cols[k], lwd = 4)
    }
    if (s$nseg > 1) abline(v = tms[s$begin[-1]], col = "grey30", lty = 2)
    # legend directly above the plot area; on the log y axis par("usr") holds log10 values, hence 10^
    usr <- par("usr")
    legend(x = mean(usr[1:2]), y = 10^usr[4], xjust = 0.5, yjust = 0, xpd = TRUE,
           horiz = TRUE, bty = "n", cex = 1.2, text.font = 2,
           legend = c("segment median (colour = segment)", "break"), col = c("black", "grey30"), lty = c(1, 2), lwd = c(4, 1))
  }, execOnResize = TRUE)   # redraw on resize (e.g. moving to another monitor) instead of replaying

  output$seg_criterion <- renderUI({
    sr <- segRes()
    req(sr)
    s <- sr$tracks[[selectedIdx()]]
    req(is.na(s$problem))
    d <- s$criterion
    p(paste0(s$nseg, " segment(s) (automatic choice: ", s$Kauto, "; minimum segment length: ", s$lmin, " locations). ",
             "Lavielle criterion D by number of segments (a value above 0.75 supports that number): ",
             paste0(d$K, ": ", round(d$D, 2), collapse = " | ")))
  })

  output$seg_table <- DT::renderDataTable({
    sr <- segRes()
    req(sr)
    res <- results()
    req(res)
    tid <- res$overview$Track[selectedIdx()]
    DT::datatable(sr$table[sr$table$Track == tid, , drop = FALSE], rownames = FALSE, selection = "none",
                  options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  ## downloads ##
  output$saveMap <- downloadHandler(
    filename = "FPT_LeafletMap.html",
    content = function(file) {
      shinybusy::show_modal_spinner(text = "Preparing map ...")
      on.exit(shinybusy::remove_modal_spinner())
      htmlwidgets::saveWidget(widget = addBaseMaps(mapLayers(), mapGroups(), osm = FALSE), file = file)
    }
  )
  
  
  output$saveMapPng <- downloadHandler(
    filename = "FPT_Map.png",
    content = function(file) {
      shinybusy::show_modal_spinner(text = "Preparing PNG ...")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      # as in Leaflet Mapper: save the map in the working directory, where the browser
      # used by webshot2 can open it; OpenStreetMap tiles are blocked from a local file
      # session$token in the name, so two users never write or delete each other's file
      html_file <- paste0("FPT_map_export_", session$token, ".html")
      on.exit(unlink(c(html_file, sub("\\.html$", "_files", html_file)), recursive = TRUE), add = TRUE)
      htmlwidgets::saveWidget(widget = addBaseMaps(mapLayers(), mapGroups(), osm = FALSE),
                              file = html_file, selfcontained = TRUE)
      Sys.sleep(2)
      webshot2::webshot(url = html_file, file = file, vwidth = 1000, vheight = 800, delay = 2)
    }
  )
  
  output$saveCsv <- downloadHandler(
    filename = "FPT_values.csv",
    content = function(file) {
      shinybusy::show_modal_spinner(text = "Preparing table ...")
      on.exit(shinybusy::remove_modal_spinner())
      res <- results()
      req(res)
      d <- res$data
      co <- matrix(NA_real_, nrow = nrow(d), ncol = 2)
      nonempty <- !sf::st_is_empty(d)
      if (any(nonempty)) co[nonempty, ] <- sf::st_coordinates(sf::st_transform(d[nonempty, ], 4326))[, 1:2]
      out <- data.frame(as.character(mt_track_id(d)),
                        formatTime(mt_time(d)),
                        "location_long" = co[, 1],
                        "location_lat" = co[, 2],
                        "fpt_value" = d$fpt_value,
                        "fpt_unit" = res$unit,
                        "fpt_behaviour" = d$fpt_behaviour,
                        "radius_m" = res$radius,
                        "threshold" = res$thr,
                        "fpt_segment" = segColumn(),
                        check.names = FALSE)
      names(out)[1:2] <- c(mt_track_id_column(d), paste0(mt_time_column(d), "_UTC"))
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$saveSummary <- downloadHandler(
    filename = "FPT_track_summary.csv",
    content = function(file) {
      res <- results()
      req(res)
      out <- res$overview
      out$radius_m <- res$radius
      out$threshold <- res$thr
      out$fpt_unit <- res$unit
      write.csv(out, file, row.names = FALSE)
    }
  )

  output$saveSegments <- downloadHandler(
    filename = "FPT_segments.csv",
    content = function(file) {
      sr <- segRes()
      if (is.null(sr)) {
        write.csv(data.frame(Note = "No segments: click 'Segment' in the sidebar first."), file, row.names = FALSE)
        return(invisible())
      }
      out <- sr$table
      out$radius_m <- sr$radius
      out$fpt_unit <- sr$unit
      out$min_segment_days <- sr$minDays
      out$number_of_segments <- sr$mode
      write.csv(out, file, row.names = FALSE)
    }
  )

  # columns of the input data that are replaced; each is reported once in the log
  overwriteWarned <- character()

  # data must be returned. Either the unmodified input data, or the modified data by the app
  reactive({
    res <- results()

    if (is.null(res)) {
      return(data)
    }

    # one checkbox per column ("Add columns to the return data"); they take effect immediately
    out <- data
    if (isTRUE(input$add_fpt_value)) out$fpt_value <- res$data$fpt_value
    if (isTRUE(input$add_fpt_behaviour)) out$fpt_behaviour <- res$data$fpt_behaviour
    # segment numbers only after "Segment"; they are cleared with every "Apply Changes"
    if (isTRUE(input$add_fpt_segment) && !is.null(segRes())) out$fpt_segment <- segColumn()

    replaced <- setdiff(intersect(c("fpt_value", "fpt_behaviour", "fpt_segment"), names(out)), overwriteWarned)
    replaced <- replaced[replaced %in% names(data)]
    if (length(replaced) > 0) {
      logger.warn(paste0("The input data already contains the column(s) ", paste(replaced, collapse = ", "),
                         ". They are replaced by the values of this App."))
      overwriteWarned <<- c(overwriteWarned, replaced)
    }
    out
  })
}