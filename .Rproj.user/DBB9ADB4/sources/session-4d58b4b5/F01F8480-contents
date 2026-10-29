library("shiny")
library("move2")
library("sf")
library("leaflet")
library("shinyBS")

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

## helper functions ##

# check the FPT settings; returns a message if they are not valid, otherwise NULL
checkSettings <- function(radius, thr) {
  if (!isTRUE(is.finite(radius) && radius > 0)) return("Please enter a radius larger than 0 m.")
  if (!isTRUE(is.finite(thr) && thr >= 0)) return("Please enter a threshold time of 0 or larger.")
  NULL
}

# check the range of radii for the variance of log(FPT); returns a message if not valid, otherwise NULL
checkRadii <- function(rmin, rmax, n) {
  if (!isTRUE(is.finite(rmin) && rmin > 0)) return("Please enter a smallest radius larger than 0 m.")
  if (!isTRUE(is.finite(rmax) && rmax > rmin)) return("The largest radius must be larger than the smallest radius.")
  if (!isTRUE(is.finite(n) && n >= 2 && n <= 100)) return("The number of radii must be between 2 and 100.")
  NULL
}

# fast: FPT <= threshold, slow: FPT > threshold, NA: FPT could not be calculated
classifyFPT <- function(fptv, thr) {
  ifelse(is.na(fptv), NA_character_, ifelse(fptv <= thr, "fast", "slow"))
}

formatTime <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")

# build the ltraj object of one track, projected to an azimuthal equidistant projection
# centred on the track so that the radius is a true distance in metres.
# Returns the ltraj, the rows of 'trk' it was built from (non-empty, in time order),
# or a problem description if FPT cannot be calculated for this track
trackLtraj <- function(trk) {
  if (is.na(sf::st_crs(trk))) return(list(problem = "no coordinate reference system"))
  ok <- which(!sf::st_is_empty(trk))
  if (length(ok) < 3) return(list(problem = "fewer than 3 locations"))
  tms <- mt_time(trk)[ok]
  if (anyDuplicated(tms) > 0) return(list(problem = "duplicated timestamps"))
  ok <- ok[order(tms)]
  ll <- sf::st_coordinates(sf::st_transform(trk[ok, ], 4326))
  aeqd <- sprintf("+proj=aeqd +lat_0=%.6f +lon_0=%.6f +units=m", mean(ll[, 2]), mean(ll[, 1]))
  xy <- sf::st_coordinates(sf::st_transform(trk[ok, ], aeqd))
  ltr <- adehabitatLT::as.ltraj(xy = data.frame(x = xy[, 1], y = xy[, 2]),
                                date = as.POSIXct(mt_time(trk)[ok]),
                                id = "track", typeII = TRUE,
                                proj4string = sp::CRS(aeqd))
  list(ltraj = ltr, rows = ok, problem = NA_character_)
}

# add base maps and the layer control; osm = FALSE for maps that are saved,
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

mapOverlays <- c("lines", "fast movement", "slow movement")

shinyModuleUserInterface <- function(id, label) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)
  
  tagList(
    titlePanel("First Passage Time Segmentation"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Tracks"),
        uiOutput(ns("tracks_ui")),
        fluidRow(
          column(6, actionButton(ns("select_all_tracks"), "Select All", class = "btn-sm")),
          column(6, actionButton(ns("unselect_tracks"), "Unselect All", class = "btn-sm"))
        ),
        # hidden copy of the track selection as one JSON string, so that "Store settings" can restore it
        tags$div(style = "display:none;", textInput(ns("tracks_json"), label = NULL, value = "")),
        hr(),
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
        numericInput(ns("vlf_rmax"), label = "Largest radius (m)", value = 100000, min = 1),
        numericInput(ns("vlf_n"), label = "Number of radii", value = 20, min = 2, max = 100),
        bsTooltip(id = ns("vlf_n"), title = "The variance of log(FPT) is calculated for this many radii between the smallest and largest radius, for the track selected in the table. A peak indicates the spatial scale at which the animal concentrates its search effort.", placement = "bottom", trigger = "hover", options = list(container = "body")),
        actionButton(ns("vlf_go"), "Calculate", class = "btn-block"),
        uiOutput(ns("vlf_msg")),
        hr(),
        h4("Download"),
        downloadButton(ns("saveMap"), "Map (HTML)", class = "btn-sm"),
        downloadButton(ns("saveMapPng"), "Map (PNG)", class = "btn-sm"),
        downloadButton(ns("saveCsv"), "FPT values (CSV)", class = "btn-sm"),
        downloadButton(ns("saveSummary"), "Track summary (CSV)", class = "btn-sm")
      ),
      mainPanel(
        width = 9,
        h4("Tracks"),
        helpText("Click a row to show that track in the plot and map."),
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("track_table"))),
        tabsetPanel(
          tabPanel("FPT plot", shinycssloaders::withSpinner(plotOutput(ns("fpt"), height = "60vh"))),
          tabPanel("Map", shinycssloaders::withSpinner(leafletOutput(ns("leafmap"), height = "60vh"))),
          tabPanel("Choose radius", shinycssloaders::withSpinner(plotOutput(ns("vlf_plot"), height = "60vh")))
        )
      )
    )
  )
}

# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  
  trackRows <- split(seq_len(nrow(data)), mt_track_id(data), drop = TRUE)
  trackIds <- names(trackRows)
  
  
  
  results <- reactiveVal(NULL)
  pendingTracks <- reactiveVal(NULL)
  lockedTracks <- reactiveVal(NULL)
  init_applied <- reactiveVal(FALSE)
  init_done <- reactiveVal(FALSE)
  
  observeEvent(input$tracks, {
    req(!is.null(input$tracks))
    pendingTracks(as.character(input$tracks))
    init_applied(TRUE)
  }, ignoreInit = FALSE)
  
  lastTrack <- reactiveVal(NULL)      # track last clicked in the table
  settingsMsg <- reactiveVal(NULL)
  vlfRes <- reactiveVal(NULL)
  vlfMsg <- reactiveVal(NULL)
  
  ## track selection ##
  output$tracks_ui <- renderUI({
    restored_sel <- isolate(input$tracks)
    
    sel <- if (!is.null(restored_sel)) {
      intersect(as.character(restored_sel), trackIds)
    } else {
      trackIds
    }
    
    logger.info(
      "tracks_ui: choices=%d, selected=%d",
      length(trackIds),
      length(sel)
    )
    
    checkboxGroupInput(
      ns("tracks"),
      label = NULL,
      choices = trackIds,
      selected = sel
    )
  })
  
  
  # keep the JSON copy up to date (also when no track is selected)
  observeEvent(input$tracks, {
    updateTextInput(session, "tracks_json", value = as.character(jsonlite::toJSON(as.character(input$tracks))))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent(input$select_all_tracks, {
    updateCheckboxGroupInput(session, "tracks", selected = trackIds)
  })
  
  observeEvent(input$unselect_tracks, {
    updateCheckboxGroupInput(session, "tracks", selected = character(0))
  })
  
  ## initialize the shown tracks once from the restored/default checkbox selection
  observe({
    if (isTRUE(init_done())) return()
    req(init_applied())
    
    sel <- pendingTracks()
    if (is.null(sel) || !length(sel)) return()
    
    lockedTracks(sel)
    init_done(TRUE)
  })
  
  shownTracks <- reactive({
    req(init_done())
    lockedTracks()
  })
  
  ## FPT calculation for all tracks: runs once at App start and on every click of "Apply Changes" ##
  observeEvent(input$goButton, {
    clicked <- isTRUE(input$goButton > 0)
    msg <- checkSettings(input$radius, input$time_thr)
    if (is.null(msg) && clicked && length(input$tracks) == 0) msg <- "Please select at least one track."
    if (!is.null(msg)) {
      settingsMsg(msg)
      logger.info(msg)
      return()
    }
    settingsMsg(NULL)
    if (clicked) lockedTracks(as.character(input$tracks))
    radius <- input$radius
    thr <- input$time_thr
    unit <- input$thr_uni
    
    fptv <- rep(NA_real_, nrow(data))
    problems <- setNames(rep(NA_character_, length(trackIds)), trackIds)
    
    withProgress(message = "Calculating first passage times", value = 0, {
      for (tid in trackIds) {
        rows <- trackRows[[tid]]
        lt <- trackLtraj(data[rows, ])
        if (is.null(lt$ltraj)) {
          problems[tid] <- lt$problem
          logger.warn(paste0("First passage time could not be calculated for track ", tid, ": ", lt$problem, ". Its fpt_value and fpt_behaviour are NA."))
        } else {
          f <- adehabitatLT::fpt(lt$ltraj, radii = radius, units = unit)
          fptv[rows[lt$rows]] <- f[[1]][, 1]
        }
        incProgress(1 / length(trackIds), detail = tid)
      }
    })
    
    if (any(c("fpt_value", "fpt_behaviour") %in% names(data))) {
      logger.warn("The input data already contains the column(s) fpt_value and/or fpt_behaviour. They are overwritten.")
    }
    annotated <- data
    annotated$fpt_value <- fptv
    annotated$fpt_behaviour <- classifyFPT(fptv, thr)
    
    overview <- do.call(rbind, lapply(trackIds, function(tid) {
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
    
    logger.info(paste0("First passage times calculated for ", length(trackIds), " track(s) with radius ", radius, " m and threshold ", thr, " ", unit, "."))
    results(list(data = annotated, overview = overview, radius = radius, thr = thr, unit = unit))
  }, ignoreNULL = FALSE)
  
  output$settings_msg <- renderUI({
    if (!is.null(settingsMsg())) p(settingsMsg(), style = "color: red;")
  })
  
  ## table of the selected tracks; a click on a row selects the track for plot and map ##
  shownOverview <- reactive({
    res <- results()
    req(res)
    res$overview[res$overview$Track %in% shownTracks(), , drop = FALSE]
  })
  
  output$track_table <- DT::renderDataTable({
    ov <- shownOverview()
    # keep the last clicked track if it is still shown, else a restored row, else the first row
    lt <- isolate(lastTrack())
    sel <- if (!is.null(lt) && lt %in% ov$Track) match(lt, ov$Track) else isolate(input$track_table_rows_selected)
    if (!(length(sel) == 1 && sel >= 1 && sel <= nrow(ov))) sel <- 1
    DT::datatable(ov, rownames = FALSE,
                  selection = list(mode = "single", selected = sel, target = "row"),
                  options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = 200, scrollCollapse = TRUE))
  })
  
  observeEvent(input$track_table_rows_selected, {
    ov <- shownOverview()
    i <- input$track_table_rows_selected
    if (length(i) == 1 && i >= 1 && i <= nrow(ov)) lastTrack(ov$Track[i])
  })
  
  selectedTrack <- reactive({
    ov <- shownOverview()
    tid <- lastTrack()
    if (!is.null(tid) && tid %in% ov$Track) tid else ov$Track[1]
  })
  
  ## FPT plot of the selected track ##
  output$fpt <- renderPlot({
    res <- results()
    req(res)
    tid <- selectedTrack()
    trk <- res$data[trackRows[[tid]], ]
    validate(need(any(!is.na(trk$fpt_value)), paste0("No first passage times could be calculated for track ", tid, ".")))
    ord <- order(mt_time(trk))
    tms <- mt_time(trk)[ord]
    v <- trk$fpt_value[ord]
    cls <- trk$fpt_behaviour[ord]
    plot(tms, v, type = "l", main = tid, ylab = paste0("FPT (", res$unit, ")"), xlab = "time")
    points(tms[cls %in% "fast"], v[cls %in% "fast"], cex = 2, pch = 20, col = "red")
    points(tms[cls %in% "slow"], v[cls %in% "slow"], cex = 2, pch = 20, col = "darkgreen")
    abline(h = res$thr, col = "blue", lwd = 2)
    legend("topright", legend = c("fast movement", "slow movement", "threshold"),
           col = c("red", "darkgreen", "blue"), pch = c(20, 20, NA), lty = c(NA, NA, 1), lwd = c(NA, NA, 2), bg = "white")
  })
  
  ## map of the selected track (without base maps, see addBaseMaps()) ##
  mapLayers <- reactive({
    res <- results()
    req(res)
    tid <- selectedTrack()
    trk <- res$data[trackRows[[tid]], ]
    trk <- trk[!sf::st_is_empty(trk), ]
    validate(need(nrow(trk) > 0, paste0("Track ", tid, " has no locations to show.")))
    trk <- trk[order(mt_time(trk)), ]
    trk_ll <- sf::st_transform(trk, 4326)
    xy <- sf::st_coordinates(trk_ll)
    cls <- trk_ll$fpt_behaviour
    popup <- sprintf("<b>Time (UTC):</b> %s<br><b>FPT:</b> %s %s<br><b>Class:</b> %s",
                     formatTime(mt_time(trk_ll)), round(trk_ll$fpt_value, 2), res$unit,
                     ifelse(is.na(cls), "not classified", cls))
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
    outl %>%
      addLegend(position = "topright", colors = c("blue", "red", "darkgreen"),
                labels = c("lines", "fast movement", "slow movement"), opacity = 0.7, title = paste("track", tid)) %>%
      addScaleBar(position = "bottomleft",
                  options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE))
  })
  
  output$leafmap <- renderLeaflet({
    addBaseMaps(mapLayers(), mapOverlays)
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
    tid <- selectedTrack()
    lt <- trackLtraj(res$data[trackRows[[tid]], ])
    if (is.null(lt$ltraj)) {
      vlfRes(list(track = tid, problem = lt$problem))
      return()
    }
    radii <- seq(input$vlf_rmin, input$vlf_rmax, length.out = round(input$vlf_n))
    withProgress(message = "Calculating variance of log(FPT)", value = 0.5, {
      # the variance of log(FPT) does not depend on the time unit
      f <- adehabitatLT::fpt(lt$ltraj, radii = radii, units = "seconds")
      v <- adehabitatLT::varlogfpt(f, graph = FALSE)
    })
    vlfRes(list(track = tid, radii = radii, var = as.numeric(v[1, ]), problem = NA_character_))
  })
  
  output$vlf_msg <- renderUI({
    if (!is.null(vlfMsg())) p(vlfMsg(), style = "color: red;")
  })
  
  output$vlf_plot <- renderPlot({
    vr <- vlfRes()
    validate(need(!is.null(vr), "Set the range of radii in the sidebar and click 'Calculate' to compute the variance of log(FPT) for the track selected in the table."))
    validate(need(is.na(vr$problem), paste0("The variance of log(FPT) could not be calculated for track ", vr$track, ": ", vr$problem, ".")))
    plot(vr$radii, vr$var, type = "b", pch = 20, main = paste("Track", vr$track),
         xlab = "radius (m)", ylab = "variance of log(FPT)")
    res <- results()
    if (!is.null(res)) {
      abline(v = res$radius, col = "blue", lty = 2)
      legend("topright", legend = "current radius", col = "blue", lty = 2, bg = "white")
    }
  })
  
  ## downloads ##
  output$saveMap <- downloadHandler(
    filename = "FPT_LeafletMap.html",
    content = function(file) {
      shinybusy::show_modal_spinner(text = "Preparing map ...")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      htmlwidgets::saveWidget(widget = addBaseMaps(mapLayers(), mapOverlays, osm = FALSE),
                              file = file, selfcontained = TRUE)
    }
  )
  
  output$saveMapPng <- downloadHandler(
    filename = "FPT_Map.png",
    content = function(file) {
      shinybusy::show_modal_spinner(text = "Preparing PNG ...")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      # save the map in the working directory, where the browser used by webshot2
      # can open it; OpenStreetMap tiles are blocked from a local file
      html_file <- "FPT_map_export.html"
      on.exit(unlink(c(html_file, "FPT_map_export_files"), recursive = TRUE), add = TRUE)
      htmlwidgets::saveWidget(widget = addBaseMaps(mapLayers(), mapOverlays, osm = FALSE),
                              file = html_file, selfcontained = TRUE)
      Sys.sleep(2)
      webshot2::webshot(url = html_file, file = file, vwidth = 1000, vheight = 800, delay = 2)
    }
  )
  
  # the CSV downloads contain the tracks selected in the sidebar
  output$saveCsv <- downloadHandler(
    filename = "FPT_values.csv",
    content = function(file) {
      shinybusy::show_modal_spinner(text = "Preparing table ...")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      res <- results()
      req(res)
      d <- res$data[sort(unlist(trackRows[shownTracks()], use.names = FALSE)), ]
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
      out <- shownOverview()
      out$radius_m <- res$radius
      out$threshold <- res$thr
      out$fpt_unit <- res$unit
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  # data must be returned. Either the unmodified input data, or the modified data by the app
  reactive({
    res <- results()
    if (is.null(res)) data else res$data
  })
}