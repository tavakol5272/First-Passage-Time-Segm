library("shiny")
library("move2")
library("sf")
library("adehabitatLT")
library("leaflet")
library("DT")
library("shinyBS")

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

## helper: first passage time of one track, one value per location
## the track is projected to an azimuthal equidistant projection centred on the track itself,
## so that the radius (in m) is measured with little distortion
fpt_one_track <- function(datai, radius, units) {
  n <- nrow(datai)
  geom <- st_geometry(datai)
  ok <- !st_is_empty(geom)
  if (sum(ok) < 2) return(rep(NA_real_, n))

  centre <- st_coordinates(st_centroid(st_combine(st_transform(geom[ok], 4326))))
  aeqd <- sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +units=m", centre[1, 2], centre[1, 1])

  xy <- matrix(NA_real_, nrow = n, ncol = 2)
  xy[ok, ] <- st_coordinates(st_transform(geom[ok], aeqd))[, 1:2]

  track <- as.ltraj(xy = data.frame(x = xy[, 1], y = xy[, 2]),
                    date = as.POSIXct(mt_time(datai)),
                    id = as.character(unique(mt_track_id(datai))),
                    typeII = TRUE)
  f <- fpt(track, radii = radius, units = units)
  f[[1]][, 1]
}

## helper: fast movement if FPT < threshold, slow movement/resting if FPT >= threshold
fpt_class <- function(fpt_val, thr) {
  ifelse(is.na(fpt_val), NA_character_, ifelse(fpt_val < thr, "fast", "slow"))
}

## helper: FPT plot of one track (used for the App and the PNG download)
draw_fpt_plot <- function(d, id, thr, unit) {
  time <- as.POSIXct(mt_time(d))
  val <- d$fpt_value
  if (all(is.na(val))) {
    plot.new()
    title(main = id)
    text(0.5, 0.5, "No FPT values for this track (the animal never left the radius).")
    return(invisible(NULL))
  }
  fast <- d$fpt_behaviour %in% "fast"
  slow <- d$fpt_behaviour %in% "slow"
  plot(time, val, type = "l", main = id, ylab = paste0("FPT (", unit, ")"), xlab = "time")
  points(time[fast], val[fast], cex = 2, pch = 20, col = "red")
  points(time[slow], val[slow], cex = 2, pch = 20, col = "darkgreen")
  abline(h = thr, col = "blue", lwd = 2)
}

## helper: leaflet map of one track (used for the App and the html download)
make_fpt_map <- function(d, id, unit) {
  d <- st_transform(d, 4326) # leaflet needs longitude/latitude
  d <- d[!st_is_empty(st_geometry(d)), ]
  bounds <- as.vector(st_bbox(d))
  coords <- st_coordinates(d)
  d_fast <- d[d$fpt_behaviour %in% "fast", ]
  d_slow <- d[d$fpt_behaviour %in% "slow", ]
  d_val <- d[!is.na(d$fpt_value), ]

  outl <- leaflet() %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addTiles(group = "StreetMap") %>%
    addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
    addPolylines(lng = coords[, 1], lat = coords[, 2], color = "blue", group = "lines")

  if (nrow(d_fast) > 0) {
    outl <- addCircles(outl, data = d_fast, fillOpacity = 0.3, opacity = 0.5, color = "red", group = "fast movement")
  }
  if (nrow(d_slow) > 0) {
    outl <- addCircles(outl, data = d_slow, fillOpacity = 0.3, opacity = 0.5, color = "darkgreen", group = "slow movement")
  }
  if (nrow(d_val) > 0) {
    pal <- colorNumeric("viridis", domain = d_val$fpt_value)
    outl <- outl %>%
      addCircles(data = d_val, fillOpacity = 0.8, opacity = 0.8, color = pal(d_val$fpt_value), group = "FPT value") %>%
      addLegend(position = "bottomright", pal = pal, values = d_val$fpt_value,
                title = paste0("FPT (", unit, ")"), group = "FPT value")
  }

  outl %>%
    addLegend(position = "topright", colors = c("blue", "red", "darkgreen"),
              labels = c("lines", "fast movement", "slow movement"), opacity = 0.7,
              title = paste("track", id)) %>%
    addScaleBar(position = "bottomleft",
                options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
    addLayersControl(
      baseGroups = c("StreetMap", "TopoMap", "Aerial"),
      overlayGroups = c("lines", "fast movement", "slow movement", "FPT value"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup("FPT value")
}

shinyModuleUserInterface <- function(id, label) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)

  tagList(
    titlePanel("First Passage Time Segmentation"),
    fluidRow(
      column(3,
             numericInput(ns("radius"), label = "Radius parameter for First Passage Time (m)", value = 30000, min = 0),
             bsTooltip(id = ns("radius"), title = "Define the radius for which you want to calculate the first passage times (when does an animal pass the radius). Unit = m.", placement = "bottom", trigger = "hover", options = list(container = "body")),

             numericInput(ns("time_thr"), label = "Threshold time for migration/resting", value = 10, min = 0),
             bsTooltip(id = ns("time_thr"), title = "Define the time threshold with which you will split your tracks into fast and slow movement locations. Locations with FPT below the threshold are fast movement, locations with FPT equal to or above the threshold are slow movement/resting. Select your time unit below.", placement = "bottom", trigger = "hover", options = list(container = "body")),

             selectInput(ns("thr_uni"), label = "Unit of the threshold time", choices = c("seconds", "hours", "days"), selected = "days"),
             bsTooltip(id = ns("thr_uni"), title = "Select the time unit of your selected threshold time.", placement = "bottom", trigger = "hover", options = list(container = "body"))),

      column(3, actionButton(ns("goButton"), "Start/Update calculation", style = "color: white; background-color: orange;"))
    ),
    div(id = ns("C"), class = "shiny-input-radiogroup", DT::dataTableOutput(ns("foo"))),

    fluidRow(
      column(width = 5, plotOutput(ns("fpt"), height = "500px")),
      column(width = 7, leafletOutput(ns("leafmap"), height = "500px"))
    ),
    fluidRow(
      column(3, downloadButton(ns("savePlot"), "Save Plot as PNG")),
      column(3, downloadButton(ns("saveMap"), "Save Map as Html"))
    )
  )
}

# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data) # before hitting the go button, the input dataset is passed on unchanged
  fpt_settings <- reactiveVal(NULL) # settings used in the last calculation

  ids <- unique(as.character(mt_track_id(data)))
  track_index <- split(seq_len(nrow(data)), factor(as.character(mt_track_id(data)), levels = ids))

  observeEvent(input$goButton, {
    if (!isTRUE(input$radius > 0) || !isTRUE(input$time_thr > 0)) {
      showNotification("Radius and threshold time must be numbers larger than 0.", type = "error")
      logger.warn("FPT calculation not started: radius and threshold time must be larger than 0.")
      return()
    }
    radius <- input$radius
    thr <- input$time_thr
    unit <- input$thr_uni
    logger.info(paste0("Calculating FPT with radius ", radius, " m and threshold ", thr, " ", unit, "."))

    fpt_all <- rep(NA_real_, nrow(data))
    withProgress(message = "Calculating First Passage Time", value = 0, {
      for (id in ids) {
        idx <- track_index[[id]]
        fpt_all[idx] <- fpt_one_track(data[idx, ], radius, unit)
        incProgress(1 / length(ids), detail = id)
      }
    })

    out <- data
    out$fpt_value <- fpt_all
    out$fpt_behaviour <- fpt_class(fpt_all, thr)
    current(out)
    fpt_settings(list(thr = thr, unit = unit))
  })

  sel_id <- reactive({
    req(fpt_settings())
    if (!is.null(input$C) && input$C %in% ids) input$C else ids[1]
  })

  sel_data <- reactive({
    req(fpt_settings())
    current()[track_index[[sel_id()]], ]
  })

  output$foo <- DT::renderDataTable({
    req(fpt_settings())
    d <- current()
    tm <- mt_time(d)
    beh <- d$fpt_behaviour
    sel <- isolate(sel_id())
    overview <- data.frame(
      show = sprintf('<input type="radio" name="%s" value="%s"%s/>', ns("C"), ids, ifelse(ids == sel, " checked", "")),
      ids = ids,
      time0 = vapply(track_index, function(i) as.character(min(tm[i])), character(1)),
      timeE = vapply(track_index, function(i) as.character(max(tm[i])), character(1)),
      n_fast = vapply(track_index, function(i) sum(beh[i] %in% "fast"), integer(1)),
      n_slow = vapply(track_index, function(i) sum(beh[i] %in% "slow"), integer(1)),
      n_na = vapply(track_index, function(i) sum(is.na(beh[i])), integer(1))
    )
    names(overview) <- c("Show Plots and Map", "Track", "First timestamp", "Last timestamp",
                         "Fast movement locations", "Slow movement locations", "Locations without FPT (NA)")
    overview
  },
  escape = FALSE,
  selection = "none",
  server = FALSE,
  rownames = FALSE,
  options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = 250, scrollCollapse = TRUE)
  )

  output$fpt <- renderPlot({
    s <- fpt_settings()
    req(s)
    draw_fpt_plot(sel_data(), sel_id(), s$thr, s$unit)
  })

  mmap <- reactive({
    s <- fpt_settings()
    req(s)
    make_fpt_map(sel_data(), sel_id(), s$unit)
  })

  output$leafmap <- renderLeaflet({
    mmap()
  })

  output$savePlot <- downloadHandler(
    filename = function() paste0("FPT_plot_", sel_id(), ".png"),
    content = function(file) {
      s <- fpt_settings()
      png(file, width = 1000, height = 700, res = 120)
      on.exit(dev.off())
      draw_fpt_plot(sel_data(), sel_id(), s$thr, s$unit)
    }
  )

  output$saveMap <- downloadHandler(
    filename = "FPT_LeafletMap.html",
    content = function(file) {
      htmlwidgets::saveWidget(widget = mmap(), file = file)
    }
  )

  # data must be returned. Either the unmodified input data, or the modified data by the app
  return(reactive({ current() }))
}
