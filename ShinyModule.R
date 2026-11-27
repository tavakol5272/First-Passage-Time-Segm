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

## constants ##

# radii for the variance of log(FPT): 50 radii from 1 km to 1000 km, evenly spaced on a log scale
# (as 10^seq(3, 6, ...) in "firstPassageTime" / "Segmenting first passage time" of the book)
RADII <- 10^seq(3, 6, length.out = 50)

# minimum segment length: one week, as lmin = 168 hourly locations in the book
MIN_SEG_DAYS <- 7

# maximum number of segments per year of track, as Kmax = 12 for one year in the book
KMAX_PER_YEAR <- 12

## helper functions ##

formatTime <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")

### Helper 1
# radius as text: metres below 1 km, otherwise km with one decimal
fmtRadius <- function(r) {
  ifelse(r < 1000, paste(round(r), "m"), paste(format(round(r / 1000, 1), nsmall = 1, trim = TRUE), "km"))
}

### Helper 2: metric projection for the FPT calculation
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

### Helper 3
# variance of log(FPT) for each radius (column), as varlogfpt() of adehabitatLT
logVar <- function(m) {
  apply(m, 2, function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 2) NA_real_ else var(log(x))
  })
}

### Helper 4
# candidate radii of a variance curve: the peak (highest variance) and the breaks in the trend
# of the curve, with the rule of "firstPassageTime": which(diff(floor(diff(vars))) == -1) + 1;
# a highest variance at the smallest or largest radius is not a real peak
findCandidates <- function(vars) {
  v <- as.numeric(vars)
  ok <- which(!is.na(v))
  if (length(ok) == 0) {
    return(list(table = data.frame(idx = integer(), name = character(), label = character()),
                peak = NA_integer_, atEdge = FALSE, noVar = TRUE))
  }
  peak <- which.max(v)
  atEdge <- peak %in% range(ok)
  brk <- which(diff(floor(diff(v))) == -1) + 1
  brk <- sort(setdiff(brk, if (!atEdge) peak))   # a break at the peak is shown once, as the peak
  idx <- c(if (!atEdge) peak, brk)
  name <- c(if (!atEdge) "Peak", if (length(brk) > 0) paste("Break", seq_along(brk)))
  tab <- if (length(idx) == 0) {
    data.frame(idx = integer(), name = character(), label = character())
  } else {
    data.frame(idx = idx, name = name, label = paste0(name, ": ", fmtRadius(RADII[idx])))
  }
  list(table = tab, peak = peak, atEdge = atEdge, noVar = FALSE)
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
# legend directly above the plot area, in rows of at most perRow entries;
# on a log axis par("usr") holds log10 values, hence 10^
topLegend <- function(legend, col, pch = NA, lty = NA, lwd = NA, xlog = FALSE, ylog = FALSE, perRow = 6) {
  usr <- par("usr")
  x <- mean(usr[1:2])
  y <- usr[4]
  if (xlog) x <- 10^x
  if (ylog) y <- 10^y
  legend(x = x, y = y, xjust = 0.5, yjust = 0, xpd = TRUE, ncol = min(length(legend), perRow),
         bty = "n", cex = 1.2, text.font = 2, legend = legend, col = col, pch = pch, lty = lty, lwd = lwd)
}

### Helper 8
# y axis labels of a log axis as plain numbers (0.05, 0.5, 5, 50) instead of 5e-02 ...
plainLogAxis <- function() {
  at <- axTicks(2)
  axis(2, at = at, labels = format(at, scientific = FALSE, drop0trailing = TRUE, trim = TRUE))
}

### Helper 9
# segment the log10 FPT series of one track into phases (Lavielle's penalised contrast, package
# segclust2d), as in "Segmenting first passage time" of Analysing and Mapping of Animal Movement in R:
# segments of at least one week (lmin from the median sampling interval of the track; 168 for
# hourly data as in the book), at most 12 segments per year, number of segments by the Lavielle criterion
segmentTrack <- function(fptv, tms) {
  keep <- which(!is.na(fptv) & fptv > 0)
  if (length(keep) < 2) return(list(problem = "no first passage times"))
  ord <- keep[order(tms[keep])]
  dt <- median(as.numeric(diff(tms[ord]), units = "secs"))
  if (!isTRUE(is.finite(dt) && dt > 0)) return(list(problem = "no sampling interval"))
  lmin <- max(2, round(MIN_SEG_DAYS * 86400 / dt))
  years <- as.numeric(difftime(max(tms[ord]), min(tms[ord]), units = "days")) / 365.25
  Kmax <- min(KMAX_PER_YEAR * max(1, ceiling(years)), floor(length(ord) / lmin))
  if (Kmax < 2) return(list(problem = "shorter than two weeks of first passage times"))
  seg <- tryCatch(suppressMessages(suppressWarnings(
    segclust2d::segmentation(data.frame(logFpt = log10(fptv[ord])), seg.var = "logFpt",
                             lmin = lmin, Kmax = Kmax, subsample = FALSE, sameSigma = TRUE))),
    error = function(e) e)
  if (inherits(seg, "error")) return(list(problem = paste("segmentation failed:", conditionMessage(seg))))
  D <- segclust2d::chooseseg_lavielle(seg$likelihood$likelihood)$lavielle
  nseg <- seg$Kopt.lavielle
  # segment() prints "User-selected number of segment" also for the automatic choice: keep it out of the log
  path <- suppressMessages(segclust2d::segment(seg, nseg = nseg))
  list(rows = ord, begin = path$begin, end = path$end, nseg = nseg, lmin = lmin, Kmax = Kmax,
       criterion = data.frame(K = seq_along(D) + 1, D = D), problem = NA_character_)
}

### Helper 10
# colours of the tracks (FPT plot and map of all tracks)
trackColours <- function(n) grDevices::hcl.colors(max(n, 2), "Dynamic")[seq_len(n)]

### Helper 11
# one map per segment, as in "Segmenting first passage time" of the book: the whole track in grey,
# the segment in black, on the land of Natural Earth; all maps with the same extent
drawSegMaps <- function(s, trackRows, co, tms, land) {
  k <- s$nseg
  nc <- min(k, 3)
  par(mfrow = c(ceiling(k / nc), nc), mar = c(1, 0.5, 2.5, 0.5))
  xyAll <- co[trackRows, , drop = FALSE]
  for (i in seq_len(k)) {
    plot(xyAll, type = "n", asp = 1, axes = FALSE, xlab = NA, ylab = NA)
    if (!is.null(land)) plot(sf::st_geometry(land), col = "grey92", border = "grey75", lwd = 0.5, add = TRUE)
    lines(xyAll, col = "grey60")
    r <- s$rows[s$begin[i]:s$end[i]]
    lines(co[r, , drop = FALSE], col = "black", lwd = 2)
    title(paste(format(min(tms[r]), "%d %b %Y", tz = "UTC"), "-", format(max(tms[r]), "%d %b %Y", tz = "UTC")),
          cex.main = 1.2)
  }
}


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
        h4("Choose radius"),
        # radius in use, candidate radii of the curve shown (one can be ticked), radius box if there is no peak
        uiOutput(ns("radius_ui")),
        actionButton(ns("use_radius"), "Use this radius", class = "btn-primary btn-block"),
        bsTooltip(id = ns("use_radius"), title = "Calculates the first passage times of all tracks with the ticked radius and divides them into segments.", placement = "bottom", trigger = "hover", options = list(container = "body")),
        uiOutput(ns("radius_msg")),
        hr(),
        h4("Add columns to the return data"),
        # each column on its own; takes effect immediately
        checkboxInput(ns("add_fpt_value"), "fpt_value (first passage time, days)", value = FALSE),
        checkboxInput(ns("add_fpt_segment"), "fpt_segment (segment number)", value = FALSE),
        hr(),
        h4("Download"),
        downloadButton(ns("saveMap"), "Map (HTML)", class = "btn-sm"),
        downloadButton(ns("saveMapPng"), "Map (PNG)", class = "btn-sm"),
        downloadButton(ns("saveSegMaps"), "Segment maps (PNG)", class = "btn-sm"),
        downloadButton(ns("saveCsv"), "FPT values (CSV)", class = "btn-sm"),
        downloadButton(ns("saveSummary"), "Track summary (CSV)", class = "btn-sm"),
        downloadButton(ns("saveSegments"), "Segments (CSV)", class = "btn-sm")
      ),
      mainPanel(
        width = 9,
        h4("Tracks"),
        helpText("Click a track to show only that track. Click it again, or click 'All tracks', to show all tracks."),
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("track_table"))),
        tabsetPanel(id = ns("main_tabs"), selected = "Choose radius",
          tabPanel("Choose radius", shinycssloaders::withSpinner(plotOutput(ns("vlf_plot"), height = "60vh"))),
          tabPanel("FPT plot", shinycssloaders::withSpinner(plotOutput(ns("fpt"), height = "60vh"))),
          tabPanel("Map", shinycssloaders::withSpinner(leafletOutput(ns("leafmap"), height = "60vh"))),
          tabPanel("Segments",
                   shinycssloaders::withSpinner(imageOutput(ns("seg_maps"), height = "auto")),
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

  # coordinates of all locations (NA for empty locations), for the segment maps
  co <- matrix(NA_real_, nrow = nrow(data_ll), ncol = 2)
  nonempty <- !sf::st_is_empty(data_ll)
  if (any(nonempty)) co[nonempty, ] <- sf::st_coordinates(data_ll[nonempty, ])[, 1:2]

  # land of Natural Earth for the segment maps, as in the book
  land <- tryCatch(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"),
                   error = function(e) {
                     logger.warn(paste("The land for the segment maps could not be loaded:", conditionMessage(e)))
                     NULL
                   })

  base <- reactiveVal(NULL)        # FPT of all tracks for all RADII and the variance curves
  radiusUse <- reactiveVal(NULL)   # radius in use for all tracks: list(value, idx, text)
  radiusMsg <- reactiveVal(NULL)

  ## at App start: FPT of all tracks for all radii, variance curves, and the peak of all tracks as radius ##
  observeEvent(TRUE, {
    trackRows <- split(seq_len(nrow(data)), mt_track_id(data), drop = TRUE)
    tms <- mt_time(data)
    fptAll <- matrix(NA_real_, nrow = nrow(data), ncol = length(RADII))
    lts <- vector("list", length(trackRows))
    ltRows <- vector("list", length(trackRows))
    problems <- setNames(rep(NA_character_, length(trackRows)), names(trackRows))

    withProgress(message = paste("Calculating first passage times for", length(RADII), "radii to find the radius"),
                 detail = "this can take a moment ...", value = 0, {
      for (j in seq_along(trackRows)) {
        tid <- names(trackRows)[j]
        rows <- trackRows[[j]]
        lt <- trackLtraj(data_ll[rows, ])
        if (is.null(lt$ltraj)) {
          problems[tid] <- lt$problem
          logger.warn(paste0("First passage time could not be calculated for track ", tid, ": ", lt$problem, "."))
        } else {
          f <- adehabitatLT::fpt(lt$ltraj, radii = RADII, units = "days")
          fptAll[rows[lt$rows], ] <- as.matrix(f[[1]])
          lts[[j]] <- lt$ltraj
          ltRows[[j]] <- rows[lt$rows]
        }
        incProgress(1 / length(trackRows), detail = tid)
      }
    })

    # rows of each track with a location, in time order (for lines and maps)
    sortedRows <- lapply(trackRows, function(r) {
      r <- r[nonempty[r]]
      r[order(tms[r])]
    })

    b <- list(trackRows = trackRows, sortedRows = sortedRows, fptAll = fptAll, lts = lts, ltRows = ltRows,
              problems = problems, curveAll = logVar(fptAll),
              curves = lapply(trackRows, function(r) logVar(fptAll[r, , drop = FALSE])))
    base(b)

    cand <- findCandidates(b$curveAll)
    if (!is.na(cand$peak) && !cand$atEdge) {
      radiusUse(list(value = RADII[cand$peak], idx = cand$peak,
                     text = paste0(fmtRadius(RADII[cand$peak]), " (Peak, all tracks)")))
      logger.info(paste0("Radius at start: peak of all tracks, ", fmtRadius(RADII[cand$peak]), "."))
    } else {
      logger.info("No clear peak in the variance of log(FPT) of all tracks: the radius has to be chosen by the user.")
    }
  }, once = TRUE)

  ## track selection: 0 = all tracks, otherwise the number of the track ##
  selectedIdx <- reactiveVal(0)
  observe({
    b <- base()
    req(b)
    i <- input$track_table_rows_selected
    selectedIdx(if (length(i) == 1 && i >= 1 && i <= length(b$trackRows)) i else 0)
  })

  trackName <- function(i) {
    if (i == 0) "all tracks" else paste("track", names(base()$trackRows)[i])
  }

  ## FPT of all tracks at the radius in use ##
  fptRes <- reactive({
    b <- base()
    ru <- radiusUse()
    if (is.null(b) || is.null(ru)) return(NULL)
    if (!is.na(ru$idx)) {
      fptv <- b$fptAll[, ru$idx]
    } else {
      # radius entered by the user: calculate the FPT for this radius
      fptv <- rep(NA_real_, nrow(data))
      withProgress(message = "Calculating first passage times", value = 0, {
        for (j in seq_along(b$lts)) {
          if (!is.null(b$lts[[j]])) {
            f <- adehabitatLT::fpt(b$lts[[j]], radii = ru$value, units = "days")
            fptv[b$ltRows[[j]]] <- f[[1]][, 1]
          }
          incProgress(1 / length(b$lts))
        }
      })
    }
    list(fptv = fptv, radius = ru$value, text = ru$text)
  })

  ## segmentation of all tracks at the radius in use, automatically ##
  segRes <- reactive({
    f <- fptRes()
    if (is.null(f)) return(NULL)
    b <- base()
    tms <- mt_time(data)
    tids <- names(b$trackRows)
    tracks <- vector("list", length(tids))
    withProgress(message = "Segmenting first passage times", value = 0, {
      for (j in seq_along(tids)) {
        rows <- b$trackRows[[j]]
        s <- segmentTrack(f$fptv[rows], tms[rows])
        if (is.na(s$problem)) {
          s$rows <- rows[s$rows]   # back to row numbers of the data
        } else {
          logger.warn(paste0("Track ", tids[j], " was not segmented: ", s$problem, "."))
        }
        tracks[[j]] <- s
        incProgress(1 / length(tids), detail = tids[j])
      }
    })

    # one row per segment, as the tables of the book; one row with a note for a track that was not segmented
    table <- do.call(rbind, lapply(seq_along(tids), function(j) {
      s <- tracks[[j]]
      if (!is.na(s$problem)) {
        return(data.frame("Track" = tids[j], "Segment" = NA, "From (UTC)" = NA, "To (UTC)" = NA,
                          "Median first passage time (days)" = NA, "Note" = paste("not segmented:", s$problem),
                          check.names = FALSE))
      }
      do.call(rbind, lapply(seq_len(s$nseg), function(i) {
        r <- s$rows[s$begin[i]:s$end[i]]
        data.frame("Track" = tids[j], "Segment" = i,
                   "From (UTC)" = formatTime(min(tms[r])), "To (UTC)" = formatTime(max(tms[r])),
                   "Median first passage time (days)" = round(10^median(log10(f$fptv[r])), 1),
                   "Note" = "", check.names = FALSE)
      }))
    }))

    nok <- sum(vapply(tracks, function(s) is.na(s$problem), logical(1)))
    logger.info(paste0("Segmented ", nok, " of ", length(tids), " track(s) at radius ", fmtRadius(f$radius), "."))
    list(tracks = tracks, table = table, radius = f$radius)
  })

  # segment number of every location; NA where the location has no FPT or its track was not segmented
  segColumn <- reactive({
    sr <- segRes()
    col <- rep(NA_integer_, nrow(data))
    if (!is.null(sr)) for (s in sr$tracks) {
      if (is.na(s$problem)) col[s$rows] <- findInterval(seq_along(s$rows), s$begin)
    }
    col
  })

  ## track table, with "All tracks" as the last row ##
  overview <- reactive({
    b <- base()
    req(b)
    sr <- segRes()
    tms <- mt_time(data)
    tids <- names(b$trackRows)
    ov <- do.call(rbind, lapply(seq_along(tids), function(j) {
      rows <- b$trackRows[[j]]
      s <- if (is.null(sr)) NULL else sr$tracks[[j]]
      note <- if (!is.na(b$problems[j])) paste("not calculated:", b$problems[j])
              else if (!is.null(s) && !is.na(s$problem)) paste("not segmented:", s$problem) else ""
      data.frame("Track" = tids[j],
                 "First timestamp (UTC)" = formatTime(min(tms[rows])),
                 "Last timestamp (UTC)" = formatTime(max(tms[rows])),
                 "Locations" = length(rows),
                 "Segments" = if (!is.null(s) && is.na(s$problem)) s$nseg else NA,
                 "Note" = note,
                 check.names = FALSE)
    }))
    rbind(ov, data.frame("Track" = "All tracks",
                         "First timestamp (UTC)" = formatTime(min(tms)),
                         "Last timestamp (UTC)" = formatTime(max(tms)),
                         "Locations" = nrow(data),
                         "Segments" = if (all(is.na(ov$Segments))) NA else sum(ov$Segments, na.rm = TRUE),
                         "Note" = "",
                         check.names = FALSE))
  })

  output$track_table <- DT::renderDataTable({
    ov <- overview()
    # keep the selection if it is still valid; no selection = all tracks
    sel <- isolate(input$track_table_rows_selected)
    if (!(length(sel) == 1 && sel >= 1 && sel <= nrow(ov))) sel <- NULL
    DT::datatable(ov, rownames = FALSE,
                  selection = list(mode = "single", selected = sel, target = "row"),
                  options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = 200, scrollCollapse = TRUE)) %>%
      DT::formatStyle("Track", target = "row", fontWeight = DT::styleEqual("All tracks", "bold"))
  })

  ## Choose radius: variance curve and candidates of the selection ##
  curveSel <- reactive({
    b <- base()
    req(b)
    i <- selectedIdx()
    vars <- if (i == 0) b$curveAll else b$curves[[i]]
    c(findCandidates(vars), list(vars = vars, scope = trackName(i)))
  })

  # candidate ticked by default: the radius in use if it is a candidate, otherwise the peak
  candDefault <- reactive({
    cv <- curveSel()
    ru <- radiusUse()
    tab <- cv$table
    if (!is.null(ru) && !is.na(ru$idx) && ru$idx %in% tab$idx) return(as.character(ru$idx))
    if (nrow(tab) > 0 && tab$name[1] == "Peak") return(as.character(tab$idx[1]))
    character()
  })

  output$radius_ui <- renderUI({
    cv <- curveSel()
    ru <- radiusUse()
    tab <- cv$table
    inUse <- if (is.null(ru)) {
      p("Radius in use: none yet. Choose a radius below.", style = "color: #d9822b;")
    } else {
      p(strong("Radius in use: "), ru$text)
    }
    noPeak <- cv$noVar || cv$atEdge
    tagList(
      inUse,
      if (nrow(tab) > 0) {
        checkboxGroupInput(ns("cand"), label = paste0("Candidate radii (", cv$scope, ")"),
                           choiceNames = tab$label, choiceValues = as.character(tab$idx),
                           selected = isolate(candDefault()))
      },
      if (noPeak) {
        tagList(
          p(if (cv$noVar) paste0("No variance of log(FPT) could be calculated for ", cv$scope, ". Enter a radius:")
            else if (nrow(tab) > 0) "No clear peak. Choose a candidate or enter a radius:"
            else "No clear peak. Enter a radius:",
            style = "color: #d9822b;"),
          numericInput(ns("radius_custom"), "Radius (m)", value = NA, min = 1)
        )
      }
    )
  })

  # only one candidate can be ticked: a new tick removes the previous one
  prevTick <- reactiveVal(character())
  observeEvent(candDefault(), prevTick(candDefault()))
  observeEvent(input$cand, {
    new <- input$cand
    if (length(new) > 1) {
      keep <- setdiff(new, prevTick())
      if (length(keep) != 1) keep <- new[length(new)]
      updateCheckboxGroupInput(session, "cand", selected = keep)
      prevTick(keep)
    } else {
      prevTick(new)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$use_radius, {
    cv <- curveSel()
    tick <- input$cand
    tick <- tick[tick %in% as.character(cv$table$idx)]
    if (length(tick) == 1) {
      j <- as.integer(tick)
      name <- cv$table$name[cv$table$idx == j]
      radiusUse(list(value = RADII[j], idx = j, text = paste0(fmtRadius(RADII[j]), " (", name, ", ", cv$scope, ")")))
    } else if ((cv$noVar || cv$atEdge) && isTRUE(is.finite(input$radius_custom) && input$radius_custom > 0)) {
      r <- input$radius_custom
      radiusUse(list(value = r, idx = NA_integer_, text = paste0(fmtRadius(r), " (entered)")))
    } else {
      msg <- if (cv$noVar || cv$atEdge) "Please tick a candidate radius or enter a radius larger than 0 m."
             else "Please tick a candidate radius."
      radiusMsg(msg)
      logger.info(msg)
      return()
    }
    radiusMsg(NULL)
    logger.info(paste("Radius in use:", radiusUse()$text))
    # show the result: open the tab "FPT plot"
    updateTabsetPanel(session, "main_tabs", selected = "FPT plot")
  })

  output$radius_msg <- renderUI({
    if (!is.null(radiusMsg())) p(radiusMsg(), style = "color: red;")
  })

  output$vlf_plot <- renderPlot({
    cv <- curveSel()
    validate(need(!cv$noVar, paste0("No variance of log(FPT) could be calculated for ", cv$scope, ".")))
    ru <- radiusUse()
    tab <- cv$table
    par(mar = c(5.1, 4.1, 6, 2.1))   # extra space at the top for the legend
    validate(need(plotFits(), "The plot area is too small. Please enlarge the window."))
    plot(RADII, cv$vars, type = "b", pch = 20, log = "x", main = "", xaxt = "n",
         xlab = "radius (log scale)", ylab = "variance of log(FPT)")
    at <- 10^(3:6)
    axis(1, at = at, labels = fmtRadius(at))
    title(main = paste0(toupper(substr(cv$scope, 1, 1)), substring(cv$scope, 2)), line = 3.5)   # title above the legend
    usr <- par("usr")
    if (nrow(tab) > 0) {
      isPeak <- tab$name == "Peak"
      cols <- ifelse(isPeak, "red", "grey30")
      abline(v = RADII[tab$idx], col = cols, lwd = ifelse(isPeak, 2, 1))
      if (any(isPeak)) points(RADII[tab$idx[isPeak]], cv$vars[tab$idx[isPeak]], pch = 19, cex = 2, col = "red")
      # name and value of each candidate, from the top down, to the right of its line
      text(x = RADII[tab$idx], y = usr[4] - 0.02 * diff(usr[3:4]), labels = tab$label,
           srt = 90, adj = c(1, 1.3), col = cols, font = 2, cex = 1.1)
    }
    if (!is.null(ru)) abline(v = ru$value, col = "blue", lty = 2, lwd = 2)
    topLegend(legend = c("peak", "break", "radius in use"), col = c("red", "grey30", "blue"),
              lty = c(1, 1, 2), lwd = c(2, 1, 2), xlog = TRUE)
  }, execOnResize = TRUE)   # redraw on resize (e.g. moving to another monitor) instead of replaying

  ## FPT plot: FPT over time at the radius in use, as the figure "First passage time for each
  ## position at different scales" of "firstPassageTime" (grey points, black line, "Radius ... metres") ##
  output$fpt <- renderPlot({
    b <- base()
    req(b)
    f <- fptRes()
    validate(need(!is.null(f), "Choose a radius first in the sidebar ('Choose radius')."))
    i <- selectedIdx()
    tids <- names(b$trackRows)
    tms <- mt_time(data)
    idxs <- if (i == 0) seq_along(tids) else i
    # rows of each track in time order; locations without FPT stay in, so the line has gaps there (as in the book)
    rl <- lapply(idxs, function(j) {
      r <- b$trackRows[[j]]
      r[order(tms[r])]
    })
    has <- vapply(rl, function(r) any(!is.na(f$fptv[r])), logical(1))
    validate(need(any(has), paste0("No first passage times could be calculated for ", trackName(i), ".")))
    allr <- unlist(rl[has])
    # one colour per track, the same as in the plot of all tracks and on the map; legend for several tracks
    multi <- sum(has) > 1
    cols <- trackColours(length(tids))[idxs]
    nr <- if (multi) ceiling(sum(has) / 6) else 0
    par(mar = c(5.1, 4.1, 3.5 + 1.6 * nr, 2.1))   # space at the top for the legend
    validate(need(plotFits(), "The plot area is too small. Please enlarge the window."))
    plot(tms[allr], f$fptv[allr], type = "n", main = "", xlab = "Time", ylab = "FPT (days)", bty = "n")
    title(main = paste0("Radius ", format(round(f$radius), big.mark = ",", scientific = FALSE), " metres (",
                        trackName(i), ")"), line = 1.6 * nr + 1.2)
    for (k in which(has)) {
      r <- rl[[k]]
      points(tms[r], f$fptv[r], pch = 16, col = adjustcolor("grey", 0.1))
      lines(tms[r], f$fptv[r], col = cols[k])
    }
    if (multi) topLegend(legend = tids[idxs][has], col = cols[has], lty = 1, lwd = 2)
  }, execOnResize = TRUE)   # redraw on resize (e.g. moving to another monitor) instead of replaying

  ## Map: track lines and points of the selection ##
  mapLayers <- reactive({
    b <- base()
    req(b)
    i <- selectedIdx()
    tids <- names(b$trackRows)
    idxs <- if (i == 0) seq_along(tids) else i
    idxs <- idxs[lengths(b$sortedRows[idxs]) > 0]
    validate(need(length(idxs) > 0, paste0("There are no locations to show for ", trackName(i), ".")))
    f <- fptRes()
    cols <- trackColours(length(tids))
    allr <- unlist(b$sortedRows[idxs])
    bounds <- range(co[allr, 1]); bounds <- c(bounds[1], range(co[allr, 2])[1], bounds[2], range(co[allr, 2])[2])

    outl <- leaflet() %>% fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
    for (j in idxs) {
      r <- b$sortedRows[[j]]
      fv <- if (is.null(f)) rep(NA_real_, length(r)) else f$fptv[r]
      popup <- sprintf("<b>Track:</b> %s<br><b>Time (UTC):</b> %s<br><b>FPT:</b> %s",
                       tids[j], formatTime(mt_time(data)[r]),
                       ifelse(is.na(fv), "not calculated", paste(round(fv, 2), "days")))
      outl <- outl %>%
        addPolylines(lng = co[r, 1], lat = co[r, 2], color = cols[j], weight = 2, group = "lines") %>%
        addCircles(lng = co[r, 1], lat = co[r, 2], popup = popup, color = cols[j],
                   fillOpacity = 0.3, opacity = 0.6, group = "points")
    }
    outl %>%
      addLegend(position = "topright", colors = cols[idxs], labels = tids[idxs], opacity = 0.7,
                title = if (i == 0) "tracks" else "track") %>%
      addScaleBar(position = "bottomleft",
                  options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE))
  })

  output$leafmap <- renderLeaflet({
    addBaseMaps(mapLayers(), c("lines", "points"))
  })

  ## Segments: one map per segment, Lavielle criterion and table ##
  # segment maps of one track as a PNG file: 1200 px wide, 400 px per row of maps
  # (used for the tab "Segments" and for the download "Segment maps (PNG)")
  writeSegMaps <- function(file, s, trackRows) {
    png(file, width = 1200, height = 400 * ceiling(s$nseg / min(s$nseg, 3)))
    on.exit(dev.off())
    drawSegMaps(s, trackRows, co, mt_time(data), land)
  }

  # segmentation of the selected track; NULL for all tracks or before a radius is chosen
  segSel <- reactive({
    sr <- segRes()
    i <- selectedIdx()
    if (is.null(sr) || i == 0) NULL else sr$tracks[[i]]
  })

  # the segment maps are drawn once into a PNG of fixed size and shown scaled to the width of the tab,
  # so they do not depend on the window size: a download, resizing the window or moving it to another
  # monitor does not redraw them; the PNG of each track and radius is kept (segMapFiles), so the maps
  # of a track are shown at once when it is selected again; the files are deleted when the session ends
  segMapFiles <- new.env()
  session$onSessionEnded(function() unlink(unlist(mget(ls(segMapFiles), envir = segMapFiles))))

  output$seg_maps <- renderImage({
    validate(need(!is.null(segRes()), "Choose a radius first in the sidebar ('Choose radius')."))
    i <- selectedIdx()
    validate(need(i != 0, "Select a track in the table to see its segment maps."))
    s <- segSel()
    validate(need(is.na(s$problem), paste0("Track ", names(base()$trackRows)[i], " was not segmented: ", s$problem, ".")))
    key <- paste(i, segRes()$radius)
    file <- segMapFiles[[key]]
    if (is.null(file) || !file.exists(file)) {
      file <- tempfile(fileext = ".png")
      writeSegMaps(file, s, base()$sortedRows[[i]])
      assign(key, file, envir = segMapFiles)
    }
    list(src = file, contentType = "image/png", width = "100%", alt = "Segment maps")
  }, deleteFile = FALSE)

  output$seg_criterion <- renderUI({
    s <- segSel()
    req(s, is.na(s$problem))
    d <- s$criterion
    Dtxt <- lapply(seq_len(nrow(d)), function(k) {
      txt <- paste0(d$K[k], ": ", round(d$D[k], 2))
      if (d$K[k] == s$nseg) strong(txt) else txt
    })
    p(paste0("Number of segments: ", s$nseg, ", chosen by the Lavielle criterion ",
             "(minimum segment length one week = ", s$lmin, " locations; at most ", s$Kmax, " segments). ",
             "D by number of segments (a value above 0.75 supports that number): "),
      do.call(tagList, Map(function(x, k) if (k < length(Dtxt)) tagList(x, " | ") else x, Dtxt, seq_along(Dtxt))))
  })

  output$seg_table <- DT::renderDataTable({
    sr <- segRes()
    req(sr)
    i <- selectedIdx()
    tab <- if (i == 0) sr$table else sr$table[sr$table$Track == names(base()$trackRows)[i], -1, drop = FALSE]
    DT::datatable(tab, rownames = FALSE, selection = "none",
                  options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  ## downloads ##
  output$saveMap <- downloadHandler(
    filename = "FPT_LeafletMap.html",
    content = function(file) {
      shinybusy::show_modal_spinner(text = "Preparing map ...")
      on.exit(shinybusy::remove_modal_spinner())
      htmlwidgets::saveWidget(widget = addBaseMaps(mapLayers(), c("lines", "points"), osm = FALSE), file = file)
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
      htmlwidgets::saveWidget(widget = addBaseMaps(mapLayers(), c("lines", "points"), osm = FALSE),
                              file = html_file, selfcontained = TRUE)
      Sys.sleep(2)
      webshot2::webshot(url = html_file, file = file, vwidth = 1000, vheight = 800, delay = 2)
    }
  )

  output$saveSegMaps <- downloadHandler(
    filename = "FPT_segment_maps.png",
    content = function(file) {
      s <- segSel()
      i <- selectedIdx()
      if (!is.null(s) && is.na(s$problem)) {
        writeSegMaps(file, s, base()$sortedRows[[i]])   # the same picture as in the tab "Segments"
      } else {
        png(file, width = 1200, height = 300)
        on.exit(dev.off())
        plot.new()
        text(0.5, 0.5, if (i == 0) "Select a track in the table to save its segment maps."
                       else "This track has no segments.", cex = 1.5)
      }
    }
  )

  output$saveCsv <- downloadHandler(
    filename = "FPT_values.csv",
    content = function(file) {
      shinybusy::show_modal_spinner(text = "Preparing table ...")
      on.exit(shinybusy::remove_modal_spinner())
      f <- fptRes()
      if (is.null(f)) {
        write.csv(data.frame(Note = "No radius in use: choose a radius in the sidebar first."), file, row.names = FALSE)
        return(invisible())
      }
      d <- data
      out <- data.frame(as.character(mt_track_id(d)),
                        formatTime(mt_time(d)),
                        "location_long" = co[, 1],
                        "location_lat" = co[, 2],
                        "fpt_value" = f$fptv,
                        "fpt_unit" = "days",
                        "radius_m" = f$radius,
                        "fpt_segment" = segColumn(),
                        check.names = FALSE)
      names(out)[1:2] <- c(mt_track_id_column(d), paste0(mt_time_column(d), "_UTC"))
      write.csv(out, file, row.names = FALSE)
    }
  )

  output$saveSummary <- downloadHandler(
    filename = "FPT_track_summary.csv",
    content = function(file) {
      out <- overview()
      f <- fptRes()
      out$radius_m <- if (is.null(f)) NA else f$radius
      write.csv(out, file, row.names = FALSE)
    }
  )

  output$saveSegments <- downloadHandler(
    filename = "FPT_segments.csv",
    content = function(file) {
      sr <- segRes()
      if (is.null(sr)) {
        write.csv(data.frame(Note = "No segments: choose a radius in the sidebar first."), file, row.names = FALSE)
        return(invisible())
      }
      out <- sr$table
      out$radius_m <- sr$radius
      out$min_segment_days <- MIN_SEG_DAYS
      write.csv(out, file, row.names = FALSE)
    }
  )

  # columns of the input data that are replaced; each is reported once in the log
  overwriteWarned <- character()

  # data must be returned. Either the unmodified input data, or the modified data by the app
  reactive({
    f <- fptRes()

    if (is.null(f)) {
      return(data)
    }

    # one checkbox per column ("Add columns to the return data"); they take effect immediately
    out <- data
    if (isTRUE(input$add_fpt_value)) out$fpt_value <- f$fptv
    if (isTRUE(input$add_fpt_segment)) out$fpt_segment <- segColumn()

    replaced <- setdiff(intersect(c("fpt_value", "fpt_segment"), names(out)), overwriteWarned)
    replaced <- replaced[replaced %in% names(data)]
    if (length(replaced) > 0) {
      logger.warn(paste0("The input data already contains the column(s) ", paste(replaced, collapse = ", "),
                         ". They are replaced by the values of this App."))
      overwriteWarned <<- c(overwriteWarned, replaced)
    }
    out
  })
}
