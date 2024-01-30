library(rENA)
library(tma)
library(lsr)
library(rcompanion)
library(ona)
library(tma)


viewover <- function (x, wh, text_col = "text", units.by = x$`_function.params`$units.by, 
                      conversation.by = x$`_function.params`$conversation.by, 
                      codes = x$rotation$codes, window_size = x$`_function.params`$window_size, 
                      more_cols = NULL, in_browser = FALSE, id_col = "QEUNIT", filename = "tmp") 
{
  unit_conv <- tma:::tma.conversations(x = x, units = wh, units.by = units.by, 
                                       conversation.by = conversation.by, codes = codes, window = window_size, 
                                       id_col = id_col)
  cols <- unique(c("QEID", id_col, units.by, conversation.by, 
                   text_col, more_cols, codes))
  cols <- cols[cols %in% colnames(x$model$raw.input)]
  tbl <- x$model$raw.input[unit_conv$convRows, cols, with = FALSE]
  unit_conv$unitRows <- unit_conv$unitRows
  unit_conv$toRemove <- unit_conv$toRemove
  unit_conv$data <- tbl
  unit_conv$units <- wh
  unit_conv$window <- window_size
  tbl_json <- jsonlite::toJSON(unit_conv, auto_unbox = TRUE)
  tmp_html <- tempfile(fileext = ".html")
  html_lines <- readLines(system.file(package = "tma", paste0("apps/viewer.html")))
  html_lines[grepl("//_ENA_MODEL_//", x = html_lines)] <- paste0("data = ", 
                                                                 tbl_json, ";")
  writeLines(text = html_lines, con = paste0(filename,".html"))
  cat(paste0("<iframe src='",filename,".html' style='width:100%;height:600px;'></iframe>"));
}

comodin<- function (x, y, ...) 
{
  p <- ona:::base_plot(...)
  attr(p, "model") <- x
  args <- list(...)
  dimensions <- colnames(as.matrix(x$points))[1:2]
  if (!is.null(args$dimensions)) {
    dimensions <- args$dimensions
  }
  attr(p, "dimensions") <- dimensions
  p
}

plot.ena.ordered.set <- function(x, y,...) {
  p<- comodin(x, y,...)
  class(p) <- c("plotly", "enaplot", "html-fill-item-overflow-hidden", "html-fill-item", class(p))
  p
}

nodes <- function (x, ..., weighted_nodes = TRUE, node_size_multiplier = 1, 
          node_color = "#3E4750", node_sizes = 1, node_direction = 2, 
          self_connections = weighted_nodes, self_connection_color = "#000000", 
          nodes_include_self = TRUE, node_halo = 0, self_connection_halo = 0, 
          node_labels = TRUE, node_position_multiplier = 1, node_saturation_multiplier = 1, 
          sender_direction = 1, weights = NULL, nodes = NULL) {
  p <- NULL
  if (is(x, "plotly")) {
    p <- x
    x <- attr(x = p, which = "model")
    if (is.null(weights)) {
      weights <- attr(p, "edge_weights")
    }
  }
  if (is.null(weights)) {
    weights <- colMeans(as.matrix(x$line.weights$ENA_DIRECTION$response))
  }
  if (is.null(nodes)) {
    nodes <- data.table::copy(x$rotation$nodes)
  }
  else {
    nodes <- data.table::copy(nodes)
  }
  raw_weights <- weights
  raw_sq_weights_raw <- ona:::to_square(raw_weights)
  raw_sq_weights <- raw_weights * node_size_multiplier
  weights <- abs(weights)
  dimensions <- attr(p, "dimensions")
  sq_weights_raw <- ona:::to_square(weights)
  sq_weights <- sq_weights_raw * node_size_multiplier
  if (nodes_include_self == TRUE) {
    node_sizes_raw <- (apply(sq_weights_raw, node_direction, 
                             sum))
    node_sizes <- (apply(sq_weights, node_direction, sum))
  }
  else {
    node_sizes_raw <- (apply(sq_weights_raw, node_direction, 
                             sum) - diag(sq_weights_raw))
    node_sizes <- (apply(sq_weights, node_direction, sum) - 
                     diag(sq_weights))
  }
  node_sizes[node_sizes == 0] <- 0.001
  nodes[, `:=`((seq(2, ncol(nodes))), lapply(.SD, `*`, node_position_multiplier)), 
        .SDcols = (seq(2, ncol(nodes)))]
  dim_indices <- which(colnames(as.matrix(nodes)) %in% dimensions)
  edge_table <- ona:::create_edge_matrix(NULL, weights = raw_weights, 
                                   nodes = nodes, direction = sender_direction, dims = dim_indices, 
                                   node_position_multiplier = 1)
  nodes_xy <- attr(edge_table, "nodes_xy")
  annotations <- list()
  node_traces <- list()
  if (weighted_nodes) {
    shape = list(type = "circle", xanchor = "x", yanchor = "y", 
                 xref = "x", yref = "y", line = list(color = "#FFFFFF", 
                                                     width = node_halo))
    if (length(node_color) == 1) {
      node_color <- base::rep(node_color, nrow(sq_weights))
    }
    if (self_connections == TRUE) {
      if (length(self_connection_color) == 1) {
        self_connection_color <- base::rep(self_connection_color, 
                                     nrow(sq_weights))
      }
      else if (length(self_connection_color) == 2) {
        self_connection_color <- self_connection_color[((diag(raw_sq_weights_raw) < 
                                                           0) * 1) + 1]
      }
    }
    for (i in seq(nrow(sq_weights))) {
      if (node_sizes[i] > 0) {
        shape$line$width = node_halo
        node <- nodes_xy[i, ]
        node_radius <- node_sizes[i]/2
        edge_line <- ona:::edge_fun(rbind(as.matrix(node), 
                                    c(0, 0)), fun = LearnGeom::CreateLinePoints)
        node_pts <- LearnGeom::IntersectLineCircle(edge_line, 
                                                   as.matrix(node), node_radius)
        shape$x0 <- node$x - (node_radius)
        shape$y0 <- node$y - (node_radius)
        shape$x1 <- node$x + (node_radius)
        shape$y1 <- node$y + (node_radius)
        shape$fillcolor <- paste0(node_color[i], ona:::alpha_to_hex(node_saturation_multiplier))
        node_traces <- c(node_traces, list(shape))
        if (node_labels == TRUE) {
          annotations <- c(annotations, list(list(x = shape$x1, 
                                                  y = shape$y1, text = node$code, xref = "x", 
                                                  yref = "y", showarrow = FALSE, arrowhead = 0, 
                                                  ax = 0, ay = -60)))
        }
        if (self_connections) {
          shape$line$width = self_connection_halo
          shape$x0 <- node$x - (diag(sq_weights)[i]/2)
          shape$y0 <- node$y - (diag(sq_weights)[i]/2)
          shape$x1 <- node$x + (diag(sq_weights)[i]/2)
          shape$y1 <- node$y + (diag(sq_weights)[i]/2)
          shape$fillcolor <- hsv(0, 0, 1, 1)
          node_traces <- c(node_traces, list(shape))
          shape$x0 <- node$x - (diag(sq_weights)[i]/2)
          shape$y0 <- node$y - (diag(sq_weights)[i]/2)
          shape$x1 <- node$x + (diag(sq_weights)[i]/2)
          shape$y1 <- node$y + (diag(sq_weights)[i]/2)
          this_color_hsv <- as.list(t(grDevices::rgb2hsv(grDevices::col2rgb(self_connection_color[i]))))
          names(this_color_hsv) <- c("h", "s", "v")
          this_color_hsv$alpha <- diag(sq_weights_raw)[i]
          shape$fillcolor <- do.call(grDevices::hsv, 
                                     this_color_hsv)
          node_traces <- c(node_traces, list(shape))
        }
      }
    }
    attr(x = p, which = "annotations") <- c(attr(x = p, 
                                                 which = "annotations"), annotations)
    attr(x = p, which = "shapes") <- c(attr(x = p, which = "shapes"), 
                                       node_traces)
  }
  else {
    nodes_xy$size <- 15
    p <- plotly::add_trace(p = p, data = nodes_xy, x = ~x, 
                           y = ~y, text = ~code, type = "scatter", mode = "markers+text", 
                           textposition = "bottom center", name = "Nodes", 
                           marker = list(sizemode = "diameter", size = ~size, 
                                         color = "black", line = list(width = 0)))
  }
  
  class(p) <- c("plotly", "enaplot", "html-fill-item-overflow-hidden", "html-fill-item", class(p))
  p
}


edges <- function (x, ..., show_edge_center = FALSE, units = NULL, 
                   edge_color = c("#FF0000",  "#0000FF"), edge_end = "outward", 
                   desaturate_edges_by = sqrt, 
                  lighten_edges_by = desaturate_edges_by, multiplier = 1, 
          edge_size_multiplier = multiplier, edges_behind_nodes = TRUE, 
          sender_direction = 1, scale_edges_to = c(0, 1), scale_edges_from = NULL, 
          edge_arrow = TRUE, edge_arrow_offset = 2, edge_arrow_width = 0.1, 
          edge_arrows_to_show = "max", edge_arrow_saturation_multiplier = 1.1, 
          fake_alpha = TRUE, edge_halo = 0.5, edge_halo_multiplier = 1.1, 
          arrow_halo = 0, edge_arrow_direction = 2, node_position_multiplier = 1, 
          weights = NULL, nodes = NULL) 
{
  .SD <- NULL
  p <- NULL
  if (is(x, "plotly")) {
    p <- x
    x <- attr(x = p, which = "model")
  }
  if (is.null(weights)) {
    weights <- colMeans(as.matrix(x$line.weights$ENA_DIRECTION$response))
  }
  else if (!is.numeric(weights)) {
    weights <- colMeans(as.matrix(weights[ENA_DIRECTION == 
                                            "response", ]))
  }
  if (is.null(nodes)) {
    nodes <- data.table::copy(x$rotation$nodes)
  }
  else {
    nodes <- data.table::copy(nodes)
  }
  nodes[, `:=`((seq(2, ncol(nodes))), lapply(.SD, `*`, node_position_multiplier)), 
        .SDcols = (seq(2, ncol(nodes)))]
  raw_weights <- weights
  raw_sq_weights_raw <- ona:::to_square(raw_weights)
  raw_sq_weights <- raw_weights * multiplier
  weights <- abs(weights)
  if (is.null(scale_edges_from)) {
    if (!is.null(model)) {
      scale_edges_from <- c(0, max(as.matrix(x$line.weights$ENA_DIRECTION$response)))
    }
    else {
      scale_edges_from <- c(0, max(raw_weights))
    }
  }
  sq_weights_raw <- ona:::to_square(weights)
  sq_weights <- sq_weights_raw * multiplier
  edge_table <- ona:::create_edge_matrix(NULL, weights = raw_weights, 
                                   nodes = nodes, direction = sender_direction)
  nodes_xy <- attr(edge_table, "nodes_xy")
  edge_path_list <- list()
  nodes_xy <- attr(edge_table, "nodes_xy")
  node_combinations <- attr(edge_table, "node_combinations")
  edge_path_list <- ona:::edge_paths(edge_table, edge_color = edge_color, 
                               edge_end = edge_end, desaturate_edges_by = desaturate_edges_by, 
                               lighten_edges_by = lighten_edges_by, edge_size_multiplier = edge_size_multiplier, 
                               scale_edges_to = scale_edges_to, scale_edges_from = scale_edges_from, 
                               edge_arrow = edge_arrow, edge_arrow_offset = edge_arrow_offset, 
                               edge_arrow_width = edge_arrow_width, edge_arrow_saturation_multiplier = edge_arrow_saturation_multiplier, 
                               fake_alpha = fake_alpha, edge_halo = edge_halo, edge_arrows_to_show = edge_arrows_to_show, 
                               arrow_halo = arrow_halo, edge_halo_multiplier = edge_halo_multiplier, 
                               sender_direction = sender_direction, edge_arrow_direction = edge_arrow_direction)
  if (show_edge_center) {
    p <- plotly::add_markers(p = p, data = edge_table, x = ~weighted_midpoint_x, 
                             y = ~weighted_midpoint_y, marker = list(color = "black"), 
                             name = ~label)
    p <- plotly::add_markers(p = p, data = edge_table, x = ~midpoint_x, 
                             y = ~midpoint_y, marker = list(color = "black"), 
                             name = ~label)
  }
  attr(x = p, which = "shapes") <- c(attr(x = p, which = "shapes"), 
                                     edge_path_list)
  attr(x = p, which = "edge_weights") <- raw_weights
  class(p) <- c("plotly", "enaplot", "html-fill-item-overflow-hidden", "html-fill-item", class(p))
  p
}


ena.plotter2 <- function (set, groupVar = NULL, groups = NULL, points = FALSE, 
          mean = FALSE, network = TRUE, networkMultiplier = 1, subtractionMultiplier = 1, 
          unit = NULL, print.plots = F, ...) 
{
  data = set$connection.counts
  if (is.null(unit) == FALSE) {
    plot = rENA:::ena.plot(enaset = set, title = unit)
    if (any(set$points$ENA_UNIT == unit) == FALSE) {
      stop("Unit does not exist!")
    }
    point.row = set$points$ENA_UNIT == unit
    point = as.matrix(set$points)[point.row, ]
    point.lw = as.matrix(set$line.weights)[point.row, ] * 
      networkMultiplier
    plot = rENA:::ena.plot.points(enaplot = plot, points = point, 
                           colors = "black")
    plot = rENA:::ena.plot.network(enaplot = plot, network = point.lw, 
                            colors = "black")
    set$plots[[length(set$plots) + 1]] <- plot
    class(plot) <- c("plotly", "enaplot", "html-fill-item-overflow-hidden", "html-fill-item", class(plot))

    if (print.plots == TRUE) {
      print(set$plots)
    }
    return(set)
  }
  if (is.null(groupVar) == TRUE) {
    plot = rENA:::ena.plot(enaset = set, title = "All Units")
    if (network == TRUE) {
      lineweights = as.matrix(set$line.weights)
      mean.lineweights = colMeans(lineweights) * networkMultiplier
      plot = rENA:::ena.plot.network(plot, network = mean.lineweights, 
                              colors = "black")
      class(plot) <- c("plotly", "enaplot", "html-fill-item-overflow-hidden", "html-fill-item", class(plot))
      
    }
    if (points == TRUE) {
      points.for.plot = as.matrix(set$points)
      plot = rENA:::ena.plot.points(enaplot = plot, points = points.for.plot, 
                             colors = "black")
      class(plot) <- c("plotly", "enaplot", "html-fill-item-overflow-hidden", "html-fill-item", class(plot))
      
    }
    if (mean == TRUE) {
      points.for.plot = as.matrix(set$points)
      plot = rENA:::ena.plot.group(plot, points.for.plot, colors = "black", 
                            labels = "Mean", confidence.interval = "box")
      class(plot) <- c("plotly", "enaplot", "html-fill-item-overflow-hidden", "html-fill-item", class(plot))
      
    }
    else if (TRUE %in% c(network, points, mean) == FALSE) {
      stop("You must set at least one of points, mean, or network to TRUE to obtain a plot.")
    }
    set$plots[[length(set$plots) + 1]] <- plot
    if (print.plots == TRUE) {
      print(set$plots)
    }
    return(set)
  }
  else if (is.null(groups) == TRUE) {
    unique.groups = unique(data[[groupVar]])
    if (length(unique.groups) == 1) {
      warning("No groups specified and group variable only contains one unique value. Generating plot for one group.")
      group = unique.groups
      group.rows = set$points[[groupVar]] == group
      g.plot = rENA:::ena.plot(enaset = set, title = group)

      if (network == TRUE) {
        g.lw = as.matrix(set$line.weights)[group.rows, 
                                           , drop = FALSE]
        g.mean.lw = colMeans(g.lw) * networkMultiplier
        g.plot = rENA:::ena.plot.network(g.plot, network = g.mean.lw, 
                                  colors = "black")
              class(g.plot) <- c("plotly", "enaplot", "html-fill-item-overflow-hidden", "html-fill-item", class(g.plot))

      }
      if (points == TRUE) {
        g.points.for.plot = as.matrix(set$points)[group.rows, 
                                                  , drop = FALSE]
        g.plot = rENA:::ena.plot.points(enaplot = g.plot, points = g.points.for.plot, 
                                 colors = "black")
      }
      if (mean == TRUE) {
        g.points.for.plot = as.matrix(set$points)[group.rows, 
                                                  , drop = FALSE]
        g.plot = rENA:::ena.plot.group(g.plot, g.points.for.plot, 
                                colors = "black", labels = group, confidence.interval = "box")
      }
      else if (TRUE %in% c(network, points, mean) == FALSE) {
        stop("You must set at least one of points, mean, or network to TRUE to obtain a plot.")
      }
      class(g.plot) <- c("plotly", "enaplot", "html-fill-item-overflow-hidden", "html-fill-item", class(g.plot))
      
      set$plots[[length(set$plots) + 1]] <- g.plot
      if (print.plots == TRUE) {
        print(set$plots)
      }
      return(set)
    }
    else {
      group1 = unique.groups[1]
      group2 = unique.groups[2]
      warning(paste0("No groups specified. Generating plots of first two unique values of group variable: ", 
                     group1, " and ", group2))
      set = rENA:::ena.plot.subtraction(set = set, groupVar = groupVar, 
                                 group1 = group1, group2 = group2, points = points, 
                                 mean = mean, network = network, networkMultiplier = networkMultiplier, 
                                 subtractionMultiplier = subtractionMultiplier)
      if (print.plots == TRUE) {
        print(set$plots)
      }
      return(set)
    }
  }
  else if (length(groups) == 1) {
    group = groups
    if (any(data[[groupVar]] == group) == FALSE) {
      stop("Group column does not contain group1 value!")
    }
    group.rows = set$points[[groupVar]] == group
    g.plot = ena.plot(enaset = set, title = group)
    if (network == TRUE) {
      g.lw = as.matrix(set$line.weights)[group.rows, , 
                                         drop = FALSE]
      g.mean.lw = colMeans(g.lw) * networkMultiplier
      g.plot = rENA:::ena.plot.network(g.plot, network = g.mean.lw, 
                                colors = "black")
    }
    if (points == TRUE) {
      g.points.for.plot = as.matrix(set$points)[group.rows, 
                                                , drop = FALSE]
      g.plot = rENA:::ena.plot.points(enaplot = g.plot, points = g.points.for.plot, 
                               colors = "black")
    }
    if (mean == TRUE) {
      g.points.for.plot = as.matrix(set$points)[group.rows, 
                                                , drop = FALSE]
      g.plot = rENA:::ena.plot.group(g.plot, g.points.for.plot, 
                              colors = "black", labels = group, confidence.interval = "box")
    }
    else if (TRUE %in% c(network, points, mean) == FALSE) {
      stop("You must set at least one of points, mean, or network to TRUE to obtain a plot.")
    }
    class(g.plot) <- c("plotly", "enaplot", "html-fill-item-overflow-hidden", "html-fill-item", class(g.plot))
    
    set$plots[[length(set$plots) + 1]] <- g.plot
    if (print.plots == TRUE) {
      print(set$plots)
    }
    return(set)
  }
  else if (length(groups) >= 2) {
    if (length(groups) > 2) {
      warning(paste0("More than two groups specified. Plotting the first two groups: ", 
                     groups))
    }
    groups.missing = groups[which(!groups %in% data[[groupVar]])]
    if (length(groups.missing) > 0) {
      stop(paste0("Group column does not contain group value(s): ", 
                  groups[groups.missing]))
    }
    set = rENA:::ena.plot.subtraction(set = set, groupVar = groupVar, 
                               group1 = groups[1], group2 = groups[2], points = points, 
                               mean = mean, network = network, networkMultiplier = networkMultiplier, 
                               subtractionMultiplier = subtractionMultiplier, ...)
    if (print.plots == TRUE) {
      print(set$plots)
    }
    return(set)
  }
}
