W2E <- function(mat) {
  edge_list <- which(mat != 0, arr.ind = TRUE)
  return(data.frame(from = edge_list[, 1], to = edge_list[, 2], weight = mat[edge_list]))
}


usemplot <- function (beta, var.number, labels, title = ""){
  p <- var.number
  contemporaneous.relations <- matrix(beta[(p + 1):(2 * p), 
                                           (p + 1):(2 * p)], nrow = p, ncol = p, byrow = F)
  lag.1.relations <- matrix(beta[(p + 1):(2 * p), 1:p], nrow = p, 
                            ncol = p, byrow = F)
  econtemporaneous <- W2E(t(contemporaneous.relations))
  elag1 <- W2E(t(lag.1.relations))
  plot.names <- 1:var.number
  if (nrow(rbind(elag1, econtemporaneous)) > var.number) {
    isLagged <- c(rep(TRUE, nrow(elag1)), rep(FALSE, nrow(econtemporaneous)))
    curve <- rep(1, length(isLagged))
    qgraph(rbind(elag1, econtemporaneous), layout = "circle", 
           lty = ifelse(isLagged, 2, 1), edge.labels = F, curve = curve, 
           fade = FALSE, posCol = "green", negCol = "red", title = title,
           labels = labels, label.cex = 1, label.norm = "O", 
           label.scale = FALSE, edge.label.cex = 5, edge.label.position = 0.3, 
           edge.width = 1)
  }
  return(NULL)
}


detrender <- function(data, vars, timevar, groupvar = NULL, verbose = TRUE) {
  detrend_func <- function(df) {
    df %>%
      mutate(across(all_of(vars), 
                    ~ .x, 
                    .names = "{col}_bkp")) %>%
      mutate(across(all_of(vars), 
                    ~ {
                      tryCatch({
                        ff <- as.formula(paste0(cur_column(), " ~ ", timevar))
                        fit <- lm(ff, data = df)
                        p_value <- anova(fit)$'Pr(>F)'[1]
                        if (p_value < 0.05) {
                          if (verbose) message(paste("Detrending variable", cur_column(), "- p-value:", p_value))
                          res <- residuals(fit)
                          if (length(res) != nrow(df)) {
                            if (verbose) warning(paste("Mismatch in residuals length for", cur_column(), ". Using original values."))
                            .x
                          } else {
                            res
                          }
                        } else {
                          if (verbose) message(paste("No significant trend for", cur_column(), "- p-value:", p_value))
                          .x
                        }
                      }, error = function(e) {
                        warning(paste("Error detrending", cur_column(), ":", e$message))
                        .x
                      })
                    }, .names = "{col}_detrended"))
  }
  
  if (!is.null(groupvar)) {
    data <- data %>%
      group_by(across(all_of(groupvar))) %>%
      group_modify(~ {
        result <- detrend_func(.x)
        # Ensure the result has the same number of rows as the input
        if (nrow(result) != nrow(.x)) {
          warning(paste("Mismatch in row count for group", paste(cur_group(), collapse = ", ")))
          .x  # Return original data for this group
        } else {
          result
        }
      }) %>%
      ungroup()
  } else {
    data <- detrend_func(data)
  }
  
  return(data)
}