# Extension of ggplot2 facet_grid to allow individual scales. Adapted from
# https://github.com/zeehio/facetscales for internal use in this package.
# Copyright (C) 2019 Sergio Oller 
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#' Lay out panels in a grid with different scales
#'
#' `facet_grid_sc` is a variant of `facet_grid`
#' @inheritParams ggplot2::facet_grid
#' @param scales A list of two elements (`x` and `y`). Each element can be either
#' `"fixed"` (scale limits shared across facets), `"free"` (with varying limits per facet), or
#'  a named list, with a different scale for each facet value. Previous scale values
#'  (`"fixed"`, `"free_x"`, `"free_y"`, `"free"` are accepted but soft-deprecated).
#'  
#' @noRd
#' @export
facet_grid_sc <- function(rows = NULL, cols = NULL, scales = "fixed",
                          space = "fixed", shrink = TRUE,
                          labeller = "label_value", as.table = TRUE,
                          switch = NULL, drop = TRUE, margins = FALSE,
                          facets = NULL) {
  # `facets` is soft-deprecated and renamed to `rows`
  if (!is.null(facets)) {
    rows <- facets
  }
  # Should become a warning in a future release
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }

  if (is.list(scales)) {
    free <- list(
      x = identical(scales$x, "free") || is.list(scales$x),
      y = identical(scales$y, "free") || is.list(scales$y)
    )
  } else {
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
    free <- list(
      x = any(scales %in% c("free_x", "free")),
      y = any(scales %in% c("free_y", "free"))
    )
  }

  custom_scales <- list(x = NULL, y = NULL)
  if (is.list(scales)) {
    # A different scale per facet:
    if (is.list(scales$x)) {
      if (is.null(names(scales$x))) {
        stop("Custom facet scales for x should be named according to facet column values", call. = FALSE)
      }
      custom_scales$x <- scales$x
    }
    if (is.list(scales$y)) {
      if (is.null(names(scales$y))) {
        stop("Custom facet scales for y should be named according to facet row values", call. = FALSE)
      }
      custom_scales$y <- scales$y
    }
  }

  space <- match.arg(space, c("fixed", "free_x", "free_y", "free"))
  space_free <- list(
    x = any(space %in% c("free_x", "free")),
    y = any(space %in% c("free_y", "free"))
  )

  if (!is.null(switch) && !switch %in% c("both", "x", "y")) {
    stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
  }

  facets_list <- ggplot2:::grid_as_facets_list(rows, cols)

  # Check for deprecated labellers
  # Adapted to ggplot version 3.4+ in Dec 2025
  if (is.list(labeller)) {
      labeller <- ggplot2::labeller(.rows = labeller, .cols = labeller)
  } else if (is.character(labeller) && length(labeller) == 1) {
      labeller <- ggplot2::labeller(.default = match.fun(labeller))
  } else if (is.function(labeller)) {
      labeller <- ggplot2::labeller(.default = labeller)
  } else {
      labeller <- ggplot2::label_value
  }

  ggproto(NULL, FacetGridScales,
          shrink = shrink,
          params = list(rows = facets_list$rows, cols = facets_list$cols, margins = margins,
                        scales = custom_scales,
                        free = free, space_free = space_free, labeller = labeller,
                        as.table = as.table, switch = switch, drop = drop)
  )
}


#' ggproto facet
#'
#' @export
FacetGridScales <- ggproto(
  "FacetGridScales", FacetGrid,
  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
    scales <- list()
    if (!is.null(params$scales$x)) {
      facet_x_names <- unique(as.character(layout[[names(params$cols)]]))
      scales$x <- lapply(params$scales$x[facet_x_names], function(x) {
        new <- x$clone()
        new$oob <- function(x, ...) x
        new
      })
    } else if (!is.null(x_scale)) {
      scales$x <- lapply(seq_len(max(layout$SCALE_X)), function(i) x_scale$clone())
    }
    if (!is.null(params$scales$y)) {
      facet_y_names <- unique(as.character(layout[[names(params$rows)]]))
      scales$y <- lapply(params$scales$y[facet_y_names], function(x){
        new <- x$clone()
        new$oob <- function(x, ...) x
        new
      })
    } else if (!is.null(y_scale)) {
      scales$y <- lapply(seq_len(max(layout$SCALE_Y)), function(i) y_scale$clone())
    }
    scales
  },
  train_scales = function(x_scales, y_scales, layout, data, params, self) {
    # Transform data first
    data <- lapply(data, function(layer_data) {
      self$finish_data(layer_data, layout,
                       x_scales, y_scales, params)
    })

    # Then use parental method for scale training
    ggproto_parent(Facet, self)$train_scales(x_scales, y_scales,
                                             layout, data, params)
  },
  finish_data = function(data, layout, x_scales, y_scales, params) {
    # Divide data by panel
    panels <- split(data, data$PANEL, drop = FALSE)
    panels <- lapply(names(panels), function(i) {
      dat  <- panels[[i]]

      # Match panel to their scales
      panel_id <- match(as.numeric(i), layout$PANEL)
      xidx <- layout[panel_id, "SCALE_X"]
      yidx <- layout[panel_id, "SCALE_Y"]

      # Decide what variables need to be transformed
      y_vars <- intersect(y_scales[[yidx]]$aesthetics, names(dat))
      x_vars <- intersect(x_scales[[xidx]]$aesthetics, names(dat))

      # Transform variables by appropriate scale
      for (j in y_vars) {
        dat[, j] <- y_scales[[yidx]]$transform(dat[, j])
      }
      for (j in x_vars) {
        dat[, j] <- x_scales[[xidx]]$transform(dat[, j])
      }
      dat
    })

    # Recombine the data
    data <- unsplit(panels, data$PANEL)
    data
  }
)
