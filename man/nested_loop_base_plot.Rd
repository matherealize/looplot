% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nested_loop.R
\name{nested_loop_base_plot}
\alias{nested_loop_base_plot}
\title{Basic facetted nested loop plots}
\usage{
nested_loop_base_plot(
  plot_data,
  grid_scales = "fixed",
  y_name = waiver(),
  x_name = waiver(),
  legend_name = "Method",
  legend_breaks = waiver(),
  legend_labels = NULL,
  labeller = label_both_custom,
  draw = list(add_points = list(alpha = 1), add_lines = list(size = 0.5, alpha = 1)),
  connect_spus = FALSE,
  colors = NULL,
  shapes = 19,
  linetypes = 1,
  sizes = 1,
  x_labels = waiver(),
  ylim = NULL,
  y_breaks = waiver(),
  y_labels = waiver(),
  na_rm = TRUE,
  base_size = 12
)
}
\arguments{
\item{plot_data}{Output list from \code{\link{nested_loop_base_data}}.}

\item{grid_scales}{Analogous to \code{scales} argument of \code{\link{facet_grid}}.}

\item{y_name}{Character which is used as y-axis label.}

\item{x_name}{Character which is used as x-axis label.}

\item{legend_name}{String which is used as legend name.}

\item{legend_breaks}{A character vector of breaks for the scales. Can be used to e.g. exclude
certain methods from the legend. If NULL, then no breaks are displayed in the
legend. Otherwise, must be the same length as \code{legend_labels}, 
if one of them is changed from the default.}

\item{legend_labels}{NULL or character vector which is used as keys in legend. Overrides variable 
columns names in resdf. Must be the same length as \code{legend_breaks}, 
if one of them is changed from the default.}

\item{labeller}{Labeller function to format strip labels of the facet grid. By default 
uses a custom version of \code{ggplot2::label_both}, but can be customized
according to \code{\link{labeller}}.}

\item{draw}{Named list of lists. Each entry specifies as its name a wrapper function
(one of \code{\link{add_points}, \link{add_lines}, \link{add_steps}}) and 
has as entry a list of named arguments which are passed to that wrapper
function via \code{do.call}. The arguments passed here correspond to the 
\code{point_shapes/size/alpha} and \code{line_linetypes/size/alpha} arguments
in \code{\link{nested_loop_plot}}.}

\item{connect_spus}{Logical - if TRUE, individual spus are connected by lines, this is necessary
to reproduce original nested loop plots as suggested in the manuscript by 
Ruecker and Schwarzer (2014). The default FALSE means not to connect 
indidivual spus which often makes it easier to spot patterns in the results.}

\item{colors}{NULL, vector of color specification of length equal to the number of 
measurement columns (M) in \code{resdf}, or a function. 
If NULL, the viridis color scale is used (see \code{\link{viridis}}).
If a function, then it is expected that the function takes a single argument
*n* and returns a vector of color specifications of length *n* (e.g. use 
the \code{\link[scales]{brewer_pal}} function).}

\item{shapes}{Single numeric or numeric vector, specifies custom shape scale for points 
added to the plot. Cycled as necessary. Corresponds to the 
\code{point_shapes} argument in \code{\link{nested_loop_plot}}.}

\item{linetypes}{Single numeric or numeric vector, specifies custom linetypes scale for lines 
added to the plot. Cycled as necessary. Corresponds to the 
\code{line_linetypes} argument in \code{\link{nested_loop_plot}}.}

\item{sizes}{Single numeric or numeric vector, specifies custom sizes for lines and 
points added to the plot. Cycled as necessary. Note that this scale affects
both points and lines due to the implementation in the underlying
\pkg{ggplot2} package. Corresponds to the \code{sizes} argument in 
\code{\link{nested_loop_plot}}.}

\item{x_labels}{If set to NULL, no labels are drawn on x-axis.}

\item{ylim}{Vector of length 2 with limits of y-axis for the measurement data. Steps 
drawn (due to \code{steps_draw} TRUE) are not affected and will adapt
to this setting automatically.}

\item{y_breaks}{Vector with user specified breaks of the y-axis. Default is to use the 
breaks as suggested by \pkg{ggplot2}.}

\item{y_labels}{Vector with user specified labels of the y-axis. Default is to use the 
labels as suggested by \pkg{ggplot2}.}

\item{na_rm}{Logical. Should missing values be removed before plotting? This means that
lines will be connected, even if a missing value is between two values.}

\item{base_size}{Numeric. base_size parameter of \code{\link{theme_bw}}.}
}
\value{
A \pkg{ggplot2} object.
}
\description{
Builds basic nested loop plots from output of \code{\link{nested_loop_base_data}}.
This function is more flexible than the functionality offered by 
\code{\link{nested_loop_plot}} regarding drawing the basis of a facetted
nested loop plot. Usually only interesting to advanced users.
Most parameter definitions are the same as in \code{\link{nested_loop_plot}}.
}
