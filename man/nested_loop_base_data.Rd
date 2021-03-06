% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nested_loop.R
\name{nested_loop_base_data}
\alias{nested_loop_base_data}
\title{Generate data for facetted nested loop plots}
\usage{
nested_loop_base_data(
  resdf,
  x,
  grid_rows = NULL,
  grid_cols = NULL,
  steps = NULL,
  steps_add = NULL,
  methods = NULL,
  pass_through = NULL,
  trans = identity,
  design_parameters_values = NULL,
  design_type = "full",
  methods_col_name = "Method",
  replace_labels = NULL,
  spu_x_shift = 1,
  parameter_decreasing = FALSE
)
}
\arguments{
\item{resdf}{Data.frame with data to be visualised in wide format with columns: 
*param1 param2 ... paramN measurement1 measurement2 ... measurementM*.
*param1* to *paramN* represent the design parameters, *measurement1* to 
*measurementM* the measured / summarised results for M different models / 
methods for the given parameters.
Design parameters are mostly treated as factors and can thus be ordered by 
the user by specifying factor levels. The only exception is the parameter
which is used for the x-axis - it is treated as a continuous variable.}

\item{x}{Name of column in resdf which defines the x-axis of the plot. Converted
to numeric values for x-axis via as.numeric.}

\item{grid_rows, grid_cols}{NULL or names of columns in resdf which define the facetting rows and columns
 of the plot. Correspond to rows and cols argument in 
\code{\link{facet_grid}}. Either or both of these can be NULL - 
then only rows, columns or no facetting at all are done.}

\item{steps}{NULL or character vector with names of columns in resdf which define further
parameter configurations and which define smallest plottable units (see 
Details below).}

\item{steps_add}{Character vector with names of columns in resdf which should be added to 
the plot as steps, but do not represent parameters. These are just added
for information and do not influence the data display. Example: show 
separation rate (reasonably rounded) for given parameter specifications.}

\item{methods}{NULL or character vector with names of columns in resdf which contain 
results from the experimental study and should be drawn in the nested 
loop plot. Default NULL means 
that all columns not mentioned in \code{x, grid_rows, grid_cols, steps} and
\code{steps_add} are used. Allows to subset to only draw methods of interest.}

\item{pass_through}{NULL or character vector with names of columns in resdf which will be passed
to post-processing, without otherwise affecting the plot. Useful to add e.g.
panel specific decorations (see corresponding section in the Gallery 
vignette of this package).}

\item{trans}{Function name or object, to be called via \code{do.call} to transform the
plotted values.}

\item{design_parameters_values}{NULL or Named list of vectors. Each entry in the list represents one of the loop
variables (\code{x, grid_rows, grid_cols, steps}) in resdf. The passed values
here override the default, observed design parameters (i.e. the unique values of 
the corresponding variable in resdf). This allows to e.g. deal with 
missing data. Usually not necessary.}

\item{design_type}{Either "full" or "partial". 
If "full", then resdf is completed to a full design, where possibly 
missing entries (because a specific parameter combination does not have 
data) are set to NA. Steps, axes etc. are then drawn as if the data
was available. Useful to show explicitly which scenarios have not been
done in the case if the design is almost full.
 If "partial" then parameter configurations without data are dropped from
 the plot and now shown.}

\item{methods_col_name}{String which is used as column name for the column which encodes which 
method was used for which result.}

\item{replace_labels}{NULL or named list of character vectors which facilitates renaming of design 
parameter values. The names correspond to names of design parameters as 
specified by resdf. Each entry is a vector of the form 
\code{c("value_name_as_character" = "replacement_value_name")}.}

\item{spu_x_shift}{Distance between two contigous data spus. Given in units of the x-axis.}

\item{parameter_decreasing}{Logical - if TRUE, design parameters are sorted to be decreasing (in terms
of factor levels).}
}
\value{
Returns a named list with components 
\describe{
\item{plotdf}{Data.frame with data to be plotted.} 
\item{input}{List of user specified input.}
\item{id_columns}{Names of columns in plotdf which encode specific 
information, inlcuding all user specified column names which represent
steps, facets and results}
} 

In detail, plotdf contains columns with information on building the 
facet grid and the steps, labels for the plot (columns with the suffix 
"_labels_") and the values to be plotted (*x_coord, y_coord*). 

Input saves out the user input (*x, grid_cols, grid_rows, steps, 
parameter_decreasing*).
}
\description{
This function transforms data into a format which is used in the 
\code{\link{nested_loop_base_plot}} function to produce basic nested loop 
plots. The data is basically transformed to long format with some 
additional information to facilitate facetting, identifying spus (see 
explanation in \code{\link{nested_loop_plot}}) and plotting steps for the
design parameters. All parameter definitions are the same as in 
\code{\link{nested_loop_plot}}. See details there for further usage notes.
}
