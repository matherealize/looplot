# Main functions ##############################################################

#' @title Generate data for facetted nested loop plots
#' 
#' @description 
#' This function transforms data into a format which is used in the 
#' \code{\link{nested_loop_base_plot}} function to produce basic nested loop 
#' plots. The data is basically transformed to long format with some 
#' additional information to facilitate facetting, identifying spus (see 
#' explanation in \code{\link{nested_loop_plot}}) and plotting steps for the
#' design parameters. All parameter definitions are the same as in 
#' \code{\link{nested_loop_plot}}. See details there for further usage notes.
#' 
#' @param resdf
#' Data.frame with data to be visualised in wide format with columns: 
#' *param1 param2 ... paramN measurement1 measurement2 ... measurementM*.
#' *param1* to *paramN* represent the design parameters, *measurement1* to 
#' *measurementM* the measured / summarised results for M different models / 
#' methods for the given parameters.
#' Design parameters are mostly treated as factors and can thus be ordered by 
#' the user by specifying factor levels. The only exception is the parameter
#' which is used for the x-axis - it is treated as a continuous variable.
#' @param x 
#' Name of column in resdf which defines the x-axis of the plot. Converted
#' to numeric values for x-axis via as.numeric.
#' @param grid_rows,grid_cols
#' NULL or names of columns in resdf which define the facetting rows and columns
#'  of the plot. Correspond to rows and cols argument in 
#' \code{\link{facet_grid}}. Either or both of these can be NULL - 
#' then only rows, columns or no facetting at all are done.
#' @param steps
#' NULL or character vector with names of columns in resdf which define further
#' parameter configurations and which define smallest plottable units (see 
#' Details below).  
#' @param steps_add
#' Character vector with names of columns in resdf which should be added to 
#' the plot as steps, but do not represent parameters. These are just added
#' for information and do not influence the data display. Example: show 
#' separation rate (reasonably rounded) for given parameter specifications.
#' @param methods
#' NULL or character vector with names of columns in resdf which contain 
#' results from the experimental study and should be drawn in the nested 
#' loop plot. Default NULL means 
#' that all columns not mentioned in \code{x, grid_rows, grid_cols, steps} and
#' \code{steps_add} are used. Allows to subset to only draw methods of interest.
#' @param trans
#' Function name or object, to be called via \code{do.call} to transform the
#' plotted values.
#' @param design_parameters_values
#' NULL or Named list of vectors. Each entry in the list represents one of the loop
#' variables (\code{x, grid_rows, grid_cols, steps}) in resdf. The passed values
#' here override the default, observed design parameters (i.e. the unique values of 
#' the corresponding variable in resdf). This allows to e.g. deal with 
#' missing data. Usually not necessary.
#' @param design_type
#' Either "full" or "partial". 
#' If "full", then resdf is completed to a full design, where possibly 
#' missing entries (because a specific parameter combination does not have 
#' data) are set to NA. Steps, axes etc. are then drawn as if the data
#' was available. Useful to show explicitly which scenarios have not been
#' done in the case if the design is almost full.
#'  If "partial" then parameter configurations without data are dropped from
#'  the plot and now shown.
#' @param parameter_decreasing
#' Logical - if TRUE, design parameters are sorted to be decreasing (in terms
#' of factor levels).
#' @param spu_x_shift
#' Distance between two contigous data spus. Given in units of the x-axis.
#' @param replace_labels
#' NULL or named list of character vectors which facilitates renaming of design 
#' parameter values. The names correspond to names of design parameters as 
#' specified by resdf. Each entry is a vector of the form 
#' \code{c("value_name_as_character" = "replacement_value_name")}.
#' @param methods_col_name
#' String which is used as column name for the column which encodes which 
#' method was used for which result.

#' 
#' @return 
#' Returns a named list with components 
#' \describe{
#' \item{plotdf}{Data.frame with data to be plotted.} 
#' \item{input}{List of user specified input.}
#' \item{id_columns}{Names of columns in plotdf which encode specific 
#' information, inlcuding all user specified column names which represent
#' steps, facets and results}
#' } 
#' 
#' In detail, plotdf contains columns with information on building the 
#' facet grid and the steps, labels for the plot (columns with the suffix 
#' "_labels_") and the values to be plotted (*x_coord, y_coord*). 
#' 
#' Input saves out the user input (*x, grid_cols, grid_rows, steps, 
#' parameter_decreasing*).
#' 
#' @export
nested_loop_base_data <- function(resdf,
                                  x,
                                  grid_rows = NULL, grid_cols = NULL,
                                  steps = NULL,
                                  steps_add = NULL,
                                  methods = NULL,
                                  trans = identity,
                                  design_parameters_values = NULL,
                                  design_type = "full",
                                  methods_col_name = "Method",
                                  replace_labels = NULL,
                                  spu_x_shift = 1,
                                  parameter_decreasing = FALSE) {
    
    # check facetting parameter
    # if any dimension of the grid parameters is NULL, we introduce an 
    # artificial parameter for that axis to keep the same interface
    # regardless of user input
    grid_rows_final = assert_not_null(grid_rows)
    grid_cols_final = assert_not_null(grid_cols)
    
    # save out user input
    input = list(x = x, 
                 grid_rows = grid_rows, grid_cols = grid_cols, 
                 steps = steps, steps_add = steps_add, 
                 parameter_decreasing = parameter_decreasing)
    
    # save out all important columns for later use
    id_columns = list(
        design_parameter = c(grid_rows_final, grid_cols_final, x, steps), 
        steps_parameter = c(steps, steps_add), 
        all_parameter = c(grid_rows_final, grid_cols_final, x, steps, steps_add)
    )

    # set methods to draw
    if (is.null(methods))
        methods = setdiff(names(resdf), id_columns$all_parameter)
    
    # Prepare basic data.frame with information for plotting
    # make sure it contains the facetting parameters and abides the 
    # design_type specification
    plotdf = add_facetting_parameter(resdf, 
                                     grid_rows_final, 
                                     grid_cols_final) %>% 
        add_design_parameter(id_columns$design_parameter, 
                             design_parameters_values, 
                             design_type)
    
    # include additional step layers and extract only the data of interest
    id_columns %<>% append(list(methods_col_name = methods_col_name))
    plotdf %<>%
        reshape2::melt(
            id.vars = id_columns$all_parameter, 
            measure.vars = methods,
            value.name = "y_coord",
            variable.name = methods_col_name) 
    
    # define plot-able units and add as new columns:
    # 1) facets defined by the grid variables
    # ordering: later variables change faster
    # id_columns stores column locations in form of name = integer index
    # facet is always a numeric value.
    id_columns %<>% 
        append(list(
            facet = "facet", 
            grid_rows = grid_rows_final, 
            grid_cols = grid_cols_final
            ))
    plotdf %<>% 
        group_by_at(c(id_columns$grid_rows, id_columns$grid_cols)) %>% 
        add_group_index(group_col = "facet", ungroup = TRUE)
    
    # 2) step groups
    # possibly more than one, as more than one variable can be used to 
    # define steps
    # possibly also none
    # we have stepX[index] corresponds to value of plotdf[[steps[i]]][index] 
    if (!is.null(steps))
        id_columns %<>% 
        append(list(steps = paste0("step", seq_along(steps))) )
    for (s in seq_along(steps)) {
        plotdf %<>% 
            group_by_at(steps[s]) %>% 
            add_group_index(group_col = paste0("step", s), ungroup = TRUE)
    }
    
    # additional steps which encode information on simulations
    if (!is.null(steps_add))
        id_columns %<>% 
        append(list(steps_add = paste0("step_add", seq_along(steps_add))))
           
    for (s in seq_along(steps_add)) {
        plotdf %<>% 
            group_by_at(steps_add[s]) %>% 
            add_group_index(group_col = paste0("step_add", s), ungroup = TRUE)
    }
    
    # 3) smallest plotable unit, taking facets into account as well
    # spu is defined as a contigous block of observations which can be 
    # connected via a line
    # we define a high spu as one where many variables vary and their 
    # data is connected by a line; for the lowest spu only a single variable
    # (x) varies
    # at the lowest level, spu simply combines facet + overall step to obtain a 
    # unique id, which is then converted to a numeric id
    # as above, this gives values as if a full design was used
    # to be flexible, we do this for every possible level of spus such
    # that the user can decide how to draw the data (this does NOT have
    # any effect on data management, there we always use the lowest spu 
    # layer)
    # read as: spuX corresponds to connecting data for which X variables vary.
    n_vary_param = length(steps) + 1 # +1 for entire facet connected
    id_columns %<>% append(list(spu = paste0("spu", 1:n_vary_param)))
    
    # note that the higher the number of varying variables, the less
    # steps we combine here - hence we need to reverse the order we 
    # compute the spu variables here
    # i.e. highest spu combines all data within a facet,
    # lowest spu facets and overall step identifier
    plotdf[, paste0("spu", n_vary_param)] = plotdf$facet
    for (s in seq_along(steps) %>% rev()) {
        plotdf[, paste0("spu", s)] = 
            paste0(plotdf[[paste0("spu", s + 1)]], "_", 
                   plotdf[[paste0("step", s)]])
    }
    # convert to factors and then to numeric
    # note that this can not be done in the loop above, since the 
    # spu levels depend on each other
    for (s in 1:n_vary_param) {
        plotdf[[paste0("spu", s)]] %<>% as.factor() %>% as.numeric()
    }

    # add spu coordinates
    # note that these are done for the lowest spu layer only 
    # the drawing may be affected by user input
    # these may differ per facet
    plotdf %<>% 
        group_by(facet) %>% 
        do(add_spu_shift(., x, spu_x_shift,
                         parameter_decreasing = parameter_decreasing))
    
    # labeling
    # we convert to factor to make sure that the ordering stays the same
    # as for the original variable
    # this means we need to work with factors everytime we use labels!
    id_columns %<>% append(list(
        x_labels = paste0(x, "_labels_")
    ))
    for (var in id_columns$all_parameter) 
        plotdf[[paste0(var, "_labels_")]] = plotdf[[var]] %>% 
        as.character() %>%
        forcats::as_factor()
    
    # replace grid params by labels
    # we assume they remain unique after renaming
    id_columns[["grid_rows"]] %<>% paste0("_labels_")
    id_columns[["grid_cols"]] %<>% paste0("_labels_")
    
    # replace labels
    if (!is.null(replace_labels)) {
        for (var in names(replace_labels))
            plotdf[[paste0(var, "_labels_")]] = 
                plotdf[[var]] %>% 
                replace_labels(replace_labels[[var]]) %>% 
                forcats::as_factor()
    }
    
    # transform data
    plotdf[["y_coord"]] = do.call(trans, list(plotdf[["y_coord"]]))
    
    list(plotdf = plotdf %>% ungroup(),
         input = input, 
         id_columns = id_columns)
}

#' @title Generate data for facetted nested loop plots
#' 
#' @description 
#' This function generates coordinates for plotting steps for design 
#' parameters based on the data from \code{\link{nested_loop_base_data}}. 
#' All parameter definitions are the same as in \code{\link{nested_loop_plot}}.
#' 
#' @param plot_data
#' Outout list from \code{\link{nested_loop_base_data}}.
#' @param steps_y_base
#' Numeric. Maximum height of steps in y-axis units. I.e. if steps are 
#' increasing (due to \code{parameter_decreasing == FALSE}) this represents the
#' y-axis value of the uppermost step, if \code{parameter_decreasing == TRUE} in 
#' \code{plot_data} this is the y-axis value of the first step.
#' @param steps_y_height
#' Numeric. Height of a single step in y-axis units. If a single numeric, 
#' the same height is used for all steps. If a vector, then the step heights
#' may vary for each layer (as defined by \code{steps} argument). Values
#' are cycled to have appropriate length.
#' @param steps_y_shift
#' Numeric. Distance in y-axis units between step layers, i.e. distance 
#' between step drawn for different design parameters (if \code{steps} 
#' comprises more than one variable). As \code{steps_y_height}, this 
#' can be a vector to allow varying shift between layers. If NULL, an 
#' automated attempt is made to set the value to \code{0.25*steps_y_height}, but
#' this may need manual tweaking.
#' 
#' @return 
#' Updates the list\code{plot_data}:
#' 
#' \enumerate{
#' \item Adds an entry for \code{stepdf}, which is a data.frame containing the 
#' values for the parameter steps to be plotted. 
#' \item Adds user input as entries to the list \code{input}: *steps_y_base, 
#' steps_y_shift, steps_y_height*.
#' }
#' 
#' @export
nested_loop_paramsteps_data <- function(plot_data, 
                                        steps_y_base = 0, steps_y_height = 1, 
                                        steps_y_shift = NULL) {
    
    steps_parameter = plot_data$id_columns$steps_parameter
    steps_columns = c(plot_data$id_columns$steps, 
                      plot_data$id_columns$steps_add)
    grid_rows = plot_data$id_columns$grid_rows
    grid_cols = plot_data$id_columns$grid_cols
    plotdf = plot_data$plotdf
    parameter_decreasing = plot_data$input$parameter_decreasing
    
    n_steps = length(steps_parameter)
    stepdf = NULL
    if (n_steps > 0) {
        # cycle steps_y_height and steps_y_shift
        steps_y_height = rep_len(steps_y_height, length.out = n_steps)
        if (is.null(steps_y_shift)) # set default
            steps_y_shift = 0.25 * steps_y_height[seq_along(steps_y_height[-1])]
        steps_y_shift = rep_len(steps_y_shift, length.out = n_steps - 1)
        
        stepdf = plotdf %>% 
            group_by(facet) %>%
            do(add_paramsteps_data(., steps_parameter,
                                   steps_columns = steps_columns, 
                                   grid_rows = grid_rows,
                                   grid_cols = grid_cols,
                                   steps_y_base = steps_y_base, 
                                   steps_y_height = steps_y_height,
                                   steps_y_shift = steps_y_shift)) 
    } 
    
    # add step data to plot_data
    plot_data %<>% append(list(
        stepdf = stepdf
    ))
    plot_data$input %<>% append(list(
        steps_y_base = steps_y_base,
        steps_y_shift = steps_y_shift, 
        steps_y_height = steps_y_height
    ))

    plot_data
}

#' @title Basic facetted nested loop plots
#' 
#' @description 
#' Builds basic nested loop plots from output of \code{\link{nested_loop_base_data}}.
#' This function is more flexible than the functionality offered by 
#' \code{\link{nested_loop_plot}} regarding drawing the basis of a facetted
#' nested loop plot. Usually only interesting to advanced users.
#' Most parameter definitions are the same as in \code{\link{nested_loop_plot}}.
#' 
#' @param plot_data
#' Output list from \code{\link{nested_loop_base_data}}.
#' @param grid_scales
#' Analogous to \code{scales} argument of \code{\link{facet_grid}}. 
#' @param y_name
#' Character which is used as y-axis label.
#' @param x_name 
#' Character which is used as x-axis label.
#' @param legend_name
#' String which is used as legend name.
#' @param legend_labels
#' NULL or character vector which is used as keys in legend. Overrides variable 
#' columns names in resdf.
#' @param draw
#' Named list of lists. Each entry specifies as its name a wrapper function
#' (one of \code{\link{add_points}, \link{add_lines}, \link{add_steps}}) and 
#' has as entry a list of named arguments which are passed to that wrapper
#' function via \code{do.call}. The arguments passed here correspond to the 
#' \code{point_shapes/size/alpha} and \code{line_linetypes/size/alpha} arguments
#' in \code{\link{nested_loop_plot}}.
#' @param connect_spus
#' Logical - if TRUE, individual spus are connected by lines, this is necessary
#' to reproduce original nested loop plots as suggested in the manuscript by 
#' Ruecker and Schwarzer (2014). The default FALSE means not to connect 
#' indidivual spus which often makes it easier to spot patterns in the results. 
#' @param colors
#' NULL, vector of color specification of length equal to the number of 
#' measurement columns (M) in \code{resdf}, or a function. 
#' If NULL, the viridis color scale is used (see \code{\link{viridis}}).
#' If a function, then it is expected that the function takes a single argument
#' *n* and returns a vector of color specifications of length *n* (e.g. use 
#' the \code{\link[scales]{brewer_pal}} function).
#' @param shapes 
#' Single numeric or numeric vector, specifies custom shape scale for points 
#' added to the plot. Cycled as necessary. Corresponds to the 
#' \code{point_shapes} argument in \code{\link{nested_loop_plot}}.
#' @param linetypes
#' Single numeric or numeric vector, specifies custom linetypes scale for lines 
#' added to the plot. Cycled as necessary. Corresponds to the 
#' \code{line_linetypes} argument in \code{\link{nested_loop_plot}}.
#' @param sizes
#' Single numeric or numeric vector, specifies custom sizes for lines and 
#' points added to the plot. Cycled as necessary. Note that this scale affects
#' both points and lines due to the implementation in the underlying
#' \pkg{ggplot2} package. Corresponds to the \code{sizes} argument in 
#' \code{\link{nested_loop_plot}}.
#' @param x_labels
#' If set to NULL, no labels are drawn on x-axis.
#' @param ylim
#' Vector of length 2 with limits of y-axis for the measurement data. Steps 
#' drawn (due to \code{steps_draw} TRUE) are not affected and will adapt
#' to this setting automatically.
#' @param y_breaks
#' Vector with user specified breaks of the y-axis. Default is to use the 
#' breaks as suggested by \pkg{ggplot2}.
#' @param y_labels
#' Vector with user specified labels of the y-axis. Default is to use the 
#' labels as suggested by \pkg{ggplot2}.
#' @param na_rm
#' Logical. Should missing values be removed before plotting? This means that
#' lines will be connected, even if a missing value is between two values.
#' @param base_size
#' Numeric. base_size parameter of \code{\link{theme_bw}}.
#' 
#' @return 
#' A \pkg{ggplot2} object.
#' 
#' @export
nested_loop_base_plot <- function(plot_data,
                                  grid_scales = "fixed",
                                  y_name = waiver(),
                                  x_name = waiver(),
                                  legend_name = "Method",
                                  legend_labels = NULL,
                                  draw = list(
                                      add_points = list(
                                          alpha = 1
                                      ), 
                                      add_lines = list(
                                          size = 0.5, 
                                          alpha = 1
                                      )
                                  ),
                                  connect_spus = FALSE, 
                                  colors = NULL, 
                                  shapes = 19, 
                                  linetypes = 1, 
                                  sizes = 1,
                                  x_labels = waiver(),
                                  ylim = NULL, # TODO: adapt for facet_grid_sc
                                  y_breaks = waiver(),
                                  y_labels = waiver(),
                                  na_rm = TRUE, 
                                  base_size = 12) {
    # extract plotting data
    plotdf = plot_data$plotdf
    input = plot_data$input
    id_columns = plot_data$id_columns
    grid_rows = id_columns$grid_rows
    grid_cols = id_columns$grid_cols

    if (legend_name != id_columns$methods_col_name) {
        plotdf %<>% rename(!!sym(legend_name) := id_columns$methods_col_name)
    }
    
    # number of distinct methods
    n_methods = n_distinct(plotdf[[legend_name]])
    
    # set default parameter
    if (is.null(legend_labels))
        legend_labels = waiver()
    
    if (is.null(colors)) 
        colors = viridisLite::viridis(n_methods)
    if (is.function(colors))
        colors = colors(n_methods)
    
    # extract x-axis information for labeling and breaks - per facet column
    x_axis_info = plotdf %>% 
        select(facet, x_coord,
               one_of(id_columns$x_labels, grid_cols))
    if (!is.null(grid_cols)) {
        x_axis_info %<>%
            group_by(!!sym(grid_cols)) 
    }
    x_axis_info %<>% 
        distinct(x_coord, .keep_all = TRUE) %>% 
        ungroup()
        
    # define base plot
    p = ggplot(plotdf, aes_string(x = "x_coord", y = "y_coord", 
                                  color = legend_name, 
                                  shape = legend_name, 
                                  linetype = legend_name, 
                                  size = legend_name)) + 
        theme_bw(base_size = base_size) + 
        theme(panel.grid.minor = element_blank(),
              panel.grid = element_blank())
    
    # add facetting
    # we work with facet_grid_sc to control each facet scale individually and
    # define facetting axes ourselves
    # this ensures that the plot has one scale per x- and y-facet
    # Note: nested_loop_base_data ensures that grid_rows / grid_cols exist in plotdf
    facet_scales = list()
    
    if (tolower(grid_scales) %in% c("free", "free_y")) {
        limits = NULL
    } else limits = range(plotdf$y_coord, na.rm = TRUE)
    
    for (val in unique(plotdf[[grid_rows]])) {
        facet_scales[["y"]][[val]] = scale_y_continuous(breaks = y_breaks,
                                                        labels = y_labels, 
                                                        limits = limits)
    }

    # for x-axis we need to fix up breaks and labels which are messed up
    # after shifting the x-values
    if (tolower(grid_scales) %in% c("free", "free_x")) {
        limits = NULL
        # derive labels for each facet individually
        for (val in unique(plotdf[[grid_cols]])) {
            x_axis = x_axis_info %>% 
                filter(!!sym(grid_cols) == val)
            
            if (is.null(x_labels)) {
                labels = NULL
            } else labels = x_axis[[id_columns$x_labels]]
            
            facet_scales[["x"]][[val]] = 
                scale_x_continuous(breaks = x_axis$x_coord, 
                                   labels = labels, 
                                   limits = limits)
        }
    } else {
        limits = range(plotdf$x_coord, na.rm = TRUE)
        # derive labels overall
        x_axis = x_axis_info %>% distinct(x_coord, .keep_all = TRUE)
        
        if (is.null(x_labels)) {
            labels = NULL
        } else labels = x_axis[[id_columns$x_labels]]
        
        for (val in unique(plotdf[[grid_cols]])) {
            facet_scales[["x"]][[val]] = 
                scale_x_continuous(breaks = x_axis$x_coord, 
                                   labels = labels, 
                                   limits = limits)
        }
    }
    
    p = p + 
        facet_grid_sc(eval( expr(!!sym(grid_rows) ~ !!sym(grid_cols))),
                      scales = facet_scales,
                      labeller = label_both_custom)
    
    # remove facet panel labels if they were added by nested_loop_base_data
    if (is.null(input$grid_cols))
        p = p + theme(
            strip.background.x = element_blank(), 
            strip.text.x = element_blank()
        )
    
    if (is.null(input$grid_rows))
        p = p + theme(
            strip.background.y = element_blank(), 
            strip.text.y = element_blank()
        )
    
    # draw data
    for (geom in names(draw)) {
        # draw lines / steps
        # requires spus, depending on user input
        if (grepl("lines|steps", geom)) {
            
            if (connect_spus) {
                spu_col = id_columns$spu[length(id_columns$spu)]
            } else spu_col = id_columns$spu[1]
            
            for (s in unique(plotdf[[spu_col]])) {
                spu_plotdf = plotdf %>% 
                    filter(!!sym(spu_col) == s)
                
                if (na_rm)
                    spu_plotdf %<>% filter(!is.na(y_coord))
                
                # skip ahead if empty data.frame to prevent issues when 
                # adding empty geometries to facets
                if (nrow(spu_plotdf) == 0)
                    next
                
                # add data argument to input to not connect the lines
                p = add_processing(p, 
                                   draw[geom] %>% 
                                       modify_depth(1, 
                                                    modifyList, 
                                                    list(data = spu_plotdf))
                                   )
            }
        } else {
            # drawing geoms that require no special preparation
            p = add_processing(p, draw[geom])
        }
    }
    
    # set scales and legend labels
    p = p + 
        scale_color_manual(values = rep_len(colors, n_methods), 
                               labels = legend_labels) +
        scale_shape_manual(values = rep_len(shapes, n_methods),
                           labels = legend_labels) + 
        scale_linetype_manual(values = rep_len(linetypes, n_methods),
                              labels = legend_labels) + 
        scale_size_manual(values = rep_len(sizes, n_methods), 
                   labels = legend_labels)
    
    # finalize
    p = p +
        xlab(ifelse(ggplot2:::is.waive(x_name), input$x, x_name)) + 
        ylab(y_name) +
        coord_cartesian(ylim = ylim, default = TRUE)
    
    p
}

#' @title Adds step functions for design parameters to a basic facetted nested loop plot
#' 
#' @description 
#' Adds steps for design parameters to a basic nested loop plots, i.e. output of
#'  \code{\link{nested_loop_base_plot}}.
#'  All parameter definitions are the same as in \code{\link{nested_loop_plot}}.
#'  
#' @param p
#' \pkg{ggplot2} object, output from \code{\link{nested_loop_plot}}.
#' @param plot_data
#' Output list from \code{\link{nested_loop_base_data}}.
#' @param steps_names
#' NULL or character value of same length as \code{steps}. Specifies names of the 
#' design parameters which are used for steps.
#' @param steps_names_annotate
#' Logical. Should steps drawn be annotated with names of corresponding
#' design parameters?
#' @param steps_values_annotate
#' Logical. Should steps drawn be annotated with values of corresponding
#' design parameters? Only the first occurence of a new value is annotated
#' to avoid visual clutter.
#' @param steps_color
#' Color specification for steps drawn.
#' @param steps_annotation_size
#' Numeric. Size of annotations for steps. Likely needs tweaking.
#' @param steps_annotation_nudge
#' Numeric. Fine-tune position of steps annotations in y-axis units. 
#' Often, the annotation is overlayed with the lines of the steps - this 
#' argument simply increases the distance between annotations and step lines,
#' similar to the \code{nudge_y} argument of \code{\link{geom_text}}.
#' @param steps_annotation_color
#' Color specification of the step annotation text.
#' 
#' @return 
#' An updated \pkg{ggplot2} object. (Note that this function does NOT return a 
#' deep copy of the original plot but rather updates it.)
#' 
#' @export
nested_loop_paramsteps_plot <- function(p, step_data,
                                   steps_names = NULL, 
                                   steps_names_annotate = TRUE,
                                   steps_values_annotate = FALSE,
                                   steps_color = "#AAAAAA", 
                                   steps_annotation_size = 5, # TODO split into nmes and values
                                   steps_annotation_nudge = 0.2,
                                   steps_annotation_color = "#AAAAAA" # TODO split
                                   ) {
    
    stepdf = step_data$stepdf
    steps = step_data$id_columns$steps_parameter
    parameter_decreasing = step_data$input$parameter_decreasing
    steps_y_height = step_data$input$steps_y_height
    id_columns = step_data$id_columns
    
    # check if there is anything to be done
    if (is.null(stepdf))
        return(p)
    
    # extract some information before adding steps
    # we want to keep to old axis breaks and labels, the steps should not add
    # grid lines
    p_build = ggplot_build(p)
    # note that we need to extract the parameters for EACH distinct facet panel, 
    # since they may differ when grid_scales != fixed in nested_loop_base_plot
    y_facets = names(get_facet_scales_prebuilt(p, "y"))
    # extract not all facets but distinct ones (i.e. one per row)
    ind = p_build$layout$layout %>% 
        distinct(ROW, .keep_all = TRUE) %>% 
        pull(PANEL)
    y_breaks = get_axis_major_breaks(p_build, "y")[ind]
    y_labels = get_axis_labels(p_build, "y")[ind]

    # TODO: make step height dependent on values?
    for (s in unique(stepdf$step_layer)) {
        p = p + geom_step(data = stepdf %>% filter(step_layer == s), 
                          aes(x = x_coord, y = y_coord), 
                          color = steps_color,
                          inherit.aes = FALSE)  
    }
    
    # TODO: improve display of axis, maybe using something like this
    # https://community.rstudio.com/t/ggplot-how-to-remove-axis-labels-on-selected-facets-only/13191/3
    # or by putting samplesizes into axis label or decreasing text size
    
    # do annotations
    # here we can run into clipping issues at the edges of the plot
    # y_expand_XXX should be used to resolve this
    
    # step names
    # if !parameter_decreasing: write value of variable on top, name below
    # other way around if parameter_decreasing (reason: if increasing, we have more space 
    # below the steps than on top of the steps)
    if (steps_names_annotate) {
        if (is.null(steps_names))
            steps_names = steps
        
        # starting point for steps_names
        step_name_x = min(stepdf$x_coord)
        
        # for all step layers
        for (s in seq_along(steps)) {
            # add step name
            # x coordinate always the same
            # y coord depends on parameter_decreasing
            if (parameter_decreasing) {
                step_name_y = max(stepdf$y_coord[stepdf$step_layer == s])
                vjust = 0
                sgn_nudge = 1
            } else {
                step_name_y = min(stepdf$y_coord[stepdf$step_layer == s])    
                vjust = 1
                sgn_nudge = -1
            } 
            p = p +
                annotate("text", label = steps_names[s],
                         x = step_name_x, y = step_name_y + 
                             steps_y_height[s] * steps_annotation_nudge * sgn_nudge,
                         vjust = vjust, hjust = 0,
                         size = steps_annotation_size,
                         color = steps_annotation_color)
        }
    }
    
    # step values
    # are always located to the opposite of the names to avoid collisions
    # step values are only plotted once, i.e. at the left most place step
    # that is taken
    # TODO: could be repeated at each step, but that seems too much
    # TODO: could be drawn beside the parameter name in parentheses
    if (steps_values_annotate) {
        # note that the labels to be drawn may depend on the facet
        # i.e. in a partial design not all facets have the same 
        # parameter specifications
        step_labels_ = stepdf %>% 
            group_by(facet, step_layer, y_coord) %>% 
            summarise(x_coord = min(x_coord), 
                      step_labels_ = first(step_labels_)) %>% 
            # join facetting information to produce labels adjusted to 
            # facets
            left_join(
                stepdf %>% select(
                    one_of(id_columns[["facet"]], 
                           id_columns[["grid_rows"]], 
                           id_columns[["grid_cols"]])), 
                by = "facet"
            )
        
        # placement depends on parameter_decreasing, but opposite of names
        # to properly show and not overlap with drawn steps, 
        # we nudge the labels slightly
        # we need to do that manually, since nudge_y is not an aesthetic
        # of geom_text
        if (parameter_decreasing) {
            vjust = 1
            sgn_nudge = -1
        } else {
            vjust = 0
            sgn_nudge = 1
        } 
        
        p = p + 
            geom_text(aes(x = x_coord, 
                          y = y_coord  + 
                              steps_y_height * steps_annotation_nudge * sgn_nudge, 
                          label = step_labels_), 
                      inherit.aes = FALSE,
                      data = step_labels_, 
                      vjust = vjust, hjust = 0, 
                      size = steps_annotation_size, 
                      color = steps_annotation_color)
    }
    
    # finalize
    # make sure the y-axis breaks are not changed by adding steps
    # an alternative approach is to replace the p$facet component by
    # another call to facet_grid_sc, which has to reproduce the x-axes and
    # update the y-axes
    step_limits = range(stepdf$y_coord, na.rm = TRUE)
    for (i in seq_along(y_facets)) {
        p = set_panel_axis_major_breaks(p, y_breaks[[i]], axis = "y", facet = i)
        p = set_panel_axis_labels(p, y_labels[[i]], axis = "y", facet = i)
        
        # update limits if necessary
        y_limits = get_axis_limits_prebuilt(p, axis = "y", facet = i)
        if (!is.null(y_limits))
            p = set_axis_limits_prebuilt(p, 
                                         c(min(step_limits[1], y_limits[1]), 
                                           max(step_limits[2], y_limits[2])),
                                         axis = "y", facet = i)
    }
    
    p
}

#' @title Facetted nested loop plots
#' 
#' @description 
#' Basic interface for generating nested loop plots, visualisations for 
#' (factorial) controlled experiments.
#' 
#' @param resdf
#' Data.frame with data to be visualised in wide format with columns: 
#' \code{param1 param2 ... paramN measurement1 measurement2 ... measurementM}.
#' \code{param1} to \code{paramN} represent the design parameters,
#' \code{measurement1} to \code{measurementM} the measured / summarised results 
#' for M different models / methods for the given parameters.
#' Design parameters are mostly treated as factors and can thus be ordered by 
#' the user by specifying factor levels. The only exception is the parameter
#' which is used for the x-axis - it is treated as a continuous variable.
#' @param x 
#' Name of column in resdf which defines the x-axis of the plot. Converted
#' to numeric values for x-axis via as.numeric.
#' @param grid_rows,grid_cols
#' NULL or names of columns in resdf which define the facetting rows and columns
#'  of the plot. Correspond to rows and cols argument in 
#' \code{\link{facet_grid}}. Either or both of these can be NULL - 
#' then only rows, columns or no facetting at all are done.
#' @param steps
#' NULL or character vector with names of columns in resdf which define further
#' parameter configurations and which define smallest plottable units (see 
#' Details below).  
#' @param steps_add
#' Character vector with names of columns in resdf which should be added to 
#' the plot as steps, but do not represent parameters. These are just added
#' for information and do not influence the data display. Example: show 
#' separation rate (reasonably rounded) for given parameter specifications.
#' @param methods
#' NULL or character vector with names of columns in resdf which contain 
#' results from the experimental study and should be drawn in the nested 
#' loop plot. Default NULL means 
#' that all columns not mentioned in \code{x, grid_rows, grid_cols, steps} and
#' \code{steps_add} are used. Allows to subset to only draw methods of interest.
#' @param trans
#' Function name or object, to be called via \code{do.call} to transform the
#' plotted values.
#' @param design_parameters_values
#' NULL or Named list of vectors. Each entry in the list represents one of the loop
#' variables (\code{x, grid_rows, grid_cols, steps}) in resdf. The passed values
#' here override the default, observed design parameters (i.e. the unique values of 
#' the corresponding variable in resdf). This allows to e.g. deal with 
#' missing data. Usually not necessary.
#' @param design_type
#' Either "full" or "partial". 
#' If "full", then resdf is completed to a full design, where possibly 
#' missing entries (because a specific parameter combination does not have 
#' data) are set to NA. Steps, axes etc. are then drawn as if the data
#' was available. Useful to show explicitly which scenarios have not been
#' done in the case if the design is almost full.
#'  If "partial" then parameter configurations without data are dropped from
#'  the plot and now shown.
#' @param parameter_decreasing
#' Logical - if TRUE, design parameters are sorted to be decreasing (in terms
#' of factor levels).
#' @param spu_x_shift
#' Distance between two contigous data spus. Given in units of the x-axis.
#' @param grid_scales
#' Analogous to \code{scales} argument of \code{\link{facet_grid}}. 
#' For some usage hints, see Details below.
#' @param replace_labels
#' NULL or named list of character vectors which facilitates renaming of design 
#' parameter values. The names correspond to names of design parameters as 
#' specified by resdf. Each entry is a vector of the form 
#' \code{c("value_name_as_character" = "replacement_value_name")}.
#' @param y_name
#' Character which is used as y-axis label.
#' @param x_name 
#' Character which is used as x-axis label.
#' @param steps_names
#' NULL or character value of same length as \code{steps}. Specifies names of the 
#' design parameters which are used for steps.
#' @param legend_name
#' String which is used as legend name.
#' @param legend_labels
#' NULL or character vector which is used as keys in legend. Overrides variable 
#' columns names in resdf.
#' @param connect_spus
#' Logical - if TRUE, individual spus are connected by lines, this is necessary
#' to reproduce original nested loop plots as suggested in the manuscript by 
#' Ruecker and Schwarzer (2014). The default FALSE means not to connect 
#' indidivual spus which often makes it easier to spot patterns in the results.
#' @param sizes
#' Single numeric or numeric vector, specifies custom sizes for lines and 
#' points added to the plot. Cycled as necessary. Note that this scale affects
#' both points and lines due to the implementation in the underlying
#' \pkg{ggplot2} package. See details for useage pointers.
#' @param point_shapes,point_size,point_alpha
#' Point drawing parameters. \code{point_shapes} is a vector of shape 
#' specifications of length equal to the number of measurement columns (M) in
#' \code{resdf} (cycled to appropriate length, if necessary).
#' The other drawing parameters are single numeric values. \code{point_size} 
#' may be set to NULL to make it scale with the methods (defined by \code{sizes}).
#' @param line_linetypes,line_alpha,line_size
#' Line or step drawing parameters. \code{line_linetypes} is a vector of
#' linetype specifications of length equal to the number of measurement
#' columns (M) of \code{resdf} (cycled to appropriate length, if necessary). 
#' The other drawing parameters are single numeric values. \code{line_size} 
#' may be set to NULL to make it scale with the methods (defined by \code{sizes})..
#' @param colors
#' NULL or vector of color specification of length equal to the number of 
#' measurement columns (M) in \code{resdf}. If NULL, the viridis color
#' scale is used (see \code{\link{viridis}}).
#' @param draw 
#' Character vector, which contains a combination of "add_points", "add_lines"
#' or "add_steps", which are all wrapper for \pkg{ggplot2} geoms. 
#' Defines which geometry is used to draw connected data. The default is to 
#' represent results by drawing points and lines. Original nested 
#' loop plots use "add_steps" only.
#' @param x_labels
#' If set to NULL, no labels are drawn on x-axis.
#' @param ylim
#' Vector of length 2 with limits of y-axis for the measurement data. Steps 
#' drawn (due to \code{steps_draw} TRUE) are not affected and will adapt
#' to this setting automatically.
#' @param y_expand_mult
#' Vector of length 2. Used for adjustments to the display area similar
#' to what \code{\link{expand_scale}} does. The lower limit of display
#' will be expanded by a fraction of the plotting range as given by the first 
#' entry of the vector, the upper limit by a fraction according to the second
#' entry. Useful to adjust the y-axis when steps for design parameters are drawn
#' below the results.
#' @param y_expand_add 
#' Vector of length 2. Used for adjustments to the display area similar
#' to what \code{\link{expand_scale}} does. The lower limit of display 
#' will be changed by addition of the first entry, the upper limit by addition of 
#' the second entry. Specified in y-axis coordinates. Useful to adjust the 
#' y-axis when steps for design parameters are drawn below the results.
#' @param y_breaks
#' Vector with user specified breaks of the y-axis. Default is to use the 
#' breaks as suggested by \pkg{ggplot2}.
#' @param y_labels
#' Vector with user specified labels of the y-axis. Default is to use the 
#' labels as suggested by \pkg{ggplot2}.
#' @param steps_draw
#' Logical. Should design parameters as given in \code{steps} be drawn 
#' as step-functions? Y limits will adjust automatically but proper display may
#' need manual tweaks using \code{y_expand_mult} and \code{y_expand_add}.
#' @param steps_y_base
#' Numeric. Maximum height of steps in y-axis units. I.e. if steps are 
#' increasing (due to \code{parameter_decreasing == FALSE}) this represents the
#' y-axis value of the uppermost step, if \code{parameter_decreasing == TRUE} this
#' is the y-axis value of the first step.
#' @param steps_y_height
#' Numeric. Height of a single step in y-axis units. If a single numeric, 
#' the same height is used for all steps. If a vector, then the step heights
#' may vary for each layer (as defined by \code{steps} argument). Values
#' are cycled to have appropriate length.
#' @param steps_y_shift
#' Numeric. Distance in y-axis units between step layers, i.e. distance 
#' between step drawn for different design parameters (if \code{steps} 
#' comprises more than one variable). As \code{steps_y_height}, this 
#' can be a vector to allow varying shift between layers. If NULL, an 
#' automated attempt is made to set the value to \code{0.25*steps_y_height}, but
#' this may need manual tweaking.
#' @param steps_names_annotate
#' Logical. Should steps drawn be annotated with names of corresponding
#' design parameters?
#' @param steps_values_annotate
#' Logical. Should steps drawn be annotated with values of corresponding
#' design parameters? Only the first occurence of a new value is annotated
#' to avoid visual clutter.
#' @param steps_color
#' Color specification for steps drawn.
#' @param steps_annotation_size
#' Numeric. Size of annotations for steps. Likely needs tweaking.
#' @param steps_annotation_nudge
#' Numeric. Fine-tune position of steps annotations in y-axis units. 
#' Often, the annotation is overlayed with the lines of the steps - this 
#' argument simply increases the distance between annotations and step lines,
#' similar to the \code{nudge_y} argument of \code{\link{geom_text}}.
#' @param steps_annotation_color
#' Color specification of the step annotation text.
#' @param na_rm
#' Logical. Should missing values be removed before plotting? This means that
#' lines will be connected, even if a missing value is between two values.
#' @param base_size
#' Numeric. base_size parameter of \code{\link{theme_bw}}.
#' @param hline_intercept
#' Intercept of a horizontal line which can be added to the plot (e.g. to 
#' mark a target value such as an error of 0). If NULL, no line is drawn.
#' @param hline_linetype,hline_size,hline_colour,hline_alpha
#' Aesthethic parameters for horizontal line, see \code{\link{geom_line}}.
#' @param return_data
#' Logical. Should the data necessary for drawing a plot be returned or 
#' the plot itself? Can be useful for debugging.
#' @param post_processing
#' NULL or a named list of lists. Each entry should have as name a wrapper 
#' function exported from this package and as entry a named list of parameters
#' which are passed to the wrapper function via \code{do.call}. Useful to 
#' adjust y-limts, add addtional lines or points to the plot or customize the
#' theme of the plot.
#' 
#' @details 
#' The basic data for nested loop plots are tabular data matrices in which
#' rows correspond to different experimental conditions, defined by a few key 
#' design parameters. The columns represent these design parameters as well
#' as results from a measurements conducted with several methods. 
#' All of the measurements for all of the design parameters are then displayed 
#' in a single nested loop plot. Their layout is defined by 
#' \code{x, grid_rows, grid_cols} and all the design parameter \code{step} variables.
#' 
#' A crucial defintion for these plots is a SPU (smallest / single plottable
#' unit). It is given by a subset of the measurements (rows), for which a 
#' fixed number of parameter varies (usually only 1, represented on the x-axis)
#' and all others are fixed (i.e. fixed facet row, column and step values).
#' Such SPUs are then treated as "contigous" and connected in the plot.
#' The most intuitive use-case is if the x-axis represents
#' samplesize - then a spu is all measurements for varying samplesize but 
#' fixed settings for all other parameters (i.e. fixed facet row, grid and 
#' steps).
#' If a SPU has more than one varying parameter then the \code{connect_spus}
#' argument may be used to define how they are plotted. IF the parameter is 
#' FALSE, then only results within a single individual spu are connected, but 
#' not between different spus. If TRUE, then all data within a facet is 
#' connected, effectively reproducing original nested loop plots as suggested
#' by Ruecker and Schwarzer (2014). 
#' 
#' The motivation for SPUs is visual readability of the plot - drawing a line 
#' through data that "belongs together" while separating it from data at
#' other design parameters adds clarity and makes patterns clearly
#' discernible in the plot.
#' 
#' This function works best with 4 to 6 design parameters - much more and the 
#' plots are likely to be unreadable due to information density.
#' The visualisation works best with a fractional factorial design.
#'  
#'  @section Axis scaling: 
#' The axis scaling is not fully free. It has the restrictions of 
#' \code{\link{facet_grid}} and thus:
#' \itemize{
#' \item scales = free_x allows for m scales along the bottom, and 1 common
#'  scale for all rows.
#'  \item scales = free_y allows for n scales along the side, and 1 common
#'   scale for all columns.
#'  \item scales = free allows for n scales along the side, and m scales 
#'  along the bottom.
#' }
#' Completely freeing both axes is currently not possible. Thus, the arrangement 
#' of variables may face some restrictions. The implementation of facetting
#' uses the \code{facet_grid_sc} function from the 
#' \href{https://github.com/zeehio/facetscales}{facetscales package on Github}.
#' 
#' @section Axis transformation:
#' Axis transformations are implemented by transformations of the data using
#' the \code{trans} argument. This is necessary because general transformations 
#' using the \pkg{ggplot2} \code{trans} argument of axis scales can not easily deal with
#' steps drawn for parameters. For details on how to work with axis 
#' transformations see the package vignettes. 
#' 
#' @section Size scale:
#' The size scale faces some restrictions due to the underlying \pkg{ggplot2} 
#' package. It affects ALL elements added to the plot, i.e. points and lines can 
#' not be scaled independently (except when set to fixed values). 
#' To control which elements are affected, \code{point_size} and \code{line_size} 
#' can both be set to single numeric values to fix their size and make them
#' constant accross all methods. 
#' If these arguments are set to NULL, then they will pick up on the overall 
#' size scale given by \code{sizes}.
#' 
#' @section Adding meta-information:
#' The \code{steps_add} argument can be used to provide contextual information
#' in the plot. For continous data, this could also be realized by additional
#'  measurement columns. Examples for usage include displaying the separation
#'  rate for a given parameter specifciation or labeling parameter
#'  specifications as "difficult scenario" / "easy scenario", etc. See 
#'  the package vignettes for useage examples.
#' 
#' @section Useage example:
#' Further details and usage examples may be found in the package vignettes.
#' 
#' @return 
#' If return_data is TRUE, then the outputs of \code{\link{nested_loop_base_data}} and 
#' \code{\link{nested_loop_paramsteps_data}} are combined and returned.
#' If FALSE, then a \pkg{ggplot2} plot object is returned.
#' 
#' @references 
#' Ruecker G, Schwarzer G. Presenting simulation results in a nested loop plot. 
#' BMC Med Res Methodol 2014; 14.
#' 
#' @examples 
#' \dontrun{
#' params = list(
#'   samplesize = c(10, 50, 100, 200, 500),
#'   param1 = c(1, 2), 
#'   param2 = c(1, 2, 3), 
#'   param3 = c(1, 2, 3, 4)
#'   )
#' design = expand.grid(params)
#' design$method1 = rnorm(n = nrow(design),
#'                        mean = design$param1 * design$param2 * design$param3, 
#'                        sd = 5 / design$samplesize) 
#' design$method2 = rnorm(n = nrow(design),
#'                        mean = design$param1 + design$param2 + design$param3,
#'                        sd = 5 / design$samplesize)
#' nested_loop_plot(design, x = "samplesize", 
#'             grid_rows = "param1", grid_cols = "param2", steps = "param3")
#' }
#' 
#' @export
nested_loop_plot <- function(resdf,
                             x,
                             grid_rows = NULL, grid_cols = NULL, # TODO: add grid_names as steps_names
                             steps = NULL,
                             steps_add = NULL,
                             methods = NULL,
                             trans = identity,
                             design_parameters_values = NULL,
                             design_type = "full",
                             parameter_decreasing = FALSE,
                             spu_x_shift = 1,
                             grid_scales = "fixed",
                             replace_labels = NULL,
                             y_name = waiver(),
                             x_name = waiver(),
                             steps_names = NULL,
                             legend_name = "Method",
                             legend_labels = NULL,
                             connect_spus = FALSE, 
                             sizes = 1,
                             point_shapes = 19, 
                             point_size = NULL,
                             point_alpha = 1,
                             line_linetypes = 1,
                             line_size = 0.5,
                             line_alpha = 1,
                             colors = NULL, 
                             draw = c("add_points", "add_lines"), 
                             x_labels = waiver(),
                             ylim = NULL,
                             y_expand_mult = NULL, 
                             y_expand_add = NULL,
                             y_breaks = waiver(),
                             y_labels = waiver(),
                             steps_draw = TRUE, 
                             steps_y_base = 0, 
                             steps_y_height = 1, 
                             steps_y_shift = NULL, 
                             steps_names_annotate = TRUE,
                             steps_values_annotate = FALSE,
                             steps_color = "#AAAAAA", 
                             steps_annotation_size = 5, # TODO split into nmes and values
                             steps_annotation_nudge = 0.2,
                             steps_annotation_color = "#AAAAAA", # TODO split
                             na_rm = TRUE, 
                             base_size = 12,
                             hline_intercept = NULL, 
                             hline_linetype = 3,
                             hline_size = 0.5, 
                             hline_colour = "black", 
                             hline_alpha = 1,
                             post_processing = NULL, 
                             return_data = FALSE) {
    
    # create plotting data
    plot_data = nested_loop_base_data(
        resdf, 
        x = x, grid_rows = grid_rows, grid_cols = grid_cols,
        trans = trans,
        steps = steps, steps_add = steps_add,
        design_parameters_values = design_parameters_values,
        design_type = design_type,
        methods_col_name = legend_name,
        replace_labels = replace_labels,
        spu_x_shift = spu_x_shift,
        parameter_decreasing = parameter_decreasing
    )
    
    # create baseplot based on simplified user input
    draw_list = list()
    for (geom in draw) {
        if (geom ==  "add_points") {
            draw_list[[geom]] = list(
                size = point_size, 
                alpha = point_alpha
            )
        }
        if (geom %in% c("add_lines", "add_steps")) {
            draw_list[[geom]] = list(
                size = line_size, 
                alpha = line_alpha
            )
        }
    }
    if (!return_data)
        p = nested_loop_base_plot(plot_data,
                                 grid_scales = grid_scales,
                                 y_name = y_name,
                                 x_name = x_name,
                                 legend_name = legend_name,
                                 legend_labels = legend_labels,
                                 draw = draw_list,
                                 connect_spus = connect_spus,
                                 colors = colors, 
                                 shapes = point_shapes, 
                                 linetypes = line_linetypes, 
                                 sizes = sizes,
                                 x_labels = x_labels, 
                                 ylim = ylim,
                                 y_breaks = y_breaks,
                                 y_labels = y_labels,
                                 na_rm = na_rm, 
                                 base_size = base_size)
    
    # add parameter steps 
    if (steps_draw) {
        plot_data = nested_loop_paramsteps_data(plot_data,
                                                steps_y_base = steps_y_base,
                                                steps_y_height = steps_y_height,
                                                steps_y_shift = steps_y_shift)
    } 
    
    if (return_data)
        return(plot_data)
    
    p = nested_loop_paramsteps_plot(p, plot_data, 
                               steps_names = steps_names, 
                               steps_names_annotate = steps_names_annotate,
                               steps_values_annotate = steps_values_annotate,
                               steps_color = steps_color, 
                               # TODO split into nmes and values
                               steps_annotation_size = steps_annotation_size, 
                               steps_annotation_nudge = steps_annotation_nudge,
                               steps_annotation_color = steps_annotation_color )
    
    # ylim adjustments, hline
    post_list = list(
        adjust_ylim = list(
            y_expand_add = y_expand_add, 
            y_expand_mult = y_expand_mult
        )
    )
    if (!is.null(hline_intercept)) 
        post_list[["add_abline"]] = list(
            intercept = hline_intercept, 
            linetype = hline_linetype,
            size = hline_size, 
            colour = hline_colour, 
            alpha = hline_alpha
        )
    
    p = add_processing(p, post_list)
    
    # do advanced post-processing adjustments specified by user
    p = add_processing(p, post_processing)
    
    p
}

# Plotting processer ##########################################################
#' @title Add post processing to \pkg{ggplot2} object
#' 
#' @param p
#' \pkg{ggplot2} object, output from \code{\link{nested_loop_plot}} function.
#' @param post_processing
#' List with post processing wrappers to apply to the plot. Name of an entry
#' specifies the function to call, the entry itself contains named arguments
#' for the wrapper function which are passed to \code{do.call}.
#' 
#' @details 
#' Uses \code{do.call} to call the functions. \code{p} is always passed first 
#' and does not need to be specified manually.
#' 
#' @return 
#' A \pkg{ggplot2} object.
#' 
#' @export
add_processing <- function(p, processing = NULL) {
    fct = names(processing)
    for (i in seq_along(processing)) {
        p = do.call(fct[i], 
                    list(p = p) %>% 
                        modifyList(processing[[i]]))
    }
    
    p
}

#' @title Wrapper to add points to nested loop plot
#' 
#' @description 
#' All parameters not explicitly described here are simply passed to 
#' \code{\link{geom_point}}. 
#' 
#' @param p
#' \pkg{ggplot2} object, output from \code{\link{nested_loop_plot}} function.
#' @param position
#' Position of the geometry layer, either "bottom" or "top". By default
#' points are added above all other geometries in the plot to be unobscured, 
#' but by setting position to "top", these points can also be plotted 
#' below other geometries.
#' 
#' @details 
#' Simple wrapper for \code{\link{geom_point}} to add  
#' points to nested loop plot. Useful to e.g. indicate special values.
#' 
#' @return
#' A \pkg{ggplot2} object.
#' 
#' @export
add_points <- function(p, position = "top", ...) {
    add_geom_at_position(p, geom_point(...), position)
}


#' @title Wrapper to add lines to nested loop plot
#' 
#' @description 
#' All parameters not explicitly described here are simply passed to 
#' \code{\link{geom_line}}. 
#' 
#' @param p
#' \pkg{ggplot2} object, output from \code{\link{nested_loop_plot}} function.
#' @param position
#' Position of the geometry layer, either "bottom" or "top". By default
#' lines are added below all other layers in the plot to not obscure them, 
#' but by setting position to "top", these lines can also be plotted 
#' above other lines.
#' 
#' @details 
#' Simple wrapper for \code{\link{geom_line}} to add  
#' lines to nested loop plot.
#' 
#' @return
#' A \pkg{ggplot2} object.
#' 
#' @export
add_lines <- function(p, position = "bottom", ...) {
    add_geom_at_position(p, geom_line(...), position)
}

#' @title Wrapper to add steps to nested loop plot
#' 
#' @description 
#' All parameters not explicitly described here are simply passed to 
#' \code{\link{geom_step}}. 
#' 
#' @param p
#' \pkg{ggplot2} object, output from \code{\link{nested_loop_plot}} function.
#' @param position
#' Position of the geometry layer, either "bottom" or "top". By default
#' steps are added below all other layers in the plot to not obscure them, 
#' but by setting position to "top", these steps can also be plotted 
#' above other lines.
#' 
#' @details 
#' Simple wrapper for \code{\link{geom_step}} to add  steps to nested loop plot. 
#' 
#' @return
#' A \pkg{ggplot2} object.
#' 
#' @export
add_steps <- function(p, position = "bottom", ...) {
    add_geom_at_position(p, geom_step(...), position)
}

#' @title Wrapper to add text to nested loop plot
#' 
#' @description 
#' All parameters not explicitly described here are simply passed to 
#' \code{\link{geom_text}}. 
#' 
#' @param p
#' \pkg{ggplot2} object, output from \code{\link{nested_loop_plot}} function.
#' @param decimals
#' Number of decimals to be display if called with default mapping to display
#' labels for each plotted value.
#' @param mapping
#' Aesthethics mapping specification for the text created by \code{\link{aes}}. 
#' This allows to use user-specified data as labels. Passed to 
#' \code{\link{geom_text}}. The mapping must specify the \code{label}
#' aesthetic. 
#' @param data
#' Specify other data source to use label data from. Must contain information
#' from the data.frame that is plotted and obtained from 
#' \code{\link{nested_loop_base_data}}. Passed to \code{\link{geom_text}}.
#' @param position
#' Position of the geometry layer, either "bottom" or "top". By default
#' text is added above all other layers in the plot to be unobstructed by other
#' geometries, but by setting position to "bottom", text can also be plotted 
#' below other geometries.
#' 
#' @details 
#' Simple wrapper for \code{\link{geom_text}} to add  
#' text to nested loop plot. Useful to e.g. annotate special values.
#' 
#' The most simple, default usage is to add the values of the plots (rounded
#' to two decimals). The plotted values are accessed via the \code{y_coord} variable
#' of the data.frame returned from \code{\link{nested_loop_base_data}}.
#' 
#' More advanced usage and custom labeldata can be introduced to the plots
#' by using the modular functions of this package. See the demo vignette for
#' an example.
#' 
#' @return
#' A \pkg{ggplot2} object.
#' 
#' @examples 
#' \dontrun{
#' # add values with two decimals to plot 
#' p = add_text(p, aes = aes(labels = round(values, 2)))
#' }
#' 
#' @export
add_text <- function(p, decimals = 2,  mapping = NULL, data = NULL, 
                     position = "top", ...) {
    if (is.null(mapping)) 
        mapping = aes(label = round(y_coord, decimals))
        
    add_geom_at_position(p, geom_text(mapping = mapping, data = data, ...),
                         position)
}

#' @title Wrapper to add lines to nested loop plot
#' 
#' @description 
#' All parameters not explicitly described here are simply passed to 
#' \code{\link{geom_abline}}. 
#' 
#' @param p
#' \pkg{ggplot2} object, output from \code{\link{nested_loop_plot}} function.
#' @param position
#' Position of the geometry layer, either "bottom" or "top". By default
#' lines are added below all other lines in the plot to not obscure them, 
#' but by setting position to "top", these lines can also be plotted 
#' above other lines.
#' 
#' @details 
#' Simple wrapper for \code{\link{geom_abline}} to add  
#' lines to nested loop plot. Useful to display e.g. target values in an 
#' experimental study, e.g. zero prediction error or zero bias (when used
#' with slope = 0 and intercept = 0).
#' 
#' @return
#' A \pkg{ggplot2} object.
#' 
#' @export
add_abline <- function(p, slope = 0, intercept = 0,
                       position = "bottom",
                       linetype = 3, size = 0.5, 
                      colour = "black", alpha = 1, ...) {
    add_geom_at_position(p, 
                         geom_abline(
                             slope = slope, intercept = intercept, 
                             linetype = linetype, size = size, 
                             colour = colour, alpha = alpha, ...), 
                         position)
}

#' @title Wrapper to add annotations to nested loop plot
#' 
#' @description 
#' All parameters not explicitly described here are simply passed to 
#' \code{\link{annotate}}. 
#' 
#' @param p
#' \pkg{ggplot2} object, output from \code{\link{nested_loop_plot}} function.
#' @param position
#' Position of the geometry layer, either "bottom" or "top". By default
#' lines are added below all other lines in the plot to not obscure them, 
#' but by setting position to "top", these lines can also be plotted 
#' above other lines.
#' 
#' @details 
#' Simple wrapper for \code{\link{annotate}} to add  
#' annotations to nested loop plot. Useful to display e.g. ribbons that 
#' indicate certain areas of interest within a plot.
#' 
#' @return
#' A \pkg{ggplot2} object.
#' 
#' @export
add_annotation <- function(p, position = "bottom", ...) {
    add_geom_at_position(p, annotate(...), position)
}

#' @title Wrapper to customize theme of nested loop plot
#' 
#' @description 
#' All parameters are passed to \code{\link{theme}}. 
#' 
#' @param p
#' \pkg{ggplot2} object, Output from \code{\link{nested_loop_plot}} function.
#' 
#' @return
#' A \pkg{ggplot2} object.
#' 
#' @export
add_custom_theme <- function(p, ...) {
    p + theme(...)
}

#' @title Adjust y-axis limits of \pkg{ggplot2} object
#' 
#' @description 
#' Set y-axis limits in a way which is useful for the output from 
#' \code{\link{nested_loop_plot}}. Makes functionality from
#' \code{\link{expand_scale}} available.
#' 
#' @param p
#' \pkg{ggplot2} object, Output from \code{\link{nested_loop_plot}} function.
#' @param y_expand_limits
#' A named list or a numeric vector. 
#' If a list, then the names of the list should correspond
#' to facet panels of the plot and the entries should be numeric vectors,
#' which apply the scale expansion only to the specified facets.
#' 
#' @details 
#' Often helpful to deal with clipping issue when e.g. text is not fully 
#' displayed within the plot. 
#' 
#' This function makes sure that any value in \code{y_expand_limits} is visible
#' in the plot. 
#' 
#' @return
#' A \pkg{ggplot2} object.
#' 
#' @export
expand_ylim <- function(p, y_expand_limits = 0) {
    y_facets = get_facet_scales_prebuilt(p, "y") %>% names()
    
    if (!is.list(y_expand_limits))
        y_expand_limits %<>%
        list() %>% 
        rep(length(y_facets)) %>% 
        set_names(y_facets)
    
    for (nm in y_facets) {
        limits = get_axis_limits_prebuilt(p, axis = "y", facet = nm)
        
        # in case the limits are already set, we can simply modify them
        if (!is.null(limits)) {
            for (val in y_expand_limits[[nm]])
                limits = c(min(limits[1], val), max(limits[2], val))
            p = set_axis_limits_prebuilt(p, limits, axis = "y", facet = nm)
        } else {
            # if limits are adaptive, then we need to follow the recipe of
            # ggplot2::expand_limits and add a geom_blank to the data
            data = data.frame(y = y_expand_limits[[nm]])
            data[[names(p$facet$params$rows)]] = factor(nm, levels = y_facets)
            p = p + 
                geom_blank(aes_all(names(data)), data, inherit.aes = FALSE)
        }
    }
    p
}

#' @title Adjust y-axis limits of \pkg{ggplot2} object
#' 
#' @description 
#' Set y-axis limits in a way which is useful for the output from 
#' \code{\link{nested_loop_plot}}. Makes functionality of 
#' \code{\link{expand_scale}} available.
#' 
#' @param y_expand_mult
#' NULL, a named list or a vector of length 2. 
#' Used for adjustments to the display area similar
#' to what \code{\link{expand_scale}} does. The lower limit of display
#' will be expanded by a fraction of the plotting range as given by the first 
#' entry of the vector, the upper limit by a fraction according to the second
#' entry. Useful to adjust the y-axis when steps for design parameters are drawn
#' below the results. If a list, then the names of the list should correspond
#' to facet panels of the plot and the entries should be vectors of length two,
#' which apply the scale expansion only to the specified facets.
#' If NULL, then no adjustment is done.
#' @param y_expand_add 
#' NULL, a named list or a vector of length 2. 
#' Used for adjustments to the display area similar
#' to what \code{\link{expand_scale}} does. The lower limit of display 
#' will be changed by addition of the first entry, the upper limit by addition of 
#' the second entry. Specified in y-axis coordinates. Useful to adjust the 
#' y-axis when steps for design parameters are drawn below the results.
#' If a list, then the names of the list should correspond
#' to facet panels of the plot and the entries should be vectors of length two,
#' which apply the scale expansion only to the specified facets.
#' If NULL, then no adjustment is done.
#' 
#' @details 
#' Often helpful to deal with clipping issue when e.g. text is not fully 
#' displayed within the plot. 
#' 
#' This function extends the axis range via additive or multiplicative 
#' factors.
#' 
#' Note that all numeric values are converted to absolute values (i.e. this 
#' function can not be used to shrink the y-axis).
#' 
#' @return
#' A \pkg{ggplot2} object.
#' 
#' @export
adjust_ylim <- function(p, y_expand_mult = NULL, y_expand_add = NULL) {
    
    if (packageVersion("ggplot2") > "3.2.1") {
        expander_fct = ggplot2::expansion
    } else expander_fct = ggplot2::expand_scale
    
    y_facets = get_facet_scales_prebuilt(p, "y") %>% names()
    
    if (!is.list(y_expand_mult))
        y_expand_mult %<>%
        list() %>% 
        rep(length(y_facets)) %>% 
        set_names(y_facets)
    
    if (!is.list(y_expand_add))
        y_expand_add %<>%
        list() %>% 
        rep(length(y_facets)) %>% 
        set_names(y_facets)
        
    for (nm in y_facets) {
        mult = y_expand_mult[[nm]]
        add = y_expand_add[[nm]]
        
        if (is.null(mult) & is.null(add))
            next
        
        if (is.null(mult))
            mult = 0
        
        if (is.null(add))
            add = 0
        
        p$facet$params$scales$y[[nm]]$expand = 
            expander_fct(mult = abs(mult), add = abs(add))
    }
    
    p
}

# ggplot2 helper ##############################################################
#' @title Adds \pkg{ggplot2} geom at specified z-level position
#' 
#' @noRd
add_geom_at_position <- function(p, g, position = "top") {
    if (position == "bottom") {
        p$layers = c(g, p$layers)  
    } else p = p + g
    
    p
}

#' @title Facet labeller helper
#' 
#' @description 
#' Customized label_both from \pkg{ggplot2} 3.0. All we added is a line that 
#' removes the suffix "_labels_" from the variable names passed to the 
#' labeller.
#' 
#' @noRd
label_both_custom <- function(labels, multi_line = TRUE, sep = ": ") {
    value <- ggplot2:::label_value(labels, multi_line = multi_line)
    variable <- ggplot2:::label_variable(labels, multi_line = multi_line)
    variable <- Map(gsub, "_labels_", "", variable) # customization
    if (multi_line) {
        out <- vector("list", length(value))
        for (i in seq_along(out)) {
            out[[i]] <- paste(variable[[i]], value[[i]], sep = sep)
        }
    }
    else {
        value <- do.call("paste", list(value, sep = ", "))
        variable <- do.call("paste", list(variable, sep = ", "))
        out <- Map(paste, variable, value, sep = sep)
        out <- list(unname(unlist(out)))
    }
    
    out
}

#' @title Extract scales for facets
#' 
#' @param p
#' A \pkg{ggplot2} object returned by \code{\link{nested_loop_base_plot}}.
#' @param axis
#' String, either 'x' or 'y' specifying the axis of the plot.
#' 
#' @noRd
get_facet_scales_prebuilt <- function(p, axis = "x") {
    p$facet$params$scales[[axis]]
}

#' @title Get major breaks of all panels in plot
#' 
#' @param p
#' Object returned by \code{\link{ggplot_build}}. If a \pkg{ggplot2} object, then it 
#' is first built using that function.
#' @param axis
#' String, either 'x' or 'y' specifying the axis of the plot.
#' 
#' @noRd
get_axis_major_breaks <- function(p, axis = "x") {
    p = assert_ggplot_built(p)
    
    if (packageVersion("ggplot2") > "3.2.1") {
        return(map(p$layout$panel_params, ~ .[[axis]]$get_breaks()))
    } else return(map(p$layout$panel_params, paste0(axis, ".major_source")))
}

#' @title Get axis labels of all panels in plot
#' 
#' @param p
#' Object returned by \code{\link{ggplot_build}}. If a \pkg{ggplot2} object, then it 
#' is first built using that function.
#' @param axis
#' String, either 'x' or 'y' specifying the axis of the plot.
#' 
#' @noRd
get_axis_labels <- function(p, axis = "x") {
    p = assert_ggplot_built(p)
    
    if (packageVersion("ggplot2") > "3.2.1") {
        return(map(p$layout$panel_params, ~ .[[axis]]$get_labels()))
    } else return(map(p$layout$panel_params, paste0(axis, ".labels")))
}

#' @title Get axis limits of axis in plot, before training
#' 
#' @param p
#' \pkg{ggplot2} object returned from \code{\link{nested_loop_base_plot}}.
#' @param axis
#' String, either 'x' or 'y' specifying the axis of the plot.
#' @param facet
#' Numeric or character, specifying the facet in which the axis should be 
#' modified.
#' 
#' @noRd
get_axis_limits_prebuilt <- function(p, axis = "x", facet = 1) {
    p$facet$params$scales[[axis]][[facet]]$limits
}

#' @title Set axis limits of axis in panel in plot, before training
#' 
#' @param p
#' \pkg{ggplot2} object returned from \code{\link{nested_loop_base_plot}}.
#' @param limits
#' Vector of length 2 with limits, or NULL.
#' @param axis
#' String, either 'x' or 'y' specifying the axis of the plot.
#' @param facet
#' Numeric or character, specifying the facet in which the axis should be 
#' modified.
#' 
#' @noRd
set_axis_limits_prebuilt <- function(p, limits, axis = "x", facet = 1) {
    p$facet$params$scales[[axis]][[facet]]$limits = limits
    p
}

#' @title Set major breaks in panels in plot
#' 
#' @param p
#' \pkg{ggplot2} object returned from \code{\link{nested_loop_base_plot}}.
#' @param breaks
#' Vector with breaks for axis.
#' @param axis
#' String, either 'x' or 'y' specifying the axis of the plot.
#' @param facet
#' Numeric or character, specifying the facet in which the axis should be 
#' modified.
#' 
#' @noRd
set_panel_axis_major_breaks <- function(p, breaks, axis = "x", facet = 1) {
    p$facet$params$scales[[axis]][[facet]]$breaks = breaks
    p
}

#' @title Set axis labels in panels in plot
#' 
#' @param p
#' \pkg{ggplot2} object returned from \code{\link{nested_loop_base_plot}}.
#' @param labels
#' Vector with labels for axis.
#' @param axis
#' String, either 'x' or 'y' specifying the axis of the plot.
#' @param facet
#' Numeric or character, specifying the facet in which the axis should be 
#' modified.
#' 
#' @noRd
set_panel_axis_labels <- function(p, labels, axis = "x", facet = 1) {
    p$facet$params$scales[[axis]][[facet]]$labels = labels
    p
}


#' @title Ensure that object is of class \code{ggplot_built}
#' 
#' @param p
#' \pkg{ggplot2} object.
#' 
#' @noRd
assert_ggplot_built <- function(p) {
    if (!("ggplot_built" %in% class(p)))
        p = ggplot_build(p)
    
    p
}

# Plotting data preparation ###################################################
#' @title Replace multiple strings
#' 
#' @details 
#' Simplified stringr::str_replace_all to remove the stringr dependence.
#' 
#' @noRd
replace_labels <- function(v, replacements) {
    for (nm in names(replacements)) {
        v %<>% gsub(nm, replacements[[nm]], .)
    }
    
    v
}

#' @title Helper to assert that a variable is not NULL
#' 
#' @details 
#' If a variable is NULL, we change it to an artifical, non-NULL string.
#' 
#' @noRd
assert_not_null <- function(param) {
    if (is.null(param))
        param = paste0("_", deparse(substitute(param)), "_")
    
    param
}

#' @title Ensures that all facetting paramters are available
#' 
#' @details 
#' If user does not specifiy columns or rows for the grid, we add artificial
#' gird columns to keep the same interface. We set it to a string value to 
#' ensure that facet_grid_sc works properly.
#' 
#' @noRd
add_facetting_parameter <- function(resdf, ...) {
    for (column in list(...)) {
        if (is.null(resdf[[column]])) 
            resdf[[column]] = 1
    }
    
    resdf
}

#' @title Helper to generate and add full design specification
#' 
#' @details 
#' This function adds missing parameter combinations to the results data.frame
#' when these do not occur in the data but should be shown in the plot. This
#' may be useful to generate more aesthetically pleasing and more readable
#' plots by ensuring the same parameter combinations in each drawn grid facet.
#' 
#' Also allows to override parameters by user specification.
#' 
#' @noRd
add_design_parameter <- function(resdf, 
                                 design_parameters, 
                                 design_parameters_values = NULL, 
                                 design_type = "full") {
    if (tolower(design_type) == "full") {
        # add missing parameter combinations
        param_values = resdf %>% select(one_of(design_parameters)) %>% 
            map(unique)
        
        # override by user specification
        for (pm in names(design_parameters))
            param_values[pm] = design_parameters_values[pm]
        
        resdf = expand.grid(param_values) %>% 
            left_join(resdf, by = design_parameters)
    }
    
    resdf
}

#' @title Helper to generate shifted x-coordinates for SPUs
#' 
#' @param gr
#' Single group data.frame generated by group_by and passed to do.
#' 
#' @details 
#' Meant to be used by do on a grouped data.frame.
#' All parameters except gr have the same definitions as in 
#' \code{\link{nested_loop_plot}}.
#' 
#' @noRd
add_spu_shift <- function(gr, x, spu_x_shift, 
                          parameter_decreasing = FALSE) {
    # add shifts to the x-variable per spu
    # in each panel, the first step is drawn normally. subsequent steps s+1
    # require shifting by all the earlier steps: we take the maximum 
    # x value of the last step s and add the spu_x_shift value to obtain 
    # the start of the next step s+1
    
    # find all spus on lowest layer
    spus = sort(unique(gr$spu1), decreasing = parameter_decreasing)
    gr$x_coord = as.numeric(gr[[x]])
    
    if (length(spus) == 1) 
        return(gr)
    
    for (s in 2:length(spus)) {
        # find last value of spu before current one
        last_max = gr %>% 
            filter(spu1 == spus[s - 1]) %>% 
            summarise(max_x_coord = max(x_coord)) %>%
            as.numeric()
        
        # find current spu
        ind_current = gr$spu1 == spus[s]
        
        # set current spu's x value - these are given by the 
        # last x-value of the spu before + a shift value defined
        # by the user input
        gr$x_coord[ind_current] = 
            gr$x_coord[ind_current] + 
            (last_max + spu_x_shift) - min(gr$x_coord[ind_current]) 
    }    
    
    gr
}

# steps are automatically adjusting for increasing or decreasing spus
#' @title Helper to generate steps coordinates to add to nested loop plot
#' 
#' @param gr
#' Single group data.frame generated by group_by and passed to do.
#' @param id_columns
#' List with entries column = integer that holds positions of relevant
#' columns with specific purpose in gr. Defined in \code{\link{nested_loop_base_data}}.
#' @param steps_y_height
#' Should be a vector of length equal to \code{steps}.
#' @param steps_y_shift
#' Should be a vector of length equal to length of \code{steps} - 1.
#' 
#' @details 
#' Meant to be used by do on a grouped data.frame (grouped by facets).
#' All parameters except explicitly documented have the same definitions as in 
#' nested_loop_plot.
#' 
#' @noRd
add_paramsteps_data <- function(gr, steps_parameter, steps_columns,
                                grid_rows, grid_cols, 
                                steps_y_base, steps_y_height, steps_y_shift) {
    # extract x coordinates
    facet_axisdf = gr %>%  
        distinct(x_coord, .keep_all = TRUE) %>% 
        arrange(x_coord)
    
    stepdf_list = list()
    # for all step layers
    for (s in seq_along(steps_columns)) {
        # find boundaries between steps
        step_layer = facet_axisdf[[steps_columns[s] ]]
        step_boundaries = c(0, diff(step_layer) )
        ind_boundaries = which(step_boundaries != 0)
        
        step_labels_ = paste0(steps_parameter[s], "_labels_")
        
        if (!length(ind_boundaries)) {
            # only a single value - no steps necessary
            stepdf_list[[s]] = 
                data.frame(x_coord = facet_axisdf$x_coord, 
                           y_coord = step_layer,
                           step_labels_ = facet_axisdf[[step_labels_]],
                           spu1 = facet_axisdf$spu1)
        } else {
            x_boundaries = sapply(ind_boundaries, 
                                  function(i) 
                                      (facet_axisdf$x_coord[i] +
                                           facet_axisdf$x_coord[i - 1]) / 2) 
            
            # add y coordinates - once for lower step, once for 
            # higher step
            stepdf_list[[s]] = 
                data.frame(x_coord = c(facet_axisdf$x_coord, 
                                       rep(x_boundaries, 2)), 
                           y_coord = c(step_layer, 
                                       step_layer[ind_boundaries - 1], 
                                       step_layer[ind_boundaries]),
                           # labels are factors, so we need to carefully 
                           # combine them using forcats::fct_c instead of 
                           # using only c (which yields integers, not factors)
                           step_labels_ = forcats::fct_c(facet_axisdf[[step_labels_]],
                                               facet_axisdf[[step_labels_]][ind_boundaries - 1],
                                               facet_axisdf[[step_labels_]][ind_boundaries]),
                           spu1 = c(facet_axisdf$spu1,
                                           facet_axisdf$spu1[ind_boundaries - 1], 
                                           facet_axisdf$spu1[ind_boundaries]), 
                           facet = facet_axisdf$facet[1])
            # add facetting information
            if (!is.null(grid_rows))
                stepdf_list[[s]][, grid_rows] = facet_axisdf[1, grid_rows]
            if (!is.null(grid_cols))
                stepdf_list[[s]][, grid_cols] = facet_axisdf[1, grid_cols]
        }
        
        # arranging and steps_y_height adjustment
        stepdf_list[[s]] %<>% 
            arrange(x_coord) %>% 
            mutate(y_coord = y_coord * steps_y_height[s])
        
        # define proper y height
        # first step only needs to account for steps_y_base
        # i.e. the highest step value should be at steps_y_base
        if (s > 1) {
            # each subsequent layer takes the minimum value of the 
            # earlier step layer as steps_y_base minus the steps_y_shift
            # between step layers
            steps_y_base = min(stepdf_list[[s - 1]]$y_coord) - 
                steps_y_shift[s - 1]
        }
        
        stepdf_list[[s]] %<>% 
            mutate(y_coord = y_coord - max(y_coord) + steps_y_base)
        
    }
    
    stepdf_list %>% 
        bind_rows(.id = "step_layer")
}

# dplyr helper ################################################################
#' @title  Add column with group indices as defined by dplyr::group_by
#' 
#' @noRd
add_group_index <- function(grouped_df, group_col = "group",
                            ungroup = FALSE) {
    ind = group_indices(grouped_df)
    
    if (!is.null(ind)) {
        grouped_df[, group_col] = ind
    }
    
    if (ungroup)
        grouped_df %<>% ungroup()
    
    grouped_df
}