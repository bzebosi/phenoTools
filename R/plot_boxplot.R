plot_boxplot <- function(
    data, x_col = "Genotype", y_col = "Value",
    fill_col = x_col, y_label = NULL, 
    x_label = NULL, title = NULL, y_limits = NULL, colors = NULL, box_alpha = 1, 
    box_fill = TRUE, box_linewidth = 1, jitter_size = 3, jitter_width = 0.2, 
    jitter_shape = 16, outlier_shape = NA, show_points = TRUE,
    ...
) {
  
  # Create the base boxplot
    if (box_fill){
    p <- ggplot2::ggplot(
      data, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[fill_col]])) + 
      ggplot2::geom_boxplot(alpha = box_alpha, linewidth = box_linewidth,
                            outlier.shape = outlier_shape, color = "black")
    # Apply a manual fill scale when colors are supplied
    if (!is.null(colors)) { p <- p + ggplot2::scale_fill_manual(values = colors) }
    
    } else {
      p <- ggplot2::ggplot(
        data, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]],
                           color = .data[[fill_col]])) +
        ggplot2::geom_boxplot(alpha = box_alpha, linewidth = box_linewidth, 
                                     fill = NA, outlier.shape = outlier_shape)
      # Apply a manual fill scale when colors are supplied
      if (!is.null(colors)) { 
        p <- p + ggplot2::scale_color_manual(values = colors)
      }
    }
  
  # Add jittered observations
  if (show_points) {
    p <- p + ggplot2::geom_jitter(shape = jitter_shape, 
      size = jitter_size, width = jitter_width)
    }
  
  # Set the visible y-axis range
  if (!is.null(y_limits)) { 
    p <- p + ggplot2::coord_cartesian(ylim = y_limits) 
    }
  
  # Add plot labels and apply the custom theme
  p + ggplot2::labs(title = title, x = x_label, y = y_label) +
    plot_theme(...)
}
