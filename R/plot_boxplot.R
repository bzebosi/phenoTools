plot_boxplot <- function(
    data, x_col = "Genotype", y_col = "Value",
    fill_col = x_col, y_label = NULL, 
    x_label = NULL, title = NULL, y_limits = NULL, colors = NULL, 
    box_alpha = 1, box_fill = TRUE, box_linewidth = 1, box_width = 0.55,
    jitter_size = 2.5, jitter_width = 0.12, jitter_shape = 16, jitter_alpha = 0.80,
    dodge_width = 0.8, outlier_shape = NA, show_points = TRUE, flip = FALSE,
    ...
) {
  
  # Determine whether multiple groups occur within each x-axis category.
  grouped_plot <- !identical(x_col, fill_col)
  
  if (grouped_plot) {
    box_position <- ggplot2::position_dodge(width = dodge_width)
    point_position <- ggplot2::position_jitterdodge(jitter.width = jitter_width,
                                                    dodge.width = dodge_width)
  } else {
    box_position <- "identity"
    point_position <- ggplot2::position_jitter(width = jitter_width)
  }
  
  # Create the base boxplot
  if (box_fill){
    p <- ggplot2::ggplot(
      data, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[fill_col]])) + 
      ggplot2::geom_boxplot(alpha = box_alpha, linewidth = box_linewidth,
                            position = box_position, width = box_width,
                            outlier.shape = outlier_shape, color = "black")
    # Apply a manual fill scale when colors are supplied
    if (!is.null(colors)) { p <- p + ggplot2::scale_fill_manual(values = colors) }
    
    } else {
      p <- ggplot2::ggplot(
        data, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]],
                           color = .data[[fill_col]])) +
        ggplot2::geom_boxplot(alpha = box_alpha, linewidth = box_linewidth,
                              position = box_position, width = box_width,
                              fill = NA, outlier.shape = outlier_shape)
      # Apply a manual fill scale when colors are supplied
      if (!is.null(colors)) { 
        p <- p + ggplot2::scale_color_manual(values = colors)
      }
    }
  
  # Add jittered observations
  if (show_points) {
    p <- p + ggplot2::geom_point(shape = jitter_shape, position = point_position,
                                 alpha = jitter_alpha, size = jitter_size)
  }
  
  # Set coordinate system and visible y-axis range
  if (flip) {
    p <- p + ggplot2::coord_flip(ylim = y_limits)
  } else if (!is.null(y_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = y_limits)
  }
  
  # Add plot labels and apply the custom theme
  p + ggplot2::labs(title = title, x = x_label, y = y_label) +
    plot_theme(...)
}
