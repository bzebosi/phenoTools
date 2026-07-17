plot_theme <- function(
    base_size = 30, base_family = "", legend_position = "none", x_text_angle = 45, 
    base_theme = ggplot2::theme_linedraw, strip_fill = "grey85", 
    show_major_grid = TRUE, show_minor_grid = FALSE, text_face = "bold", 
    base_linewidth = NULL, line_colour = "black", grid_colour = "grey90"){
  
  # Internal spacing settings
  space_unit = "cm"
  strip_margin = 1
  panel_spacing <- 2
  
  # scale line widths
  if (is.null(base_linewidth)) { base_linewidth <- base_size / 20 }
  
  # Adjust x-axis label
  x_text_hjust <- if (x_text_angle == 0) 0.5 else 1
  
  # create major grid-line settings
  major_grid <- if (show_major_grid) {
    ggplot2::element_line(colour = grid_colour, linewidth = base_linewidth * 0.4)
  } else {
    ggplot2::element_blank()
  }
  
  # Create minor grid-line settings
  minor_grid <- if (show_minor_grid) {
    ggplot2::element_line(colour = grid_colour, linewidth = base_linewidth * 0.2)
  } else {
    ggplot2::element_blank()
  }
  
  # Start from the selected complete theme
  base_theme(base_size = base_size, base_family = base_family) +
    
    ggplot2::theme(
      # Shared text appearance
      text = ggplot2::element_text(face = text_face, colour = line_colour),
      
      # Plot title
      plot.title = ggplot2::element_text(size = base_size, hjust = 0.5),
      
      # Axis text
      axis.text.x = ggplot2::element_text(
        angle = x_text_angle, hjust = x_text_hjust, size = base_size * 0.7),
      
      axis.text.y = ggplot2::element_text(size = base_size),
      
      # Axis titles
      axis.title = ggplot2::element_text(size = base_size),
      
      # Axis lines and ticks
      axis.line = ggplot2::element_line(colour = line_colour, 
                                        linewidth = base_linewidth),
      
      axis.ticks = ggplot2::element_line(colour = line_colour, 
                                         linewidth = base_linewidth),
    
      axis.ticks.length = grid::unit(base_linewidth * 0.3, space_unit),
      
      # Panel
      panel.border = ggplot2::element_rect(
        colour = line_colour, linewidth = base_linewidth * 1.5, fill = NA),
      
      panel.spacing = grid::unit(panel_spacing,"lines"),
      panel.grid.major = major_grid, panel.grid.minor = minor_grid,
      
      # Facet strips
      strip.background = ggplot2::element_rect(
        fill = strip_fill, colour = line_colour, linewidth = base_linewidth * 1.5),
      
      strip.text = ggplot2::element_text(size = base_size),
      
      # facet-strip padding
      strip.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = strip_margin, r = 0, b = strip_margin, 
                                 l = 0, unit = space_unit)),
      
      strip.text.y = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, r = strip_margin, b = 0, 
                                 l = strip_margin, unit = space_unit)),
      
      # Legend
      legend.position = legend_position,
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size * 0.7)
    )
}