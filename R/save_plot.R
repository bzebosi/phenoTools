
save_plot <- function(
    plot, plots_dir = "plots", file_name, device = "pdf",
    width = 8, height = 6, dpi = 300) {
  
  # Create output directory if it does not exist
  if (!dir.exists(plots_dir)) { dir.create(plots_dir, recursive = TRUE) }
  
  # Construct output file name
  date_tag <- format(Sys.Date(), "%Y%m%d")
  file_path <- file.path(plots_dir, paste0(date_tag, "_", file_name, ".", device))
  
  # Save plot
  ggplot2::ggsave(filename = file_path, plot = plot, device = device, 
                  width = width, height = height, dpi = dpi)
  
  # Get full path
  full_path <- normalizePath(file_path, mustWork = FALSE)
  
  message("Plot saved to: ", full_path)
  
  invisible(file_path)
}
