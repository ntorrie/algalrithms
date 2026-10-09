#' Process an absorption scan from a Cary Spectrophotometer
#'
#' @param null_point_wavelength set the wavelength at which the null point occurs
#' @param path_length_m the path length of the cuvette your sample was scanned in
#' @param smoothing_window_nm your desired smoothing window. must be an odd number
#' @param method which smoothing method do you want to use. must be "savitsky_golay" or "linear"
#' 
#' @return Returns a cleaned and smoothed dataframe, and a plot showing the smoothed and unsmoothed absorption scan
#' 
#' @author Nicole Torrie
#' 
#' @export
#'


# method must be savitsky_golay or linear


al_process_cdom_absorption_scan <- function(data = data,
                                            null_point_wavelength = 685,
                                            path_length_m = 0.1,
                                            smoothing_window_nm = 15,
                                            method = "savitsky_golay") {
  
  # calculate the null point value
  null_point_value <- cbind_data_df$avg_sample_abs[cbind_data_df$`Wavelength (nm)` == null_point_wavelength]
  
  # clean and smooth the dataset, based on the desired smoothing method
  if(method == "savitsky_golay"){
    
    cdom_processing_df <- cbind_data_df %>% 
      mutate(blank_correction = avg_sample_abs - avg_blank_abs) %>% # subtract blank
      mutate(null_point_correction = blank_correction - null_point_value) %>% # subtract null point
      mutate(corrected_absorption = (((2.3 * null_point_correction) / path_length_m))) %>% # convert to absorption 
      mutate(corrected_absorption_smoothed = sgolayfilt(corrected_absorption, p = 2, n = smoothing_window_nm, m = 0)) #smooth
    
    cdom_processing_df
             
   ggtitle = "Savitsky Golay Smoothed"
    
  } else if (method == "linear"){
    
    cdom_processing_df <- cbind_data_df %>% 
      mutate(blank_correction = avg_sample_abs - avg_blank_abs) %>% # subtract blank
      mutate(null_point_correction = blank_correction - null_point_value) %>% # subtract null point
      mutate(corrected_absorption = (((2.3 * null_point_correction) / path_length_m))) %>% # convert to absorption 
      mutate(corrected_absorption_smoothed = rollmean(corrected_absorption, k = smoothing_window_nm, fill = NA, align = "center")) #smooth
             
    cdom_processing_df
    
    ggtitle = "Linear Smoothed"
  }
  
  # plot the absorption scan
  ggplot(cdom_processing_df, aes(x = `Wavelength (nm)`)) +
    geom_line(linewidth = 1, aes(y = corrected_absorption, color = "No Smoothing")) +
    geom_line(linewidth = 0.5, aes(y = corrected_absorption_smoothed, color = "Smoothed")) + 
    scale_color_manual(values = c("No Smoothing" = "pink", "Smoothed" = "blue")) +
    labs(color = "Legend", y = "Absorption") +
    theme_minimal() +
    ggtitle(ggtitle)
  
}





# apply corrections
# set desired null point wavelength
# set cuvette path length
# null_point_wavelength <- 685
# path_length_m <- 0.1
# smoothing_window_nm <- 15
# tail_length_nm <- 7
# 
# 
# null_point_value <- cbind_data_df$avg_sample_abs[cbind_data_df$`Wavelength (nm)` == null_point_wavelength]
# 
# cdom_processing_df <- cbind_data_df %>% 
#   mutate(blank_correction = avg_sample_abs - avg_blank_abs) %>% # subtract blank
#   mutate(null_point_correction = blank_correction - null_point_value) %>% # subtract null point
#   mutate(corrected_absorption = (((2.3 * null_point_correction) / path_length_m))) %>% # convert to absorption 
#   mutate(corrected_absorption_smoothed_linear = rollmean(corrected_absorption, k = smoothing_window_nm, fill = NA, align = "center")) %>%
#   mutate(corrected_absorption_smoothed_savitzky_golay = sgolayfilt(corrected_absorption, p = 2, n = smoothing_window_nm, m = 0)) 
# 
# 
# 
# 
# ggplot(cdom_processing_df, aes(x = `Wavelength (nm)`)) +
#   geom_line(linewidth = 1, aes(y = corrected_absorption, color = "No Smoothing")) +
#   geom_line(linewidth = 0.5, aes(y = corrected_absorption_smoothed_linear, color = "Linear Smoothing")) + 
#   geom_line(linewidth = 0.5, aes(y = corrected_absorption_smoothed_savitzky_golay, color = "Savitzky-golay Smoothing")) + 
#   scale_color_manual(values = c("No Smoothing" = "pink", "Linear Smoothing" = "blue", "Savitzky-golay Smoothing" = "red")) +
#   labs(color = "Legend", y = "Absorption") +
#   theme_minimal()




