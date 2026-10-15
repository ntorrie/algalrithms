#' Process an absorption scan from a Cary Spectrophotometer
#'
#' @param data absorption data to pass through the funciton. 
#' @param null_point_wavelength set the wavelength at which the null point occurs
#' @param path_length_m the path length of the cuvette your sample was scanned in
#' @param smoothing_window_nm your desired smoothing window. must be an odd number
#' @param method which smoothing method do you want to use. must be "savitsky_golay" or "linear"
#' @param title what distinguishing sample name do you want included in the plot title
#' 
#' @return Returns a cleaned and smoothed dataframe, and a plot showing the smoothed and unsmoothed absorption scan
#' 
#' @author Nicole Torrie
#' 
#' @export
#'


# method must be savitsky_golay or linear


al_process_cdom_absorption_scan <- function(data,
                                            null_point_wavelength = 685,
                                            path_length_m = 0.01,
                                            smoothing_window_nm = 15,
                                            method = "savitsky_golay") {
  
  # calculate the null point value
  null_point_value <- cbind_data_df$avg_sample_abs[cbind_data_df$wavelength_nm == null_point_wavelength]
  
  # clean and smooth the dataset, based on the desired smoothing method
  if(method == "savitsky_golay"){
    
    cdom_processing_df <- cbind_data_df %>% 
      mutate(blank_correction = avg_sample_abs - avg_blank_abs) %>% # subtract blank
      mutate(null_point_correction = blank_correction - null_point_value) %>% # subtract null point
      mutate(corrected_absorption = (((2.3 * null_point_correction) / path_length_m))) %>% # convert to absorption 
      mutate(corrected_absorption_smoothed = sgolayfilt(corrected_absorption, p = 2, n = smoothing_window_nm, m = 0)) %>% #smooth
      mutate(null_point_wavelength_nm = null_point_wavelength) %>% # add new columns with metadata
      mutate(path_length_m = path_length_m) %>%
      mutate(smoothing_window_nm = smoothing_window_nm) %>%
      mutate(method = method)
      
    
  } else if (method == "linear"){
    
    cdom_processing_df <- cbind_data_df %>% 
      mutate(blank_correction = avg_sample_abs - avg_blank_abs) %>% # subtract blank
      mutate(null_point_correction = blank_correction - null_point_value) %>% # subtract null point
      mutate(corrected_absorption = (((2.3 * null_point_correction) / path_length_m))) %>% # convert to absorption 
      mutate(corrected_absorption_smoothed = rollmean(corrected_absorption, k = smoothing_window_nm, fill = NA, align = "center")) %>% #smooth
      mutate(null_point_wavelength_nm = null_point_wavelength) %>% # add new columns with metadata
      mutate(path_length_m = path_length_m) %>%
      mutate(smoothing_window_nm = smoothing_window_nm) %>%
      mutate(method = method)
             
  }
  
  processed_cdom <- cdom_processing_df
  
  return(processed_cdom)
  
}





