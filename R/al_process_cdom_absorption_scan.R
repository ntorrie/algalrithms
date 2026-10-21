#' Process an absorption scan from Cary Spectrophotometer
#'
#' @param data absorption data to pass through the funciton. 
#' @param null_point_wavelength set the wavelength at which the null point occurs
#' @param anchor_wavelength set the wavelength at which to anchor the data for normalization
#' @param path_length_m the path length of the cuvette your sample was scanned in
#' @param smoothing_window_nm your desired smoothing window. must be an odd number
#' @param derivative_window_nm your desired window for generating a slope for the derivatives
#' @param method which smoothing method do you want to use. must be "savitsky_golay" or "linear"
#' @param title what distinguishing sample name do you want included in the plot title
#' 
#' @return Returns a cleaned and smoothed dataframe, with columns for derivative analysis
#' 
#' @author Nicole Torrie
#' 
#' @export
#'


# method must be savitsky_golay or linear
# input dataframe must contain the following columns: wavelength_nm, avg_sample_abs, avg_blank_abs

al_process_cdom_absorption_scan <- function(data = cbind_data_df,
                                            null_point_wavelength = 685,
                                            anchor_wavelength = 220,
                                            path_length_m = 0.01,
                                            smoothing_window_nm = 15,
                                            derivative_window_nm = 7,
                                            method = "savitsky_golay") {
  
# look up the null point value
null_point_value <- cbind_data_df$avg_sample_abs[cbind_data_df$wavelength_nm == null_point_wavelength]

cdom_processing_df <- cbind_data_df %>% 
  # correct and smooth the data
  mutate(blank_correction = avg_sample_abs - avg_blank_abs) %>% # subtract blank
  mutate(null_point_correction = blank_correction - null_point_value) %>% # subtract null point
  mutate(corrected_absorption = (((2.3 * null_point_correction) / path_length_m))) %>% # convert to absorption 
  mutate(corrected_absorption_smoothed = case_when(  
    method == "savitsky_golay" ~ sgolayfilt(corrected_absorption, p = 2, n = smoothing_window_nm, m = 0),
    method == "linear" ~ rollmean(corrected_absorption, k = smoothing_window_nm, fill = NA, align = "center"))) %>% #smooth based on desired method
  mutate(anchor_value = corrected_absorption_smoothed[wavelength_nm == anchor_wavelength]) %>% # look up the anchor value
  mutate(corrected_absorption_smoothed_normalized = corrected_absorption_smoothed / anchor_value) %>% # normalize the spectra based on the anchor wavelength
  
  # add new columns with metadata
  mutate(anchor_wavelength_nm = anchor_wavelength) %>%
  mutate(null_point_wavelength_nm = null_point_wavelength) %>% 
  mutate(path_length_m = path_length_m) %>%
  mutate(smoothing_window_nm = smoothing_window_nm) %>%
  mutate(method = method) %>%
 
 # add derivative columns
  mutate(
    first_derivative = (lead(corrected_absorption_smoothed_normalized, ((derivative_window_nm - 1) / 2)) - lag(corrected_absorption_smoothed_normalized, ((derivative_window_nm - 1) / 2))) /
                       (lead(wavelength_nm, ((derivative_window_nm - 1) / 2)) - lag(wavelength_nm, ((derivative_window_nm - 1) / 2)))) %>%
  mutate(
    second_derivative = (lead(first_derivative, ((derivative_window_nm - 1) / 2)) - lag(first_derivative, ((derivative_window_nm - 1) / 2))) /
                        (lead(wavelength_nm, ((derivative_window_nm - 1) / 2)) - lag(wavelength_nm, ((derivative_window_nm - 1) / 2)))) %>%
  mutate(
    third_derivative = (lead(second_derivative, ((derivative_window_nm - 1) / 2)) - lag(second_derivative, ((derivative_window_nm - 1) / 2))) /
                       (lead(wavelength_nm, ((derivative_window_nm - 1) / 2)) - lag(wavelength_nm, ((derivative_window_nm - 1) / 2)))) %>%
  mutate(
    fourth_derivative = (lead(third_derivative, ((derivative_window_nm - 1) / 2)) - lag(third_derivative, ((derivative_window_nm - 1) / 2))) /
                        (lead(wavelength_nm, ((derivative_window_nm - 1) / 2)) - lag(wavelength_nm, ((derivative_window_nm - 1) / 2))))
    
     
  processed_cdom <- cdom_processing_df
         
  return(processed_cdom)
         
  }
  
