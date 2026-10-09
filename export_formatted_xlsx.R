#' Export a list of dataframes to a formatted, multi-tab XLSX file
#'
#' @param df_list A list of dataframes to export.
#' @param tab_names A character vector of sheet names corresponding to df_list.
#' @param filepath String representing the output file path.
#'
#' @return Saves an .xlsx file to the specified filepath.
#'
export_formatted_xlsx <- function(df_list, tab_names, filepath) {
    
    # QA Check: Ensure lists are same length
    if (length(df_list) != length(tab_names)) {
        stop("Error: df_list and tab_names must be the exact same length.")
    }
    
    # Initialize openxlsx2 workbook
    wb <- wb_workbook()
    
    # Loop through dataframes and apply standard formatting
    for (i in seq_along(df_list)) {
        df <- df_list[[i]]
        
        # Standardise column names on the way out
        # any_of() ensures this won't break if a dataframe doesn't contain these specific columns
        df <- df |> 
            dplyr::rename(dplyr::any_of(c(
                "financial_year" = "fy",
                "financial_year" = "fy_end",
                "bu_name" = "business_unit",
                "bu_name" = "operating unit",
                "bu_name" = "operating_unit"
            )))
        
        sheet_name <- tab_names[i]
        
        wb <- wb %>%
            wb_add_worksheet(sheet = sheet_name) %>%
            wb_add_data(sheet = sheet_name, x = df) %>%
            # Robustly bold the header row based on exact column count
            wb_add_font(
                sheet = sheet_name, 
                dims = wb_dims(rows = 1, cols = seq_len(ncol(df))), 
                bold = TRUE
            ) %>%
            # Standardise column widths
            wb_set_col_widths(
                sheet = sheet_name, 
                cols = seq_len(ncol(df)), 
                widths = 18
            ) %>%
            # Wrap text across all populated cells (headers + data)
            wb_add_cell_style(
                sheet = sheet_name, 
                dims = wb_dims(x = df), 
                wrap_text = "1"
            )
    }
    
    # Export 
    wb_save(wb, file = filepath)
    message(glue::glue("Success: Formatted workbook saved to {filepath}"))
}

# Example usage:
# export_formatted_xlsx(
#   df_list = list(df_baseline, df_forecast),
#   tab_names = c("Baseline_RDEL", "Forecast_FTE"),
#   filepath = file.path("~/Desktop", glue::glue("BU_Returns_{Sys.Date()}.xlsx"))
# )