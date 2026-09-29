export_styled_wb <- function(df_list, sheet_names, file_path) {
    if (length(df_list) != length(sheet_names)) {
        stop("Error: Number of dataframes must match number of sheet names.")
    }
    
    wb <- wb_workbook()
    
    for (i in seq_along(df_list)) {
        df <- df_list[[i]]
        
        wb <- wb %>%
            wb_add_worksheet(sheet = sheet_names[i]) %>%
            wb_add_data(x = df) %>%
            wb_add_font(dims = "1:1", bold = TRUE) %>%
            wb_set_col_widths(cols = 1:ncol(df), widths = 18) %>%
            wb_add_cell_style(dims = wb_dims(x = df), wrap_text = "1")
    }
    
    wb_save(wb, file = file_path, overwrite = TRUE)
    message(glue::glue("Saved to: {file_path}"))
}

# Usage Example:
# export_styled_wb(
#   df_list = list(output_bu_index, output_bu_index_core),
#   sheet_names = c("BU Index", "Core BU Index"),
#   file_path = "~/Desktop/2026-09-01 -PROC- BUs Spine.xlsx"
# )