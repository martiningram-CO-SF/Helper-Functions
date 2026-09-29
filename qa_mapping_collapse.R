#' @title QA Check for Business Unit Mapping Collapses and Value Variances
#'
#' @description 
#' Compares a pre- and post-standardisation dataframe to detect silent data corruption 
#' during business unit name cleaning. Flags cases where multiple original BU names 
#' merge into a single clean BU name ("Super BUs"), or where numeric baseline 
#' values (e.g., RDEL, FTE) mutate during processing, such as summing/aggregating.
#' 
#' @details 
#' Assumes a 1:1 row alignment between `df1` and `df2`. If the function detects a failure, 
#' it automatically assigns the flagged data to `qa_super_bus_flagged` in the Global Environment 
#' for immediate inspection.
#'
#' @param df1 Dataframe. The original dataset (pre-mapping).
#' @param df2 Dataframe. The processed dataset (post-mapping). Must retain the exact row order of df1.
#' @param df1_names Character. Column name containing the original BU names in df1.
#' @param df2_names Character. Column name containing the standardised BU names in df2.
#' @param df1_numbers Character. Column name containing the numeric values (e.g., baselines) in df1.
#' @param df2_numbers Character. Column name containing the numeric values in df2.
#'
#' @return Invisibly returns a tibble (`collapse_check`) containing any flagged rows 
#'   with their calculated variances. Prints a PASS/FAIL summary to the console.
#' @export
#'
#' @examples
#' \dontrun{
#' super_bus <- qa_mapping_collapse(
#'   df1 = df_raw, 
#'   df2 = df_clean, 
#'   df1_names = "BU_Name", 
#'   df2_names = "bu_name_clean", 
#'   df1_numbers = "RDEL_Value", 
#'   df2_numbers = "RDEL_Value"
#' )
#' }

library(dplyr)

qa_mapping_collapse <- function(df1, df2, df1_names, df2_names, df1_numbers, df2_numbers) {
    
    qa_df <- tibble(
        orig_name = df1[[df1_names]],
        clean_name = df2[[df2_names]],
        val_pre = as.numeric(df1[[df1_numbers]]),
        val_post = as.numeric(df2[[df2_numbers]])
    )
    
    collapse_check <- qa_df |> 
        group_by(clean_name) |> 
        summarise(
            n_orig_names = n_distinct(orig_name),
            merged_names = paste(unique(orig_name), collapse = " | "),
            total_val_pre = sum(val_pre, na.rm = TRUE),
            total_val_post = sum(val_post, na.rm = TRUE),
            .groups = "drop"
        ) |> 
        # Calculate variance BEFORE filtering
        mutate(value_variance = total_val_post - total_val_pre) |> 
        # Trigger if multiple names collapsed OR if values changed
        filter(n_orig_names > 1 | abs(value_variance) > 0.001) |> 
        arrange(desc(n_orig_names), desc(abs(value_variance)))
    
    # --- QA Output Console Messages ---
    div <- paste0(rep("=", 60), collapse = "")
    n_flags <- nrow(collapse_check)
    
    if (n_flags == 0) {
        message(paste0(
            "\n", div, "\n",
            "QA MAPPING COLLAPSE SUMMARY: PASS\n",
            div, "\n",
            "0 'Super BUs' created and 0 numeric variances found.\n",
            "Baselines remain distinct and values match perfectly.\n",
            div, "\n"
        ))
    } else {
        assign("qa_super_bus_flagged", collapse_check, envir = .GlobalEnv)
        message(paste0(
            "\n", div, "\n",
            "QA MAPPING COLLAPSE SUMMARY: FAIL\n",
            div, "\n",
            "WARNING: ", n_flags, " row(s) flagged for structural collapse (Super BUs) OR numeric variance.\n",
            "This risks artificially inflating/deflating the dataset or baseline values.\n",
            div, "\n",
            "-> Affected rows exported to global environment as 'qa_super_bus_flagged'\n",
            div, "\n"
        ))
    }
    
    return(invisible(collapse_check))
}
# Usage: 
# super_bus <- qa_mapping_collapse(df_raw, df_clean, "BU_Name", "bu_name_clean", "RDEL_Value", "RDEL_Value")