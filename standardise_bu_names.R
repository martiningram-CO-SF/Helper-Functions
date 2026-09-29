library(dplyr)
library(readxl)

path_dict <- "~/Google Drive/My Drive/CO - Finance - Financial Strategy (Official Folder)/Standardised Datasets/Dictionaries/2026-07-28 BU_ID to BU Name Dictionary.xlsx"

# --- Dictionary Loading Block ---
dict_master <- read_xlsx(path_dict, sheet = "BU-ID_Dictionary") |> 
    janitor::clean_names()

dict_messy <- read_xlsx(path_dict, sheet = "Messy_BU_Names") |> 
    janitor::clean_names()

dict_changes <- read_xlsx(path_dict, sheet = "BU_Renames") |> 
    janitor::clean_names()

# --- Updated Function ---
standardise_bu_names <- function(df, bu_col_string, dict_master, dict_messy, dict_changes) {
    
    # --- Dynamic Renaming of Pre-existing Output Columns ---
    rename_map <- c(
        "raw_bu_acronym" = "bu_acronym",
        "raw_bu_id" = "bu_id",
        "raw_master_bu_id" = "master_bu_id"
    )
    df <- df |> rename(any_of(rename_map))
    
    # --- Pre-computation Stats ---
    orig_total <- nrow(df)
    uniq_total <- length(unique(df[[bu_col_string]]))
    rows_per_bu <- round(orig_total / uniq_total, 1)
    
    n_empty <- sum(is.na(df[[bu_col_string]]) | df[[bu_col_string]] == "")
    pct_empty <- round((n_empty / orig_total) * 100, 1)
    
    # --- Dictionary Deduplication ---
    master_unique <- dict_master |> 
        select(bu_name, bu_id, master_bu_id, bu_acronym) |> 
        distinct(bu_name, .keep_all = TRUE)
    
    messy_unique <- dict_messy |> 
        distinct(messy_bu_name, .keep_all = TRUE)
    
    changes_unique <- dict_changes |> 
        distinct(old_bu_name, .keep_all = TRUE)
    
    # --- Join & Mutate ---
    df_out <- df |> 
        # 1. Apply messy name mapping
        left_join(messy_unique, by = setNames("messy_bu_name", bu_col_string), relationship = "many-to-one") |> 
        mutate(
            bu_name_clean = if_else(type == "True BU" & !is.na(clean_bu_name), 
                                    clean_bu_name, 
                                    .data[[bu_col_string]])
        ) |> 
        # 2. Apply explicit clean name overrides from 3rd tab
        left_join(changes_unique, by = join_by(bu_name_clean == old_bu_name), relationship = "many-to-one") |> 
        mutate(
            bu_name_clean = coalesce(renamed_bu_name, bu_name_clean)
        ) |> 
        # 3. Join against master using the finalized clean name
        left_join(master_unique, by = join_by(bu_name_clean == bu_name), relationship = "many-to-one") |>
        mutate(
            bu_acronym = case_when(
                !is.na(bu_acronym) ~ bu_acronym,
                type == "Sub-unit /Budget" ~ paste0("Sub - ", abbreviate(bu_name_clean, minlength = 4)),
                type %in% c("QA", "Totals", "QA/Totals") ~ paste0("QA - ", abbreviate(bu_name_clean, minlength = 4)),
                TRUE ~ abbreviate(bu_name_clean, minlength = 4)
            )
        )
    
    # --- QA Metrics & Export ---
    valid_rows <- df_out |> 
        filter(!is.na(.data[[bu_col_string]]) & .data[[bu_col_string]] != "")
    
    n_total <- nrow(valid_rows)
    
    n_trans <- sum(!is.na(valid_rows$type) & valid_rows$type == "True BU")
    n_match <- sum(!is.na(valid_rows$bu_id))
    n_untrans <- sum(is.na(valid_rows$type) & is.na(valid_rows$bu_id))
    
    uniq_trans <- length(unique(valid_rows[[bu_col_string]][!is.na(valid_rows$type) & valid_rows$type == "True BU"]))
    uniq_match <- length(unique(valid_rows[[bu_col_string]][!is.na(valid_rows$bu_id)]))
    
    orphans <- valid_rows |> 
        filter(is.na(type) & is.na(bu_id)) |> 
        distinct(.data[[bu_col_string]]) |> 
        rename(messy_bu_name = all_of(bu_col_string)) 
    
    uniq_untrans <- nrow(orphans)
    
    if (n_untrans > 0 ){
        assign("qa_missing_bu_translations", orphans, envir = .GlobalEnv)
    }
    
    pct_trans <- round((n_trans / n_total) * 100, 1)
    pct_match <- round((n_match / n_total) * 100, 1)
    pct_untrans <- round((n_untrans / n_total) * 100, 1)
    
    div <- paste0(rep("=", 60), collapse = "")
    
    orphan_msg <- if (n_untrans > 0) {
        paste0(
            "-> ", uniq_untrans, " unique BUs without name translations exported to 'qa_missing_bu_translations'\n",
            div, "\n", 
            "Updates can be added to here:\n", 
            "https://docs.google.com/spreadsheets/d/1wZ-12T-15yL_OB5eAN_R7zF-T31DEpX-/edit?gid=43888044#gid=43888044\n",
            "NOTE: Ensure you close the tab once finished editing it to ensure updates sync.\n"
        )
    } else { "" }
    
    message(paste0(
        "\n", div, "\n",
        "BU NAME STANDARDISATION SUMMARY\n",
        div, "\n",
        "Dataset Rows: ", orig_total, "\n",
        "Unique BU Name Values: ", uniq_total, " (~", rows_per_bu, " rows per BU)\n",
        "Empty/NA BU Name Rows: ", n_empty, " (", pct_empty, "% of rows)\n",
        div, "\n",
        n_trans, " (", pct_trans, "%) messy BU name rows translated (", uniq_trans, " unique BUs)\n",
        n_match, " (", pct_match, "%) clean or translated rows matched to BU_IDs (", uniq_match, " unique BUs)\n",
        n_untrans, " (", pct_untrans, "%) valid rows left untranslated\n",
        div, "\n",
        orphan_msg
    ))
    
    # --- Clean & Return ---
    df_out |> 
        select(-type, -clean_bu_name, -renamed_bu_name) |> 
        relocate(
            any_of(c(
                "bu_id", "raw_bu_id", 
                "bu_name_clean", bu_col_string, 
                "bu_acronym", "raw_bu_acronym", 
                "master_bu_id", "raw_master_bu_id"
            ))
        )
}