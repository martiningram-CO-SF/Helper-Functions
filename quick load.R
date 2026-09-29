# # 02 custom functions ----
# 
# # Define the path to your helper functions directory 
# # (Adjust path based on your exact Helper Functions folder location)
# helper_path <- "~/Documents/RWork/SF-Data-Standardisation/Dictionaries/"
# 
# # Dynamically find and source all .R files in the directory
# list.files(path = helper_path, pattern = "\\.R$", full.names = TRUE) %>% 
#     purrr::walk(source)

# ALTERNATIVE: If you prefer strict control over load order, comment the above out and use:
# source(file.path(helper_path, "standardise_bu_names.R"))
# source(file.path(helper_path, "qa_mapping_collapse.R"))