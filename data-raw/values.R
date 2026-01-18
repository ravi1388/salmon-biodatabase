df_sep <- read.csv("data/sockeye/SEP_BIODATA.csv")

# # [1] "X"
# # [2] "id"
# # [3] "sample_date_impute"
# # [4] "year"
# [5] "stock_impute"
# [6] "enpro_site_river_location_impute"
# [7] "oto_stock_of_origin_impute"
# [8] "sex_final_impute"
# # [9] "pohl"
# [10] "age_ocean"
# [11] "adipose_fin_clip_impute"
# [12] "final_use_distribution_impute"
# # [13] "data_source"

unique_vals <- df_sep |> 
  # map(unique) |>
  map(\(x) {
    x <- unique(x)
    subset(x, !is.na(x))
  }) |>
  keep_at(c("stock_impute", "enpro_site_river_location_impute", 
            "oto_stock_of_origin_impute", "sex_final_impute", "age_ocean",
            "adipose_fin_clip_impute", "final_use_distribution_impute")) 

unique_vals <- unique_vals|> 
  map(paste, collapse = ", ") |> 
  # map(list) |> 
  as_tibble() |> 
  pivot_longer(names(unique_vals), names_to = "names", values_to = "values")

write.csv(unique_vals, "data/sockeye/internal/values.csv")
