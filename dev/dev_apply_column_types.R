

check_value_outliers <- function(df, expected_values) {
  # Apply a function to every column of the dataframe
  outliers <- lapply(df, function(col) {
    # Find unique values in the data that are NOT in the expected list
    found_outliers <- setdiff(unique(col), expected_values)
    
    # Return NULL if empty, or the outliers if they exist
    if (length(found_outliers) > 0) return(found_outliers) else return(NULL)
  })
  
  # Remove the NULL entries so you only see columns with problems
  outliers <- outliers[!sapply(outliers, is.null)]
  
  return(outliers)
}


