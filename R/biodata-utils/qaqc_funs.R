#' **QAQC functions**
#' 
#' These are a set of functions used to standardize dataset(s) before they are 
#' compiled and/or added to the `sockeye` biodatabase. They aim to provide
#' consistency in:
#' - Column names among like columns
#' - Data types among like columns
#' - Values among like columns
#' 
#' It is centered around the creation and progression of a generalized data
#' object, the `qaqc_object`, in the qaqc process.
#' 
#' ** Slots associated with `qaqc_object`**
#' - `$qa_flag` Indicates the last stage completed by the `qaqc_object`.
#' - `$dat_name` The name of the data source raw data files are associated with.
#' - `$target_attr` Specifies which of the following attributes is being 
#'   standardized, column names ('names'), column data types ('types') or column
#'   data values ('values').
#' - `$dataset` A list of dataframes, each corresponding to a single raw data 
#'   file to be standardized.
#' - `$col_attr` A list of dataframes, each containing values for attributes
#'   related to a corresponding raw data file.
#' - `$col_match` A list of dataframes, each containing values for 
#'   attributes from each raw data file mapped to standardized values from an
#'   internally stored column map for the `sockeye` biodatabase.
#' - `$dataset_newattr` A list of dataframes, corresponding to a copy of
#'   `$dataset` following standardization.
#' - `$qaqc_results` A nested list of results for each raw data file, and each
#'   stage of standardization.
#' 
#' Here is a list of functions contained within:
#' - `make_qaqc_object()` Creates a generalized data object, the `qaqc_object`,
#'   used to carry datasets through the qaqc process.
#' - `qaqc_status()` Used to print metadata associated with the `qaqc_object`.
#' - `qaqc_advance_stage()` Controls the progress of the `qaqc_object` through
#'   the qaqc process by conducting checks on the `qaqc_object`, updating
#'   attributes such as `$qaqc_flag` and `$target_attr` and halting the process
#'   once all checks are complete.
#' - `update_qaqc_flag` Contains logic used to update `$qaqc_flag`. Called by
#'   `qaqc_advance_stage()`.
#' - `get_col_attr()` Obtains values related to the target attribute from the 
#'   source (raw) data in `$dataset`. These are subsequently stored in
#'   `$col_attr`.
#' - `match_col_attr()` Obtains standardized values for attributes, mapped to
#'   those from the source data. Stored in `$col_match`.
#' - `get_match_result()` Obtains the results of attribute standardization from
#'   `apply_new_attr` and handles the creation and output of summary and 
#'   detailed metadata about the standardization process.
#' - `apply_new_attr()` Handles logic used to implement attribute
#'   standardization. Called by `get_match_result()`.
#' - `standardize_column_types()` Handles logic used to apply target data type 
#'   formats to corresponding columns in the source data.
#' - `qaqc_result_summary()` Used to print the results contained in 
#'   `$qaqc_results`.


make_qaqc_object <- function(dataset) {
  
  dat_name <- modify(dataset, \(x) {
    return(unique(x$dat_name))
  }) |> unlist() |> unique()
  
  if(length(dat_name) > 1) {
    stop("QAQC functions can only handle one data source at a time, but multiple data sources detected: ", 
         paste(dat_name, collapse = ", "))
  }
  
  qaqc_object <- list(qa_flag = "unchecked",
                    dat_name = dat_name,
                    target_attr = "names",
                    dataset = dataset)
  warning("`qa_flag` set to 'unchecked'", call. = F)
  return(qaqc_object)
}



qaqc_status <- function(qaqc_object) {
  
  return(list(qa_flag = qaqc_object$qa_flag,
              dat_name = qaqc_object$dat_name,
              target_attr = qaqc_object$target_attr,
              attributes = names(qaqc_object)))
  
}



qaqc_advance_stage <- function(qaqc_object) {
  
  qaqc_object <- update_qaqc_flag(qaqc_object)
  
  if(qaqc_object$qa_flag == qaqc_object$target_attr) {
    
    if(qaqc_object$qa_flag == "values") {
      message("QAQC complete! Call `qaqc_result_summary(`qaqc_object`)` to 
              review results of standardization.")
      return(qaqc_object)
    }
    
    qaqc_object$col_attr <- NULL
    qaqc_object$col_match <- NULL
    qaqc_object$dataset <- qaqc_object$dataset_newattr
    qaqc_object$dataset_newattr <- NULL
    
    target_attr <- qaqc_object$target_attr
    
    if(target_attr == "names") {
      qaqc_object$target_attr <- "types"
    } else if(target_attr == "types") {
      qaqc_object$target_attr <- "values"
    }
    
    warning(sprintf("Column '%s' checked, setting `target_attr` to '%s'", 
                    target_attr, qaqc_object$target_attr), call. = F)
  }
  
  return(qaqc_object)
  
}



update_qaqc_flag <- function(qaqc_object) {
  
    # Column names checked - add 'names' flag
    if(qaqc_object$qa_flag == "unchecked" &
       qaqc_object$target_attr == "names" &
       (!is.null(qaqc_object$dataset_newattr) |
        !is.null(qaqc_object$col_attr) |
        !is.null(qaqc_object$col_match))) {
      
      qaqc_object$qa_flag <- qaqc_object$target_attr
      warning("Updated `qa_flag` to ", 
              sprintf("'%s'\n", qaqc_object$target_attr), call. = F) 
      
    }
    
    # Column types checked - add 'types' flag
    if(# length(qaqc_object$qa_flag) == 1 & 
       # qaqc_object$qa_flag != "in progress" & 
       qaqc_object$qa_flag == "names" &
       qaqc_object$target_attr == "types") {
      # qaqc_object$qa_flag <- list(c(qaqc_object$qa_flag, qaqc_object$target_attr))
      qaqc_object$qa_flag <- qaqc_object$target_attr
      warning("Updated `qa_flag` to ",
              sprintf("'%s'\n", qaqc_object$target_attr), call. = F)
    }
    
    # Column values checked - add 'values' flag
    if(qaqc_object$qa_flag == "types" & qaqc_object$target_attr == "values") {
      qaqc_object$qa_flag <- qaqc_object$target_attr
      warning("Updated `qa_flag` to ", 
              sprintf("'%s'\n", qaqc_object$target_attr), call. = F)
    }
  # }
  
  return(qaqc_object)
  
}



get_col_attr <- function(qaqc_object) {
  
  if(!is.null(qaqc_object$dataset_newattr)) {
    stop(sprintf("QAQC for '%s' completed. Call `qaqc_status('qaqc_object')` to determine progress.",
                                                         qaqc_object$target_attr))
  }
  
  dataset <- qaqc_object$dataset
  dat_name <- qaqc_object$dat_name
  
  col_attr <- modify(dataset, \(x) {
    
    df <- map(x, \(y) {
      return(class(y))
    }) |> 
      bind_cols() |> 
      distinct() |>
      pivot_longer(names(x),
                   names_to = "col_names",
                   values_to = "col_types") |>
      mutate(dat_name = unique(x$dat_name))
    
    if(qaqc_object$target_attr == "values") {
      
      vals <- x |> 
        map(\(y) {
          y <- sort(unique(y))
        })
      
      vals <- vals|> 
        map(paste, collapse = ", ") |> 
        as_tibble() |> 
        pivot_longer(names(vals), names_to = "col_names", values_to = "col_values")
      
      df <- df |> left_join(vals, by = "col_names")
    }
    
    return(df)
  })
  
  col_attr <- modify(col_attr, \(x) {
    dat_name <- unique(x$dat_name)
    these_cols <- names(x)
    ind <- grep("col", these_cols)
    these_cols[ind] <- paste0(these_cols[ind], "_", dat_name)
    names(x) <- these_cols
    return(x)
  })
  
  qaqc_object_attr <- qaqc_object
  qaqc_object_attr$col_attr <- col_attr
  
  return(qaqc_object_attr)
}



match_col_attr <- function(qaqc_object_attr, col_map = load_col_map()) {
  
  if(is.null(qaqc_object_attr$col_attr)) {
    stop(sprintf("`$col_attr` not provided. Call `get_col_attr()` to extract column '%s', or `qaqc_status('qaqc_object')` to determine current progress.",
                 qaqc_object_attr$target_attr))
  }
  
  if(!is.null(qaqc_object_attr$dataset_newattr)) {
    stop(sprintf("QAQC for '%s' completed. Call `qaqc_status('qaqc_object')` to determine current progress.",
                 qaqc_object_attr$target_attr))
  }
  
  dataset <- qaqc_object_attr$dataset
  target_attr <- qaqc_object_attr$target_attr
  col_attr <- qaqc_object_attr$col_attr
  dat_name <- qaqc_object_attr$dat_name
  
  col_match <- modify(col_attr, \(col_attr_sub) {
    if(target_attr == "names") {
      
      col_attr_sub <- left_join(col_attr_sub, col_map, by = paste0("col_names_", dat_name))
    } else {
      
      this_col <- names(col_attr_sub)
      this_col[grep(paste0("col_names_", dat_name), this_col)] <- "col_names_target"
      names(col_attr_sub) <- this_col
      
      col_attr_sub <- col_attr_sub |> left_join(col_map, by = "col_names_target")
      col_attr_sub[paste0("col_names_", dat_name)] <- NULL
      
      this_col <- names(col_attr_sub)
      this_col[grep("col_names_target", this_col)] <- paste0("col_names_", dat_name)
      names(col_attr_sub) <- this_col
    }
    
    these_cols <- names(col_attr_sub)
    new_cols <- gsub(dat_name, "source", these_cols)
    new_cols <- gsub(target_attr, "attr", new_cols)
    names(col_attr_sub) <- new_cols
    
    if(target_attr %in% c("names", "types")) {
      col_attr_sub <- add_match_flag(col_attr_sub)
    } else if(target_attr == "values") {
      col_attr_sub <- handle_values_match(col_attr_sub)
    } else {
      stop(sprintf("Standardization for '%s' not yet available", target_attr))
    }
    
    
    if(target_attr == "names") {
      col_attr_sub <- col_attr_sub |> 
        select(col_attr_source, col_attr_target, result)
    } else {
      col_attr_sub <- col_attr_sub |> 
        select(col_names_source, col_attr_source, col_names_target, col_attr_target, result)
    }
    
    return(col_attr_sub)
  })
  
  qaqc_object_match <- qaqc_object_attr
  qaqc_object_match$col_match <- col_match
  
  return(qaqc_object_match)
}

add_match_flag <- function(col_attr_sub) {
  
  col_attr_sub |> 
    mutate(result = case_when(col_attr_source == col_attr_target ~ "matched",
                              col_attr_source != col_attr_target ~ "mapped",
                              is.na(col_attr_target) ~ "new"),
           result = factor(result, levels = c("matched", "mapped", "new")))
}

#' `handle_values_match`()
#' This function checks the values of each column, comparing to the strictness 
#' and expected values of the target schema.
#' 
#' **Strictness**
#' There are 3 levels of strictness:
#' - Full: All rows must have a value, no `NA`s allowed
#' - Partial: At least one row must have a value, some `NA`s allowed
#' - None: All rows can have `NA` values
#' 
#' **Expected values**
#' Only a few columns are required to contain a specific set of values. These 
#' are:
#' - "stock"
#' - "site_river_location"
#' - "stock_of_origin"
#' - "sex_final"
#' - "age_ocean"
#' - "adipose_fin_clip"
#' - "final_use_distribution"

check_strictness <- function(col_attr_sub) {
  
  df <- col_attr_sub |> 
    filter(!is.na(col_names_target))
    select(col_names_source)

  


  if(TRUE %in% is.na(unique(vals))) {
    stop("Column 'id' contains ")
  }
}

handle_values_match <- function(col_attr_sub) {

}


get_qaqc_result <- function(qaqc_object_match) {
  
  if(is.null(qaqc_object_match$col_match)) {
    stop(sprintf("`$col_match` not provided. Call `match_col_attr('qaqc_object')` to create mapping for source and target column '%s', or`qaqc_status('qaqc_object')` to determine current progress.",
                 qaqc_object_match$target_attr))
  }
  
  if(!is.null(qaqc_object_attr$dataset_newattr)) {
    stop(sprintf("QAQC for '%s' completed. Call `qaqc_status('qaqc_object')` to determine current progress.",
                 qaqc_object_attr$target_attr))
  }
  
  dat_name <- qaqc_object_match$dat_name
  target_attr <- qaqc_object_match$target_attr
  dataset <- qaqc_object_match$dataset
  col_match <- qaqc_object_match$col_match
  
  dataset_newattr <- apply_newattr(col_match, dataset, target_attr)
  
  qaqc_result <- modify(col_match, \(x) {
    x <- x |> 
      arrange(result)
    old_cols <- names(x)
    new_cols <- gsub("attr", target_attr, old_cols)
    names(x) <- new_cols
    return(x)
  })
  
  qaqc_object_result <- qaqc_object_match
  qaqc_object_result[['dataset_newattr']] <- dataset_newattr
  qaqc_object_result[['qaqc_results']][[target_attr]] <- qaqc_result
  # qaqc_object_result <- update_qaqc_flag(qaqc_object_result)
  
  qaqc_result_sum <- modify(col_match, \(x) {
    table(x$result) |> data.frame() |> 
      select(result = Var1, ncols = Freq )
  })
  
  message("Finished checking column ", target_attr, " for '", str_to_title(dat_name), "'.")
  print(qaqc_result_sum)
  message("Call qaqc_result_summary(`qaqc_object`) to review results.")
  
  return(qaqc_object_result)
}



apply_newattr <- function(col_match, dataset, target_attr) {
  dataset_newattr <- map2(col_match, dataset, \(col_match_sub, dataset_sub) {
    
    # Create new column with updated attributes
    col_match_sub <- col_match_sub |> 
      mutate(col_attr_new = ifelse(result == "new", 
                                   col_attr_source, col_attr_target))
    # Pull source and new attributes
    col_attr_new <- col_match_sub |> 
      pull(col_attr_new)
    col_attr_source <- col_match_sub |> 
      pull(col_attr_source)
    
    # Apply standardized column names
    if(target_attr == "names") {
      names(dataset_sub) <- col_attr_new
    }
    
    # Apply standardized column types
    if(target_attr == "types") {
      dataset_sub <- standardize_column_types(dataset_sub, col_attr_new, col_attr_source)
    }
    
    # Apply standard column values
    if(target_attr == "values") {
      dataset_sub <- standardize_column_values(dataset_sub, col_attr_new, col_attr_source)
    }
    
    return(dataset_sub)
  })
  
  return(dataset_newattr)
}


standardize_column_types <- function(dataset_sub, col_attr_new, col_attr_source) {
  
  pmap(list(dataset_sub, names(dataset_sub), col_attr_new, col_attr_source), 
         \(this_col, this_col_name, new_attr, source_attr) {
           if(new_attr != source_attr) {
             if(new_attr == "character") {
               warning(sprintf("Converting '%s' to type `%s`", this_col_name, new_attr), call. = F)
                            this_col <- as.character(this_col)
                            } else if(new_attr == "numeric") {
                              warning(sprintf("Converting '%s' to type `%s`", this_col_name, new_attr), call. = F)
                              this_col <- as.numeric(this_col)
                              } else {
                                stop(
                                  sprintf("Standardization not yet available for type '%s'.", new_attr), 
                                  call. = F)
                              }
           } else {
             return(this_col)
           }
         }) |> bind_cols()
  
  # return(dataset_sub)
}

standardize_column_values <- function(dataset_sub, col_attr_new, col_attr_source) {
  
  dataset_sub <- 
    pmap(list(dataset_sub, names(dataset_sub), col_attr_new, col_attr_source), 
         \(this_col, this_col_name, new_attr, source_attr) {
           if(new_attr != source_attr) {
             if(new_attr == "character") {
               warning(sprintf("Converting '%s' to type `%s`", this_col_name, new_attr), call. = F)
               this_col <- as.character(this_col)
             } else if(new_attr == "numeric") {
               warning(
                 sprintf("Converting '%s' to type `%s`", this_col_name, new_attr), 
                 call. = F)
               this_col <- as.numeric(this_col)
             } else {
               stop(
                 sprintf("Standardization not yet available for type '%s'.", new_attr), 
                 call. = F)
             }
           }
         })
  
  return(dataset_sub)
}



qaqc_result_summary <- function(qaqc_object_result) {
  
  print(qaqc_object_result[['qaqc_results']])
  
}



standardize_col_attr <- function(dataset, dat_name, target_attr, col_map = load_col_map()) {
  col_attr <- get_col_attr(dataset, dat_name, target_attr)
  
  col_match <- match_col_attr(col_attr, dat_name, col_map, target_attr)
  
  qaqc_result <- get_qaqc_result(dataset, col_match, dat_name, target_attr)
  
  return(qaqc_result)
  
}