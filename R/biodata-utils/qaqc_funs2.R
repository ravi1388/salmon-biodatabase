#' get_col_attr         :: get_col_attr
#' match_col_attr       :: match_col_attr
#' get_match_result      :: get_match_result
#' standardize_col_attr :: standardize_col_attr

get_col_attr <- function(dataset, dat_name, attr_type) {
  
  dataset <- add_qaqc_flag(dataset = dataset, attr_type = attr_type)
  
  col_attr <- modify(dataset, \(x) {
      
    df <- map(x, \(y) {
      return(class(y))
    }) |> 
      bind_cols() |> 
      distinct() |>
      pivot_longer(names(x),
                   names_to = "col_names",
                   values_to = "col_types") |>
      mutate(path = unique(x$path))
    
    if(attr_type == "values") {
      mutate(df, path = unique(x$path),
             colvals = vals)
    }
    
    return(df)
  })
  
  col_attr <- modify(col_attr, \(x) {
    these_cols <- names(x)
    ind <- grep("col", these_cols)
    these_cols[ind] <- paste0(these_cols[ind], "_", dat_name)
    names(x) <- these_cols
    return(x)
  })
  
  qa_object <- list(dataset = dataset,
                    col_attr = col_attr)
  return(qa_object)
}


match_col_attr <- function(qa_object_attr, col_map = load_col_map(), attr_type) {
  
  dataset <- qa_object_attr$dataset
  col_attr <- qa_object_attr$col_attr
  
  if(!attr_type %in% c("names", "types", "vals")) {
    stop("Invalid argument for 'col_attr'. Use one of 'names', 'types' or 'vals'.")
  }
  
  col_match <- modify(col_attr, \(x) {
    x <- left_join(x, col_map, by = paste0("col_", attr_type, "_", dat_name))
    these_cols <- names(x)
    new_cols <- gsub(dat_name, "dat", these_cols)
    new_cols <- gsub(attr_type, "attr", new_cols)
    names(x) <- new_cols
    
    x <- x |> 
      select(col_attr_dat, col_attr_sock, path) |> 
      mutate(result = case_when(col_attr_dat == col_attr_sock ~ "matched...",
                                col_attr_dat != col_attr_sock ~ "mapped...",
                                is.na(col_attr_sock) ~ "new..."),
             result = factor(result, levels = c("matched...", "mapped...", "new...")))
  })
  
  qa_object_match <- list(dataset = dataset,
                    col_match = col_match)
  return(qa_object_match)
}

get_match_result <- function(qa_object_match, dat_name, attr_type) {
  
  dataset <- qa_object_match$dataset
  col_match <- qa_object_match$col_match
  
  match_result <- modify(col_match, \(x) {
    x <- x |> bind_rows() |> arrange(result) |> 
      select(-col_attr_sock)
  })
  
  match_result_sum <- modify(col_match, \(x) {
    table(x$result) |> data.frame() |> mutate(path = unique(x$path)) |> 
      select(result = Var1, ncols = Freq, path = path)
  })
  
  # match_result <- col_match |> bind_rows() |> arrange(result) |> 
  #     select(-col_attr_sock)
  # 
  # match_result_sum <- table(match_result$result) |> bind_rows() |> colSums()
  
  dataset_newattr <- apply_newattr(col_match, dataset)
  
  dataset_newattr <- add_qaqc_flag(dataset_newattr)
  
  speak("Name check complete for '", str_to_title(dat_name), "'.")
  speak(names(match_result_sum), " ", match_result_sum, "\n")
  
  result <- readline_enter("Press <Enter> to continue or type 'R' to review results:")
  result <- toupper(result)
  if(result == "R") {
    print(match_result)
    return(dataset_newattr)
  } else if(isTRUE(result)) {
    return(dataset_newattr)
  }
}

apply_newattr <- function(col_match, dataset) {
  dataset_newattr <- map2(col_match, dataset, \(x, y) {
    x <- x |> 
      mutate(col_attr_new = ifelse(result == "new...", 
                                  col_attr_dat, col_attr_sock)) |> 
      pull(col_attr_new)
    
    # Apply standard column names
    if(attr_type == "names") {
      names(y) <- x
    }
    
    # Apply standard column types
    if(attr_type == "types") {
      dataset_newattr <- modify2(x, y, \(x, y) {
        stop("Column types standardization not yet active...")
      })
    }
    
    # Apply standard column values
    if(attr_type == "values") {
      dataset_newattr <- modify2(x, y, \(x, y) {
        stop("Column values standardization not yet active...")
      })
    }
    
    return(y)
  })
  
  return(dataset_newattr)
}

add_qaqc_flag <- function(dataset, attr_type) {
  
  dataset_flag <- modify(dataset, \(x) {
    
    # No qa conducted - Add column `qa_flag` containing NAs
    if(suppressWarnings(is.null(x$qa_flag))) {
      if(attr_type != "names") {
        stop("Run checks on column names first by setting attr_type to 'names'.")
      }
      if(attr_type == "names") {
        x$qa_flag <- NA
      }
      
    } else {
      # Column names checked - add 'names' flag
      if(length(unique(x$qa_flag)) == 1 & 
         is.na(unique(x$qa_flag)) &
         attr_type == "names") {
        x$qa_flag <- attr_type
      }
      
      # Column types checked - add 'types' flag
      if(length(unique(x$qa_flag)) == 1 & 
         !is.na(unique(x$qa_flag)) & 
         unique(x$qa_flag) == "names" &
         attr_type == "types") {
        x$qa_flag <- list(c(unique(x$qa_flag), attr_type))
      }
      
      # Column values checked - add 'values' flag
      if(length(unique(x$qa_flag)) == 1 & 
         !is.na(unique(x$qa_flag)) & 
         "types" %in% unlist(unique(x$qa_flag)) &
         attr_type == "values") {
        x$qa_flag <- list(c(unlist(unique(x$qa_flag)), attr_type))
      }
    }
    return(x)
    
  })
  
  return(dataset_flag)
  
}

standardize_col_attr <- function(dataset, dat_name, attr_type, col_map = load_col_map()) {
  col_attr <- get_col_attr(dataset, dat_name, attr_type)
  
  col_match <- match_col_attr(col_attr, dat_name, col_map, attr_type)
  
  match_result <- get_match_result(dataset, col_match, dat_name, attr_type)
  
  return(match_result)
  
}