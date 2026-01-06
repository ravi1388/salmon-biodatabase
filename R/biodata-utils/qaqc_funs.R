make_qa_object <- function(dataset) {
  
  dat_name <- modify(dataset, \(x) {
    return(unique(x$dat_name))
  }) |> unlist() |> unique()
  
  if(length(dat_name) > 1) {
    stop("QAQC functions can only handle one data source at a time, but multiple data sources detected: ", 
         paste(dat_name, collapse = ", "))
  }
  
  qa_object <- list(qa_flag = "unchecked",
                    dat_name = dat_name,
                    target_attr = "names",
                    dataset = dataset)
  speak_warning("`qa_flag` set to 'unchecked'")
  return(qa_object)
}

advance_qa_stage <- function(qa_object) {
  
  if(qa_object$qa_flag == qa_object$target_attr) {
    
    qa_object$col_attr <- NULL
    qa_object$col_match <- NULL
    qa_object$dataset <- qa_object$dataset_newattr
    qa_object$dataset_newattr <- NULL
    
    target_attr <- qa_object$target_attr
    
    if(target_attr == "names") {
      qa_object$target_attr <- "types"
    } else if(target_attr == "types") {
      qa_object$target_attr <- "values"
    }
    warning(sprintf("Column '%s' checked, setting `target_attr` to '%s'", 
                    target_attr, qa_object$target_attr))
  }
  
  return(qa_object)
  
}

get_col_attr <- function(qa_object) {
  
  dataset <- qa_object$dataset
  dat_name <- qa_object$dat_name
  
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
    
    if(qa_object$target_attr == "values") {
      mutate(df, colvals = vals)
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
  
  qa_object_attr <- qa_object
  qa_object_attr$col_attr <- col_attr
  qa_object_attr <- update_qaqc_flag(qa_object_attr)
  
  return(qa_object_attr)
}


match_col_attr <- function(qa_object_attr, col_map = load_col_map()) {
  
  dataset <- qa_object_attr$dataset
  target_attr <- qa_object_attr$target_attr
  col_attr <- qa_object_attr$col_attr
  dat_name <- qa_object_attr$dat_name
  
  col_match <- modify(col_attr, \(x) {
    # x <- left_join(x, col_map, by = paste0("col_", target_attr, "_", dat_name))
    x <- left_join(x, col_map, by = paste0("col_names_", dat_name))
    these_cols <- names(x)
    new_cols <- gsub(dat_name, "source", these_cols)
    new_cols <- gsub(target_attr, "attr", new_cols)
    names(x) <- new_cols
    
    x <- x |> 
      mutate(result = case_when(col_attr_source == col_attr_target ~ "matched...",
                                col_attr_source != col_attr_target ~ "mapped...",
                                is.na(col_attr_target) ~ "new..."),
             result = factor(result, levels = c("matched...", "mapped...", "new...")))
    
    if(target_attr == "names") {
      x <- x |> 
        select(col_attr_source, col_attr_target, result)
    } else {
      x <- x |> 
        select(col_names_source, col_attr_source, col_names_target, col_attr_target, result)
    }
    
  })
  
  qa_object_match <- qa_object_attr
  qa_object_match$col_match <- col_match
  
  return(qa_object_match)
}

get_match_result <- function(qa_object_match) {
  
  dat_name <- qa_object_match$dat_name
  target_attr <- qa_object_match$target_attr
  dataset <- qa_object_match$dataset
  col_match <- qa_object_match$col_match
  
  match_result <- modify(col_match, \(x) {
    x <- x |> 
      arrange(result) |> 
      select(result, col_attr_source)
  })
  
  match_result_sum <- modify(col_match, \(x) {
    table(x$result) |> data.frame() |> 
      select(result = Var1, ncols = Freq )
  })
  
  names(match_result_sum) <- names(col_match)
  
  dataset_newattr <- apply_newattr(col_match, dataset, target_attr)
  qa_object_result <- qa_object_match
  qa_object_result$dataset_newattr <- dataset_newattr
  qa_object_result$match_result <- match_result
  qa_object_result <- update_qaqc_flag(qa_object_result)
  
  speak("Finished checking column ", target_attr, " for '", str_to_title(dat_name), "'.")
  # speak(names(match_result_sum), " ", match_result_sum, "\n")
  print(match_result_sum)
  speak("Call match_result_summary(`qa_object`) to review results.")
  
  return(qa_object_result)
}

match_result_summary <- function(qa_object_result) {
  
  print(qa_object_result$match_result)
  
}

apply_newattr <- function(col_match, dataset, target_attr) {
  dataset_newattr <- map2(col_match, dataset, \(x, y) {
    x <- x |> 
      mutate(col_attr_new = ifelse(result == "new...", 
                                   col_attr_source, col_attr_target)) |> 
      pull(col_attr_new)
    
    # Apply standard column names
    if(target_attr == "names") {
      names(y) <- x
    }
    
    # Apply standard column types
    if(target_attr == "types") {
      dataset_newattr <- modify2(x, y, \(x, y) {
        stop("Column types standardization not yet active...")
      })
    }
    
    # Apply standard column values
    if(target_attr == "values") {
      dataset_newattr <- modify2(x, y, \(x, y) {
        stop("Column values standardization not yet active...")
      })
    }
    
    return(y)
  })
  
  return(dataset_newattr)
}

update_qaqc_flag <- function(qa_object) {
  
  # No qa conducted - Add column `qa_flag` containing NAs
  if(qa_object$qa_flag == "unchecked") {
    if(qa_object$target_attr != "names") {
      stop("Run checks on column names first by setting target_attr to 'names'.")
    }
    
    if(qa_object$target_attr == "names") {
      qa_object$qa_flag <- "in progress"
      speak_warning("Updated `qa_flag` to 'in progress...'\n")
    }
    
  } else {
    # Column names checked - add 'names' flag
    if(length(qa_object$qa_flag) == 1 & 
       qa_object$qa_flag == "in progress" &
       qa_object$target_attr == "names") {
      qa_object$qa_flag <- qa_object$target_attr
      speak_warning("Updated `qa_flag` to ", sprintf("'%s'\n", qa_object$target_attr))
    }
    
    # Column types checked - add 'types' flag
    if(length(qa_object$qa_flag) == 1 & 
       qa_object$qa_flag != "in progress" & 
       qa_object$qa_flag == "names" &
       qa_object$target_attr == "types") {
      qa_object$qa_flag <- list(c(qa_object$qa_flag, qa_object$target_attr))
      speak_warning("Updated `qa_flag` to ", sprintf("'%s'\n", qa_object$target_attr))
    }
    
    # Column values checked - add 'values' flag
    if(length(unique(qa_object$qa_flag)) == 1 & 
       qa_object$qa_flag != "in progress" & 
       "types" %in% unlist(qa_object$qa_flag) &
       qa_object$target_attr == "values") {
      qa_object$qa_flag <- list(c(unlist(qa_object$qa_flag), qa_object$target_attr))
      speak_warning("Updated `qa_flag` to ", sprintf("'%s'\n", qa_object$target_attr))
    }
  }
  
  return(qa_object)
  
}

qa_status <- function(qa_object) {
  return(list(qa_flag = qa_object$qa_flag,
              dat_name = qa_object$dat_name,
              target_attr = qa_object$target_attr))
}

standardize_col_attr <- function(dataset, dat_name, target_attr, col_map = load_col_map()) {
  col_attr <- get_col_attr(dataset, dat_name, target_attr)
  
  col_match <- match_col_attr(col_attr, dat_name, col_map, target_attr)
  
  match_result <- get_match_result(dataset, col_match, dat_name, target_attr)
  
  return(match_result)
  
}