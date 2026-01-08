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
  speak_warning("`qa_flag` set to 'unchecked'")
  return(qaqc_object)
}



qaqc_result_summary <- function(qaqc_object_result) {
  
  print(qaqc_object_result[['qaqc_results']])
  
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
  
  # if(qaqc_object$qa_flag == "unchecked") {
  #   if(qaqc_object$target_attr != "names") {
  #     stop("Run checks on column names first by setting target_attr to 'names'.")
  #   }
    
    # if(qaqc_object$target_attr == "names") {
    #   qaqc_object$qa_flag <- "in progress"
    #   speak_warning("Updated `qa_flag` to 'in progress...'\n")
    # }
    
  # } else {
    # Column names checked - add 'names' flag
    if(# length(qaqc_object$qa_flag) == 1 & 
       # qaqc_object$qa_flag == "in progress" &
      qaqc_object$qa_flag == "unchecked" &
      qaqc_object$target_attr == "names" &
      (!is.null(qaqc_object$dataset_newattr) |
       !is.null(qaqc_object$col_attr) |
       !is.null(qaqc_object$col_match))) {
      
      qaqc_object$qa_flag <- qaqc_object$target_attr
      speak_warning("Updated `qa_flag` to ", sprintf("'%s'\n", qaqc_object$target_attr)) 
      
    }
    
    # Column types checked - add 'types' flag
    if(# length(qaqc_object$qa_flag) == 1 & 
       # qaqc_object$qa_flag != "in progress" & 
       qaqc_object$qa_flag == "names" &
       qaqc_object$target_attr == "types") {
      # qaqc_object$qa_flag <- list(c(qaqc_object$qa_flag, qaqc_object$target_attr))
      qaqc_object$qa_flag <- qaqc_object$target_attr
      speak_warning("Updated `qa_flag` to ", sprintf("'%s'\n", qaqc_object$target_attr))
    }
    
    # Column values checked - add 'values' flag
    if(# length(unique(qaqc_object$qa_flag)) == 1 & 
       qaqc_object$qa_flag == "types" &
       # qaqc_object$qa_flag != "in progress" &
       # "types" %in% unlist(qaqc_object$qa_flag) &
       qaqc_object$target_attr == "values") {
      qaqc_object$qa_flag <- list(c(unlist(qaqc_object$qa_flag), qaqc_object$target_attr))
      speak_warning("Updated `qa_flag` to ", sprintf("'%s'\n", qaqc_object$target_attr))
    }
  # }
  
  return(qaqc_object)
  
}



get_col_attr <- function(qaqc_object) {
  
  if(!is.null(qaqc_object$col_match) |
     !is.null(qaqc_object$dataset_newattr)) {
    stop(sprintf("QAQC for '%s' completed. Call `qaqc_status('qaqc_object')` to determine progress or `qaqc_advance_stage(qaqc_object)` to move to next stage.",
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
  
  qaqc_object_attr <- qaqc_object
  qaqc_object_attr$col_attr <- col_attr
  # qaqc_object_attr <- update_qaqc_flag(qaqc_object_attr)
  
  return(qaqc_object_attr)
}



match_col_attr <- function(qaqc_object_attr, col_map = load_col_map()) {
  
  if(is.null(qaqc_object$col_attr)) {
    stop(sprintf("Source column '%s' not yet extracted. Call `qaqc_status('qaqc_object')` to determine current progress.",
                 qaqc_object$target_attr))
  }
  
  if(!is.null(qaqc_object$dataset_newattr)) {
    stop(sprintf("QAQC for '%s' completed. Call `qaqc_status('qaqc_object')` to determine current progress.",
                 qaqc_object$target_attr))
  }
  
  dataset <- qaqc_object_attr$dataset
  target_attr <- qaqc_object_attr$target_attr
  col_attr <- qaqc_object_attr$col_attr
  dat_name <- qaqc_object_attr$dat_name
  
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
  
  qaqc_object_match <- qaqc_object_attr
  qaqc_object_match$col_match <- col_match
  
  return(qaqc_object_match)
}



get_qaqc_result <- function(qaqc_object_match) {
  
  if(is.null(qaqc_object$col_match)) {
    stop(sprintf("Source and target column '%s' not yet matched. Call `qaqc_status('qaqc_object')` to determine current progress.",
                 qaqc_object$target_attr))
  }
  
  if(is.null(qaqc_object$dataset_newattr)) {
    stop(sprintf("QAQC for '%s' incomplete. Call `qaqc_status('qaqc_object')` to determine current progress.",
                 qaqc_object$target_attr))
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
  
  speak("Finished checking column ", target_attr, " for '", str_to_title(dat_name), "'.")
  print(qaqc_result_sum)
  speak("Call qaqc_result_summary(`qaqc_object`) to review results.")
  
  return(qaqc_object_result)
}



apply_newattr <- function(col_match, dataset, target_attr) {
  dataset_newattr <- map2(col_match, dataset, \(col_match_sub, dataset_sub) {
    
    # Create new column with updated attributes
    col_match_sub <- col_match_sub |> 
      mutate(col_attr_new = ifelse(result == "new...", 
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
      dataset_sub <- pmap(dataset_sub, names(dataset_sub), col_attr_new, col_attr_source, 
                    \(this_col, this_col_name, new_attr, source_attr) {
      if(new_attr != source_attr) {
          if(new_attr == "character") {
            warning(sprintf("Converting '%s' to type `%s`", this_col_name, 
                            new_attr), 
                    call. = F)
            this_col <- as.character(this_col)
            
          } else if(new_attr == "numeric") {
            warning(sprintf("Converting '%s' to type `%s`", this_col_name, 
                            new_attr),
                    call. = F)
            this_col <- as.numeric(this_col)
            
          } else {
            stop(sprintf("Standardization not yet available for type '%s'.", new_attr))
          }
        }
      })
    }
    
    # Apply standard column values
    if(target_attr == "values") {
      dataset_sub <- modify2(x, y, \(x, y) {
        speak_stop("Column values standardization not yet active...")
      })
    }
    
    return(dataset_sub)
  })
  
  return(dataset_newattr)
}



standardize_col_attr <- function(dataset, dat_name, target_attr, col_map = load_col_map()) {
  col_attr <- get_col_attr(dataset, dat_name, target_attr)
  
  col_match <- match_col_attr(col_attr, dat_name, col_map, target_attr)
  
  qaqc_result <- get_qaqc_result(dataset, col_match, dat_name, target_attr)
  
  return(qaqc_result)
  
}