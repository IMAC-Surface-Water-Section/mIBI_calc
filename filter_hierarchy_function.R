# Function for identifying redundant levels of taxonomic resolution (default method is RPKC-S)
# e.g. both Chironomini and Chironomus sp. in the same sample; Chironomini would be potentially redundant and is therefore considered ambiguous.
# Uses RPKC-S, DPAC-S, or APTC-S method (based on user choice) derrived from Meredith, Trebitz, & Hoffman, 2019 (Resolving taxonomic ambiguities: effects on rarity, projected richness, and indices in macroinvertebrate datasets)
filter_hierarchy <- function(df, group_col, hierarchy_cols, count_col = "count", method = "RPKC-S", to_filter = TRUE) {
  if(!(method %in% c("RPKC-S", "DPAC-S", "APTC-S"))){
    return(errorCondition("Please enter a valid method (RPKC-S, DPAC-S, or APTC-S)"))
    break
  }
  dt <- as.data.table(df)
  
  dt[, {
    to_keep <- !is.na(get(hierarchy_cols[1])) # Keep all rows that aren't NA at the lowest level (e.g. genus)
    if(method %in% c("DPAC-S", "APTC-S")){
      new_count <- get(count_col) # Initialize new_count with original counts
    }
    # For each higher level (e.g. genus group, tribe, family, etc.)
    for(i in 2:length(hierarchy_cols)){
      h_col <- hierarchy_cols[i]
      # Find rows where all lower levels are NA but not current level
      possible_dupes <- !to_keep & !is.na(get(h_col))
      for(j in 1:(i-1)){
        possible_dupes <- possible_dupes & is.na(get(hierarchy_cols[j]))
      }
      if(any(possible_dupes)){
        for(val in unique(get(h_col)[possible_dupes])){ # For each possible duplicate at this level
          if(method == "RPKC-S"){ # default
            if(!(val %in% get(h_col)[to_keep & !is.na(get(h_col))])){ # If this value doesn't exist in any row we're already keeping
              to_keep <- to_keep | (possible_dupes & get(h_col) == val)
            }
          } else if(method  %in% c("DPAC-S", "APTC-S")){ # If method is DPAC-S
            # If children exist, redistribute the parent's count proportionally among children
            children_with_val <- to_keep & !is.na(get(h_col)) & get(h_col) == val
            if(any(children_with_val)){
              parent_rows <- possible_dupes & get(h_col) == val
              if(any(parent_rows)){
                parent_count <- sum(get(count_col)[parent_rows])
                if(method == "DPAC-S"){
                  # Get counts of all children
                  child_indices <- which(children_with_val)
                  child_counts <- new_count[child_indices]
                  total_child_count <- sum(child_counts)
                  
                  proportions <- child_counts/total_child_count
                  # Distribute parent counts proportionally to each child
                  for(k in seq_along(child_indices)){
                    new_count[child_indices[k]] <- (parent_count*proportions[k]) + new_count[child_indices[k]]
                  }
                } else if(method == "APTC-S"){
                  # Find the child with the highest count
                  child_counts <- new_count[children_with_val]
                  max_count <- max(child_counts)
                  max_child_indices <- which(children_with_val)[child_counts == max_count]
                  # If there are ties, randomly select a child
                  if(length(max_child_indices) > 1){
                    selected_child_index <- sample(max_child_indices, 1)
                  } else {
                    selected_child_index <- max_child_indices[1]
                  }
                  
                  # Add parent's count to the selected child
                  new_count[selected_child_index] <- new_count[selected_child_index] + parent_count
                }
              }
            } else {
              to_keep <- to_keep | (possible_dupes & get(h_col) == val)
            }
          }
        }
      }
    } # if to_filter == TRUE (default), filter out ambiguous taxa. If to_filter == FALSE, add column indicating if taxon is ambiguous
    if(method %in% c("DPAC-S", "APTC-S")){
      if(to_filter){
        .SD[to_keep][, new_count := new_count[to_keep]] # filter
      } else {
        cbind(.SD, is_ambiguous = !to_keep)[, new_count := new_count] # Add column indicating if taxon is ambiguous (TRUE = ambiguous, FALSE = not ambiguous)
      }
    } else if(method == "RPKC-S"){
      if(to_filter) {
        .SD[to_keep] # filter
      } else {
        cbind(.SD, is_ambiguous = !to_keep) # Add column indicating if taxon ambiguous (TRUE = ambiguous, FALSE = not ambiguous)
      }
    }
  }, by = group_col]
}
