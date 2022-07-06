run_prioritization <- function(objective, step_size, effort_assumption){

  ranking <- list()

  current_pick <- 0

  if(objective == "biodiversity"){

    current_state <- baseline_state[ ,bio_feature_names]

    unprotected_matrix <- unprotected_matrix[ ,bio_feature_names]

    while(nrow(unprotected_matrix) > 0){

      current_pick <- sum(current_pick, 1)

      slopes <- calculate_slopes(objective = "biodiversity",
                                 current_state = current_state,
                                 max_slopes = max_slopes_bio)

      delta <- unprotected_matrix%*%as.matrix(slopes)

      best_cell_indeces <- doBy::which.maxn(delta, n = step_size)

      best_cells <- delta[best_cell_indeces,] %>%
        enframe() %>%
        set_names(c('cell_id', "delta")) %>%
        mutate(pick_order = current_pick)

      ranking <- bind_rows(ranking, best_cells)

      current_state <- unprotected_matrix[best_cell_indeces, ,drop = F] %>%
        colSums() +
        current_state

      unprotected_matrix <- unprotected_matrix[-best_cell_indeces, , drop = F]
    }
  } else if (objective == "carbon"){

    current_state <- baseline_state[ ,"carbon", drop = F]

    unprotected_matrix <- unprotected_matrix[ ,"carbon", drop  = F]

    while(nrow(unprotected_matrix) > 0){

      current_pick <- sum(current_pick, 1)

      slopes <- calculate_slopes(objective = "carbon",
                                 current_state = current_state,
                                 max_slopes = max_slope_carbon)

      delta <- unprotected_matrix%*%as.matrix(slopes)

      best_cell_indeces <- doBy::which.maxn(delta, n = step_size)

      best_cells <- delta[best_cell_indeces,] %>%
        enframe() %>%
        set_names(c('cell_id', "delta")) %>%
        mutate(pick_order = current_pick)

      ranking <- bind_rows(ranking, best_cells)

      current_state <- unprotected_matrix[best_cell_indeces, ,drop = F] %>%
        colSums() +
        current_state

      unprotected_matrix <- unprotected_matrix[-best_cell_indeces, , drop = F]
    }
  } else if (objective == "food"){

    current_state <- baseline_state[ , stock_names]

    unprotected_matrix <- unprotected_matrix[ ,stock_names]

    while(nrow(unprotected_matrix) > 0){

      current_pick <- sum(current_pick, 1)

      slopes <- calculate_slopes(objective = "food",
                                 current_state = current_state,
                                 effort_assumption = effort_assumption)

      delta <- unprotected_matrix%*%as.matrix(slopes)

      best_cell_indeces <- doBy::which.maxn(delta, n = step_size)

      best_cells <- delta[best_cell_indeces,] %>%
        enframe() %>%
        set_names(c('cell_id', "delta")) %>%
        mutate(pick_order = current_pick)

      ranking <- bind_rows(ranking, best_cells)

      current_state <- unprotected_matrix[best_cell_indeces, ,drop = F] %>%
        colSums() +
        current_state

      unprotected_matrix <- unprotected_matrix[-best_cell_indeces, , drop = F]
    }
  }
  ranking$cell_id <- as.integer(ranking$cell_id)

  return(ranking)
}
