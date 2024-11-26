get_matrix_dimensions <- function(elements) {
  best_rows <- NULL
  best_cols <- NULL
  min_diff <- Inf
  
  for (rows in 1:ceiling(sqrt(elements))) {
    cols <- ceiling(elements / rows)
    
    if (abs(cols - 2 * rows) <= min_diff) {
      best_rows <- rows
      best_cols <- cols
      min_diff <- abs(cols - 2 * rows)
    }
  }
  
  return(list(rows = best_rows, cols = best_cols))
} # Determine the best distribution for any number of samples

regular_poly_coords <- function(vertex){
  angle <- 2 * pi * (seq_len(vertex) - 1) / vertex
  x <- cos(angle)
  y <- sin(angle)
  
  coords <- data.frame(y, x)
  return(coords)
}# get coordinates of a regular polygon

get_minimal_representation <- function(v) {
  n <- length(v)
  
  v_str <- paste(v, collapse = "")
  v_double_str <- paste0(v_str, v_str)
  rotations <- substring(v_double_str, seq_len(n), seq(n, 2*n - 1))
  
  v_rev_str <- paste(rev(v), collapse = "")
  v_rev_double_str <- paste0(v_rev_str, v_rev_str)
  rev_rotations <- substring(v_rev_double_str, seq_len(n), seq(n, 2*n - 1))
  
  all_rotations <- c(rotations, rev_rotations)
  min_rotation_str <- min(all_rotations) # Find the lexicographically minimal string representation
  min_rotation <- as.numeric(strsplit(min_rotation_str, "")[[1]])
  
  return(min_rotation)
} 
# From a vector of positions, find the lexicographically equivalent to the vector.
# for example: c(1, 0, 0, 1) → c(0, 0, 1, 1)


#### Second version####
centrifuge_distribution <- function(slot_number, tube_number, tolerance = 0) {
  positions <- combn(slot_number - 1, tube_number - 1)
  
  unique_patterns_set <- new.env(hash = TRUE, parent = emptyenv())
  
  coords <- regular_poly_coords(slot_number)
  
  for (i in seq_len(ncol(positions))) {
    pos_ones <- positions[, i]
    binary_vector <- rep(0, slot_number)
    binary_vector[1] <- 1
    binary_vector[pos_ones + 1] <- 1
    
    #verifying the center of mass
    logical_vector <- as.logical(binary_vector)
    
    Xdev <- sum(coords$x[logical_vector])
    Ydev <- sum(coords$y[logical_vector])
    
    magnitud <- round(sqrt(Xdev^2 + Ydev^2), 5)
    
    if(magnitud <= tolerance/100){
      min_representation <- get_minimal_representation(binary_vector)
      pattern_string <- paste(min_representation, collapse = "")
      assign(pattern_string, NULL, envir = unique_patterns_set)
    }
  }
  
  unique_patterns <- ls(envir = unique_patterns_set)
  
  all_permutations <- do.call(rbind, lapply(unique_patterns, function(pat) {
    as.numeric(strsplit(pat, "")[[1]])
  }))
  
  return(all_permutations)
}

# Returns a binary matrix where 0 represent an empty slot and a 1 represent
# a filled slot. Function considers repetition by circularization and avoid it.
# In addition, the function filter unbalanced (according threshold) distributions

centrifuge_positions <- function(slot_number, tube_number, tolerance = 0, randomize = T){
  
  distribution <- centrifuge_distribution(slot_number, tube_number, tolerance = tolerance)
  if(is.null(distribution)) return(setNames(list(NULL), tube_number))
  distribution_transformed <- distribution == 1
  positions <- t(apply(distribution_transformed, 1, function(row) which(row)))
  
  if(randomize){
    adds <- sample(0:(slot_number - 1), nrow(positions), replace = TRUE)
    new_positions <- positions + adds
    new_positions[new_positions > slot_number] <- new_positions[new_positions > slot_number] - slot_number
    positions <- t(apply(new_positions, 1, sort))
  }
  
  polygon_coords <- regular_poly_coords(slot_number)
  patterns <- lapply(1:nrow(positions), function(i) polygon_coords[positions[i, ], ])
  names(patterns)[1] <- tube_number
  
  return(patterns)
}
# having the matrix with all filled slots, this functions returns only
# filled positions. In addition, randomize this positions consider the circular
# nature of the centrifuge

centrifuge_plot <- function(polygon_coords, selected_coords, dot_size=3,
                            dot_color1="#27499d", dot_color2="gray30", 
                            number_size=5, row_num=1, col_num=1){
  par(mfrow=c(row_num, col_num))
  par(xpd = TRUE)
  par(pty = "s")
  par(mar = rep(1.5, 4)+0.1)
  
  if(length(selected_coords)==0) selected_coords <- list(selected_coords)
  
  iteraciones <- min(length(selected_coords), col_num*row_num)
  for(i in 1:iteraciones){
    plot(polygon_coords, cex = dot_size, col = dot_color2, axes = F, xlab = "", ylab = "")
    if(length(selected_coords[[i]]) > 0){
      points(selected_coords[[i]], pch = 16, cex = dot_size, col = dot_color1)
    }else points(0, 0, pch=4, col = dot_color2, cex=200,  lwd = 3)
    number <- ifelse(length(selected_coords[[i]])==0, names(selected_coords)[i], nrow(selected_coords[[i]]))
    if(number_size!=0) text(0, 0, number, cex=number_size)
  }
}

# slot_number <- 12
# tube_number <- 4
# polygon_coords <- regular_poly_coords(slot_number)
# patrones <- centrifuge_positions(slot_number, tube_number, tolerance = 0)
# centrifuge_plot(polygon_coords, patrones, col_num=2, row_num=1, dot_size = 1.5)
