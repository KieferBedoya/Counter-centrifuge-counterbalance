# combn_simplified <- function(x, m) {
#   x <- seq_len(x)
#   n <- length(x)
#   m <- as.integer(m)
#   e <- 0
#   h <- m
#   a <- seq_len(m) 
#   count <- as.integer(round(choose(n, m)))  # Total number of combinations
#   
#   out <- matrix(0, nrow = m, ncol = count)  # Preallocate output matrix
#   out[, 1L] <- x[a]  # Store the first combination
#   
#   i <- 2L
#   nmmp1 <- n - m + 1L
#   while (a[1L] != nmmp1) {  # Generate all combinations
#     if (e < n - h) {
#       h <- 1L
#       e <- a[m]
#       j <- 1L
#     } else {
#       e <- a[m - h]
#       h <- h + 1L
#       j <- 1L:h
#     }
#     a[m - h + j] <- e + j  # Update indices
#     out[, i] <- x[a]  # Store the current combination
#     i <- i + 1L
#   }
#   out  # Return the result as a matrix
# }


#### Functions for Sivek's theorem ####
# Check if a target can be represented as a nonnegative linear combination of given primes
# This is similar to a coin change problem. We use a DP array where dp[i+1] indicates
# whether we can form the sum 'i' using the given primes.
canRepresent <- function(primes, k) { #dynamic programming 
  if (k == 0) return(TRUE)
  
  dp <- rep(FALSE, k + 1) #let's find sums from 0 to k
  dp[1] <- TRUE  # sum=0 is always possible
  
  for (p in primes) {
    if (p <= k) {
      for (val in p:k) {
        if (dp[(val - p) + 1]) {
          dp[val + 1] <- TRUE
        }
      }
    }
  }
  
  dp[k + 1]
}

# Check if the centrifuge is balanced according to Sivek's theorem
# slot_number = n, tube_number = k
# Conditions (from Sivek's Theorem):
# 1) If they are not coprimes then it's balanced
# 2) If two coprime primes divide n, for large enough k, k and n-k can be formed by these primes
is.balanced <- function(slot_number, tube_number) {
  # Trivial balanced cases
  if (tube_number == 0 || tube_number == slot_number) return(TRUE)

  prime_factors <- unique(numbers::primeFactors(slot_number))
  
  # If they are not coprimes then it's balanced
  if (!numbers::coprime(slot_number, tube_number)) return(TRUE)
  
  k <- tube_number
  n_minus_k <- slot_number - tube_number
  
  # If one prime factor: k and n-k must both be multiples of that prime
  if (length(prime_factors) == 1) {
    p <- prime_factors[1]
    return((k %% p == 0) && (n_minus_k %% p == 0))
  }
  
  # More than two prime factors: just check representability via DP
  canRepresent(prime_factors, k) && canRepresent(prime_factors, n_minus_k)
}


#### Functions for centrifuge ####
regular_poly_coords <- function(vertex){
  angle <- 2 * pi * (seq_len(vertex) - 1) / vertex
  x <- cos(angle)
  y <- sin(angle)
  
  coords <- data.frame(y, x)
  return(coords)
}# get coordinates of a regular polygon

get_minimal_representation <- function(v) {
  v <- as.integer(v)
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

# If the numbers of holes and the number of tubes are not coprimes, and we only need
# one possible distribution, this can be obtained directly
unique_distribution <- function(slot_number, tube_number){
  if(numbers::coprime(slot_number, tube_number)) return(NULL)
  
  primes <- numbers::Primes(slot_number)
  
  for (n in primes) {
    if (slot_number %% n == 0 && tube_number %% n == 0) {
      ones_length <- tube_number / n
      zeros_length <- (slot_number - tube_number) / n

      distribution <- rep(c(1, 0), times = c(ones_length, zeros_length))
      
      return(matrix(distribution, nrow = 1, ncol = slot_number))
    }
  }
}

total_distribution <- function(slot_number, tube_number, tolerance = 0, unique = F) {
  # check this for tolerance
  if(slot_number == tube_number) return(matrix(1, 1, slot_number))
  if(tube_number == 1) return(NULL)
  if(tube_number == 2 && slot_number%%2 == 0) return(unique_distribution(slot_number, tube_number))
  if(tube_number == 2 && slot_number%%2 != 0) return(NULL)
  if(!is.balanced(slot_number, tube_number)) return(NULL)
  
  selected_patterns_set <- new.env(hash = TRUE, parent = emptyenv())
  coords <- regular_poly_coords(slot_number)
  
  tol_sq <- (tolerance/100)^2
  binary_vector <- integer(slot_number)

  ### combn code
  x <- seq_len(slot_number - 1)
  m <- as.integer(tube_number - 1)

  n <- length(x)
  e <- 0
  h <- m
  a <- seq_len(m)

  pos_ones <- x[a]  # Store the first combination

  i <- 2L
  nmmp1 <- n - m + 1L
  while (a[1L] != nmmp1) {
    if (e < n - h) {
      h <- 1L
      e <- a[m]
      j <- 1L
    } else {
      e <- a[m - h]
      h <- h + 1L
      j <- 1L:h
    }
    a[m - h + j] <- e + j  # Update indices

    ### function code
    pos_ones <- x[a]  # Store the current combination
    binary_vector[] <- 0L #reset binary vector
    binary_vector[1] <- 1L
    binary_vector[pos_ones + 1L] <- 1L

    #verifying the center of mass
    logical_vector <- (binary_vector == 1L)

    Xdev <- sum(coords$x[logical_vector])
    Ydev <- sum(coords$y[logical_vector])

    magnitud <- round(Xdev^2 + Ydev^2, 5)

    if(magnitud <= tolerance){
      min_representation <- get_minimal_representation(binary_vector)
      pattern_string <- paste0(min_representation, collapse = "")
      assign(pattern_string, NULL, envir = selected_patterns_set)
      if(unique) break
    }

    i <- i + 1L
  }

  selected_patterns <- ls(envir = selected_patterns_set)

  all_permutations <- do.call(rbind, lapply(selected_patterns, function(pat) {
    as.integer(strsplit(pat, "")[[1]])
  }))

  return(all_permutations)
}

centrifuge_distribution <- function(slot_number, tube_number, tolerance = 0, unique = F){
  slot_number <- as.integer(slot_number)
  tube_number <- as.integer(tube_number)
  if (unique) {
    all_permutations <- unique_distribution(slot_number, tube_number)
    if (!is.null(all_permutations)) return(all_permutations)
  }
  return(total_distribution(slot_number, tube_number, tolerance = tolerance, unique = unique))
}

# Returns a binary matrix where 0 represent an empty slot and a 1 represent
# a filled slot. Function considers repetition by circularization and avoid it.
# In addition, the function filter unbalanced (according threshold) distributions

centrifuge_positions <- function(slot_number, tube_number, tolerance = 0, unique = F, randomize = T){
  
  distribution <- centrifuge_distribution(slot_number, tube_number, tolerance = tolerance, unique = unique)
  if(is.null(distribution)) return(setNames(list(NULL), tube_number))
  distribution_transformed <- (distribution == 1L)
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

#### Functions for plotting ####
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

#### Functions for shiny ####
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



# slot_number <- 26
# tube_number <- 12
# polygon_coords <- regular_poly_coords(slot_number)
# patrones <- centrifuge_positions(slot_number, tube_number, tolerance = 0)
# centrifuge_plot(polygon_coords, patrones, col_num=2, row_num=1, dot_size = 1.5)
# 
# start <- Sys.time()
# xd <- centrifuge_distribution(30, 17, unique = TRUE)
# Sys.time()-start

