#funciones
sum_round_vector <- function(x, add, maximum){
  new.x <- x + add
  while(length(which(new.x > maximum)) != 0){
    new.x[which(new.x > maximum)] <- new.x[which(new.x > maximum)] - maximum
  }
  while(length(which(new.x < 1)) != 0){
    new.x[which(new.x < 1)] <- new.x[which(new.x < 1)] + maximum
  }
  return(new.x)
} #suma valores a un vector circular (si excede un valor maximo, se retorna al inicio)

circular_comb <- function(x, maximum){
  new.xs <- lapply(1:(maximum-1), function(i) sum_round_vector(x, i, maximum))
  new.xs <- unique(lapply(new.xs, sort))
  # new.xs <- new.xs[-which(sapply(new.xs, function(i) all(i%in%x)))]
  return(new.xs)
} #brinda todas las posibles combinaciones iguales de un vector circular

regular_poly_coords <- function(vertex){
  angle <- 2 * pi * (seq_len(vertex) - 1) / vertex
  x <- cos(angle)
  y <- sin(angle)
  
  coords <- data.frame(y, x)
  return(coords)
}

tube_position <- function(polygon_coords, tubes_number, tolerance = 0){
  #numero de combinaciones de posiciones
  posiciones <- combn(1:nrow(polygon_coords), tubes_number)
  
  #como es circular, si fijamos el punto 1, todas las demas son repeticiones
  posiciones <- as.matrix(posiciones[,posiciones[1,]==1])
  
  #reteniendo solo aquellos patrones que presentan un centro de masa en el medio
  patrones <- list()
  for(i in 1:ncol(posiciones)){
    coord <- polygon_coords[posiciones[,i] ,]
    Xdev <- sum(coord$x)
    Ydev <- sum(coord$y)
    magnitud <- round(sqrt(Xdev^2+Ydev^2), 5)
    if(magnitud<=(tolerance/100)){
      patrones <- c(patrones, list(coord))
    }
  }
  
  #descartando aleatoriamente patrones de tubos iguales por rotacion
  a <- 1
  while(a < length(patrones)){
    sel_posiciones <- lapply(patrones, function(k) as.numeric(rownames(k)))
    
    newpos <- circular_comb(sel_posiciones[[a]], nrow(polygon_coords))
    coincidencias <- sapply(1:length(newpos), function(lp_np) {
      sapply(sel_posiciones, function(sel_pos) all(sel_pos %in% newpos[[lp_np]]))
    })
    del <- which(apply(coincidencias, 1, sum)>0)
    if(length(del)>1){
      no.del <- sample(del, 1)
      trans.patrones <- patrones[no.del]
      patrones <- patrones[-del]
      patrones <- c(patrones, trans.patrones)
    }
    a <- a+1
  }
  
  if(length(patrones)==0) patrones <- list(patrones)
  names(patrones)[1] <- tubes_number
  
  return(patrones)
}

centrifuge_plot <- function(polygon_coords, selected_coords, dot_size=3,
                            dot_color1="#27499d", dot_color2="gray30", 
                            number_size=5, row_num=1, col_num=1){
  par(mfrow=c(row_num, col_num))
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

# polygon_coords <- regular_poly_coords(10)
# patrones <- tube_position(polygon_coords, 1, tolerance = 5)
# patrones
# lapply(lapply(patrones, function(k) apply(k, 2, sum)), function(j) sqrt(j[1]^2+j[2]^2))
# centrifuge_plot(polygon_coords, patrones, col_num=2, row_num=1, dot_size = 1.5)
