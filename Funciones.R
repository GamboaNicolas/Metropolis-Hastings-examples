plot_hist <- function(muestra, d_objetivo) {
    muestra |> 
    ggplot() +
    geom_histogram(aes(x = x, after_stat(density)), color = "black", fill = "dodgerblue3", bins = 40) +
    geom_line(aes(x = x, y = d_objetivo(x)), color = "red", linewidth = 1.2) +
    labs(x = "x", y = "Densidad")
}

plot_trace <- function(muestra) {
  muestra |>
    ggplot() +
    geom_line(aes(x = iteracion, y = x)) +
    labs(x = "Iteración", y = "Muestra")
}

plot_autocor <- function(muestra) {
  tibble(
    rezago = 0:20,
    autocorrelacion = acf(muestra$x,lag.max = 20,plot = F)$acf
  ) |> 
    ggplot(aes(x = rezago, y = autocorrelacion))+
    geom_point(size = 2)+
    geom_col(width = 0.07)+
    geom_line(linewidth = 1, color = "blue")+
    geom_hline(yintercept = 0, linewidth = 0.9) +
    labs(x = "Rezago", y = "Autocorrelación")
}

plot_hotmap <- function(muestra, d_objetivo, puntos = T) {
  
  if (ncol(muestra) != (2+1)) {
    
  }
  
  df_grilla <- expand.grid(x = seq(min(muestra$dim_1),max(muestra$dim_1), length.out = 200),
                           y = seq(min(muestra$dim_2),max(muestra$dim_2), length.out = 200))
  df_grilla$z <- d_objetivo(as.matrix(df_grilla))
  
  hotmap <- muestra |> 
    ggplot(aes(x = dim_1, y = dim_2)) +
    geom_density2d_filled() +
    stat_contour(aes(x = x, y = y, z = z), data = df_grilla, col = "white", alpha = 0.5) +
    # labs(subtitle = "En blanco la distribución objetivo")
    theme(legend.title = "Densidad")
  if (puntos) {
    hotmap +
      geom_point(color = "black", 
                 fill = "firebrick2", 
                 size = 2, pch = 21, 
                 alpha = 0.3)
  } else {
    return(hotmap)
  }
}



