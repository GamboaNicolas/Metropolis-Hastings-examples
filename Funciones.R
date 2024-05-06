plot_hist <- function(muestra, d_objetivo) {
    muestra |> 
    ggplot() +
    geom_histogram(aes(x = x, after_stat(density)), color = "black", fill = "dodgerblue3", bins = 40) +
    geom_line(aes(x = x, y = d_objetivo(x)), color = "red", linewidth = 1.2) +
    labs(x = "x", y = "Densidad")
}

plot_trace <- function(muestra) {
  
  if (ncol(muestra) == 3) {
    
    muestra |>
      pivot_longer(dim_1:dim_2, names_to = "dimension", values_to = "x") |> 
      ggplot() +
      geom_line(aes(x = iteracion, y = x, color = dimension), linewidth = 0.5) +
      labs(x = "Iteración", y = "Muestra") +
      facet_wrap(~dimension, ncol = 1, labeller = as_labeller(c(dim_1="x[1]", dim_2="x[2]"), default = label_parsed)) +
      theme(legend.position = "None", strip.background = element_blank())
  } else {
    
    muestra |>
      ggplot() +
      geom_line(aes(x = iteracion, y = x)) +
      labs(x = "Iteración", y = "Muestra")
  }
}

plot_autocor <- function(muestra) {
  
  
  if (ncol(muestra) == 3) {
    
    primero <- tibble(
      rezago = 0:20,
      autocorrelacion = acf(muestra$dim_1,lag.max = 20,plot = F)$acf
    ) |> 
      ggplot(aes(x = rezago, y = autocorrelacion))+
      geom_point(size = 2)+
      geom_col(width = 0.07)+
      geom_line(linewidth = 1, color = "blue")+
      geom_hline(yintercept = 0, linewidth = 0.6) +
      labs(x = "Rezago", y = "Autocorrelación", subtitle = expression(x[1]))
    
    segundo <- tibble(
      rezago = 0:20,
      autocorrelacion = acf(muestra$dim_2,lag.max = 20,plot = F)$acf
    ) |> 
      ggplot(aes(x = rezago, y = autocorrelacion))+
      geom_point(size = 2)+
      geom_col(width = 0.07)+
      geom_line(linewidth = 1, color = "blue")+
      geom_hline(yintercept = 0, linewidth = 0.6) +
      labs(x = "Rezago", y = "Autocorrelación", subtitle = expression(x[2]))
    
    primero/segundo
    
  } else {
    
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
}

plot_hotmap <- function(muestra, d_objetivo, puntos = T) {
  
  df_grilla <- expand.grid(x = seq(min(muestra$dim_1),max(muestra$dim_1), length.out = 200),
                           y = seq(min(muestra$dim_2),max(muestra$dim_2), length.out = 200))
  df_grilla$z <- d_objetivo(df_grilla)
  df_grilla <- as.matrix(df_grilla)
  
  hotmap <- muestra |> 
    ggplot(aes(x = dim_1, y = dim_2)) +
    geom_density2d_filled() +
    stat_contour(aes(x = x, y = y, z = z), data = df_grilla, col = "white", alpha = 0.5) +
    labs(x = expression(x[1]),
         y = expression(x[2]), fill = expression(p^"*" ~ "("~x[1]~", "~x[2]~" | a, b)"))
    
  
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


W <- function(muestra) {
  mean(apply(muestra, 2, var))
}

B <- function(muestra) {
  media_cadenas <- apply(muestra, 2, mean)
  var(media_cadenas)*nrow(muestra)
}

R_hat <- function(muestra) {
  
  S <- nrow(muestra)
  M <- ncol(muestra)
  W <- W(muestra)
  B <- B(muestra)
  
  sqrt(((S-1)/S * W + 1/S*B) / W)
}


n_eff_calculo <- function(x) {
  
  s <- length(x)
  
  autocorrelaciones <- acf(x, plot = F, lag.max = Inf)$acf
  limite <- which(autocorrelaciones < 0.025)[1] # Se agrega un limite para despreciar correlaciones muy chicas
  
  s / (1 + 2 * sum(autocorrelaciones[2:limite]))
  
}

n_eff <- function(x) {
  
  apply(x, 2, n_eff_calculo)
  
}



plot_path <- function(muestra, d_objetivo, paths = 500) {
  
  df_grilla <- expand.grid(x = seq(min(muestra$dim_1),max(muestra$dim_1), length.out = 200),
                           y = seq(min(muestra$dim_2),max(muestra$dim_2), length.out = 200))
  df_grilla$z <- d_objetivo(df_grilla)
  df_grilla <- as.matrix(df_grilla)
  
  
  muestra |>
    mutate(dim_1_next = c(dim_1[-1], dim_1[nrow(muestra)]),
           dim_2_next = c(dim_2[-1], dim_2[nrow(muestra)])) |>
    slice_head(n = paths) |> 
    ggplot(aes(x = dim_1, y = dim_2)) +
    geom_segment(aes(x = dim_1, xend= dim_1_next, y = dim_2, yend = dim_2_next, alpha = iteracion), color = "#202C59") +
    geom_point(aes(x = dim_1, y = dim_2, alpha = iteracion), color = "#202C59") +
    geom_point(aes(x = dim_1, y = dim_2), data = muestra[1,],color = "#F18805", size = 3, pch = 17, alpha = 1) +
    stat_contour(aes(x = x, y = y, z = z), data = df_grilla, col = "black") +
    labs(x = expression(x[1]),
         y = expression(x[2]), fill = expression(p^"*" ~ "("~x[1]~", "~x[2]~" | a, b)")) + 
    theme(legend.position = "none")
}


