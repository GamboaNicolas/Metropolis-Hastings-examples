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
