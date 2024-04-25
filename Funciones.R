# plot_hist <- function(x, obj) {
#   
#   data.frame(x = seq_along(x), y = x,
#              y_obj = obj(x)) %>% 
#     ggplot() +
#     geom_histogram(aes(x = y, after_stat(density)), color = "black", fill = "dodgerblue3") +
#     geom_line(aes(x = y, y = y_obj), color = "red", linewidth = 1.2)
# }
# 
# plot_trace <- function(x) {
#   data.frame(x = seq_along(x), y = x) %>% 
#     ggplot() +
#     geom_line(aes(x = x, y = y))
# }