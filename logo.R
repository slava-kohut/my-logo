#
# Slava's logo
#
library(ggplot2)
library(RColorBrewer)

#
# Goldstein-Price test function
#
logo_func <- function(x, y) {
  term1 <- 1 + (x + y + 1)^2*(19 - 14*x + 3*x^2 - 14*y + 6*x*y + 3*y^2)
  term2 <- 30 + (2*x - 3*y)^2*(18 - 32*x + 12*x^2 + 48*y - 36*x*y + 27*y^2)
  term1*term2
}

logo_grid <- expand.grid(seq(-2, 2, length.out = 100),
                         seq(-2, 2, length.out = 100))

logo_grid$value <- purrr::map2_dbl(logo_grid$Var1, logo_grid$Var2, logo_func)

ggplot(data = logo_grid, aes(Var1, Var2)) +
 geom_raster(aes(fill = value), interpolate = T) +
  scale_fill_gradientn(colours =  brewer.pal(11,'Spectral'), trans = 'log10', limits = c(10, 1.e6),
                       oob = scales::squish) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(-0.7, -0.7, -0.7, -0.7), "cm")) +
ggsave('C:/Users/Slava.Kohut/Desktop/1.png', device = 'jpg', width = 10, height = 10, units = 'cm')

#
# magma, viridis, cividis, inferno, plasma
#
ggplot(data = logo_grid, aes(Var1, Var2)) +
  geom_raster(aes(fill = value), interpolate = T) +
  scale_fill_gradientn(colours =  viridis::inferno(11), trans = 'log10', limits = c(10, 1.e6),
                       oob = scales::squish) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(-0.7, -0.7, -0.7, -0.7), "cm")) +
  ggsave('C:/Users/Slava.Kohut/Desktop/2.png', device = 'jpg', width = 10, height = 10, units = 'cm')