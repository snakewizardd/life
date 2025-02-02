library(ggplot2)
library(gganimate)
library(dplyr)

x <- seq(-pi, pi, length.out = 100)
y <- seq(-pi, pi, length.out = 100)
grid <- expand.grid(x = x, y = y)

times <- seq(0, 2 * pi, length.out = 40)
time_df <- data.frame(t = times)

df <- merge(grid, time_df)
df <- df %>% mutate(z = sin(x^2 + y^2 + t))

p <- ggplot(df, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme_minimal() +
  labs(title = 't = {frame_time}') +
  transition_time(t)

animate(p, nframes = 40, fps = 10, renderer = gifski_renderer("./outputs/high_iq_animation.gif"))
