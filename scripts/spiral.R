library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)
library(viridis)
library(gifski)

set.seed(42)
n_points <- 200
time_steps <- 60

df <- data.frame(
  point_id = 1:n_points,
  angle = seq(0, 10 * pi, length.out = n_points),
  radius_start = seq(1, 10, length.out = n_points),
  time = 1
)


frames <- list()
for(t in 1:time_steps){
  
  df_t <- df %>%
    mutate(
      radius = radius_start + (t*0.1),
      x = radius * cos(angle + t*0.1),
      y = radius * sin(angle + t*0.1),
      time = t
    )
  
  frames[[t]] <- df_t
}

df_all <- do.call(rbind, frames)


p <- ggplot(df_all, aes(x = x, y = y, color = radius)) +
  geom_point(size = 2) +
  scale_color_viridis(option = "magma") +
  coord_equal() +
  theme_void() +
  labs(title = "Rotating Spiral, Time: {frame_time}") +
  transition_time(time)


anim <- animate(p, nframes = time_steps, fps = 10, renderer = gifski_renderer(), width = 600, height = 500)

anim_save("rotating_spiral.gif", animation = anim)