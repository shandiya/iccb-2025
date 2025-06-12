library(sf)
library(tidyverse)
library(rnaturalearth)
library(ggtext)
library(showtext)
library(gganimate)
library(galah)
library(ambient)
library(MetBrewer)
library(ggnewscale)

showtext_auto()
font_add_google("Red Hat Display", "red_hat")

# title image ----------
make_wavy_circle <- function(center = c(0, 0),
                             radius,
                             n_points = 200,
                             noise_amplitude = 0.2,
                             frequency = 1,
                             seed = NULL) {
  
  theta <- seq(0, 2 * pi, length.out = n_points + 1)[-1]
  
  # Circular coordinates to sample seamless 2D Perlin noise
  x_noise <- cos(theta) * frequency
  y_noise <- sin(theta) * frequency
  r_noise <- gen_perlin(x = x_noise, y = y_noise)
  
  # Modulate radius with periodic noise
  r <- radius + noise_amplitude * r_noise
  
  tibble(x = center[1] + r * cos(theta),
         y = center[2] + r * sin(theta)) %>% 
    bind_rows(slice(., 1)) 
}

yearly_counts <- galah_call() |> 
  apply_profile(ALA) |> 
  filter(year >= 1900, year < 2024) |> 
  group_by(year) |> 
  atlas_counts()

df_wavy_circles <- yearly_counts |> 
  mutate(year = as.numeric(year),
         radius = scales::rescale(year, to = c(2, 42)),
         amplitude = scales::rescale(year, to = c(0.1, 0.9))) |>
  rowwise() |> 
  mutate(frequency = sample(2:8, 1)) |> 
  pmap_dfr(function(year, count, radius, frequency, amplitude) {
    make_wavy_circle(radius = radius, 
                     noise_amplitude = amplitude, 
                     frequency = frequency) |>
      mutate(year = year, count = count)
  })


df_text <- tibble(x = seq(-0.5, -25, length.out = 5),
                  y = seq(-3, -25, length.out = 5),
                  text = c("1900", "1925", "1950", "1975", "2000"),
                  colour = c(rep("dark", 2), rep("light", 3)))

ggplot() +
  geom_polygon(data = df_wavy_circles, 
               aes(x, y, 
                   group = year, 
                   colour = log(count)),
               fill = NA, 
               linewidth = 3) +
  scale_colour_gradientn(colors = met.brewer("Tam")) +
  coord_equal() +
  new_scale_color() +
  geom_text(data = df_text,
            aes(x = x, y = y, label = text, colour = colour)) +
  scale_color_manual(values = c("#ababab", "#dedede")) +
  theme_void() +
  theme(legend.position = "none")

ggsave("images/title.png", height = 9, width = 16, units = "in")


# GBIF map ------

world <- ne_countries(scale = "medium", returnclass = "sf")

gbif_tbl <- readRDS("data/gbif_tbl.RDS")

countries <- gbif_tbl |> 
  mutate(Participant = str_remove(Participant, ",.*")) |> 
  mutate(Participant = if_else(Participant == "Korea", 
                               "Republic of Korea", 
                               Participant)) |>
  full_join(world, by = join_by(Participant == name_long)) |> 
  select(Participant, Membership, geometry) |> 
  st_as_sf()

caption <- "<span style = 'color:#808E33;'>Voting participants</span> & <span style = 'color:#9c4d90;'>associate country participants</span> in the GBIF network"

ggplot(countries) +
  geom_sf(aes(fill = Membership), colour = "#ababab") +
  scale_fill_manual(values = c( "#9c4d90", "#808E33"),
                    na.value = "#efefef") +
  labs(caption = caption) +
  theme_void() +
  theme(plot.caption = element_textbox(hjust = 0.5, 
                                       family = "red_hat", 
                                       size = 100,
                                       colour = "#727272"), 
        legend.position = "none")

ggsave("images/gbif_network.png", height = 9, width = 16, units = "in")


# Animated line graph ALA records -----

yearly_counts <- galah_call() |> 
  filter(year >= 1900) |>
  group_by(year) |> 
  atlas_counts()

counts_until_1900 <- galah_call() |> 
  filter(year < 1900) |>
  atlas_counts() |> 
  pull(count)

counts_without_year <- galah_call() |> 
  filter(is.na(year)) |>
  atlas_counts() |> 
  pull(count)

yearly_cumsum <- yearly_counts |> 
  mutate(year = as.numeric(year),
         # add counts up until 1900
         count = if_else(year == 1900, 
                         count + counts_until_1900 + counts_without_year, 
                         count)) |> 
  arrange(year) |> 
  mutate(cumul_count = cumsum(count))

p <- ggplot(yearly_cumsum, 
            aes(x = year, 
                y = cumul_count, 
                colour = cumul_count)) +
  geom_point(size = 2) +
  geom_line(aes(group = 1), linewidth = 1.2) +
  scale_y_continuous(name = NULL, 
                     labels = scales::label_comma(),
                     limits = c(0, 155000000),
                     n.breaks = 4) +
  scale_x_continuous(name = NULL, 
                     limits = c(1900, 2025),
                     breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  scale_color_gradient(low = "#bcc99d", high = "#344118") +
  theme_minimal() + 
  theme(legend.position = "none",
        text = element_text(size = 30, colour = "#999999"), 
        plot.background = element_rect(fill = "#fffaf3", colour = "#fffaf3"),
        panel.background = element_rect(fill = "#fffaf3", colour = "#fffaf3"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "#efefef")) +
  transition_reveal(year) 

animate(p, 
        #renderer = gifski_renderer(loop = FALSE)
        width = 1000, 
        height = 600, 
        fps = 20,
        duration = 15, 
        end_pause = 20)

anim_save("images/ala_records.gif")
