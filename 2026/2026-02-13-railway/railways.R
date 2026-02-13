# Railway lines around Ireland -------------------------------------------------
library(osmdata)
library(dplyr)
library(showtext)
library(ggplot2)

# Fonts ------------------------------------------------------------------------
font_add_google("Inter")
map_font <- "Inter"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

# Map annotation
tag <- stringr::str_glue(
  "<span style='font-family:{map_font}; font-weight:bold; font-size:8pt; color:#e6e6e6;'>RAILWAYS OF IRELAND</span>
  <span style='font-family:{map_font}; font-size:5pt; color:#e6e6e6;'>Active and former railways</span>
  "
)

# Social data
social <- cmBrand::social_brand()

# Ireland + UK shape file ------------------------------------------------------
ireland <- rnaturalearth::ne_countries(
  scale = "medium",
  country = c("Ireland", "United Kingdom"),
  returnclass = "sf"
)

# Irish railway data from OSM --------------------------------------------------
irish_railways <- getbb("Ireland") |>
  opq(timeout = 100) |>
  add_osm_feature(
    key = "railway",
    value = c("abandoned", "rail")
  ) |>
  osmdata_sf()

irish_lines <- irish_railways |>
  purrr::pluck("osm_lines") |>
  filter(railway %in% c("rail", "abandoned")) |>
  sf::st_transform(2157)

# Plot -------------------------------------------------------------------------
ggplot() +
  # UK
  geom_sf(
    data = ireland |> filter(admin == "United Kingdom"),
    fill = "#161616",
    colour = NA
  ) +
  # Ireland
  geom_sf(
    data = ireland |> filter(admin == "Ireland"),
    fill = "#1b1b1b",
    colour = "grey10"
  ) +
  # Active railways (outer glow)
  geom_sf(
    data = irish_lines |> filter(railway == "rail"),
    colour = "#f5f5f5",
    linewidth = 1.2,
    alpha = 0.08,
    lineend = "round"
  ) +
  # Active railways (inner glow)
  geom_sf(
    data = irish_lines |> filter(railway == "rail"),
    colour = "#f5f5f5",
    linewidth = 0.9,
    alpha = 0.18,
    lineend = "round"
  ) +
  # Active railways (core glow)
  geom_sf(
    data = irish_lines |> filter(railway == "rail"),
    colour = "#eaeaea",
    linewidth = 0.45,
    lineend = "round"
  ) +
  # Abandoned railways
  geom_sf(
    data = irish_lines |> filter(railway == "abandoned"),
    colour = "#7a7a7a",
    linewidth = 0.25,
    linetype = "22",
    alpha = 0.35
  ) +
  labs(
    caption = social,
    tag = tag
  ) +
  # Zooming into just Ireland and NI
  coord_sf(
    xlim = c(-11.1, -5.2),
    ylim = c(51.1, 55.5),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.caption = ggtext::element_markdown(
      hjust = 0,
      size = rel(0.4),
      family = map_font,
      colour = "#eaeaea",
      margin = margin(0, 0, 5, 5)
    ),
    plot.tag = ggtext::element_textbox_simple(
      hjust = 0,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      maxwidth = 0.5
    ),
    plot.tag.position = c(0.5, 0.1),
    plot.background = element_rect(fill = "#121212", colour = NA),
    panel.background = element_rect(fill = "#121212", colour = NA)
  ) +
  ggview::canvas(width = 3, height = 4.5) -> fig

# Export -----------------------------------------------------------------------
ggview::save_ggplot(
  fig,
  here::here("2026/2026-02-13-railway/railways.png"),
  bg = "#121212"
)
