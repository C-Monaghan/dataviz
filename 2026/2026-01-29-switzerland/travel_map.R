# Packages ---------------------------------------------------------------------
pacman::p_load(
  dplyr,
  stringr,
  purrr,
  sf,
  rnaturalearth,
  ggplot2,
  showtext
)

# Fonts ------------------------------------------------------------------------
font_add("fa7-brands", here::here("fonts/FA-7-Regular.otf"))
font_add_google("Montserrat")
font_add_google("Source Sans Pro")

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

title_font <- "Montserrat"
body_font <- "Source Sans Pro"

# Setting up caption -----------------------------------------------------------
tt <- str_glue(
  "**Data**: Personal Google timeline data &bull; **Date:** Jan 18-23 2026 <br>"
)
li <- str_glue("<span style='font-family:fa7-brands'>&#xf08c;</span>")
gh <- str_glue("<span style='font-family:fa7-brands'>&#xf09b;</span>")
bs <- str_glue("<span style='font-family:fa7-brands'>&#xe671;</span>")

caption_text <- str_glue(
  "{tt} {li} c-monaghan &bull; {gh} c-monaghan &bull; {bs} c-monaghan &bull; #rstats #ggplot2"
)

# Theme ------------------------------------------------------------------------
theme_map <- function() {
  theme_void(base_size = 12, base_family = body_font) +
    theme(
      plot.title = ggtext::element_textbox_simple(
        size = rel(1.6),
        face = "bold",
        colour = "#2c3e50",
        family = title_font,
        margin = margin(t = 15, r = 0, b = 5, l = 0)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = rel(1),
        colour = "#2c3e50",
        family = title_font,
        margin = margin(t = 0, r = 0, b = 5, l = 0)
      ),
      plot.caption = ggtext::element_markdown(
        size = rel(0.8),
        colour = "#999999",
        lineheight = 1.2,
        hjust = 0
      ),
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold", color = "#2c3e50"),
      legend.text = element_text(size = 9, color = "#34495e"),
      legend.margin = margin(t = 10, b = 5),
      legend.key.width = unit(1.2, "cm"),
      legend.key.height = unit(0.3, "cm")
    )
}

# Read in timeline data --------------------------------------------------------
data <- jsonlite::read_json(
  here::here("2026/2026-01-29-switzerland/data/timeline.json"),
  simplifyVector = TRUE
) |>
  purrr::pluck("semanticSegments") |>
  tibble::as_tibble()

# Extracting coordinates from data ---------------------------------------------
coords_data <- data |>
  janitor::clean_names() |>
  mutate(
    start_time = lubridate::ymd_hms(start_time),
    end_time = lubridate::ymd_hms(end_time),
    year = lubridate::year(start_time),
    month = lubridate::month(start_time, label = TRUE),
    day = lubridate::day(start_time),
    wday = lubridate::wday(start_time, label = TRUE)
  ) |>
  filter(!purrr::map_lgl(timeline_path, is.null)) |>
  tidyr::unnest(timeline_path) |>
  tidyr::separate(
    col = point,
    into = c("lon", "lat"),
    sep = ",\\s*",
    remove = TRUE
  ) |>
  mutate(
    lat = as.numeric(stringr::str_remove(lat, "°")),
    lon = as.numeric(stringr::str_remove(lon, "°")),
  ) |>
  select(start_time, end_time, year, month, day, wday, lat, lon)

# Turning into sf object -------------------------------------------------------
coords_sf <- coords_data |>
  # Getting coordinates from Switzerland trip
  filter(year == 2026, month == "Jan", day %in% c(18:23)) |>
  # Convert to an sf object
  st_as_sf(coords = c("lat", "lon"), crs = 4326, remove = FALSE) |>
  # Arrange coordinate point as trips
  # Multiple coordinates fall within a "trip window" (i.e., a 2hr timestamp)
  # We take all this coords within that timestamp and convert it into one long
  # string
  arrange(start_time) |>
  group_by(start_time, end_time) |>
  reframe(
    year = year,
    month = month,
    day = wday,
    geometry = st_cast(st_combine(geometry), "LINESTRING")
  ) |>
  # Convert to an sf object
  st_as_sf()

# Getting map data of Switzerland and surrounding area -------------------------
map_data <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(name %in% c("Switzerland", "France", "Germany"))

# Only intersting in the points around our trips
# i.e., not ALL of France of Germany
bbox <- st_bbox(coords_sf)
bbox_expanded <- bbox + c(-1.5, -1.5, 2.5, 1.5)

map_crop <- map_data |>
  st_crop(bbox_expanded) |>
  mutate(
    country_color = case_when(
      name == "Switzerland" ~ "#e8f4f8",
      TRUE ~ "#f0f0f0"
    )
  )

# Create day labels for legend
day_labels <- coords_sf |>
  st_drop_geometry() |>
  distinct(day) |>
  arrange(factor(
    day,
    levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  )) |>
  pull(day)

# Setting Basel as the "point of origin" ---------------------------------------
origin <- coords_sf |>
  st_cast("POINT") |>
  slice_min(start_time) |>
  slice_head(n = 1) |>
  mutate(name = "Basel")

# Places travelled to ----------------------------------------------------------
# Perhaps a better way of doing this?
map_labels <- tibble::tribble(
  ~place          , ~x        , ~y       , ~repel ,
  "Basel"         ,  7.559102 , 47.58803 , TRUE   ,
  "Germany"       , 10        , 49       , FALSE  ,
  "France"        ,  6.25     , 49       , FALSE  ,
  "Colmar"        ,  7.3546   , 48.0803  , TRUE   ,
  "Lauterbrunnen" ,  7.9091   , 46.5935  , TRUE   ,
  "Lucerne"       ,  8.3093   , 47.0502  , TRUE   ,
  "Freiburg"      ,  7.8421   , 47.9990  , TRUE   ,
  "Rhine Falls"   ,  8.6154   , 47.6781  , TRUE   ,
  "Zurich"        ,  8.5417   , 47.3769  , TRUE   ,
)

# Plotting ---------------------------------------------------------------------
ggplot() +
  # Base map of Switzerland
  geom_sf(
    data = map_crop,
    aes(fill = country_color),
    colour = "grey75",
    linewidth = 0.4
  ) +
  scale_fill_identity() +
  # Adding journey lines
  geom_sf(
    data = coords_sf,
    aes(colour = day, geometry = geometry),
    linewidth = 0.75,
    lineend = "round",
    linejoin = "round",
    alpha = 0.7
  ) +
  # Adding trip destinations
  geom_point(
    data = map_labels |> filter(repel == TRUE, place != "Basel"),
    aes(x = x, y = y),
    shape = 21,
    fill = "#3498db",
    colour = "black",
    size = 3,
    stroke = 1
  ) +
  # Setting Basel as "home location"
  geom_point(
    data = map_labels |> filter(place == "Basel"),
    aes(x = x, y = y),
    shape = 21,
    fill = "#e74c3c",
    colour = "black",
    size = 3.5,
    stroke = 1.5
  ) +
  # Adding labels to locations
  ggtext::geom_richtext(
    data = map_labels |> filter(!repel),
    aes(x = x, y = y, label = place),
    fill = alpha("white", 0.7),
    label.color = NA,
    colour = "#2c3e50",
    size = 3.5
  ) +
  ggrepel::geom_label_repel(
    data = map_labels |> filter(repel, place != "Basel"),
    aes(x = x, y = y, label = place),
    size = 3.5,
    box.padding = 0,
    label.size = 0.15,
    nudge_x = c(-0.5, 1, 0.75, 0, 1, -0.3),
    nudge_y = c(0.15, 0, 0.25, 0.25, 0.5, 0.45),
    fill = alpha("white", 0.9),
    colour = "#2c3e50",
    segment.color = "grey50",
    family = body_font,
    seed = 9075,
  ) +
  ggrepel::geom_label_repel(
    data = map_labels |> filter(place == "Basel"),
    aes(x = x, y = y, label = place),
    size = 3.5,
    box.padding = 0,
    label.size = 0.15,
    nudge_x = c(-1),
    nudge_y = c(0.15),
    fill = alpha("#e74c3c", 0.1),
    colour = "#c0392b",
    fontface = "bold",
    family = title_font,
    seed = 9075,
  ) +
  # Customisations
  # Title and subtitle
  labs(
    title = "Switzerland Journey",
    subtitle = "A week of travel through Swiss cities and neighboring regions",
    color = "Day of Travel",
    caption = caption_text
  ) +
  ggokabeito::scale_color_okabe_ito() +
  # Map limits
  coord_sf(
    xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
    ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
    expand = FALSE
  ) +
  theme_map() +
  ggview::canvas(width = 5, height = 7) -> fig

# Exporting --------------------------------------------------------------------
ggview::save_ggplot(
  plot = fig,
  file = here::here("2026/2026-01-29-switzerland/switzerland.png")
)
