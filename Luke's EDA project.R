
library(rinat)
library(tidyverse)

indx <- setNames( rep(c('1:Winter', '2:Spring', '3:Summer',
                        '4:Fall'),each=3), c("12","01","02","03","04","05","06","07","08","09","10","11"))


indx_breeding <- setNames( rep(c('Breeding Season', 'Non-Breeding Season','Non-Breeding Season'),each=4),
                           c("10","11","12","01","02","03","04","05","06","07","08","09"))

Oceanic_Ray <- get_inat_obs(query = "Oceanic Manta Ray",maxresults = 1000) %>% 
  select(scientific_name, time_observed_at, latitude, longitude) %>% 
  filter(scientific_name == "Mobula birostris",time_observed_at != "") %>% 
  mutate(
    year = substr(time_observed_at,0,4),
    month = substr(time_observed_at,6,7),
    day = substr(time_observed_at,9,10), 
    season = (indx[as.character(month)]))%>% 
  print()

Reef_Ray <- get_inat_obs(query = "Reef Manta Ray",maxresults = 1000) %>% 
  select(scientific_name, time_observed_at, latitude, longitude) %>% 
  filter(scientific_name == "Mobula alfredi",time_observed_at != "") %>% 
  mutate(
    year = substr(time_observed_at,0,4),
    month = substr(time_observed_at,6,7),
    day = substr(time_observed_at,9,10),
    season = (indx[as.character(month)]),
    breeding = (indx_breeding[as.character(month)]))%>% 
  print()

world <- map_data("world")

world_australia <- map_data("world") %>% 
  filter(long >= 50,
         lat <= 25,
         lat >= -50)

# Reef Australia Figure
Reef_Ray_australia <- Reef_Ray %>% 
  filter(longitude >= 50,
         latitude <= 25,
         latitude >= -50) %>% 
  print()

ggplot() +
  geom_map(
    data = world_australia, map = world_australia,
    aes(long, lat, map_id = region)
  ) +
  geom_point(
    data = Reef_Ray_australia,
    mapping = aes(
      x = longitude, 
      y = latitude,
      shape = season,
      size = ".1",
      alpha = ".1")
    ) +
  coord_equal()+
  facet_wrap(~season,ncol = 2) +
  guides(fill = "none")+
  labs(
    title = "Reef Ray Australia Sightings"
  )

ggsave("Reef_Ray_Australia.png",
  height = 6,
  width = 8,
  units = "in",
  dpi = 400)

# Oceanic Ray World Map
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) +
  geom_point(
    data = Oceanic_Ray,
    mapping = aes(
      x = longitude, 
      y = latitude,
      shape = season,
      size = ".1",
      alpha = ".1")
    ) +
  coord_equal()+
  facet_wrap(~season,ncol = 2) +
  guides(fill = "none")+
  labs(
    title = "Oceanic Ray Global Sightings"
  )

ggsave("Oceanic_Ray.png",
  height = 6,
  width = 8,
  units = "in",
  dpi = 400)

# Breeding Seasons for Reef Rays 
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) +
  geom_point(
    data = Reef_Ray,
    mapping = aes(
      x = longitude, 
      y = latitude,
      shape = season,
      size = ".1",
      alpha = ".1")
    ) +
  coord_equal()+
  facet_wrap(~breeding,ncol = 2) +
  guides(fill = "none")+
  labs(
    title = "Reef Ray Breeding Seasons"
  )

ggsave("Reef_Ray_Breeding.png",
  height = 6,
  width = 8,
  units = "in",
  dpi = 400)

# Reef Ray World Map
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) +
  geom_point(
    data = Reef_Ray,
    mapping = aes(
      x = longitude, 
      y = latitude,
      shape = season,
      size = ".1",
      alpha = ".1")
    ) +
  coord_equal()+
  facet_wrap(~season,ncol = 2) +
  guides(fill = "none")+
  labs(
    title = "Reef Ray Global Sightings"
  )

ggsave("Reef_Ray_Global.png",
  height = 6,
  width = 8,
  units = "in",
  dpi = 400)

# Density Plot for Reef Rays Global sightings
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) +
  geom_density_2d_filled(
    data = Reef_Ray,
    mapping = aes(
      x = longitude, 
      y = latitude,
      # color = season
    ),
    alpha = .5
  ) +
  coord_equal()+
  facet_wrap(~season,ncol = 2) +
  guides(fill = "none") +
  labs(
    title = "Reef Ray Global Sightings",
    caption="Lighter colors indicate denser sightings"
  )

ggsave("Density_Reef_Ray_Global.png",
  height = 6,
  width = 8,
  units = "in",
  dpi = 400)

# Density Plot for Oceanic Rays Global Sightings
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) +
  geom_density_2d_filled(
    data = Oceanic_Ray,
    mapping = aes(
      x = longitude, 
      y = latitude,
      # color = season
    ),
    alpha = .5
  ) +
  coord_equal()+
  facet_wrap(~season,ncol = 2) +
  guides(fill = "none") +
  labs(
    title = "Oceanic Ray Global Sightings",
    caption="Lighter colors indicate denser sightings"
  )

ggsave("Density_Oceanic_Ray_Global.png",
  height = 6,
  width = 8,
  units = "in",
  dpi = 400)

# Density Plot for Reef Rays Australia
ggplot() +
  geom_map(
    data = world_australia, map = world_australia,
    aes(long, lat, map_id = region)
  ) +
  geom_density_2d_filled(
    data = Reef_Ray_australia,
    mapping = aes(
      x = longitude, 
      y = latitude,
      # color = season
    ),
    alpha = .5
  ) +
  coord_equal()+
  facet_wrap(~season,ncol = 2) +
  guides(fill = "none") +
  labs(
    title = "Reef Rays Australia Sightings",
    caption="Lighter colors indicate denser sightings"
  )

ggsave("Density_Reef_Ray_Australia.png",
  height = 6,
  width = 8,
  units = "in",
  dpi = 400)

# Density Breeding Seasons for Reef Rays
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) +
  geom_density_2d_filled(
    data = Reef_Ray,
    mapping = aes(
      x = longitude, 
      y = latitude,
      # color = season
    ),
    alpha = .5
  ) +
  coord_equal()+
  facet_wrap(~breeding,ncol = 2) +
  guides(fill = "none") +
  labs(
    title = "Density of Reef Ray Breeding Season",
    caption="Lighter colors indicate denser sightings"
  )

ggsave("Density_Reef_Ray_Breeding.png",
  height = 6,
  width = 8,
  units = "in",
  dpi = 400)
