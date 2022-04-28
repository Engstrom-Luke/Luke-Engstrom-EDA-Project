# Luke-Engstrom-EDA-Project

---
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## **Using iNaturalist to Identify Breeding Locations of Manta Ray**

Authors: Luke Engstrom

Co-Author: Dr. Christopher Merkord

Affiliations: Minnesota State University of Moorhead

## Abstract

-   Manta ray migrate where the food is, which makes them hard to study​

-   In addition, they are at risk from fisheries and bycatch (Stewart et al 2016)​

-   Using iNaturalist data to see if there are migration patterns​

-   Then identifying breeding grounds of two different species of manta ray​

-   With this information we propose to make these sites protected grounds

## Introduction

-   Manta ray are an elusive species, juveniles have never been studied in the wild (Stewart et al 2018)​

-   Mobula birostris (oceanic ray) and Mobula alfredi (reef ray) are both migratory species​

-   Reef Rays typically have their mating season from October to January (Simpkins 2013)​

-   Oceanic Rays are considered year-round, but it depends on what waters they are in (Manta Ray Reproduction 2018)​

-   We are working to identify breeding grounds to propose marine protected areas

## Methods

-   Using iNaturalist data, a citizen science program used to catalog sightings of organisms​

-   Then cleaned up the data using rNat and tidyverse​

-   We filtered the data to include species name, latitude, longitude, year, month, and day​

-   Then made a map with the filtered data​

## Results

-   Oceanic rays may have breeding grounds near the west coast of the US, Gulf of Mexico, New Zealand, Australia, and Indonesia​

-   Reef rays may have breeding grounds near India in the Maldives and Indonesia​

-   Reef rays have higher sightings in winter near India in the Maldives and Indonesia​

Figure 1.

```{r Oceanic, echo=FALSE, message=FALSE, warning=FALSE}
library(rinat)
library(tidyverse)
indx <- setNames( rep(c('1:Winter', '2:Spring', '3:Summer',
                        '4:Fall'),each=3), c("12","01","02","03","04","05","06","07","08","09","10","11"))
indx_breeding <- setNames( rep(c('Breeding Season', 'Non-Breeding Season','Non-Breeding Season'),each=4),
                  c("10","11","12","01","02","03","04","05","06","07","08","09"))
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

```

Figure 2.

```{r Density of Oceanic Rays, echo=FALSE, message=FALSE, warning=FALSE}
library(rinat)
library(tidyverse)
indx <- setNames( rep(c('1:Winter', '2:Spring', '3:Summer',
                        '4:Fall'),each=3), c("12","01","02","03","04","05","06","07","08","09","10","11"))
indx_breeding <- setNames( rep(c('Breeding Season', 'Non-Breeding Season','Non-Breeding Season'),each=4),
                  c("10","11","12","01","02","03","04","05","06","07","08","09"))
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

```

Figure 3.

```{r Reef four seasons, echo=FALSE, message=FALSE, warning=FALSE}
library(rinat)
library(tidyverse)
indx <- setNames( rep(c('1:Winter', '2:Spring', '3:Summer',
                        '4:Fall'),each=3), c("12","01","02","03","04","05","06","07","08","09","10","11"))
indx_breeding <- setNames( rep(c('Breeding Season', 'Non-Breeding Season','Non-Breeding Season'),each=4),
                  c("10","11","12","01","02","03","04","05","06","07","08","09"))
Reef_Ray <- get_inat_obs(query = "Reef Manta Ray",maxresults = 1000) %>% 
  select(scientific_name, time_observed_at, latitude, longitude) %>% 
  filter(scientific_name == "Mobula alfredi",time_observed_at != "") %>% 
  mutate(
    year = substr(time_observed_at,0,4),
    month = substr(time_observed_at,6,7),
    day = substr(time_observed_at,9,10),
    season = (indx[as.character(month)]),
    breeding = (indx_breeding[as.character(month)]))
world <- map_data("world")
world_australia <- map_data("world") %>% 
  filter(long >= 50,
         lat <= 25,
         lat >= -50)
Reef_Ray_australia <- Reef_Ray %>% 
  filter(longitude >= 50,
         latitude <= 25,
         latitude >= -50)
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
```

Figure 3. Map depicting Reef Ray sightings during breeding and non breeding seasons.

```{r Reef breeding, echo=FALSE, message=FALSE, warning=FALSE}
library(rinat)
library(tidyverse)
indx <- setNames( rep(c('1:Winter', '2:Spring', '3:Summer',
                        '4:Fall'),each=3), c("12","01","02","03","04","05","06","07","08","09","10","11"))
indx_breeding <- setNames( rep(c('Breeding Season', 'Non-Breeding Season','Non-Breeding Season'),each=4),
                  c("10","11","12","01","02","03","04","05","06","07","08","09"))
Reef_Ray <- get_inat_obs(query = "Reef Manta Ray",maxresults = 1000) %>% 
  select(scientific_name, time_observed_at, latitude, longitude) %>% 
  filter(scientific_name == "Mobula alfredi",time_observed_at != "") %>% 
  mutate(
    year = substr(time_observed_at,0,4),
    month = substr(time_observed_at,6,7),
    day = substr(time_observed_at,9,10),
    season = (indx[as.character(month)]),
    breeding = (indx_breeding[as.character(month)]))
world <- map_data("world")
world_australia <- map_data("world") %>% 
  filter(long >= 50,
         lat <= 25,
         lat >= -50)
Reef_Ray_australia <- Reef_Ray %>% 
  filter(longitude >= 50,
         latitude <= 25,
         latitude >= -50)
ggplot() +
  geom_map(
    data = world_australia, map = world_australia,
    aes(long, lat, map_id = region)
  ) +
  geom_point(data = Reef_Ray_australia,
             mapping = aes(x = longitude, y = latitude,
                           color = season,
                           size = ".1",
                           alpha = ".1"))+
  coord_equal()+
  facet_wrap(~breeding,ncol = 2)
```

Figure 4.

## Discussion

-   Protected marine grounds could be set up in New Zealand, Australia, Indonesia, India, the west coast of the US, and the Gulf of Mexico​
-   The Gulf of Mexico has become a hot spot for the Oceanic rays (Stewart et al 2018)​
-   Our data support this because of the high density in the Gulf of Mexico​

## Refferences

Manta Ray Reproduction - Manta Ray Facts and Information. 2018. Mantaray-worldcom. <https://www.mantaray-world.com/manta-ray-reproduction/>.

Simpkins K. Manta alfredi (Alfred manta). Animal Diversity Web. <https://animaldiversity.org/accounts/Manta_alfredi/#>:\~:text=Females%20mate%20once%20every%201.

Stewart JD, Beale CS, Fernando D, Sianipar AB, Burton RS, Semmens BX, Aburto-Oropeza O. 2016. Spatial ecology and conservation of Manta birostris in the Indo-Pacific. Biological Conservation. 200:178–183. <doi:10.1016/j.biocon.2016.05.016>.

Stewart JD, Nuttall M, Hickerson EL, Johnston MA. 2018. Important juvenile manta ray habitat at Flower Garden Banks National Marine Sanctuary in the northwestern Gulf of Mexico. Marine Biology. 165(7). <doi:10.1007/s00227-018-3364-5>.

## Acknowledgments

We thank Dr. Chris Merkord for help with data access and analysis.

## Other Images
