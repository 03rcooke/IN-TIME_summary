#### Figure production ####

library(dplyr)
library(ggplot2)
library(sf)

# snapshot package versions
# renv::snapshot()

ggplot2::theme_set(cowplot::theme_cowplot())

#### Load and tidy database ####

# load and tidy database
# HERE LOAD DATA

# biogeographic realms for Olson
realms <- sf::read_sf("data/realms.shp")

#### Data summary ####

# number of records
length(unique(dat_n$occurrence_id))
# 4970982

# number of samples
length(unique(dat_n$sample_id))
# 3336021

# number of sources
length(unique(dat_n$source_title))
# 146

# number of records per source
r_source <- dat_n %>%
  dplyr::count(source_title, name = "records") %>% 
  dplyr::arrange(-records)

# top 5
sum(r_source$records[1:5])

# all others
sum(r_source$records[6:nrow(r_source)])

# assemblage time series (unique combinations of source_title, location_latitude, location_longitude, and sample_technique)
ts <- dat_n %>% 
  # group by attributes that should be the same within an assemblage time series 
  # allowed to change between samples include sample_id, dates, occurrence_id, and sampling effort
  dplyr::group_by(source_title, location_latitude, location_longitude, sample_technique) %>% 
  dplyr::mutate(ts_id = dplyr::cur_group_id()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(ts_name = paste0(source_title, "_", ts_id))

# View(dplyr::select(ts, ts_id, ts_name, sample_id, occurrence_id, source_title, date_start, date_end, location_latitude, location_longitude, sample_latitude, sample_longitude, taxon_order, taxon_name_preferred))
# 
# View(dplyr::distinct(ts, ts_id, ts_name, .keep_all = TRUE) %>% dplyr::select(ts_id, ts_name, source_title, location_latitude, location_longitude, country, sample_realm))

# number of assemblage time series
length(unique(ts$ts_id))
# 12129

# number of time series x date combinations
dplyr::distinct(ts, ts_id, date_start) %>% 
  nrow(.)
# 348388

# number of time series per study
ts_source <- ts %>% 
  dplyr::distinct(ts_id, .keep_all = TRUE) %>% 
  dplyr::group_by(source_title) %>%
  dplyr::summarise(count = n())

# median time series per study
median(ts_source$count)
# minimum time series per study
min(ts_source$count)
# maximum time series per study
max(ts_source$count)

# span of time series
ts_yrs <- ts %>% 
  dplyr::mutate(date_start_year = as.numeric(format(date_start, "%Y")),
                date_end_year = as.numeric(format(date_end, "%Y"))) %>% 
  dplyr::group_by(ts_name) %>% 
  dplyr::summarise(min_year = min(date_start_year),
                   max_year = max(date_end_year)) %>% 
  dplyr::mutate(span = (max_year - min_year) + 1)

# median span of time series 
median(ts_yrs$span)
# minimum span of time series 
min(ts_yrs$span)
# maximum span of time series 
max(ts_yrs$span)

## number of sampling occasions
socc <- ts %>% 
  dplyr::group_by(ts_name) %>% 
  dplyr::summarise(samp_occ = dplyr::n_distinct(date_start), 
                   samp_occ_yrs = dplyr::n_distinct(as.numeric(format(date_start, "%Y")))) %>% 
  dplyr::left_join(ts_yrs, by = "ts_name")

# median number of sampling occasions per time series 
median(socc$samp_occ)
# minimum 
min(socc$samp_occ)
# maximum  
max(socc$samp_occ)

# median number of years sampled per time series 
median(socc$samp_occ_yrs)
# minimum 
min(socc$samp_occ_yrs)
# maximum  
max(socc$samp_occ_yrs)

# time series sampled at least once a year across their entire span
nrow(dplyr::filter(socc, samp_occ_yrs == span))

# population time series (unique combinations of source_title, location_latitude, location_longitude, and sample_technique)
ts_pop <- dat_n %>% 
  # group by attributes that should be the same within a population time series 
  dplyr::group_by(source_title, location_latitude, location_longitude, sample_technique, taxon_name_preferred) %>% 
  dplyr::mutate(ts_pop_id = dplyr::cur_group_id()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(ts_pop_name = paste0(source_title, "_", taxon_name_preferred, ts_pop_id))

# number of population time series
length(unique(ts_pop$ts_pop_id))
# 621518

ts_pop_spp <- ts_pop %>% 
  # remove subspecies text in names
  dplyr::mutate(taxon_name_preferred = stringr::str_replace(taxon_name_preferred, " subsp. \\w+", "")) %>% 
  # is the record resolved to species level (i.e., two words)?
  dplyr::mutate(word_count = stringr::str_count(taxon_name_preferred, "\\S+")) %>% 
  # only records resolved to species level
  dplyr::filter(word_count == 2)

# number of population time series resolved to species level
length(unique(ts_pop_spp$ts_pop_id))
# 305547

# number of orders
length(unique(dat_n$taxon_order))
# 24 (out of 43 total insect orders [length(insect_orders)])

# number of families
length(unique(dat_n$taxon_family))
# 565

# years
d_year <- dat_n %>% 
  dplyr::mutate(date_start_year = as.numeric(format(date_start, "%Y")),
                date_end_year = as.numeric(format(date_end, "%Y")))

# minimum year
min(d_year$date_start_year)
# 1750

# maximum year
max(d_year$date_end_year)
# 2023

# experimental treatment time series
# unique(dat_n$sample_treatment)
ts_treat <- ts %>%
  dplyr::filter(sample_treatment %in% c("TREATMENT_1", "TREATMENT_2", "TREATMENT_3", "TREATMENT_4", "TREATMENT_5", "TREATMENT_6", "TREATMENT_7", "TREATMENT_8", "TREATMENT_9", "TREATMENT_10", "TREATMENT_11")) %>%
  dplyr::summarise(dplyr::n_distinct(ts_id))

# number of experimental treatment time series
ts_treat[[1]]

# proportion of experimental treatment time series
(ts_treat[[1]] / length(unique(ts$ts_id))) * 100

# post-event time series
# unique(dat_n$sample_data_purpose)
ts_purpose <- ts %>%
  dplyr::filter(sample_data_purpose %in% c("POST_EVENT_POLLUTION", "POST_EVENT_INVASIVES", "POST_EVENT_WEATHER", "POST_RESTORATION", "POST_EVENT", "POST_REINTRODUCTION")) %>%
  dplyr::summarise(dplyr::n_distinct(ts_id))

# number of post-event time series
ts_purpose[[1]]

# proportion of post-event time series
(ts_purpose[[1]] / length(unique(ts$ts_id))) * 100

## species

# distinct species records
dis_spp_r <- ts %>% 
  # remove subspecies text in names
  dplyr::mutate(taxon_name_preferred = stringr::str_replace(taxon_name_preferred, " subsp. \\w+", "")) %>% 
  # is the record resolved to species level (i.e., two words)?
  dplyr::mutate(word_count = stringr::str_count(taxon_name_preferred, "\\S+")) %>% 
  # only records resolved to species level
  dplyr::filter(word_count == 2)

# number of species-level records
nrow(dis_spp_r)
# 2762064

# unique species
dis_spp <- dis_spp_r %>% 
  dplyr::distinct(taxon_name_preferred, .keep_all = TRUE)

# number of unique species recorded
nrow(dis_spp)
# 10975

## number of unique taxonomic units per time series
ut <- ts %>% 
  dplyr::group_by(ts_name) %>% 
  dplyr::summarise(unique_tax = dplyr::n_distinct(taxon_name_preferred))

# median number of unique taxonomic units of time series 
median(ut$unique_tax)
# minimum 
min(ut$unique_tax)
# maximum 
max(ut$unique_tax)

## number of unique species per time series
us <- dis_spp_r %>% 
  dplyr::group_by(ts_name) %>% 
  dplyr::summarise(unique_tax = dplyr::n_distinct(taxon_name_preferred))

# median number of unique taxonomic units of time series 
median(us$unique_tax)
# minimum = 0 (some time series have no species level data)
# maximum 
max(us$unique_tax)

#### Summary stats collated ####

sum_stats <- data.frame(name = c("records",
                                 "samples",
                                 "sources",
                                 "time series",
                                 "time series x date combinations",
                                 "orders",
                                 "families",
                                 "species-level records",
                                 "unique species",
                                 "min year",
                                 "max year",
                                 "minimum number of sites per study",
                                 "maximum number of sites per study",
                                 "minimum number of time series per study",
                                 "maximum number of time series per study",
                                 "median span of time series",
                                 "minimum span of time series",
                                 "maximum span of time series",
                                 "number of experimental treatment time series",
                                 "proportion of experimental treatment time series",
                                 "number of post-event time series",
                                 "proportion of post-event time series"), 
                        value = c(length(unique(dat_n$occurrence_id)),
                                  length(unique(dat_n$sample_id)),
                                  length(unique(dat_n$source_title)),
                                  length(unique(ts$ts_id)),
                                  nrow(dplyr::distinct(ts, ts_id, date_start)),
                                  length(unique(dat_n$taxon_order)),
                                  length(unique(dat_n$taxon_family)),
                                  nrow(dis_spp_r),
                                  nrow(dis_spp),
                                  min(d_year$date_start_year),
                                  max(d_year$date_end_year),
                                  min(sites_source$count),
                                  max(sites_source$count),
                                  min(ts_source$count),
                                  max(ts_source$count),
                                  median(ts_yrs$span),
                                  min(ts_yrs$span),
                                  max(ts_yrs$span),
                                  ts_treat[[1]],
                                  (ts_treat[[1]] / length(unique(ts$ts_id))) * 100,
                                  ts_purpose[[1]],
                                  (ts_purpose[[1]] / length(unique(ts$ts_id))) * 100), 
                        notes = c("unique occurrence_id; after excluding duplicates, bad plot ids, and records with zeros [I also removed a record that had an abundance of -2]",
                                  "unique sample_id",
                                  "unique source_id",
                                  "group by attributes that should be the same within a time series; allowed to change between samples include sample_id, dates, occurrence_id, and sampling effort",
                                  rep("", 18)))

write.csv(sum_stats, "outputs/sum_stats.csv", row.names = FALSE)

##### Realms #####

# number of records per realm
r_realm <- dat_n %>%
  dplyr::count(sample_realm, name = "records")

# number of studies per realm
stud_realm <- dat_n %>%
  dplyr::group_by(sample_realm) %>%
  dplyr::summarise(sources = dplyr::n_distinct(source_title))

# number of time series per realm
ts_realm <- ts %>%
  dplyr::group_by(sample_realm) %>%
  dplyr::summarise(ts = dplyr::n_distinct(ts_id))

# number of species-level records per realm
spr_realm <- dis_spp_r %>%
  dplyr::count(sample_realm, name = "spp_n")

# orders, families, species
ord_realm <- dat_n %>%
  dplyr::group_by(sample_realm) %>%
  dplyr::summarise(orders = dplyr::n_distinct(taxon_order))

fam_realm <- dat_n %>%
  dplyr::group_by(sample_realm) %>%
  dplyr::summarise(families = dplyr::n_distinct(taxon_family))

sp_realm <- dis_spp %>%
  dplyr::count(sample_realm, name = "spp")

realm_out <- dplyr::left_join(r_realm, stud_realm, by = "sample_realm") %>%
  dplyr::left_join(ts_realm, by = "sample_realm") %>%
  dplyr::left_join(spr_realm, by = "sample_realm") %>%
  dplyr::left_join(ord_realm, by = "sample_realm") %>%
  dplyr::left_join(fam_realm, by = "sample_realm") %>%
  dplyr::left_join(sp_realm, by = "sample_realm")

write.csv(realm_out, "outputs/realm_out.csv", row.names = FALSE)

##### Taxonomic #####

## Order

# number of records per order
r_order <- dat_n %>%
  dplyr::count(taxon_order, name = "records")

# number of studies per order
stud_order <- dat_n %>%
  dplyr::group_by(taxon_order) %>%
  dplyr::summarise(sources = dplyr::n_distinct(source_title))

# number of time series per order
ts_order <- ts %>%
  dplyr::group_by(taxon_order) %>%
  dplyr::summarise(ts = dplyr::n_distinct(ts_id))

# number of species-level records per order
spr_order <- dis_spp_r %>%
  dplyr::count(taxon_order, name = "spp_n")

# families, species
fam_order <- dat_n %>%
  dplyr::group_by(taxon_order) %>%
  dplyr::summarise(families = dplyr::n_distinct(taxon_family))

sp_order <- dis_spp %>%
  dplyr::count(taxon_order, name = "spp")

order_out <- dplyr::left_join(r_order, stud_order, by = "taxon_order") %>%
  dplyr::left_join(ts_order, by = "taxon_order") %>%
  dplyr::left_join(spr_order, by = "taxon_order") %>%
  dplyr::left_join(fam_order, by = "taxon_order") %>%
  dplyr::left_join(sp_order, by = "taxon_order")

write.csv(order_out, "outputs/order_out.csv", row.names = FALSE)

## Family

# number of records per family
r_family <- dat_n %>%
  dplyr::group_by(taxon_order) %>%
  dplyr::count(taxon_family, name = "records")

# number of studies per family
stud_family <- dat_n %>%
  dplyr::group_by(taxon_order, taxon_family) %>%
  dplyr::summarise(sources = dplyr::n_distinct(source_title))

# number of time series per family
ts_family <- ts %>%
  dplyr::group_by(taxon_order, taxon_family) %>%
  dplyr::summarise(ts = dplyr::n_distinct(ts_id))

# number of species-level records per family
spr_family <- dis_spp_r %>%
  dplyr::group_by(taxon_order) %>%
  dplyr::count(taxon_family, name = "spp_n")

# species
sp_family <- dis_spp %>%
  dplyr::group_by(taxon_order) %>%
  dplyr::count(taxon_family, name = "spp")

family_out <- dplyr::left_join(r_family, stud_family, by = dplyr::join_by(taxon_order, taxon_family)) %>%
  dplyr::left_join(ts_family, by = dplyr::join_by(taxon_order, taxon_family)) %>%
  dplyr::left_join(spr_family, by = dplyr::join_by(taxon_order, taxon_family)) %>%
  dplyr::left_join(sp_family, by = dplyr::join_by(taxon_order, taxon_family))

write.csv(family_out, "outputs/family_out.csv", row.names = FALSE)

##### Sample techniques #####

# number of records per technique
r_technique <- dat_n %>%
  dplyr::count(sample_technique, name = "records")

# number of studies per technique
stud_technique <- dat_n %>%
  dplyr::group_by(sample_technique) %>%
  dplyr::summarise(sources = dplyr::n_distinct(source_title))

# number of time series per technique
ts_technique <- ts %>%
  dplyr::group_by(sample_technique) %>%
  dplyr::summarise(ts = dplyr::n_distinct(ts_id))

# number of species-level records per technique
spr_technique <- dis_spp_r %>%
  dplyr::count(sample_technique, name = "spp_n")

# orders, families, species
ord_technique <- dat_n %>%
  dplyr::group_by(sample_technique) %>%
  dplyr::summarise(orders = dplyr::n_distinct(taxon_order))

fam_technique <- dat_n %>%
  dplyr::group_by(sample_technique) %>%
  dplyr::summarise(families = dplyr::n_distinct(taxon_family))

sp_technique <- dis_spp %>%
  dplyr::count(sample_technique, name = "spp")

technique_out <- dplyr::left_join(r_technique, stud_technique, by = "sample_technique") %>%
  dplyr::left_join(ts_technique, by = "sample_technique") %>%
  dplyr::left_join(spr_technique, by = "sample_technique") %>%
  dplyr::left_join(ord_technique, by = "sample_technique") %>%
  dplyr::left_join(fam_technique, by = "sample_technique") %>%
  dplyr::left_join(sp_technique, by = "sample_technique")

write.csv(technique_out, "outputs/technique_out.csv", row.names = FALSE)

##### Biogeographic realms #####

# convert records to points
points <- sf::st_as_sf(dat_n, coords = c("location_longitude", "location_latitude"), crs = 4326)

# simplify geometries
realms_simplified <- sf::st_simplify(realms, dTolerance = 0.01)

# join points to realms
points_realms <- sf::st_join(points, realms_simplified, left = TRUE)

df_points_realms <- as.data.frame(points_realms)

dat_bio <- df_points_realms %>%
  dplyr::mutate(REALM = dplyr::case_match(REALM,
                                          "NA" ~ "Nearctic",
                                          "PA" ~ "Palearctic",
                                          "NT" ~ "Neotropic",
                                          "IM" ~ "Indo-Malay",
                                          "AT" ~ "Afrotropic",
                                          "OC" ~ "Oceania",
                                          "AN" ~ "Antarctic",
                                          "AA" ~ "Australasia")) %>%
  # join time series info
  dplyr::left_join(dplyr::select(ts, sample_id, occurrence_id, ts_id), ., by = dplyr::join_by(sample_id, occurrence_id))

# NA realms
ms <- dplyr::distinct(dat_bio, ts_id, .keep_all = TRUE) %>%
  dplyr::filter(is.na(REALM)) %>%
  dplyr::select(sample_id, occurrence_id, ts_id, source_title, country, sample_location_name, sample_latitude, sample_longitude, sample_grid_ref, REALM, geometry)

# ms_ms <- dplyr::filter(ms, !ts_id %in% c(5, 169, 210, 275, 2159, 3205, 4266, 4271, 4273, 4331, 4374, 4375, 4381, 4388, 4437, 4440, 4444, 4445, 4446, 4520, 4521, 4535, 2220, 2222, 2225, 2228, 2230, 3357, 3368, 3903, 4806, 4807, 4818))

# sample_ids for ts_ids
ms_pal <- dplyr::filter(dat_bio, ts_id %in% c(878, 861, 5218, 10731, 10668, 10682, 10734, 10565, 10675, 10669, 10815, 10560, 10625, 10739, 10567, 10738, 11898, 10981, 11328, 11324, 11868, 11411, 10740, 5537, 7421, 7992, 7983, 8016, 96, 8409, 7984, 7994, 7982, 11728, 11861, 11897, 11848, 8586, 10983, 22, 11030, 11890, 11845, 9578, 10814, 10829, 11863, 11862, 11829, 10982, 11334, 11844, 11852, 11833, 5252, 259, 7981, 8385, 8336, 5270, 4674, 11894, 8828
                                              , 33, 1062, 5001, 8846))
ms_nea <- dplyr::filter(dat_bio, ts_id %in% c(12051, 12063, 12052, 10186, 9723, 9712, 10175, 10173, 10170, 10167, 10165))

# fix missing realms
dat_bio <- dat_bio %>%
  dplyr::mutate(REALM = dplyr::case_when(occurrence_id %in% ms_pal$occurrence_id ~ "Palearctic",
                                         occurrence_id %in% ms_nea$occurrence_id ~ "Nearctic",
                                         TRUE ~ REALM))

# number of records per biogeographic realm
r_bio <- dat_bio %>%
  dplyr::count(REALM, name = "records")

# number of studies per biogeographic realm
stud_bio <- dat_bio %>%
  dplyr::group_by(REALM) %>%
  dplyr::summarise(sources = dplyr::n_distinct(source_title))

# number of time series per biogeographic realm
ts_bio <- dat_bio %>%
  dplyr::group_by(REALM) %>%
  dplyr::summarise(ts = dplyr::n_distinct(ts_id))

# number of species-level records per biogeographic realm
spr_bio <- dplyr::left_join(dplyr::select(dis_spp_r, sample_id, occurrence_id), dat_bio, by = dplyr::join_by(sample_id, occurrence_id)) %>%
  dplyr::count(REALM, name = "spp_n")

# orders, families, species
ord_bio <- dat_bio %>%
  dplyr::group_by(REALM) %>%
  dplyr::summarise(orders = dplyr::n_distinct(taxon_order))

fam_bio <- dat_bio %>%
  dplyr::group_by(REALM) %>%
  dplyr::summarise(families = dplyr::n_distinct(taxon_family))

sp_bio <- dplyr::left_join(dplyr::select(dis_spp, sample_id, occurrence_id, taxon_name_entered, word_count), dat_bio, by = dplyr::join_by(sample_id, occurrence_id)) %>%
  dplyr::count(REALM, name = "spp")

bio_out <- dplyr::left_join(r_bio, stud_bio, by = "REALM") %>%
  dplyr::left_join(ts_bio, by = "REALM") %>%
  dplyr::left_join(spr_bio, by = "REALM") %>%
  dplyr::left_join(ord_bio, by = "REALM") %>%
  dplyr::left_join(fam_bio, by = "REALM") %>%
  dplyr::left_join(sp_bio, by = "REALM")

write.csv(bio_out, "outputs/bio_out.csv", row.names = FALSE)

# percentages
perc_bio_out <- dplyr::mutate(bio_out, records = (records / dplyr::filter(sum_stats, name == "records")$value[1]) * 100) %>%
  dplyr::mutate(sources = (sources / dplyr::filter(sum_stats, name == "sources")$value[1]) * 100) %>%
  dplyr::mutate(ts = (ts / dplyr::filter(sum_stats, name == "time series")$value[1]) * 100) %>%
  dplyr::mutate(spp_n = (spp_n / dplyr::filter(sum_stats, name == "species-level records")$value[1]) * 100) %>%
  dplyr::mutate(orders = (orders / dplyr::filter(sum_stats, name == "orders")$value[1]) * 100) %>%
  dplyr::mutate(families = (families / dplyr::filter(sum_stats, name == "families")$value[1]) * 100) %>%
  dplyr::mutate(spp = (spp / dplyr::filter(sum_stats, name == "unique species")$value[1]) * 100)

#### Fig 1 - spatial plot ####

# basemap
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

## spatial realm

# records spatial
r_sf_realm <- dat_n %>%
  dplyr::count(source_title, location_latitude, location_longitude, sample_technique, sample_realm) %>% 
  sf::st_as_sf(coords = c("location_longitude", "location_latitude"), crs = 4326) %>% 
  # neaten names
  dplyr::mutate(sample_realm = dplyr::case_match(sample_realm,
                                                 "TERRESTRIAL" ~ "Terrestrial",
                                                 "FRESHWATER" ~ "Freshwater", 
                                                 "TERR_FRESH" ~ "Terrestrial-freshwater")) %>% 
  # as factor
  dplyr::mutate(sample_realm = factor(sample_realm, levels = c("Terrestrial", "Freshwater", "Terrestrial-freshwater")))

p_sf_realm <- ggplot(world) +
  # colour = NA removes country borders
  geom_sf(fill = "grey80", colour = NA) +
  geom_sf(data = r_sf_realm, aes(size = n, colour = sample_realm), alpha = 0.7) +
  coord_sf(crs = "+proj=moll", xlim = c(-17596912, 17839433)) +
  scale_colour_manual(values = c("#66c2a5", "#8da0cb", "#fc8d62")) +
  guides(size = "none") +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        plot.margin = margin(-10, -10, -10, -10, "pt"))

## bars

# number of records per realm
r_realm <- dat_n %>% 
  dplyr::count(sample_realm) %>% 
  # percentages
  dplyr::mutate(frac = n / sum(n)) %>% 
  dplyr::mutate(ymax = cumsum(frac)) %>% 
  dplyr::mutate(ymin = c(0, head(.$ymax, n = -1))) %>% 
  # neaten names
  dplyr::mutate(sample_realm = dplyr::case_match(sample_realm,
                                                 "TERRESTRIAL" ~ "Terrestrial",
                                                 "FRESHWATER" ~ "Freshwater", 
                                                 "TERR_FRESH" ~ "Terrestrial/freshwater")) %>% 
  # as factor
  dplyr::mutate(sample_realm = factor(sample_realm, levels = c("Terrestrial", "Freshwater", "Terrestrial/freshwater"))) %>% 
  dplyr::mutate(label_pos = (ymax + ymin) / 2)

# bar records per realm
p_r_realm <- ggplot(r_realm, aes(x = sample_realm, y = n, fill = sample_realm)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("#66c2a5", "#8da0cb", "#fc8d62")) +
  scale_y_continuous(limits = c(NA, max(r_realm$n) * 1.1)) +
  labs(title = "Records") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# number of time series per realm
ts_realm <- dat_n %>% 
  # unique time series
  dplyr::distinct(source_title, location_latitude, location_longitude, sample_technique, .keep_all = TRUE) %>% 
  dplyr::count(sample_realm) %>% 
  # percentages
  dplyr::mutate(frac = n / sum(n)) %>% 
  dplyr::mutate(ymax = cumsum(frac)) %>% 
  dplyr::mutate(ymin = c(0, head(.$ymax, n = -1))) %>% 
  # neaten names
  dplyr::mutate(sample_realm = dplyr::case_match(sample_realm,
                                                 "TERRESTRIAL" ~ "Terrestrial",
                                                 "FRESHWATER" ~ "Freshwater", 
                                                 "TERR_FRESH" ~ "Terrestrial/freshwater")) %>% 
  # as factor
  dplyr::mutate(sample_realm = factor(sample_realm, levels = c("Terrestrial", "Freshwater", "Terrestrial/freshwater"))) %>% 
  dplyr::mutate(label_pos = (ymax + ymin) / 2)

# bar time series per realm 
p_ts_realm <- ggplot(ts_realm, aes(x = sample_realm, y = n, fill = sample_realm)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("#66c2a5", "#8da0cb", "#fc8d62")) +
  scale_y_continuous(limits = c(NA, max(ts_realm$n) * 1.1)) +
  labs(title = "Time series") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# combined figure
p_comb_spatial <- cowplot::plot_grid(
  p_sf_realm, 
  cowplot::plot_grid(p_r_realm,
                     p_ts_realm, nrow = 2, labels = c("b", "c")), ncol = 2, rel_widths = c(1, 0.3), labels = c("a", "")) +
  theme(plot.background = ggplot2::element_rect(fill = "white", colour = "white"))

cowplot::save_plot("outputs/spatial_comb.png", p_comb_spatial, base_width = 9, base_height = 4)

#### Fig 2 - cumulative records, time series span and freq, techniques ####

## records

# records per year
r_year <- dat_n %>%
  dplyr::mutate(date_start_year = as.numeric(format(date_start, "%Y"))) %>% 
  # number of records per year
  dplyr::count(date_start_year)

# cumulative records
r_year$cumsum <- cumsum(r_year$n)

# proportion post-1970
((max(r_year$cumsum) - r_year[r_year$date_start_year == 1969, ]$cumsum) / max(r_year$cumsum)) * 100

# plot cumulative records per year
p_r_cumsum_both <- ggplot2::ggplot(r_year, ggplot2::aes(x = date_start_year, y = cumsum)) +
  ggplot2::geom_line() +
  #ggplot2::geom_point(ggplot2::aes(size = n), colour = "grey", alpha = 0.5) +
  ggplot2::scale_x_continuous(name = "Year") +
  ggplot2::scale_y_continuous(name = "Records in database", breaks = c(0, 1000000, 2000000, 3000000, 4000000, 5000000), labels = c("0", "1,000,000", "2,000,000", "3,000,000", "4,000,000", "5,000,000")) +
  ggplot2::theme(legend.position = "none")

# plot cumulative records per year 1950 onwards
p_r_cumsum_1950 <- p_r_cumsum_both +
  ggplot2::scale_x_continuous(name = "Year", limits = c(1950, NA)) +
  ggplot2::geom_point(ggplot2::aes(size = n), colour = "grey", alpha = 0.5)

p_r_cumsum <- p_r_cumsum_both +
  ggplot2::scale_x_continuous(name = "", breaks = c(1750, 1850, 1950)) +
  ggplot2::scale_y_continuous(name = "", breaks = c(0, 1000000, 2000000, 3000000, 4000000, 5000000), labels = c("", "", "", "", "", "")) +
  ggplot2::geom_point(ggplot2::aes(size = n), colour = "grey", alpha = 0.2)

# combine
p_comb_cumsum <- cowplot::ggdraw(p_r_cumsum_1950) +
  cowplot::draw_plot(p_r_cumsum, x = 0.25, y = 0.45, width = 0.4, height = 0.5)

## span of time series

ts_yrs_cat <- ts_yrs %>%
  dplyr::mutate(span_cat = cut(span, breaks = c(-Inf, 10, 20, 30, 50, Inf), labels = labels <- c("<10", "10-19", "20-29", "30-49", "50+"), right = FALSE))

p_span <- ggplot(data = ts_yrs_cat, aes(x = span_cat)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count), y = after_stat(count)), stat = "count", vjust = -0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  #scale_fill_viridis_d(option = "B") +
  labs(x = "Span of time series (years)", y = "Number of time series") +
  theme(legend.position = "none")

## most sampled years

samp_yrs <- ts %>% 
  dplyr::mutate(date_start_year = as.numeric(format(date_start, "%Y"))) %>% 
  dplyr::group_by(date_start_year) %>% 
  dplyr::summarise(count = dplyr::n_distinct(ts_name))

p_samp_yrs <- ggplot(samp_yrs, aes(x = date_start_year, y = count)) +
  geom_bar(stat = "identity") +
  ggplot2::scale_x_continuous(name = "Year", limits = c(1950, NA), breaks = seq(1950, 2020, by = 5)) +
  labs(x = "Year", y = "Number of time series sampled")

## stacked bar chart sample technique

ts_samp_tech <- ts %>% 
  dplyr::distinct(ts_id, .keep_all = TRUE) %>%
  dplyr::count(sample_technique, sample_realm) %>% 
  dplyr::filter(sample_technique != "UNKNOWN") %>% 
  tidyr::complete(sample_technique, sample_realm, fill = list(n = NA)) %>%
  dplyr::mutate(sample_technique = stringr::str_to_sentence(stringr::str_replace_all(sample_technique, "_", " "))) %>% 
  # neaten names
  dplyr::mutate(sample_realm = dplyr::case_match(sample_realm,
                                                 "TERRESTRIAL" ~ "Terrestrial", "FRESHWATER" ~ "Freshwater", "TERR_FRESH" ~ "Terrestrial/freshwater")) %>% 
  # as factor
  dplyr::mutate(sample_realm = factor(sample_realm, levels = c("Terrestrial", "Freshwater", "Terrestrial/freshwater")))

ts_samp_tech_sum <- ts_samp_tech %>% 
  dplyr::group_by(sample_technique) %>% 
  dplyr::summarise(sum_n = sum(n, na.rm = TRUE)) %>% 
  dplyr::arrange(sum_n)

ts_samp_tech <- ts_samp_tech %>% 
  dplyr::mutate(sample_technique = factor(sample_technique, levels = ts_samp_tech_sum$sample_technique))

# stacked bar plot
p_stacked_bar <- ggplot(ts_samp_tech, aes(x = sample_technique, y = n)) +
  geom_bar(stat = "identity", aes(fill = sample_realm)) +
  geom_text(data = ts_samp_tech_sum, aes(x = sample_technique, y = sum_n, label = sum_n), position = position_stack(vjust = 1), hjust = -0.1) +
  scale_fill_manual(name = "Realm", values = c("#66c2a5", "#8da0cb", "#fc8d62")) +
  scale_y_continuous(limits = c(0, max(ts_samp_tech_sum$sum_n) * 1.02)) +
  coord_flip() +
  labs(y = "Number of time series") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.6, 0.3),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.5), "cm"))

## combine

p_comb_time <- cowplot::plot_grid(
  cowplot::plot_grid(p_comb_cumsum, p_span, ncol = 2, labels = "auto"), 
  p_samp_yrs,
  p_stacked_bar, 
  nrow = 3, rel_heights = c(1, 1, 1.4), labels = c("", "c", "d"))  +
  theme(plot.background = ggplot2::element_rect(fill = "white", colour = "white"))

cowplot::save_plot("outputs/comb_time.png", p_comb_time, base_width = 9, base_height = 14)

#### Fig 3 - treemap families ####

# treemap for insect orders with families
tree_fam_func <- function(dat, col_pal, level) {
  
  df <- dat %>% 
    dplyr::group_by(taxon_order) %>%
    # distinct time series
    {if (level == "ts") dplyr::distinct(., ts_id, taxon_family, .keep_all = TRUE) else .} %>% 
    dplyr::count(taxon_family) %>% 
    tidyr::drop_na()
  
  p <- ggplot2::ggplot(df, ggplot2::aes(area = n, fill = taxon_order, subgroup = taxon_order, group = taxon_family)) +
    # families
    treemapify::geom_treemap(colour = "grey40") +
    # order
    treemapify::geom_treemap_subgroup_border(colour = "grey70") +
    # family text
    treemapify::geom_treemap_text(aes(label = taxon_family), colour = "black", place = "centre", min.size = 10) +
    ggplot2::scale_fill_manual(values = col_pal) +
    ggplot2::theme(legend.position = "none")
  
}

# records per order
r_ord <- dat_n %>% 
  dplyr::count(taxon_order) 

# records per family
r_fam <- dat_n %>% 
  dplyr::count(taxon_family) 

# time series per order
ts_ord <- ts %>% 
  dplyr::distinct(ts_id, taxon_order) %>%
  dplyr::count(taxon_order)

# time series per family
ts_fam <- ts %>% 
  dplyr::distinct(ts_id, taxon_family) %>%
  dplyr::count(taxon_family)

# specify colours for orders
col_pal <- c("Archaeognatha" = "grey90", "Blattodea" = "grey90", "Coleoptera" = "#87CEEB", "Dermaptera" = "grey90", "Diptera" = "#FFDAB9", "Embioptera" = "grey90", "Ephemeroptera" = "#EEE8AA", "Hemiptera" = "#F08080", "Hymenoptera" = "grey90", "Lepidoptera" = "#D8BFD8", "Mantodea" = "grey90", "Mecoptera" = "grey90", "Megaloptera" = "grey90", "Neuroptera" = "grey90", "Odonata" = "grey90", "Orthoptera" = "grey90", "Phasmida" = "grey90",      "Plecoptera" = "#7FFFD4", "Psocodea" = "grey90", "Raphidioptera" = "grey90", "Siphonaptera" = "grey90", "Thysanoptera" = "grey90", "Trichoptera" = "#B0E0E6", "Zygentoma" = "grey90")

# treemap of records (observations) per insect family
tree_fam <- tree_fam_func(dat = dat_n, col_pal = col_pal, level = "records")

tree_fam

# legend
fake_data <- data.frame(taxon_order = factor(c("Lepidoptera", "Trichoptera", "Diptera", "Coleoptera", "Ephemeroptera", "Plecoptera", "Hemiptera", "Other"), levels = c("Lepidoptera", "Trichoptera", "Diptera", "Coleoptera", "Ephemeroptera", "Plecoptera", "Hemiptera", "Other")), value = 1)

# fake plot
p_fake <- ggplot(fake_data, aes(x = taxon_order, y = value, fill = taxon_order)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#D8BFD8", "#B0E0E6", "#FFDAB9", "#87CEEB", "#EEE8AA", "#7FFFD4", "#F08080", "grey90")) +
  labs(fill = "")

# get the legend
leg <- cowplot::get_legend(p_fake)

tree_fake_comb <- cowplot::plot_grid(tree_fam, leg, ncol = 2, rel_widths = c(1, 0.2))

## bar time series

ts_ord_tot <- ts_ord %>% 
  dplyr::arrange(n)

ts_ord_realm <- ts %>% 
  dplyr::group_by(sample_realm) %>% 
  dplyr::distinct(ts_id, taxon_order) %>%
  dplyr::count(taxon_order) %>% 
  # neaten names
  dplyr::mutate(sample_realm = dplyr::case_match(sample_realm,
                                                 "TERRESTRIAL" ~ "Terrestrial", "FRESHWATER" ~ "Freshwater", "TERR_FRESH" ~ "Terrestrial/freshwater")) %>% 
  # as factor
  dplyr::mutate(sample_realm = factor(sample_realm, levels = c("Terrestrial", "Freshwater", "Terrestrial/freshwater")))

ts_ord_bar <- ts_ord_realm %>% 
  dplyr::mutate(taxon_order = dplyr::case_when(taxon_order %in% c("Blattodea", "Thysanoptera", "Zygentoma", "Dermaptera", "Psocodea", "Embioptera", "Mantodea", "Archaeognatha", "Siphonaptera", "Mecoptera", "Phasmida", "Raphidioptera") ~ "Other", .default = taxon_order)) %>%
  dplyr::group_by(taxon_order, sample_realm) %>%
  dplyr::summarise(n = sum(n)) %>% 
  dplyr::mutate(taxon_order = factor(taxon_order, levels = c("Other", ts_ord_tot$taxon_order)))

ts_ord_sum <- ts_ord_bar %>% 
  dplyr::group_by(taxon_order) %>% 
  dplyr::summarise(n = sum(n))

# stacked bar plot
ts_bar <- ggplot(ts_ord_bar, aes(x = taxon_order, y = n)) +
  geom_bar(stat = "identity", aes(fill = sample_realm)) +
  geom_text(data = ts_ord_sum, aes(x = taxon_order, y = n, label = n), position = position_stack(vjust = 1), hjust = -0.1) +
  scale_fill_manual(name = "Realm", values = c("#66c2a5", "#8da0cb", "#fc8d62")) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, .06))) +
  labs(y = "Number of time series") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.7, 0.3),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.5), "cm"))

## combine

p_tree_comb <- cowplot::plot_grid(
  cowplot::plot_grid(NULL, tree_fake_comb, ncol = 2, rel_widths = c(0.02, 1), labels = c("a", "")), 
  ts_bar, 
  nrow = 2, rel_heights = c(1, 0.6), labels = c("", "b")) +
  theme(plot.background = ggplot2::element_rect(fill = "white", colour = "white"))

cowplot::save_plot("outputs/tree_comb.png", p_tree_comb, base_width = 10, base_height = 10)

#### Fig 4 - taxonomic coverage PREDICTS style ####

# distinct species records
dis_spp <- dat_n %>% 
  # remove subspecies text in names
  dplyr::mutate(taxon_name_preferred = stringr::str_replace(taxon_name_preferred, " subsp. \\w+", "")) %>% 
  # is the record resolved to species level (i.e., two words)?
  dplyr::mutate(word_count = stringr::str_count(taxon_name_preferred, "\\S+")) %>% 
  # only records resolved to species level
  dplyr::filter(word_count == 2) %>% 
  # unique species records
  dplyr::distinct(taxon_name_preferred, .keep_all = TRUE) 

## order level

tax_stork <- data.frame(taxon_order = c("Archaeognatha", "Zygentoma", "Ephemeroptera", "Odonata", "Orthoptera", "Phasmida", "Embioptera", "Grylloblattodea", "Mantophasmatodea", "Plecoptera", "Dermaptera", "Zoraptera", "Mantodea", "Blattodea", "Psocoptera", "Phthiraptera", "Thysanoptera", "Hemiptera", "Hymenoptera", "Strepsiptera", "Coleoptera", "Neuroptera", "Megaloptera", "Raphidioptera", "Trichoptera", "Lepidoptera", "Diptera", "Siphonaptera", "Mecoptera"), n_spp = c(513, 560, 3240, 5899, 23855, 3014, 463, 34, 15, 3743, 1978, 37, 2400, 7314, 5720, 5102, 5864, 103590, 116861, 609, 386500, 5868, 354, 254, 14391, 157338, 155477, 2075, 757)) %>% 
  # match stork name to our database name
  dplyr::mutate(taxon_order = dplyr::case_match(taxon_order, "Psocoptera" ~ "Psocodea", .default = taxon_order))

nrow(dplyr::filter(dat_n, is.na(taxon_order)))
# 0 NAs

sp_ord <- dis_spp %>% 
  dplyr::count(taxon_order) 

# total number of orders
sum(!is.na(sp_ord$taxon_order))
# 15 orders represented

tax_cov <- dplyr::left_join(sp_ord, tax_stork, by = "taxon_order") %>% 
  dplyr::mutate(perc = (n / n_spp) * 100)

# plot
p_tax_cov <- ggplot(data = tax_cov, aes(x = log10(n_spp), y = log10(n))) +
  # 10% line
  geom_abline(slope = 1, intercept = log10(0.1), lty = 2, colour = "#238b45") +
  # 1% line
  geom_abline(slope = 1, intercept = log10(0.01), lty = 2, colour = "#74c476") +
  # 0.1% line
  geom_abline(slope = 1, intercept = log10(0.001), lty = 2, colour = "#bae4b3") +
  annotate(geom = "label",
           label = c("10%", "1%", "0.1%"),
           x = c(log10(455000), log10(500000), log10(430000)),
           y = c(log10(455000 * 0.1), log10(500000 * 0.01), log10(430000 * 0.001)),
           colour = c("#238b45", "#74c476", "#bae4b3"),
           alpha = 0.5) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = taxon_order),
                           min.segment.length = unit(0, 'lines'),
                           force = 20,
                           force_pull = 0.5,
                           colour = "grey40") +
  coord_equal() +
  scale_x_continuous(name = "Estimated described species", breaks = log10(c(1, 10, 100, 1000, 10000, 100000)), labels = c("1", "10", "100", "1,000", "10,000", "100,000")) +
  scale_y_continuous(name = "Species represented in database", breaks = log10(c(1, 10, 100, 1000, 10000, 100000)), labels = c("1", "10", "100", "1,000", "10,000", "100,000"))

cowplot::save_plot("outputs/tax_cov.png", p_tax_cov, base_width = 6, base_height = 7)

## family level

# number of described species per family from Andy Purvis (mainly from Zhang 2011 Zootaxa 3148:7-327)
tax_ap <- read.csv("data/Arthropod families in PREDICTS with global S share.csv")

nrow(dplyr::filter(dat_n, is.na(taxon_family)))
# 24604 NAs

sp_fam <- dis_spp %>% 
  dplyr::group_by(taxon_order) %>% 
  dplyr::count(taxon_family) %>% 
  # collapse families together where appropriate - see tax_ap
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Anobiidae", "Ptinidae"), "Ptinidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Aphodiidae", "Scarabaeidae"), "Scarabaeidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Apionidae", "Brentidae"), "Brentidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Brachyceridae", "Curculionidae"), "Curculionidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Cetoniidae", "Scarabaeidae"), "Scarabaeidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Dasytidae", "Melyridae"), "Melyridae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Dryophthoridae", "Curculionidae"), "Curculionidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Dynastidae", "Scarabaeidae"), "Scarabaeidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Helophoridae", "Hydrophilidae"), "Hydrophilidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Hydrochidae", "Hydrophilidae"), "Hydrophilidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Malachiidae", "Melyridae"), "Melyridae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Melolonthidae", "Scarabaeidae"), "Scarabaeidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Rhynchitidae", "Attelabidae"), "Attelabidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Rutelidae", "Scarabaeidae"), "Scarabaeidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Caliscelidae", "Issidae"), "Issidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Eumenidae", "Vespidae"), "Vespidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Pantheidae", "Noctuidae"), "Noctuidae", taxon_family)) %>%
  dplyr::mutate(taxon_family = ifelse(taxon_family %in% c("Synthemistidae", "Corduliidae"), "Corduliidae", taxon_family)) %>%
  dplyr::group_by(taxon_order, taxon_family) %>%
  dplyr::summarise(n = sum(n))

# total number of families
sum(!is.na(sp_fam$taxon_family))
# 365 families represented

tax_cov_fam <- dplyr::left_join(sp_fam, tax_ap, by = c("taxon_family" = "Family")) %>% 
  dplyr::rename(n_spp = Global_S_described) %>% 
  dplyr::mutate(perc = (n / n_spp) * 100)

tax_cov_fam_sub <- dplyr::filter(tax_cov_fam, taxon_order %in% c("Lepidoptera", "Trichoptera", "Diptera", "Coleoptera", "Ephemeroptera", "Plecoptera", "Hemiptera")) %>% 
  dplyr::mutate(taxon_order_fact = factor(taxon_order, levels = c("Lepidoptera", "Trichoptera", "Diptera", "Coleoptera", "Ephemeroptera", "Plecoptera", "Hemiptera")))

# facetted plot
p_tax_cov_fam_fc <- ggplot(data = tax_cov_fam_sub, aes(x = log10(n_spp), y = log10(n))) +
  facet_wrap(~taxon_order_fact, ncol = 2) +
  # 10% line
  geom_abline(slope = 1, intercept = log10(0.1), lty = 2, colour = "#238b45") +
  # 1% line
  geom_abline(slope = 1, intercept = log10(0.01), lty = 2, colour = "#74c476") +
  # 0.1% line
  geom_abline(slope = 1, intercept = log10(0.001), lty = 2, colour = "#bae4b3") +
  geom_point() + 
  coord_equal() +
  scale_x_continuous(name = "Estimated described species", breaks = log10(c(1, 100, 10000)), labels = c("1", "100", "10,000")) +
  scale_y_continuous(name = "Species represented in database", breaks = log10(c(1, 10, 100, 1000, 10000, 100000)), labels = c("1", "10", "100", "1,000", "10,000", "100,000"))

## combine

tax_cov_comb <- cowplot::plot_grid(p_tax_cov, p_tax_cov_fam_fc, ncol = 2, labels = "auto") +
  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = "white"))

cowplot::save_plot("outputs/tax_cov_comb.png", tax_cov_comb, base_width = 9, base_height = 6)

#### Supplementary ####

##### Fig SX - family taxonomic coverage PREDICTS style #####

tax_cov_fam <- dplyr::left_join(sp_fam, tax_ap, by = c("taxon_family" = "Family")) %>%
  dplyr::rename(n_spp = Global_S_described) %>% 
  dplyr::mutate(perc = (n / n_spp) * 100)

r_ord_sort <- dplyr::arrange(r_ord, -n) %>% 
  dplyr::filter(taxon_order %in% tax_cov_fam$taxon_order)

tax_cov_fam <- tax_cov_fam %>% 
  dplyr::mutate(taxon_order_fact = factor(taxon_order, levels = r_ord_sort$taxon_order))

# facetted plot
p_tax_cov_fam_fc_supp <- ggplot(data = tax_cov_fam, aes(x = log10(n_spp), y = log10(n))) +
  facet_wrap(~taxon_order_fact, ncol = 4) +
  # 10% line
  geom_abline(slope = 1, intercept = log10(0.1), lty = 2, colour = "#238b45") +
  # 1% line
  geom_abline(slope = 1, intercept = log10(0.01), lty = 2, colour = "#74c476") +
  # 0.1% line
  geom_abline(slope = 1, intercept = log10(0.001), lty = 2, colour = "#bae4b3") +
  geom_point() + 
  coord_equal() +
  scale_x_continuous(name = "Estimated described species", breaks = log10(c(1, 100, 10000, 100000)), labels = c("1", "100", "10,000", "100,000")) +
  scale_y_continuous(name = "Species represented in database", breaks = log10(c(1, 10, 100, 1000, 10000, 100000)), labels = c("1", "10", "100", "1,000", "10,000", "100,000")) +
  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = "white"))

cowplot::save_plot("outputs/tax_cov_supp.png", p_tax_cov_fam_fc_supp, base_width = 7.5, base_height = 6)
