## ---- packages --------

# %%
#load needed packages. make sure they are installed.
library(here) #for data loading/saving
library(dplyr)
library(skimr)
library(ggplot2)
library(gt) # for tables
library(tidyr)

## ---- load-data ----
# %%
# processed (cleaned) datasets. double check the names!!
data_location_1 <- here::here("data","processed-data","blend1.rds")
data_location_2 <- here::here("data","processed-data","height1.rds")
data_location_3 <- here::here("data","processed-data","species1.rds")
data_location_4 <- here::here("data","processed-data","identification1.rds") 
# I intentionally did not load the ornothophilicity text data because I don't need it for the EDA, but you could if you wanted to.
# It could also just be replaced with text descriptions so it's not a big deal.

# read in the data
blend1 <- readRDS(data_location_1)
height1 <- readRDS(data_location_2)
species1 <- readRDS(data_location_3)
identification1 <- readRDS(data_location_4)

summary(blend1)
summary(height1)
summary(species1)
summary(identification1)

## ---- load-data ----
# processed (cleaned) datasets. double check the names!!
data_location_1 <- here::here("data","processed-data","blend1.rds")
data_location_2 <- here::here("data","processed-data","height1.rds")
data_location_3 <- here::here("data","processed-data","species1.rds")
data_location_4 <- here::here("data","processed-data","identification1.rds") 
# I intentionally did not load the ornothophilicity text data because I don't need it for the EDA, but you could if you wanted to.
# It could also just be replaced with text descriptions so it's not a big deal.

# read in the data
blend1 <- readRDS(data_location_1)
height1 <- readRDS(data_location_2)
species1 <- readRDS(data_location_3)
identification1 <- readRDS(data_location_4)

summary(blend1)
summary(height1)
summary(species1)
summary(identification1)

# for height counts, I want to see how many mosquitoes were caught at each height. 
# data from other sources suggest that height should either increase or decrease particular species abundance
# and general abundance as well.
height_counts <- height1 %>%
  filter(height %in% c("10ft", "midlevel")) %>%
  count(date, location, trap_type, height, name = "mosq") %>%
  mutate(
    height = factor(
      height,
      levels = c("10ft", "midlevel"),
      labels = c("canopy (10ft)", "midlevel (4–6ft)")
    )
  )

## ---- fig_height_box_points_mean_se ----
abundance_height_boxplot <- ggplot(height_counts, aes(x = height, y = mosq, fill = height)) +
  geom_boxplot(color = "black", linewidth = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.12, height = 0, size = 2, alpha = 0.4, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.12, color = "black", linewidth = 0.8) +
  stat_summary(fun = mean, geom = "point", size = 3.5, color = "black") +
  scale_fill_manual(values = c("canopy (10ft)" = "#A6CEE3", "midlevel (4–6ft)" = "#FDBF6F")) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 10)) +
  labs(
    x = "Height",
    y = "Mosquitoes per trap-night",
    title = "Mosquito abundance by trap height",
    subtitle = "Trap-night totals with mean ± SE overlaid"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

# save figure
figure_file <- here::here("results", "figures", "eda-figures", "abundance-height-boxplot.png")
ggsave(filename = figure_file, plot = abundance_height_boxplot)
# chat gpt suggested these parameters but I'll hold off on them, width = 7, height = 5, dpi = 300) 
# might switch to dot plot another time, but this is decent for EDA.

## ---- abundance-height-table ----
abundance_height_table <- height_counts %>%
  dplyr::group_by(height) %>%
  dplyr::summarise(
    n_trapnights = dplyr::n(), # number of trap days/nights
    mean_mosq = mean(mosq), # mean number of mosquitoes per trap-night
    median_mosq = median(mosq), # median
    sd_mosq = sd(mosq), # standard deviation
    .groups = "drop"
  )

print(abundance_height_table)

# save to file
summarytable_file1 <- here("results", "tables", "abundance-height-table.rds")
saveRDS(abundance_height_table, file = summarytable_file1)

#This was only across a few total trap-nights, so I don't think it's worth doing a more detailed table with more stats, but you could if you wanted to. You could also do this for species abundance by height if you wanted to get more specific.

## ---- pooled_abundance_height ----
pooled_height <- height1 %>%
  dplyr::filter(height %in% c("10ft","midlevel")) %>%
  dplyr::mutate(height = dplyr::recode(height,
                                       "10ft" = "canopy (10ft)",
                                       "midlevel" = "midlevel (4–6ft)")) %>%
  dplyr::count(height, name = "mosq_total") %>%
  dplyr::mutate(prop_total = mosq_total / sum(mosq_total))

pooled_height_plot <- ggplot(pooled_height, aes(height, mosq_total, fill = height)) +
  geom_col(width = 0.60, color = "black", linewidth = 1.2) +        # thinner bars, thicker outline
  geom_text(aes(label = mosq_total), vjust = 1.2, fontface = "bold") + # totals inside bar
  scale_fill_manual(values = c("canopy (10ft)" = "#A6CEE3", "midlevel (4–6ft)" = "#FDBF6F")) +
  labs(title = "Pooled mosquito abundance by height",
       subtitle = "Total mosquitoes across all collections",
       x = "Height", y = "Total mosquitoes") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

pooled_height_plot
# number inside bar is total mosquitoes caught, not mean. Would need more trap nights for mean.
saveRDS(pooled_height, here::here("results","tables","pooled-abundance-height.rds"))
ggsave(here::here("results","figures","eda-figures","pooled-abundance-height.png"),
       plot = pooled_height_plot)

## ---- species_top_bar_by_blend_arm ----
# long version with arms
sp_long <- species1 %>%
  select(species, blend, treatment, control) %>%
  pivot_longer(cols = c(treatment, control), names_to = "arm", values_to = "count") %>%
  mutate(arm = recode(arm, treatment = "treatment", control = "control"))

# pick top 10 species by overall mean abundance (across everything)
top_species <- sp_long %>%
  group_by(species) %>%
  summarise(mean_count = mean(count), .groups = "drop") %>%
  arrange(desc(mean_count)) %>%
  slice_head(n = 10) %>%
  pull(species)

sp_summary <- sp_long %>%
  filter(species %in% top_species) %>%
  group_by(species, blend, arm) %>%
  summarise(mean_count = mean(count), .groups = "drop")

top_species_blend <- ggplot(sp_summary, aes(x = reorder(species, mean_count), y = mean_count, fill = arm)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c(
    "treatment" = "#A6CEE3",
    "control"   = "#FDBF6F"
  )) +
  coord_flip() +
  facet_wrap(~ blend) +
  labs(
    title = "Top species abundance by blend and arm",
    subtitle = "Bars show mean count per pair (treatment vs paired control)",
    x = "Species", y = "Mean mosquitoes per pair", fill = "Arm"
  )
figure_file <- here::here("results", "figures", "eda-figures", "top-species-blend.png")
ggsave(filename = figure_file, plot = top_species_blend) #, width = 8, height = 5, dpi = 300)

# ok this is actually cooler than i expected. Ur. sapphirinia has been implicated as a carrier of EEEV!!
# this is odd though because Ur. sapphirinia is not known to be ornithohilic, but they have been seen to 
# gravitate towards habitats with greater An. crucians comp. populations, and An. crucians IS implicated!
## ---- height --------
p1 <- mydata %>% ggplot(aes(x=Height)) + geom_histogram() 
plot(p1)
figure_file = here("results", "figures", "height-distribution.png")
ggsave(filename = figure_file, plot=p1) 


## ---- fitfig_blend_logrr ----
m_blend <- lm(logRR ~ blend, data = species1)
summary(m_blend)
# tests whether avg log r.r. differs by blend (pooled across species and pairs)
# outcome is logRR and predictor is blend with 1= blend 0= control
# basically seeing if mean attraction effect (aka logrr) is different
figure_file <- here::here("results", "figures", "eda-figures", "lm-logRR-blend.rds")
saveRDS(m_blend, file = figure_file)

## ---- fitfig_species_blend_all ----
species_blend_all <- species1 %>%
  ggplot(aes(x = blend, y = logRR)) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.4) +
  geom_smooth(aes(group = species), method = "lm", se = FALSE) +
  facet_wrap(~ species, scales = "free_y") +
  labs(
    title = "Blend effect on log response ratio varies by species",
    subtitle = "Each panel shows an lm fit of logRR ~ blend within that species",
    x = "Blend", y = "logRR"
  )

plot(species_blend_all)
# logRR = log (treatment+0.5/control+0.5) to avoid log(0) issues, but this is just for visualization. Would need to do a more formal analysis for any conclusions.
# each panel is 1 species. each grey dot is 1 trap pair for species.
# logRR is log response ratio of treatment vs paired control (effect size)
# =0 means no effect, >0 blend caught more, <0 means blend caught fewer.
figure_file <- here::here("results", "figures", "eda-figures", "fit-species-blend-all.png")
ggsave(filename = figure_file, plot = species_blend_all, width = 12, height = 10, dpi = 300)

