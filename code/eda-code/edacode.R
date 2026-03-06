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

## ---- hypothesis_testing_Blend_A_vs_Blend_B ----
# is Blend A > Blend B relative to control? I hypothesize that Blend A increases captures.
# hypothesis testing
# Test whether blend_a increases trapping relative to control
# MORE than blend_b does, using the paired-deployment unit
# ("pair_id") as the replicate.
#
# Approach
# --------
# 1) Restrict to blend_a and blend_b deployments (drop NA blends).
# 2) Collapse (sum) treatment and control counts across species
#    within each pair_id × blend (so each deployment contributes
#    ONE effect size).
# 3) Compute logRR per deployment:
#       logRR = log( (treatment + 0.5) / (control + 0.5) )
#    (+0.5 avoids log(0) and matches your earlier definition).
# 4) Hypothesis test:
#    H0: blend_a is NOT greater than blend_b (location shift <= 0)
#    H1: blend_a > blend_b
#    - Wilcoxon rank-sum (robust, nonparametric)
#    - Permutation test on difference in medians (robust with ties)
#
# Output
# ------
# - n deployments per blend
# - median logRR per blend
# - delta = median(logRR_A) - median(logRR_B)
# - exp(delta) as fold-advantage of A over B
# - Wilcoxon p-value (one-sided)
# - Permutation p-value (one-sided)
library(dplyr)
df <- identification1
drops:
# - NA blends (control-only deployments like sept19_1, sept19_3, sept24_3)
# - any other unexpected blend labels, if they exist
df_ab <- df %>%
  filter(blend %in% c("blend_a", "blend_b"))
#The replicate is the deployment (pair_id).
# So we sum treatment and control across species within each pair_id × blend.
pairs <- df_ab %>%
  group_by(pair_id, blend) %>%
  summarise(
    # total mosquitoes caught in the blend trap across all species
    treatment_total = sum(treatment, na.rm = TRUE),
    # total mosquitoes caught in the paired control trap across all species
    control_total   = sum(control,   na.rm = TRUE),
    .groups = "drop"
  )
#Compute effect size per deployment with logRR
pairs <- pairs %>%
  mutate(
    logRR = log((treatment_total + 0.5) / (control_total + 0.5)),
    # Set factor order so alternative="greater" corresponds to A > B
    blend = factor(blend, levels = c("blend_a", "blend_b"))
  )

#effect size
# Delta = median(logRR_A) - median(logRR_B)
delta <- median(pairs$logRR[pairs$blend == "blend_a"]) -
         median(pairs$logRR[pairs$blend == "blend_b"])

cat("\n=== Effect size ===\n")
cat("Delta median logRR (A - B):", delta, "\n")
cat("exp(delta) fold-advantage of A over B:", exp(delta), "\n")

#Wilcoxon rank-sum test (one-sided: A > B)
# exact = FALSE avoids warnings about ties and forces the normal approximation,
# which is standard when ties are present (common with count-derived logRR).
cat("\n=== Wilcoxon rank-sum test (one-sided: blend_a > blend_b) ===\n")
wtest <- wilcox.test(logRR ~ blend, data = pairs, alternative = "greater", exact = FALSE)
print(wtest)

# Why do this?
# - With small n and tied values, a permutation test is transparent and robust.
# - We permute blend labels among deployments (pair_id rows).
#
# Statistic: difference in medians (A - B), same as delta above.
cat("\n=== Permutation test (one-sided, statistic = median difference) ===\n")

B <- 10000     # number of permutations (increase to 50000 for extra stability)
seed <- 1      # set seed so results are reproducible

set.seed(seed)

obs <- delta  # observed median difference (A - B)

perm_stats <- replicate(B, {
  perm_blend <- sample(pairs$blend)  # shuffle labels
  median(pairs$logRR[perm_blend == "blend_a"]) -
    median(pairs$logRR[perm_blend == "blend_b"])
})

# One-sided p-value: proportion of permutations with statistic >= observed
p_perm <- mean(perm_stats >= obs)

cat("Observed median difference (A - B):", obs, "\n")
cat("Permutation p-value (one-sided):", p_perm, "\n")

# Shows each deployment as one point, separated by blend.
 ggplot(pairs, aes(x = blend, y = logRR)) +
   geom_jitter(width = 0.08, height = 0, size = 2, alpha = 0.8) +   geom_hline(yintercept = 0, linetype = "dashed") +
   labs(
     title = "Deployment-level logRR by blend",
     subtitle = "Each point is one paired trap deployment (pair_id); dashed line = no effect (logRR=0)",
     x = "Blend", y = "logRR"
   )

## ---- 10) Ornithophilic mosquitos----
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# long format (same as you had)
sp_long <- species1 %>%
  select(species, blend, treatment, control) %>%
  pivot_longer(cols = c(treatment, control), names_to = "arm", values_to = "count") %>%
  mutate(arm = recode(arm, treatment = "treatment", control = "control"))

# ornithophilic species you specified (exact)
orn_species <- c(
  "Ae_atlanticustor",
  "Ae_vexans",
  "Ae_spp",
  "Cq_perturbans",
  "Cx_erraticus",
  "Cx_restuans",
  "Cx_nigripalpus",
  "Mn_dyari",
  "Ae_dupreei",
  "Cs_melanura",
  "Cx_salinarius"
)

# mean counts for treatment/control (for the bars)
sp_summary <- sp_long %>%
  filter(species %in% orn_species) %>%
  group_by(species, blend, arm) %>%
  summarise(mean_count = mean(count, na.rm = TRUE), .groups = "drop")

# compute proportion in treatment per species × blend (for the labels)
sp_prop <- sp_summary %>%
  tidyr::pivot_wider(names_from = arm, values_from = mean_count, values_fill = 0) %>%
  mutate(
    prop_treat = ifelse((treatment + control) == 0, NA_real_, treatment / (treatment + control)),
    # put label just to the right of the bar pair: use the larger of the two bars
    label_x = pmax(treatment, control, na.rm = TRUE)
  )

# order species by overall mean abundance (across blends + arms), like your old plot
orn_order <- sp_long %>%
  filter(species %in% orn_species) %>%
  group_by(species) %>%
  summarise(overall_mean = mean(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(overall_mean)) %>%
  pull(species)

sp_summary <- sp_summary %>%
  mutate(species = factor(species, levels = rev(orn_order)))

sp_prop <- sp_prop %>%
  mutate(species = factor(species, levels = rev(orn_order)))

# plot
dodge_w <- 0.9

orn_plot <- ggplot(sp_summary, aes(x = species, y = mean_count, fill = arm)) +
  geom_col(position = position_dodge(width = dodge_w), color = "black", linewidth = 0.8) +
  # one label per species × blend showing proportion in treatment
  geom_text(
    data = sp_prop,
    aes(x = species, y = label_x, label = sprintf("%.2f", prop_treat)),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 3.6
  ) +
  scale_fill_manual(
    values = c("treatment" = "#A6CEE3",  # light blue
               "control"   = "#FFD34D"), # yellow
    name = "Trap"
  ) +
  coord_flip() +
  facet_wrap(~ blend) +
  # numeric axis breaks 1–10 (this is the COUNT axis, even though it's horizontal after flip)
  scale_y_continuous(
    breaks = 1:10,
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Ornithophilic species abundance by blend and trap",
    subtitle = "Bars show mean count per pair (treatment vs paired control); labels show proportion in treatment",
    x = "Species",
    y = "Mean mosquitoes per pair"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

figure_file <- here::here("results", "figures", "eda-figures", "ornithophilic-species-blend2.png")
ggsave(filename = figure_file, plot = orn_plot, width = 10, height = 7, dpi = 300)

orn_plot
