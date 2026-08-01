############
# Sculpin data model fitting
# M.kinneen 01/08/2026
############
library(ggplot2)
library(here)
library(dplyr)
library(tidyr)
library(readxl)
library(glmmTMB)
library(DHARMa)
library(emmeans)




#1. Start by reading in the sculpin and 3 spine data, and checking replicatuon
dat <- read.csv(here("data","sculpin_stickleback_long.csv"))

# traps per day, and how many days per year-site?
dat %>%
  distinct(year, treatment, Location, DOY, trap.no) %>%
  count(year, treatment, Location, DOY)

#Summary plot


#2. Start wraningling the data into the format we want
#Use year-site as independent replciates
# uSE YEAR-SITE as a factor level
# Nest days as RE within year-site replaictes
model_dat <- dat %>%
  mutate(
    year      = factor(year),
    treatment = factor(treatment),
    site      = factor(Location),
    species   = factor(species),
    trap      = factor(trap.no),
    year_site = factor(paste(year, site, sep = ":")), #Make the year-site factor
    day       = factor(paste(year_site, DOY, sep = ":")),
    value     = as.integer(value)
  )|>
  mutate(treatment = relevel(factor(treatment),ref = 'C')) #sets the control as the reference level

summary(model_dat)

#3. Sculpin model 
#Strat by fitting each species with a poisson and neg binomi. 
# Proobably use NB but no harm looking at both

#3.1 Model fitting
sculp_dat <- model_dat|>filter(species == "sculpin")

#Lets fit poissonn an Nbinom and comapre them
sc_pois <- glmmTMB( #Poisson model
  value ~ treatment + (1 | year_site) + (1 | day),
  data   = sculp_dat,
  family = poisson
)

sc_n_binom <- glmmTMB( #Poisson model
  value ~ treatment + (1 | year_site) + (1 | day),
  data   = sculp_dat,
  family = nbinom2
)

#3.2 model seleciton

#Compare the resiudals ob both models
sim_pois <- simulateResiduals(sc_pois)
sim_nb   <- simulateResiduals(sc_n_binom)

png(here("output","figures", "sculpin_diagnostics_pois_vs_nb.png"),
    width = 2400, height = 2400, res = 300)   # res=300 -> print quality
par(mfrow = c(2, 2))
plotQQunif(sim_pois);    plotResiduals(sim_pois)
plotQQunif(sim_nb);      plotResiduals(sim_nb)
par(mfrow = c(1, 1))
dev.off()

png(here("output","figures", "sculpin_nb_dharma.png"),
    width = 2400, height = 1200, res = 300)
plot(sim_nb)
dev.off()

#Resiudal rsults point towards nbinom, as poisson fails dharma diags.
#Poiss KS = 0.02, indcating sogncat devaiton from poisson dist.
#Confrom with AIC
AIC(sc_pois, sc_n_binom) #compare AIC
#nbinom much better


#Check zero inflation
png(here("output","figures", "sculpin_nb_zeroinflation.png"),
    width = 1600, height = 1200, res = 300)
testZeroInflation(sim_nb)
dev.off()
# p> 0.05, ratio = 0.93

# Sculpin model with negative binomial looks good based on resiudals
# and zero inflation doestn seem like an issue.

# 3.2 Model output and summary
summary(sc_n_binom)


#Random effects
# Effect of day is approimatelytwice that of site. This indicates that that sampling
# conditions on the day ahad twice is much impac ton catch rates as the sie that was sampled.
# 
# 

#Fixed effects
car::Anova(sc_n_binom)
#Treatment is signficant

#Do per treament effects
emmeans::emmeans(sc_n_binom, pairwise ~ treatment, type = "response")


# Aminos are essentially the same as the control (A = C*1.01)
# Eggs and egg juice yield catch rates that are 15.7 and 2.04 times higher
# than the control, respectively.
#When we consider pairwise comparisons, catch rates are 7.7 times higher than
# when egg juice alone is used.
#However, J os only estimated for fuel dump in 2020 and 2025


# Plot code for results

# pull the emmeans into a data frame
emm_df <- as.data.frame(emmeans(sc_n_binom, ~ treatment, type = "response"))
emm_df$treatment <- factor(emm_df$treatment,
                           levels = emm_df$treatment[order(emm_df$response)])

ggplot(emm_df, aes(x = treatment, y = response)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.15, linewidth = 0.6, colour = "grey30") +
  geom_point(size = 4, colour = "#2c3e50") +
  scale_y_log10(
    breaks = c(0.5, 1, 2, 5, 10, 20, 40),
    labels = c("0.5", "1", "2", "5", "10", "20", "40")
  ) +
  labs(
    x = "Treatment",
    y = "Predicted catch per trap (log scale)",
    title = "Sculpin catch by treatment",
    subtitle = "Model-estimated means \u00b1 95% CI (negative binomial GLMM)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(colour = "grey40", size = 11),
    axis.title    = element_text(face = "bold"),
    axis.text     = element_text(colour = "grey20"),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.4)
  )

ggsave(here("output","figures", "sculpin_catch_by_treatment.png"),
       width = 7, height = 5, dpi = 300)