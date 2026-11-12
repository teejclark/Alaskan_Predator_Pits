#### CODE FOR PREDATOR-PIT MANUSCRIPT
#### Clark-Wolf et al., Journal of Animal Ecology
#### Wolf-moose predator pit dynamics, Tanana Flats, Alaska (1963-2013)

library(tidyverse)
library(ggplot2)
library(patchwork)
library(imputeTS)
library(depmixS4)
library(rEDM)

# import data
dat <- read.csv("finaldata.csv")

##################################################################################################################
#### DATA PREPARATION ####

# interpolate missing values in wolf and moose counts/densities
dat$No.Wolves_int <- na_interpolation(dat$No..Wolves)
dat$No.Moose_int <- na_interpolation(dat$No..Moose)
dat$Moose.Density_int <- na_interpolation(dat$Moose.Density)
dat$Wolf.Density_int <- na_interpolation(dat$Wolf.Density)

# calculate moose population growth rate (lambda = N[t+1]/N[t])
dat$Moose.Lambda_int <- c(NA, dat$No.Moose_int[2:length(dat$No.Moose_int)] /
                               dat$No.Moose_int[1:(length(dat$No.Moose_int) - 1)])

# Period: pre- vs. post-wolf-control (1963-1982 vs. 1983-2013)
dat <- dat %>%
  mutate(Period = case_when(
    Year >= 1963 & Year <= 1982 ~ "pre-control",
    Year >= 1983 & Year <= 2013 ~ "post-control"
  ))

# Period2: HMM-identified density states
# high density (initial) = 1963-1970; low density = 1971-1986; high density (post-recovery) = 1987-2013
dat <- dat %>%
  mutate(Period2 = case_when(
    Year >= 1963 & Year <= 1970 ~ "High density (1963-1970)",
    Year >= 1971 & Year <= 1986 ~ "Low density (1971-1986)",
    Year >= 1987 & Year <= 2013 ~ "High density (1987-2013)"
  ),
  Period2 = factor(Period2, levels = c("High density (1963-1970)",
                                        "Low density (1971-1986)",
                                        "High density (1987-2013)")))

# state-specific subsets used throughout
blaha <- dat[dat$Period == "pre-control", ]
blahb <- dat[dat$Period == "post-control", ]
blahc <- dat[dat$Period2 == "High density (1963-1970)", ]
blahd <- dat[dat$Period2 == "Low density (1971-1986)", ]
blahe <- dat[dat$Period2 == "High density (1987-2013)", ]

##################################################################################################################
#### FIGURE 1: wolf/moose densities and covariates over time ####

p1 <- ggplot(dat, aes(x = Year)) +
  geom_segment(x = 1976, xend = 1976, y = 0, yend = Inf, linetype = "dashed", color = "black", lwd = 1) +
  geom_segment(x = 1983, xend = 1983, y = 0, yend = Inf, linetype = "dashed", color = "black", lwd = 1) +
  annotate("rect", xmin = 1976, xmax = 1983, ymin = 0, ymax = Inf, alpha = 0.5) +
  geom_line(aes(y = Moose.Density_int * 10, color = "Moose"), lwd = 2) +
  geom_line(aes(y = Wolf.Density_int, color = "Wolves"), lwd = 2) +
  scale_y_continuous(
    name = expression(paste("Wolf Density (# per 1,000 ", km^2, ")")),
    sec.axis = sec_axis(~. / 10, name = expression(paste("Moose Density (# per ", km^2, ")")))
  ) +
  scale_color_manual(values = c("Moose" = "#3d348b", "Wolves" = "#f35b04"), guide = guide_legend(title = NULL)) +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 20), axis.text = element_text(size = 18, color = "black"),
    axis.text.y.right = element_text(margin = margin(r = 10)),
    axis.title.y.right = element_text(margin = margin(r = 20)),
    axis.text.x = element_text(size = 18), axis.title.x = element_text(size = 20)
  )

p2 <- ggplot(dat, aes(x = Year)) +
  geom_segment(x = 1976, xend = 1976, y = 0, yend = Inf, linetype = "dashed", color = "black", lwd = 0.8) +
  geom_segment(x = 1983, xend = 1983, y = 0, yend = Inf, linetype = "dashed", color = "black", lwd = 0.8) +
  annotate("rect", xmin = 1976, xmax = 1983, ymin = 0, ymax = Inf, alpha = 0.5) +
  geom_line(aes(y = Harvest..), color = "#e09f3e", lwd = 1.5) +
  scale_y_continuous(name = "Total Moose Harvest\nRate (%)") +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12, color = "black"))

p3 <- ggplot(dat, aes(x = Year)) +
  geom_segment(x = 1976, xend = 1976, y = 0, yend = Inf, linetype = "dashed", color = "black", lwd = 0.8) +
  geom_segment(x = 1983, xend = 1983, y = 0, yend = Inf, linetype = "dashed", color = "black", lwd = 0.8) +
  annotate("rect", xmin = 1976, xmax = 1983, ymin = 0, ymax = Inf, alpha = 0.5) +
  geom_line(aes(y = Total.Snowfall..cm.), color = "#90e0ef", lwd = 1.5) +
  scale_y_continuous(name = "Total Snowfall\n(cm)") +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12, color = "black"))

p4 <- ggplot(dat, aes(x = Year)) +
  geom_segment(x = 1976, xend = 1976, y = 0, yend = Inf, linetype = "dashed", color = "black", lwd = 0.8) +
  geom_segment(x = 1983, xend = 1983, y = 0, yend = Inf, linetype = "dashed", color = "black", lwd = 0.8) +
  annotate("rect", xmin = 1976, xmax = 1983, ymin = 0, ymax = Inf, alpha = 0.5) +
  geom_line(aes(y = fire), color = "#c1121f", lwd = 1.5) +
  scale_y_continuous(name = "Habitat Quality\n(ha burned,\n10-20yr lag)") +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12, color = "black"))

p1 / (p2 | p3 | p4) + plot_layout(heights = c(2, 1))
ggsave("figure1.png", width = 12, height = 9, dpi = 600)

##################################################################################################################
#### FIGURE S1: interpolation of wolf and moose count data ####

a <- ggplot(dat) +
  geom_line(aes(x = Year, y = No.Moose_int), lwd = 1, color = "blue") +
  geom_point(aes(x = Year, y = No.Moose_int), size = 3, color = "red") +
  geom_point(aes(x = Year, y = No..Moose), size = 3, color = "blue") +
  ylab("# Moose") +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20), axis.text = element_text(size = 18, color = "black"))

b <- ggplot(dat) +
  geom_line(aes(x = Year, y = No.Wolves_int), lwd = 1, color = "blue") +
  geom_point(aes(x = Year, y = No.Wolves_int), size = 3, color = "red") +
  geom_point(aes(x = Year, y = No..Wolves.Edit), size = 3, color = "blue") +
  ylab("# Wolves") +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20), axis.text = element_text(size = 18, color = "black"))

a + b
ggsave("figure_s1.png", width = 12, height = 7, dpi = 600)

##################################################################################################################
#### FIGURE S3: wolf-moose phase diagram ####

ggplot(dat, aes(x = Moose.Density_int, y = Wolf.Density_int)) +
  geom_path(aes(color = Year), lwd = 1.2, lineend = "round") +
  geom_point(aes(color = Year, shape = Period2), size = 3) +
  scale_color_viridis_c(option = "plasma", name = "Year") +
  scale_shape_manual(
    values = c("High density (1963-1970)" = 16,
               "Low density (1971-1986)" = 17,
               "High density (1987-2013)" = 15),
    name = "State"
  ) +
  labs(
    x = expression(paste("Moose Density (# per ", km^2, ")")),
    y = expression(paste("Wolf Density (# per 1,000 ", km^2, ")"))
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14), legend.text = element_text(size = 12))

ggsave("figure_s3.png", dpi = 600, width = 10, height = 10)

##################################################################################################################
#### HYSTERESIS ANALYSIS: wolf effect on moose growth, before vs. after wolf control ####

# basic pre/post-control test
summary(glm(Moose.Lambda_int ~ No.Wolves_int, data = blaha)) # beta = -0.00203, p<0.0001; r^2=0.665
summary(glm(Moose.Lambda_int ~ No.Wolves_int, data = blahb)) # beta = -0.00111, p=0.11; r^2=0.085

# Figure 2: pre/post-control wolf-moose relationship
ggplot(dat, aes(No.Wolves_int, Moose.Lambda_int, color = Period)) +
  geom_point(size = 3) +
  geom_smooth(aes(fill = Period), method = "lm", se = T, lwd = 2) +
  xlab("# Wolves") + ylab("Moose Population Growth") +
  scale_color_manual(values = c("pre-control" = "#3d348b", "post-control" = "#f35b04")) +
  scale_fill_manual(values = c("pre-control" = "#3d348b", "post-control" = "#f35b04")) +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20), axis.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 20), legend.text = element_text(size = 18))

ggsave("figure2.png", width = 10, height = 7, dpi = 600)

# robustness check: controlling for cow harvest rate (response to initial review)
summary(glm(Moose.Lambda ~ Wolf.Density + Cow.Harvest.Rate, data = blaha)) # beta = -0.0345, p<0.0001; r^2=0.787
summary(glm(Moose.Lambda ~ Wolf.Density + Cow.Harvest.Rate, data = blahb)) # beta = -0.0213, p=0.104; r^2=0.222

# expanded regression (Table S4): all hypothesized drivers simultaneously, response to Reviewer 2
summary(glm(Moose.Lambda_int ~ No.Wolves_int + Cow.Harvest.Rate + Total.Snowfall..cm. + fire + Bull.Harvest.Rate,
            data = blaha))
summary(glm(Moose.Lambda_int ~ No.Wolves_int + Cow.Harvest.Rate + Total.Snowfall..cm. + fire + Bull.Harvest.Rate,
            data = blahb))

# state-specific regressions (Fig. S4): wolf effect by HMM-identified state
summary(glm(Moose.Lambda_int ~ No.Wolves_int, data = blahc)) # high density (1963-1970)
summary(glm(Moose.Lambda_int ~ No.Wolves_int, data = blahd)) # low density (1971-1986)
summary(glm(Moose.Lambda_int ~ No.Wolves_int, data = blahe)) # high density (1987-2013)

# Figure S4: state-dependent wolf-moose relationship
ggplot(dat, aes(No.Wolves_int, Moose.Lambda_int, color = Period2)) +
  geom_point(size = 3) +
  geom_smooth(aes(fill = Period2), method = "lm", se = T, lwd = 2) +
  xlab("# Wolves") + ylab("Moose Population Growth") +
  scale_color_manual(
    values = c("High density (1963-1970)" = "#7b2d8b",
               "Low density (1971-1986)"  = "#f35b04",
               "High density (1987-2013)" = "#3d348b"),
    name = "State") +
  scale_fill_manual(
    values = c("High density (1963-1970)" = "#7b2d8b",
               "Low density (1971-1986)"  = "#f35b04",
               "High density (1987-2013)" = "#3d348b"),
    name = "State") +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20), axis.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 20), legend.text = element_text(size = 18))

ggsave("figure_s4.png", width = 10, height = 7, dpi = 600)

##################################################################################################################
#### FIGURE S2: Hidden Markov Model of moose density states ####
# following Chen et al. 2016

hmm <- function(observations){
  mod <- depmix(No.Moose_int ~ 1, data = observations, nstates = 2,
                trstart = runif(4), instart = c(15000, 5000))
  fm <- fit(mod)
  results <- fm@posterior
  results$state <- factor(results$state, levels = c("2", "1"))
  bind_cols(observations, results)
}

set.seed(12345) # stochastic value yielding 2-state solution

dat %>%
  hmm() %>%
  ggplot(aes(Year, No.Moose_int, col = state)) +
  geom_point(size = 2) +
  ylab("# Moose") +
  labs(color = "Stable State") +
  scale_color_discrete(labels = c("Low", "High")) +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 20), axis.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 20), legend.text = element_text(size = 18))

ggsave("figure_s2.png", width = 10, height = 7, dpi = 600)

##################################################################################################################
#### EMPIRICAL DYNAMIC MODELING (EDM) ####
# following Deyle et al. (2017) Fish and Fisheries; https://ha0ye.github.io/rEDM/articles/rEDM.html

# scale variables
reg <- as.data.frame(dat$Year); colnames(reg) <- c("time")
reg$wolves <- scale(dat$No.Wolves_int)
reg$snow <- scale(dat$Total.Snowfall..cm.)
reg$harvest <- scale(dat$Harvest..)
reg$moose <- scale(dat$No.Moose_int)
reg$fire <- scale(dat$fire)
reg$bull <- scale(dat$Bull.Harvest.Rate)

# 1) embedding dimension (E)
rho_E <- rEDM::EmbedDimension(dataFrame = reg, columns = "moose", target = "moose",
                               lib = "1 51", pred = "1 51", showPlot = T)

# 2) test for nonlinearity
rho_theta_E9 <- rEDM::PredictNonlinear(dataFrame = reg, columns = "moose", target = "moose",
                                        lib = "1 49", pred = "1 49", E = E)
# theta > 0; evidence for nonlinearity

# 3) multivariate forecast skill (Table S1)
lib <- c(1, NROW(reg)); pred <- c(1, NROW(reg))

# full model (all 4 hypothesized drivers)
block_lnlp_full <- block_lnlp(reg, lib = lib, pred = pred,
                               columns = c("wolves", "snow", "harvest", "fire"),
                               target_column = "moose", stats_only = F,
                               first_column_time = T, silent = T)
obs <- block_lnlp_full$model_output[2]
pred_full <- block_lnlp_full$model_output[3]
blah <- data.frame(cbind(obs, pred_full))
summary(lm(pred_full$Predictions ~ obs$Observations))

# best model (wolves + harvest + fire, excluding snow)
block_lnlp_best <- block_lnlp(reg, lib = lib, pred = pred,
                               columns = c("wolves", "fire", "harvest"),
                               target_column = "moose", stats_only = F,
                               first_column_time = T, silent = T)

# univariate models
block_lnlp_wolves <- block_lnlp(reg, lib = lib, pred = pred, columns = c("wolves"),
                                 target_column = "moose", stats_only = F,
                                 first_column_time = T, silent = T)
block_lnlp_snow <- block_lnlp(reg, lib = lib, pred = pred, columns = c("snow"),
                               target_column = "moose", stats_only = F,
                               first_column_time = T, silent = T)
block_lnlp_harvest <- block_lnlp(reg, lib = lib, pred = pred, columns = c("harvest"),
                                  target_column = "moose", stats_only = F,
                                  first_column_time = T, silent = T)
block_lnlp_fire <- block_lnlp(reg, lib = lib, pred = pred, columns = c("fire"),
                               target_column = "moose", stats_only = F,
                               first_column_time = T, silent = T)

# Figure 4: forecast skill bar chart (Table S1)
blah_fig4 <- data.frame(
  category = c("All Vars", "Minus Snow", "Harvest", "Snow", "Fire", "Wolves"),
  value = c(0.449, 0.561, 0.066, 0.087, 0.373, 0.023)
)

ggplot(blah_fig4, aes(x = reorder(category, -value), y = value, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("All Vars" = "red", "Minus Snow" = "blue", "Harvest" = "green",
                                "Snow" = "yellow", "Fire" = "pink", "Wolves" = "black")) +
  xlab("Variables") + ylab("R-Squared") +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 24), axis.text = element_text(size = 18, color = "black"),
        legend.position = "none")

ggsave("figure4.png", width = 10, height = 7, dpi = 600)

##################################################################################################################
#### FIGURE S5: observed vs. predicted moose population size ####

blah$Year <- reg$time[1:nrow(blah)]

p_timeseries <- ggplot(blah, aes(x = 1:nrow(blah))) +
  geom_line(aes(y = Observations, color = "Observed"), lwd = 1.5) +
  geom_line(aes(y = Predictions, color = "Predicted"), lwd = 1.5, linetype = "dashed") +
  scale_color_manual(values = c("Observed" = "#3d348b", "Predicted" = "#f35b04"), name = NULL) +
  ylab("Std. Moose Population Size") + xlab("Year") +
  theme_classic() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 14), legend.position = "top")

p_correlation <- ggplot(blah, aes(x = Observations, y = Predictions)) +
  geom_point(size = 3, color = "#3d348b") +
  geom_smooth(method = "lm", se = TRUE, color = "#f35b04", fill = "#f35b04") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", lwd = 1) +
  xlab("Observed Std. Moose Population Size") + ylab("Predicted Std. Moose Population Size") +
  theme_classic() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14, color = "black"))

p_timeseries / p_correlation
ggsave("figure_s5.png", width = 10, height = 14, dpi = 600)

##################################################################################################################
#### SCENARIO ANALYSIS: counterfactual wolf control (Figure 5) ####

reg$moose_embed <- c(embed(reg$moose, dimension = 2)[, 1], NA)

# full model fit check
block_lnlp_check <- block_lnlp(reg, lib = c(1, NROW(reg)), pred = c(1, NROW(reg)),
                                columns = c("wolves", "snow", "harvest", "fire", "moose_embed", "moose"),
                                target_column = "moose", stats_only = F,
                                first_column_time = T, silent = T)
obs_check <- block_lnlp_check$model_output[2]
pred_check <- block_lnlp_check$model_output[3]
summary(lm(pred_check$Predictions ~ obs_check$Observations)) # R^2 = 0.86

# counterfactual: unfavorable conditions throughout the post-control period
# (lower wolves, harvest, snow, fire than observed)
moose_unfavorable <- reg
moose_unfavorable$wolves[19:51] <- moose_unfavorable$wolves[19:51] - 0.2
moose_unfavorable$harvest[19:51] <- moose_unfavorable$harvest[19:51] - 0.46
moose_unfavorable$snow[19:51] <- moose_unfavorable$snow[19:51] - 0.4
moose_unfavorable$fire[19:51] <- moose_unfavorable$fire[19:51] - 2

reg_inc <- rbind(reg, rep(NaN, ncol(reg)), moose_unfavorable)

scenario_baseline <- block_lnlp(reg, method = "simplex",
                                 columns = c("wolves", "snow", "harvest", "fire"),
                                 target_column = "moose", stats_only = F, silent = T)

scenario_unfavorable <- block_lnlp(reg_inc,
                                    lib = c(1, nrow(reg)), pred = c((nrow(reg) + 2), nrow(reg_inc)),
                                    method = "simplex",
                                    columns = c("wolves", "snow", "harvest", "fire"),
                                    target_column = "moose", stats_only = F, silent = T)

wolves_nochange <- scenario_baseline$model_output
wolves_increase <- scenario_unfavorable$model_output

wolves_total <- data.frame(cbind(wolves_nochange, wolves_increase))
wolves_total$year <- c(reg$time, NA)

b <- ggplot(wolves_total) +
  geom_line(aes(x = year, y = Observations), lwd = 2) +
  geom_line(aes(x = year, y = Predictions.1), col = "red", size = 2) +
  xlab("Year") + ylab("Standardized Moose Population Size") +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 24), axis.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 24), legend.text = element_text(size = 18))

ggsave("figure5.png", width = 10, height = 7, dpi = 600)

##################################################################################################################
#### SENSITIVITY ANALYSIS (Figure 3, Figure 6) ####

identify_transition <- function(input_vector) {
  # classify state based on the average of the last 30 time points
  last_30_values <- tail(input_vector, 30)
  average_value <- mean(last_30_values)
  result <- ifelse(abs(average_value - 1) < abs(average_value - 2), 1, 0)
  return(result)
  # 1 = transitioned to high density state (escaped predator pit)
  # 0 = did not transition (remained in predator pit)
}

simulate_state <- function(reg, wolves_val, harvest_val, snow_val, fire_val) {

  moose_increase <- reg
  moose_increase$wolves[17:51] <- moose_increase$wolves[17:51] + wolves_val
  moose_increase$harvest[17:51] <- moose_increase$harvest[17:51] + harvest_val
  moose_increase$snow[17:51] <- moose_increase$snow[17:51] + snow_val
  moose_increase$fire[17:51] <- moose_increase$fire[17:51] + fire_val

  reg_inc <- rbind(reg, rep(NaN, ncol(reg)), moose_increase)

  scenario_inc <- block_lnlp(reg_inc,
                              lib = c(1, nrow(reg)),
                              pred = c((nrow(reg) + 2), nrow(reg_inc)),
                              method = "simplex",
                              columns = c("wolves", "snow", "harvest", "fire"),
                              target_column = "moose",
                              stats_only = F, silent = T)

  wolves_increase <- scenario_inc$model_output

  mod <- depmix(Predictions ~ 1, data = wolves_increase[4:53, ], nstates = 2, instart = c(1, 0))
  fm <- fit(mod)
  results <- fm@posterior
  identify_transition(results$state)
}

# randomized sensitivity analysis: vary each covariate by +/- 2 SD (uniform draw)
nsims <- 10000
wolves_val <- runif(nsims, -2, 2)
harvest_val <- runif(nsims, -2, 2)
snow_val <- runif(nsims, -2, 2)
fire_val <- runif(nsims, -2, 2)
change_dat <- data.frame(wolves_val, harvest_val, snow_val, fire_val,
                          state_change = rep(NA, length(wolves_val)))

for (i in 1:nsims) {
  change_dat$state_change[i] <- simulate_state(reg,
                                                change_dat$wolves_val[i],
                                                change_dat$harvest_val[i],
                                                change_dat$snow_val[i],
                                                change_dat$fire_val[i])
}

# binomial GLM: likelihood of escaping the predator pit as a function of each covariate
model <- glm(state_change ~ wolves_val + harvest_val + snow_val + fire_val,
             data = change_dat, family = "binomial")
summary(model)

# increasing wolves = less likely to escape predator pit (negative)
# increasing harvest = less likely to escape predator pit (negative)
# increasing snow = less likely to escape predator pit (negative)
# increasing fire = more likely to escape predator pit (positive)
