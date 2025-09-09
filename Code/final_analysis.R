#### CODE FOR PREDATOR-PIT MANUSCRIPT

library(tidyverse)
library(ggplot2)

# import data
dat <- read.csv("finaldata.csv")

##################################################################################################################

#### LOOK AT DATA

# plot moose, wolf, and caribou densities over time
ggplot(dat[!is.na(dat$Moose.Density),], aes(Year, Moose.Density))+
  geom_point(size = 2)+
  geom_line(size = 1.5)

ggplot(dat[!is.na(dat$No..Caribou),], aes(Year, No..Caribou))+
  geom_point(size = 2)+
  geom_line(size = 1.5)

ggplot(dat[!is.na(dat$Wolf.Density),], aes(Year, Wolf.Density))+
  geom_point(size = 2)+
  geom_line(size = 1.5)

# plot wolf density vs. moose growth rate
ggplot(dat, aes(Wolf.Density, Moose.Lambda, color = Period))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")

summary(glm(Moose.Lambda ~ Wolf.Density + Period, data = dat))

blaha <- dat[dat$Period == "pre-control",]
blahb <- dat[dat$Period == "post-control",]

summary(glm(Moose.Lambda ~ Wolf.Density, data = blaha)) # beta = -0.0345, p<0.0001; r^2=0.787
summary(glm(Moose.Lambda ~ Wolf.Density, data = blahb)) # beta =-0.0213, p=0.104; r^2=0.222

dat$Moose.Density_int <- na_interpolation(dat$Moose.Density)
dat$Wolf.Density_int <- na_interpolation(dat$Wolf.Density)

# plot wolves vs. moose
ggplot(dat, aes(x = Year))+
  geom_segment(x = 1976, xend = 1976, y = 0, yend = Inf, linetype = "dashed", color = "black", lwd = 1)+
  geom_segment(x = 1983, xend = 1983, y = 0, yend = Inf, linetype = "dashed", color = "black", lwd = 1)+
  annotate("rect", xmin = 1976, xmax = 1983, ymin = 0, ymax = Inf, alpha = 0.5)+
  geom_line(aes(y = Moose.Density_int*10, color = "Moose"), size = 1, lwd = 2) +
  geom_line(aes(y = Wolf.Density_int, color = "Wolves"), size = 1, lwd = 2) +
  scale_y_continuous(
    name = expression(paste("Wolf Density (# per 1,000 ", km^2, ")")),
    sec.axis = sec_axis(~./10, name = expression(paste("Moose Density (# per ", km^2, ")")))
  ) +
  scale_color_manual(
    values = c("Moose" = "#3d348b", "Wolves" = "#f35b04"),
    guide = guide_legend(title = NULL)
  ) +
  theme_classic()+
  theme(
    legend.position = "none",
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=20),
    axis.text = element_text(size=18, color="black"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    axis.text.y.right = element_text(margin = margin(r = 10)),
    axis.title.y.right = element_text(margin = margin(r = 20)))

ggsave("figure1_alt.png", width = 10, height = 7, dpi = 600)

##################################################################################################################

#### INTERPOLATE DATA
# NOTE: This is STEP 1b - we showed hysteresis by testing for time-dependent driver-response relationships

library(imputeTS)

dat$No.Wolves_int <- na_interpolation(dat$No..Wolves)
#dat$No.Wolves_int <- na_interpolation(dat$No..Wolves.Edit)
dat$No.Moose_int <- na_interpolation(dat$No..Moose)

# change graphs to density for easier visualization
a <- ggplot(dat, aes(Year, Moose.Density_int))+
  geom_line(size = 1.5)+
  geom_point(size = 3, color = "red")+
  ylab("No. Moose")+
  theme_classic()

b <- ggplot(dat, aes(Year, Wolf.Density_int))+
  geom_line(size = 1.5)+
  geom_point(size = 3, color = "red")+
  ylab("No. Wolves")+
  theme_classic()

a+b

# graph imputation
a <- ggplot(dat)+
  geom_line(aes(x = Year, y = No.Moose_int), lwd = 1, color = "blue")+
  geom_point(aes(x = Year, y = No.Moose_int), size = 3, color = "red")+
  geom_point(aes(x = Year, y = No..Moose), size = 3, color = "blue")+
  ylab("# Moose")+
  theme_classic()+
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=20),
    axis.text = element_text(size=18, color="black"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))

ggplot_na_imputations(dat$No..Moose, dat$No.Moose_int)
ggplot_na_imputations(dat$No..Wolves.Edit, dat$No.Wolves_int)
dat$No.Wolves_int[51] <- 280.275
dat$No..Wolves.Edit[51] <- 280.275
dat$No.Wolves_int[50] <- 234

b <- ggplot(dat)+
  geom_line(aes(x = Year, y = No.Wolves_int), lwd = 1, color = "blue")+
  geom_point(aes(x = Year, y = No.Wolves_int), size = 3, color = "red")+
  geom_point(aes(x = Year, y = No..Wolves.Edit), size = 3, color = "blue")+
  ylab("# Wolves")+
  theme_classic()+
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=20),
    axis.text = element_text(size=18, color="black"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))

library(patchwork)
a+b
ggsave("figure_s1.png", width = 12, height = 7, dpi = 600)


# calculate moose lambda
dat$Moose.Lambda_int <- c(NA,dat$No.Moose_int[2:length(dat$No.Moose_int)]/dat$No.Moose_int[1:(length(dat$No.Moose_int)-1)])

ggplot(dat, aes(No.Wolves_int, Moose.Lambda_int, color = Period))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")

blaha <- dat[dat$Period == "pre-control",]
blahb <- dat[dat$Period == "post-control",]

summary(glm(Moose.Lambda_int ~ No.Wolves_int, data = blaha)) # beta = -0.00203, p<0.0001; r^2=0.665
summary(glm(Moose.Lambda_int ~ No.Wolves_int, data = blahb)) # beta = -0.00111, p=0.11; r^2=0.085

# plot time-dependent driver-response relationships
ggplot(dat, aes(No.Wolves_int, Moose.Lambda_int, color = Period))+
  geom_point(size = 3)+
  geom_smooth(aes(fill = Period), method = "lm", se = T, lwd = 2)+
  xlab("# Wolves")+
  ylab("Moose Population Growth")+
  scale_color_manual(
    values = c("pre-control" = "#3d348b", "post-control" = "#f35b04"))+
  scale_fill_manual(
    values = c("pre-control" = "#3d348b", "post-control" = "#f35b04"))+
  theme_classic()+
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=20),
    axis.text = element_text(size=18, color="black"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))

ggsave("figure2_alt.png", width = 10, height = 7, dpi = 600)

##################################################################################################################

#### CHANGEPOINT ANALYSIS

library(depmixS4)

# HIDDEN MARKOV MODEL

# first, discard initial high data (first 3 data points)
dat$No.Moose_int_disc <- dat$No.Moose_int

# write HMM based on Chen et al. 2016

hmm <- function(observations){
  mod <- depmix(No.Moose_int_disc~1, data = observations, nstates = 2,
                trstart = runif(4), instart = c(15000,5000))
  fm <- fit(mod)
  results <- fm@posterior
  results$state <- factor(results$state, levels = c("2", "1")) #as.character(results$state)
  bind_cols(observations, results)
}

set.seed(12345) # STOCHASTIC VALUE = 2 states

# plot?
dat %>%
  hmm() %>%
  ggplot(aes(Year, No.Moose_int_disc, col=state))+
  geom_point(size = 2)+
  ylab("# Moose")+
  labs(color = "Stable State")+
  scale_color_discrete(labels = c("Low", "High"))+
  theme_classic()+
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=20),
    axis.text = element_text(size=18, color="black"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))

ggsave("figure_s2.png", width = 10, height = 7, dpi = 600)

##################################################################################################################

#### LOOKING AT CAUSAL FACTORS
#https://ha0ye.github.io/rEDM/articles/rEDM.html#real-data-examples
#https://cran.r-project.org/web/packages/rEDM/vignettes/rEDM-tutorial.pdf
# following: Deyle et al. (2017) Fish and Fisheries

# STEPS: 1) simplex projection, identify E
# 2) S-maps - quantify nonlinearity and usefulness of rEDM
# 3) Univariate CCM - test for causation from different drives
# 4) recreating Fig. 8 with block_lnlp

library(rEDM)

# scale vars and add to new dataset
reg <- as.data.frame(dat$Year); colnames(reg) <- c("time")
reg$wolves <- scale(dat$No.Wolves_int)
reg$snow <- scale(dat$Total.Snowfall..cm.)
reg$harvest <- scale(dat$Harvest..)
reg$moose <- scale(dat$No.Moose_int)
reg$fire <- scale(dat$fire)

# try editing out data

# 1) First evaluate embedding dimension (E)
rho_E <- rEDM::EmbedDimension(dataFrame = reg, columns = "moose", target = "moose",
                        lib = "1 26", pred = "27 51", showPlot = T)
# E=9 has the best skill (E=2 for moose_pop)

# 2) Test for nonlinearity
E <- 1
rho_theta_E9 <- rEDM::PredictNonlinear(dataFrame = reg, columns = "moose", target = "moose",
                                       lib = "1 26", pred = "27 51", E = E)
# theta = >0; evidence for nonlinearity


# 3) Multivariate Models - see if adding extra variables will improve predictions
# ALL VARS

# ALL VARIABLES;
lib <- c(1, NROW(reg))
pred <- c(1, NROW(reg))
block_lnlp_output <- block_lnlp(reg, lib = lib, pred = pred, 
                                columns = c("wolves","snow","harvest","fire"),
                                target_column = "moose", stats_only = F,
                                first_column_time = T, silent = T)

obs <- block_lnlp_output$model_output[2]
pred <- block_lnlp_output$model_output[3]
blah <- data.frame(cbind(obs,pred))
summary(lm(pred$Predictions ~ obs$Observations))

# WOLVES SNOW HARVEST
lib <- c(1, NROW(reg))
pred <- c(1, NROW(reg))
block_lnlp_output <- block_lnlp(reg, lib = lib, pred = pred, 
                                columns = c("wolves","snow","harvest"),
                                target_column = "moose", stats_only = F,
                                first_column_time = T, silent = T)

obs <- block_lnlp_output$model_output[2]
pred <- block_lnlp_output$model_output[3]
blah <- data.frame(cbind(obs,pred))

# SNOW AND WOLVES
lib <- c(1, NROW(reg))
pred <- c(1, NROW(reg))
block_lnlp_output2 <- block_lnlp(reg, lib = lib, pred = pred, 
                                columns = c("wolves","snow"),
                                target_column = "moose", stats_only = F,
                                first_column_time = T, silent = T)

obs2 <- block_lnlp_output2$model_output[2]
pred2 <- block_lnlp_output2$model_output[3]
blah2 <- data.frame(cbind(obs2,pred2))

# WOLVES
lib <- c(1, NROW(reg))
pred <- c(1, NROW(reg))
block_lnlp_output3 <- block_lnlp(reg, lib = lib, pred = pred, 
                                 columns = c("wolves"),
                                 target_column = "moose", stats_only = F,
                                 first_column_time = T, silent = T)

obs3 <- block_lnlp_output3$model_output[2]
pred3 <- block_lnlp_output3$model_output[3]
blah3 <- data.frame(cbind(obs3,pred3))

# SNOW
lib <- c(1, NROW(reg))
pred <- c(1, NROW(reg))
block_lnlp_output4 <- block_lnlp(reg, lib = lib, pred = pred, 
                                 columns = c("snow"),
                                 target_column = "moose", stats_only = F,
                                 first_column_time = T, silent = T)
obs4 <- block_lnlp_output4$model_output[2]
pred4 <- block_lnlp_output4$model_output[3]
blah4 <- data.frame(cbind(obs4,pred4))

# HARVEST
lib <- c(1, NROW(reg))
pred <- c(1, NROW(reg))
block_lnlp_output5 <- block_lnlp(reg, lib = lib, pred = pred, 
                                 columns = c("harvest"),
                                 target_column = "moose", stats_only = F,
                                 first_column_time = T, silent = T)
obs5 <- block_lnlp_output5$model_output[2]
pred5 <- block_lnlp_output5$model_output[3]
blah5 <- data.frame(cbind(obs5,pred5))

# FIRE
lib <- c(1, NROW(reg))
pred <- c(1, NROW(reg))
block_lnlp_output5 <- block_lnlp(reg, lib = lib, pred = pred, 
                                 columns = c("wolves", "fire"),
                                 target_column = "moose", stats_only = F,
                                 first_column_time = T, silent = T)
obs5 <- block_lnlp_output5$model_output[2]
pred5 <- block_lnlp_output5$model_output[3]
blah5 <- data.frame(cbind(obs5,pred5))

# HARVEST AND SNOW
lib <- c(1, NROW(reg))
pred <- c(1, NROW(reg))
block_lnlp_output6 <- block_lnlp(reg, lib = lib, pred = pred, 
                                 columns = c("snow","harvest"),
                                 target_column = "moose", stats_only = F,
                                 first_column_time = T, silent = T); block_lnlp_output5$stats
obs6 <- block_lnlp_output6$model_output[2]
pred6 <- block_lnlp_output6$model_output[3]
blah6 <- data.frame(cbind(obs6,pred6))


# create data.frame for bar graph
blah <- data.frame(category = c("All Vars", "Minus Snow", "Harvest", "Snow", "Fire", "Wolves"),
                   value = c(0.537, 0.581, 0.066, 0.087, 0.373, 0.023))

ggplot(blah, aes(x = reorder(category, -value), y = value, fill = category))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("All Vars" = "red", "Minus Snow" = "blue", "Harvest" = "green",
                               "Snow" = "yellow", "Fire" = "pink", "Wolves" = "black"))+
  xlab("Variables")+
  ylab("R-Squared")+
  theme_classic()+
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=24),
    axis.text = element_text(size=18, color="black"),
    legend.position = "none")


ggsave("figure4.png", width = 10, height = 7, dpi = 600)

##################################################################################################################
#### Scenario Analysis - Moose ####
# GOAL: can we stop the transition to a high density state?

# add in embedded moose data for better prediction?
reg$moose_embed <- c(embed(reg$moose, dimension = 2)[,1],NA)

lib <- c(1, NROW(reg))
pred <- c(1, NROW(reg))
block_lnlp_output <- block_lnlp(reg, lib = lib, pred = pred, 
                                columns = c("wolves","snow","harvest","fire", "moose_embed", "moose"),
                                target_column = "moose", stats_only = F,
                                first_column_time = T, silent = T)
obs <- block_lnlp_output$model_output[2]
pred <- block_lnlp_output$model_output[3]
summary(lm(pred$Predictions ~ obs$Observations)) # r-squared is 0.8618, OK!

# WOLVES #
# NOTE: for now, just increase/decrease by 0.5SD
wolves_increase <- reg$wolves + 2
wolves_decrease <- reg$wolves -  2

# make new datasets
reg_inc0 <- reg
reg_inc0$wolves <- wolves_increase
reg_dec0 <- reg
reg_dec0$wolves <- wolves_decrease

# combine datasets for simulated time-series
reg_inc <- rbind(reg, rep(NaN, ncol(reg)), reg_inc0)
reg_dec <- rbind(reg, rep(NaN, ncol(reg)), reg_dec0)

scenario_reg <- block_lnlp(reg,
                           method = "simplex",
                           columns = c("wolves","snow","harvest","fire", "moose_embed", "moose"),
                           target_column = "moose",
                           stats_only = F,
                           silent = T)

scenario_inc <- block_lnlp(reg_inc,
                           lib = c(1, nrow(reg)),
                           pred = c((nrow(reg)+2), nrow(reg_inc)),
                           method = "simplex",
                           columns = c("wolves","snow","harvest","fire", "moose_embed", "moose"),
                           target_column = "moose",
                           stats_only = F,
                           silent = T)

scenario_dec <- block_lnlp(reg_dec,
                           lib = c(1, nrow(reg)),
                           pred = c((nrow(reg)+2), nrow(reg_dec)),
                           method = "simplex",
                           columns = c("wolves","snow","harvest","fire", "moose_embed", "moose"),
                           target_column = "moose",
                           stats_only = F,
                           silent = T)

# plot
wolves_nochange <- scenario_reg$model_output
wolves_increase <- scenario_inc$model_output
wolves_decrease <- scenario_dec$model_output

plot(wolves_nochange$Observations,
     type = "l", col = "black", lwd = 2, xlab = "time",
     ylab = "Normalized Moose Population Size")
points(wolves_nochange$Predictions, bg = "black", pch = 21) # normal prediction
points(wolves_increase$Predictions, bg = "red", pch = 24) # increase wolves, all years
points(wolves_decrease$Predictions, bg = "blue", pch = 25) # decrease wolves, all years


## SCENARIO - NO WOLF CONTROL
# would moose have increased to high density state without wolf control?
# control was from 1976 to 1982 (row 14 to 20)
wolves_increase <- reg$wolves
wolves_increase[14:20,] <- wolves_increase[14:20,]+2
plot(reg$time, reg$wolves, type = "l")
points(reg$time, wolves_increase, col = "red", type = "l")

reg_inc0 <- reg
reg_inc0$wolves <- wolves_increase
reg_inc <- rbind(reg, rep(NaN, ncol(reg)), reg_inc0)

scenario_reg <- block_lnlp(reg,
                           method = "simplex",
                           columns = c("wolves","snow","harvest","fire", "moose_embed", "moose"),
                           target_column = "moose",
                           stats_only = F,
                           silent = T)

scenario_inc <- block_lnlp(reg_inc,
                           lib = c(1, nrow(reg)),
                           pred = c((nrow(reg)+2), nrow(reg_inc)),
                           method = "simplex",
                           columns = c("wolves","snow","harvest","fire", "moose_embed", "moose"),
                           target_column = "moose",
                           stats_only = F,
                           silent = T)

wolves_nochange <- scenario_reg$model_output
wolves_increase <- scenario_inc$model_output

plot(wolves_nochange$Observations,
     type = "l", col = "black", lwd = 2, xlab = "time",
     ylab = "Normalized Moose Population Size")
points(wolves_nochange$Predictions, bg = "black", pch = 21) # normal prediction
points(wolves_increase$Predictions, bg = "red", pch = 24) # increase wolves, all years


## SCENARIO - Make everything bad for moose - maybe from 5 to 30
# do they still shift back up?
moose_increase <- reg
moose_increase$wolves[19:51] <- moose_increase$wolves[19:51]+-.2#2 # more wolves
moose_increase$harvest[19:51] <- moose_increase$harvest[19:51]+-.46#2 # more harvest
moose_increase$snow[19:51] <- moose_increase$snow[19:51]+-.4#2 # more snow
moose_increase$fire[19:51] <- moose_increase$fire[19:51]+-2 # decrease fire

reg_inc <- rbind(reg, rep(NaN, ncol(reg)), moose_increase)

scenario_reg <- block_lnlp(reg,
                           method = "simplex",
                           columns = c("wolves","snow","harvest","fire"),#,"moose"), #"moose_embed"),
                           target_column = "moose",
                           stats_only = F, 
                           silent = T)

scenario_inc <- block_lnlp(reg_inc,
                           lib = c(1, nrow(reg)),
                           pred = c((nrow(reg)+2), nrow(reg_inc)),
                           method = "simplex",
                           columns = c("wolves","snow","harvest","fire"),
                           target_column = "moose",
                           stats_only = F,
                           silent = T)

wolves_nochange <- scenario_reg$model_output
wolves_increase <- scenario_inc$model_output

plot(wolves_nochange$Observations,
     type = "l", col = "black", lwd = 2, xlab = "time",
     ylab = "Normalized Moose Population Size")
points(wolves_nochange$Predictions, bg = "black", pch = 21) # normal prediction
points(wolves_increase$Predictions, bg = "red", pch = 24) # increase wolves, all years

# plot change together
wolves_total <- data.frame(cbind(wolves_nochange, wolves_increase))
wolves_total$year <-c(reg$time,NA) 

b <- ggplot(wolves_total)+
  geom_line(aes(x = year, y = Observations), lwd = 2)+ 
  #geom_point(aes(x= year, y = Predictions), col = "black", size = 2)+
  geom_line(aes(x= year, y = Predictions.1), col = "red", size = 2)+
  xlab("Year")+
  ylab("Standardized Moose Population Size")+
  theme_classic()+
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=24),
    axis.text = element_text(size=18, color="black"),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 18))

ggsave("figure5.png", width = 10, height = 7, dpi = 600)

# combined figures
library(patchwork)
a+b+plot_annotation(tag_levels = "a")&theme(plot.tag = element_text(size = 24))

ggsave("figure5_alt.png", width = 12, height = 7, dpi = 600)

##################################################################################################################

#### SENSITIVITY ANALYSIS ####
# NOTE: use timing of breakpoint to separate the two...
# 2 state model = middle stable state; 1 state = doesn't change!!!! = go off of breakpoints!
hmm <- function(observations){
  mod <- depmix(Predictions~1, data = observations, nstates = 2)
  fm <- fit(mod)
  results <- fm@posterior
  results$state <- as.character(results$state)
  bind_cols(observations, results)
}

set.seed(12352) # STOCHASTIC VALUE = 2 breakpoints

# plot?
wolves_nochange[4:53,] %>%
  hmm() %>%
  ggplot(aes(Index, Predictions, col=state))+
  geom_point(size = 2)+
  #geom_line(size = 1.5)
  theme_classic()

# fucking around with data issues
mod <- depmix(Predictions ~ 1, data = wolves_increase[4:53,], nstates = 2, instart = c(1,0))
              #trstart = matrix(c(0.05,0.05,0.05,0.7)))
fm <- fit(mod)
results <- fm@posterior; results; identify_transition(results$state)


# try last 30 vals
identify_transition <- function(input_vector) {
  # Take the last 20 values
  last_20_values <- tail(input_vector, 30)
  
  # Calculate the average of the last 20 values
  average_value <- mean(last_20_values)
  
  # Determine if the average is closer to 1 or 2
  result <- ifelse(abs(average_value - 1) < abs(average_value - 2), 1, 0)
  
  return(result)
}



vector1 <- c(1,1,1,1,2,2,2)
vector2 <- c(1,1,1,2,2,2,1,1,1)


identify_transition(vector1) # FALSE (0)
identify_transition(vector2) # TRUE (1)
# 1= transitions to high density state; 0 = doesn't transition

# test with real data...
identify_transition(results$state) # TRUE
identify_transition(results$state) # FALSE!!!!


# START TO BUILD FUNCTION FOR EDM
simulate_state <- function(reg, wolves_val, harvest_val, snow_val, fire_val){
  
  #EDM STUFF
  # NOTE: changed to 17 from 11
  moose_increase <- reg
  moose_increase$wolves[17:51] <- moose_increase$wolves[17:51]+wolves_val # more wolves
  moose_increase$harvest[17:51] <- moose_increase$harvest[17:51]+harvest_val # more harvest
  moose_increase$snow[17:51] <- moose_increase$snow[17:51]+snow_val # more snow
  moose_increase$fire[17:51] <- moose_increase$fire[17:51]+fire_val # decrease fire
  
  reg_inc <- rbind(reg, rep(NaN, ncol(reg)), moose_increase)
  
  scenario_inc <- block_lnlp(reg_inc,
                             lib = c(1, nrow(reg)),
                             pred = c((nrow(reg)+2), nrow(reg_inc)),
                             method = "simplex",
                             columns = c("wolves","snow","harvest","fire"),
                             target_column = "moose",
                             stats_only = F,
                             silent = T)
  
  wolves_increase <- scenario_inc$model_output
  
  # CHANGEPOINT STUFF
  #set.seed(12352)
  mod <- depmix(Predictions ~ 1, data = wolves_increase[4:53,], nstates = 2, instart = c(1,0))
                #trstart = matrix(c(0.05,0.05,0.05,0.5)))
  fm <- fit(mod)
  results <- fm@posterior
  identify_transition(results$state) 
  # 1 = did transition to high density state (out of pred pit)
  # 0 = did not transition (still in pred pit)
  
}

# set up for loop
nsims <- 10000
wolves_val <- runif(nsims, -2, 2)
harvest_val <- runif(nsims, -2, 2)
snow_val <- runif(nsims, -2, 2)
fire_val <- runif(nsims, -2, 2)
change_dat <- data.frame(wolves_val, harvest_val, snow_val, fire_val,
                         state_change = rep(NA,length(wolves_val)))

for (i in 1:nsims){
  
  change_dat$state_change[i] <- simulate_state(reg, 
                                               change_dat$wolves_val[i],
                                               change_dat$harvest_val[i],
                                               change_dat$snow_val[i],
                                               change_dat$fire_val[i]
                                               )
  
}

# new analysis - keep things stable and change other things
nsims <- 1000
wolves_val <- 0 # more wolves (2)
harvest_val <- 0 # more harvest (2)
snow_val <- 0 # more snow (2)
fire_val <- 0 # less fire (less habitat) (-2)
change_dat <- as.vector(rep(NA, nsims))

for (i in 1:nsims){
  
  change_dat[i] <- simulate_state(reg, 
                                               wolves_val,
                                               harvest_val,
                                               snow_val,
                                               fire_val
  )
  
}

length(change_dat[change_dat==1])/nsims

# binomial GLM
summary(glm(state_change ~ wolves_val + harvest_val + snow_val + fire_val,
            data = change_dat, family = "binomial"))

# increasing wolves = less likely to get out of predator pit (NEGATIVE) = CONFIRMED 
# increasing harvest = less likely to get out of predator pit (NEGATIVE) = CONFIRMED 
# increasing snow = less likely to get out of predator pit (NEGATIVE) = CONFIRMED 
# increasing fire = more likely to get out of predator pit (POSITIVE) = CONFIRMED 

# binomial GLM plot - create predict and maybe probability...
model <- glm(state_change ~ wolves_val + harvest_val + snow_val + fire_val,
    data = change_dat, family = "binomial")

dat <- data.frame(
  covariate = c("Wolves", "Harvest", "Snow", "Fire"),
  mean = c(-0.08471, -0.11275, -0.03010, 0.16051),
  se = c(0.01742, 0.01748, 0.01751, 0.01748)
)

dat$covariate <- factor(dat$covariate, c("Fire", "Harvest", "Wolves", "Snow"))

dat$lowerci <- dat$mean - 1.96*dat$se
dat$upperci <- dat$mean + 1.96*dat$se

a <- ggplot(dat, aes(x = covariate, y = mean))+
  geom_point(position = position_dodge(width = 0.3), size = 4)+
  geom_errorbar(
    aes(ymin = lowerci, ymax = upperci),
    position = position_dodge(width = 0.3),
    width = 0.2, size = 1.5)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  xlab("Covariates")+
  ylab("Sensitivity Analysis Values")+
  theme_classic()+
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=24),
    axis.text = element_text(size=18, color="black"),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 18))

ggsave("figure6.png", width = 10, height = 7, dpi = 600)

##################################################################################################################

# heatmap?
library(ggplot2)
library(dplyr)
library(tidyr)

# Define the items
items <- c(1, 2, 3, 4)

# Generate all subsets
all_subsets <- unlist(lapply(0:length(items), function(k) combn(items, k, simplify = FALSE)), recursive = FALSE)

# Convert subsets to binary matrix
binary_matrix <- lapply(all_subsets, function(subset) as.integer(items %in% subset)) %>%
  do.call(rbind, .) %>%
  as.data.frame()

# Add subset names for labeling
binary_matrix$Subset <- sapply(all_subsets, function(subset) paste0("{", paste(subset, collapse = ","), "}"))
binary_matrix$names <- c("None","Wolves","Harvest","Snow","Fire",
                         "Wolves + Harvest", "Wolves + Snow", "Wolves + Fire", "Harvest + Snow","Harvest + Fire", "Snow + Fire",
                         "Wolves + Harvest + Snow", "Wolves + Harvest + Fire", "Wolves + Snow + Fire", "Harvest + Snow + Fire",
                         "Wolves + Harvest + Snow + Fire")
binary_matrix$prop <- c(0,0,0.41,0,0,.43,.28,0,.45,.45,0,.70,.48,.72,.5,.77)

# Convert to long format for ggplot
long_data <- binary_matrix %>%
  pivot_longer(cols = starts_with("V"), names_to = "Item", values_to = "InSubset") %>%
  mutate(Item = as.integer(gsub("V", "", Item)))


long_data <- binary_matrix %>%
  pivot_longer(-c(Subset, prop), names_to = "Item", values_to = "InSubset") %>%
  mutate(Item = as.integer(gsub("V", "", Item)))  # Rename columns for clarity

# bar plot
ggplot(binary_matrix, aes(x = reorder(names, prop), y = prop)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip for better readability
  labs(x = "Subset", y = "Proportion", title = "Proportions by Subset") +
  theme_minimal()


# simpler bar plot (collapsed based on the # of "good" (0) covariates)
zero <- 0
one <- c(37.7,0,0,0)
two <- c(0,0,40.8,42.6,45.0,51.4)
three <- c(75.4,70.9,49.8,49.6)
four <- c(77.2)

blah <- data.frame(Variables = c("0","1","2","3","4"),
                   mean = c(zero,mean(one),mean(two),mean(three),four),
                   low = c(zero,zero,(mean(two)-sd(two)),(mean(three)-sd(three)),four),
                   high = c(zero,(mean(one)+sd(one)),(mean(two)+sd(two)),(mean(three)+sd(three)),four))

b <- ggplot(blah, aes(x = Variables, y = mean))+
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8)+
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.3, color = "black")+
  coord_flip()+
  labs(x = "# of Favorable Covariates", y = "% Escaping Predator Pit")+
  theme_classic()+
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=24),
    axis.text = element_text(size=18, color="black"),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 18))

# combined figures
library(patchwork)
a+b+plot_annotation(tag_levels = "a")&theme(plot.tag = element_text(size = 24))

ggsave("figure5_altalt.png", width = 12, height = 7, dpi = 600)

