## testing code for Stella

# mock_data <- data.frame(nest = rep(c("G01", "G01", "G02", "G03", "G04", "G04", "G04"), 5),
#                         chick = rep(c("A", "B", "A", "B", "A", "B", "C"), 5),
#                         age = c(1, 1, 1, 1, 1, 1, 1,
#                                 4, 5, 4, 4, 4, 4, 6,
#                                 8, 8, 9, 8, 10, 8, 8,
#                                 12, 12, 12, 12, 14, 13, 12,
#                                 22, 21, 24, 22, 26, 25, 24),
#                         mass = c(runif(7, 60, 125),
#                                  runif(7, 80, 700),
#                                  runif(7, 100, 400),
#                                  runif(7, 150, 800),
#                                  runif(7, 100, 1500))) %>%
#   mutate(chickID = paste0(nest, chick))
# 
# full_model <- nls(mass ~ SSlogis(age, Asym, xmid, scal), data = mock_data)

setwd("~/Downloads")

real_data <- read_csv("Stella_data.csv") %>%
  mutate(across(everything(), ~ as.character(.))) %>% # convert all to char to allow NA replace
  mutate(across(everything(), ~ na_if(., "\xa0"))) %>%
  pivot_longer(cols = c(starts_with("date"),
                        starts_with("status"),
                        starts_with("mass"),
                        starts_with("headbill"),
                        starts_with("expculmen")),
               names_to = c(".value", "sampling_event"),
               names_sep = "_") %>%
  filter(!is.na(date)) %>%
  mutate(hatchdate = as.Date(hatchdate, format = "%m/%d/%Y"),
         date = as.Date(date, format = "%m/%d/%Y"),
         age = difftime(date, hatchdate, units = "days") %>% as.numeric()) %>%
  mutate(across(status:expculmen, ~ as.numeric(.)))

###### fall 2025 code ######

library(nlme)

# just for reference
logistic_fun <- function(t, A, k, t0) {
  A / (1 + exp(-k * (t - t0)))
}

## A: asymptotic mass
## t0 = inflection day
## k = growth rate constant (how fast they approach A)

chicks_rescaled <- real_data %>%
  rename(chick_id = Chick_ID) %>%
  filter(!is.na(mass)) %>%
  mutate(nest_ID = factor(nest_ID),
         chick_id = factor(chick_id),
         Chickrank = factor(Chickrank),
         week = age / 7,
         week_c = week - mean(week, na.rm = TRUE))

by_chick <- chicks_rescaled %>%
  group_by(chick_id) %>%
  summarise(n = n(),
            n_unique_t = n_distinct(week_c),
            t_span = max(week_c) - min(week_c),
            .groups = "drop")

# minimal filter for seeding (NOT for final model):
seed_ok <- by_chick %>%
  filter(n_unique_t >= 3,   # need 3+ unique times
         t_span >= 1.0) %>%   # need ≥1 week span
  pull(chick_id)

seed_data <- chicks_rescaled %>%
  filter(chick_id %in% seed_ok)

# use SSlogis to get good starting values
nls_model <- nls(mass ~ SSlogis(week_c, Asym, xmid, scal),
                 data = seed_data)
summary(nls_model)

start_fixef <- as.numeric(coef(nls_model))   # c(Asym, xmid, scal)
names(start_fixef) <- c("Asym","xmid","scal")
start_fixef

## Asym 1609.927866
## xmid 1.074145
## scal 1.001637

ctrl <- nlmeControl(maxIter = 200, msMaxIter = 200, pnlsMaxIter = 50,
                    tolerance = 1e-6, pnlsTol = 1e-6)

fit1 <- nlme(
  mass  ~ SSlogis(week_c, Asym, xmid, scal),
  data   = chicks_rescaled,
  fixed  = Asym + xmid + scal ~ 1,
  random = Asym + xmid + scal ~ 1 | chick_id,
  start  = start_fixef,
  control = ctrl
)
summary(fit1)

# residuals vs fitted & by group
plot(fit1)

cor_abs <- cor(abs(res), fit, use = "complete.obs")
cor_abs # 0.05482276 << 0.2 -- good!

# autocorrelation check
ACF(fit1) # no strong autocorrelation

# now add hatch effects!
fit2 <- nlme(
  mass  ~ SSlogis(week_c, Asym, xmid, scal),
  data   = chicks_rescaled,
  fixed  = list(
    Asym ~ Chickrank,
    xmid ~ Chickrank,
    scal ~ Chickrank
  ),
  random = Asym + xmid + scal ~ 1 | chick_id,
  start  = c(
    # Asym:
    Asym_Intercept  = 1544.175,
    Asym_ChickrankB = 0,
    Asym_ChickrankC = 0,
    # xmid:
    xmid_Intercept  = 0.995,
    xmid_ChickrankB = 0,
    xmid_ChickrankC = 0,
    # scal:
    scal_Intercept  = 0.946,
    scal_ChickrankB = 0,
    scal_ChickrankC = 0
  ),
  control = ctrl
)
summary(fit2)
AIC(fit1, fit2)
# fit1 4086.934
# fit2 4092.097 --> worsened fit (slightly)

# try varying one parameter at a time

# only Asym varies by hatch order
fit_Asym <- update(fit1,
                   fixed = list(Asym ~ Chickrank, xmid ~ 1, scal ~ 1),
                   start  = c(
                     # Asym:
                     Asym_Intercept  = 1544.175,
                     Asym_ChickrankB = 0,
                     Asym_ChickrankC = 0,
                     # xmid:
                     xmid  = 0.995,
                     # scal:
                     scal  = 0.946))
AIC(fit1, fit_Asym)
# fit1     4086.934
# fit_Asym 4088.601 -- almost the same, but still worsened slightly

# only xmid varies by hatch order
fit_xmid <- update(fit1,
                   fixed = list(Asym ~ 1, xmid ~ Chickrank, scal ~ 1),
                   random = Asym + xmid ~ 1 | chick_id,  # <- drop random scal for now
                   start  = c(
                     # Asym:
                     Asym  = 1544.175,
                     # xmid:
                     xmid_Intercept  = 0.995,
                     xmid_ChickrankB = 0,
                     xmid_ChickrankC = 0,
                     # scal:
                     scal  = 0.946))
AIC(fit1, fit_xmid)
# fit1     4086.934
# fit_xmid 4097.478 --> worse again

fit_scal <- update(fit1,
                   fixed = list(Asym ~ 1, xmid ~ 1, scal ~ Chickrank),
                   start  = c(
                     # Asym:
                     Asym  = 1544.175,
                     # xmid:
                     xmid  = 0.995,
                     # scal:
                     scal_Intercept  = 0.946,
                     scal_ChickrankB = 0,
                     scal_ChickrankC = 0))
AIC(fit1, fit_scal)
# fit1     4086.934
# fit_scal 4087.753

fe <- fixef(fit1)
re <- ranef(fit1)
pars <- as.data.frame(re) %>%
  mutate(Asym = fe["Asym"] + Asym,
         xmid = fe["xmid"] + xmid,
         scal = fe["scal"] + scal,
         k = 1 / (fe["scal"] + scal))
head(pars)




SS_model <- nls(mass ~ SSlogis(week_c, Asym, xmid, scal), data = chicks_rescaled)
summary(SS_model)

# Asym ≈ A --> 1610
# xmid ≈ t0 --> 1.07
# scal ≈ 1/k --> 1/1.001637 = 0.9983657

growth_model <- nlme(mass ~ A / (1 + exp(-k * (week_c - t0))),
                     data = chicks_rescaled,
                     fixed = A + k + t0 ~ 1,
                     random = A + k + t0 ~ 1 | chick_id,
                     start = c(start_fixef["Asym"],
                               1 / start_fixef["scal"],
                               start_fixef["xmid"]),
                     control = ctrl)
summary(growth_model)

fit1 <- nlme(
  mass  ~ SSlogis(week_c, Asym, xmid, scal),
  data   = chicks_rescaled,
  fixed  = Asym + xmid + scal ~ 1,
  random = Asym + xmid + scal ~ 1 | chick_id,
  start  = start_fixef,
  control = ctrl
)



###### summer 2025 code ######
full_model <- nls(mass ~ SSlogis(age, Asym, xmid, scal), data = real_data)

ggplot(data = drop_na(real_data, mass), aes(age, mass)) +
  geom_point() +
  geom_line(aes(y = predict(full_model)))

summary(full_model)[10] # outputs all coefficients

summary(full_model)[[10]][, 1] # outputs coefficient estimates
summary(full_model)[[10]][, 2] # outputs coefficient standard errors

# this doesn't work on a per-chick basis
sapply(unique(real_data$Chick_ID), function(x) {
  model <- nls(mass ~ SSlogis(age, Asym, xmid, scal), data = real_data[real_data$Chick_ID == x, ])
  summary(model)[[10]][, 1]
})

ggplot(real_data[real_data$Chick_ID == "25G01A", ], aes(age, mass)) +
  geom_point()

# library(ggtrendline)
# 
# single_model <- nls(mass ~ SSexp3P(age, a, b, c),
#                     data = real_data[real_data$Chick_ID == "25G01A", ])
# 
# ggplot(data = real_data[real_data$Chick_ID == "25G01A", ], aes(age, mass)) +
#   geom_point() +
#   geom_line(aes(y = predict(single_model)))
# 
# summary(single_model)

A_model <- nls(mass ~ SSlogis(age, Asym, xmid, scal), data = real_data[real_data$Chickrank == "A", ])
B_model <- nls(mass ~ SSlogis(age, Asym, xmid, scal), data = real_data[real_data$Chickrank == "B", ])
C_model <- nls(mass ~ SSlogis(age, Asym, xmid, scal), data = real_data[real_data$Chickrank == "C", ])

summary(A_model)

ggplot() +
  geom_point(data = drop_na(real_data, mass),
             aes(age, mass, col = Chickrank)) +
  scale_color_manual(values = c("#F8766D", "#619CFF", "black")) +
  geom_line(data = drop_na(real_data[real_data$Chickrank == "A", ], mass),
            aes(x = age, y = predict(A_model)),
            col = "#F8766D") +
  geom_line(data = drop_na(real_data[real_data$Chickrank == "B", ], mass),
            aes(x = age, y = predict(B_model)),
            col = "#619CFF") +
  geom_line(data = drop_na(real_data[real_data$Chickrank == "C", ], mass),
            aes(x = age, y = predict(C_model)),
            col = "black")

log_params <- bind_cols(Chickrank = c("A", "B", "C"),
                        bind_rows(summary(A_model)[[10]][, 1],
                                  summary(B_model)[[10]][, 1],
                                  summary(C_model)[[10]][, 1]))

log_errors <- bind_cols(Chickrank = c("A", "B", "C"),
                        bind_rows(summary(A_model)[[10]][, 2],
                                  summary(B_model)[[10]][, 2],
                                  summary(C_model)[[10]][, 2])) %>%
  rename(Asym_SE = Asym,
         xmid_SE = xmid,
         scal_SE = scal)

log_params <- bind_cols(log_params, select(log_errors, 2:4))

ggplot(log_params, aes(Chickrank, Asym)) +
  geom_col() +
  geom_errorbar(aes(ymin = Asym - Asym_SE, ymax = Asym + Asym_SE))
