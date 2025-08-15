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
