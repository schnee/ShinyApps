library(dplyr)
library(tidyr)
library(Lahman)

# Grab career batting average of non-pitchers
# (allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)

# Add player names
career <- Master %>%
  tbl_df() %>%
  select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

# Estimate hyperparameters alpha0 and beta0 for empirical Bayes
career_filtered <- career %>% filter(AB >= 500)
m <- MASS::fitdistr(career_filtered$average, dbeta,
                    start = list(shape1 = 1, shape2 = 10))

alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

# For each player, update the beta prior based on the evidence
# to get posterior parameters alpha1 and beta1
career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0)) %>%
  mutate(alpha1 = H + alpha0,
         beta1 = AB - H + beta0) %>%
  arrange(desc(eb_estimate))

# while we're at it, save them as separate objects too for later:
aaron <- career_eb %>% filter(name == "Hank Aaron")
piazza <- career_eb %>% filter(name == "Mike Piazza")
two_players <- bind_rows(aaron, piazza)

two_players

min_ave <- min(two_players$average)
max_ave <- max(two_players$average)

library(broom)
library(ggplot2)
theme_set(theme_bw())

two_players %>%
  inflate(x = seq(0.9*min_ave, 1.1*max_ave, .00025)) %>%
  mutate(density = dbeta(x, alpha1, beta1)) %>%
  ggplot(aes(x, density, color = name)) +
  geom_line() +
  labs(x = "Batting average", color = "")

x <- seq(0.95*min_ave, 1.05*max_ave, .00025)
crossing(piazza_x = x, aaron_x = x) %>%
  mutate(piazza_density = dbeta(piazza_x, piazza$alpha1, piazza$beta1),
         aaron_density = dbeta(aaron_x, aaron$alpha1, aaron$beta1),
         joint = piazza_density * aaron_density) %>%
  ggplot(aes(piazza_x, aaron_x, fill = joint)) +
  geom_tile() +
  geom_abline() +
  scale_fill_gradient2(low = "white", high = "red") +
  labs(x = "Piazza batting average",
       y = "Aaron batting average",
       fill = "Joint density") +
  theme(legend.position = "none")
