## EXERCISES FOR CHAPTER 3 OF BAYESIAN DATA ANALYSIS

library(brms)
library(magrittr)
library(ggplot2)

# 3.2 ---------------------------------------------------------------------

poll_data <- tibble::tribble(
  ~ survey, ~ bush, ~ dukakis, ~ other,
  "pre-debate", 294, 307, 38,
  "post-debate", 288, 332, 19
) %>%
  dplyr::mutate(size = bush + dukakis + other)

poll_data$y <- with(poll_data, cbind(bush, dukakis, other))

# Note that I didn't specify any prior so might be using whatever defaults brms
# has. I checked and there are weakly informative student t priors. Note that
# for practical effects using a predictor here is equivalent to fitting two
# independent multinomials?
m3.2 <- brm(bf(y | trials(size) ~ survey),
            data = poll_data,
            family = multinomial(),
            cores = 4)

# This is how I would have done it but it includes support for "other" not
# wether there's been a shift from one to another
posterior_predict(m3.2) %>%
  tibble::as.tibble() %>%
  dplyr::mutate(bush_difference = `1.bush` - `2.bush`) %>%
  ggplot(aes(bush_difference)) +
  geom_density() +
  geom_vline(xintercept = 0)

# I think this is what the exercise asks
probabilities_bush <- posterior_samples(m3.2) %>%
  tibble::as.tibble() %>%
  dplyr::mutate(odds_dukakis_pre = exp(b_mudukakis_Intercept +
                                         b_mudukakis_surveypreMdebate),
                odds_dukakis_post = exp(b_mudukakis_Intercept),
                odds_other_pre = exp(b_muother_Intercept +
                                       b_muother_surveypreMdebate),
                odds_other_post = exp(b_muother_Intercept),
                total_odds_pre = 1 + odds_dukakis_pre + odds_other_pre,
                total_odds_post = 1 + odds_dukakis_post + odds_dukakis_post,
                p_dukakis_pre = odds_dukakis_pre / total_odds_pre,
                p_dukakis_post = odds_dukakis_post / total_odds_post,
                p_bush_pre = 1 / total_odds_pre,
                p_bush_post = 1 / total_odds_post,
                a1 = p_bush_pre / (p_bush_pre + p_dukakis_pre),
                a2 = p_bush_post / (p_bush_post + p_dukakis_post))

probabilities_bush %>%
  ggplot(aes(x = a2 - a1))  +
  geom_density() +
  geom_vline(xintercept = 0)

# The probability that there has beeen a shift towards bush is:
probabilities_bush %$% {
  difference <- a2 - a1
  sum(difference > 0) / length(difference)
}

# 3.3 ---------------------------------------------------------------------

chikens <- tibble::tribble(
  ~group, ~ n, ~mean_flow, ~ sd_flow,
  "control", 32, 1.013, 0.24,
  "exposed", 36, 1.173, 0.20
) %>%
  dplyr::mutate(standard_error = (sd_flow ^ 2) / sqrt(n))

# Wondering whether doing this properly would imply making sigma also depend on
# the group? I tried that but computer says no. Also, I have no idea how to
# specify an uniform prior for the logarithm of sigma. Presumably something
# specifying sigmalink = "log" and then some bounds?
m3.3 <- brm(bf(mean_flow | se(standard_error) ~ group),
            prior = c(set_prior("uniform(0, 2)", class = "Intercept")),
            data = chikens,
            cores = 4)

# A) Posterior distribution of mu_0
posterior_samples(m3.3) %>%
  tibble::as_tibble() %>%
  ggplot(aes(x = b_Intercept)) +
  geom_density()

# B) Posterior distribution of mu_t - mu_0
posterior_samples(m3.3) %>%
  tibble::as_tibble() %>%
  ggplot(aes(x = b_groupexposed)) +
  geom_density()

# 3.4 ---------------------------------------------------------------------

cardiac_mortality <- tibble::tribble(
  ~group, ~n, ~n_died,
  "control", 374, 39,
  "treatment", 680, 22
)


# A) Non informative priors
p3.4 <- c(prior(student_t(1,0,10), class = "Intercept"),
          prior(student_t(1,0,10), class = "b"))

m3.4 <- brm(n_died | trials(n) ~ group,
            family = binomial(),
            # Very non-informative priors
            prior = p3.4,
            data = cardiac_mortality,
            cores = 4)

posterior3.4 <- posterior_samples(m3.4) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(p0 = plogis(b_Intercept),
                p1 = plogis(b_Intercept + b_grouptreatment))

posterior3.4 %>%
  ggplot() +
  geom_density(aes(x = p0, colour = "control")) +
  geom_density(aes(x = p1, colour = "treatment"))

# B) Posterior of odds ratio
posterior3.4 %>%
  ggplot() +
  geom_density(aes(x = exp(b_grouptreatment)))

# 3.5 ---------------------------------------------------------------------

# This requires stan and I'm running out of time...
