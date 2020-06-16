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

m3.2 <- brm(bf(y | trials(size) ~ survey, a ~ bush),
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
