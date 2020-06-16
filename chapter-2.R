## EXERCISES FOR CHAPTER 2 OF BAYESIAN DATA ANALYSIS

library(brms)
library(magrittr)
library(ggplot2)

# 2.1 ---------------------------------------------------------------------

data2.1 <- tibble::tibble(
  y = 3,
  n = 10,
  censored = "left"
)

model2.1 <- brm(y | trials(n) + cens(censored) ~ 1,
                family = binomial(),
                prior = c(set_prior("beta(4,4)", class = "Intercept")),
                data = data2.1)

results2.1 <- tibble::tibble(
  prior = rbeta(4000, 4, 4),
  posterior = posterior_samples(model2.1, "Intercept") %>%
    extract2(1)
)

results2.1 %>%
  tidyr::pivot_longer(cols = c("prior", "posterior")) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = 0.4)

# 2.2 ---------------------------------------------------------------------

n <- 1000

data2.2 <- tibble::tibble(
  y = 0,
  n = 2
)

model2.2 <- brm(y | trials(n) ~ 1,
                family = binomial(),
                data = data2.2)

posterior_samples(model2.2, "Intercept") %>%
  extract2(1) %>% hist()

pheads <- list(
  coin1 = 0.6,
  coin2 = 0.4
)

coin_draws <- purrr::map(pheads, ~ rbinom(n, 2, .))
p2tails <- purrr::map_dbl(coin_draws, ~ sum(. == 0)/1000)
p2tails <- p2tails / sum(p2tails)
pnext_head <- weighted.mean(unlist(pheads), p2tails)

(expected_additional_flips <- 1 / pnext_head)

# 2.3 ---------------------------------------------------------------------

n <- 1000

outcomes2.3 <- purrr::map(1:n, ~ sample(1:6, n, replace = T)) %>%
  purrr::map_dbl(~ sum(. == 6)/n)

tibble::tibble(x = outcomes2.3) %>%
  ggplot(aes(x)) + geom_density()

(quantile(outcomes2.3, c(0.05, 0.25, 0.5, 0.75, 0.95)))

# 2.4 ---------------------------------------------------------------------

n <- 1000

theta <- list(
  p1_12 = 1/12,
  p1_6 = 1/6,
  p1_4 = 1/4
)

prior_probs <- c(0.25, 0.5, 0.25)

sample_theta <- function(this_theta) {
  purrr::map(1:n, ~ sample(1:6, n,
                           replace = T,
                           prob = c(rep((1-this_theta)/5, 5), this_theta))) %>%
    purrr::map_dbl(~ sum(. == 6)/n)
}

outcomes2.4 <- theta %>%
  purrr::map(sample_theta) %>%
  unlist() %>%
  sample(n, replace = TRUE, prob = rep(prior_probs, each = n))

tibble::tibble(x = outcomes2.4) %>%
  ggplot(aes(x)) + geom_density()

(quantile(outcomes2.4, c(0.05, 0.25, 0.5, 0.75, 0.95)))

# 2.8 ---------------------------------------------------------------------


