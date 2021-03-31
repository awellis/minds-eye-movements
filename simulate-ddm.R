library(tidyverse)
library(kogpsy)
library(rtdists)


## sim subjects ----



sub_n  <- 80 # number of subjects in this simulation
sub_sd <- 0.1 # in (ms) SD for the subjects' random intercept
beta_mean <- mean(rbeta(sub_n, 0.6, 2)) # to standardize the recurrence values around 0
beta_sd <- sd( rbeta(sub_n, 0.6, 2)) # to standardize the recurrence values around 0


sub <- tibble(
    sub_id = 1:sub_n,
    sub_intercept  = rnorm(sub_n, 0, sub_sd), # random intercept
    sub_rec = rbeta(sub_n, 0.6, 2),  # between-subjects factor, z-standardized
    rec_cond_interaction_effect = rnorm(sub_n, mean = 0.1, sd = 1) # this is per person fixed
)


sub



stim_n  <- 40 # number of stimuli
stim_sd <- 0.1 # SD for the random intercept


stim <- tibble(
    stim_id = 1:stim_n,
    stim_intercept = rnorm(stim_n, 0, stim_sd)
)


trials <- crossing(
    sub_id = sub$sub_id,
    stim_id = stim$stim_id,
    stim_version = c("congruent", "incongruent")) %>%
    left_join(sub, by = "sub_id") %>%
    left_join(stim, by = "stim_id")




grand_intercept <- 0 # overall mean DV
stim_version_eff <- 0.1 # mean difference between versions: incongruent - congruent
error_sd         <- 0.05  # residual (error) SD
rec_effect <- rnorm(1, mean = 5, sd = 0.1)




d <- trials %>%
    mutate(
        # effect-code subject condition and stimulus version
        # sub_cond.e = recode(sub_cond, "hard" = -0.5, "easy" = +0.5),
        stim_version_e = recode(stim_version,
                                "congruent" = -0.5,
                                "incongruent" = +0.5),
        err = rnorm(nrow(.), 0, error_sd),
        driftrate = grand_intercept + sub_intercept + stim_intercept + err + # intercepts and error
            (stim_version_e * stim_version_eff) +  #effect of the congruence
            (sub_rec * rec_effect) +  # effect of recurrence (äuä 0)
            (rec_cond_interaction_effect * sub_rec * stim_version_e), # interaction btw rec und congruence

        dv = rtdists::rdiffusion(nrow(.), a = 1, v = driftrate, t0 = 0.5)
    )


d <- d %>%
    mutate(rt = pull(dv, rt), response = pull(dv, response)) %>%
    select(-dv)


# d %>%
#   ggplot(aes(rt, fill = response)) +
#   geom_histogram() +
#   facet_grid(sub_id ~ stim_version)



d %>%
    ggplot(aes(x = rt, y = ..density.., color = stim_version)) +
    geom_density()


## brms ----


library(brms)


dd <- d %>%
    select(sub_id, stim_id, condition = stim_version, rec = sub_rec, rt, response)


priors <- get_prior(rt | dec(response) ~ 1 + condition*rec +
                        (1 + condition*rec | sub_id),
                    family = wiener(),
                    data = dd)


priors <- prior(normal(0, 1), class = b)


m1 <- brm(rt | dec(response) ~ 1 + condition*rec +
              (1 + condition*rec | sub_id),
          family = wiener(),
          prior = priors,
          sample_prior = "only",
          cores = 3,
          chains = 3,
          data = dd)






















