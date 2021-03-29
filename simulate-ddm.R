library(tidyverse)
library(kogpsy)
library(rtdists)

out <- drift_diffusion(bias = 0.3, driftrate = 0.8)
get_response_rt(out)

## start sim 1 ----
n <- 1e3
d <- rdiffusion(n, a = 1, v = 0.45, t0 = 0.3, s = 0.1)


my_intercept = 0
my_b_congruence = 0.01
my_congruence = 1
my_b_rec = 0
my_rec = 0.3 # this will also change between participants according to my own data
my_b_interaction = -0.5 # this will be the thing that we change between participants. Also this should vary a bit within participants, so generate trial-noise

my_dr <- my_intercept + my_b_congruence*my_congruence + my_b_rec* my_rec + my_b_interaction * my_congruence * my_rec

d <- rdiffusion(n, a = 1, v = my_dr, t0 = 0.3, s = 0.1)

d %>% ggplot(aes(rt)) + geom_histogram() + facet_wrap(~response)

## start sim 2 ----



single_sim(ntrials) {
    # n_trials <- 20

    #this varies a bit within participants, but the means stay the same, because we assume that the effects are the same for all
    my_intercept = rnorm(n = 1, mean = 0, sd = 0.1)
    my_b_congruence = rnorm(n = 1, mean = 0.01, sd = 0.01)
    my_congruence = 1
    my_b_rec = rnorm(n = 1, mean = 0, sd = 0.01)

    true_dr <-  my_intercept + my_b_congruence*my_congruence +
        my_b_rec* my_rec + my_b_interaction * my_congruence * my_rec
    error_sd = 0.1

    # these are the differences between participants
    my_rec = rnorm(n = 1, 0.3, sd = 0.02) # this will  change between participants according to my own data
    my_b_interaction = -0.2 # this will be the thing that we change between participants. Also this should vary a bit within participants, so generate trial-noise



    mysim <- map_df(1:ntrials, ~rdiffusion(n, a = 1, v = my_dr, t0 = 0.3, s = 0.1))







    # set up the 20 trials
    rts <- rep(NA, ntrials)
    for (i in 1:ntrials) {
        my_dr <- rnorm(mean = true_dr, sd = error, n = 1)
        tmpout <- my_drift_diffusion(bias = 0.3, driftrate = my_dr, decision_boundary = 1)
        rts[i] <- tail(tmpout$time, 1)
    }
}

mysim <- map_df(1:1000, ~ind_sim(50, 50, 10, 11, 2.5, 2.5))





## sim 3 ----
curr_true_rec <- rbeta(1, 0.6, 2)

# interaction: sample based on djikstra: half of participants show no influence, twice as many show facilitation as show inhibition. a normal distribution with a slightly positive mean will do.
curr_b_interaction = rnorm(1, mean = 0.2, sd = 0.3)

#hist(rnorm(1000, mean = 0.2, sd = 0.3))

for (currTrial in 1:n_trials) {

    #include the trial-by-trial variance
    #this varies a bit within participants, but the means stay the same, because we assume that the effects are the same for all
    my_intercept = rnorm(n = 1, mean = 0, sd = 0.1)

    my_b_congruence = rnorm(n = 1, mean = 0.1, sd = 0.01) # sligtly positive because there is an overall trend

    my_b_rec = rnorm(n = 1, mean = 0, sd = 0.01) # we don't expect a general effect

    if (currTrial <= 10) { #because in half of the trials there will be a congruent image
        my_congruence = 1 }
    else {
        my_congruence = 0}

    # set the true drift rate
    true_dr <-  my_intercept +
        my_b_congruence * my_congruence +
        my_b_rec* curr_true_rec +
        curr_b_interaction * my_congruence * curr_true_rec

    #error <- abs(true_dr*0.01)
    my_dr <- rnorm(mean = true_dr, sd = 0.01, n = 1)

    currOut <- my_drift_diffusion(bias = 0.05, driftrate = my_dr, decision_boundary = 1) # rtdists rdiffusion

    currAnswer <- ifelse(tail(currOut$dv,1) > 0, 1, -1)
    currRT <- tail(currOut$time, 1)

    result_file$congruence[line_counter] <- my_congruence
    result_file$rec[line_counter] <- curr_true_rec
    result_file$true_dr[line_counter] <- true_dr
    result_file$rt[line_counter] <-  abs(currRT)
    result_file$answer[line_counter] <- currAnswer
    result_file$true_interaction_beta[line_counter] <- curr_b_interaction

    line_counter <- line_counter + 1
} # trial loop



## sim subjects ----
sub_n  <- 10 # number of subjects in this simulation
sub_sd <- 0.1 # in (ms) SD for the subjects' random intercept

sub <- tibble(
    sub_id = 1:sub_n,
    sub_intercept  = rnorm(sub_n, 0, sub_sd), # random intercept
    sub_rec = rbeta(nrow(.), 0.6, 2) # between-subjects factor
)

sub


stim_n  <- 10 # number of stimuli in this simulation
stim_sd <- 0.1 # SD for the stimuli's random intercept

stim <- tibble(
    stim_id = 1:stim_n,
    stim_intercept = rnorm(stim_n, 0, stim_sd) # random intercept
)

trials <- crossing(
    sub_id = sub$sub_id, # get subject IDs from the sub data table
    stim_id = stim$stim_id, # get stimulus IDs from the stim data table
    stim_version = c("congruent", "incongruent") # all subjects see both congruent and incongruent versions of all stimuli
) %>%
    left_join(sub, by = "sub_id") %>% # includes the intercept and conditin for each subject
    left_join(stim, by = "stim_id")   # includes the intercept for each stimulus


# set variables to use in calculations below
grand_intercept          <- 0.5 # overall mean DV
# sub_cond_eff     <- 50  # mean difference between conditions: hard - easy
stim_version_eff <- 0.1  # mean difference between versions: incongruent - congruent
cond_version_ixn <-  0  # interaction between version and condition
error_sd         <- 0.05  # residual (error) SD

d <- trials %>%
    mutate(
        # effect-code subject condition and stimulus version
        # sub_cond.e = recode(sub_cond, "hard" = -0.5, "easy" = +0.5),
        stim_version_e = recode(stim_version,
                                "congruent" = -0.5,
                                "incongruent" = +0.5),
        # calculate error term (normally distributed residual with SD set above)
        err = rnorm(nrow(.), 0, error_sd),
        # calculate DV from intercepts, effects, and error
        driftrate = grand_intercept + sub_intercept + stim_intercept + err +
            # (sub_cond.e * sub_cond_eff) +
            (stim_version_e * stim_version_eff),
            # (sub_cond.e * stim_version.e * cond_version_ixn) # in this example, this is always 0 and could be omitted
        dv = rtdists::rdiffusion(nrow(.), a = 1, v = driftrate, t0 = 0.5)
    )

d <- d %>%
    mutate(rt = pull(dv, rt), response = pull(dv, response)) %>%
    select(-dv)

d %>%
    ggplot(aes(rt, fill = response)) +
    geom_histogram() +
    facet_grid(sub_id ~ stim_version)


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
          data = dd)
