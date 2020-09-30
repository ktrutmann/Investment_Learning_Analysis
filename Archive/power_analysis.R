library(tidyverse)

# TODO: [ ] Power analysis for a "trend effect" over the different conditions.

# Setup -------------------------------------------------------------------
n_sims_per_combination <- 5000
effect_sizes <- c(.1, .2, .3, .4, .5)
sample_sizes <- seq(20, 150, by = 5)

n_effect_sizes <- length(effect_sizes)
n_sample_sizes <- length(sample_sizes)

# Set up the data structures to save the results in
p_vals <- array(NA, dim = c(length(sample_sizes), length(effect_sizes), n_sims_per_combination))


# Main Loops --------------------------------------------------------------
for (i_sample_size in 1:n_sample_sizes){
    this_sample_size <- sample_sizes[i_sample_size]

    for (i_effect_size in 1:n_effect_sizes){
        this_effect_size <- effect_sizes[i_effect_size]

        for (i_sim in 1:n_sims_per_combination){
            sample_1 <- rnorm(this_sample_size)
            sample_2 <- rnorm(this_sample_size, mean = this_effect_size)

            # The core test
            p_vals[i_sample_size, i_effect_size, i_sim] <- t.test(sample_1, sample_2)$p.value

        } # sim loop
        print(str_c('Effect size ', this_effect_size, ' done.'))
    } # effect size loop
    print(str_c('Sample size ', this_sample_size, ' done.'))
} # sample size loop


# Plotting ----------------------------------------------------------------
is_sig <- p_vals < .05
prop_sig <- apply(is_sig, c(1, 2), mean)
dimnames(prop_sig) <- list(as.character(sample_sizes), as.character(effect_sizes))

sig_long <- as_tibble(prop_sig) %>%
    add_column(sample_size = sample_sizes) %>%
    gather(key = 'effect_size', value = 'Power', -sample_size)

ggplot(sig_long, aes(x = sample_size, y = Power, color = effect_size)) +
    geom_line() +
    labs(x = 'n per Group', color = 'Effect Size') +
    theme_minimal()


# "Interaction" between block and main condition ---------------------------

t.test(DE_diff ~ main_condition, data = filter(complete_table, main_condition != 'states_shown'))

power.t.test(delta = 0.1518852, sd=sd(complete_table$DE_diff), power = .85)
