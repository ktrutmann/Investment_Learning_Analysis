library(tidyverse)

FILE_LOCATION <- '../Experiment/Investment_Task/price_paths/'
N_PERIODS <- 501

# Load Paths --------------------------------------------------------------
all_files <- list.files(FILE_LOCATION) %>% `[`(str_detect(., 'price_path'))
n_files <- length(all_files)

price_matrix <- matrix(NA, nrow = n_files, ncol = N_PERIODS)
state_matrix <- matrix(NA, nrow = n_files, ncol = N_PERIODS - 1)

for (i in 1:n_files){
    dat <- read_csv(str_c(FILE_LOCATION, all_files[i]))
    price_matrix[i, ] <- dat$price
    state_matrix[i, ] <- dat$good_state[1:(N_PERIODS - 1)]
}

# Check Numbers -----------------------------------------------------------
dif_matrix <- t(apply(price_matrix, 1, diff))

# In the good state, there should be .65 increases:
mean(dif_matrix[state_matrix] > 0)

# In the bad state it should be .35:
mean(dif_matrix[!state_matrix] > 0)

# Does a good state follow a good state? Should be .8
mean(apply(state_matrix, 1, diff) == 0)