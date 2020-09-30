library(ggplot)
library(plotly)

theme_set(theme_minimal())

# All generated price paths ---------------------------------------
{ggplot(data = filter(dat_main_long, !is.na(main_condition)),
         aes(x = round_number, y = price, group = participant_code, color = main_condition)) +
      geom_line(alpha = .5) +
      labs(x = 'Round Number', color = 'Condition') +
      scale_color_manual(labels = c('Baseline', 'Probabilities Shown', 'States Shown'),
                         values = c('red', 'blue', 'green'))} %>%
ggplotly()


# Exploring beliefs of individual participants ---------------------------
{vpn <- 5
	filter(dat_main_long,
		participant_code == unique(dat_main_long$participant_code)[vpn]) %>%
	  dplyr::select(round_number, rl_belief, bayes_prob_up, belief) %>%
	  mutate(belief = belief / 100) %>%
	  pivot_longer(-round_number) %>%
	ggplot(
	    aes(x = round_number, y = value, color = name)) +
	    geom_line()} %>%
	    ggplotly()


# Example plots of participant behavior ---------------------------------------
this_block <- 1
vpn <- 1

# TODO: Check whether short/long colors are correct!!!
for (vpn in seq_len(length(unique(dat_main_long$participant_code)))) {

  this_dat <- filter(dat_main_long, i_block == this_block - 1,
                     participant_code == unique(participant_code)[vpn]) %>%
    mutate(hold_laged = factor(lead(hold)))
  this_de <- filter(complete_table,
  	participant_code == this_dat$participant_code[1]) %>%
    dplyr::select(str_c('DE_', this_block))

  p1 <- ggplot(this_dat,
               aes(i_round_in_block, price, color = hold_laged, group = 1)) +
        geom_line(size = 1) +
        labs(y = 'Price', color = 'Invested') +
        scale_color_manual(labels = c('Short', 'None', 'Long'),
                           values = c('green', 'red4', 'cyan4')) +
        xlab('Period') +
        ggtitle(str_c('Subject ', vpn, ', Block ', this_block),
                subtitle = str_c('DE = ', round(this_de, 3),
                ' (Subject Code: ', this_dat$participant_code[1], ')'))

  gathered_beliefs <- this_dat %>%
      mutate(reported_belief = belief / 100) %>%
      dplyr::select(i_round_in_block, bayes_prob_up, reported_belief, state) %>%
      gather(key = Path, value = Belief, -i_round_in_block, -state)

  p2 <- ggplot(gathered_beliefs,
               aes(i_round_in_block, Belief, color = Path)) +
        geom_rect(inherit.aes = FALSE,
          aes(xmin = i_round_in_block - 1, xmax = i_round_in_block,
          	ymin = 0, ymax = 1,
              fill = factor(state)), alpha = .15) +
        geom_hline(yintercept = .78, color = 'red4') +
        geom_line(size = .8) +
        scale_color_manual(labels = c('Bayesian', 'Data'), values = c('cyan3', 'blue4')) +
        scale_fill_manual(labels = c('Bad', 'Good'), values = c('red4', 'blue4')) +
        labs(x = 'Period', fill = 'State', color = 'Belief Path')

  save_kevplot(str_c('//Participant_Actions//Subj_',
                     unique(dat_main_long$participant_code)[vpn],
                     '_block_', this_block,
                     '_', this_dat$condition[5]))
  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = 'last'))
  dev.off()
}


# Price moves after trading decisions ---------------------------------------

# Find the indexes where the moves happened and make sure there's enough remaining periods
max_shift <- 5

target_moves <- with(dat_main_long,
  which(abs(transaction) == 1))

moves_after_decision <- tibble(
  original_move_ix = rep(target_moves, each = max_shift + 1),
  move_id = rep(seq_along(target_moves), each = max_shift + 1),
  target_shift = rep(0:max_shift, length(target_moves)))  %>%
  mutate(move_ix = original_move_ix + target_shift)

full_shift_df <- dat_main_long %>%
  dplyr::select(hold, transaction, price,
    bayes_prob_up, position) %>%
  dplyr::slice(moves_after_decision$move_ix) %>%
  bind_cols(moves_after_decision) %>%
  group_by(move_id) %>%  # Normalizing price
  mutate(price_normal = price - price[target_shift == 0],
    original_hold = hold[target_shift == 0],
    original_move = factor(transaction[target_shift == 0]),
    price_normal = case_when(
      original_hold == 0 & original_move == -1 ~ - price_normal,
      original_hold == -1 ~ - price_normal,
      TRUE ~ price_normal),
    original_position = factor(position[target_shift == 0]),
    n_blocks = length(unique(i_block))) %>%  # Removing block overlaps
  ungroup() %>%
  filter(n_blocks == 1,
    original_position != 'No Returns')

dat_prepared <- full_shift_df %>%
  group_by(original_position, target_shift) %>%
  summarize(avg_price = mean(price_normal),
    se = sd(price_normal) / sqrt(n()),
    ci_upper = avg_price + qt(.9, n()) * se,
    ci_lower = avg_price - qt(.9, n()) * se,
    n = n())

ggplot(dat_prepared,
  aes(x = target_shift, y = avg_price, color = original_position,
      group = original_position)) +
  geom_line(size = 1.25) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(), width = .05) +
  scale_color_manual(values = c('Black', 'skyblue3', 'cyan2'),
                    labels = c('Buy', 'Sell Gain', 'Sell Loss')) +
  labs(x = 'Periods since Transaction', y = 'Normalised Mean Price',
       color = 'Transaction', group = 'Transaction')
