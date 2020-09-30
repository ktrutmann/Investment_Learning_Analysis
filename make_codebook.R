#' The goal of this file is to create a list with the same entry names
#' as can be found in the respective dataframes or tables
#' but containing an explanation of said variables (so, a codebook).

cb_dat_main_long <- as.list(names(dat_main_long))
cb_complete_table <- as.list(names(complete_table))

names(cb_dat_main_long) <- names(dat_main_long)
names(cb_complete_table) <- names(complete_table)


# dat_main_long -----------------------------------------------------------
cb_dat_main_long$strategies <- list(random = 'Ich habe die Aktie per Zufallsprinzip gekauft und verkauft.',
                      feeling = 'Ich vertraute meinem Bauchgefühl über die Preisentwicklung.',
                      rational = 'Ich investierte, wenn ich der Überzeugung war, dass es wahrscheinlicher als 50% ist, dass der Preis ansteigen wird.',
                      risk_averse = 'Ich investierte nur, wenn ich sehr sicher war, dass der Preis ansteigen wird.',
                      inertia = 'Ich versuchte die Aktie jeweils möglichst lange zu halten.',
                      DE = 'Ich versuchte die Aktie möglichst nur zu Verkaufen, wenn ich damit Gewinn gemacht habe.',
                      anti_DE = 'Ich versuchte, Verluste möglichst schnell wieder zu verkaufen.')

cb_dat_main_long$hold <- 'Whether the participant held the asset at the START of the period. The decision at time t can thus be inferred from hold[t+1].'

cb_dat_main_long$cash <- 'Cash holdings at the start of the period'

cb_dat_main_long$final_cash <- 'The final cash earnings after the last period and after all assets have been sold. Should be empty except for the last period.'

cb_dat_main_long$returns <- 'The current paper returns of the asset. As soon as the asset is sold, the return is reset to 0.'

cb_dat_main_long$transaction <- '-1 being a sell, 1 being a buy and 0 being doing nothing.'

cb_dat_main_long$base_price <- 'The price for which the asset was bought, if held.'

cb_dat_main_long$time_to_order <- 'How many seconds it took from loading the page to submitting the order?
    unfocused_time_to_order: How many seconds was the participant away from the trading page while it was loaded. Optimally this is always 0.'

cb_dat_main_long$price_move_from_last_corrected <- 'Whether the price move was "positive" in regards
    to an investment. This will therefore be TRUE for a price-decrease if the participant was shorted.'

cb_dat_main_long$position <- '"Gain" meaning that the current price is either higher if invested,
    or lower if shorted, than the price at which the asset was bought. The opposite is true for "Loss".
    "No Returns" means that the participant was invested, but the price reverted back towards the
    buying-price. "Not Invested" means that the participant was not invested in this period.'

cb_dat_main_long$position_end_of_last_period <- 'Will mostly be the same as a laged version of `position`.
    However, if the participant decided to buy in round n, then `position` would be "Not Invested" while this
    variable would show "No Returns", as the price move before the next period now matters.'

cb_dat_main_long$cumulative_moves_while_invested <- 'Records how many times the price has moved
    favorably/unfavorably sice buying/shorting the asset. Going from holding directly to shorting
    or vice versa resets the counter. Note that each number can be reached by more than one "paths".'

cb_dat_main_long$belief_diff_flipped <- 'The difference in beliefs from the last to the next period.
    Further, if the rational update was downward, this update has been "flipped",
    so that all updates should be positive. This preserves "inverse updates" (e.g. more optimism despite
    having seen a downward move) which abs() would also flip.'

cb_dat_main_long$belief_diff_bayes_corrected_flipped <- 'The difference in beliefs from the last to the next period,
    corrected by (i.e. the difference to) the "rational" basian update. Further, if the rational update was downward, this update
    has been "flipped", so that all updates should be positive.'

cb_dat_main_long$belief_diff_rl_corrected_flipped <- 'The difference in beliefs from the last to the next period,
    corrected by (i.e. the difference to) a reinforcement learner with a constant learning rate of .3.
    Further, if the rational update was downward, this update has been "flipped", so that all updates should be positive.'

cb_dat_main_long$updating_type <- 'Whether the participant over- or under- updated,
    or whether they updated in the wrong direction. One of {Over, Under, Wrong}'


# complete_table ----------------------------------------------------------


