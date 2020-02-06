#### Count ####
poss_in <- 50

num_fg_flipped <- 2

#### ORTG ####
ortg_in <- 100

ppp_in <- ortg_in/100 

points_in <- poss_in * ppp_in

ortg_miss_2 <- 100 * (points_in - 2 * num_fg_flipped)/(poss_in + num_fg_flipped)

ortg_make_2 <- 100 * (points_in + 2 * num_fg_flipped)/(poss_in + num_fg_flipped)

# ortg_lwr_3 <- (points_in - 3 * num_fg_flipped)/poss_in
# 
# ortg_upr_3 <- (points_in + 3 * num_fg_flipped)/poss_in

#### DRTG ####
drtg_in <- 110

ppp_in <- drtg_in/100 

points_in <- poss_in * ppp_in

drtg_make_2 <- 100 * (points_in + 2 * num_fg_flipped)/(poss_in + num_fg_flipped)

drtg_miss_2 <- 100 * (points_in - 2 * num_fg_flipped)/(poss_in + num_fg_flipped)

# drtg_lwr_3 <- (points_in + 3 * num_fg_flipped)/poss_in
# 
# drtg_upr_3 <- (points_in - 3 * num_fg_flipped)/poss_in

#### NetRTG ####
netrtg_in <- ortg_in - drtg_in

netrtg_lwr_2 <- round(ortg_miss_2 - drtg_make_2, 1)

netrtg_upr_2 <- round(ortg_make_2 - drtg_miss_2, 1) 
# 
# netrtg_lwr_3 <- ortg_lwr_3 - drtg_lwr_3
# 
# netrtg_upr_3 <- ortg_upr_3 - drtg_upr_3

print(paste0("Flip ", num_fg_flipped, " FG2: ", 
             netrtg_lwr_2, " | ",
             netrtg_in, " | ",
             netrtg_upr_2))

# 
# print(paste0("Flip ", num_fg_flipped, " FG3:", 
#              netrtg_lwr_3, "-",
#              netrtg_in, "-",
#              netrtg_upr_3))



