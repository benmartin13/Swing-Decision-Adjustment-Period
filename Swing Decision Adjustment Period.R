library(tidyverse)

# Split data into MLB and MiLB
# Turn data from pitch-by-pitch to player profile data with swing decision metrics
# Look at metrics for first 10 days that a player played in MLB


#Split data into MLB and MiLB
mlb <- filter(train, level == "MLB")
milb <- filter(train, level != "MLB")

#Create data frame with each unique batter ID and the metrics you want
mlb %>% dplyr::group_by(batter_id) %>% dplyr::select(batter_id) -> mlb_hitters
mlb_hitters <- unique(mlb_hitters)

milb %>% dplyr::group_by(batter_id) %>% dplyr::select(batter_id) -> milb_hitters
milb_hitters <- unique(milb_hitters)

x <- nrow(mlb_hitters)

mlb_player_data <- data.frame(batter_id = character(x),
                              swing_rate = numeric(x),
                              whiff_rate = numeric(x),
                              z_swing = numeric(x),
                              o_swing = numeric(x),
                              first_date = numeric(x),
                              last_date = numeric(x))
mlb_player_data$batter_id = mlb_hitters$batter_id

## Defining strike zone - plate_side = 0 is middle of plate, need to go (17/2)/12 in each direction (8.5 inches)/12 =  .708 feet
#                       - average height is 1.75 at bottom, 3.42 at top according to baseball prospectus

#Loop to fill the data frame player-by-player
for(i in 1:x){
  
  data <- filter(mlb, batter_id == mlb_hitters$batter_id[i])
  whiff <- round((nrow(filter(data, pitch_call == "StrikeSwinging"))/nrow(filter(data, pitch_call %in% swings))*100),1)
  swing <- round((nrow(filter(data, is_swing == 1))/nrow(data))*100, 1)
  mlb_player_data$whiff_rate[i] <- whiff
  mlb_player_data$swing_rate[i] <- swing
  
  strikes <- filter(data, plate_height <= 3.42 & plate_height >= 1.75 &
                      plate_side <=  8.5/12 & plate_side >= -8.5/12)
  
  balls <- filter(data, plate_height > 3.42 | plate_height < 1.75 |
                    plate_side >  8.5/12 | plate_side < -8.5/12)
  
  z <- round((nrow(filter(strikes, is_swing == 1))/nrow(strikes))*100,1)
  o <- round((nrow(filter(balls, is_swing == 1))/nrow(balls))*100,1)
  
  mlb_player_data$z_swing[i] <- z
  mlb_player_data$o_swing[i] <- o
  
  mlb_player_data$first_date[i] <- as.Date.numeric(min(data$date))
  mlb_player_data$last_date[i] <- as.Date.numeric(max(data$date))
}

#Repeat for MiLB
y <- nrow(milb_hitters)

milb_player_data <- data.frame(batter_id = character(y),
                               swing_rate = numeric(y),
                               whiff_rate = numeric(y),
                               z_swing = numeric(y),
                               o_swing = numeric(y),
                               first_date = numeric(y),
                               last_date = numeric(y))
milb_player_data$batter_id = milb_hitters$batter_id

for(i in 1:y){
  
  data <- filter(milb, batter_id == milb_hitters$batter_id[i])
  whiff <- round((nrow(filter(data, pitch_call == "StrikeSwinging"))/nrow(filter(data, pitch_call %in% swings))*100),1)
  swing <- round((nrow(filter(data, is_swing == 1))/nrow(data))*100, 1)
  milb_player_data$whiff_rate[i] <- whiff
  milb_player_data$swing_rate[i] <- swing
  
  strikes <- filter(data, plate_height <= 3.42 & plate_height >= 1.75 &
                      plate_side <=  8.5/12 & plate_side >= -8.5/12)
  
  balls <- filter(data, plate_height > 3.42 | plate_height < 1.75 |
                    plate_side >  8.5/12 | plate_side < -8.5/12)
  
  z <- round((nrow(filter(strikes, is_swing == 1))/nrow(strikes))*100,1)
  o <- round((nrow(filter(balls, is_swing == 1))/nrow(balls))*100,1)
  
  milb_player_data$z_swing[i] <- z
  milb_player_data$o_swing[i] <- o
  
  milb_player_data$first_date[i] <- as.Date.numeric(min(data$date))
  milb_player_data$last_date[i] <- as.Date.numeric(max(data$date))
}

# create data frame of players that played in minors and majors
combined <- filter(left_join(mlb_player_data, milb_player_data, by = "batter_id"), swing_rate.y != "NA")

#Now re-calculate their major league numbers to only include the first 10 days of their major league time
# This creates a data frame with a player's minor league metrics, 
# and the metrics from their first 10 days in MLB

z <- nrow(combined)

combined %>% dplyr::group_by(batter_id) %>% dplyr::select(batter_id) -> comb_hitters
comb_hitters <- unique(comb_hitters)

comb_player_data <- data.frame(batter_id = character(z),
                               swing_rate_milb = numeric(z),
                               whiff_rate_milb = numeric(z),
                               z_swing_milb = numeric(z),
                               o_swing_milb = numeric(z),
                               first_date_milb = numeric(z),
                               last_date_milb = numeric(z),
                               swing_rate_mlb = numeric(z),
                               whiff_rate_mlb = numeric(z),
                               z_swing_mlb = numeric(z),
                               o_swing_mlb = numeric(z),
                               first_date_mlb = numeric(z),
                               last_date_mlb = numeric(z))

comb_player_data[,1:7] = combined[,c(1, 8:13)]
comb_player_data$first_date_mlb = combined$first_date.x

for(i in 1:z){
  data <- filter(mlb, batter_id == comb_player_data$batter_id[i])
  data <- filter(data,  date <= (as.Date(comb_player_data$first_date_mlb[i])+10))
  whiff <- round((nrow(filter(data, pitch_call == "StrikeSwinging"))/nrow(filter(data, pitch_call %in% swings))*100),1)
  swing <- round((nrow(filter(data, is_swing == 1))/nrow(data))*100, 1)
  comb_player_data$whiff_rate_mlb[i] <- whiff
  comb_player_data$swing_rate_mlb[i] <- swing
  
  strikes <- filter(data, plate_height <= 3.42 & plate_height >= 1.75 &
                      plate_side <=  8.5/12 & plate_side >= -8.5/12)
  
  balls <- filter(data, plate_height > 3.42 | plate_height < 1.75 |
                    plate_side >  8.5/12 | plate_side < -8.5/12)
  
  z <- round((nrow(filter(strikes, is_swing == 1))/nrow(strikes))*100,1)
  o <- round((nrow(filter(balls, is_swing == 1))/nrow(balls))*100,1)
  
  comb_player_data$z_swing_mlb[i] <- z
  comb_player_data$o_swing_mlb[i] <- o
  
  comb_player_data$first_date_mlb[i] <- as.Date(min(data$date))
  comb_player_data$last_date_mlb[i] <- as.Date(max(data$date))
  
}

comb_player_data$last_date_mlb <- as.character(comb_player_data$last_date_mlb)
comb_player_data$first_date_mlb <- as.Date.numeric(comb_player_data$first_date_mlb)
comb_player_data$last_date_mlb <- as.Date.numeric(comb_player_data$last_date_mlb)

#Look at mean values for all metrics in MiLB and MLB
colMeans(na.omit(comb_player_data[,c(2:5, 8:11)]))

#Run T-Tests to see if difference between MLB and MiLB is significant
t.test(comb_player_data$swing_rate_milb, comb_player_data$swing_rate_mlb, paired = T) #p-value .10
t.test(comb_player_data$whiff_rate_milb, comb_player_data$whiff_rate_mlb, paired = T) #p-value 7.2 e-7
t.test(comb_player_data$z_swing_milb, comb_player_data$z_swing_mlb, paired = T)       #p-value .51
t.test(comb_player_data$o_swing_milb, comb_player_data$o_swing_mlb, paired = T)       #p-value .0005

#Look at MLB averages for Whiff and O-Swing for all MLB players
colMeans(mlb_player_data[,c(3,5)])





