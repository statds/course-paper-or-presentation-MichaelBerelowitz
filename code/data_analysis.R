games <- read.csv(file = "C:/Users/mikeb/OneDrive/Desktop/STAT 3494W/game_logs.csv", header = TRUE)
games <- games[which(games$date > 20120101),]

# new variable stating whether or not the home team won
games$home_team_won[games$h_score > games$v_score] <- "Yes"
games$home_team_won[games$h_score < games$v_score] <- "No"

# new variable with difference between home team and away team score
games$run_difference <- games$h_score - games$v_score

# table showing frequencies of which home vs. away team won
home_team_won <- transform(as.data.frame(table(games$home_team_won)), proportion = Freq/nrow(games))
names(home_team_won)[names(home_team_won) == "Var1"] <- "home_team_won"

mean(games$h_score)
mean(games$v_score)
mean(games$run_difference)
sd(games$h_score)
sd(games$v_score)
sd(games$run_difference)

# Histograms for runs scored in the 2012-2016 seasons
par(mfrow=c(1,2))
hist(games$h_score, col="blue", xlab = "Runs Scored", main="Home Team Runs Scored")
hist(games$v_score, col="red", xlab = "Runs Scored", main="Visiting Team Runs Scored")

# Box plot for runs scored in the 2012-2016 seasons
boxplot(games$h_score, col="blue", ylab="Runs Scored", main="Home Team Runs Scored")
boxplot(games$v_score, col="red", ylab="Runs Scored", main="Visiting Team Runs Scored")


## 2020 data
covid_games <- read.csv(file = "C:/Users/mikeb/OneDrive/Desktop/STAT 3494W/GL2020.csv", header = FALSE)
names(covid_games)[names(covid_games) == "V10"] <- "v_score"
names(covid_games)[names(covid_games) == "V11"] <- "h_score"

# # new variable stating whether or not the home team won for the 2020 data
covid_games$home_team_won[covid_games$h_score > covid_games$v_score] <- "Yes"
covid_games$home_team_won[covid_games$h_score < covid_games$v_score] <- "No"

# new variable with difference between home team and away team score
covid_games$run_difference <- covid_games$h_score - covid_games$v_score

# table showing frequencies of which home vs. away team won for the 2020 data
covid_home_team_won <- transform(as.data.frame(table(covid_games$home_team_won)), proportion = Freq/nrow(covid_games))
names(covid_home_team_won)[names(covid_home_team_won) == "Var1"] <- "home_team_won"

mean(covid_games$h_score)
mean(covid_games$v_score)
mean(covid_games$run_difference)
sd(covid_games$h_score)
sd(covid_games$v_score)
sd(covid_games$run_difference)

# Histograms for runs scored in the 2020 season
hist(covid_games$h_score, col="blue", xlab = "Runs Scored", main="2020 Home Team Runs Scored")
hist(covid_games$v_score, col="red", xlab = "Runs Scored", main="2020 Visiting Team Runs Scored")

# Box plot fot runs scored in the 2020 season
boxplot(covid_games$h_score, col="blue", ylab="Runs Scored", main="2020 Home Team Runs Scored")
boxplot(covid_games$v_score, col="red", ylab="Runs Scored", main="2020 Visiting Team Runs Scored")



#### Tests

### Tests for 2012-2016 data

## t test for 2012-2016 data to test if home teams average more runs than away teams
# first test for equal variances
var.test(games$h_score, games$v_score) # p-value of 3.358e-05, so variances not equal

tTest <- t.test(games$h_score, games$v_score, alternative = "greater", equal.variances = FALSE)
# p-value of 0.001201, so there is evidence that home teams score more runs


## z test for 2012-2016 data to test if home teams win more than 50% of games
propTest <- prop.test(home_team_won$Freq[home_team_won$home_team_won == "Yes"], nrow(games), p = 0.5,
                      alternative = "greater")
# p-value of 1.55e-14, so there is evidence that home teams win majority of games



### Tests for 2020 data


## t test for 2020 data to test if home teams average more runs than away teams without fans in the crowd
# first test for equal variances
var.test(covid_games$h_score, covid_games$v_score) # p-value of 0.1188, no evidence against equal variances

covid_tTest <- t.test(covid_games$h_score, covid_games$v_score, alternative = "greater", equal.variances = TRUE)
# p-value of 0.1047, so no evidence that home teams score more than away teams without fans in the crowd

## z test for 2020 data to test if home teams win more then 50% of games without fans in the crowd
covid_propTest <- prop.test(covid_home_team_won$Freq[covid_home_team_won$home_team_won == "Yes"],
                            nrow(covid_games), p = 0.5, alternative = "greater")
# p-value of 0.001196, so there is evidence that home teams win majority of games without fans in the crowd



### Tests for comparison


## t test to see if the difference between home and away team scores is higher when there are fans in the crowd
# first test for equal variances
var.test(games$run_difference, covid_games$run_difference) # p-value of 8.653e-07, so variances not equal

rd_tTest <- t.test(games$run_difference, covid_games$run_difference, alternative = "greater", equal.variances = FALSE)
rd_tTest2 <- t.test(games$run_difference, covid_games$run_difference, equal.variances = FALSE)
# p-value of .6825, so no evidence of higher home run difference with fans in the crowd

## two-proportion z test to test if home teams win a higher percentage of games when there are fans
two_propTest <- prop.test(c(home_team_won$Freq[home_team_won$home_team_won == "Yes"],
                            covid_home_team_won$Freq[covid_home_team_won$home_team_won == "Yes"]),
                          c(nrow(games), nrow(covid_games)), alternative = "greater")

two_propTest2 <- prop.test(c(home_team_won$Freq[home_team_won$home_team_won == "Yes"],
                            covid_home_team_won$Freq[covid_home_team_won$home_team_won == "Yes"]),
                          c(nrow(games), nrow(covid_games)))
