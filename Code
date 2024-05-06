---
title: "MLB-Pythagorean-Expectation-and-Playoff-Performance-Analysis"
output: html_document          # Output format is HTML
date: "2023-12-14"             # Date of the report
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)  # Ensures that R code is visible in the output file
{r}

# Load necessary libraries
library(tidyverse)  # Data manipulation and visualization
library(rvest)      # Web scraping
library(ggplot2)    # Creating visualizations
library(ggrepel)    # Better label placements in plots
library(baseballr)  # Baseball statistics analysis
library(broom)      # Tidying model outputs


# Define a function to scrape batting data against teams for a given year
scrape.batting.against <- function(yr){
    url <- paste("https://www.baseball-reference.com/leagues/majors/", yr, "-batting-pitching.shtml", sep="")
    webpage <- read_html(url)
    webpage %>% html_table(fill=TRUE) -> tables
}


# Initialize an empty tibble to store batting against data
batting.against <- tibble()

# Loop through years 2012 to 2023 to scrape data
for(season in 2012:2023) {
  Sys.sleep(runif(1) * 10)  # Random sleep to prevent hammering the server
  scrape.batting.against(season) -> batting.against.one.season
  batting.against.one.season[[1]] %>% head(30) -> batting.against.one.season
  batting.against.one.season %>% 
    mutate(Tm = recode(Tm, "Cleveland Indians" = "Cleveland Guardians", 
                             "Los Angeles Angels of Anaheim" = "Anaheim Angels", 
                             "Los Angeles Angels" = "Anaheim Angels")) -> batting.against.one.season
  batting.against.one.season %>% bind_rows(batting.against) -> batting.against
}


# Rename columns for clarity and consistency, convert to numeric for analysis
batting.against %>% rename(x2B = "2B", x3B = "3B") -> batting.against
batting.against %>% 
  mutate(PA = as.numeric(PA), H = as.numeric(H), x2B = as.numeric(x2B), 
         x3B = as.numeric(x3B), HR = as.numeric(HR), BB = as.numeric(BB), 
         OPS = as.numeric(OPS), IBB = as.numeric(IBB), SO = as.numeric(SO)) -> batting.against
batting.against %>% select(Tm, PA, H, x2B, x3B, HR, BB, OPS, IBB, SO) -> batting.against


# Aggregate data by team, summing up and averaging necessary columns
batting.against %>% 
  group_by(Tm) %>% 
  summarise(PA = sum(PA), H = sum(H), x2B = sum(x2B), x3B = sum(x3B), 
            HR = sum(HR), BB = sum(BB), OPS = mean(OPS), IBB = sum(IBB), 
            SO = sum(SO)) -> batting.against


# Calculate rates per plate appearance and rename OPS for clarity
batting.against %>% 
  mutate(HR.per.pa.against = HR / PA, x1B = H - (x2B + x3B + HR), 
         x1B.per.pa.against = x1B / PA, UBB = BB - IBB, 
         UBB.per.pa.against = UBB / PA, SO.per.pa.for = SO / PA) %>% 
  rename("OPS.against" = "OPS") -> batting.against
batting.against %>% 
  select(Tm, HR.per.pa.against, x1B.per.pa.against, SO.per.pa.for, 
         UBB.per.pa.against, OPS.against) -> batting.against.rates.per.PA


# Scrape batting data for teams, similar steps as above, with modifications for 'batting for'
scrape.batting.for <- function(yr){
    url <- paste("https://www.baseball-reference.com/leagues/majors/", yr, "-standard-batting.shtml", sep="")
    webpage <- read_html(url)
    webpage %>% html_table(fill=TRUE) -> tables
}


batting.for <- tibble()
for(season in 2012:2023) {
  Sys.sleep(runif(1) * 10)
  scrape.batting.for(season) -> batting.for.one.season
  batting.for.one.season[[1]] %>% head(30) -> batting.for.one.season
  batting.for.one.season %>% 
    mutate(Tm = recode(Tm, "Cleveland Indians" = "Cleveland Guardians", 
                             "Los Angeles Angels of Anaheim" = "Anaheim Angels", 
                             "Los Angeles Angels" = "Anaheim Angels")) -> batting.for.one.season
  batting.for.one.season %>% bind_rows(batting.for) -> batting.for
}


# Rename and mutate columns similar to 'batting against'
batting.for %>% rename(x2B = "2B", x3B = "3B") -> batting.for
batting.for %>% 
  mutate(PA = as.numeric(PA), H = as.numeric(H), x2B = as.numeric(x2B), 
         x3B = as.numeric(x3B), HR = as.numeric(HR), BB = as.numeric(BB), 
         OPS = as.numeric(OPS), IBB = as.numeric(IBB), SO = as.numeric(SO)) -> batting.for
batting.for %>% select(Tm, PA, H, x2B, x3B, HR, BB, OPS, IBB, SO) -> batting.for

# Aggregate and calculate rates per PA for 'batting for', renaming OPS
batting.for %>% 
  group_by(Tm) %>% 
  summarise(PA = sum(PA), H = sum(H), x2B = sum(x2B), x3B = sum(x3B), 
            HR = sum(HR), BB = sum(BB), OPS = mean(OPS), IBB = sum(IBB), 
            SO = sum(SO)) -> batting.for

batting.for %>% 
  mutate(HR.per.pa.for = HR / PA, x1B = H - (x2B + x3B + HR), 
         x1B.per.pa.for = x1B / PA, UBB = BB - IBB, 
         UBB.per.pa.for = UBB / PA, SO.per.pa.against = SO / PA) %>% 
  rename("OPS.for" = "OPS") -> batting.for
batting.for %>% 
  select(Tm, HR.per.pa.for, x1B.per.pa.for, SO.per.pa.against, 
         UBB.per.pa.for, OPS.for) -> batting.for.rates.per.PA

# Collect postseason data for each year from 2012 to 2023, storing team performance in playoffs
# Aggregate data, renaming columns and combining yearly data into a comprehensive dataset

mlb_schedule_postseason(2012) -> One

One <- One %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winsone <- One %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winsonetwo <- One %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winsone <- winsone %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winsonetwo <- winsonetwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalOne <- bind_rows(winsone, winsonetwo)

FinalOne <- FinalOne %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentytwelve <- FinalOne %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))


# -- 2013 --

mlb_schedule_postseason(2013) -> Two

Two <- Two %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winstwo <- Two %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winstwotwo <- Two %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winstwo <- winstwo %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winstwotwo <- winstwotwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalTwo <- bind_rows(winstwo, winstwotwo)

FinalTwo <- FinalTwo %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentythirteen <- FinalTwo %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))

#-- 2014 --

mlb_schedule_postseason(2014) -> Three

Three <- Three %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winsthree <- Three %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winsthreetwo <- Three %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winsthree <- winsthree %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winsthreetwo <- winsthreetwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalThree <- bind_rows(winsthree, winsthreetwo)

FinalThree <- FinalThree %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentyfourteen <- FinalThree %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))

#-- 2015 --

mlb_schedule_postseason(2015) -> Four

Four <- Four %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winsfour <- Four %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winsfourtwo <- Four %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winsfour <- winsfour %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winsfourtwo <- winsfourtwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalFour <- bind_rows(winsfour, winsfourtwo)

FinalFour <- FinalFour %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentyfifteen <- FinalFour %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))

# -- 2016 --

mlb_schedule_postseason(2016) -> Five

Five <- Five %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winsfive <- Five %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winsfivetwo <- Five %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winsfive <- winsfive %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winsfivetwo <- winsfivetwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalFive <- bind_rows(winsfive, winsfivetwo)

FinalFive <- FinalFive %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentysixteen <- FinalFive %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))

# -- 2017 --

mlb_schedule_postseason(2017) -> Six

Six <- Six %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winssix <- Six %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winssixtwo <- Six %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winssix <- winssix %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winssixtwo <- winssixtwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalSix <- bind_rows(winssix, winssixtwo)

FinalSix <- FinalSix %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentyseventeen <- FinalSix %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))

# -- 2018 --

mlb_schedule_postseason(2018) -> Seven

Seven <- Seven %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winsseven <- Seven %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winsseventwo <- Seven %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winsseven <- winsseven %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winsseventwo <- winsseventwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalSeven <- bind_rows(winsseven, winsseventwo)

FinalSeven <- FinalSeven %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentyeighteen <- FinalSeven %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))


# -- 2019 --

mlb_schedule_postseason(2019) -> Eight

Eight <- Eight %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winseight <- Eight %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winseighttwo <- Eight %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winseight <- winseight %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winseighttwo <- winseighttwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalEight <- bind_rows(winseight, winseighttwo)

FinalEight <- FinalEight %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentynineteen <- FinalEight %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))

# -- 2020 --

mlb_schedule_postseason(2020) -> Nine

Nine <- Nine %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winsnine <- Nine %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winsninetwo <- Nine %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winsnine <- winsnine %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winsninetwo <- winsninetwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalNine <- bind_rows(winsnine, winsninetwo)

FinalNine <- FinalNine %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentytwenty <- FinalNine %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))


# -- 2021 --

mlb_schedule_postseason(2021) -> Ten

Ten <- Ten %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winsten <- Ten %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winstentwo <- Ten %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winsten <- winsten %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winstentwo <- winstentwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalTen <- bind_rows(winsten, winstentwo)

FinalTen <- FinalTen %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentytwentyone <- FinalTen %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))

# -- 2022 --

mlb_schedule_postseason(2022) -> Eleven

Eleven <- Eleven %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winsEleven <- Eleven %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winsEleventwo <- Eleven %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winsEleven <- winsEleven %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winsEleventwo <- winsEleventwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalEleven <- bind_rows(winsEleven, winsEleventwo)

FinalEleven <- FinalEleven %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentytwentytwo <- FinalEleven %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))

# -- 2023 --

mlb_schedule_postseason(2023) -> Twelve

Twelve <- Twelve %>%
  select(season, description, teams_away_score, teams_away_is_winner, teams_home_is_winner, teams_away_league_record_wins, teams_away_league_record_losses, teams_away_team_name, teams_home_team_name) %>%
  na.omit()

winsTwelve <- Twelve %>% 
  group_by(teams_away_team_name) %>%
  summarize(teams_away_is_winner)

winsTwelvetwo <- Twelve %>%
  group_by(teams_home_team_name) %>%
  summarize(teams_home_is_winner)

winsTwelve <- winsTwelve %>%
  rename(team = teams_away_team_name, win = teams_away_is_winner)

winsTwelvetwo <- winsTwelvetwo %>%
  rename(team = teams_home_team_name, win = teams_home_is_winner)

FinalTwelve <- bind_rows(winsTwelve, winsTwelvetwo)

FinalTwelve <- FinalTwelve %>%
  mutate(wins = as.numeric(win),
         losses = as.numeric(!win))

twentytwentythree <- FinalTwelve %>%
  group_by(team) %>%
  summarize(total_wins = sum(wins),
            total_loss = sum(losses))


# Combining all yearly postseason data into a single dataset
twentytwelve %>% bind_rows(twentythirteen, twentyfourteen, twentyfifteen, twentysixteen, twentyseventeen, twentyeighteen, twentynineteen, twentytwenty, twentytwentyone, twentytwentytwo, twentytwentythree) -> wins

# Recode team names for consistency
wins %>% mutate(team = recode(team, "Cleveland Indians" = "Cleveland Guardians", "Los Angeles Angels of Anaheim" = "Anaheim Angels", "Los Angeles Angels" = "Anaheim Angels")) -> wins

# Summarize total wins and losses by team over the years
wins %>% group_by(team) %>% summarise("W" = sum(total_wins), "L" = sum(total_loss)) -> wins.by.team

# Calculate total games played in playoffs by each team
wins.by.team %>% mutate("Playoff GP" = W + L) -> playoff.wins.by.team

# Join batting against and batting for data, and then merge with playoff data
batting.against.rates.per.PA %>% inner_join(batting.for.rates.per.PA, by = c("Tm"="Tm")) -> batting.rates
batting.rates %>% inner_join(playoff.wins.by.team, by = c("Tm"="team")) -> rates.playoffs


# Calculate ratios and performance metrics for analysis
rates.playoffs %>% 
  mutate("Playoff Win Pct" = W / (W + L), "HR per PA Ratio" = HR.per.pa.for / HR.per.pa.against, "Singles per PA Ratio" = x1B.per.pa.for / x1B.per.pa.against, "Unintentional BB per PA Ratio" = UBB.per.pa.for / UBB.per.pa.against, "SO per PA Ratio" = SO.per.pa.for / SO.per.pa.against, OPS.ratio = OPS.for / OPS.against) %>% 
  select(Tm, "Playoff Win Pct", "HR per PA Ratio", "Singles per PA Ratio", "Unintentional BB per PA Ratio", "SO per PA Ratio", OPS.ratio, "Playoff GP") -> rates.ratios.playoffs

# Rename OPS ratio for clarity
rates.ratios.playoffs %>% rename("OPS Ratio" = "OPS.ratio") -> rates.ratios.playoffs


# Plotting and analysis of different performance metrics against playoff win percentage
# For each metric (HR/PA, Singles/PA, etc.), plot its differential against playoff win percentage, apply linear regression model, and analyze residuals


# Plot HR/PA Ratio
bestHR.PA <- rates.ratios.playoffs %>%
  arrange(desc(`HR per PA Ratio`)) %>%
  head(3)

HRperPAplot <- ggplot(rates.ratios.playoffs, aes(x = `HR per PA Ratio` , y = `Playoff Win Pct`)) +
  geom_point(alpha = .5, color = "brown1", aes(size = `Playoff GP`)) +
  xlab("HR / PA Differential") +
  ylab("Playoff Win Percentage") +
  ggtitle("Home Run / Plate Appearance Differential vs. Playoff Win %") +
  geom_smooth(method = "lm", se = FALSE, color = "cornflowerblue") +
  geom_text_repel(data = bestHR.PA, color = "black",
                  aes(label = paste(Tm), cex = 20), show.legend = F)
print(HRperPAplot)

# Plot Singles/PA Ratio
bestS.PA <- rates.ratios.playoffs %>%
  arrange(desc(`Singles per PA Ratio`)) %>%
  head(3)

SperPAplot <- ggplot(rates.ratios.playoffs, aes(x = `Singles per PA Ratio` , y = `Playoff Win Pct`)) +
  geom_point(alpha = .5, color = "darkolivegreen4", aes(size = `Playoff GP`)) +
  xlab("Singles / PA Differential") +
  ylab("Playoff Win Percentage") +
  ggtitle("Singles / Plate Appearance Differential vs. Playoff Win %") +
  geom_smooth(method = "lm", se = FALSE, color = "cornflowerblue") +
  geom_text_repel(data = bestS.PA, color = "black",
                  aes(label = paste(Tm), cex = 20), show.legend = F)
print(SperPAplot)

# Plot Unintentional Base-on-balls/PA Ratio
bestUBB.PA <- rates.ratios.playoffs %>%
  arrange(desc(`Unintentional BB per PA Ratio`)) %>%
  head(3)

UBBperPAplot <- ggplot(rates.ratios.playoffs, aes(x = `Unintentional BB per PA Ratio` , y = `Playoff Win Pct`)) +
  geom_point(alpha = .5, color = "darkorchid2", aes(size = `Playoff GP`)) +
  xlab("UBB / PA Differential") +
  ylab("Playoff Win Percentage") +
  ggtitle("Unintentional Walks / Plate Appearance Differential vs. Playoff Win %") +
  geom_smooth(method = "lm", se = FALSE, color = "cornflowerblue") +
  geom_text_repel(data = bestUBB.PA, color = "black",
                  aes(label = paste(Tm), cex = 20), show.legend = F)
print(UBBperPAplot)

# Plot Strikeouts/PA Ratio
bestSO.PA <- rates.ratios.playoffs %>%
  arrange(desc(`SO per PA Ratio`)) %>%
  head(3)

SOperPAplot <- ggplot(rates.ratios.playoffs, aes(x = `SO per PA Ratio` , y = `Playoff Win Pct`)) +
  geom_point(alpha = .5, color = "darkorange1", aes(size = `Playoff GP`)) +
  xlab("SO / PA Differential") +
  ylab("Playoff Win Percentage") +
  ggtitle("Strikeouts / Plate Appearance Differential vs. Playoff Win %") +
  geom_smooth(method = "lm", se = FALSE, color = "cornflowerblue") +
  geom_text_repel(data = bestSO.PA, color = "black",
                  aes(label = paste(Tm), cex = 20), show.legend = F)
print(SOperPAplot)

# Plot OPS Ratio
bestOPSratio <- rates.ratios.playoffs %>%
  arrange(desc(`OPS Ratio`)) %>%
  head(3)

OPSplot <- ggplot(rates.ratios.playoffs, aes(x = `OPS Ratio` , y = `Playoff Win Pct`)) +
  geom_point(alpha = .5, color = "deeppink2", aes(size = `Playoff GP`)) +
  xlab("OPS Differential") +
  ylab("Playoff Win Percentage") +
  ggtitle("On-base Plus Slugging Percentage Differential vs. Playoff Win %") +
  geom_smooth(method = "lm", se = FALSE, color = "cornflowerblue") +
  geom_text_repel(data = bestOPSratio, color = "black",
                  aes(label = paste(Tm), cex = 20), show.legend = F)
print(OPSplot)

# Linear Regression Models for Each Performance Metric
# Analyze the strength of relationships between performance metrics and playoff success
# Include residual analysis to identify teams that significantly differ from the model predictions

# HR/PA Ratio Model
hr.linfit <- lm(`Playoff Win Pct` ~ `HR per PA Ratio`, data = rates.ratios.playoffs)
summary(hr.linfit)  # Display the summary of the model

HR_aug <- augment(hr.linfit, data = rates.ratios.playoffs)  # Augment the model with data to facilitate analysis
HR_aug  # Display augmented data

HR_resid_summary <- HR_aug %>%
  summarize(N = n(), avg = mean(.resid),  # Average residual
            RMSE = sqrt(mean(.resid^2)))  # Root mean square error

HR_resid_summary  # Display residual summary

HR.rmse <- HR_resid_summary %>%
  pull(RMSE)  # Extract the RMSE for use in further analysis

HR_aug %>% summarise(N = n(), within_one = sum(abs(.resid) < HR.rmse), within_two = sum(abs(.resid) < 2 * HR.rmse)) %>% mutate(within_one_pct = within_one / N, within_two_pct = within_two / N) -> error.dist.HR

highlight_teams_HR <- HR_aug %>% arrange(desc(abs(.resid))) %>% head(12)  # Highlight the teams with the highest residuals

resid.plot.HR.ratio <- ggplot(HR_aug, aes(x = `HR per PA Ratio`, y = .resid)) + 
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, linetype = 5) + 
  xlab("HR For / HR Against Ratio") + 
  ylab("Residual") + 
  ggtitle("MLB 2012-2023 Residuals of HR For to HR Against Ratio on Playoff Win %") + 
  geom_point(data = highlight_teams_HR, color = "#78a5db") + 
  geom_text_repel(data = highlight_teams_HR, color = "#78a5db", aes(label = paste(Tm)))
print(resid.plot.HR.ratio)

# Singles/PA Ratio Model
s.linfit <- lm(`Playoff Win Pct` ~ `Singles per PA Ratio`, data = rates.ratios.playoffs)
summary(s.linfit)  # Summary of the model

Singles_aug <- augment(s.linfit, data = rates.ratios.playoffs)  # Augment the data
Singles_aug  # Display augmented data

Singles_resid_summary <- Singles_aug %>%
  summarize(N = n(), avg = mean(.resid),  # Average residual
            RMSE = sqrt(mean(.resid^2)))  # Root mean square error

Singles_resid_summary  # Display residual summary

Singles.rmse <- Singles_resid_summary %>%
  pull(RMSE)  # Extract the RMSE for further analysis

Singles_aug %>% summarise(N = n(), within_one = sum(abs(.resid) < Singles.rmse), within_two = sum(abs(.resid) < 2 * Singles.rmse)) %>% mutate(within_one_pct = within_one / N, within_two_pct = within_two / N) -> error.dist.Singles

highlight_teams_Singles <- Singles_aug %>% arrange(desc(abs(.resid))) %>% head(12)  # Highlight teams with the highest residuals

resid.plot.1B.ratio <- ggplot(Singles_aug, aes(x = `Singles per PA Ratio`, y = .resid)) + 
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, linetype = 5) + 
  xlab("Singles For / Singles Against Ratio") + 
  ylab("Residual") + 
  ggtitle("MLB 2012-2023 Residuals of 1B For to 1B Against Ratio on Playoff Win %") + 
  geom_point(data = highlight_teams_Singles, color = "#78a5db") + 
  geom_text_repel(data = highlight_teams_Singles, color = "#78a5db", aes(label = paste(Tm)))
print(resid.plot.1B.ratio)

# Unintentional Base-on-balls/PA Ratio Model
ubb.linfit <- lm(`Playoff Win Pct` ~ `Unintentional BB per PA Ratio`, data = rates.ratios.playoffs)
summary(ubb.linfit)  # Summary of the model

UBB_aug <- augment(ubb.linfit, data = rates.ratios.playoffs)  # Augment the data
UBB_aug  # Display augmented data

UBB_resid_summary <- UBB_aug %>%
  summarize(N = n(), avg = mean(.resid),  # Average residual
            RMSE = sqrt(mean(.resid^2)))  # Root mean square error

UBB_resid_summary  # Display residual summary

UBB.rmse <- UBB_resid_summary %>%
  pull(RMSE)  # Extract the RMSE for further analysis

UBB_aug %>% summarise(N = n(), within_one = sum(abs(.resid) < UBB.rmse), within_two = sum(abs(.resid) < 2 * UBB.rmse)) %>% mutate(within_one_pct = within_one / N, within_two_pct = within_two / N) -> error.dist.UBB

highlight_teams_UBB <- UBB_aug %>% arrange(desc(abs(.resid))) %>% head(12)  # Highlight teams with the highest residuals

resid.plot.UBB.ratio <- ggplot(UBB_aug, aes(x = `Unintentional BB per PA Ratio`, y = .resid)) + 
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, linetype = 5) + 
  xlab("Unintentional BB For / Unintentional BB Against Ratio") + 
  ylab("Residual") + 
  ggtitle("MLB 2012-2023 Residuals of UBB For to UBB Against Ratio on Playoff Win %") + 
  geom_point(data = highlight_teams_UBB, color = "#78a5db") + 
  geom_text_repel(data = highlight_teams_UBB, color = "#78a5db", aes(label = paste(Tm)))
print(resid.plot.UBB.ratio)

# Strikeouts/PA Ratio Model
so.linfit <- lm(`Playoff Win Pct` ~ `SO per PA Ratio`, data = rates.ratios.playoffs)
summary(so.linfit)  # Summary of the model

SO_aug <- augment(so.linfit, data = rates.ratios.playoffs)  # Augment the data
SO_aug  # Display augmented data

SO_resid_summary <- SO_aug %>%
  summarize(N = n(), avg = mean(.resid),  # Average residual
            RMSE = sqrt(mean(.resid^2)))  # Root mean square error

SO_resid_summary  # Display residual summary

SO.rmse <- SO_resid_summary %>%
  pull(RMSE)  # Extract the RMSE for further analysis

SO_aug %>% summarise(N = n(), within_one = sum(abs(.resid) < SO.rmse), within_two = sum(abs(.resid) < 2 * SO.rmse)) %>% mutate(within_one_pct = within_one / N, within_two_pct = within_two / N) -> error.dist.SO

highlight_teams_SO <- SO_aug %>% arrange(desc(abs(.resid))) %>% head(12)  # Highlight teams with the highest residuals

resid.plot.SO.ratio <- ggplot(SO_aug, aes(x = `SO per PA Ratio`, y = .resid)) + 
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, linetype = 5) + 
  xlab("SO For / SO Against Ratio") + 
  ylab("Residual") + 
  ggtitle("MLB 2012-2023 Residuals of SO For to SO Against Ratio on Playoff Win %") + 
  geom_point(data = highlight_teams_SO, color = "#78a5db") + 
  geom_text_repel(data = highlight_teams_SO, color = "#78a5db", aes(label = paste(Tm)))
print(resid.plot.SO.ratio)

# OPS Ratio Model
ops.linfit <- lm(`Playoff Win Pct` ~ `OPS Ratio`, data = rates.ratios.playoffs)
summary(ops.linfit)  # Summary of the model

OPS_aug <- augment(ops.linfit, data = rates.ratios.playoffs)  # Augment the data
OPS_aug  # Display augmented data

OPS_resid_summary <- OPS_aug %>%
  summarize(N = n(), avg = mean(.resid),  # Average residual
            RMSE = sqrt(mean(.resid^2)))  # Root mean square error

OPS_resid_summary  # Display residual summary

OPS.rmse <- OPS_resid_summary %>%
  pull(RMSE)  # Extract the RMSE for further analysis

OPS_aug %>% summarise(N = n(), within_one = sum(abs(.resid) < OPS.rmse), within_two = sum(abs(.resid) < 2 * OPS.rmse)) %>% mutate(within_one_pct = within_one / N, within_two_pct = within_two / N) -> error.dist.OPS

highlight_teams_OPS <- OPS_aug %>% arrange(desc(abs(.resid))) %>% head(12)  # Highlight teams with the highest residuals

resid.plot.OPS.ratio <- ggplot(OPS_aug, aes(x = `OPS Ratio`, y = .resid)) + 
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, linetype = 5) + 
  xlab("OPS For / OPS Against Ratio") + 
  ylab("Residual") + 
  ggtitle("MLB 2012-2023 Residuals of OPS For to OPS Against Ratio on Playoff Win %") + 
  geom_point(data = highlight_teams_OPS, color = "#78a5db") + 
  geom_text_repel(data = highlight_teams_OPS, color = "#78a5db", aes(label = paste(Tm)))
print(resid.plot.OPS.ratio)

# Analysis of the Pythagorean expectation model for playoff runs
playoff_runs <- read_csv("C:/Users/User/OneDrive/Documents/SAL 602 (Intro to R)/Week 8/Runs_Playoffs_RFinal.csv")  # Load playoff runs data

playoff_runs$RD <- with(playoff_runs, R - RA)  # Calculate run differential
playoff_runs$WinPCT <- with(playoff_runs, W / (W + L))  # Calculate winning percentage

linfit <- lm(WinPCT ~ RD, data = playoff_runs)  # Linear model of winning percentage on run differential
linfit  # Display model

plot(playoff_runs$RD, playoff_runs$WinPCT,  # Plot winning percentage against run differential
     xlab = "Playoff Run Differential",
     ylab = "Playoff Winning Percentage")
abline(a = coef(linfit)[1], b = coef(linfit)[2], lwd = 2)  # Add regression line to the plot

playoff_runs$linWinPCT <- predict(linfit)  # Predict winning percentage from the model
playoff_runs$linResiduals <- residuals(linfit)  # Calculate residuals from the model

highlight_teams_Pythag <- playoff_runs %>% arrange(desc(abs(playoff_runs$linResiduals)))  # Highlight teams with the highest residuals

resid.plot.Pythag <- ggplot(OPS_aug, aes(x = playoff_runs$RD, y = playoff_runs$linResiduals)) + 
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, linetype = 5) + 
  xlab("Playoff Run Differential") + 
  ylab("Residual") + 
  ggtitle("MLB 2012-2023 Residuals of Playoff Run Differential on Playoff Win %") + 
  geom_point(data = highlight_teams_Pythag, color = "#78a5db") + 
  geom_text_repel(data = highlight_teams_Pythag, color = "#78a5db", aes(label = paste(Team)))
print(resid.plot.Pythag)

playoff_runs$pythagorean_WinPCT <- with(playoff_runs, R^2 / (R^2 + RA^2))  # Calculate Pythagorean winning percentage
playoff_runs$pythagoreanResiduals <- playoff_runs$WinPCT - playoff_runs$pythagorean_WinPCT  # Calculate residuals from Pythagorean expectation
mean(playoff_runs$pythagoreanResiduals)  # Average of Pythagorean residuals
sqrt(mean(playoff_runs$pythagoreanResiduals ^ 2))  # Root mean square error of Pythagorean residuals

playoff_runs$logWRatio <- log(playoff_runs$W / playoff_runs$L)  # Log ratio of wins to losses
playoff_runs$logRRatio <- log(playoff_runs$R / playoff_runs$RA)  # Log ratio of runs scored to runs allowed

playoff_runs[is.na(playoff_runs) | playoff_runs=="Inf" | playoff_runs=="-Inf"] <- NA  # Handle infinite and NA values

pyFit <- lm(logWRatio ~ 0 + logRRatio, data = playoff_runs)  # Linear model of log win ratio on log run ratio
pyFit  # Display model

head(playoff_runs)  # Display first few rows of playoff runs data
