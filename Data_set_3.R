library(ggplot2)
library(cowplot)
library(tidyverse)
library(dplyr)
library(nortest)

premier_league <- read.csv("C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/premier_league_stats.csv")


premier_league_top_4 = premier_league %>% filter(team == "Liverpool"| team == "Manchester United"
                                                 | team == "Chelsea" | team == "Arsenal" | 
                                                   team == "Tottenham Hotspur" | team == "Manchester City"
                                                 | team == "Leicester City")


sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/prem_league_goals_summary_top4.txt')
cat("=====================================================\n")
cat("Summary statistics of goals scored by top 4 teams in the premier league.\n")
cat("=====================================================\n")
as.data.frame(group_by(premier_league_top_4, team) %>%
                summarise(
                  count = n(),
                  mean = mean(goals, na.rm = TRUE),
                  sd = sd(goals, na.rm = TRUE)
                ))
sink()

boxplot((premier_league_top_4 %>% filter(team == "Liverpool"))$goals, 
        (premier_league_top_4 %>% filter(team == "Manchester United"))$goals,
        (premier_league_top_4 %>% filter(team == "Manchester City"))$goals,
        (premier_league_top_4 %>% filter(team == "Chelsea"))$goals,
        (premier_league_top_4 %>% filter(team == "Arsenal"))$goals,
        (premier_league_top_4 %>% filter(team == "Tottenham Hotspur"))$goals,
        (premier_league_top_4 %>% filter(team == "Leicester City"))$goals,
        names = c("Liverpool", "Man U", "Man City", "Chelsea", "Arsenal", "Tottenham", "Leicester"), las=2)



premier_league_relegation = premier_league %>% filter(team == "Sheffield United" | team == "Charlton Athletic"
                                                      | team == "Watford" | team == "Reading" | team == "Birmingham City" |
                                                        team == "Derby County" | team == "Newcastle United" | team == "Middlesbrough"
                                                      | team == "West Bromwich Albion" | team == "Burnley" | team == "Hull City"|
                                                        team == "Portsmouth"| team == "West Ham United" | team == "Blackpool" |
                                                        team == "Wolverhampton Wanderers" | team == "Blackburn Rovers" | team == "Bolton Wanderers"
                                                      | team == "Norwich City" | team == "Fulham" | team == "Cardiff City" 
                                                      | team == "Queens Park Rangers" | team == "Aston Villa" | team == "Sunderland"
                                                      | team == "Swansea City" | team == "Stoke City")



sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/prem_league_goals_summary_relegation.txt')
cat("=====================================================\n")
cat("Summary statistics of goals scored by relegation teams
    in the premier league.\n")
cat("=====================================================\n")
as.data.frame(group_by(premier_league_relegation, team) %>%
                summarise(
                  count = n(),
                  mean = mean(goals, na.rm = TRUE),
                  sd = sd(goals, na.rm = TRUE)
                ))
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/premier_league_shapiro.txt')
cat("=====================================================\n")
cat("Shapiro-wilk - Premier league top 4 teams goals 
    2006/2007 to 2017/2018.\n")
cat("=====================================================\n")
shapiro.test(premier_league_top_4$goals)
cat("=====================================================\n")
cat("Shapiro-wilk test - Premier league relegation teams goals 
    2006/2007 to 2017/2018.\n")
shapiro.test(premier_league_relegation$goals)
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/premier_league_anderson.txt')
cat("=====================================================\n")
cat("Anderson-Darling - Premier league top 4 teams goals 
    2006/2007 to 2017/2018.\n")
cat("=====================================================\n")
ad.test(premier_league_top_4$goals)
cat("=====================================================\n")
cat("Anderson-Darling test - Premier league relegation teams goals 
    2006/2007 to 2017/2018.\n")
ad.test(premier_league_relegation$goals)
sink()


par(mfrow = c(1,2))
qqnorm(premier_league_top_4$goals, pch = 21,bg = "dodgerblue", col = "steelblue", ylab="Sample quantiles", xlab="Theoretical quantiles", main="Top 4 teams total goals 2006/2007 - 2017/2018")
qqline(premier_league_top_4$goals, lwd = 2)
qqnorm(premier_league_relegation$goals, pch = 21,bg = "dodgerblue", col = "steelblue", ylab="Sample quantiles", xlab="Theoretical quantiles", main="Relegation teams total goals 2006/2007 - 2017/2018")
qqline(premier_league_relegation$goals, lwd = 2)


#boxplots


Top_4_goals_density = ggplot(premier_league_top_4, aes(x=goals)) + 
  geom_density() + ggtitle("Top 4 teams total goals 2006/2007 - 2017/2018") +
  xlab("Total goals") + ylab("Density") +
  theme_bw()

Relegation_goals_density = ggplot(premier_league_relegation, aes(x=goals)) + 
  geom_density() + ggtitle("Relegation teams total goals 2006/2007 - 2017/2018") +
  xlab("Total goals") + ylab("Density") +
  theme_bw()

plot_grid(Top_4_goals_density, Relegation_goals_density)


sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/premier_league_variance_test.txt')
cat("=====================================================\n")
cat("F test - Premier league top 4 teams vs relegation teams 
    goals 2006/2007 to 2017/2018.\n")
cat("=====================================================\n")
var.test(premier_league_top_4$goals, premier_league_relegation$goals, alternative = "greater")
sink()


sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/premier_league_t_test_test.txt')
cat("=====================================================\n")
cat("One sided two samplet-test - Premier league top 4 teams vs relegation teams 
     goals 2006/2007 to 2017/2018.\n")
cat("=====================================================\n")
t.test(premier_league_top_4$goals, premier_league_relegation$goals, alternative = "greater", var.equal = F)
sink()


premier_league_top_4$team = as.factor(premier_league_top_4$team)


aov(goals ~ team, data = premier_league_top_4)

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/premier_league_t_test_test.txt')
cat("=====================================================\n")
cat("ANOVA test - Premier league top 4 teams 
     goals 2006/2007 to 2017/2018.\n")
cat("=====================================================\n")
summary(aov(goals ~ team, data = premier_league_top_4))
sink()
