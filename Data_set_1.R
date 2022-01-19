##########################################################################
#my_data_california = my_data[my_data$State %in% cali,]
library(dplyr)
library(cowplot)

set.seed(1)
Rio_2016_olympics <- read.csv("C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/Rio_2016_Olympics.csv")
Rio_2016_olympics = Rio_2016_olympics[!(is.na(Rio_2016_olympics$height) | Rio_2016_olympics$height==""), ]

Rio_2016_olympics_female = Rio_2016_olympics %>% filter(sex == "female")
Rio_2016_olympics_female_5000 = sample_n(Rio_2016_olympics_female ,5000)
                          
Rio_2016_olympics_male = Rio_2016_olympics %>% filter(sex == "male")
Rio_2016_olympics_male_5000 = sample_n(Rio_2016_olympics_male ,5000)

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/Rio_olympics_shapiro.txt')
cat("=====================================================\n")
cat("Shapiro-Wilk test of the heights of male athletes competing in Rio Olympics.\n")
cat("=====================================================\n")
shapiro.test(Rio_2016_olympics_male_5000$height )
cat("=====================================================\n")
cat("Shapiro-Wilk test of the heights of female athletes competing in Rio Olympics.\n")
cat("=====================================================\n")
shapiro.test(Rio_2016_olympics_female_5000$height)
sink()

Male_density = ggplot(Rio_2016_olympics_male, aes(x=height)) + 
  geom_density() + ggtitle("Male athelte height") +
  xlab("Height") + 
  ylab("Density") + theme_bw()

Female_density = ggplot(Rio_2016_olympics_female, aes(x=height)) + 
  geom_density() + ggtitle("Female athelte height") +
  xlab("Height") + 
  ylab("Density") + theme_bw()

plot_grid(Male_density, Female_density)


par(mfrow = c(1,2))
plot(density(Rio_2016_olympics_male$height ))
plot(density(Rio_2016_olympics_female$height ))

par(mfrow = c(1,2))
qqnorm(Rio_2016_olympics_male$height, pch = 1, ylab="Sample quantiles", xlab="Theoretical quantiles", main="Male athlete height")
qqline(Rio_2016_olympics_male$height, col = "steelblue", lwd = 2)
qqnorm(Rio_2016_olympics_female$height, pch = 1, ylab="Sample quantiles", xlab="Theoretical quantiles", main="Female athlete height")
qqline(Rio_2016_olympics_female$height, col = "steelblue", lwd = 2)

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/Rio_olympics_summary_stats.txt')
cat("=====================================================\n")
cat("Summary statistics of the heights of athletes competing in Rio Olympics.\n")
cat("=====================================================\n")
as.data.frame(group_by(Rio_2016_olympics, sex) %>%
  summarise(
    count = n(),
    mean = mean(height, na.rm = TRUE),
    sd = sd(height, na.rm = TRUE)
  ))
sink()

boxplot(Rio_2016_olympics_male$height, Rio_2016_olympics_female$height, names = c("Male Height", "Female Height") ,col = c("green", "blue"))



#You need to know what variance your data has before running a t-test.

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/Rio_olympics_variance_test.txt')
cat("=====================================================\n")
cat("F test investigating variance of the heights of athletes competing in Rio Olympics.\n")
cat("=====================================================\n")
var.test(Rio_2016_olympics_male$height, Rio_2016_olympics_female$height, alternative = "greater")
sink()


#The null hypothesis is that the two means are equal
#the alternative is that they are not

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/Rio_olympics_t_test.txt')
cat("=====================================================\n")
cat("Two sample welch t-test of the heights of athletes competing in Rio Olympics.\n")
cat("=====================================================\n")
t.test(Rio_2016_olympics_male$height, Rio_2016_olympics_female$height, alternative = "greater", var.equal = FALSE)
sink()
