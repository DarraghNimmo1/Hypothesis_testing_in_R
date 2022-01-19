library(ggplot2)
library(dplyr)
library(corrplot)


US_births_2018 <- read.csv("C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/2018_US_births.csv")

set.seed(1)
US_births_2018 = sample_n(US_births_2018, 5000)

ggplot(US_births_2018, aes(x=DBWT)) + 
  geom_density() + ggtitle("2018 US birth weights") +
  xlab("Birth Weights (grams)") + 
  ylab("Density") + theme_bw()


sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/2018_US_birth_weight_summary_stats.txt')
cat("=====================================================\n")
cat("Summary statistics of US 2018 birth weights.\n")
cat("=====================================================\n")
as.data.frame(group_by(US_births_2018, SEX) %>%
                summarise(
                  count = n(),
                  mean = mean(DBWT, na.rm = TRUE),
                  sd = sd(DBWT, na.rm = TRUE)
                ))
sink()


US_births_2018 = data.frame(US_births_2018$CIG_0, US_births_2018$DBWT)
colnames(US_births_2018) = c("CIG_0", "DBWT")
head(US_births_2018)

US_births_2018$DBWT_categories <- cut(US_births_2018$DBWT, breaks = c(0, 3000, Inf),
                  labels = c("Below average", "Above average"), include.lowest = TRUE)

US_births_2018$CIG_categories = cut(US_births_2018$CIG_0, breaks = c(0, 1, Inf),
                             labels = c("Non Smoker", "Smoker"), include.lowest = TRUE)

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/2018_US_Birth_Cigarette_table.txt')
cat("=====================================================\n")
cat("Contingency table of 2018 US birth weights in grams and 
    prior cigarette smoking status of the mother.\n")
cat("=====================================================\n")
table(US_births_2018$DBWT_categories, US_births_2018$CIG_categories)
sink()

US_births_2018_data_table = table(US_births_2018$DBWT_categories, US_births_2018$CIG_categories)

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/2018_US_Birth_Cigarette_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & 
    smoking status of mother prior to pregnancy.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/2018_US_Birth_Cigarette_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - Observed data.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$observed
cat("=====================================================\n")
cat("Chi-square test of Indpendence - Expected data.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$expected
cat("=====================================================\n")
cat("Chi-square test of Indpendence - Residuals.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$residuals
sink()

library(corrplot)

corrplot(chisq.test(US_births_2018_data_table)$residuals, is.cor = FALSE)

##

US_births_2018 <- read.csv("C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/2018_US_births.csv")

set.seed(1)
US_births_2018 = sample_n(US_births_2018, 5000)

US_births_2018 = data.frame(US_births_2018$DOB_MM, US_births_2018$DBWT)
colnames(US_births_2018) = c("Birth_month", "DBWT")
head(US_births_2018)

US_births_2018$DBWT_categories <- cut(US_births_2018$DBWT, breaks = c(0, 3000, Inf),
                               labels = c("Below average", "Above average"), include.lowest = TRUE)

US_births_2018$Birth_month = cut(US_births_2018$Birth_month, breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                             labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), include.lowest = TRUE)


head(US_births_2018, n=100)

US_births_2018_data_table = table(US_births_2018$DBWT_categories, US_births_2018$Birth_month)

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_birth_month_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & month of the year.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_birth_month_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & month of the year.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$observed
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_birth_month_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & month of the year.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$expected
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_birth_month_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & month of the year.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$residuals
sink()

corrplot(chisq.test(US_births_2018_data_table)$residuals, is.cor = FALSE)

##

US_births_2018 <- read.csv("C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/2018_US_births.csv", na.strings=c("","NA"))
US_births_2018 = US_births_2018[!(is.na(US_births_2018$DMAR) | US_births_2018$DMAR==""), ]

set.seed(1)
US_births_2018 = sample_n(US_births_2018, 5000)

US_births_2018 = data.frame(US_births_2018$DMAR, US_births_2018$DBWT)
colnames(US_births_2018) = c("Martial", "DBWT")
head(US_births_2018)

US_births_2018$DBWT_categories <- cut(US_births_2018$DBWT, breaks = c(0, 3000, Inf),
                               labels = c("Below average", "Above average"), include.lowest = TRUE)

US_births_2018$Martial_status = cut(US_births_2018$Martial, breaks = c(0,1 ,2),
                          labels = c("Married", "Not_married"), include.lowest = TRUE)


head(US_births_2018, n=100)

US_births_2018_data_table = table(US_births_2018$DBWT_categories, US_births_2018$Martial_status)

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_martial_status_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & martial status.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_martial_status_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & martial status.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$observed
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_martial_status_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & martial status.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$expected
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_martial_status_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & martial status.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$residuals
sink()

corrplot(chisq.test(US_births_2018_data_table)$residuals, is.cor = FALSE)

##

US_births_2018 <- read.csv("C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/2018_US_births.csv", na.strings=c("","NA"))
US_births_2018 = US_births_2018[!(US_births_2018$MEDUC== 9), ]

set.seed(1)
US_births_2018 = sample_n(US_births_2018, 5000)

US_births_2018 = data.frame(US_births_2018$FEDUC, US_births_2018$DBWT)
colnames(US_births_2018) = c("FEDUC", "DBWT")
head(US_births_2018)

US_births_2018$DBWT_categories <- cut(US_births_2018$DBWT, breaks = c(0, 3000, Inf),
                               labels = c("Below average", "Above average"), include.lowest = TRUE)

US_births_2018$FEDUC_categories <- cut(US_births_2018$FEDUC, breaks = c(0, 2, 4,6, 7, 8 ),
                                labels = c("No High school", "High School","Bachelor","Master","Dcotorate"), include.lowest = TRUE)


US_births_2018_data_table = table(US_births_2018$DBWT_categories, US_births_2018$FEDUC_categories)

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_father_education_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & fathers education.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_father_education_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & fathers education.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$observed
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_father_education_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & fathers education.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$expected
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_father_education_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & fathers education.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$residuals
sink()

corrplot(chisq.test(US_births_2018_data_table)$residuals, is.cor = FALSE)

##

US_births_2018 <- read.csv("C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/2018_US_births.csv", na.strings=c("","NA"))
US_births_2018 = US_births_2018[!(US_births_2018$MEDUC== 9), ]

set.seed(1)
US_births_2018 = sample_n(US_births_2018, 5000)

US_births_2018 = data.frame(US_births_2018$MEDUC, US_births_2018$DBWT)
colnames(US_births_2018) = c("MEDUC", "DBWT")
head(US_births_2018)

US_births_2018$DBWT_categories <- cut(US_births_2018$DBWT, breaks = c(0, 3000, Inf),
                               labels = c("Below average", "Above average"), include.lowest = TRUE)

US_births_2018$MEDUC_categories <- cut(US_births_2018$MEDUC, breaks = c(0, 2, 4,6, 7, 8 ),
                                labels = c("No High school", "High School","Bachelor","Master","Dcotorate"), include.lowest = TRUE)


head(US_births_2018, n=100)

US_births_2018_data_table = table(US_births_2018$DBWT_categories, US_births_2018$MEDUC_categories)


sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_mother_education_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & mothers education.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_mother_education_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & mothers education.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$observed
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_mother_education_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & mothers education.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$expected
sink()

sink('C:/Users/GenDataSci021/Desktop/R/Post_grad_assignment/US_births_2018_mother_education_Chi-square_test.txt')
cat("=====================================================\n")
cat("Chi-square test of Indpendence - 2018 US birth weights & mothers education.\n")
cat("=====================================================\n")
chisq.test(US_births_2018_data_table)$residuals
sink()

corrplot(chisq.test(US_births_2018_data_table)$residuals, is.cor = FALSE)
