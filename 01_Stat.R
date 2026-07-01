#setting 
install.packages("tidyverse")
install.packages("Epi")
install.packages("knitr")

library(tidyverse)
library(dplyr)
library(knitr)

#import data into R
d <- read.csv("C:/Users/Yu-Zhen Chou/Desktop/Files/02 Master/05 HDS/IIDS67631 Statistics for Health Data Science/01. Assessment 1/SHDS_Assessment1_data.csv",header = TRUE)
head(d)  
str(d)
dim(d)
names(d)
summary(d)

d$Diabetes_012 <- as.factor(d$Diabetes_012)
d$HighChol   <- as.factor(d$HighChol)
d$CholCheck  <- as.factor(d$CholCheck)
d$Smoker     <- as.factor(d$Smoker)
d$Sex        <- as.factor(d$Sex)
d$Stroke     <- as.factor(d$Stroke)


# ================ Table 1 for summary the data ====================
variables <- setdiff(names(d), c("X", "Diabetes_012"))
str(variables)

# Original
tab <- table(d$Sex)
percent <- round(tab / sum(tab) * 100, 1)
paste0(tab, " (", percent, "%)")

library(dplyr)
library(tidyr)
library(knitr)

# Step 1: 建立交叉表格
tab <- table(d$BMI, d$Diabetes_012)
tab_df <- as.data.frame(tab)

# Step 2: 加上百分比（以 HighChol 為分組基準）
tab_df <- tab_df %>%
  group_by(Var1) %>%
  mutate(
    Percent = round(Freq / sum(Freq) * 100, 1),
    Combined = paste0(Freq, " (", Percent, "%)")
  )%>%
  ungroup()

# Step 4: 轉成橫向格式（你喜歡的樣式）
final_table <- tab_df %>%
  pivot_wider(names_from = Var2, values_from = Combined) %>%
  rename(BMI = Var1,
         `Diabetes = 0` = `0`,
         `Diabetes = 1` = `1`) %>%
  select(BMI, `Diabetes = 0`, `Diabetes = 1`)%>%
  group_by(BMI) %>%
  summarise(
    `Diabetes = 0` = coalesce(`Diabetes = 0`[!is.na(`Diabetes = 0`)], NA_character_),
    `Diabetes = 1` = coalesce(`Diabetes = 1`[!is.na(`Diabetes = 1`)], NA_character_)
  )

# Step 5: 呈現表格
kable(final_table,)

#============================= 連續變相 ====================================
MentHlth <- d %>%
  group_by(Diabetes_012) %>%
  summarise(
    mean_MentHlth = round(mean(MentHlth, na.rm = TRUE), 2),
    sd_MentHlth = round(sd(MentHlth, na.rm = TRUE), 2),
    n = n()
  )
MentHlth <-d %>%
  group_by(Diabetes_012) %>%
  summarise(
    MentHlth_avg_sd = paste0(round(mean(MentHlth, na.rm=TRUE), 2), 
                        " (", round(sd(MentHlth, na.rm=TRUE), 2), ")")
  )
MentHlth


# check the data are normal distribution or not
par(mfrow = c(2,1))
hist(d$BMI, main = "BMI Histogram", xlab = "BMI", col = "lightblue")
qqnorm(d$BMI, main = "QQ plot for BMI")
qqline(d$BMI, col = "red")
ggplot(d,aes(x="", y=BMI))+
  geom_boxplot(fill = "lightblue")+
  labs(title= "BMI distribution", y = "BMI")

par(mfrow = c(2,1))
hist(log(d$BMI), main = "log of BMI Histogram", xlab = "log of BMI", col = "purple")
qqnorm(log(d$BMI), main = "QQ plot for log of BMI")
qqline(log(d$BMI), col = "red")

var.test(BMI~Diabetes_012,data=d)
t.test(BMI~Diabetes_012, data = d, var.equal = FALSE)

hist(d$Age, main = "Age Histogram", xlab = "Age", col = "lightgreen")
qqnorm(d$Age, main = "QQ plot for Age")
qqline(d$Age, col = "red")
ggplot(d,aes(x="", y=Age))+
  geom_boxplot(fill = "lightgreen")+
  labs(title= "Age distribution", y = "Age")

hist(log(d$Age), main = "log of Age Histogram", xlab = "log of Age", col = "green")
qqnorm(log(d$Age), main = "QQ plot for log of Age")
qqline(log(d$Age), col = "red")
logage <- data.frame(logage = log(d$Age), age = d$Age)
ggplot(logage,aes(x="", y=logage))+
  geom_boxplot(fill = "green")+
  labs(title= "log of Age distribution", y = "log of Age")

var.test(Age~Diabetes_012,data=d)
t.test(Age~Diabetes_012, data = d, var.equal = FALSE)

hist(d$MentHlth, main = "Mental Health Day", xlab = "MentHlth", col = "lightpink")
qqnorm(d$MentHlth, main = "QQ plot for Mental Health Day")
qqline(d$MentHlth, col = "red")
ggplot(d,aes(x="", y=MentHlth))+
  geom_boxplot(fill = "lightpink")+
  labs(title= "MentHlth distribution", y = "MentHlth")
dev.off()

var.test(MentHlth~Diabetes_012,data=d)
t.test(MentHlth~Diabetes_012, data = d, var.equal = FALSE)

# ========================== 未來會用到的 table 1 =============================
install.packages("furniture")
library(furniture)
d2 <- d[, c("Diabetes_012", cols)]
table1(d2, splitby = "Diabetes_012", test = TRUE)
# table1(d,splitby = "Diabetes_012", test = TRUE) 會將編號也納入計算


chisq.test(d$Diabetes_012,d$HighChol,correct=FALSE)
chisq.test(d$Diabetes_012,d$Sex,correct=FALSE)








#=================== Q2. compare data between region ===========================


# z-test for proportion: DB group prevalence is 14% or not
# H0: p = 0.14
# H0: p != 0.14
prop.test(sum(d$Diabetes_012 == 1), nrow(d), p = 0.14, alternative = "two.sided", correct = FALSE, conf.level = 0.95)

#H0: bmi = 28
#H1: bmi != 28
t.test(d$BMI, mu = 28, conf.level = 0.95)
#H0: age = 55
#H1: age != 55
t.test(d$Age, mu = 55, conf.level = 0.95)

# H0: no association between Smoker and Diabetes
# H1: an association between Smoker and Diabetes
chisq.test(d$Diabetes_012,d$Smoker,correct=FALSE)
library(Epi)
twoby2(table(Exposure = d$Smoker, Disease = d$Diabetes_012))

install.packages("epiR")
library(epiR)
tab <- table(d$Smoker, d$Diabetes_012)
epi.2by2(tab, method = "cohort.count", conf.level = 0.95)

d_name <- d %>%
  mutate(
    Smoker_str = ifelse(Smoker == 0, "Non-Smoker", "Smoker"),
    Diabetes_str = ifelse(Diabetes_012 == 0, "No Diabetes", "Diabetes")
  )

df <- d_new %>%
  group_by(Diabetes_str, Smoker_str) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Diabetes_str) %>%
  mutate(prop = count / sum(count))

ggplot(df, aes(x = Diabetes_str, y = prop, fill = Smoker_str, label = scales::percent(prop, accuracy=0.01))) +
  geom_col(position = "fill") +
  geom_text(
    position = position_fill(vjust = 0.9),
    color = "black",
    size = 3
  ) +
  labs(title = "Association between Diabetes and Smoker",
       x = "Diabetes",
       y = "proportion",
       fill = "Smocker") +
  scale_fill_manual(values=c("#F8766D", "#00BFC4"))


#stratified analysis

#Sex: 0=female
chisq.test(table(d$Smoker[d$Sex == 0], d$Diabetes_012[d$Sex == 0]))
chisq.test(table(d$Smoker[d$Sex == 1], d$Diabetes_012[d$Sex == 1]))

chisq.test(table(d$Smoker[d$HighChol == 0], d$Diabetes_012[d$HighChol == 0]))
chisq.test(table(d$Smoker[d$HighChol == 1], d$Diabetes_012[d$HighChol == 1]))

chisq.test(table(d$Smoker[d$BMI < 40], d$Diabetes_012[d$BMI < 40]))
chisq.test(table(d$Smoker[d$BMI >= 40], d$Diabetes_012[d$BMI >= 40]))

chisq.test(table(d$Smoker[d$Age < 65], d$Diabetes_012[d$Age < 65]))
chisq.test(table(d$Smoker[d$Age >= 65], d$Diabetes_012[d$Age >= 65]))


#table 2
library(gt)
table_2_gt <- res_32 %>%
  gt() %>%
  tab_header(
    title = md("**Table 2. Comparison with last year's findings**"),
    subtitle = md("_With 95% confidence intervals and effect sizes_")
  ) %>%
  cols_align(
    align = "center", 
    columns = everything()
  ) %>%
  cols_width(
    Metric ~ px(200),
    Reference ~ px(80),
    Estimate ~ px(100),
    CI_95 ~ px(180),
    Diff_vs_ref ~ px(100),
    P_value ~ px(80),
    Effect_size ~ px(130),
    Sensitivity_nonparam_p ~ px(120),
    N ~ px(60)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.names = "Arial",
    table.font.size = 11,
    table.width = px(1050),
    data_row.padding = px(6),
    heading.align = "center",
    heading.title.font.size = 13,
    heading.subtitle.font.size = 11,
    column_labels.border.top.width = px(1),
    column_labels.border.bottom.width = px(1),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    table_body.hlines.width = px(0.5),
    table_body.vlines.width = px(0.5)
  )



==========================???==============================

# prop.test
prop.test(sum(d$Diabetes_012 == 1), length(d$Diabetes_012))



============================ Practice ===============================
# last year
d_reg <- read.csv("C:/Users/Yu-Zhen Chou/Desktop/Files/02 Master/05 HDS/IIDS67631 Statistics for Health Data Science/01. Assessment 1/PRACTICE data for assessment 1.csv",header = TRUE)
names(d_reg)

tab_reg_DB <- table(d_reg$DIABETES)
percent_reg_DB <- prop.table(tab_reg_DB)*100
print(round(percent_reg_DB,2))

avg_reg_bmi <- mean(d_reg$BMIO, na.rm = TRUE)
print(round(avg_reg_bmi,2))

avg_reg_age <- mean(d_reg$AGE, na.rm = TRUE)
print(round(avg_reg_age,2))

chisq.test(d_reg$DIABETES,d_reg$SMOKING,correct=FALSE)
twoby2(d_reg$DIABETES, d_reg$SMOKING)

# H0: no association between HighChol and CholCheck
# H1: an association between HighChol and CholCheck
table(d$HighChol, d$CholCheck)
chisq.test(d$HighChol, d$CholCheck,correct=FALSE)
twoby2(d$HighChol, d$CholCheck)
#Because p-value less than 0.05 (p-value < 2.2e-16), null hypothesis is rejected. There is an association between high cholesterol and cholesterol check.
#According to Sample Odds Ratio: 2.8806 (Confidential interval: 2.7397 - 3.0288), the patient who conduct the test has 2.88 times odds to diagnosis high cholesterol compare with non-check.



