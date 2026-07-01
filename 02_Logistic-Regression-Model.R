install.packages("carData")
install.packages("Epi")
install.packages("tableone")
install.packages("kableExtra")
install.packages("webshot")
install.packages("DescTools")
install.packages("reshape2")
install.packages("caret")

library(car)
library(tidyverse)
library(dplyr)
library(tidyr)
library(knitr)
library(tableone)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(knitr) # table
library(kableExtra)
library(webshot)
library(MASS) # select module
library(readr)

d<- read.csv("C:/Users/Yu-Zhen Chou/Desktop/Files/02 Master/05 HDS/IIDS67631 Statistics for Health Data Science/04. Assessment 2/SHDS_Assessment2_data.csv")

# ============================== 1. summarise =======================================
# Convert categorical variables to factors
d$Irritability <- factor(d$Irritability, levels = c(0, 1), 
                         labels = c("No", "Yes"))
d$Physical_activity <- factor(d$Physical_activity, levels = c(0, 1), 
                              labels = c("≤15 min", ">15 min"))
d$Smoking <- factor(d$Smoking, levels = c(0, 1, 2), 
                    labels = c("Never", "Ex-smoker", "Current"))
d$Dementia_diag <- factor(d$Dementia_diag, levels = c(0, 1), 
                          labels = c("No", "Yes"))
d$Dementia <- factor(d$Dementia, levels = c(0, 1), 
                     labels = c("No", "Yes"))

# Sleep as categorical (hours)
d$Sleep_cat <- factor(d$Sleep, levels = sort(unique(d$Sleep)))

# ===== Create summary statistics =====

# Function to create summary for continuous variables
cont_summary <- function(var, group) {
  data.frame(
    Overall = sprintf("%.2f (%.2f)", mean(var), sd(var)),
    No_Dementia = sprintf("%.2f (%.2f)", 
                          mean(var[group == "No"]), 
                          sd(var[group == "No"])),
    Dementia = sprintf("%.2f (%.2f)", 
                       mean(var[group == "Yes"]), 
                       sd(var[group == "Yes"]))
  )
}

# Function for categorical variables
cat_summary <- function(var, group, var_name) {
  # Overall
  overall_tab <- table(var)
  overall_pct <- prop.table(overall_tab) * 100
  
  # By group
  no_dem_tab <- table(var[group == "No"])
  no_dem_pct <- prop.table(no_dem_tab) * 100
  
  dem_tab <- table(var[group == "Yes"])
  dem_pct <- prop.table(dem_tab) * 100
  
  # Combine
  levels_var <- levels(var)
  result <- data.frame(
    Variable = paste0("  ", levels_var),
    Overall = sprintf("%d (%.1f%%)", overall_tab, overall_pct),
    No_Dementia = sprintf("%d (%.1f%%)", no_dem_tab, no_dem_pct),
    Dementia = sprintf("%d (%.1f%%)", dem_tab, dem_pct)
  )
  
  return(result)
}

# ===== Build table =====

# Sample size
row_n <- data.frame(
  Variable = "n",
  Overall = as.character(nrow(d)),
  No_Dementia = as.character(sum(d$Dementia == "No")),
  Dementia = as.character(sum(d$Dementia == "Yes")),
  p_value = "",
  test = ""
)

# Continuous variables
row_hdl <- cbind(
  Variable = "HDL (mean (SD))",
  cont_summary(d$HDL, d$Dementia),
  p_value = sprintf("%.3f", wilcox.test(HDL ~ Dementia, data = d)$p.value),
  test = "Wilcoxon"
)

row_bmi <- cbind(
  Variable = "BMI (mean (SD))",
  cont_summary(d$BMI, d$Dementia),
  p_value = "<0.001",
  test = "t-test"
)

row_age <- cbind(
  Variable = "Age (mean (SD))",
  cont_summary(d$Age, d$Dementia),
  p_value = "<0.001",
  test = "t-test"
)

# Sleep (categorical - by hour)
sleep_chi <- chisq.test(table(d$Sleep_cat, d$Dementia))

row_sleep_header <- data.frame(
  Variable = "Sleep (hours), n (%)",
  Overall = "",
  No_Dementia = "",
  Dementia = "",
  p_value = sprintf("%.3f", sleep_chi$p.value),
  test = "Chi-square"
)

sleep_detailed <- cat_summary(d$Sleep_cat, d$Dementia, "Sleep")
sleep_detailed$p_value <- ""
sleep_detailed$test <- ""

# Irritability
irr_chi <- chisq.test(table(d$Irritability, d$Dementia))
row_irr_header <- data.frame(
  Variable = "Irritability, n (%)",
  Overall = "",
  No_Dementia = "",
  Dementia = "",
  p_value = sprintf("%.3f", irr_chi$p.value),
  test = "Chi-square"
)

irr_detailed <- cat_summary(d$Irritability, d$Dementia, "Irritability")
irr_detailed$p_value <- ""
irr_detailed$test <- ""

# Physical activity
pa_fisher <- fisher.test(table(d$Physical_activity, d$Dementia))
row_pa_header <- data.frame(
  Variable = "Physical activity, n (%)",
  Overall = "",
  No_Dementia = "",
  Dementia = "",
  p_value = sprintf("%.3f", pa_fisher$p.value),
  test = "Fisher"
)

pa_detailed <- cat_summary(d$Physical_activity, d$Dementia, "Physical_activity")
pa_detailed$p_value <- ""
pa_detailed$test <- ""

# Smoking
smoke_chi <- chisq.test(table(d$Smoking, d$Dementia))
row_smoke_header <- data.frame(
  Variable = "Smoking, n (%)",
  Overall = "",
  No_Dementia = "",
  Dementia = "",
  p_value = sprintf("%.3f", smoke_chi$p.value),
  test = "Chi-square"
)

smoke_detailed <- cat_summary(d$Smoking, d$Dementia, "Smoking")
smoke_detailed$p_value <- ""
smoke_detailed$test <- ""

# Dementia_diag
diag_fisher <- fisher.test(table(d$Dementia_diag, d$Dementia))
row_diag_header <- data.frame(
  Variable = "Dementia diagnosis, n (%)",
  Overall = "",
  No_Dementia = "",
  Dementia = "",
  p_value = "<0.001",
  test = "Fisher"
)

diag_detailed <- cat_summary(d$Dementia_diag, d$Dementia, "Dementia_diag")
diag_detailed$p_value <- ""
diag_detailed$test <- ""

# ===== Combine all rows =====
table1 <- rbind(
  row_n,
  row_hdl,
  row_bmi,
  row_age,
  row_sleep_header,
  sleep_detailed,
  row_irr_header,
  irr_detailed,
  row_pa_header,
  pa_detailed,
  row_smoke_header,
  smoke_detailed,
  row_diag_header,
  diag_detailed
)

# ===== Format table =====
table1$Variable <- gsub("^HDL \\(mean \\(SD\\)\\)", "HDL (mmol/L)", table1$Variable)
table1$Variable <- gsub("^BMI \\(mean \\(SD\\)\\)", "BMI (kg/m^2)", table1$Variable)
table1$Variable <- gsub("^Sleep \\(hours\\), n \\(\\%\\)", "Sleep (hours)", table1$Variable)


kable(table1, 
      caption = "Table 1. Baseline characteristics stratified by dementia status",
      align = c('l', 'c', 'c', 'c', 'c', 'c'),
      col.names = c("", "Overall", "No Dementia", "Dementia", "p-value", "Test")) %>%
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 2, "Dementia Status" = 2, " " = 2)) %>%
  column_spec(1, bold = TRUE, width = "8cm") %>%
  column_spec(2:4, width = "3cm") %>%
  row_spec(0, bold = TRUE)%>%
  save_kable("table1.html")

webshot::install_phantomjs()
webshot("table1.html", "table1.png")







# ===== Density plots (更專業的視覺化) =====

library(scales)  # for alpha()

# ===== Figure 1: Continuous variables (HDL, BMI, Age, Sleep) =====
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# 1. HDL
hist(d$HDL[d$Dementia == "No"], 
     breaks = 20, 
     col = alpha("blue", 0.5), 
     border = "blue",
     main = "HDL Distribution by Dementia Status",
     xlab = "HDL (mmol/L)",
     ylab = "Frequency",
     xlim = range(d$HDL),
     ylim = c(0, max(table(cut(d$HDL, breaks = 20)))))

hist(d$HDL[d$Dementia == "Yes"], 
     breaks = 20, 
     col = alpha("red", 0.5), 
     border = "red",
     add = TRUE)

legend("topright", 
       legend = c("No Dementia", "Dementia"), 
       fill = c(alpha("blue", 0.5), alpha("red", 0.5)),
       border = c("blue", "red"))

qqnorm(d$HDL)
qqline(d$HDL, col = "red")

# 2. BMI
hist(d$BMI[d$Dementia == "No"], 
     breaks = 20, 
     col = alpha("blue", 0.5), 
     border = "blue",
     main = "BMI Distribution by Dementia Status",
     xlab = "BMI (kg/m²)",
     ylab = "Frequency",
     xlim = range(d$BMI))

hist(d$BMI[d$Dementia == "Yes"], 
     breaks = 20, 
     col = alpha("red", 0.5), 
     border = "red",
     add = TRUE)

qqnorm(d$BMI)
qqline(d$BMI, col = "red")

# 3. Age
hist(d$Age[d$Dementia == "No"], 
     breaks = 15, 
     col = alpha("blue", 0.5), 
     border = "blue",
     main = "Age Distribution by Dementia Status",
     xlab = "Age (years)",
     ylab = "Frequency",
     xlim = range(d$Age))

hist(d$Age[d$Dementia == "Yes"], 
     breaks = 15, 
     col = alpha("red", 0.5), 
     border = "red",
     add = TRUE)

qqnorm(d$Age)
qqline(d$Age, col = "red")

# 4. Sleep (continuous)
hist(d$Sleep[d$Dementia == "No"], 
     breaks = seq(min(d$Sleep)-0.5, max(d$Sleep)+0.5, by = 1), 
     col = alpha("blue", 0.5), 
     border = "blue",
     main = "Sleep Duration by Dementia Status",
     xlab = "Sleep (hours)",
     ylab = "Frequency",
     xlim = range(d$Sleep))

hist(d$Sleep[d$Dementia == "Yes"], 
     breaks = seq(min(d$Sleep)-0.5, max(d$Sleep)+0.5, by = 1), 
     col = alpha("red", 0.5), 
     border = "red",
     add = TRUE)

qqnorm(d$Sleep)
qqline(d$Sleep, col = "red")
dev.off()

# ===== Figure 2: categorical plots =====

# ===== ggplot2 version (更專業) =====
# ===== Function to create ggplot stacked bar chart =====
# ===== Function to create ggplot stacked bar chart =====
create_ggplot_stacked <- function(var, var_name, colors) {
  
  # Prepare data
  plot_data <- data.frame(
    Variable = var,
    Group = c(
      rep("Overall", length(var)),
      ifelse(d$Dementia == "Yes", "Dementia\n(Yes)", NA),
      ifelse(d$Dementia == "No", "No Dementia\n(No)", NA)
    )
  ) %>%
    filter(!is.na(Group)) %>%
    group_by(Group, Variable) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Group) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Order groups
  plot_data$Group <- factor(plot_data$Group, 
                            levels = c("Overall", "Dementia\n(Yes)", "No Dementia\n(No)"))
  
  # Create plot
  p <- ggplot(plot_data, aes(x = Group, y = Percentage, fill = Variable)) +
    geom_bar(stat = "identity", color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", Percentage)),
              position = position_stack(vjust = 0.5),
              color = "white", fontface = "bold", size = 4) +
    scale_fill_manual(values = colors, name = var_name) +
    scale_y_continuous(limits = c(0, 100), 
                       breaks = seq(0, 100, 25),
                       expand = c(0, 0)) +
    labs(title = paste("Distribution of", var_name),
         x = "",
         y = "Percentage (%)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# ===== Create individual plots =====
# 1. Irritability
p1 <- create_ggplot_stacked(d$Irritability, 
                            "Irritability", 
                            colors = c("No" = "blue", "Yes" = "red"))
p1
# 2. Physical Activity
p2 <- create_ggplot_stacked(d$Physical_activity, 
                            "Physical Activity", 
                            colors = c("≤15 min" = "blue", ">15 min" = "red"))
p2
# 3. Smoking
p3 <- create_ggplot_stacked(d$Smoking, 
                            "Smoking Status", 
                            colors = c("Never" = "blue", 
                                       "Ex-smoker" = "#00A087", 
                                       "Current" = "red"))
p3
# 4. Dementia Diagnosis
p4 <- create_ggplot_stacked(d$Dementia_diag, 
                            "Dementia Diagnosis", 
                            colors = c("No" = "blue", "Yes" = "red"))
p4


# Save individual plots
ggsave("Q1_Physical_Activity.png", p2, width = 6, height = 5)
ggsave("Q1_Smoking.png", p3, width = 6, height = 5)
ggsave("Q1_Dementia_Diagnosis.png", p4, width = 6, height = 5)

table(d$Dementia, d$Irritability)

# 手動創建煩躁數據（根據您的 table 結果）
irr_data <- data.frame(
  Group = c("Overall", "Overall", 
            "Dementia\n(Yes)", "Dementia\n(Yes)",
            "No Dementia\n(No)", "No Dementia\n(No)"),
  Irritability = c("No", "Yes", 
                   "No", "Yes",
                   "No", "Yes"),
  n = c(709, 288,  # Overall: 669+40=709, 276+12=288
        40, 12,     # Dementia Yes
        669, 276),  # Dementia No
  Percentage = c(71.1, 28.9,  # Overall
                 76.9, 23.1,  # Dementia Yes: 40/52, 12/52
                 70.8, 29.2)  # Dementia No: 669/945, 276/945
)

# 設定分組因子順序
irr_data$Group <- factor(irr_data$Group, 
                         levels = c("Overall", "Dementia\n(Yes)", "No Dementia\n(No)"))

# 設定 Irritability 因子順序（確保 No 在上，Yes 在下）
irr_data$Irritability <- factor(irr_data$Irritability, 
                                levels = c("No", "Yes"))
# 繪製圖表 - 使用 linewidth 代替 size
p_irr <- ggplot(irr_data, aes(x = Group, y = Percentage, fill = Irritability)) +
  geom_bar(stat = "identity", color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 5) +
  scale_fill_manual(values = c("No" = "#0000FF", "Yes" = "#FF0000")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(title = "Distribution of Irritability",
       x = "", y = "Percentage (%)", fill = "Irritability") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right",
    panel.grid.major.x = element_blank()
  )

# 顯示圖表
print(p_irr)
ggsave("Q1_Irritability.png",p_irr, width = 6, height = 5)

# ========================== 2. HDL & smoker ===========================
# Table 2: HDL descriptive statistics by smoking group==================
library(knitr)
library(kableExtra)

hdl_table <- d %>%
  mutate(Smoking = factor(Smoking, 
                          levels = c(0, 1, 2),
                          labels = c("Non-smoker", "Ex-smoker", "Current smoker"))) %>%
  group_by(Smoking) %>%
  summarise(
    N = n(),
    `Median (IQR)` = sprintf("%.2f (%.2f - %.2f)", 
                             median(HDL), 
                             quantile(HDL, 0.25), 
                             quantile(HDL, 0.75)),
    Min = round(min(HDL), 2),
    Max = round(max(HDL), 2)
  )

kable(hdl_table,
      caption = "Table 2. HDL Cholesterol Levels by Smoking Status",
      align = c('l', 'c', 'c', 'c', 'c')) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE)

# Assumption 1: Independence =============================================
# study design: observations are independent patients from Greater Manchester. No repeated or clustering mentioned.

# Assumption 2: Normality
# H0: Data are normally distributed
# H1: Data are not normally distributed
par(mfrow = c(1,3), oma=c(2,0,2,0))
qqnorm(d$HDL[d$Smoking==0], main="Non-smoker")
qqline(d$HDL[d$Smoking==0], col = "red", lwd = 2)
qqnorm(d$HDL[d$Smoking==1], main="Ex-smoker")
qqline(d$HDL[d$Smoking==1], col = "red", lwd = 2)
qqnorm(d$HDL[d$Smoking==2], main="Current smoker")
qqline(d$HDL[d$Smoking==2], col = "red", lwd = 2)
mtext("Figure 1. QQ plot of HDL by Smoking Status", side=1, line=1, outer=TRUE, cex=1.2, font=2)

dev.off()

ggplot(d, aes(x = HDL, fill = factor(Smoking), color = factor(Smoking))) +
  geom_density(alpha = 0.3, size = 1.2) +
  labs(
    title = NULL,
    caption = "Figure 2. The histogram of HDL under different smoking status.",
    x = "HDL (mmol/L)",
    y = "Density",
    fill = "Smoking",
    color = "Smoking"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("0" = "#ffcccc", "1" = "#91eb97", "2" = "#99ccff")
  ) +
  scale_color_manual(
    values = c("0" = "#e41a1c", "1" = "#4daf4a", "2" = "#377eb8")
  )


by(d$HDL, d$Smoking, shapiro.test)

# Assumption 3: Homogeneity of variance  -> ANOVA
# H0: σ²(Non-smoker) = σ²(Ex-smoker) = σ²(Current smoker)
# H1: At least one variance differs
library(car)
leveneTest(HDL ~ factor(Smoking), data=d)

# Kruskal–Wallis ==================================================
# H0: μ(Non-smoker) = μ(Ex-smoker) = μ(Current smoker) mean of HDL is same in non, current, pre- smokers
# H1: At least one group mean differs
kruskal.test(HDL ~ Smoking, data = d)

# buz p closed to 0.05, pairwise adjust
pairwise.wilcox.test(d$HDL, d$Smoking, p.adjust.method = "bonferroni")

# Add to your analysis:
library(DescTools)
MedianCI(d$HDL[d$Smoking == 0], conf.level = 0.95)
MedianCI(d$HDL[d$Smoking == 1], conf.level = 0.95)
MedianCI(d$HDL[d$Smoking == 2], conf.level = 0.95)

# box plot =========================================
# 1. 準備資料標籤
d3<- read.csv("C:/Users/Yu-Zhen Chou/Desktop/Files/02 Master/05 HDS/IIDS67631 Statistics for Health Data Science/04. Assessment 2/SHDS_Assessment2_data.csv")

d_plot <- d3 %>%
  mutate(
    Smoking_label = factor(Smoking, 
                           levels = c(0, 1, 2),
                           labels = c("Non-smoker", "Ex-smoker", "Current smoker"))
  )

# 2. 計算統計量（用於標註）
smoking_stats <- d_plot %>%
  group_by(Smoking_label) %>%
  summarise(
    n = n(),
    median = median(HDL),
    q1 = quantile(HDL, 0.25),
    q3 = quantile(HDL, 0.75)
  )

# 3. 繪製 Figure 3
fig3 <- ggplot(d_plot, aes(x = Smoking_label, y = HDL, fill = Smoking_label)) +
  # Boxplot
  geom_boxplot(alpha = 0.7, 
               outlier.colour = "red", 
               outlier.shape = 16,
               outlier.size = 2) +
  
  # 加入資料點（jitter）
  geom_jitter(alpha = 0.3, width = 0.15, size = 1.5, color = "gray30") +
  
  # 標註樣本數
  geom_text(data = smoking_stats,
            aes(x = Smoking_label, 
                y = min(d$HDL) - 0.15,
                label = paste0("n = ", n)),
            size = 3.5,
            inherit.aes = FALSE) +
  
  # 標題和軸標籤
  labs(
    title = "",  # 標題放在下面caption
    x = "Smoking Status",
    y = "HDL Cholesterol (mmol/L)",
    caption = "Figure 2. HDL cholesterol levels across smoking groups. \nBoxes represent median and interquartile range; red dots are outliers."
  ) +
  
  # 主題設定
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic", margin = margin(t = 10)),
    plot.caption.position = "plot",
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10)
  ) +
  
  # 顏色設定
  scale_fill_brewer(palette = "Set2") +
  
  # Y軸範圍（留空間給標註）
  scale_y_continuous(limits = c(min(d$HDL) - 0.3, max(d$HDL) + 0.2),
                     breaks = seq(0, 4, 0.5))

# 顯示圖形
print(fig3)

# 儲存高解析度圖片
ggsave("Q2_f1_HDL_Smoking.png", 
       plot = fig3, 
       width = 8, 
       height = 6, 
       dpi = 300,
       bg = "white")



# ============================ 3. risk factors for dementia ==========================
# Logistic Regression
# Assumptions 1. Outcomes are independent observations.
# Assumptions 2. The response variable Yi is not normally distributed. Dementia is binomial.
# Assumptions 3. The relationship between the transformed response by logit and the explanatory variables is linear.
# Assumptions 4. The random errors are independent but not normally distributed.
# Assumptions 5. Under large sample size, the parameters are estimated by maximum likelihood estimation (MLE). 

### 3.1. Full Model Analysis =======
d2 <- d[ , !(names(d) %in% c("ID","Dementia_diag"))]
d2$Irritability <- factor(d2$Irritability, levels = c(0, 1), labels = c("No", "Yes"))

# Smoking: keep as ordered factor (0, 1, 2)
d2$Smoking <- factor(d2$Smoking, 
                     levels = c(0, 1, 2), 
                     labels = c("Non-smoker", "Ex-smoker", "Current"))

# Physical_activity: 0 = <=15 min, 1 = >15 min
d2$Physical_activity <- factor(d2$Physical_activity, 
                               levels = c(0, 1), 
                               labels = c("<=15 min", ">15 min"))

# Dementia: 0 = No, 1 = Yes (keep as numeric for glm)
d2$Dementia <- as.numeric(d2$Dementia)
full_m <- glm(Dementia ~ ., data = d2, family = binomial(link = "logit"))
summary(full_m)

null_m <- glm(Dementia ~ 1, data=d2, family="binomial")
n <- nrow(d)

### 3.2. Model selection ================================================================
# AIC
aic_model <- stepAIC(null_m, scope=list(lower=null_m, upper=full_m),direction="both", k = 2)
summary(aic_model)

### Model Comparison (LRT)
# H0: The bic_model fits the data as well as the full model
anova(full_m, aic_model, test='LRT')
# H0: The null model fits the data as well as the bic_model
anova(null_m, aic_model, test="LRT")

### 3.3  Model Diagnostics ===========================================================
par(mfrow=c(2,2), oma = c(2, 0, 0, 0))
plot(aic_model)
mtext("Figure 3. Diagnostic Plots for Selected Model", 
      side = 1, line = 0, outer = TRUE, cex = 1.2, font = 0.6)
dev.off()
# Cook's distance
cooksd <- cooks.distance(aic_model)
plot(cooksd, type="h", main="Cook's Distance",
     ylab="Cook's Distance", xlab="Observation Index")
abline(h=4/nrow(d), col="red", lty=8)
text(x=1:length(cooksd), y=cooksd, 
     labels=ifelse(cooksd > 4/nrow(d), names(cooksd), ""), 
     pos=3, cex=0.7)
mtext("Figure 4. Cook's distance Plot for Selected Model", side=1, line=4, cex=1, font=2)

# check the feature of influential points
influential <- which(cooksd > 4/nrow(d))
influential_points <- d[influential, ]
summary(influential_points)

table(d$Dementia)
table(d[-influential, ]$Dementia)
# All influential points identified have Dementia = 1, indicating all influential cases belong to the disease group."
# Check which group influential points belong to 
table(d[influential, "Dementia"])

### 3.4. Interpret Coefficients (Odds Ratios)============
final_or <- exp(coef(aic_model))
final_ci <- exp(confint(aic_model))
results_table <- data.frame(
  OR = final_or,
  CI_lower = final_ci[,1],
  CI_upper = final_ci[,2],
  p_value = summary(aic_model)$coefficients[,4]
)
results_table <- results_table[-1, ] #intercept
results_table$significance <- case_when(
  results_table$p_value < 0.001 ~ "***",
  results_table$p_value < 0.01 ~ "**",
  results_table$p_value < 0.05 ~ "*",
  TRUE ~ ""
)
results_table$CI_95 <- sprintf("(%.2f - %.2f)", 
                               results_table$CI_lower, 
                               results_table$CI_upper)
print(results_table)

final_table2 <- data.frame(
  Variable = rownames(results_table),
  OR = sprintf("%.2f", results_table$OR),
  CI_95 = results_table$CI_95,
  p_value = results_table$p_value,
  Sig = results_table$significance
)
print(final_table2)

kbl_tb4 <- kable(final_table2,
                 col.names = c("Variable", "OR", "95% CI", "p-value", ""),
                 caption = "Table 3. Risk Factors for selected module") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE) %>%
  footnote(general = "* p<0.05; ** p<0.01; *** p<0.001")
print(kbl_tb4)
save_kable(kbl_tb4, file = "Q3_t3_OR.png")



# ===== 多的 ======
# 1. the relation between each variable and Dementia 
# H₀: β₁ = 0 (no association between X and dementia)
# H₁: β₁ ≠ 0

univariate_results <- data.frame()
for(var in names(predictors)) {
  formula <- as.formula(paste("Dementia ~", var))
  m <- glm(formula, data=d, family="binomial")
  
  coef_summary <- summary(m)$coefficients[2,]
  or <- exp(coef(m)[2])
  ci <- exp(confint(m)[2,])
  
  univariate_results <- rbind(univariate_results, data.frame(
    Variable = var,
    OR = or,
    CI_lower = ci[1],
    CI_upper = ci[2],
    p_value = coef_summary[4]
  ))
}
row.names(univariate_results) <- NULL
print(univariate_results)

kbl_tb2 <- kable(univariate_results, 
                 format = "html",
                 digits = 3,
                 col.names = c("Variable", "OR", "95% CI Lower", "95% CI Upper", "p-value"),
                 caption = "Table 3. Univariate Logistic Regression Results") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  row_spec(0, bold = TRUE) %>% # title
  row_spec(which(univariate_results$p_value < 0.05), 
           bold = TRUE, color = "white", background = "#4472C4")

save_kable(kbl_tb2, file = "Q3_t3_univar.png")


# correlation between factors: Heatmap
library(ggplot2)
library(reshape2)
library(corrplot)
dev.off()

# All factors correlation
melted_cor <- melt(cor_matrix)

ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +  # 加數字
  scale_fill_gradient2(low = "#6D9EC1", mid = "white", high = "#E46726",
                       midpoint = 0, limit = c(-1, 1),
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Correlation Heatmap of Risk Factors") +
  coord_fixed()  # 保持方形格子

# Dementia and risk factors
# Line bar
cor_with_dementia <- cor(numeric_vars[, -8], numeric_vars$Dementia)
cor_df <- data.frame(
  Variable = rownames(cor_with_dementia),
  Correlation = cor_with_dementia[,1]
)

ggplot(cor_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", aes(fill = Correlation)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Correlation with Dementia",
       x = "Variables", y = "Correlation Coefficient")

# Heatmap
cor_subset <- cor_matrix["Dementia", , drop = FALSE] 
melted_cor <- melt(cor_subset)
melted_cor <- melted_cor[as.character(melted_cor$Var1) != as.character(melted_cor$Var2), ]
ggplot(melted_cor, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limit = c(-0.2, 0.2), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = "Correlation with Dementia") +
  coord_fixed()

# 2. Multicollinearity assessment: VIF 
vif_values <- vif(full_m)
print(vif_values)
kbl_tb3 <- kable(vif_values, 
                 format = "html",
                 digits = 3,
                 col.names = names(vif_values),
                 caption = "Table 4. Variance Inflation Factor") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  row_spec(0, bold = TRUE) %>% # title)
  print(kbl_tb3)
save_kable(kbl_tb3, file = "Q3_t4_vif.png")


# Refit model without influential points
d_clean <- d[-influential, ]
model_sensitivity <- glm(Dementia ~ Age + BMI, 
                         family = binomial, 
                         data = d_clean)
summary(model_sensitivity)
library(knitr)
comparison <- data.frame(
  Variable = c("Age", "BMI"),
  Original_OR = exp(coef(bic_model)[-1]),
  Sensitivity_OR = exp(coef(model_sensitivity)[-1]),
  Change_pct = (exp(coef(model_sensitivity)[-1]) - 
                  exp(coef(bic_model)[-1])) / 
    exp(coef(bic_model)[-1]) * 100
)

kable(comparison, digits = 3)


# ============================== 4. reliable the dementia diagnosis ======================

library(knitr)
library(kableExtra)

# 1. Matrix ==================================================================
conf_matrix <- table(Predicted = d$Dementia_diag, True = d$Dementia)

# Create matrix with row and column names
conf_matrix_labeled <- conf_matrix
rownames(conf_matrix_labeled) <- c("Negative (No Dementia)", 
                                   "Positive (Dementia)")
colnames(conf_matrix_labeled) <- c("No Dementia", "Dementia")

# Add marginal totals
conf_with_totals <- addmargins(conf_matrix_labeled)

kable(conf_with_totals,
      caption = "Table 4. Confusion Matrix: Clinical Diagnosis vs True Status",
      align = 'c',
      col.names = c("No Dementia", "Dementia", "Total")) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Clinical Diagnosis" = 1, "True Dementia Status" = 3)) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)
# plot ==========
agreement_data <- data.frame(
  Diagnosis = d$Dementia_diag,
  True_Status = d$Dementia,
  Agreement = ifelse(d$Dementia_diag == d$Dementia, "Correct", "Incorrect")
)

library(dplyr)
library(ggplot2)
library(scales)

# proportion
plot_df <- agreement_data %>%
  group_by(True_Status, Diagnosis) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(True_Status) %>%
  mutate(prop = n/sum(n))

ggplot(plot_df, aes(x = factor(True_Status), y = prop, fill = factor(Diagnosis))) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = percent(prop, accuracy = 0.1)),
    position = position_fill(vjust = 0.5),
    color = "black",
    fontface = "bold",
    size = 3
  ) +
  scale_fill_manual(
    values = c("0" = "#00BFC4", "1" = "#F8766D"),
    labels = c("Negative", "Positive"),
    name = "Diagnosis"
  ) +
  scale_x_discrete(labels = c("No Dementia", "Dementia")) +
  labs(
    caption = "Figure 5. Diagnostic Agreement by True Dementia Status",
    x = "True Dementia Status",
    y = "Proportion"
  ) +
  theme_minimal()

# 2. diagnosis parameter ==============================================
TP <- sum(d$Dementia_diag == 1 & d$Dementia == 1)
TN <- sum(d$Dementia_diag == 0 & d$Dementia == 0)
FP <- sum(d$Dementia_diag == 1 & d$Dementia == 0)
FN <- sum(d$Dementia_diag == 0 & d$Dementia == 1)

sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
PPV <- TP / (TP + FP)
NPV <- TN / (TN + FN)
accuracy <- (TP + TN) / nrow(d)

# Function to calculate Wilson score CI
wilson_ci <- function(x, n, conf.level = 0.95) {
  p <- x / n
  z <- qnorm((1 + conf.level) / 2)
  
  denominator <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denominator
  margin <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denominator
  
  c(lower = center - margin, upper = center + margin)
}

# Calculate CIs
sens_ci <- wilson_ci(TP, TP + FN)
spec_ci <- wilson_ci(TN, TN + FP)
ppv_ci <- wilson_ci(TP, TP + FP)
npv_ci <- wilson_ci(TN, TN + FN)
acc_ci <- wilson_ci((TP + TN), nrow(d))

ci_str <- function(ci) sprintf("%.3f ~ %.3f", ci[1], ci[2])

diagnostic_metrics <- data.frame(
  Characteristics = c("Sensitivity", "Specificity", "PPV (Precision)", "NPV", "Accuracy"),
  Value = round(c(sensitivity, specificity, PPV, NPV, accuracy), 5),
  Percent = sprintf("%.1f%%", 100 * c(sensitivity, specificity, PPV, NPV, accuracy)),
  "95% CI" = c(ci_str(sens_ci), ci_str(spec_ci), ci_str(ppv_ci), ci_str(npv_ci), ci_str(acc_ci))
)

kable(diagnostic_metrics, digits = 5,
      align = c("l", "l", "l", "l"),
      col.names = c("Characteristics", "Value", "%", "95% CI"),
      caption = "Table 5. Diagnostic Test Performance Characteristics") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE)




# 加了 LR+/- 但沒採用 =========
# Calculate confusion matrix elements
TP <- sum(d$Dementia_diag == 1 & d$Dementia == 1)
TN <- sum(d$Dementia_diag == 0 & d$Dementia == 0)
FP <- sum(d$Dementia_diag == 1 & d$Dementia == 0)
FN <- sum(d$Dementia_diag == 0 & d$Dementia == 1)

# Calculate diagnostic metrics
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
PPV <- TP / (TP + FP)
NPV <- TN / (TN + FN)
accuracy <- (TP + TN) / nrow(d)
LR_positive <- sensitivity / (1 - specificity)
LR_negative <- (1 - sensitivity) / specificity

# Function to calculate Wilson score CI
wilson_ci <- function(x, n, conf.level = 0.95) {
  p <- x / n
  z <- qnorm((1 + conf.level) / 2)
  
  denominator <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denominator
  margin <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denominator
  
  c(lower = center - margin, upper = center + margin)
}

# Calculate CIs
sens_ci <- wilson_ci(TP, TP + FN)
spec_ci <- wilson_ci(TN, TN + FP)
ppv_ci <- wilson_ci(TP, TP + FP)
npv_ci <- wilson_ci(TN, TN + FN)
acc_ci <- wilson_ci((TP + TN), nrow(d))

# Helper function for CI string formatting
ci_str <- function(ci) sprintf("%.3f ~ %.3f", ci[1], ci[2])

# Create diagnostic metrics table
diagnostic_metrics <- data.frame(
  Characteristics = c("Sensitivity", "Specificity", "PPV (Precision)", "NPV", 
                      "Accuracy", "LR+", "LR-"),
  Value = round(c(sensitivity, specificity, PPV, NPV, accuracy, 
                  LR_positive, LR_negative), 5),
  Percent = c(sprintf("%.1f%%", 100 * c(sensitivity, specificity, PPV, NPV, accuracy)),
              "-", "-"),
  CI = c(ci_str(sens_ci), ci_str(spec_ci), ci_str(ppv_ci), 
         ci_str(npv_ci), ci_str(acc_ci), "-", "-")
)

kable(diagnostic_metrics, digits = 5,
      align = c("l", "l", "l", "l"),
      col.names = c("Characteristics", "Value", "%", "95% CI"),
      caption = "Table 5. Diagnostic Test Performance Characteristics") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE)


# Cohen's Kappa -> substantial agreement between disease & diagnosis ========
install.packages("irr")
library(irr)
kappa_result <- kappa2(data.frame(d$Dementia, d$Dementia_diag))
print(kappa_result)

# McNemar's Test（評估診斷偏差）
# H0: 診斷無系統性偏差（FP = FN）
mcnemar_test <- mcnemar.test(conf_matrix)
print(mcnemar_test)
# McNemar檢定結果[p = 0.0056]，顯示診斷[有]系統性偏差；
# back to matrix False Positive = 18 > False Negative = 4, 
# so tendent toward overdiagnosis.

