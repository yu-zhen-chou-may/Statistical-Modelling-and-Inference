# ====== class practice =======
install.packages("lme4")

data <- read.csv("C:/Users/Yu-Zhen Chou/Desktop/Files/02 Master/02 HDS_semester 1/IIDS67641 Statistical Modelling and Inference for Health/00. data/madras.csv")

ma2 <- glmer(Y~MONTH+AGE+GENDER+(1|ID), data = data, family = 'binomial')
summary(ma2)

ma3 <- glmer(Y~MONTH+AGE+GENDER+(MONTH|ID), data = data, family = 'binomial')
summary(ma3)

# Model 3b: + Interaction MONTH*GENDER
ma5 <- glmer(Y ~ MONTH*GENDER + AGE + (MONTH|ID), 
             data = data, family = 'binomial')

# Model 3c: + Interaction MONTH*AGE
ma6 <- glmer(Y ~ MONTH*AGE + GENDER + (MONTH|ID), 
             data = data, family = 'binomial')


# ====== Assessment 1 =======
library(lme4)
library(tidyverse)
library(ggplot2)
library(knitr)
library(kableExtra)
library(patchwork)  # 用於組合圖表

PD_dat <- read_rds("C:/Users/Yu-Zhen Chou/Desktop/Files/02 Master/02 HDS_semester 1/IIDS67641 Statistical Modelling and Inference for Health/00. data/PD_dat.RData")

# ====== 1. EDA ======
# missing data =====
sapply(PD_dat[, c("VAS", "Age_Baseline", "DAS28_Baseline",
                  "Visit_number", "ID")],
       function(x) sum(is.na(x)))
# extreme value =====
PD_dat %>%
  summarise(
    VAS_min   = min(VAS, na.rm = TRUE),
    VAS_max   = max(VAS, na.rm = TRUE),
    Age_min   = min(Age_Baseline, na.rm = TRUE),
    Age_max   = max(Age_Baseline, na.rm = TRUE),
    DAS28_min = min(DAS28_Baseline, na.rm = TRUE),
    DAS28_max = max(DAS28_Baseline, na.rm = TRUE)
  )

# scatter for VAS, ID = color =====
ggplot(PD_dat, aes(x = Visit_number, y = VAS,
                   group = ID, colour = as.factor(ID))) +
  geom_point(alpha = 0.5, size = 1) +
  geom_line(alpha = 0.5) +
  labs(x = "Visit number", y = "VAS",
       colour = "ID",
       caption = "Figure 1. The distribution of VAS over visits, each trajectory colored by patient ID."
       ) +
  theme_bw() +
  theme(legend.position = "none", # ID 太多時把圖例關掉
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 10),
        plot.caption.position = "plot"
  )

# --- (a) 線圖：每個病人的 VAS 軌跡 ---
p1a <- ggplot(PD_dat, aes(x = Visit_number, y = VAS, group = ID, color = factor(ID))) +
  geom_line(alpha = 0.4, linewidth = 0.5) +
  labs(
    title = "(a) Individual VAS trajectories over time coloured by ID",
    x = "Visit number",
    y = "VAS"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 10)
  ) +
  scale_x_continuous(breaks = 1:10)

# --- (b) 箱型圖：每次訪視的 VAS 分布 ---
p1b <- ggplot(PD_dat, aes(x = factor(Visit_number), y = VAS)) +
  geom_boxplot(fill = "blue", alpha = 0.7, outlier.size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "white") +
  labs(
    title = "(b) Box plots display VAS distribution at each visit",
    subtitle = "      White diamonds indicating means",
    x = "Visit number",
    y = "VAS"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 10))

# --- 組合圖表（上下排列，標題在下）---
figure1 <- (p1a / p1b) +
  plot_annotation(
    caption = "Figure 1. VAS progression over visits."
  ) &
  theme(plot.caption = element_text(hjust = 0.5, size = 9))  # 置中+調字體

print(figure1)


ggsave("03_F1_VAS_trajectories.png", figure1, width = 7, height = 8, dpi = 300)


# ======= 描述性統計表：每次訪視的 VAS ======
vas_summary <- PD_dat %>%
  group_by(Visit_number) %>%
  summarise(
    Mean = mean(VAS, na.rm = TRUE),
    SD = sd(VAS, na.rm = TRUE),
    Median = median(VAS, na.rm = TRUE),
    Q1 = quantile(VAS, 0.25, na.rm = TRUE),
    Q3 = quantile(VAS, 0.75, na.rm = TRUE),
    IQR = IQR(VAS, na.rm = TRUE),
    Min = min(VAS, na.rm = TRUE),
    Max = max(VAS, na.rm = TRUE),
    Range = paste0(round(Min, 1), " - ", round(Max, 1))
  ) %>%
  mutate(
    `Mean (SD)` = paste0(round(Mean, 1), " (", round(SD, 1), ")"),
    `Median (IQR)` = paste0(round(Median, 1), " (", round(Q1, 1), ", ", round(Q3, 1), ")")
  ) %>%
  select(Visit_number, `Mean (SD)`, `Median (IQR)`, Range)

# 顯示表格
print(vas_summary)

# 儲存為格式化表格
kable(vas_summary, 
      caption = "Table: VAS summary statistics by visit number",
      col.names = c("Visit", "Mean (SD)", "Median (IQR)", "Range"),
      align = c("c", "c", "c", "c"))

# DAS28 分組的 VAS 軌跡圖 =================================================================
PD_dat <- PD_dat %>%
  mutate(
    DAS28_group = case_when(
      DAS28_Baseline < 2.6 ~ "Remission (<2.6)",
      DAS28_Baseline >= 2.6 & DAS28_Baseline < 3.2 ~ "Low (2.6-3.2)",
      DAS28_Baseline >= 3.2 & DAS28_Baseline <= 5.1 ~ "Moderate (3.2-5.1)",
      DAS28_Baseline > 5.1 ~ "High (>5.1)"
    ),
    DAS28_group = factor(DAS28_group, 
                         levels = c("Remission (<2.6)", "Low (2.6-3.2)", 
                                    "Moderate (3.2-5.1)", "High (>5.1)"))
  )

# 計算每組在每次訪視的平均 VAS 和標準誤
vas_by_das28 <- PD_dat %>%
  group_by(DAS28_group, Visit_number) %>%
  summarise(
    mean_VAS = mean(VAS, na.rm = TRUE),
    se_VAS = sd(VAS, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# 計算每組的樣本數（只需要算一次）
das28_group_n <- PD_dat %>%
  filter(Visit_number == 1) %>%
  count(DAS28_group) %>%
  mutate(group_label = paste0(DAS28_group, "\n(n=", n, ")"))

# 計算每組的斜率（使用簡單線性回歸）
slope_by_group <- PD_dat %>%
  group_by(DAS28_group) %>%
  summarise(
    slope = coef(lm(VAS ~ Visit_number))[2],
    final_visit_mean = mean(VAS[Visit_number == 10], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(slope_label = paste0("β = ", sprintf("%.2f", slope)))

colnames(slope_by_group)
slope_by_group <- slope_by_group |>
  dplyr::rename(group_label = DAS28_group)
# 合併樣本數資訊到繪圖數據
order_groups <- vas_by_das28 |>
  dplyr::filter(Visit_number == 1) |>
  dplyr::arrange(mean_VAS) |>
  dplyr::pull(group_label)
vas_by_das28$group_label  <- factor(vas_by_das28$group_label,
                                    levels = order_groups)
slope_by_group$group_label <- factor(slope_by_group$group_label,
                                     levels = order_groups)
# 繪製分組軌跡圖（含信賴區間、樣本數和斜率）
figure_das28 <- ggplot(vas_by_das28, aes(x = Visit_number, y = mean_VAS, 
                                         color = group_label, fill = group_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_ribbon(aes(ymin = mean_VAS - 1.96*se_VAS, ymax = mean_VAS + 1.96*se_VAS),
              alpha = 0.2, color = NA) +
  # 添加斜率標籤在每條線的末端
  geom_text(data = slope_by_group, 
            aes(x = 10.3, y = final_visit_mean, label = slope_label),
            hjust = 0, size = 3.5, fontface = "bold", show.legend = FALSE,
            color = "black",          # 固定顏色
            inherit.aes = FALSE) +
  labs(
    x = "Visit number",
    y = "Mean VAS (95% CI)",
    color = "Disease Activity Group",
    fill = "Disease Activity Group",
    caption = "Figure 2. VAS trajectories by baseline DAS28 disease activity groups. 
Lines show mean VAS with 95% confidence intervals (shaded areas). Sample sizes are shown in parentheses. 
β represents the slope (rate of VAS change per visit) for each group, calculated from simple linear regression."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    legend.text     = element_text(size = 9),
    plot.caption    = element_text(hjust = 0.5, size = 9, face = "italic",
                                   margin = margin(t = 6))
  ) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 11)) +  # 延伸 x 軸以顯示標籤
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

print(figure_das28)


# 顯示各組樣本數
das28_group_n <- PD_dat %>%
  filter(Visit_number == 1) %>%
  count(DAS28_group)
print(das28_group_n)

# hist and QQ for age and DAS =====
base_dat <- PD_dat %>% filter(Visit_number == 1)
shapiro.test(base_dat$Age_Baseline)
shapiro.test(base_dat$DAS28_Baseline)


par(mfrow = c(1, 2), mar = c(4, 4, 3, 2), oma = c(3, 0, 0, 0))
## Age
hist(base_dat$Age_Baseline,
     breaks = 20,
     main = "Histogram of Age",
     xlab  = "Age at baseline",
     col   = "#E6CCFF",
     border = "white")

## DAS28
hist(base_dat$DAS28_Baseline,
     breaks = 20,
     main = "Histogram of DAS28",
     xlab  = "DAS28 at baseline",
     col   = "lightgreen",
     border = "white")

# 整頁 caption 在下方中央
mtext("Figure 3. The distribution of Age and DAS28 at baseline, each patient contributes only once.",
      side = 1, line = 1, outer = TRUE, cex = 0.9, font = 2)

dev.off()

# (1) Baseline Age vs Baseline VAS 的相關性

# 提取 baseline 數據
baseline_data <- PD_dat %>%
  filter(Visit_number == 1)

# 計算 Pearson 相關係數
cor_age_vas <- cor.test(baseline_data$Age_Baseline, baseline_data$VAS)
cor_age_vas
cat("\n=== Correlation: Age vs Baseline VAS ===\n")
cat("Pearson's r =", round(cor_age_vas$estimate, 3), "\n")
cat("95% CI: [", round(cor_age_vas$conf.int[1], 3), ",", 
    round(cor_age_vas$conf.int[2], 3), "]\n")
cat("p-value =", format.pval(cor_age_vas$p.value, digits = 3), "\n")

# 繪製散點圖
p_age_vas_baseline <- ggplot(baseline_data, aes(x = Age_Baseline, y = VAS)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "blue", fill = "lightblue") +
  geom_smooth(method = "loess", color = "red", linetype = "dashed", se = FALSE) +
  labs(
    title = "(a) Baseline Age vs Baseline VAS",
    subtitle = paste0("Pearson's r = ", round(cor_age_vas$estimate, 3), 
                      ", p ", ifelse(cor_age_vas$p.value < 0.001, "< 0.001", 
                                     paste0("= ", round(cor_age_vas$p.value, 3)))),
    x = "Age at baseline (years)",
    y = "VAS at baseline"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 10))


# (2) Age 對 VAS 隨時間變化的影響

# 將 Age 分組（用中位數或三分位數）
PD_dat <- PD_dat %>%
  mutate(
    Age_group = case_when(
      Age_Baseline < quantile(Age_Baseline, 0.33, na.rm = TRUE) ~ "Younger (<54 yrs)",
      Age_Baseline >= quantile(Age_Baseline, 0.33, na.rm = TRUE) & 
        Age_Baseline < quantile(Age_Baseline, 0.67, na.rm = TRUE) ~ "Middle (54-67 yrs)",
      Age_Baseline >= quantile(Age_Baseline, 0.67, na.rm = TRUE) ~ "Older (≥67 yrs)"
    ),
    Age_group = factor(Age_group, levels = c("Younger (<54 yrs)", 
                                             "Middle (54-67 yrs)", 
                                             "Older (≥67 yrs)"))
  )

# 計算各年齡組的 VAS 軌跡
vas_by_age <- PD_dat %>%
  group_by(Age_group, Visit_number) %>%
  summarise(
    mean_VAS = mean(VAS, na.rm = TRUE),
    se_VAS = sd(VAS, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p_age_vas_trajectory <- ggplot(vas_by_age, aes(x = Visit_number, y = mean_VAS, 
                                               color = Age_group, fill = Age_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = mean_VAS - 1.96*se_VAS, ymax = mean_VAS + 1.96*se_VAS),
              alpha = 0.2, color = NA) +
  labs(
    title = "(b) VAS trajectories by age group",
    x = "Visit number",
    y = "Mean VAS (95% CI)",
    color = "Age Group",
    fill = "Age Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = 1:10)

# 組合兩張圖
figure_age_vas <- p_age_vas_baseline / p_age_vas_trajectory +
  plot_annotation(
    title = "Figure 3. Relationship between Age and VAS",
    theme = theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  )

print(figure_age_vas)

# correlation  =====
cor.test(base_dat$Age_Baseline,
         base_dat$DAS28_Baseline,
         use = "complete.obs",
         method = "pearson")
# 取 baseline 資料（假設 Visit_number == 1）
base_dat <- PD_dat[PD_dat$Visit_number == 1, ]
cor.test(base_dat$DAS28_Baseline,
         base_dat$VAS,
         use = "complete.obs",
         method = "pearson")
# for VAS and DAS
cor.test(base_dat$Age_Baseline, base_dat$DAS28_Baseline, use = "complete.obs")
cor.test(PD_dat$DAS28_Baseline[PD_dat$Visit_number==1], 
         PD_dat$VAS[PD_dat$Visit_number==1], 
         use = "complete.obs")
# Scatter plot with smooth line
ggplot(base_dat, aes(x = DAS28_Baseline, y = VAS)) +
  geom_point(alpha = 0.3) +
  #geom_smooth(method = "loess", color = "red", se = TRUE) +  # 非線性平滑線
  geom_smooth(method = "lm", color = "blue", se = TRUE, fill = "lightgrey",    # 誤差區間底色：淺灰
              alpha = 0.7) +   # 線性線
  labs( x = "Baseline DAS28",                  # X 軸名稱
        y = "VAS (visit 1)",
    caption = "Figure 4. Correlation between baseline DAS28 and VAS (visit 1). (Blue: Linear fit)") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0.5,      # 置中
                                    face = "italic",  # 斜體（期刊常規範）
                                    size = 11,        # 適當字體大小
                                    margin = margin(t = 15)))  # 與圖表間距


# 將 DAS28 加入平方項或分類變數來檢驗非線性
m_quadratic <- lmer(VAS ~ DAS28_Baseline + I(DAS28_Baseline^2) + 
                      Visit_number + Age_Baseline + 
                      (1 + Visit_number | ID), 
                    data = PD_dat)
anova(m0_rs, m_quadratic)

# ====== 2. LMM =======
# random intercept
m0 <- lmer(VAS ~ Age_Baseline +DAS28_Baseline +Visit_number + (1|ID), data = PD_dat )
summary(m0)

# 2. random slope for time
m1 <- lmer(VAS ~ Age_Baseline +DAS28_Baseline +Visit_number + (1+ Visit_number|ID), data = PD_dat )
summary(m1)

# 3. interaction: DAS effect VAS with time
m_das_int <- lmer(VAS ~ DAS28_Baseline*Visit_number+ Age_Baseline + (1+ Visit_number|ID), data = PD_dat )
summary(m_das_int)
confint(m_das_int,method="boot")

logLik(m0)
logLik(m1)
logLik(m_das_int)

# 4. define age is confounding or not
m_das_rs <- lmer(VAS ~ DAS28_Baseline + Visit_number + (1 + Visit_number | ID), data = PD_dat)
m0_rs <- lmer(VAS ~ Age_Baseline + DAS28_Baseline + Visit_number + (1 + Visit_number | ID), data = PD_dat)
# 使用 anova 進行 Likelihood Ratio Test
lrt_result <- anova(m_das_rs, m0_rs)
print(lrt_result)
summary(m_das_rs)
summary(m_das_rs)$coef["DAS28_Baseline", ] 
summary(m0_rs)$coef["DAS28_Baseline", ]
# 提取結果
delta_aic <- AIC(m_das_rs) - AIC(m0_rs)
p_value <- lrt_result$`Pr(>Chisq)`[2]

cat("\n=== Likelihood Ratio Test Results ===\n")
cat("ΔAIC =", round(delta_aic, 1), "\n")
cat("Chi-square =", round(lrt_result$Chisq[2], 2), "\n")
cat("df =", lrt_result$Df[2], "\n")
cat("p-value =", format.pval(p_value, digits = 3), "\n")

# 比較 DAS28 係數
cat("\n=== DAS28 Coefficient Comparison ===\n")
cat("Without Age:", round(fixef(m_das_rs)["DAS28_Baseline"], 3), "\n")
cat("With Age:", round(fixef(m0_rs)["DAS28_Baseline"], 3), "\n")
cat("Relative change:", 
    round(abs((fixef(m0_rs)["DAS28_Baseline"] - 
                 fixef(m_das_rs)["DAS28_Baseline"]) / 
                fixef(m_das_rs)["DAS28_Baseline"] * 100), 2), "%\n")



# Appendix: 年齡是否改變 VAS 的時間趨勢
m_age_int <- lmer(VAS ~ Age_Baseline*Visit_number +DAS28_Baseline + (1+ Visit_number|ID), data = PD_dat )
summary(m_age_int)

# ====== 3. module checking =======
## 1. 殘差 vs fitted ----
resid_das_int  <- resid(m_das_int)
fitted_das_int <- fitted(m_das_int)

plot(fitted_das_int, resid_das_int,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs fitted (m_das_int)")
abline(h = 0, col = "red", lwd = 2)

## 2. 殘差 QQ-plot ----
qqnorm(resid_das_int, main = "QQ-plot of residuals (m_das_int)")
qqline(resid_das_int, col = "red", lwd = 2)

## 3. random effects QQ-plot ----
re_das_int <- ranef(m_das_int)$ID
par(mfrow = c(1, 2))
# random intercept
qqnorm(re_das_int[,"(Intercept)"],
       main = "QQ-plot of random intercepts (ID)")
qqline(re_das_int[,"(Intercept)"], col = "red", lwd = 2)

# random slope (Visit_number)
qqnorm(re_das_int[,"Visit_number"],
       main = "QQ-plot of random slopes for Visit_number (ID)")
qqline(re_das_int[,"Visit_number"], col = "red", lwd = 2)

## 4. 殘差 vs time 與 DAS28 ----
plot(PD_dat$Visit_number, resid_das_int,
     xlab = "Visit number", ylab = "Residuals",
     main = "Residuals vs Visit_number (m_das_int)")
abline(h = 0, col = "red")

plot(PD_dat$DAS28_Baseline, resid_das_int,
     xlab = "Baseline DAS28", ylab = "Residuals",
     main = "Residuals vs DAS28_Baseline (m_das_int)")
abline(h = 0, col = "red")

dev.off()


# ==== 合圖 ======
## 以最終模型 m_das_int 為例
m_final <- m_das_int

resid_final  <- resid(m_final)
fitted_final <- fitted(m_final)

# ========== Figure A: Residual diagnostics ==========
png("Figure3_residual_diagnostics.png", width = 1600, height = 1200, res = 200)
par(mfrow = c(2, 2), mar = c(4,4,3,2), oma = c(3,0,0,0))

# 1. QQ-plot of residuals
qqnorm(resid_final, main = "QQ-plot of residuals")
qqline(resid_final, col = "red", lwd = 2)

# 2. Residuals vs fitted
plot(fitted_final, resid_final,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs fitted")
abline(h = 0, col = "red", lwd = 2)

# 3. Residuals vs visit number
plot(PD_dat$Visit_number, resid_final,
     xlab = "Visit number", ylab = "Residuals",
     main = "Residuals vs visit number")
abline(h = 0, col = "red", lwd = 2)

# 4. Residuals vs baseline DAS28
plot(PD_dat$DAS28_Baseline, resid_final,
     xlab = "Baseline DAS28", ylab = "Residuals",
     main = "Residuals vs DAS28_Baseline")
abline(h = 0, col = "red", lwd = 2)

mtext("Figure 5. Residual diagnostics", side = 1, line = 1, outer = TRUE, cex = 1.1, font = 2)
dev.off()

# ========== Figure B: Random-effects QQ plots ==========
re_final <- ranef(m_final)$ID

png("Figure5_random_effects_QQ.png", width = 1600, height = 600, res = 200)
par(mfrow = c(1, 2), mar = c(4,4,3,2), oma = c(3,0,0,0))

# random intercept
qqnorm(re_final[,"(Intercept)"],
       main = "QQ-plot of random intercepts (ID)")
qqline(re_final[,"(Intercept)"], col = "red", lwd = 2)

# random slope
qqnorm(re_final[,"Visit_number"],
       main = "QQ-plot of random slopes for Visit_number (ID)")
qqline(re_final[,"Visit_number"], col = "red", lwd = 2)

mtext("Figure 6. Random-effects QQ plots", side = 1, line = 1, outer = TRUE, cex = 1.1, font = 2)
dev.off()


# ===== 5. Clinical Significance =====
# Calculate effect for clinically relevant DAS28 change
coef_summary <- summary(m_das_int)$coefficients
das28_change <- 2  # moderate to high disease activity

effect_baseline <- das28_change * coef_summary["DAS28_Baseline", "Estimate"]
effect_interaction <- das28_change * coef_summary["DAS28_Baseline:Visit_number", "Estimate"]

# Effect at different time points
visits <- c(1, 5, 10)
for(v in visits) {
  total_effect <- effect_baseline + effect_interaction * v
  cat(sprintf("Visit %d: VAS difference = %.1f\n", v, total_effect))
}



# 從你的交互作用模型提取係數
coef_summary <- summary(m_das_int)$coefficients

# DAS28 的主效應
das28_main <- coef_summary["DAS28_Baseline", "Estimate"]
das28_interaction <- coef_summary["DAS28_Baseline:Visit_number", "Estimate"]

# 計算實際場景
# 假設 DAS28 從 3 增加到 5（臨床上中度到高度疾病活動）
das28_change <- 2

# 在 baseline (visit 1)
effect_baseline <- das28_change * das28_main
cat("At baseline, DAS28 change of 2 units → VAS change:", round(effect_baseline, 1), "\n")

# 在 visit 10
effect_visit10 <- das28_change * das28_main + 
  das28_change * das28_interaction * 10
cat("At visit 10, DAS28 change of 2 units → VAS change:", round(effect_visit10, 1), "\n")

# 計算 standardized effect size (如果需要)
# 提取 random effects 和 residual variance
vars <- as.data.frame(VarCorr(m_das_int))
total_var <- sum(vars$vcov)

# Cohen's d approximate
cohens_d <- das28_change * das28_main / sqrt(total_var)
cat("Approximate Cohen's d:", round(cohens_d, 2), "\n")


  
# TABLE 1: Baseline Characteristics =====================================================================
  
  # Extract baseline data only
  baseline_data <- PD_dat %>% 
    filter(Visit_number == 1) %>%
    select(ID, Age_Baseline, DAS28_Baseline, VAS)
  
  # Calculate summary statistics
  table1_data <- data.frame(
    Variable = c("Age (years)", "DAS28", "VAS"),
    `Mean (SD)` = c(
      sprintf("%.1f (%.1f)", mean(baseline_data$Age_Baseline), sd(baseline_data$Age_Baseline)),
      sprintf("%.2f (%.2f)", mean(baseline_data$DAS28_Baseline), sd(baseline_data$DAS28_Baseline)),
      sprintf("%.1f (%.1f)", mean(baseline_data$VAS), sd(baseline_data$VAS))
    ),
    `Median (IQR)` = c(
      sprintf("%.1f (%.1f, %.1f)", 
              median(baseline_data$Age_Baseline),
              quantile(baseline_data$Age_Baseline, 0.25),
              quantile(baseline_data$Age_Baseline, 0.75)),
      sprintf("%.2f (%.2f, %.2f)", 
              median(baseline_data$DAS28_Baseline),
              quantile(baseline_data$DAS28_Baseline, 0.25),
              quantile(baseline_data$DAS28_Baseline, 0.75)),
      sprintf("%.1f (%.1f, %.1f)", 
              median(baseline_data$VAS),
              quantile(baseline_data$VAS, 0.25),
              quantile(baseline_data$VAS, 0.75))
    ),
    Range = c(
      sprintf("%.1f - %.1f", min(baseline_data$Age_Baseline), max(baseline_data$Age_Baseline)),
      sprintf("%.2f - %.2f", min(baseline_data$DAS28_Baseline), max(baseline_data$DAS28_Baseline)),
      sprintf("%.1f - %.1f", min(baseline_data$VAS), max(baseline_data$VAS))
    ),
    check.names = FALSE
  )
  
  # Print Table 1
  cat("\n=== TABLE 1: Baseline Characteristics (N=500) ===\n\n")
  kable(table1_data, 
        format = "simple",
        align = c('l', 'c', 'c', 'c'),
        caption = "Table 1. Baseline characteristics of study participants") %>%
    print()
  
  # For LaTeX/Word output (better formatting):
  tab1_kbl <- kable(
    table1_data,
    col.names = c("Variable", "Mean (SD)", "Median (IQR)", "Range"),
    caption   = "Table 1. Baseline characteristics of participants (N = 500)"
  ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE
    )
  
  print(tab1_kbl)
  
  # 直接存成 PNG 圖片檔
  save_kable(tab1_kbl, file = "Table1_Baseline.png")
  
# TABLE 2: Model Comparison (AIC/BIC) =======================================================================
  
  # Fit all models (assuming you've already done this)
  # Random intercept only
  m_ri <- lmer(VAS ~ Age_Baseline + DAS28_Baseline + Visit_number + 
                 (1 | ID), data = PD_dat)
  
  # Random intercept + slope
  m_ris <- lmer(VAS ~ Age_Baseline + DAS28_Baseline + Visit_number + 
                  (1 + Visit_number | ID), data = PD_dat)
  
  # Random intercept + slope + interaction
  m_int <- lmer(VAS ~ Age_Baseline + DAS28_Baseline * Visit_number + 
                  (1 + Visit_number | ID), data = PD_dat)
  
  # Extract model fit statistics
  table2_data <- data.frame(
    Model = c(
      "Random intercept only",
      "Random intercept + slope",
      "Random intercept + slope + interaction"
    ),
    `Fixed Effects` = c(
      "Age + DAS28 + Visit",
      "Age + DAS28 + Visit",
      "Age + DAS28 × Visit"
    ),
    `Random Effects` = c(
      "(1 │ ID)",
      "(1 + Visit │ ID)",
      "(1 + Visit │ ID)"
    ),
    `Log-Likelihood` = c(
      sprintf("%.1f", logLik(m_ri)),
      sprintf("%.1f", logLik(m_ris)),
      sprintf("%.1f", logLik(m_int))
    ),
    AIC = c(
      sprintf("%.1f", AIC(m_ri)),
      sprintf("%.1f", AIC(m_ris)),
      sprintf("%.1f", AIC(m_int))
    ),
    BIC = c(
      sprintf("%.1f", BIC(m_ri)),
      sprintf("%.1f", BIC(m_ris)),
      sprintf("%.1f", BIC(m_int))
    ),
    check.names = FALSE
  )

table2_kbl <- kable(
    table2_data,
    col.names = c("Model", "Fixed Effects", "Random Effects",
                  "Log-Likelihood", "AIC", "BIC"),
    caption   = "Table 2. Comparison of linear mixed-effects models"
    ) %>%
    kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "hover")
    ) %>%
    footnote(
      general = "Lower AIC/BIC values indicate better model fit.",
      general_title = "Note:"
    )
  
  print(table2_kbl)
  save_kable(table2_kbl, file = "Table2_modelcompared.png")
  
# TABLE 3: Final Model Estimates with 95% CI =========================================================================

# 1. fixed effects
fixed_coefs <- fixef(final_model)
ci_fixed <- confint(final_model, parm = "beta_", method = "Wald", oldNames = FALSE)

table3_fixed <- data.frame(
  Parameter = c("Intercept",
                "Age (per year)",
                "DAS28 (per unit)",
                "Visit number (per visit)",
                "DAS28 × Visit number"),
  Estimate = sprintf("%.2f", fixed_coefs),
  `95% CI`  = sprintf("(%.2f, %.2f)", ci_fixed[, 1], ci_fixed[, 2]),
  check.names = FALSE
)

# 2. random effects
random_vars <- as.data.frame(VarCorr(final_model))
vc <- VarCorr(final_model)
sd_intercept <- attr(vc$ID, "stddev")[1]
sd_slope     <- attr(vc$ID, "stddev")[2]
cor_is       <- attr(vc$ID, "correlation")[1, 2]
sd_resid <- attr(vc, "sc")

table3_random <- data.frame(
  Parameter = c("Between-patient SD (intercept)",
                "Between-patient SD (slope for visit)",
                "Correlation (intercept–slope)",
                "Residual SD"),
  Estimate = c(
    sprintf("%.2f", c(sd_intercept, sd_slope, cor_is, sd_resid))
  ),
  `95% CI` = c("-", "-", "-", "-"),
  check.names = FALSE
)

# 3. 合併（沒有 Section 欄）
combined_table3 <- rbind(table3_fixed, table3_random)

combined_table3_kbl <- kable(
  combined_table3,
  col.names = c("Parameter", "Estimate", "95% CI"),
  caption   = "Table 3. Parameter estimates from the final linear mixed-effects model",
  align     = c("l", "r", "c")
) %>%
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped", "hover")) %>%
  pack_rows("Fixed Effects",  1, nrow(table3_fixed)) %>%
  pack_rows("Random Effects", nrow(table3_fixed) + 1, nrow(combined_table3))

combined_table3_kbl
save_kable(combined_table3_kbl, file = "Table3_final_model.png")


# 1. Fixed effects
table3_fixed2 <- data.frame(
  Parameter = c("Intercept",
                "Age (per year)",
                "DAS28 (per unit)",
                "Visit number (per visit)",
                "DAS28 × Visit number"),
  Estimate = sprintf("%.2f", fixed_coefs),
  `95% CI`  = sprintf("(%.2f, %.2f)", ci_fixed[,1], ci_fixed[,2]),
  Interpretation = c(
    "Expected VAS at baseline for reference values",
    "Change in VAS per 1-year increase in age",
    "Change in VAS per 1-unit increase in baseline DAS28",
    "Change in VAS per visit when baseline DAS28 = 0",
    "Additional change in VAS per visit for each 1-unit higher baseline DAS28"
  ),
  check.names = FALSE
)

# 2. Random effects
table3_random2 <- data.frame(
  Parameter = c("Between-patient SD (intercept)",
                "Between-patient SD (slope for visit)",
                "Correlation (intercept–slope)",
                "Residual SD"),
  Estimate = c(
    sprintf("%.2f", c(sd_intercept, sd_slope, cor_is, sd_resid))
  ),
  `95% CI` = c("-", "-", "-", "-"),
  Interpretation = c(
    "Variability in baseline VAS between patients",
    "Variability in VAS trajectories between patients",
    "Relationship between baseline level and trajectory",
    "Within-patient measurement variability"
  ),
  check.names = FALSE
)

# 3. 合併（不再有 Section 欄）
combined_table32 <- rbind(table3_fixed2, table3_random2)

# 使用 pack_rows 當作列標題
combined_table32_kbl <- kable(
  combined_table32,
  col.names = c("Parameter", "Estimate", "95% CI", "Interpretation"),
  caption   = "Table 3. Parameter estimates from the final linear mixed-effects model",
  align     = c("l", "r", "c", "l")
) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  pack_rows("Fixed Effects",  1, nrow(table3_fixed)) %>%
  pack_rows("Random Effects", nrow(table3_fixed) + 1, nrow(combined_table3))

combined_table32_kbl
save_kable(combined_table32_kbl, file = "Table3_final_model_with inter.png")


