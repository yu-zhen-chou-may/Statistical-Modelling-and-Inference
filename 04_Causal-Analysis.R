# install.packages("mice")
# install.packages("geepack")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(webshot2)  # 用於將 HTML 表格轉換為 PNG
library(patchwork)
library(survival)    # For survival analysis (exploratory)
library(survminer)
library(mice)        # Multiple imputation
library(cowplot)
library(MatchIt)     # Propensity score matching
library(skimr)
library(geepack)
heffpox_full <- read_csv("C:/Users/e83534yc/OneDrive - The University of Manchester/Documents/heffpox_2526.csv")
heffpox <- heffpox_full %>%
  filter(heffpox == 1)
# ===== I. Descriptive Analysis =====
# female   <- sum(heffpox$sex == 0, na.rm = TRUE)
# female
# TEVENT 等於 10 天的人數
n_10   <- sum(heffpox$TEVENT == 10, na.rm = TRUE)
n_all  <- sum(!is.na(heffpox$TEVENT))
prop_10 <- n_10 / n_all * 100
c(n_10 = n_10, prop_10 = prop_10)

sum(heffpox$diabetes)

skim(heffpox)
heffpox <- heffpox |>
  mutate(across(
    .cols = c(heffpox, sex, smoking, diabetes, milnepan, icu, Status, death7day),
    .fns  = as.factor
  ))

heffpox %>%
  count(milnepan) %>%                      # 計算各值的 n
  mutate(pct = 100 * n / sum(n))          # 換算成百分比

# ===== Table 1 =====
# ===== 前置：把 0/1 轉成文字
heffpox_lab <- heffpox |>
  mutate(
    milnepan  = factor(milnepan, levels = c(0, 1),
                       labels = c("No milnepan", "Milnepan")),
    sex       = factor(sex, levels = c(0, 1),
                       labels = c("Female", "Male")),
    smoking   = factor(smoking, levels = c(0, 1),
                       labels = c("No current smoker", "Smoker")),
    diabetes  = factor(diabetes, levels = c(0, 1),
                       labels = c("No", "Yes")),
    icu       = factor(icu, levels = c(0, 1),
                       labels = c("No", "Yes")),
    Status    = factor(Status, levels = c(0, 1),
                       labels = c("Censored", "Died")),
    death7day = factor(death7day, levels = c(0, 1),
                       labels = c("Alive ≥7 days", "Death <7 days"))
  )

# ===== 1. missing 比例（註腳用）
n_miss_bmi   <- sum(is.na(heffpox_lab$bmi))
miss_bmi     <- mean(is.na(heffpox_lab$bmi)) * 100
n_miss_smoke <- sum(is.na(heffpox_lab$smoking))
miss_smoke   <- mean(is.na(heffpox_lab$smoking)) * 100

# ===== 2. 連續變數的 Mean (SD)
cont_median_iqr <- function(x) {
  x  <- x[!is.na(x)]
  med <- median(x)
  q1  <- quantile(x, 0.25)
  q3  <- quantile(x, 0.75)
  sprintf("%.1f (%.1f–%.1f)", med, q1, q3)
}

# ===== 3. Sample size 行：顯示 n (%)
n_total       <- nrow(heffpox_lab)
n_no_milnepan <- sum(heffpox_lab$milnepan == "No milnepan", na.rm = TRUE)
n_milnepan    <- sum(heffpox_lab$milnepan == "Milnepan",    na.rm = TRUE)

sample_size_row <- tibble(
  Variable      = "Sample size",
  level         = "",
  Overall       = sprintf("%d (100.0%%)", n_total),
  `No milnepan` = sprintf("%d (%.1f%%)", n_no_milnepan,
                          100 * n_no_milnepan / n_total),
  `Milnepan`    = sprintf("%d (%.1f%%)", n_milnepan,
                          100 * n_milnepan / n_total)
)

# ===== 4. 描述統計：overall 與 by milnepan

## 4.1 連續變數：age, bmi, TEVENT
cont_vars <- c("age", "bmi", "TEVENT")

cont_tab <- lapply(cont_vars, function(v) {
  tibble(
    Variable      = v,
    Overall       = cont_median_iqr(heffpox_lab[[v]]),
    `No milnepan` = cont_median_iqr(heffpox_lab[[v]][heffpox_lab$milnepan == "No milnepan"]),
    `Milnepan`    = cont_median_iqr(heffpox_lab[[v]][heffpox_lab$milnepan == "Milnepan"])
  )
}) |> bind_rows()

## 4.2 類別變數
cat_vars <- c("sex", "smoking", "diabetes", "icu", "Status", "death7day")

make_cat_rows <- function(var_name) {
  dat <- heffpox_lab |>
    select(milnepan, all_of(var_name)) |>
    mutate(across(all_of(var_name),
                  forcats::fct_explicit_na, na_level = "Missing"))
  
  # overall
  overall <- dat |>
    count(!!sym(var_name)) |>
    mutate(
      pct = 100 * n / sum(n),
      Overall = sprintf("%d (%.1f%%)", n, pct)
    ) |>
    select(level = !!sym(var_name), Overall)
  
  # No milnepan
  g0 <- dat |>
    filter(milnepan == "No milnepan") |>
    count(!!sym(var_name)) |>
    mutate(
      pct = 100 * n / sum(n),
      `No milnepan` = sprintf("%d (%.1f%%)", n, pct)
    ) |>
    select(level = !!sym(var_name), `No milnepan`)
  
  # Milnepan
  g1 <- dat |>
    filter(milnepan == "Milnepan") |>
    count(!!sym(var_name)) |>
    mutate(
      pct = 100 * n / sum(n),
      `Milnepan` = sprintf("%d (%.1f%%)", n, pct)
    ) |>
    select(level = !!sym(var_name), `Milnepan`)
  
  full_join(overall, g0, by = "level") |>
    full_join(g1, by = "level") |>
    arrange(level) |>
    mutate(
      Variable = ifelse(row_number() == 1, var_name, ""),
      level    = as.character(level)
    ) |>
    relocate(Variable, level)
}

cat_tab <- lapply(cat_vars, make_cat_rows) |>
  bind_rows()

# ===== 5. 整理變數名稱

cont_tab2 <- cont_tab |>
  mutate(
    level = "",
    Variable = case_when(
      Variable == "age"    ~ "Age (years)*",
      Variable == "bmi"    ~ "BMI (kg/m^2)*",
      Variable == "TEVENT" ~ "Time-to-event (days)*",
      TRUE ~ Variable
    )
  ) |>
  select(Variable, level, Overall, `No milnepan`, `Milnepan`)

cat_tab2 <- cat_tab |>
  mutate(
    Variable = case_when(
      Variable == "sex"       ~ "Sex",
      Variable == "smoking"   ~ "Smoking status",
      Variable == "diabetes"  ~ "Diabetes",
      Variable == "icu"       ~ "ICU admission",
      Variable == "Status"    ~ "Vital status",
      Variable == "death7day" ~ "Death within 7 days",
      TRUE ~ Variable
    )
  )

# ===== 6. 合併成 Table 1 資料框
tab1_data_raw <- bind_rows(
  sample_size_row,
  cont_tab2,
  cat_tab2
)

# ===== 7. 插入區段標題列
# 找出 ICU admission 的位置（用 Variable 名稱）
idx_icu_raw <- which(tab1_data_raw$Variable == "ICU admission")[1]

# 建立區段列
row_demo <- tibble(
  Variable      = "Demographics and Baseline Characteristics",
  level         = "",
  Overall       = "",
  `No milnepan` = "",
  `Milnepan`    = ""
)

row_outcome <- tibble(
  Variable      = "Clinical Outcomes",
  level         = "",
  Overall       = "",
  `No milnepan` = "",
  `Milnepan`    = ""
)

# 在 sample size 後插入 row_demo，在 ICU admission 前插入 row_outcome
tab1_data <- bind_rows(
  tab1_data_raw[1, ],                      # 1: Sample size
  row_demo,                                # 2: Demographics 標題
  tab1_data_raw[2:(idx_icu_raw - 1), ],    # 3...: demographics 與 baseline 變數
  row_outcome,                             # Clinical Outcomes 標題
  tab1_data_raw[idx_icu_raw:nrow(tab1_data_raw), ]
)

# 重新抓兩個區段標題的 row index
idx_demo    <- which(tab1_data$Variable == "Demographics and Baseline Characteristics")
idx_outcome <- which(tab1_data$Variable == "Clinical Outcomes")

# Sample size 在第 1 列（因為第 0 列是表頭）
idx_sample  <- 1

tab1_kable <- tab1_data |>
  kable(
    format    = "html",
    col.names = c("Variable", "Category", "Overall", "No milnepan", "Milnepan"),
    caption   = "<b>Table 1.</b> Baseline characteristics and outcomes by treatment (milnepan)",
    escape    = FALSE,
    align     = c("l", "l", "c", "c", "c")
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = FALSE,
    font_size         = 12
  ) |>
  # 表頭
  row_spec(0, bold = TRUE, color = "white", background = "#4A90E2") |>
  # Sample size 底色 #FFF9E6
  row_spec(idx_sample, background = "#FFF9E6") |>
  # 兩個區段標題統一樣式
  row_spec(idx_demo,    bold = TRUE, background = "#E8F4F8") |>
  row_spec(idx_outcome, bold = TRUE, background = "#E8F4F8") |>
  footnote(
    general = sprintf(
      "BMI missing in %d patients (%.1f%%); smoking status missing in %d patients (%.1f%%).
      For continuous variables* show median (IQR); categorical variables indicate number (percentage).",
      n_miss_bmi, miss_bmi, n_miss_smoke, miss_smoke
    ),
    general_title = "Note:",
    threeparttable = TRUE
  )

tab1_kable

# 如需輸出圖片
kableExtra::save_kable(
  tab1_kable,
  file    = "Table1.png",
  zoom    = 2,
  density = 300
)

# ===== Fig2. Variables distribution =====
# ========== 1. 連續變數：age, BMI, TEVENT 的 histogram =====
heffpox$milnepan_f <- factor(
  heffpox$milnepan,
  levels = c(0, 1),
  labels = c("No treatment", "Treatment")
)
p_age <- heffpox |>
  ggplot(aes(x = age, fill = milnepan_f)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 30,
    position = "identity",
    alpha = 0.5,
    colour = "#1F77B4"
  ) +
  scale_fill_manual(
    values = c("No treatment" = "#9ecae1",
               "Treatment"    = "lightcoral"),
    name = NULL
  ) +
  labs(
    title = "Age (years)",
    x = NULL,
    y = "Density"
  ) +
  theme_minimal()
p_bmi <- heffpox |>
  ggplot(aes(x = bmi, fill = milnepan_f)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 30,
    position = "identity",
    alpha = 0.5,
    colour = "#1F77B4"
  ) +
  scale_fill_manual(
    values = c("No treatment" = "#9ecae1",
               "Treatment"    = "lightcoral"),
    name = NULL
  ) +
  labs(
    title = "BMI (kg/m^2)",
    x = NULL,
    y = "Density"
  ) +
  theme_minimal()
p_tevent <- heffpox |>
  ggplot(aes(x = TEVENT, fill = milnepan_f)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 30,
    position = "identity",
    alpha = 0.5,
    colour = "#1F77B4"
  ) +
  geom_vline(
    xintercept = 7,
    linetype = "dashed",
    colour = "blue",
    linewidth = 0.8
  ) +
  scale_fill_manual(
    values = c("No treatment" = "#9ecae1",
               "Treatment"    = "lightcoral"),
    name = NULL
  ) +
  labs(
    title = "Time-to-event (days)",
    x = "Days",
    y = "Density"
  ) +
  theme_minimal()
fig2 <- p_age / p_bmi / p_tevent
fig2


ggsave(
  filename = "Figure2_continuous_final.png",
  plot     = fig2,
  width    = 8,
  height   = 14,
  dpi      = 400
)


# ========== 2. 類別變數：sex, smoking status, diabetes======
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

# 確認 treatment factor
heffpox_lab$milnepan_f <- factor(
  heffpox_lab$milnepan,
  levels = c("No milnepan", "Milnepan"),
  labels = c("No treatment", "Treatment")
)

# ===== 1. 長格式，處理 NA & 簡短標籤 =====
cat_long_all <- heffpox_lab |>
  select(sex, smoking, diabetes, icu, Status, death7day, milnepan_f) |>
  pivot_longer(
    cols      = c(sex, smoking, diabetes, icu, Status, death7day),
    names_to  = "variable",
    values_to = "level"
  ) |>
  mutate(
    level = fct_recode(
      level,
      # Smoking
      "No"       = "No current smoker",
      "Smoker"      = "Smoker",
      # Sex
      "Female"   = "Female",
      "Male"     = "Male",
      # death7day
      "Alive ≥7" = "Alive ≥7 days",
      "Death <7" = "Death <7 days"
    )
  )

# ===== 2. 計算組內百分比 =====
cat_summary_all <- cat_long_all |>
  group_by(variable, milnepan_f, level) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(variable, milnepan_f) |>
  mutate(
    pct = 100 * n / sum(n)
  ) |>
  ungroup() |>
  mutate(
    variable = factor(
      variable,
      levels = c("sex", "smoking", "diabetes", "icu", "Status", "death7day"),
      labels = c("Sex", "Smoking status", "Diabetes", "ICU admission", "Vital status", "Death within 7 days")
    )
  )

# ===== 3. 繪圖 =====
fig2_2 <- ggplot(cat_summary_all,
                 aes(x = level, y = pct, fill = milnepan_f)) +
  geom_col(position = position_dodge(width = 0.8),     alpha = 0.5,
           colour = "#1F77B4") +
  labs(
    x = "Category",
    y = "Percentage",
    fill = NULL
  ) +
  scale_fill_manual(values = c("No treatment" = "#9ecae1",
                               "Treatment"    = "lightcoral")) +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(hjust = 0.5)
  )

fig2_2

# ===== II. Imputed Missing Data =====
# 總樣本數
N <- nrow(heffpox)
N
# 1) 完整資料：bmi & smoking 都不缺
n_complete <- sum(!is.na(heffpox$bmi) & !is.na(heffpox$smoking))
pct_complete <- n_complete / N * 100

# 2) 只缺 bmi：bmi 缺、smoking 不缺
n_miss_bmi_only <- sum(is.na(heffpox$bmi) & !is.na(heffpox$smoking))
pct_miss_bmi_only <- n_miss_bmi_only / N * 100

# 3) 只缺 smoking：smoking 缺、bmi 不缺
n_miss_smoke_only <- sum(!is.na(heffpox$bmi) & is.na(heffpox$smoking))
pct_miss_smoke_only <- n_miss_smoke_only / N * 100

# 4) 兩個都缺：bmi & smoking 都缺
n_miss_both <- sum(is.na(heffpox$bmi) & is.na(heffpox$smoking))
pct_miss_both <- n_miss_both / N * 100

cbind(
  pattern = c("complete", "miss_bmi_only", "miss_smoke_only", "miss_both"),
  n   = c(n_complete, n_miss_bmi_only, n_miss_smoke_only, n_miss_both),
  pct = round(c(pct_complete, pct_miss_bmi_only, pct_miss_smoke_only, pct_miss_both), 1)
)


# ===== Table 2 =====
heffpox_imp <- heffpox |>
  select(ID, sex, age, bmi, smoking, diabetes,
         milnepan, icu, TEVENT, Status, death7day)
mp <- md.pattern(heffpox_imp)

heffpox_imp <- heffpox_imp |>
  mutate(
    bmi_miss = ifelse(is.na(bmi), 1, 0),
    smoke_miss = ifelse(is.na(smoking), 1, 0)
  )

# bmi
cat_vars <- c("sex", "diabetes", "milnepan",
              "icu", "Status", "death7day", "smoking")

bmi_cat_tests <- lapply(cat_vars, function(v) {
  tab <- table(heffpox_imp$bmi_miss, heffpox_imp[[v]])
  test <- suppressWarnings(chisq.test(tab))
  data.frame(
    var    = v,
    stat   = unname(test$statistic),
    df     = unname(test$parameter),
    pvalue = unname(test$p.value)
  )
})

bmi_cat_tests <- do.call(rbind, bmi_cat_tests)
bmi_cat_tests

cont_vars <- c("age", "TEVENT")

bmi_cont_tests <- lapply(cont_vars, function(v) {
  x0 <- heffpox_imp[[v]][heffpox_imp$bmi_miss == 0]
  x1 <- heffpox_imp[[v]][heffpox_imp$bmi_miss == 1]
  test <- t.test(x0, x1)
  data.frame(
    var    = v,
    mean0  = mean(x0, na.rm = TRUE),
    mean1  = mean(x1, na.rm = TRUE),
    stat   = unname(test$statistic),
    df     = unname(test$parameter),
    pvalue = unname(test$p.value)
  )
})

bmi_cont_tests <- do.call(rbind, bmi_cont_tests)
bmi_cont_tests

# table for bmi
## 類別變數
bmi_cat_tbl <- bmi_cat_tests |>
  mutate(
    Variable = case_when(
      var == "sex"       ~ "Sex",
      var == "diabetes"  ~ "Diabetes",
      var == "milnepan"  ~ "Milnepan use",
      var == "icu"       ~ "ICU admission",
      var == "Status"    ~ "Vital status",
      var == "death7day" ~ "Death within 7 days",
      var == "smoking"   ~ "Smoking status",
      TRUE               ~ var
    ),
    Test           = "Chi-squared",
    p_raw          = pvalue,                           # 先存真正的 p
    Stars          = case_when(                        # 設定星號規則
      p_raw < 0.001 ~ "***",
      p_raw < 0.01  ~ "**",
      p_raw < 0.05  ~ "*",
      TRUE          ~ ""
    ),
    `Test statistic` = sprintf("%.2f", stat),
    df             = sprintf("%.0f", df),
    `p-value`      = ifelse(p_raw < 0.001,
                            "<0.001",
                            sprintf("%.3f", p_raw))
  ) |>
  select(Variable, Test, `Test statistic`, df, `p-value`, Stars)

## 連續變數
bmi_cont_tbl <- bmi_cont_tests |>
  mutate(
    Variable = case_when(
      var == "age"    ~ "Age (years)",
      var == "TEVENT" ~ "Time-to-event (days)",
      TRUE            ~ var
    ),
    Test     = "t-test",
    p_raw    = pvalue,
    Stars    = case_when(
      p_raw < 0.001 ~ "***",
      p_raw < 0.01  ~ "**",
      p_raw < 0.05  ~ "*",
      TRUE          ~ ""
    ),
    `Test statistic` = sprintf("%.2f", stat),
    df               = sprintf("%.0f", df),
    `p-value`        = ifelse(p_raw < 0.001,
                              "<0.001",
                              sprintf("%.3f", p_raw))
  ) |>
  select(Variable, Test, `Test statistic`, df, `p-value`, Stars)

## 合併
bmi_all_tbl <- bind_rows(bmi_cont_tbl, bmi_cat_tbl)

bmi_all_tbl |>
  kable(
    format    = "html",
    caption   = "A. Associations between BMI missingness and patient characteristics",
    col.names = c("Variable", "Test", "Test statistic", "df", "p-value", "Signif."),
    escape    = FALSE
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = FALSE,
    font_size         = 11
  ) |>
  row_spec(0, bold = TRUE, background = "#4A90E2", color = "white")

bmi_diab_tab <- heffpox_imp |>
  group_by(diabetes, bmi_miss) |>
  summarise(n = n(), .groups = "drop_last") |>
  mutate(
    total   = sum(n),
    percent = 100 * n / total
  ) |>
  arrange(diabetes, bmi_miss)
bmi_diab_tab

bmi_milne_tab <- heffpox_imp |>
  group_by(milnepan, bmi_miss) |>
  summarise(n = n(), .groups = "drop_last") |>
  mutate(
    total   = sum(n),
    percent = 100 * n / total
  ) |>
  arrange(milnepan, bmi_miss)
bmi_milne_tab

bmi_status_tab <- heffpox_imp |>
  group_by(Status, bmi_miss) |>
  summarise(n = n(), .groups = "drop_last") |>
  mutate(
    total   = sum(n),
    percent = 100 * n / total
  ) |>
  arrange(Status, bmi_miss)
bmi_status_tab

bmi_smoke_tab <- heffpox_imp |>
  group_by(smoking, bmi_miss) |>
  summarise(n = n(), .groups = "drop_last") |>
  mutate(
    total   = sum(n),
    percent = 100 * n / total
  ) |>
  arrange(smoking, bmi_miss)
bmi_smoke_tab

bmi_age_tevent_tab <- heffpox_imp |>
  group_by(bmi_miss) |>
  summarise(
    n       = n(),
    mean_age   = mean(age, na.rm = TRUE),
    sd_age     = sd(age, na.rm = TRUE),
    mean_tevent= mean(TEVENT, na.rm = TRUE),
    sd_tevent  = sd(TEVENT, na.rm = TRUE),
    .groups = "drop"
  )
bmi_age_tevent_tab


# =========== smoking
cat_vars2 <- c("sex", "diabetes", "milnepan",
               "icu", "Status", "death7day")

smoke_cat_tests <- lapply(cat_vars2, function(v) {
  tab <- table(heffpox_imp$smoke_miss, heffpox_imp[[v]])
  test <- suppressWarnings(chisq.test(tab))
  data.frame(
    var    = v,
    stat   = unname(test$statistic),
    df     = unname(test$parameter),
    pvalue = unname(test$p.value)
  )
})

smoke_cat_tests <- do.call(rbind, smoke_cat_tests)
smoke_cat_tests

cont_vars2 <- c("age", "TEVENT", "bmi")

smoke_cont_tests <- lapply(cont_vars2, function(v) {
  x0 <- heffpox_imp[[v]][heffpox_imp$smoke_miss == 0]
  x1 <- heffpox_imp[[v]][heffpox_imp$smoke_miss == 1]
  test <- t.test(x0, x1)
  data.frame(
    var    = v,
    mean0  = mean(x0, na.rm = TRUE),
    mean1  = mean(x1, na.rm = TRUE),
    stat   = unname(test$statistic),
    df     = unname(test$parameter),
    pvalue = unname(test$p.value)
  )
})

smoke_cont_tests <- do.call(rbind, smoke_cont_tests)
smoke_cont_tests

# table for smoking
## 類別變數
smoke_cat_tbl <- smoke_cat_tests |>
  mutate(
    Variable = case_when(
      var == "sex"       ~ "Sex",
      var == "diabetes"  ~ "Diabetes",
      var == "milnepan"  ~ "Milnepan use",
      var == "icu"       ~ "ICU admission",
      var == "Status"    ~ "Vital status",
      var == "death7day" ~ "Death within 7 days",
      TRUE               ~ var
    ),
    Test    = "Chi-squared",
    p_raw   = pvalue,
    Stars   = case_when(
      p_raw < 0.001 ~ "***",
      p_raw < 0.01  ~ "**",
      p_raw < 0.05  ~ "*",
      TRUE          ~ ""
    ),
    `Test statistic` = sprintf("%.2f", stat),
    df              = sprintf("%.0f", df),
    `p-value`       = ifelse(p_raw < 0.001,
                             "<0.001",
                             sprintf("%.3f", p_raw))
  ) |>
  select(Variable, Test, `Test statistic`, df, `p-value`, Stars)

## 連續變數
smoke_cont_tbl <- smoke_cont_tests |>
  mutate(
    Variable = case_when(
      var == "age"    ~ "Age (years)",
      var == "TEVENT" ~ "Time-to-event (days)",
      var == "bmi"    ~ "BMI (kg/m^2)",
      TRUE            ~ var
    ),
    Test   = "t-test",
    p_raw  = pvalue,
    Stars  = case_when(
      p_raw < 0.001 ~ "***",
      p_raw < 0.01  ~ "**",
      p_raw < 0.05  ~ "*",
      TRUE          ~ ""
    ),
    `Test statistic` = sprintf("%.2f", stat),
    df              = sprintf("%.0f", df),
    `p-value`       = ifelse(p_raw < 0.001,
                             "<0.001",
                             sprintf("%.3f", p_raw))
  ) |>
  select(Variable, Test, `Test statistic`, df, `p-value`, Stars)

## 合併輸出
smoke_all_tbl <- bind_rows(smoke_cont_tbl, smoke_cat_tbl)

smoke_all_tbl |>
  kable(
    format    = "html",
    caption   = "B. Associations between smoking status missingness and patient characteristics",
    col.names = c("Variable", "Test", "Test statistic", "df", "p-value", "Signif."),
    escape    = FALSE
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = FALSE,
    font_size         = 11
  ) |>
  row_spec(0, bold = TRUE, background = "#4A90E2", color = "white")
# ===== Detailed explain =====
levels(heffpox2$smoking)

heffpox2 <- heffpox |>
  mutate(
    bmi_miss     = is.na(bmi),
    smoke_miss   = is.na(smoking),
    diabetes     = as.factor(diabetes),
    milnepan     = as.factor(milnepan),
    death7day    = as.factor(death7day),
    smoking      = as.factor(smoking)
  )

## ---- (A) 比較：BMI 缺失 vs 不缺失 ----

# 1. 年齡平均數（mean age 50.0 vs 41.5）
age_by_bmi_miss <- heffpox2 |>
  group_by(bmi_miss) |>
  summarise(
    n    = n(),
    age_mean = mean(age, na.rm = TRUE),
    age_sd   = sd(age, na.rm = TRUE),
    .groups = "drop"
  )

# 2. 糖尿病：缺失 vs 不缺失中，diabetes=1 的比例（35.7% vs 7.7）
diab_by_bmi_miss <- heffpox2 |>
  group_by(bmi_miss) |>
  summarise(
    n          = n(),
    n_diab     = sum(diabetes == 1, na.rm = TRUE),
    pct_diab   = 100 * n_diab / n,
    .groups    = "drop"
  )

# 3. milnepan 使用：缺失 vs 不缺失中，milnepan=1 的比例（35.0% vs 30.7）
milnepan_by_bmi_miss <- heffpox2 |>
  group_by(bmi_miss) |>
  summarise(
    n             = n(),
    n_milnepan    = sum(milnepan == 1, na.rm = TRUE),
    pct_milnepan  = 100 * n_milnepan / n,
    .groups       = "drop"
  )

# 4. 死亡：缺失 vs 不缺失中，death7day=1 的比例（34.6% vs 31.2）
death_by_bmi_miss <- heffpox2 |>
  group_by(bmi_miss) |>
  summarise(
    n           = n(),
    n_death     = sum(death7day == 1, na.rm = TRUE),
    pct_death   = 100 * n_death / n,
    .groups     = "drop"
  )

# 5. 事件時間 TEVENT：缺失 vs 不缺失之平均（「shorter time to event」）
tevent_by_bmi_miss <- heffpox2 |>
  group_by(bmi_miss) |>
  summarise(
    n            = n(),
    tevent_mean  = mean(TEVENT, na.rm = TRUE),
    tevent_sd    = sd(TEVENT, na.rm = TRUE),
    .groups      = "drop"
  )

# 6. current smoking：缺失 vs 不缺失中 current smoker 的比例（35.1% vs 31.4）
heffpox2 <- heffpox2 |>
  mutate(
    current_smoker = if_else(smoking == "1", 1, 0, missing = NA_real_)
  )

smoke_by_bmi_miss <- heffpox2 |>
  group_by(bmi_miss) |>
  summarise(
    n                 = n(),
    n_current_smoker  = sum(current_smoker == 1, na.rm = TRUE),
    denom             = sum(!is.na(current_smoker)),
    pct_current       = 100 * n_current_smoker / denom,
    .groups           = "drop"
  )

smoke_by_bmi_miss


## ---- (B) 比較：smoking 缺失 vs 不缺失 ----

# 1. 年齡平均數（50.0 vs 44.0）
age_by_smoke_miss <- heffpox2 |>
  group_by(smoke_miss) |>
  summarise(
    n    = n(),
    age_mean = mean(age, na.rm = TRUE),
    age_sd   = sd(age, na.rm = TRUE),
    .groups = "drop"
  )

# 2. 糖尿病：缺失 vs 不缺失中 diabetes=1 的比例（4.9% vs 1.9）
diab_by_smoke_miss <- heffpox2 |>
  group_by(smoke_miss) |>
  summarise(
    n        = n(),
    n_diab   = sum(diabetes == 1, na.rm = TRUE),
    pct_diab = 100 * n_diab / n,
    .groups  = "drop"
  )

## ---- (C) 把各個 summary 合併成 Table 2 風格（可選）----

# 例：整理成一個長表，用來對照文字敘述
table2_missing <- list(
  age_by_bmi_miss,
  diab_by_bmi_miss,
  milnepan_by_bmi_miss,
  death_by_bmi_miss,
  tevent_by_bmi_miss,
  smoke_by_bmi_miss,
  age_by_smoke_miss,
  diab_by_smoke_miss
)

table2_missing


# ===== imputed method 1 =====
# 先讓 mice 幫你產生預設 method
ini <- mice(heffpox, maxit = 0)

meth <- ini$method
meth["bmi"]     <- "pmm"     # 連續變數，pmm
meth["smoking"] <- "logreg"  # 二元變數，邏輯斯

imp <- mice(
  heffpox,
  m      = 20,
  maxit  = 20,
  method = meth,
  seed   = 123
)
imp$method          # 每個變數用什麼 method

fit <- with(
  imp,
  coxph(Surv(TEVENT, Status == 1) ~ bmi + smoking + age + sex + diabetes + milnepan)
)
pool(fit)
summary(pool(fit), conf.int = TRUE, conf.level = 0.95)
imp$predictorMatrix # 每個變數的預測因子用哪些

# ===== impute method 2 (detailed) =====
dat_mi <- heffpox |>
  select(
    heffpox,
    sex,
    age,
    bmi,
    smoking,
    diabetes,
    milnepan, # exposure
    icu,
    TEVENT,
    Status,
    death7day
  )

ini  <- mice(dat_mi, maxit = 0, print = FALSE)
meth <- ini$method
pred <- ini$predictorMatrix

## 1. 只插補 bmi 和 smoking
meth[]          <- ""        # 先全部關掉
meth["bmi"]     <- "pmm"     # 連續
meth["smoking"] <- "logreg"  # 二元 0/1

## 2. predictor matrix：所有變數都可以預測 bmi / smoking
pred[,]    <- 1
diag(pred) <- 0              # 自己不能預測自己

## 3. 執行 MI
set.seed(2026)
imp2 <- mice(
  data            = dat_mi,
  m               = 10,
  method          = meth,
  predictorMatrix = pred,
  maxit           = 10,
  print           = FALSE
)
fit2 <- with(
  imp2,
  coxph(Surv(TEVENT, Status == 1) ~ bmi + smoking + age + sex + diabetes + milnepan)
)
pool(fit2)
summary(pool(fit2), conf.int = TRUE, conf.level = 0.95)

# ===== Figure 3 dignosis plot =====
## 4. MI 診斷圖 ---------------------------------------------------------

# 4.1 Trace plots：檢查收斂（各次迭代的插補值）
plot(imp2, c("bmi", "smoking"))

# 4.2 Density plots：觀察 vs 插補分佈
densityplot(imp2, ~ bmi)
densityplot(imp2, ~ smoking)

## 5. Cox 模型：在每一個插補資料集上配適 ------------------------------

fit2 <- with(
  imp2,
  coxph(Surv(TEVENT, Status == 1) ~
          bmi + smoking + age + sex + diabetes + milnepan)
)

## 6. Rubin’s rules：合併估計值（pooled estimates） --------------------

pooled_fit2 <- pool(fit2)

# 摘要（含 HR、95% CI）
summary(pooled_fit2, conf.int = TRUE, conf.level = 0.95)

## 7. 取一個完成資料集（例如第 1 個）作為範例 ---------------------------

complete1 <- complete(imp2, action = 1)

# 用 complete1 再跑一次 Cox，當作單一完成資料集的比較
fit_complete1 <- coxph(Surv(TEVENT, Status == 1) ~
                         bmi + smoking + age + sex + diabetes + milnepan,
                       data = complete1)

summary(fit_complete1)

# ===== imputed 3 =====
set.seed(5)
imp3 <- mice(
  data            = dat_mi,
  m               = 5,
  method          = meth,
  predictorMatrix = pred,
  maxit           = 5,
  print           = FALSE
)
fit3 <- with(
  imp3,
  coxph(Surv(TEVENT, Status == 1) ~ bmi + smoking + age + sex + diabetes + milnepan)
)
pool(fit3)
summary(pool(fit3), conf.int = TRUE, conf.level = 0.95)

# ===== Table 3 =====
tab_fit2 <- summary(pool(fit2), conf.int = TRUE, conf.level = 0.95) |>
  mutate(
    HR      = exp(estimate),
    HR_low  = exp(conf.low),
    HR_high = exp(conf.high),
    HR_CI   = sprintf("%.2f (%.2f–%.2f)", HR, HR_low, HR_high),
    p_star = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    p_display = sprintf("%.3f%s", p.value, p_star),
    term = recode(term,
                  "bmi"      = "BMI (per 1 kg/m²)",
                  "age"      = "Age (per 1 year)",
                  "smoking"  = "Current smoker (vs no)",
                  "sex"      = "Male (vs female)",
                  "diabetes" = "Diabetes (yes vs no)",
                  "milnepan" = "Milnepan (yes vs no)")
  ) |>
  select(
    Variable = term,
    estimate,
    std.error,
    HR_CI,
    p_display,
    p.value
  )

# p 不顯著 (p >= 0.05) 的列號（記得 +1 因為第 0 列是表頭）
non_sig_rows <- which(tab_fit2$p.value >= 0.05) + 1

table_output <- tab_fit2 |>
  select(-p.value) |>
  kbl(
    align    = c("l", "c", "c", "c", "c"),
    digits   = 3,
    col.names = c("Variable", "estimate", "std.error", "HR (95% CI)", "p value"),
    caption  = "Table 3. Cox Proportional Hazards Regression Results"
  ) |>
  kable_styling(
    bootstrap_options = c("hover"),
    full_width = FALSE,
    position   = "center",
    font_size  = 14
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#4A90E2")
table_output

# ===== III. Survival Analysis =====
# 1. Complete‑case Cox
cc_data <- heffpox_full

# 2. 建立 Surv 物件
surv_obj <- Surv(time = cc_data$TEVENT, event = cc_data$Status)

# 3. KM 曲線（以 milnepan 分組）
fit_km <- survfit(surv_obj ~ milnepan, data = cc_data)

# ===== Figure 3. KM + log-rank p =====
heffpox_surv <- cc_data |>
  mutate(
    milnepan   = factor(milnepan, levels = c(0, 1),
                        labels = c("No milnepan", "Milnepan")),
    status_num = ifelse(Status == 1, 1, 0)
  )

# 有 95% CI、沒有 p-value，也拿掉 risk table
km_plot <- ggsurvplot(
  fit_km,
  data        = heffpox_surv,
  conf.int    = TRUE,           # 95% CI
  risk.table  = FALSE,          # 不要下面那張表
  pval        = TRUE,          # 不在圖上顯示 p-value
  xlab        = "Days to death since admission",
  ylab        = "Survival probability",
  ggtheme     = theme_bw(),
  legend.title = "Treatment",
  legend.labs  = levels(heffpox_surv$milnepan)
)

km_plot$plot <- km_plot$plot +
  labs(caption = "Figure 3. Kaplan–Meier curves of TEVENT by milnepan treatment status") +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))
km_plot$plot

# 存成適當大小圖片
ggsave(
  filename = "Figure3_KM_milnepan.png",
  plot     = km_plot$plot,
  width    = 6, height = 4, dpi = 300, units = "in"
)

# ===== Table 4 log-rank =====
logrank_res <- survdiff(surv_obj ~ milnepan, data = cc_data)
logrank_res

lrt <- survdiff(Surv(TEVENT, status_num) ~ milnepan, data = heffpox_surv)

logrank_tab <- data.frame(
  Group    = c("No milnepan", "Milnepan"),
  N        = as.numeric(lrt$n),
  Observed = lrt$obs,
  Expected = lrt$exp
)

# 只在第一列放檢定結果
chisq_val <- lrt$chisq
df_val    <- length(lrt$n) - 1
p_val     <- 1 - pchisq(chisq_val, df_val)

logrank_tab <- logrank_tab |>
  mutate(
    chisq   = c(round(chisq_val, 1), NA),
    df      = c(df_val, NA),
    p_value = c(sprintf("%.3f", p_val), NA)   # 小數點後 3 位
  )

# 把 NA 顯示成 "-"
logrank_tab_display <- logrank_tab |>
  mutate(
    chisq   = ifelse(is.na(chisq),   "-", as.character(chisq)),
    df      = ifelse(is.na(df),      "-", as.character(df)),
    p_value = ifelse(is.na(p_value), "-", p_value)
  )

logrank_table <- logrank_tab_display |>
  kbl(
    align    = c("l", "c", "c", "c", "c", "c", "c"),
    col.names = c("Group", "N", "Observed deaths", "Expected deaths",
                  "Chi-square", "df", "p value"),
    caption  = "Table 4. Log-rank test comparing survival by milnepan status"
  ) |>
  kable_styling(
    bootstrap_options = c("hover"),
    full_width = FALSE,
    position   = "center",
    font_size  = 14
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#4A90E2")

logrank_table
# ===== IV. Causal Analysis =====
# ===== (1) MI with multivariable logistic regression =====
# 多重插補後的 logistic regression
fit_logit_mi <- with(
  imp2,
  glm(
    death7day ~ milnepan + age + sex + bmi + smoking + diabetes,
    family = binomial(link = "logit")
  )
)

# 用 Rubin's rules 合併
pool_logit_mi <- pool(fit_logit_mi)
summary_logit_mi <- summary(pool_logit_mi, conf.int = TRUE, conf.level = 0.95)

summary_logit_mi

tab_logit_mi <- summary_logit_mi |>
  mutate(
    OR      = exp(estimate),
    OR_low  = exp(`2.5 %`),
    OR_high = exp(`97.5 %`),
    OR_CI   = sprintf("%.2f–%.2f",OR_low, OR_high),
    p_value = sprintf("%.3f", p.value)
  ) |>
  # 換好看一點的名稱（視需要）
  mutate(
    term = recode(term,
                  "(Intercept)" = "Intercept",
                  "milnepan"    = "Milnepan (vs no milnepan)",
                  "age"         = "Age (per 1 year)",
                  "sex"         = "Male (vs female)",
                  "bmi"         = "BMI (per 1 kg/m²)",
                  "smoking"     = "Current smoker (vs no)",
                  "diabetes"    = "Diabetes (yes vs no)")
  )
tab_logit_mi

# ===== Table 5 =====
# 1. 只保留需要的欄位，並加上 p 值星號
tab_logit_disp <- tab_logit_mi |>
  mutate(
    p_star = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    p_display = sprintf("%.3f%s", p.value, p_star)
  ) |>
  select(
    Variable = term,
    estimate,
    std.error,
    OR,
    OR_CI,
    p_display
  )

# 2. 建表（MI logistic regression）
table_logit_mi <- tab_logit_disp |>
  kbl(
    align    = c("l", "c", "c", "c", "c", "c"),
    digits   = 3,
    col.names = c("Variable", "estimate", "STD",
                  "Adjusted OR", "95% CI", "p value"),
    caption  = "Table 5. Adjusted OR for 7-day mortality from logistic regression with MI"
  ) |>
  kable_styling(
    bootstrap_options = c("hover"),
    full_width = FALSE,
    position   = "center",
    font_size  = 14
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#4A90E2")|>
  add_footnote(
    label = "STD = standard error.",
    notation = "none"
  )

table_logit_mi

# ===== (2) Multi-imputed data with propensity score matching =====
# 取第 1 個插補資料集
dat_mi1 <- complete(imp2, action = 1)

# Propensity score model（和 IPTW 那個一致）
fit_treat_mi1 <- glm(
  milnepan ~ age + sex + bmi + smoking + diabetes,
  data   = dat_mi1,
  family = binomial()
)

summary(fit_treat_mi1)  # 可對應你 Table 6 的結構
confint(fit_treat_mi1)

# ===== Table 6 =====
coef_ps <- summary(fit_treat_mi1)$coefficients |>
  as.data.frame()
coef_ps$Variable <- rownames(coef_ps)
rownames(coef_ps) <- NULL

coef_ps$Signif <- cut(
  coef_ps$`Pr(>|z|)`,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", " ")
)

coef_ps <- coef_ps[, c("Variable", "Estimate", "Std. Error", "z value", "Pr(>|z|)", "Signif")]

tab_ps_mi1 <- coef_ps |>
  kable(
    format    = "html",
    col.names = c("Variable", "Estimate", "STD", "z value", "p-value", " "),
    caption   = "Table 6. Propensity score model on imputed dataset 1",
    digits    = c(NA, 3, 3, 2, 3, NA),
    align     = c("l", "c", "c", "c", "c", "c"),
    escape    = FALSE
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = FALSE,
    font_size         = 12
  ) |>
  row_spec(
    0,
    bold       = TRUE,
    color      = "white",
    background = "#4A90E2"
  )

tab_ps_mi1
# 1. 抓係數表
coef_ps <- summary(fit_treat_mi1)$coefficients |>
  as.data.frame()
coef_ps$Variable <- rownames(coef_ps)
rownames(coef_ps) <- NULL

coef_ps$Signif <- cut(
  coef_ps$`Pr(>|z|)`,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", " ")
)

coef_ps <- coef_ps[, c("Variable", "Estimate", "Std. Error", "z value", "Pr(>|z|)", "Signif")]

tab_ps_mi1 <- coef_ps |>
  kable(
    format    = "html",
    col.names = c("Variable", "Estimate", "STD", "z value", "p-value", " "),
    caption   = "Table 6. Propensity score model (imputed dataset 1)",
    digits    = c(NA, 3, 3, 2, 3, NA),
    align     = c("l", "c", "c", "c", "c", "c"),
    escape    = FALSE
  ) |>
  kable_styling(...) |>
  row_spec(0, bold = TRUE, color = "white", background = "#4A90E2")
tab_ps_mi1
coef_ps <- coef_ps[, c("Variable", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]

# 2. 建表 & 存圖
tab_ps_mi1 <- coef_ps |>
  kable(
    format    = "html",
    col.names = c("Variable", "Estimate", "STD", "z value", "p-value"),
    caption   = "Table X. Propensity score model (imputed dataset 1)",
    digits    = c(NA, 3, 3, 2, 3),
    align     = c("l", "c", "c", "c", "c"),
    escape    = FALSE
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = FALSE,
    font_size         = 12
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#4A90E2")

tab_ps_mi1



# ===== 在 PS 空間最近鄰配對，1:1，不替換 ======
m.out_mi1 <- matchit(
  milnepan ~ age + sex + bmi + smoking + diabetes,
  data   = dat_mi1,
  method = "nearest",
  ratio  = 1,
  replace = TRUE
)

summary(m.out_mi1)       # 看 treated/control 數量和 balance
dat_match_mi1 <- match.data(m.out_mi1)

# ===== Table 7 =====
library(knitr)
library(kableExtra)
library(dplyr)

s_m <- summary(m.out_mi1)

# Before matching
bal_before <- as.data.frame(s_m$sum.all)
bal_before$Variable <- rownames(bal_before)
rownames(bal_before) <- NULL
bal_before <- bal_before[, c("Variable", "Means Treated", "Means Control", "Std. Mean Diff.")]

# After matching
bal_after <- as.data.frame(s_m$sum.matched)
bal_after$Variable <- rownames(bal_after)
rownames(bal_after) <- NULL
bal_after <- bal_after[, c("Variable", "Means Treated", "Means Control", "Std. Mean Diff.")]

# 合併：before 和 after 並排
table7_data <- bal_before |>
  left_join(bal_after, by = "Variable", suffix = c("_before", "_after")) |>
  select(
    Variable,
    `Means Treated_before`, `Means Control_before`, `Std. Mean Diff._before`,
    `Means Treated_after`, `Means Control_after`, `Std. Mean Diff._after`
  ) |>
  rename(
    Variable = Variable,
    Treated_Mean_Before = `Means Treated_before`,
    Control_Mean_Before = `Means Control_before`,
    SMD_Before = `Std. Mean Diff._before`,
    Treated_Mean_After = `Means Treated_after`,
    Control_Mean_After = `Means Control_after`,
    SMD_After = `Std. Mean Diff._after`
  )

# 加 Sample Sizes 行
sample_row <- data.frame(
  Variable = "Sample size (N)",
  Treated_Mean_Before = 3948,
  Control_Mean_Before = 4244,
  SMD_Before = NA,
  Treated_Mean_After = 3948,
  Control_Mean_After = 3948,
  SMD_After = NA
)

table7_data <- bind_rows(table7_data, sample_row)

# 建 Table 7
table7 <- table7_data |>
  kable(
    format    = "html",
    col.names = c(
      "Variable",
      "Treated", "Control", "SMD",
      "Treated", "Control", "SMD"
    ),
    caption   = "Table 7. Covariate balance before and after 1:1 propensity score matching",
    digits    = c(NA, 1, 1, 3, 1, 1, 3),
    align     = c("l", rep("c", 6)),
    escape    = FALSE
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = FALSE,
    font_size         = 12
  ) |>
  add_header_above(
    c(" " = 1, "Before matching" = 3, "After matching" = 3),
    bold = TRUE,
    color = "white",
    background = "#4A90E2"
  ) |>
  row_spec(
    0,
    bold       = TRUE,
    color      = "white",
    background = "#4A90E2"
  )

table7

kableExtra::save_kable(
  table7,
  file    = "Table7_balance_comparison.png",
  zoom    = 2,
  density = 300
)






s_m <- summary(m.out_mi1)
bal_all  <- as.data.frame(s_m$sum.all)
# names(bal_all)
bal_match <- as.data.frame(s_m$sum.matched)

# All data balance
bal_all$Variable <- rownames(bal_all)
rownames(bal_all) <- NULL

# 2. 把 Variable 放到第一欄，其餘欄位照 names(bal_all) 的順序
bal_all <- bal_all[, c("Variable",
                       "Means Treated",
                       "Means Control",
                       "Std. Mean Diff.",
                       "Var. Ratio",
                       "eCDF Mean",
                       "eCDF Max",
                       "Std. Pair Dist.")]

# 3. 建 kable 表格（這裡沒有 p-value，就不用星號）
tab_bal_all <- bal_all |>
  kable(
    format    = "html",
    col.names = c("Variable",
                  "Mean (treated)",
                  "Mean (control)",
                  "Std. mean diff",
                  "Var ratio",
                  "eCDF mean",
                  "eCDF max",
                  "Std. pair dist"),
    caption   = "Table 7. Covariate balance before matching",
    digits    = 3,
    align     = c("l", rep("c", 7)),
    escape    = FALSE
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = FALSE,
    font_size         = 12
  ) |>
  row_spec(
    0,
    bold       = TRUE,
    color      = "white",
    background = "#4A90E2"
  )

tab_bal_all


# ===== Figure 4 plot balance =====
# 1. 計算 PS：配對前用全資料，配對後用 matched 資料
dat_mi1$ps <- predict(fit_treat_mi1, type = "response")
dat_match_mi1$ps <- predict(fit_treat_mi1, newdata = dat_match_mi1, type = "response")

# 2. 配對前 PS 分布（重疊、半透明）
p_before <- ggplot(dat_mi1,
                   aes(x = ps,
                       colour = factor(milnepan),
                       fill   = factor(milnepan))) +
  geom_density(alpha = 0.3) +
  labs(
    x = "Propensity score",
    y = "Density",
    colour = "Milnepan",
    fill   = "Milnepan",
    title  = "Before matching: Limited overlap between treated (milnepan) and control groups"
  ) +
  theme_minimal()

# 3. 配對後 PS 分布
p_after <- ggplot(dat_match_mi1,
                  aes(x = ps,
                      colour = factor(milnepan),
                      fill   = factor(milnepan))) +
  geom_density(alpha = 0.3) +
  labs(
    x = "Propensity score",
    y = "Density",
    colour = "Milnepan",
    fill   = "Milnepan",
    title  = "After matching: Improved overlap between treated and control groups"
  ) +
  theme_minimal()

# 4. 合成 Figure 4
fig4 <- plot_grid(
  p_before, p_after,
  labels = c("A", "B"),
  ncol   = 1  # 1 欄 = 上下排
)

fig4
# 加總標題
fig4_final <- plot_grid(
  fig4, legend,
  ncol = 1,
  rel_heights = c(3, 0.5)  # 主圖佔3份，圖例佔0.5份
)

# 最底加 Figure 4 標題
fig4_final <- ggdraw(fig4_final) +
  draw_label(
    "Figure 4. Propensity score distributions comparing covariate balance before (A) and after (B) matching",
    x = 0, y = 0.02,
    hjust = 0, vjust = 0,
    size = 12, fontface = "bold"
  )

fig4_final



# 1. PS 分布（配對前 vs 配對後）
dat_mi1$ps <- predict(fit_treat_mi1, type = "response")

# 配對前
p1 <- ggplot(dat_mi1, aes(x = ps, colour = factor(milnepan))) +
  geom_density() +
  labs(x = "Propensity score", colour = "Milnepan",
       title = "PS distribution before matching")
p1
# 配對後
dat_match_mi1$ps <- predict(fit_treat_mi1, newdata = dat_match_mi1, type = "response")

p2 <- ggplot(dat_match_mi1, aes(x = ps, colour = factor(milnepan))) +
  geom_density() +
  labs(x = "Propensity score", colour = "Milnepan",
       title = "PS distribution after matching")
p2

library(ggplot2)

dat_mi1$ps <- predict(fit_treat_mi1, type = "response")

ggplot(dat_mi1, aes(x = ps, colour = factor(milnepan), fill = factor(milnepan))) +
  geom_density(alpha = 0.3) +
  labs(
    x = "Propensity score",
    colour = "Milnepan",
    fill   = "Milnepan",
    title  = "Propensity score distribution before matching"
  )


# ===== logistic regr =====
fit_psmatch_mi1 <- glm(
  death7day ~ milnepan,
  data   = dat_match_mi1,
  family = binomial()
)

summary(fit_psmatch_mi1)

co_m <- summary(fit_psmatch_mi1)$coef["milnepan", ]
logOR_m <- co_m["Estimate"]
se_m    <- co_m["Std. Error"]

OR_m       <- exp(logOR_m)
OR_m_low   <- exp(logOR_m - 1.96 * se_m)
OR_m_high  <- exp(logOR_m + 1.96 * se_m)
p_m        <- co_m["Pr(>|z|)"]

c(OR_m, OR_m_low, OR_m_high, p_m)

# ===== robustness check with 1-3 imputed data =====
get_psmatch_or <- function(k) {
  dat_k <- complete(imp2, action = k)
  m.out <- matchit(
    milnepan ~ age + sex + bmi + smoking + diabetes,
    data   = dat_k,
    method = "nearest",
    ratio  = 1
  )
  dat_km <- match.data(m.out)
  fit    <- glm(death7day ~ milnepan, data = dat_km, family = binomial())
  co     <- summary(fit)$coef["milnepan", ]
  est    <- co["Estimate"]
  se     <- co["Std. Error"]
  OR     <- exp(est)
  OR_l   <- exp(est - 1.96 * se)
  OR_u   <- exp(est + 1.96 * se)
  p      <- co["Pr(>|z|)"]
  c(imputation = k, OR = OR, OR_low = OR_l, OR_high = OR_u, p = p)
}

res_1to3 <- do.call(rbind, lapply(1:3, get_psmatch_or))
res_1to3







# 從 imp2 抽出第 1 個插補後資料
dat3 <- complete(imp3, action = 1)
dim(dat3)
ps_model3 <- glm(
  milnepan ~ age + sex + bmi + smoking + diabetes,
  data = dat3,
  family = binomial()
)
summary(ps_model3)
dat3$ps <- predict(ps_model3, type = "response")

set.seed(0205)
sub_id <- sample(seq_len(nrow(dat3)), size = 8000)
dat3_sub <- dat3[sub_id, ]
m.out3_sub <- matchit(
  milnepan ~ ps,         # 用已有的 PS
  data   = dat3_sub,
  method = "nearest",
  ratio  = 1,
  replace = TRUE         # 允許重複使用對照，計算更穩、要求較低
)
summary(m.out3_sub)

# ========
m_test <- 10  # 先測試
for (k in 1:m_test) {
  dat_k <- dat_mi_long %>% filter(.imp == k)
  mod_match <- matchit(
    milnepan ~ age + sex + bmi + smoking + diabetes,
    data   = dat_k,
    method = "nearest",
    ratio  = 1
  )
  print(summary(mod_match))
}


for (k in 1:m) {
  dat_k <- dat_mi_long |> filter(.imp == k)
  
  # 估 PS 並做 matching（可加 caliper，例如 0.2 SD of logit PS）
  mod_match <- matchit(
    milnepan ~ age + sex + bmi + smoking + diabetes,
    data   = dat_k,
    method = "nearest",
    ratio  = 1
  )
  
  # 把 matched data 存起來（含 matchit 產生的權重、subclass 等）
  matched_dat_k <- match.data(mod_match)
  
  ps_match_list[[k]] <- matched_dat_k
}
# 存每一個 imputation 的 log(OR) 和 SE
logOR_vec <- numeric(m)
se_vec    <- numeric(m)

for (k in 1:m) {
  dat_k_matched <- ps_match_list[[k]]
  
  fit_k <- glm(
    death7day ~ milnepan,
    data   = dat_k_matched,
    family = binomial(link = "logit")
  )
  
  co <- summary(fit_k)$coef["milnepan", ]  # 只取 treatment 係數
  logOR_vec[k] <- co["Estimate"]
  se_vec[k]    <- co["Std. Error"]
}

M <- m

# 完全樣本分析的自由度，這裡可粗略用大的 df，例如 1e6，或用第一個模型的 df
v_com <- 1e6

# Q_bar: 完整分析估計值的平均
Q_bar <- mean(logOR_vec)

# U_bar: 完整分析變異的平均
U_bar <- mean(se_vec^2)

# B: between-imputation variance
B <- var(logOR_vec)

# Total variance
T_var <- U_bar + (1 + 1/M) * B

# 對應的 t 分布自由度（簡化版）
df_old <- (M - 1) * (1 + U_bar / ((1 + 1/M) * B))^2

# Pooled log(OR) ± 95% CI
logOR_pooled <- Q_bar
se_pooled    <- sqrt(T_var)

OR_pooled    <- exp(logOR_pooled)
OR_low       <- exp(logOR_pooled - qt(0.975, df_old) * se_pooled)
OR_high      <- exp(logOR_pooled + qt(0.975, df_old) * se_pooled)

# p-value
t_stat <- logOR_pooled / se_pooled
p_val  <- 2 * (1 - pt(abs(t_stat), df_old))

OR_pooled
OR_low
OR_high
p_val
# 把這個 pooled 結果整理成 kable 表（例如 Table 6：Effect of milnepan on 7‑day mortality from PS matching with MI）

# ===== (3) MI with IPTW =====
dat1 <- complete(imp2, action = 1)

# 估 P(milnepan = 1 | L)
fit_treat <- glm(
  milnepan ~ age + sex + bmi + smoking + diabetes,
  data   = dat1,
  family = binomial()
)

summary(fit_treat)
# ===== Table 8 Propensity score model =====
coef_tab <- summary(fit_treat)$coefficients |>
  as.data.frame()
coef_tab$Variable <- rownames(coef_tab)
rownames(coef_tab) <- NULL

coef_tab$Signif <- cut(
  coef_tab$`Pr(>|z|)`,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", " ")
)

coef_tab <- coef_tab[, c("Variable", "Estimate", "Std. Error", "z value", "Pr(>|z|)", "Signif")]

tab_ps <- coef_tab |>
  kable(
    format    = "html",
    col.names = c("Variable", "Estimate", "Std. Error", "z value", "p-value", " "),  # 6 個名字
    caption   = "Table 8. Logistic regression model for the propensity score of receiving milnepan",
    escape    = FALSE,
    digits    = c(NA, 3, 3, 2, 3, NA),
    align     = c("l", "c", "c", "c", "c", "c")
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = FALSE,
    font_size         = 12
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#4A90E2")

tab_ps

# ===== IP weight construction =====
# P(A = a | L) for each person
p.milnepan.obs <- ifelse(
  dat1$milnepan == 1,
  predict(fit_treat, type = "response"),
  1 - predict(fit_treat, type = "response")
)

# 非穩定權重： 1 / P(A = a | L)
dat1$w_iptw <- 1 / p.milnepan.obs
summary(dat1$w_iptw)

# denominator: P(A = 1 | L) = 上面 fit_treat
pd.milnepan <- predict(fit_treat, type = "response")

# numerator: 邊際 P(A = 1)
numer.fit <- glm(
  milnepan ~ 1,
  data   = dat1,
  family = binomial()
)
pn.milnepan <- predict(numer.fit, type = "response")

# stabilized weights
dat1$sw <- ifelse(
  dat1$milnepan == 1,
  pn.milnepan / pd.milnepan,
  (1 - pn.milnepan) / (1 - pd.milnepan)
)

summary(dat1$sw)
# ===== Table 9 iptw vs sw =====
# 把 summary 轉成純 numeric + names
w_iptw_sum <- summary(dat1$w_iptw)
sw_sum     <- summary(dat1$sw)

w_vals <- as.numeric(w_iptw_sum)
names(w_vals) <- names(w_iptw_sum)

sw_vals <- as.numeric(sw_sum)
names(sw_vals) <- names(sw_sum)

tab7_data <- bind_rows(
  data.frame(
    Weight_type = "Unstabilized IPW",
    Min    = w_vals["Min."],
    Q1     = w_vals["1st Qu."],
    Median = w_vals["Median"],
    Mean   = w_vals["Mean"],
    Q3     = w_vals["3rd Qu."],
    Max    = w_vals["Max."]
  ),
  data.frame(
    Weight_type = "Stabilized IPW",
    Min    = sw_vals["Min."],
    Q1     = sw_vals["1st Qu."],
    Median = sw_vals["Median"],
    Mean   = sw_vals["Mean"],
    Q3     = sw_vals["3rd Qu."],
    Max    = sw_vals["Max."]
  )
)
rownames(tab7_data) <- NULL
tab7 <- tab7_data |>
  kable(
    format    = "html",
    col.names = c("Weight type", "Min", "Q1", "Median", "Mean", "Q3", "Max"),
    caption   = "Table 9. Distribution of inverse probability weights for milnepan",
    digits    = 3,
    align     = c("l", rep("c", 6)),
    escape    = FALSE
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = FALSE,
    font_size         = 12
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#4A90E2")

tab7
# ===== Marginal structural model =====
msm_iptw <- geeglm(
  death7day ~ milnepan,
  data    = dat1,
  weights = sw,
  id      = 1:nrow(dat1),
  corstr  = "independence",
  family  = binomial()
)

summary(msm_iptw)

# ===== Table 10 =====
gee_sum <- summary(msm_iptw)
coef_tab_msm <- as.data.frame(gee_sum$coefficients)
coef_tab_msm$Variable <- rownames(coef_tab_msm)
rownames(coef_tab_msm) <- NULL

# 計算 OR 和 95% CI
coef_tab_msm <- coef_tab_msm |>
  mutate(
    OR      = exp(Estimate),
    OR_low  = exp(Estimate - 1.96 * Std.err),
    OR_high = exp(Estimate + 1.96 * Std.err),
    OR_CI   = sprintf("(%.2f, %.2f)", OR_low, OR_high)
  )

# 依 p-value 產生星號
coef_tab_msm$Signif <- cut(
  coef_tab_msm$`Pr(>|W|)`,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", " ")
)

# 整理欄位順序與名稱
coef_tab_msm <- coef_tab_msm |>
  select(Variable, Estimate, Std.err, Wald, `Pr(>|W|)`, Signif, OR, OR_CI)

colnames(coef_tab_msm)[colnames(coef_tab_msm) == "Std.err"]   <- "STD"
colnames(coef_tab_msm)[colnames(coef_tab_msm) == "Pr(>|W|)"] <- "p-value"

tab_msm <- coef_tab_msm |>
  kable(
    format    = "html",
    col.names = c("Variable",
                  "Estimate",
                  "STD",
                  "Wald",
                  "p-value",
                  " ",
                  "OR",
                  "95% CI"),
    caption   = "Table 10. Marginal structural model for 7-day mortality",
    digits    = c(NA, 3, 3, 2, 3, NA, 2, NA),
    align     = c("l", "c", "c", "c", "c", "c", "c", "c"),
    escape    = FALSE
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = FALSE,
    font_size         = 12
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#4A90E2")

tab_msm
# ===== (4) Complete‑case sensitivity analysis =====
# (A) 建立 complete‑case 資料集
cc <- heffpox |>
  dplyr::select(
    death7day,
    milnepan,
    age,
    sex,
    bmi,
    smoking,
    diabetes
  ) |>
  tidyr::drop_na()

# (B) Complete‑case logistic regression
fit_cc_logit <- glm(
  death7day ~ milnepan + age + sex + bmi + smoking + diabetes,
  data   = cc,
  family = binomial()
)

summary(fit_cc_logit)

# 把 milnepan 的 OR 和 95% CI 算出來
co_cc   <- summary(fit_cc_logit)$coef["milnepan", ]
logOR   <- co_cc["Estimate"]
se_cc   <- co_cc["Std. Error"]

OR_cc      <- exp(logOR)
OR_cc_low  <- exp(logOR - 1.96 * se_cc)
OR_cc_high <- exp(logOR + 1.96 * se_cc)
p_cc       <- co_cc["Pr(>|z|)"]

c(OR_cc, OR_cc_low, OR_cc_high, p_cc)

# ===== (C) Complete‑case IPTW + MSM =====
fit_cc_treat <- glm(
  milnepan ~ age + sex + bmi + smoking + diabetes,
  data   = cc,
  family = binomial()
)

summary(fit_cc_treat)

# 預測 P(A = 1 | L)
pA1_cc <- predict(fit_cc_treat, type = "response")
# 邊際治療機率 P(A = 1)
fit_cc_numer <- glm(
  milnepan ~ 1,
  data   = cc,
  family = binomial()
)
pA1_marg_cc <- predict(fit_cc_numer, type = "response")

# stabilized weights
cc$sw_cc <- ifelse(
  cc$milnepan == 1,
  pA1_marg_cc / pA1_cc,
  (1 - pA1_marg_cc) / (1 - pA1_cc)
)

summary(cc$sw_cc)

msm_cc <- geeglm(
  death7day ~ milnepan,
  data    = cc,
  family  = binomial(),
  weights = sw_cc,
  id      = 1:nrow(cc),
  corstr  = "independence"
)

summary(msm_cc)

# 把 milnepan 的 OR 和 95% CI 算出來
co_msm_cc <- summary(msm_cc)$coef["milnepan", ]
logOR_msm_cc <- co_msm_cc["Estimate"]
se_msm_cc    <- co_msm_cc["Std.err"]

OR_msm_cc      <- exp(logOR_msm_cc)
OR_msm_cc_low  <- exp(logOR_msm_cc - 1.96 * se_msm_cc)
OR_msm_cc_high <- exp(logOR_msm_cc + 1.96 * se_msm_cc)
p_msm_cc       <- co_msm_cc["Pr(>|W|)"]

c(OR_msm_cc, OR_msm_cc_low, OR_msm_cc_high, p_msm_cc)
# ===== Table 11 compare 6 models =====

# Complete-case PS matching
m.out_cc <- matchit(milnepan ~ age + sex + bmi + smoking + diabetes,
                    data = cc, method = "nearest", ratio = 1)

dat_match_cc <- match.data(m.out_cc)
fit_psmatch_cc <- glm(death7day ~ milnepan, data = dat_match_cc, family = binomial())
summary(fit_psmatch_cc)

# 直接給我這4個數字：
co_cc <- summary(fit_psmatch_cc)$coef["milnepan", ]
OR_cc <- exp(co_cc["Estimate"])
CI_cc <- exp(confint(fit_psmatch_cc)["milnepan", ])
p_cc <- co_cc["Pr(>|z|)"]
print(c(OR_cc, CI_cc[1], CI_cc[2], p_cc))


## 小工具：抓 milnepan 的 Estimate、Std、OR、CI、p
extract_or <- function(fit, type = c("glm", "gee")) {
  type <- match.arg(type)
  s  <- summary(fit)
  cf <- as.data.frame(s$coef)
  cf$term <- rownames(cf)
  row <- cf[cf$term == "milnepan", , drop = FALSE]
  if (nrow(row) == 0L) stop("找不到 milnepan 這一列")
  
  if (type == "glm") {
    est <- row$Estimate
    se  <- row$`Std. Error`
    p   <- row$`Pr(>|z|)`
  } else {
    est <- row$Estimate
    se  <- row$Std.err
    p   <- row$`Pr(>|W|)`
  }
  
  OR   <- exp(est)
  OR_l <- exp(est - 1.96 * se)
  OR_u <- exp(est + 1.96 * se)
  
  data.frame(
    Estimate = est,
    Std      = se,
    OR       = OR,
    OR_low   = OR_l,
    OR_high  = OR_u,
    p_value  = p
  )
}

## 1) MI + multivariable logistic（用 pooled 結果）
row_mi <- summary_logit_mi[summary_logit_mi$term == "milnepan", ]

logOR_mi <- row_mi$estimate
se_mi    <- row_mi$std.error
OR_mi    <- exp(logOR_mi)
OR_mi_low  <- exp(row_mi$`2.5 %`)
OR_mi_high <- exp(row_mi$`97.5 %`)
p_mi       <- row_mi$p.value

row1 <- data.frame(
  Methods = "MI + multivariable logistic",
  Estimate = logOR_mi,
  Std      = se_mi,
  OR       = OR_mi,
  OR_low   = OR_mi_low,
  OR_high  = OR_mi_high,
  p_value  = p_mi
)

## 2)
row2 <- extract_or(msm_iptw, type = "gee") |>
  mutate(Methods = "MI + IPTW MSM")


row4 <- extract_or(fit_cc_logit, type = "glm") |>
  mutate(Methods = "Complete-case + logistic")

row5 <- extract_or(msm_cc, type = "gee") |>
  mutate(Methods = "Complete-case + IPTW MSM")



## 合併 + CI 文字 + 星號
tab9_data <- bind_rows(row1, row2, row3, row4) |>
  select(Methods, Estimate, Std, OR, OR_low, OR_high, p_value) |>
  mutate(
    OR_CI = sprintf("(%.2f, %.2f)", OR_low, OR_high),
    Signif = cut(
      p_value,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
      labels = c("***", "**", "*", ".", " ")
    )
  ) |>
  select(Methods, Estimate, Std, OR, OR_CI, p_value, Signif)

rownames(tab9_data) <- NULL

tab9 <- tab9_data |>
  kable(
    format    = "html",
    col.names = c("Methods",
                  "Estimate (log-OR)",
                  "STD",
                  "Adjusted OR",
                  "95% CI for OR",
                  "p-value",
                  " "),
    caption   = "Table 9. Estimated effect of milnepan on 7-day mortality across analyses",
    digits    = c(NA, 3, 3, 2, NA, 3, NA),
    align     = c("l", "c", "c", "c", "c", "c", "c"),
    escape    = FALSE
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = FALSE,
    font_size         = 12
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#4A90E2")

tab9







