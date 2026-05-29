library(ggplot2)

theme_medical <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    panel.grid.minor = element_blank()
  )

# 1. Kaplan-Meier Survival Curve
set.seed(42)
n <- 100
time_a <- rexp(n, rate = 0.02)
time_b <- rexp(n, rate = 0.035)
surv_data <- data.frame(
  time = c(pmin(time_a, 60), pmin(time_b, 60)),
  group = rep(c("Treatment A", "Treatment B"), each = n)
)
surv_data$time <- round(surv_data$time, 1)

surv_plot_data <- do.call(rbind, lapply(split(surv_data, surv_data$group), function(d) {
  sorted <- sort(d$time)
  nn <- length(sorted)
  data.frame(
    time = c(0, rep(sorted, each = 2)),
    surv = c(1, rep(1 - (1:nn) / nn, each = 2))[1:(2*nn+1)],
    group = d$group[1]
  )
}))

png("km_survival_curve.png", width = 800, height = 600, res = 150)
ggplot(surv_plot_data, aes(x = time, y = surv, color = group)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Kaplan-Meier Survival Curve",
       x = "Time (months)", y = "Survival Probability", color = "Group") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_color_manual(values = c("#2E86AB", "#E74C3C")) +
  theme_medical
dev.off()

# 2. ROC Curve
set.seed(123)
n <- 500
scores <- c(rnorm(n/2, mean = 1.5, sd = 1), rnorm(n/2, mean = 0, sd = 1))
labels <- c(rep(1, n/2), rep(0, n/2))
thresholds <- sort(unique(scores), decreasing = TRUE)
tpr <- sapply(thresholds, function(t) sum(scores >= t & labels == 1) / sum(labels == 1))
fpr <- sapply(thresholds, function(t) sum(scores >= t & labels == 0) / sum(labels == 0))
auc_val <- round(sum(diff(c(0, fpr)) * (head(tpr, -1) + tail(tpr, -1)) / 2), 3)

roc_df <- data.frame(FPR = c(0, fpr, 1), TPR = c(0, tpr, 1))

png("roc_curve.png", width = 800, height = 600, res = 150)
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.6, y = 0.3, label = paste0("AUC = ", auc_val), size = 5, fontface = "bold") +
  labs(title = "ROC Curve", x = "1 - Specificity (FPR)", y = "Sensitivity (TPR)") +
  coord_equal() +
  theme_medical
dev.off()

# 3. Box Plot (Group Comparison)
set.seed(456)
box_data <- data.frame(
  value = c(rnorm(30, 120, 15), rnorm(30, 135, 18), rnorm(30, 110, 12)),
  group = factor(rep(c("Control", "Drug A", "Drug B"), each = 30),
                 levels = c("Control", "Drug A", "Drug B"))
)

png("boxplot_comparison.png", width = 800, height = 600, res = 150)
ggplot(box_data, aes(x = group, y = value, fill = group)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = c("#95C8D8", "#E74C3C", "#F39C12")) +
  labs(title = "Group Comparison (Box Plot)",
       x = "Group", y = "Systolic Blood Pressure (mmHg)") +
  theme_medical + theme(legend.position = "none")
dev.off()

# 4. Bar Chart with Error Bars
set.seed(789)
bar_data <- data.frame(
  group = factor(c("Placebo", "Low Dose", "Mid Dose", "High Dose"),
                 levels = c("Placebo", "Low Dose", "Mid Dose", "High Dose")),
  mean = c(5.2, 12.8, 24.5, 31.0),
  se = c(1.8, 2.5, 3.1, 2.9)
)

png("barplot_errorbars.png", width = 800, height = 600, res = 150)
ggplot(bar_data, aes(x = group, y = mean, fill = group)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, linewidth = 0.8) +
  scale_fill_manual(values = c("#BDC3C7", "#85C1E9", "#3498DB", "#1A5276")) +
  labs(title = "Dose-Response Relationship",
       x = "Treatment Group", y = "Response Rate (%)") +
  theme_medical + theme(legend.position = "none")
dev.off()

# 5. Scatter Plot with Regression
set.seed(101)
n <- 80
age <- runif(n, 20, 80)
bp <- 70 + 0.8 * age + rnorm(n, 0, 10)
scatter_df <- data.frame(Age = age, SBP = bp)

png("scatter_regression.png", width = 800, height = 600, res = 150)
ggplot(scatter_df, aes(x = Age, y = SBP)) +
  geom_point(alpha = 0.6, color = "#2E86AB", size = 2.5) +
  geom_smooth(method = "lm", color = "#E74C3C", fill = "#E74C3C", alpha = 0.15, linewidth = 1.2) +
  labs(title = "Scatter Plot with Linear Regression",
       x = "Age (years)", y = "Systolic Blood Pressure (mmHg)") +
  theme_medical
dev.off()

# 6. Histogram with Normal Curve
set.seed(202)
hist_data <- data.frame(value = rnorm(500, mean = 70, sd = 10))

png("histogram_normal.png", width = 800, height = 600, res = 150)
ggplot(hist_data, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "#2E86AB", alpha = 0.7, color = "white") +
  stat_function(fun = dnorm, args = list(mean = 70, sd = 10), color = "#E74C3C", linewidth = 1.2) +
  labs(title = "Histogram with Normal Distribution Curve",
       x = "Test Score", y = "Density") +
  theme_medical
dev.off()

# 7. Forest Plot (Meta-analysis style)
forest_data <- data.frame(
  study = factor(c("Study A", "Study B", "Study C", "Study D", "Study E", "Overall"),
                 levels = rev(c("Study A", "Study B", "Study C", "Study D", "Study E", "Overall"))),
  or = c(0.75, 0.82, 0.60, 0.91, 0.70, 0.74),
  lower = c(0.55, 0.61, 0.38, 0.72, 0.50, 0.62),
  upper = c(1.02, 1.10, 0.95, 1.15, 0.98, 0.88),
  is_overall = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
)

png("forest_plot.png", width = 900, height = 550, res = 150)
ggplot(forest_data, aes(x = or, y = study)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, linewidth = 0.8) +
  geom_point(aes(size = is_overall, shape = is_overall, color = is_overall)) +
  scale_shape_manual(values = c(16, 18)) +
  scale_size_manual(values = c(3, 5)) +
  scale_color_manual(values = c("#2E86AB", "#E74C3C")) +
  labs(title = "Forest Plot (Meta-analysis)",
       x = "Odds Ratio (95% CI)", y = "") +
  theme_medical + theme(legend.position = "none")
dev.off()

# 8. Bland-Altman Plot
set.seed(303)
n <- 60
method1 <- rnorm(n, 100, 20)
method2 <- method1 + rnorm(n, 2, 8)
ba_df <- data.frame(
  mean_val = (method1 + method2) / 2,
  diff_val = method1 - method2
)
mean_diff <- mean(ba_df$diff_val)
sd_diff <- sd(ba_df$diff_val)

png("bland_altman.png", width = 800, height = 600, res = 150)
ggplot(ba_df, aes(x = mean_val, y = diff_val)) +
  geom_point(alpha = 0.6, color = "#2E86AB", size = 2.5) +
  geom_hline(yintercept = mean_diff, color = "#E74C3C", linewidth = 1) +
  geom_hline(yintercept = mean_diff + 1.96 * sd_diff, linetype = "dashed", color = "#F39C12") +
  geom_hline(yintercept = mean_diff - 1.96 * sd_diff, linetype = "dashed", color = "#F39C12") +
  annotate("text", x = max(ba_df$mean_val), y = mean_diff, label = "Mean", hjust = 1.1, color = "#E74C3C", fontface = "bold") +
  annotate("text", x = max(ba_df$mean_val), y = mean_diff + 1.96 * sd_diff, label = "+1.96 SD", hjust = 1.1, color = "#F39C12") +
  annotate("text", x = max(ba_df$mean_val), y = mean_diff - 1.96 * sd_diff, label = "-1.96 SD", hjust = 1.1, color = "#F39C12") +
  labs(title = "Bland-Altman Plot",
       x = "Mean of Two Methods", y = "Difference (Method 1 - Method 2)") +
  theme_medical
dev.off()

# 9. Heatmap (Correlation Matrix)
set.seed(404)
vars <- c("Age", "BMI", "SBP", "DBP", "HR", "Chol", "Glu")
n_vars <- length(vars)
cor_mat <- matrix(runif(n_vars^2, -1, 1), n_vars, n_vars)
cor_mat <- (cor_mat + t(cor_mat)) / 2
diag(cor_mat) <- 1
cor_mat <- round(pmin(pmax(cor_mat, -1), 1), 2)

heat_df <- expand.grid(Var1 = vars, Var2 = vars)
heat_df$value <- as.vector(cor_mat)

png("heatmap_correlation.png", width = 800, height = 700, res = 150)
ggplot(heat_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3.2) +
  scale_fill_gradient2(low = "#2E86AB", mid = "white", high = "#E74C3C", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap", x = "", y = "", fill = "r") +
  theme_medical +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# 10. Paired Before-After Plot
set.seed(505)
n <- 20
before <- rnorm(n, 140, 15)
after <- before - rnorm(n, 12, 8)
paired_df <- data.frame(
  id = rep(1:n, 2),
  time = factor(rep(c("Before", "After"), each = n), levels = c("Before", "After")),
  value = c(before, after)
)

png("paired_before_after.png", width = 800, height = 600, res = 150)
ggplot(paired_df, aes(x = time, y = value)) +
  geom_line(aes(group = id), alpha = 0.3, color = "grey50") +
  geom_point(aes(color = time), size = 3, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 5, shape = 18, color = "#1A1A1A") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), linewidth = 1.5, color = "#1A1A1A") +
  scale_color_manual(values = c("#E74C3C", "#2E86AB")) +
  labs(title = "Paired Before-After Comparison",
       x = "", y = "Systolic Blood Pressure (mmHg)") +
  theme_medical + theme(legend.position = "none")
dev.off()

cat("All 10 graphs generated successfully.\n")
