library(ggplot2)

# ── 共通テーマ: 白地＋グリッド＋軸のみ、データなし ──
theme_template <- theme_bw(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey92", linewidth = 0.2),
    plot.margin = margin(15, 15, 10, 10)
  )

empty <- data.frame(x = numeric(0), y = numeric(0))

# ================================================================
# ■ 生化学 (Biochemistry)
# ================================================================

# 1. Michaelis-Menten
png("biochem_michaelis_menten.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  scale_x_continuous(name = "[S]  (substrate concentration, mM)",
                     limits = c(0, 10), breaks = seq(0, 10, 1), expand = c(0, 0)) +
  scale_y_continuous(name = expression(italic(v) ~ " (reaction velocity, " * mu * "mol/min)"),
                     limits = c(0, 10), breaks = seq(0, 10, 1), expand = c(0, 0)) +
  labs(title = "Michaelis-Menten Plot") +
  theme_template
dev.off()

# 2. Lineweaver-Burk
png("biochem_lineweaver_burk.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5) +
  scale_x_continuous(name = expression("1/[S]  (mM" ^ -1 * ")"),
                     limits = c(-3, 10), breaks = seq(-3, 10, 1), expand = c(0, 0)) +
  scale_y_continuous(name = expression("1/" * italic(v) ~ " (" * mu * "mol/min)" ^ -1),
                     limits = c(-3, 10), breaks = seq(-3, 10, 1), expand = c(0, 0)) +
  labs(title = "Lineweaver-Burk Plot (Double Reciprocal)") +
  theme_template
dev.off()

# 3. pH Titration Curve
png("biochem_titration.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_hline(yintercept = 7, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  scale_x_continuous(name = "Volume of titrant added (mL)",
                     limits = c(0, 50), breaks = seq(0, 50, 5), expand = c(0, 0)) +
  scale_y_continuous(name = "pH",
                     limits = c(0, 14), breaks = seq(0, 14, 1), expand = c(0, 0)) +
  labs(title = "pH Titration Curve") +
  theme_template
dev.off()

# 4. Enzyme Inhibition (Log scale dose-response for inhibitor)
png("biochem_enzyme_inhibition.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  scale_x_continuous(name = "[S]  (substrate concentration, mM)",
                     limits = c(0, 10), breaks = seq(0, 10, 1), expand = c(0, 0)) +
  scale_y_continuous(name = expression(italic(v) ~ " (reaction velocity, " * mu * "mol/min)"),
                     limits = c(0, 10), breaks = seq(0, 10, 1), expand = c(0, 0)) +
  labs(title = "Enzyme Kinetics with Inhibitor",
       subtitle = "Plot curves for: No inhibitor / Competitive / Non-competitive") +
  theme_template
dev.off()


# ================================================================
# ■ 生理学 (Physiology)
# ================================================================

# 5. Oxygen Dissociation Curve
png("physiol_o2_dissociation.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  scale_x_continuous(name = expression("P" * O[2] ~ "(mmHg)"),
                     limits = c(0, 120), breaks = seq(0, 120, 10), expand = c(0, 0)) +
  scale_y_continuous(name = expression("S" * O[2] ~ "(% saturation)"),
                     limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  labs(title = expression("Oxygen-Hemoglobin Dissociation Curve (" * O[2] * ")")) +
  theme_template
dev.off()

# 6. Frank-Starling Curve
png("physiol_frank_starling.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  scale_x_continuous(name = "Preload  (End-Diastolic Volume, mL)",
                     limits = c(0, 250), breaks = seq(0, 250, 25), expand = c(0, 0)) +
  scale_y_continuous(name = "Stroke Volume (mL)  /  Cardiac Output (L/min)",
                     limits = c(0, 150), breaks = seq(0, 150, 10), expand = c(0, 0)) +
  labs(title = "Frank-Starling Curve",
       subtitle = "Plot curves for: Normal / Heart Failure / Exercise") +
  theme_template
dev.off()

# 7. Action Potential
png("physiol_action_potential.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5) +
  scale_x_continuous(name = "Time (ms)",
                     limits = c(0, 10), breaks = seq(0, 10, 0.5), expand = c(0, 0)) +
  scale_y_continuous(name = "Membrane Potential (mV)",
                     limits = c(-100, 60), breaks = seq(-100, 60, 10), expand = c(0, 0)) +
  labs(title = "Action Potential") +
  theme_template
dev.off()

# 8. Cardiac Action Potential (long)
png("physiol_cardiac_ap.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5) +
  scale_x_continuous(name = "Time (ms)",
                     limits = c(0, 400), breaks = seq(0, 400, 25), expand = c(0, 0)) +
  scale_y_continuous(name = "Membrane Potential (mV)",
                     limits = c(-100, 60), breaks = seq(-100, 60, 10), expand = c(0, 0)) +
  labs(title = "Cardiac Action Potential",
       subtitle = "Phases: 0 (depol.) / 1 (early repol.) / 2 (plateau) / 3 (repol.) / 4 (rest)") +
  theme_template
dev.off()

# 9. Spirometry (Flow-Volume Loop)
png("physiol_flow_volume.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5) +
  scale_x_continuous(name = "Volume (L)",
                     limits = c(0, 6), breaks = seq(0, 6, 0.5), expand = c(0, 0)) +
  scale_y_continuous(name = "Flow (L/s)",
                     limits = c(-8, 12), breaks = seq(-8, 12, 1), expand = c(0, 0)) +
  labs(title = "Flow-Volume Loop (Spirometry)",
       subtitle = "Above axis = Expiration / Below axis = Inspiration") +
  theme_template
dev.off()

# 10. Pressure-Volume Loop (Cardiac)
png("physiol_pv_loop.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  scale_x_continuous(name = "Left Ventricular Volume (mL)",
                     limits = c(0, 200), breaks = seq(0, 200, 20), expand = c(0, 0)) +
  scale_y_continuous(name = "Left Ventricular Pressure (mmHg)",
                     limits = c(0, 160), breaks = seq(0, 160, 10), expand = c(0, 0)) +
  labs(title = "Cardiac Pressure-Volume Loop") +
  theme_template
dev.off()


# ================================================================
# ■ 薬理学 (Pharmacology)
# ================================================================

# 11. Dose-Response Curve (log scale)
png("pharm_dose_response.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  scale_x_log10(name = "Dose / Concentration (log scale)",
                limits = c(0.001, 1000),
                breaks = 10^(-3:3),
                labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000")) +
  scale_y_continuous(name = "Response (% of maximum)",
                     limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  labs(title = "Dose-Response Curve",
       subtitle = "Plot curves for: Agonist / + Competitive antagonist / + Non-competitive antagonist") +
  theme_template
dev.off()

# 12. Pharmacokinetics (Concentration-Time)
png("pharm_pk_curve.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  scale_x_continuous(name = "Time after administration (hr)",
                     limits = c(0, 24), breaks = seq(0, 24, 2), expand = c(0, 0)) +
  scale_y_continuous(name = expression("Plasma concentration (" * mu * "g/mL)"),
                     limits = c(0, 50), breaks = seq(0, 50, 5), expand = c(0, 0)) +
  labs(title = "Pharmacokinetic Profile (Concentration-Time Curve)") +
  theme_template
dev.off()

# 13. PK - Semi-log (for half-life)
png("pharm_pk_semilog.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  scale_x_continuous(name = "Time after administration (hr)",
                     limits = c(0, 24), breaks = seq(0, 24, 2), expand = c(0, 0)) +
  scale_y_log10(name = expression("Plasma concentration (" * mu * "g/mL)  [log scale]"),
                limits = c(0.1, 100),
                breaks = c(0.1, 0.5, 1, 5, 10, 50, 100)) +
  labs(title = "Pharmacokinetic Profile (Semi-log)",
       subtitle = expression("Slope = " * -k[e] * " / 2.303,   " * t["1/2"] * " = 0.693 / " * k[e])) +
  theme_template
dev.off()

# 14. Therapeutic Window
png("pharm_therapeutic_window.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#E74C3C", linewidth = 0.6) +
  geom_hline(yintercept = 8, linetype = "dashed", color = "#2E86AB", linewidth = 0.6) +
  annotate("rect", xmin = 0, xmax = 72, ymin = 8, ymax = 20, fill = "#2E86AB", alpha = 0.08) +
  annotate("text", x = 70, y = 21.5, label = "Toxic level", hjust = 1, color = "#E74C3C", size = 3.5, fontface = "bold") +
  annotate("text", x = 70, y = 6.5, label = "Sub-therapeutic", hjust = 1, color = "#2E86AB", size = 3.5, fontface = "bold") +
  annotate("text", x = 70, y = 14, label = "Therapeutic\nwindow", hjust = 1, color = "#333", size = 3.5) +
  scale_x_continuous(name = "Time (hr)",
                     limits = c(0, 72), breaks = seq(0, 72, 6), expand = c(0, 0)) +
  scale_y_continuous(name = expression("Plasma concentration (" * mu * "g/mL)"),
                     limits = c(0, 30), breaks = seq(0, 30, 2), expand = c(0, 0)) +
  labs(title = "Repeated Dosing & Therapeutic Window") +
  theme_template
dev.off()


# ================================================================
# ■ 臨床研究・統計 (Clinical Research / Statistics)
# ================================================================

# 15. ROC Curve (blank)
png("stat_roc_curve.png", width = 700, height = 700, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey60") +
  scale_x_continuous(name = "1 - Specificity  (False Positive Rate)",
                     limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
  scale_y_continuous(name = "Sensitivity  (True Positive Rate)",
                     limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
  labs(title = "ROC Curve",
       subtitle = "AUC = ____") +
  coord_equal() +
  theme_template
dev.off()

# 16. Kaplan-Meier (blank)
png("stat_kaplan_meier.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  scale_x_continuous(name = "Time (months)",
                     limits = c(0, 60), breaks = seq(0, 60, 6), expand = c(0, 0)) +
  scale_y_continuous(name = "Survival Probability",
                     limits = c(0, 1), breaks = seq(0, 1, 0.1),
                     labels = function(x) paste0(x * 100, "%"), expand = c(0, 0)) +
  labs(title = "Kaplan-Meier Survival Curve",
       subtitle = "Log-rank  p = ____") +
  theme_template
dev.off()

# 17. Forest Plot (blank)
studies <- paste("Study", LETTERS[1:8])
studies <- c(studies, "Overall")
y_pos <- rev(seq_along(studies))
fp_df <- data.frame(study = factor(studies, levels = rev(studies)), y = y_pos)

png("stat_forest_plot.png", width = 900, height = 600, res = 150)
ggplot(fp_df, aes(x = 1, y = study)) +
  geom_blank() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_x_continuous(name = "Odds Ratio / Hazard Ratio / Risk Ratio  (95% CI)",
                     limits = c(0.1, 10), trans = "log10",
                     breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10)) +
  labs(title = "Forest Plot", y = "") +
  annotate("text", x = 0.3, y = 0.6, label = "Favours Treatment", color = "grey50", size = 3.2, fontface = "italic") +
  annotate("text", x = 3.5, y = 0.6, label = "Favours Control", color = "grey50", size = 3.2, fontface = "italic") +
  theme_template
dev.off()

# 18. Bland-Altman (blank)
png("stat_bland_altman.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5) +
  annotate("text", x = 195, y = 1, label = "Mean diff = ____", hjust = 1, color = "#E74C3C", size = 3.5) +
  annotate("text", x = 195, y = 45, label = "+1.96 SD = ____", hjust = 1, color = "#F39C12", size = 3.2) +
  annotate("text", x = 195, y = -43, label = "-1.96 SD = ____", hjust = 1, color = "#F39C12", size = 3.2) +
  scale_x_continuous(name = "Mean of Two Methods",
                     limits = c(0, 200), breaks = seq(0, 200, 20), expand = c(0, 0)) +
  scale_y_continuous(name = "Difference  (Method A - Method B)",
                     limits = c(-50, 50), breaks = seq(-50, 50, 10), expand = c(0, 0)) +
  labs(title = "Bland-Altman Plot") +
  theme_template
dev.off()

# 19. Calibration Plot
png("stat_calibration.png", width = 700, height = 700, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey60") +
  scale_x_continuous(name = "Predicted Probability",
                     limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
  scale_y_continuous(name = "Observed Probability",
                     limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
  labs(title = "Calibration Plot") +
  coord_equal() +
  theme_template
dev.off()


# ================================================================
# ■ 腎臓・体液 (Nephrology / Fluid Balance)
# ================================================================

# 20. Clearance / GFR
png("nephro_clearance.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  scale_x_continuous(name = expression("Plasma concentration of marker (" * mu * "mol/L)"),
                     limits = c(0, 500), breaks = seq(0, 500, 50), expand = c(0, 0)) +
  scale_y_continuous(name = "Rate  (mg/min):  Filtration / Reabsorption / Excretion / Secretion",
                     limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  labs(title = "Renal Handling of Solute (Glucose Titration Curve)",
       subtitle = "Plot: Filtered / Reabsorbed / Excreted  (Tm = ____)") +
  theme_template
dev.off()


# ================================================================
# ■ 血液ガス・酸塩基 (Acid-Base)
# ================================================================

# 21. Davenport Diagram
png("acidbase_davenport.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_hline(yintercept = 24, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 7.4, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  annotate("text", x = 7.15, y = 42, label = "Metabolic\nalkalosis", color = "grey55", size = 3) +
  annotate("text", x = 7.65, y = 42, label = "Respiratory\nalkalosis", color = "grey55", size = 3) +
  annotate("text", x = 7.15, y = 8, label = "Respiratory\nacidosis", color = "grey55", size = 3) +
  annotate("text", x = 7.65, y = 8, label = "Metabolic\nacidosis", color = "grey55", size = 3) +
  scale_x_continuous(name = "Arterial pH",
                     limits = c(7.0, 7.8), breaks = seq(7.0, 7.8, 0.05), expand = c(0, 0)) +
  scale_y_continuous(name = expression("[HCO"[3]^{"-"} * "]  (mEq/L)"),
                     limits = c(0, 50), breaks = seq(0, 50, 5), expand = c(0, 0)) +
  labs(title = "Davenport Diagram (Acid-Base Map)") +
  theme_template
dev.off()


# ================================================================
# ■ 血液学 (Hematology)
# ================================================================

# 22. Coagulation Cascade (PT / aPTT blank grid)
png("hemato_coag_grid.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_hline(yintercept = 13, linetype = "dashed", color = "#E74C3C", linewidth = 0.4) +
  geom_hline(yintercept = 35, linetype = "dashed", color = "#2E86AB", linewidth = 0.4) +
  annotate("text", x = 0.3, y = 14, label = "PT upper limit", color = "#E74C3C", hjust = 0, size = 3) +
  annotate("text", x = 0.3, y = 36, label = "aPTT upper limit", color = "#2E86AB", hjust = 0, size = 3) +
  scale_x_continuous(name = "Sample / Time point",
                     limits = c(0, 10), breaks = seq(0, 10, 1), expand = c(0, 0)) +
  scale_y_continuous(name = "Clotting Time (seconds)",
                     limits = c(0, 80), breaks = seq(0, 80, 5), expand = c(0, 0)) +
  labs(title = "Coagulation Test Results  (PT / aPTT)") +
  theme_template
dev.off()


# ================================================================
# ■ 内分泌 (Endocrinology)
# ================================================================

# 23. Glucose Tolerance Test
png("endo_ogtt.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  geom_hline(yintercept = 200, linetype = "dashed", color = "#E74C3C", linewidth = 0.4) +
  geom_hline(yintercept = 140, linetype = "dashed", color = "#F39C12", linewidth = 0.4) +
  annotate("text", x = 118, y = 205, label = "Diabetes (200)", color = "#E74C3C", hjust = 1, size = 3) +
  annotate("text", x = 118, y = 145, label = "IGT (140)", color = "#F39C12", hjust = 1, size = 3) +
  scale_x_continuous(name = "Time after glucose load (min)",
                     limits = c(0, 120), breaks = seq(0, 120, 15), expand = c(0, 0)) +
  scale_y_continuous(name = "Blood Glucose (mg/dL)",
                     limits = c(0, 350), breaks = seq(0, 350, 25), expand = c(0, 0)) +
  labs(title = "Oral Glucose Tolerance Test (OGTT)") +
  theme_template
dev.off()

# 24. Hormone Diurnal Rhythm
png("endo_diurnal.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) +
  geom_blank() +
  annotate("rect", xmin = 22, xmax = 30, ymin = 0, ymax = 100, fill = "grey90", alpha = 0.5) +
  annotate("text", x = 26, y = 95, label = "Sleep", color = "grey50", size = 3.5) +
  scale_x_continuous(name = "Time of day (hr)",
                     limits = c(6, 30), breaks = seq(6, 30, 2),
                     labels = c("6:00","8:00","10:00","12:00","14:00","16:00",
                                "18:00","20:00","22:00","0:00","2:00","4:00","6:00"),
                     expand = c(0, 0)) +
  scale_y_continuous(name = "Hormone level (arbitrary units / ng/dL)",
                     limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  labs(title = "Hormone Diurnal Rhythm",
       subtitle = "Plot: Cortisol / GH / Melatonin / etc.") +
  theme_template +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
dev.off()


cat("All 24 templates generated successfully.\n")
