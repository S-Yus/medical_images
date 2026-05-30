library(ggplot2)

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
# ■ 微生物学 (Microbiology)
# ================================================================

# 1. Bacterial Growth Curve
png("micro_growth_curve.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  scale_x_continuous(name = "Time (hr)", limits = c(0, 24), breaks = seq(0, 24, 2), expand = c(0, 0)) +
  scale_y_log10(name = "CFU/mL  (log scale)", limits = c(1e3, 1e10),
                breaks = 10^(3:10), labels = expression(10^3, 10^4, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10)) +
  annotate("text", x = 3, y = 5e9, label = "Lag", color = "grey55", size = 3.5) +
  annotate("text", x = 9, y = 5e9, label = "Log", color = "grey55", size = 3.5) +
  annotate("text", x = 16, y = 5e9, label = "Stationary", color = "grey55", size = 3.5) +
  annotate("text", x = 22, y = 5e9, label = "Death", color = "grey55", size = 3.5) +
  labs(title = "Bacterial Growth Curve") +
  theme_template
dev.off()

# 2. Time-Kill Curve
png("micro_time_kill.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  geom_hline(yintercept = 3, linetype = "dashed", color = "#E74C3C", linewidth = 0.4) +
  annotate("text", x = 23, y = 3.3, label = "99.9% kill (3-log reduction)", color = "#E74C3C", hjust = 1, size = 3) +
  scale_x_continuous(name = "Time (hr)", limits = c(0, 24), breaks = seq(0, 24, 2), expand = c(0, 0)) +
  scale_y_continuous(name = expression("Log"[10] * " CFU/mL"), limits = c(0, 10), breaks = seq(0, 10, 1), expand = c(0, 0)) +
  labs(title = "Time-Kill Curve", subtitle = "Plot: Control / 1x MIC / 2x MIC / 4x MIC") +
  theme_template
dev.off()

# 3. MIC Determination
png("micro_mic.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  scale_x_continuous(name = expression("Antibiotic concentration (" * mu * "g/mL)  [log"[2] * " scale]"),
                     limits = c(0, 10), breaks = 0:10,
                     labels = c("0", "0.0625", "0.125", "0.25", "0.5", "1", "2", "4", "8", "16", "32")) +
  scale_y_continuous(name = expression("OD"[600] * "  (Optical Density)"),
                     limits = c(0, 1.5), breaks = seq(0, 1.5, 0.1), expand = c(0, 0)) +
  labs(title = "MIC Determination (Broth Microdilution)",
       subtitle = "MIC = lowest concentration with no visible growth") +
  theme_template + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
dev.off()

# 4. Zone of Inhibition
png("micro_disk_diffusion.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  geom_hline(yintercept = 18, linetype = "dashed", color = "#2E86AB", linewidth = 0.4) +
  geom_hline(yintercept = 13, linetype = "dashed", color = "#F39C12", linewidth = 0.4) +
  annotate("text", x = 0.3, y = 19, label = "Susceptible (S)", color = "#2E86AB", hjust = 0, size = 3) +
  annotate("text", x = 0.3, y = 14, label = "Intermediate (I)", color = "#F39C12", hjust = 0, size = 3) +
  annotate("text", x = 0.3, y = 10, label = "Resistant (R)", color = "#E74C3C", hjust = 0, size = 3) +
  scale_x_continuous(name = "Antibiotic", limits = c(0, 10), breaks = 1:10, expand = c(0, 0)) +
  scale_y_continuous(name = "Zone diameter (mm)", limits = c(0, 40), breaks = seq(0, 40, 2), expand = c(0, 0)) +
  labs(title = "Disk Diffusion (Zone of Inhibition)") +
  theme_template
dev.off()


# ================================================================
# ■ 免疫学 (Immunology)
# ================================================================

# 5. Primary / Secondary Immune Response
png("immuno_response.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey60") +
  geom_vline(xintercept = 28, linetype = "dotted", color = "grey60") +
  annotate("text", x = 1, y = 9500, label = "1st exposure", color = "grey50", hjust = 0, size = 3) +
  annotate("text", x = 29, y = 9500, label = "2nd exposure", color = "grey50", hjust = 0, size = 3) +
  scale_x_continuous(name = "Days after antigen exposure", limits = c(0, 56), breaks = seq(0, 56, 7), expand = c(0, 0)) +
  scale_y_log10(name = "Antibody titer (log scale)", limits = c(1, 10000),
                breaks = c(1, 10, 100, 1000, 10000)) +
  labs(title = "Primary & Secondary Immune Response",
       subtitle = "Plot: IgM (blue) / IgG (red) for each exposure") +
  theme_template
dev.off()

# 6. ELISA Standard Curve
png("immuno_elisa.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  scale_x_log10(name = "Standard concentration (pg/mL)", limits = c(1, 10000),
                breaks = c(1, 10, 100, 1000, 10000)) +
  scale_y_continuous(name = expression("OD"[450]), limits = c(0, 3), breaks = seq(0, 3, 0.25), expand = c(0, 0)) +
  labs(title = "ELISA Standard Curve (4-PL)") +
  theme_template
dev.off()

# 7. Complement Activity (CH50)
png("immuno_ch50.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  geom_hline(yintercept = 50, linetype = "dashed", color = "#E74C3C", linewidth = 0.4) +
  annotate("text", x = 95, y = 53, label = "CH50", color = "#E74C3C", hjust = 1, size = 3.5) +
  scale_x_continuous(name = "Serum volume / dilution", limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  scale_y_continuous(name = "Hemolysis (%)", limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  labs(title = "Complement Hemolytic Activity (CH50)") +
  theme_template
dev.off()

# 8. Flow Cytometry Dot Plot Template
png("immuno_facs.png", width = 700, height = 700, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "grey60", linewidth = 0.3) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "grey60", linewidth = 0.3) +
  annotate("text", x = 0.5, y = 4.5, label = "Q1\nCD4-/CD8+", color = "grey50", size = 3) +
  annotate("text", x = 4.5, y = 4.5, label = "Q2\nCD4+/CD8+", color = "grey50", size = 3) +
  annotate("text", x = 0.5, y = 0.5, label = "Q3\nCD4-/CD8-", color = "grey50", size = 3) +
  annotate("text", x = 4.5, y = 0.5, label = "Q4\nCD4+/CD8-", color = "grey50", size = 3) +
  scale_x_continuous(name = "CD4 (fluorescence intensity, log)", limits = c(0, 5), breaks = 0:5, expand = c(0, 0)) +
  scale_y_continuous(name = "CD8 (fluorescence intensity, log)", limits = c(0, 5), breaks = 0:5, expand = c(0, 0)) +
  labs(title = "Flow Cytometry Dot Plot") +
  coord_equal() + theme_template
dev.off()


# ================================================================
# ■ 疫学・公衆衛生 (Epidemiology / Public Health)
# ================================================================

# 9. Epidemic Curve
png("epi_epidemic_curve.png", width = 900, height = 550, res = 150)
days <- data.frame(day = 1:30)
ggplot(days, aes(x = day)) + geom_blank() +
  scale_x_continuous(name = "Date / Day of outbreak", limits = c(0.5, 30.5), breaks = 1:30, expand = c(0, 0)) +
  scale_y_continuous(name = "Number of cases", limits = c(0, 30), breaks = seq(0, 30, 2), expand = c(0, 0)) +
  labs(title = "Epidemic Curve (Epi Curve)",
       subtitle = "Use geom_bar() to add case counts per day") +
  theme_template + theme(axis.text.x = element_text(size = 8))
dev.off()

# 10. 2x2 Table Template
png("epi_2x2_table.png", width = 700, height = 500, res = 150)
tbl <- data.frame(
  x = c(1, 2, 1, 2, 1, 2, 0, 0, 3, 3, 0, 3),
  y = c(2, 2, 1, 1, 0, 0, 2, 1, 2, 1, 0, 0),
  label = c("a", "b", "c", "d",
            "a + b", "c + d",
            "a + c", "b + d",
            "a + c", "b + d",
            "n", "n")
)
ggplot() +
  geom_tile(data = data.frame(x = c(1,2,1,2), y = c(2,2,1,1)),
            aes(x, y), fill = NA, color = "grey30", linewidth = 1.2, width = 0.95, height = 0.95) +
  annotate("text", x = 1, y = 2.65, label = "Exposed", fontface = "bold", size = 4) +
  annotate("text", x = 2, y = 2.65, label = "Unexposed", fontface = "bold", size = 4) +
  annotate("text", x = 0.2, y = 2, label = "Disease+", fontface = "bold", size = 4, hjust = 0) +
  annotate("text", x = 0.2, y = 1, label = "Disease−", fontface = "bold", size = 4, hjust = 0) +
  annotate("text", x = 1, y = 2, label = "a", size = 6, color = "#2E86AB") +
  annotate("text", x = 2, y = 2, label = "b", size = 6, color = "#2E86AB") +
  annotate("text", x = 1, y = 1, label = "c", size = 6, color = "#2E86AB") +
  annotate("text", x = 2, y = 1, label = "d", size = 6, color = "#2E86AB") +
  annotate("text", x = 1.5, y = 0.2, label = "OR = (a×d) / (b×c)\nRR = [a/(a+b)] / [c/(c+d)]", size = 3.5, color = "grey40") +
  scale_x_continuous(limits = c(-0.2, 3.2)) +
  scale_y_continuous(limits = c(-0.2, 3)) +
  labs(title = "2×2 Contingency Table") +
  theme_void() + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16))
dev.off()

# 11. Age-specific Incidence Rate
png("epi_age_incidence.png", width = 800, height = 600, res = 150)
age_groups <- data.frame(age = factor(c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"),
                                      levels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")))
ggplot(age_groups, aes(x = age, y = 0)) + geom_blank() +
  scale_y_continuous(name = "Incidence rate (per 100,000 person-years)",
                     limits = c(0, 500), breaks = seq(0, 500, 50), expand = c(0, 0)) +
  labs(title = "Age-Specific Incidence Rate", x = "Age group") +
  theme_template + theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# 12. Screening Performance
png("epi_screening.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  scale_x_continuous(name = "Prevalence (%)", limits = c(0, 50), breaks = seq(0, 50, 5), expand = c(0, 0)) +
  scale_y_continuous(name = "Predictive Value (%)", limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  labs(title = "PPV & NPV vs Prevalence",
       subtitle = "Plot: PPV / NPV at fixed Se & Sp") +
  theme_template
dev.off()


# ================================================================
# ■ 神経科学 (Neuroscience)
# ================================================================

# 13. EEG Waveform Template (multi-channel)
png("neuro_eeg.png", width = 900, height = 700, res = 150)
channels <- c("Fp1", "Fp2", "F3", "F4", "C3", "C4", "P3", "P4", "O1", "O2")
ch_df <- data.frame(channel = factor(channels, levels = rev(channels)), y = rev(seq_along(channels)))
ggplot(ch_df, aes(x = 0, y = channel)) + geom_blank() +
  scale_x_continuous(name = "Time (seconds)", limits = c(0, 10), breaks = seq(0, 10, 0.5), expand = c(0, 0)) +
  labs(title = "EEG Recording Template (10-20 system)", y = "Channel") +
  theme_template + theme(panel.grid.major.y = element_line(color = "grey80"))
dev.off()

# 14. Nerve Conduction Velocity
png("neuro_ncv.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  geom_hline(yintercept = 0, color = "grey50") +
  scale_x_continuous(name = "Time (ms)", limits = c(0, 20), breaks = seq(0, 20, 1), expand = c(0, 0)) +
  scale_y_continuous(name = "Amplitude (mV)", limits = c(-5, 15), breaks = seq(-5, 15, 1), expand = c(0, 0)) +
  labs(title = "Nerve Conduction Study (NCS)",
       subtitle = "Latency = ____ms,  Amplitude = ____mV,  NCV = ____m/s") +
  theme_template
dev.off()

# 15. EMG Template
png("neuro_emg.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  geom_hline(yintercept = 0, color = "grey50") +
  scale_x_continuous(name = "Time (ms)", limits = c(0, 100), breaks = seq(0, 100, 5), expand = c(0, 0)) +
  scale_y_continuous(name = expression("Amplitude (" * mu * "V)"),
                     limits = c(-500, 500), breaks = seq(-500, 500, 100), expand = c(0, 0)) +
  labs(title = "Electromyography (EMG)",
       subtitle = "Rest / Mild contraction / Maximal contraction") +
  theme_template
dev.off()

# 16. Pupil Light Reflex
png("neuro_pupil.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  annotate("rect", xmin = 2, xmax = 4, ymin = 0, ymax = 10, fill = "#fef3c7", alpha = 0.4) +
  annotate("text", x = 3, y = 9.5, label = "Light ON", color = "#b45309", size = 3.5) +
  scale_x_continuous(name = "Time (seconds)", limits = c(0, 8), breaks = seq(0, 8, 0.5), expand = c(0, 0)) +
  scale_y_continuous(name = "Pupil diameter (mm)", limits = c(0, 10), breaks = seq(0, 10, 0.5), expand = c(0, 0)) +
  labs(title = "Pupillary Light Reflex") +
  theme_template
dev.off()


# ================================================================
# ■ 解剖学 (Anatomy)
# ================================================================

# 17. Dermatome Chart Grid
png("anat_dermatome.png", width = 800, height = 600, res = 150)
levels <- c("C2","C3","C4","C5","C6","C7","C8","T1","T2","T3","T4","T5","T6",
            "T7","T8","T9","T10","T11","T12","L1","L2","L3","L4","L5","S1","S2","S3","S4","S5")
derm_df <- data.frame(level = factor(levels, levels = levels))
ggplot(derm_df, aes(x = level, y = 0)) + geom_blank() +
  scale_y_continuous(name = "Sensory score (0 = absent, 1 = impaired, 2 = normal)",
                     limits = c(0, 2), breaks = 0:2, expand = c(0.05, 0.05)) +
  labs(title = "Dermatome Sensory Assessment", x = "Spinal level") +
  theme_template + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))
dev.off()


# ================================================================
# ■ 病理学 (Pathology)
# ================================================================

# 18. Cell Count Template
png("path_cell_count.png", width = 800, height = 600, res = 150)
cells <- c("Neutrophil", "Lymphocyte", "Monocyte", "Eosinophil", "Basophil")
cell_df <- data.frame(cell = factor(cells, levels = cells))
ggplot(cell_df, aes(x = cell, y = 0)) + geom_blank() +
  scale_y_continuous(name = "Percentage (%)", limits = c(0, 80), breaks = seq(0, 80, 5), expand = c(0, 0)) +
  labs(title = "Differential White Blood Cell Count", x = "Cell type") +
  theme_template
dev.off()


# ================================================================
# ■ 循環器内科 (Cardiology) - Wiggers追加
# ================================================================

# 19. Wiggers Diagram
png("cardio_wiggers.png", width = 900, height = 700, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  scale_x_continuous(name = "Time (seconds)", limits = c(0, 0.8), breaks = seq(0, 0.8, 0.05), expand = c(0, 0)) +
  scale_y_continuous(name = "Pressure (mmHg) / Volume (mL)",
                     limits = c(0, 140), breaks = seq(0, 140, 10), expand = c(0, 0)) +
  labs(title = "Wiggers Diagram",
       subtitle = "Aortic P / LV P / LA P / LV Volume / ECG / Heart sounds") +
  theme_template
dev.off()


# ================================================================
# ■ 呼吸器 (Pulmonology)
# ================================================================

# 20. Lung Volumes & Capacities
png("pulm_volumes.png", width = 900, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  scale_x_continuous(name = "Time (seconds)", limits = c(0, 30), breaks = seq(0, 30, 2), expand = c(0, 0)) +
  scale_y_continuous(name = "Volume (L)", limits = c(0, 7), breaks = seq(0, 7, 0.5), expand = c(0, 0)) +
  annotate("text", x = 28, y = 6.5, label = "TLC", color = "grey50", hjust = 1, size = 3) +
  annotate("text", x = 28, y = 1.5, label = "RV", color = "grey50", hjust = 1, size = 3) +
  labs(title = "Lung Volumes & Capacities (Spirometry)",
       subtitle = "TV / IRV / ERV / RV / VC / FRC / TLC") +
  theme_template
dev.off()

# 21. A-aDO2
png("pulm_aado2.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey60") +
  scale_x_continuous(name = expression("P"["A"] * O[2] ~ "(Alveolar, mmHg)"),
                     limits = c(0, 150), breaks = seq(0, 150, 10), expand = c(0, 0)) +
  scale_y_continuous(name = expression("P"["a"] * O[2] ~ "(Arterial, mmHg)"),
                     limits = c(0, 150), breaks = seq(0, 150, 10), expand = c(0, 0)) +
  labs(title = expression("A-a Gradient  (P"["A"] * O[2] ~ "vs P"["a"] * O[2] * ")"),
       subtitle = expression("Normal A-aDO"[2] * " < 10-15 mmHg")) +
  coord_equal() + theme_template
dev.off()


# ================================================================
# ■ 消化器 (Gastroenterology)
# ================================================================

# 22. Gastric Acid Secretion
png("gi_acid_secretion.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 40, fill = "#fef3c7", alpha = 0.3) +
  annotate("text", x = 1, y = 38, label = "Meal", color = "#b45309", size = 3) +
  scale_x_continuous(name = "Time (hr)", limits = c(0, 6), breaks = seq(0, 6, 0.5), expand = c(0, 0)) +
  scale_y_continuous(name = "Gastric acid output (mEq/hr)", limits = c(0, 40), breaks = seq(0, 40, 5), expand = c(0, 0)) +
  labs(title = "Gastric Acid Secretion",
       subtitle = "Cephalic / Gastric / Intestinal phase") +
  theme_template
dev.off()

# 23. Liver Function Tests over time
png("gi_lft.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  geom_hline(yintercept = 40, linetype = "dashed", color = "#F39C12", linewidth = 0.3) +
  annotate("text", x = 0.3, y = 43, label = "Upper normal limit (ALT/AST ~40)", color = "#F39C12", hjust = 0, size = 3) +
  scale_x_continuous(name = "Days / Weeks", limits = c(0, 30), breaks = seq(0, 30, 2), expand = c(0, 0)) +
  scale_y_continuous(name = "Enzyme level (U/L)", limits = c(0, 500), breaks = seq(0, 500, 50), expand = c(0, 0)) +
  labs(title = "Liver Function Tests (LFT) Time Course",
       subtitle = "Plot: AST / ALT / ALP / GGT / T-Bil") +
  theme_template
dev.off()


# ================================================================
# ■ 整形外科 (Orthopedics)
# ================================================================

# 24. Range of Motion
png("ortho_rom.png", width = 800, height = 600, res = 150)
joints <- c("Shoulder\nFlexion", "Shoulder\nAbduction", "Elbow\nFlexion", "Wrist\nFlexion",
            "Hip\nFlexion", "Hip\nAbduction", "Knee\nFlexion", "Ankle\nDorsiflexion")
joint_df <- data.frame(joint = factor(joints, levels = joints))
ggplot(joint_df, aes(x = joint, y = 0)) + geom_blank() +
  scale_y_continuous(name = "Range of Motion (degrees)", limits = c(0, 180), breaks = seq(0, 180, 10), expand = c(0, 0)) +
  labs(title = "Joint Range of Motion (ROM) Chart", x = "") +
  theme_template + theme(axis.text.x = element_text(size = 8, lineheight = 0.9))
dev.off()


# ================================================================
# ■ 眼科 (Ophthalmology)
# ================================================================

# 25. Visual Acuity over time
png("eye_visual_acuity.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  scale_x_continuous(name = "Time (weeks / months)", limits = c(0, 12), breaks = seq(0, 12, 1), expand = c(0, 0)) +
  scale_y_continuous(name = "LogMAR Visual Acuity",
                     limits = c(-0.2, 1.5), breaks = seq(-0.2, 1.5, 0.1), expand = c(0, 0)) +
  annotate("text", x = 11.5, y = 0, label = "20/20", color = "grey50", hjust = 1, size = 3) +
  annotate("text", x = 11.5, y = 0.3, label = "20/40", color = "grey50", hjust = 1, size = 3) +
  annotate("text", x = 11.5, y = 1.0, label = "20/200", color = "grey50", hjust = 1, size = 3) +
  labs(title = "Visual Acuity Over Time (LogMAR)") +
  theme_template
dev.off()


# ================================================================
# ■ 産婦人科 (Obstetrics / Gynecology)
# ================================================================

# 26. Partograph (Cervical Dilation)
png("obgyn_partograph.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  geom_hline(yintercept = 4, linetype = "dashed", color = "#2E86AB", linewidth = 0.4) +
  annotate("text", x = 0.3, y = 4.3, label = "Active phase (4 cm)", color = "#2E86AB", hjust = 0, size = 3) +
  scale_x_continuous(name = "Time (hours from admission)", limits = c(0, 24), breaks = seq(0, 24, 1), expand = c(0, 0)) +
  scale_y_continuous(name = "Cervical dilation (cm)", limits = c(0, 10), breaks = seq(0, 10, 1), expand = c(0, 0)) +
  labs(title = "Partograph (Cervicograph)",
       subtitle = "Plot: Cervical dilation & Fetal descent (station)") +
  theme_template
dev.off()

# 27. Fetal Growth Curve
png("obgyn_fetal_growth.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  scale_x_continuous(name = "Gestational age (weeks)", limits = c(12, 42), breaks = seq(12, 42, 2), expand = c(0, 0)) +
  scale_y_continuous(name = "Estimated fetal weight (g)", limits = c(0, 5000), breaks = seq(0, 5000, 500), expand = c(0, 0)) +
  labs(title = "Fetal Growth Curve",
       subtitle = "Plot: 10th / 50th / 90th percentile & patient data") +
  theme_template
dev.off()


# ================================================================
# ■ 小児科 (Pediatrics)
# ================================================================

# 28. Growth Chart (Weight)
png("peds_growth_weight.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  scale_x_continuous(name = "Age (months)", limits = c(0, 36), breaks = seq(0, 36, 3), expand = c(0, 0)) +
  scale_y_continuous(name = "Weight (kg)", limits = c(0, 18), breaks = seq(0, 18, 1), expand = c(0, 0)) +
  labs(title = "Growth Chart (Weight-for-Age, 0-36 months)",
       subtitle = "Plot: 3rd / 15th / 50th / 85th / 97th percentile") +
  theme_template
dev.off()

# 29. Growth Chart (Height)
png("peds_growth_height.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  scale_x_continuous(name = "Age (years)", limits = c(0, 20), breaks = seq(0, 20, 1), expand = c(0, 0)) +
  scale_y_continuous(name = "Height (cm)", limits = c(40, 190), breaks = seq(40, 190, 10), expand = c(0, 0)) +
  labs(title = "Growth Chart (Height-for-Age, 0-20 years)",
       subtitle = "Plot: 3rd / 50th / 97th percentile & patient data") +
  theme_template
dev.off()


# ================================================================
# ■ 精神科 (Psychiatry)
# ================================================================

# 30. Symptom Rating Scale over time
png("psych_rating_scale.png", width = 800, height = 600, res = 150)
ggplot(empty, aes(x, y)) + geom_blank() +
  scale_x_continuous(name = "Week of treatment", limits = c(0, 12), breaks = seq(0, 12, 1), expand = c(0, 0)) +
  scale_y_continuous(name = "Symptom score (e.g. HAM-D / PANSS / MADRS)",
                     limits = c(0, 50), breaks = seq(0, 50, 5), expand = c(0, 0)) +
  labs(title = "Psychiatric Symptom Rating Scale",
       subtitle = "Plot treatment response over time") +
  theme_template
dev.off()


cat("All 30 new templates generated successfully.\n")
