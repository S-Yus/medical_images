#!/usr/bin/env Rscript
# ============================================================
# MedGraph Free — Gap Detection & Coverage Analysis
# ============================================================
# Purpose: Systematically identify missing/underserved medical
#          graph templates by multi-axis classification.
# Output:  analysis/ directory with JSON + Markdown reports
# NOTE:    Does NOT modify site structure or generate images.
# ============================================================

suppressPackageStartupMessages({
  library(jsonlite)
})

cat("=== MedGraph Gap Analysis ===\n")
cat(sprintf("Start: %s\n", Sys.time()))

OUT_DIR <- "analysis"
dir.create(OUT_DIR, showWarnings = FALSE)

# ── 1. Load existing templates ────────────────────────────────
# Minimal D() — just capture metadata, no ggplot needed
D <- function(id, cat, en, ja, xl, yl, xr, yr, dj, tags = "",
              sub = NULL, xlog = FALSE, ylog = FALSE,
              hl = NULL, vl = NULL, ann = NULL, zones = NULL,
              xb = NULL, yb = NULL, xlb = NULL, ylb = NULL,
              type = "xy") {
  list(id = id, cat = cat, en = en, ja = ja,
       xl = xl, yl = yl, xr = xr, yr = yr,
       dj = dj, tags = tags, sub = sub,
       xlog = xlog, ylog = ylog,
       hl = hl, vl = vl, ann = ann, zones = zones,
       xb = xb, yb = yb, xlb = xlb, ylb = ylb,
       type = type)
}

source("template_defs.R", local = TRUE)
N <- length(TEMPLATES)
cat(sprintf("Loaded %d existing templates\n", N))

# ── 2. Category metadata ─────────────────────────────────────
CAT_META <- list(
  biochem  = list(ja = "生化学",       field = "basic_science"),
  physiol  = list(ja = "生理学",       field = "basic_science"),
  pharm    = list(ja = "薬理学",       field = "basic_science"),
  micro    = list(ja = "微生物学",     field = "basic_science"),
  immuno   = list(ja = "免疫学",       field = "basic_science"),
  anat     = list(ja = "解剖学",       field = "basic_science"),
  path     = list(ja = "病理学",       field = "basic_science"),
  epi      = list(ja = "疫学・公衆衛生", field = "public_health"),
  stat     = list(ja = "臨床統計",     field = "research_methods"),
  neuro    = list(ja = "神経科学",     field = "clinical"),
  cardio   = list(ja = "循環器",       field = "clinical"),
  pulm     = list(ja = "呼吸器",       field = "clinical"),
  nephro   = list(ja = "腎臓",         field = "clinical"),
  endo     = list(ja = "内分泌",       field = "clinical"),
  gi       = list(ja = "消化器",       field = "clinical"),
  hemato   = list(ja = "血液",         field = "clinical"),
  obgyn    = list(ja = "産婦人科",     field = "clinical"),
  peds     = list(ja = "小児科",       field = "clinical"),
  ortho    = list(ja = "整形外科",     field = "clinical"),
  eye      = list(ja = "眼科",         field = "clinical"),
  psych    = list(ja = "精神科",       field = "clinical"),
  radio    = list(ja = "放射線",       field = "clinical"),
  surg     = list(ja = "外科・麻酔",   field = "clinical"),
  emer     = list(ja = "救急",         field = "clinical"),
  chem     = list(ja = "臨床検査",     field = "diagnostic"),
  derm     = list(ja = "皮膚科",       field = "clinical"),
  ent      = list(ja = "耳鼻咽喉科",   field = "clinical"),
  uro      = list(ja = "泌尿器科",     field = "clinical")
)

# ── 3. Classification Axis Definitions ───────────────────────

## 3a. Figure Type taxonomy
FIGURE_TYPES <- c(
  # Standard XY
  "line_chart", "scatter_plot", "curve_fit", "log_plot",
  # Statistical
  "boxplot", "violin", "dot_plot", "histogram", "bar_chart",
  "forest_plot", "funnel_plot", "bland_altman",
  # Survival / Diagnostic

  "kaplan_meier", "roc_curve", "calibration_plot",
  # Special medical
  "ecg_tracing", "audiogram", "visual_field", "growth_chart",
  "flow_volume_loop", "pv_loop", "bullseye",
  # Epidemiological
  "epicurve", "pyramid", "tornado",
  # Genomic
  "manhattan_plot", "volcano_plot",
  # Multi-dimensional
  "radar_chart", "heatmap", "ternary",
  # Clinical tools
  "nomogram", "swimmer_plot", "table_2x2", "pedigree",
  # Regression
  "scatter_regression"
)

## 3b. Data Type taxonomy
DATA_TYPES <- c(
  "continuous_numeric",     # enzyme kinetics, PK, dose-response

  "categorical",            # group comparisons
  "time_series",            # monitoring, trends
  "survival_data",          # censored time-to-event
  "diagnostic_accuracy",    # sensitivity/specificity
  "proportion_rate",        # prevalence, incidence
  "genomic_molecular",      # gene expression, SNPs
  "physiological_signal",   # ECG, EEG, spirometry
  "pharmacokinetic",        # ADME, concentration-time
  "ordinal_score",          # scales, ratings
  "spatial_anatomical",     # dermatomes, visual fields
  "compositional",          # ternary, body fluid
  "count_frequency"         # histogram, frequency distribution
)

## 3c. Purpose taxonomy
PURPOSES <- c(
  "comparison",             # compare groups/conditions
  "trend_monitoring",       # track over time
  "distribution",           # show data spread
  "correlation",            # relationship between vars
  "diagnostic_eval",        # test performance
  "survival_analysis",      # time-to-event
  "dose_response",          # PK/PD, drug effect
  "kinetic_dynamic",        # enzyme, reaction kinetics
  "reference_standard",     # normal ranges, standards
  "screening_triage",       # clinical decision tools
  "meta_analysis",          # evidence synthesis
  "quality_assessment",     # QC, agreement
  "classification",         # categorize patients/samples
  "genetic_analysis",       # linkage, GWAS
  "epidemiologic_summary"   # outbreak, population
)

## 3d. Comparison Structure
COMPARISON_STRUCTURES <- c(
  "single_entity",          # one curve/measurement
  "paired",                 # before-after, two methods
  "multi_group",            # 3+ groups
  "control_vs_treatment",   # experimental design
  "time_course",            # repeated measures
  "hierarchical",           # nested/meta
  "two_by_two"              # 2x2 table
)

## 3e. Annotation Features
ANNOTATION_FEATURES <- c(
  "reference_lines",        # hl, vl
  "zones_bands",            # colored zones
  "text_labels",            # annotations
  "thresholds",             # clinical cutoffs
  "guide_grids",            # radar, polar grids
  "none"
)

## 3f. Use Case
USE_CASES <- c(
  "lecture_handout",        # classroom teaching
  "lab_report",             # practical/experiment
  "clinical_report",        # patient data
  "research_paper",         # publication figure
  "case_presentation",      # clinical conference
  "thesis_dissertation",    # academic work
  "exam_prep"               # study aid
)

# ── 4. Synonym Dictionary ────────────────────────────────────
# Maps variant terms → canonical form for normalized matching

SYNONYMS <- list(
  # Figure type synonyms
  "kaplan-meier"    = c("km", "kaplan", "meier", "survival curve", "カプランマイヤー", "生存曲線"),
  "roc"             = c("roc curve", "receiver operating", "ROC曲線", "感度特異度"),
  "forest"          = c("forest plot", "meta-analysis plot", "フォレストプロット", "メタアナリシス"),
  "boxplot"         = c("box plot", "box-and-whisker", "箱ひげ図", "ボックスプロット"),
  "violin"          = c("violin plot", "バイオリンプロット"),
  "heatmap"         = c("heat map", "ヒートマップ", "熱地図"),
  "scatter"         = c("scatter plot", "scattergram", "散布図"),
  "bar"             = c("bar chart", "bar graph", "棒グラフ"),
  "histogram"       = c("ヒストグラム", "度数分布"),
  "bland-altman"    = c("bland altman", "agreement plot", "ブランドアルトマン", "一致性"),
  "funnel"          = c("funnel plot", "ファネルプロット", "出版バイアス"),
  "ecg"             = c("ekg", "electrocardiogram", "心電図", "ECG"),
  "audiogram"       = c("聴力図", "オージオグラム"),
  "nomogram"        = c("ノモグラム", "計算図表"),
  "manhattan"       = c("manhattan plot", "マンハッタンプロット", "GWAS"),
  "volcano"         = c("volcano plot", "ボルケーノプロット", "差次的発現"),
  "pv-loop"         = c("pressure volume", "圧容量ループ", "PVループ"),
  "flow-volume"     = c("フローボリューム", "肺機能"),
  "growth-chart"    = c("growth curve", "成長曲線", "パーセンタイル"),
  "swimmer"         = c("swimmer plot", "スイマープロット", "治療経過"),
  "tornado"         = c("tornado diagram", "トルネード図", "感度分析"),
  "pyramid"         = c("population pyramid", "人口ピラミッド"),
  "epicurve"        = c("epidemic curve", "流行曲線", "エピカーブ"),
  "calibration"     = c("calibration plot", "キャリブレーション", "予測精度"),
  "radar"           = c("spider chart", "radar chart", "レーダーチャート"),
  "pedigree"        = c("family tree", "家系図", "遺伝系図"),
  "bullseye"        = c("bulls eye", "ブルズアイ", "極座標マップ"),

  # Medical field synonyms
  "biochemistry"    = c("biochem", "生化学", "分子生物学"),
  "physiology"      = c("physiol", "生理学", "機能"),
  "pharmacology"    = c("pharm", "薬理学", "薬物動態", "PK/PD"),
  "microbiology"    = c("micro", "微生物学", "感染症", "抗菌薬"),
  "immunology"      = c("immuno", "免疫学", "アレルギー"),
  "epidemiology"    = c("epi", "疫学", "公衆衛生", "統計"),
  "cardiology"      = c("cardio", "循環器", "心臓", "不整脈"),
  "pulmonology"     = c("pulm", "呼吸器", "肺", "気管支"),
  "nephrology"      = c("nephro", "腎臓", "透析", "電解質"),
  "endocrinology"   = c("endo", "内分泌", "ホルモン", "糖尿病"),
  "gastroenterology"= c("gi", "消化器", "肝臓", "消化管"),
  "hematology"      = c("hemato", "血液", "凝固", "貧血"),
  "obstetrics"      = c("obgyn", "産婦人科", "妊娠", "分娩"),
  "pediatrics"      = c("peds", "小児科", "新生児", "成長"),
  "orthopedics"     = c("ortho", "整形外科", "骨", "関節"),
  "ophthalmology"   = c("eye", "眼科", "視力", "視野"),
  "psychiatry"      = c("psych", "精神科", "心理", "認知"),
  "neuroscience"    = c("neuro", "神経科学", "脳", "神経"),
  "pathology"       = c("path", "病理学", "組織", "細胞診"),
  "radiology"       = c("radio", "放射線", "画像", "線量"),
  "surgery"         = c("surg", "外科", "麻酔", "手術"),
  "emergency"       = c("emer", "救急", "外傷", "蘇生"),
  "dermatology"     = c("derm", "皮膚科", "皮膚"),
  "ent"             = c("耳鼻咽喉科", "耳鼻科", "聴覚", "聴力"),
  "urology"         = c("uro", "泌尿器科", "腎", "前立腺"),

  # Data type synonyms
  "dose-response"   = c("用量反応", "dose response", "EC50", "IC50"),
  "pharmacokinetics"= c("PK", "薬物動態", "血中濃度", "AUC", "Cmax"),
  "enzyme-kinetics" = c("酵素反応", "Michaelis", "Km", "Vmax"),
  "survival"        = c("生存", "死亡", "予後", "hazard"),
  "diagnostic"      = c("診断", "スクリーニング", "感度", "特異度"),
  "growth"          = c("成長", "発達", "percentile", "パーセンタイル"),
  "electrophysiology" = c("電気生理", "活動電位", "action potential"),
  "hemodynamics"    = c("血行動態", "圧", "血圧", "心拍出量")
)

# ── 5. Classification Rules ──────────────────────────────────
# Classify each template along 7 axes using type + keywords

classify_figure_type <- function(t) {
  tp <- t$type
  switch(tp,
    kaplan       = "kaplan_meier",
    roc          = "roc_curve",
    forest       = "forest_plot",
    funnel       = "funnel_plot",
    bland_altman = "bland_altman",
    boxplot      = "boxplot",
    violin       = "violin",
    ecg          = "ecg_tracing",
    audiogram    = "audiogram",
    polar        = "visual_field",
    flow_volume  = "flow_volume_loop",
    pv_loop      = "pv_loop",
    growth       = "growth_chart",
    bullseye     = "bullseye",
    swimmer      = "swimmer_plot",
    tornado      = "tornado",
    pyramid      = "pyramid",
    manhattan    = "manhattan_plot",
    volcano      = "volcano_plot",
    dot          = "dot_plot",
    nomogram     = "nomogram",
    epicurve     = "epicurve",
    table2x2     = "table_2x2",
    pedigree     = "pedigree",
    calibration  = "calibration_plot",
    scatter_reg  = "scatter_regression",
    radar        = "radar_chart",
    ternary      = "ternary",
    heatmap      = "heatmap",
    # Default xy — further classify by keywords
    {
      txt <- tolower(paste(t$en, t$ja, t$tags, t$dj))
      if (grepl("log.*plot|semi.?log", txt)) "log_plot"
      else if (grepl("scatter|散布", txt)) "scatter_plot"
      else if (grepl("bar|棒", txt)) "bar_chart"
      else if (grepl("histogram|度数|ヒストグラム", txt)) "histogram"
      else if (t$xlog || t$ylog) "log_plot"
      else if (!is.null(t$sub) && grepl("curve|曲線", tolower(t$sub))) "curve_fit"
      else "line_chart"
    }
  )
}

classify_data_type <- function(t) {
  txt <- tolower(paste(t$en, t$ja, t$tags, t$dj, t$xl, t$yl))
  if (t$type %in% c("kaplan", "swimmer")) return("survival_data")
  if (t$type %in% c("roc", "calibration")) return("diagnostic_accuracy")
  if (t$type %in% c("ecg")) return("physiological_signal")
  if (t$type %in% c("manhattan", "volcano", "pedigree")) return("genomic_molecular")
  if (t$type %in% c("growth")) return("time_series")
  if (t$type %in% c("epicurve")) return("count_frequency")
  if (t$type %in% c("pyramid")) return("proportion_rate")
  if (t$type %in% c("table2x2")) return("categorical")
  if (t$type %in% c("radar")) return("ordinal_score")
  if (t$type %in% c("ternary")) return("compositional")
  if (t$type %in% c("bullseye", "polar", "audiogram")) return("spatial_anatomical")

  if (grepl("pk|pharmacokinet|血中濃度|薬物動態|cmax|auc|半減期", txt)) return("pharmacokinetic")
  if (grepl("酵素|enzyme|km|vmax|kinetic|速度論", txt)) return("continuous_numeric")
  if (grepl("score|スコア|scale|尺度|rating|評価", txt)) return("ordinal_score")
  if (grepl("time|時間|temporal|経時|day|hour|month|year|年|月|日", txt)) return("time_series")
  if (grepl("surviv|生存|死亡|予後|hazard", txt)) return("survival_data")
  if (grepl("diagnos|感度|特異度|roc|sensitivity|specificity", txt)) return("diagnostic_accuracy")
  if (grepl("prevalence|incidence|有病率|罹患率|rate|割合", txt)) return("proportion_rate")
  if (grepl("gene|遺伝|dna|snp|allele|pcr|genome", txt)) return("genomic_molecular")
  if (grepl("ecg|eeg|emg|電位|wave|波形|signal", txt)) return("physiological_signal")
  if (grepl("group|群|comparison|比較|vs|差", txt)) return("categorical")
  if (grepl("frequency|頻度|count|度数|件数", txt)) return("count_frequency")
  "continuous_numeric"
}

classify_purpose <- function(t) {
  txt <- tolower(paste(t$en, t$ja, t$tags, t$dj))
  tp <- t$type

  if (tp %in% c("forest", "funnel")) return("meta_analysis")
  if (tp %in% c("bland_altman")) return("quality_assessment")
  if (tp %in% c("kaplan", "swimmer")) return("survival_analysis")
  if (tp %in% c("roc", "calibration", "table2x2")) return("diagnostic_eval")
  if (tp %in% c("epicurve")) return("epidemiologic_summary")
  if (tp %in% c("manhattan", "volcano", "pedigree")) return("genetic_analysis")
  if (tp %in% c("nomogram")) return("screening_triage")
  if (tp %in% c("tornado")) return("comparison")
  if (tp %in% c("pyramid")) return("epidemiologic_summary")
  if (tp %in% c("growth")) return("reference_standard")

  if (grepl("dose.?response|用量反応|ec50|ic50|ed50|ld50", txt)) return("dose_response")
  if (grepl("kinetic|速度|enzyme|酵素|reaction|反応", txt)) return("kinetic_dynamic")
  if (grepl("compar|比較|versus|vs|differ|差", txt)) return("comparison")
  if (grepl("trend|推移|monitor|モニタ|経時|変化|tracking", txt)) return("trend_monitoring")
  if (grepl("distribut|分布|spread|ばらつき", txt)) return("distribution")
  if (grepl("correlat|相関|associat|関連|relationship|関係", txt)) return("correlation")
  if (grepl("diagnos|診断|screen|スクリーニング|検査性能", txt)) return("diagnostic_eval")
  if (grepl("surviv|生存|予後|mortality|死亡", txt)) return("survival_analysis")
  if (grepl("reference|基準|normal|正常|standard|標準|検量", txt)) return("reference_standard")
  if (grepl("classif|分類|stage|ステージ|grade|グレード", txt)) return("classification")
  if (grepl("epidem|疫学|outbreak|アウトブレイク|prevalence|有病", txt)) return("epidemiologic_summary")
  "correlation"
}

classify_comparison <- function(t) {
  txt <- tolower(paste(t$en, t$ja, t$tags, t$dj, if(!is.null(t$sub)) t$sub else ""))
  tp <- t$type

  if (tp == "table2x2") return("two_by_two")
  if (tp == "forest") return("hierarchical")
  if (tp %in% c("bland_altman")) return("paired")

  if (grepl("comparison|比較|multiple|複数|multi", txt)) return("multi_group")
  if (grepl("before.*after|前後|paired|対応|pre.*post", txt)) return("paired")
  if (grepl("control.*treat|対照|群間|intervention|介入|vs", txt)) return("control_vs_treatment")
  if (grepl("time.?course|経時|serial|連続|longitudinal|縦断", txt)) return("time_course")
  if (!is.null(t$sub) && grepl("/", t$sub)) return("control_vs_treatment")
  "single_entity"
}

classify_annotations <- function(t) {
  feats <- c()
  if (!is.null(t$hl) || !is.null(t$vl)) feats <- c(feats, "reference_lines")
  if (!is.null(t$zones))                 feats <- c(feats, "zones_bands")
  if (!is.null(t$ann))                   feats <- c(feats, "text_labels")
  if (t$type %in% c("radar", "polar", "bullseye", "audiogram")) feats <- c(feats, "guide_grids")
  txt <- tolower(paste(t$en, t$ja, t$tags, t$dj))
  if (grepl("threshold|閾値|cutoff|カットオフ|基準値", txt)) feats <- c(feats, "thresholds")
  if (length(feats) == 0) feats <- "none"
  paste(feats, collapse = ",")
}

classify_use_case <- function(t) {
  txt <- tolower(paste(t$en, t$ja, t$tags, t$dj, t$cat))
  cat_field <- if (!is.null(CAT_META[[t$cat]])) CAT_META[[t$cat]]$field else "clinical"

  cases <- c()
  # Research paper figures
  if (t$type %in% c("forest", "funnel", "kaplan", "roc", "manhattan", "volcano",
                     "bland_altman", "calibration", "swimmer")) {
    cases <- c(cases, "research_paper")
  }
  # Lab reports
  if (grepl("検量線|standard curve|assay|アッセイ|実験|lab", txt)) {
    cases <- c(cases, "lab_report")
  }
  # Clinical reports
  if (grepl("patient|患者|clinical|臨床|chart|チャート|monitor|モニタ", txt) ||
      t$type %in% c("ecg", "growth", "nomogram", "bullseye")) {
    cases <- c(cases, "clinical_report")
  }
  # Lecture/teaching
  if (cat_field == "basic_science" || grepl("教育|teaching|lecture|概念", txt)) {
    cases <- c(cases, "lecture_handout")
  }
  # Case presentation
  if (t$type %in% c("ecg", "growth", "audiogram", "pedigree") ||
      grepl("case|症例|カンファ", txt)) {
    cases <- c(cases, "case_presentation")
  }
  if (length(cases) == 0) cases <- "lecture_handout"
  paste(unique(cases), collapse = ",")
}

# ── 6. Classify All Templates ────────────────────────────────
cat("Classifying templates on 7 axes...\n")

classified <- lapply(TEMPLATES, function(t) {
  list(
    id              = t$id,
    cat             = t$cat,
    en              = t$en,
    ja              = t$ja,
    type            = t$type,
    tags            = t$tags,
    figure_type     = classify_figure_type(t),
    data_type       = classify_data_type(t),
    purpose         = classify_purpose(t),
    comparison      = classify_comparison(t),
    medical_field   = t$cat,
    annotations     = classify_annotations(t),
    use_case        = classify_use_case(t)
  )
})

cat(sprintf("Classified %d templates\n", length(classified)))

# ── 7. Build Coverage Matrix ─────────────────────────────────
# Count templates per (figure_type × medical_field) combination

fig_types_used <- sort(unique(sapply(classified, `[[`, "figure_type")))
med_fields     <- sort(unique(sapply(classified, `[[`, "medical_field")))

coverage_matrix <- matrix(0L, nrow = length(fig_types_used), ncol = length(med_fields),
                          dimnames = list(fig_types_used, med_fields))

for (cl in classified) {
  coverage_matrix[cl$figure_type, cl$medical_field] <-
    coverage_matrix[cl$figure_type, cl$medical_field] + 1L
}

# ── 8. Define Theoretical Template Space ──────────────────────
# Not every (figure_type × medical_field) is medically meaningful.
# Define which combinations are valid.

# Medical relevance matrix: which figure types are relevant per field category?
FIELD_RELEVANT_FIGURES <- list(
  # Basic science fields: standard xy curves dominate
  basic_science = c("line_chart", "curve_fit", "log_plot", "scatter_plot",
                    "scatter_regression", "bar_chart", "boxplot", "heatmap",
                    "histogram", "radar_chart", "dot_plot"),
  # Clinical fields: clinical-specific + standard
  clinical      = c("line_chart", "curve_fit", "scatter_plot", "boxplot",
                    "violin", "bar_chart", "histogram", "kaplan_meier",
                    "roc_curve", "forest_plot", "heatmap", "radar_chart",
                    "nomogram", "dot_plot", "scatter_regression",
                    "calibration_plot", "tornado"),
  # Research methods: all statistical/research figures
  research_methods = c("boxplot", "violin", "histogram", "scatter_plot",
                       "scatter_regression", "bar_chart", "forest_plot",
                       "funnel_plot", "bland_altman", "kaplan_meier",
                       "roc_curve", "calibration_plot", "heatmap",
                       "radar_chart", "dot_plot", "tornado",
                       "swimmer_plot", "nomogram", "line_chart"),
  # Public health
  public_health = c("line_chart", "bar_chart", "histogram", "scatter_plot",
                    "epicurve", "pyramid", "heatmap", "kaplan_meier",
                    "forest_plot", "roc_curve", "dot_plot", "boxplot",
                    "tornado", "table_2x2"),
  # Diagnostics
  diagnostic    = c("line_chart", "scatter_plot", "bar_chart", "boxplot",
                    "histogram", "heatmap", "roc_curve", "bland_altman",
                    "calibration_plot", "dot_plot", "scatter_regression")
)

# Specialty-specific must-have figures
SPECIALTY_FIGURES <- list(
  cardio  = c("ecg_tracing", "pv_loop", "bullseye"),
  pulm    = c("flow_volume_loop"),
  eye     = c("visual_field"),
  ent     = c("audiogram"),
  peds    = c("growth_chart"),
  stat    = c("kaplan_meier", "roc_curve", "forest_plot", "funnel_plot",
              "bland_altman", "calibration_plot", "swimmer_plot"),
  epi     = c("epicurve", "pyramid", "table_2x2"),
  hemato  = c("kaplan_meier"),  # survival for leukemia etc.
  path    = c("manhattan_plot", "volcano_plot"),
  obgyn   = c("growth_chart"),  # fetal growth
  psych   = c("radar_chart"),   # symptom profiles
  neuro   = c("ecg_tracing")    # EEG-like tracings
)

# Generate valid_combinations
cat("Generating theoretical template space...\n")

valid_combinations <- list()
for (field in med_fields) {
  cat_info <- CAT_META[[field]]
  field_cat <- if (!is.null(cat_info)) cat_info$field else "clinical"

  # Get relevant figure types for this field's category
  relevant <- FIELD_RELEVANT_FIGURES[[field_cat]]
  if (is.null(relevant)) relevant <- FIELD_RELEVANT_FIGURES[["clinical"]]

  # Add specialty-specific figures
  special <- SPECIALTY_FIGURES[[field]]
  if (!is.null(special)) relevant <- unique(c(relevant, special))

  for (fig in relevant) {
    valid_combinations[[length(valid_combinations) + 1]] <- list(
      figure_type   = fig,
      medical_field = field
    )
  }
}

cat(sprintf("Theoretical space: %d valid (figure_type × medical_field) combinations\n",
            length(valid_combinations)))

# ── 9. Coverage Scoring ──────────────────────────────────────
# For each valid combination, compute coverage_score

coverage_results <- lapply(valid_combinations, function(vc) {
  fig <- vc$figure_type
  field <- vc$medical_field

  # Direct matches
  matches <- Filter(function(cl) {
    cl$figure_type == fig && cl$medical_field == field
  }, classified)
  direct_count <- length(matches)

  # Cross-field matches (same figure type, any field)
  cross_matches <- Filter(function(cl) cl$figure_type == fig, classified)
  cross_count <- length(cross_matches)

  # Score: 0-1 range
  # Direct match is worth more; cross-field match gives partial credit
  score <- min(1.0, direct_count * 0.5 + (cross_count > 0) * 0.15)
  if (direct_count >= 3) score <- 1.0
  if (direct_count == 2) score <- max(score, 0.8)
  if (direct_count == 1) score <- max(score, 0.5)

  # Classify coverage level
  level <- if (score >= 0.8) "covered"
           else if (score >= 0.5) "weakly_covered"
           else if (score >= 0.15) "poorly_covered"
           else "missing"

  list(
    figure_type   = fig,
    medical_field = field,
    direct_count  = direct_count,
    cross_count   = cross_count,
    score         = round(score, 3),
    level         = level,
    existing_ids  = sapply(matches, `[[`, "id")
  )
})

# Summary
levels <- sapply(coverage_results, `[[`, "level")
cat(sprintf("\nCoverage Summary:\n"))
cat(sprintf("  Covered:        %d (%d%%)\n", sum(levels == "covered"),
            round(100 * sum(levels == "covered") / length(levels))))
cat(sprintf("  Weakly covered: %d (%d%%)\n", sum(levels == "weakly_covered"),
            round(100 * sum(levels == "weakly_covered") / length(levels))))
cat(sprintf("  Poorly covered: %d (%d%%)\n", sum(levels == "poorly_covered"),
            round(100 * sum(levels == "poorly_covered") / length(levels))))
cat(sprintf("  Missing:        %d (%d%%)\n", sum(levels == "missing"),
            round(100 * sum(levels == "missing") / length(levels))))

# ── 10. Priority Scoring for Missing/Underserved ─────────────
# priority = demand + medical_importance + generality + gap - redundancy

# Demand weights: how commonly used is this figure type?
DEMAND <- list(
  line_chart       = 0.9, scatter_plot     = 0.8, curve_fit        = 0.7,
  log_plot         = 0.5, boxplot          = 0.9, violin           = 0.7,
  dot_plot         = 0.6, histogram        = 0.8, bar_chart        = 0.9,
  forest_plot      = 0.8, funnel_plot      = 0.6, bland_altman     = 0.7,
  kaplan_meier     = 0.9, roc_curve        = 0.9, calibration_plot = 0.6,
  ecg_tracing      = 0.5, audiogram        = 0.4, visual_field     = 0.4,
  growth_chart     = 0.7, flow_volume_loop = 0.5, pv_loop          = 0.4,
  bullseye         = 0.3, epicurve         = 0.6, pyramid          = 0.5,
  tornado          = 0.5, manhattan_plot   = 0.4, volcano_plot     = 0.5,
  radar_chart      = 0.6, heatmap          = 0.7, ternary          = 0.3,
  nomogram         = 0.6, swimmer_plot     = 0.5, table_2x2        = 0.7,
  pedigree         = 0.4, scatter_regression = 0.7
)

# Medical importance: clinical fields get higher weight
FIELD_IMPORTANCE <- list(
  cardio = 0.95, pulm = 0.85, nephro = 0.80, endo = 0.80, gi = 0.80,
  hemato = 0.85, neuro = 0.85, obgyn = 0.80, peds = 0.85, surg = 0.75,
  emer = 0.80, ortho = 0.70, eye = 0.65, psych = 0.75, derm = 0.60,
  ent = 0.65, uro = 0.65, radio = 0.65, path = 0.70,
  stat = 0.90, epi = 0.80, biochem = 0.75, physiol = 0.80,
  pharm = 0.80, micro = 0.75, immuno = 0.70, anat = 0.65, chem = 0.70
)

# Generality: how widely applicable across specialties?
compute_generality <- function(fig_type, classified_list) {
  fields_using <- unique(sapply(
    Filter(function(cl) cl$figure_type == fig_type, classified_list),
    `[[`, "medical_field"
  ))
  min(1.0, length(fields_using) / 5)  # normalize: 5+ fields = max generality
}

# Compute priority for missing/underserved combinations
cat("\nComputing priority scores...\n")

generation_queue <- list()
for (cr in coverage_results) {
  if (cr$level %in% c("missing", "poorly_covered", "weakly_covered")) {
    demand <- if (!is.null(DEMAND[[cr$figure_type]])) DEMAND[[cr$figure_type]] else 0.5
    med_imp <- if (!is.null(FIELD_IMPORTANCE[[cr$medical_field]])) FIELD_IMPORTANCE[[cr$medical_field]] else 0.5
    generality <- compute_generality(cr$figure_type, classified)
    gap <- 1.0 - cr$score  # bigger gap = higher priority

    # Redundancy: penalize if many similar templates already exist in the field
    field_templates <- Filter(function(cl) cl$medical_field == cr$medical_field, classified)
    redundancy <- min(0.3, length(field_templates) / 100)

    priority <- (demand * 0.25 + med_imp * 0.25 + generality * 0.15 +
                 gap * 0.25 - redundancy * 0.10)
    priority <- round(priority, 4)

    # Generate suggested template name
    fig_label <- gsub("_", " ", cr$figure_type)
    field_label <- if (!is.null(CAT_META[[cr$medical_field]])) {
      CAT_META[[cr$medical_field]]$ja
    } else cr$medical_field

    generation_queue[[length(generation_queue) + 1]] <- list(
      figure_type     = cr$figure_type,
      medical_field   = cr$medical_field,
      field_ja        = field_label,
      coverage_level  = cr$level,
      coverage_score  = cr$score,
      priority_score  = priority,
      demand          = demand,
      medical_importance = med_imp,
      generality      = generality,
      gap_size        = gap,
      suggested_name  = paste0(fig_label, " (", field_label, ")"),
      existing_similar = cr$existing_ids
    )
  }
}

# Sort by priority descending
generation_queue <- generation_queue[order(-sapply(generation_queue, `[[`, "priority_score"))]

cat(sprintf("Generation queue: %d candidates\n", length(generation_queue)))

# ── 11. Detect Near-Duplicates ────────────────────────────────
cat("Detecting near-duplicates...\n")

duplicate_candidates <- list()
for (i in 1:(N - 1)) {
  for (j in (i + 1):N) {
    a <- classified[[i]]
    b <- classified[[j]]
    # Same figure type + same field + similar names
    if (a$figure_type == b$figure_type && a$medical_field == b$medical_field) {
      # Compute name similarity (Jaccard on words)
      wa <- unique(strsplit(tolower(paste(a$en, a$ja)), "[\\s,]+")[[1]])
      wb <- unique(strsplit(tolower(paste(b$en, b$ja)), "[\\s,]+")[[1]])
      inter <- length(intersect(wa, wb))
      union <- length(union(wa, wb))
      sim <- if (union > 0) inter / union else 0

      if (sim > 0.4) {
        duplicate_candidates[[length(duplicate_candidates) + 1]] <- list(
          id_a       = a$id,
          id_b       = b$id,
          en_a       = a$en,
          en_b       = b$en,
          ja_a       = a$ja,
          ja_b       = b$ja,
          similarity = round(sim, 3),
          same_type  = TRUE,
          same_field = TRUE
        )
      }
    }
  }
}

cat(sprintf("Found %d near-duplicate candidate pairs\n", length(duplicate_candidates)))

# ── 12. Identify Weak Categories ──────────────────────────────
cat("Analyzing category coverage...\n")

weak_categories <- list()
for (field in med_fields) {
  field_templates <- Filter(function(cl) cl$medical_field == field, classified)
  n_templates <- length(field_templates)

  # Count unique figure types in this field
  field_fig_types <- unique(sapply(field_templates, `[[`, "figure_type"))
  n_fig_types <- length(field_fig_types)

  # Expected minimum based on field category
  cat_info <- CAT_META[[field]]
  field_cat <- if (!is.null(cat_info)) cat_info$field else "clinical"
  expected_figs <- length(FIELD_RELEVANT_FIGURES[[field_cat]])
  if (!is.null(SPECIALTY_FIGURES[[field]])) {
    expected_figs <- expected_figs + length(SPECIALTY_FIGURES[[field]])
  }

  # Missing figure types for this field
  relevant_figs <- FIELD_RELEVANT_FIGURES[[field_cat]]
  special_figs <- SPECIALTY_FIGURES[[field]]
  all_expected <- unique(c(relevant_figs, special_figs))
  missing_figs <- setdiff(all_expected, field_fig_types)

  coverage_pct <- round(100 * n_fig_types / max(1, length(all_expected)), 1)

  weak_categories[[length(weak_categories) + 1]] <- list(
    field          = field,
    field_ja       = if (!is.null(CAT_META[[field]])) CAT_META[[field]]$ja else field,
    n_templates    = n_templates,
    n_figure_types = n_fig_types,
    expected_types = length(all_expected),
    coverage_pct   = coverage_pct,
    missing_types  = missing_figs,
    status         = if (coverage_pct >= 80) "good"
                     else if (coverage_pct >= 50) "fair"
                     else "weak"
  )
}

# Sort by coverage ascending (weakest first)
weak_categories <- weak_categories[order(sapply(weak_categories, `[[`, "coverage_pct"))]

# ── 13. Items Needing Human Review ────────────────────────────
needs_review <- list()

# Build ID lookup for fast access
template_ids <- sapply(TEMPLATES, `[[`, "id")

# Templates with very short descriptions
for (cl in classified) {
  idx <- match(cl$id, template_ids)
  if (is.na(idx)) next
  t <- TEMPLATES[[idx]]
  if (nchar(t$dj) < 30) {
    needs_review[[length(needs_review) + 1]] <- list(
      id     = cl$id,
      en     = cl$en,
      reason = "very_short_description",
      detail = sprintf("Description only %d chars", nchar(t$dj))
    )
  }
  if (nchar(t$tags) < 5) {
    needs_review[[length(needs_review) + 1]] <- list(
      id     = cl$id,
      en     = cl$en,
      reason = "missing_tags",
      detail = sprintf("Tags: '%s'", t$tags)
    )
  }
}

# ── 14. Search Index Generation ───────────────────────────────
cat("Generating search index...\n")

search_index <- lapply(classified, function(cl) {
  idx <- match(cl$id, template_ids)
  if (is.na(idx)) return(NULL)
  t <- TEMPLATES[[idx]]

  # Normalize all text for search
  all_text <- tolower(paste(t$en, t$ja, t$tags, t$dj, t$xl, t$yl,
                            if (!is.null(t$sub)) t$sub else ""))
  # Extract keywords
  words <- unique(strsplit(gsub("[^a-zA-Z0-9\\u3040-\\u9FFF]", " ", all_text), "\\s+")[[1]])
  words <- words[nchar(words) > 1]

  # Add synonym expansions
  expanded <- words
  for (syn_name in names(SYNONYMS)) {
    syn_terms <- SYNONYMS[[syn_name]]
    if (any(sapply(syn_terms, function(s) grepl(s, all_text, fixed = TRUE)))) {
      expanded <- c(expanded, syn_name, syn_terms)
    }
  }
  expanded <- unique(expanded)

  list(
    id            = cl$id,
    cat           = cl$cat,
    en            = cl$en,
    ja            = cl$ja,
    figure_type   = cl$figure_type,
    data_type     = cl$data_type,
    purpose       = cl$purpose,
    medical_field = cl$medical_field,
    keywords      = expanded,
    url           = paste0(cl$cat, "/", cl$id, ".html")
  )
})

search_index <- Filter(Negate(is.null), search_index)

# ── 15. Search Facets ─────────────────────────────────────────
search_facets <- list(
  figure_type = sort(unique(sapply(classified, `[[`, "figure_type"))),
  data_type   = sort(unique(sapply(classified, `[[`, "data_type"))),
  purpose     = sort(unique(sapply(classified, `[[`, "purpose"))),
  comparison  = sort(unique(sapply(classified, `[[`, "comparison"))),
  medical_field = sort(unique(sapply(classified, `[[`, "medical_field"))),
  use_case    = sort(unique(unlist(strsplit(sapply(classified, `[[`, "use_case"), ","))))
)

# ── 16. Related Templates ────────────────────────────────────
cat("Computing related templates...\n")

related_templates <- list()
for (i in seq_along(classified)) {
  cl <- classified[[i]]
  scores <- numeric(length(classified))

  for (j in seq_along(classified)) {
    if (i == j) next
    cl2 <- classified[[j]]
    s <- 0
    if (cl$figure_type == cl2$figure_type) s <- s + 3
    if (cl$data_type == cl2$data_type)     s <- s + 2
    if (cl$purpose == cl2$purpose)         s <- s + 2
    if (cl$medical_field == cl2$medical_field) s <- s + 2
    if (cl$comparison == cl2$comparison)   s <- s + 1
    scores[j] <- s
  }

  top_idx <- head(order(-scores), 6)
  top_idx <- top_idx[scores[top_idx] >= 3]  # minimum similarity threshold

  related_templates[[cl$id]] <- lapply(top_idx, function(idx) {
    list(
      id    = classified[[idx]]$id,
      en    = classified[[idx]]$en,
      ja    = classified[[idx]]$ja,
      cat   = classified[[idx]]$cat,
      score = scores[idx]
    )
  })
}

# ── 17. Output Files ─────────────────────────────────────────
cat("\nWriting output files...\n")

# 17a. coverage_matrix.json
cm_list <- list()
for (i in seq_len(nrow(coverage_matrix))) {
  for (j in seq_len(ncol(coverage_matrix))) {
    if (coverage_matrix[i, j] > 0 || paste0(rownames(coverage_matrix)[i]) %in%
        sapply(valid_combinations, `[[`, "figure_type")) {
      cm_list[[length(cm_list) + 1]] <- list(
        figure_type   = rownames(coverage_matrix)[i],
        medical_field = colnames(coverage_matrix)[j],
        count         = coverage_matrix[i, j]
      )
    }
  }
}
write_json(cm_list, file.path(OUT_DIR, "coverage_matrix.json"),
           pretty = TRUE, auto_unbox = TRUE)

# 17b. missing_templates.json
missing_only <- Filter(function(x) x$coverage_level == "missing", generation_queue)
write_json(missing_only, file.path(OUT_DIR, "missing_templates.json"),
           pretty = TRUE, auto_unbox = TRUE)

# 17c. generation_queue.json (all candidates, priority-sorted)
write_json(generation_queue, file.path(OUT_DIR, "generation_queue.json"),
           pretty = TRUE, auto_unbox = TRUE)

# 17d. weak_categories.json
write_json(weak_categories, file.path(OUT_DIR, "weak_categories.json"),
           pretty = TRUE, auto_unbox = TRUE)

# 17e. duplicate_or_near_duplicate_candidates.json
write_json(duplicate_candidates, file.path(OUT_DIR, "duplicate_or_near_duplicate_candidates.json"),
           pretty = TRUE, auto_unbox = TRUE)

# 17f. needs_human_review.json
write_json(needs_review, file.path(OUT_DIR, "needs_human_review.json"),
           pretty = TRUE, auto_unbox = TRUE)

# 17g. search_index.json
write_json(search_index, file.path(OUT_DIR, "search_index.json"),
           pretty = TRUE, auto_unbox = TRUE)

# 17h. search_synonyms.json
write_json(SYNONYMS, file.path(OUT_DIR, "search_synonyms.json"),
           pretty = TRUE, auto_unbox = TRUE)

# 17i. search_facets.json
write_json(search_facets, file.path(OUT_DIR, "search_facets.json"),
           pretty = TRUE, auto_unbox = TRUE)

# 17j. related_templates.json
write_json(related_templates, file.path(OUT_DIR, "related_templates.json"),
           pretty = TRUE, auto_unbox = TRUE)

# 17k. classified_templates.json (full classification of existing templates)
write_json(classified, file.path(OUT_DIR, "classified_templates.json"),
           pretty = TRUE, auto_unbox = TRUE)

# ── 18. Coverage Report (Markdown) ────────────────────────────
cat("Generating coverage report...\n")

report <- character()
r <- function(...) report <<- c(report, paste0(...))

r("# MedGraph Coverage Report")
r("")
r(sprintf("Generated: %s", Sys.time()))
r(sprintf("Total existing templates: **%d**", N))
r(sprintf("Categories: **%d**", length(med_fields)))
r(sprintf("Theoretical space: **%d** valid combinations", length(valid_combinations)))
r("")

r("## Coverage Summary")
r("")
r(sprintf("| Level | Count | %% |"))
r("|-------|------:|---:|")
r(sprintf("| Covered | %d | %d%% |", sum(levels == "covered"),
          round(100 * sum(levels == "covered") / length(levels))))
r(sprintf("| Weakly covered | %d | %d%% |", sum(levels == "weakly_covered"),
          round(100 * sum(levels == "weakly_covered") / length(levels))))
r(sprintf("| Poorly covered | %d | %d%% |", sum(levels == "poorly_covered"),
          round(100 * sum(levels == "poorly_covered") / length(levels))))
r(sprintf("| Missing | %d | %d%% |", sum(levels == "missing"),
          round(100 * sum(levels == "missing") / length(levels))))
r("")

r("## Category Status")
r("")
r("| Category | JA | Templates | Figure Types | Expected | Coverage | Status |")
r("|----------|-----|----------:|------------:|---------:|---------:|--------|")
for (wc in weak_categories) {
  r(sprintf("| %s | %s | %d | %d | %d | %.0f%% | %s |",
            wc$field, wc$field_ja, wc$n_templates, wc$n_figure_types,
            wc$expected_types, wc$coverage_pct, wc$status))
}
r("")

r("## Top 30 Priority Candidates")
r("")
r("| # | Figure Type | Field | Priority | Gap | Level |")
r("|---|-------------|-------|----------|-----|-------|")
for (k in seq_len(min(30, length(generation_queue)))) {
  gq <- generation_queue[[k]]
  r(sprintf("| %d | %s | %s (%s) | %.3f | %.2f | %s |",
            k, gq$figure_type, gq$medical_field, gq$field_ja,
            gq$priority_score, gq$gap_size, gq$coverage_level))
}
r("")

r("## Figure Type Distribution")
r("")
fig_counts <- table(sapply(classified, `[[`, "figure_type"))
fig_counts <- sort(fig_counts, decreasing = TRUE)
r("| Figure Type | Count |")
r("|-------------|------:|")
for (ft in names(fig_counts)) {
  r(sprintf("| %s | %d |", ft, fig_counts[ft]))
}
r("")

r("## Data Type Distribution")
r("")
dt_counts <- table(sapply(classified, `[[`, "data_type"))
dt_counts <- sort(dt_counts, decreasing = TRUE)
r("| Data Type | Count |")
r("|-----------|------:|")
for (dt in names(dt_counts)) {
  r(sprintf("| %s | %d |", dt, dt_counts[dt]))
}
r("")

r("## Purpose Distribution")
r("")
pu_counts <- table(sapply(classified, `[[`, "purpose"))
pu_counts <- sort(pu_counts, decreasing = TRUE)
r("| Purpose | Count |")
r("|---------|------:|")
for (pu in names(pu_counts)) {
  r(sprintf("| %s | %d |", pu, pu_counts[pu]))
}
r("")

r("## Near-Duplicate Candidates")
r("")
if (length(duplicate_candidates) > 0) {
  r("| Template A | Template B | Similarity |")
  r("|------------|------------|----------:|")
  for (dc in duplicate_candidates) {
    r(sprintf("| %s | %s | %.1f%% |", dc$en_a, dc$en_b, dc$similarity * 100))
  }
} else {
  r("No near-duplicate candidates found.")
}
r("")

r("## Missing Figure Types per Category")
r("")
for (wc in weak_categories) {
  if (length(wc$missing_types) > 0) {
    r(sprintf("### %s (%s) — %d missing types", wc$field, wc$field_ja, length(wc$missing_types)))
    r("")
    for (mt in wc$missing_types) {
      r(sprintf("- %s", mt))
    }
    r("")
  }
}

r("## Items Needing Human Review")
r("")
if (length(needs_review) > 0) {
  r("| ID | Name | Reason | Detail |")
  r("|-----|------|--------|--------|")
  for (nr in needs_review) {
    r(sprintf("| %s | %s | %s | %s |", nr$id, nr$en, nr$reason, nr$detail))
  }
} else {
  r("All templates pass quality checks.")
}
r("")

r("---")
r("*End of report*")

writeLines(report, file.path(OUT_DIR, "coverage_report.md"))

# ── 19. Final Summary ────────────────────────────────────────
cat("\n=== OUTPUT FILES ===\n")
output_files <- list.files(OUT_DIR, full.names = TRUE)
for (f in output_files) {
  cat(sprintf("  %s (%s)\n", f, format(file.size(f), big.mark = ",")))
}

cat(sprintf("\n=== GAP ANALYSIS COMPLETE ===\n"))
cat(sprintf("Time: %s\n", Sys.time()))
cat(sprintf("Templates analyzed: %d\n", N))
cat(sprintf("Valid combinations: %d\n", length(valid_combinations)))
cat(sprintf("Generation queue: %d candidates\n", length(generation_queue)))
cat(sprintf("Missing: %d / Poorly covered: %d / Weakly covered: %d\n",
            sum(levels == "missing"),
            sum(levels == "poorly_covered"),
            sum(levels == "weakly_covered")))
