#!/usr/bin/env Rscript
# Generate D() calls for ALL missing figure_type × medical_field combinations
# to achieve 100% coverage

# The gap analysis identifies these missing combinations per category.
# We generate medically appropriate template definitions for each.

# ── Missing types per category (from coverage_report.md) ──
# anat (basic_science, 10 missing): line_chart, curve_fit, log_plot, scatter_plot, scatter_regression, bar_chart, boxplot, histogram, radar_chart, dot_plot
# emer (clinical, 12 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# radio (clinical, 12 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, kaplan_meier, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# surg (clinical, 12 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, radar_chart, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# neuro (clinical, 12 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado, ecg_tracing
# endo (clinical, 12 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# ortho (clinical, 11 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# psych (clinical, 11 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# uro (clinical, 11 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# pharm (basic_science, 7 missing): curve_fit, scatter_plot, scatter_regression, bar_chart, histogram, radar_chart, dot_plot
# ent (clinical, 11 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# eye (clinical, 12 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# obgyn (clinical, 12 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado, growth_chart
# derm (clinical, 10 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# gi (clinical, 10 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# hemato (clinical, 11 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# nephro (clinical, 10 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, dot_plot, scatter_regression, calibration_plot, tornado
# peds (clinical, 11 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# pulm (clinical, 11 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado
# path (basic_science, 8 missing): curve_fit, log_plot, scatter_plot, scatter_regression, bar_chart, histogram, dot_plot, manhattan_plot
# biochem (basic_science, 6 missing): scatter_plot, scatter_regression, bar_chart, histogram, radar_chart, dot_plot
# chem (diagnostic, 6 missing): scatter_plot, bar_chart, histogram, calibration_plot, dot_plot, scatter_regression
# micro (basic_science, 6 missing): curve_fit, scatter_plot, scatter_regression, bar_chart, histogram, dot_plot
# physiol (basic_science, 6 missing): curve_fit, scatter_plot, scatter_regression, histogram, radar_chart, dot_plot
# cardio (clinical, 9 missing): curve_fit, scatter_plot, violin, bar_chart, histogram, dot_plot, scatter_regression, calibration_plot, tornado
# epi (public_health, 6 missing): bar_chart, histogram, scatter_plot, kaplan_meier, roc_curve, dot_plot
# immuno (basic_science, 4 missing): curve_fit, scatter_regression, bar_chart, dot_plot
# stat (research_methods, 1 missing): scatter_regression

cat("Generating gap-fill template definitions...\n")

# All new templates will be appended to template_defs.R
lines <- character()
lines <- c(lines, "", "# ================================================================")
lines <- c(lines, "# GAP-FILL PHASE 2: Complete 100% figure-type coverage")
lines <- c(lines, "# ================================================================")

# Helper function to format a D() call
D_line <- function(id, cat, en, ja, xl, yl, xr, yr, dj, tags, ...) {
  extra <- list(...)
  xr_str <- sprintf("c(%s)", paste(xr, collapse=","))
  yr_str <- sprintf("c(%s)", paste(yr, collapse=","))
  base <- sprintf('D("%s","%s","%s","%s",\n  "%s","%s",%s,%s,\n  "%s","%s"',
                  id, cat, en, ja, xl, yl, xr_str, yr_str, dj, tags)

  extras <- character()
  for (nm in names(extra)) {
    v <- extra[[nm]]
    if (is.character(v) && length(v) == 1) {
      extras <- c(extras, sprintf('%s="%s"', nm, v))
    } else if (is.character(v) && length(v) > 1) {
      extras <- c(extras, sprintf('%s=c(%s)', nm, paste(sprintf('"%s"', v), collapse=",")))
    } else {
      extras <- c(extras, sprintf('%s=%s', nm, deparse(v)))
    }
  }

  if (length(extras) > 0) {
    paste0(base, ",\n  ", paste(extras, collapse=","), ")")
  } else {
    paste0(base, ")")
  }
}

# ================================================================
# STAT (1 missing: scatter_regression)
# ================================================================
lines <- c(lines, "", "# ── stat: scatter_regression ──")
lines <- c(lines, D_line("scatter-reg-generic","stat","Scatter with Regression","回帰直線付き散布図",
  "Predictor (X)","Outcome (Y)",c(0,100),c(0,100),
  "回帰直線付き散布図テンプレート。二変量の相関分析に使用。","回帰,散布図,相関,regression,scatter",
  type="scatter_reg"))

# ================================================================
# IMMUNO (4 missing: curve_fit, scatter_regression, bar_chart, dot_plot)
# ================================================================
lines <- c(lines, "", "# ── immuno: curve_fit, scatter_regression, bar_chart, dot_plot ──")
lines <- c(lines, D_line("immuno-binding-curve","immuno","Antibody Binding Curve","抗体結合曲線",
  "Antibody Concentration","Binding (%)",c(0,100),c(0,100),
  "抗体結合曲線テンプレート。抗体濃度と結合率の関係を表示。","抗体,結合,curve,曲線,免疫",
  sub="Binding curve"))
lines <- c(lines, D_line("immuno-scatter-reg","immuno","Immune Cell Scatter Regression","免疫細胞散布回帰",
  "CD4 Count","CD8 Count",c(0,2000),c(0,1500),
  "免疫細胞散布回帰テンプレート。リンパ球サブセット間の相関分析。","scatter,regression,CD4,CD8,免疫",
  type="scatter_reg"))
lines <- c(lines, D_line("immuno-cytokine-bar","immuno","Cytokine Level Bar Chart","サイトカインレベル棒グラフ",
  "Cytokine","Concentration (pg/mL)",c(0,100),c(0,500),
  "サイトカインレベル棒グラフテンプレート。各種サイトカインの発現量比較。","サイトカイン,棒グラフ,bar,IL,TNF"))
lines <- c(lines, D_line("immuno-subset-dot","immuno","Lymphocyte Subset Dot Plot","リンパ球サブセットドットプロット",
  "","Count (%)",c(0,4),c(0,80),
  "リンパ球サブセットドットプロットテンプレート。各サブセットの割合比較。","ドット,dot plot,リンパ球,サブセット",
  type="dot", xlb=c("CD4+","CD8+","NK","B cell")))

# ================================================================
# EPI (6 missing: bar_chart, histogram, scatter_plot, kaplan_meier, roc_curve, dot_plot)
# ================================================================
lines <- c(lines, "", "# ── epi: bar_chart, histogram, scatter_plot, kaplan_meier, roc_curve, dot_plot ──")
lines <- c(lines, D_line("epi-incidence-bar","epi","Incidence Rate Bar Chart","罹患率棒グラフ",
  "Disease","Incidence (/100,000)",c(0,10),c(0,500),
  "罹患率棒グラフテンプレート。疾患別の発生率を比較表示。","罹患率,bar,棒グラフ,発生率,疫学"))
lines <- c(lines, D_line("epi-age-histogram","epi","Age Distribution Histogram","年齢分布ヒストグラム",
  "Age (years)","Frequency (度数)",c(0,100),c(0,50),
  "年齢分布ヒストグラムテンプレート。患者集団の年齢構成を表示。","ヒストグラム,histogram,年齢,度数,分布"))
lines <- c(lines, D_line("epi-exposure-scatter","epi","Exposure-Outcome Scatter","曝露・アウトカム散布図",
  "Exposure Level","Disease Rate",c(0,100),c(0,50),
  "曝露・アウトカム散布図テンプレート。曝露量と疾患発生の関係。","scatter,散布図,曝露,アウトカム,疫学"))
lines <- c(lines, D_line("epi-cohort-km","epi","Cohort Survival Curve","コホート生存曲線",
  "Time (years)","Survival Probability",c(0,10),c(0,1),
  "コホート生存曲線テンプレート。前向きコホート研究の生存分析。","kaplan,生存,コホート,追跡,疫学",
  type="kaplan"))
lines <- c(lines, D_line("epi-screening-roc","epi","Screening Test ROC","スクリーニング検査ROC",
  "1 - Specificity","Sensitivity",c(0,1),c(0,1),
  "スクリーニング検査ROCテンプレート。集団検診の検査精度評価。","ROC,スクリーニング,感度,特異度,疫学",
  type="roc"))
lines <- c(lines, D_line("epi-prevalence-dot","epi","Disease Prevalence Dot Plot","疾患有病率ドットプロット",
  "","Prevalence (%)",c(0,5),c(0,30),
  "疾患有病率ドットプロットテンプレート。地域別の有病率を比較。","dot plot,ドット,有病率,prevalence",
  type="dot", xlb=c("Region A","Region B","Region C","Region D","Region E")))

# ================================================================
# BIOCHEM (6 missing: scatter_plot, scatter_regression, bar_chart, histogram, radar_chart, dot_plot)
# ================================================================
lines <- c(lines, "", "# ── biochem: scatter_plot, scatter_regression, bar_chart, histogram, radar_chart, dot_plot ──")
lines <- c(lines, D_line("biochem-conc-scatter","biochem","Concentration Scatter Plot","濃度散布図",
  "Standard Concentration","Measured Absorbance",c(0,100),c(0,2),
  "濃度散布図テンプレート。標準品と測定値の相関分析。","scatter,散布図,濃度,吸光度,生化学"))
lines <- c(lines, D_line("biochem-activity-scatter-reg","biochem","Enzyme Activity Scatter Regression","酵素活性散布回帰",
  "Substrate (mM)","Reaction Rate (U/mg)",c(0,50),c(0,200),
  "酵素活性散布回帰テンプレート。基質濃度と反応速度の回帰分析。","scatter,regression,酵素,反応速度,生化学",
  type="scatter_reg"))
lines <- c(lines, D_line("biochem-protein-bar","biochem","Protein Expression Bar Chart","タンパク質発現棒グラフ",
  "Protein","Relative Expression",c(0,6),c(0,5),
  "タンパク質発現棒グラフテンプレート。各タンパク質の発現量比較。","bar,棒グラフ,タンパク質,発現,ウェスタン"))
lines <- c(lines, D_line("biochem-mol-weight-histogram","biochem","Molecular Weight Histogram","分子量ヒストグラム",
  "Molecular Weight (kDa)","Frequency (度数)",c(0,200),c(0,30),
  "分子量ヒストグラムテンプレート。タンパク質の分子量分布を表示。","histogram,ヒストグラム,分子量,度数,生化学"))
lines <- c(lines, D_line("biochem-nutrient-radar","biochem","Nutrient Profile Radar","栄養素プロファイルレーダー",
  "","",c(0,1),c(0,1),
  "栄養素プロファイルレーダーテンプレート。栄養素バランスの評価。","radar,レーダー,栄養,プロファイル,生化学",
  type="radar", xlb=c("Protein","Carbohydrate","Fat","Vitamin","Mineral","Fiber")))
lines <- c(lines, D_line("biochem-enzyme-dot","biochem","Enzyme Activity Dot Plot","酵素活性ドットプロット",
  "","Activity (U/L)",c(0,5),c(0,500),
  "酵素活性ドットプロットテンプレート。各酵素の活性値を比較。","dot plot,ドット,酵素,活性,生化学",
  type="dot", xlb=c("AST","ALT","ALP","GGT","LDH")))

# ================================================================
# PHYSIOL (6 missing: curve_fit, scatter_plot, scatter_regression, histogram, radar_chart, dot_plot)
# ================================================================
lines <- c(lines, "", "# ── physiol: curve_fit, scatter_plot, scatter_regression, histogram, radar_chart, dot_plot ──")
lines <- c(lines, D_line("physiol-length-tension-curve","physiol","Length-Tension Curve","長さ-張力曲線",
  "Sarcomere Length (μm)","Tension (%)",c(1,4),c(0,100),
  "長さ-張力曲線テンプレート。筋の長さと張力の関係。","curve,曲線,張力,筋,生理学",
  sub="Length-tension curve"))
lines <- c(lines, D_line("physiol-vo2-scatter","physiol","VO2-Workload Scatter","VO2-運動負荷散布図",
  "Workload (W)","VO2 (mL/min)",c(0,300),c(0,3000),
  "VO2-運動負荷散布図テンプレート。運動強度と酸素消費量の関係。","scatter,散布図,VO2,運動,生理学"))
lines <- c(lines, D_line("physiol-co-hr-scatter-reg","physiol","CO vs HR Scatter Regression","心拍出量-心拍数散布回帰",
  "Heart Rate (bpm)","Cardiac Output (L/min)",c(40,200),c(2,25),
  "心拍出量-心拍数散布回帰テンプレート。心拍数と心拍出量の回帰分析。","scatter,regression,心拍出量,心拍数",
  type="scatter_reg"))
lines <- c(lines, D_line("physiol-bp-histogram","physiol","Blood Pressure Histogram","血圧分布ヒストグラム",
  "Systolic BP (mmHg)","Frequency (度数)",c(80,200),c(0,40),
  "血圧分布ヒストグラムテンプレート。集団の血圧値分布を表示。","histogram,ヒストグラム,血圧,度数,分布"))
lines <- c(lines, D_line("physiol-organ-function-radar","physiol","Organ Function Radar","臓器機能レーダー",
  "","",c(0,1),c(0,1),
  "臓器機能レーダーテンプレート。各臓器の機能評価を放射状に表示。","radar,レーダー,臓器,機能,生理学",
  type="radar", xlb=c("Heart","Lung","Kidney","Liver","Brain","GI")))
lines <- c(lines, D_line("physiol-electrolyte-dot","physiol","Electrolyte Dot Plot","電解質ドットプロット",
  "","Concentration (mEq/L)",c(0,5),c(0,150),
  "電解質ドットプロットテンプレート。各電解質の血清値を比較。","dot plot,ドット,電解質,Na,K,Ca",
  type="dot", xlb=c("Na+","K+","Ca2+","Mg2+","Cl-")))

# ================================================================
# MICRO (6 missing: curve_fit, scatter_plot, scatter_regression, bar_chart, histogram, dot_plot)
# ================================================================
lines <- c(lines, "", "# ── micro: curve_fit, scatter_plot, scatter_regression, bar_chart, histogram, dot_plot ──")
lines <- c(lines, D_line("micro-growth-fit-curve","micro","Bacterial Growth Curve Fit","細菌増殖曲線フィッティング",
  "Time (hours)","OD600",c(0,24),c(0,2),
  "細菌増殖曲線フィッティングテンプレート。増殖パラメータの推定。","curve,曲線,増殖,フィッティング,微生物",
  sub="Growth curve fit"))
lines <- c(lines, D_line("micro-resistance-scatter","micro","Resistance vs MIC Scatter","耐性-MIC散布図",
  "MIC (μg/mL)","Resistance Rate (%)",c(0,128),c(0,100),
  "耐性-MIC散布図テンプレート。抗菌薬感受性と耐性率の関係。","scatter,散布図,MIC,耐性,微生物"))
lines <- c(lines, D_line("micro-biofilm-scatter-reg","micro","Biofilm-OD Scatter Regression","バイオフィルム散布回帰",
  "OD570 (Biofilm)","CFU/mL",c(0,3),c(0,1e8),
  "バイオフィルム散布回帰テンプレート。バイオフィルム量と菌数の回帰。","scatter,regression,バイオフィルム,散布",
  type="scatter_reg"))
lines <- c(lines, D_line("micro-susceptibility-bar","micro","Antibiotic Susceptibility Bar","抗菌薬感受性棒グラフ",
  "Antibiotic","Susceptibility (%)",c(0,8),c(0,100),
  "抗菌薬感受性棒グラフテンプレート。各抗菌薬の感受性率を比較。","bar,棒グラフ,感受性,抗菌薬,微生物"))
lines <- c(lines, D_line("micro-zone-histogram","micro","Inhibition Zone Histogram","阻止円径ヒストグラム",
  "Zone Diameter (mm)","Frequency (度数)",c(0,40),c(0,20),
  "阻止円径ヒストグラムテンプレート。阻止円径の分布を表示。","histogram,ヒストグラム,阻止円,度数,微生物"))
lines <- c(lines, D_line("micro-organism-dot","micro","Organism Isolation Dot Plot","分離菌ドットプロット",
  "","Isolation Rate (%)",c(0,5),c(0,50),
  "分離菌ドットプロットテンプレート。各菌種の分離頻度を比較。","dot plot,ドット,分離菌,頻度,微生物",
  type="dot", xlb=c("E.coli","S.aureus","K.pneum","P.aerug","Enterococcus")))

# ================================================================
# CHEM (6 missing: scatter_plot, bar_chart, histogram, calibration_plot, dot_plot, scatter_regression)
# ================================================================
lines <- c(lines, "", "# ── chem: scatter_plot, bar_chart, histogram, calibration_plot, dot_plot, scatter_regression ──")
lines <- c(lines, D_line("chem-method-scatter","chem","Method Comparison Scatter","測定法比較散布図",
  "Method A","Method B",c(0,200),c(0,200),
  "測定法比較散布図テンプレート。二つの測定法の一致度を評価。","scatter,散布図,method comparison,測定法"))
lines <- c(lines, D_line("chem-analyte-bar","chem","Analyte Concentration Bar Chart","分析物濃度棒グラフ",
  "Analyte","Concentration",c(0,8),c(0,200),
  "分析物濃度棒グラフテンプレート。各分析物の測定値を比較。","bar,棒グラフ,分析物,濃度,臨床検査"))
lines <- c(lines, D_line("chem-result-histogram","chem","Lab Result Histogram","検査値ヒストグラム",
  "Value","Frequency (度数)",c(0,200),c(0,50),
  "検査値ヒストグラムテンプレート。検査値の度数分布を表示。","histogram,ヒストグラム,検査値,度数,分布"))
lines <- c(lines, D_line("chem-assay-calibration","chem","Assay Calibration Plot","アッセイ較正プロット",
  "Predicted","Observed",c(0,1),c(0,1),
  "アッセイ較正プロットテンプレート。測定法の較正評価。","calibration,較正,アッセイ,臨床検査",
  type="calibration"))
lines <- c(lines, D_line("chem-panel-dot","chem","Lab Panel Dot Plot","検査パネルドットプロット",
  "","Value",c(0,6),c(0,300),
  "検査パネルドットプロットテンプレート。複数検査項目の結果表示。","dot plot,ドット,パネル,検査,臨床検査",
  type="dot", xlb=c("BUN","Cr","Na","K","Cl","CO2")))
lines <- c(lines, D_line("chem-reagent-scatter-reg","chem","Reagent Lot Scatter Regression","試薬ロット散布回帰",
  "Old Lot Value","New Lot Value",c(0,200),c(0,200),
  "試薬ロット散布回帰テンプレート。ロット変更時の互換性評価。","scatter,regression,試薬,ロット,臨床検査",
  type="scatter_reg"))

# ================================================================
# PATH (8 missing: curve_fit, log_plot, scatter_plot, scatter_regression, bar_chart, histogram, dot_plot, manhattan_plot)
# ================================================================
lines <- c(lines, "", "# ── path: curve_fit, log_plot, scatter_plot, scatter_regression, bar_chart, histogram, dot_plot, manhattan_plot ──")
lines <- c(lines, D_line("path-staining-curve","path","Staining Intensity Curve","染色強度曲線",
  "Antibody Concentration","Staining Score",c(0,100),c(0,4),
  "染色強度曲線テンプレート。免疫染色の濃度依存性を表示。","curve,曲線,染色,免疫組織化学,病理",
  sub="Staining curve"))
lines <- c(lines, D_line("path-tumor-growth-log","path","Tumor Growth Semi-log","腫瘍増殖セミログ",
  "Time (days)","Tumor Volume (mm³)",c(0,60),c(1,10000),
  "腫瘍増殖セミログテンプレート。腫瘍体積の経時変化を対数軸で表示。","semi-log,セミログ,腫瘍,増殖,病理",
  ylog=TRUE))
lines <- c(lines, D_line("path-marker-scatter","path","Biomarker Scatter Plot","バイオマーカー散布図",
  "Marker A Expression","Marker B Expression",c(0,100),c(0,100),
  "バイオマーカー散布図テンプレート。二つの腫瘍マーカーの相関分析。","scatter,散布図,バイオマーカー,発現,病理"))
lines <- c(lines, D_line("path-ki67-scatter-reg","path","Ki-67 vs Survival Scatter Regression","Ki-67生存散布回帰",
  "Ki-67 Index (%)",  "Overall Survival (months)",c(0,100),c(0,60),
  "Ki-67増殖指数と予後の散布回帰テンプレート。増殖活性と予後の関連。","scatter,regression,Ki-67,生存,病理",
  type="scatter_reg"))
lines <- c(lines, D_line("path-tumor-type-bar","path","Tumor Type Bar Chart","腫瘍型棒グラフ",
  "Histological Type","Count",c(0,6),c(0,100),
  "腫瘍型棒グラフテンプレート。組織型別の症例数を比較。","bar,棒グラフ,組織型,腫瘍,病理"))
lines <- c(lines, D_line("path-grade-histogram","path","Pathological Grade Histogram","病理グレードヒストグラム",
  "Grade Score","Frequency (度数)",c(0,10),c(0,30),
  "病理グレードヒストグラムテンプレート。腫瘍グレードの分布。","histogram,ヒストグラム,グレード,度数,病理"))
lines <- c(lines, D_line("path-ihc-score-dot","path","IHC Score Dot Plot","免疫染色スコアドットプロット",
  "","H-Score",c(0,4),c(0,300),
  "免疫染色スコアドットプロットテンプレート。各抗体のH-Score比較。","dot plot,ドット,免疫染色,H-Score,病理",
  type="dot", xlb=c("ER","PR","HER2","Ki-67")))
lines <- c(lines, D_line("path-gwas-manhattan","path","Tumor GWAS Manhattan","腫瘍GWASマンハッタン",
  "Chromosome","",c(0,1),c(0,15),
  "腫瘍GWASマンハッタンプロットテンプレート。腫瘍関連遺伝子座を表示。","manhattan,マンハッタン,GWAS,腫瘍,病理",
  type="manhattan"))

# ================================================================
# PHARM (7 missing: curve_fit, scatter_plot, scatter_regression, bar_chart, histogram, radar_chart, dot_plot)
# ================================================================
lines <- c(lines, "", "# ── pharm: curve_fit, scatter_plot, scatter_regression, bar_chart, histogram, radar_chart, dot_plot ──")
lines <- c(lines, D_line("pharm-binding-curve","pharm","Receptor Binding Curve","受容体結合曲線",
  "Ligand Concentration (nM)","Bound (%)",c(0,100),c(0,100),
  "受容体結合曲線テンプレート。リガンド-受容体結合の飽和曲線。","curve,曲線,受容体,結合,薬理",
  sub="Binding curve"))
lines <- c(lines, D_line("pharm-pk-scatter","pharm","PK Parameter Scatter","薬物動態パラメータ散布図",
  "AUC (ng·h/mL)","Cmax (ng/mL)",c(0,1000),c(0,200),
  "薬物動態パラメータ散布図テンプレート。AUCとCmaxの関係。","scatter,散布図,PK,AUC,Cmax"))
lines <- c(lines, D_line("pharm-dose-efficacy-scatter-reg","pharm","Dose-Efficacy Scatter Regression","用量-効果散布回帰",
  "Dose (mg)","Efficacy (%)",c(0,100),c(0,100),
  "用量-効果散布回帰テンプレート。用量と有効率の回帰分析。","scatter,regression,用量,効果,薬理",
  type="scatter_reg"))
lines <- c(lines, D_line("pharm-drug-class-bar","pharm","Drug Class Comparison Bar","薬効群別比較棒グラフ",
  "Drug Class","Efficacy (%)",c(0,6),c(0,100),
  "薬効群別比較棒グラフテンプレート。薬効群ごとの有効率を比較。","bar,棒グラフ,薬効群,比較,薬理"))
lines <- c(lines, D_line("pharm-tmax-histogram","pharm","Tmax Distribution Histogram","Tmax分布ヒストグラム",
  "Tmax (hours)","Frequency (度数)",c(0,12),c(0,20),
  "Tmax分布ヒストグラムテンプレート。最高血中濃度到達時間の分布。","histogram,ヒストグラム,Tmax,度数,薬理"))
lines <- c(lines, D_line("pharm-safety-radar","pharm","Drug Safety Radar","薬剤安全性レーダー",
  "","",c(0,1),c(0,1),
  "薬剤安全性レーダーテンプレート。有害事象プロファイルを放射状に表示。","radar,レーダー,安全性,有害事象,薬理",
  type="radar", xlb=c("Hepatotoxicity","Nephrotoxicity","Cardiotoxicity","Neurotoxicity","GI","Hematologic")))
lines <- c(lines, D_line("pharm-bioavail-dot","pharm","Bioavailability Dot Plot","バイオアベイラビリティドットプロット",
  "","Bioavailability (%)",c(0,5),c(0,100),
  "バイオアベイラビリティドットプロットテンプレート。製剤間のBA比較。","dot plot,ドット,バイオアベイラビリティ,BA,薬理",
  type="dot", xlb=c("Tablet","Capsule","Liquid","IV","Patch")))

# ================================================================
# CARDIO (9 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── cardio: curve_fit, scatter_plot, violin, bar_chart, histogram, dot_plot, scatter_regression, calibration_plot, tornado ──")
lines <- c(lines, D_line("cardio-lvef-curve","cardio","LVEF Recovery Curve","LVEF回復曲線",
  "Time (months)","LVEF (%)",c(0,12),c(20,70),
  "LVEF回復曲線テンプレート。心不全治療後のEF回復を追跡。","curve,曲線,LVEF,心不全,回復",
  sub="LVEF recovery curve"))
lines <- c(lines, D_line("cardio-ef-bnp-scatter","cardio","EF vs BNP Scatter","EF-BNP散布図",
  "LVEF (%)","BNP (pg/mL)",c(10,70),c(0,2000),
  "EF-BNP散布図テンプレート。左室駆出率とBNP値の関係。","scatter,散布図,EF,BNP,心不全"))
lines <- c(lines, D_line("cardio-bp-drug-violin","cardio","BP by Drug Class Violin","薬剤別血圧バイオリン",
  "Drug Class","SBP (mmHg)",c(0,4),c(80,200),
  "薬剤別血圧バイオリンプロットテンプレート。降圧薬の効果比較。","violin,バイオリン,血圧,降圧薬,循環器",
  type="violin", xlb=c("ACEi","ARB","CCB","Diuretic")))
lines <- c(lines, D_line("cardio-valve-bar","cardio","Valve Disease Bar Chart","弁膜症棒グラフ",
  "Valve","Prevalence (%)",c(0,5),c(0,15),
  "弁膜症棒グラフテンプレート。弁膜症種別の有病率を比較。","bar,棒グラフ,弁膜症,有病率,循環器"))
lines <- c(lines, D_line("cardio-hr-histogram","cardio","Heart Rate Histogram","心拍数ヒストグラム",
  "Heart Rate (bpm)","Frequency (度数)",c(40,160),c(0,30),
  "心拍数ヒストグラムテンプレート。心拍数の分布を表示。","histogram,ヒストグラム,心拍数,度数,循環器"))
lines <- c(lines, D_line("cardio-biomarker-dot","cardio","Cardiac Biomarker Dot Plot","心筋バイオマーカードットプロット",
  "","Level",c(0,4),c(0,500),
  "心筋バイオマーカードットプロットテンプレート。各マーカー値を比較。","dot plot,ドット,バイオマーカー,心筋,循環器",
  type="dot", xlb=c("Troponin","CK-MB","BNP","D-dimer")))
lines <- c(lines, D_line("cardio-risk-scatter-reg","cardio","CV Risk Factor Scatter Regression","心血管リスク散布回帰",
  "LDL-C (mg/dL)","CIMT (mm)",c(50,200),c(0.3,1.5),
  "心血管リスク散布回帰テンプレート。LDLとIMTの回帰分析。","scatter,regression,LDL,IMT,循環器",
  type="scatter_reg"))
lines <- c(lines, D_line("cardio-risk-calibration","cardio","CV Risk Score Calibration","心血管リスクスコア較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "心血管リスクスコア較正テンプレート。リスクスコアの較正評価。","calibration,較正,リスクスコア,循環器",
  type="calibration"))
lines <- c(lines, D_line("cardio-sensitivity-tornado","cardio","Cardiac CEA Tornado","循環器費用効果トルネード",
  "ICER (USD/QALY)","",c(-50000,50000),c(0,6),
  "循環器費用効果トルネード図テンプレート。心血管治療のCEA感度分析。","tornado,トルネード,費用効果,CEA,循環器",
  type="tornado", ylb=c("Drug cost","Efficacy","Readmission","Mortality","QoL","Discount rate")))

# ================================================================
# NEURO (12 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado, ecg_tracing)
# ================================================================
lines <- c(lines, "", "# ── neuro: 12 missing types ──")
lines <- c(lines, D_line("neuro-recovery-curve","neuro","Neurological Recovery Curve","神経機能回復曲線",
  "Time (weeks)","Function Score (%)",c(0,52),c(0,100),
  "神経機能回復曲線テンプレート。脳卒中後の機能回復経過。","curve,曲線,回復,脳卒中,神経",
  sub="Recovery curve"))
lines <- c(lines, D_line("neuro-volume-scatter","neuro","Brain Volume Scatter","脳容積散布図",
  "Age (years)","Brain Volume (mL)",c(20,90),c(900,1500),
  "脳容積散布図テンプレート。加齢に伴う脳容積変化を表示。","scatter,散布図,脳容積,加齢,MRI"))
lines <- c(lines, D_line("neuro-pain-violin","neuro","Pain Score Violin","疼痛スコアバイオリン",
  "Treatment","VAS Score",c(0,3),c(0,10),
  "疼痛スコアバイオリンプロットテンプレート。疼痛治療の効果比較。","violin,バイオリン,疼痛,VAS,神経",
  type="violin", xlb=c("Placebo","Drug A","Drug B")))
lines <- c(lines, D_line("neuro-stroke-type-bar","neuro","Stroke Type Bar Chart","脳卒中型別棒グラフ",
  "Stroke Type","Incidence (/100K)",c(0,5),c(0,200),
  "脳卒中型別棒グラフテンプレート。脳卒中の種類別発生率を比較。","bar,棒グラフ,脳卒中,発生率,分類"))
lines <- c(lines, D_line("neuro-mmse-histogram","neuro","MMSE Score Histogram","MMSEスコアヒストグラム",
  "MMSE Score","Frequency (度数)",c(0,30),c(0,40),
  "MMSEスコアヒストグラムテンプレート。認知機能検査の得点分布。","histogram,ヒストグラム,MMSE,認知,度数"))
lines <- c(lines, D_line("neuro-stroke-forest","neuro","Stroke Prevention Forest","脳卒中予防フォレスト",
  "Odds Ratio (95% CI)","",c(0.1,10),c(0,6),
  "脳卒中予防フォレストプロットテンプレート。予防介入のメタ分析。","forest,フォレスト,脳卒中,予防,メタ分析",
  type="forest", ylb=c("Anticoagulation","Antiplatelet","Statin","BP Control","Carotid","Lifestyle")))
lines <- c(lines, D_line("neuro-stroke-nomogram","neuro","Stroke Risk Nomogram","脳卒中リスクノモグラム",
  "","",c(0,1),c(0,7),
  "脳卒中リスクノモグラムテンプレート。脳卒中発症リスクの予測。","nomogram,ノモグラム,脳卒中,リスク,予測",
  type="nomogram", ylb=c("Age","AF","HTN","DM","Prior Stroke","Points","10-yr Risk")))
lines <- c(lines, D_line("neuro-csf-dot","neuro","CSF Findings Dot Plot","髄液所見ドットプロット",
  "","Value",c(0,5),c(0,500),
  "髄液所見ドットプロットテンプレート。髄液検査の各項目を比較。","dot plot,ドット,髄液,CSF,神経",
  type="dot", xlb=c("Cell Count","Protein","Glucose","IgG Index","OCB")))
lines <- c(lines, D_line("neuro-nihss-scatter-reg","neuro","NIHSS vs Outcome Scatter Regression","NIHSS-予後散布回帰",
  "NIHSS Score","mRS at 90d",c(0,40),c(0,6),
  "NIHSSスコアと予後の散布回帰テンプレート。脳卒中重症度と予後の関連。","scatter,regression,NIHSS,mRS,予後",
  type="scatter_reg"))
lines <- c(lines, D_line("neuro-stroke-calibration","neuro","Stroke Risk Calibration","脳卒中リスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "脳卒中リスクスコア較正テンプレート。リスクモデルの妥当性検証。","calibration,較正,脳卒中,リスク",
  type="calibration"))
lines <- c(lines, D_line("neuro-headache-tornado","neuro","Headache Treatment Tornado","頭痛治療トルネード",
  "Effect Size","",c(-2,2),c(0,6),
  "頭痛治療トルネード図テンプレート。片頭痛治療のCEA感度分析。","tornado,トルネード,頭痛,費用効果",
  type="tornado", ylb=c("Drug cost","Efficacy","Recurrence","QoL","Work loss","Duration")))
lines <- c(lines, D_line("neuro-eeg-tracing","neuro","EEG Tracing Template","脳波トレーシング",
  "Time (seconds)","Voltage (μV)",c(0,2.5),c(-100,100),
  "脳波トレーシングテンプレート。脳波記録用紙の標準フォーマット。","ECG,EEG,脳波,トレーシング,神経",
  type="ecg"))

# ================================================================
# ENDO (12 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── endo: 12 missing types ──")
lines <- c(lines, D_line("endo-insulin-curve","endo","Insulin Secretion Curve","インスリン分泌曲線",
  "Glucose (mg/dL)","Insulin (μU/mL)",c(0,400),c(0,200),
  "インスリン分泌曲線テンプレート。グルコース刺激に対するインスリン応答。","curve,曲線,インスリン,分泌,内分泌",
  sub="Insulin secretion curve"))
lines <- c(lines, D_line("endo-hba1c-bmi-scatter","endo","HbA1c vs BMI Scatter","HbA1c-BMI散布図",
  "BMI (kg/m²)","HbA1c (%)",c(15,45),c(4,14),
  "HbA1c-BMI散布図テンプレート。肥満度と血糖コントロールの関係。","scatter,散布図,HbA1c,BMI,糖尿病"))
lines <- c(lines, D_line("endo-thyroid-violin","endo","Thyroid Function Violin","甲状腺機能バイオリン",
  "Condition","TSH (mIU/L)",c(0,3),c(0,20),
  "甲状腺機能バイオリンプロットテンプレート。甲状腺疾患のTSH分布比較。","violin,バイオリン,甲状腺,TSH,内分泌",
  type="violin", xlb=c("Normal","Hypo","Hyper")))
lines <- c(lines, D_line("endo-dm-complication-bar","endo","DM Complication Bar Chart","糖尿病合併症棒グラフ",
  "Complication","Prevalence (%)",c(0,6),c(0,50),
  "糖尿病合併症棒グラフテンプレート。各合併症の有病率を比較。","bar,棒グラフ,糖尿病,合併症,内分泌"))
lines <- c(lines, D_line("endo-glucose-histogram","endo","Fasting Glucose Histogram","空腹時血糖ヒストグラム",
  "Glucose (mg/dL)","Frequency (度数)",c(60,300),c(0,40),
  "空腹時血糖ヒストグラムテンプレート。血糖値の度数分布を表示。","histogram,ヒストグラム,血糖,度数,糖尿病"))
lines <- c(lines, D_line("endo-dm-screening-roc","endo","DM Screening ROC","糖尿病スクリーニングROC",
  "1 - Specificity","Sensitivity",c(0,1),c(0,1),
  "糖尿病スクリーニングROC曲線テンプレート。検査の診断精度評価。","ROC,糖尿病,スクリーニング,感度,特異度",
  type="roc"))
lines <- c(lines, D_line("endo-insulin-forest","endo","Insulin Therapy Forest","インスリン療法フォレスト",
  "HbA1c Reduction (%)","",c(-2,1),c(0,6),
  "インスリン療法フォレストプロットテンプレート。各インスリン製剤の効果比較。","forest,フォレスト,インスリン,HbA1c",
  type="forest", ylb=c("Basal","Bolus","Premix","GLP-1 RA","SGLT2i","Combination")))
lines <- c(lines, D_line("endo-dm-nomogram","endo","DM Complication Nomogram","糖尿病合併症ノモグラム",
  "","",c(0,1),c(0,7),
  "糖尿病合併症ノモグラムテンプレート。合併症リスクの予測。","nomogram,ノモグラム,糖尿病,合併症,リスク",
  type="nomogram", ylb=c("Duration","HbA1c","SBP","LDL-C","Smoking","Points","10-yr Risk")))
lines <- c(lines, D_line("endo-hormone-dot","endo","Hormone Level Dot Plot","ホルモン値ドットプロット",
  "","Level",c(0,5),c(0,100),
  "ホルモン値ドットプロットテンプレート。各ホルモンの測定値を比較。","dot plot,ドット,ホルモン,内分泌",
  type="dot", xlb=c("TSH","FT4","Cortisol","ACTH","GH")))
lines <- c(lines, D_line("endo-hba1c-scatter-reg","endo","HbA1c vs FPG Scatter Regression","HbA1c-FPG散布回帰",
  "FPG (mg/dL)","HbA1c (%)",c(60,300),c(4,14),
  "HbA1c-FPG散布回帰テンプレート。空腹時血糖とHbA1cの回帰分析。","scatter,regression,HbA1c,FPG,糖尿病",
  type="scatter_reg"))
lines <- c(lines, D_line("endo-dm-calibration","endo","DM Risk Calibration","糖尿病リスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "糖尿病リスクスコア較正テンプレート。リスクモデルの妥当性検証。","calibration,較正,糖尿病,リスク",
  type="calibration"))
lines <- c(lines, D_line("endo-dm-tornado","endo","DM Treatment Tornado","糖尿病治療トルネード",
  "ICER (USD/QALY)","",c(-30000,30000),c(0,6),
  "糖尿病治療トルネード図テンプレート。治療の費用効果感度分析。","tornado,トルネード,糖尿病,費用効果",
  type="tornado", ylb=c("Drug cost","HbA1c effect","Hypoglycemia","Weight","CV benefit","Discount")))

# ================================================================
# GI (10 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── gi: 10 missing types ──")
lines <- c(lines, D_line("gi-motility-curve","gi","GI Motility Curve","消化管運動曲線",
  "Time (min)","Pressure (mmHg)",c(0,120),c(0,100),
  "消化管運動曲線テンプレート。食道内圧検査の圧曲線を表示。","curve,曲線,消化管運動,内圧,消化器",
  sub="Motility curve"))
lines <- c(lines, D_line("gi-fibrosis-scatter","gi","Fibrosis Score Scatter","肝線維化スコア散布図",
  "FIB-4 Index","APRI",c(0,10),c(0,5),
  "肝線維化スコア散布図テンプレート。各スコアの相関分析。","scatter,散布図,線維化,FIB-4,APRI"))
lines <- c(lines, D_line("gi-ibd-violin","gi","IBD Activity Violin","IBD活動性バイオリン",
  "Disease","CRP (mg/dL)",c(0,3),c(0,10),
  "IBD活動性バイオリンプロットテンプレート。炎症性腸疾患のCRP分布比較。","violin,バイオリン,IBD,CRP,消化器",
  type="violin", xlb=c("Remission","Mild","Severe")))
lines <- c(lines, D_line("gi-etiology-bar","gi","Liver Disease Etiology Bar","肝疾患原因棒グラフ",
  "Etiology","Cases (%)",c(0,6),c(0,40),
  "肝疾患原因棒グラフテンプレート。肝疾患の原因別割合を比較。","bar,棒グラフ,肝疾患,原因,消化器"))
lines <- c(lines, D_line("gi-alt-histogram","gi","ALT Distribution Histogram","ALT分布ヒストグラム",
  "ALT (U/L)","Frequency (度数)",c(0,200),c(0,40),
  "ALT分布ヒストグラムテンプレート。肝酵素値の度数分布。","histogram,ヒストグラム,ALT,度数,肝臓"))
lines <- c(lines, D_line("gi-cirrhosis-nomogram","gi","Cirrhosis Nomogram","肝硬変ノモグラム",
  "","",c(0,1),c(0,7),
  "肝硬変ノモグラムテンプレート。肝硬変の予後予測。","nomogram,ノモグラム,肝硬変,予後,消化器",
  type="nomogram", ylb=c("MELD","Albumin","Bilirubin","Ascites","Encephalopathy","Points","1-yr Survival")))
lines <- c(lines, D_line("gi-liver-enzyme-dot","gi","Liver Enzyme Dot Plot","肝酵素ドットプロット",
  "","Value (U/L)",c(0,5),c(0,500),
  "肝酵素ドットプロットテンプレート。各肝酵素の測定値を比較。","dot plot,ドット,肝酵素,AST,ALT",
  type="dot", xlb=c("AST","ALT","ALP","GGT","T-Bil")))
lines <- c(lines, D_line("gi-fib4-scatter-reg","gi","FIB-4 vs Biopsy Scatter Regression","FIB-4生検散布回帰",
  "FIB-4 Index","Fibrosis Stage (Metavir)",c(0,10),c(0,4),
  "FIB-4と肝生検の散布回帰テンプレート。非侵襲的マーカーの精度評価。","scatter,regression,FIB-4,生検,線維化",
  type="scatter_reg"))
lines <- c(lines, D_line("gi-hcc-calibration","gi","HCC Risk Calibration","肝癌リスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "肝癌リスクスコア較正テンプレート。肝癌発症リスクモデルの検証。","calibration,較正,肝癌,リスク",
  type="calibration"))
lines <- c(lines, D_line("gi-ibd-tornado","gi","IBD Treatment Tornado","IBD治療トルネード",
  "ICER (USD/QALY)","",c(-50000,50000),c(0,6),
  "IBD治療トルネード図テンプレート。炎症性腸疾患治療のCEA感度分析。","tornado,トルネード,IBD,費用効果",
  type="tornado", ylb=c("Drug cost","Remission rate","Surgery","Hospitalization","QoL","Duration")))

# ================================================================
# HEMATO (11 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── hemato: 11 missing types ──")
lines <- c(lines, D_line("hemato-growth-factor-curve","hemato","Growth Factor Response Curve","造血因子応答曲線",
  "EPO Dose (U/kg)","Hb Change (g/dL)",c(0,300),c(0,5),
  "造血因子応答曲線テンプレート。EPO投与量とヘモグロビン上昇の関係。","curve,曲線,EPO,造血因子,血液",
  sub="Response curve"))
lines <- c(lines, D_line("hemato-reti-scatter","hemato","Reticulocyte Scatter","網赤血球散布図",
  "Reticulocyte (%)",  "Hb (g/dL)",c(0,20),c(4,18),
  "網赤血球散布図テンプレート。網赤血球数とヘモグロビンの関係。","scatter,散布図,網赤血球,Hb,血液"))
lines <- c(lines, D_line("hemato-wbc-violin","hemato","WBC Distribution Violin","白血球分布バイオリン",
  "Cell Type","Count (×10³/μL)",c(0,4),c(0,15),
  "白血球分布バイオリンプロットテンプレート。白血球分画の分布比較。","violin,バイオリン,白血球,分画,血液",
  type="violin", xlb=c("Neutrophil","Lymphocyte","Monocyte","Eosinophil")))
lines <- c(lines, D_line("hemato-blood-type-bar","hemato","Blood Type Distribution Bar","血液型分布棒グラフ",
  "Blood Type","Frequency (%)",c(0,5),c(0,50),
  "血液型分布棒グラフテンプレート。血液型の頻度分布を表示。","bar,棒グラフ,血液型,頻度,血液"))
lines <- c(lines, D_line("hemato-hb-histogram","hemato","Hemoglobin Histogram","ヘモグロビンヒストグラム",
  "Hb (g/dL)","Frequency (度数)",c(4,18),c(0,40),
  "ヘモグロビンヒストグラムテンプレート。Hb値の度数分布。","histogram,ヒストグラム,Hb,ヘモグロビン,度数"))
lines <- c(lines, D_line("hemato-lymphoma-roc","hemato","Lymphoma Marker ROC","リンパ腫マーカーROC",
  "1 - Specificity","Sensitivity",c(0,1),c(0,1),
  "リンパ腫マーカーROC曲線テンプレート。腫瘍マーカーの診断精度。","ROC,リンパ腫,マーカー,診断,血液",
  type="roc"))
lines <- c(lines, D_line("hemato-risk-nomogram","hemato","Hematologic Risk Nomogram","血液腫瘍リスクノモグラム",
  "","",c(0,1),c(0,7),
  "血液腫瘍リスクノモグラムテンプレート。予後リスクスコアの予測。","nomogram,ノモグラム,血液腫瘍,リスク",
  type="nomogram", ylb=c("Age","LDH","Stage","ECOG","Extranodal","Points","5-yr Survival")))
lines <- c(lines, D_line("hemato-cbc-dot","hemato","CBC Dot Plot","CBC ドットプロット",
  "","Value",c(0,5),c(0,500),
  "CBCドットプロットテンプレート。全血算の各項目を比較。","dot plot,ドット,CBC,全血算,血液",
  type="dot", xlb=c("WBC","RBC","Hb","Plt","MCV")))
lines <- c(lines, D_line("hemato-ferritin-scatter-reg","hemato","Ferritin vs Iron Scatter Regression","フェリチン-鉄散布回帰",
  "Serum Ferritin (ng/mL)","Serum Iron (μg/dL)",c(0,500),c(0,200),
  "フェリチン-鉄散布回帰テンプレート。鉄代謝マーカーの回帰分析。","scatter,regression,フェリチン,鉄,貧血",
  type="scatter_reg"))
lines <- c(lines, D_line("hemato-ipi-calibration","hemato","IPI Score Calibration","IPIスコア較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "IPIスコア較正テンプレート。リンパ腫予後指標の妥当性検証。","calibration,較正,IPI,リンパ腫,予後",
  type="calibration"))
lines <- c(lines, D_line("hemato-treatment-tornado","hemato","Hematologic Treatment Tornado","血液疾患治療トルネード",
  "ICER (USD/QALY)","",c(-80000,80000),c(0,6),
  "血液疾患治療トルネード図テンプレート。治療費用効果の感度分析。","tornado,トルネード,血液,費用効果",
  type="tornado", ylb=c("Drug cost","CR rate","Relapse","Transplant","OS","Discount")))

# ================================================================
# NEPHRO (10 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── nephro: 10 missing types ──")
lines <- c(lines, D_line("nephro-gfr-decline-curve","nephro","GFR Decline Curve","GFR低下曲線",
  "Time (years)","eGFR (mL/min/1.73m²)",c(0,10),c(0,120),
  "GFR低下曲線テンプレート。CKD進行のGFR経時変化。","curve,曲線,GFR,CKD,進行",
  sub="GFR decline curve"))
lines <- c(lines, D_line("nephro-cr-bun-scatter","nephro","Cr vs BUN Scatter","Cr-BUN散布図",
  "Serum Cr (mg/dL)","BUN (mg/dL)",c(0,10),c(0,100),
  "Cr-BUN散布図テンプレート。腎機能マーカーの相関分析。","scatter,散布図,Cr,BUN,腎臓"))
lines <- c(lines, D_line("nephro-electrolyte-violin","nephro","Electrolyte Violin","電解質バイオリン",
  "Electrolyte","Value (mEq/L)",c(0,4),c(0,150),
  "電解質バイオリンプロットテンプレート。電解質値の分布比較。","violin,バイオリン,電解質,Na,K",
  type="violin", xlb=c("Na+","K+","Cl-","HCO3-")))
lines <- c(lines, D_line("nephro-ckd-stage-bar","nephro","CKD Stage Bar Chart","CKDステージ棒グラフ",
  "CKD Stage","Patients (%)",c(0,6),c(0,40),
  "CKDステージ棒グラフテンプレート。CKDステージ別の患者割合。","bar,棒グラフ,CKD,ステージ,腎臓"))
lines <- c(lines, D_line("nephro-cr-histogram","nephro","Creatinine Histogram","クレアチニンヒストグラム",
  "Cr (mg/dL)","Frequency (度数)",c(0,10),c(0,40),
  "クレアチニンヒストグラムテンプレート。Cr値の度数分布。","histogram,ヒストグラム,Cr,クレアチニン,度数"))
lines <- c(lines, D_line("nephro-ckd-forest","nephro","CKD Intervention Forest","CKD介入フォレスト",
  "Risk Ratio (95% CI)","",c(0.2,5),c(0,6),
  "CKD介入フォレストプロットテンプレート。腎保護療法のメタ分析。","forest,フォレスト,CKD,介入,メタ分析",
  type="forest", ylb=c("ACEi/ARB","SGLT2i","Finerenone","BP Control","Protein Restrict","Combined")))
lines <- c(lines, D_line("nephro-urine-dot","nephro","Urinalysis Dot Plot","尿検査ドットプロット",
  "","Value",c(0,5),c(0,300),
  "尿検査ドットプロットテンプレート。尿検査の各項目を比較。","dot plot,ドット,尿検査,UA,腎臓",
  type="dot", xlb=c("Protein","RBC","WBC","Cast","Crystal")))
lines <- c(lines, D_line("nephro-egfr-scatter-reg","nephro","eGFR vs Cystatin Scatter Regression","eGFR-シスタチン散布回帰",
  "Cr-based eGFR","Cystatin C-based eGFR",c(0,120),c(0,120),
  "eGFR散布回帰テンプレート。Cr式とCys-C式の一致度分析。","scatter,regression,eGFR,シスタチン,腎臓",
  type="scatter_reg"))
lines <- c(lines, D_line("nephro-risk-calibration","nephro","CKD Risk Calibration","CKDリスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "CKDリスクスコア較正テンプレート。腎障害進行リスクモデルの検証。","calibration,較正,CKD,リスク,腎臓",
  type="calibration"))
lines <- c(lines, D_line("nephro-dialysis-tornado","nephro","Dialysis CEA Tornado","透析費用効果トルネード",
  "ICER (USD/QALY)","",c(-100000,100000),c(0,6),
  "透析費用効果トルネード図テンプレート。透析療法のCEA感度分析。","tornado,トルネード,透析,費用効果",
  type="tornado", ylb=c("Dialysis cost","Transplant","Mortality","QoL","Complications","Discount")))

# ================================================================
# OBGYN (12 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado, growth_chart)
# ================================================================
lines <- c(lines, "", "# ── obgyn: 12 missing types ──")
lines <- c(lines, D_line("obgyn-hcg-curve","obgyn","hCG Rise Curve","hCG上昇曲線",
  "Gestational Week","hCG (mIU/mL)",c(3,14),c(0,200000),
  "hCG上昇曲線テンプレート。妊娠初期のhCG推移を表示。","curve,曲線,hCG,妊娠,産科",
  sub="hCG rise curve"))
lines <- c(lines, D_line("obgyn-bw-ga-scatter","obgyn","Birth Weight vs GA Scatter","出生体重-在胎週数散布図",
  "Gestational Age (weeks)","Birth Weight (g)",c(24,42),c(500,5000),
  "出生体重-在胎週数散布図テンプレート。在胎週数と出生体重の関係。","scatter,散布図,出生体重,在胎週数,産科"))
lines <- c(lines, D_line("obgyn-labor-violin","obgyn","Labor Duration Violin","分娩時間バイオリン",
  "Parity","Duration (hours)",c(0,3),c(0,24),
  "分娩時間バイオリンプロットテンプレート。経産回数別の分娩時間分布。","violin,バイオリン,分娩,時間,産科",
  type="violin", xlb=c("Nullipara","Multipara","Grand")))
lines <- c(lines, D_line("obgyn-delivery-bar","obgyn","Delivery Mode Bar Chart","分娩方法棒グラフ",
  "Mode","Rate (%)",c(0,4),c(0,70),
  "分娩方法棒グラフテンプレート。分娩方法別の割合を比較。","bar,棒グラフ,分娩,帝王切開,産科"))
lines <- c(lines, D_line("obgyn-bw-histogram","obgyn","Birth Weight Histogram","出生体重ヒストグラム",
  "Birth Weight (g)","Frequency (度数)",c(1500,5000),c(0,40),
  "出生体重ヒストグラムテンプレート。出生体重の度数分布。","histogram,ヒストグラム,出生体重,度数,産科"))
lines <- c(lines, D_line("obgyn-preterm-forest","obgyn","Preterm Prevention Forest","早産予防フォレスト",
  "Risk Ratio (95% CI)","",c(0.1,5),c(0,6),
  "早産予防フォレストプロットテンプレート。早産予防介入のメタ分析。","forest,フォレスト,早産,予防,メタ分析",
  type="forest", ylb=c("Progesterone","Cerclage","Pessary","Tocolysis","Antibiotics","Bed Rest")))
lines <- c(lines, D_line("obgyn-preeclampsia-nomogram","obgyn","Preeclampsia Nomogram","妊娠高血圧ノモグラム",
  "","",c(0,1),c(0,7),
  "妊娠高血圧ノモグラムテンプレート。妊娠高血圧症候群のリスク予測。","nomogram,ノモグラム,妊娠高血圧,リスク",
  type="nomogram", ylb=c("Age","BMI","Parity","MAP","UtA-PI","PAPP-A","Risk")))
lines <- c(lines, D_line("obgyn-prenatal-dot","obgyn","Prenatal Test Dot Plot","出生前検査ドットプロット",
  "","Value",c(0,4),c(0,5),
  "出生前検査ドットプロットテンプレート。各検査項目のMoM値を比較。","dot plot,ドット,出生前,MoM,産科",
  type="dot", xlb=c("AFP","hCG","uE3","Inhibin")))
lines <- c(lines, D_line("obgyn-bw-ga-scatter-reg","obgyn","BW vs GA Scatter Regression","出生体重-GA散布回帰",
  "Gestational Age (weeks)","Birth Weight (g)",c(24,42),c(500,5000),
  "出生体重と在胎週数の散布回帰テンプレート。成長パターンの回帰分析。","scatter,regression,出生体重,在胎週数",
  type="scatter_reg"))
lines <- c(lines, D_line("obgyn-risk-calibration","obgyn","Obstetric Risk Calibration","産科リスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "産科リスクスコア較正テンプレート。産科合併症リスクモデルの検証。","calibration,較正,産科,リスク",
  type="calibration"))
lines <- c(lines, D_line("obgyn-cs-tornado","obgyn","C-Section CEA Tornado","帝王切開費用効果トルネード",
  "ICER","",c(-20000,20000),c(0,6),
  "帝王切開費用効果トルネード図テンプレート。分娩方法の費用効果分析。","tornado,トルネード,帝王切開,費用効果",
  type="tornado", ylb=c("Surgery cost","Complication","Recovery","Fertility","NICU","Readmission")))
lines <- c(lines, D_line("obgyn-fetal-growth-chart","obgyn","Fetal Weight Growth Chart","胎児体重成長曲線",
  "Gestational Age (weeks)","Estimated Fetal Weight (g)",c(20,42),c(0,5000),
  "胎児体重成長曲線テンプレート。在胎週数別の推定胎児体重パーセンタイル。","growth,成長曲線,胎児体重,パーセンタイル",
  type="growth"))

# ================================================================
# PEDS (11 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── peds: 11 missing types ──")
lines <- c(lines, D_line("peds-milestone-curve","peds","Developmental Milestone Curve","発達マイルストーン曲線",
  "Age (months)","Skills Achieved (%)",c(0,60),c(0,100),
  "発達マイルストーン曲線テンプレート。運動・言語発達の経過を表示。","curve,曲線,発達,マイルストーン,小児",
  sub="Milestone curve"))
lines <- c(lines, D_line("peds-bmi-scatter","peds","BMI-Age Scatter","BMI-年齢散布図",
  "Age (years)","BMI (kg/m²)",c(2,18),c(10,35),
  "BMI-年齢散布図テンプレート。小児のBMIと年齢の関係。","scatter,散布図,BMI,年齢,小児"))
lines <- c(lines, D_line("peds-drug-dose-violin","peds","Drug Dose Violin","薬用量バイオリン",
  "Age Group","Dose (mg/kg)",c(0,4),c(0,50),
  "薬用量バイオリンプロットテンプレート。年齢群別の薬用量分布比較。","violin,バイオリン,薬用量,小児,年齢",
  type="violin", xlb=c("Neonate","Infant","Child","Adolescent")))
lines <- c(lines, D_line("peds-vaccine-bar","peds","Vaccine Coverage Bar Chart","ワクチン接種率棒グラフ",
  "Vaccine","Coverage (%)",c(0,8),c(0,100),
  "ワクチン接種率棒グラフテンプレート。各ワクチンの接種率を比較。","bar,棒グラフ,ワクチン,接種率,小児"))
lines <- c(lines, D_line("peds-weight-histogram","peds","Weight Histogram","体重ヒストグラム",
  "Weight (kg)","Frequency (度数)",c(0,80),c(0,30),
  "小児体重ヒストグラムテンプレート。体重の度数分布を表示。","histogram,ヒストグラム,体重,度数,小児"))
lines <- c(lines, D_line("peds-treatment-forest","peds","Pediatric Treatment Forest","小児治療フォレスト",
  "Risk Ratio (95% CI)","",c(0.1,5),c(0,6),
  "小児治療フォレストプロットテンプレート。小児治療のメタ分析。","forest,フォレスト,小児,治療,メタ分析",
  type="forest", ylb=c("Drug A","Drug B","Surgery","Watchful","Combined","Supportive")))
lines <- c(lines, D_line("peds-risk-nomogram","peds","Pediatric Risk Nomogram","小児リスクノモグラム",
  "","",c(0,1),c(0,6),
  "小児リスクノモグラムテンプレート。小児疾患の重症度予測。","nomogram,ノモグラム,小児,リスク,重症度",
  type="nomogram", ylb=c("Age","Weight","HR","SpO2","GCS","Risk Score")))
lines <- c(lines, D_line("peds-growth-marker-dot","peds","Growth Marker Dot Plot","成長マーカードットプロット",
  "","Value",c(0,5),c(0,100),
  "成長マーカードットプロットテンプレート。成長関連検査値を比較。","dot plot,ドット,成長,マーカー,小児",
  type="dot", xlb=c("IGF-1","GH","TSH","Bone Age","Z-score")))
lines <- c(lines, D_line("peds-bw-height-scatter-reg","peds","Weight vs Height Scatter Regression","体重-身長散布回帰",
  "Height (cm)","Weight (kg)",c(50,180),c(3,80),
  "体重-身長散布回帰テンプレート。小児の身長と体重の回帰分析。","scatter,regression,体重,身長,小児",
  type="scatter_reg"))
lines <- c(lines, D_line("peds-risk-calibration","peds","Pediatric Risk Calibration","小児リスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "小児リスクスコア較正テンプレート。小児重症度スコアの検証。","calibration,較正,小児,リスク",
  type="calibration"))
lines <- c(lines, D_line("peds-vaccine-tornado","peds","Vaccine CEA Tornado","ワクチン費用効果トルネード",
  "ICER (USD/QALY)","",c(-10000,10000),c(0,6),
  "ワクチン費用効果トルネード図テンプレート。予防接種の費用効果分析。","tornado,トルネード,ワクチン,費用効果",
  type="tornado", ylb=c("Vaccine cost","Efficacy","Herd immunity","AE rate","Coverage","Discount")))

# ================================================================
# ORTHO (11 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── ortho: 11 missing types ──")
lines <- c(lines, D_line("ortho-healing-curve","ortho","Fracture Healing Curve","骨折治癒曲線",
  "Time (weeks)","Healing Score (%)",c(0,24),c(0,100),
  "骨折治癒曲線テンプレート。骨折修復の経時的な進行を追跡。","curve,曲線,骨折,治癒,整形外科",
  sub="Healing curve"))
lines <- c(lines, D_line("ortho-bmd-age-scatter","ortho","BMD vs Age Scatter","骨密度-年齢散布図",
  "Age (years)","BMD (g/cm²)",c(20,90),c(0.4,1.4),
  "骨密度-年齢散布図テンプレート。加齢に伴う骨密度変化を表示。","scatter,散布図,骨密度,BMD,年齢"))
lines <- c(lines, D_line("ortho-pain-violin","ortho","Joint Pain Score Violin","関節痛スコアバイオリン",
  "Treatment","VAS Score",c(0,3),c(0,10),
  "関節痛スコアバイオリンプロットテンプレート。治療別の疼痛スコア分布。","violin,バイオリン,関節痛,VAS,整形外科",
  type="violin", xlb=c("NSAID","Steroid","Biologic")))
lines <- c(lines, D_line("ortho-fracture-bar","ortho","Fracture Site Bar Chart","骨折部位棒グラフ",
  "Fracture Site","Incidence (/100K)",c(0,6),c(0,300),
  "骨折部位棒グラフテンプレート。骨折発生部位別の頻度を比較。","bar,棒グラフ,骨折,部位,整形外科"))
lines <- c(lines, D_line("ortho-rom-histogram","ortho","Range of Motion Histogram","関節可動域ヒストグラム",
  "ROM (degrees)","Frequency (度数)",c(0,180),c(0,30),
  "関節可動域ヒストグラムテンプレート。ROMの度数分布を表示。","histogram,ヒストグラム,ROM,可動域,度数"))
lines <- c(lines, D_line("ortho-fracture-roc","ortho","Fracture Risk ROC","骨折リスクROC",
  "1 - Specificity","Sensitivity",c(0,1),c(0,1),
  "骨折リスクROC曲線テンプレート。骨折リスクスコアの診断精度。","ROC,骨折,リスク,診断,整形外科",
  type="roc"))
lines <- c(lines, D_line("ortho-frax-nomogram","ortho","FRAX Nomogram","FRAXノモグラム",
  "","",c(0,1),c(0,7),
  "FRAXノモグラムテンプレート。骨折リスク評価ツール。","nomogram,ノモグラム,FRAX,骨折,リスク",
  type="nomogram", ylb=c("Age","BMI","BMD","Prior Fx","Glucocort","Points","10-yr Risk")))
lines <- c(lines, D_line("ortho-joint-dot","ortho","Joint Score Dot Plot","関節スコアドットプロット",
  "","Score",c(0,5),c(0,100),
  "関節スコアドットプロットテンプレート。各関節の機能スコアを比較。","dot plot,ドット,関節,スコア,整形外科",
  type="dot", xlb=c("Shoulder","Elbow","Hip","Knee","Ankle")))
lines <- c(lines, D_line("ortho-bmd-scatter-reg","ortho","BMD vs T-score Scatter Regression","BMD-Tスコア散布回帰",
  "T-score","BMD (g/cm²)",c(-4,2),c(0.4,1.4),
  "BMD-Tスコア散布回帰テンプレート。骨密度測定値の回帰分析。","scatter,regression,BMD,Tスコア,骨粗鬆症",
  type="scatter_reg"))
lines <- c(lines, D_line("ortho-oa-calibration","ortho","OA Progression Calibration","変形性関節症較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "変形性関節症較正テンプレート。OA進行リスクモデルの検証。","calibration,較正,OA,変形性関節症",
  type="calibration"))
lines <- c(lines, D_line("ortho-tka-tornado","ortho","TKA CEA Tornado","TKA費用効果トルネード",
  "ICER (USD/QALY)","",c(-30000,30000),c(0,6),
  "人工膝関節置換術費用効果トルネード図テンプレート。TKAのCEA感度分析。","tornado,トルネード,TKA,費用効果",
  type="tornado", ylb=c("Implant cost","Efficacy","Revision","Recovery","Infection","Discount")))

# ================================================================
# PSYCH (11 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── psych: 11 missing types ──")
lines <- c(lines, D_line("psych-remission-curve","psych","Remission Rate Curve","寛解率曲線",
  "Treatment Week","Remission Rate (%)",c(0,52),c(0,100),
  "寛解率曲線テンプレート。精神疾患治療の寛解率推移。","curve,曲線,寛解,治療,精神科",
  sub="Remission curve"))
lines <- c(lines, D_line("psych-symptom-scatter","psych","Symptom Severity Scatter","症状重症度散布図",
  "Anxiety Score","Depression Score",c(0,40),c(0,40),
  "症状重症度散布図テンプレート。不安と抑うつの相関分析。","scatter,散布図,不安,抑うつ,精神科"))
lines <- c(lines, D_line("psych-medication-violin","psych","Medication Response Violin","薬物応答バイオリン",
  "Drug","HAM-D Change",c(0,3),c(-30,5),
  "薬物応答バイオリンプロットテンプレート。抗うつ薬のHAM-D変化量分布。","violin,バイオリン,HAM-D,抗うつ薬,精神科",
  type="violin", xlb=c("SSRI","SNRI","TCA")))
lines <- c(lines, D_line("psych-diagnosis-bar","psych","Diagnosis Frequency Bar","診断頻度棒グラフ",
  "Diagnosis","Prevalence (%)",c(0,6),c(0,20),
  "診断頻度棒グラフテンプレート。精神疾患の診断別有病率。","bar,棒グラフ,診断,有病率,精神科"))
lines <- c(lines, D_line("psych-phq9-histogram","psych","PHQ-9 Score Histogram","PHQ-9スコアヒストグラム",
  "PHQ-9 Score","Frequency (度数)",c(0,27),c(0,30),
  "PHQ-9スコアヒストグラムテンプレート。うつ病スクリーニングの得点分布。","histogram,ヒストグラム,PHQ-9,うつ病,度数"))
lines <- c(lines, D_line("psych-screening-roc","psych","Depression Screening ROC","うつ病スクリーニングROC",
  "1 - Specificity","Sensitivity",c(0,1),c(0,1),
  "うつ病スクリーニングROC曲線テンプレート。スクリーニングツールの精度。","ROC,うつ病,スクリーニング,精度,精神科",
  type="roc"))
lines <- c(lines, D_line("psych-suicide-nomogram","psych","Suicide Risk Nomogram","自殺リスクノモグラム",
  "","",c(0,1),c(0,7),
  "自殺リスクノモグラムテンプレート。自殺リスクの予測ツール。","nomogram,ノモグラム,自殺,リスク,精神科",
  type="nomogram", ylb=c("Age","Gender","Hx Attempt","Substance","Isolation","Points","Risk Level")))
lines <- c(lines, D_line("psych-cognitive-dot","psych","Cognitive Test Dot Plot","認知検査ドットプロット",
  "","Score",c(0,5),c(0,30),
  "認知検査ドットプロットテンプレート。各認知機能検査のスコア比較。","dot plot,ドット,認知,検査,精神科",
  type="dot", xlb=c("MMSE","MoCA","TMT-A","TMT-B","WCST")))
lines <- c(lines, D_line("psych-hamd-scatter-reg","psych","HAM-D vs BDI Scatter Regression","HAM-D-BDI散布回帰",
  "HAM-D Score","BDI Score",c(0,50),c(0,60),
  "HAM-D-BDI散布回帰テンプレート。うつ病評価尺度の相関分析。","scatter,regression,HAM-D,BDI,精神科",
  type="scatter_reg"))
lines <- c(lines, D_line("psych-risk-calibration","psych","Psychiatric Risk Calibration","精神科リスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "精神科リスクスコア較正テンプレート。リスクモデルの妥当性検証。","calibration,較正,精神科,リスク",
  type="calibration"))
lines <- c(lines, D_line("psych-antidep-tornado","psych","Antidepressant CEA Tornado","抗うつ薬費用効果トルネード",
  "ICER (USD/QALY)","",c(-20000,20000),c(0,6),
  "抗うつ薬費用効果トルネード図テンプレート。抗うつ薬治療のCEA感度分析。","tornado,トルネード,抗うつ薬,費用効果",
  type="tornado", ylb=c("Drug cost","Efficacy","Adherence","Side effects","QoL","Duration")))

# ================================================================
# EYE (12 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── eye: 12 missing types ──")
lines <- c(lines, D_line("eye-iop-curve","eye","IOP Diurnal Curve","眼圧日内変動曲線",
  "Time of Day","IOP (mmHg)",c(6,22),c(8,30),
  "眼圧日内変動曲線テンプレート。緑内障の眼圧日内変動を追跡。","curve,曲線,眼圧,IOP,日内変動",
  sub="IOP diurnal curve"))
lines <- c(lines, D_line("eye-acuity-scatter","eye","VA vs Age Scatter","視力-年齢散布図",
  "Age (years)","Visual Acuity (logMAR)",c(20,90),c(-0.3,1.5),
  "視力-年齢散布図テンプレート。加齢に伴う視力変化を表示。","scatter,散布図,視力,年齢,眼科"))
lines <- c(lines, D_line("eye-iop-drug-violin","eye","IOP by Drug Violin","薬剤別眼圧バイオリン",
  "Drug","IOP (mmHg)",c(0,4),c(8,30),
  "薬剤別眼圧バイオリンプロットテンプレート。緑内障点眼薬の効果比較。","violin,バイオリン,眼圧,点眼,眼科",
  type="violin", xlb=c("PG analog","Beta-blocker","CAI","Alpha-2")))
lines <- c(lines, D_line("eye-disease-bar","eye","Eye Disease Bar Chart","眼疾患棒グラフ",
  "Disease","Prevalence (%)",c(0,5),c(0,15),
  "眼疾患棒グラフテンプレート。眼疾患の有病率を比較。","bar,棒グラフ,眼疾患,有病率,眼科"))
lines <- c(lines, D_line("eye-iop-histogram","eye","IOP Distribution Histogram","眼圧分布ヒストグラム",
  "IOP (mmHg)","Frequency (度数)",c(5,40),c(0,30),
  "眼圧分布ヒストグラムテンプレート。眼圧値の度数分布。","histogram,ヒストグラム,IOP,眼圧,度数"))
lines <- c(lines, D_line("eye-glaucoma-roc","eye","Glaucoma Screening ROC","緑内障スクリーニングROC",
  "1 - Specificity","Sensitivity",c(0,1),c(0,1),
  "緑内障スクリーニングROC曲線テンプレート。RNFL厚の診断精度。","ROC,緑内障,RNFL,スクリーニング,眼科",
  type="roc"))
lines <- c(lines, D_line("eye-treatment-forest","eye","Eye Treatment Forest","眼科治療フォレスト",
  "Risk Ratio (95% CI)","",c(0.1,5),c(0,6),
  "眼科治療フォレストプロットテンプレート。眼科治療のメタ分析。","forest,フォレスト,眼科,治療,メタ分析",
  type="forest", ylb=c("Anti-VEGF","Laser","Surgery","Steroid","Combined","Control")))
lines <- c(lines, D_line("eye-risk-nomogram","eye","Glaucoma Risk Nomogram","緑内障リスクノモグラム",
  "","",c(0,1),c(0,7),
  "緑内障リスクノモグラムテンプレート。緑内障発症リスクの予測。","nomogram,ノモグラム,緑内障,リスク,眼科",
  type="nomogram", ylb=c("Age","IOP","CCT","C/D Ratio","Family Hx","Points","5-yr Risk")))
lines <- c(lines, D_line("eye-oct-dot","eye","OCT Parameter Dot Plot","OCTパラメータドットプロット",
  "","Thickness (μm)",c(0,4),c(0,400),
  "OCTパラメータドットプロットテンプレート。網膜層厚の各セクター比較。","dot plot,ドット,OCT,網膜,眼科",
  type="dot", xlb=c("RNFL","GCL","IPL","INL")))
lines <- c(lines, D_line("eye-va-scatter-reg","eye","VA vs RNFL Scatter Regression","視力-RNFL散布回帰",
  "RNFL Thickness (μm)","Visual Acuity (logMAR)",c(30,120),c(-0.3,1.5),
  "視力-RNFL散布回帰テンプレート。RNFL厚と視力の回帰分析。","scatter,regression,RNFL,視力,眼科",
  type="scatter_reg"))
lines <- c(lines, D_line("eye-risk-calibration","eye","Glaucoma Risk Calibration","緑内障リスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "緑内障リスクスコア較正テンプレート。リスクモデルの妥当性検証。","calibration,較正,緑内障,リスク",
  type="calibration"))
lines <- c(lines, D_line("eye-surgery-tornado","eye","Eye Surgery CEA Tornado","眼科手術費用効果トルネード",
  "ICER (USD/QALY)","",c(-20000,20000),c(0,6),
  "眼科手術費用効果トルネード図テンプレート。手術の費用効果分析。","tornado,トルネード,眼科,手術,費用効果",
  type="tornado", ylb=c("Surgery cost","VA gain","Complication","Reoperation","QoL","Discount")))

# ================================================================
# ENT (11 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── ent: 11 missing types ──")
lines <- c(lines, D_line("ent-hearing-loss-curve","ent","Hearing Loss Progression Curve","聴力低下進行曲線",
  "Age (years)","Hearing Level (dB HL)",c(20,90),c(0,80),
  "聴力低下進行曲線テンプレート。加齢性難聴の進行を追跡。","curve,曲線,聴力,難聴,進行",
  sub="Hearing loss curve"))
lines <- c(lines, D_line("ent-pta-scatter","ent","PTA vs SRT Scatter","純音-SRT散布図",
  "PTA (dB HL)","SRT (dB HL)",c(0,100),c(0,100),
  "純音聴力-SRT散布図テンプレート。純音聴力とSRTの相関。","scatter,散布図,PTA,SRT,耳鼻"))
lines <- c(lines, D_line("ent-vertigo-violin","ent","Vertigo Score Violin","めまいスコアバイオリン",
  "Treatment","DHI Score",c(0,3),c(0,100),
  "めまいスコアバイオリンプロットテンプレート。めまい治療の効果比較。","violin,バイオリン,めまい,DHI,耳鼻",
  type="violin", xlb=c("Epley","Medication","VRT")))
lines <- c(lines, D_line("ent-disease-bar","ent","ENT Disease Bar Chart","耳鼻疾患棒グラフ",
  "Disease","Incidence (/100K)",c(0,5),c(0,200),
  "耳鼻疾患棒グラフテンプレート。耳鼻咽喉科疾患の発生率を比較。","bar,棒グラフ,耳鼻,疾患,発生率"))
lines <- c(lines, D_line("ent-hearing-histogram","ent","Hearing Level Histogram","聴力レベルヒストグラム",
  "Hearing Level (dB HL)","Frequency (度数)",c(0,100),c(0,30),
  "聴力レベルヒストグラムテンプレート。聴力検査値の度数分布。","histogram,ヒストグラム,聴力,度数,耳鼻"))
lines <- c(lines, D_line("ent-treatment-forest","ent","ENT Treatment Forest","耳鼻治療フォレスト",
  "Risk Ratio (95% CI)","",c(0.1,5),c(0,6),
  "耳鼻治療フォレストプロットテンプレート。耳鼻治療のメタ分析。","forest,フォレスト,耳鼻,治療,メタ分析",
  type="forest", ylb=c("Surgery","Hearing Aid","Implant","Medication","Rehab","Combined")))
lines <- c(lines, D_line("ent-hearing-nomogram","ent","Hearing Loss Nomogram","聴力低下ノモグラム",
  "","",c(0,1),c(0,6),
  "聴力低下ノモグラムテンプレート。聴力予後の予測ツール。","nomogram,ノモグラム,聴力,予後,耳鼻",
  type="nomogram", ylb=c("Age","Noise Exposure","Duration","Family Hx","Comorbidity","Risk")))
lines <- c(lines, D_line("ent-audiometry-dot","ent","Audiometry Dot Plot","聴力検査ドットプロット",
  "","Hearing Level (dB HL)",c(0,4),c(0,80),
  "聴力検査ドットプロットテンプレート。各周波数の聴力閾値を比較。","dot plot,ドット,聴力,周波数,耳鼻",
  type="dot", xlb=c("250Hz","500Hz","1kHz","2kHz")))
lines <- c(lines, D_line("ent-pta-scatter-reg","ent","PTA vs Age Scatter Regression","PTA-年齢散布回帰",
  "Age (years)","PTA (dB HL)",c(20,90),c(0,80),
  "PTA-年齢散布回帰テンプレート。加齢と聴力の回帰分析。","scatter,regression,PTA,年齢,耳鼻",
  type="scatter_reg"))
lines <- c(lines, D_line("ent-hearing-calibration","ent","Hearing Test Calibration","聴力検査較正",
  "Predicted","Observed",c(0,1),c(0,1),
  "聴力検査較正テンプレート。聴力予測モデルの妥当性検証。","calibration,較正,聴力,検査",
  type="calibration"))
lines <- c(lines, D_line("ent-ci-tornado","ent","Cochlear Implant Tornado","人工内耳費用効果トルネード",
  "ICER (USD/QALY)","",c(-50000,50000),c(0,6),
  "人工内耳費用効果トルネード図テンプレート。人工内耳のCEA感度分析。","tornado,トルネード,人工内耳,費用効果",
  type="tornado", ylb=c("Device cost","Hearing gain","Surgery","Rehab","QoL","Discount")))

# ================================================================
# URO (11 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── uro: 11 missing types ──")
lines <- c(lines, D_line("uro-psa-curve","uro","PSA Kinetics Curve","PSA動態曲線",
  "Time (years)","PSA (ng/mL)",c(0,10),c(0,20),
  "PSA動態曲線テンプレート。PSA倍加時間と増加速度を追跡。","curve,曲線,PSA,動態,泌尿器",
  sub="PSA kinetics curve"))
lines <- c(lines, D_line("uro-gfr-psa-scatter","uro","eGFR vs PSA Scatter","eGFR-PSA散布図",
  "eGFR (mL/min/1.73m²)","PSA (ng/mL)",c(0,120),c(0,20),
  "eGFR-PSA散布図テンプレート。腎機能とPSA値の相関。","scatter,散布図,eGFR,PSA,泌尿器"))
lines <- c(lines, D_line("uro-ipss-violin","uro","IPSS Score Violin","IPSSスコアバイオリン",
  "Treatment","IPSS Score",c(0,3),c(0,35),
  "IPSSスコアバイオリンプロットテンプレート。BPH治療の症状スコア分布比較。","violin,バイオリン,IPSS,BPH,泌尿器",
  type="violin", xlb=c("Alpha-blocker","5ARI","Combined")))
lines <- c(lines, D_line("uro-surgery-bar","uro","Urological Surgery Bar","泌尿器手術棒グラフ",
  "Procedure","Cases",c(0,5),c(0,500),
  "泌尿器手術棒グラフテンプレート。手術種別の件数を比較。","bar,棒グラフ,手術,泌尿器,件数"))
lines <- c(lines, D_line("uro-psa-histogram","uro","PSA Distribution Histogram","PSA分布ヒストグラム",
  "PSA (ng/mL)","Frequency (度数)",c(0,20),c(0,40),
  "PSA分布ヒストグラムテンプレート。PSA値の度数分布。","histogram,ヒストグラム,PSA,度数,泌尿器"))
lines <- c(lines, D_line("uro-prostate-forest","uro","Prostate Treatment Forest","前立腺治療フォレスト",
  "Hazard Ratio (95% CI)","",c(0.2,5),c(0,6),
  "前立腺治療フォレストプロットテンプレート。前立腺癌治療のメタ分析。","forest,フォレスト,前立腺,治療,メタ分析",
  type="forest", ylb=c("Surgery","Radiation","Hormone","Chemo","Active Surv","Combined")))
lines <- c(lines, D_line("uro-prostate-nomogram","uro","Prostate Cancer Nomogram","前立腺癌ノモグラム",
  "","",c(0,1),c(0,7),
  "前立腺癌ノモグラムテンプレート。前立腺癌の予後予測ツール。","nomogram,ノモグラム,前立腺,予後,泌尿器",
  type="nomogram", ylb=c("PSA","Gleason","Stage","Age","Core %","Points","5-yr Recur")))
lines <- c(lines, D_line("uro-renal-function-dot","uro","Renal Function Dot Plot","腎機能ドットプロット",
  "","Value",c(0,4),c(0,200),
  "腎機能ドットプロットテンプレート。腎機能検査の各項目比較。","dot plot,ドット,腎機能,検査,泌尿器",
  type="dot", xlb=c("Cr","BUN","UA","Cystatin")))
lines <- c(lines, D_line("uro-psa-scatter-reg","uro","PSA vs Volume Scatter Regression","PSA-体積散布回帰",
  "Prostate Volume (mL)","PSA (ng/mL)",c(0,150),c(0,20),
  "PSA-前立腺体積散布回帰テンプレート。PSA密度の評価。","scatter,regression,PSA,体積,泌尿器",
  type="scatter_reg"))
lines <- c(lines, D_line("uro-risk-calibration","uro","Urological Risk Calibration","泌尿器リスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "泌尿器リスクスコア較正テンプレート。リスクモデルの検証。","calibration,較正,泌尿器,リスク",
  type="calibration"))
lines <- c(lines, D_line("uro-turp-tornado","uro","TURP CEA Tornado","TURP費用効果トルネード",
  "ICER (USD/QALY)","",c(-20000,20000),c(0,6),
  "TURP費用効果トルネード図テンプレート。BPH手術のCEA感度分析。","tornado,トルネード,TURP,BPH,費用効果",
  type="tornado", ylb=c("Surgery cost","IPSS improve","Complication","Reoperation","QoL","Discount")))

# ================================================================
# DERM (10 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── derm: 10 missing types ──")
lines <- c(lines, D_line("derm-wound-healing-curve","derm","Wound Healing Curve","創傷治癒曲線",
  "Time (days)","Wound Area (%)",c(0,30),c(0,100),
  "創傷治癒曲線テンプレート。創傷面積の経時的変化を追跡。","curve,曲線,創傷,治癒,皮膚",
  sub="Healing curve"))
lines <- c(lines, D_line("derm-pasi-scatter","derm","PASI vs DLQI Scatter","PASI-DLQI散布図",
  "PASI Score","DLQI Score",c(0,72),c(0,30),
  "PASI-DLQI散布図テンプレート。乾癬の重症度とQOLの相関。","scatter,散布図,PASI,DLQI,乾癬"))
lines <- c(lines, D_line("derm-eczema-violin","derm","Eczema Severity Violin","湿疹重症度バイオリン",
  "Treatment","SCORAD",c(0,3),c(0,100),
  "湿疹重症度バイオリンプロットテンプレート。アトピー治療の重症度分布比較。","violin,バイオリン,湿疹,SCORAD,皮膚",
  type="violin", xlb=c("Topical","Systemic","Biologic")))
lines <- c(lines, D_line("derm-lesion-bar","derm","Skin Lesion Type Bar Chart","皮膚病変型棒グラフ",
  "Lesion Type","Count",c(0,5),c(0,100),
  "皮膚病変型棒グラフテンプレート。病変タイプ別の頻度を比較。","bar,棒グラフ,病変,皮膚,分類"))
lines <- c(lines, D_line("derm-thickness-histogram","derm","Breslow Thickness Histogram","ブレスロー深度ヒストグラム",
  "Breslow Thickness (mm)","Frequency (度数)",c(0,10),c(0,30),
  "ブレスロー深度ヒストグラムテンプレート。メラノーマの厚さ分布。","histogram,ヒストグラム,ブレスロー,メラノーマ,度数"))
lines <- c(lines, D_line("derm-melanoma-nomogram","derm","Melanoma Nomogram","メラノーマノモグラム",
  "","",c(0,1),c(0,7),
  "メラノーマノモグラムテンプレート。メラノーマの予後予測。","nomogram,ノモグラム,メラノーマ,予後,皮膚",
  type="nomogram", ylb=c("Breslow","Ulceration","Mitosis","LN Status","Age","Points","5-yr Survival")))
lines <- c(lines, D_line("derm-skin-test-dot","derm","Skin Test Dot Plot","皮膚テストドットプロット",
  "","Wheal Size (mm)",c(0,5),c(0,20),
  "皮膚テストドットプロットテンプレート。各アレルゲンの皮膚反応を比較。","dot plot,ドット,皮膚テスト,アレルゲン",
  type="dot", xlb=c("HDM","Pollen","Cat","Mold","Food")))
lines <- c(lines, D_line("derm-pasi-scatter-reg","derm","PASI vs BSA Scatter Regression","PASI-BSA散布回帰",
  "BSA (%)","PASI Score",c(0,100),c(0,72),
  "PASI-BSA散布回帰テンプレート。乾癬の重症度指標の回帰分析。","scatter,regression,PASI,BSA,乾癬",
  type="scatter_reg"))
lines <- c(lines, D_line("derm-risk-calibration","derm","Melanoma Risk Calibration","メラノーマリスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "メラノーマリスクスコア較正テンプレート。リスクモデルの妥当性検証。","calibration,較正,メラノーマ,リスク",
  type="calibration"))
lines <- c(lines, D_line("derm-biologic-tornado","derm","Biologic Therapy Tornado","生物学的製剤トルネード",
  "ICER (USD/QALY)","",c(-50000,50000),c(0,6),
  "生物学的製剤費用効果トルネード図テンプレート。乾癬治療のCEA感度分析。","tornado,トルネード,生物学的製剤,費用効果",
  type="tornado", ylb=c("Drug cost","PASI75 rate","Infection","Discontinuation","QoL","Discount")))

# ================================================================
# PULM (11 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── pulm: 11 missing types ──")
lines <- c(lines, D_line("pulm-fev1-decline-curve","pulm","FEV1 Decline Curve","FEV1低下曲線",
  "Age (years)","FEV1 (L)",c(20,80),c(0,5),
  "FEV1低下曲線テンプレート。COPD患者のFEV1経年低下を表示。","curve,曲線,FEV1,COPD,低下",
  sub="FEV1 decline curve"))
lines <- c(lines, D_line("pulm-fev1-fvc-scatter","pulm","FEV1/FVC Scatter","FEV1-FVC散布図",
  "FVC (L)","FEV1 (L)",c(0,6),c(0,5),
  "FEV1-FVC散布図テンプレート。肺機能パラメータの相関分析。","scatter,散布図,FEV1,FVC,呼吸器"))
lines <- c(lines, D_line("pulm-spo2-violin","pulm","SpO2 Distribution Violin","SpO2分布バイオリン",
  "Condition","SpO2 (%)",c(0,3),c(70,100),
  "SpO2分布バイオリンプロットテンプレート。疾患別のSpO2分布比較。","violin,バイオリン,SpO2,酸素,呼吸器",
  type="violin", xlb=c("Normal","COPD","Pneumonia")))
lines <- c(lines, D_line("pulm-disease-bar","pulm","Lung Disease Bar Chart","肺疾患棒グラフ",
  "Disease","Prevalence (%)",c(0,5),c(0,20),
  "肺疾患棒グラフテンプレート。肺疾患の有病率を比較。","bar,棒グラフ,肺疾患,有病率,呼吸器"))
lines <- c(lines, D_line("pulm-fev1-histogram","pulm","FEV1 Distribution Histogram","FEV1分布ヒストグラム",
  "FEV1 (% predicted)","Frequency (度数)",c(20,120),c(0,30),
  "FEV1分布ヒストグラムテンプレート。FEV1予測値の度数分布。","histogram,ヒストグラム,FEV1,度数,呼吸器"))
lines <- c(lines, D_line("pulm-copd-forest","pulm","COPD Treatment Forest","COPD治療フォレスト",
  "Risk Ratio (95% CI)","",c(0.2,3),c(0,6),
  "COPD治療フォレストプロットテンプレート。COPD治療のメタ分析。","forest,フォレスト,COPD,治療,メタ分析",
  type="forest", ylb=c("LABA","LAMA","ICS/LABA","Triple","PDE4i","O2 therapy")))
lines <- c(lines, D_line("pulm-copd-nomogram","pulm","COPD Exacerbation Nomogram","COPD増悪ノモグラム",
  "","",c(0,1),c(0,7),
  "COPD増悪ノモグラムテンプレート。増悪リスクの予測ツール。","nomogram,ノモグラム,COPD,増悪,リスク",
  type="nomogram", ylb=c("FEV1","Exac. Hx","mMRC","BMI","Eosinophil","Points","1-yr Risk")))
lines <- c(lines, D_line("pulm-gas-dot","pulm","Blood Gas Dot Plot","血液ガスドットプロット",
  "","Value",c(0,5),c(0,150),
  "血液ガスドットプロットテンプレート。血液ガス各項目を比較。","dot plot,ドット,血液ガス,ABG,呼吸器",
  type="dot", xlb=c("pH×10","PaO2","PaCO2","HCO3","BE")))
lines <- c(lines, D_line("pulm-fev1-scatter-reg","pulm","FEV1 vs DLCO Scatter Regression","FEV1-DLCO散布回帰",
  "FEV1 (% pred)","DLCO (% pred)",c(20,120),c(20,120),
  "FEV1-DLCO散布回帰テンプレート。肺機能パラメータの回帰分析。","scatter,regression,FEV1,DLCO,呼吸器",
  type="scatter_reg"))
lines <- c(lines, D_line("pulm-risk-calibration","pulm","COPD Risk Calibration","COPDリスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "COPDリスクスコア較正テンプレート。増悪リスクモデルの検証。","calibration,較正,COPD,リスク",
  type="calibration"))
lines <- c(lines, D_line("pulm-inhaler-tornado","pulm","Inhaler CEA Tornado","吸入薬費用効果トルネード",
  "ICER (USD/QALY)","",c(-30000,30000),c(0,6),
  "吸入薬費用効果トルネード図テンプレート。COPD吸入薬のCEA感度分析。","tornado,トルネード,吸入薬,COPD,費用効果",
  type="tornado", ylb=c("Drug cost","FEV1 effect","Exacerbation","Pneumonia","QoL","Discount")))

# ================================================================
# SURG (12 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, radar_chart, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── surg: 12 missing types ──")
lines <- c(lines, D_line("surg-healing-curve","surg","Surgical Wound Healing Curve","術後創傷治癒曲線",
  "Post-op Day","Healing Score (%)",c(0,30),c(0,100),
  "術後創傷治癒曲線テンプレート。手術創の治癒過程を追跡。","curve,曲線,創傷,治癒,外科",
  sub="Wound healing curve"))
lines <- c(lines, D_line("surg-bmi-complication-scatter","surg","BMI vs Complication Scatter","BMI-合併症散布図",
  "BMI (kg/m²)","Complication Rate (%)",c(15,50),c(0,30),
  "BMI-合併症散布図テンプレート。術前BMIと術後合併症の関係。","scatter,散布図,BMI,合併症,外科"))
lines <- c(lines, D_line("surg-pain-violin","surg","Post-op Pain Violin","術後疼痛バイオリン",
  "Analgesia","NRS Score",c(0,4),c(0,10),
  "術後疼痛バイオリンプロットテンプレート。鎮痛法別の疼痛分布比較。","violin,バイオリン,疼痛,NRS,外科",
  type="violin", xlb=c("IV PCA","Epidural","Nerve Block","Oral")))
lines <- c(lines, D_line("surg-procedure-bar","surg","Surgical Procedure Bar Chart","術式棒グラフ",
  "Procedure","Cases",c(0,5),c(0,500),
  "術式棒グラフテンプレート。術式別の手術件数を比較。","bar,棒グラフ,術式,件数,外科"))
lines <- c(lines, D_line("surg-op-time-histogram","surg","Operative Time Histogram","手術時間ヒストグラム",
  "Op Time (min)","Frequency (度数)",c(0,360),c(0,30),
  "手術時間ヒストグラムテンプレート。手術時間の度数分布。","histogram,ヒストグラム,手術時間,度数,外科"))
lines <- c(lines, D_line("surg-complication-roc","surg","Surgical Complication ROC","術後合併症ROC",
  "1 - Specificity","Sensitivity",c(0,1),c(0,1),
  "術後合併症ROC曲線テンプレート。術後合併症予測スコアの精度。","ROC,合併症,予測,スコア,外科",
  type="roc"))
lines <- c(lines, D_line("surg-asa-radar","surg","Preoperative Assessment Radar","術前評価レーダー",
  "","",c(0,1),c(0,1),
  "術前評価レーダーテンプレート。術前全身状態を多軸で評価。","radar,レーダー,術前,評価,外科",
  type="radar", xlb=c("Cardiac","Pulmonary","Renal","Hepatic","Nutritional","Functional")))
lines <- c(lines, D_line("surg-risk-nomogram","surg","Surgical Risk Nomogram","手術リスクノモグラム",
  "","",c(0,1),c(0,7),
  "手術リスクノモグラムテンプレート。術後合併症リスクの予測。","nomogram,ノモグラム,手術,リスク,外科",
  type="nomogram", ylb=c("Age","ASA","Emergency","BMI","Wound Class","Points","30-d Mortality")))
lines <- c(lines, D_line("surg-lab-dot","surg","Preoperative Lab Dot Plot","術前検査ドットプロット",
  "","Value",c(0,5),c(0,300),
  "術前検査ドットプロットテンプレート。術前検査の各項目を比較。","dot plot,ドット,術前,検査,外科",
  type="dot", xlb=c("Hb","WBC","Plt","Alb","Cr")))
lines <- c(lines, D_line("surg-los-scatter-reg","surg","LOS vs Complication Scatter Regression","在院日数-合併症散布回帰",
  "Complications (n)","Length of Stay (days)",c(0,5),c(0,30),
  "在院日数-合併症散布回帰テンプレート。合併症数と在院日数の回帰分析。","scatter,regression,在院日数,合併症,外科",
  type="scatter_reg"))
lines <- c(lines, D_line("surg-risk-calibration","surg","Surgical Risk Calibration","手術リスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "手術リスクスコア較正テンプレート。リスクモデルの妥当性検証。","calibration,較正,手術,リスク",
  type="calibration"))
lines <- c(lines, D_line("surg-laparoscopic-tornado","surg","Laparoscopic CEA Tornado","腹腔鏡費用効果トルネード",
  "ICER (USD/QALY)","",c(-20000,20000),c(0,6),
  "腹腔鏡手術費用効果トルネード図テンプレート。腹腔鏡vs開腹のCEA分析。","tornado,トルネード,腹腔鏡,費用効果",
  type="tornado", ylb=c("Equip cost","LOS","Conversion","Complication","QoL","Discount")))

# ================================================================
# RADIO (12 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, kaplan_meier, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── radio: 12 missing types ──")
lines <- c(lines, D_line("radio-dose-response-curve","radio","Radiation Dose-Response Curve","放射線量-反応曲線",
  "Dose (Gy)","TCP/NTCP (%)",c(0,80),c(0,100),
  "放射線量-反応曲線テンプレート。腫瘍制御確率と正常組織障害確率。","curve,曲線,線量,TCP,NTCP",
  sub="Dose-response curve"))
lines <- c(lines, D_line("radio-suv-scatter","radio","SUV Scatter Plot","SUV散布図",
  "SUVmax (Pre)","SUVmax (Post)",c(0,20),c(0,20),
  "SUV散布図テンプレート。PET-CTのSUV変化を比較。","scatter,散布図,SUV,PET,放射線"))
lines <- c(lines, D_line("radio-dose-violin","radio","Dose Distribution Violin","線量分布バイオリン",
  "Organ","Dose (Gy)",c(0,4),c(0,60),
  "線量分布バイオリンプロットテンプレート。臓器別の被曝線量分布。","violin,バイオリン,線量,臓器,放射線",
  type="violin", xlb=c("PTV","Lung","Heart","Spinal Cord")))
lines <- c(lines, D_line("radio-modality-bar","radio","Imaging Modality Bar Chart","画像モダリティ棒グラフ",
  "Modality","Exams (/month)",c(0,5),c(0,1000),
  "画像モダリティ棒グラフテンプレート。各画像検査の実施件数を比較。","bar,棒グラフ,モダリティ,検査,放射線"))
lines <- c(lines, D_line("radio-hu-histogram","radio","HU Distribution Histogram","HU分布ヒストグラム",
  "Hounsfield Units","Frequency (度数)",c(-1000,1000),c(0,40),
  "HU分布ヒストグラムテンプレート。CT値の度数分布を表示。","histogram,ヒストグラム,HU,CT値,度数"))
lines <- c(lines, D_line("radio-rt-survival-km","radio","Radiotherapy Survival Curve","放射線治療生存曲線",
  "Time (months)","Survival Probability",c(0,60),c(0,1),
  "放射線治療生存曲線テンプレート。放射線治療後の生存分析。","kaplan,生存,放射線治療,予後",
  type="kaplan"))
lines <- c(lines, D_line("radio-rt-forest","radio","Radiotherapy Forest","放射線治療フォレスト",
  "Hazard Ratio (95% CI)","",c(0.2,5),c(0,6),
  "放射線治療フォレストプロットテンプレート。放射線治療のメタ分析。","forest,フォレスト,放射線,治療,メタ分析",
  type="forest", ylb=c("IMRT","SBRT","Proton","Brachy","Chemo-RT","Combined")))
lines <- c(lines, D_line("radio-risk-nomogram","radio","Radiation Risk Nomogram","放射線リスクノモグラム",
  "","",c(0,1),c(0,7),
  "放射線リスクノモグラムテンプレート。放射線障害リスクの予測。","nomogram,ノモグラム,放射線,リスク",
  type="nomogram", ylb=c("Total Dose","Fraction","Volume","Age","Organ","Points","Toxicity Risk")))
lines <- c(lines, D_line("radio-dvh-dot","radio","DVH Parameter Dot Plot","DVHパラメータドットプロット",
  "","Dose (Gy)",c(0,5),c(0,60),
  "DVHパラメータドットプロットテンプレート。臓器別の線量制約を比較。","dot plot,ドット,DVH,線量,放射線",
  type="dot", xlb=c("D95 PTV","V20 Lung","V25 Heart","D0.1 Cord","Dmax Lens")))
lines <- c(lines, D_line("radio-suv-scatter-reg","radio","SUV vs Size Scatter Regression","SUV-腫瘍径散布回帰",
  "Tumor Size (cm)","SUVmax",c(0,10),c(0,20),
  "SUV-腫瘍径散布回帰テンプレート。腫瘍径とSUV値の回帰分析。","scatter,regression,SUV,腫瘍径,PET",
  type="scatter_reg"))
lines <- c(lines, D_line("radio-risk-calibration","radio","Radiation Risk Calibration","放射線リスク較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "放射線障害リスクスコア較正テンプレート。リスクモデルの検証。","calibration,較正,放射線,リスク",
  type="calibration"))
lines <- c(lines, D_line("radio-rt-tornado","radio","Radiotherapy CEA Tornado","放射線治療費用効果トルネード",
  "ICER (USD/QALY)","",c(-50000,50000),c(0,6),
  "放射線治療費用効果トルネード図テンプレート。放射線治療のCEA感度分析。","tornado,トルネード,放射線,費用効果",
  type="tornado", ylb=c("Equipment","Fractions","Efficacy","Toxicity","QoL","Discount")))

# ================================================================
# EMER (12 missing: curve_fit, scatter_plot, violin, bar_chart, histogram, roc_curve, forest_plot, nomogram, dot_plot, scatter_regression, calibration_plot, tornado)
# ================================================================
lines <- c(lines, "", "# ── emer: 12 missing types ──")
lines <- c(lines, D_line("emer-resuscitation-curve","emer","Resuscitation Response Curve","蘇生応答曲線",
  "Time (min)","ROSC Rate (%)",c(0,30),c(0,100),
  "蘇生応答曲線テンプレート。心肺蘇生の時間経過と転帰。","curve,曲線,蘇生,ROSC,救急",
  sub="ROSC response curve"))
lines <- c(lines, D_line("emer-lactate-scatter","emer","Lactate vs Mortality Scatter","乳酸-死亡率散布図",
  "Lactate (mmol/L)","Mortality (%)",c(0,15),c(0,100),
  "乳酸-死亡率散布図テンプレート。乳酸値と転帰の関係。","scatter,散布図,乳酸,死亡率,救急"))
lines <- c(lines, D_line("emer-triage-violin","emer","Triage Score Violin","トリアージスコアバイオリン",
  "Category","Wait Time (min)",c(0,4),c(0,240),
  "トリアージスコアバイオリンプロットテンプレート。緊急度別の待ち時間分布。","violin,バイオリン,トリアージ,待ち時間,救急",
  type="violin", xlb=c("Red","Orange","Yellow","Green")))
lines <- c(lines, D_line("emer-chief-complaint-bar","emer","Chief Complaint Bar Chart","主訴棒グラフ",
  "Chief Complaint","Cases",c(0,6),c(0,500),
  "主訴棒グラフテンプレート。救急受診の主訴別件数を比較。","bar,棒グラフ,主訴,救急,受診"))
lines <- c(lines, D_line("emer-age-histogram","emer","ED Age Histogram","救急年齢ヒストグラム",
  "Age (years)","Frequency (度数)",c(0,100),c(0,40),
  "救急年齢ヒストグラムテンプレート。救急患者の年齢分布。","histogram,ヒストグラム,年齢,救急,度数"))
lines <- c(lines, D_line("emer-sepsis-roc","emer","Sepsis Screening ROC","敗血症スクリーニングROC",
  "1 - Specificity","Sensitivity",c(0,1),c(0,1),
  "敗血症スクリーニングROC曲線テンプレート。敗血症スコアの診断精度。","ROC,敗血症,qSOFA,スクリーニング,救急",
  type="roc"))
lines <- c(lines, D_line("emer-intervention-forest","emer","ED Intervention Forest","救急介入フォレスト",
  "Odds Ratio (95% CI)","",c(0.1,10),c(0,6),
  "救急介入フォレストプロットテンプレート。救急処置のメタ分析。","forest,フォレスト,救急,介入,メタ分析",
  type="forest", ylb=c("Epinephrine","Amiodarone","TXA","Antibiotics","Cooling","Fluids")))
lines <- c(lines, D_line("emer-trauma-nomogram","emer","Trauma Mortality Nomogram","外傷死亡ノモグラム",
  "","",c(0,1),c(0,7),
  "外傷死亡ノモグラムテンプレート。外傷患者の死亡リスク予測。","nomogram,ノモグラム,外傷,死亡,救急",
  type="nomogram", ylb=c("Age","GCS","SBP","ISS","Base Deficit","Points","Mortality")))
lines <- c(lines, D_line("emer-vital-dot","emer","Vital Signs Dot Plot","バイタルサインドットプロット",
  "","Value",c(0,5),c(0,200),
  "バイタルサインドットプロットテンプレート。各バイタルの測定値比較。","dot plot,ドット,バイタル,救急",
  type="dot", xlb=c("HR","SBP","RR","SpO2","Temp")))
lines <- c(lines, D_line("emer-gcs-scatter-reg","emer","GCS vs Outcome Scatter Regression","GCS-転帰散布回帰",
  "GCS Score","mRS at Discharge",c(3,15),c(0,6),
  "GCS-転帰散布回帰テンプレート。意識レベルと転帰の回帰分析。","scatter,regression,GCS,mRS,救急",
  type="scatter_reg"))
lines <- c(lines, D_line("emer-sepsis-calibration","emer","Sepsis Score Calibration","敗血症スコア較正",
  "Predicted Risk","Observed Risk",c(0,1),c(0,1),
  "敗血症スコア較正テンプレート。敗血症予測スコアの妥当性検証。","calibration,較正,敗血症,スコア",
  type="calibration"))
lines <- c(lines, D_line("emer-ems-tornado","emer","EMS Response Tornado","救急搬送トルネード",
  "Impact (minutes)","",c(-30,30),c(0,6),
  "救急搬送トルネード図テンプレート。搬送時間に影響する要因分析。","tornado,トルネード,搬送,救急",
  type="tornado", ylb=c("Distance","Traffic","Crew skill","Equipment","Triage","Weather")))

# ================================================================
# ANAT (10 missing: line_chart, curve_fit, log_plot, scatter_plot, scatter_regression, bar_chart, boxplot, histogram, radar_chart, dot_plot)
# ================================================================
lines <- c(lines, "", "# ── anat: 10 missing types ──")
lines <- c(lines, D_line("anat-growth-line","anat","Body Proportion Line Chart","体型比率推移グラフ",
  "Age (years)","Head-to-Body Ratio",c(0,18),c(0,0.5),
  "体型比率推移グラフテンプレート。発達に伴う体型比率の変化を表示。","line,グラフ,体型,比率,解剖"))
lines <- c(lines, D_line("anat-allometry-curve","anat","Allometric Growth Curve","アロメトリー成長曲線",
  "Body Mass (kg)","Organ Mass (g)",c(0,80),c(0,2000),
  "アロメトリー成長曲線テンプレート。器官の相対成長を表示。","curve,曲線,アロメトリー,成長,解剖",
  sub="Allometric curve"))
lines <- c(lines, D_line("anat-cell-count-log","anat","Cell Count Semi-log","細胞数セミログ",
  "Time (days)","Cell Count",c(0,14),c(100,1e7),
  "細胞数セミログテンプレート。細胞増殖を対数軸で表示。","semi-log,セミログ,細胞,増殖,解剖",
  ylog=TRUE))
lines <- c(lines, D_line("anat-length-weight-scatter","anat","Body Length vs Weight Scatter","身長-体重散布図",
  "Body Length (cm)","Weight (kg)",c(40,180),c(2,80),
  "身長-体重散布図テンプレート。発達段階の身長体重関係を表示。","scatter,散布図,身長,体重,解剖"))
lines <- c(lines, D_line("anat-organ-weight-scatter-reg","anat","Organ Weight Scatter Regression","臓器重量散布回帰",
  "Body Weight (kg)","Organ Weight (g)",c(0,100),c(0,2000),
  "臓器重量散布回帰テンプレート。体重と臓器重量の回帰分析。","scatter,regression,臓器,重量,解剖",
  type="scatter_reg"))
lines <- c(lines, D_line("anat-tissue-composition-bar","anat","Tissue Composition Bar Chart","組織構成棒グラフ",
  "Tissue Type","Percentage (%)",c(0,5),c(0,60),
  "組織構成棒グラフテンプレート。体組成の各組織割合を比較。","bar,棒グラフ,組織,体組成,解剖"))
lines <- c(lines, D_line("anat-muscle-force-box","anat","Muscle Force Boxplot","筋力ボックスプロット",
  "Muscle Group","Force (N)",c(0,4),c(0,500),
  "筋力ボックスプロットテンプレート。筋群別の発揮筋力分布を表示。","boxplot,箱ひげ,筋力,筋群,解剖",
  type="boxplot", xlb=c("Quad","Hamstring","Biceps","Triceps")))
lines <- c(lines, D_line("anat-bone-density-histogram","anat","Bone Density Histogram","骨密度ヒストグラム",
  "BMD (g/cm²)","Frequency (度数)",c(0.4,1.4),c(0,30),
  "骨密度ヒストグラムテンプレート。骨密度測定値の度数分布。","histogram,ヒストグラム,骨密度,度数,解剖"))
lines <- c(lines, D_line("anat-body-region-radar","anat","Body Region Assessment Radar","体部位評価レーダー",
  "","",c(0,1),c(0,1),
  "体部位評価レーダーテンプレート。各体部位の状態を多軸で評価。","radar,レーダー,体部位,評価,解剖",
  type="radar", xlb=c("Head/Neck","Upper Ext","Thorax","Abdomen","Pelvis","Lower Ext")))
lines <- c(lines, D_line("anat-organ-size-dot","anat","Organ Size Dot Plot","臓器サイズドットプロット",
  "","Size (cm)",c(0,5),c(0,30),
  "臓器サイズドットプロットテンプレート。主要臓器のサイズを比較。","dot plot,ドット,臓器,サイズ,解剖",
  type="dot", xlb=c("Heart","Liver","Kidney","Spleen","Pancreas")))

# ================================================================
# Count
# ================================================================
n_templates <- sum(grepl('^D\\("', lines))
cat(sprintf("Generated %d new template definitions\n", n_templates))

# Append to template_defs.R
cat("Appending to template_defs.R...\n")
con <- file("template_defs.R", "a")
writeLines(lines, con)
close(con)

# Verify
all_lines <- readLines("template_defs.R", warn=FALSE)
total <- length(grep('^D\\("', all_lines))
cat(sprintf("Total templates in file: %d\n", total))

# Check for duplicates
ids <- sub('^D\\("([^"]+)".*', '\\1', all_lines[grep('^D\\("', all_lines)])
dups <- ids[duplicated(ids)]
if (length(dups) > 0) {
  cat(sprintf("WARNING: %d duplicate IDs: %s\n", length(dups), paste(head(dups,10), collapse=", ")))
} else {
  cat("No duplicate IDs - clean!\n")
}
