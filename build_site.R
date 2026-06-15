#!/usr/bin/env Rscript
# ============================================================
# MedGraph Free - Complete Site Builder
# Generates 6,000+ template images and 400+ individual pages
# ============================================================

library(ggplot2)

cat("=== MedGraph Free Site Builder ===\n")
cat(sprintf("Start: %s\n", Sys.time()))

# ── Configuration ──
SITE_URL  <- "https://med-graph.com"
IMG_DIR   <- "img"
dir.create(IMG_DIR, showWarnings = FALSE)

# ── Category Definitions ──
CATS <- list(
  biochem = list(ja="生化学", en="Biochemistry", icon="\U0001F9EA"),
  physiol = list(ja="生理学", en="Physiology", icon="\U0001F9EC"),
  pharm   = list(ja="薬理学", en="Pharmacology", icon="\U0001F48A"),
  micro   = list(ja="微生物学", en="Microbiology", icon="\U0001F9A0"),
  immuno  = list(ja="免疫学", en="Immunology", icon="\U0001F9AC"),
  epi     = list(ja="疫学・公衆衛生", en="Epidemiology", icon="\U0001F4CA"),
  stat    = list(ja="臨床統計", en="Biostatistics", icon="\U0001F4C8"),
  neuro   = list(ja="神経科学", en="Neuroscience", icon="\U0001F9E0"),
  cardio  = list(ja="循環器", en="Cardiology", icon="\U00002764"),
  pulm    = list(ja="呼吸器", en="Pulmonology", icon="\U0001FAC1"),
  nephro  = list(ja="腎臓", en="Nephrology", icon="\U0001F9B7"),
  endo    = list(ja="内分泌", en="Endocrinology", icon="\U0001F9EA"),
  gi      = list(ja="消化器", en="Gastroenterology", icon="\U0001F9EA"),
  hemato  = list(ja="血液", en="Hematology", icon="\U0001FA78"),
  obgyn   = list(ja="産婦人科", en="OB/GYN", icon="\U0001F476"),
  peds    = list(ja="小児科", en="Pediatrics", icon="\U0001F476"),
  ortho   = list(ja="整形外科", en="Orthopedics", icon="\U0001F9B4"),
  eye     = list(ja="眼科", en="Ophthalmology", icon="\U0001F441"),
  psych   = list(ja="精神科", en="Psychiatry", icon="\U0001F9E0"),
  path    = list(ja="病理学", en="Pathology", icon="\U0001F52C"),
  radio   = list(ja="放射線", en="Radiology", icon="\U00002622"),
  anat    = list(ja="解剖学", en="Anatomy", icon="\U0001F9B4"),
  surg    = list(ja="外科・麻酔", en="Surgery", icon="\U0001FA7A"),
  emer    = list(ja="救急", en="Emergency", icon="\U0001F6D1"),
  chem    = list(ja="臨床検査", en="Clinical Chemistry", icon="\U0001F9EA"),
  derm    = list(ja="皮膚科", en="Dermatology", icon="\U0001F9EA"),
  ent     = list(ja="耳鼻咽喉科", en="ENT", icon="\U0001F442"),
  uro     = list(ja="泌尿器科", en="Urology", icon="\U0001F9EA")
)

# ── Sub-category Definitions (topic-based tabs per category) ──
SUBCATS <- list(
  biochem = list(
    list(id="enzyme",   ja="酵素動力学",
         kw=c("enzyme","酵素","km","vmax","michaelis","lineweaver","eadie","hanes","kinetic","速度","inhibit","阻害","substrate","基質","allosteric","アロステリック","cooperat","協同")),
    list(id="quant",    ja="検量線・定量",
         kw=c("standard curve","検量","bradford","bca","beer","lambert","吸光","assay","定量","spectro","分光","western","blot","バンド")),
    list(id="protein",  ja="タンパク質・核酸",
         kw=c("protein","タンパク","dna","rna","sds","page","scatchard","hill","denaturation","変性","melting","融解","binding","結合")),
    list(id="ph",       ja="pH・緩衝液",
         kw=c("ph","titration","滴定","henderson","hasselbalch","buffer","緩衝","amino acid","アミノ酸","pka")),
    list(id="reaction", ja="反応・熱力学",
         kw=c("arrhenius","reaction","反応座標","temperature","温度","energy","エネルギー","chromatography","クロマトグラフィー","溶出"))
  ),
  physiol = list(
    list(id="cv",   ja="循環器系",
         kw=c("cardiac","心","frank","starling","pv loop","wiggers","venous","return","guyton","baroreceptor","圧受容","心拍","output","血圧","coronary")),
    list(id="resp", ja="呼吸器系",
         kw=c("lung","肺","spirometry","スパイロ","flow.volume","compliance","コンプライアンス","capnography","カプノ","respiratory","呼吸","v/q","換気","volume")),
    list(id="neuro",ja="神経・筋",
         kw=c("nerve","神経","action potential","活動電位","muscle","筋","twitch","収縮","tetanus","強縮","membrane","膜電位","smooth","平滑","graded","段階","sa node","洞房")),
    list(id="renal",ja="腎・体液",
         kw=c("renal","腎","countercurrent","対向流","osmol","浸透","body fluid","体液","starling force","毛細血管")),
    list(id="gas",  ja="血液ガス",
         kw=c("o2","co2","oxygen","酸素","dissociation","解離","hemoglobin","ヘモグロビン","bohr","fetal","胎児"))
  ),
  pharm = list(
    list(id="dr",   ja="用量反応",
         kw=c("dose","用量","response","反応","ec50","ed50","ld50","td50","graded","quantal","量子","段階")),
    list(id="pk",   ja="薬物動態",
         kw=c("pk","pharmacokinet","薬物動態","iv","oral","bolus","multiple","反復","concentration","血中濃度","half","半減","semilog","bioavail","first.pass","loading","maintenance","clearance","accumul","蓄積")),
    list(id="interact",ja="拮抗薬・相互作用",
         kw=c("antagonist","拮抗","agonist","作動","competitive","noncompetitive","partial","相互作用","synergy","相乗","isobolo","schild")),
    list(id="safety",ja="治療域・安全性",
         kw=c("therapeutic","治療","window","index","safety","安全","receptor","受容体","occupancy","占有"))
  ),
  micro = list(
    list(id="growth_curve",ja="増殖・動態",
         kw=c("growth","増殖","kill","殺菌","time.kill","curve","動態","pk.pd")),
    list(id="resistance", ja="薬剤感受性",
         kw=c("antibiotic","抗菌","抗生","mic","zone","阻止","感受性","resistance","耐性")),
    list(id="diagnostics",ja="診断・検査",
         kw=c("titer","力価","elisa","pcr","増幅","serology","抗体","gram","染色")),
    list(id="epi_micro",  ja="疫学・疾患",
         kw=c("epidemic","流行","outbreak","アウトブレイク","infection","感染","hiv","viral","ウイルス","incubation","潜伏"))
  ),
  immuno = list(
    list(id="antibody", ja="抗体・免疫応答",
         kw=c("antibody","抗体","immunoglobulin","ig","titer","力価","response","応答","primary","secondary","一次","二次")),
    list(id="cells",    ja="細胞・サイトカイン",
         kw=c("cell","細胞","cytokine","サイトカイン","t cell","b cell","lymphocyte","リンパ","nk","マクロファージ","complement","補体")),
    list(id="allergy",  ja="アレルギー・自己免疫",
         kw=c("allergy","アレルギー","hypersensitivity","過敏","autoimmune","自己免疫","ige","histamine","ヒスタミン")),
    list(id="vaccine",  ja="ワクチン・免疫学的検査",
         kw=c("vaccine","ワクチン","immunization","免疫化","elisa","facs","flow cytometry","フローサイトメトリー","hla"))
  ),
  epi = list(
    list(id="measure",  ja="疫学指標",
         kw=c("incidence","罹患","prevalence","有病","rate","率","risk","リスク","odds","ratio","比","relative","相対","absolute","nnt","arr")),
    list(id="study",    ja="研究デザイン",
         kw=c("cohort","コホート","case.control","症例対照","rct","trial","試験","bias","バイアス","confound","交絡","power","検出力","sample","標本")),
    list(id="screen",   ja="スクリーニング",
         kw=c("screen","スクリーニング","sensitivity","感度","specificity","特異度","ppv","npv","likelihood","尤度","roc","diagnostic")),
    list(id="pop",      ja="人口・疾病統計",
         kw=c("population","人口","pyramid","ピラミッド","mortality","死亡","life table","生命表","demographic","人口動態","age","年齢"))
  ),
  stat = list(
    list(id="descriptive",ja="記述統計・分布",
         kw=c("boxplot","箱ひげ","violin","バイオリン","histogram","ヒストグラム","qq","正規","distribution","分布","descriptive","記述","dot plot","ドット","scatter","散布")),
    list(id="inference", ja="検定・推定",
         kw=c("bland","altman","一致","regression","回帰","correlation","相関","confidence","信頼","p value","sample size","標本","effect size","効果量","power","検出力")),
    list(id="survival",  ja="生存分析",
         kw=c("kaplan","meier","生存","survival","km","hazard","ハザード","cox","累積","incidence")),
    list(id="diagnostic",ja="診断・検査統計",
         kw=c("roc","sensitivity","感度","specificity","特異度","auc","calibration","キャリブレーション","nomogram","ノモグラム","2x2","二値")),
    list(id="meta",      ja="メタアナリシス",
         kw=c("forest","フォレスト","funnel","ファネル","meta","メタ","heterogeneity","異質性","publication bias","出版バイアス")),
    list(id="special",   ja="特殊プロット",
         kw=c("swimmer","スイマー","tornado","トルネード","waterfall","ウォーターフォール","radar","レーダー","heatmap","ヒートマップ","manhattan","マンハッタン","volcano","ボルケーノ","lod","cnv","genomic"))
  ),
  neuro = list(
    list(id="electro",  ja="電気生理・波形",
         kw=c("eeg","脳波","evoked","誘発電位","emg","筋電","nerve conduction","神経伝導","action potential","活動電位","波形")),
    list(id="cognitive",ja="認知・精神機能",
         kw=c("cognitive","認知","mmse","moca","memory","記憶","attention","注意","reaction time","反応時間","learning","学習")),
    list(id="neuro_anat",ja="神経解剖・機能",
         kw=c("dermatome","デルマトーム","homunculus","ホムンクルス","cerebral","大脳","icp","頭蓋内圧","csf","髄液","blood.brain","血液脳")),
    list(id="clinical_n",ja="臨床神経学",
         kw=c("stroke","脳卒中","glasgow","gcs","nihss","seizure","てんかん","pain","疼痛","nrs","vas"))
  ),
  cardio = list(
    list(id="hemodynamics",ja="血行動態",
         kw=c("pressure","圧","cardiac output","心拍出","frank","starling","pv","loop","wiggers","ventricular","心室","aortic","大動脈","sv","前負荷","後負荷")),
    list(id="rhythm",   ja="不整脈・心電図",
         kw=c("ecg","ekg","心電図","arrhythmia","不整脈","qt","st","interval","hr","心拍","リズム","ペースメーカー")),
    list(id="coronary", ja="冠動脈・虚血",
         kw=c("coronary","冠動脈","ischemia","虚血","troponin","トロポニン","stemi","acs","flow","blood flow","冠血流")),
    list(id="vascular", ja="血管・弁膜",
         kw=c("valve","弁","stenosis","狭窄","regurgitation","逆流","murmur","心雑音","aneurysm","動脈瘤","hypertension","高血圧","pulse","脈")),
    list(id="imaging_c",ja="心エコー・画像",
         kw=c("echo","エコー","ejection","駆出","bullseye","ブルズアイ","strain","ストレイン","doppler","ドプラ"))
  ),
  pulm = list(
    list(id="function", ja="呼吸機能検査",
         kw=c("spirometry","スパイロ","fev","fvc","flow.volume","フローボリューム","peak","ピーク","dlco","肺活量")),
    list(id="mech",     ja="呼吸力学",
         kw=c("compliance","コンプライアンス","resistance","気道抵抗","pressure","volume","圧","ventilat","換気")),
    list(id="gas_ex",   ja="ガス交換",
         kw=c("oxygen","酸素","co2","capno","カプノ","v/q","換気血流","abg","ガス","diffus","拡散")),
    list(id="disease_p",ja="疾患・臨床",
         kw=c("asthma","喘息","copd","fibrosis","線維","pneumo","肺炎","ards","sleep","睡眠","apnea","無呼吸"))
  ),
  nephro = list(
    list(id="physiol_n",ja="腎生理",
         kw=c("gfr","糸球体","clearance","クリアランス","autoregulation","自動調節","tubul","尿細管","countercurrent","対向流","concentration","濃縮")),
    list(id="electrolyte",ja="電解質・酸塩基",
         kw=c("sodium","na","ナトリウム","potassium","k","カリウム","calcium","ca","カルシウム","acid","base","酸","塩基","ph","bicarbonate","重炭酸","anion gap")),
    list(id="disease_n",ja="腎疾患・透析",
         kw=c("ckd","akut","急性","慢性","dialysis","透析","proteinuria","蛋白尿","creatinine","クレアチニン","bun","urea"))
  ),
  endo = list(
    list(id="glucose",  ja="糖代謝",
         kw=c("glucose","血糖","insulin","インスリン","hba1c","ogtt","糖負荷","diabetes","糖尿","pancrea","膵")),
    list(id="thyroid",  ja="甲状腺",
         kw=c("thyroid","甲状腺","tsh","t3","t4","thyroxine")),
    list(id="adrenal",  ja="副腎・下垂体",
         kw=c("cortisol","コルチゾール","acth","adrenal","副腎","pituitary","下垂体","aldosterone","アルドステロン","gh","growth hormone","成長ホルモン","diurnal","日内変動")),
    list(id="calcium_e",ja="カルシウム・骨代謝",
         kw=c("calcium","カルシウム","pth","parathyroid","副甲状腺","vitamin d","ビタミンd","bone","骨","phosphat","リン"))
  ),
  gi = list(
    list(id="motility", ja="消化管運動",
         kw=c("motility","運動","peristal","蠕動","gastric","胃","emptying","排出","swallow","嚥下")),
    list(id="secretion",ja="分泌・消化",
         kw=c("acid","酸","pepsin","ペプシン","secretion","分泌","bile","胆汁","enzyme","消化","absorption","吸収")),
    list(id="liver",    ja="肝・胆・膵",
         kw=c("liver","肝","hepat","alt","ast","bilirubin","ビリルビン","gallbladder","胆嚢","child","pugh","meld")),
    list(id="clinical_g",ja="臨床・内視鏡",
         kw=c("endoscop","内視鏡","stool","便","calprotectin","colonoscop","bristol","scoring","スコア"))
  ),
  hemato = list(
    list(id="cbc",      ja="血算・血球",
         kw=c("cbc","hemoglobin","ヘモグロビン","hematocrit","ヘマトクリット","rbc","wbc","platelet","血小板","reticulocyte","網赤血球","mcv")),
    list(id="coagulation",ja="凝固・線溶",
         kw=c("coagulation","凝固","pt","aptt","inr","fibrinogen","フィブリノゲン","d.dimer","Dダイマー","dic","cascade","カスケード","thrombo","血栓","anticoagul","抗凝固")),
    list(id="anemia",   ja="貧血・鉄代謝",
         kw=c("anemia","貧血","iron","鉄","ferritin","フェリチン","transferrin","トランスフェリン","hemolysis","溶血","b12","folate","葉酸")),
    list(id="malig_h",  ja="血液腫瘍",
         kw=c("leukemia","白血病","lymphoma","リンパ腫","survival","生存","bone marrow","骨髄","blast","芽球","治療"))
  ),
  obgyn = list(
    list(id="pregnancy",ja="妊娠・分娩",
         kw=c("pregnan","妊娠","fetal","胎児","contraction","収縮","ctg","bishop","ビショップ","labor","分娩","cervical","子宮頸","partogram")),
    list(id="fetal",    ja="胎児発育・評価",
         kw=c("growth","成長","biophysical","bpp","nst","amniotic","羊水","weight","体重","ultrasound","超音波","bpd","fl")),
    list(id="hormone_o",ja="ホルモン・月経",
         kw=c("hormone","ホルモン","estrogen","エストロゲン","progester","プロゲステロン","menstr","月経","ovul","排卵","hcg","lh","fsh")),
    list(id="gyn",      ja="婦人科疾患",
         kw=c("cervical cancer","子宮","ovarian","卵巣","endometri","内膜","fibroid","筋腫","screen","スクリーニング"))
  ),
  peds = list(
    list(id="growth_p", ja="成長・発達",
         kw=c("growth","成長","height","身長","weight","体重","percentile","パーセンタイル","bmi","head","頭囲","developmental","発達","milestone","マイルストーン")),
    list(id="neonatal", ja="新生児",
         kw=c("neonatal","新生児","apgar","アプガー","bilirubin","ビリルビン","jaundice","黄疸","gestational","在胎")),
    list(id="vaccine_p",ja="予防接種・感染",
         kw=c("vaccine","ワクチン","immuniz","予防接種","infection","感染","fever","発熱")),
    list(id="clinical_p",ja="臨床・栄養",
         kw=c("nutrition","栄養","fluid","輸液","dehydration","脱水","pain","疼痛","score","スコア","assess","評価","radar","レーダー"))
  ),
  ortho = list(
    list(id="bone",     ja="骨・骨折",
         kw=c("bone","骨","fracture","骨折","healing","治癒","density","骨密度","dexa","osteopor","骨粗鬆")),
    list(id="joint",    ja="関節・可動域",
         kw=c("joint","関節","rom","range","可動域","goniometry","角度","arthritis","関節炎","cartilage","軟骨")),
    list(id="rehab",    ja="リハビリ・機能",
         kw=c("rehab","リハビリ","function","機能","strength","筋力","gait","歩行","pain","疼痛","score","スコア","outcome","転帰"))
  ),
  eye = list(
    list(id="visual",   ja="視力・視野",
         kw=c("visual","視力","acuity","field","視野","perimetry","ペリメトリー","blind","盲点","scotoma","暗点")),
    list(id="pressure_e",ja="眼圧・緑内障",
         kw=c("pressure","眼圧","glaucoma","緑内障","iop","tonometry","トノメトリー")),
    list(id="retina",   ja="網膜・眼底",
         kw=c("retina","網膜","macula","黄斑","oct","angiography","造影","diabetic","糖尿病","fundus","眼底")),
    list(id="lens",     ja="水晶体・屈折",
         kw=c("lens","水晶体","cataract","白内障","refraction","屈折","myopia","近視","astigmatism","乱視"))
  ),
  psych = list(
    list(id="scale",    ja="評価尺度",
         kw=c("score","スコア","scale","尺度","rating","評価","phq","gad","hdrs","madrs","bdi","stai","panss")),
    list(id="cognitive_p",ja="認知機能",
         kw=c("cognitive","認知","mmse","moca","memory","記憶","dementia","認知症","intelligence","知能")),
    list(id="clinical_ps",ja="臨床・治療",
         kw=c("treatment","治療","response","反応","remission","寛解","relapse","再発","sleep","睡眠","circadian","概日","side effect","副作用")),
    list(id="substance",ja="依存・発達",
         kw=c("alcohol","アルコール","substance","物質","addiction","依存","withdrawal","離脱","autism","自閉","adhd","発達障害"))
  ),
  path = list(
    list(id="histopath",ja="組織病理",
         kw=c("histolog","組織","patholog","病理","grade","グレード","stage","ステージ","tnm","classification","分類")),
    list(id="molecular",ja="分子病理",
         kw=c("pcr","増幅","melting","融解","gene","遺伝子","mutation","変異","expression","発現","immunohistochem","免疫染色","biomarker","バイオマーカー")),
    list(id="cytology", ja="細胞診・血液",
         kw=c("cytology","細胞","smear","塗抹","flow","フローサイトメトリー","cell count","細胞数","leukocyte","白血球")),
    list(id="autopsy",  ja="剖検・法医学",
         kw=c("autopsy","剖検","forensic","法医","death","死亡","decomp","腐敗","toxicol","中毒"))
  ),
  radio = list(
    list(id="dose",     ja="線量・防護",
         kw=c("dose","線量","radiation","放射線","exposure","被曝","protection","防護","hounsfield","hu")),
    list(id="contrast", ja="造影・画像",
         kw=c("contrast","造影","enhancement","増強","signal","信号","roi","関心領域","snr")),
    list(id="planning", ja="治療計画",
         kw=c("plan","計画","dvh","target","標的","isodose","等線量","fractionation","分割")),
    list(id="nuclear",  ja="核医学",
         kw=c("nuclear","核医学","pet","spect","uptake","取り込み","suv","thyroid","甲状腺","scintigraphy","シンチ"))
  ),
  surg = list(
    list(id="anesthesia",ja="麻酔・モニタリング",
         kw=c("anesthesia","麻酔","monitor","モニタリング","mac","蒸気圧","vaporizer","気化器","airway","気道","intubation","挿管")),
    list(id="operative",ja="手術・術式",
         kw=c("operative","手術","surgical","外科","blood loss","出血","suture","縫合","wound","創傷","healing","治癒")),
    list(id="periop",   ja="周術期管理",
         kw=c("perioper","周術期","recovery","回復","complication","合併症","risk","リスク","asa","score","スコア","dvt","vte"))
  ),
  emer = list(
    list(id="resuscitation",ja="蘇生・外傷",
         kw=c("resuscitation","蘇生","cpr","心肺","acls","bls","trauma","外傷","shock","ショック","hemorrh","出血")),
    list(id="triage",   ja="トリアージ・評価",
         kw=c("triage","トリアージ","severity","重症度","score","スコア","sofa","qsofa","apache","gcs","glasgow")),
    list(id="toxicology",ja="中毒・環境",
         kw=c("poison","中毒","toxicol","overdose","過量","antidote","拮抗薬","decontam","除染","burn","熱傷","hypothermia","低体温")),
    list(id="procedure",ja="手技・処置",
         kw=c("procedure","処置","intubation","挿管","line","ライン","drainage","ドレナージ","sepsis","敗血症","bundle"))
  ),
  chem = list(
    list(id="qc",       ja="精度管理",
         kw=c("qc","精度管理","levey","jennings","westgard","control","管理図","cv","変動係数","calibration","校正")),
    list(id="clinical_c",ja="臨床検査値",
         kw=c("reference","基準","range","範囲","normal","正常","panel","パネル","trend","推移","critical","緊急")),
    list(id="method",   ja="検査法・原理",
         kw=c("method","方法","assay","アッセイ","elisa","immunoassay","免疫測定","chromatography","クロマトグラフィー","mass","質量分析")),
    list(id="interpret",ja="結果解釈",
         kw=c("interpret","解釈","decision","判断","algorithm","アルゴリズム","cutoff","カットオフ","roi","delta","変動"))
  ),
  derm = list(
    list(id="scoring",  ja="重症度評価",
         kw=c("score","スコア","severity","重症度","pasi","scorad","easi","bsa","面積")),
    list(id="wound_d",  ja="創傷・治癒",
         kw=c("wound","創傷","healing","治癒","burn","熱傷","ulcer","潰瘍","skin","皮膚")),
    list(id="photo",    ja="光線・アレルギー",
         kw=c("uv","紫外","phototherapy","光線","patch test","パッチテスト","allergy","アレルギー","melanoma","黒色腫"))
  ),
  ent = list(
    list(id="audiology",ja="聴覚・平衡",
         kw=c("audio","聴力","hearing","聴覚","tympan","ティンパノメトリー","abr","oae","vertigo","めまい","nystagmus","眼振","vestibular","前庭")),
    list(id="airway_e", ja="気道・音声",
         kw=c("voice","音声","vocal","声帯","larynx","喉頭","pharynx","咽頭","airway","気道","sleep","睡眠","apnea","無呼吸")),
    list(id="nose",     ja="鼻・副鼻腔",
         kw=c("nasal","鼻","sinus","副鼻腔","rhinitis","鼻炎","smell","嗅覚","olfactory"))
  ),
  uro = list(
    list(id="voiding",  ja="排尿機能",
         kw=c("void","排尿","urodynamic","尿流動態","flow","尿流","residual","残尿","frequency","頻尿","bladder","膀胱")),
    list(id="prostate", ja="前立腺・腫瘍",
         kw=c("prostate","前立腺","psa","cancer","がん","staging","病期","gleason","グリソン")),
    list(id="stone",    ja="結石・感染",
         kw=c("stone","結石","calcul","尿路","uti","感染","urine","尿","creatinine","クレアチニン"))
  ),
  anat = list(
    list(id="gross",    ja="肉眼解剖",
         kw=c("dermatome","デルマトーム","myotome","筋節","anatomy","解剖","region","部位","surface","体表")),
    list(id="hist",     ja="組織学",
         kw=c("histology","組織","cell","細胞","microscop","顕微鏡","staining","染色")),
    list(id="embryo",   ja="発生学",
         kw=c("embryo","発生","develop","分化","organogen","器官形成","fetal","胎児"))
  )
)

# Auto-classify a template into its subcategory
classify_subcat <- function(t) {
  cat_subcats <- SUBCATS[[t$cat]]
  if (is.null(cat_subcats)) return("other")

  txt <- tolower(paste(t$en, t$ja, t$tags, t$dj, t$xl, t$yl,
                        if (!is.null(t$sub)) t$sub else ""))

  for (sc in cat_subcats) {
    for (kw in sc$kw) {
      if (grepl(kw, txt, fixed = TRUE)) return(sc$id)
    }
  }
  "other"
}

# Get subcategory display name
get_subcat_ja <- function(cat_id, subcat_id) {
  cat_subcats <- SUBCATS[[cat_id]]
  if (is.null(cat_subcats)) return(NULL)
  for (sc in cat_subcats) {
    if (sc$id == subcat_id) return(sc$ja)
  }
  NULL
}

# ── Template Definition Helper ──
D <- function(id, cat, en, ja, xl, yl, xr, yr, dj, tags="",
              sub=NULL, xlog=FALSE, ylog=FALSE,
              hl=NULL, vl=NULL, ann=NULL, zones=NULL,
              xb=NULL, yb=NULL, xlb=NULL, ylb=NULL,
              type="xy") {
  list(id=id, cat=cat, en=en, ja=ja, xl=xl, yl=yl, xr=xr, yr=yr,
       dj=dj, tags=tags, sub=sub, xlog=xlog, ylog=ylog,
       hl=hl, vl=vl, ann=ann, zones=zones,
       xb=xb, yb=yb, xlb=xlb, ylb=ylb, type=type)
}

# ── Theme Generator ──
make_theme <- function(style = "standard", base_size = 14) {
  th <- switch(style,
    standard = theme_bw(base_size = base_size) + theme(
      plot.title = element_text(face="bold", hjust=0.5, size=base_size+2),
      plot.subtitle = element_text(hjust=0.5, color="grey40", size=base_size-3),
      panel.grid.major = element_line(color="grey85", linewidth=0.4),
      panel.grid.minor = element_line(color="grey92", linewidth=0.2),
      plot.margin = margin(15,15,10,10)),
    minimal = theme_minimal(base_size = base_size) + theme(
      plot.title = element_text(face="bold", hjust=0.5, size=base_size+2),
      plot.subtitle = element_text(hjust=0.5, color="grey40", size=base_size-3),
      plot.margin = margin(15,15,10,10)),
    classic = theme_classic(base_size = base_size) + theme(
      plot.title = element_text(face="bold", hjust=0.5, size=base_size+2),
      plot.subtitle = element_text(hjust=0.5, color="grey40", size=base_size-3),
      plot.margin = margin(15,15,10,10)),
    presentation = theme_minimal(base_size = 20) + theme(
      plot.title = element_text(face="bold", hjust=0.5, size=24),
      plot.subtitle = element_text(hjust=0.5, color="grey40", size=16),
      axis.title = element_text(face="bold"),
      panel.grid.major = element_line(color="grey80", linewidth=0.5),
      plot.margin = margin(20,20,15,15)),
    dark = theme_dark(base_size = base_size) + theme(
      plot.title = element_text(face="bold", hjust=0.5, size=base_size+2, color="white"),
      plot.subtitle = element_text(hjust=0.5, color="grey70", size=base_size-3),
      plot.background = element_rect(fill="#2d2d2d"),
      panel.background = element_rect(fill="#3d3d3d"),
      axis.text = element_text(color="grey80"),
      axis.title = element_text(color="grey90"),
      plot.margin = margin(15,15,10,10))
  )
  return(th)
}

# ── Special Plot Helpers ──
spcols <- function(style) {
  dk <- style=="dark"
  list(bg=if(dk)"#2d2d2d"else"white", fg=if(dk)"grey80"else"grey40",
       tc=if(dk)"white"else"black", gc=if(dk)"grey50"else"grey80",
       ac=if(dk)"grey60"else"grey70", fl=if(dk)"#3d3d3d"else"#f0f4ff")
}
sptitle <- function(t, lang) switch(lang, en=t$en, ja=t$ja, NULL)
sptheme <- function(style, cols) {
  theme_void(base_size=if(style=="presentation")20 else 14) +
    theme(plot.background=element_rect(fill=cols$bg,color=NA),
          plot.title=element_text(hjust=0.5,face="bold",color=cols$tc,
                                  size=if(style=="presentation")24 else 16),
          plot.subtitle=element_text(hjust=0.5,color=cols$fg,size=if(style=="presentation")14 else 11),
          plot.margin=margin(15,15,10,10))
}

# Radar/Spider chart: xr[2]=num axes, xlb=axis labels
make_radar <- function(t, style, lang) {
  C <- spcols(style); n <- max(3L, as.integer(t$xr[2]))
  a <- seq(0, 2*pi, length.out=n+1)[-(n+1)]
  p <- ggplot()+coord_fixed(xlim=c(-1.4,1.4),ylim=c(-1.4,1.4))
  for(r in seq(.2,1,.2)){
    cr <- data.frame(x=r*cos(seq(0,2*pi,len=100)),y=r*sin(seq(0,2*pi,len=100)))
    p <- p+geom_path(data=cr,aes(x,y),color=C$gc,linewidth=.3)
  }
  for(i in 1:n) p <- p+annotate("segment",x=0,y=0,xend=cos(a[i]),yend=sin(a[i]),color=C$ac,linewidth=.3)
  if(lang!="none"&&!is.null(t$xlb)) for(i in seq_along(t$xlb))
    p <- p+annotate("text",x=1.22*cos(a[i]),y=1.22*sin(a[i]),label=t$xlb[i],size=3,color=C$fg)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# Ternary diagram: xlb=c(label_A,label_B,label_C)
make_ternary <- function(t, style, lang) {
  C <- spcols(style); h <- sqrt(3)/2
  tri <- data.frame(x=c(0,1,.5,0),y=c(0,0,h,0))
  p <- ggplot()+coord_fixed(xlim=c(-.15,1.15),ylim=c(-.12,h+.1))
  p <- p+geom_path(data=tri,aes(x,y),color=C$ac,linewidth=.6)
  for(i in 1:9){f<-i/10
    p <- p+annotate("segment",x=f*.5,y=f*h,xend=1-f*.5,yend=f*h,color=C$gc,linewidth=.15,alpha=.5)
    p <- p+annotate("segment",x=f,y=0,xend=.5+f*.5,yend=(1-f)*h,color=C$gc,linewidth=.15,alpha=.5)
    p <- p+annotate("segment",x=f*.5,y=f*h,xend=f,yend=0,color=C$gc,linewidth=.15,alpha=.5)
  }
  if(lang!="none"&&!is.null(t$xlb)&&length(t$xlb)>=3){
    p <- p+annotate("text",x=-.06,y=-.05,label=t$xlb[1],size=3.5,color=C$fg,hjust=1)
    p <- p+annotate("text",x=1.06,y=-.05,label=t$xlb[2],size=3.5,color=C$fg,hjust=0)
    p <- p+annotate("text",x=.5,y=h+.06,label=t$xlb[3],size=3.5,color=C$fg,vjust=0)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# Heatmap grid: xr[2]=ncol, yr[2]=nrow, xlb/ylb=labels
make_heatmap <- function(t, style, lang) {
  C <- spcols(style)
  nc <- max(2L,as.integer(t$xr[2])); nr <- max(2L,as.integer(t$yr[2]))
  g <- expand.grid(x=1:nc,y=1:nr)
  p <- ggplot(g,aes(x,y))+geom_tile(fill=C$fl,color=C$gc,linewidth=.3)
  xl <- if(lang=="none")""else t$xl; yl <- if(lang=="none")""else t$yl
  p <- p+labs(x=xl,y=yl)
  if(!is.null(t$xlb)&&lang!="none") p<-p+scale_x_continuous(breaks=1:nc,labels=head(t$xlb,nc))
  if(!is.null(t$ylb)&&lang!="none") p<-p+scale_y_continuous(breaks=1:nr,labels=head(t$ylb,nr))
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt)
  p+make_theme(style)+theme(panel.grid=element_blank())
}

# Audiogram: fixed frequency/dB grid, inverted y-axis
make_audiogram <- function(t, style, lang) {
  C <- spcols(style)
  freqs <- c(125,250,500,1000,2000,4000,8000)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none")""else"Frequency (Hz)"
  yl <- if(lang=="none")""else"Hearing Level (dB HL)"
  p <- p+scale_x_log10(name=xl,limits=c(100,10000),breaks=freqs,labels=freqs)+
    scale_y_reverse(name=yl,limits=c(120,-10),breaks=seq(-10,120,10))
  p <- p+annotate("rect",xmin=100,xmax=10000,ymin=-10,ymax=25,fill="green",alpha=.06)
  if(lang!="none") p<-p+annotate("text",x=200,y=10,label="Normal",color="grey50",size=3)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt)
  p+make_theme(style)
}

# Polar/Visual field: concentric rings with meridians
make_polar_vf <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot()+coord_fixed(xlim=c(-35,35),ylim=c(-35,35))
  for(r in seq(10,30,10)){
    cr <- data.frame(x=r*cos(seq(0,2*pi,len=100)),y=r*sin(seq(0,2*pi,len=100)))
    p <- p+geom_path(data=cr,aes(x,y),color=C$gc,linewidth=.3)
  }
  for(a in seq(0,pi-.01,pi/4))
    p <- p+annotate("segment",x=-30*cos(a),y=-30*sin(a),xend=30*cos(a),yend=30*sin(a),color=C$gc,linewidth=.2)
  if(lang!="none"){
    for(r in c(10,20,30)) p<-p+annotate("text",x=r+1,y=1,label=paste0(r,"°"),size=2.5,color=C$fg)
    p <- p+annotate("text",x=0,y=-33,label="Inferior",size=3,color=C$fg)
    p <- p+annotate("text",x=0,y=33,label="Superior",size=3,color=C$fg)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt)
  p+sptheme(style,C)
}

# ── Kaplan-Meier Survival Curve Template ──
make_kaplan <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Time"
  yl <- if(lang=="none") "" else "Survival Probability"
  p <- p+scale_x_continuous(name=xl,limits=c(0,as.numeric(t$xr[2])),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,1),breaks=seq(0,1,.2),expand=c(0,0))
  p <- p+geom_hline(yintercept=0.5,linetype="dashed",color="grey60",linewidth=0.3)
  if(lang!="none") p <- p+annotate("text",x=as.numeric(t$xr[2])*0.02,y=0.52,label="Median",color="grey50",hjust=0,size=3)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── ROC Curve Template ──
make_roc <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "1 - Specificity (FPR)"
  yl <- if(lang=="none") "" else "Sensitivity (TPR)"
  p <- p+scale_x_continuous(name=xl,limits=c(0,1),breaks=seq(0,1,.2),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,1),breaks=seq(0,1,.2),expand=c(0,0))
  p <- p+geom_abline(slope=1,intercept=0,linetype="dashed",color="grey60",linewidth=0.4)
  if(lang!="none") p <- p+annotate("text",x=0.65,y=0.55,label="Reference\n(AUC = 0.5)",color="grey50",size=3,angle=35)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+coord_fixed()
}

# ── Forest Plot Template ──
make_forest <- function(t, style, lang) {
  C <- spcols(style)
  n <- if(!is.null(t$ylb)) length(t$ylb) else max(3L,as.integer(t$yr[2]))
  lbls <- if(!is.null(t$ylb)) head(t$ylb,n) else paste("Study",1:n)
  df <- data.frame(y=1:n,lab=lbls)
  p <- ggplot(df,aes(x=0,y=y))+geom_blank()
  p <- p+geom_vline(xintercept=1,linetype="dashed",color="grey50",linewidth=0.5)
  if(lang!="none") p <- p+geom_vline(xintercept=0,linetype="dotted",color="grey80",linewidth=0.3)
  for(i in 1:n) p <- p+annotate("segment",x=-0.5,xend=2.5,y=i,yend=i,color=C$gc,linewidth=0.15)
  xl <- if(lang=="none") "" else if(nchar(t$xl)>0) t$xl else "Odds Ratio (95% CI)"
  yl <- if(lang=="none") "" else ""
  p <- p+scale_x_continuous(name=xl,limits=c(-1,3.5))
  if(lang!="none") {
    p <- p+scale_y_continuous(name=yl,breaks=1:n,labels=lbls,expand=expansion(add=0.8))
  } else {
    p <- p+scale_y_continuous(name=yl,breaks=1:n,labels=rep("",n),expand=expansion(add=0.8))
  }
  if(lang!="none"){
    p <- p+annotate("text",x=-0.8,y=n+0.7,label="Favours\nControl",size=2.5,color="grey50",hjust=0.5)
    p <- p+annotate("text",x=2.8,y=n+0.7,label="Favours\nTreatment",size=2.5,color="grey50",hjust=0.5)
  }
  # Diamond shape for summary
  dx <- c(0.5,1,1.5,1); dy_off <- c(0,0.2,0,-0.2)
  dm <- data.frame(x=dx,y=dy_off+0.1)
  p <- p+geom_polygon(data=dm,aes(x=x,y=y),fill=C$gc,color=C$ac)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Funnel Plot Template ──
make_funnel <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else if(nchar(t$xl)>0) t$xl else "Effect Size"
  yl <- if(lang=="none") "" else if(nchar(t$yl)>0) t$yl else "Standard Error"
  cx <- mean(as.numeric(t$xr))
  p <- p+scale_x_continuous(name=xl,limits=as.numeric(t$xr))+
    scale_y_reverse(name=yl,limits=c(as.numeric(t$yr[2]),0))
  p <- p+geom_vline(xintercept=cx,linetype="dashed",color="grey50",linewidth=0.4)
  # Funnel lines
  se_max <- as.numeric(t$yr[2])
  p <- p+annotate("segment",x=cx-1.96*se_max,y=se_max,xend=cx,yend=0,linetype="dotted",color="grey60",linewidth=0.3)
  p <- p+annotate("segment",x=cx+1.96*se_max,y=se_max,xend=cx,yend=0,linetype="dotted",color="grey60",linewidth=0.3)
  if(lang!="none") p <- p+annotate("text",x=cx,y=se_max*0.05,label="Overall Effect",color="grey50",size=3,vjust=0)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Bland-Altman Plot Template ──
make_bland_altman <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Mean of Two Methods"
  yl <- if(lang=="none") "" else "Difference (Method 1 - Method 2)"
  p <- p+scale_x_continuous(name=xl,limits=as.numeric(t$xr))+
    scale_y_continuous(name=yl,limits=as.numeric(t$yr))
  p <- p+geom_hline(yintercept=0,linetype="solid",color="grey40",linewidth=0.5)
  lim <- as.numeric(t$yr[2])*0.6
  p <- p+geom_hline(yintercept=lim,linetype="dashed",color="#dc2626",linewidth=0.3)
  p <- p+geom_hline(yintercept=-lim,linetype="dashed",color="#dc2626",linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=as.numeric(t$xr[1])+diff(as.numeric(t$xr))*0.02,y=lim*1.1,label="+1.96 SD",color="#dc2626",hjust=0,size=3)
    p <- p+annotate("text",x=as.numeric(t$xr[1])+diff(as.numeric(t$xr))*0.02,y=-lim*1.1,label="-1.96 SD",color="#dc2626",hjust=0,size=3)
    p <- p+annotate("text",x=as.numeric(t$xr[1])+diff(as.numeric(t$xr))*0.02,y=0.05*diff(as.numeric(t$yr)),label="Mean Diff",color="grey40",hjust=0,size=3)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Box Plot Template ──
make_boxplot <- function(t, style, lang) {
  C <- spcols(style)
  ng <- if(!is.null(t$xlb)) length(t$xlb) else max(2L,as.integer(t$xr[2]))
  glb <- if(!is.null(t$xlb)) head(t$xlb,ng) else paste("Group",1:ng)
  df <- data.frame(g=factor(rep(1:ng,each=5),labels=glb),
                   v=rep(c(0.2,0.35,0.5,0.65,0.8),ng)*diff(as.numeric(t$yr))+as.numeric(t$yr[1]))
  xl <- if(lang=="none") "" else t$xl
  yl <- if(lang=="none") "" else t$yl
  p <- ggplot(df,aes(x=g,y=v))+geom_boxplot(fill=C$fl,color=C$ac,width=0.5,outlier.shape=NA)
  if(lang=="none") p <- p+scale_x_discrete(name="",labels=rep("",ng)) else p <- p+labs(x=xl)
  p <- p+labs(y=yl)+scale_y_continuous(limits=as.numeric(t$yr))
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Violin Plot Template ──
make_violin <- function(t, style, lang) {
  C <- spcols(style)
  ng <- max(2L,as.integer(t$xr[2]))
  glb <- if(!is.null(t$xlb)) head(t$xlb,ng) else paste("Group",1:ng)
  set.seed(42)
  df <- data.frame(g=factor(rep(1:ng,each=100),labels=glb),
                   v=rnorm(ng*100,mean=mean(as.numeric(t$yr)),sd=diff(as.numeric(t$yr))/6))
  xl <- if(lang=="none") "" else t$xl
  yl <- if(lang=="none") "" else t$yl
  p <- ggplot(df,aes(x=g,y=v))+geom_violin(fill=C$fl,color=C$ac,alpha=0.3,trim=FALSE)
  if(lang=="none") p <- p+scale_x_discrete(name="",labels=rep("",ng)) else p <- p+labs(x=xl)
  p <- p+labs(y=yl)+scale_y_continuous(limits=as.numeric(t$yr))
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── ECG Grid Template ──
make_ecg <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xmax <- as.numeric(t$xr[2]); ymax <- as.numeric(t$yr[2]); ymin <- as.numeric(t$yr[1])
  # Small grid (1mm = 0.04s x 0.1mV)
  for(x in seq(0,xmax,0.04)) p <- p+geom_vline(xintercept=x,color="#ffcccc",linewidth=0.1)
  for(y in seq(ymin,ymax,0.1)) p <- p+geom_hline(yintercept=y,color="#ffcccc",linewidth=0.1)
  # Large grid (5mm = 0.2s x 0.5mV)
  for(x in seq(0,xmax,0.2)) p <- p+geom_vline(xintercept=x,color="#ff8888",linewidth=0.25)
  for(y in seq(ymin,ymax,0.5)) p <- p+geom_hline(yintercept=y,color="#ff8888",linewidth=0.25)
  xl <- if(lang=="none") "" else "Time (seconds)"
  yl <- if(lang=="none") "" else "Voltage (mV)"
  p <- p+scale_x_continuous(name=xl,limits=c(0,xmax),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(ymin,ymax),expand=c(0,0))
  if(lang!="none"){
    p <- p+annotate("text",x=xmax*0.98,y=ymax*0.95,label="25 mm/s\n10 mm/mV",color="grey50",hjust=1,size=2.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+theme_minimal(base_size=if(style=="presentation")20 else 14)+
    theme(panel.grid=element_blank(),
          plot.title=element_text(hjust=0.5,face="bold",size=if(style=="presentation")24 else 16),
          plot.background=element_rect(fill=if(style=="dark")"#2d2d2d"else"white",color=NA),
          axis.text=element_text(color=if(style=="dark")"grey80"else"grey40"),
          axis.title=element_text(color=if(style=="dark")"grey90"else"grey30"),
          plot.margin=margin(15,15,10,10))
}

# ── Flow-Volume Loop (Spirometry) Template ──
make_flow_volume <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Volume (L)"
  yl <- if(lang=="none") "" else "Flow (L/s)"
  p <- p+scale_x_continuous(name=xl,limits=as.numeric(t$xr),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=as.numeric(t$yr),expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color="grey40",linewidth=0.5)
  p <- p+geom_vline(xintercept=0,color="grey40",linewidth=0.5)
  if(lang!="none"){
    p <- p+annotate("text",x=as.numeric(t$xr[2])*0.7,y=as.numeric(t$yr[2])*0.85,label="Expiration",color="#2563eb",size=3.5)
    p <- p+annotate("text",x=as.numeric(t$xr[2])*0.7,y=as.numeric(t$yr[1])*0.7,label="Inspiration",color="#dc2626",size=3.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Pressure-Volume Loop (Cardiac) Template ──
make_pv_loop <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Volume (mL)"
  yl <- if(lang=="none") "" else "Pressure (mmHg)"
  p <- p+scale_x_continuous(name=xl,limits=as.numeric(t$xr),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=as.numeric(t$yr),expand=c(0,0))
  if(lang!="none"){
    p <- p+annotate("text",x=mean(as.numeric(t$xr)),y=as.numeric(t$yr[2])*0.9,label="Isovolumic\nRelaxation",color="grey50",size=3)
    p <- p+annotate("text",x=as.numeric(t$xr[1])+diff(as.numeric(t$xr))*0.2,y=as.numeric(t$yr[2])*0.5,label="Isovolumic\nContraction",color="grey50",size=3)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Growth Chart (Percentile Curves) Template ──
make_growth <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else t$xl
  yl <- if(lang=="none") "" else t$yl
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  # Percentile curves as gentle arcs
  pcts <- c(3,10,25,50,75,90,97)
  for(i in seq_along(pcts)){
    frac <- (i-1)/(length(pcts)-1)
    ybase <- yr[1]+frac*diff(yr)*0.7
    xs <- seq(xr[1],xr[2],length.out=50)
    ys <- ybase + diff(yr)*0.15*sqrt((xs-xr[1])/diff(xr))
    ys <- pmin(ys,yr[2])
    cdf <- data.frame(x=xs,y=ys)
    p <- p+geom_line(data=cdf,aes(x,y),color=C$gc,linewidth=0.4,linetype=if(pcts[i]==50)"solid"else"dashed")
    if(lang!="none") p <- p+annotate("text",x=xr[2]*0.95,y=tail(ys,1),label=paste0(pcts[i],"%"),size=2.5,color=C$fg,hjust=1)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Bullseye (Cardiac 17-segment) Template ──
make_bullseye <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot()+coord_fixed(xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
  segs <- list(
    list(r1=0,r2=0.25,a1=0,a2=2*pi,n=1),   # Apex (1 segment)
    list(r1=0.25,r2=0.6,n=4),   # Mid (4 segments)
    list(r1=0.6,r2=0.9,n=6),    # Mid (6 segments)
    list(r1=0.9,r2=1.2,n=6)     # Basal (6 segments)
  )
  seg_num <- 1
  seg_labels <- if(!is.null(t$xlb)) t$xlb else as.character(1:17)
  for(ring in segs){
    ns <- ring$n
    for(j in 1:ns){
      if(ns==1){a1<-0;a2<-2*pi}else{a1<-(j-1)*2*pi/ns;a2<-j*2*pi/ns}
      th <- seq(a1,a2,length.out=30)
      if(ring$r1==0){
        arc <- data.frame(x=c(0,ring$r2*cos(th),0),y=c(0,ring$r2*sin(th),0))
      } else {
        arc <- data.frame(
          x=c(ring$r1*cos(th),rev(ring$r2*cos(th))),
          y=c(ring$r1*sin(th),rev(ring$r2*sin(th)))
        )
      }
      p <- p+geom_polygon(data=arc,aes(x,y),fill=C$fl,color=C$ac,linewidth=0.3)
      if(lang!="none"&&seg_num<=length(seg_labels)){
        rmid <- (ring$r1+ring$r2)/2
        amid <- (a1+a2)/2
        p <- p+annotate("text",x=rmid*cos(amid),y=rmid*sin(amid),label=seg_labels[seg_num],size=2.5,color=C$fg)
      }
      seg_num <- seg_num+1
    }
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── Swimmer Plot Template ──
make_swimmer <- function(t, style, lang) {
  C <- spcols(style)
  n <- max(3L,as.integer(t$yr[2]))
  lbls <- if(!is.null(t$ylb)) head(t$ylb,n) else paste("Patient",1:n)
  df <- data.frame(y=1:n,xend=seq(from=as.numeric(t$xr[2])*0.3,to=as.numeric(t$xr[2])*0.9,length.out=n))
  p <- ggplot(df,aes(y=y))
  for(i in 1:n) p <- p+annotate("segment",x=0,xend=df$xend[i],y=i,yend=i,color=C$gc,linewidth=3)
  xl <- if(lang=="none") "" else if(nchar(t$xl)>0) t$xl else "Time (months)"
  yl <- if(lang=="none") "" else ""
  p <- p+scale_x_continuous(name=xl,limits=as.numeric(t$xr),expand=c(0.02,0))
  if(lang!="none") {
    p <- p+scale_y_continuous(name=yl,breaks=1:n,labels=lbls,expand=expansion(add=0.5))
  } else {
    p <- p+scale_y_continuous(name=yl,breaks=1:n,labels=rep("",n),expand=expansion(add=0.5))
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.major.y=element_blank())
}

# ── Tornado/Sensitivity Analysis Diagram ──
make_tornado <- function(t, style, lang) {
  C <- spcols(style)
  n <- max(3L,as.integer(t$yr[2]))
  lbls <- if(!is.null(t$ylb)) head(t$ylb,n) else paste("Parameter",1:n)
  vals <- seq(0.3,0.9,length.out=n)*diff(as.numeric(t$xr))/2
  df <- data.frame(y=1:n,low=-rev(vals),high=rev(vals))
  p <- ggplot(df,aes(y=y))+geom_vline(xintercept=0,color="grey40",linewidth=0.5)
  for(i in 1:n){
    p <- p+annotate("rect",xmin=df$low[i],xmax=df$high[i],ymin=i-0.35,ymax=i+0.35,fill=C$fl,color=C$ac,linewidth=0.3)
  }
  xl <- if(lang=="none") "" else if(nchar(t$xl)>0) t$xl else "Impact on Outcome"
  p <- p+scale_x_continuous(name=xl,limits=as.numeric(t$xr))
  if(lang!="none") {
    p <- p+scale_y_continuous(name="",breaks=1:n,labels=lbls,expand=expansion(add=0.5))
  } else {
    p <- p+scale_y_continuous(name="",breaks=1:n,labels=rep("",n),expand=expansion(add=0.5))
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.major.y=element_blank())
}

# ── Population Pyramid Template ──
make_pyramid <- function(t, style, lang) {
  C <- spcols(style)
  ages <- if(!is.null(t$ylb)) t$ylb else c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
  n <- length(ages)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xmax <- as.numeric(t$xr[2])
  for(i in 1:n){
    w <- xmax*0.5*(n-i+1)/n
    p <- p+annotate("rect",xmin=-w,xmax=0,ymin=i-0.4,ymax=i+0.4,fill=if(C$bg=="#2d2d2d")"#4466aa"else"#b3cde3",color=C$ac,linewidth=0.2)
    p <- p+annotate("rect",xmin=0,xmax=w*0.9,ymin=i-0.4,ymax=i+0.4,fill=if(C$bg=="#2d2d2d")"#aa4466"else"#fbb4ae",color=C$ac,linewidth=0.2)
  }
  p <- p+geom_vline(xintercept=0,color="grey40",linewidth=0.5)
  xl <- if(lang=="none") "" else "Population"
  p <- p+scale_x_continuous(name=xl,limits=c(-xmax,xmax))
  if(lang!="none") {
    p <- p+scale_y_continuous(name="Age",breaks=1:n,labels=ages,expand=expansion(add=0.5))
    p <- p+annotate("text",x=-xmax*0.7,y=n+0.5,label="Male",color="#4466aa",size=3.5,fontface="bold")
    p <- p+annotate("text",x=xmax*0.7,y=n+0.5,label="Female",color="#aa4466",size=3.5,fontface="bold")
  } else {
    p <- p+scale_y_continuous(name="",breaks=1:n,labels=rep("",n),expand=expansion(add=0.5))
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.major.y=element_blank())
}

# ── Manhattan Plot Template ──
make_manhattan <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Chromosome"
  yl <- if(lang=="none") "" else expression(-log[10](italic(p)))
  chroms <- 1:22
  chr_pos <- seq(0,1,length.out=22)
  p <- p+scale_x_continuous(name=xl,limits=c(0,1),breaks=chr_pos,
    labels=if(lang=="none") rep("",22) else as.character(chroms))+
    scale_y_continuous(name=if(lang=="none")""else"-log10(p)",limits=c(0,as.numeric(t$yr[2])),expand=c(0,0))
  # Genome-wide significance line
  gwas_line <- -log10(5e-8)
  if(gwas_line <= as.numeric(t$yr[2])){
    p <- p+geom_hline(yintercept=gwas_line,linetype="dashed",color="#dc2626",linewidth=0.4)
    if(lang!="none") p <- p+annotate("text",x=0.02,y=gwas_line+0.3,label="p = 5e-8",color="#dc2626",hjust=0,size=2.5)
  }
  suggestive <- -log10(1e-5)
  p <- p+geom_hline(yintercept=suggestive,linetype="dotted",color="#2563eb",linewidth=0.3)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Volcano Plot Template ──
make_volcano <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else expression(log[2]~"Fold Change")
  yl <- if(lang=="none") "" else expression(-log[10](italic(p)))
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  p <- p+scale_x_continuous(name=if(lang=="none")""else"log2(Fold Change)",limits=xr)+
    scale_y_continuous(name=if(lang=="none")""else"-log10(p)",limits=yr,expand=c(0,0))
  p <- p+geom_vline(xintercept=c(-1,1),linetype="dashed",color="#2563eb",linewidth=0.3)
  p <- p+geom_hline(yintercept=1.3,linetype="dashed",color="#dc2626",linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=xr[1]*0.7,y=yr[2]*0.9,label="Down-\nregulated",color="#2563eb",size=3)
    p <- p+annotate("text",x=xr[2]*0.7,y=yr[2]*0.9,label="Up-\nregulated",color="#dc2626",size=3)
    p <- p+annotate("text",x=xr[2]*0.9,y=1.5,label="p=0.05",color="#dc2626",hjust=1,size=2.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Dot Plot / Strip Chart Template ──
make_dot <- function(t, style, lang) {
  C <- spcols(style)
  ng <- max(2L,as.integer(t$xr[2]))
  glb <- if(!is.null(t$xlb)) head(t$xlb,ng) else paste("Group",1:ng)
  xl <- if(lang=="none") "" else t$xl
  yl <- if(lang=="none") "" else t$yl
  p <- ggplot(data.frame(x=factor(glb,levels=glb),y=0),aes(x,y))+geom_blank()
  if(lang=="none") p <- p+scale_x_discrete(name="",labels=rep("",ng)) else p <- p+labs(x=xl)
  p <- p+labs(y=yl)+scale_y_continuous(limits=as.numeric(t$yr))
  for(i in 1:ng) p <- p+annotate("segment",x=i-0.3,xend=i+0.3,y=mean(as.numeric(t$yr)),yend=mean(as.numeric(t$yr)),color=C$gc,linewidth=0.5)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Nomogram Template ──
make_nomogram <- function(t, style, lang) {
  C <- spcols(style)
  n <- max(3L,as.integer(t$yr[2]))
  vars <- if(!is.null(t$ylb)) head(t$ylb,n) else paste("Variable",1:n)
  p <- ggplot()+coord_cartesian(xlim=c(-0.5,10.5),ylim=c(-0.5,n+1.5))
  # Points scale at top
  if(lang!="none"){
    p <- p+annotate("text",x=-0.3,y=n+1,label="Points",size=3,color=C$fg,hjust=1)
    for(x in seq(0,10,2)) p <- p+annotate("text",x=x,y=n+1,label=x*10,size=2.5,color=C$fg)
  }
  p <- p+annotate("segment",x=0,xend=10,y=n+0.7,yend=n+0.7,color=C$ac,linewidth=0.5)
  for(x in seq(0,10,1)) p <- p+annotate("segment",x=x,xend=x,y=n+0.6,yend=n+0.8,color=C$ac,linewidth=0.3)
  # Variable scales
  for(i in 1:n){
    p <- p+annotate("segment",x=0,xend=10,y=i,yend=i,color=C$gc,linewidth=0.4)
    for(x in seq(0,10,2)) p <- p+annotate("segment",x=x,xend=x,y=i-0.1,yend=i+0.1,color=C$ac,linewidth=0.3)
    if(lang!="none") p <- p+annotate("text",x=-0.3,y=i,label=vars[i],size=3,color=C$fg,hjust=1)
  }
  # Total points / probability at bottom
  p <- p+annotate("segment",x=0,xend=10,y=0,yend=0,color=C$ac,linewidth=0.5)
  if(lang!="none"){
    p <- p+annotate("text",x=-0.3,y=0,label="Risk",size=3,color=C$fg,hjust=1)
    for(x in c(0,2,4,6,8,10)) p <- p+annotate("text",x=x,y=-0.3,label=paste0(x*10,"%"),size=2.5,color=C$fg)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── Epidemic Curve Template ──
make_epicurve <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else if(nchar(t$xl)>0) t$xl else "Date of Onset"
  yl <- if(lang=="none") "" else if(nchar(t$yl)>0) t$yl else "Number of Cases"
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  # Create empty bar outlines
  nbars <- min(20L, as.integer(diff(xr)))
  bw <- diff(xr)/nbars
  for(i in 1:nbars){
    p <- p+annotate("rect",xmin=xr[1]+(i-1)*bw,xmax=xr[1]+i*bw-bw*0.05,
                     ymin=0,ymax=yr[2]*0.02,fill=C$fl,color=C$gc,linewidth=0.2)
  }
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0.02,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── 2x2 Contingency Table Template ──
make_table2x2 <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot()+coord_fixed(xlim=c(-0.8,2.8),ylim=c(-0.8,2.8))
  # Grid
  for(x in c(0,1,2)) p <- p+annotate("segment",x=x,xend=x,y=0,yend=2,color=C$ac,linewidth=0.5)
  for(y in c(0,1,2)) p <- p+annotate("segment",x=0,xend=2,y=y,yend=y,color=C$ac,linewidth=0.5)
  # Fill cells
  cells <- list(c(0.5,1.5,"a"),c(1.5,1.5,"b"),c(0.5,0.5,"c"),c(1.5,0.5,"d"))
  for(cl in cells){
    p <- p+annotate("rect",xmin=as.numeric(cl[1])-0.48,xmax=as.numeric(cl[1])+0.48,
                     ymin=as.numeric(cl[2])-0.48,ymax=as.numeric(cl[2])+0.48,fill=C$fl,alpha=0.3)
    p <- p+annotate("text",x=as.numeric(cl[1]),y=as.numeric(cl[2]),label=cl[3],size=6,color=C$fg,fontface="bold")
  }
  if(lang!="none"){
    xlb <- if(!is.null(t$xlb)) t$xlb else c("Disease +","Disease -")
    ylb <- if(!is.null(t$ylb)) t$ylb else c("Exposure +","Exposure -")
    p <- p+annotate("text",x=0.5,y=2.4,label=xlb[1],size=3.5,color=C$tc,fontface="bold")
    p <- p+annotate("text",x=1.5,y=2.4,label=xlb[2],size=3.5,color=C$tc,fontface="bold")
    p <- p+annotate("text",x=-0.4,y=1.5,label=ylb[1],size=3.5,color=C$tc,fontface="bold",angle=90)
    p <- p+annotate("text",x=-0.4,y=0.5,label=ylb[2],size=3.5,color=C$tc,fontface="bold",angle=90)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── Pedigree Chart Template ──
make_pedigree <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot()+coord_fixed(xlim=c(-3,3),ylim=c(-3,3))
  # Generation I: parents
  # Male = square, Female = circle
  sq <- function(cx,cy,sz=0.3){
    data.frame(x=cx+c(-sz,sz,sz,-sz,-sz),y=cy+c(-sz,-sz,sz,sz,-sz))
  }
  ci <- function(cx,cy,r=0.3){
    th <- seq(0,2*pi,length.out=50)
    data.frame(x=cx+r*cos(th),y=cy+r*sin(th))
  }
  # Gen I
  p <- p+geom_polygon(data=sq(-1,2),aes(x,y),fill="white",color=C$tc,linewidth=0.5)
  p <- p+geom_polygon(data=ci(1,2),aes(x,y),fill="white",color=C$tc,linewidth=0.5)
  p <- p+annotate("segment",x=-0.7,xend=0.7,y=2,yend=2,color=C$tc,linewidth=0.5)
  # Gen II
  p <- p+annotate("segment",x=0,xend=0,y=2,yend=1,color=C$tc,linewidth=0.4)
  p <- p+annotate("segment",x=-1.5,xend=1.5,y=1,yend=1,color=C$tc,linewidth=0.4)
  p <- p+geom_polygon(data=sq(-1.5,0),aes(x,y),fill="white",color=C$tc,linewidth=0.5)
  p <- p+geom_polygon(data=ci(0,0),aes(x,y),fill="white",color=C$tc,linewidth=0.5)
  p <- p+geom_polygon(data=sq(1.5,0),aes(x,y),fill="white",color=C$tc,linewidth=0.5)
  for(xx in c(-1.5,0,1.5)) p <- p+annotate("segment",x=xx,xend=xx,y=1,yend=0.3,color=C$tc,linewidth=0.4)
  # Gen III
  p <- p+annotate("segment",x=-1.5,xend=-0.7,y=0,yend=0,color=C$tc,linewidth=0.5)
  p <- p+annotate("segment",x=-1.1,xend=-1.1,y=0,yend=-1,color=C$tc,linewidth=0.4)
  p <- p+annotate("segment",x=-2,xend=-0.2,y=-1,yend=-1,color=C$tc,linewidth=0.4)
  p <- p+geom_polygon(data=ci(-2,-2),aes(x,y),fill="white",color=C$tc,linewidth=0.5)
  p <- p+geom_polygon(data=sq(-0.2,-2),aes(x,y),fill="white",color=C$tc,linewidth=0.5)
  for(xx in c(-2,-0.2)) p <- p+annotate("segment",x=xx,xend=xx,y=-1,yend=-1.7,color=C$tc,linewidth=0.4)
  if(lang!="none"){
    p <- p+annotate("text",x=-2.8,y=2,label="I",size=4,color=C$fg,fontface="bold")
    p <- p+annotate("text",x=-2.8,y=0,label="II",size=4,color=C$fg,fontface="bold")
    p <- p+annotate("text",x=-2.8,y=-2,label="III",size=4,color=C$fg,fontface="bold")
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── Calibration Plot Template ──
make_calibration <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Predicted Probability"
  yl <- if(lang=="none") "" else "Observed Probability"
  p <- p+scale_x_continuous(name=xl,limits=c(0,1),breaks=seq(0,1,.2),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,1),breaks=seq(0,1,.2),expand=c(0,0))
  p <- p+geom_abline(slope=1,intercept=0,linetype="dashed",color="grey50",linewidth=0.4)
  if(lang!="none") p <- p+annotate("text",x=0.75,y=0.65,label="Perfect\nCalibration",color="grey50",size=3,angle=40)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+coord_fixed()
}

# ── Scatter with Regression Template ──
make_scatter_reg <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else t$xl
  yl <- if(lang=="none") "" else t$yl
  p <- p+scale_x_continuous(name=xl,limits=as.numeric(t$xr),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=as.numeric(t$yr),expand=c(0,0))
  # Diagonal regression line suggestion
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  p <- p+annotate("segment",x=xr[1]+diff(xr)*0.1,xend=xr[2]-diff(xr)*0.1,
                   y=yr[1]+diff(yr)*0.2,yend=yr[2]-diff(yr)*0.1,
                   linetype="dashed",color="grey60",linewidth=0.4)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Hypnogram (Sleep Staging) Template ──
make_hypnogram <- function(t, style, lang) {
  C <- spcols(style)
  stages <- if(lang=="none") rep("",5) else c("Wake","REM","N1","N2","N3")
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- c(0.5,5.5)
  for(i in 1:5) p <- p+geom_hline(yintercept=i,color=C$gc,linewidth=0.2)
  xl <- if(lang=="none") "" else "Time (hours)"
  p <- p+scale_x_continuous(name=xl,limits=xr,breaks=seq(xr[1],xr[2],1),expand=c(0,0))+
    scale_y_continuous(name="",limits=yr,breaks=1:5,labels=rev(stages),expand=c(0,0))
  if(lang!="none"){
    cols <- c("#e74c3c","#3498db","#2ecc71","#9b59b6","#1abc9c")
    for(i in 1:5) p <- p+annotate("rect",xmin=xr[1],xmax=xr[1]+diff(xr)*0.01,ymin=i-0.4,ymax=i+0.4,fill=cols[i],alpha=0.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Spirogram (Time-Volume) Template ──
make_spirogram <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Time (seconds)"
  yl <- if(lang=="none") "" else "Volume (L)"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  p <- p+geom_vline(xintercept=1,linetype="dashed",color="#e74c3c",linewidth=0.4)
  if(lang!="none"){
    p <- p+annotate("text",x=1.1,y=yr[2]*0.95,label="1 sec",color="#e74c3c",hjust=0,size=3)
    p <- p+annotate("text",x=xr[2]*0.7,y=yr[2]*0.85,label="FVC",color="#2563eb",size=3.5)
    p <- p+annotate("text",x=0.5,y=yr[2]*0.6,label="FEV1",color="#e74c3c",size=3.5)
    p <- p+annotate("segment",x=0,xend=1,y=yr[2]*0.55,yend=yr[2]*0.55,
                    arrow=arrow(length=unit(0.1,"inches"),ends="both"),color="grey50",linewidth=0.3)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Davenport Diagram (Acid-Base) Template ──
make_davenport <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "pH"
  yl <- if(lang=="none") "" else expression(HCO[3]^{textstyle("-")}~(mEq/L))
  p <- p+scale_x_continuous(name=xl,limits=c(7.0,7.8),breaks=seq(7.0,7.8,0.1),expand=c(0,0))+
    scale_y_continuous(name=if(lang=="none") "" else "HCO3- (mEq/L)",limits=c(0,45),expand=c(0,0))
  p <- p+geom_vline(xintercept=7.4,linetype="dashed",color="grey50",linewidth=0.3)
  p <- p+geom_hline(yintercept=24,linetype="dashed",color="grey50",linewidth=0.3)
  pco2_vals <- c(20,40,60,80)
  pco2_cols <- c("#3498db","#2c3e50","#e74c3c","#c0392b")
  for(k in seq_along(pco2_vals)){
    pco2 <- pco2_vals[k]
    ph_seq <- seq(7.0,7.8,0.01)
    hco3 <- 0.03*pco2*10^(ph_seq-6.1)
    df <- data.frame(x=ph_seq,y=hco3)
    df <- df[df$y<=45 & df$y>=0,]
    p <- p+geom_line(data=df,aes(x,y),color=pco2_cols[k],linewidth=0.5,alpha=0.6)
    if(lang!="none" && nrow(df)>0){
      lbl_row <- df[which.min(abs(df$y-40)),]
      if(nrow(lbl_row)>0) p <- p+annotate("text",x=lbl_row$x[1],y=min(42,lbl_row$y[1]+1.5),
        label=paste0("PCO2=",pco2),color=pco2_cols[k],size=2.5)
    }
  }
  if(lang!="none"){
    p <- p+annotate("text",x=7.15,y=35,label="Metabolic\nAlkalosis",color="grey50",size=2.8)
    p <- p+annotate("text",x=7.15,y=10,label="Respiratory\nAcidosis",color="grey50",size=2.8)
    p <- p+annotate("text",x=7.65,y=35,label="Respiratory\nAlkalosis",color="grey50",size=2.8)
    p <- p+annotate("text",x=7.65,y=10,label="Metabolic\nAcidosis",color="grey50",size=2.8)
    p <- p+annotate("point",x=7.4,y=24,color="#e74c3c",size=3)
    p <- p+annotate("text",x=7.42,y=25.5,label="Normal",color="#e74c3c",size=2.8)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── VCG Triaxial (Hexaxial Reference) Template ──
make_vcg_triaxial <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot()+coord_fixed(xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
  angles_deg <- c(0,-30,-60,-90,-120,-150,180,150,120,90,60,30)
  angles_rad <- angles_deg*pi/180
  for(a in angles_rad) p <- p+annotate("segment",x=-1.2*cos(a),xend=1.2*cos(a),
    y=-1.2*sin(a),yend=1.2*sin(a),color=C$gc,linewidth=0.3)
  p <- p+annotate("path",x=cos(seq(0,2*pi,length.out=100)),y=sin(seq(0,2*pi,length.out=100)),
    color=C$ac,linewidth=0.3)
  p <- p+annotate("path",x=0.5*cos(seq(0,2*pi,length.out=100)),y=0.5*sin(seq(0,2*pi,length.out=100)),
    color=C$gc,linewidth=0.2,linetype="dotted")
  if(lang!="none"){
    leads <- data.frame(
      label=c("I (0°)","II (60°)","III (120°)","aVR (-150°)","aVL (-30°)","aVF (90°)"),
      angle=c(0,60,120,-150,-30,90), stringsAsFactors=FALSE)
    for(i in 1:nrow(leads)){
      a <- -leads$angle[i]*pi/180
      p <- p+annotate("text",x=1.35*cos(a),y=1.35*sin(a),label=leads$label[i],
        color=C$fg,size=2.5,fontface="bold")
    }
    deg_marks <- seq(-180,170,30)
    for(d in deg_marks){
      a <- -d*pi/180
      p <- p+annotate("text",x=1.1*cos(a),y=1.1*sin(a),label=paste0(ifelse(d>0,"+",""),d,"°"),
        color="grey60",size=1.8)
    }
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── DVH (Dose-Volume Histogram) Template ──
make_dvh <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- c(0,100)
  xl <- if(lang=="none") "" else "Dose (Gy)"
  yl <- if(lang=="none") "" else "Volume (%)"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  if(lang!="none"){
    organs <- if(!is.null(t$xlb)) t$xlb else c("PTV","Lung","Heart","Spinal Cord","Esophagus")
    cols <- c("#e74c3c","#3498db","#2ecc71","#f39c12","#9b59b6")
    for(i in seq_along(organs)){
      x_off <- xr[2]*(0.6+0.08*i)
      p <- p+annotate("segment",x=x_off-2,xend=x_off,y=95-5*i,yend=95-5*i,color=cols[min(i,5)],linewidth=1)
      p <- p+annotate("text",x=x_off+1,y=95-5*i,label=organs[i],color=cols[min(i,5)],hjust=0,size=2.5)
    }
  }
  p <- p+geom_hline(yintercept=c(95,50,5),linetype="dotted",color="grey70",linewidth=0.2)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── CTG (Cardiotocograph) Template ──
make_ctg <- function(t, style, lang) {
  C <- spcols(style)
  xr <- as.numeric(t$xr)
  p <- ggplot()+coord_cartesian(xlim=xr,ylim=c(-0.5,2.5))
  for(y in seq(60,210,10)){
    yn <- (y-60)/150*1+1.2
    p <- p+annotate("segment",x=xr[1],xend=xr[2],y=yn,yend=yn,
      color=if(y%%30==0) C$gc else adjustcolor(C$gc,alpha.f=0.3),linewidth=if(y%%30==0) 0.3 else 0.15)
  }
  for(y in seq(0,100,10)){
    yn <- y/100*0.8+0.1
    p <- p+annotate("segment",x=xr[1],xend=xr[2],y=yn,yend=yn,
      color=if(y%%25==0) C$gc else adjustcolor(C$gc,alpha.f=0.3),linewidth=if(y%%25==0) 0.3 else 0.15)
  }
  for(x in seq(xr[1],xr[2],1)){
    p <- p+annotate("segment",x=x,xend=x,y=-0.1,yend=2.5,color=C$gc,linewidth=0.15)
  }
  p <- p+annotate("segment",x=xr[1],xend=xr[2],y=1.1,yend=1.1,color=C$ac,linewidth=0.5)
  base_fhr <- (110-60)/150+1.2
  base_fhr2 <- (160-60)/150+1.2
  p <- p+annotate("segment",x=xr[1],xend=xr[2],y=base_fhr,yend=base_fhr,linetype="dashed",color="#2563eb",linewidth=0.3)
  p <- p+annotate("segment",x=xr[1],xend=xr[2],y=base_fhr2,yend=base_fhr2,linetype="dashed",color="#2563eb",linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=xr[1]+0.2,y=2.45,label="FHR (bpm)",color=C$fg,hjust=0,size=3,fontface="bold")
    p <- p+annotate("text",x=xr[1]+0.2,y=0.95,label="UA (mmHg)",color=C$fg,hjust=0,size=3,fontface="bold")
    for(y in c(60,90,120,150,180,210)){
      yn <- (y-60)/150*1+1.2
      p <- p+annotate("text",x=xr[1]-0.3,y=yn,label=y,color="grey50",size=2)
    }
    for(y in c(0,25,50,75,100)){
      yn <- y/100*0.8+0.1
      p <- p+annotate("text",x=xr[1]-0.3,y=yn,label=y,color="grey50",size=2)
    }
    p <- p+annotate("text",x=xr[2]+0.1,y=base_fhr,label="110",color="#2563eb",size=2)
    p <- p+annotate("text",x=xr[2]+0.1,y=base_fhr2,label="160",color="#2563eb",size=2)
  }
  xl <- if(lang=="none") "" else "Time (min)"
  p <- p+labs(x=xl,y="")
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── Levey-Jennings QC Chart Template ──
make_levey_jennings <- function(t, style, lang) {
  C <- spcols(style)
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  mid <- mean(yr)
  sd1 <- diff(yr)/6
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Run Number"
  yl <- if(lang=="none") "" else "Value"
  p <- p+scale_x_continuous(name=xl,limits=xr,breaks=seq(xr[1],xr[2],2),expand=c(0.02,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  p <- p+geom_hline(yintercept=mid,color=C$ac,linewidth=0.6)
  cols_sd <- c("#2ecc71","#f39c12","#e74c3c")
  for(s in 1:3){
    p <- p+annotate("rect",xmin=xr[1],xmax=xr[2],ymin=mid-s*sd1,ymax=mid-(s-1)*sd1,
      fill=cols_sd[s],alpha=0.06)
    p <- p+annotate("rect",xmin=xr[1],xmax=xr[2],ymin=mid+(s-1)*sd1,ymax=mid+s*sd1,
      fill=cols_sd[s],alpha=0.06)
    p <- p+geom_hline(yintercept=mid+s*sd1,linetype=c("dashed","dotted","solid")[s],color=cols_sd[s],linewidth=0.4)
    p <- p+geom_hline(yintercept=mid-s*sd1,linetype=c("dashed","dotted","solid")[s],color=cols_sd[s],linewidth=0.4)
  }
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2]*0.98,y=mid,label="Mean",hjust=1,color=C$fg,size=2.5)
    for(s in 1:3){
      p <- p+annotate("text",x=xr[2]*0.98,y=mid+s*sd1,label=paste0("+",s,"SD"),hjust=1,color=cols_sd[s],size=2.3)
      p <- p+annotate("text",x=xr[2]*0.98,y=mid-s*sd1,label=paste0("-",s,"SD"),hjust=1,color=cols_sd[s],size=2.3)
    }
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Fagan Nomogram Template ──
make_fagan <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot()+coord_cartesian(xlim=c(-1,11),ylim=c(0,10))
  for(col_x in c(1,5,9)){
    p <- p+annotate("segment",x=col_x,xend=col_x,y=0.5,yend=9.5,color=C$ac,linewidth=0.5)
    for(tick_y in seq(0.5,9.5,0.5)){
      p <- p+annotate("segment",x=col_x-0.15,xend=col_x+0.15,y=tick_y,yend=tick_y,color=C$ac,linewidth=0.3)
    }
  }
  p <- p+annotate("segment",x=1,xend=9,y=5,yend=5,linetype="dotted",color="grey60",linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=1,y=10,label="Pre-test\nProbability",color=C$fg,size=3,fontface="bold")
    p <- p+annotate("text",x=5,y=10,label="Likelihood\nRatio",color=C$fg,size=3,fontface="bold")
    p <- p+annotate("text",x=9,y=10,label="Post-test\nProbability",color=C$fg,size=3,fontface="bold")
    probs <- c(0.1,0.2,0.5,1,2,5,10,20,30,50,70,80,90,95,99)
    for(pr in probs){
      y_pos <- 0.5+9*(log10(pr/(100-pr))+2)/4
      if(y_pos>=0.5 && y_pos<=9.5){
        p <- p+annotate("text",x=0.5,y=y_pos,label=paste0(pr,"%"),color="grey50",size=2,hjust=1)
        p <- p+annotate("text",x=9.5,y=y_pos,label=paste0(pr,"%"),color="grey50",size=2,hjust=0)
      }
    }
    lrs <- c(0.001,0.01,0.1,0.5,1,2,5,10,100,1000)
    for(lr in lrs){
      y_pos <- 0.5+9*(log10(lr)+3)/6
      if(y_pos>=0.5 && y_pos<=9.5)
        p <- p+annotate("text",x=5.5,y=y_pos,label=lr,color="grey50",size=2,hjust=0)
    }
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── FACS 2D (Flow Cytometry Quadrant) Template ──
make_facs_2d <- function(t, style, lang) {
  C <- spcols(style)
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else t$xl
  yl <- if(lang=="none") "" else t$yl
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  if(isTRUE(t$xlog)){
    p <- p+scale_x_log10(name=xl,limits=xr)+scale_y_log10(name=yl,limits=yr)
    xmid <- 10^(mean(log10(xr))); ymid <- 10^(mean(log10(yr)))
  } else {
    p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
      scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
    xmid <- mean(xr); ymid <- mean(yr)
  }
  p <- p+geom_hline(yintercept=ymid,color="#e74c3c",linewidth=0.5,linetype="dashed")
  p <- p+geom_vline(xintercept=xmid,color="#e74c3c",linewidth=0.5,linetype="dashed")
  if(lang!="none"){
    qlabels <- if(!is.null(t$xlb)) t$xlb else c("Q1","Q2","Q3","Q4")
    p <- p+annotate("text",x=xr[1]+diff(xr)*0.15,y=yr[2]-diff(yr)*0.08,label=qlabels[min(2,length(qlabels))],color="grey50",size=3.5)
    p <- p+annotate("text",x=xr[2]-diff(xr)*0.15,y=yr[2]-diff(yr)*0.08,label=qlabels[min(1,length(qlabels))],color="grey50",size=3.5)
    p <- p+annotate("text",x=xr[1]+diff(xr)*0.15,y=yr[1]+diff(yr)*0.08,label=qlabels[min(3,length(qlabels))],color="grey50",size=3.5)
    p <- p+annotate("text",x=xr[2]-diff(xr)*0.15,y=yr[1]+diff(yr)*0.08,label=qlabels[min(4,length(qlabels))],color="grey50",size=3.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Ventilator Waveform (3-panel) Template ──
make_ventilator_wave <- function(t, style, lang) {
  C <- spcols(style)
  xr <- as.numeric(t$xr)
  p <- ggplot()+coord_cartesian(xlim=xr,ylim=c(0,3.3))
  panels <- if(lang!="none") c("Pressure (cmH2O)","Flow (L/min)","Volume (mL)") else rep("",3)
  for(i in 0:2){
    y0 <- 2.2-i*1.1; y1 <- y0+0.9
    p <- p+annotate("rect",xmin=xr[1],xmax=xr[2],ymin=y0,ymax=y1,fill=NA,color=C$gc,linewidth=0.3)
    for(gy in seq(y0,y1,0.18)){
      p <- p+annotate("segment",x=xr[1],xend=xr[2],y=gy,yend=gy,color=C$gc,linewidth=0.1,alpha=0.3)
    }
    if(i==1) p <- p+annotate("segment",x=xr[1],xend=xr[2],y=y0+0.45,yend=y0+0.45,
      color="grey50",linewidth=0.3,linetype="dashed")
    if(lang!="none") p <- p+annotate("text",x=xr[1]+0.1,y=y1-0.05,label=panels[i+1],
      color=C$fg,size=2.5,hjust=0,vjust=1,fontface="bold")
  }
  for(x in seq(xr[1],xr[2],1)){
    p <- p+annotate("segment",x=x,xend=x,y=0,yend=3.3,color=C$gc,linewidth=0.1,alpha=0.3)
  }
  xl <- if(lang=="none") "" else "Time (seconds)"
  p <- p+labs(x=xl,y="")
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── Guyton Cardiac Output Diagram Template ──
make_guyton <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Right Atrial Pressure (mmHg)"
  yl <- if(lang=="none") "" else "Cardiac Output / Venous Return (L/min)"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  p <- p+geom_vline(xintercept=0,color="grey40",linewidth=0.4)
  p <- p+geom_hline(yintercept=0,color="grey40",linewidth=0.4)
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2]*0.7,y=yr[2]*0.85,label="Cardiac\nFunction",color="#2563eb",size=3.5,fontface="italic")
    p <- p+annotate("text",x=xr[1]*0.3,y=yr[2]*0.5,label="Venous\nReturn",color="#e74c3c",size=3.5,fontface="italic")
    p <- p+annotate("point",x=2,y=yr[2]*0.45,color="#2c3e50",size=3)
    p <- p+annotate("text",x=2.5,y=yr[2]*0.45,label="Operating\nPoint",color="#2c3e50",size=2.8,hjust=0)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)
}

# ── Lung Volumes Diagram Template ──
make_lung_volumes <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot()+coord_cartesian(xlim=c(0,10),ylim=c(0,7))
  vols <- list(
    list(name="RV",  y0=0.5, y1=2.0, col="#9b59b6"),
    list(name="ERV", y0=2.0, y1=3.2, col="#3498db"),
    list(name="TV",  y0=3.2, y1=3.9, col="#2ecc71"),
    list(name="IRV", y0=3.9, y1=6.2, col="#e67e22"))
  for(v in vols){
    p <- p+annotate("rect",xmin=1,xmax=5,ymin=v$y0,ymax=v$y1,fill=v$col,alpha=0.2,color=v$col,linewidth=0.5)
    if(lang!="none") p <- p+annotate("text",x=3,y=(v$y0+v$y1)/2,label=v$name,color=v$col,size=4,fontface="bold")
  }
  if(lang!="none"){
    braces <- list(
      list(name="TLC", y0=0.5, y1=6.2, x=6),
      list(name="VC",  y0=2.0, y1=6.2, x=7),
      list(name="IC",  y0=3.2, y1=6.2, x=8),
      list(name="FRC", y0=0.5, y1=3.2, x=9))
    for(b in braces){
      p <- p+annotate("segment",x=b$x,xend=b$x,y=b$y0,yend=b$y1,color=C$fg,linewidth=0.5)
      p <- p+annotate("segment",x=b$x-0.15,xend=b$x,y=b$y0,yend=b$y0,color=C$fg,linewidth=0.5)
      p <- p+annotate("segment",x=b$x-0.15,xend=b$x,y=b$y1,yend=b$y1,color=C$fg,linewidth=0.5)
      p <- p+annotate("text",x=b$x+0.3,y=(b$y0+b$y1)/2,label=b$name,color=C$fg,size=3.5,fontface="bold")
    }
  }
  p <- p+labs(x="",y="")
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── BBT (Basal Body Temperature) Chart Template ──
make_bbt <- function(t, style, lang) {
  C <- spcols(style)
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Cycle Day"
  yl <- if(lang=="none") "" else "Temperature (°C)"
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name=xl,limits=xr,breaks=seq(xr[1],xr[2],1),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,breaks=seq(yr[1],yr[2],0.1),expand=c(0,0))
  cover_line <- mean(yr)
  p <- p+geom_hline(yintercept=cover_line,color="#e74c3c",linewidth=0.4,linetype="dashed")
  ov_day <- 14
  p <- p+geom_vline(xintercept=ov_day,color="#2563eb",linewidth=0.4,linetype="dotted")
  if(lang!="none"){
    p <- p+annotate("text",x=ov_day+0.3,y=yr[2]-0.02,label="Ovulation",color="#2563eb",hjust=0,size=2.8)
    p <- p+annotate("text",x=7,y=yr[1]+0.02,label="Follicular Phase",color="#3498db",size=3)
    p <- p+annotate("text",x=21,y=yr[1]+0.02,label="Luteal Phase",color="#e74c3c",size=3)
    p <- p+annotate("text",x=xr[2]-0.5,y=cover_line+0.02,label="Coverline",color="#e74c3c",hjust=1,size=2.5)
  }
  for(y in seq(yr[1],yr[2],0.1)){
    p <- p+geom_hline(yintercept=y,color=C$gc,linewidth=0.1)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── SROC (Summary ROC) Template ──
make_sroc <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "1 - Specificity (FPR)"
  yl <- if(lang=="none") "" else "Sensitivity (TPR)"
  p <- p+scale_x_continuous(name=xl,limits=c(0,1),breaks=seq(0,1,0.2),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,1),breaks=seq(0,1,0.2),expand=c(0,0))
  p <- p+geom_abline(slope=1,intercept=0,linetype="dotted",color="grey60",linewidth=0.3)
  theta <- seq(0,2*pi,length.out=100)
  cx <- 0.25; cy <- 0.82; rx <- 0.12; ry <- 0.08
  p <- p+annotate("polygon",x=cx+rx*cos(theta),y=cy+ry*sin(theta),fill="#2563eb",alpha=0.15,color="#2563eb",linewidth=0.3)
  cx2 <- 0.25; cy2 <- 0.82; rx2 <- 0.06; ry2 <- 0.04
  p <- p+annotate("polygon",x=cx2+rx2*cos(theta),y=cy2+ry2*sin(theta),fill="#2563eb",alpha=0.2,color="#2563eb",linewidth=0.3,linetype="dashed")
  p <- p+annotate("point",x=cx,y=cy,color="#e74c3c",size=3)
  if(lang!="none"){
    p <- p+annotate("text",x=cx+0.15,y=cy,label="Summary\nPoint",color="#e74c3c",size=2.8,hjust=0)
    p <- p+annotate("text",x=0.65,y=0.55,label="95% Confidence\nRegion",color="#2563eb",size=2.5)
    p <- p+annotate("text",x=0.65,y=0.35,label="95% Prediction\nRegion",color="#2563eb",size=2.5,fontface="italic")
    sroc_x <- seq(0.01,0.99,0.01)
    sroc_y <- 1/(1+exp(-3+4*log(sroc_x/(1-sroc_x+0.01)+0.01)))
    sroc_y <- pmin(pmax(sroc_y,0),1)
    p <- p+annotate("line",x=sroc_x,y=sroc_y,color="#2563eb",linewidth=0.5,alpha=0.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+coord_fixed()
}

# ── CONSORT Flowchart Template ──
make_consort <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot()+coord_cartesian(xlim=c(0,10),ylim=c(0,10))
  box <- function(x,y,w,h,label,col="#2563eb"){
    p <<- p+annotate("rect",xmin=x-w/2,xmax=x+w/2,ymin=y-h/2,ymax=y+h/2,
      fill=NA,color=col,linewidth=0.5)
    if(lang!="none") p <<- p+annotate("text",x=x,y=y,label=label,color=C$fg,size=2.5)
  }
  arrow_down <- function(x,y1,y2){
    p <<- p+annotate("segment",x=x,xend=x,y=y1,yend=y2,color=C$ac,linewidth=0.4,
      arrow=arrow(length=unit(0.08,"inches")))
  }
  arrow_right <- function(x1,x2,y){
    p <<- p+annotate("segment",x=x1,xend=x2,y=y,yend=y,color=C$ac,linewidth=0.4,
      arrow=arrow(length=unit(0.08,"inches")))
  }
  box(5,9.2,4,0.8,"Assessed for eligibility\n(n =   )")
  arrow_down(5,8.8,8.0)
  arrow_right(5,8.5,8.4); box(8.5,8.4,2.5,0.6,"Excluded (n =   )")
  box(5,7.6,4,0.8,"Randomized\n(n =   )")
  arrow_down(3,7.2,6.4)
  arrow_down(7,7.2,6.4)
  box(3,6.0,3.2,0.8,"Allocated to\nIntervention (n =   )")
  box(7,6.0,3.2,0.8,"Allocated to\nControl (n =   )")
  arrow_down(3,5.6,4.8)
  arrow_down(7,5.6,4.8)
  box(3,4.4,3.2,0.8,"Follow-up\n(n =   )")
  box(7,4.4,3.2,0.8,"Follow-up\n(n =   )")
  arrow_down(3,4.0,3.2)
  arrow_down(7,4.0,3.2)
  box(3,2.8,3.2,0.8,"Analyzed\n(n =   )")
  box(7,2.8,3.2,0.8,"Analyzed\n(n =   )")
  if(lang!="none"){
    phases <- c("Enrollment","Allocation","Follow-Up","Analysis")
    ys <- c(9.2,7.0,4.4,2.8)
    for(i in seq_along(phases))
      p <- p+annotate("text",x=0.3,y=ys[i],label=phases[i],color="grey50",size=2.5,angle=90,fontface="bold")
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── VCG Loop (3-plane vector) ──
make_vcg <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  lim <- 1.5
  p <- p+geom_hline(yintercept=0,color=C$gc,linewidth=0.3)+geom_vline(xintercept=0,color=C$gc,linewidth=0.3)
  for(r in seq(0.5,1.5,0.5)) p <- p+annotate("path",x=r*cos(seq(0,2*pi,len=100)),y=r*sin(seq(0,2*pi,len=100)),color=C$gc,linewidth=0.15,linetype="dotted")
  if(lang!="none"){
    p <- p+annotate("text",x=lim*0.9,y=0.1,label="Frontal",color="grey50",size=2.5)
  }
  xl <- if(lang=="none") "" else "X (mV)"; yl <- if(lang=="none") "" else "Y (mV)"
  p <- p+scale_x_continuous(name=xl,limits=c(-lim,lim),expand=c(0,0))+scale_y_continuous(name=yl,limits=c(-lim,lim),expand=c(0,0))
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank(),aspect.ratio=1)
}

# ── M-mode Echo ──
make_mmode <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  for(d in seq(yr[1],yr[2],length.out=8)) p <- p+geom_hline(yintercept=d,color=C$gc,linewidth=0.15,linetype="dotted")
  xl <- if(lang=="none") "" else "Time (sec)"; yl <- if(lang=="none") "" else "Depth (cm)"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+scale_y_continuous(name=yl,limits=yr,expand=c(0,0),trans="reverse")
  if(lang!="none"){
    structs <- c("RV","IVS","LV","PW")
    ds <- seq(yr[1]+diff(yr)*0.15,yr[2]-diff(yr)*0.1,length.out=4)
    for(i in 1:4) p <- p+annotate("text",x=xr[1]+diff(xr)*0.02,y=ds[i],label=structs[i],color="grey50",size=2,hjust=0)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Swan-Ganz Catheter Waveform ──
make_swan_ganz <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Time"; yl <- if(lang=="none") "" else "Pressure (mmHg)"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  if(lang!="none"){
    zones <- c("RA","RV","PA","PCWP")
    xs <- xr[1]+diff(xr)*c(0.1,0.3,0.55,0.8)
    for(i in 1:4) p <- p+annotate("text",x=xs[i],y=yr[2]*0.95,label=zones[i],color="#3498db",size=3,fontface="bold")
    p <- p+geom_vline(xintercept=xr[1]+diff(xr)*c(0.2,0.45,0.7),linetype="dashed",color=C$gc,linewidth=0.3)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── V/Q Ratio Distribution ──
make_va_q <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "VA/Q Ratio"; yl <- if(lang=="none") "" else "Blood Flow / Ventilation"
  p <- p+scale_x_continuous(name=xl,limits=c(0,3),breaks=seq(0,3,0.5),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,1.5),expand=c(0,0))
  p <- p+geom_vline(xintercept=1.0,linetype="dashed",color="grey50",linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=0.3,y=1.3,label="Shunt",color="#e74c3c",size=2.5)+
      annotate("text",x=2.5,y=1.3,label="Dead Space",color="#3498db",size=2.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Acid-Base Map (4-quadrant) ──
make_acid_base_map <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name=if(lang=="none") "" else "PaCO2 (mmHg)",limits=c(10,80),expand=c(0,0))+
    scale_y_continuous(name=if(lang=="none") "" else "HCO3- (mEq/L)",limits=c(5,45),expand=c(0,0))
  p <- p+geom_hline(yintercept=24,linetype="dashed",color="grey50",linewidth=0.3)+
    geom_vline(xintercept=40,linetype="dashed",color="grey50",linewidth=0.3)
  if(lang!="none"){
    labs_txt <- c("Resp. Alkalosis\nMetab. Alkalosis","Metab.\nAlkalosis","Resp. Acidosis\nMetab. Acidosis","Metab.\nAcidosis")
    xs <- c(20,20,60,60); ys <- c(38,12,38,12)
    for(i in 1:4) p <- p+annotate("text",x=xs[i],y=ys[i],label=labs_txt[i],color="grey50",size=2,lineheight=0.9)
    p <- p+annotate("point",x=40,y=24,color="#e74c3c",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Insulin-Glucose Clamp ──
make_clamp <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr)
  p <- p+scale_x_continuous(name=if(lang=="none") "" else "Time (min)",limits=xr,expand=c(0,0))+
    scale_y_continuous(name="",limits=c(0,2),expand=c(0,0),breaks=NULL)
  p <- p+geom_hline(yintercept=1,color=C$gc,linewidth=0.5)
  if(lang!="none"){
    p <- p+annotate("text",x=xr[1]+5,y=1.7,label="Glucose (mg/dL)",color="#e74c3c",size=2.5,hjust=0)+
      annotate("text",x=xr[1]+5,y=0.3,label="Insulin Infusion Rate",color="#3498db",size=2.5,hjust=0)+
      annotate("rect",xmin=xr[1],xmax=xr[1]+diff(xr)*0.15,ymin=0.95,ymax=1.05,fill="grey80",alpha=0.3)+
      annotate("text",x=xr[1]+diff(xr)*0.075,y=1.5,label="Baseline",color="grey50",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── HRM (High-Resolution Manometry) ──
make_hrm <- function(t, style, lang) {
  C <- spcols(style)
  nr <- 20; nc <- 30
  df <- expand.grid(x=1:nc,y=1:nr)
  df$z <- 0
  p <- ggplot(df,aes(x,y,fill=z))+geom_tile()+
    scale_fill_gradient2(low="#3498db",mid="white",high="#e74c3c",midpoint=0,guide="none")
  xl <- if(lang=="none") "" else "Time (sec)"; yl <- if(lang=="none") "" else "Sensor Position (cm)"
  p <- p+scale_x_continuous(name=xl,expand=c(0,0))+scale_y_continuous(name=yl,expand=c(0,0),trans="reverse")
  if(lang!="none"){
    p <- p+annotate("text",x=2,y=2,label="UES",color="grey30",size=2.5,hjust=0)+
      annotate("text",x=2,y=18,label="LES",color="grey30",size=2.5,hjust=0)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── CSF/ICP Pressure Waveform ──
make_csf_pressure <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Time (sec)"; yl <- if(lang=="none") "" else "ICP (mmHg)"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  p <- p+geom_hline(yintercept=c(5,15,20),linetype=c("dotted","dashed","dashed"),color=c(C$gc,"#f39c12","#e74c3c"),linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2]*0.95,y=16,label="Upper Normal",color="#f39c12",size=2,hjust=1)+
      annotate("text",x=xr[2]*0.95,y=21,label="Elevated",color="#e74c3c",size=2,hjust=1)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Amsler Grid ──
make_amsler <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  for(i in seq(-5,5,0.5)) p <- p+geom_hline(yintercept=i,color=C$gc,linewidth=0.15)+geom_vline(xintercept=i,color=C$gc,linewidth=0.15)
  p <- p+annotate("point",x=0,y=0,color=C$fg,size=2)
  p <- p+scale_x_continuous(name="",limits=c(-5,5),breaks=seq(-5,5,1),expand=c(0,0))+
    scale_y_continuous(name="",limits=c(-5,5),breaks=seq(-5,5,1),expand=c(0,0))
  if(lang!="none") p <- p+annotate("text",x=0,y=-5.3,label="10° visual field",color="grey50",size=2,vjust=1)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid=element_blank(),aspect.ratio=1)
}

# ── Corneal Topography ──
make_corneal_topo <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  cols <- c("#3498db","#2ecc71","#f1c40f","#e67e22","#e74c3c")
  for(i in 5:1){
    r <- i*0.8
    p <- p+annotate("path",x=r*cos(seq(0,2*pi,len=100)),y=r*sin(seq(0,2*pi,len=100)),color=cols[i],linewidth=0.5)
  }
  p <- p+annotate("point",x=0,y=0,color=C$fg,size=1)
  p <- p+scale_x_continuous(name="",limits=c(-5,5),expand=c(0,0))+scale_y_continuous(name="",limits=c(-5,5),expand=c(0,0))
  if(lang!="none"){
    p <- p+annotate("text",x=4,y=-4.5,label="D (diopters)",color="grey50",size=2)+
      annotate("text",x=0,y=4.5,label="Superior",color="grey50",size=2)+
      annotate("text",x=0,y=-4.5,label="Inferior",color="grey50",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid=element_blank(),aspect.ratio=1)
}

# ── Hess Chart ──
make_hess <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  for(i in -2:2) p <- p+geom_hline(yintercept=i*15,color=C$gc,linewidth=0.15)+geom_vline(xintercept=i*15,color=C$gc,linewidth=0.15)
  dirs <- expand.grid(x=c(-15,0,15),y=c(-15,0,15))
  p <- p+annotate("point",x=dirs$x,y=dirs$y,color="#e74c3c",size=1.5,shape=3)
  p <- p+scale_x_continuous(name="",limits=c(-35,35),expand=c(0,0))+scale_y_continuous(name="",limits=c(-35,35),expand=c(0,0))
  if(lang!="none"){
    p <- p+annotate("text",x=-30,y=33,label="Left Eye",color="#e74c3c",size=2.5)+
      annotate("text",x=30,y=33,label="Right Eye",color="#2ecc71",size=2.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid=element_blank(),aspect.ratio=1)
}

# ── Caloric Test (Butterfly Chart) ──
make_caloric <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "SPV (deg/sec)"; yl <- ""
  p <- p+scale_x_continuous(name=xl,limits=c(-80,80),breaks=seq(-80,80,20),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0.5,4.5),breaks=1:4,labels=if(lang=="none") rep("",4) else c("R-Warm","R-Cool","L-Warm","L-Cool"),expand=c(0.1,0.1))
  p <- p+geom_vline(xintercept=0,color=C$gc,linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=-60,y=4.3,label="Left-beating",color="#3498db",size=2)+
      annotate("text",x=60,y=4.3,label="Right-beating",color="#e74c3c",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Rhinomanometry ──
make_rhinomanometry <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Pressure (Pa)"; yl <- if(lang=="none") "" else "Flow (cm³/s)"
  p <- p+scale_x_continuous(name=xl,limits=c(-300,300),breaks=seq(-300,300,100),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(-800,800),breaks=seq(-800,800,200),expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color=C$gc,linewidth=0.3)+geom_vline(xintercept=0,color=C$gc,linewidth=0.3)
  p <- p+geom_vline(xintercept=c(-150,150),linetype="dashed",color="#e74c3c",linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=200,y=600,label="Insp.",color="grey50",size=2.5)+
      annotate("text",x=-200,y=-600,label="Exp.",color="grey50",size=2.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Pressure-Flow Study (Schäfer Nomogram) ──
make_pfs <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Qmax (mL/s)"; yl <- if(lang=="none") "" else "pdet.Qmax (cmH2O)"
  p <- p+scale_x_continuous(name=xl,limits=c(0,30),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,120),expand=c(0,0))
  if(lang!="none"){
    p <- p+annotate("segment",x=0,xend=15,y=40,yend=40,color="#2ecc71",linewidth=0.5)+
      annotate("segment",x=0,xend=4,y=100,yend=40,color="#e74c3c",linewidth=0.5)+
      annotate("text",x=20,y=20,label="Unobstructed",color="#2ecc71",size=2.5)+
      annotate("text",x=5,y=90,label="Obstructed",color="#e74c3c",size=2.5)+
      annotate("text",x=12,y=55,label="Equivocal",color="#f39c12",size=2.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Gait Analysis ──
make_gait <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Gait Cycle (%)"; yl <- if(lang=="none") "" else "Angle (deg)"
  p <- p+scale_x_continuous(name=xl,limits=c(0,100),breaks=seq(0,100,10),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=as.numeric(t$yr),expand=c(0,0))
  p <- p+geom_vline(xintercept=60,linetype="dashed",color="#e74c3c",linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("rect",xmin=0,xmax=60,ymin=t$yr[1],ymax=t$yr[1]+diff(as.numeric(t$yr))*0.05,fill="#3498db",alpha=0.3)+
      annotate("rect",xmin=60,xmax=100,ymin=t$yr[1],ymax=t$yr[1]+diff(as.numeric(t$yr))*0.05,fill="#2ecc71",alpha=0.3)+
      annotate("text",x=30,y=t$yr[1]+diff(as.numeric(t$yr))*0.1,label="Stance (60%)",color="#3498db",size=2)+
      annotate("text",x=80,y=t$yr[1]+diff(as.numeric(t$yr))*0.1,label="Swing (40%)",color="#2ecc71",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Psych Profile (Subtest Line) ──
make_psych_profile <- function(t, style, lang) {
  C <- spcols(style)
  xlb <- if(!is.null(t$xlb)) t$xlb else paste0("S",1:10)
  n <- length(xlb)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  yl <- if(lang=="none") "" else "Score"
  p <- p+scale_x_continuous(name="",limits=c(0.5,n+0.5),breaks=1:n,labels=if(lang=="none") rep("",n) else xlb,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=as.numeric(t$yr),expand=c(0,0))
  for(v in c(-2,-1,0,1,2)){
    mid <- mean(as.numeric(t$yr))
    rng <- diff(as.numeric(t$yr))/2
    yv <- mid+v*rng/3
    p <- p+geom_hline(yintercept=yv,color=C$gc,linewidth=0.15,linetype=if(v==0) "solid" else "dotted")
  }
  if(lang!="none") p <- p+annotate("text",x=n+0.3,y=mean(as.numeric(t$yr)),label="Mean",color="grey50",size=2,hjust=1)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank(),axis.text.x=element_text(angle=45,hjust=1,size=5))
}

# ── Mood Chart ──
make_mood_chart <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr)
  xl <- if(lang=="none") "" else "Day"; yl <- if(lang=="none") "" else "Mood"
  p <- p+scale_x_continuous(name=xl,limits=xr,breaks=seq(xr[1],xr[2],7),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(-3,3),breaks=-3:3,labels=if(lang=="none") rep("",7) else c("Severe\nDepression","Moderate","Mild","Euthymic","Mild","Moderate","Severe\nMania"),expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color=C$gc,linewidth=0.5)+
    geom_hline(yintercept=c(-2,2),linetype="dashed",color="#f39c12",linewidth=0.3)+
    geom_hline(yintercept=c(-3,3),linetype="dashed",color="#e74c3c",linewidth=0.3)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank(),axis.text.y=element_text(size=4))
}

# ── Vital Signs Timeline ──
make_vital_timeline <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr)
  p <- p+scale_x_continuous(name=if(lang=="none") "" else "Time (hours)",limits=xr,expand=c(0,0))+
    scale_y_continuous(name="",limits=c(0,4),breaks=NULL,expand=c(0,0))
  if(lang!="none"){
    vitals <- c("HR (bpm)","BP (mmHg)","SpO2 (%)","Temp (°C)")
    cols <- c("#e74c3c","#3498db","#2ecc71","#f39c12")
    for(i in 1:4){
      yb <- (i-1); yt <- i
      p <- p+geom_hline(yintercept=yb,color=C$gc,linewidth=0.3)+
        annotate("text",x=xr[1]+diff(xr)*0.02,y=yb+0.5,label=vitals[i],color=cols[i],size=2,hjust=0,fontface="bold")
    }
  }
  p <- p+geom_hline(yintercept=4,color=C$gc,linewidth=0.3)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid=element_blank())
}

# ── Anesthesia Record ──
make_anesthesia_record <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr)
  p <- p+scale_x_continuous(name=if(lang=="none") "" else "Time (min)",limits=xr,breaks=seq(xr[1],xr[2],30),expand=c(0,0))+
    scale_y_continuous(name="",limits=c(0,5),breaks=NULL,expand=c(0,0))
  if(lang!="none"){
    params <- c("HR","BP","SpO2","EtCO2","MAC")
    for(i in 1:5){
      p <- p+geom_hline(yintercept=i-1,color=C$gc,linewidth=0.2)+
        annotate("text",x=xr[1]+2,y=i-0.5,label=params[i],color="grey40",size=2,hjust=0)
    }
    p <- p+geom_hline(yintercept=5,color=C$gc,linewidth=0.2)+
      annotate("text",x=mean(xr),y=4.8,label="Events / Drugs",color="grey50",size=2,fontface="italic")
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid=element_blank())
}

# ── CT Window Table ──
make_ct_window <- function(t, style, lang) {
  C <- spcols(style)
  tissue_names <- c("Air","Fat","Water","Muscle","Blood","Bone","Metal","Calcium")
  tissues <- if(lang=="none") paste0(" ",seq_along(tissue_names)) else tissue_names
  hu_min <- c(-1000,-120,0,30,30,400,1000,100)
  hu_max <- c(-900,-60,0,60,70,2000,4000,300)
  n <- length(tissues)
  df <- data.frame(tissue=factor(tissues,levels=rev(tissues)),ymin=hu_min,ymax=hu_max,x=1)
  p <- ggplot(df,aes(xmin=0.5,xmax=1.5,ymin=ymin,ymax=ymax))+geom_rect(fill="#3498db",alpha=0.3,color="#3498db",linewidth=0.3)
  show_labels <- if(lang=="none") rep("",n) else tissues
  p <- p+scale_y_continuous(name=if(lang=="none") "" else "Hounsfield Units (HU)",limits=c(-1100,2100),expand=c(0,0))+
    scale_x_continuous(name="",limits=c(0,n+0.5),breaks=1:n,labels=show_labels,expand=c(0,0))
  p <- p+coord_flip()
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── CNV Plot ──
make_cnv <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Chromosome"; yl <- if(lang=="none") "" else "Log2 Ratio"
  p <- p+scale_x_continuous(name=xl,limits=c(0,23),breaks=1:22,labels=if(lang=="none") rep("",22) else c(1:22),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(-2,2),expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color=C$gc,linewidth=0.3)+
    geom_hline(yintercept=c(-1,1),linetype="dashed",color="#e74c3c",linewidth=0.2)
  for(i in 1:22) p <- p+geom_vline(xintercept=i+0.5,color=C$gc,linewidth=0.1)
  if(lang!="none"){
    p <- p+annotate("text",x=22,y=1.7,label="Gain",color="#e74c3c",size=2)+
      annotate("text",x=22,y=-1.7,label="Loss",color="#3498db",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank(),axis.text.x=element_text(size=5))
}

# ── Electrophoresis Pattern ──
make_electrophoresis <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  bands <- if(lang=="none") rep("",6) else c("Albumin","Alpha-1","Alpha-2","Beta","Gamma","Origin")
  n <- length(bands)
  p <- p+scale_x_continuous(name=if(lang=="none") "" else "Migration",limits=c(0,7),breaks=1:n,labels=bands,expand=c(0.05,0.05))+
    scale_y_continuous(name=if(lang=="none") "" else "Density",limits=c(0,1),expand=c(0,0))
  for(i in 1:n) p <- p+geom_vline(xintercept=i,color=C$gc,linewidth=0.1,linetype="dotted")
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Isobologram ──
make_isobologram <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Drug A Dose"; yl <- if(lang=="none") "" else "Drug B Dose"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  p <- p+annotate("segment",x=xr[2],xend=0,y=0,yend=yr[2],color="#e74c3c",linewidth=0.5,linetype="dashed")
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2]*0.3,y=yr[2]*0.3,label="Synergy",color="#2ecc71",size=3)+
      annotate("text",x=xr[2]*0.7,y=yr[2]*0.7,label="Antagonism",color="#e74c3c",size=3)+
      annotate("text",x=xr[2]*0.55,y=yr[2]*0.4,label="Additive",color="grey50",size=2.5,angle=-45)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Schild Plot ──
make_schild <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "log[Antagonist]"; yl <- if(lang=="none") "" else "log(DR - 1)"
  p <- p+scale_x_continuous(name=xl,limits=as.numeric(t$xr),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=as.numeric(t$yr),expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color=C$gc,linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=mean(as.numeric(t$xr)),y=as.numeric(t$yr)[2]*0.8,label="Slope = 1\n(competitive)",color="#3498db",size=2.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Henderson-Hasselbalch Diagram ──
make_hh_diagram <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "pH"; yl <- if(lang=="none") "" else "[A-]/[HA]"
  p <- p+scale_x_continuous(name=xl,limits=c(0,14),breaks=0:14,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,100),expand=c(0,0))
  p <- p+geom_hline(yintercept=50,linetype="dashed",color="#e74c3c",linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=3,y=80,label="HA dominant",color="#e74c3c",size=2.5)+
      annotate("text",x=11,y=80,label="A⁻ dominant",color="#3498db",size=2.5)+
      annotate("text",x=7,y=55,label="pKa = pH at 50%",color="grey50",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Galbraith Plot ──
make_galbraith <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "1/SE"; yl <- if(lang=="none") "" else "z-score (ES/SE)"
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color=C$gc,linewidth=0.3)+
    geom_hline(yintercept=c(-2,2),linetype="dashed",color="#e74c3c",linewidth=0.3)
  if(lang!="none") p <- p+annotate("text",x=xr[2]*0.8,y=2.3,label="±2 (95% CI)",color="#e74c3c",size=2)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── L'Abbé Plot ──
make_labbe <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Control Event Rate"; yl <- if(lang=="none") "" else "Treatment Event Rate"
  p <- p+scale_x_continuous(name=xl,limits=c(0,1),expand=c(0,0))+scale_y_continuous(name=yl,limits=c(0,1),expand=c(0,0))
  p <- p+annotate("segment",x=0,xend=1,y=0,yend=1,color=C$gc,linewidth=0.5,linetype="dashed")
  if(lang!="none"){
    p <- p+annotate("text",x=0.7,y=0.3,label="Treatment\nbetter",color="#2ecc71",size=2.5)+
      annotate("text",x=0.3,y=0.7,label="Control\nbetter",color="#e74c3c",size=2.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank(),aspect.ratio=1)
}

# ── Baujat Plot ──
make_baujat <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Contribution to Overall Result"; yl <- if(lang=="none") "" else "Influence on Heterogeneity"
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  mx <- mean(xr); my <- mean(yr)
  p <- p+geom_hline(yintercept=my,linetype="dotted",color=C$gc,linewidth=0.3)+
    geom_vline(xintercept=mx,linetype="dotted",color=C$gc,linewidth=0.3)
  if(lang!="none") p <- p+annotate("text",x=xr[2]*0.8,y=yr[2]*0.8,label="High influence\n& contribution",color="#e74c3c",size=2)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Deeks Funnel Plot ──
make_deeks_funnel <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "1/sqrt(ESS)"; yl <- if(lang=="none") "" else "log Diagnostic Odds Ratio"
  p <- p+scale_x_continuous(name=xl,limits=c(0,0.5),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(-3,6),expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color=C$gc,linewidth=0.3)
  if(lang!="none") p <- p+annotate("text",x=0.4,y=5,label="Asymmetry\ntest line",color="grey50",size=2)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Lexis Diagram ──
make_lexis <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Calendar Year"; yl <- if(lang=="none") "" else "Age"
  p <- p+scale_x_continuous(name=xl,limits=xr,breaks=seq(xr[1],xr[2],5),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,breaks=seq(yr[1],yr[2],10),expand=c(0,0))
  for(y in seq(xr[1]-yr[2],xr[2],5)){
    x0 <- max(xr[1],y); x1 <- min(xr[2],y+yr[2])
    if(x0<x1) p <- p+annotate("segment",x=x0,xend=x1,y=x0-y,yend=x1-y,color=C$gc,linewidth=0.15,linetype="dotted")
  }
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2]-2,y=yr[2]-2,label="Cohort\ndiagonals",color="grey50",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── DAG (Directed Acyclic Graph) ──
make_dag <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name="",limits=c(0,10),expand=c(0,0),breaks=NULL)+
    scale_y_continuous(name="",limits=c(0,10),expand=c(0,0),breaks=NULL)
  if(lang!="none"){
    nodes <- data.frame(x=c(2,8,5,5),y=c(5,5,8,2),label=c("Exposure","Outcome","Confounder","Mediator"))
    for(i in 1:4) p <- p+annotate("rect",xmin=nodes$x[i]-1.3,xmax=nodes$x[i]+1.3,ymin=nodes$y[i]-0.5,ymax=nodes$y[i]+0.5,fill="white",color=C$fg,linewidth=0.3)+
      annotate("text",x=nodes$x[i],y=nodes$y[i],label=nodes$label[i],color=C$fg,size=2.5)
    p <- p+annotate("segment",x=3.3,xend=6.7,y=5,yend=5,arrow=arrow(length=unit(0.1,"in")),color=C$fg,linewidth=0.4)+
      annotate("segment",x=5,xend=3,y=7.5,yend=5.5,arrow=arrow(length=unit(0.1,"in")),color="#e74c3c",linewidth=0.4)+
      annotate("segment",x=5,xend=7,y=7.5,yend=5.5,arrow=arrow(length=unit(0.1,"in")),color="#e74c3c",linewidth=0.4)+
      annotate("segment",x=3.3,xend=5,y=4.7,yend=2.5,arrow=arrow(length=unit(0.1,"in")),color="#3498db",linewidth=0.3)+
      annotate("segment",x=5,xend=6.7,y=2.5,yend=4.7,arrow=arrow(length=unit(0.1,"in")),color="#3498db",linewidth=0.3)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── Vaccine Schedule (Gantt) ──
make_vaccine_schedule <- function(t, style, lang) {
  C <- spcols(style)
  vaccines <- if(lang=="none") rep("",8) else c("HepB","Rotavirus","DTaP","Hib","PCV13","IPV","MMR","Varicella")
  n <- length(vaccines)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name=if(lang=="none") "" else "Age (months)",limits=c(0,72),breaks=c(0,2,4,6,12,15,18,24,48,72),expand=c(0,0))+
    scale_y_continuous(name="",limits=c(0.5,n+0.5),breaks=1:n,labels=rev(vaccines),expand=c(0,0))
  for(i in 1:n) p <- p+geom_hline(yintercept=i-0.5,color=C$gc,linewidth=0.1)
  if(lang!="none"){
    for(i in 1:n){
      y <- n+1-i
      xs <- switch(i,c(0,1,6),c(2,4),c(2,4,6,15),c(2,4,6,12),c(2,4,6,12),c(2,4,6),c(12,48),c(12,48))
      for(x in xs) p <- p+annotate("point",x=x,y=y,color="#3498db",size=1.5)
    }
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank(),axis.text.y=element_text(size=5))
}

# ── Denver Developmental Screening ──
make_denver <- function(t, style, lang) {
  C <- spcols(style)
  domains <- if(lang=="none") rep("",4) else c("Gross Motor","Fine Motor","Language","Personal-Social")
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name=if(lang=="none") "" else "Age (months)",limits=c(0,72),breaks=seq(0,72,6),expand=c(0,0))+
    scale_y_continuous(name="",limits=c(0.5,4.5),breaks=1:4,labels=rev(domains),expand=c(0.05,0.05))
  cols <- c("#e74c3c","#3498db","#2ecc71","#f39c12")
  for(i in 1:4) p <- p+geom_hline(yintercept=i-0.5,color=C$gc,linewidth=0.15)+
    annotate("rect",xmin=0,xmax=72,ymin=i-0.45,ymax=i+0.45,fill=cols[i],alpha=0.05)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Feedback Loop (Hormone Axis) ──
make_feedback_loop <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name="",limits=c(0,10),breaks=NULL,expand=c(0,0))+
    scale_y_continuous(name="",limits=c(0,10),breaks=NULL,expand=c(0,0))
  if(lang!="none"){
    boxes <- data.frame(x=c(5,5,5),y=c(8.5,5.5,2.5),label=c("Hypothalamus","Ant. Pituitary","Target Gland"))
    for(i in 1:3) p <- p+annotate("rect",xmin=boxes$x[i]-2,xmax=boxes$x[i]+2,ymin=boxes$y[i]-0.6,ymax=boxes$y[i]+0.6,fill="white",color=C$fg,linewidth=0.3)+
      annotate("text",x=boxes$x[i],y=boxes$y[i],label=boxes$label[i],color=C$fg,size=2.5)
    p <- p+annotate("segment",x=5,xend=5,y=7.9,yend=6.1,arrow=arrow(length=unit(0.1,"in")),color="#3498db",linewidth=0.4)+
      annotate("segment",x=5,xend=5,y=4.9,yend=3.1,arrow=arrow(length=unit(0.1,"in")),color="#3498db",linewidth=0.4)+
      annotate("segment",x=7.5,xend=7.5,y=2.5,yend=8.5,arrow=arrow(length=unit(0.1,"in")),color="#e74c3c",linewidth=0.4,linetype="dashed")+
      annotate("text",x=8.3,y=5.5,label="(-) Feedback",color="#e74c3c",size=2,angle=90)+
      annotate("text",x=4,y=7,label="Releasing\nHormone",color="#3498db",size=2)+
      annotate("text",x=4,y=4,label="Tropic\nHormone",color="#3498db",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── PK Compartment Model ──
make_pk_compartment <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name="",limits=c(0,10),breaks=NULL,expand=c(0,0))+
    scale_y_continuous(name="",limits=c(0,6),breaks=NULL,expand=c(0,0))
  if(lang!="none"){
    p <- p+annotate("rect",xmin=0.5,xmax=3,ymin=2,ymax=4,fill="white",color="#3498db",linewidth=0.5)+
      annotate("text",x=1.75,y=3,label="Central\nCompartment",color="#3498db",size=2.5)+
      annotate("rect",xmin=4.5,xmax=7,ymin=2,ymax=4,fill="white",color="#2ecc71",linewidth=0.5)+
      annotate("text",x=5.75,y=3,label="Peripheral\nCompartment",color="#2ecc71",size=2.5)+
      annotate("segment",x=3,xend=4.5,y=3.3,yend=3.3,arrow=arrow(length=unit(0.08,"in")),color=C$fg,linewidth=0.3)+
      annotate("segment",x=4.5,xend=3,y=2.7,yend=2.7,arrow=arrow(length=unit(0.08,"in")),color=C$fg,linewidth=0.3)+
      annotate("text",x=3.75,y=3.7,label="k12",color="grey50",size=2)+
      annotate("text",x=3.75,y=2.3,label="k21",color="grey50",size=2)+
      annotate("segment",x=1.75,xend=1.75,y=4,yend=5,arrow=arrow(length=unit(0.08,"in"),ends="first"),color="#f39c12",linewidth=0.3)+
      annotate("text",x=1.75,y=5.3,label="ka (absorption)",color="#f39c12",size=2)+
      annotate("segment",x=1.75,xend=1.75,y=2,yend=1,arrow=arrow(length=unit(0.08,"in")),color="#e74c3c",linewidth=0.3)+
      annotate("text",x=1.75,y=0.7,label="ke (elimination)",color="#e74c3c",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── Scoring Table (generic) ──
make_scoring_table <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xlb <- if(!is.null(t$xlb)) t$xlb else paste0("Item",1:5)
  n <- length(xlb)
  p <- p+scale_x_continuous(name="",limits=c(0,4),breaks=0:3,labels=if(lang=="none") rep("",4) else c("0","1","2","3"),expand=c(0.05,0.05))+
    scale_y_continuous(name="",limits=c(0.5,n+0.5),breaks=1:n,labels=if(lang=="none") rep("",n) else rev(xlb),expand=c(0.05,0.05))
  for(i in 0:n) p <- p+geom_hline(yintercept=i+0.5,color=C$gc,linewidth=0.2)
  for(j in 0:3) p <- p+geom_vline(xintercept=j+0.5,color=C$gc,linewidth=0.2)
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid=element_blank(),axis.text.y=element_text(size=5))
}

# ── Disk Diffusion (Antibiotic Sensitivity) ──
make_disk_diffusion <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name="",limits=c(-6,6),breaks=NULL,expand=c(0,0))+
    scale_y_continuous(name="",limits=c(-6,6),breaks=NULL,expand=c(0,0))
  p <- p+annotate("path",x=5*cos(seq(0,2*pi,len=100)),y=5*sin(seq(0,2*pi,len=100)),color=C$gc,linewidth=0.5)
  if(lang!="none"){
    angles <- seq(0,2*pi,length.out=7)[1:6]
    abx <- c("AMP","CIP","GEN","MER","VAN","TZP")
    for(i in 1:6){
      cx <- 3*cos(angles[i]); cy <- 3*sin(angles[i])
      p <- p+annotate("path",x=cx+0.5*cos(seq(0,2*pi,len=50)),y=cy+0.5*sin(seq(0,2*pi,len=50)),color="#3498db",linewidth=0.3)+
        annotate("text",x=cx,y=cy,label=abx[i],color="#3498db",size=2)
      p <- p+annotate("path",x=cx+1.2*cos(seq(0,2*pi,len=50)),y=cy+1.2*sin(seq(0,2*pi,len=50)),color=C$gc,linewidth=0.15,linetype="dotted")
    }
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid=element_blank(),aspect.ratio=1)
}

# ── Phylogenetic Tree ──
make_phylogenetic <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name=if(lang=="none") "" else "Genetic Distance",limits=c(0,10),expand=c(0,0))+
    scale_y_continuous(name="",limits=c(0,8),breaks=NULL,expand=c(0,0))
  if(lang!="none"){
    taxa <- c("Species A","Species B","Species C","Species D","Species E")
    ys <- c(1,2,4,5.5,7)
    for(i in 1:5) p <- p+annotate("text",x=9.5,y=ys[i],label=taxa[i],color=C$fg,size=2,hjust=1)
    p <- p+annotate("segment",x=1,xend=1,y=1,yend=7,color=C$fg,linewidth=0.3)+
      annotate("segment",x=1,xend=3,y=1.5,yend=1.5,color=C$fg,linewidth=0.3)+
      annotate("segment",x=3,xend=7,y=1,yend=1,color=C$fg,linewidth=0.3)+
      annotate("segment",x=3,xend=7,y=2,yend=2,color=C$fg,linewidth=0.3)+
      annotate("segment",x=1,xend=4,y=4.75,yend=4.75,color=C$fg,linewidth=0.3)+
      annotate("segment",x=4,xend=7,y=4,yend=4,color=C$fg,linewidth=0.3)+
      annotate("segment",x=4,xend=7,y=5.5,yend=5.5,color=C$fg,linewidth=0.3)+
      annotate("segment",x=1,xend=7,y=7,yend=7,color=C$fg,linewidth=0.3)+
      annotate("segment",x=3,xend=3,y=1,yend=2,color=C$fg,linewidth=0.3)+
      annotate("segment",x=4,xend=4,y=4,yend=5.5,color=C$fg,linewidth=0.3)
    p <- p+annotate("text",x=0.5,y=0.5,label="Root",color="grey50",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid=element_blank())
}

# ── Westgard Rules Chart ──
make_westgard <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- c(-4,4)
  xl <- if(lang=="none") "" else "Run Number"
  p <- p+scale_x_continuous(name=xl,limits=xr,breaks=seq(xr[1],xr[2],5),expand=c(0,0))+
    scale_y_continuous(name=if(lang=="none") "" else "SD Units",limits=yr,breaks=-4:4,expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color=C$fg,linewidth=0.3)+
    geom_hline(yintercept=c(-1,1),color="#2ecc71",linewidth=0.2,linetype="dotted")+
    geom_hline(yintercept=c(-2,2),color="#f39c12",linewidth=0.3,linetype="dashed")+
    geom_hline(yintercept=c(-3,3),color="#e74c3c",linewidth=0.3,linetype="dashed")
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2],y=3.5,label="1₃s",color="#e74c3c",size=2,hjust=1)+
      annotate("text",x=xr[2],y=2.5,label="2₂s",color="#f39c12",size=2,hjust=1)+
      annotate("text",x=xr[2],y=1.5,label="R₄s",color="#f39c12",size=2,hjust=1)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Interference Experiment ──
make_interference <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Interferent Concentration"; yl <- if(lang=="none") "" else "% Recovery"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  p <- p+geom_hline(yintercept=100,color=C$gc,linewidth=0.3)+
    geom_hline(yintercept=c(90,110),linetype="dashed",color="#f39c12",linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("rect",xmin=xr[1],xmax=xr[2],ymin=90,ymax=110,fill="#2ecc71",alpha=0.05)+
      annotate("text",x=xr[2]*0.8,y=105,label="±10% tolerance",color="#2ecc71",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Oxygen Dissociation Curve ──
make_oxy_dissociation <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "PaO2 (mmHg)"; yl <- if(lang=="none") "" else "SpO2 (%)"
  p <- p+scale_x_continuous(name=xl,limits=c(0,120),breaks=seq(0,120,20),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,100),breaks=seq(0,100,10),expand=c(0,0))
  p <- p+geom_hline(yintercept=c(50,90),linetype="dotted",color=C$gc,linewidth=0.2)+
    geom_vline(xintercept=c(27,60),linetype="dotted",color=C$gc,linewidth=0.2)
  if(lang!="none"){
    p <- p+annotate("point",x=27,y=50,color="#e74c3c",size=2)+
      annotate("text",x=30,y=45,label="P50 = 27 mmHg",color="#e74c3c",size=2,hjust=0)+
      annotate("text",x=90,y=60,label="Right shift:\npH↓, T↑, CO2↑\n2,3-DPG↑",color="#3498db",size=2)+
      annotate("text",x=10,y=85,label="Left shift:\npH↑, T↓, CO2↓\nfetal Hb, CO",color="#e74c3c",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Frank-Starling Curve ──
make_frank_starling <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Preload (LVEDV)"; yl <- if(lang=="none") "" else "Cardiac Output / Stroke Volume"
  p <- p+scale_x_continuous(name=xl,limits=c(0,300),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,15),expand=c(0,0))
  if(lang!="none"){
    p <- p+annotate("text",x=250,y=13,label="Sympathetic\nstimulation",color="#2ecc71",size=2.5)+
      annotate("text",x=250,y=8,label="Normal",color="#3498db",size=2.5)+
      annotate("text",x=250,y=4,label="Heart failure",color="#e74c3c",size=2.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Action Potential ──
make_action_potential <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Time (ms)"; yl <- if(lang=="none") "" else "Membrane Potential (mV)"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  p <- p+geom_hline(yintercept=c(-70,0,30),linetype=c("dashed","dotted","dotted"),color=c("#e74c3c",C$gc,C$gc),linewidth=0.2)
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2]*0.85,y=-75,label="RMP ≈ -70mV",color="#e74c3c",size=2)+
      annotate("text",x=xr[1]+diff(xr)*0.15,y=yr[2]*0.9,label="0: Depol.",color="grey50",size=2)+
      annotate("text",x=xr[1]+diff(xr)*0.3,y=yr[2]*0.7,label="1: Early\nRepol.",color="grey50",size=1.8)+
      annotate("text",x=xr[1]+diff(xr)*0.5,y=yr[2]*0.3,label="2: Plateau",color="grey50",size=2)+
      annotate("text",x=xr[1]+diff(xr)*0.7,y=-30,label="3: Repol.",color="grey50",size=2)+
      annotate("text",x=xr[2]*0.9,y=-60,label="4: Rest",color="grey50",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Starling Forces (Capillary Exchange) ──
make_starling_forces <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name="",limits=c(0,10),breaks=NULL,expand=c(0,0))+
    scale_y_continuous(name=if(lang=="none") "" else "Pressure (mmHg)",limits=c(-10,45),expand=c(0,0))
  if(lang!="none"){
    p <- p+annotate("rect",xmin=1,xmax=4.5,ymin=0,ymax=35,fill="#e74c3c",alpha=0.1)+
      annotate("rect",xmin=5.5,xmax=9,ymin=0,ymax=15,fill="#3498db",alpha=0.1)+
      annotate("text",x=2.75,y=40,label="Arteriolar End",color="#e74c3c",size=2.5,fontface="bold")+
      annotate("text",x=7.25,y=40,label="Venular End",color="#3498db",size=2.5,fontface="bold")+
      annotate("text",x=2.75,y=32,label="Pc = 35",color="#e74c3c",size=2)+
      annotate("text",x=7.25,y=12,label="Pc = 15",color="#3498db",size=2)+
      geom_hline(yintercept=25,linetype="dashed",color="#9b59b6",linewidth=0.4)+
      annotate("text",x=9.5,y=26,label="πc = 25",color="#9b59b6",size=2,hjust=1)+
      annotate("text",x=2.75,y=-5,label="Net: Filtration",color="#e74c3c",size=2,fontface="bold")+
      annotate("text",x=7.25,y=-5,label="Net: Absorption",color="#3498db",size=2,fontface="bold")
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid=element_blank())
}

# ── Renal Clearance Curves ──
make_renal_clearance <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Plasma Concentration (mg/dL)"; yl <- if(lang=="none") "" else "Rate (mg/min)"
  p <- p+scale_x_continuous(name=xl,limits=c(0,600),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,800),expand=c(0,0))
  if(lang!="none"){
    p <- p+annotate("text",x=500,y=750,label="Filtered",color="#3498db",size=2.5)+
      annotate("text",x=500,y=350,label="Excreted",color="#e74c3c",size=2.5)+
      annotate("text",x=300,y=150,label="Reabsorbed",color="#2ecc71",size=2.5)+
      annotate("text",x=150,y=250,label="Tm",color="#f39c12",size=2.5,fontface="bold")+
      geom_vline(xintercept=200,linetype="dotted",color="#f39c12",linewidth=0.3)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Compliance Curve (P-V Hysteresis) ──
make_compliance <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Transpulmonary Pressure (cmH2O)"; yl <- if(lang=="none") "" else "Volume (L)"
  p <- p+scale_x_continuous(name=xl,limits=c(-5,30),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,6),expand=c(0,0))
  if(lang!="none"){
    p <- p+annotate("text",x=20,y=5,label="Inflation",color="#e74c3c",size=2.5)+
      annotate("text",x=8,y=3.5,label="Deflation",color="#3498db",size=2.5)+
      annotate("text",x=15,y=1,label="Hysteresis",color="grey50",size=2,fontface="italic")+
      annotate("text",x=25,y=3,label="(surfactant\neffect)",color="grey50",size=1.8)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Wiggers Diagram (Cardiac Cycle) ──
make_wiggers <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name=if(lang=="none") "" else "Time (sec)",limits=c(0,0.8),breaks=seq(0,0.8,0.1),expand=c(0,0))+
    scale_y_continuous(name="",limits=c(0,6),breaks=NULL,expand=c(0,0))
  for(y in 1:5) p <- p+geom_hline(yintercept=y,color=C$gc,linewidth=0.2)
  if(lang!="none"){
    panels <- c("ECG","Heart Sounds","Aortic Pressure","Ventricular\nPressure","Ventricular\nVolume")
    ys <- c(5.5,4.5,3.5,2.5,0.5)
    cols <- c("#2ecc71","#9b59b6","#e74c3c","#3498db","#f39c12")
    for(i in 1:5) p <- p+annotate("text",x=0.02,y=ys[i]+0.4,label=panels[i],color=cols[i],size=1.8,hjust=0,fontface="bold",lineheight=0.8)
    p <- p+geom_vline(xintercept=c(0.1,0.38,0.48),linetype="dotted",color="grey70",linewidth=0.2)+
      annotate("text",x=0.24,y=5.9,label="Systole",color="grey50",size=2)+
      annotate("text",x=0.6,y=5.9,label="Diastole",color="grey50",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid=element_blank())
}

# ── JVP Waveform ──
make_jvp <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Time"; yl <- if(lang=="none") "" else "JVP (cmH2O)"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  if(lang!="none"){
    xs <- xr[1]+diff(xr)*c(0.08,0.18,0.3,0.42,0.55)
    p <- p+annotate("text",x=xs[1],y=yr[2]*0.9,label="a",color="#e74c3c",size=3,fontface="bold")+
      annotate("text",x=xs[2],y=yr[1]+diff(yr)*0.3,label="x",color="#3498db",size=3,fontface="bold")+
      annotate("text",x=xs[3],y=yr[2]*0.7,label="c",color="#e74c3c",size=3,fontface="bold")+
      annotate("text",x=xs[4],y=yr[2]*0.85,label="v",color="#e74c3c",size=3,fontface="bold")+
      annotate("text",x=xs[5],y=yr[1]+diff(yr)*0.25,label="y",color="#3498db",size=3,fontface="bold")+
      annotate("text",x=xr[1]+diff(xr)*0.85,y=yr[2]*0.9,label="a=atrial\nc=tricuspid\nv=venous filling\nx,y=descents",color="grey50",size=1.5,lineheight=0.9)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Phonocardiogram ──
make_phonocardiogram <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr)
  p <- p+scale_x_continuous(name=if(lang=="none") "" else "Time (sec)",limits=xr,expand=c(0,0))+
    scale_y_continuous(name=if(lang=="none") "" else "Amplitude",limits=c(-1,1),expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color=C$gc,linewidth=0.3)
  if(lang!="none"){
    cycle <- diff(xr)/2
    for(i in 0:1){
      s1x <- xr[1]+i*cycle+cycle*0.05
      s2x <- xr[1]+i*cycle+cycle*0.45
      p <- p+annotate("rect",xmin=s1x-cycle*0.02,xmax=s1x+cycle*0.02,ymin=-0.8,ymax=0.8,fill="#e74c3c",alpha=0.3)+
        annotate("text",x=s1x,y=-0.95,label="S1",color="#e74c3c",size=2.5,fontface="bold")+
        annotate("rect",xmin=s2x-cycle*0.015,xmax=s2x+cycle*0.015,ymin=-0.6,ymax=0.6,fill="#3498db",alpha=0.3)+
        annotate("text",x=s2x,y=-0.95,label="S2",color="#3498db",size=2.5,fontface="bold")+
        annotate("text",x=(s1x+s2x)/2,y=0.95,label="Systole",color="grey50",size=2)
    }
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── CO2 Dissociation Curve ──
make_co2_dissociation <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "PCO2 (mmHg)"; yl <- if(lang=="none") "" else "CO2 Content (mL/dL)"
  p <- p+scale_x_continuous(name=xl,limits=c(0,80),breaks=seq(0,80,10),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,70),expand=c(0,0))
  p <- p+geom_vline(xintercept=c(40,46),linetype="dotted",color=C$gc,linewidth=0.2)
  if(lang!="none"){
    p <- p+annotate("text",x=60,y=55,label="Reduced Hb\n(venous)",color="#3498db",size=2.5)+
      annotate("text",x=60,y=40,label="Oxygenated Hb\n(arterial)",color="#e74c3c",size=2.5)+
      annotate("text",x=35,y=10,label="Haldane\nEffect",color="grey50",size=2,fontface="italic")+
      annotate("segment",x=40,xend=46,y=48,yend=52,arrow=arrow(length=unit(0.08,"in"),ends="both"),color="#9b59b6",linewidth=0.3)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Log Dose-Response Curve ──
make_dose_response <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- c(0,100)
  xl <- if(lang=="none") "" else "log [Drug]"; yl <- if(lang=="none") "" else "% Maximum Response"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,breaks=seq(0,100,25),expand=c(0,0))
  p <- p+geom_hline(yintercept=c(50,100),linetype="dotted",color=C$gc,linewidth=0.2)
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2]*0.9,y=95,label="Emax",color="#e74c3c",size=2.5,hjust=1)+
      annotate("text",x=mean(xr),y=55,label="EC50",color="#3498db",size=2.5)+
      annotate("segment",x=mean(xr),xend=mean(xr),y=0,yend=50,linetype="dashed",color="#3498db",linewidth=0.3)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── ADME PK Timeline ──
make_adme <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr)
  xl <- if(lang=="none") "" else "Time (hours)"; yl <- if(lang=="none") "" else "Plasma Concentration"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,1),expand=c(0,0))
  p <- p+geom_hline(yintercept=c(0.3,0.8),linetype="dashed",color=c("#2ecc71","#e74c3c"),linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2]*0.9,y=0.85,label="Toxic level",color="#e74c3c",size=2,hjust=1)+
      annotate("text",x=xr[2]*0.9,y=0.35,label="MEC",color="#2ecc71",size=2,hjust=1)+
      annotate("rect",xmin=xr[1],xmax=xr[2],ymin=0.3,ymax=0.8,fill="#3498db",alpha=0.03)+
      annotate("text",x=xr[2]*0.9,y=0.55,label="Therapeutic\nwindow",color="#3498db",size=2,hjust=1)+
      annotate("text",x=xr[1]+diff(xr)*0.15,y=0.15,label="Absorption",color="#f39c12",size=2)+
      annotate("text",x=xr[1]+diff(xr)*0.55,y=0.15,label="Elimination",color="#9b59b6",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Coagulation Cascade ──
make_coagulation <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name="",limits=c(0,10),breaks=NULL,expand=c(0,0))+
    scale_y_continuous(name="",limits=c(0,10),breaks=NULL,expand=c(0,0))
  if(lang!="none"){
    box <- function(p,x,y,lab,col) p+annotate("rect",xmin=x-1,xmax=x+1,ymin=y-0.35,ymax=y+0.35,fill="white",color=col,linewidth=0.3)+annotate("text",x=x,y=y,label=lab,color=col,size=2)
    p <- box(p,2.5,9,"Intrinsic\n(XII→XI→IX→VIII)","#3498db")
    p <- box(p,7.5,9,"Extrinsic\n(TF + VII)","#e74c3c")
    p <- box(p,5,6,"Factor X → Xa","#9b59b6")
    p <- box(p,5,4,"Prothrombin → Thrombin","#f39c12")
    p <- box(p,5,2,"Fibrinogen → Fibrin","#2ecc71")
    p <- p+annotate("segment",x=2.5,xend=5,y=8.65,yend=6.35,arrow=arrow(length=unit(0.08,"in")),color="#3498db",linewidth=0.3)+
      annotate("segment",x=7.5,xend=5,y=8.65,yend=6.35,arrow=arrow(length=unit(0.08,"in")),color="#e74c3c",linewidth=0.3)+
      annotate("segment",x=5,xend=5,y=5.65,yend=4.35,arrow=arrow(length=unit(0.08,"in")),color="#9b59b6",linewidth=0.3)+
      annotate("segment",x=5,xend=5,y=3.65,yend=2.35,arrow=arrow(length=unit(0.08,"in")),color="#f39c12",linewidth=0.3)+
      annotate("text",x=5,y=7.3,label="Common\nPathway",color="grey50",size=2,fontface="italic",lineheight=0.8)+
      annotate("text",x=2,y=7,label="aPTT",color="#3498db",size=2)+
      annotate("text",x=8,y=7,label="PT/INR",color="#e74c3c",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+sptheme(style,C)
}

# ── NCV Waveform ──
make_ncv <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Time (ms)"; yl <- if(lang=="none") "" else "Amplitude (mV)"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color=C$gc,linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("segment",x=xr[1]+diff(xr)*0.1,xend=xr[1]+diff(xr)*0.1,y=yr[1]*0.8,yend=yr[2]*0.5,linetype="dashed",color="#f39c12",linewidth=0.3)+
      annotate("text",x=xr[1]+diff(xr)*0.1,y=yr[1]*0.9,label="Onset\nLatency",color="#f39c12",size=2)+
      annotate("text",x=xr[1]+diff(xr)*0.3,y=yr[2]*0.9,label="Amplitude",color="#e74c3c",size=2)+
      annotate("segment",x=xr[1]+diff(xr)*0.3,xend=xr[1]+diff(xr)*0.3,y=yr[1]*0.5,yend=yr[2]*0.7,color="#e74c3c",linewidth=0.3,arrow=arrow(length=unit(0.06,"in"),ends="both"))+
      annotate("text",x=xr[2]*0.7,y=yr[1]*0.5,label="Duration",color="#3498db",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Cumulative Incidence (Competing Risk) ──
make_cumulative_incidence <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr)
  xl <- if(lang=="none") "" else "Time"; yl <- if(lang=="none") "" else "Cumulative Incidence"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,1),breaks=seq(0,1,0.2),expand=c(0,0))
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2]*0.7,y=0.7,label="Event of\ninterest",color="#e74c3c",size=2.5)+
      annotate("text",x=xr[2]*0.7,y=0.3,label="Competing\nrisk",color="#3498db",size=2.5)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Cell Survival Curve (Radiation) ──
make_cell_survival <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Dose (Gy)"; yl <- if(lang=="none") "" else "Surviving Fraction (log)"
  p <- p+scale_x_continuous(name=xl,limits=c(0,12),breaks=seq(0,12,2),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(-4,0),breaks=-4:0,labels=c("0.0001","0.001","0.01","0.1","1"),expand=c(0,0))
  p <- p+geom_hline(yintercept=-1,linetype="dotted",color=C$gc,linewidth=0.2)
  if(lang!="none"){
    p <- p+annotate("text",x=3,y=-0.5,label="Shoulder\n(repair)",color="#2ecc71",size=2)+
      annotate("text",x=9,y=-3,label="Exponential\n(lethal)",color="#e74c3c",size=2)+
      annotate("text",x=8,y=-0.5,label="LQ model:\nS = exp(-αD - βD²)",color="grey50",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Fever Pattern ──
make_fever_pattern <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr)
  xl <- if(lang=="none") "" else "Day"; yl <- if(lang=="none") "" else "Temperature (°C)"
  p <- p+scale_x_continuous(name=xl,limits=xr,breaks=seq(xr[1],xr[2],1),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(36,41),breaks=seq(36,41,0.5),expand=c(0,0))
  p <- p+geom_hline(yintercept=37,linetype="dashed",color="#2ecc71",linewidth=0.3)+
    geom_hline(yintercept=38.3,linetype="dotted",color="#f39c12",linewidth=0.2)
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2]-0.5,y=37.2,label="Normal",color="#2ecc71",size=2,hjust=1)+
      annotate("text",x=xr[2]-0.5,y=38.5,label="Fever",color="#f39c12",size=2,hjust=1)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Waterfall Plot ──
make_waterfall <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  n <- if(!is.null(t$xr)) as.numeric(t$xr)[2] else 30
  xl <- if(lang=="none") "" else "Patient"; yl <- if(lang=="none") "" else "Best % Change from Baseline"
  p <- p+scale_x_continuous(name=xl,limits=c(0,n+1),breaks=NULL,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(-100,60),breaks=seq(-100,60,20),expand=c(0,0))
  p <- p+geom_hline(yintercept=c(-30,20),linetype="dashed",color=c("#2ecc71","#e74c3c"),linewidth=0.3)+
    geom_hline(yintercept=0,color=C$gc,linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=n*0.9,y=-35,label="PR (−30%)",color="#2ecc71",size=2,hjust=1)+
      annotate("text",x=n*0.9,y=25,label="PD (+20%)",color="#e74c3c",size=2,hjust=1)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Spider Plot (Individual Trajectories) ──
make_spider <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr)
  xl <- if(lang=="none") "" else "Time (weeks)"; yl <- if(lang=="none") "" else "% Change from Baseline"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(-100,100),breaks=seq(-100,100,25),expand=c(0,0))
  p <- p+geom_hline(yintercept=c(-30,0,20),linetype=c("dashed","solid","dashed"),color=c("#2ecc71",C$gc,"#e74c3c"),linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=xr[2]*0.9,y=-35,label="PR",color="#2ecc71",size=2)+
      annotate("text",x=xr[2]*0.9,y=25,label="PD",color="#e74c3c",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Q-Q Plot ──
make_qq <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "Theoretical Quantiles"; yl <- if(lang=="none") "" else "Sample Quantiles"
  rng <- as.numeric(t$xr)
  p <- p+scale_x_continuous(name=xl,limits=rng,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=rng,expand=c(0,0))
  p <- p+annotate("segment",x=rng[1],xend=rng[2],y=rng[1],yend=rng[2],color="#e74c3c",linewidth=0.5,linetype="dashed")
  if(lang!="none"){
    p <- p+annotate("text",x=rng[2]*0.7,y=rng[1]+diff(rng)*0.15,label="Reference line\n(normal distribution)",color="#e74c3c",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank(),aspect.ratio=1)
}

# ── Partogram ──
make_partogram <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  p <- p+scale_x_continuous(name=if(lang=="none") "" else "Time (hours)",limits=c(0,16),breaks=0:16,expand=c(0,0))+
    scale_y_continuous(name="",limits=c(0,2),breaks=NULL,expand=c(0,0))
  p <- p+geom_hline(yintercept=1,color=C$gc,linewidth=0.5)
  if(lang!="none"){
    p <- p+annotate("text",x=0.3,y=1.7,label="Cervical Dilation (cm)",color="#e74c3c",size=2,hjust=0,fontface="bold")+
      annotate("text",x=0.3,y=0.3,label="Descent (station)",color="#3498db",size=2,hjust=0,fontface="bold")
    for(d in seq(0,10,2)) p <- p+annotate("text",x=0.1,y=1+d/20,label=d,color="grey50",size=1.5)
    p <- p+annotate("segment",x=4,xend=10,y=1.1,yend=1.5,linetype="dashed",color="#f39c12",linewidth=0.3)+
      annotate("text",x=7,y=1.55,label="Alert line",color="#f39c12",size=2)+
      annotate("segment",x=8,xend=14,y=1.1,yend=1.5,linetype="dashed",color="#e74c3c",linewidth=0.3)+
      annotate("text",x=11,y=1.55,label="Action line",color="#e74c3c",size=2)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid=element_blank())
}

# ── ERG (Electroretinogram) ──
make_erg <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Time (ms)"; yl <- if(lang=="none") "" else "Amplitude (µV)"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color=C$gc,linewidth=0.3)+
    geom_vline(xintercept=0,linetype="dashed",color="#f39c12",linewidth=0.3)
  if(lang!="none"){
    p <- p+annotate("text",x=0,y=yr[2]*0.9,label="Flash",color="#f39c12",size=2)+
      annotate("text",x=xr[1]+diff(xr)*0.15,y=yr[1]*0.7,label="a-wave",color="#e74c3c",size=2.5,fontface="bold")+
      annotate("text",x=xr[1]+diff(xr)*0.35,y=yr[2]*0.7,label="b-wave",color="#3498db",size=2.5,fontface="bold")
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── ABR (Auditory Brainstem Response) ──
make_abr <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xr <- as.numeric(t$xr); yr <- as.numeric(t$yr)
  xl <- if(lang=="none") "" else "Latency (ms)"; yl <- if(lang=="none") "" else "Amplitude (µV)"
  p <- p+scale_x_continuous(name=xl,limits=xr,expand=c(0,0))+
    scale_y_continuous(name=yl,limits=yr,expand=c(0,0))
  p <- p+geom_hline(yintercept=0,color=C$gc,linewidth=0.3)
  if(lang!="none"){
    waves <- c("I","II","III","IV","V")
    lats <- c(1.5,2.5,3.5,4.8,5.5)
    sites <- c("AN","CN","SOC","LL","IC")
    for(i in 1:5){
      p <- p+annotate("text",x=lats[i],y=yr[2]*0.85,label=waves[i],color="#e74c3c",size=3,fontface="bold")+
        annotate("text",x=lats[i],y=yr[2]*0.6,label=paste0("(",sites[i],")"),color="grey50",size=1.5)
      p <- p+geom_vline(xintercept=lats[i],linetype="dotted",color=C$gc,linewidth=0.1)
    }
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Forrester Classification ──
make_forrester <- function(t, style, lang) {
  C <- spcols(style)
  p <- ggplot(data.frame(x=0,y=0),aes(x,y))+geom_blank()
  xl <- if(lang=="none") "" else "PCWP (mmHg)"; yl <- if(lang=="none") "" else "CI (L/min/m²)"
  p <- p+scale_x_continuous(name=xl,limits=c(0,30),breaks=seq(0,30,5),expand=c(0,0))+
    scale_y_continuous(name=yl,limits=c(0,5),breaks=seq(0,5,0.5),expand=c(0,0))
  p <- p+geom_hline(yintercept=2.2,linetype="dashed",color="#e74c3c",linewidth=0.4)+
    geom_vline(xintercept=18,linetype="dashed",color="#e74c3c",linewidth=0.4)
  if(lang!="none"){
    p <- p+annotate("text",x=9,y=3.8,label="I\nNormal",color="#2ecc71",size=3,fontface="bold")+
      annotate("text",x=24,y=3.8,label="II\nPulm.\nCongestion",color="#f39c12",size=2.5,fontface="bold",lineheight=0.8)+
      annotate("text",x=9,y=1.2,label="III\nLow\nPerfusion",color="#3498db",size=2.5,fontface="bold",lineheight=0.8)+
      annotate("text",x=24,y=1.2,label="IV\nCardiogenic\nShock",color="#e74c3c",size=2.5,fontface="bold",lineheight=0.8)
  }
  tt <- sptitle(t,lang); if(!is.null(tt)) p <- p+labs(title=tt,subtitle=t$sub)
  p+make_theme(style)+theme(panel.grid.minor=element_blank())
}

# ── Plot Generator ──
# lang: "en" = English labels, "ja" = Japanese title, "none" = no text
make_plot <- function(t, style = "standard", lang = "en") {
  tp <- if(!is.null(t$type)) t$type else "xy"
  if(tp!="xy") return(switch(tp,
    radar=make_radar(t,style,lang), ternary=make_ternary(t,style,lang),
    heatmap=make_heatmap(t,style,lang), audiogram=make_audiogram(t,style,lang),
    polar=make_polar_vf(t,style,lang),
    kaplan=make_kaplan(t,style,lang), roc=make_roc(t,style,lang),
    forest=make_forest(t,style,lang), funnel=make_funnel(t,style,lang),
    bland_altman=make_bland_altman(t,style,lang),
    boxplot=make_boxplot(t,style,lang), violin=make_violin(t,style,lang),
    ecg=make_ecg(t,style,lang), flow_volume=make_flow_volume(t,style,lang),
    pv_loop=make_pv_loop(t,style,lang), growth=make_growth(t,style,lang),
    bullseye=make_bullseye(t,style,lang), swimmer=make_swimmer(t,style,lang),
    tornado=make_tornado(t,style,lang), pyramid=make_pyramid(t,style,lang),
    manhattan=make_manhattan(t,style,lang), volcano=make_volcano(t,style,lang),
    dot=make_dot(t,style,lang), nomogram=make_nomogram(t,style,lang),
    epicurve=make_epicurve(t,style,lang), table2x2=make_table2x2(t,style,lang),
    pedigree=make_pedigree(t,style,lang), calibration=make_calibration(t,style,lang),
    scatter_reg=make_scatter_reg(t,style,lang),
    hypnogram=make_hypnogram(t,style,lang), spirogram=make_spirogram(t,style,lang),
    davenport=make_davenport(t,style,lang), vcg_triaxial=make_vcg_triaxial(t,style,lang),
    dvh=make_dvh(t,style,lang), ctg=make_ctg(t,style,lang),
    levey_jennings=make_levey_jennings(t,style,lang), fagan=make_fagan(t,style,lang),
    facs_2d=make_facs_2d(t,style,lang), ventilator_wave=make_ventilator_wave(t,style,lang),
    guyton=make_guyton(t,style,lang), lung_volumes=make_lung_volumes(t,style,lang),
    bbt=make_bbt(t,style,lang), sroc=make_sroc(t,style,lang),
    consort=make_consort(t,style,lang),
    vcg=make_vcg(t,style,lang), mmode=make_mmode(t,style,lang),
    swan_ganz=make_swan_ganz(t,style,lang), va_q=make_va_q(t,style,lang),
    acid_base_map=make_acid_base_map(t,style,lang), clamp=make_clamp(t,style,lang),
    hrm=make_hrm(t,style,lang), csf_pressure=make_csf_pressure(t,style,lang),
    amsler=make_amsler(t,style,lang), corneal_topo=make_corneal_topo(t,style,lang),
    hess=make_hess(t,style,lang), caloric=make_caloric(t,style,lang),
    rhinomanometry=make_rhinomanometry(t,style,lang), pfs=make_pfs(t,style,lang),
    gait=make_gait(t,style,lang), psych_profile=make_psych_profile(t,style,lang),
    mood_chart=make_mood_chart(t,style,lang), vital_timeline=make_vital_timeline(t,style,lang),
    anesthesia_record=make_anesthesia_record(t,style,lang), ct_window=make_ct_window(t,style,lang),
    cnv=make_cnv(t,style,lang), electrophoresis=make_electrophoresis(t,style,lang),
    isobologram=make_isobologram(t,style,lang), schild=make_schild(t,style,lang),
    hh_diagram=make_hh_diagram(t,style,lang), galbraith=make_galbraith(t,style,lang),
    labbe=make_labbe(t,style,lang), baujat=make_baujat(t,style,lang),
    deeks_funnel=make_deeks_funnel(t,style,lang), lexis=make_lexis(t,style,lang),
    dag=make_dag(t,style,lang), vaccine_schedule=make_vaccine_schedule(t,style,lang),
    denver=make_denver(t,style,lang), feedback_loop=make_feedback_loop(t,style,lang),
    pk_compartment=make_pk_compartment(t,style,lang), scoring_table=make_scoring_table(t,style,lang),
    disk_diffusion=make_disk_diffusion(t,style,lang), phylogenetic=make_phylogenetic(t,style,lang),
    westgard=make_westgard(t,style,lang), interference=make_interference(t,style,lang),
    oxy_dissociation=make_oxy_dissociation(t,style,lang), frank_starling=make_frank_starling(t,style,lang),
    action_potential=make_action_potential(t,style,lang), starling_forces=make_starling_forces(t,style,lang),
    renal_clearance=make_renal_clearance(t,style,lang), compliance=make_compliance(t,style,lang),
    wiggers=make_wiggers(t,style,lang), jvp=make_jvp(t,style,lang),
    phonocardiogram=make_phonocardiogram(t,style,lang), co2_dissociation=make_co2_dissociation(t,style,lang),
    dose_response=make_dose_response(t,style,lang), adme=make_adme(t,style,lang),
    coagulation=make_coagulation(t,style,lang), ncv=make_ncv(t,style,lang),
    cumulative_incidence=make_cumulative_incidence(t,style,lang), cell_survival=make_cell_survival(t,style,lang),
    fever_pattern=make_fever_pattern(t,style,lang), waterfall=make_waterfall(t,style,lang),
    spider=make_spider(t,style,lang), qq=make_qq(t,style,lang),
    partogram=make_partogram(t,style,lang), erg=make_erg(t,style,lang),
    abr=make_abr(t,style,lang), forrester=make_forrester(t,style,lang),
    make_radar(t,style,lang)))

  empty <- data.frame(x = numeric(0), y = numeric(0))
  p <- ggplot(empty, aes(x, y)) + geom_blank()

  # Axis labels based on language
  x_label <- if (lang == "none") "" else t$xl
  y_label <- if (lang == "none") "" else t$yl

  # X axis
  if (isTRUE(t$xlog)) {
    xargs <- list(name=x_label, limits=t$xr)
    if (!is.null(t$xb)) xargs$breaks <- t$xb
    if (!is.null(t$xlb) && lang != "none") xargs$labels <- t$xlb
    p <- p + do.call(scale_x_log10, xargs)
  } else {
    xargs <- list(name=x_label, limits=t$xr, expand=c(0,0))
    if (!is.null(t$xb)) xargs$breaks <- t$xb
    if (!is.null(t$xlb) && lang != "none") xargs$labels <- t$xlb
    p <- p + do.call(scale_x_continuous, xargs)
  }
  # Y axis
  if (isTRUE(t$ylog)) {
    yargs <- list(name=y_label, limits=t$yr)
    if (!is.null(t$yb)) yargs$breaks <- t$yb
    if (!is.null(t$ylb) && lang != "none") yargs$labels <- t$ylb
    p <- p + do.call(scale_y_log10, yargs)
  } else {
    yargs <- list(name=y_label, limits=t$yr, expand=c(0,0))
    if (!is.null(t$yb)) yargs$breaks <- t$yb
    if (!is.null(t$ylb) && lang != "none") yargs$labels <- t$ylb
    p <- p + do.call(scale_y_continuous, yargs)
  }

  # Horizontal reference lines (lines always shown; labels only if lang != "none")
  if (!is.null(t$hl)) for (h in t$hl) {
    yval <- as.numeric(h[[1]])
    col <- if (length(h)>=3) as.character(h[[3]]) else "grey60"
    p <- p + geom_hline(yintercept=yval, linetype="dashed", color=col, linewidth=0.4)
    if (lang != "none" && length(h)>=2 && nchar(as.character(h[[2]]))>0)
      p <- p + annotate("text", x=t$xr[1]+diff(t$xr)*0.03, y=yval+diff(t$yr)*0.02,
                         label=as.character(h[[2]]), color="grey50", hjust=0, size=3)
  }
  # Vertical reference lines
  if (!is.null(t$vl)) for (v in t$vl) {
    xval <- as.numeric(v[[1]])
    col <- if (length(v)>=3) as.character(v[[3]]) else "grey60"
    p <- p + geom_vline(xintercept=xval, linetype="dotted", color=col, linewidth=0.4)
    if (lang != "none" && length(v)>=2 && nchar(as.character(v[[2]]))>0)
      p <- p + annotate("text", x=xval+diff(t$xr)*0.01, y=t$yr[2]*0.96,
                         label=as.character(v[[2]]), color="grey50", hjust=0, size=3)
  }
  # Text annotations (skip for lang="none")
  if (lang != "none" && !is.null(t$ann)) for (a in t$ann) {
    col <- if (length(a)>=4) as.character(a[[4]]) else "grey55"
    sz  <- if (length(a)>=5) as.numeric(a[[5]]) else 3.5
    p <- p + annotate("text", x=as.numeric(a[[1]]), y=as.numeric(a[[2]]), label=as.character(a[[3]]), color=col, size=sz)
  }
  # Shaded zones (always shown - they are visual, not text)
  if (!is.null(t$zones)) for (z in t$zones) {
    fl <- if (length(z)>=5) as.character(z[[5]]) else "blue"
    al <- if (length(z)>=6) as.numeric(z[[6]]) else 0.06
    p <- p + annotate("rect", xmin=as.numeric(z[[1]]), xmax=as.numeric(z[[2]]), ymin=as.numeric(z[[3]]), ymax=as.numeric(z[[4]]), fill=fl, alpha=al)
  }

  # Title
  if (lang == "none") {
    # No title for text-free version
  } else {
    title_text <- if (lang == "ja") t$ja else t$en
    if (!is.null(t$sub)) p <- p + labs(title=title_text, subtitle=t$sub)
    else p <- p + labs(title=title_text)
  }

  p + make_theme(style)
}

# ── Image Generation ──
# langs: "en" (no suffix), "ja" (_ja suffix), "none" (_notxt suffix)
generate_all_images <- function(templates, only_lang=NULL) {
  styles <- c("standard","minimal","classic","presentation","dark")
  sizes <- list(standard=c(800,600), wide=c(1200,600), square=c(700,700))
  langs <- if (!is.null(only_lang)) only_lang else c("en","ja","none")
  total <- length(templates) * length(styles) * length(sizes) * length(langs)
  cat(sprintf("Generating %d images (%s)...\n", total, paste(langs, collapse="+")))
  n <- 0; skipped <- 0
  for (t in templates) {
    for (lng in langs) {
      lang_sfx <- switch(lng, en="", ja="_ja", none="_notxt")
      for (sty in styles) {
        for (szn in names(sizes)) {
          sz <- sizes[[szn]]
          sfx <- lang_sfx
          if (sty != "standard") sfx <- paste0(sfx, "_", sty)
          if (szn != "standard") sfx <- paste0(sfx, "_", szn)
          fn <- file.path(IMG_DIR, sprintf("%s_%s%s.png", t$cat, gsub("-","_",t$id), sfx))
          # Skip if already exists (for incremental builds)
          if (file.exists(fn)) { n <- n+1; skipped <- skipped+1; next }
          tryCatch({
            png(fn, width=sz[1], height=sz[2], res=150)
            print(make_plot(t, sty, lng))
            dev.off()
          }, error=function(e) {
            cat(sprintf("ERR: %s - %s\n", fn, e$message))
            try(dev.off(), silent=TRUE)
          })
          n <- n + 1
          if (n %% 200 == 0) cat(sprintf("  %d / %d (%.0f%%)\n", n, total, n/total*100))
        }
      }
    }
  }
  cat(sprintf("Images done: %d (skipped %d existing)\n", n, skipped))
  return(n)
}

# ── R Code Generator (auto-generated for each template) ──
make_r_code <- function(t) {
  xsc <- if (isTRUE(t$xlog)) "scale_x_log10" else "scale_x_continuous"
  ysc <- if (isTRUE(t$ylog)) "scale_y_log10" else "scale_y_continuous"
  sprintf('library(ggplot2)

# ---- %s ----
# あなたのデータを入力してください
df <- data.frame(
  x = c(),
  y = c()
)

ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 2) +
  geom_line() +
  %s(name = "%s", limits = c(%s, %s)) +
  %s(name = "%s", limits = c(%s, %s)) +
  labs(title = "%s") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))',
    t$en, xsc, gsub('"', '\\\\"', t$xl), t$xr[1], t$xr[2],
    ysc, gsub('"', '\\\\"', t$yl), t$yr[1], t$yr[2], t$en)
}

# ── CSS: external stylesheet ──
PAGE_CSS_LINK <- '<link rel="stylesheet" href="../style.css">'

# ── HTML Page Generator for Individual Templates (SEO enhanced) ──
generate_template_page <- function(t, all_t) {
  cja <- CATS[[t$cat]]$ja
  cen <- CATS[[t$cat]]$en
  img_base <- sprintf("%s_%s", t$cat, gsub("-","_",t$id))
  r_code <- make_r_code(t)

  # Related templates (same category, max 8)
  rel <- Filter(function(x) x$cat==t$cat && x$id!=t$id, all_t)
  rel <- head(rel, 8)
  rel_html <- paste(sapply(rel, function(r) {
    ri <- sprintf("%s_%s", r$cat, gsub("-","_",r$id))
    sprintf('<a href="%s.html" class="related-card"><img src="../img/%s.png" alt="%s - %sグラフテンプレート" loading="lazy"><span>%s</span></a>',
            r$id, ri, r$ja, CATS[[r$cat]]$ja, r$ja)
  }), collapse="\n")

  # Tags
  tag_list <- strsplit(t$tags, ",")[[1]]
  tags_html <- paste(sapply(trimws(tag_list), function(tg) sprintf('<span>%s</span>', tg)), collapse="")
  keywords <- paste(c(trimws(tag_list), cja, cen, "グラフテンプレート", "医学部", "CC0", "無料", "R", "ggplot2"), collapse=",")

  # SEO: enhanced description
  seo_desc <- sprintf('%s %s（%s）の白紙グラフテンプレート。著作権フリー（CC0）で医学部レポート・発表・論文にそのまま使用可能。5スタイル×3サイズ×3言語で合計45パターン。R+ggplot2コード付き。PNG形式で即ダウンロード。',
                      t$dj, cja, cen)

  # SEO: BreadcrumbList + ImageObject + HowTo schema
  ld_json <- sprintf('[{"@context":"https://schema.org","@type":"BreadcrumbList","itemListElement":[{"@type":"ListItem","position":1,"name":"HOME","item":"%s/"},{"@type":"ListItem","position":2,"name":"%s","item":"%s/c/%s.html"},{"@type":"ListItem","position":3,"name":"%s"}]},{"@context":"https://schema.org","@type":"ImageObject","name":"%s","description":"%s","contentUrl":"%s/img/%s.png","thumbnailUrl":"%s/img/%s.png","license":"https://creativecommons.org/publicdomain/zero/1.0/","acquireLicensePage":"%s/","creator":{"@type":"Organization","name":"MedGraph Free"},"copyrightNotice":"CC0 Public Domain","encodingFormat":"image/png"},{"@context":"https://schema.org","@type":"HowTo","name":"%sの使い方","description":"Rコードで%sを作成する方法","step":[{"@type":"HowToStep","name":"テンプレートをダウンロード","text":"好みのスタイル・サイズを選択してPNG画像をダウンロード"},{"@type":"HowToStep","name":"Rコードをコピー","text":"ggplot2コードをコピーしてRStudioに貼り付け"},{"@type":"HowToStep","name":"データを入力","text":"自分の実験データをdata.frameに入力して実行"}]}]',
    SITE_URL, cja, SITE_URL, t$cat, t$ja,
    t$ja, gsub('"', '\\\\"', t$dj), SITE_URL, img_base, SITE_URL, img_base, SITE_URL,
    t$ja, t$ja)

  html <- sprintf('<!DOCTYPE html>
<html lang="ja">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1.0">
<title>%s テンプレート【無料・著作権フリー】| MedGraph Free</title>
<meta name="description" content="%s">
<meta name="keywords" content="%s">
<meta name="google-site-verification" content="Wiq_d_MCZ8j5m7XH4dvaZ4jq3i0SqZRQOSwVi4Rr5gU">
<link rel="icon" href="../favicon.svg" type="image/svg+xml">
<link rel="icon" href="../favicon-48.png" sizes="48x48" type="image/png">
<link rel="apple-touch-icon" href="../apple-touch-icon.png">
<link rel="canonical" href="%s/%s/%s.html">
<meta property="og:type" content="article">
<meta property="og:title" content="%s テンプレート【無料・CC0】| MedGraph Free">
<meta property="og:description" content="%s">
<meta property="og:image" content="%s/img/%s.png">
<meta property="og:image:width" content="800">
<meta property="og:image:height" content="600">
<meta property="og:url" content="%s/%s/%s.html">
<meta property="og:site_name" content="MedGraph Free">
<meta property="og:locale" content="ja_JP">
<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:title" content="%s テンプレート | MedGraph Free">
<meta name="twitter:description" content="%s">
<meta name="twitter:image" content="%s/img/%s.png">
<script type="application/ld+json">%s</script>
%s
</head>
<body>
<header class="site-header"><div class="inner">
<h1><a href="../">MedGraph Free</a></h1>
<nav><a href="../">HOME</a><a href="../c/%s.html">%s</a></nav>
</div></header>
<nav class="breadcrumb" aria-label="パンくずリスト"><a href="../">HOME</a> &gt; <a href="../c/%s.html">%s</a> &gt; %s</nav>
<main>
<article>
<h1 class="page-title">%s</h1>
<p class="desc">%s</p>
<div class="tags" role="list" aria-label="タグ">%s</div>
<section class="preview-section">
<h2 class="sr-only">プレビューとダウンロード</h2>
<div class="switcher-row">
<div class="style-tabs" id="styleTabs" role="tablist" aria-label="スタイル選択">
<button class="active" onclick="swStyle(this,\'standard\')" role="tab" aria-selected="true">Standard</button>
<button onclick="swStyle(this,\'minimal\')" role="tab">Minimal</button>
<button onclick="swStyle(this,\'classic\')" role="tab">Classic</button>
<button onclick="swStyle(this,\'presentation\')" role="tab">Presentation</button>
<button onclick="swStyle(this,\'dark\')" role="tab">Dark</button>
</div>
<div class="style-tabs" id="langTabs" role="tablist" aria-label="言語選択">
<button class="active" onclick="swLang(this,\'en\')" role="tab" aria-selected="true">English</button>
<button onclick="swLang(this,\'ja\')" role="tab">日本語</button>
<button onclick="swLang(this,\'none\')" role="tab">テキストなし</button>
</div>
<div class="style-tabs" id="sizeTabs" role="tablist" aria-label="サイズ選択">
<button class="active" onclick="swSize(this,\'standard\')" role="tab" aria-selected="true">800 x 600</button>
<button onclick="swSize(this,\'wide\')" role="tab">1200 x 600</button>
<button onclick="swSize(this,\'square\')" role="tab">700 x 700</button>
</div>
</div>
<div class="preview-img"><img id="pv" src="../img/%s.png" alt="%s - %sグラフテンプレート（著作権フリー・CC0）" width="800" height="600"></div>
<div class="dl-buttons">
<a id="dl1" href="../img/%s.png" download class="dl-btn">PNG をダウンロード</a>
</div>
</section>
<section class="code-section">
<div class="code-header"><h2>R Code (ggplot2)</h2><button class="copy-btn" onclick="copyCode()">コードをコピー</button></div>
<pre id="rcode">%s</pre>
</section>
</article>
<aside class="related" aria-label="関連テンプレート"><h2>関連する%sテンプレート</h2><div class="related-grid">%s</div></aside>
</main>
<footer>
<p>MedGraph Free &mdash; 全テンプレートCC0（パブリックドメイン）。商用利用を含むあらゆる用途に無料で使用可能。</p>
<p><a href="../">テンプレート一覧に戻る</a> | <a href="https://github.com/S-Yus/medical_images">GitHub</a></p>
</footer>
<script>
const B="%s";
let curLang="",curStyle="",curSize="";
function swStyle(btn,s){
  document.querySelectorAll("#styleTabs button").forEach(b=>{b.classList.remove("active");b.setAttribute("aria-selected","false")});
  btn.classList.add("active");btn.setAttribute("aria-selected","true");
  curStyle=s==="standard"?"":"_"+s;upd();
}
function swLang(btn,l){
  document.querySelectorAll("#langTabs button").forEach(b=>{b.classList.remove("active");b.setAttribute("aria-selected","false")});
  btn.classList.add("active");btn.setAttribute("aria-selected","true");
  curLang=l==="en"?"":l==="ja"?"_ja":"_notxt";upd();
}
function swSize(btn,z){
  document.querySelectorAll("#sizeTabs button").forEach(b=>{b.classList.remove("active");b.setAttribute("aria-selected","false")});
  btn.classList.add("active");btn.setAttribute("aria-selected","true");
  curSize=z==="standard"?"":"_"+z;upd();
}
function upd(){
  const p="../img/"+B+curLang+curStyle+curSize;
  document.getElementById("pv").src=p+".png";
  document.getElementById("dl1").href=p+".png";
}
function copyCode(){
  const c=document.getElementById("rcode").textContent;
  navigator.clipboard.writeText(c).then(()=>{
    const b=document.querySelector(".copy-btn");b.textContent="Copied!";
    setTimeout(()=>b.textContent="コードをコピー",2000);
  });
}
</script>
</body>
</html>',
  t$ja, seo_desc, keywords,
  SITE_URL, t$cat, t$id,
  t$ja, gsub('"', '\\"', substr(t$dj, 1, 80)),
  SITE_URL, img_base,
  SITE_URL, t$cat, t$id,
  t$ja, gsub('"', '\\"', substr(t$dj, 1, 80)),
  SITE_URL, img_base,
  ld_json, PAGE_CSS_LINK,
  t$cat, cja,
  t$cat, cja, t$ja,
  t$ja, t$dj, tags_html,
  img_base, t$ja, cja,
  img_base,
  r_code, cja, rel_html,
  img_base)

  dir.create(t$cat, showWarnings=FALSE, recursive=TRUE)
  writeLines(html, file.path(t$cat, paste0(t$id, ".html")), useBytes=TRUE)
}

# ── Category Page Generator ──
generate_category_page <- function(cat_id, cat_templates) {
  cja <- CATS[[cat_id]]$ja
  cen <- CATS[[cat_id]]$en
  dir.create("c", showWarnings=FALSE)

  # Classify each template into subcategory
  sc_map <- sapply(cat_templates, classify_subcat)
  sc_ids <- unique(sc_map)

  # Build tab buttons — "全て" first, then subcategories
  sc_defs <- SUBCATS[[cat_id]]
  has_tabs <- !is.null(sc_defs) && length(sc_ids) > 1
  tab_html <- ""
  if (has_tabs) {
    btns <- sprintf('<button class="tab active" data-sc="all">全て<span class="tab-count">%d</span></button>',
                    length(cat_templates))
    for (sc_def in sc_defs) {
      n <- sum(sc_map == sc_def$id)
      if (n > 0) {
        btns <- paste0(btns, sprintf(
          '<button class="tab" data-sc="%s">%s<span class="tab-count">%d</span></button>',
          sc_def$id, sc_def$ja, n))
      }
    }
    # "Other" tab if any templates don't match
    n_other <- sum(sc_map == "other")
    if (n_other > 0) {
      btns <- paste0(btns, sprintf(
        '<button class="tab" data-sc="other">その他<span class="tab-count">%d</span></button>',
        n_other))
    }
    tab_html <- sprintf('<div class="tabs">%s</div>', btns)
  }

  # Template cards with data-sc attribute
  cards <- paste(sapply(seq_along(cat_templates), function(i) {
    t <- cat_templates[[i]]
    ib <- sprintf("%s_%s", t$cat, gsub("-","_",t$id))
    sprintf('<a href="../%s/%s.html" class="tcard" data-sc="%s"><img src="../img/%s.png" alt="%s" loading="lazy"><div class="tname">%s</div><div class="tdesc">%s</div></a>',
            t$cat, t$id, sc_map[i], ib, t$ja, t$ja, substr(t$dj, 1, 60))
  }), collapse="\n")

  # Tab filtering JavaScript
  tab_js <- if (has_tabs) '
<script>
document.querySelectorAll(".tab").forEach(function(btn){
  btn.addEventListener("click",function(){
    document.querySelectorAll(".tab").forEach(function(b){b.classList.remove("active")});
    btn.classList.add("active");
    var sc=btn.getAttribute("data-sc");
    document.querySelectorAll(".tcard").forEach(function(card){
      card.style.display=(sc==="all"||card.getAttribute("data-sc")===sc)?"":"none";
    });
  });
});
</script>' else ""

  # SEO: enhanced description
  seo_desc <- sprintf('%s（%s）の医学部レポート・発表用グラフテンプレート一覧。%d種類の著作権フリー（CC0）白紙テンプレートを無料提供。R+ggplot2コード付き。5スタイル×3サイズ×3言語。PNG形式で即ダウンロード可能。',
                      cja, cen, length(cat_templates))
  seo_kw <- paste(c(cja, cen, "グラフテンプレート", "医学部", "レポート", "無料", "著作権フリー", "CC0", "R", "ggplot2", "テンプレート"), collapse=",")

  # SEO: BreadcrumbList + CollectionPage + ItemList schema
  item_list_items <- paste(sapply(seq_along(cat_templates), function(i) {
    tt <- cat_templates[[i]]
    sprintf('{"@type":"ListItem","position":%d,"url":"%s/%s/%s.html","name":"%s"}', i, SITE_URL, tt$cat, tt$id, tt$ja)
  }), collapse=",")

  ld_json <- sprintf('[{"@context":"https://schema.org","@type":"BreadcrumbList","itemListElement":[{"@type":"ListItem","position":1,"name":"HOME","item":"%s/"},{"@type":"ListItem","position":2,"name":"%s"}]},{"@context":"https://schema.org","@type":"CollectionPage","name":"%s グラフテンプレート一覧","description":"%s","url":"%s/c/%s.html","isPartOf":{"@type":"WebSite","name":"MedGraph Free","url":"%s/"},"numberOfItems":%d,"mainEntity":{"@type":"ItemList","numberOfItems":%d,"itemListElement":[%s]}}]',
    SITE_URL, cja,
    cja, gsub('"', '\\\\"', seo_desc), SITE_URL, cat_id, SITE_URL,
    length(cat_templates), length(cat_templates), item_list_items)

  # SEO: first template image for OG
  first_img <- sprintf("%s_%s", cat_templates[[1]]$cat, gsub("-","_",cat_templates[[1]]$id))

  html <- sprintf('<!DOCTYPE html>
<html lang="ja">
<head>
<meta charset="UTF-8"><meta name="viewport" content="width=device-width,initial-scale=1.0">
<title>%s（%s）グラフテンプレート一覧【%d種類・無料】| MedGraph Free</title>
<meta name="description" content="%s">
<meta name="keywords" content="%s">
<link rel="icon" href="../favicon.svg" type="image/svg+xml">
<link rel="icon" href="../favicon-48.png" sizes="48x48" type="image/png">
<link rel="apple-touch-icon" href="../apple-touch-icon.png">
<link rel="canonical" href="%s/c/%s.html">
<meta property="og:type" content="website">
<meta property="og:title" content="%s グラフテンプレート一覧【%d種類・無料・CC0】">
<meta property="og:description" content="%s">
<meta property="og:image" content="%s/img/%s.png">
<meta property="og:url" content="%s/c/%s.html">
<meta property="og:site_name" content="MedGraph Free">
<meta property="og:locale" content="ja_JP">
<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:title" content="%s グラフテンプレート一覧 | MedGraph Free">
<meta name="twitter:description" content="%d種類の%sグラフテンプレートを無料提供。著作権フリー（CC0）・Rコード付き。">
<script type="application/ld+json">%s</script>
<link rel="stylesheet" href="../style.css">
</head>
<body>
<header class="hd"><div class="inner"><h1><a href="../">MedGraph Free</a></h1><nav><a href="../">HOME</a></nav></div></header>
<nav class="bc" aria-label="パンくずリスト"><a href="../">HOME</a> &gt; %s</nav>
<main>
<h1 class="cat-title">%s (%s)</h1>
<p class="count">%d種類のテンプレート</p>
%s
<div class="grid">%s</div>
<section class="seo-text" style="max-width:800px;margin:24px 0;font-size:13px;color:#666;line-height:1.7">
<h2 style="font-size:16px;color:#1a1a1a;margin-bottom:8px">%sグラフテンプレートについて</h2>
<p>MedGraph Freeの%s（%s）カテゴリでは、%d種類の白紙グラフテンプレートを著作権フリー（CC0・パブリックドメイン）で提供しています。医学部の講義レポート、実習レポート、研究発表、ポスター発表、卒業論文などに、ダウンロードしてそのまま利用できます。各テンプレートにはR言語（ggplot2）のコードが付属しており、自分のデータを入力するだけでグラフを再現可能です。Standard・Minimal・Classic・Presentation・Darkの5スタイル、800×600・1200×600・700×700の3サイズ、英語・日本語・テキストなしの3言語から選択できます。</p>
</section>
</main>
<footer><p>MedGraph Free &mdash; 全テンプレートCC0（パブリックドメイン）</p><p><a href="../">テンプレート一覧</a> | <a href="https://github.com/S-Yus/medical_images">GitHub</a></p></footer>
%s
</body></html>',
  cja, cen, length(cat_templates),
  seo_desc, seo_kw,
  SITE_URL, cat_id,
  cja, length(cat_templates),
  gsub('"', '\\"', substr(seo_desc, 1, 100)),
  SITE_URL, first_img,
  SITE_URL, cat_id,
  cja, length(cat_templates), cja,
  ld_json,
  cja,
  cja, cen, length(cat_templates),
  tab_html, cards,
  cja, cja, cen, length(cat_templates),
  tab_js)

  writeLines(html, file.path("c", paste0(cat_id, ".html")), useBytes=TRUE)
}

# ── Main Index Page Generator ──
generate_index <- function(all_t) {
  # Group by category
  by_cat <- split(all_t, sapply(all_t, function(x) x$cat))
  total <- length(all_t)
  total_dl <- total * 45  # 5 styles * 3 sizes * 3 languages

  # Category cards with English subtitle
  cat_cards <- paste(sapply(names(by_cat), function(cn) {
    ts <- by_cat[[cn]]
    cja <- CATS[[cn]]$ja
    cen <- CATS[[cn]]$en
    sprintf('<a href="c/%s.html" class="cat-card" id="%s"><div class="cat-name">%s</div><div class="cat-en">%s</div><div class="cat-count">%d種類</div></a>',
            cn, cn, cja, cen, length(ts))
  }), collapse="\n")

  # SEO: category list for structured data
  cat_list_items <- paste(sapply(seq_along(names(by_cat)), function(i) {
    cn <- names(by_cat)[i]
    cja <- CATS[[cn]]$ja
    sprintf('{"@type":"ListItem","position":%d,"url":"%s/c/%s.html","name":"%s グラフテンプレート"}', i, SITE_URL, cn, cja)
  }), collapse=",")

  # SEO: FAQ schema
  faq_items <- paste(c(
    '{"@type":"Question","name":"MedGraph Freeとは何ですか？","acceptedAnswer":{"@type":"Answer","text":"MedGraph Freeは医学部生のための無料グラフテンプレート集です。生化学・生理学・薬理学など28分野のグラフテンプレートを著作権フリー（CC0）で提供しています。"}}',
    '{"@type":"Question","name":"テンプレートは無料で使えますか？","acceptedAnswer":{"@type":"Answer","text":"はい、全テンプレートはCC0（パブリックドメイン）ライセンスです。商用利用を含むあらゆる用途に、許可不要・帰属表示不要で無料で使用できます。"}}',
    '{"@type":"Question","name":"どのような形式でダウンロードできますか？","acceptedAnswer":{"@type":"Answer","text":"PNG形式でダウンロードできます。各テンプレートは5スタイル（Standard・Minimal・Classic・Presentation・Dark）×3サイズ（800×600・1200×600・700×700）×3言語（英語・日本語・テキストなし）の計45パターンから選択可能です。"}}',
    '{"@type":"Question","name":"Rコードは付いていますか？","acceptedAnswer":{"@type":"Answer","text":"はい、各テンプレートにR言語（ggplot2パッケージ）のコードが付属しています。自分のデータをdata.frameに入力するだけでグラフを再現できます。"}}'
  ), collapse=",")

  ld_json <- sprintf('[{"@context":"https://schema.org","@type":"WebSite","name":"MedGraph Free","alternateName":"メドグラフ フリー","url":"%s/","description":"医学部生のためのグラフテンプレート集。全%d分野%d種類。著作権フリー（CC0）。R+ggplot2コード付き。","inLanguage":"ja","potentialAction":{"@type":"SearchAction","target":{"@type":"EntryPoint","urlTemplate":"%s/?q={search_term_string}"},"query-input":"required name=search_term_string"}},{"@context":"https://schema.org","@type":"ItemList","name":"医学グラフテンプレート カテゴリ一覧","numberOfItems":%d,"itemListElement":[%s]},{"@context":"https://schema.org","@type":"FAQPage","mainEntity":[%s]}]',
    SITE_URL, length(by_cat), total, SITE_URL,
    length(by_cat), cat_list_items, faq_items)

  nc <- length(by_cat)

  head_html <- sprintf('<!DOCTYPE html>
<html lang="ja" prefix="og: https://ogp.me/ns#">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1.0">
<title>MedGraph Free | 医学部生のための無料グラフテンプレート集【%d分野%d種類・著作権フリー・Rコード付き】</title>
<meta name="description" content="医学部のレポート・発表・論文用グラフテンプレートを無料提供。全%d分野%d種類・合計%dパターンをPNG形式で即ダウンロード。著作権フリー（CC0）で商用利用も可。R+ggplot2コード付き。生化学・生理学・薬理学・循環器・病理学など全28分野対応。">
<meta name="keywords" content="医学部,グラフテンプレート,レポート,無料,著作権フリー,CC0,R,ggplot2,医学生,テンプレート,白紙グラフ,実習レポート,生化学,生理学,薬理学,病理学,公衆衛生,臨床検査,ダウンロード,PNG">
<meta name="google-site-verification" content="Wiq_d_MCZ8j5m7XH4dvaZ4jq3i0SqZRQOSwVi4Rr5gU">
<link rel="icon" href="favicon.svg" type="image/svg+xml">
<link rel="icon" href="favicon-48.png" sizes="48x48" type="image/png">
<link rel="apple-touch-icon" href="apple-touch-icon.png">
<link rel="canonical" href="%s/">
<meta property="og:type" content="website">
<meta property="og:title" content="MedGraph Free | 医学部生のための無料グラフテンプレート集【%d種類・CC0】">
<meta property="og:description" content="全%d分野%d種類の白紙グラフテンプレートを無料提供。著作権フリー（CC0）・Rコード付き。">
<meta property="og:url" content="%s/">
<meta property="og:image" content="%s/og-image.png">
<meta property="og:site_name" content="MedGraph Free">
<meta property="og:locale" content="ja_JP">
<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:title" content="MedGraph Free | 医学部グラフテンプレート%d種類【無料・CC0】">
<meta name="twitter:description" content="医学部のレポート・発表用グラフテンプレートを全%d分野%d種類無料提供。著作権フリー（CC0）・Rコード付き。">
<script type="application/ld+json">%s</script>
<link rel="stylesheet" href="style.css">
</head>
<body>
<header class="hero" role="banner">
<h1>MedGraph Free</h1>
<p>医学部生のための無料グラフテンプレート集。著作権フリー（CC0）でレポート・発表・論文にそのまま使えます。</p>
<div class="stats" aria-label="サイト統計">
<div class="stat"><div class="num">%d</div><div class="lbl">Templates</div></div>
<div class="stat"><div class="num">%d</div><div class="lbl">Images</div></div>
<div class="stat"><div class="num">%d</div><div class="lbl">Categories</div></div>
<div class="stat"><div class="num">5</div><div class="lbl">Styles</div></div>
</div>
<div class="search-box"><input type="search" id="search" placeholder="分野を検索（例: 生化学, 循環器, 薬理学）" oninput="filterCats(this.value)" aria-label="分野検索"></div>
</header>',
  nc, total, nc, total, total_dl, SITE_URL,
  total, nc, total, SITE_URL, SITE_URL,
  total, nc, total, ld_json,
  total, total_dl, nc)

  body_html <- sprintf('<main>
<section aria-label="分野一覧">
<div class="cats-grid">%s</div>
<div class="no-results" id="noResults"><p>該当する分野が見つかりませんでした</p></div>
</section>
<section class="seo-text" style="max-width:900px;margin:32px auto;padding:0 20px;font-size:13px;color:#666;line-height:1.8">
<h2 style="font-size:18px;color:#1a1a1a;margin-bottom:12px;font-family:Georgia,serif;font-weight:normal">MedGraph Free について</h2>
<p>MedGraph Freeは、医学部生のためのグラフテンプレート集です。<strong>生化学</strong>（ミカエリス・メンテンプロット、ラインウィーバー・バークプロット、pH滴定曲線など）、<strong>生理学</strong>（酸素解離曲線、フランク・スターリング曲線、活動電位など）、<strong>薬理学</strong>（用量反応曲線、薬物動態曲線など）をはじめ、<strong>循環器</strong>（心電図、血圧波形、PVループ）、<strong>病理学</strong>、<strong>疫学・公衆衛生</strong>、<strong>臨床統計</strong>など全%d分野・%d種類のテンプレートを収録しています。</p>
<p>すべてのテンプレートは<strong>著作権フリー（CC0・パブリックドメイン）</strong>で、商用利用を含むあらゆる用途に許可不要・帰属表示不要で使用できます。講義レポート、実習レポート、研究発表、ポスター発表、卒業論文、学会発表スライドなどにそのままお使いいただけます。</p>
<p>各テンプレートは<strong>5スタイル</strong>（Standard・Minimal・Classic・Presentation・Dark）× <strong>3サイズ</strong>（800×600・1200×600・700×700）× <strong>3言語</strong>（英語・日本語・テキストなし）の合計45パターンで提供。PNG形式で即ダウンロード可能です。さらに、各テンプレートには<strong>R言語（ggplot2）のコード</strong>が付属しており、自分のデータを入力するだけでグラフを再現できます。</p>
</section>', cat_cards, nc, total)

  # JS kept outside sprintf to avoid escaping issues
  tail_html <- '<section class="request-section" id="request">
<div class="request-card">
<h2>テンプレートリクエスト</h2>
<p>欲しいグラフテンプレートがありましたらお気軽にリクエストください。</p>
<form id="requestForm" class="request-form">
<div class="form-row">
<label for="req-name">テンプレート名 <span class="req">*</span></label>
<input type="text" id="req-name" name="template_name" required placeholder="例: 腎クリアランス曲線">
</div>
<div class="form-row">
<label for="req-category">分野</label>
<input type="text" id="req-category" name="category" placeholder="例: 腎臓">
</div>
<div class="form-row">
<label for="req-detail">用途・説明</label>
<textarea id="req-detail" name="detail" rows="3" placeholder="どのような場面で使いたいか、参考情報など"></textarea>
</div>
<div class="form-row">
<label for="req-email">メールアドレス（任意）</label>
<input type="email" id="req-email" name="email" placeholder="完成時に通知を受け取る場合">
</div>
<button type="submit" class="request-btn">リクエストを送信</button>
</form>
<div id="requestSuccess" class="form-success" style="display:none">
<p>リクエストを受け付けました。ありがとうございます。</p>
</div>
</div>
</section>
</main>
<footer>
<p>MedGraph Free &mdash; CC0 (Public Domain). Free for any use including commercial.</p>
<p>Generated with R + ggplot2. <a href="https://github.com/S-Yus/medical_images">GitHub</a></p>
</footer>
<script>
function filterCats(q){
  q=q.toLowerCase();
  var found=0;
  document.querySelectorAll(".cat-card").forEach(function(c){
    var v=c.textContent.toLowerCase().indexOf(q)>=0;
    c.style.display=v?"":"none";
    if(v)found++;
  });
  document.getElementById("noResults").classList.toggle("show",q&&found===0);
}
document.getElementById("requestForm").addEventListener("submit",function(e){
  e.preventDefault();
  var f=this;
  var nm=document.getElementById("req-name").value;
  var cat=document.getElementById("req-category").value||"未指定";
  var dt=document.getElementById("req-detail").value||"なし";
  var em=document.getElementById("req-email").value||"なし";
  var title=encodeURIComponent("Template Request: "+nm);
  var body=encodeURIComponent("テンプレート名: "+nm+"\\n分野: "+cat+"\\n用途・説明: "+dt+"\\n連絡先: "+em);
  window.open("https://github.com/S-Yus/medical_images/issues/new?title="+title+"&labels=request&body="+body,"_blank");
  f.style.display="none";
  document.getElementById("requestSuccess").style.display="block";
});
</script>
</body></html>'

  html <- paste0(head_html, "\n", body_html, "\n", tail_html)

  writeLines(html, "index.html", useBytes=TRUE)
  cat(sprintf("index.html generated (%d categories, %d templates)\n", length(by_cat), total))
}

# ── Sitemap Generator (SEO enhanced with lastmod) ──
generate_sitemap <- function(all_t) {
  today <- format(Sys.Date(), "%Y-%m-%d")

  urls <- sprintf('  <url><loc>%s/</loc><lastmod>%s</lastmod><changefreq>weekly</changefreq><priority>1.0</priority></url>', SITE_URL, today)

  # Category pages
  cat_urls <- sapply(unique(sapply(all_t, function(x) x$cat)), function(cn) {
    sprintf('  <url><loc>%s/c/%s.html</loc><lastmod>%s</lastmod><changefreq>weekly</changefreq><priority>0.8</priority></url>', SITE_URL, cn, today)
  })

  # Template pages with image
  tmpl_urls <- sapply(all_t, function(t) {
    ib <- sprintf("%s_%s", t$cat, gsub("-","_",t$id))
    sprintf('  <url>\n    <loc>%s/%s/%s.html</loc>\n    <lastmod>%s</lastmod>\n    <changefreq>monthly</changefreq>\n    <priority>0.6</priority>\n    <image:image>\n      <image:loc>%s/img/%s.png</image:loc>\n      <image:title>%s</image:title>\n      <image:caption>%s</image:caption>\n      <image:license>https://creativecommons.org/publicdomain/zero/1.0/</image:license>\n    </image:image>\n  </url>',
            SITE_URL, t$cat, t$id, today, SITE_URL, ib, t$ja, gsub('"', '&quot;', substr(t$dj, 1, 100)))
  })

  xml <- paste(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9" xmlns:image="http://www.google.com/schemas/sitemap-image/1.1">',
    urls, cat_urls, tmpl_urls,
    '</urlset>'
  ), collapse="\n")

  writeLines(xml, "sitemap.xml", useBytes=TRUE)
  cat(sprintf("sitemap.xml generated (%d URLs)\n", 1 + length(cat_urls) + length(tmpl_urls)))
}

# ── 404 Page Generator ──
generate_404 <- function() {
  html <- '<!DOCTYPE html>
<html lang="ja">
<head>
<meta charset="UTF-8"><meta name="viewport" content="width=device-width,initial-scale=1.0">
<title>ページが見つかりません | MedGraph Free</title>
<meta name="robots" content="noindex">
<link rel="icon" href="/favicon.svg" type="image/svg+xml">
<link rel="stylesheet" href="/style.css">
</head>
<body>
<header class="site-header"><div class="inner">
<h1><a href="/">MedGraph Free</a></h1>
<nav><a href="/">HOME</a></nav>
</div></header>
<main style="text-align:center;padding:60px 20px">
<h1 class="page-title" style="font-size:48px;margin-bottom:16px">404</h1>
<p class="desc">お探しのページは見つかりませんでした。</p>
<p style="margin-top:20px"><a href="/" style="font-size:15px">テンプレート一覧に戻る</a></p>
</main>
<footer>
<p>MedGraph Free &mdash; CC0 (Public Domain)</p>
</footer>
</body></html>'
  writeLines(html, "404.html", useBytes=TRUE)
  cat("404.html generated\n")
}

# ============================
# LOAD TEMPLATES & RUN
# ============================
cat("Loading template definitions...\n")
source("template_defs.R", encoding="UTF-8")
cat(sprintf("Loaded %d templates\n", length(TEMPLATES)))

# 1. Generate images (skip if SKIP_IMAGES env var is set)
if (Sys.getenv("SKIP_IMAGES") == "") {
  generate_all_images(TEMPLATES)
} else {
  cat("Skipping image generation (SKIP_IMAGES set)\n")
}

# 2. Generate individual pages
cat("Generating individual HTML pages...\n")
for (t in TEMPLATES) generate_template_page(t, TEMPLATES)
cat(sprintf("Generated %d template pages\n", length(TEMPLATES)))

# 3. Generate category pages
by_cat <- split(TEMPLATES, sapply(TEMPLATES, function(x) x$cat))
for (cn in names(by_cat)) generate_category_page(cn, by_cat[[cn]])
cat(sprintf("Generated %d category pages\n", length(by_cat)))

# 4. Generate index
generate_index(TEMPLATES)

# 5. Generate sitemap
generate_sitemap(TEMPLATES)

# 6. Generate 404 page
generate_404()

cat(sprintf("\n=== BUILD COMPLETE ===\nTime: %s\n", Sys.time()))
