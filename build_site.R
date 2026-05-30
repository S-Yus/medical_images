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

# ── Template Definition Helper ──
D <- function(id, cat, en, ja, xl, yl, xr, yr, dj, tags="",
              sub=NULL, xlog=FALSE, ylog=FALSE,
              hl=NULL, vl=NULL, ann=NULL, zones=NULL,
              xb=NULL, yb=NULL, xlb=NULL, ylb=NULL) {
  list(id=id, cat=cat, en=en, ja=ja, xl=xl, yl=yl, xr=xr, yr=yr,
       dj=dj, tags=tags, sub=sub, xlog=xlog, ylog=ylog,
       hl=hl, vl=vl, ann=ann, zones=zones,
       xb=xb, yb=yb, xlb=xlb, ylb=ylb)
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

# ── Plot Generator ──
# lang: "en" = English labels, "ja" = Japanese title, "none" = no text
make_plot <- function(t, style = "standard", lang = "en") {
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

# ── CSS for Individual Pages ──
PAGE_CSS <- '
*{margin:0;padding:0;box-sizing:border-box}
body{font-family:"Helvetica Neue",Arial,"Hiragino Sans",sans-serif;background:#f8f9fa;color:#333;line-height:1.7}
a{color:#2563eb;text-decoration:none}a:hover{text-decoration:underline}
.site-header{background:linear-gradient(135deg,#1e3a5f,#2563eb);color:#fff;padding:12px 0;position:sticky;top:0;z-index:100}
.site-header .inner{max-width:1200px;margin:0 auto;padding:0 20px;display:flex;align-items:center;justify-content:space-between}
.site-header h1{font-size:18px}.site-header h1 a{color:#fff}
.site-header nav a{color:rgba(255,255,255,.85);margin-left:20px;font-size:14px}
.breadcrumb{max-width:1000px;margin:20px auto 0;padding:0 20px;font-size:13px;color:#666}
.breadcrumb a{color:#2563eb}
main{max-width:1000px;margin:20px auto 40px;padding:0 20px}
h1.page-title{font-size:28px;margin:16px 0 8px;color:#1e3a5f}
.desc{font-size:15px;color:#555;margin-bottom:20px}
.tags{margin-bottom:20px}.tags span{display:inline-block;background:#e8f0fe;color:#1a56db;padding:3px 10px;border-radius:12px;font-size:12px;margin:2px 4px 2px 0}
.preview-section{background:#fff;border-radius:12px;padding:24px;box-shadow:0 2px 8px rgba(0,0,0,.06);margin-bottom:24px}
.switcher-row{display:flex;gap:16px;margin-bottom:16px;flex-wrap:wrap;align-items:flex-start}
.style-tabs{display:flex;gap:8px;flex-wrap:wrap}
.style-tabs button{padding:8px 16px;border:1px solid #ddd;border-radius:6px;background:#fff;cursor:pointer;font-size:13px;transition:.2s}
.style-tabs button.active{background:#2563eb;color:#fff;border-color:#2563eb}
.style-tabs button:hover:not(.active){background:#f0f4ff}
.preview-img{text-align:center;margin-bottom:16px}
.preview-img img{max-width:100%;border:1px solid #eee;border-radius:8px}
.dl-buttons{display:flex;gap:10px;flex-wrap:wrap;justify-content:center}
.dl-btn{display:inline-block;padding:10px 20px;background:#2563eb;color:#fff;border-radius:8px;font-size:14px;transition:.2s}
.dl-btn:hover{background:#1d4ed8;text-decoration:none}
.dl-btn.outline{background:#fff;color:#2563eb;border:1px solid #2563eb}
.dl-btn.outline:hover{background:#f0f4ff}
.code-section{background:#fff;border-radius:12px;padding:24px;box-shadow:0 2px 8px rgba(0,0,0,.06);margin-bottom:24px}
.code-section h2{font-size:20px;margin-bottom:12px;color:#1e3a5f}
.code-header{display:flex;justify-content:space-between;align-items:center;margin-bottom:8px}
.copy-btn{padding:6px 14px;background:#f3f4f6;border:1px solid #ddd;border-radius:6px;cursor:pointer;font-size:13px}
.copy-btn:hover{background:#e5e7eb}
pre{background:#1e293b;color:#e2e8f0;padding:16px;border-radius:8px;overflow-x:auto;font-size:13px;line-height:1.6}
.related{margin-bottom:40px}
.related h2{font-size:20px;margin-bottom:16px;color:#1e3a5f}
.related-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(160px,1fr));gap:12px}
.related-card{background:#fff;border-radius:8px;padding:10px;box-shadow:0 1px 4px rgba(0,0,0,.06);text-align:center;transition:.2s}
.related-card:hover{transform:translateY(-2px);box-shadow:0 4px 12px rgba(0,0,0,.1);text-decoration:none}
.related-card img{width:100%;border-radius:6px;margin-bottom:6px}
.related-card span{font-size:12px;color:#333;display:block}
footer{background:#1e293b;color:#94a3b8;text-align:center;padding:30px 20px;font-size:13px}
footer a{color:#60a5fa}
@media(max-width:600px){h1.page-title{font-size:22px}.dl-buttons{flex-direction:column}.style-tabs{gap:4px}}
'

# ── HTML Page Generator for Individual Templates ──
generate_template_page <- function(t, all_t) {
  cja <- CATS[[t$cat]]$ja
  img_base <- sprintf("%s_%s", t$cat, gsub("-","_",t$id))
  r_code <- make_r_code(t)

  # Related templates (same category, max 8)
  rel <- Filter(function(x) x$cat==t$cat && x$id!=t$id, all_t)
  rel <- head(rel, 8)
  rel_html <- paste(sapply(rel, function(r) {
    ri <- sprintf("%s_%s", r$cat, gsub("-","_",r$id))
    sprintf('<a href="%s.html" class="related-card"><img src="../img/%s.png" alt="%s" loading="lazy"><span>%s</span></a>',
            r$id, ri, r$ja, r$ja)
  }), collapse="\n")

  # Tags
  tag_list <- strsplit(t$tags, ",")[[1]]
  tags_html <- paste(sapply(trimws(tag_list), function(tg) sprintf('<span>%s</span>', tg)), collapse="")

  html <- sprintf('<!DOCTYPE html>
<html lang="ja">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1.0">
<title>%s テンプレート | MedGraph Free</title>
<meta name="description" content="%s 著作権フリー（CC0）の白紙グラフテンプレート。5スタイル・3サイズ。Rコード付き。医学部レポート・発表用。">
<meta name="google-site-verification" content="Wiq_d_MCZ8j5m7XH4dvaZ4jq3i0SqZRQOSwVi4Rr5gU">
<link rel="icon" href="../favicon.svg" type="image/svg+xml">
<link rel="icon" href="../favicon-48.png" sizes="48x48" type="image/png">
<link rel="apple-touch-icon" href="../apple-touch-icon.png">
<link rel="canonical" href="%s/%s/%s.html">
<meta property="og:type" content="article">
<meta property="og:title" content="%s テンプレート | MedGraph Free">
<meta property="og:description" content="%s">
<meta property="og:image" content="%s/img/%s.png">
<meta property="og:url" content="%s/%s/%s.html">
<meta name="twitter:card" content="summary_large_image">
<script type="application/ld+json">
{"@context":"https://schema.org","@type":"ImageObject","name":"%s","description":"%s","contentUrl":"%s/img/%s.png","license":"https://creativecommons.org/publicdomain/zero/1.0/","acquireLicensePage":"%s/"}
</script>
<style>%s</style>
</head>
<body>
<header class="site-header"><div class="inner">
<h1><a href="../">MedGraph Free</a></h1>
<nav><a href="../">HOME</a><a href="../c/%s.html">%s</a></nav>
</div></header>
<div class="breadcrumb"><a href="../">HOME</a> &gt; <a href="../c/%s.html">%s</a> &gt; %s</div>
<main>
<h1 class="page-title">%s</h1>
<p class="desc">%s</p>
<div class="tags">%s</div>
<section class="preview-section">
<div class="switcher-row">
<div class="style-tabs" id="styleTabs">
<button class="active" onclick="swStyle(this,\'standard\')">Standard</button>
<button onclick="swStyle(this,\'minimal\')">Minimal</button>
<button onclick="swStyle(this,\'classic\')">Classic</button>
<button onclick="swStyle(this,\'presentation\')">Presentation</button>
<button onclick="swStyle(this,\'dark\')">Dark</button>
</div>
<div class="style-tabs" id="langTabs">
<button class="active" onclick="swLang(this,\'en\')">English</button>
<button onclick="swLang(this,\'ja\')">日本語</button>
<button onclick="swLang(this,\'none\')">テキストなし</button>
</div>
</div>
<div class="preview-img"><img id="pv" src="../img/%s.png" alt="%s"></div>
<div class="dl-buttons">
<a id="dl1" href="../img/%s.png" download class="dl-btn">Download (800x600)</a>
<a id="dl2" href="../img/%s_wide.png" download class="dl-btn outline">Wide (1200x600)</a>
<a id="dl3" href="../img/%s_square.png" download class="dl-btn outline">Square (700x700)</a>
</div>
</section>
<section class="code-section">
<div class="code-header"><h2>R Code (ggplot2)</h2><button class="copy-btn" onclick="copyCode()">Copy</button></div>
<pre id="rcode">%s</pre>
</section>
<section class="related"><h2>Related Templates</h2><div class="related-grid">%s</div></section>
</main>
<footer>
<p>MedGraph Free &mdash; CC0 (Public Domain). Free for any use.</p>
<p><a href="../">Back to Home</a></p>
</footer>
<script>
const B="%s";
let curLang="",curStyle="";
function swStyle(btn,s){
  document.querySelectorAll("#styleTabs button").forEach(b=>b.classList.remove("active"));
  btn.classList.add("active");
  curStyle=s==="standard"?"":"_"+s;
  upd();
}
function swLang(btn,l){
  document.querySelectorAll("#langTabs button").forEach(b=>b.classList.remove("active"));
  btn.classList.add("active");
  curLang=l==="en"?"":l==="ja"?"_ja":"_notxt";
  upd();
}
function upd(){
  const p="../img/"+B+curLang+curStyle;
  document.getElementById("pv").src=p+".png";
  document.getElementById("dl1").href=p+".png";
  document.getElementById("dl2").href=p+"_wide.png";
  document.getElementById("dl3").href=p+"_square.png";
}
function copyCode(){
  const c=document.getElementById("rcode").textContent;
  navigator.clipboard.writeText(c).then(()=>{
    const b=document.querySelector(".copy-btn");b.textContent="Copied!";
    setTimeout(()=>b.textContent="Copy",2000);
  });
}
</script>
</body>
</html>',
  # title, desc, canonical x3, og:title, og:desc, og:image x2, og:url x2,
  # ld name, ld desc, ld img x2, ld license,
  # css, nav cat x2, breadcrumb cat x2 + name, h1, desc, tags,
  # preview img, alt, dl x3, rcode, related, js base
  t$ja, t$dj,
  SITE_URL, t$cat, t$id,
  t$ja, t$dj,
  SITE_URL, img_base,
  SITE_URL, t$cat, t$id,
  t$ja, t$dj, SITE_URL, img_base, SITE_URL,
  PAGE_CSS,
  t$cat, cja,
  t$cat, cja, t$ja,
  t$ja, t$dj, tags_html,
  img_base, t$ja,
  img_base, img_base, img_base,
  r_code, rel_html,
  img_base)

  dir.create(t$cat, showWarnings=FALSE, recursive=TRUE)
  writeLines(html, file.path(t$cat, paste0(t$id, ".html")), useBytes=TRUE)
}

# ── Category Page Generator ──
generate_category_page <- function(cat_id, cat_templates) {
  cja <- CATS[[cat_id]]$ja
  cen <- CATS[[cat_id]]$en
  dir.create("c", showWarnings=FALSE)

  cards <- paste(sapply(cat_templates, function(t) {
    ib <- sprintf("%s_%s", t$cat, gsub("-","_",t$id))
    sprintf('<a href="../%s/%s.html" class="tcard"><img src="../img/%s.png" alt="%s" loading="lazy"><div class="tname">%s</div><div class="tdesc">%s</div></a>',
            t$cat, t$id, ib, t$ja, t$ja, substr(t$dj, 1, 60))
  }), collapse="\n")

  html <- sprintf('<!DOCTYPE html>
<html lang="ja">
<head>
<meta charset="UTF-8"><meta name="viewport" content="width=device-width,initial-scale=1.0">
<title>%s（%s）グラフテンプレート一覧 | MedGraph Free</title>
<meta name="description" content="%sのレポート用グラフテンプレート一覧。%d種類の著作権フリー白紙テンプレート。Rコード付き。">
<link rel="icon" href="../favicon.svg" type="image/svg+xml">
<link rel="icon" href="../favicon-48.png" sizes="48x48" type="image/png">
<link rel="apple-touch-icon" href="../apple-touch-icon.png">
<link rel="canonical" href="%s/c/%s.html">
<style>
*{margin:0;padding:0;box-sizing:border-box}
body{font-family:"Helvetica Neue",Arial,"Hiragino Sans",sans-serif;background:#f8f9fa;color:#333;line-height:1.7}
a{color:#2563eb;text-decoration:none}
.hd{background:linear-gradient(135deg,#1e3a5f,#2563eb);color:#fff;padding:12px 0}
.hd .inner{max-width:1200px;margin:0 auto;padding:0 20px;display:flex;align-items:center;justify-content:space-between}
.hd h1{font-size:18px}.hd h1 a{color:#fff}.hd nav a{color:rgba(255,255,255,.85);margin-left:20px;font-size:14px}
.bc{max-width:1200px;margin:20px auto 0;padding:0 20px;font-size:13px;color:#666}
main{max-width:1200px;margin:20px auto 40px;padding:0 20px}
h1.cat-title{font-size:28px;margin:16px 0 8px;color:#1e3a5f}
.count{color:#666;margin-bottom:20px;font-size:14px}
.grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(220px,1fr));gap:16px}
.tcard{background:#fff;border-radius:10px;overflow:hidden;box-shadow:0 2px 8px rgba(0,0,0,.06);transition:.2s}
.tcard:hover{transform:translateY(-3px);box-shadow:0 6px 16px rgba(0,0,0,.1);text-decoration:none}
.tcard img{width:100%%}
.tname{padding:8px 12px 2px;font-weight:bold;font-size:14px;color:#1e3a5f}
.tdesc{padding:2px 12px 10px;font-size:12px;color:#666}
footer{background:#1e293b;color:#94a3b8;text-align:center;padding:30px 20px;font-size:13px}
</style>
</head>
<body>
<header class="hd"><div class="inner"><h1><a href="../">MedGraph Free</a></h1><nav><a href="../">HOME</a></nav></div></header>
<div class="bc"><a href="../">HOME</a> &gt; %s</div>
<main>
<h1 class="cat-title">%s (%s)</h1>
<p class="count">%d templates available</p>
<div class="grid">%s</div>
</main>
<footer><p>MedGraph Free &mdash; CC0 (Public Domain)</p></footer>
</body></html>',
  cja, cen, cja, length(cat_templates), SITE_URL, cat_id,
  cja, cja, cen, length(cat_templates), cards)

  writeLines(html, file.path("c", paste0(cat_id, ".html")), useBytes=TRUE)
}

# ── Main Index Page Generator ──
generate_index <- function(all_t) {
  # Group by category
  by_cat <- split(all_t, sapply(all_t, function(x) x$cat))
  total <- length(all_t)
  total_dl <- total * 45  # 5 styles * 3 sizes * 3 languages

  # Category cards (text only, no emoji)
  cat_cards <- paste(sapply(names(by_cat), function(cn) {
    ts <- by_cat[[cn]]
    cja <- CATS[[cn]]$ja
    sprintf('<a href="c/%s.html" class="cat-card"><div class="cat-name">%s</div><div class="cat-count">%d templates</div></a>',
            cn, cja, length(ts))
  }), collapse="\n")

  # All template cards grouped by category
  all_cards <- paste(sapply(names(by_cat), function(cn) {
    cja <- CATS[[cn]]$ja
    ts <- by_cat[[cn]]
    tcards <- paste(sapply(ts, function(t) {
      ib <- sprintf("%s_%s", t$cat, gsub("-","_",t$id))
      sprintf('<a href="%s/%s.html" class="card"><img src="img/%s.png" alt="%s" loading="lazy"><div class="card-body"><h3>%s</h3><p>%s</p></div></a>',
              t$cat, t$id, ib, t$ja, t$ja, substr(t$dj,1,50))
    }), collapse="\n")
    sprintf('<section class="cat-section" id="%s"><h2><a href="c/%s.html">%s (%d)</a></h2><div class="card-grid">%s</div></section>',
            cn, cn, cja, length(ts), tcards)
  }), collapse="\n")

  # Navigation links
  nav_links <- paste(sapply(names(by_cat), function(cn) {
    sprintf('<a href="#%s">%s</a>', cn, CATS[[cn]]$ja)
  }), collapse="")

  html <- sprintf('<!DOCTYPE html>
<html lang="ja" prefix="og: https://ogp.me/ns#">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1.0">
<title>MedGraph Free | 医学部生のための無料グラフテンプレート集【著作権フリー・Rコード付き】</title>
<meta name="description" content="医学部のレポート・発表用グラフテンプレートを無料提供。全%d分野%d種類・%dダウンロード可能。著作権フリー（CC0）。R+ggplot2コード付き。5スタイル×3サイズ。">
<meta name="keywords" content="医学部,グラフテンプレート,レポート,無料,著作権フリー,CC0,R,ggplot2,医学生,テンプレート">
<meta name="google-site-verification" content="Wiq_d_MCZ8j5m7XH4dvaZ4jq3i0SqZRQOSwVi4Rr5gU">
<link rel="icon" href="favicon.svg" type="image/svg+xml">
<link rel="icon" href="favicon-48.png" sizes="48x48" type="image/png">
<link rel="apple-touch-icon" href="apple-touch-icon.png">
<link rel="canonical" href="%s/">
<meta property="og:type" content="website">
<meta property="og:title" content="MedGraph Free | 医学部生のための無料グラフテンプレート集">
<meta property="og:description" content="全%d分野%d種類の白紙グラフテンプレートを無料提供。著作権フリー（CC0）・Rコード付き。">
<meta property="og:url" content="%s/">
<meta property="og:image" content="%s/og-image.png">
<meta name="twitter:card" content="summary_large_image">
<script type="application/ld+json">
{"@context":"https://schema.org","@type":"WebSite","name":"MedGraph Free","url":"%s/","description":"医学部生のためのグラフテンプレート集。全%d分野%d種類。CC0。Rコード付き。","inLanguage":"ja"}
</script>
<style>
*{margin:0;padding:0;box-sizing:border-box}
body{font-family:"Helvetica Neue",Arial,"Hiragino Sans",sans-serif;background:#f8f9fa;color:#333;line-height:1.7}
a{color:#2563eb;text-decoration:none}a:hover{text-decoration:underline}
.hero{background:linear-gradient(135deg,#0f172a,#1e3a5f 40%%,#2563eb);color:#fff;padding:60px 20px;text-align:center}
.hero h1{font-size:36px;margin-bottom:10px}.hero p{font-size:16px;color:rgba(255,255,255,.85);max-width:700px;margin:0 auto 20px}
.hero .stats{display:flex;gap:30px;justify-content:center;flex-wrap:wrap}
.hero .stat{text-align:center}.hero .stat .num{font-size:32px;font-weight:bold}.hero .stat .lbl{font-size:13px;color:rgba(255,255,255,.7)}
.nav-bar{background:#fff;border-bottom:1px solid #e5e7eb;padding:10px 0;position:sticky;top:0;z-index:100;overflow-x:auto;white-space:nowrap}
.nav-bar .inner{max-width:1200px;margin:0 auto;padding:0 20px;display:flex;gap:6px;flex-wrap:nowrap}
.nav-bar .inner::after{content:"";min-width:20px;flex-shrink:0}
.nav-bar a{padding:6px 12px;border-radius:6px;font-size:12px;color:#475569;white-space:nowrap}
.nav-bar a:hover{background:#f0f4ff;text-decoration:none}
.cats-grid{max-width:1200px;margin:30px auto;padding:0 20px;display:grid;grid-template-columns:repeat(auto-fill,minmax(150px,1fr));gap:12px}
.cat-card{background:#fff;border-radius:10px;padding:18px 16px;text-align:center;box-shadow:0 2px 6px rgba(0,0,0,.05);transition:.2s;border-left:3px solid #2563eb}
.cat-card:hover{transform:translateY(-2px);box-shadow:0 4px 12px rgba(0,0,0,.1);text-decoration:none}
.cat-name{font-size:14px;font-weight:bold;color:#1e3a5f}.cat-count{font-size:11px;color:#666;margin-top:2px}
.request-section{max-width:1200px;margin:40px auto;padding:0 20px;text-align:center}
.request-card{background:#fff;border-radius:12px;padding:30px;box-shadow:0 2px 8px rgba(0,0,0,.06)}
.request-card h2{font-size:20px;color:#1e3a5f;margin-bottom:8px}
.request-card p{font-size:14px;color:#666;margin-bottom:16px}
.request-btn{display:inline-block;padding:12px 28px;background:#2563eb;color:#fff;border-radius:8px;font-size:15px;transition:.2s}
.request-btn:hover{background:#1d4ed8;text-decoration:none}
.cat-section{max-width:1200px;margin:30px auto;padding:0 20px}
.cat-section h2{font-size:22px;color:#1e3a5f;margin-bottom:14px;padding-bottom:6px;border-bottom:2px solid #2563eb}
.cat-section h2 a{color:#1e3a5f}
.card-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(200px,1fr));gap:14px;margin-bottom:30px}
.card{background:#fff;border-radius:10px;overflow:hidden;box-shadow:0 2px 6px rgba(0,0,0,.05);transition:.2s}
.card:hover{transform:translateY(-2px);box-shadow:0 6px 16px rgba(0,0,0,.1);text-decoration:none}
.card img{width:100%%}
.card-body{padding:8px 12px 12px}
.card-body h3{font-size:13px;color:#1e3a5f;margin-bottom:3px}
.card-body p{font-size:11px;color:#666}
footer{background:#1e293b;color:#94a3b8;text-align:center;padding:40px 20px;font-size:13px}
footer a{color:#60a5fa}
.search-box{max-width:500px;margin:20px auto 0;position:relative}
.search-box input{width:100%%;padding:12px 16px;border-radius:8px;border:none;font-size:15px;outline:none}
@media(max-width:600px){.hero h1{font-size:24px}.hero .stat .num{font-size:24px}}
</style>
</head>
<body>
<section class="hero">
<h1>MedGraph Free</h1>
<p>医学部生のための無料グラフテンプレート集。著作権フリー（CC0）でレポート・発表にそのまま使えます。</p>
<div class="stats">
<div class="stat"><div class="num">%d</div><div class="lbl">Templates</div></div>
<div class="stat"><div class="num">%d</div><div class="lbl">Downloads</div></div>
<div class="stat"><div class="num">%d</div><div class="lbl">Categories</div></div>
<div class="stat"><div class="num">5</div><div class="lbl">Styles</div></div>
</div>
<div class="search-box"><input type="text" id="search" placeholder="Search templates..." oninput="filterCards(this.value)"></div>
</section>
<nav class="nav-bar"><div class="inner">%s</div></nav>
<div class="cats-grid">%s</div>
%s
<section class="request-section">
<div class="request-card">
<h2>テンプレートのリクエスト</h2>
<p>欲しいグラフテンプレートがありましたらお気軽にリクエストください。</p>
<a href="https://github.com/S-Yus/medical_images/issues/new?title=Template+Request:&labels=request&body=リクエストするテンプレート名：%%0A分野：%%0A用途・説明：" class="request-btn" target="_blank" rel="noopener">リクエストを送る</a>
</div>
</section>
<footer>
<p>MedGraph Free &mdash; All templates are CC0 (Public Domain). Free for any use including commercial.</p>
<p>Generated with R + ggplot2. <a href="https://github.com/S-Yus/medical_images">GitHub</a></p>
</footer>
<script>
function filterCards(q){
  q=q.toLowerCase();
  document.querySelectorAll(".card").forEach(c=>{
    const t=c.textContent.toLowerCase();
    c.style.display=t.includes(q)?"":"none";
  });
  document.querySelectorAll(".cat-section").forEach(s=>{
    const cards=s.querySelectorAll(".card");
    let any=false;
    cards.forEach(c=>{if(c.style.display!=="none")any=true;});
    s.style.display=(any||!q)?"":"none";
  });
  document.querySelectorAll(".cat-card").forEach(c=>{
    if(!q){c.style.display="";return;}
    c.style.display=c.textContent.toLowerCase().includes(q)?"":"none";
  });
}
</script>
</body></html>',
  length(by_cat), total, total_dl, SITE_URL,
  length(by_cat), total, SITE_URL, SITE_URL, SITE_URL,
  length(by_cat), total,
  total, total_dl, length(by_cat),
  nav_links, cat_cards, all_cards)

  writeLines(html, "index.html", useBytes=TRUE)
  cat(sprintf("index.html generated (%d categories, %d templates)\n", length(by_cat), total))
}

# ── Sitemap Generator ──
generate_sitemap <- function(all_t) {
  urls <- sprintf('  <url><loc>%s/</loc><changefreq>weekly</changefreq><priority>1.0</priority></url>', SITE_URL)

  # Category pages
  cat_urls <- sapply(unique(sapply(all_t, function(x) x$cat)), function(cn) {
    sprintf('  <url><loc>%s/c/%s.html</loc><changefreq>weekly</changefreq><priority>0.8</priority></url>', SITE_URL, cn)
  })

  # Template pages with image
  tmpl_urls <- sapply(all_t, function(t) {
    ib <- sprintf("%s_%s", t$cat, gsub("-","_",t$id))
    sprintf('  <url>\n    <loc>%s/%s/%s.html</loc>\n    <changefreq>monthly</changefreq>\n    <priority>0.6</priority>\n    <image:image><image:loc>%s/img/%s.png</image:loc><image:title>%s</image:title></image:image>\n  </url>',
            SITE_URL, t$cat, t$id, SITE_URL, ib, t$ja)
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

cat(sprintf("\n=== BUILD COMPLETE ===\nTime: %s\n", Sys.time()))
