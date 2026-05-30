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
  n <- max(3L,as.integer(t$yr[2]))
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
  ng <- max(2L,as.integer(t$xr[2]))
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
<div class="style-tabs" id="sizeTabs">
<button class="active" onclick="swSize(this,\'standard\')">800 x 600</button>
<button onclick="swSize(this,\'wide\')">1200 x 600</button>
<button onclick="swSize(this,\'square\')">700 x 700</button>
</div>
</div>
<div class="preview-img"><img id="pv" src="../img/%s.png" alt="%s"></div>
<div class="dl-buttons">
<a id="dl1" href="../img/%s.png" download class="dl-btn">Download</a>
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
let curLang="",curStyle="",curSize="";
function swStyle(btn,s){
  document.querySelectorAll("#styleTabs button").forEach(b=>b.classList.remove("active"));
  btn.classList.add("active");
  curStyle=s==="standard"?"":"_"+s;upd();
}
function swLang(btn,l){
  document.querySelectorAll("#langTabs button").forEach(b=>b.classList.remove("active"));
  btn.classList.add("active");
  curLang=l==="en"?"":l==="ja"?"_ja":"_notxt";upd();
}
function swSize(btn,z){
  document.querySelectorAll("#sizeTabs button").forEach(b=>b.classList.remove("active"));
  btn.classList.add("active");
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
  img_base,
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
.no-results{display:none;text-align:center;padding:60px 20px;max-width:1200px;margin:0 auto}
.no-results.show{display:block}
.no-results p{font-size:18px;color:#666;margin-bottom:8px}
.no-results .hint{font-size:14px;color:#999}
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
<div class="search-box"><input type="text" id="search" placeholder="テンプレートを検索..." oninput="filterCards(this.value)"></div>
</section>
<nav class="nav-bar"><div class="inner">%s</div></nav>
<div class="cats-grid">%s</div>
<div class="no-results" id="noResults"><p>該当するテンプレートが見つかりませんでした</p><p class="hint">別のキーワードで検索するか、<a href="https://github.com/S-Yus/medical_images/issues/new?title=Template+Request:&labels=request" target="_blank">リクエスト</a>をお送りください</p></div>
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
  let total=0;
  document.querySelectorAll(".card").forEach(c=>{
    const t=c.textContent.toLowerCase();
    const v=t.includes(q);
    c.style.display=v?"":"none";
    if(v)total++;
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
  const nr=document.getElementById("noResults");
  if(nr) nr.classList.toggle("show",q&&total===0);
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
