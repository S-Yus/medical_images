#!/usr/bin/env Rscript
# Generate favicon PNGs, apple-touch-icon, and OG image
library(ggplot2)
library(grid)

cat("=== Generating brand assets ===\n")

# ── Color palette ──
bg_dark  <- "#1e3a5f"
bg_light <- "#2563eb"
accent   <- "#60a5fa"
white    <- "#ffffff"

# ── Helper: draw the logo icon ──
draw_logo <- function(size, filename, padding=0.15) {
  png(filename, width=size, height=size, bg="transparent", res=150)
  grid.newpage()

  p <- padding
  # Background rounded rect
  grid.roundrect(
    x=0.5, y=0.5, width=1-p*2, height=1-p*2,
    r=unit(0.15, "snpc"),
    gp=gpar(fill=bg_dark, col=NA)
  )
  # Gradient overlay (simulated with semi-transparent rect)
  grid.roundrect(
    x=0.55, y=0.45, width=0.9-p*2, height=0.9-p*2,
    r=unit(0.15, "snpc"),
    gp=gpar(fill=bg_light, col=NA, alpha=0.4)
  )

  # Grid lines
  for (yy in c(0.35, 0.45, 0.55, 0.65)) {
    grid.lines(x=c(0.2+p, 0.85-p), y=c(yy, yy),
               gp=gpar(col=alpha(white, 0.08), lwd=1))
  }
  # Axes
  grid.lines(x=c(0.2+p, 0.85-p), y=c(0.25+p, 0.25+p),
             gp=gpar(col=alpha(white, 0.2), lwd=2))
  grid.lines(x=c(0.2+p, 0.2+p), y=c(0.25+p, 0.8-p),
             gp=gpar(col=alpha(white, 0.2), lwd=2))

  # Sigmoid curve (dose-response style)
  xx <- seq(0.22+p, 0.83-p, length.out=80)
  # Normalize to 0-1 range for sigmoid
  xn <- (xx - min(xx)) / (max(xx) - min(xx))
  yy <- 0.27+p + (0.72-p*2) * (1 / (1 + exp(-10*(xn - 0.5))))
  grid.lines(x=xx, y=yy,
             gp=gpar(col=white, lwd=4, lineend="round", linejoin="round"))
  # Glow effect
  grid.lines(x=xx, y=yy,
             gp=gpar(col=alpha(accent, 0.5), lwd=7, lineend="round", linejoin="round"))

  # Medical cross (top right)
  cx <- 0.75; cy <- 0.78
  cw <- 0.08; ch <- 0.025
  grid.rect(x=cx, y=cy, width=cw, height=ch,
            gp=gpar(fill=alpha(white, 0.9), col=NA))
  grid.rect(x=cx, y=cy, width=ch, height=cw,
            gp=gpar(fill=alpha(white, 0.9), col=NA))

  dev.off()
  cat(sprintf("  %s (%dx%d)\n", filename, size, size))
}

# ── Generate favicon PNGs ──
draw_logo(32, "favicon-32.png", 0.05)
draw_logo(48, "favicon-48.png", 0.05)
draw_logo(180, "apple-touch-icon.png", 0.08)
draw_logo(192, "icon-192.png", 0.08)
draw_logo(512, "icon-512.png", 0.08)

# ── Generate OG Image (1200x630) ──
cat("  Generating OG image...\n")
png("og-image.png", width=1200, height=630, res=150)
grid.newpage()

# Background
grid.rect(gp=gpar(fill=bg_dark, col=NA))
# Gradient overlay
grid.rect(x=0.6, y=0.4, width=1.2, height=1,
          gp=gpar(fill=bg_light, col=NA, alpha=0.3))

# Subtle grid
for (yy in seq(0.15, 0.85, by=0.1)) {
  grid.lines(x=c(0.05, 0.95), y=c(yy, yy),
             gp=gpar(col=alpha(white, 0.04), lwd=1))
}
for (xx in seq(0.1, 0.9, by=0.1)) {
  grid.lines(x=c(xx, xx), y=c(0.05, 0.95),
             gp=gpar(col=alpha(white, 0.04), lwd=1))
}

# Decorative curves (background)
for (offset in c(-0.05, 0.05, 0.15)) {
  xx <- seq(0.02, 0.98, length.out=100)
  xn <- (xx - min(xx)) / (max(xx) - min(xx))
  yy <- 0.2 + offset + 0.25 * (1 / (1 + exp(-8*(xn - 0.4+offset))))
  grid.lines(x=xx, y=yy,
             gp=gpar(col=alpha(accent, 0.12), lwd=3, lineend="round"))
}

# Main title
grid.text("MedGraph Free",
          x=0.5, y=0.62,
          gp=gpar(col=white, fontsize=42, fontface="bold",
                  fontfamily="Helvetica"))
# Subtitle
grid.text(
  "Medical Graph Templates for Students",
  x=0.5, y=0.48,
  gp=gpar(col=alpha(white, 0.8), fontsize=18, fontfamily="Helvetica"))
# Stats line
grid.text(
  "407 Templates  |  24 Categories  |  CC0 License  |  R Code Included",
  x=0.5, y=0.35,
  gp=gpar(col=alpha(accent, 0.9), fontsize=13, fontfamily="Helvetica"))

# Medical cross
cx <- 0.5; cy <- 0.82
grid.rect(x=cx, y=cy, width=0.035, height=0.012,
          gp=gpar(fill=alpha(white, 0.7), col=NA))
grid.rect(x=cx, y=cy, width=0.012, height=0.035,
          gp=gpar(fill=alpha(white, 0.7), col=NA))

# URL
grid.text("med-graph.com", x=0.5, y=0.12,
          gp=gpar(col=alpha(white, 0.5), fontsize=14, fontfamily="Helvetica"))

dev.off()
cat("  og-image.png (1200x630)\n")

cat("=== Brand assets complete ===\n")
