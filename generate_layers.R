#all sizes in mm
library(plotrix)
library(gtools)

draw.mounting.slots <- function(diameter) {
  draw.circle(35, 35, r=diameter / 2)
  draw.circle(35, 560, r=diameter / 2)
  draw.circle(560, 35, r=diameter / 2)
  draw.circle(560, 560, r=diameter / 2)
}


draw.grid <- function(sizes, r1, r2, draw.function) {
  for (i in 0:19)
    for (j in 0:19) {
      if (sizes[i * 20 + j + 1] == 1)
        r <- r1
      else
        r <- r2

      draw.function(60 + i * 200 / 8, 60 + j * 200 / 8, r)

      }
}


draw.outer.frame <- function() {
  x1 <- 40
  y1 <- 0

  x2 <- 555
  y2 <- 0

  x3 <- 595
  y3 <- 40

  x4 <- 595
  y4 <- 555

  x5 <- 555
  y5 <- 595

  x6 <- 40
  y6 <- 595

  x7 <- 0
  y7 <- 555

  x8 <- 0
  y8 <- 40

  lines(c(x1,x2,x3,x4,x5,x6,x7,x8, x1), c(y1,y2,y3,y4,y5,y6,y7,y8, y1))
}

draw.inner.frame <- function() {
  x1 <- 50
  y1 <- 50

  x2 <- 545
  y2 <- 50

  x3 <- 545
  y3 <- 545

  x4 <- 50
  y4 <- 545

  lines(c(x1,x2,x3,x4, x1), c(y1,y2,y3,y4, y1))
}


draw.hex <- function(x,y,r_incircle) {
  #draw a hexagon with a center at x,y, and with an inscribed circle (incirce) radius r_incircle
  x1 <- x - r_incircle / sqrt(3)
  y1 <- y + r_incircle

  x2 <- x + r_incircle / sqrt(3)
  y2 <- y + r_incircle

  x3 <- x + 2 * r_incircle / sqrt(3)
  y3 <- y

  x4 <- x + r_incircle / sqrt(3)
  y4 <- y - r_incircle

  x5 <- x - r_incircle / sqrt(3)
  y5 <- y - r_incircle

  x6 <- x - 2 * r_incircle / sqrt(3)
  y6 <- y

  lines(c(x1,x2,x3,x4,x5,x6, x1), c(y1,y2,y3,y4,y5,y6, y1))
}

open.pdf <- function(title, width_in_mm, height_in_mm, margin_in_mm) {
  pdf(file=title, width=(width_in_mm + 2 * margin_in_mm) / 25.4, height=(height_in_mm + 2 * margin_in_mm) / 25.4 )   #units: inches
  plot.new()
  par(mai=c(margin_in_mm / 25.4, margin_in_mm / 25.4, margin_in_mm / 25.4, margin_in_mm / 25.4))  #mai - margins in inces
  plot.window(c(0, width_in_mm), c(0, height_in_mm), asp=1, xaxs="i", yaxs="i")  #="i" to avoid scale by 4%
}

close.pdf <- function() {
  dev.off()
}


open.pdf("12mm_frame.pdf", 595, 595, 10)
draw.outer.frame()
draw.inner.frame()
draw.mounting.slots(2)
close.pdf()

sizes <- c(rep(0,200), rep(1,200))
permuted_sizes <- gtools::permute(sizes)

open.pdf("3mm_inner_layer.pdf", 595, 595, 10)
draw.outer.frame()
draw.grid(permuted_sizes, 5, 6, plotrix::draw.circle)
draw.mounting.slots(2)
close.pdf()

open.pdf("3mm_outer_layer.pdf", 595, 595, 10)
draw.outer.frame()
draw.grid(permuted_sizes, 5, 6, plotrix::draw.circle)
draw.mounting.slots(5)
close.pdf()

open.pdf("4mm_layer.pdf", 595, 595, 10)
draw.outer.frame()
draw.grid(permuted_sizes, 13/2, 17/2, draw.hex)
draw.mounting.slots(5)
close.pdf()
