iso.polygon <- function(sources, mixture, TEF = 0, its = 1500, res = 250){
  #*********************************************************#
  #simulation paramater setting
  # The result of hot map is highly sensitive to the max and min of sources, set the iso_1_max...manullay is better than auto set
  max.1.n <- which(sources[, 1] == max(sources[, 1]))
  min.1.n <- which(sources[, 1] == min(sources[, 1]))
  max.2.n <- which(sources[, 3] == max(sources[, 3]))
  min.2.n <- which(sources[, 3] == min(sources[, 3]))
  iso_1_max <- qnorm(.999999, mean = sources[max.1.n, 1], sd = sources[max.1.n, 2]) * 1.5
  iso_1_min <- qnorm(.1, mean = sources[min.1.n, 1], sd = sources[min.1.n, 2]) * 1.5
  iso_2_max <- qnorm(.999999, mean = sources[max.2.n, 3], sd = sources[max.2.n, 4]) *0.2
  iso_2_min <- qnorm(.1, mean = sources[min.2.n, 3], sd = sources[min.2.n, 4]) * 1.5
  #return(list(iso_1_min, iso_1_max, iso_2_min, iso_2_max, max.2.n, min.2.n))
  step_1 <- (iso_1_max - iso_1_min)/(res - 1)
  step_2 <- (iso_2_max - iso_2_min)/(res - 1)
  seq_1 <- seq(iso_1_min, iso_1_max, by = step_1)
  seq_2 <- seq(iso_2_min, iso_2_max, by = step_2)
  m_x <- outer(seq_2*0, seq_1, FUN = "+")
  m_y <- outer(seq_2, seq_1*0, FUN = "+")
  m_y_f <- m$y[res:1, ]
  m <- list(x = m_x, y = m_y, y_f = m_y_f)
  Par_values <- array(0, c(its, (nrow(sources)*4 + 3)))
  p <- array(0, (c(its, nrow(mixture))))
  mix_reg <- array(0, c(res, res))
  polystate <- array(0, c(its, 10))
  #*********************************************************#
  #run the MC simulation to generate random isotope polygons#
  for(i in 1 : its){
    v <- array(0, c(nrow(sources), 2))
    f <- array(0, c(nrow(TEF), 2))
    for(j in 1:nrow(sources)){
      v[j, 1] <- rnorm(1, mean = sources[j, 1], sd = sources[j, 2])
      v[j, 2] <- rnorm(1, mean = sources[j, 3], sd = sources[j, 4])
      f[j, 1] <- rnorm(1, mean = TEF[j, 1], sd = TEF[j, 2])
      f[j, 2] <- rnorm(1, mean = TEF[j, 3], sd = TEF[j, 4])
    }
    V <- v + f
    hull <- chull(V)
    hull_a <- append(hull, hull[1])
    P <- point.in.polygon(mixture[, 1], mixture[, 2], V[hull_a, 1], V[hull_a, 2])
    P_n <- as.numeric(P)
    p[i, ] <- P_n
    poly_a <- areapl(V[hull_a, ])
    m_r <- point.in.polygon(m$x, m$y_f, V[hull_a, 1], V[hull_a, 2])
    m_r_s <- matrix(m_r, nrow = res, byrow = F)
    m_r_s[m_r_s > 1] <- 1
    mix_reg <- mix_reg + m_r_s
    vals <- c(v[, 1], v[, 2], f[, 1], f[, 2], 0, 0, 0)
    Par_values[i, ] <- vals
    Par_values[i, ncol(Par_values) - 2] <- poly_a
    Par_values[i, ncol(Par_values) - 1] <- i
    Par_values[i, ncol(Par_values)] <- var(Par_values[1:i, ncol(Par_values) - 2])
    if (i %% 10 == 0) cat(paste("iteration", i, "\n"))
  }
    #return(list(hull_a = hull_a, V = V, P = P_n, m_r = m_r, m = m, m_r_s = m_r_s, mix_reg = mix_reg))
  #*********************************************************#
  #Draw the figure#
  #return(list(iso_1_min, iso_1_max, iso_2_min, iso_2_max))
  cat("Simulation is done! Drawing figure, press <ENTER> to continue\n")
  
  readline()
  invisible()
  #FIGURE 1: variace of polygon
  cat("Figure 1: variance of polygon during the simulation progress\n")
  Iterations <- Par_values[, ncol(Par_values) - 1]
  Variance <- Par_values[, ncol(Par_values)]
  plot(Iterations, Variance, type = "n", main = "Variance of polygon area")
  lines(Iterations, Variance, lty = 1, lwd = 1.5, col = "red")
  cat("Press <ENTER> to continue\n")
  readline()
  invisible()
  #FIGURE 2: proportion of iteration that inside the polygon
  cat("Figure 2: proportion of iteration that inside the polygon\n")
  p[p > 1] <- 1
  Probabilities <- colSums(p)/its
  windows()
  barplot(Probabilities, xlab = "Consumer", main = "Proportion of iteration that inside the polygon", ylab = "Proportion of iteration", ylim = c(0, 1), 
          names.arg = seq(1, nrow(mixture), by = 1), col = "blue")
  cat("Press <ENTER> to continue\n")
  readline()
  invisible()
  #FIGURE 3: mixing region hot map
  cat("Figure 3 : mixing region hot map with colors\n")
  mix_reg <- mix_reg/its
  mix_reg[mix_reg == 0] <- NA
  mix_regt <- t(mix_reg[ncol(mix_reg) : 1, ])
  windows()
  image(seq_1, seq_2, mix_regt, main = "Mixing region hot map", col = colorRampPalette(c("blue", "light blue", "green", "light green", "yellow", "red"))(100), 
        xlab = colnames(sources)[1], ylab = colnames(sources)[3], useRaster = T)
  cont <- c(0.05, seq(0.1, 1, by = .1))
  contour(seq_1, seq_2, mix_regt, levels = cont, add = T, drawlabels = F, lwd = 1.9)
  sources_TEF <- sources + TEF
  points(sources_TEF[, 1], sources_TEF[, 3], col = "orangered", pch = 4, lwd = 2.5, cex = 2.5)
  points(mixture, pch = 19, cex = 1.3)
  windows()  #create colour bar for figure 3
  #cust_color <- colorRampPalette(c("blue", "light blue", "green", "light green", "yellow", "red"))
  z <- matrix(1:100, nrow=1)
  x <- 1
  y <- seq(0,1,len=100)
  image(x,y,z,col=colorRampPalette(c("blue", "light blue", "green", "light green", "yellow", "red"))(100), 
        xaxt="n", xlab="", ylab="", useRaster=TRUE, bty="n", las=1)
  cat("Press <ENTER> to continue\n")
  readline()
  invisible()
  #FIGURE 4: mixing region hot map in white and black
  cat("Figure 4 : mixing region hot map in white and black\n")
  windows()
  plot(seq_1, seq_2, typ = "n", xlab = colnames(sources)[1], ylab = colnames(sources)[3])
  cont = c(.01, seq(.1, 1, by = .1))
  contour(seq_1, seq_2, mix_regt, levels = cont, add = T, drawlabels = F, lwd = 1.9, main = "Mixing region coutour map")
  points(sources_TEF[, 1], sources_TEF[, 3], col = "black", pch = 4, lwd = 2, cex = 1.5)
  points(mixture, pch = 19, cex = 1.3)
  cat("Press <ENTER> to continue\n")
  readline()
  invisible()
  #FIGURE 5: Bi-plot with single 95% contour line
  cat("Figure 4 : Bi-plot with single 95% contour line\n")
  windows()
  plot(seq_1, seq_2, type = "n", xlab = colnames(sources)[1], ylab = colnames(sources)[3])
  cont <- c(.05)
  contour(seq_1, seq_2, mix_regt, levels = cont, add = T, drawlabels = F, lwd = 1.9)
  points(sources_TEF[, 1], sources_TEF[, 3], col = "black", pch = 4, lwd = 2, cex = 1.5)
  arrows(sources_TEF[,1]-sources_TEF[,2], sources_TEF[,3], #horizontal error bars (SD)
         sources_TEF[,1]+sources_TEF[,2], sources_TEF[,3], length=0, angle=90, code=3)
  arrows(sources_TEF[,1], sources_TEF[,3]-sources_TEF[,4], #vertical error bars (SD)
         sources_TEF[,1], sources_TEF[,3]+sources_TEF[,4], length=0, angle=90, code=3)
  points(mixture, pch = 19, cex = 1.3)
  labels <- paste("S", 1:nrow(sources), sep = ".")
  text(sources_TEF[,1], sources_TEF[,3], labels=labels, pos=3)
  cat("Press <ENTER> to continue\n")
  readline()
  invisible()
  
  #*********************************************************#
  #Write data to file#
  answer <- readline("Do you want to write simulation result to file?, Y/N: ")
  if(answer == "Y"){
    p_a <- rbind(p, Probabilities)
    f_consumer <- "Consumer result.csv"
    write.table(p_a, file = f_consumer, sep = ",", row.names = F)
    col_names <- c(rep("d13C",nrow(sources)),rep("d15N",nrow(sources)),
                   rep("13C_TEF",nrow(sources)),rep("15N_TEF",nrow(sources)),
                   "Poly_Area","Iteration","Variance")
    col_nums <- c(rep(1:nrow(sources),4),0,0,0)
    col_n <- paste(col_names, col_nums)
    f_para <- "Parameter_Values.csv"
    write.table(Par_values, file = f_para, sep = ",", row.names=FALSE, col.names=col_n)
    cat("WRITING....OK!\n")
    cat("Thanks for using, exit...\n")
  }
  else
    cat("Thanks for using, exit...\n")
  #return(mix_reg)
  
}
