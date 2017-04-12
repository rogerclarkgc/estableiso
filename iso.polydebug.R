#need library sp and splancs
iso.polydebug <- function(sources, mixture, TEF = 0, its = 1500, res = 250){
  #*********************************************************#
  #simulation paramater setting
  # The result of hot map is highly sensitive to the max and min of sources, set the iso_1_max...manullay is better than auto set
  max.1.n <- which(sources[, 1] == max(sources[, 1]))
  min.1.n <- which(sources[, 1] == min(sources[, 1]))
  max.2.n <- which(sources[, 3] == max(sources[, 3]))
  min.2.n <- which(sources[, 3] == min(sources[, 3]))
  #iso_1_max <- qnorm(.999999, mean = sources[max.1.n, 1], sd = sources[max.1.n, 2])
  iso_1_max <- 30
  #iso_1_min <- qnorm(.1, mean = sources[min.1.n, 1], sd = sources[min.1.n, 2])
  iso_1_min <- -8
  #iso_2_max <- qnorm(.999999, mean = sources[max.2.n, 3], sd = sources[max.2.n, 4])
  iso_2_max <- 24
  #iso_2_min <- qnorm(.1, mean = sources[min.2.n, 3], sd = sources[min.2.n, 4])
  iso_2_min <- -2
  #return(list(iso_1_min, iso_1_max, iso_2_min, iso_2_max, max.2.n, min.2.n))
  step_1 <- (iso_1_max - iso_1_min)/(res - 1)
  step_2 <- (iso_2_max - iso_2_min)/(res - 1)
  seq_1 <- seq(iso_1_min, iso_1_max, by = step_1)
  seq_2 <- seq(iso_2_min, iso_2_max, by = step_2)
  m_x <- outer(seq_2*0, seq_1, FUN = "+")
  m_y <- outer(seq_2, seq_1*0, FUN = "+")
  #m_y_f <- m$y[res:1, ]
  m_y_f <- m_y[res:1, ]
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
  return(list(hull_a = hull_a, V = V, P = P_n, m_r = m_r, m = m, m_r_s = m_r_s, mix_reg = mix_reg))
}