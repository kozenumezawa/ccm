library(rEDM)

determineEmbeddingDimension <- function(data) {
  lib <- c(1, 30)
  pred <- c(50, 90)  
  simplex_output <- simplex(data, lib, pred)
  plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)")
}

predictionDeacy <- function(data, Em) {
  lib <- c(1, 30)
  pred <- c(50, 90)  
  simplex_output <- simplex(data, lib, pred, E = Em, tp = 1:10)
  par(mar = c(4, 4, 1, 1))
  plot(simplex_output$tp, simplex_output$rho, type = "l", xlab = "Time to Prediction (tp)", ylab = "Forecast Skill (rho)")
}

identifyingNonlinearity <- function(data, Em) {
  lib <- c(1, 30)
  pred <- c(50, 90)  
  smap_output <- s_map(data, lib, pred, E=Em)
  par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
  plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")
}

drawCCM <- function(Accm, Bccm, Em, TAU) {
  Accm_Bccm <- data.frame(Accm=Accm, Bccm=Bccm)
  Accm_xmap_Bccm <- ccm(Accm_Bccm, E = Em, lib_column = "Accm", tau = TAU,
                        target_column = "Bccm", lib_sizes = seq(10, 200, by = 10), random_libs = TRUE)
  Bccm_xmap_Accm <- ccm(Accm_Bccm, E = Em, lib_column = "Bccm", tau = TAU,
                        target_column = "Accm", lib_sizes = seq(10, 200, by = 10), random_libs = TRUE)
  Accm_xmap_Bccm_means <- ccm_means(Accm_xmap_Bccm)
  Bccm_xmap_Accm_means <- ccm_means(Bccm_xmap_Accm)
  par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
  plot(Accm_xmap_Bccm_means$lib_size, pmax(0, Accm_xmap_Bccm_means$rho), type = "l", col = "red", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim = c(0, 1))
  lines(Bccm_xmap_Accm_means$lib_size, pmax(0, Bccm_xmap_Accm_means$rho), col = "blue")
  legend(x = "topleft", bty = "n", legend = c("Y xmap X", "X xmap Y"), col = c("red", "blue"), lwd = 1, inset = 0.02, cex = 1.5)
}
inputdata <- read.csv("./csv/protein.csv", header=TRUE)
time <- inputdata$time
ratio <- inputdata$ratio
round <- inputdata$Round

TIMESTEP <- 100 

t <- 1:TIMESTEP
# show  inputdata
plot(t, ratio, type = "l",xlim=c(0, TIMESTEP), ylim=c(0,2), xlab = "t", ylab = "X(t)", col = "black")
lines(t, round, type = "l", xlab = "t", ylab = "Y(t)", col = "red")
legend("topleft", c("ratio", "round"), lty=c(1,2), col=c(1,2), lwd=2, bty="n", cex=1.2)

Accm <- as.numeric(ratio)
Bccm <- as.numeric(round)

# create ARmodel
# ratio_normalized <- Accm - 1
# plot(t, ratio_normalized, type = "l",xlim=c(0, TIMESTEP), ylim=c(-1,1), xlab = "t", ylab = "X(t)", col = "red")
# ratio <- ar(ratio_normalized, aic=TRUE)
# ratio$ar

# determine Embedding Dimension
determineEmbeddingDimension(Accm)
determineEmbeddingDimension(Bccm)
E <- 7
# Prediction Decay
predictionDeacy(data = Accm, Em = E)
predictionDeacy(data = Bccm, Em = E)
TAU = 1
# Identifying Nonlinearity
identifyingNonlinearity(data = Accm, Em = E)
identifyingNonlinearity(data = Bccm, Em = E)
# draw CCM
drawCCM(Accm = Accm, Bccm = Bccm, E = E, TAU = TAU)
