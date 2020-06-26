gh_install_packages("albagc/SCOUTerRpack")
# Exploring the reference dataset
# The demo X matrix will be used for all the examples. First we build the PCA model.
X <- as.matrix.data.frame(X)
pcamodel_ref <- pcamb_classic(X, 2, 0.05, "cent")

###############################################################################################
# Types of outliers PLOT
###############################################################################################
set.seed(1218)
indsel <- sample(c(1, 8, 15, 20, 24, 30, 35, 40))
Xobot <- matrix(NA, 1, ncol(X))
Xot2 <- matrix(NA, 1, ncol(X))
Xospe <- matrix(NA, 1, ncol(X))
for (ind in indsel){
  x <- t(as.matrix(X[ind,]))
  oboth <- scout(x, pcamodel_ref, T2.y = 30 + runif(1, min = -2, max = 2), 
                      SPE.y = 30 + runif(1, min = -2, max = 2))
  ot2 <- scout(x, pcamodel_ref, T2.y = 30 + runif(1, min = -2, max = 2))
  ospe <- scout(x, pcamodel_ref, SPE.y = 30 + runif(1, min = -2, max = 2))
  
  Xobot <- rbind(Xobot, oboth$X)
  Xot2 <- rbind(Xot2, ot2$X)
  Xospe <- rbind(Xospe, ospe$X)
}
X.0 <- X[-c(indsel,1,2, 9, 25),]
c.0 <- matrix(0,nrow(X.0),1)
c.1 <- as.vector(t(kronecker(matrix(1,1,8), c(1:3))))
Xall <- rbind(X.0, Xobot[-1,], Xot2[-1,], Xospe[-1,])
pcaxplot1 <- pcame(Xall, pcamodel_ref)
c.all <- c(c.0, c.1)


# Score plot
df.plot <- as.data.frame(cbind(pcaxplot1$T2, pcaxplot1$SPE, c.all, c.all > 0))
colnames(df.plot) = c("T2", "SPE", "tag", "set")
df.plot$tag <- as.factor(df.plot$tag)
df.plot$set[df.plot$set == 0] <- "Obs.ref"
df.plot$set[df.plot$set == 1] <- "Obs.new"
df.plot$set <- as.factor(df.plot$set)
ggplot(data = df.plot, aes(x = T2, y = SPE)) +
  geom_vline(aes(xintercept = pcamodel_ref$limt2), color = "red", size = 0.75, 
             linetype = "dashed") +
  geom_hline(aes(yintercept = pcamodel_ref$limspe), color = "red", size = 0.75,
             linetype = "dashed") +
  geom_point(aes(x = T2, y = SPE, shape = tag), color = "blue", size = 3, alpha = 0.5) +
  scale_shape_manual("Observations", values = c(16, 17, 15, 3), 
                     labels = c("Non outlying", "Extreme and orthogonal outliers", 
                                "Extreme or good leverage outliers", "Moderate or orthogonal outliers")) +
  guides(shape = guide_legend(title.position = "top", direction = "horizontal", label.position = "right", ncol = 2,
                              title.theme = element_text(size = 12, hjust = 0.5) , override.aes = list(alpha = 1))) +
  theme_bw() + labs(subtitle = bquote(UCL[.(paste0((1 - pcamodel_ref$alpha)*100, "%"))]), 
                    x = bquote(italic(~T[A]^2)), y = "SPE", title = "Distance plot") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(face = "italic", size = 14),
        axis.title.y = element_text(face = "italic", size = 14),
        legend.text = element_text(size = 12), legend.position = "bottom")
################################################################################################
# First examples
# Project the data on the PCA model and visualize.
Tplot <- scoreplot(X, pcamodel_ref, plottitle = "Score plot")
Dplot <- distplot(X, pcamodel_ref, plottitle = "Distance plot")
ggarrange(Dplot, Tplot, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")
pcax <- pcame(X, pcamodel_ref)
obscontribpanel(pcax, pcamodel_ref, which.max(pcax$SPE))
barwithucl(pcax$SPE, c(1:10), ucl = pcamodel_ref$limspe, plotname = "SPE") + 
  theme(plot.title = element_text(face = "italic"))
################################################################################################
# Generation of ONE outlier (one step).
################################################################################################
# An observation is chosen randomly from X and the scout function is used
# in order to shift it according to the specified target values for the SPE
# and T^2. In this demo we set 3 scenarios, varying independently and
# simultaneously the pair of statistics, setting a target value of 20.
xnew <- xshift(X[1,], pcamodel_ref$P, a = 2, b = 2)
set.seed(1218)
indsel <- sample(1:nrow(X), 1)
x <- t(as.matrix(X[indsel,]))
bad.lev.obs <- scout(x, pcamodel_ref, T2.y = 40, SPE.y = 40, mode = "simple")
extreme.obs <- scout(x, pcamodel_ref, T2.y = 40, mode = "simple")
moderate.obs <- scout(x, pcamodel_ref, SPE.y = 40, mode = "simple")

################################################################################################
# Generate a SET of outliers (one step).
################################################################################################
# Following the previous example, now the simulation is generalized from a
# single observation, to a set of observations which will present the
# specifications for SPE ant T^2.
n <- nrow(X)
X.outliers <- scout(X, pcamodel_ref, T2.y = matrix(40, n, 1), mode = "simple") 
X.all <- rbind(X, X.outliers$X)
tag.all <- c(matrix(0, n, 1), X.outliers$tag)
Tplot <- scoreplot(X.all, pcamodel_ref, obstag = tag.all, plottitle = "Score plot")
Dplot <- distplot(X.all, pcamodel_ref, obstag = tag.all, plottitle = "Distance plot")
ggarrange(Dplot, Tplot, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")

################################################################################################
# Generate of one outlier with 20 STEPS linearly spaced.
################################################################################################
# In this example it is included an intermediate step between the initial
# values {SPE, T^2} and the target ones, which is the incremental variation
# of the SPE or/and the T^2. There are two new parameters to set:
#    - The number of m steps to perform until reaching the target values
#    for each statistic.
#    - The spacing between steps gamma, which tunes the linearity of the
#    spacing. If any value is provided, a linear spacing (gamma = 1) is
#    performed.
n <- nrow(X)
# Shift SPE and T^2
outsteps <- scout(x, pcamodel_ref, T2.y = 40, SPE.y = 40, nsteps = 10, mode = "steps")
X.all <- rbind(x, outsteps$X)
tag.all <- c(0, outsteps$tag)
Tplot <- scoreplot(X.all, pcamodel_ref, obstag = tag.all, plottitle = "Score plot")
Dplot <- distplot(X.all, pcamodel_ref, obstag = tag.all, plottitle = "Distance plot")
ggarrange(Dplot, Tplot, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")

################################################################################################
# Generate series of outliers with 20 STEPS linearly and non-linearly spaced.
################################################################################################
# This section performs different shifts returning as a plot the evolution
# of the statistics along the shift steps, for different values of the
# gamma parameter in each statistic.
n <- nrow(X)
x.pca <- pcame(x, pcamodel_ref)
gparam <- c(0.3, 0.5, 1, 1.5, 3)
Xall <- x
SPEgamma <- matrix(NA, nrow = 21, ncol = 0)
T2gamma <- matrix(NA, nrow = 21, ncol = 0)
for (gn in 1:length(gparam)){
  outsteps <- scout(x, pcamodel_ref, T2.y = 20, SPE.y = 20,
                       nsteps = 20, gspe = gparam[gn], gt2 = gparam[gn],
                       mode = "steps")
  Xall <- rbind(Xall, outsteps$X)
  SPEgamma <- cbind(SPEgamma, c(x.pca$SPE, t(outsteps$SPE)))
  T2gamma <- cbind(T2gamma, c(x.pca$T2, outsteps$T2))
}
df.gamma.steps <- data.frame(as.factor(as.vector(kronecker(matrix(1, 21, 1), t(gparam)))),
                             as.vector(SPEgamma), as.vector(T2gamma),
                             as.vector(kronecker(matrix(1, 1, gn), 1:21)))

colnames(df.gamma.steps) <- c("gamma", "SPE", "T2", "step")
data.step.10 <- df.gamma.steps[df.gamma.steps$step == 10,]
spe.gamma <- ggplot(data = df.gamma.steps, mapping = aes(x = step, y = SPE, group = gamma)) +
  geom_line() + geom_point() +
  theme_bw() + labs(x = "Steps", y = bquote(italic(SPE[i]))) +
  geom_text(data = data.step.10, mapping = aes(x = step, y = SPE + 1,
                                               label = paste("gamma ==", gamma)), parse = T, size = 3)

t2.gamma <- ggplot(data = df.gamma.steps, mapping = aes(x = step, y = T2, group = gamma)) +
  geom_line() + geom_point() +
  theme_bw() + labs(x = "Steps", y = bquote(italic(T.["A,i"]^2))) +
  geom_text(data = data.step.10, mapping = aes(x = step, y = T2 + 0.5,
                                               label = paste("gamma ==", gamma)), parse = T, size = 3) 

ggarrange(spe.gamma, t2.gamma, nrow = 1, ncol = 2)


# This is a second scenario where different gamma values are used to control the spacing of each
# statistic
nsteps <- 20
Xall2 <- x
SPEgamma <- matrix(NA, nrow = nsteps + 1, ncol = 0)
T2gamma <- matrix(NA, nrow = nsteps + 1, ncol = 0)
npar <- length(gparam)
for (gn in 1:npar){
  outsteps <- scout(x, pcamodel_ref, T2.y = 20, SPE.y = 20,
                       nsteps = nsteps, gspe = gparam[gn], gt2 = gparam[npar + 1 - gn],
                       mode = "steps")
  Xall2 <- rbind(Xall2, outsteps$X)
  SPEgamma <- cbind(SPEgamma, c(x.pca$SPE, t(outsteps$SPE)))
  T2gamma <- cbind(T2gamma, c(x.pca$T2, outsteps$T2))
}
df.gamma.steps <- data.frame(as.factor(as.vector(kronecker(matrix(1, 21, 1), t(gparam)))),
                             as.factor(as.vector(kronecker(matrix(1, 21, 1), t(rev(gparam))))),
                             as.vector(SPEgamma), as.vector(T2gamma),
                             as.vector(kronecker(matrix(1, 1, gn), 1:21)))

colnames(df.gamma.steps) <- c("gammaSPE", "gammaT2", "SPE", "T2", "step")
data.step.10 <- df.gamma.steps[df.gamma.steps$step == 10,]
data.step.10$T2 <- data.step.10$T2 - 0.2
data.step.10$SPE <- data.step.10$SPE + 1
ggplot(data = df.gamma.steps, mapping = aes(x = T2, y = SPE, group = gammaSPE)) +
  geom_line() + geom_point() +
  scale_fill_viridis_d() + scale_color_viridis_d() +
  theme_bw() + labs(y = bquote(italic(SPE[i])), x = bquote(italic(T["A,i"]^2))) +
  geom_text(data = data.step.10, mapping = aes(x = T2, y = SPE + 1,
                                               label = paste("gamma[T^2] ==", gammaT2)), parse = T, size = 3) +
  geom_text(data = data.step.10, mapping = aes(x = T2, y = SPE,
                                               label = paste("gamma[SPE] ==", gammaSPE)), parse = T, size = 3) +
  theme(axis.title.y = element_text(face = "italic"), axis.text.x = element_text(face = "italic"))

################################################################################################
# Generate a grid of outlying observations
################################################################################################
# Finally, a last example is simulated. In this case, instead of increasing
# in a step-wise joint manner both the SPE and the T^2, a grid of steps is
# created. This implies simulating all possible combinations of pairs for
# the {SPE, T^2} along their increments. Thus, instead of having M new
# observations where M is the number of steps, a set of MspexMt2 observations is
# created for all combinations of both statistics.
# A grid with 3 steps for the T^2 and 2 steps for the SPE is simulated, 
# with steps non-linearly spaced.
outgrid <- scout(x, pcamodel_ref, T2.y = 40, SPE.y = 40,
                                nsteps.spe = 2, nsteps.t2 = 3, 
                                gspe = 3, gt2 =0.3,
                                mode = "grid")

X.all <- rbind(x, outgrid$X)
tag.all <- c(0, outgrid$tag)
Tplot <- scoreplot(X.all, pcamodel_ref, obstag = tag.all, plottitle = "Score plot") 
Dplot <- distplot(X.all, pcamodel_ref, obstag = tag.all, plottitle = "Distance plot") 
ggarrange(Dplot, Tplot, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom") 