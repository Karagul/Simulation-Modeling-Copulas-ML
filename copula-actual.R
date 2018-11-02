# Copula package
library(copula)
# Fancy 3D plain scatterplots
library(scatterplot3d)
# ggplot2
library(ggplot2)
# Useful package to set ggplot plots one next to the other
library(grid)
set.seed(235)







# Generate a bivariate normal copula with rho = 0.7
normal <- normalCopula(param = 0.7, dim = 2)
# Generate a bivariate t-copula with rho = 0.8 and df = 2
stc <- tCopula(param = 0.8, dim = 2, df = 2)


# Build a Frank, a Gumbel and a Clayton copula
frank <- frankCopula(dim = 2, param = 8)
gumbel <- gumbelCopula(dim = 3, param = 5.6)
clayton <- claytonCopula(dim = 4, param = 19)

# Print information on the Frank copula
print(frank)


# Select the copula
cp <- claytonCopula(param = c(3.4), dim = 2)

# Generate the multivariate distribution (in this case it is just bivariate) with normal and t marginals
multivariate_dist <- mvdc(copula = cp,
                          margins = c("norm", "t"),
                          paramMargins = list(list(mean = 2, sd=3),
                                              list(df = 2)) )

print(multivariate_dist)

# Generate random samples
fr <- rCopula(2000, frank)
gu <- rCopula(2000, gumbel)
cl <- rCopula(2000, clayton)

# Plot the samples
p1 <- qplot(fr[,1], fr[,2], colour = fr[,1], main="Frank copula random samples theta = 8", xlab = "u", ylab = "v")
p2 <- qplot(gu[,1], gu[,2], colour = gu[,1], main="Gumbel copula random samples theta = 5.6", xlab = "u", ylab = "v") 
p3 <- qplot(cl[,1], cl[,2], colour = cl[,1], main="Clayton copula random samples theta = 19", xlab = "u", ylab = "v")

# Define grid layout to locate plots and print each graph^(1)
pushViewport(viewport(layout = grid.layout(1, 3)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p3, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))

samples <- rMvdc(2000, multivariate_dist)
scatterplot3d(samples[,1], samples[,2], color = "blue",pch = ".")


# Generate the normal copula and sample some observations
coef_ <- 0.8
mycopula <- normalCopula(coef_, dim = 2)
u <- rCopula(2000, mycopula)

# Compute the density
pdf_ <- dCopula(u, mycopula)

# Compute the CDF
cdf <- pCopula(u, mycopula)

# Generate random sample observations from the multivariate distribution
v <- rMvdc(2000, multivariate_dist)

# Compute the density
pdf_mvd <- dMvdc(v, multivariate_dist)

# Compute the CDF
cdf_mvd <- pMvdc(v, multivariate_dist)


par(mfrow = c(1, 3))
# 3D plain scatterplot of the density, plot of the density and contour plot
scatterplot3d(u[,1], u[,2], pdf_, color="red", main="Density", xlab ="u1", ylab="u2", zlab="dCopula", pch=".")
persp(mycopula, dCopula, main ="Density")
contour(mycopula, dCopula, xlim = c(0, 1), ylim=c(0, 1), main = "Contour plot")

par(mfrow = c(1, 3))
# 3D plain scatterplot of the CDF, plot of the CDF and contour plot
scatterplot3d(u[,1], u[,2], cdf, color="red", main="CDF", xlab = "u1", ylab="u2", zlab="pCopula",pch=".")
persp(mycopula, pCopula, main = "CDF")
contour(mycopula, pCopula, xlim = c(0, 1), ylim=c(0, 1), main = "Contour plot")

# 3D plain scatterplot of the multivariate distribution
par(mfrow = c(1, 2))
scatterplot3d(v[,1],v[,2], pdf_mvd, color="red", main="Density", xlab = "u1", ylab="u2", zlab="pMvdc",pch=".")
scatterplot3d(v[,1],v[,2], cdf_mvd, color="red", main="CDF", xlab = "u1", ylab="u2", zlab="pMvdc",pch=".")
persp(multivariate_dist, dMvdc, xlim = c(-4, 4), ylim=c(0, 2), main = "Density")
contour(multivariate_dist, dMvdc, xlim = c(-4, 4), ylim=c(0, 2), main = "Contour plot")
persp(multivariate_dist, pMvdc, xlim = c(-4, 4), ylim=c(0, 2), main = "CDF")
contour(multivariate_dist, pMvdc, xlim = c(-4, 4), ylim=c(0, 2), main = "Contour plot")





frank <- frankCopula(dim = 2, param = 3)
clayton <- claytonCopula(dim = 2, param = 1.2)
gumbel <- gumbelCopula(dim = 2, param = 1.5)

par(mfrow = c(1, 3))

# Density plot
persp(frank, dCopula, main ="Frank copula density")
persp(clayton, dCopula, main ="Clayton copula density")
persp(gumbel, dCopula, main ="Gumbel copula density")

# Contour plot of the densities
contour(frank, dCopula, xlim = c(0, 1), ylim=c(0, 1), main = "Contour plot Frank")
contour(clayton, dCopula, xlim = c(0, 1), ylim=c(0, 1), main = "Contour plot Clayton")
contour(gumbel, dCopula, xlim = c(0, 1), ylim=c(0, 1), main = "Contour plot Gumbel")


