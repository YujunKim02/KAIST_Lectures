### Gaussian Annulus ###
library("wordspace")
library("ggplot2")
d = 1e4
n = 1e3
# 1-(a) Generate n random vectors
X = matrix(rnorm(d*n), nrow = d) # d * n

# 1-(b) Compute Squared nomrs
x_norm = colNorms(X, p=2)^2

# 1-(c) Draw a histogram

png("x_norm.png")
hist = hist(x_norm, 50)
dev.off()

# 1-(d) Sample mean and Sample standard deviation
mean(x_norm)
sd(x_norm)

#1-(e)

### Random Projection ###
m = 300 # 100, 300, 500
# 2-(a) Generate U
U = matrix(rnorm(d*m), nrow = d) # d * m

# 2-(b)
fX = t(X)%*%U # n * m

# 2-(c)
Z = data.frame(fX, t(X)) # each row has f(X_i), X_i
dim(Z)

process <- function(z, m) {
    nfx = norm(z[1:m], "2")
    nx = norm(z[(m+1):(m+d)], "2")
    ((1/sqrt(m))*nfx - nx)/nx
}

R = apply(Z, 1, function(z) process(z, m)) # n elements

# 2-(d) Histogram of r_i
png(sprintf("r_i with m=%s.png", m))
hist(R,50)
dev.off()
# 2-(e)
sum(abs(R) <= 0.1)/n

# 2-(f)


