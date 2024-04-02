#
# Demo R code for SVD Analysis of Yield Curve
#

# Read input file
A_input = read.csv('GovtBondWeek17.csv')

# Delte the first date column
A = A_input[,2:12]#*0.01

# Print the dimension of A 
dim(A)

# Put the term date of Bonds in a vector
term = c(3,6,9,12,18,24,30, 36,60,120,240)

# Print the yield data for various dates

matplot(term, t(A), type="l")

matplot(term, t(A)[,120:140], type="l")

#
# Do the SVD for A and put in matrix D
#
meanCurve = apply(A, 2, mean)
meanMatrix = matrix(rep(1,nrow(A)), nrow = nrow(A)) %*% meanCurve
A0 = A - meanMatrix

D = svd(A0)

D$u[,1:2] = -D$u[,1:2]
D$v[,1:2] = -D$v[,1:2]

# Singular values
D$d

# Explain power of cumulated singular vectors
cumsum(D$d^2/sum(D$d^2))*100

# Plot singular vector curves for yields

# Plot the single i-th right singular vector curves without SCALING
i=2
# Plot right singular vector curve
plot(term,-D$d[i]*D$v[,i],type="l")
# Plot left singular vector curve
plot(1:length(D$u[,i]),abs(D$u[,i]),type="l")

# Plot the righ singular vector curves from s to t without SCALING
s=4
t=6
matplot(term, D$v[,s:t],type="l", col=c(2,3,4,5,6,7,8,9))
abline(h=0, col=1)

# Plot the righ singular vector curves from s to t with SCALING
s=1
t=3
matplot(term, D$v[,s:t]%*%diag(D$d[s:t]),type="l", col=c(2,3,4,5,6,7,8,9))
abline(h=0, col=1)


# Plot the left-singular vectors
s=1
t=3
matplot(1:nrow(A), D$u[,s:t]%*%diag(D$d[s:t]),type="l", col=c(2,3,4,5,6,7,8,9))
abline(h=0, col=1)

# 
# End of Code
# 