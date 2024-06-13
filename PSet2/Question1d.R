B = matrix(c(1, -0.1, 0.2, 1), nrow=2,ncol=2, byrow = TRUE)

b2 = solve(B)

A1= matrix(c(0.3, 0.3, 0.1, 0.4), nrow=2,ncol=2, byrow = TRUE)

irf_y= matrix(nrow=31, ncol=2, dimnames=list(c(paste("t=",0:30)),(c("E_yt","E_zt"))))

for (i in 1:31) {
irf_y[i,1:2]=(A1^(i)%*%b2)[1,]
}

irf_z= matrix(nrow=31, ncol=2, dimnames=list(c(paste("t=",0:30)),(c("E_yt","E_zt"))))
for (i in 1:31) {
irf_z[i,1:2]=(A1^(i)%*%b2)[2,]
}