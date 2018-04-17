# distances between points with nearest neighbor approx


x = c(1,2,3,4,5,6,7,8,9)
y = c(1,2,3,4,5,6,7,8,9)

a = x + 2
b = y * 2

all = c(a,b,x,y)
plot(a,b,pch = 20, ylim = c(min(all), max(all)), xlim = c(min(all), max(all)), asp = 1)
points(x,y,pch = 20, col = 'red')


X = cbind(x,y)
A = cbind(a,b)



nearest_xy = function(A,X,to = 1, plot = F) {
	
	Z = as.matrix(dist(rbind(X,A)))

	aseq = ((length(x)+1):length(c(x,a))); xseq = (1:(length(x)))

	Z = Z [ aseq , xseq ]

	# so now have distances between every point in each xy dataset. now pick which set to choose the nearest neighbor 

	# if the A dataset is the one to match to...
	if (to == 1) {
		v = apply(Z, 1, 'min')

		j = 1
		m = c()
		for (i in aseq) {
			jkl = Z[paste0(i),]
			lkj = which(jkl %in% v[paste0(i)])[1]
			m[j] = lkj
			j = j + 1
		}

		T = cbind(A,X[m,])
	}

	# if the X dataset is the one to match to...

	else if (to == 2) {
		v = apply(Z, 2, 'min')

		j = 1
		m = c()
		for (i in xseq) {
			jkl = Z[,paste0(i)]
			lkj = which(jkl %in% v[paste0(i)])[1]
			m[j] = lkj
			j = j + 1
		}

		T = cbind(X,A[m,])
	}

	else {stop('pick a variable to match to.')}

	if (plot == T) {
		par(bg = 'grey90')
		plot(T[,1], T[,2], type = 'n', ylim = c(min(all), max(all)), xlim = c(min(all), max(all)), asp = 1)

		points(X[,1], X[,2], col = 'black', pch = 20)
		points(A[,1], A[,2], col = 'blue3', pch = 20)

		points(T[,3], T[,4], col = 'red3', pch = 1, cex = 1.5)

		segments(T[,1], T[,2],T[,3], T[,4], col = 'red3', lty = 3)
	}
}



