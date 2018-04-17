mycross = function(a,b) {
	A = rbind(a,b)
	v = c(det(A[,c(2,3)]),-det(A[,c(1,3)]),det(A[,c(1,2)]))
	return(v)
}