myqr = function(A) {
  a = qr(A)
  q = qr.Q(a)
  r = qr.R(a)
  return(list(Q = q, R = r))
}
