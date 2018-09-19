#'Opgave 1
#1.
vektorfunktion = function(x) {
produkt = x[1]
  for (i in 2:length(x)){ 
    produkt = produkt*x[i]}
  return(produkt)}

vektorfunktion(c(1,2,3))

prod(c(1,2,3))

#2.
fibo = function(n) {
  if(n !=round(n)){stop("Skal være heltal")}
  if(n < 1){stop("Skal være positiv")}
 if(n==1){return(1)}
  if (n==2){return(1)}
  return (fibo(n-2)+fibo(n-1))
}
fibo(10)
fibo(1)
fibo(0)
fibo(3.5)
