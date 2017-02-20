# 2 a 4 miércoles, 550
# número aleatorio con igual probabilidad
#v<-runif(100)
#print(v)

# La ruina del jugador
#Un jugador tiene $m
n<-200
m<-100
apuesta<-1
probabilidad<-0.47
tiradas<-0
dinero<-0

while(m>0 && m<=300){
	jugar<-runif(1)
	#print(jugar)
	if(jugar<=probabilidad){
		m<-m+1
		tiradas<-tiradas+1
		dinero<-c(dinero,m)
	}
	else{	
		m<-m-1
		tiradas<-tiradas+1
		dinero<-c(dinero, m)
	}
}

#print(dinero)

plot(dinero, ylab="Dinero",xlab="Tiradas", type="l", col="green")

problema2<-function(tiros){
	repetir<-function(m){
			perder<-0
			while(m>0 && m<=300){
				jugar<-runif(1)
				if(jugar<=probabilidad){
					m<-m+1
					tiradas<-tiradas+1
					dinero<-c(dinero,m)
				}
				else{	
					m<-m-1
					tiradas<-tiradas+1
					dinero<-c(dinero, m)
				}
			}

			if(m<=0){
				return(c(tiradas, "P"))
			}else{
				return(c(tiradas, "G"))
			}
	}

	dinero<-100
	jugadas<-tiros

	win<-0
	lose<-0

	aux<-0
	contador<-0
	suma<-0

	while(contador<jugadas){
		aux<-repetir(dinero)
		# print(aux[c(F,T)]=="P")
		# if(aux[c(F,T)]=="P"){
		# 	lose<-lose+1
		# }else{
		# 	win<-win+1
		# }
		#print(as.numeric(aux[1]))
		suma<-suma + as.numeric(aux[1])
		contador<-contador+1
	}
	print(suma)
	cat("Juegos[",jugadas,"]","Tiradass=",suma/jugadas,"\n")
}

problema3b<-function(tiros){
	repetir<-function(m){
			perder<-0
			ganar<-0
			apuesta<-5
			while(m>0 && m<=105){
				jugar<-runif(1)
				if(jugar<=probabilidad){
					m<-m+apuesta
					tiradas<-tiradas+1
					ganar<-ganar+1
					dinero<-c(dinero,m)
				}
				else{	
					m<-m-apuesta
					tiradas<-tiradas+1
					perder<-perder+1
					dinero<-c(dinero, m)
				}
				apuesta<-apuesta+5
			}
			# print(tiradas)
			# print(ganar)
			# print(perder)
			# if(m==0){
			# 	return(perder)
			# }else{
			# 	return(-1)
			# }
			return(m)
	}

	dinero<-100
	jugadas<-tiros
	
	perdidas<-0
	ganadas<-0
	
	contador<-0
	aux<-0

	while(contador<jugadas){
		aux<-repetir(dinero)
		if(aux<=0){
			perdidas<-perdidas+1
		}else{
			ganadas<-ganadas+1
		}
		contador<-contador+1
	}
	cat("Juegos[",jugadas,"]","P(ganar +5)=", ganadas/jugadas," P(perder +5)", perdidas/jugadas,"\n")
}



problema4<-function(tiros){
	repetir<-function(m){
			jugar<-runif(1)
			if(jugar<=probabilidad){
				m<-m*2
				return(TRUE)
			}
			else{	
				m<-m-m
				return(FALSE)
			}
	}

	money<-100
	aux<-FALSE
	contador<-0
	jugadas<-tiros
	win<-0
	lose<-0

	while(contador<jugadas){
		aux<-repetir(money)
		if(aux == TRUE){
			win<-win+1			
		}
		else{
			lose<-lose+1
		}
		contador<-contador+1
	}
	#print(sumas)
	#print(perdidas)
	cat("Juegos[",jugadas,"]","P(ganar todo)", win/jugadas," P(perder todo) ",lose/jugadas,"\n")
}


problema2(20)
problema3b(200)
problema4(200)