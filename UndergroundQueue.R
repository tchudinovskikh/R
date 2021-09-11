station_name = c('жулебино', 'лермонтовский проспект', 'выхино', 'рязанский проспект', 
                 'кузьминки', 'текстильщики', 'волгоградский проспект', 'пролетарская')

station_kind = c('ж', 'ж', 'ж', 'п', 'ж', 'п', 'п', 'ж')

bitok=c(0.2, 0.8, 1.4, 2, 2.6, 3.2, 3.8, 4.4, 5)

sigmoida = function(x) return(1/(1 + exp(-0.03*x)))
f = function(x) return(sigmoida(x)*(1 - sigmoida(x)))

arrival_new=function(t,s){
  if(station_kind[s]=='ж'){
    koef=150
  }else{
    koef=50
  }
  return(round(mean(rpois(100,koef*f(t-60-30*s)))))
}

arr=matrix(0,nrow=540,ncol=8)


  for(i in 1:540){
    for(j in 1:8){
      arr[i,j]=arr[i,j]+arrival_new(i,j)
    }
  }



deter=function(s,t){
  w=0
  for(d in 1:108){
    if(tats[d,s]==t){
      w=d
    }
  }
  return(w)
}

tats=matrix(c(seq(5,540,5),
              seq(10,545,5),
              seq(15,550,5),seq(20,555,5),seq(25,560,5),seq(30,565,5),seq(35,570,5),seq(40,575,5)), nrow=108,ncol=8)


enter_or_not=function(s,n){ # s - station, n - number of people in train
  return(rbinom(1,1,1-(n/500)^bitok[s]))
}

itogo=function(t){
  pats=rep(0,8)
  pint=rep(0,108)
  for(i in 1:t){
    for(j in 1:8){
      pats[j]=pats[j]+arr[i,j]
      if(i%%5==0 && pats[j]>0 && i>=j*5){
        l=0
        k=deter(j,i)
        for(q in 1:pats[j]){
          y=enter_or_not(j,pint[k])
          l=l+y
          pint[k]=pint[k]+y
        }
        pats[j]=pats[j]-l
      }
    }  
  }
  g=list(c(pats), c(pint))
  return(g)
}


d=matrix(0,nrow=540,ncol=8)
for(t in 1:540){
  d[t,]=itogo(t)[[1]]
}

print(max(d))

for(i in 1:540){
  for(j in 1:8){
    if(d[i,j]==max(d)){
      print(c(i,j))
    }
  }
}
