support <- function(l) {
  for(i in l[1][1]){
    rule = l[1][1][i]
    count = 0
    
    for(j in nrow(wine.tra.is)){
      c = setequal(intersect(l, wine.tra.is[j,]), l)
      if(c= TRUE) { count = count+1}
    }
    support_values[i] = count/178
  }
  return(support_values)
}

supportOFTwo <- function(l) {
  for(i in l[1][1]){
    rule = l[[1][1][i], -ncol(l[[1][1][i]])]
    count = 0
    
    for(j in nrow(wine.tra.is)){
      c = setequal(rule, wine.tra.is[j,]), rule)
      if(c= TRUE) { count = count+1}
    }
    support_values_of_two[i] = count/178
  }
  return(support_values_of_two)
}

