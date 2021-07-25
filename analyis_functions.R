get.sum = function(country){
  sum(genre.per.country$n[genre.per.country$countries==country], na.rm=T)
}

get.theta = Vectorize(function(genre, country){
  #print(length(get.sum(country)))
  sum(genre.per.country$n[genre.per.country$countries==country & genre.per.country$new_genre==genre]/get.sum(country), na.rm=T)
},vectorize.args = "country")



get.counts.per.genre = Vectorize(function(country){
  data.one.country = genre.per.country[genre.per.country$countries==country,]
  counts = rep(0,n)
  names(counts) = names(alphahat)
  for(kk in 1:n){
    counts[which(names(counts)==data.one.country$new_genre[kk])] = data.one.country$n[kk]
  }
  counts
})


plot_countries_posterior= function(country.vec){
  for(j in 1:length(country.vec)){
    y = get.counts.per.genre(country.vec[j])
    ##y is a vector of length 14 with counts of genre observations for country j
    ##needs to be in the same order as alphahat
    alphas.post = alphahat + y
    thetas.post[,,j] = gtools::rdirichlet(nsamp, alphas.post)
    mm = apply(thetas.post[,,j],2,median, na.rm=T)
    topthree[j,] = names(alphahat)[order(mm,decreasing=T)[1:3]]
    if(grind_to_death){
      print("generating filepath...")
      filepath = paste(tolower(country.vec[j]),"_imgs/",dataset,"_",order,"_",genre_array,"_GtD.pdf", sep="")
      text_str = paste(dataset, order, genre_array, "Grindcore Reassigned", sep= ", ")
    }else{
      print("generating filepath...")
      filepath = paste(tolower(country.vec[j]),"_imgs/",dataset,"_",order,"_",genre_array,".pdf", sep="")
      text_str = paste(dataset, order, genre_array, sep=", ")
    }
    pdf(filepath, width=10)
    print(filepath)
    boxplot(thetas.post[,order(mm),j], main=country.vec[j], col="turquoise", ylab=expression(theta))
    #mtext(text_str)
    dev.off()
    
    botthree[j,] = names(alphahat)[order(mm,decreasing=F)[1:3]]
  }
}
