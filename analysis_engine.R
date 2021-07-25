################################################################################
## Bianca Luedeker & MaryLena Bleile
## Bayes Project:  Heavy Metal Analysis
## Last Update: Dec 5, 12:54 pm
################################################################################

#### Required Packages ####
library(tidyverse)
library(countrycode)  ##Give continent for each country
library(stringr)
library(dplyr)
set.seed(6390)
##update configs
source("update_config.R")
source("analyis_functions.R")
source("clean_functions.R")
#### Data Cleaning ####

raw.dataset = get_data(dataset=dataset)
dataset.cleaned.country = clean_country(full.data.set = raw.dataset)
cleaned.country.no.repeats = remove_repeats(dataset.cleaned.country)
  

################################################################################

#### Determine Genres ####
if(!(dataset=='original')){
cleaned.country.no.repeats$style = cleaned.country.no.repeats$genre}

#length(unique(without.band.repeats$style)) ## a total of 1227 styles

#View(sort(unique(without.band.repeats$style)))  ## View all styles listed

bands.per.style <- cleaned.country.no.repeats %>% group_by(style) %>% count() %>% arrange(desc(n))
#View(bands.per.style)
#write.csv(bands.per.style, "bands_per_style.csv")
if(genre_array=='full'){
genres.chronologically = c("Heavy", "Thrash","Power","Black", "Death","Doom","Gothic", "Alternative")}else if(genre_array =='no_doom'){
  genres.chronologically = c("Heavy", "Thrash","Power","Black", "Death","Gothic", "Alternative")
}
#### Categorize bands into a single style ####
if(dataset=='original'){
  styles <- cleaned.country.no.repeats$style
}else{
  styles <- cleaned.country.no.repeats$genre
}


####Code for original, choronological classification#####
if(order == 'chronological'){
styles[str_detect(styles, "ymphonic")] <- "Gothic"
styles[str_detect(styles, "othic")] <- "Gothic"
styles[str_detect(styles, "eavy")] <- "Heavy"
styles[str_detect(styles, "rash")] <- "Thrash"
styles[str_detect(styles, "ower")] <- "Power"
styles[str_detect(styles, "rog")] <- "Power"
styles[str_detect(styles, "lack")] <- "Black"
styles[str_detect(styles, "eath")] <- "Death"
if(grind_to_death){
styles[str_detect(styles, "rindcore")] <- "Death"}
if(genre_array =='full'){
styles[str_detect(styles, "oom")] <- "Doom"
styles[str_detect(styles, "tmospheric")] <- "Doom"}
styles[str_detect(styles, "lternative")] <- "Alternative"

styles[str_detect(styles, "Nu")] <- "Alternative"

}else{
####Reverse Chronological classificaiton for model checking
styles[str_detect(styles, "ymphonic")] <- "Gothic"
styles[str_detect(styles, "othic")] <- "Gothic"
styles[str_detect(styles, "lternative")] <- "Alternative"
styles[str_detect(styles, "rog")] <- "Alternative"
styles[str_detect(styles, "Nu")] <- "Alternative"
styles[str_detect(styles, "oom")] <- "Doom"
styles[str_detect(styles, "tmospheric")] <- "Doom"
styles[str_detect(styles, "eath")] <- "Death"
styles[str_detect(styles, "rindcore")] <- "Death"
styles[str_detect(styles, "lack")] <- "Black"
styles[str_detect(styles, "ower")] <- "Power"
styles[str_detect(styles, "rash")] <- "Thrash"

styles[str_detect(styles, "eavy")] <- "Heavy"
}


styles[which(!(styles %in% genres.chronologically))] <- "Other"

cleaned.data <- cbind(cleaned.country.no.repeats, data.frame(new_genre = styles))
write.csv(cleaned.data, paste("data/",dataset,"-cleaned.csv",sep=""))

#### Prepare data by genres per country ####
genre.per.country <- cleaned.data %>% group_by(countries, new_genre) %>% count()

#View(genre.per.country)
#write.csv(genre.per.country, "genre_per_country.csv")

n=length(genres.chronologically)+1
##Compute EB estimates for hyperpriors
alphahat = numeric(n)

for(i in 1:n){
  genre = as.character(unique(genre.per.country$new_genre))[i]
  theta.hats = get.theta(genre, unique(as.character(genre.per.country$countries[1:99])))
  #print(theta.hats)
  theta.hats = as.numeric(matrix(theta.hats))
  theta.hats[is.na(theta.hats)]=0
  s2=var(theta.hats)
  theta.hat = mean(theta.hats)
  B = (1-theta.hat)*(theta.hat*(1-theta.hat)/s2-1)
  alphahat[i] = theta.hat*B/(1-theta.hat)
}
names(alphahat) = unique(genre.per.country$new_genre)

##sample from the posterior of theta given alpha and the data
nsamp=1000

thetas.post=array(NA, dim=c(nsamp, n, length(unique(genre.per.country$countries))), dimnames=list(1:nsamp, unique(genre.per.country$new_genre), unique(genre.per.country$countries)))



topthree = data.frame(One = NA, Two=NA,Three=NA)
botthree = data.frame(One = NA, Two=NA,Three=NA)

plot_countries_posterior(interest)

rownames(topthree) = unique(genre.per.country$countries)

results = summary(as.factor(topthree$One))
s= summary(as.factor(topthree$Two))
 ##uncomment this line when running with original ordering
if(order=='chronological'){
s$Gothic=0}



t=summary(as.factor(topthree$Three))
##uncomment these two lines when running with reverse chonological ordering
if(!(order=='chronological')){
s$Heavy=0
t$Thrash=0}
results = cbind(results, s)
results = cbind(results, t)
colnames(results)=c("Rank 1","Rank 2","Rank 3")

###generate appendix
if(generate_appendix){
quant.f = function(x){
  s = apply(x, 2, quantile, probs=c(0.025,0.975))
}

post.summary = apply(thetas.post,3,quant.f)

for(k in int2){
  print(xtable::xtable(quant.f(thetas.post[,,k]), caption=paste("Posterior Credible Intervals for ", dimnames(thetas.post)[[3]][k])))
}

for(j in int2){
  y = get.counts.per.genre(unique(genre.per.country$countries)[j])
  ##y is a vector of length 14 with counts of genre observations for country j
  ##needs to be in the same order as alphahat
  alphas.post = alphahat + y
  thetas.post[,,j] = gtools::rdirichlet(nsamp, alphas.post)
  mm = apply(thetas.post[,,j],2,median, na.rm=T)
  #topthree[j,] = names(alphahat)[order(mm,decreasing=T)[1:3]]
  boxplot(thetas.post[,order(mm),j], main=unique(genre.per.country$countries)[j], col="turquoise", ylab=expression(theta))
  
  #botthree[j,] = names(alphahat)[order(mm,decreasing=F)[1:3]]
}

#### Model checking Predictive Posterior Distribution ####
## Added by BL on 12/7/2020 2:07 pm
## The randomly selected country was country number 56.

## Counts for country 56 - Mexico
yy <- get.counts.per.genre(unique(genre.per.country$countries)[56]) 
total <- sum(yy)
alpha.post <- alphahat+yy
## Draw 20 histograms of posterior counts.
nn <- 20 ## Number of posterior distributions.
par(mfrow=c(4,5))

for(ii in 1:nn){
  theta.post <- gtools::rdirichlet(1, alpha.post)
  pred.dist <- rmultinom(1, 16, theta.post)
  rownames(pred.dist) <- c("Th", "De", "Al", "Bl", "Do", "Go", "He", "Ot", "Po")
  barplot(pred.dist[,1], ylim=c(0, 10))
}

par(mfrow=c(1,1))
barplot(yy[,1], ylim=c(0, 10))}