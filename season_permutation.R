#!/usr/bin/env Rscript

arguments = commandArgs(trailing=TRUE)

# arguments = '1'

seed = as.integer(arguments[1])

gelada.scans = readRDS('gelada_scans.rds')

dir.create('results',showWarnings=FALSE)
dir.create('stats',showWarnings=FALSE)
dir.create('stats/pval',showWarnings=FALSE)
dir.create('stats/best',showWarnings=FALSE)

if (seed) {
	# # set.seed(seed)
	# gelada.scans$Season = sample(gelada.scans$Season,replace=FALSE)
	# gelada.scans$context3 = sample(gelada.scans$context3,replace=FALSE)
	gelada.scans = do.call(rbind,lapply(split(gelada.scans,gelada.scans$ID),function(x) {
		x$Season = sample(x$Season,replace=FALSE)
		x$context3 = sample(x$context3,replace=FALSE)
		x
	}))
	rownames(gelada.scans) = NULL
}

gelada.scans.split = split(gelada.scans,with(gelada.scans,list(ID,context3,Season)))

gelada.scan.rates2 = do.call(rbind,lapply(names(gelada.scans.split),function(i) {
	these = unlist(strsplit(i,split='\\.'))
	id = these[1]
	context = these[2]
	season = these[3]

	x = gelada.scans.split[[i]]

	if (nrow(x)) {
		data.frame(
			id = id,
			elo_rating = unique(x$Elo.Rating),
			agg_rate = sum(x$within_aggression)/nrow(x),
			btw_rate = sum(x$between_aggression)/nrow(x),
			season = season,
			context = context,
			n=nrow(x)
		)
	} else {
		data.frame(
			id = id,
			elo_rating = NA,
			agg_rate = NA,
			btw_rate = NA,
			season = season,
			context = context,
			n=0
		)
	}
}))
rownames(gelada.scan.rates2) = NULL

results = do.call(rbind,lapply(split(gelada.scan.rates2,with(gelada.scan.rates2,list(context,season))),function(x) {
	result = summary(lm(agg_rate~elo_rating,data=x))
	out = data.frame(
		context=unique(x$context),
		season=unique(x$season),
		beta=coef(result)['elo_rating','Estimate'],
		serr=coef(result)['elo_rating','Std. Error'],
		tval=coef(result)['elo_rating','t value'],
		pval=coef(result)['elo_rating','Pr(>|t|)']
	)
	out
}))
results$seed = seed
# rownames(results) = NULL

saveRDS(results,file=file.path('results',paste0('results_permutation_',formatC(seed,width=10,flag=0),'.rds')))

if (seed) {
	write(subset(results,context == 'Feed Below' & season == 'Dry')$pval,file=file.path('stats/pval',paste0('pval_',formatC(seed,width=10,flag=0),'.txt')))
	write(as.integer(rownames(results)[which.min(results$tval)] == 'Feed Below.Dry'),file=file.path('stats/best',paste0('best_',formatC(seed,width=10,flag=0),'.txt')))
}

