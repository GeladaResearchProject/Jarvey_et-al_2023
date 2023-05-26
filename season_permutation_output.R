#!/usr/bin/env Rscript

arguments = commandArgs(trailing=TRUE)

seed = as.integer(arguments[1])

results = readRDS(file.path('results',paste0('results_permutation_',formatC(seed,width=10,flag=0),'.rds')))

if (seed) {
	dir.create('stats/pvaleach',showWarnings=FALSE)
	dir.create('stats/tvaleach',showWarnings=FALSE)
	dir.create('stats/bestdiff',showWarnings=FALSE)
	codes = c('Feed Below.Dry','Feed/Forage.Dry','Social/Rest.Dry','Feed Below.Wet','Feed/Forage.Wet','Social/Rest.Wet')
	for (i in seq_len(length(codes))) {
		r = codes[i]
		write(results[r,'tval'],file=file.path('stats/tvaleach',paste0('tvaleach_',formatC(i,width=2,flag=0),'_',formatC(seed,width=10,flag=0),'.txt')))
	}
}
