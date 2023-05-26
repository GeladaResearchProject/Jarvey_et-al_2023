#!/usr/bin/env Rscript

real.results = readRDS(file.path('results',paste0('results_permutation_',formatC(0,width=10,flag=0),'.rds')))

if (!file.exists('stats/results_pval.txt.gz')) system(paste0('cat stats/pval/* | gzip -c > stats/results_pval.txt.gz'))
if (!file.exists('stats/results_best.txt.gz')) system(paste0('cat stats/best/* | gzip -c > stats/results_best.txt.gz'))

codes = c('Feed Below.Dry','Feed/Forage.Dry','Social/Rest.Dry','Feed Below.Wet','Feed/Forage.Wet','Social/Rest.Wet')

for (i in 1:length(codes)) {
	if (!file.exists(paste0('stats/results_pvaleach_',formatC(i,width=2,flag=0),'.txt.gz'))) system(paste0('cat stats/pvaleach/pvaleach_',formatC(i,width=2,flag=0),'_* | gzip -c > stats/results_pvaleach_',formatC(i,width=2,flag=0),'.txt.gz'))
	if (!file.exists(paste0('stats/results_tvaleach_',formatC(i,width=2,flag=0),'.txt.gz'))) system(paste0('cat stats/tvaleach/tvaleach_',formatC(i,width=2,flag=0),'_* | gzip -c > stats/results_tvaleach_',formatC(i,width=2,flag=0),'.txt.gz'))
	if (!file.exists(paste0('stats/results_bestdiff_',formatC(i,width=2,flag=0),'.txt.gz'))) system(paste0('cat stats/bestdiff/bestdiff_',formatC(i,width=2,flag=0),'_* | gzip -c > stats/results_bestdiff_',formatC(i,width=2,flag=0),'.txt.gz'))
}

library(data.table)

permutation.results = do.call(rbind,lapply(1:length(codes),function(i) {
	code = codes[i]
	values = unlist(strsplit(code,'\\.'))
	context = values[1]
	season = values[2]
	pval.each = fread(paste0('stats/results_pvaleach_',formatC(i,width=2,flag=0),'.txt.gz'))$V1
	tval.each = fread(paste0('stats/results_tvaleach_',formatC(i,width=2,flag=0),'.txt.gz'))$V1
	best.diff = fread(paste0('stats/results_bestdiff_',formatC(i,width=2,flag=0),'.txt.gz'))$V1
	out = data.frame(
		rbind(
			data.frame(stat = factor('pval',levels=c('pval','tval','tdiff')),value=pval.each),
			data.frame(stat = factor('tval',levels=c('pval','tval','tdiff')),value=tval.each),
			data.frame(stat = factor('tdiff',levels=c('pval','tval','tdiff')),value=best.diff)
		),
		code = code,
		context = factor(context,levels=c('Social/Rest','Feed/Forage','Feed Below')),
		season = factor(season,levels=c('Wet','Dry'),labels=c('Wet Season','Dry Season'))
	)
}))

saveRDS(permutation.results,file='permutation_results.rds')

true.results = do.call(rbind,lapply(1:nrow(real.results),function(i) {
	code = rownames(real.results)[i]
	values = unlist(strsplit(code,'\\.'))
	context = values[1]
	season = values[2]
	pval.each = real.results[code,'pval']
	tval.each = real.results[code,'tval']
	best.diff = min(real.results[!rownames(real.results) %in% code,'tval']) - real.results[code,'tval']
	out = data.frame(
		rbind(
			data.frame(stat = factor('pval',levels=c('pval','tval','tdiff')),value=pval.each),
			data.frame(stat = factor('tval',levels=c('pval','tval','tdiff')),value=tval.each),
			data.frame(stat = factor('tdiff',levels=c('pval','tval','tdiff')),value=best.diff)
		),
		code = code,
		context = factor(context,levels=c('Social/Rest','Feed/Forage','Feed Below')),
		season = factor(season,levels=c('Wet','Dry'),labels=c('Wet Season','Dry Season'))
	)
}))

test.results = do.call(rbind,lapply(1:nrow(real.results),function(i) {
	code = rownames(real.results)[i]
	values = unlist(strsplit(code,'\\.'))
	context = values[1]
	season = values[2]
	pval.each = mean(permutation.results[permutation.results$code == code & permutation.results$stat == 'pval','value'] < true.results[true.results$code == code & true.results$stat == 'pval','value'])
	tval.each = mean(permutation.results[permutation.results$code == code & permutation.results$stat == 'tval','value'] < true.results[true.results$code == code & true.results$stat == 'tval','value'])
	best.diff = mean(permutation.results[permutation.results$code == code & permutation.results$stat == 'tdiff','value'] > true.results[true.results$code == code & true.results$stat == 'tdiff','value'])
	out = data.frame(
		rbind(
			data.frame(stat = factor('pval',levels=c('pval','tval','tdiff')),value=pval.each),
			data.frame(stat = factor('tval',levels=c('pval','tval','tdiff')),value=tval.each),
			data.frame(stat = factor('tdiff',levels=c('pval','tval','tdiff')),value=best.diff)
		),
		code = code,
		context = factor(context,levels=c('Social/Rest','Feed/Forage','Feed Below')),
		season = factor(season,levels=c('Wet','Dry'),labels=c('Wet Season','Dry Season'))
	)
}))

true.results$test = test.results$value

this.stat = 'pval'
x.min = 0
x.max = 8
y.text = 1.25e4
x.nudge = 0.1
p1 = ggplot() +
	geom_histogram(data=subset(permutation.results,stat == this.stat),aes(-log10(value)),bins=50) +
	geom_vline(data=subset(true.results,stat == this.stat),aes(xintercept=-log10(value)),color='#ff0000') +
	geom_text(data=subset(true.results,stat == this.stat),aes(x=-log10(value),y=y.text,label=paste0('italic(p)[perm.]~"= ',format(round(test,5),nsmall=5,scientific=FALSE),'"')),nudge_x=x.nudge,parse=TRUE,hjust=0) +
	facet_grid(season~context) +
	coord_cartesian(xlim=c(x.min,x.max),ylim=c(0,1.2*y.text)) +
	theme_classic(base_size=24) +
	theme(
		strip.text.y=element_text(angle=0),
		strip.background=element_blank(),
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		legend.title=element_blank()
	) +
	xlab(expression(-log[10]~italic(p)*'-value')) +
	ylab('Count')


this.stat = 'tdiff'
x.min = -6
x.max = 4
y.text = 0.8e4
x.nudge=0.1
x.lab = expression('Standardized'~italic(beta)~'difference')
p2 = ggplot() +
	geom_histogram(data=subset(permutation.results,stat == this.stat),aes(-value),bins=100) +
	geom_vline(data=subset(true.results,stat == this.stat),aes(xintercept=-value),color='#ff0000') +
	geom_text(data=subset(true.results,stat == this.stat),aes(x=-value,y=y.text,label=paste0('italic(p)[perm.]~"= ',format(round(test,5),nsmall=5,scientific=FALSE),'"')),nudge_x=x.nudge,parse=TRUE,hjust=0) +
	facet_grid(season~context) +
	coord_cartesian(xlim=c(x.min,x.max),ylim=c(0,1.1*y.text)) +
	theme_classic(base_size=24) +
	theme(
		strip.text.y=element_text(angle=0),
		strip.background=element_blank(),
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		legend.title=element_blank()
	) +
	xlab() +
	ylab('Count')

this.stat = 'tval'
x.min = -8
x.max = 2
y.text = 0.8e4
x.nudge = 0.1
x.lab = expression('Standardized'~italic(beta))
p3 = ggplot() +
	geom_histogram(data=subset(permutation.results,stat == this.stat),aes(value),bins=100) +
	geom_vline(data=subset(true.results,stat == this.stat),aes(xintercept=value),color='#ff0000') +
	geom_text(data=subset(true.results,stat == this.stat),aes(x=value,y=y.text,label=paste0('italic(p)[perm.]~"= ',format(round(test,5),nsmall=5,scientific=FALSE),'"')),nudge_x=x.nudge,parse=TRUE,hjust=0) +
	facet_grid(season~context) +
	coord_cartesian(xlim=c(x.min,x.max),ylim=c(0,1.1*y.text)) +
	theme_classic(base_size=24) +
	theme(
		strip.text.y=element_text(angle=0),
		strip.background=element_blank(),
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		legend.title=element_blank()
	) +
	xlab(expression('Std.'~italic(beta))) +
	ylab('Count')

library(egg)
pdf(file='permutations.pdf',useDingbats=FALSE,width=10,height=7)
ggarrange(p3,p2,nrow=2)
dev.off()


