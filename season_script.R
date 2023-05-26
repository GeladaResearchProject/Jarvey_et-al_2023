#!/usr/bin/env Rscript

gelada.scans = readRDS('~/Downloads/scans_with_dobs.RDS')

imp.rain = read.csv('~/Downloads/imp1_rain.csv',row.names=1)

library(lubridate)

imp.rain$Date = mdy(imp.rain$Date)

gelada.scans = merge(gelada.scans,imp.rain,by='Date',all.x=TRUE)

gelada.scans$context = factor(gelada.scans$Context,levels=c('Rest','Social','Move','FA','FB'))

gelada.scans$context3 = factor(gelada.scans$Context,levels=c('Rest','Social','Move','FA','FB'),labels=c('Social/Rest','Social/Rest','Feed/Forage','Feed/Forage','Feed Below'))

gelada.scan.rates = do.call(rbind,lapply(split(gelada.scans,with(gelada.scans,list(ID,context,Season))),function(x) {
	if (nrow(x)) {
		data.frame(
			id = unique(x$ID),
			elo_rating = unique(x$Elo.standardized),
			agg_rate = sum(x$within_aggression)/nrow(x),
			btw_rate = sum(x$between_aggression)/nrow(x),
			season = unique(x$Season),
			context = unique(x$context),
			n=nrow(x)
		)
	} else {
		data.frame(
			id = character(0),
			elo_rating = numeric(0),
			agg_rate = numeric(0),
			btw_rate = numeric(0),
			season = character(0),
			context = character(0),
			n=integer(0)
		)
	}
}))
rownames(gelada.scan.rates) = NULL

gelada.scan.rates = within(gelada.scan.rates,{
	season_label = factor(season,levels=c('Wet','Dry'),labels=c('Wet season','Dry season'))
	context_label = factor(context,levels=c('Rest','Social','Move','FA','FB'),labels=c('Rest','Social','Move','Feed above','Feed below'))
})

saveRDS(gelada.scans,file='gelada_scans.rds')

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
			elo_rating = unique(x$Elo.standardized),
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

gelada.scan.rates2 = within(gelada.scan.rates2,{
	season_label = factor(season,levels=c('Wet','Dry'),labels=c('Wet season','Dry season'))
	context_label = factor(context,levels=c('Social/Rest','Feed/Forage','Feed Below'),labels=c('Social/Rest','Feed/Forage','Feed Below'))
})

p1 = ggplot() +
	geom_point(data=gelada.scan.rates,aes(elo_rating,agg_rate,alpha=n)) +
	geom_smooth(data=gelada.scan.rates,aes(elo_rating,agg_rate),method=lm,se=FALSE) +
	facet_grid(season_label~context_label) +
	scale_x_continuous(breaks=seq(500,1500,500)) +
	scale_alpha_continuous(name='Number of observations') +
	theme_classic() +
	theme(
		strip.text.y=element_text(angle=0),
		strip.background=element_blank(),
		axis.text.x = element_text(angle=-45,hjust=0,vjust=1)
	) +
	xlab('Elo Rating') +
	ylab('Aggression Rate')


p2 = ggplot() +
#	geom_point(data=gelada.scan.rates2,aes(elo_rating,agg_rate,alpha=n),size=2) +
	geom_smooth(data=gelada.scan.rates2,aes(elo_rating,agg_rate),method=lm,se=TRUE,color=NA,fill='#999999',alpha=0.2,size=0) +
	geom_point(data=gelada.scan.rates2,aes(elo_rating,agg_rate,color=ifelse(n > (mean(n) + 1.5 * sd(n)),mean(n) + 1.5 * sd(n),n)),size=2) +
	geom_smooth(data=gelada.scan.rates2,aes(elo_rating,agg_rate),method=lm,se=FALSE,color='#000000',linetype=2,size=1) +
	facet_grid(season_label~context_label) +
	scale_x_continuous(breaks=seq(500,1500,500)) +
	scale_y_continuous(breaks=seq(0,0.8,0.4)) +
	scale_color_viridis(name=expression(italic(N)['obs.']),breaks=seq(0,100,50)) +
	# scale_alpha_continuous(name=expression(italic(N)['obs.'])) +
	coord_cartesian(ylim=c(0,1)) +
	theme_classic(base_size=24) +
	theme(
		strip.text.y=element_text(angle=0),
		strip.background=element_blank(),
		axis.text.x = element_text(angle=-45,hjust=0,vjust=1),
		legend.position='right'
	) +
	# guides(alpha=guide_legend(override.aes = list(size=4))) +
	xlab('Elo Rating') +
	ylab('Aggression Rate')
ggsave(p2,file='interaction_comparison.pdf',useDingbats=FALSE,width=10,height=7)

p2 = ggplot() +
#	geom_point(data=gelada.scan.rates2,aes(elo_rating,agg_rate,alpha=n),size=2) +
	geom_smooth(data=gelada.scan.rates2,aes(elo_rating,agg_rate),method=lm,se=TRUE,color=NA,fill='#999999',alpha=0.2,size=0) +
	geom_point(data=gelada.scan.rates2,aes(elo_rating,agg_rate,color=ifelse(n > (mean(n) + 1.5 * sd(n)),mean(n) + 1.5 * sd(n),n)),size=2) +
	geom_smooth(data=gelada.scan.rates2,aes(elo_rating,agg_rate),method=lm,se=FALSE,color='#000000',linetype=2,size=1) +
	facet_grid(season_label~context_label) +
	scale_x_continuous(breaks=seq(-1,1,1)) +
	scale_y_continuous(breaks=seq(0,0.8,0.4)) +
	scale_color_viridis(name=expression(italic(N)['obs.']),breaks=seq(0,100,50)) +
	# scale_alpha_continuous(name=expression(italic(N)['obs.'])) +
	coord_cartesian(ylim=c(0,1)) +
	theme_classic(base_size=24) +
	theme(
		strip.text.y=element_text(angle=0),
		strip.background=element_blank(),
		# axis.text.x = element_text(angle=-45,hjust=0,vjust=1),
		legend.position='right'
	) +
	# guides(alpha=guide_legend(override.aes = list(size=4))) +
	xlab('Standardized Rank') +
	ylab('Aggression Rate')
ggsave(p2,file='interaction_comparison_v3.pdf',useDingbats=FALSE,width=10,height=7)

p2 = ggplot() +
	geom_point(data=gelada.scan.rates2,aes(elo_rating,agg_rate,alpha=n),size=2) +
	geom_smooth(data=gelada.scan.rates2,aes(elo_rating,agg_rate),method=lm,se=FALSE) +
	facet_grid(season_label~context_label) +
	scale_x_continuous(breaks=seq(500,1500,500)) +
	scale_y_continuous(breaks=seq(0,0.8,0.4)) +
	# scale_color_viridis(name=expression(italic(N)['obs.']),breaks=seq(0,100,50)) +
	scale_alpha_continuous(name=expression(italic(N)['obs.'])) +
	coord_cartesian(ylim=c(0,1)) +
	theme_classic(base_size=24) +
	theme(
		strip.text.y=element_text(angle=0),
		strip.background=element_blank(),
		axis.text.x = element_text(angle=-45,hjust=0,vjust=1),
		legend.position='right'
	) +
	# guides(alpha=guide_legend(override.aes = list(size=4))) +
	xlab('Elo Rating') +
	ylab('Aggression Rate')
ggsave(p2,file='interaction_comparison_v2.pdf',useDingbats=FALSE,width=10,height=7)



library(tidyverse)

gelada.scan.rates.long = pivot_longer(as_tibble(gelada.scan.rates2),cols=c('agg_rate','btw_rate'))

gelada.scan.rates.long$group_context = factor(gelada.scan.rates.long$name,levels=c('agg_rate','btw_rate'),labels=c('within group','between group'))

library(ggbeeswarm)

p3 = ggplot() +
	geom_quasirandom(data=gelada.scan.rates.long,aes(group_context,value,color=group_context)) +
	geom_boxplot(data=gelada.scan.rates.long,aes(group_context,value),width=0.1,color='#666666',fill='#ffffff',outlier.shape=NA,alpha=0.75) +
	facet_grid(season_label~context_label) +
	scale_color_brewer(palette='Dark2') +
	scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.5)) +
	# scale_fill_brewer(palette='Dark2') +
	theme_classic(base_size=24) +
	theme(
		strip.text.y=element_text(angle=0),
		strip.background=element_blank(),
		# axis.text.x = element_text(angle=-45,hjust=0,vjust=1),
		axis.text.x = element_blank(),
		axis.ticks.x = element_blank(),
		axis.title.x = element_blank(),
		legend.title=element_blank(),
		legend.position='bottom'
	) +
	guides(color=guide_legend(override.aes = list(size=4,alpha=1))) +
	ylab('Aggression Rate')
ggsave(p3,file='group_comparison.pdf',useDingbats=FALSE,width=10,height=7)




out = do.call(rbind,lapply(split(gelada.scans,gelada.scans$Date),function(x) {
	data.frame(
		Date = unique(x$Date),
		Rate = with(x,sum(Context == 'FB')/sum(Context %in% c('FA','FB'))),
		Rain = unique(x$Rain90),
		Rain30 = unique(x$Rain30),
		Rain90 = unique(x$Rain90)
	)
}))


p = ggplot(out,aes(x=Rain,y=Rate,fill=as.integer(Date))) +
	geom_point(size=3,color='#000000',shape=21) +
	scale_fill_viridis(
		breaks=as.integer(as.Date(paste('2015',formatC(seq(2,7),width=2,flag=0),'15',sep='-'))),
		labels=rev(lubridate::month(as.Date(paste('2015',formatC(seq(2,7),width=2,flag=0),'15',sep='-')),label=TRUE,abbr=TRUE))
	) +
	theme_classic(base_size=24) +
	xlab('Cumulative Rainfall (mm. in past 90 days)') +
	ylab(expression('Fraction Feeding Below')) +
	guides(fill=guide_colorbar()) +
	theme(legend.key.height=unit(50,'native'),legend.title=element_blank())
ggsave(p,file='rainfall_by_feeding_rate.pdf',useDingbats=FALSE,width=10,height=7)


p = ggplot(out,aes(x=Rain30,y=Rate,fill=as.integer(Date))) +
	geom_point(size=3,color='#000000',shape=21) +
	scale_fill_viridis(
		breaks=as.integer(as.Date(paste('2015',formatC(seq(2,7),width=2,flag=0),'15',sep='-'))),
		labels=lubridate::month(as.Date(paste('2015',formatC(seq(2,7),width=2,flag=0),'15',sep='-')),label=TRUE,abbr=TRUE)
		#direction=-1
	) +
	theme_classic(base_size=24) +
	xlab('Cumulative Rainfall (mm. in past 30 days)') +
	ylab(expression('Fraction Feeding Below')) +
	guides(fill=guide_colorbar()) +
	theme(legend.key.height=unit(50,'native'),legend.title=element_blank())
ggsave(p,file='rainfall_by_feeding_rate30.pdf',useDingbats=FALSE,width=10,height=7)

