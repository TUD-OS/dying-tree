library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(tikzDevice)

dir <- '19-12-12_260418.21482'
ht.str <- 'without hyperthreading'

dir <- '19-12-12_260418.27320'
ht.str <- 'with hyperthreading'

dirs <- c('01-21-13_270418.21249', '01-20-08_270418.14076')

dirs <- c('11-50-58_270418.13008')
dirs <- c('13-17-26_270418.2851', '13-27-33_270418.4706', '14-02-43_270418.19261', '17-38-30_270418.9696')
dirs <- c('19-12-24_270418.30131', '19-28-51_270418.4656', '01-28-58_280418.21147')
dirs <- c('10-32-46_280418.9478', '11-13-18_280418.11639')
dirs <- c('11-13-18_280418.11639', '12-00-33_280418.16403')
dirs <- c('12-23-44_280418.15049')
dirs <- c('16-24-29_280418.9047')
dirs <- c('16-38-59_280418.19211')
dirs <- c('17-52-18_280418.25048')

dirs <- c('10-32-01_290418.23296')
dirs <- c('10-50-13_290418.14317')
dirs <- c('11-02-08_290418.5705')

## 48 nodes with HT with faults and  without HT with faults
dirs <- c('13-24-51_290418.25577', '14-32-00_290418.4021')

dirs <- c('15-52-11_290418.6415', '16-27-39_290418.11716')


dirs <- c('08-40-37_020518.5448', '10-04-51_020518.21567')

plot.avg <- function(df) {
    breaks <- unique(df$Size)
    nodes <- unique(df$Nproc)
    ggplot(df, aes(Size, AvgTime.m, group=paste0(factor(Corr), factor(FaultCount)))) +
        geom_ribbon(aes(ymin=AvgTime.m - AvgTime.sd, ymax=AvgTime.m + AvgTime.sd), alpha=0.3) +
        geom_line(aes(col=factor(Corr), lty=factor(FaultCount))) +
        geom_point(aes(shape=factor(Corr), col=factor(Corr))) +
        scale_x_log10(breaks = breaks, labels = breaks) +
        ylab('Latency, us') +
        xlab("Size, b") +
        theme_Publication() %+replace%
        theme(legend.position = 'top',
              legend.direction='horizontal') +
        ggtitle(paste(nodes, 'nodes, Average and std deviation ', ht.str))
}
plot.med <- function(df) {
    breaks <- unique(df$Size)
    nodes <- unique(df$Nproc)
    ggplot(df, aes(Size, AvgTime.50, group=paste0(factor(Corr), factor(FaultCount)))) +
        geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha=0.3) +
        geom_line(aes(col=factor(Corr), lty=factor(FaultCount))) +
        geom_point(aes(shape=factor(Corr), col=factor(Corr))) +
        scale_x_log10(breaks = breaks, labels = breaks) +
        ylab('Latency, us') +
        xlab("Size, b") +
        theme_Publication() %+replace%
        theme(legend.position = 'top',
              legend.direction='horizontal')+
        ggtitle(paste(nodes, 'nodes, Median, 25% and 75% quartiles', ht.str))
}
plot.med.max <- function(df) {
    breaks <- unique(df$Size)
    nodes <- unique(df$Nproc)
    ggplot(df, aes(Size, MaxTime.50, group=paste0(factor(Corr), factor(FaultCount)))) +
        geom_ribbon(aes(ymin=MaxTime.25, ymax=MaxTime.75), alpha=0.3) +
        geom_line(aes(col=factor(Corr), lty=factor(FaultCount))) +
        geom_point(aes(shape=factor(Corr), col=factor(Corr))) +
        scale_x_log10(breaks = breaks, labels = breaks) +
        ylab('Latency, us') +
        xlab("Size, b") +
        theme_Publication() %+replace%
        theme(legend.position = 'top',
              legend.direction='horizontal')+
        ggtitle(paste(nodes, 'nodes, Median, 25% and 75% quartiles of maximum', ht.str))
}
plot.smooth <- function(df) {
    breaks <- unique(df$Size)
    nodes <- unique(df$Nproc)
    ggplot(df, aes(Size, AvgTime, col=factor(Corr))) +
        geom_smooth() +
        scale_x_log10(breaks = breaks, labels = breaks) +
        ylab('Latency, us') +
        xlab("Size, b") +
        theme_Publication() %+replace%
        theme(legend.position = 'top',
              legend.direction='horizontal')+
        ggtitle(paste(nodes, 'nodes, Smoothed mean', ht.str))
}

pdf('/tmp/daint2.pdf', 11, 6)
## for (plot.f in c(plot.avg, plot.med, plot.med.max)) {
##     for (dir in dirs) {
##         df <- read.csv(paste0('../logs/daint/', dir, '/table.csv'))  %>%
##             group_by(CorrType, Corr, Nnodes, Nproc, FaultCount, Size) %>%
##             filter(AvgTime <= quantile(AvgTime, 0.9)) %>%
##             summarize(AvgTime.50 = median(AvgTime),
##                       AvgTime.25 = quantile(AvgTime, .25),
##                       AvgTime.75 = quantile(AvgTime, .75),
##                       AvgTime.m = mean(AvgTime),
##                       AvgTime.sd = sd(AvgTime),
##                       MaxTime.50 = median(MaxTime),
##                       MaxTime.25 = quantile(MaxTime, .25),
##                       MaxTime.75 = quantile(MaxTime, .75)) %>%
##             ungroup() %>%
##             mutate(Corr = sprintf("%s (%s)", CorrType, Corr))
##         print(plot.f(df))
##     }
## }
for (plot.f in c(plot.avg, plot.med, plot.med.max)) {
    for (dir in dirs) {
        df <- read.csv(paste0('../logs/daint/', dir, '/table.csv'))  %>%
            group_by(CorrType, Corr, Nnodes, Nproc, FaultCount, Size) %>%
            filter(Corr == 0) %>%
            summarize(AvgTime.50 = median(AvgTime),
                      AvgTime.25 = quantile(AvgTime, .25),
                      AvgTime.75 = quantile(AvgTime, .75),
                      AvgTime.m = mean(AvgTime),
                      AvgTime.sd = sd(AvgTime),
                      MaxTime.50 = median(MaxTime),
                      MaxTime.25 = quantile(MaxTime, .25),
                      MaxTime.75 = quantile(MaxTime, .75)) %>%
            ungroup() %>%
            mutate(Corr = sprintf("%s (%s)", CorrType, Corr))
        print(plot.f(filter(df, Size<=256)) + ylim(c(0, 50)))
    }
}
for (dir in dirs) {
    df <- read.csv(paste0('../logs/daint/', dir, '/table.csv')) %>%
            mutate(Corr = sprintf("%s (%s)", CorrType, Corr))
    print(plot.smooth(df))
    print(plot.smooth(filter(df, Size<=256)))
}
dev.off()

df <- read.csv(paste0('../logs/daint/', dir, '/table.csv')) 

theme_Publication <- function(base_size=14, base_family="CM Roman") {
    library(grid)
     #, 
    # (theme_foundation(base_size=base_size, base_family=base_family) +
    (theme_foundation() +
        theme(text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(size = rel(1)),
               axis.title.y = element_text(angle=90,vjust=1, size = 10, margin=margin(0, unit="cm")),
               axis.title.x = element_text(vjust = -0.2, size = 10, margin=margin(0.1, unit="cm")),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#c0c0c0"),
               panel.grid.minor = element_blank(),
               panel.spacing = unit(0, "lines"),
               legend.key = element_rect(colour = NA),
               legend.title = element_text(face="italic", size = 10),
               legend.key.size= unit(0.3, "cm"),
               legend.margin = margin(0., unit="cm"),
               legend.box.margin = margin(0., unit="cm"),
               legend.box.spacing = unit(0, "cm"),
               plot.margin=unit(c(1, 1, 1, 1),"mm"),
               strip.background=element_rect(colour="#ffffff",fill="#ffffff"),
               strip.text = element_text(family=NULL, size=10)
          ))
}

colors <- c('#e41a1c', '#377eb8','#4daf4a','#984ea3', "#ff7f00", "black")
fill <- scale_fill_manual(name="Broadcast", values=colors)
col <- scale_color_manual(name="Broadcast", values=colors)
shape <- scale_shape_manual(name="Broadcast", values=c(15, 16, 17, 14, 13))
lty <- scale_linetype_manual(name="Faults", values=c(1, 2, 3, 4))
                                                
breaks <- sort(unique(df$Size))
labels <- breaks

tikz('../../../../tree-broadcast/daint-ribbon.tex', width=3.2, height=2.0)
df %>%
    group_by(CorrType, Corr, Nnodes, Nproc, FaultCount, Size) %>%
    summarize(AvgTime.50 = median(AvgTime),
              AvgTime.25 = quantile(AvgTime, .25),
              AvgTime.75 = quantile(AvgTime, .75),
              AvgTime.m = mean(AvgTime),
              AvgTime.sd = sd(AvgTime),
              MaxTime.50 = median(MaxTime),
              MaxTime.25 = quantile(MaxTime, .25),
              MaxTime.75 = quantile(MaxTime, .75)) %>%
    ungroup() %>%
    filter(!(Corr %in% c(4, 8))) %>%
    mutate(Corr = ifelse(CorrType == 'Native', 'Cray MPI', ifelse(Corr == 0, 'Binomial', sprintf("%s (%s)", CorrType, Corr)))) %>%
    filter(Size >= 4 & Size <= 1024) %>%
    ggplot(aes(Size, AvgTime.50, shape = factor(Corr))) +
    geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha=0.3) +
    geom_line(aes(col=factor(Corr))) +
    geom_point(aes(col=factor(Corr))) +
    scale_x_log10(breaks = breaks, labels = labels) +
    ylab('Latency, $\\mu{}s$') +
    xlab("Size, b") +
    theme_Publication() %+replace%
    theme(legend.position = c(0.25,0.7),
          legend.margin = margin(0.5, unit='mm'),
          legend.box.margin = margin(c(1, 1, 1, 1), unit='mm'),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black", fill='white')) +
    ylim(c(0, 160)) +
    fill + col + lty + shape
dev.off()

size.labeller <- function(x) {
    sprintf("%s bytes payload", x)
}

tikz('../../tree-broadcast/daint.tex', width=3.2, height=2.0)
df %>% filter(Size %in% c(8, 256),
              Nproc %in% breaks) %>%
    group_by(Algorithm, Nproc, FaultCount, Size) %>%
    summarize(AvgTime.50 = median(AvgTime),
              AvgTime.25 = quantile(AvgTime, .25),
              AvgTime.75 = quantile(AvgTime, .75))  %>%
    ggplot(aes(as.factor(Nproc),
               AvgTime.50, group=paste0(factor(Algorithm), factor(FaultCount)))) +
    geom_line(aes(col=factor(Algorithm), lty=factor(FaultCount))) +
    geom_point(aes(shape=factor(Algorithm), col=factor(Algorithm))) +
    facet_grid(~Size, labeller = as_labeller(size.labeller)) +
    ylab('Latency, $\\mu{}s$') +  xlab("Cores") +
    theme_Publication() %+replace%
    theme(legend.position = c(0.5,0.9),
          legend.direction='horizontal',
          legend.margin = margin(0.5, unit='mm'),
          legend.box.margin = margin(c(1, 0, 1, 0.5), unit='mm'),
          legend.spacing = unit(0.5, 'mm'),
          legend.key.height = unit(2, 'mm'),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black", fill='white')) +
    fill + col + lty + shape
dev.off()

tikz('../../tree-broadcast/daint.tex', width=3.2, height=2.0)
df %>% filter(Size %in% c(8, 256),
              Nproc %in% breaks) %>%
    group_by(Algorithm, Nproc, FaultCount, Size) %>%
    summarize(AvgTime.50 = median(AvgTime),
              AvgTime.25 = quantile(AvgTime, .25),
              AvgTime.75 = quantile(AvgTime, .75))  %>%
    ggplot(aes(as.factor(Nproc),
               AvgTime.50, group=paste0(factor(Algorithm), factor(FaultCount)))) +
    geom_line(aes(col=factor(Algorithm), lty=factor(FaultCount))) +
    geom_point(aes(shape=factor(Algorithm), col=factor(Algorithm))) +
    facet_grid(~Size) +
    ylab('Latency, $\\mu{}s$') +  xlab("Cores") +
    theme_Publication() %+replace%
    theme(legend.position = 'top',
          legend.direction='horizontal',
          legend.margin = margin(0.5, unit='mm'),
          legend.box.margin = margin(c(0, 5, 1, 0.5), unit='mm'),
          legend.spacing = unit(0.5, 'mm'),
          legend.key.height = unit(2, 'mm'),
          legend.background = element_blank()) +
    fill + col + lty + shape
dev.off()
