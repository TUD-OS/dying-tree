library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(tikzDevice)

dirs <- c('18-00-13_300418.2674.taurus')

dir <- '10-08-23_010518.4806.taurus'
dir <- '12-18-26_010518.12209.taurus'
dir <- '13-09-55_010518.13461.taurus'

dir <- '14-18-10_010518.32479.taurus'
dir <- '16-01-25_010518.29132.taurus'
dir <- '02-00-09_020518.19669.taurus'

dir <- '08-55-36_020518.2906.taurus'

dir <- '09-26-01_020518.10513.taurus'

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

pdf('/tmp/taurus.pdf', 11, 6)
for (plot.f in c(plot.avg, plot.med, plot.med.max)) {
    for (dir in dirs) {
        df <- read.csv(paste0('../logs/taurus/', dir, '/table.csv'))  %>%
            group_by(CorrType, Corr, Nnodes, Nproc, FaultCount, Size) %>%
            filter(AvgTime <= quantile(AvgTime, 0.9)) %>%
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
        print(plot.f(df))
    }
}
for (plot.f in c(plot.avg, plot.med, plot.med.max)) {
    for (dir in dirs) {
        df <- read.csv(paste0('../logs/taurus/', dir, '/table.csv'))  %>%
            group_by(CorrType, Corr, Nnodes, Nproc, FaultCount, Size) %>%
            filter(AvgTime <= quantile(AvgTime, 0.9)) %>%
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
        print(plot.f(filter(df, Size<=256)))
    }
}
for (dir in dirs) {
    df <- read.csv(paste0('../logs/taurus/', dir, '/table.csv')) %>%
            mutate(Corr = sprintf("%s (%s)", CorrType, Corr))
    print(plot.smooth(df))
    print(plot.smooth(filter(df, Size<=256)))
}
dev.off()

df <- read.csv(paste0('../logs/taurus/', dir, '/table.csv')) %>%
    mutate(Nproc = Nnodes * 24)

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
                                                
df <- read.csv(paste0('../logs/taurus/', dir, '/table.csv')) %>%
    mutate(Nproc = Nnodes * 24) %>%
    mutate(Corr = ifelse(Corr == 6, 'Open~MPI',
                  ifelse(Corr == 7, 'Corrected', 'XXX')))
df.sum <- df %>% 
    group_by(Corr, Algorithm, Nproc, FaultCount, Size) %>%
    summarize(AvgTime.50 = median(AvgTime),
              AvgTime.25 = quantile(AvgTime, .25),
              AvgTime.75 = quantile(AvgTime, .75))
tikz('../../../../tree-broadcast/taurus.tex', width=3, height=2.0)
breaks <- sort(unique(df.sum$Size))
labels <- breaks
df.sum %>% filter(Size %in% c(8, 256) & (Algorithm == 'Open~MPI' | Corr == 2)) %>%
    ggplot(aes(as.factor(Nproc),
               AvgTime.50, group=paste0(factor(Algorithm), factor(FaultCount)))) +
    geom_line(aes(col=factor(Algorithm), lty=factor(FaultCount))) +
    geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha=0.3) +
    geom_point(aes(shape=factor(Algorithm), col=factor(Algorithm))) +
    facet_grid(~Size) +
    ylab('Latency, $\\mu{}s$') +  xlab("Cores") +
    theme_Publication() %+replace%
    theme(legend.position = 'right',
          legend.margin = margin(0.5, unit='mm'),
          legend.box.margin = margin(c(1, 1, 1, 1), unit='mm'),
          legend.spacing = unit(1, 'mm'),
          legend.background = element_blank()) +
    ylim(c(11,18)) +
    fill + col + lty + shape
dev.off()

df <- read.csv(paste0('../logs/taurus/', dir, '/table.csv')) %>%
    mutate(Nproc = Nnodes * 24)
df.sum <- df %>% 
    group_by(Corr, Algorithm, Nproc, FaultCount, Size) %>%
    summarize(AvgTime.50 = median(AvgTime),
              AvgTime.25 = quantile(AvgTime, .25),
              AvgTime.75 = quantile(AvgTime, .75))
tikz('../../../../tree-broadcast/taurus.tex', width=2.1, height=2.0)

breaks <- sort(unique(df.sum$Size))
labels <- breaks
col <- scale_color_manual(name="Correction", values=colors)
shape <- scale_shape_manual(name="Correction", values=c(15, 16, 17, 14, 13))
df.sum %>% filter(Size %in% c(256) & (Algorithm == 7)) %>%
    ggplot(aes(as.factor(Nproc),
               AvgTime.50, group=paste0(factor(Algorithm), factor(Corr)))) +
    geom_line(aes(col=factor(Corr))) +
    geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha=0.3) +
    geom_point(aes(shape=factor(Corr))) +
    facet_grid(~FaultCount) +
    ylab('Latency, $\\mu{}s$') +  xlab("Cores") +
    theme_Publication() %+replace%
    theme(legend.position = 'top',
          legend.direction='horizontal',
          legend.box='vertical',
          legend.margin = margin(0.5, unit='mm'),
          legend.box.margin = margin(c(0, 0, 1, 0.5), unit='mm'),
          legend.spacing = unit(0.5, 'mm'),
          legend.key.height = unit(2, 'mm'),
          legend.background = element_blank()) +
    col + shape + ylim(c(12,20))

dev.off()

pdf('/tmp/taurus2.pdf', 16, 9)
df.sum %>% filter(FaultCount == 0 & Algorithm != 'Native' & Nproc == 432) %>%
    ggplot(aes(Size,
               AvgTime.50, group=paste0(factor(Algorithm), factor(Corr)))) +
    geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha=0.3) +
    geom_line(aes(col=factor(Algorithm), lty=factor(Corr))) +
    geom_point(aes(shape=factor(Algorithm), col=factor(Algorithm))) +
    scale_x_log10(breaks = breaks, labels = breaks) +
    facet_grid(~Nproc) +
    ylab('Latency, $\\mu{}s$') +  xlab("Size, b") +
    theme_Publication() %+replace%
    theme(legend.position = 'top',
          legend.direction='horizontal',
          legend.margin = margin(0.5, unit='mm'),
          legend.box.margin = margin(c(0, 5, 1, 0.5), unit='mm'),
          legend.spacing = unit(0.5, 'mm'),
          legend.key.height = unit(2, 'mm'),
          legend.background = element_blank()) +
    fill + col + shape + scale_linetype_manual(name="Correction", values=c(1, 2, 3, 4)) +
        ylim(c(0,20))
df.sum %>% filter(Corr == 2) %>%
    ggplot(aes(Size,
               AvgTime.50, group=paste0(factor(Algorithm), factor(FaultCount)))) +
    geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha=0.3) +
    geom_line(aes(col=factor(Algorithm), lty=factor(FaultCount))) +
    geom_point(aes(shape=factor(Algorithm), col=factor(Algorithm))) +
    scale_x_log10(breaks = breaks, labels = breaks) +
    facet_grid(~Nproc) +
    ylab('Latency, $\\mu{}s$') +  xlab("Size, b") +
    theme_Publication() %+replace%
    theme(legend.position = 'top',
          legend.direction='horizontal',
          legend.margin = margin(0.5, unit='mm'),
          legend.box.margin = margin(c(0, 5, 1, 0.5), unit='mm'),
          legend.spacing = unit(0.5, 'mm'),
          legend.key.height = unit(2, 'mm'),
          legend.background = element_blank()) +
    fill + col + lty + shape +
        ggtitle("Correction 2")
df.sum %>% filter(Corr == 4) %>%
    ggplot(aes(Size,
               AvgTime.50, group=paste0(factor(Algorithm), factor(FaultCount)))) +
    geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha=0.3) +
    geom_line(aes(col=factor(Algorithm), lty=factor(FaultCount))) +
    geom_point(aes(shape=factor(Algorithm), col=factor(Algorithm))) +
    scale_x_log10(breaks = breaks, labels = breaks) +
    facet_grid(~Nproc) +
    ylab('Latency, $\\mu{}s$') +  xlab("Size, b") +
    theme_Publication() %+replace%
    theme(legend.position = 'top',
          legend.direction='horizontal',
          legend.margin = margin(0.5, unit='mm'),
          legend.box.margin = margin(c(0, 5, 1, 0.5), unit='mm'),
          legend.spacing = unit(0.5, 'mm'),
          legend.key.height = unit(2, 'mm'),
          legend.background = element_blank()) +
    fill + col + lty + shape +
        ggtitle("Correction 4")
dev.off()
