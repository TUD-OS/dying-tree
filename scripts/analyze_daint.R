library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(tikzDevice)
library(stringr)


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

dirs <- c('08-11-27_030518.1892', '00-27-47_030518.31439', '08-06-15_030518.28618', '06-49-23_030518.13274', '07-09-50_030518.18168', '06-54-29_030518.14817')

dirs <- c('18-04-20_160818.17474', '20-03-10_160818.1464')

dirs <- c('14-18-09_170818.22896', '14-18-54_170818.11243', '14-18-54_170818.30747', '14-18-54_170818.413', '14-19-08_170818.16306')

dirs <- c('22-40-56_170818.7462', '170818_22-49-29.18183', '170818_22-49-55.5705')

dirs <- c('180818_00-10-18.28049', '170818_23-47-31.6091', '170818_23-29-24.26143', '170818_23-33-51.28153', '170818_23-33-52.32189', '170818_23-38-58.8863')

dirs <- c('180818_11-41-38.6027')

dirs <- c('180818_11-50-10.25330', '180818_11-50-10.28131', '180818_11-50-10.30858', '180818_11-50-10.9209')


read.dirs <- function(dirs) {
    df <- NULL
    for (dir in dirs) {
        df.cur <- read.csv(paste0('../logs/daint/', dir, '/table.csv'), stringsAsFactors=FALSE) 
        if (is.null(df)) {
            df <- df.cur
        } else {
            df <- bind_rows(df, df.cur)
        }
    }
    df <- df %>%
        select(-FaultList, -Iterations)

    df <- df %>%
        group_by(TreeType, LameK, CorrType, Corr, Nnodes, Nproc, FaultCount, Size, GossipRounds) %>%
        summarize(AvgTime.50 = median(AvgTime),
                  AvgTime.25 = quantile(AvgTime, .25),
                  AvgTime.75 = quantile(AvgTime, .75),
                  AvgTime.m = mean(AvgTime),
                  AvgTime.sd = sd(AvgTime),
                  MaxTime.50 = median(MaxTime),
                  MaxTime.25 = quantile(MaxTime, .25),
                  MaxTime.75 = quantile(MaxTime, .75)) %>%
        ungroup() %>%
        mutate(CorrFull = sprintf("%s (%s)", CorrType, Corr))
    return (df)
}

cur.size = 128

## Baseline comparison
df <- read.dirs(c('180818_14-53-53.19551', '180818_15-12-16.6507', '180818_11-50-10.30858', '180818_11-50-10.28131_170818_23-47-31.6091', '180818_00-10-18.28049_180818_11-29-41.20840', '180818_17-04-13.32120'))


colors <- c('#e41a1c', '#377eb8','#4daf4a','#984ea3', "#ff7f00", "black")
## fill <- scale_fill_manual(name="Broadcast", values=colors)
col <- scale_color_manual(name="Broadcast", values=colors)
shape <- scale_shape_manual(name="Broadcast", values=c(15, 16, 17, 14, 13))
lty <- scale_linetype_manual(name="Correction", values=c(1, 2, 3, 4))
breaks <- sort(unique(df$Nproc))
labels <- breaks
tikz('../../../../tree-broadcast/plots/daint-baseline.tex', width=3.2, height=2.0)
df %>%
    filter(Size == cur.size, TreeType=='tree_binomial', Corr==0, CorrType != 'Mapping') %>%
    ggplot(aes(as.factor(Nproc), AvgTime.50, group=factor(CorrType), shape=factor(CorrType))) +
    geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha = 0.3)+ 
    geom_line(aes(col=factor(CorrType))) +
    geom_point(aes(col=factor(CorrType))) +
    theme_Publication() %+replace%
    theme(legend.position = 'top',
          legend.box = 'vertical',
          legend.spacing.y = unit(1, unit='mm'),
          legend.margin = margin(0.0, unit='mm'),
          legend.key.width = unit(5, unit='mm'),
          legend.box.margin = margin(c(0, 0, 0, 0), unit='mm'),
          legend.background = element_blank()) +
    scale_x_discrete(breaks = breaks, labels = labels) +
    ylab('Latency, $\\mu{}s$') +
    xlab("Processes") +
    col + lty + shape +
    ylim(c(0,75))
dev.off()


## Gossip
df.gossip <- read.dirs(c('180818_23-01-27.12313', '180818_23-09-20.14878', '180818_22-02-24.12371', '180818_22-29-02.26699', '180818_22-53-01.10754', '180818_23-26-15.10011'))

df.all <- df.gossip %>%
    group_by(Size, TreeType, Nnodes) %>%
    filter(AvgTime.50 == min(AvgTime.50)) %>%
    ungroup() %>%
    filter(TreeType == 'gossip') %>%
    bind_rows(df)

colors <- c("black", '#e41a1c', '#377eb8','#4daf4a','#984ea3', "#ff7f00")
## fill <- scale_fill_manual(name="Broadcast", values=colors)
col <- scale_color_manual(name="Broadcast", values=colors)
shape <- scale_shape_manual(name="Broadcast", values=c(15, 16, 17, 14, 13))
lty <- scale_linetype_manual(name="Correction", values=c(1, 2, 3, 4))
breaks <- sort(unique(df$Nproc))
labels <- breaks

tikz('../../../../tree-broadcast/plots/daint-no-faults.tex', width=3.2, height=2.0)

annotation.df <- rbindlist(
    list(
        list(x=7000, xend=12800, y=120, yend=103, color='black', label="Gossip", vjust='bottom', hjust='center'),
        list(20000, 15700, 13, 32.5, '#e41a1c', 'Binomial (Cray)', 'top', 'center'),
        list(23500, 26500, 92, 74, '#377eb8', 'Binomial (ours)', 'bottom', 'center'),
        list(9000, 14800, 45, 41, '#4daf4a', "Binomial (Cray, no SM)", 'center', 'right')
    ),
    use.name=TRUE, fill=FALSE, idcol=FALSE)
p <- df.all %>%
    filter(Size == cur.size,
           CorrType != 'Mapping') %>%
    mutate(Type = as.factor(paste0(TreeType, CorrType))) %>%
    ggplot(aes(Nproc, AvgTime.50, group=Type)) +
    geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha = 0.3)+ 
    geom_line(aes(col=Type)) +
    geom_point(aes(shape=Type, col=Type)) +
    theme_Publication() %+replace%
    theme(legend.position = 'none',
          legend.box = 'vertical',
          legend.spacing.y = unit(1, unit='mm'),
          legend.margin = margin(0.0, unit='mm'),
          legend.key.width = unit(5, unit='mm'),
          legend.box.margin = margin(c(0, 0, 0, 0), unit='mm'),
          legend.background = element_blank()) +
    scale_x_log10(breaks = breaks, labels = labels) +
    ylab('Latency, $\\mu{}s$') +
    xlab("Processes") +
    ylim(c(0,130)) +
    annotate("segment",
             x=annotation.df$x, xend=annotation.df$xend,
             y=annotation.df$y, yend=annotation.df$yend,
             color=annotation.df$color,
             arrow=arrow(length = unit(2, "points"), type='closed'), lineend = 'round') +
    annotate("text",
             x=annotation.df$x, y=annotation.df$y+0.05,
             label=annotation.df$label,
             vjust=annotation.df$vjust, hjust=annotation.df$hjust,
             color=annotation.df$color, size=3, alpha=1) +
    col
ggplotly(p)

dev.off()

## df.trees <- read.dirs(c('190818_13-33-52.27729', '190818_13-37-15.8603', '190818_13-37-15.31546'))
## df.trees <- read.dirs(c('190818_14-54-39.24986', '190818_14-54-59.22228', '190818_14-55-13.8801'))
## df.trees <- read.dirs(c('190818_15-19-20.30623', '190818_15-19-20.23258', '190818_15-19-20.2114'))

df.trees <- read.dirs(c('190818_15-40-23.17345', '190818_15-41-24.21472', '190818_20-07-04.30393', '190818_19-37-36.29645', '190818_19-52-19.21339', '190818_20-09-26.20031'))

df.all <- df.gossip %>%
    group_by(Size, TreeType, Nnodes) %>%
    filter(AvgTime.50 == min(AvgTime.50)) %>%
    ungroup() %>%
    filter(TreeType == 'gossip') %>%
    bind_rows(df.trees)


tikz('../../../../tree-broadcast/plots/daint-correction.tex', width=3.2, height=2.0)
annotation.df <- rbindlist(
    list(
        list(x=4700, xend=7000, y=56, yend=53.5, color='#4daf4a', label='Binomial ($d=2$)', vjust='bottom', hjust='right'),
        list(20500, 10700, 19, 30.5, '#ff7f00', 'Lame ($d=0, k=4$)', 'top', 'center'),
        list(2000, 3000, 12, 21, '#e41a1c', "Binomial ($d=0$)", 'top', 'center'),
        list(1800, 4000, 47, 35, '#377eb8', "Binomial ($d=1$)", 'bottom', 'center'),
        list(16000, 20000, 62, 47.5, '#984ea3', "Binomial ($d=2$, with faults)", 'bottom', 'center')
    ),
    use.name=TRUE, fill=FALSE, idcol=FALSE)
colors <- c('#e41a1c', '#377eb8','#4daf4a','#984ea3', "#ff7f00")
## fill <- scale_fill_manual(name="Broadcast", values=colors)
col <- scale_color_manual(name="Broadcast", values=colors)
shape <- scale_shape_manual(name="Broadcast", values=c(15, 16, 17, 14, 13, 12))
lty <- scale_linetype_manual(name="Correction", values=c(1, 2, 3, 4))
breaks <- sort(unique(df$Nproc))
labels <- breaks
p <- df.trees %>%
    mutate(group = as.factor(paste(TreeType, CorrType, Corr, FaultCount))) %>%
    filter(Size %in% c(cur.size),
           CorrType != 'Native') %>%
    mutate(CorrType = ifelse(CorrType == 'Native', 'Cray MPI', 'Corrected')) %>%
    ggplot(aes(Nproc, AvgTime.50, group=group)) +
    geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha = 0.3)+ 
    geom_line(aes(col=group, lty=as.factor(Corr))) +
    geom_point(aes(col=group, shape=group)) +
    theme_Publication() %+replace%
    theme(legend.position = 'none',
          legend.box = 'vertical',
          legend.spacing.y = unit(1, unit='mm'),
          legend.margin = margin(0.0, unit='mm'),
          legend.key.width = unit(5, unit='mm'),
          legend.box.margin = margin(c(0, 0, 0, 0), unit='mm'),
          legend.background = element_blank()) +
    scale_x_log10(breaks = breaks, labels = labels) +
    ylab('Latency, $\\mu{}s$') +
    xlab("Processes") +
    ylim(c(0,65)) +
    annotate("segment",
             x=annotation.df$x, xend=annotation.df$xend,
             y=annotation.df$y, yend=annotation.df$yend,
             color=annotation.df$color,
             arrow=arrow(length = unit(2, "points"), type='closed'), lineend = 'round') +
    annotate("text",
             x=annotation.df$x, y=annotation.df$y+0.05,
             label=annotation.df$label,
             vjust=annotation.df$vjust, hjust=annotation.df$hjust,
             color=annotation.df$color, size=3, alpha=1) +
    col
dev.off()


## breaks <- sort(unique(df$Size))
## labels <- breaks
## df.all %>%
##     mutate(group = as.factor(paste(TreeType, CorrType, Corr, FaultCount))) %>%
##     filter(Nnodes %in% c(512)) %>%
##     mutate(CorrType = ifelse(CorrType == 'Native', 'Cray MPI', 'Corrected')) %>%
##     ggplot(aes(Size, AvgTime.50, group=group)) +
##     geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha = 0.3)+ 
##     geom_line(aes(col=group, lty=as.factor(Corr))) +
##     geom_point(aes(col=group, shape=group)) +
##     theme_Publication() %+replace%
##     theme(legend.position = 'top',
##           legend.box = 'vertical',
##           legend.spacing.y = unit(1, unit='mm'),
##           legend.margin = margin(0.0, unit='mm'),
##           legend.key.width = unit(5, unit='mm'),
##           legend.box.margin = margin(c(0, 0, 0, 0), unit='mm'),
##           legend.background = element_blank()) +
##     ylab('Latency, $\\mu{}s$') +
##     xlab("Size") +
##     scale_x_log10(breaks = breaks, labels = labels)

## +
##     ylim(c(0,200))

## +
##     col
