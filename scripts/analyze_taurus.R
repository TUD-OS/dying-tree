library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(tikzDevice)
library(data.table)

dirs <- c('18-00-13_300418.2674.taurus')

dir <- '10-08-23_010518.4806.taurus'
dir <- '12-18-26_010518.12209.taurus'
dir <- '13-09-55_010518.13461.taurus'

dir <- '14-18-10_010518.32479.taurus'
dir <- '16-01-25_010518.29132.taurus'
dir <- '02-00-09_020518.19669.taurus'

dir <- '08-55-36_020518.2906.taurus'

dir <- '09-26-01_020518.10513.taurus'

dir <- '00-48-16_030518.930.taurus'


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
                                                
df <- read.csv(paste0('../../logs/taurus/', dir, '/table.csv')) %>%
    mutate(Nproc = Nnodes * 24) %>%
    mutate(Algorithm = ifelse(Algorithm == 6, 'Open~MPI',
                  ifelse(Algorithm == 7, 'Corrected', 'XXX')))
df.sum <- df %>% 
    group_by(Corr, Algorithm, Nproc, FaultCount, Size) %>%
    summarize(AvgTime.50 = median(AvgTime),
              AvgTime.25 = quantile(AvgTime, .25),
              AvgTime.75 = quantile(AvgTime, .75))

tikz('../../../../tree-broadcast/plots/taurus.tex', width=3, height=2.0)
annotation.df <- rbindlist(
    list(
        list(x=600, xend=600, y=9, yend=12.2, color='#e41a1c', label="Ours ($d=0$)", vjust='top', hjust='right'),
        list(505, 490, 8.35, 6.95, '#e41a1c', '', 'bottom', 'left'),
        list(1000, 1400, 15.5, 14.85, '#4daf4a', 'Ours ($d=2$, faults)', 'center', 'right'),
        list(1200, 1400, 6.5, 8.8, '#4daf4a', 'Ours ($d=2$, faults)', 'top', 'right'),
        list(1400, 1700, 12, 14.8, '#984ea3', 'Ours ($d=4$, faults)', 'top', 'right'),
        list(1400, 1700, 11.9, 9.7, '#984ea3', '', 'bottom', 'right'),
        list(790, 700, 10.5, 11.8, '#377eb8', " OMPI", 'bottom', 'left'),
        list(950, 1100, 10.5, 8.1, '#377eb8', "", 'top', 'right')
    ),
    use.name=TRUE, fill=FALSE, idcol=FALSE)
breaks <- sort(unique(df.sum$Nproc))
labels <- breaks
df.sum %>% filter(Size %in% c(8, 256),
                       Algorithm == 'Open~MPI'
                       | FaultCount != 0
                       | Corr == 0,
                       FaultCount != 1) %>%
    mutate(group = paste0(factor(Algorithm), factor(FaultCount), factor(Size), factor(Corr))) %>%
    ggplot(aes(Nproc,
               AvgTime.50, group=group)) +
    geom_line(aes(col=factor(paste0(Corr, Algorithm)), lty=factor(FaultCount))) +
    geom_ribbon(aes(ymin=AvgTime.25, ymax=AvgTime.75), alpha=0.3) +
    geom_point(aes(shape=factor(Algorithm), col=factor(paste0(Corr, Algorithm)))) +
    scale_x_log10(breaks = breaks, labels = labels) +
    ylab('Latency, $\\mu{}s$') +  xlab("Processes") +
    theme_Publication() %+replace%
    theme(legend.position = 'none',
          legend.margin = margin(0.5, unit='mm'),
          legend.box.margin = margin(c(1, 1, 1, 1), unit='mm'),
          legend.spacing = unit(1, 'mm'),
          legend.key.height = unit(3, 'mm'),
          legend.key.width = unit(4, 'mm'),
          legend.background = element_blank()) +
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
    annotate("text",
             x=1810, y=15.15, label="$\\}$", size=6) +
    annotate("text",
             x=1930, y=14.6, label="256 bytes", size=3, angle=-90) +
    annotate("text",
             x=1810, y=9.45, label="$\\}$", size=6) +
    annotate("text",
             x=1930, y=9, label="8 bytes", size=3, angle=-90) +
    fill + col + lty + shape
dev.off()



