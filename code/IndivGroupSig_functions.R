#######################################################################################
##                        Functions for Whoop Signatures Paper                        ##        
##                                                                                   ##
##                             by - Kenna D S Lehmann                                ##
##                                                                                   ##
##                            last updated: 8 July 2020                               ##
#######################################################################################
## DESCRIPTION:
##
##
##
#######################################################################################



library(signal)
library(oce)
library(tuneR)
library(seewave)

##### Plotting spectrogram functions

hot_theme <- theme(panel.grid.major.y = element_line(color="black", linetype = "dotted"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(fill="black"),
                   panel.border = element_rect(linetype = "solid", fill = NA, color = "black"),
                   axis.line = element_blank(),
                   legend.position = "left",
                   legend.justification = "left",
                   legend.background = element_rect(fill="white"),
                   legend.key.width = unit(20, "native"),
                   legend.key.height = unit(30, "native"),
                   legend.title = element_text(size=10, color="black"),
                   legend.text = element_text(size=10, color="black"),
                   plot.background = element_rect(fill="white"),
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_line(color="black"))

hot_colors <-viridis(n=20, begin=0, end=0.75)

mkspec <- function(file){
  wav <- readWave(file)
  ggspectro(wave = wav, f = wav@samp.rate, wl=2000, ovlp = 90, norm=TRUE) + scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(limits=c(0,2), breaks = seq(from = .5, to = 2, by=.5), expand = c(0,0), position = "left")+
    geom_raster(aes(fill=amplitude), hjust = 0, vjust = 0, interpolate = F)+
    scale_fill_gradientn(colours = hot_colors, name = "Amplitude \n (dB)", na.value = "transparent", limits = c(-60,0))+
    hot_theme
}
