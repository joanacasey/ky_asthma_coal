library( data.table)
library( ggplot2)
library( hyspdisp) #devtools::install_github("lhenneman/hyspdisp", force = TRUE)
library( ggsn)
library( openxlsx)
library( grid)

## Change these file paths
ZIPexposures.file <- '~/Dropbox/Harvard/RFMeval_Local/Comparisons_Intermodel/evaluate_RFMs_intermediates/ZIPexposures.hyspdisp_over_large_allunits_Louisville.csv'
plot_out.file <- '~/Dropbox/Harvard/Manuscripts/Propeller_Coal_Asthma/plots/pp_rankings2.png'

## ====================================================== ##
##----- load data on which facilities impact which ZIPs
c <- fread( ZIPexposures.file)
c$FacID <- as( c$FacID, 'character')
setnames( c, 'zip', 'ZIP')

## download important spatial data
#### ZIP code-ZCTA crosswalk file
crosswalkin <- data.table( read.xlsx( xlsxFile = "https://www.udsmapper.org/docs/zip_to_zcta_2017.xlsx"))
setnames( crosswalkin, old = "ZIP_CODE", new = "ZIP")

#### ZCTA shapefile, download if not present
zcta_dir <- file.path('~', 'Desktop')
zcta_file <- file.path( 'cb_2017_us_zcta510_500k.zip')
download.file( url = 'ftp://ftp2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_zcta510_500k.zip',
               destfile = zcta_file)
unzip( zcta_file, exdir = zcta_dir)

#### Load the ZCTA shapefile using the shapefile command
zcta_shapefile <- file.path( zcta_dir, 'cb_2017_us_zcta510_500k.shp')
zcta <- st_read( zcta_shapefile)

#### set names for consistency with crosswalk file
setnames( zcta, 'ZCTA5CE10', 'ZCTA')
zips <- merge( zcta, crosswalkin, by = "ZCTA", all = F, allow.cartesian = TRUE) 
zips$ZIP <- formatC( zips$ZIP, width = 5, format = "d", flag = "0") # to merge on zcta ID

## link facility impacts to shapefile data
c_sf <- data.table( merge( zips, c, by = c('ZIP'), all.y = T))

## ==================================================================== ##
##----- define the facilities, link to emissions and retirement data
facs <- c( '1363', '1364', '6166', '983')

## ID impacts by facility and restrict to 2012
c_2012  <- c_sf[FacID %in% facs & year == 2012] 

# grab the units data from hyspdisp
data( "units2012")
units2012[, FacID := gsub( '\\..*', '', uID)]
fac.label <- units2012[ FacID %in% facs]

# define labels and positions
xlabs = - 86.05
ylabs =   37.9
fac.label[ FacID == 1363, `:=` (facility = 'Cane Run, Facility #1363', 
                                faclabel = paste( 'Unit 4 - Retired (May 2015)',
                                                  'Unit 5 - Retired (May 2015)',
                                                  'Unit 6 - Retired (May 2015)', sep = '\n'),
                                xlab = xlabs,
                                ylab = ylabs)]
fac.label[ FacID == 1364, `:=` (facility = 'Mill Creek, Facility #1364', 
                                faclabel = paste( 'Unit 1 - Wet limestone scrubber (May 2015)',
                                                  'Unit 2 - Wet limestone scrubber (May 2015)',
                                                  'Unit 3 - Wet limestone scrubber (Jun 2016)',
                                                  'Unit 4 - Wet limestone scrubber (Dec 2014)', sep = '\n'),
                                xlab = xlabs,
                                ylab = ylabs)]
fac.label[ FacID == 6166, `:=` (facility = 'Rockport, Facility #6166', 
                                faclabel = paste( 'Unit MB1 - Dry sodium scrubber (Apr 2015)',
                                                  'Unit MB2 - Dry sodium scrubber (Apr 2015)', sep = '\n'),
                                xlab = xlabs,
                                ylab = ylabs)]
fac.label[ FacID == 983, `:=` (facility = 'Clifty Creek, Facility #983', 
                               faclabel = paste( 'Unit 1 - Wet limestone scrubber (Mar 2013)',
                                                 'Unit 2 - Wet limestone scrubber (Mar 2013)',
                                                 'Unit 3 - Wet limestone scrubber (Mar 2013)',
                                                 'Unit 4 - Wet limestone scrubber (May 2013)',
                                                 'Unit 5 - Wet limestone scrubber (May 2013)',
                                                 'Unit 6 - Wet limestone scrubber (Mar 2013)', sep = '\n'),
                               xlab = xlabs,
                               ylab = ylabs)]



## ====================================================== ##
##-------- create labels for states
data.an2 <- data.table( x =    c( -88,        -85.15,    -84.95), 
                        y =     c( 38.95,      38.9,      38.6), 
                        label = c("Illinois", "Indiana", "Kentucky"))



## ====================================================== ##
##-------- make the main plot
facility_rank_plot <- ggplot( ) +
  facet_wrap( . ~ FacID, ncol = 2) +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    fill = "white",
    colour = "white",
    size = .25
  ) +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    fill = NA,
    colour = "grey50",
    size = .25
  )  +
  geom_sf(data = c_2012,
          size = 0.01,
          aes(fill  = factor( Fac_rank_exp),
              color = factor( Fac_rank_exp),
              geometry = geometry)) +
  coord_sf(
    xlim = c(-87.15, -84.75),
    ylim = c(37.3, 38.9),
    datum = NA
  )  +
  geom_point( data = fac.label,
              aes( x = Longitude,
                   y = Latitude,
                   size = SOx)) +
  scale_color_viridis(name = 'Facility Rank',
                      discrete = T,
                      direction = -1,
                      alpha = .7,
                      guide = guide_legend( title.position = 'top',
                                            title.hjust = 0.5,
                                            title.vjust = 0)) + 
  scale_fill_viridis( name = 'Facility Rank',
                      discrete = T,
                      direction = -1,
                      alpha = .7,
                      guide = guide_legend( title.position = 'top',
                                            title.hjust = 0.5,
                                            title.vjust = 0 )) + 
  scale_size_continuous(limits = c( 900, 4e5),
                        breaks = c( 1e3, 1e4, 1e5),
                        labels = c( '1e+03', '1e+04', '1e+05'),
                        trans = 'log10',
                        guide = guide_legend( title=expression(paste('2012 total ', SO[2], ' emissions [tons]')),
                                              title.position='top',
                                              title.hjust = 0.5,
                                              title.vjust = 0)) +
  geom_text(data = data.an2, 
            aes( x = x,
                 y = y,
                 label = label),
            inherit.aes = FALSE,
            color = 'gray50',
            size = 3) +
  geom_text(data = fac.label, 
            aes( label = facility),
            x = -87.15,
            y = 38.9,
            inherit.aes = FALSE,
            hjust = 'left',
            color = 'black',
            fontface = 'bold',
            size = 4) +
  geom_label( 
    data = fac.label,
    aes( x = xlab,
         y = ylab,
         label = faclabel),
    hjust = 'left',
    vjust = 'top',
    size = 2) +  
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 24),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.box = 'vertical',
    legend.title.align = 1,
    legend.position = "bottom", #c(.75, .25),
    legend.background = element_rect(fill = 'transparent'),
    legend.key.size = unit(.02, 'npc'),
    legend.direction = 'horizontal',
    legend.justification = c(0.08, 1), 
    legend.text = element_text( size = 12,
                                vjust = .5),
    strip.text = element_blank(),
    strip.background = element_blank( )
  ) + 
  ggsn::scalebar( location = 'bottomleft',
                  anchor = c( x = -87, y = 37.4), 
                  x.min = -87.15,
                  y.min = 37.3,
                  x.max = -84.75,
                  y.max = 38.9,
                  dist = 30, 
                  height = 0.02, 
                  st.dist = 0.04, 
                  st.size = 3, 
                  dd2km = TRUE, 
                  model = 'WGS84',
                  facet.var = 'FacID',
                  facet.lev = '6166')


## ====================================================== ##
##-------- make the inset plot
gg_inset <-  ggplot( ) +
  theme_bw() + 
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    fill = NA,
    colour = "grey70",
    size = .25
  )  +
  geom_rect(aes(xmin = -87.15, 
                xmax = -84.75, 
                ymin = 37.3, 
                ymax = 38.9), 
            alpha=0,
            color = 'black',
            size = .5, 
            linetype=1) +
  theme_bw() + 
  theme( axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         panel.grid = element_blank())

## ====================================================== ##
##-------- save the whole plot
png(file = plot_out.file,
    w = 2000, h = 1800, 
    res=300)
grid.newpage()
v1 <- viewport( width = 1, height = 1, 
                x = 0.5, y = 0.5) #plot area for the main map
v2 <- viewport( width = 0.3, height = 0.2, 
                x = 0.75, y = 0.13) #plot area for the inset map
print( facility_rank_plot,
       vp = v1) 
print( gg_inset, 
       vp = v2)
dev.off()

