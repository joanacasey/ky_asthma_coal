library( data.table)
library( ggplot2)
library( viridis)
library( sf)
library( scales)
library( lwgeom)
## ====================================================== ##
##              Function to plot spatial change           ##
##              between periods over time, spatial        ##
## ====================================================== ##
spatial.change.from.year.g <- function( unit.use = 'all',
                                        time = c( 'month', 'quarter', 'year'),
                                        change.from.year = 2014,
                                        plot.year = 2015,
                                        change.time.start = NULL,
                                        change.type = c( 'pct', 'abs'),
                                        dt = copy( timeseries.dt),
                                        col.limits = c( -1, 1)){
  # subset data.table unit(s) for plotting
  if( unit.use == 'all'){
    use.dt <- dt
  } else
    use.dt <- dt[grep(unit.use, uID)]
  
  # housekeeping
  use.dt[, `:=` (ZIP = as( ZIP, 'character'),
                 month = month( date.month))]
  
  # rename variables
  if( length( time) > 1){
    print( paste( 'time default to', time[1]))
    time <- time[1]
  }
  setnames( use.dt, time, 'time')
  
  # sum HyADS 
  sum.by <- c( 'ZIP', 'time', 'year')
  use.dt2 <- use.dt[, .( hyads = sum( exposure)), by = sum.by]
  
  # merge with ZIPs, take area
  use.dt2.sf <- merge( use.dt2, zips, by = 'ZIP')
  use.dt2.sf[, area := as.numeric( st_area( geometry))]
  area.sum <- sum( use.dt2.sf[time == 1 & year == change.from.year]$area)
  
  # define base periods
  if( !is.null( change.time.start)){
    year.time.base <- c( paste( change.from.year, change.time.start:max( use.dt2.sf$time), sep = '.'),
                         paste( change.from.year + 1, 1:(change.time.start - 1), sep = '.'))
    year.time.plot <- c( paste( change.from.year + 1, change.time.start:max( use.dt2.sf$time), sep = '.'),
                         paste( change.from.year + 2, 1:(change.time.start - 1), sep = '.'))
  } else{
    year.time.base <- paste( change.from.year, 1:max( use.dt2.sf$time), sep = '.')
    year.time.plot <- paste( change.from.year + 1, 1:max( use.dt2.sf$time), sep = '.')
  }
  
  # take first 'time' in 'change.from.year'
  use.dt2.sf[, year.time := paste( year, time, sep = '.')]
  use.dt2.sf <- merge( use.dt2.sf, 
                       use.dt2.sf[ year.time %in% year.time.base, .( ZIP, year, time, hyads)],
                       by = c( 'ZIP', 'time'), all.x = T)
  
  # Legend title
  units.names <- c( 'all' = 'All',
                    '1363' = 'Cane Run',
                    '1364' = 'Mill Creek',
                    '983'  = 'Clifty Creek',
                    '6166' = 'Rockport')
  pct.abs <- c( 'abs' = 'absolute', 'pct' = 'percent')
  fac.n <- ifelse( grepl('\\|', unit.use), '', gsub( '-.*$', '', as.character( unit.use)))
  unit.n <- ifelse( grepl('\\|', unit.use), '', gsub( '^.*-', '', as.character( unit.use)))
  title.unit <- ifelse( is.na( units.names[ fac.n]), "Select units'", paste( units.names[ fac.n], "Unit", unit.n))
  title = paste( title.unit, "HyADS", pct.abs[change.type], "change")
  
  # take change from base year
  if( change.type[1] == 'pct' | length( change.type) > 1){
    use.dt2.sf[, change := (hyads.x - hyads.y) / hyads.y]
    mean.pct <- use.dt2.sf[, mean( change), by = .( year.time, time, year.x)]
    setnames( mean.pct, c( 'V1'), c( 'mean.p'))
    
    col.scale <-   scale_fill_viridis( name = title,
                                       limits = col.limits,
                                       labels = percent_format( accuracy = 1),
                                       direction = 1,
                                       guide = guide_colorbar( title.position = 'top',
                                                               barwidth = unit( 16, 'cm')))
  } else{
    use.dt2.sf[, `:=` (change = hyads.x - hyads.y,
                       frac.increase = (hyads.x - hyads.y) / hyads.y)]
    mean.pct <- use.dt2.sf[ , mean( frac.increase), 
                            by = .( year.time, time, year.x)]
    setnames( mean.pct, c( 'V1'), c( 'mean.p'))
    
    col.scale <-   scale_fill_viridis( name = title,
                                       limits = col.limits,
                                       direction = 1,
                                       guide = guide_colorbar( title.position = 'top',
                                                               barwidth = unit( 16, 'cm')))
  }
  
  if( time == 'month')
    fw <- facet_wrap( ~ time + year.x, ncol = 4, labeller = label_bquote( atop( .(month.name[as.numeric( as.character( time))]),
                                                                                .( as.numeric( as.character( year.x)))~-~.( as.numeric( as.character( year.x)) - 1))))
  if( time == 'quarter')
    fw <- facet_wrap( ~ time + year.x, ncol = 4, 
                      labeller = label_bquote( atop( Quarter~.(as.numeric( as.character( time))),
                                                     .( as.numeric( as.character( year.x)))~-~.( as.numeric( as.character( year.x)) - 1))))
  if( time == 'year')
    fw <- facet_wrap( ~ time, ncol = 4) 
  
  # grab the data to plot
  dt.plot <- use.dt2.sf[ year.time %in% year.time.plot]
  dt.plot[, `:=` (time = factor( time, levels = gsub( '^.*\\.', '', year.time.plot)),
                  year.x = factor( year.x, levels = unique( gsub( '\\..*', '', year.time.plot))))]
  pct.plot <- mean.pct[ year.time %in% year.time.plot]
  pct.plot[, `:=` (time = factor( time, levels = levels( dt.plot$time)),
                   year.x = factor( year.x, levels =  levels( dt.plot$year.x)))]
  
  # make the plot
  ggchange <- ggplot( dt.plot) +
    geom_sf( aes( geometry = geometry,
                  fill = change),
             color = 'white', size = .1) +
    geom_label( data = pct.plot,
                aes( label = paste( 'mean =', ifelse( mean.p > 0,
                                                      paste0( '+', percent( mean.p, accuracy = 2)),
                                                      percent( mean.p, accuracy = 2)))),
                x = -85.36, y = 38, hjust = 1, color = 'grey20') +
    fw +
    col.scale +
    theme_bw() +
    expand_limits( fill = 0) +
    theme( axis.text = element_blank(),
           axis.title = element_blank(),
           axis.ticks = element_blank(),
           legend.position = 'bottom',
           legend.text = element_text( size = 14),
           legend.title = element_text( size = 16, hjust = .5, face = 'bold'),
           panel.grid = element_blank(),
           strip.background = element_rect( fill = NA),
           strip.text = element_text( size = 16))
  
  return( ggchange)
}


## ====================================================== ##
##    Read the ZIP code shapefile                     ##
## ====================================================== ##
## load shapefiles and crosswalk for plotting
zcta_shapefile <- 'cb_2015_us_zcta510_500k.shp'
crosswalk_csv <- 'Zip_to_ZCTA_crosswalk_2015_JSI.csv'
cw <- fread( crosswalk_csv, keepLeadingZeros = TRUE)
zips <- st_read(zcta_shapefile)
setnames( zips, 'ZCTA5CE10', 'ZCTA')
zips <- merge( zips, cw, by = "ZCTA", all = F, allow.cartesian = TRUE)
zips$ZIP <- formatC( zips$ZIP, width = 5, format = "d", flag = "0")

## ====================================================== ##
##    Read the HyADS timeseries data                      ##
## ====================================================== ##
timeseries.dt <- fread( "louisville_exposure2_share.csv")

# create quarterly time series
quarts <- c( rep( 1, 3),
             rep( 2, 3),
             rep( 3, 3),
             rep( 4, 3))
names(quarts) <- 1:12
timeseries.dt[, quarter := quarts[month]]


## ====================================================== ##
##    Read the HyADS timeseries data                      ##
## ====================================================== ##
timeseries.dt.m <- melt( timeseries.dt[, c( 'ZIP', 'date.month', 'year', 'month', 'uID', 'quarter', 'exposure')],
                         id.vars = c("ZIP", "date.month", "year", "month", "uID", "quarter"))

# sum by ZIP code, unit, and quarter
timeseries.dt.m2 <- timeseries.dt.m[, sum( value), by = .( ZIP, uID, variable, date.month)]
setnames( timeseries.dt.m2, 'V1', 'value')

# average across all ZIPs
timeseries.dt.m3 <- timeseries.dt.m2[, mean( value, na.rm = T), by = .( uID, variable, date.month)]
setnames( timeseries.dt.m3, 'V1', 'HyADS')



## ====================================================== ##
##    Figure 1A                                           ##
## ====================================================== ##
# define colors for plotting
colfuncb <- colorRampPalette(c("lightblue", "blue"))
colfuncg <- colorRampPalette(c("lightgreen", "darkgreen"))
colfuncy <- colorRampPalette(c("orange", "darkorange"))
colfuncp <- colorRampPalette(c("plum1", "purple"))
colos <- c( colfuncp(6),
            colfuncb(3),
            colfuncg(4),
            colfuncy(2))
names(colos) <- unique( timeseries.dt$uID)


Figure1A <- ggplot( timeseries.dt.m3,
                    aes( y = HyADS, x = as.Date( date.month), fill = uID)) + 
  theme_bw() + 
  geom_area( data = timeseries.dt.m3, position = 'stack') +
  scale_fill_manual(name = "Unit ID",
                    values = colos,
                    breaks = names(colos),
                    labels = names(colos),
                    guide = guide_legend(nrow = 3,
                                         title.position = 'left')) + 
  scale_y_continuous(label = scales::scientific)	 +
  scale_x_date(expand = c(0,0))	 +
  ylab("HyADS, unitless") + 
  xlab('Year') +
  theme( axis.text = element_text(size = 16),
         axis.title = element_text( size = 18, face = 'bold'),
         axis.title.x = element_blank(),
         legend.direction = 'horizontal',
         legend.key.size = unit(.5,'line'),
         legend.position = c(.75, .8),
         legend.text = element_text(size=14),
         legend.text.align = 0,
         legend.title = element_text( size = 16),
         plot.title = element_text(size = 20),
         plot.margin = unit( c( 1, 1, 1, 1), "cm"),
         strip.text = element_text(size = 16, face = 'bold'),
         strip.background = element_rect(fill = 'white')) 


## ====================================================== ##
##    Figure 1B                                           ##
##    Apply the spatial change function:                  ##
## ====================================================== ##

Figure1B <- spatial.change.from.year.g( unit.use = '1363|1364-1|1364-2|6166',
                                        time = c( 'quarter'),
                                        change.from.year = 2014,
                                        change.time.start = 3,
                                        change.type = c( 'abs'),
                                        col.limits = c( NA, NA))

## ====================================================== ##
##    Figure 4B                                           ##
##    Apply the spatial change function:                  ##
## ====================================================== ##
Figure4B <- spatial.change.from.year.g( unit.use = '1364-3',
                                        time = c( 'month'),
                                        change.from.year = 2015,
                                        plot.year = 2016,
                                        change.type = c( 'abs'),
                                        col.limits = c( NA, NA))

