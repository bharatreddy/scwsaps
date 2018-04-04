pro plot_gbm_locs, date, time, coords=coords


xrangePlot = [-44, 44]
yrangePlot = [-44,30]


if ~keyword_set(coords) then $
	coords = "mlt"


ps_open, '/home/bharatr/Docs/projects/plots/gbm-' + strtrim( string(date), 2) + '.ps'

map_plot_panel,date=date,time=time,coords=coords,/no_fill,xrange=xrangePlot, $
                yrange=yrangePlot,pos=mapPos1,/isotropic,grid_charsize='0.5',/north, charsize = 0.5


overlay_gbm, coords=coords, date=date, time=time, /annotate, gbm_charsize=0.4, /gbm_themis, gbm_charcolor=get_blue()
overlay_gbm, coords=coords, date=date, time=time, /annotate, gbm_charsize=0.4, /intermagnet, gbm_charcolor=get_green()


ps_close,/no_filename


end