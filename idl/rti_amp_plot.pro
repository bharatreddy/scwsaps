pro rti_amp_plot

common rad_data_blk


date = 20110516
timeRange = [ 0600, 1000 ]
rtiCoords = 'magn'
selRad = 'cvw'
selBeam = 9
velScale = [ -500, 500 ]


yrange=[50,65]

rad_load_colortable,/website

rad_fit_read, date, selRad, /filter
amp_read, date

ps_open, '/home/bharatr/Docs/plots/sd-amp-rti-' + strtrim( string(date), 2) + '.ps'

	rad_fit_plot_rti_panel,1.25,1.,0,0, date=date, time=timeRange, param='velocity',coords=rtiCoords, yrange=yrange, scatterflag=3, scale=velScale
	plot_colorbar, 1.5, 1., 0.4, 0., scale=velScale, param='velocity', /with_info

ps_close,/no_filename

end