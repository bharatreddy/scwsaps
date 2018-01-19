pro rti_amp_plot

common rad_data_blk
common amp_data_blk

date = 20110516
timeRange = [ 0600, 1000 ]
selTime = 0820
rtiCoords = 'magn'
selRad = 'cvw'
selBeam = 9
velScale = [ -500, 500 ]
ampCoords = 'mlt'

min_value = 0.1
selMLT =21.


yrange=[50,65]

rad_load_colortable,/website

rad_fit_read, date, selRad, /filter


;; AMPERE analysis
amp_read, date

if strcmp(ampCoords,'mlt',/fold) then $
	mlt0inmlonh = 0. $
else $
	mlt0inmlonh = -mltdavit(year,(jul-julday(1,1,year,0))*86400.d,0.)

; int_hemi is 0 for north and 1 for south
int_hemi = 0
sfjul, date, selTime, jul
dd = min( abs( (*amp_data[int_hemi]).mjuls-jul ), index)
nlats = (*amp_data[int_hemi]).nlat[index]
nlons = (*amp_data[int_hemi]).nlon[index]
mlats = 90.-reform((*amp_data[int_hemi]).colat[index, *])
mlts = ( reform((*amp_data[int_hemi]).mlt[index, *]) + mlt0inmlonh )*(strcmp(ampCoords,'mlt',/fold) ? 1. : 15.)
tmp = calc_stereo_coords(mlats, mlts, mlt=strcmp(ampCoords,'mlt',/fold))
xxs = fltarr(nlats, nlons+1)
yys = fltarr(nlats, nlons+1)
jrs = fltarr(nlats, nlons+1)
for a=0, nlats-1 do begin
	for b=0, nlons do begin
		xxs[a,b] = tmp[0,(b mod nlons)*nlats+a]
		yys[a,b] = tmp[1,(b mod nlons)*nlats+a]
		jrs[a,b] = abs((*amp_data[int_hemi]).jr[index,(b mod nlons)*nlats+a]) lt min_value ? 0. : (*amp_data[int_hemi]).jr[index,(b mod nlons)*nlats+a]
	endfor
endfor





selMLTinds = where( mlts eq selMLT )
currMlats = mlats[selMLTinds]
currMlts = mlts[selMLTinds]
currJrs = jrs[selMLTinds]
print, currMlts
print, "-----------------------------------------------------------------------"
print, currMlats
print, "-----------------------------------------------------------------------"
print, currJrs
print, "-----------------------------------------------------------------------"
print, selTime







ps_open, '/home/bharatr/Docs/plots/sd-amp-rti-' + strtrim( string(date), 2) + '.ps'

	rad_fit_plot_rti_panel,1.25,1.,0,0, date=date, time=timeRange, param='velocity',coords=rtiCoords, yrange=yrange, scatterflag=3, scale=velScale
	plot_colorbar, 1.5, 1., 0.4, 0., scale=velScale, param='velocity', /with_info

ps_close,/no_filename

end