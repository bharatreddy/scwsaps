pro get_gmf_thm,date=date, time=time, param=param,silent=silent,xdata=xdata,ydata=ydata,detrend=detrend
	;yrange=yrange, $
	;silent=silent, bar=bar, $
	;charthick=charthick, charsize=charsize, psym=psym, $ 
	;xstyle=xstyle, ystyle=ystyle, xtitle=xtitle, ytitle=ytitle, $
	;xticks=xticks, xminor=xminor, yticks=yticks, yminor=yminor, $
	;linestyle=linestyle, linecolor=linecolor, linethick=linethick, $
	;xtickformat=xtickformat, ytickformat=ytickformat, $
	;xtickname=xtickname, ytickname=ytickname, $
	;position=position, $
	;first=first, last=last, with_info=with_info, no_title=no_title, $
	;detrend=detrend

common gbm_data_blk

if gbm_info.nrecs eq 0L then begin
	if ~keyword_set(silent) then $
		prinfo, 'No data loaded.'
	return
endif

if ~keyword_set(param) then $
	param = 'bx_mag'

if ~keyword_set(date) then begin
	if ~keyword_set(silent) then $
		prinfo, 'No DATE given, trying for scan date.'
	caldat, gbm_data.juls[0], month, day, year
	date = year*10000L + month*100L + day
endif

if ~keyword_set(time) then $
	time = [0000,2400]

sfjul, date, time, sjul, fjul
xrange = [sjul, fjul]

; get data
xtag = 'juls'
ytag = param
if ~tag_exists(gbm_data, xtag) then begin
	prinfo, 'Parameter '+xtag+' does not exist in GBM_DATA.'
	return
endif
if ~tag_exists(gbm_data, ytag) then begin
	prinfo, 'Parameter '+ytag+' does not exist in GBM_DATA.'
	return
endif
dd = execute('xdata = gbm_data.'+xtag)
dd = execute('ydata = gbm_data.'+ytag)

; select data to plot
juls_inds = where(xdata ge sjul-10.d/1440.d and xdata le fjul+10.d/1440.d, cc)
if cc eq 0 then begin
	if ~keyword_set(silent) then $
		prinfo, 'No data found for time '+format_time(time)
	return
endif

; get indeces of data to plot
data_inds = where(gbm_data.juls ge sjul-1.d/1440.d and $
		gbm_data.juls le fjul+1.d/1440.d, $
		ndata_inds)

ydata = ydata[data_inds]
xdata = xdata[data_inds]

if keyword_set(detrend) then begin
	ydata = detrend_data(ydata,/mean)
endif

;if ( param eq 'bz_mag' or param eq 'bx_mag' or param eq 'bt_mag' ) and ~keyword_set(detrend) then ydata /= 1e3

xdata=xdata
ydata=ydata

end
