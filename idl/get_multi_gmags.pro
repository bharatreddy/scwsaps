pro get_multi_gmags,station,date=date,time=time, paramg=paramg,xdata,ydata,detrend=detrend
 common gbm_data_blk
          ;sTime=format_juldate(calc_jul(date,time[0]),/themis)
          ;eTime=format_juldate(calc_jul(date,time[1]),/themis)

          ;str_date = format_date(date)
          ;str_time = format_time(time)
          ;sTime = strmid(str_date,0,4)+'-'+strmid(str_date,4,2)+'-'+strmid(str_date,6,2)+'/'+strmid(str_time,0,2)+':'+strmid(str_time,2,2)+':00'
          ;eTime = strmid(str_date,0,4)+'-'+strmid(str_date,4,2)+'-'+strmid(str_date,6,2)+'/'+strmid(str_time,5,2)+':'+strmid(str_time,7,2)+':00'
          ;thm_load_gmag,site=station,trange=[sTime,eTime]

          gbm_read, date, station, time=time

	if keyword_set(detrend) then begin
          	get_gmf_thm,date=date, time=time, param=paramg,silent=silent,xdata=xdata,ydata=ydata,/detrend
	endif else begin
          	get_gmf_thm,date=date, time=time, param=paramg,silent=silent,xdata=xdata,ydata=ydata
	endelse

end
