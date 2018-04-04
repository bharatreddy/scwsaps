;station='hlp' & date=20140925 & time=[0600,0615] 

pro read_inter_magnet,date,time,station,xdata,ydata,detrend=detrend

        stime=format_juldate(calc_jul(date,time[0]),/themis)
        etime=format_juldate(calc_jul(date,time[1]),/themis)

        p_stations=['hlp','lyc','nur','ups','api']
        q_stations=['ppt','mbo','kou']
        v_stations=['mea','new','shu','sit','stj','vic','frn','ott','bou','blc','bsl','tuc','frd','aae','hon','dmc']


    	;Set the data formats and indicate the paths of files   
    	format1='(A10,X,A12)'
    	format2='(30X,f10.2,f10.2,f10.2)'    
        if where(p_stations eq station) ne -1 then begin
		filepaths='/home/xueling/data/gmag/'+station+string(date,format='(I8)')+'psec.sec'  
	endif else begin
		if where(v_stations eq station) ne -1 then begin
 		       filepaths='/home/xueling/data/gmag/'+station+string(date,format='(I8)')+'vsec.sec'  
		endif else begin
			 filepaths='/home/xueling/data/gmag/'+station+string(date,format='(I8)')+'qsec.sec'  
		endelse
	endelse      

	if where(q_stations eq station) ne -1 then begin
		header = strarr(22)
		dlines=file_lines(filepaths)-22
	endif else begin
		header = strarr(21)
		dlines=file_lines(filepaths)-21
		if station eq 'dmc' then begin
			header = strarr(28)
			dlines=file_lines(filepaths)-28	
		endif
	endelse

		openr,lun,filepaths,/get_lun
		readf,lun,header
		xdata0 = strarr(2,dlines) 
		readf,lun,xdata0,format=format1
		free_lun,lun

		openr,lun,filepaths,/get_lun 
		readf,lun,header  
		ydata = dblarr(3,dlines)
		readf,lun,ydata,format=format2
		free_lun,lun


        xdata=time_double(xdata0[0,*]+'/'+xdata0[1,*])

        ind=where(xdata ge time_double(stime) and xdata le time_double(etime))

        xdata=xdata[ind]
	ydata=ydata[*,ind]

   if keyword_set(detrend) then begin
   	ydata[0,*]=ydata[0,*]-mean(ydata[0,*])
   	ydata[1,*]=ydata[1,*]-mean(ydata[1,*])
   	ydata[2,*]=ydata[2,*]-mean(ydata[2,*]) 
   endif 

	xdata=double(xdata/3600./24.+JULDAY(1, 1, 1970, 0, 0, 0))
end
