;date=20140925  & time=[0500,0700]  &  stations=['shu','vic','new','anna','stj']  & paramg=['bx_mag','by_mag']
;plot_mag_lon_chain_undetrend,date,time,stations,paramg=paramg,filename=['Figure_lon_chain_undetrend_L2-8_H','Figure_lon_chain_undetrend_L2-8_D']

;date=20140925 & time=[0500,0700] & stations=['wgry','vulc','gull','thrf','bmls'] & paramg=['bx_mag','by_mag']
;plot_mag_lon_chain_undetrend,date,time,stations,paramg=paramg,filename=['Figure_lon_chain_undetrend_L3-5_H','Figure_lon_chain_undetrend_L3-5_D']

;date=20140925 & time=[0500,0700] & stations=['atha','mstk','vuls','new','ukia','ccnv'] & paramg=['bx_mag','by_mag']
;plot_mag_lon_chain_undetrend,date,time,stations,paramg=paramg,filename=['Figure_lat_chain_undetrend_MLT21_H','Figure_lat_chain_undetrend_MLT21_D']

;date=20140925 & time=[0500,0700] & stations=['fchu','gill','lgrr','pina','thrf','osak'] & paramg=['bx_mag','by_mag']
;plot_mag_lon_chain_undetrend,date,time,stations,paramg=paramg,filename=['Figure_lat_chain_undetrend_MLT23_H','Figure_lat_chain_undetrend_MLT23_D']

pro plot_mag_lon_chain_undetrend,date,time,stations,paramg=paramg,filename=filename

	common gbm_data_blk
        heap_gc,/ptr,/verbose
        carisma_stations=['ANNA','BACK','CONT','DAWS','ESKI','FCHP','FCHU','FSIM','FSMI','GILL','GULL','ISLL','LGRR',$
                          'MCMU','MSTK','NORM','OSAK','OXFO','PINA','POLS','RABB','RANK','TALO','THRF','VULC','WEYB','WGRY']
        themis_stations=['CDRT','CRVR','ATHA','ARCT','BETT','BMLS','CCNV','CHBG','CIGO','DRBY','EAGL','FSIM','FSMI','FYTS',$
                         'FYKN','GAKO','GBAY','GILL','HOML','HOMR','HOTS','INUV','KAKO','KAPU','KIAN','KUUJ','LOYS','MCGR',$
                         'NAIN','NRSQ','PANG','PGEO','POKR','PINA','PINE','PTRS','RANK','RBAY','RMUS','SNAP','SNKQ','SWNO',$
                         'TALO','TPAS','TRAP','UKIA','WHIT']
        intermagnet_stations=['SHU','VIC','NEW','MEA','SIT','STJ','UPS','NUR','FRN','OTT']
        SA_stations=['ABAN','ABJA','ALGR','BANG','BELM','CMRN','CNKY','DAVO','ETHI','MANL','NMBA','PETR','PUKT',$
                     'PUT','ANT','SER','CER','VLD','OSO','PNT','PAC','ESC','OHI','PAL']
        ISEE_stations=['MSR','RIK','KAG','KTB']

        param=strmid(paramg,0,2)
        num_stas=n_elements(stations)

        MyPtrArr0=ptrarr(num_stas,2)
        MyPtrArr1=ptrarr(num_stas,2)
        for i=0,num_stas-1 do begin
            if where(themis_stations eq strupcase(stations[i])) ne -1 then begin
            	get_multi_gmags,stations[i],date=date,time=time, paramg=paramg[0],xdata,ydata           
            	MyPtrArr0[i,0]=ptr_new(xdata)
            	MyPtrArr0[i,1]=ptr_new(ydata)
            	get_multi_gmags,stations[i],date=date,time=time, paramg=paramg[1],xdata,ydata           
            	MyPtrArr1[i,0]=ptr_new(xdata)
            	MyPtrArr1[i,1]=ptr_new(ydata)
            endif else begin
        	if where(carisma_stations eq strupcase(stations[i])) ne -1 then begin
            		read_carisma,date,time,stations[i],xdata,ydata,param=param[0];,/detrend      
            		MyPtrArr0[i,0]=ptr_new(xdata)
            		MyPtrArr0[i,1]=ptr_new(ydata)
           		read_carisma,date,time,stations[i],xdata,ydata,param=param[1];,/detrend      
            		MyPtrArr1[i,0]=ptr_new(xdata)
            		MyPtrArr1[i,1]=ptr_new(ydata)
		endif else begin
       	        	if where(intermagnet_stations eq strupcase(stations[i])) ne -1 then begin
	    			read_inter_magnet,date,time,stations[i],xdata,ydata;,/detrend         
            			MyPtrArr0[i,0]=ptr_new(xdata)
            			MyPtrArr0[i,1]=ptr_new(ydata[0,*])
	    			MyPtrArr1[i,0]=ptr_new(xdata)
            			MyPtrArr1[i,1]=ptr_new(ydata[1,*])
			endif else begin
				if where(SA_stations eq strupcase(stations[i])) ne -1 then begin
	    				read_SAMBA_AMBER,date,time,stations[i],xdata,ydata;,/detrend      
            				MyPtrArr0[i,0]=ptr_new(xdata)
            				MyPtrArr0[i,1]=ptr_new(ydata[0,*])
	    				MyPtrArr1[i,0]=ptr_new(xdata)
            				MyPtrArr1[i,1]=ptr_new(ydata[1,*])
				endif else begin
					if where(ISEE_stations eq strupcase(stations[i])) ne -1 then begin
            					read_STEP210,date,time,stations[i],xdata,ydata;,/detrend      
            					MyPtrArr0[i,0]=ptr_new(xdata)
            					MyPtrArr0[i,1]=ptr_new(ydata[0,*])
	    					MyPtrArr1[i,0]=ptr_new(xdata)
            					MyPtrArr1[i,1]=ptr_new(ydata[1,*])
					endif else begin
               					print,'No such station (' +stations[i] +') in carisma, themis and part of intermagnet ground magnetometers!!!'
                        			return
					endelse
				endelse
			endelse
		endelse
	   endelse
        endfor



	sfjul, date, time, sjul, fjul, long=long
	xrange = [sjul, fjul] 

        xticks=4
        yticks=2
        xminor=5
        yminor=4
        dummy=''
        dummy=label_date(DATE_FORMAT='%H:%I')
        xtickname=replicate(' ', xticks+10)
        xtickformat='label_date'  

        panel_num=num_stas
        ;my_position,panel_num,posi1,dp,/gap
        posi=fltarr(panel_num,4)
        ;posi[0,*]=posi1
        ;for i=1,panel_num-1 do begin
        ;posi[i,*]=posi1-i*dp
        ;endfor

  	gapsize=0.005
  	sp=(0.90-gapsize*(num_stas-1))/num_stas
        posi[0,*]=[0.18,0.97-sp,0.88,0.97]
  	dp=[0,sp+gapsize,0,sp+gapsize]

	for i = 1,num_stas-1 do begin
		posi[i,*]=posi[i-1,*]-dp
	endfor
        ;posi[1,*]=posi[0,*]-dp
        ;posi[2,*]=posi[1,*]-dp
        ;posi[3,*]=posi[2,*]-dp;-3*[0,gapsize,0,gapsize]
        ;posi[4,*]=posi[3,*]-dp
        ;posi[5,*]=posi[4,*]-dp
;        posi[6,*]=posi[5,*]-dp

        ycharsize=1.0
        charsize=1.2
        thick=3.


        sfjul,20140925,060925,x_onset1,/long
        sfjul,20140925,060655,x_onset2,/long    ;radar velocity onset
        sfjul,20140925,060533,x_onset3,/long
        sfjul,20140925,060805,x_onset4,/long    ;ground magnetometer onset

        popen,filename[0], xsize=8.0, ysize=9.0
yvert=[-100000.,100000.]
        for i=0,num_stas-2 do begin
                temp0=*MyPtrArr0[i,1]
                temp1=*MyPtrArr1[i,1]
        	yl=floor(min([temp0,temp1]))
        	yh=ceil(max([temp0,temp1]))
        	ymax=max([abs(yl),abs(yh)])
       	 	;yrange=[yl,ymax]
                ;yrange=[-5,5]
      		plot, *MyPtrArr0[i,0],*MyPtrArr0[i,1], /nodata,xrange=xrange,xticks=xticks, xminor=xminor,xtickname=xtickname,/xstyle,$
        	       yticks=yticks, yminor=yminor, ytitle='!3'+strupcase(stations[i])+', H [nT]', position=posi[i,*],charsize=charsize,$
	               yrange=yrange,thick=thick, /ystyle
        	oplot, *MyPtrArr0[i,0],*MyPtrArr0[i,1],linestyle=0,psym=0,thick=thick
        	;oplot, *MyPtrArr1[i,0],*MyPtrArr1[i,1],linestyle=2,psym=0,color=get_blue(),thick=thick
        	oplot,[x_onset1,x_onset1],yvert,linestyle=2,color=get_red() ,thick=thick       
        	oplot,[x_onset2,x_onset2],yvert,linestyle=2,color=get_red(),thick=thick
        	oplot,[x_onset3,x_onset3],yvert,linestyle=2,color=get_red(),thick=thick
        	oplot,[x_onset4,x_onset4],yvert,linestyle=2,color=get_red(),thick=thick 
	  	;if i eq 0 then begin
		;	oplot,[xjul[20],xjul[90]],[10,10],linestyle=0,color=get_black() ,thick=thick 
		;	oplot,[xjul[20],xjul[90]],[6.5,6.5],linestyle=2,color=get_blue() ,thick=thick 
	  	;endif

        endfor

        temp0=*MyPtrArr0[num_stas-1,1]
        temp1=*MyPtrArr1[num_stas-1,1]
        yl=floor(min([temp0,temp1]))
        yh=ceil(max([temp0,temp1]))
        ymax=max([abs(yl),abs(yh)])
        ;yrange=[-ymax,ymax]
        ;yrange=[-5,5]

       	plot, *MyPtrArr0[num_stas-1,0],temp0, /nodata,xrange=xrange,xticks=xticks, xminor=xminor,xtickname=xtickname, xtickformat=xtickformat, xtitle='Time [UT]', /xstyle,$
             yticks=yticks, yminor=yminor, ytitle='!3'+strupcase(stations[num_stas-1])+', H [nT]',position=posi[num_stas-1,*],charsize=charsize,$
	     yrange=yrange,thick=thick, /ystyle
        oplot, *MyPtrArr0[num_stas-1,0],temp0,linestyle=0,psym=0,thick=thick
       	;oplot, *MyPtrArr1[num_stas-1,0],temp1,linestyle=2,psym=0,color=get_blue(),thick=thick
        oplot,[x_onset1,x_onset1],yvert,linestyle=2,color=get_red(),thick=thick        
        oplot,[x_onset2,x_onset2],yvert,linestyle=2,color=get_red(),thick=thick
        oplot,[x_onset3,x_onset3],yvert,linestyle=2,color=get_red(),thick=thick
        oplot,[x_onset4,x_onset4],yvert,linestyle=2,color=get_red() ,thick=thick  
	pclose


        popen,filename[1], xsize=8.0, ysize=9.0
yvert=[-100000.,100000.]
        for i=0,num_stas-2 do begin
                temp0=*MyPtrArr0[i,1]
                temp1=*MyPtrArr1[i,1]
        	yl=floor(min([temp0,temp1]))
        	yh=ceil(max([temp0,temp1]))
        	ymax=max([abs(yl),abs(yh)])
       	 	;yrange=[yl,ymax]
                ;yrange=[-5,5]
      		plot, *MyPtrArr1[i,0],*MyPtrArr1[i,1], /nodata,xrange=xrange,xticks=xticks, xminor=xminor,xtickname=xtickname,/xstyle,$
        	       yticks=yticks, yminor=yminor, ytitle='!3'+strupcase(stations[i])+', D [nT]', position=posi[i,*],charsize=charsize,$
	               yrange=yrange,thick=thick, /ystyle
        	;oplot, *MyPtrArr0[i,0],*MyPtrArr0[i,1],linestyle=0,psym=0,thick=thick
        	oplot, *MyPtrArr1[i,0],*MyPtrArr1[i,1],linestyle=2,psym=0,color=get_blue(),thick=thick
        	oplot,[x_onset1,x_onset1],yvert,linestyle=2,color=get_red() ,thick=thick       
        	oplot,[x_onset2,x_onset2],yvert,linestyle=2,color=get_red(),thick=thick
        	oplot,[x_onset3,x_onset3],yvert,linestyle=2,color=get_red(),thick=thick
        	oplot,[x_onset4,x_onset4],yvert,linestyle=2,color=get_red(),thick=thick 
	  	;if i eq 0 then begin
		;	oplot,[xjul[20],xjul[90]],[10,10],linestyle=0,color=get_black() ,thick=thick 
		;	oplot,[xjul[20],xjul[90]],[6.5,6.5],linestyle=2,color=get_blue() ,thick=thick 
	  	;endif

        endfor

        temp0=*MyPtrArr0[num_stas-1,1]
        temp1=*MyPtrArr1[num_stas-1,1]
        yl=floor(min([temp0,temp1]))
        yh=ceil(max([temp0,temp1]))
        ymax=max([abs(yl),abs(yh)])
        ;yrange=[-ymax,ymax]
        ;yrange=[-5,5]

       	plot, *MyPtrArr1[num_stas-1,0],temp1, /nodata,xrange=xrange,xticks=xticks, xminor=xminor,xtickname=xtickname, xtickformat=xtickformat, xtitle='Time [UT]', /xstyle,$
             yticks=yticks, yminor=yminor, ytitle='!3'+strupcase(stations[num_stas-1])+', D [nT]',position=posi[num_stas-1,*],charsize=charsize,$
	     yrange=yrange,thick=thick, /ystyle
        ;oplot, *MyPtrArr0[num_stas-1,0],temp0,linestyle=0,psym=0,thick=thick
       	oplot, *MyPtrArr1[num_stas-1,0],temp1,linestyle=2,psym=0,color=get_blue(),thick=thick
        oplot,[x_onset1,x_onset1],yvert,linestyle=2,color=get_red(),thick=thick        
        oplot,[x_onset2,x_onset2],yvert,linestyle=2,color=get_red(),thick=thick
        oplot,[x_onset3,x_onset3],yvert,linestyle=2,color=get_red(),thick=thick
        oplot,[x_onset4,x_onset4],yvert,linestyle=2,color=get_red() ,thick=thick  
	pclose



        ptr_free,MyPtrArr0
        ptr_free,MyPtrArr1
	print,'Done'





end
