pro sd_tec_amp

common rad_data_blk
common radarinfo
common tec_data_blk
common omn_data_blk
common aur_data_blk
common kpi_data_blk
common amp_data_blk



dateSel = [20130214,20130214];[20130726, 20130726];[20110516, 20110516];
timeRange = [0400, 0600];[0600, 0900];[ 0200,0400 ];[0730,0900]


dt_skip_time=5.d ;;; we search data the grd file every 2 min
del_jul=dt_skip_time/1440.d ;;; This is the time step used to read the data --> Selected to be 60 min

rad_fan_ids = [ 208, 209, 206, 207, 204, 205, 33, 32, 40 ]



;; set plot/map parameters
xrangePlot = [-44, 44]
yrangePlot = [-44,30]
velScale = [-500,500]
tecScale = [0.,20.]
ampScale = [ -1.2, 1.2 ]
cntrMinVal = 0.1
n_levels = 5
coords = "mlt"
omnCharsize = 0.5



;; we need to adjust the xticks with the number of hours plotted...
;; we need to adjust the xticks with the number of hours plotted...
;; we need to adjust the xticks with the number of hours plotted...
num_hours_for_plot = round( ( timeRange[1]/100 ) - ( timeRange[0]/100 ) )


xticks_major_num = 8
xminor_ticks_num = 6
if ( (num_hours_for_plot eq 24.) or (num_hours_for_plot eq 22.)) then begin
        xticks_major_num = num_hours_for_plot/2
        xminor_ticks_num = 6
endif

if ( (num_hours_for_plot eq 23.) ) then begin
        xticks_major_num = 9
        xminor_ticks_num = 6
endif

if ( (num_hours_for_plot ge 13.) and (num_hours_for_plot le 21.) ) then begin
        xticks_major_num = 9
        xminor_ticks_num = 4
endif

if ( (num_hours_for_plot ge 6.) and (num_hours_for_plot le 12.) ) then begin
        xticks_major_num = num_hours_for_plot
        xminor_ticks_num = 6
endif


if ( (num_hours_for_plot ge 3.) and (num_hours_for_plot lt 6.) ) then begin
        xticks_major_num = 2*num_hours_for_plot
        xminor_ticks_num = 6
endif

if ( (num_hours_for_plot gt 1.) and (num_hours_for_plot le 2.) ) then begin
        xticks_major_num = 12
        xminor_ticks_num = 5
endif


if ( (num_hours_for_plot le 1.) ) then begin
        xticks_major_num = 12
        xminor_ticks_num = 5
endif
;; we need to adjust the xticks with the number of hours plotted...
;; we need to adjust the xticks with the number of hours plotted...
;; we need to adjust the xticks with the number of hours plotted...


;;;; Print the date in a proper format on the plot, so get year, month and day from date variable.
;;;; Print the date in a proper format on the plot, so get year, month and day from date variable.
;;;; Print the date in a proper format on the plot, so get year, month and day from date variable.
year_plot=fix(dateSel[0]/1e4)
mndy=double(dateSel[0])-double(year_plot*1e4)
month_plot=fix(mndy/1e2)
day_plot=fix(mndy-month_plot*1e2)
month_list_plot=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

date_in_plot_format=month_list_plot[month_plot-1]+'/'+strtrim(string(day_plot),2)+'/'+strtrim(string(year_plot),2)
;;;; Print the date in a proper format on the plot, so get year, month and day from date variable.
;;;; Print the date in a proper format on the plot, so get year, month and day from date variable.
;;;; Print the date in a proper format on the plot, so get year, month and day from date variable.














tec_read, dateSel
rad_map_read, dateSel
amp_read, dateSel

sfjul,dateSel,timeRange,sjul_search,fjul_search




nele_search=((fjul_search-sjul_search)/del_jul)+1 ;; Num of 2-min times to be searched..
npanels = round((fjul_search-sjul_search)*1440.d/dt_skip_time) + 1


ps_open, '/home/bharatr/Docs/projects/nsfp/plots/sd-amp-tec-' + strtrim( string(dateSel[0]), 2) + '.ps'

for srch=0,nele_search-1 do begin
        clear_page
        set_format, /sardi
        ;;;Calculate the current jul
        juls_curr=sjul_search+srch*del_jul
        juls_curr_tec = juls_curr + 5.d/1440.d

        sfjul,dateCurrPlot,timeCurrPlot,juls_curr,/jul_to_date
        sfjul,dateCurrTEC,timeCurrTEC,juls_curr_tec,/jul_to_date

        print, "dateCurrPlot,timeCurrPlot------------------------------------>", dateCurrPlot,timeCurrPlot

        if (timeCurrPlot ne 0) then begin
                timeCurrTEC = timeCurrPlot
        endif

        ;;_position = define_panel(1, 1, 0, 0, aspect=aspect, /bar) 




        ;; plot omni data
        omn_read,dateCurrPlot,time=timeRange

        pos_omn_panel1=define_panel(1.25,8.5,0,0, aspect=aspect,/bar)
        pos_omn_panel2=define_panel(1.25,8.5,0,1, aspect=aspect,/bar)

        jinds_omn_by_finite=where(finite(omn_data.by_gsm))
        jinds_omn_bz_finite=where(finite(omn_data.bz_gsm))
        yrange_omn_min_val=min([min(omn_data.by_gsm[jinds_omn_by_finite]),min(omn_data.bz_gsm[jinds_omn_bz_finite])])
        yrange_omn_max_val=max([10,max(omn_data.by_gsm[jinds_omn_by_finite]),max(omn_data.bz_gsm[jinds_omn_bz_finite])])
        yrange_omn=[5*(round(yrange_omn_min_val/5)-1),5*(round(yrange_omn_max_val/5)+1)]
        jinds_omn_PD_check=where(finite(omn_data.pd))
    if (jinds_omn_PD_check[0] ne -1) then begin
            max_omn_PD=max(omn_data.pd[jinds_omn_PD_check])
    endif else begin
        max_omn_PD=10
    endelse

;---------------------------------------------------AUR/KP-----------------------------------------

    aur_read,dateCurrPlot,time=timeRange
    kpi_read,dateCurrPlot,time=timeRange

    yrange_symh=[0,50*(round(max(aur_data.ae_index)/50)+1)];[50*(round(min(aur_data.sym_h)/50)-1),50*(round(max(aur_data.sym_h)/50)+1)]

    plot,[0],[0],/nodata,xtickformat='label_date',pos=pos_omn_panel2, $
                xticks= xticks_major_num,xminor= xminor_ticks_num,xstyle=1,xrange=[sjul_search,fjul_search],xtitle='TIME (UT)',xticklen=0.05,$
                ytitle='AE Index',charsize=omnCharsize,ystyle=1,yminor=3,yticks=3,yrange=yrange_symh;[0,1000]

    oplot,aur_data.juls,aur_data.ae_index
    sym_xval=[aur_data.juls[0],aur_data.juls,aur_data.juls[n_elements(aur_data.juls)-1]]
    sym_y_minval=0
    polyfill,sym_xval,[sym_y_minval,aur_data.ae_index,sym_y_minval],color=get_gray()


    
    oplot,[juls_curr,juls_curr],[!y.crange[0],!y.crange[1]],linestyle=2,thick=3, color=get_black()

;---------------------------------------------------AUR/KP-----------------------------------------


    omn_plot_panel, date=dateCurrPlot, time=timeRange, position=pos_omn_panel1, yrange=yrange_omn, $
                                param='bz_gsm',charsize=omnCharsize,xtickformat=_xtickformat, xminor= xminor_ticks_num, xticks= xticks_major_num, $
                                xstyle=1, linecolor=get_red(), ytitle='OMNI-IMF[nT]', linethick=2,ystyle=1,yminor=4,yticks=fix(((yrange_omn[1])-(yrange_omn[0]))/5)



        oplot,[omn_data.juls[0],omn_data.juls[n_elements(omn_data.juls)-1]],[0,0],linestyle='2',thick=2
        loadct,0
        oplot,omn_data.juls,omn_data.by_gsm,color=100

        
        oplot,[juls_curr,juls_curr],[!y.crange[0],!y.crange[1]],linestyle=2,thick=3, color=get_black()

        rad_load_colortable,/leicester

        titleStr = strtrim( string(dateCurrPlot[0]), 2) + "-" + strtrim( string(timeCurrPlot[0]), 2)

        mapPos1 = define_panel(2,1.2,0,0.4, aspect=aspect,/bar)
        mapPos2 = define_panel(2,1.2,1,0.4, aspect=aspect,/bar)

        map_plot_panel,date=dateCurrPlot,time=timeCurrPlot,coords=coords,/no_fill,xrange=xrangePlot, $
                yrange=yrangePlot,pos=mapPos1,/isotropic,grid_charsize='0.5',/north, charsize = 0.5,/no_coast, title = titleStr


        ;; plot the time thing in hh:mm UT format
        ;; plot the time thing in hh:mm UT format
        str_hr_mv_ind_curr = strtrim( fix(timeCurrPlot/100), 2 )

        min_mv_ind_curr = fix( timeCurrPlot - fix(timeCurrPlot/100)*100 )
        if ( min_mv_ind_curr lt 10 ) then $
                str_min_mv_ind_curr = '0' + strtrim( min_mv_ind_curr, 2 ) $
        else $
                str_min_mv_ind_curr = strtrim( min_mv_ind_curr, 2 )

        ;xyouts, !x.crange[1]-.45*(!x.crange[1]-!x.crange[0]), !y.crange[1]+.02*(!y.crange[1]-!y.crange[0]), $
        ;        str_hr_mv_ind_curr+':'+str_min_mv_ind_curr+' UT', align=1, charsize=0.75


        ndots_anim = npanels-1
        load_usersym, /circle, /no_fill

        ;; This strtrim and string stuff is to plot the time in the movies in hh:mm UT format
        str_hr_mv_ind_st = strtrim( fix(timeRange[0]/100), 2 )
        str_hr_mv_ind_end = strtrim( fix(timeRange[1]/100), 2 )

        min_mv_ind_st = fix( timeRange[0] - fix(timeRange[0]/100)*100 )
        if ( min_mv_ind_st lt 10 ) then $
                str_min_mv_ind_st = '0' + strtrim( min_mv_ind_st, 2 ) $
        else $
                str_min_mv_ind_st = strtrim( min_mv_ind_st, 2 )

        min_mv_ind_end = fix( timeRange[1] - fix(timeRange[1]/100)*100 )
        if ( min_mv_ind_end lt 10 ) then $
                str_min_mv_ind_end = '0' + strtrim( min_mv_ind_end, 2 ) $
        else $
                str_min_mv_ind_end = strtrim( min_mv_ind_end, 2 )

        st_time_mv_ind = str_hr_mv_ind_st+':'+str_min_mv_ind_st+' UT'
        ;xyouts, !x.crange[0]+ndots_anim*0.11*((!x.crange[1]-!x.crange[0])/ndots_anim), !y.crange[1]+.02*(!y.crange[1]-!y.crange[0]), $
        ;                        st_time_mv_ind, align=1, charsize=0.75

        end_time_mv_ind = str_hr_mv_ind_end+':'+str_min_mv_ind_end+' UT';strtrim(string(time[1]),2)+' UT'       
        ;xyouts, !x.crange[0]+(ndots_anim+ndots_anim*0.0075)*((!x.crange[1]-!x.crange[0])/ndots_anim), !y.crange[1]+.02*(!y.crange[1]-!y.crange[0]), $
        ;        end_time_mv_ind, align=1, charsize=0.75



        ;; plot the time thing in hh:mm UT format
        ;; plot the time thing in hh:mm UT format



        rad_load_colortable, /bluewhitered
        ;amp_overlay_current, date = dateCurrPlot, time=timeCurrPlot, coords = coords, $
                        ;scale=ampScale, thick=3.,/fill
        amp_overlay_current, date = dateCurrPlot, time=timeCurrPlot, coords = coords, $
            scale=ampScale, neg_color=70, pos_color=190,/fill, thick=7., min_value=0.1
            amp_overlay_current, date = dateCurrPlot, time=timeCurrPlot, coords = coords, $
        scale=ampScale, thick=2, n_levels=4, min_value=0.1
        
        rad_load_colortable,/leicester
        ;rad_map_overlay_vectors, date = dateCurrPlot, time=timeCurrPlot, coords = coords, radar_ids=rad_fan_ids, $
                         ;/no_fov_names, /no_show_Nvc,/no_vector_scale, scale=velScale, symsize=0.25;,fixed_color = get_green()
        rad_map_overlay_scan, rad_fan_ids, juls_curr, /AJ_filter, coords=coords, scale=velScale, rad_sct_flg_val=2

        ;overlay_coast, coords=coords, jul=juls_curr, /no_fill
        map_overlay_grid, grid_linestyle=0, grid_linethick=1, grid_linecolor=get_gray()
        
        plot_colorbar, 2., 1.6, 1.2, 1., /square, scale=velScale, parameter='velocity', legend='Velocity [m/s]',/left

                

        map_plot_panel,date=dateCurrPlot,time=timeCurrPlot,coords=coords,/no_fill,xrange=xrangePlot, $
                yrange=yrangePlot,pos=mapPos2,/isotropic,grid_charsize='0.5',/north, charsize = 0.5,/no_coast

        rad_load_colortable, /bw
        ;;plot tec vectors
        tec_median_filter,date=dateCurrTEC,time=timeCurrTEC, threshold=0.10
        overlay_tec_median, date=dateCurrTEC, time=timeCurrTEC, scale=tecScale, coords=coords

        
        rad_load_colortable, /leicester
        ;rad_map_overlay_vectors, date = dateCurrPlot, time=timeCurrPlot, coords = coords, radar_ids=rad_fan_ids, $
                         ;/no_fov_names, /no_show_Nvc,/no_vector_scale, scale=velScale, symsize=0.25;,fixed_color = 215
        rad_map_overlay_scan, rad_fan_ids, juls_curr, /AJ_filter, coords=coords, scale=velScale, rad_sct_flg_val=2

        rad_map_overlay_poes_bnd, dateCurrPlot, timeCurrPlot, coords = coords, $
                fitline_color = get_red(), fitline_style = 3, $
                fitline_thick = 5

        rad_load_colortable, /bw
        plot_colorbar, 2., 1.6, 1.25, 1., /square,scale=tecScale,legend='Total Electron Content [TECU]', level_format='(f6.2)',param='power'
        rad_load_colortable, /leicester ;; this is for the next page (omni plots are being plotted in black and white)
        

endfor

ps_close, /no_filename


end





