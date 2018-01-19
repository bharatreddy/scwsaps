pro rti_panel, xmaps, ymaps, xmap, ymap                            $
    ,date               = date                                                  $
    ,time               = time                                                  $
    ,long               = long                                                  $
    ,param              = param                                                 $
    ,beam               = beam                                                  $
    ,channel            = channel                                               $
    ,scan_id            = scan_id                                               $
    ,coords             = coords                                                $
    ,yrange             = yrange                                                $
    ,scale              = scale                                                 $
    ,freq_band          = freq_band                                             $
    ,silent             = silent                                                $
    ,bar                = bar                                                   $
    ,charthick          = charthick                                             $
    ,charsize           = charsize                                              $
    ,xstyle             = xstyle                                                $
    ,ystyle             = ystyle                                                $
    ,xtitle             = xtitle                                                $
    ,ytitle             = ytitle                                                $
    ,xticks             = xticks                                                $
    ,xminor             = xminor                                                $
    ,yticks             = yticks                                                $
    ,yminor             = yminor                                                $
    ,xtickformat        = xtickformat                                           $
    ,ytickformat        = ytickformat                                           $
    ,xtickname          = xtickname                                             $
    ,ytickname          = ytickname                                             $
    ,position           = position                                              $
    ,ground             = ground                                                $
    ,grid               = grid                                                  $
    ,last               = last                                                  $
    ,first              = first                                                 $
    ,with_info          = with_info                                             $
    ,no_title           = no_title                                              $
    ,SCATTERFLAG        = _scatterflag                                          $
    ,title              = title                                                 $
    ,exclude            = exclude                                               $
    ,sc_values          = sc_values                                             $
    ,min_power          = min_power                                             $
    ,sun                = sun							  $
    ,xticklen		= xticklen						  $
    ,yticklen		= yticklen					          $
    ,no_plot_mode_change = no_plot_mode_change $
    ,website = website

common rad_data_blk
common amp_data_blk


date = 20110516
timeRange = [ 0600, 1000 ]
selTime = 0820
rtiCoords = 'magn'
selRad = 'cvw'
selBeam = 7
velScale = [ -500, 500 ]
ampScale = [ -0.4, 0.4 ]
ampCoords = 'mlt'
coords=rtiCoords

min_value = 0.1
selMLT =22.
scale=velScale
beam=selBeam

yrange=[50,65]

rad_load_colortable,/website

rad_fit_read, date, selRad, /filter



time=timeRange

charsize=1.
SCATTERFLAG = 3


; get index for current data
data_index = rad_fit_get_data_index()
if data_index eq -1 then $
	return

if (*rad_fit_info[data_index]).nrecs eq 0L then begin
	if ~keyword_set(silent) then begin
		prinfo, 'No data in index '+string(data_index)
		rad_fit_info
	endif
	return
endif

if ~keyword_set(param) then $
	param = get_parameter()

if ~is_valid_parameter(param) then begin
	prinfo, 'Invalid plotting parameter: '+param
	return
endif

if n_elements(beam) eq 0 then $
	beam = rad_get_beam()

if ~keyword_set(coords) then $
	coords = get_coordinates()

if ~is_valid_coord_system(coords) then begin
	prinfo, 'Not a valid coordinate system: '+coords
	prinfo, 'Using gate.'
	coords = 'gate'
endif

if ~keyword_set(freq_band) then $
;	freq_band = get_default_range('tfreq')
;	Setting this as data can be from outside of the 'standard band' and we should plot it still.  -KTS 20150807
	freq_band = [8,20]
if n_params() lt 4 then begin
	if ~keyword_set(silent) and ~keyword_set(position) then $
		prinfo, 'XMAPS, YMAPS, XMAP and YMAP not set, using default.'
	xmaps = 1.25
	ymaps = 1.
	xmap = 0.
	ymap = 0
endif
if ~keyword_set(position) then $
	position = define_panel(xmaps, ymaps, xmap, ymap, bar=bar, with_info=with_info, no_title=no_title)

if ~keyword_set(date) then begin
	if ~keyword_set(silent) then $
		prinfo, 'No DATE given, trying for scan date.'
	caldat, (*rad_fit_data[data_index]).juls[0], month, day, year
	date = year*10000L + month*100L + day
endif

if ~keyword_set(time) then $
	time = [0000,2400]

sfjul, date, time, sjul, fjul, long=long
xrange = [sjul, fjul]

if n_elements(xtitle) eq 0 then $
	_xtitle = 'Time UT' $
else $
	_xtitle = xtitle

if n_elements(xtickformat) eq 0 then $
	_xtickformat = 'label_date' $
else $
	_xtickformat = xtickformat

if n_elements(xtickname) eq 0 then $
	_xtickname = '' $
else $
	_xtickname = xtickname

if n_elements(ytitle) eq 0 then $
	_ytitle = get_default_title(coords) $
else $
	_ytitle = ytitle

if n_elements(ytickformat) eq 0 then $
	_ytickformat = '' $
else $
	_ytickformat = ytickformat

if n_elements(ytickname) eq 0 then $
	_ytickname = '' $
else $
	_ytickname = ytickname

if ~keyword_set(scan_id) then $
	scan_id = -1

if ~keyword_set(ground) then $
	ground = -1

if n_elements(channel) eq 0 and scan_id eq -1 then begin
	channel = (*rad_fit_info[data_index]).channels[0]
endif
if ~keyword_set(xstyle) then $
	xstyle = 1

if ~keyword_set(ystyle) then $
	ystyle = 1
if ~keyword_set(charsize) then $
	charsize = get_charsize(xmaps, ymaps)

if ~keyword_set(scale) then begin
	if strcmp(get_parameter(), param) then $
		scale = get_scale() $
	else $
		scale = get_default_range(param)
endif

if ~keyword_set(yrange) then $
	yrange = get_default_range(coords)

if ~keyword_set(xticks) then $
	_xticks = get_xticks(sjul, fjul, xminor=_xminor) $
else $
	_xticks = xticks

if keyword_set(xminor) then $
	_xminor = xminor

if n_elements(min_power) eq 0 then $
	min_power = 0.

; Determine maximum width to plot scan - to decide how big a 'data gap' has to
; be before it really is a data gap.  Default to 5 minutes
if ~keyword_set(max_gap) then $
	max_gap = 2.5


ps_open, '/home/bharatr/Docs/plots/sd-amp-rti-' + strtrim( string(date), 2) + '.ps'    

; set up coordinate system for plot
plot, [0,0], /nodata, xstyle=5, ystyle=5, $
	yrange=yrange, xrange=xrange, position=position

; get data
xtag = 'juls'
ytag = param
if ~tag_exists((*rad_fit_data[data_index]), xtag) then begin
	if ~keyword_set(silent) then $
		prinfo, 'Parameter '+xtag+' does not exist in RAD_FIT_DATA.'
	return
endif
if ~tag_exists((*rad_fit_data[data_index]), ytag) then begin
	if ~keyword_set(silent) then $
		prinfo, 'Parameter '+ytag+' does not exist in RAD_FIT_DATA.'
	return
endif
s = execute('xdata = (*rad_fit_data[data_index]).'+xtag)
s = execute('ydata = (*rad_fit_data[data_index]).'+ytag)

; get power if min_power keyword is set
if min_power gt 0. then $
	pwr = (*rad_fit_data[data_index]).power

; select data to plot
; must fit beam, channel, scan_id, time (roughly) and frequency
;
; check first whether data is available for the user selection
; then find data to actually plot
beam_inds = where((*rad_fit_data[data_index]).beam eq beam, cc)
if cc eq 0 then begin
	if ~keyword_set(silent) then $
		prinfo, 'No data found for beam '+string(beam)
	return
endif
txdata = xdata[beam_inds]
tchann = (*rad_fit_data[data_index]).channel[beam_inds]
ttfreq = (*rad_fit_data[data_index]).tfreq[beam_inds]
tscani = (*rad_fit_data[data_index]).scan_id[beam_inds]
if n_elements(channel) ne 0 then begin
	scch_inds = where(tchann eq channel, cc)
	if cc eq 0 then begin
		if ~keyword_set(silent) then $
			prinfo, 'No data found for for beam '+string(beam)+$
				' and channel '+string(channel)
		return
	endif
endif else if scan_id ne -1 then begin
	if scan_id eq -1 then $
		return
	scch_inds = where(tscani eq scan_id, cc)
	if cc eq 0 then begin
		if ~keyword_set(silent) then $
			prinfo, 'No data found for for beam '+string(beam)+$
				' and scan_id '+string(scan_id)
		return
	endif
endif
tchann = 0
tscani = 0
txdata = txdata[scch_inds]
ttfreq = ttfreq[scch_inds]
juls_inds = where(txdata ge sjul-10.d/1440.d and txdata le fjul+10.d/1440.d, cc)
if cc eq 0 then begin
	if ~keyword_set(silent) then $
		prinfo, 'No data found for beam '+string(beam)+$
			' and time '+format_time(time)
	return
endif
txdata = 0
ttfreq = ttfreq[juls_inds]
tfre_inds = where(ttfreq*0.001 ge freq_band[0] and ttfreq*0.001 le freq_band[1], cc)
if cc eq 0 then begin
	if ~keyword_set(silent) then $
		prinfo, 'No data found for beam '+string(beam)+$
			' and channel '+string(channel)+' and time '+format_time(time) + $
			' and freq. band '+strjoin(string(freq_band,format='(F4.1)'),'-')
    return
endif
ttfreq = 0
; get indeces of data to plot
if n_elements(channel) ne 0 then begin
	beam_inds = where((*rad_fit_data[data_index]).beam eq beam and $
		(*rad_fit_data[data_index]).channel eq channel and $
		(*rad_fit_data[data_index]).juls ge sjul-10.d/1440.d and $
		(*rad_fit_data[data_index]).juls le fjul+10.d/1440.d and $
		(*rad_fit_data[data_index]).tfreq*0.001 ge freq_band[0] and $
		(*rad_fit_data[data_index]).tfreq*0.001 le freq_band[1], $
		nbeam_inds)
endif else if scan_id ne -1 then begin
	beam_inds = where((*rad_fit_data[data_index]).beam eq beam and $
		(*rad_fit_data[data_index]).scan_id eq scan_id and $
		(*rad_fit_data[data_index]).juls ge sjul-10.d/1440.d and $
		(*rad_fit_data[data_index]).juls le fjul+10.d/1440.d and $
		(*rad_fit_data[data_index]).tfreq*0.001 ge freq_band[0] and $
		(*rad_fit_data[data_index]).tfreq*0.001 le freq_band[1], $
		nbeam_inds)
endif

old_lagfr = (*rad_fit_data[data_index]).lagfr[beam_inds[0]]
old_smsep = (*rad_fit_data[data_index]).smsep[beam_inds[0]]

; check if interferometer data is plotted, whether it is available
if strcmp(param, 'phi0', /fold) or strcmp(param, 'elevation', /fold) then begin
	dummy = where( (*rad_fit_data[data_index]).xcf[beam_inds] gt 0b, xcc )
	if xcc lt 2L then begin
		prinfo, 'No interferometer data for this time.'
		return
	endif
endif

; get color preferences
foreground  = get_foreground()

; and some user preferences
IF N_ELEMENTS(_scatterflag) EQ 0 THEN scatterflag = rad_get_scatterflag() ELSE scatterflag = _scatterflag

; shift/rotate color indeces if param is velocity
;rot = 0
;shi = 0
;IF param EQ 'velocity' then begin
;	if strcmp(get_colortable(), 'bluewhitered', /fold) or strcmp(get_colortable(), 'leicester', /fold) or strcmp(get_colortable(), 'default', /fold) THEN $
;		rot = 1
;	if strcmp(get_colortable(), 'aj', /fold) or strcmp(get_colortable(), 'bw', /fold) or strcmp(get_colortable(), 'whitered', /fold) THEN $
;		shi = 1
;endif

ajul = (sjul+fjul)/2.d
caldat, ajul, mm, dd, year
yrsec = (ajul-julday(1,1,year,0,0,0))*86400.d

CASE coords OF
    'mix_rang': gsCoords = 'gs_rang'
    'mix_geog': gsCoords = 'gs_geog'
    'mix_magn': gsCoords = 'gs_magn'
          ELSE: gsCoords = coords
ENDCASE

IF (coords EQ 'gs_rang')  OR (coords EQ 'gs_geog')  OR (coords EQ 'gs_magn')    $
OR (coords EQ 'mix_rang') OR (coords EQ 'mix_geog') OR (coords EQ 'mix_magn')   $
    THEN gsMapping = 1 ELSE gsMapping = 0

RAD_DEFINE_BEAMS                                                                $
    ,(*rad_fit_info[data_index]).id                                             $
    ,(*rad_fit_info[data_index]).nbeams                                         $
    ,(*rad_fit_info[data_index]).ngates                                         $
    ,year                                                                       $
    ,yrsec                                                                      $
    ,coords             = coords                                                $
    ,lagfr0             = (*rad_fit_data[data_index]).lagfr[beam_inds[0]]       $
    ,smsep0             = (*rad_fit_data[data_index]).smsep[beam_inds[0]]       $
    ,fov_loc_full       = fov_loc_full                                          $
    ,fov_loc_center     = fov_loc_center

RAD_DEFINE_BEAMS                                                                $
    ,(*rad_fit_info[data_index]).id                                             $
    ,(*rad_fit_info[data_index]).nbeams                                         $
    ,(*rad_fit_info[data_index]).ngates                                         $
    ,year                                                                       $
    ,yrsec                                                                      $
    ,coords             = gsCoords                                              $
    ,lagfr0             = (*rad_fit_data[data_index]).lagfr[beam_inds[0]]       $
    ,smsep0             = (*rad_fit_data[data_index]).smsep[beam_inds[0]]       $
    ,fov_loc_full       = gs_fov_loc_full                                       $
    ,fov_loc_center     = gs_fov_loc_center


; plot sunrise/sunset/solar noon
if keyword_set(sun) then begin
	rad_calc_sunset, date, (*rad_fit_info[data_index]).code, beam, (*rad_fit_info[data_index]).ngates, $
		risetime=risetime, settime=settime, solnoon=solnoon

    sunlinecol = 190
    tcmap = get_colortable()
    loadct, 0

    toverflow = where(risetime lt sjul, cc, complement=tunderflow, ncomplement=cccomp)
    if cc gt 0 then $
    risetime[toverflow] = risetime[toverflow]+1.d
    toverflow = where(settime gt fjul, cc, complement=tunderflow, ncomplement=cccomp)
    if cc gt 0 then $
        settime[toverflow] = settime[toverflow]-1.d
    toutbound = where(risetime gt fjul, cc)
    if cc gt 0 then $
        risetime[toutbound] = 0.
    toutbound = where(settime lt sjul, cc)
    if cc gt 0 then $
        settime[toutbound] = 0.

    
    for ig=0,n_elements(risetime)-2 do begin
        if fov_loc_center[0,beam,ig+1] le yrange[1] and fov_loc_center[0,beam,ig] ge yrange[0] then begin
            ylims_night = fov_loc_center[0,beam,ig]*[1,0,0,1] + fov_loc_center[0,beam,ig+1]*[0,1,1,0]

            if settime[ig+1] lt risetime[ig+1] then begin
                if settime[ig] ge risetime[ig] then continue
                xlims_night = [settime[ig:ig+1], reverse(risetime[ig:ig+1])] 
                if settime[ig+1] eq 0. or settime[ig] eq 0. then $
                    xlims_night = [xrange[0]*[1,1], reverse(risetime[ig:ig+1])]
                if risetime[ig+1] ne 0. and risetime[ig] ne 0. then $
                    polyfill, xlims_night, ylims_night , col=sunlinecol*1.2
            endif
            if settime[ig+1] gt risetime[ig+1] then begin
                if settime[ig] le risetime[ig] then continue
                xlims_night = [xrange[0]*[1,1], reverse(risetime[ig:ig+1])]
                if risetime[ig] ne 0. and risetime[ig+1] ne 0. then $
                    polyfill, xlims_night, ylims_night , col=sunlinecol*1.2
                xlims_night = [settime[ig:ig+1], xrange[1]*[1,1]]
                polyfill, xlims_night, ylims_night , col=sunlinecol*1.2
            endif
        endif
    endfor
    nleg = 0

    rad_load_colortable, tcmap
    set_colortable, tcmap
endif

; Added by EGT 20170410 for website plotting
if keyword_set(website) and param eq 'lag0power' then begin
    tcmap = get_colortable()
    loadct, 13
    tcsteps = get_colorsteps()
    set_colorsteps, 240
endif

; overplot data
; Cycle through beams to plot
FOR b=0L, nbeam_inds-2L DO BEGIN

	; If lag to first range or gate length has changed then update
	; field-of-view info
	IF ((*rad_fit_data[data_index]).lagfr[beam_inds[b]] NE old_lagfr)                       $
        OR ((*rad_fit_data[data_index]).smsep[beam_inds[b]] NE old_smsep) THEN BEGIN
		RAD_DEFINE_BEAMS                                                                $
                    ,(*rad_fit_info[data_index]).id                                             $
                    ,(*rad_fit_info[data_index]).nbeams                                         $
		    ,(*rad_fit_info[data_index]).ngates                                         $
                    ,year                                                                       $
                    ,yrsec                                                                      $
                    ,coords             = coords                                                $
		    ,lagfr0             = (*rad_fit_data[data_index]).lagfr[beam_inds[b]]       $
                    ,smsep0             = (*rad_fit_data[data_index]).smsep[beam_inds[b]]       $
		    ,fov_loc_full       = fov_loc_full                                          $
                    ,fov_loc_center     = fov_loc_center

		RAD_DEFINE_BEAMS                                                                $
                    ,(*rad_fit_info[data_index]).id                                             $
                    ,(*rad_fit_info[data_index]).nbeams                                         $
		    ,(*rad_fit_info[data_index]).ngates                                         $
                    ,year                                                                       $
                    ,yrsec                                                                      $
                    ,coords             = gsCoords                                              $
		    ,lagfr0             = (*rad_fit_data[data_index]).lagfr[beam_inds[b]]       $
                    ,smsep0             = (*rad_fit_data[data_index]).smsep[beam_inds[b]]       $
		    ,fov_loc_full       = gs_fov_loc_full                                       $
                    ,fov_loc_center     = gs_fov_loc_center

		old_lagfr = (*rad_fit_data[data_index]).lagfr[beam_inds[b]]
		old_smsep = (*rad_fit_data[data_index]).smsep[beam_inds[b]]
	ENDIF

	start_time = xdata[beam_inds[b]]
	; check for data gaps
	if xdata[beam_inds[b+1]] gt start_time+max_gap/1440.d then $
		; make this point 1 minute long, just for default reasons
		end_time = start_time+1./1440.d $
	else $
		end_time = xdata[beam_inds[b+1]]

	; check for freaky time travel scenario, as ETS calls it....
	if end_time lt start_time then begin
		prinfo, 'EndTime < StartTime: '+format_juldate(start_time, /time)+'->'+format_juldate(end_time, /time)
		end_time = start_time+1./1440.d
	endif

    ; Added by EGT on 20140821 for Kodiak data w/data on more beams than in hardware file
    sz = size(fov_loc_center)
    IF beam GE sz[2] THEN BEGIN
        RAD_DEFINE_BEAMS, (*rad_fit_info[data_index]).id, max((*rad_fit_data[data_index]).beam)+1, $
            (*rad_fit_info[data_index]).ngates, year, yrsec, coords = coords, $
            lagfr0 = (*rad_fit_data[data_index]).lagfr[beam_inds[b]], $
            smsep = (*rad_fit_data[data_index]).smsep[beam_inds[b]], fov_loc_full = fov_loc_full, $
            fov_loc_center = fov_loc_center
    ENDIF

	; cycle through ranges
	FOR r=0, (*rad_fit_info[data_index]).ngates-1 DO BEGIN
                gs = (*rad_fit_data[data_index]).gscatter[beam_inds[b],r]

                IF KEYWORD_SET(gsMapping) AND KEYWORD_SET(gs) THEN BEGIN
                    IF (ABS(gs_fov_loc_center[0,beam,r+1]) LT ABS(yrange[0]))                           $
                    OR (ABS(gs_fov_loc_center[0,beam,r])   GT ABS(yrange[1])) THEN CONTINUE
                ENDIF ELSE BEGIN
                    IF (ABS(fov_loc_center[0,beam,r+1]) LT ABS(yrange[0]))                              $
                    OR (ABS(fov_loc_center[0,beam,r])   GT ABS(yrange[1])) THEN CONTINUE
                ENDELSE

		; only plot points with real data in it
		IF ydata[beam_inds[b],r] eq 10000. THEN $
			continue

		; only plot values above power threshold
		if min_power gt 0. then $
			if pwr[beam_inds[b],r] lt min_power then $
				continue

		; check whether to exclude values
		if n_elements(exclude) eq 2 then begin
                  IF exclude[0] NE exclude[1] THEN BEGIN
			if ydata[beam_inds[b],r] lt exclude[0] or ydata[beam_inds[b],r] gt exclude[1] then $
				continue
                        ENDIF
		endif

		; if only plotting ground scatter, skip all points where
		; the gscatter flag is not 1
		IF scatterflag EQ 1 AND gs NE 1 THEN CONTINUE

		; if only plotting ionospheric scatter, skip all points where
		; the gscatter flag is 1
		IF scatterflag EQ 2 AND gs EQ 1 THEN CONTINUE

		; if scatter flag is 3, plot ground scatter in gray
		; only for velocity, though
;		IF param EQ 'velocity' AND ( ( scatterflag EQ 3 AND $
;			(*rad_fit_data[data_index]).gscatter[beam_inds[b],r] EQ 1 ) or abs(ydata[beam_inds[b],r]) lt ground ) THEN $
;				col = get_gray() $
;		ELSE $
;			col = get_color_index(ydata[beam_inds[b],r], param=param, scale=scale, sc_values=sc_values)

        ; Changed from above by EGT on 20140722 to match rad_fit_overlay_scan_multi.pro functionality
        IF param EQ 'velocity' AND ( scatterflag EQ 3 AND (*rad_fit_data[data_index]).gscatter[beam_inds[b],r] EQ 1 AND ground EQ -1 ) THEN $
                col = get_gray() $
        ELSE IF param EQ 'velocity' AND ( scatterflag EQ 3 AND abs(ydata[beam_inds[b],r]) LE ground ) THEN $
                col = get_gray() $
        ELSE $
                col = get_color_index(ydata[beam_inds[b],r], param=param, scale=scale, sc_values=sc_values)

		;print, b, r, ydata[beam_inds[b],r], col
		;print, scale, sc_values

		; finally plot the point
                IF KEYWORD_SET(gsMapping) AND gs EQ 1 THEN BEGIN
                    POLYFILL,[start_time,start_time,end_time,end_time]                      $
                        ,[gs_fov_loc_center[0,beam,r]  , gs_fov_loc_center[0,beam,r+1]      $
                        , gs_fov_loc_center[0,beam,r+1], gs_fov_loc_center[0,beam,r]]       $
                        ,COL=col,NOCLIP=0
                ENDIF ELSE BEGIN
                    POLYFILL,[start_time,start_time,end_time,end_time]                      $
                        ,[fov_loc_center[0,beam,r]  , fov_loc_center[0,beam,r+1]            $
                        , fov_loc_center[0,beam,r+1], fov_loc_center[0,beam,r]]             $
                        ,COL=col,NOCLIP=0
                ENDELSE
	ENDFOR

	if ~keyword_set(no_plot_mode_change) then begin
		; check whether we have a change of scan id
		if (*rad_fit_data[data_index]).scan_id[beam_inds[b]] ne (*rad_fit_data[data_index]).scan_id[beam_inds[b+1L]] then begin
			oplot, replicate(end_time, 2), !y.crange, linestyle=2, thick=!p.thick, color=get_gray()
		endif

		; check whether we have a change of hf/if mode
		if (*rad_fit_data[data_index]).ifmode[beam_inds[b]] ne (*rad_fit_data[data_index]).ifmode[beam_inds[b+1L]] then begin
			oplot, replicate(end_time, 2), !y.crange, linestyle=2, thick=!p.thick, color=get_gray()
		endif
	endif

ENDFOR

; Added by EGT 20170410 for website plotting
if keyword_set(website) and param eq 'lag0power' then begin
    rad_load_colortable, tcmap
    set_colortable, tcmap
    set_colorsteps, tcsteps
endif

; plot sunrise/sunset/solar noon
if keyword_set(sun) then begin
;	rad_calc_sunset, date, (*rad_fit_info[data_index]).code, beam, (*rad_fit_info[data_index]).ngates, $
;		risetime=risetime, settime=settime, solnoon=solnoon
	oplot, risetime, fov_loc_center[0,beam,*], linestyle=2, thick=!p.thick
	oplot, settime, fov_loc_center[0,beam,*], linestyle=2, thick=!p.thick
	oplot, solnoon, fov_loc_center[0,beam,*], linestyle=2, thick=!p.thick
endif

;IF param EQ 'velocity' then $
;	rad_load_colortable

; check if format is sardines.
; if yes, loose the x axis information
; unless it is given
fmt = get_format(sardines=sd, tokyo=ty)
if sd and ~keyword_set(last) then begin
	if ~keyword_set(xtitle) then $
		_xtitle = ' '
	if ~keyword_set(xtickformat) then $
		_xtickformat = ''
	if ~keyword_set(xtickname) then $
		_xtickname = replicate(' ', 60)
endif
if ty and ~keyword_set(first) then begin
	if ~keyword_set(ytitle) then $
		_ytitle = ' '
	if ~keyword_set(ytickformat) then $
		_ytickformat = ''
	if ~keyword_set(ytickname) then $
		_ytickname = replicate(' ', 60)
endif

; Overlay grid if required
if keyword_set(grid) then begin
	_gridstyle = 1
	_xticklen = 1.
	_yticklen = 1.
endif else begin
	if ~keyword_set(xticklen) then $
		_xticklen = -.02 $;-!x.ticklen
	else $
		_xticklen = xticklen
		
	if ~keyword_set(yticklen) then $
		_yticklen = -.02 $;-!y.ticklen
	else $
		_yticklen = yticklen
endelse




    ;; AMPERE analysis
amp_read, date

load_usersym,/circle
rad_load_colortable, /bluewhitered

dt_skip_time=2.d ;;; we search data every 2 min
del_jul=dt_skip_time/1440.d 


sfjul, date, timeRange, sjul, fjul

nele_search=((fjul-sjul)/del_jul)+1 

for srch=0,nele_search-1 do begin

    currJul=sjul+srch*del_jul

    if strcmp(ampCoords,'mlt',/fold) then $
        mlt0inmlonh = 0. $
    else $
        mlt0inmlonh = -mltdavit(year,(currJul-julday(1,1,year,0))*86400.d,0.)

    ; int_hemi is 0 for north and 1 for south
    int_hemi = 0
    dd = min( abs( (*amp_data[int_hemi]).mjuls-currJul ), index)
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


    julVal = (*amp_data[int_hemi]).mjuls[index]
    selMLTinds = where( mlts eq selMLT )
    currMlats = mlats[selMLTinds]
    currMlts = mlts[selMLTinds]
    currJrs = jrs[selMLTinds]

    lenCurrAmp = size(currJrs)

    

    julArr = dblarr( lenCurrAmp[1] ) + julVal
    for ai=0,n_elements(currJrs)-1 do begin
        ampCurrCol = get_color_index(currJrs[ai], param='power', scale=ampScale)
        oplot, [julVal], [currMlats[ai]], psym=8,color=ampCurrCol
    endfor

endfor

rad_load_colortable,/website



























; "over"plot axis
plot, [0,0], /nodata, position=position, $
	charthick=charthick, charsize=charsize, $
	yrange=yrange, xrange=xrange, $
	xstyle=xstyle, ystyle=ystyle, xtitle=_xtitle, ytitle=_ytitle, $
	xticks=_xticks, xminor=_xminor, yticks=yticks, yminor=yminor, $
	xtickformat=_xtickformat, ytickformat=_ytickformat, $
	xtickname=_xtickname, ytickname=_ytickname, $
	color=get_foreground(), title=title, $
	xticklen=_xticklen, yticklen=_xticklen, $
	xgridstyle=_gridstyle, ygridstyle=_gridstyle





    plot_colorbar, 1.5, 1., 0.4, 0., scale=velScale, param='velocity', /with_info

ps_close,/no_filename

end

