%%
%%  wings_view.erl --
%%
%%     This module implements most of the commands in the View menu.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_view).
-export([menu/0,command/2,
	 virtual_mirror/2,
	 init/0,initial_properties/0,delete_all/1,reset/0,
	 current/0,set_current/1,
	 load_matrices/1,projection/0,projection/1,
	 modelview/0,align_view_to_normal/1,
	 eye_point/0,export_views/1,import_views/2,camera_info/2]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,flatmap/2,zip/2]).

menu() ->
    L = wings_pref:get_value(number_of_lights),
    [{?__(68,"Show"),{show,
     [{?__(1,"Ground Plane"),show_groundplane,?__(2,"Show the ground plane"),
       crossmark(show_groundplane)},
      {?__(3,"Axes"),show_axes,?__(4,"Show the coordinate axes"),crossmark(show_axes)},
      {?__(78,"Camera Image Plane"),show_cam_imageplane,?__(79,"Show the camera image plane"),crossmark(show_cam_imageplane)},
      {?__(48,"Show Info Text"),show_info_text,
       ?__(49,"Show an informational text at the top of this Geometry window"),
       crossmark(show_info_text)},
        separator,
      {?__(17,"Show Saved BB"),show_bb,?__(18,"Display any saved bounding box"),crossmark(show_bb)},
      {?__(69,"Show BB Center"),show_bb_center,
       ?__(70,"Display the Center of any saved bounding box"),crossmark(show_bb_center)},
      {?__(61,"Clipping Planes"),clip_plane,?__(62,"Toggle extra clipping planes (use default axis)"),
       crossmark(clip_plane)},
      {?__(23,"Show Normals"),show_normals,?__(24,"Show normals for selected elements"),
       crossmark(show_normals)},
        separator,
      {?__(42,"Show Colors"),show_colors,
       ?__(43,"Show vertex colors"),
       crossmark(show_colors)},
      {?__(44,"Show Materials"),show_materials,
       ?__(45,"Show materials"),
       crossmark(show_materials)},
      {?__(46,"Show Textures"),show_textures,
       ?__(47,"Show textures"),
       crossmark(show_textures)},
      {?__(76,"Show Normal Maps"),show_normal_maps,
       ?__(77,"Use Normal Maps to display rough materials"),
       crossmark(show_normal_maps)},
      separator,
      {?__(74,"Filter Textures"),filter_texture,
       ?__(75,"Activates Texture Filtering for all textures. (Disable filtering for accurate display of very low resolution textures)"),
	   crossmark(filter_texture)}]}},
     separator,
     {?__(7,"Wireframe"),wireframe,?__(8,"Display selected objects as a wireframe (same for all objects if nothing is selected)")},
     {?__(9,"Shade"),shade,?__(10,"Display selected objects as shaded (same for all objects if nothing is selected)")},
     {?__(11,"Toggle Wireframe"),toggle_wireframe,
      ?__(12,"Toggle display mode for selected objects (same for all objects if nothing is selected)")},
     {?__(19,"Show Edges"),show_edges,?__(20,"Show edges in workmode"),crossmark(show_edges)},
     {?__(72,"Show Backfaces"),show_backfaces,
      ?__(73,"Show backfaces when there is a hole or hiddwn faces in an object"),crossmark(show_backfaces)},
     {?__(21,"Show Wireframe Backfaces"),show_wire_backfaces,
      ?__(22,"Show wireframe backfaces"),crossmark(show_wire_backfaces)},
     separator,
     {?__(5,"Workmode"),workmode,?__(6,"Toggle flat/smooth shading"),
      crossmark(workmode)},
     {?__(13,"Toggle Proxy Mode"),smooth_proxy,
      ?__(14,"Toggle the smooth proxy mode for selected objects")},
     {?__(15,"Quick Smoothed Preview"),quick_preview,
      ?__(16,"Toggle the smooth proxy mode for all objects")},
     separator |
     [{?__(36,"Scene Lights"),scene_lights,
       ?__(37,"Use the lights defined in the scene"),
       crossmark(scene_lights)},
      {one_of(L == 1, ?__(38,"Use Camera Light"),?__(39,"Use a camera work Light")),toggle_lights,
       one_of(L == 1, ?__(40,"Use two work lights"),?__(41,"Use a simple sky light simulation"))},
      separator,
      {?__(31,"Orthographic View"),orthogonal_view,
       ?__(32,"Toggle between orthographic and perspective views"),
       crossmark(orthogonal_view)},
      {?__(25,"Reset View"),reset,?__(26,"Reset view to the default position")},
      {?__(27,"Aim"),aim,?__(28,"Aim the camera at the selected element")},
      {?__(66,"Highlight Aim"),highlight_aim,
       ?__(67,"Aim camera at mouseover highlight. (Requires 'Use Highlight as Temporary Selection' enabled, and an assigned hotkey)")},
      {?__(29,"Frame"),frame,?__(30,"Dolly to show all selected elements (or all objects if nothing is selected)")},
      {?__(65,"Frame Disregards Mirror"),frame_disregards_mirror,crossmark(frame_disregards_mirror)},
      {?__(57,"Align to Selection"),align_to_selection,
       ?__(58,"Align the view to the normal of the selection")},
      separator,
      {?__(33,"Saved Views "),
       {views, 	views_submenu()}},

      {?__(50,"View Along"),{along,[{?__(51,"+X"),x},
				    {?__(52,"+Y"),y},
				    {?__(53,"+Z"),z},
				    {?__(54,"-X"),neg_x},
				    {?__(55,"-Y"),neg_y},
				    {?__(56,"-Z"),neg_z},
				    separator,
				    {?__(71,"Nearest Axis"),nearest_axis}]}},
      separator,
      {?__(34,"Camera Settings..."),camera_settings,?__(35,"Set field of view, and near and far clipping planes")},
      separator,
      {?__(59,"Auto Rotate"),auto_rotate,?__(60,"Spin the view")}]].

crossmark(Key) ->
    %% Start with everything false, update when values loaded
    [{crossmark, wings_pref:get_value(Key, false)}].

views_submenu() ->
    [{?__(1,"Next"),next, ?__(11, "Move camera to next view")},
     {?__(2,"Current"),current, ?__(12, "Move camera to current view")},
     {?__(3,"Prev"),prev, ?__(13, "Move camera to current view")},
     {?__(4,"Save..."),save, ?__(14,"Save this view"), [option]},
     {?__(8,"Delete"),delete, ?__(18, "Delete the current view")}].


view_index(I, N) when is_integer(I), is_integer(N), N > 0 ->
    J = (I-1) rem N,
    if J < 0 -> J + 1 + N;
       true -> J + 1
    end.

command(reset, St) ->
    reset(),
    St;
command(workmode, St) ->
    ?SLOW(toggle_option(workmode)),
    St;
command(toggle_wireframe, #st{sel=[]}=St) ->
    wireframe_all(toggle, St),
    St;
command(toggle_wireframe, St) ->
    wireframe_sel(toggle, St),
    St;
command(wireframe, #st{sel=[]}=St) ->
    wireframe_all(true, St),
    St;
command(wireframe, St) ->
    wireframe_sel(true, St),
    St;
command(shade, #st{sel=[]}=St) ->
    wireframe_all(false, St),
    St;
command(shade, St) ->
    wireframe_sel(false, St),
    St;
command(smooth_proxy, St) ->
    wings_proxy:setup(St),
    St;
command(quick_preview, St) ->
    ?SLOW(wings_proxy:quick_preview(St)),
    St;
command(orthogonal_view, St) ->
    toggle_option(orthogonal_view),
    St;
command(Option={show,What}, St) ->
    Prev = toggle_option(Option),
    Keep = [show_groundplane, show_axes, show_info_text, show_cam_imageplane,
            show_bb, show_bb_center, clip_plane],
    KeepDls = lists:member(What, Keep),
    if
        KeepDls ->
            ignore;
	What =:= show_normals ->
	    Prev andalso wings_dl:map(fun(D, _) -> D#dlo{normals=none} end, []);
	What =:= filter_texture ->
            wings_image:filter_images(not Prev),
	    wings_dl:map(fun(#dlo{proxy_data=PD}=D, _) ->
				 %% We only need to invalidate display lists.
				 D#dlo{work=none,smooth=none,
				       proxy_data=wings_proxy:invalidate(PD, dl)}
			 end, []);
       true -> %% show_textures, show_normal_maps, show_colors
	    wings_dl:map(fun(#dlo{proxy_data=PD}=D, _) ->
				 %% Must invalidate vertex buffers.
				 D#dlo{work=none,smooth=none,vab=none,
				       proxy_data=wings_proxy:invalidate(PD, vab)};
			    (D, _) -> D
			 end, [])
    end,
    St;
command(show_edges, St) ->
    Bool = toggle_option(show_edges),
    case Bool of
	false -> St;
	true ->
	    wings_dl:map(fun(D, _) -> D#dlo{hard=none} end, []),
	    St
    end;
command(show_backfaces, St) ->
    toggle_option(show_backfaces),
    St;
command({highlight_aim,{Type,{Selmode,Sel,MM}}}, St) ->
    highlight_aim(Type, Selmode, Sel, MM, St),
    St;
command(highlight_aim, St) ->
    aim(St),
    St;
command(aim, St) ->
    aim(St),
    St;
command(frame, St) ->
    frame(St),
    St;
command({views,Views}, St) ->
    views(Views, St);
command({along,Axis}, St) ->
    along(Axis),
    St;
command(auto_rotate, St) ->
    auto_rotate(St);
command(rotate_left, St) ->
    #view{azimuth=Az0} = View = current(),
    Az = Az0 + ?GET(auto_rotate),
    set_current(View#view{azimuth=Az}),
    St;
command(align_to_selection, St) ->
    aim(St),
    align_to_selection(St);
command(toggle_lights, St) ->
    toggle_lights(),
    St;
command(scene_lights, St) ->
    toggle_option(scene_lights),
    St;
command(camera_settings, St) ->
    camera(St);
command(Key, St) ->
    toggle_option(Key),
    St.

virtual_mirror(create, #st{selmode=face}=St0) ->
    St = wings_sel:map(fun virtual_mirror_fun/2, St0),
    {save_state,St#st{sel=[]}};
virtual_mirror(create, _) ->
    wings_u:error_msg(?__(1,"Virtual mirror requires a face selection."));
virtual_mirror(break, St) ->
    break_mirror(St);
virtual_mirror(freeze, St0) ->
    freeze_mirror(St0).

wireframe_all(false, _) ->
    wings_wm:set_prop(wireframed_objects, gb_sets:empty());
wireframe_all(true, St) ->
    All = all_selectable(St),
    wings_wm:set_prop(wireframed_objects, All);
wireframe_all(toggle, St) ->
    Selectable = all_selectable(St),
    Prev = wings_wm:get_prop(wireframed_objects),
    Changed = gb_sets:difference(Selectable, Prev),
    New = gb_sets:union(Changed, gb_sets:difference(Prev, Selectable)),
    wings_wm:set_prop(wireframed_objects, New).

all_selectable(St) ->
    F = fun(#{id:=I,perm:=P}, A) when ?IS_SELECTABLE(P) -> [I|A];
           (#{}, A) -> A
        end,
    Selectable = wings_obj:fold(F, [], St),
    gb_sets:from_list(Selectable).

wireframe_sel(false, St) ->
    Prev = wings_wm:get_prop(wireframed_objects),
    Sel = sel_to_set(St),
    New = gb_sets:difference(Prev, Sel),
    wings_wm:set_prop(wireframed_objects, New);
wireframe_sel(true, St) ->
    Prev = wings_wm:get_prop(wireframed_objects),
    Sel = sel_to_set(St),
    New = gb_sets:union(Prev, Sel),
    wings_wm:set_prop(wireframed_objects, New);
wireframe_sel(toggle, St) ->
    W0 = wings_wm:get_prop(wireframed_objects),
    Sel = sel_to_set(St),
    W1 = gb_sets:difference(W0, Sel),
    W = gb_sets:union(W1, gb_sets:difference(Sel, W0)),
    wings_wm:set_prop(wireframed_objects, W).

sel_to_set(St) ->
    Sel = wings_sel:selected_ids(St),
    gb_sets:from_list(Sel).

virtual_mirror_fun(Faces, We0) ->
    case gb_sets:to_list(Faces) of
	[Face] ->
	    We = wings_we:create_mirror(Face, We0),
	    wings_we:mirror_flatten(We, We);
	_ ->
	    wings_u:error_msg(?__(1,"Only a single face must be selected per object."))
    end.

break_mirror(St) ->
    with_mirror_objs(fun wings_we:break_mirror/1, St).

freeze_mirror(St) ->
    with_mirror_objs(fun wings_we:freeze_mirror/1, St).

with_mirror_objs(Update, St0) when is_function(Update, 1) ->
    case sel_mirror_objects(St0) of
        [] ->
            St0;
        [_|_]=Mirrors ->
            St = wings_obj:update(Update, Mirrors, St0),
            {save_state,wings_sel:valid_sel(St)}
    end.

sel_mirror_objects(#st{sel=[]}=St) ->
    MF = fun(#{id:=Id,perm:=P}, #we{mirror=Face})
               when ?IS_SELECTABLE(P), is_integer(Face) ->
                 [Id];
            (_, _) ->
                 []
         end,
    RF = fun erlang:'++'/2,
    wings_obj:dfold(MF, RF, [], St);
sel_mirror_objects(St) ->
    MF = fun(_, #we{mirror=none}) -> [];
            (_, #we{id=Id}) -> [Id]
         end,
    RF = fun erlang:'++'/2,
    wings_sel:dfold(MF, RF, [], St).


-define(RANGE_FOV, {1.0,179.9}).
-define(RANGE_NEAR_CLIP, {0.001,1000.0}).
-define(RANGE_FAR_CLIP, {100.0,infinity}).
-define(RANGE_ZOOM_SLIDER, {-1.0,4.0}).
-define(RANGE_NEGATIVE_SIZE, {1,infinity}).
-define(RANGE_LENS_LENGTH_ZOOM, {0.01,infinity}).

camera(St) ->
    Active = wings_wm:this(),
    View0 = wings_wm:get_prop(Active, current_view),
    NegH = wings_pref:get_value(negative_height),
    NegW = wings_pref:get_value(negative_width),
    #view{fov=Fov0,hither=Hither0,yon=Yon0} = View0,
    NegativeFormat =
	case {NegH,NegW} of
	    {24,36} -> {24,36};
	    {34,60} -> {34,60};
	    {45,60} -> {45,60};
	    {60,60} -> {60,60};
	    _ -> custom
	end,
    Props =
	camera_propconv(from_fov,
			[{negative_height,NegH},{negative_width,NegW},
			 {fov,Fov0}]),
    LensType = pget(lens_type, Props),
    LensLength = pget(lens_length, Props),
    Zoom = pget(zoom, Props),
    ZoomSlider = pget(zoom_slider, Props),
    FovHook = fun(Var,Val,Sto) ->
		    camera_update_1(Var, Val, Sto)
	      end,
    FovHook2 = fun(Var,Val,Sto) -> camera_update_2(Var, Val, Sto) end,
    NegHook = fun(_Var, Val, Sto) ->
		    case Val of
			default ->
			    #view{fov=Fov1,hither=Hither1,yon=Yon1} = DefView = default_view(),
			    Props =
				camera_propconv(from_fov,
						[{negative_height,NegH},{negative_width,NegW},{fov,Fov1}]),
			    gbupdate([{fov, Fov0},{height, Hither1},{yon, Yon1},
				      {lens_type, pget(lens_type, Props)},
				      {lens_length, pget(lens_length, Props)},
				      {zoom, pget(zoom, Props)},
				      {zoom_slider, pget(zoom_slider, Props)},
				      {negative_format,{NegH,NegW}}], Sto),
			    Keys = [negative_height, negative_width],
			    wings_dialog:enable(Keys, false, Sto),
			    wings_dialog:enable(lens_type, true, Sto),
			    wings_wm:set_prop(Active, current_view, DefView);
			_ ->
			    Keys = [negative_height, negative_width],
			    wings_dialog:enable(Keys, Val =:= custom, Sto),
			    wings_dialog:enable(lens_type, Val =/= custom, Sto),
			    FovHook2(lens_type, wings_dialog:get_value(lens_type, Sto), Sto)
		    end
	      end,
    LensFrame =
	{vframe,
	 [{hframe,
	   [{label,?__(1,"Negative Format")},
	    {menu,
	     [{"24x36 [3:2]" ,{24,36}},
	      {"34x60 [16:9]",{34,60}},
	      {"45x60 [4:3]" ,{45,60}},
	      {"60x60 [1:1]" ,{60,60}},
	      {?__(4,"Custom"),custom},
	      {?__(19,"Reset to default"),default}],
	     NegativeFormat,
	     [{key,negative_format}, {hook, NegHook}]},
	    {hframe,
	     [{text, NegH, [{key,negative_height},{range,?RANGE_NEGATIVE_SIZE},
			    {hook,FovHook}]},
	      {label,?__(5,"x")},
	      {text, NegW, [{key,negative_width},{range,?RANGE_NEGATIVE_SIZE},
			    {hook,FovHook}]}]}]},
	  {hframe,
	   [{menu,
	     [{?__(6,"Wide-Angle Lens"),wide_angle},
	      {?__(7,"Moderate Wide-Angle Lens"),moderate_wide_angle},
	      {?__(8,"Standard Lens"),standard},
	      {?__(9,"Portrait Telephoto Lens"),short_tele},
	      {?__(10,"Telephoto Lens"),tele},
	      {?__(11,"Custom Lens"),custom}],
	     LensType,
	     [{key,lens_type}, {hook,FovHook2}]},
	    panel,
	    {label,?__(12,"Length")},
	    {text,LensLength,[{key,lens_length}, {range,?RANGE_LENS_LENGTH_ZOOM}, {hook,FovHook2}]}]},
	  {hframe,
	   [{slider,
	     [{key,zoom_slider},{range,?RANGE_ZOOM_SLIDER},
	      {value,ZoomSlider}, {hook,FovHook2}]},
	    {text, Zoom,[{key,zoom}, {range,?RANGE_LENS_LENGTH_ZOOM}, {hook,FovHook2}]},
	    {label,?__(13,"x Zoom")}]}],
	 [{title,?__(14,"Lens")},{minimized,true}]},
    Qs =
	[LensFrame,
	 {hframe,
	  [{vframe,[{label,?__(15,"Field of View")},
		    {label,?__(16,"Near Clipping Plane")},
		    {label,?__(17,"Far Clipping Plane")}]},
	   {vframe,[{text,Fov0,[{range,?RANGE_FOV},{key,fov},{hook,FovHook}]},
		    {text,Hither0,[{key,height},{range,?RANGE_NEAR_CLIP}]},
		    {text,Yon0,[{key,yon},{range,?RANGE_FAR_CLIP}]}]},
	   {vframe,[help_button(camera_settings_fov),
		    panel,
		    panel]}]}],
    ShowImgPlane = wings_wm:get_prop(show_cam_imageplane),
    wings_wm:set_prop(show_cam_imageplane, true),
    wings_dialog:dialog(?__(18,"Camera Settings"), {preview,Qs},
			fun
			    ({dialog_preview,Res}) ->
				update_camera(Active, View0, Res),
				{preview,St,St};
			    (cancel) ->
				wings_wm:set_prop(Active, show_cam_imageplane, ShowImgPlane),
				wings_wm:set_prop(Active, current_view, View0),
				St;
			    (Res) ->
				update_camera(Active, View0, Res),
				wings_wm:set_prop(Active, show_cam_imageplane, ShowImgPlane),
				{commit,St,St}
			end).

update_camera(Win, View0, Ps) ->
    [{negative_format,_},
     {negative_height,_},{negative_width,_},
     {lens_type,_},{lens_length,_},
     {zoom_slider,_},{zoom,_},
     {fov,Fov},{height,Hither},{yon,Yon}] = Ps,

    {NH,NW} = camera_propconv_negative_format(Ps),
    View = View0#view{fov=Fov,hither=Hither,yon=Yon},
    wings_wm:set_prop(Win, current_view, View),
    wings_pref:set_value(negative_height, NH),
    wings_pref:set_value(negative_width, NW),
    ignore.

camera_update_1(Var, Val, Sto) ->
    Props = gbget(lists:delete(Var, [fov,negative_format,
				     negative_height,negative_width]),
		  Sto),
    gbupdate(camera_propconv(from_fov, [{Var,Val}|Props]),
	     gbupdate(Var, Val, Sto)).

camera_update_2(Var, Val, Sto) ->
    [_|Props0] = Props1 =
	gbget([lens_length,
	       negative_format,negative_height,negative_width], Sto),
    Props2 = camera_propconv(fov, [{Var,Val}|Props1])++Props0,
    Props = camera_propconv(from_fov, Props2)++Props2,
    gbupdate(proplists:delete(Var, Props), gbupdate(Var, Val, Sto)).

camera_propconv(from_fov, Props) ->
    {NegH,NegW} = NegF = camera_propconv_negative_format(Props),
    Fov = pget(fov, Props),
    LensLength =
	case catch 0.5*NegH/math:tan(Fov*math:pi()/360) of
	    {'EXIT',_} -> 0.0;
	    L when is_float(L) -> L
	end,
    LensType =
	camera_lens_type(NegF, LensLength),
    Zoom =
	LensLength / math:sqrt(NegH*NegH + NegW*NegW),
    ZoomSlider =
	case catch math:log(Zoom) / math:log(2) of
	    {'EXIT',_} ->
		{Z,_} = ?RANGE_ZOOM_SLIDER,
		Z;
	    Z when is_float(Z) -> Z
	end,
    [{lens_length,LensLength},
     {lens_type,LensType},
     {zoom,Zoom},
     {zoom_slider,wings_util:limit(ZoomSlider, ?RANGE_ZOOM_SLIDER)}];
camera_propconv(fov, Props) ->
    {NegH,NegW} = NegF = camera_propconv_negative_format(Props),
    Zoom =
	case pget(zoom_slider, Props) of
	    undefined ->
		pget(zoom, Props);
	    ZoomSlider ->
		math:pow(2, ZoomSlider)
	end,
    LensLength =
	case Zoom of
	    undefined ->
		case pget(lens_type, Props) of
		    custom ->
			pget(lens_length, Props);
		    undefined ->
			pget(lens_length, Props);
		    LensType ->
			camera_lens_length(NegF, LensType)
		end;
	    _ ->
		Zoom * math:sqrt(NegH*NegH + NegW*NegW)
	end,
    Fov =
	if is_float(LensLength) ->
		case catch 360*math:atan(0.5*NegH/LensLength)/math:pi() of
		    {'EXIT',_} -> 180.0;
		    F when is_float(F) -> F
		end
	end,
    [{fov,wings_util:limit(Fov, ?RANGE_FOV)}].

camera_propconv_negative_format(Props) ->
    case pget(negative_format, Props) of
	{_,_} = NegativeFormat ->
	    NegativeFormat;
	_ ->
	    {pget(negative_height, Props),
	     pget(negative_width, Props)}
    end.

camera_lens_type({24,36}, LensLength) ->
    case round(LensLength) of
	24 -> wide_angle;
	35 -> moderate_wide_angle;
	50 -> standard;
	85 -> short_tele;
	135 -> tele;
	_ -> custom
    end;
camera_lens_type({34,60}, LensLength) ->
    case round(LensLength) of
	35 -> wide_angle;
	55 -> moderate_wide_angle;
	80 -> standard;
	135 -> short_tele;
	215 -> tele;
	_ -> custom
    end;
camera_lens_type({45,60}, LensLength) ->
    case round(LensLength) of
	40 -> wide_angle;
	60 -> moderate_wide_angle;
	85 -> standard;
	150 -> short_tele;
	235 -> tele;
	_ -> custom
    end;
camera_lens_type({60,60}, LensLength) ->
    case round(LensLength) of
	40 -> wide_angle;
	60 -> moderate_wide_angle;
	80 -> standard;
	150 -> short_tele;
	250 -> tele;
	_ -> custom
    end;
camera_lens_type(_, _) -> custom.

camera_lens_length({24,36}, LensType) ->
    float(case LensType of
	      wide_angle	  -> 24;
	      moderate_wide_angle -> 35;
	      standard		  -> 50;
	      short_tele	  -> 85;
	      tele		  -> 135
	  end);
camera_lens_length({34,60}, LensType) ->
    float(case LensType of
	      wide_angle	  -> 35;
	      moderate_wide_angle -> 55;
	      standard		  -> 80;
	      short_tele	  -> 135;
	      tele		  -> 215
	  end);
camera_lens_length({45,60}, LensType) ->
    float(case LensType of
	      wide_angle	  -> 40;
	      moderate_wide_angle -> 60;
	      standard		  -> 85;
	      short_tele	  -> 150;
	      tele		  -> 250
	  end);
camera_lens_length({60,60}, LensType) ->
    float(case LensType of
	      wide_angle	  -> 40;
	      moderate_wide_angle -> 60;
	      standard		  -> 80;
	      short_tele	  -> 150;
	      tele		  -> 250
	  end);
camera_lens_length(_, _) -> undefined.

gbget([], _Sto) -> [];
gbget([Key|Keys], Sto) -> [{Key,wings_dialog:get_value(Key, Sto)}|gbget(Keys, Sto)].

gbupdate(Key, Val, Sto) -> wings_dialog:set_value(Key, Val, Sto).

gbupdate([], Sto) -> Sto;
gbupdate([{Key,Val}|KVs], Sto) ->
    gbupdate(KVs, gbupdate(Key, Val, Sto)).

pget(Key, Props) -> proplists:get_value(Key, Props).
%%% pget(Key, Props, Default) -> proplists:get_value(Key, Props, Default).



%%%
%%% The Auto Rotate command.
%%%

-record(tim,
	{timer, 				%Current timer.
	 delay, 				%Current delay.
	 st,					%St record.
         start=erlang:monotonic_time(),
         frames=0
	 }).

auto_rotate(St) ->
    auto_rotate_help(),
    Delay = 0, %% Render as fast as possible and sync with gl:finish()
    ?SET(auto_rotate, max(1/120, wings_pref:get_value(auto_rotate_angle)/60)),
    Active = wings_wm:this(),
    {W,H} = wings_wm:win_size(Active),
    X = W div 2, Y = H div 2,
    wings_io:warp(X,Y),
    Tim = #tim{delay=Delay,st=St},
    case wings_pref:get_value(develop_time_commands, false) of
        false -> ok;
        true  -> wings_util:profile_start(fprof)
    end,
    {seq,push,set_auto_rotate_timer(Tim)}.

auto_rotate_event({action, Cmd={view, rotate_left}}, Tim) ->
    auto_rotate_event_1(Cmd, Tim);
auto_rotate_event(Event, #tim{st=St, timer=Timer}=Tim) ->
    case wings_camera:event(Event, St) of
	next -> auto_rotate_event_1(Event, Tim);
	Other ->
            wings_io:cancel_timer(Timer),
            Do = fun(Ev) ->
			 auto_rotate_help(),
			 wings_io:putback_event(Ev),
                         set_auto_rotate_timer(Tim#tim{frames=301})
		 end,
	    {seq, Do, Other}
    end.

auto_rotate_event_1(redraw, Tim) ->
    wings_io:batch(fun() ->
                           gl:finish(), %% Sync all prev commands
                           auto_rotate_redraw(Tim)
                   end),
    keep;
auto_rotate_event_1(#mousemotion{}, _) -> keep;
auto_rotate_event_1(got_focus, _) -> keep;
auto_rotate_event_1(#mousebutton{state=?SDL_PRESSED}, _) -> keep;
auto_rotate_event_1(#keyboard{}=Kb, Tim) ->
    Deg = ?GET(auto_rotate),
    Incr = max(1/120, abs(Deg)*0.1),
    case wings_hotkey:event(Kb) of
	{select,more} ->
            ?SET(auto_rotate, min(45/60,  Deg+Incr)),
            get_event(Tim);
	{select,less} ->
            Deg = ?GET(auto_rotate),
            ?SET(auto_rotate, max(-45/60, Deg-Incr)),
	    get_event(Tim);
        next -> keep;
        {view, toggle_lights} -> toggle_lights(), keep;
        {view, scene_lights} -> toggle_option(scene_lights), keep;
        {view, reset} -> reset(), keep;
        {view, toggle_wireframe} -> toggle_option(workmode), keep;
	_Cmd ->
	    stop_timer(Tim)
    end;
auto_rotate_event_1({view,rotate_left=Cmd}, #tim{st=St}=Tim) ->
    command(Cmd, St),
    wings_wm:dirty(),
    set_auto_rotate_timer(Tim);
auto_rotate_event_1(lost_focus, _) ->
    keep;
auto_rotate_event_1(_Event, Tim) ->
    stop_timer(Tim).

auto_rotate_redraw(#tim{st=Redraw}) when is_function(Redraw) ->
    Redraw();
auto_rotate_redraw(#tim{st=#st{}=St}=Tim) ->
    wings_wm:clear_background(),
    wings_render:render(St),
    wings_io:info(timer_stats(Tim)).

auto_rotate_help() ->
    Msg1 = wings_msg:button_format(?__(1,"Stop rotating")),
    Msg2 = wings_camera:help(),
    P = wings_util:key_format("[+]",
			      ?__(2,"Increase speed")),
    M = wings_util:key_format("[-]",
			      ?__(3,"Decrease speed")),
    Message = wings_msg:join([Msg1,Msg2,P,M]),
    wings_wm:message(Message).

stop_timer(#tim{timer=Timer}) ->
    case wings_pref:get_value(develop_time_commands, false) of
        false -> ok;
        true  -> wings_util:profile_stop(fprof)
    end,
    wings_wm:dirty(),
    wings_io:cancel_timer(Timer),
    pop.

set_auto_rotate_timer(#tim{delay=Delay, frames=Fs}=Tim0) ->
    Timer = wings_io:set_timer(Delay, {action, {view,rotate_left}}),
    Tim = case Fs > 300 of
              true  -> Tim0#tim{timer=Timer, frames=0, start=erlang:monotonic_time()};
              false -> Tim0#tim{timer=Timer, frames=Fs+1}
          end,
    get_event(Tim).

timer_stats(#tim{start=T0, frames=Fs}) ->
    T1 = erlang:monotonic_time(),
    Time = 1+erlang:convert_time_unit(T1-T0, native, millisecond),
    io_lib:format("Rotate: ~.1f deg ~w fps", [?GET(auto_rotate)*60, round(1000.0*Fs/Time)]).

get_event(Tim) ->
    {replace,fun(Ev) -> auto_rotate_event(Ev, Tim) end}.

%%%
%%% Other stuff.
%%%

toggle_option(Key0) ->
    Key = case Key0 of
	      {show, K} -> K;
	      _ -> Key0
	  end,
    case wings_wm:lookup_prop(Key) of
	none ->
	    Prev = wings_pref:get_value(Key, false),
	    wings_pref:set_value(Key, not Prev),
	    wings_wm:send(top_frame, {menu,{view, Key0, not Prev}}),
	    Prev;
	{value,Bool} ->
	    wings_wm:set_prop(Key, not Bool),
	    wings_wm:send(top_frame, {menu,{view, Key0, not Bool}}),
	    Bool
    end.

current() ->
    wings_wm:get_prop(current_view).

set_current(View) ->
    wings_wm:set_prop(current_view, View),
    View.

init() ->
    wings_pref:set_default(show_edges, true),
    wings_pref:set_default(show_backfaces, true),
    wings_pref:set_default(number_of_lights, 1),
    wings_pref:set_default(show_normals, false),
    wings_pref:set_default(show_bb, true),
    wings_pref:set_default(show_bb_center, true),
    wings_pref:set_default(show_colors, true),
    wings_pref:set_default(show_materials, true),
    wings_pref:set_default(show_textures, true),
    wings_pref:set_default(show_normal_maps, true),
    wings_pref:set_default(filter_texture, true),
    wings_pref:set_default(frame_disregards_mirror, false),
    wings_pref:set_default(scene_lights, false),
    Lights0 = wings_pref:get_value(number_of_lights),
    update_menu(toggle_light(Lights0)).

initial_properties() ->
    [{workmode,true},
     {orthogonal_view,false},
     {clip_plane,false},
     {show_axes,true},
     {show_groundplane,true},
     {show_cam_imageplane,false},
     {wireframed_objects,gb_sets:empty()},
     {current_view,default_view()},
     {allow_rotation,true},
     {show_info_text,true},
     {show_wire_backfaces,false}
    ].

delete_all(St) -> St#st{views={0,{}}}.

reset() ->
    reset(current()).

reset(View) ->
    set_current(View#view{origin={0.0,0.0,0.0},
			  azimuth=-45.0,elevation=25.0,
			  distance=?CAMERA_DIST,
			  pan_x=0.0,pan_y=0.0,
			  along_axis=none}).

default_view() ->
    #view{origin={0.0,0.0,0.0},
	  azimuth=-45.0,elevation=25.0,
	  distance=?CAMERA_DIST,
	  pan_x=0.0,pan_y=0.0,
	  along_axis=none,
	  fov=45.0,
	  hither=0.1,
	  yon=10000.0}.

load_matrices(IncludeLights) ->
    gl:matrixMode(?GL_PROJECTION),
    TPM = projection(e3d_transform:identity()),
    {UseSceneLights, TMM} = modelview(IncludeLights),
    {TPM,TMM, UseSceneLights}.

projection() ->
    OP0 = gl:getDoublev(?GL_PROJECTION_MATRIX),
    projection(e3d_transform:init(list_to_tuple(OP0))).

projection(In) ->
    {W,H} = wings_wm:win_size(),
    Aspect = W/H,
    #view{distance=D,fov=Fov,hither=Hither,yon=Yon,along_axis=AA} =
	current(),
    Ortho = wings_wm:get_prop(orthogonal_view)
	orelse ((AA =/= none) andalso
		wings_pref:get_value(force_ortho_along_axis)),
    TP = case Ortho of
	     false ->
		 e3d_transform:perspective(Fov, Aspect, Hither, Yon);
	     true ->
		 Sz = D*math:tan(Fov*math:pi()/180/2),
		 e3d_transform:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, Hither, Yon)
	 end,
    TPM = e3d_transform:mul(In, TP),
    gl:loadMatrixd(e3d_transform:matrix(TPM)),
    TPM.

modelview() ->
    modelview(false).

modelview(IncludeLights) ->
    #view{origin=Origin,distance=Dist,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = current(),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),

    UseSceneLights =
        case IncludeLights of
            true ->
                wings_pref:get_value(scene_lights) andalso
                    wings_light:any_enabled_lights();
            false ->
                false
        end,

    TM0 = e3d_transform:translate(e3d_transform:identity(), {PanX, PanY, -Dist}),
    TM1 = e3d_transform:rotate(TM0, El, {1.0,0.0,0.0}),
    TM2 = e3d_transform:rotate(TM1, Az, {0.0,1.0,0.0}),
    TMM = e3d_transform:translate(TM2, Origin),

    gl:loadMatrixd(e3d_transform:matrix(TMM)),
    {UseSceneLights, TMM}.

%% Calculate the location of the viewer in 3D space.
%% (The (0,0,0) point multiplied by the inverse view transformation matrix.)
eye_point() ->
    InvTrans = view_matrix(current()),
    EP = e3d_mat:mul_point(InvTrans, {0.0,0.0,0.0}),
    %% LP = {0.0,0.5,0.0},
    %% io:format("EP ~p + ~p = ~p~n", [EP, LP, e3d_vec:add(EP, LP)]),
    %% io:format("EP ~p~n", [e3d_mat:mul_point(InvTrans, LP)]),
    EP.

view_matrix(#view{origin=Origin,distance=Dist,azimuth=Az,elevation=El,
		 pan_x=PanX, pan_y=PanY}) ->
    M0 = e3d_mat:translate(e3d_vec:neg(Origin)),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(-Az, {0.0,1.0,0.0})),
    M2 = e3d_mat:mul(M1, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    e3d_mat:mul(M2, e3d_mat:translate(-PanX, -PanY, Dist)).

%%%% Highlight Aim is called via event handler (see wings:do_hotkey/2)
highlight_aim(add, Selmode, Sel0, MM, #st{selmode=body,sel=Sel1}=St) ->
    Sel = lists:subtract(Sel0,Sel1),
    aim(St#st{selmode=Selmode,sel=Sel},MM);
highlight_aim(delete, Selmode, Sel0, MM, #st{selmode=body,sel=Sel1}=St) ->
    Sel = lists:subtract(Sel1,Sel0),
    aim(St#st{selmode=Selmode,sel=Sel},MM);
highlight_aim(add, Selmode, Sel0, MM, #st{sel=Sel1}=St) ->
    Sel = highlighted_element(Sel0, Sel1),
    aim(St#st{selmode=Selmode,sel=Sel},MM);
highlight_aim(delete, Selmode, Sel0, MM, #st{sel=Sel1}=St) ->
    Sel = highlighted_element(Sel1, Sel0),
    aim(St#st{selmode=Selmode,sel=Sel},MM).

highlighted_element([{Id,Sel0}], [{Id,Sel1}]) ->
    [{Id,gb_sets:subtract(Sel0,Sel1)}];
highlighted_element(Sel0, Sel1) ->
    [{Id,_}] = lists:subtract(Sel0,Sel1),
    S0 = get_sel_from_hl_obj(Id, Sel0),
    S1 = get_sel_from_hl_obj(Id, Sel1),
    [{Id,gb_sets:subtract(S0,S1)}].

get_sel_from_hl_obj(Id, Sel) ->
    case orddict:find(Id, Sel) of
      {_,SelSet} -> SelSet;
      error -> gb_sets:empty()
    end.

%%%% Aim Camera
aim(#st{sel=[]}) ->
    View = current(),
    set_current(View#view{origin=e3d_vec:zero()});
aim(St) ->
    Origin0 = wings_sel:center(St),
    Origin = e3d_vec:neg(Origin0),
    #view{distance=Dist0} = View = current(),
    Dist = case e3d_vec:dist(eye_point(), Origin0) of
	       D when D < Dist0 -> D;
	       _Other -> Dist0
	   end,
    set_current(View#view{origin=Origin,distance=Dist,pan_x=0.0,pan_y=0.0}).

aim(#st{sel=[]}, _) ->
    View = current(),
    set_current(View#view{origin=e3d_vec:zero()});
aim(St, {_, _, original}) ->
    aim(St);
aim(St, {Id, _Elem, mirror}) ->
    #view{distance=Dist0} = View = current(),
    EyePoint = eye_point(),
    F = fun(#we{mirror=Face}=We) ->
                MirNormal = wings_face:normal(Face, We),
                Mirror = wings_face:center(Face, We),

                Original = wings_sel:center(St),
                Distance = dist_along_vector(Mirror, Original, MirNormal),
                Origin0 = e3d_vec:add_prod(Original, MirNormal, Distance * 2),
                Origin = e3d_vec:neg(Origin0),

                Dist = min(e3d_vec:dist(EyePoint, Origin0), Dist0),

                {Origin,Dist}
        end,
    {Origin,Dist} = wings_obj:with_we(F, Id, St),
    set_current(View#view{origin=Origin,distance=Dist,pan_x=0.0,pan_y=0.0}).

dist_along_vector(PosA,PosB,Vector) ->
    %% Return Distance between PosA and PosB along Vector
    {Xa,Ya,Za} = PosA,
    {Xb,Yb,Zb} = PosB,
    {Vx,Vy,Vz} = e3d_vec:norm(Vector),
    Vx*(Xa-Xb)+Vy*(Ya-Yb)+Vz*(Za-Zb).

frame(#st{sel=[]}=St) ->
    MF = fun(#{perm:=P}, #we{vp=Vtab}=We) when ?IS_VISIBLE(P) ->
                 case wings_util:array_is_empty(Vtab) of
                     false -> wings_vertex:bounding_box(We);
                     true -> none
                 end;
            (_, _) -> none
         end,
    RF = fun(none, none) ->
                 none;
            ([_|_]=BB, none) ->
                 BB;
            (none, [_|_]=BB) ->
                 BB;
            ([_|_]=BB1, [_|_]=BB0) ->
                 e3d_vec:bounding_box(BB1 ++ BB0)
         end,
    BB = wings_obj:dfold(MF, RF, none, St),
    frame_1(BB);
frame(St0) ->
    St = case wings_pref:get_value(frame_disregards_mirror) of
             true ->
                 kill_mirror(St0);
             false ->
                 St0
         end,
    frame_1(wings_sel:bounding_box(St)).

frame_1(none) -> ok;
frame_1([A,B]) ->
    C = e3d_vec:average(A, B),
    #view{fov=Fov, hither=Hither} = View = current(),
    R = e3d_vec:len(e3d_vec:sub(A, B)) / 2,
    Dist = max(R/math:tan(Fov*math:pi()/2/180),Hither),
    set_current(View#view{origin=e3d_vec:neg(C),
			  distance=Dist,pan_x=0.0,pan_y=0.0}).

kill_mirror(St) ->
    wings_obj:we_map(fun wings_we:break_mirror/1, St).

views({save,[Legend]}, #st{views={_,{}}}=St0) ->
    St = St0#st{views={1,{{current(),Legend}}},saved=false},
    wings_u:caption(St);
views({save,[Legend]}, #st{views={CurrentView,Views}}=St0) ->
    J = view_index(CurrentView, tuple_size(Views)),
    {L1,L2} = lists:split(J, tuple_to_list(Views)),
    St = St0#st{views={J+1,list_to_tuple(L1++[{current(),Legend}|L2])},
		saved=false},
    wings_u:caption(St);
views({save,Ask}, #st{views={CurrentView,Views}}) when is_atom(Ask) ->
    View = current(),
    S = tuple_size(Views),
    if S > 0 ->
	    case element(view_index(CurrentView, S), Views) of
		{View,_} ->
		    wings_u:message(?__(1,"This view is already the current"));
		_ ->
		    views_save_dialog(Ask, [view_legend(View)])
	    end;
       true ->
	    views_save_dialog(Ask, [view_legend(View)])
    end;
views(_, #st{views={_,{}}}) ->
    wings_u:message(?__(2,"No saved views"));
views(next, #st{views={CurrentView,Views}}=St) ->
    J = view_index(CurrentView + 1, tuple_size(Views)),
    {View,_} = element(J, Views),
    set_current(View),
    St#st{views={J,Views}};
views(current, #st{views={CurrentView,Views}}) ->
    {View,_} = element(view_index(CurrentView, tuple_size(Views)), Views),
    set_current(View),
    wings_wm:dirty();
views(prev, #st{views={CurrentView,Views}}=St) ->
    J = view_index(CurrentView - 1, tuple_size(Views)),
    {View,_} = element(J, Views),
    set_current(View),
    St#st{views={J,Views}};
views({jump,J}, #st{views={CurrentView,Views}}=St) ->
    views_jump(J, St, CurrentView, Views);
views({move,J}, #st{views={CurrentView,Views}}=St) ->
    views_move(J, St, CurrentView, Views);
views({rename,Idx}, #st{views={_,Views}}=St) ->
    views(rename, St#st{views={Idx,Views}});
views(rename, #st{views={CurrentView,Views}}=St) ->
    J = view_index(CurrentView, tuple_size(Views)),
    {View,Legend} = element(J, Views),
    wings_dialog:dialog(?__(3,"Rename view"),
			views_rename_qs([Legend]),
			fun([[]]) -> ignore;
			   ([NewLegend]) ->
				St#st{views={CurrentView,
					     setelement(J, Views, {View,NewLegend})}}
			end);
views(delete, #st{views={CurrentView,Views}}=St) ->
    View = current(),
    J = view_index(CurrentView, tuple_size(Views)),
    case element(J, Views) of
	{View,_} ->
	    I = J - 1,
	    {L1,[_|L2]} = lists:split(I, tuple_to_list(Views)),
	    St#st{views={I,list_to_tuple(L1++L2)}};
	_ ->
	    wings_u:message(?__(4,"You have to be at the current view"))
    end;
views(delete_all, St) ->
    This = wings_wm:this(),
    wings_u:yes_no(
      ?__(5,"Are you sure you want to delete all saved views?"),
      fun() ->
	      wings_wm:send(This, {new_state,delete_all(St)}),
	      ignore
      end).

views_save_dialog(Ask, Options) ->
    wings_dialog:dialog(Ask, ?__(1,"Save view as"),
			views_rename_qs(Options),
			fun(Opts) -> {view,{views,{save,Opts}}} end).

views_rename_qs([Legend]) ->
    [{hframe,[{label,?__(1,"Name")},{text,Legend,[{width,22}]}]}].

views_jump(J, St, CurrentView, Views) ->
    S = tuple_size(Views),
    case {view_index(J, S),view_index(CurrentView, S)} of
	{J,J} ->
	    {View,_} = element(J, Views),
	    set_current(View),
	    wings_wm:dirty(),
	    St;
	{J,_} ->
	    {View,_} = element(J, Views),
	    set_current(View),
	    St#st{views={J,Views}};
	_ ->
	    wings_u:message(?__(1,"No such view slot"))
    end.

views_move(J, St, CurrentView, Views) ->
    S = tuple_size(Views),
    case {view_index(J, S),view_index(CurrentView, S)} of
	{J,J} ->
	    {View,_} = element(J, Views),
	    set_current(View),
	    wings_wm:dirty();
	{J,C} when J < C ->
	    {V1,[{View,_}=VL|V2]} = lists:split(C-1, tuple_to_list(Views)),
	    {V3,V4} = lists:split(J-1, V1),
	    set_current(View),
	    St#st{views={J,list_to_tuple(V3++([VL|V4]++V2))}};
	{J,C} ->
	    {V1,[{View,_}=VL|V2]} = lists:split(C-1, tuple_to_list(Views)),
	    {V3,V4} = lists:split(J-C, V2),
	    set_current(View),
	    St#st{views={J,list_to_tuple(V1++(V3++[VL|V4]))}};
	_ ->
	    wings_u:message(?__(1,"No such view [")++integer_to_list(J)++"]")
    end.

toggle_lights() ->
    %% Invalidate displaylists so that shader data get set correctly
    %% for materials
    case wings_pref:get_value(scene_lights, true) of
        false -> ok;
        true -> toggle_option(scene_lights)
    end,
    Lights0 = wings_pref:get_value(number_of_lights),
    update_menu(Lights0),
    wings_wm:send(top_frame, {menu,{view, number_of_lights, toggle_light(Lights0)}}),
    wings_pref:set_value(number_of_lights, toggle_light(Lights0)).

toggle_light(Lights0) ->
    1 + (2 + Lights0) rem 2.

update_menu(Lights0) ->
    wings_menu:update_menu(view, toggle_lights,
			   one_of(Lights0 == 1,
				  ?__(2,"Use Camera Light"),
				  ?__(1,"Use Hemisphere Light")),
			   one_of(Lights0 == 1,
				  ?__(4,"Use a camera work light"),
				  ?__(3,"Use a simple sky light simulation"))).

along(x) -> along(x, -90.0, 0.0);
along(y) -> along(y, 0.0, 90.0);
along(z) -> along(z, 0.0, 0.0);
along(neg_x) -> along(x, 90.0, 0.0);
along(neg_y) -> along(y, 0.0, -90.0);
along(neg_z) -> along(z, 180.0, 0.0);
along(nearest_axis) ->
  %% Set view to nearest cardinal axis and keep the orientation of the model.
    #view{azimuth=Az0,elevation=El0} = View = current(),
    [Az,El] = foldl(fun(D0, Acc) ->
        D1 = round(D0),
        D2 = D1 rem 180,
        Rot = D1 div 180 rem 2,
        [round_to_cardinal(Rot, D2)|Acc]
    end, [], [El0,Az0]),
    Along = along(Az, El),
    set_current(View#view{azimuth=Az,elevation=El,along_axis=Along}).

round_to_cardinal(Rot, Deg) when Deg < -135 ->
    if Rot =:= 0 -> 180.0; true -> 0.0 end;
round_to_cardinal(Rot, Deg) when Deg < -45 ->
    if Rot =:= 0 -> -90.0; true -> 90.0 end;
round_to_cardinal(Rot, Deg) when Deg =< 45 ->
    if Rot =:= 0 -> 0.0; true -> 180.0 end;
round_to_cardinal(Rot, Deg) when Deg =< 135 ->
    if Rot =:= 0 -> 90.0; true -> -90.0 end;
round_to_cardinal(Rot, _) ->
    if Rot =:= 0 -> 180.0; true -> 0.0 end.

along(Along, Az, El) ->
    View = current(),
    set_current(View#view{azimuth=Az,elevation=El,along_axis=Along}).

along(Az, El) when Az =:= 90.0; Az =:= -90.0 ->
    case El of
        +0.0 -> x;
        90.0 -> y;
        -90.0 -> y;
        180.0 -> x;
        _ -> none
    end;
along(Az, El) when El =:= 90.0; El =:= -90.0 ->
    case Az of
        +0.0 -> y;
        90.0 -> x;
        -90.0 -> x;
        180.0 -> y;
        _ -> none
    end;
along(Az, El) when Az =:= +0.0; Az =:= 180.0 ->
    case El of
        +0.0 -> z;
        180.0 -> z;
        _ -> none
    end;
along(_, _) -> none.

align_to_selection(#st{sel=[]}=St) -> St;
align_to_selection(#st{selmode=vertex}=St) ->
    N = average_normals(
          fun(Vs, We) ->
		  [wings_vertex:normal(V, We) || V <- Vs]
	  end, St),
    align_view_to_selection(N),
    St;
align_to_selection(#st{selmode=edge}=St) ->
    N = average_normals(
	  fun(Edges, #we{es=Etab}=We) ->
		  flatmap(fun(Edge) ->
                                  #edge{lf=Lf,rf=Rf} = array:get(Edge, Etab),
                                  [wings_face:normal(Lf, We),
                                   wings_face:normal(Rf, We)]
                          end, Edges)
	  end, St),
    align_view_to_selection(N),
    St;
align_to_selection(#st{selmode=face}=St) ->
    N = average_normals(
	  fun(Faces, We) ->
                  [wings_face:normal(Face, We) || Face <- Faces]
	  end, St),
    align_view_to_selection(N),
    St;
align_to_selection(St) -> St.

align_view_to_selection(N) ->
    {Az,El} = align_view_to_normal(N),
    View = current(),
    set_current(View#view{azimuth=Az,elevation=El}).

average_normals(CalcNormals, St) ->
    MF = fun(Items, We) ->
                 Ns = CalcNormals(gb_sets:to_list(Items), We),
                 e3d_vec:add(Ns)
         end,
    RF = fun e3d_vec:add/2,
    Ns = wings_sel:dfold(MF, RF, e3d_vec:zero(), St),
    e3d_vec:norm(Ns).

align_view_to_normal({Nx,Ny,Nz}) ->
    Z = {0.0,0.0,1.0},
    Az0 = e3d_vec:dot(e3d_vec:norm({Nx,0.0,Nz}), Z),
    Az1 = to_degrees(math:acos(Az0)),
    Az = if
	     Nx < 0 -> Az1;
	     true -> -Az1
	 end,
    El = to_degrees(math:asin(Ny)),
	{Az,El}.

to_degrees(A) when is_float(A) ->
    A*180.0/math:pi().

one_of(true, S, _) -> S;
one_of(false,_, S) -> S.

%%%
%%% Export and import of views.
%%%

export_views(#st{views={_,Views0}}) ->
    Views1 = tuple_to_list(Views0),
    Views = case get(wings_not_running) of
		undefined ->
		    CurrentView = current(),
		    case get_view_index(CurrentView,Views1) of
			undefined ->
			    [{CurrentView,"current_view"}]++Views1;
			Idx ->
			    View = element(Idx,Views0),
			    Views2 = Views1 -- [View],  % tuple index starts from 1 and lists from 0
			    [View]++Views2
		    end;
		_ ->
		    Views1
	    end,
    export_views_1(Views).

export_views_1([{View,Name}|Views]) ->
    Tags = [aim,distance_to_aim,azimuth,elevation,tracking,fov,hither,yon],
    Props = [{name,Name}|zip(Tags, camera_info(Tags, View))],
    [{view,Props}|export_views_1(Views)];
export_views_1([]) -> [].

import_views(Views, #st{views={CurrentView0,{}}}=St) -> % loading a project
    NewViews0 = import_views_1(Views),
    NewViews = case NewViews0 of
		   [] ->
		       reset(),
		       CurrentView = CurrentView0,
		       NewViews0;
		   _ ->
		       {{View,_},NewViews1} = remove_cur_view(NewViews0),
		       (get(wings_not_running) == undefined) andalso set_current(View),
		       CurrentView = case get_view_index(View,NewViews1) of
					 undefined -> CurrentView0;
					 Idx -> Idx
				     end,
		       NewViews1
	       end,
    St#st{views={CurrentView,list_to_tuple(NewViews)}};
import_views(Views, #st{views={CurrentView,OldViews}}=St) -> % merging a project
    NewViews0 = import_views_1(Views),
    {{_,_},NewViews1} = remove_cur_view(NewViews0),
    NewViews = tuple_to_list(OldViews)++NewViews1,
    St#st{views={CurrentView,list_to_tuple(NewViews)}}.

import_views_1([{view,As}|Views]) ->
    [import_view(As)|import_views_1(Views)];
import_views_1([]) -> [].

import_view(As) ->
    import_view(As, #view{}, undefined).

import_view([{aim,Aim}|As], View, Name) ->
    import_view(As, View#view{origin=Aim}, Name);
import_view([{distance_to_aim,Dist}|As], View, Name) ->
    import_view(As, View#view{distance=Dist}, Name);
import_view([{azimuth,Az}|As], View, Name) ->
    import_view(As, View#view{azimuth=Az}, Name);
import_view([{elevation,El}|As], View, Name) ->
    import_view(As, View#view{elevation=El}, Name);
import_view([{tracking,{X,Y}}|As], View, Name) ->
    import_view(As, View#view{pan_x=X,pan_y=Y}, Name);
import_view([{fov,Fov}|As], View, Name) ->
    import_view(As, View#view{fov=Fov}, Name);
import_view([{hither,Hither}|As], View, Name) ->
    import_view(As, View#view{hither=Hither}, Name);
import_view([{yon,Yon}|As], View, Name) ->
    import_view(As, View#view{yon=Yon}, Name);
import_view([{name,Name}|As], View, _) ->
    import_view(As, View, Name);
import_view([], #view{azimuth=Az,elevation=El}=View, undefined) ->
    {View#view{along_axis=along(Az, El)},view_legend(View)};
import_view([], #view{azimuth=Az,elevation=El}=View, Name) ->
    {View#view{along_axis=along(Az, El)},Name}.


%%%
%%% import/export utilities
%%%

get_view_index(#view{}=View, Views) ->
    get_view_index_1(View, Views, 0).
get_view_index_1(#view{}, [], _) -> undefined;
get_view_index_1(#view{}=View, [{View,_Name}|_], Acc) -> Acc+1;
get_view_index_1(#view{}=View, [_|Views], Acc) ->
    get_view_index_1(View,Views,Acc+1).

remove_cur_view([View|_]=Views0) ->
    Views = case View of
        {_, "current_view"} ->
            Views0 -- [View];
        _ ->
            Views0
    end,
    {View,Views}.

%%%
%%% Camera info.
%%%

camera_info([aim|As], #view{origin=Aim}=View) ->
    [Aim|camera_info(As, View)];
camera_info([distance_to_aim|As], #view{distance=Dist}=View) ->
    [Dist|camera_info(As, View)];
camera_info([azimuth|As], #view{azimuth=Az}=View) ->
    [Az|camera_info(As, View)];
camera_info([elevation|As], #view{elevation=El}=View) ->
    [El|camera_info(As, View)];
camera_info([tracking|As], #view{pan_x=X,pan_y=Y}=View) ->
    [{X,Y}|camera_info(As, View)];
camera_info([fov|As], #view{fov=Fov}=View) ->
    %% Field of view.
    [Fov|camera_info(As, View)];
camera_info([hither|As], #view{hither=Hither}=View) ->
    %% Near clipping plane.
    [Hither|camera_info(As, View)];
camera_info([yon|As], #view{yon=Yon}=View) ->
    %% Far clipping plane.
    [Yon|camera_info(As, View)];
camera_info([pos_dir_up|As], View) ->
    [camera_pos_dir_up(View)|camera_info(As, View)];
camera_info([], _) -> [].

camera_pos_dir_up(#view{distance=Dist}=View) ->
    M = view_matrix(View),
    Pos = e3d_mat:mul_point(M, {0.0,0.0,0.0}),
    Aim = e3d_mat:mul_point(M, {0.0,0.0,-Dist}),
    Above = e3d_mat:mul_point(M, {0.0,1.0,0.0}),
    Dir = e3d_vec:sub(Aim, Pos),
    Up = e3d_vec:sub(Above, Pos),
    %% Pos is position of camera.
    %% Dir is the vector from Pos to the Aim point.
    %% Up points up from Pos and is normalized, so Pos+Up is a point 1.0
    %% above the camera, with the cameras notion of above.
    %% Up is also orthogonal to Dir, so Up x Dir = Left.
    {Pos,Dir,Up}.

view_legend(#view{distance=Dist,along_axis=Along}=View) ->
    [{Pos,Dir,_}] = camera_info([pos_dir_up], View),
    From = ?__(from, "From"),
    DistStr = ?__(distance, "distance"),
    AlongStr = case Along of
		   x -> ?__(2,"along +X");
		   y -> ?__(3,"along +Y");
		   z -> ?__(4,"along +Z");
		   neg_x -> ?__(5,"along -X");
		   neg_y -> ?__(6,"along -Y");
		   neg_z -> ?__(7,"along -Z");
		   none ->
		       Aim = e3d_vec:add(Pos, Dir),
		       AimLen = e3d_vec:len(Aim),
		       if AimLen < Dist*0.000001 ->
			       %% Close enough to Origin
			       ?__(8,"towards Origin");
			  true ->
			       ?__(9,"towards ")++pos_legend(e3d_vec:neg(Dir))
		       end
	       end,
    %% io_lib:format/2 cannot be used here since ~s doesn't accept
    %% characters > 255.
    Legend = [From," ",pos_legend(Pos)," ",AlongStr,
	      add_dist_str(DistStr),io_lib:format(" ~.4g", [Dist])],
    lists:flatten(Legend).

%% Don't add a space if the string is empty (Italian).
add_dist_str("") -> "";
add_dist_str(S) -> [$\s|S].

pos_legend({X,Y,Z}) ->
    %% Sort the coordinates as greatest absolute value first.
    %% Remove the ones with absolute value lower then tan(45/2) times
    %% the maximum. This should use the same style as N, NW, W type
    %% compass direction notation where each value has
    %% a precision of +-45/2 degrees.
    %% Return e.g "+X+Y", "-Z+X" depending on the sign of the remaining
    %% coordinates, largest first, as a deep charlist.
    [{Max,_,_}|_] = L =
	    lists:reverse(lists:sort(
			    [{abs(X),$X,X},{abs(Y),$Y,Y},{abs(Z),$Z,Z}])),
    [[if Val < 0 -> $-;
	 true	 -> $+ end,Char]
     || {Abs,Char,Val} <- L, Abs >= Max*0.4142135624]. % tan(22.5 degrees)

help_button(Subject) ->
    Title = help(title, Subject),
    TextFun = fun () -> help(text, Subject) end,
    {help,Title,TextFun}.

help(title, camera_settings_fov) ->
    ?__(1,"Camera Settings: Field of View");
help(text, camera_settings_fov) ->
    [?__(2,"Sets the vertical field of view, i.e. the angle from lower to upper border seen from the camera. "),
     ?__(3,"The Lens controls sets the field of view according to other perhaps more well-known entities, at least for a photographer. The negative size is stored among the user preferences."),
     ?__(4,"The Lens controls make the height of the picture right for the chosen lens, so if you do not have the same aspect ratio for the negative size in the Lens frame, as for the size of the Geometry window, the width will be in error."),
     ?__(5,"Note: the zoom factor is relative a standard lens length of the negative diagonal, while only the negative height affects the effective lens length.")].
