%%
%%  wpc_dxf_m.erl --
%%
%%     DXF multipurpose import/export
%%
%%  Copyright (c) 2023-2024 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
%%  The DXF importer can import 2D drawings and 3D meshes.
%%

-module(wpc_dxf_m).

-export([init/0,menu/2,command/2]).
-ifdef(TEST).
-export([t11/0,t12/0,t13/0,t14/0,t15/0,t16/0]).
-export([t/0, t_solids/0, t2/0, t3/0, t4/0, t5/0]).
-export([t0/0]).
-endif().

-import(lists, [map/2,foldl/3,keydelete/3,keyreplace/4,sort/1]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

init() ->
    wpa:pref_set_default(?MODULE, ignore_border, false),
    wpa:pref_set_default(?MODULE, ignore_border_top, "1"),
    wpa:pref_set_default(?MODULE, ignore_border_bottom, "1"),
    wpa:pref_set_default(?MODULE, ignore_border_left, "1"),
    wpa:pref_set_default(?MODULE, ignore_border_right, "1"),
    true.

menu({file,import}, Menu) ->
    menu_entry(Menu);
menu({file,export}, Menu) ->
    menu_entry_exp(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry_exp(Menu);
menu(_, Menu) -> Menu.

menu_entry(Menu) ->
    Menu ++ [{?__(1,"DXF Outlines / Mesh (.dxf)..."),dxf_multipurpose,[option]}].

menu_entry_exp(Menu) ->
    Menu ++ [{?__(1,"DXF Mesh (.dxf)..."),dxf_multipurpose,[option]}].


command({file,{import,{dxf_multipurpose,Ask}}}, St) ->
    do_import(Ask, St);
command({file,{export,{dxf_multipurpose,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{dxf_multipurpose,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) ->
    next.

%%%
%%% Export.
%%%

export_info() ->
    ?__(1,"This exporter is intended to export to software that expect\n"
    "DXF files. If you are exporting a model with full colors and textures to a\n"
    "modeler or game engine, you might want to try glTF, COLLADA, or OBJ first.").

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    ModelUnit  = wpa:pref_get(?MODULE, model_units, cm),
    DXFUnit    = wpa:pref_get(?MODULE, dxf_units, mm),
    Dialog = [
        {label_column, [
            {?__(11,"Model units (for Wings3D Unit):"),
                {menu, units(), ModelUnit, [{key,model_units}]} },
            {?__(12,"DXF units:"),
                {menu,
                    [{?__(14, "Same as above"),same}] ++ units(),
                    DXFUnit, [{key,dxf_units}]} }
        ]}
    ] ++ common_mesh_options(export) ++ [
        {label, export_info()}
    ],
    wpa:dialog(Ask, ?__(1,"DXF Mesh Export Options"), Dialog,
           fun(Res) ->
               {file,{Op,{dxf_multipurpose,Res}}}
           end);
do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    
    Ps = [{include_uvs,true},
          {tesselation, quadrangulate},
          {include_hard_edges, true},
      {subdivisions,SubDivs}|export_props()],
    Exporter(Ps, export_fun(Attr)).
export_props() ->
    [{extensions,[
        {".dxf",?__(1,"DXF Mesh File")}]}].


export_transform(E3dFile, KeyVals) ->
    Mat = wpa:export_matrix(KeyVals),
    e3d_file:transform(E3dFile, Mat).

export_fun(Attr) ->
    fun(Filename, Contents_0) ->
        set_pref(Attr),

        ModelUnits = proplists:get_value(model_units, Attr, cm),
        DXFUnits = proplists:get_value(dxf_units, Attr, mm),

        #e3d_file{objs=ObjsW,creator=Creator}=
            mat_to_col(export_transform(Contents_0, Attr)),
        Units = units_tuple(ModelUnits, DXFUnits),
        ScaleF = scale_from_units(export, Units),
        ObjsW_1 = scale_objects(ObjsW, ScaleF),
        write_dxf_file(Filename, DXFUnits, [obj_to_dxfobj(O1) || O1 <- ObjsW_1], Creator),
        ok
    end.


units_tuple(ModelUnits, same) ->
    units_tuple(ModelUnits, ModelUnits);
units_tuple(ModelUnits, DXFUnits) ->
    {ModelUnits, DXFUnits}.


units() ->
    [{?__(1, "Meters"),meter},
     {?__(2, "Decimeters"),dm},
     {?__(3, "Centimeters"),cm},
     {?__(4, "Millimeters"),mm},
     {?__(5, "Microns"),micron},
     {?__(6, "Yards"),yd},
     {?__(7, "Feet"),ft},
     {?__(8, "Inches"),in},

     {?__(9, "Gigameter"),gm},
     {?__(10, "Kilometer"),km},
     {?__(11, "Hectometer"),hectometer},
     {?__(12, "Decameter"),decameter},
     {?__(13, "Nanometer"),nm},
     {?__(14, "Angstrom"),ang},

     {?__(15, "Mile"),mi},
     {?__(16, "Thou (Mils)"),thou},
     {?__(17, "Micro-inch"),microinch},

     {?__(20, "Parsec"),parsec},
     {?__(18, "Light year"),ly},
     {?__(19, "Astronomical Unit"),au}].



%%%
%%% Import.
%%%

more_info() ->
    [?__(1,
        "This DXF multipurpose importer can import both 2D and 3D objects:\n\n"
        "1. If the DXF uses closed LWPOLYLINE or POLYLINE, the importer "
        "will turn the closed path into a mesh.\n\n"
        "2. If the DXF contains LINE, ARC, and other non-path entities, "
        "the importer will need to do more processing to turn the linework into "
        "a flat mesh by determining polygons from a loose set of lines. This "
        "means complicated linework might take quite a bit of time to fully calculate "
        "line intersections and path traversals. If possible, try re-importing "
        "the linework into a CAD and combine the loose lines into a closed path "
        "POLYLINE for better results.\n\n"
        "3. 2D SOLID entities are turned into individual meshes.\n\n"
        "4. 3D objects using either 3DFACE or POLYLINE vertice list meshes "
        "are imported directly.\n\n"
        "Objects in DXFs can sometimes be facing 'Z up', check the <b>Swap Y and Z axes</b> "
        "setting to flip axes if required.\n\n")].

info_button() ->
    Title = ?__(1,"DXF Multipurpose Import Information"),
    TextFun = fun () -> more_info() end,
    {help,Title,TextFun}.

common_mesh_options(Type) ->
    T = wpa:dialog_template(?MODULE, Type, [
        tesselation,
        include_normals,
        include_colors,
        include_uvs,
        default_filetype]),
    T_1 = common_mesh_options_remove_filetype(T),
    [T_1].
common_mesh_options_remove_filetype({vframe, L}) ->
    L_1 = common_mesh_options_remove_filetype_2(lists:reverse(L)),
    {vframe, lists:reverse(L_1)}.
common_mesh_options_remove_filetype_2([panel|L]) ->
    common_mesh_options_remove_filetype_2(L);
common_mesh_options_remove_filetype_2([{vframe, [{menu,_,_,PL}],_}|L]=L1) ->
    case proplists:get_value(key, PL) of
        default_filetype ->
            common_mesh_options_remove_filetype_2(L);
        _ ->
            L1
    end;
common_mesh_options_remove_filetype_2(L) ->
    L.


do_import(Ask, _St) when is_atom(Ask) ->
    ModelUnit  = wpa:pref_get(?MODULE, model_units, cm),
    LinesMode = wpa:pref_get(?MODULE, linesmode, fill),
    CenterShape = wpa:pref_get(?MODULE, center_shape, false),
    
    Dialog = [
        {label_column, [
            {?__(11,"Model units (for Wings3D Unit):"),
                {menu, units(), ModelUnit, [{key,model_units}]} },
            {?__(12,"DXF units:"),
                {menu,
                    [{?__(13, "Detect"),detect}] ++ units(),
                    detect, [{key,dxf_units}]} }
        ]},
        {label, ?__(14,
                "Sometimes DXF files might not contain units information.\n"
                "Selecting a unit will override any unit specified in the DXF.")}
      ] ++ common_mesh_options(import) ++ [
       panel,
       {vframe,
        [{hframe,[
            {label,?__(7,"Method:")},
            {menu, [{?__(8,"Ignore"),ignore},
                    {?__(9,"Fill"),fill}], LinesMode, [{key,linesmode}]} ]}
        ],
        [{title,?__(6,"2D Elements")}]},
       {?__(17,"Center Shape"),CenterShape,[{key,center_shape}]},
       panel,
       {label,?__(5,
            "This importer can import both 2D and 3D objects.\n"
            "Refer to DXF importer notes.")},
       info_button()
       ],
    wpa:dialog(Ask, ?__(1,"DXF Multipurpose Import Options"), Dialog,
           fun(Res) ->
               {file,{import,{dxf_multipurpose,Res}}}
           end);
do_import(Attr, St) ->
    wpa:import(import_props(), import_fun(Attr), St).
import_props() ->
    [{extensions,[
        {".dxf",?__(1,"DXF Plotter or Mesh File")},
        {".dxb",?__(2,"DXF Simple Binary File")}]}].

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

import_transform(E3dFile, KeyVals) ->
    Mat = wpa:import_matrix(KeyVals),
    e3d_file:transform(E3dFile, Mat).


-record(dxfc, {
    commands = [] :: [any()],
    blocks = [],
    params = []
}).


import_fun(Attr) ->
    fun(Filename) ->
        set_pref(Attr),
        LinesMode = proplists:get_value(linesmode, Attr, 1.0),
        CenterShape = proplists:get_value(center_shape, Attr, false),

        ModelUnits = proplists:get_value(model_units, Attr, cm),
        DXFUnit_0 = proplists:get_value(dxf_units, Attr, cm),

        {ok, Cont_0} = read_dxf_file(Filename),
        DXFUnit = case DXFUnit_0 of
            detect ->
                get_file_units(Cont_0);
            SpecUnit ->
                SpecUnit
        end,

        #dxfc{commands=CommandsList} = expand_inserts(Cont_0),
        
        Scale = {1.0, 1.0, 1.0},
        Commands_0 = rescale(Scale, adjust_commands(CommandsList)),

        Scale_Contours = 1.0,
        
        %% Turn the different kind of objects into meshes
        Commands_1 = face_commands_to_objects(Commands_0),
        Commands_2 = mesh_sequences_to_objects(Commands_1),
        Commands_3 = solid_commands_to_objects(Commands_2, Scale_Contours),
        Objs_0 = contours_to_objects(Commands_3, Scale_Contours, LinesMode),
        
        Objs_1 = [center_obj(O, CenterShape) || {obj, O} <- Objs_0],

        ScaleF = scale_from_units(import, units_tuple(ModelUnits, DXFUnit)),
        Objs_2 = scale_objects(Objs_1, ScaleF),
        
        {ok, import_transform(#e3d_file{objs=Objs_2}, Attr)}
    end.
    
center_obj(Obj, false) ->
    Obj;
center_obj(#e3d_object{obj=#e3d_mesh{vs=Vs0}=Mesh}=Obj, true) ->
    Center = e3d_vec:average(e3d_vec:bounding_box(Vs0)),
    Vec = e3d_vec:sub(e3d_vec:zero(),Center),
    Vs = [ e3d_vec:add(V,Vec) || V <- Vs0],
    Obj#e3d_object{obj=Mesh#e3d_mesh{vs=Vs}}.


%%
%%  DXF Export
%%

obj_to_dxfobj(#e3d_object{obj=Mesh,name=ObjName}) ->
    #e3d_mesh{vs=Vs,vc=Vc,ns=Ns}=Mesh,
    VNC = {array:from_list(Vs), array:from_list(Ns), array:from_list(Vc)},
    wr_vlist_for_objs(VNC, ObjName, [Mesh]).

%%%
%%%

-record(v, {
    p,
    ns,
    co
}).
-record(f, {
    v
}).
-record(o, {
    f,
    layer
}).


write_dxf(Fp, L) ->
    OL1 = write_dxf(Fp, L, []),
    ok = file:write(Fp, OL1).
write_dxf(Fp, [{A,B}|L], OL)
  when is_list(A), is_list(B) ->
    Str = io_lib:format("~s~s~s~s", [
        string:pad(A, 3, leading),
        [13,10],
        B,
        [13,10]]),
    write_dxf(Fp, L, [Str|OL]);
write_dxf(Fp, [L0|L], OL)
  when is_list(L0) ->
    OL1 = write_dxf(Fp, L0, []),
    write_dxf(Fp, L, [OL1|OL]);
write_dxf(_Fp, [], OL) ->
    lists:reverse(OL).



wr_face_pnt(PA1,PA2,PA3,{X,Y,Z}) ->
    [
        {PA1, float_to_list(X, [{decimals,6},compact])},
        {PA2, float_to_list(Y, [{decimals,6},compact])},
        {PA3, float_to_list(Z, [{decimals,6},compact])}
    ].

wr_face(LayerName, Verts) ->
    
    {Point1,Point2,Point3,Point4,Co1} =
        case Verts of
            [#v{p={X1_0,Y1_0,Z1_0},co=Co1_0},
             #v{p={X2_0,Y2_0,Z2_0}},
             #v{p={X3_0,Y3_0,Z3_0}},
             #v{p={X4_0,Y4_0,Z4_0}}] ->
                { {X1_0,Y1_0,Z1_0},
                  {X2_0,Y2_0,Z2_0},
                  {X3_0,Y3_0,Z3_0},
                  {X4_0,Y4_0,Z4_0},Co1_0 };

            [#v{p={X1_0,Y1_0,Z1_0},co=Co1_0},
             #v{p={X2_0,Y2_0,Z2_0}},
             #v{p={X3_0,Y3_0,Z3_0}}] ->
                { {X1_0,Y1_0,Z1_0},
                  {X2_0,Y2_0,Z2_0},
                  {X3_0,Y3_0,Z3_0},
                  {X3_0,Y3_0,Z3_0},Co1_0 }
        end,
    %% Face
    [
        {"0", "3DFACE"},
        {"8", LayerName},
        {"62", integer_to_list(wr_fcol(Co1))},
        wr_face_pnt("10", "20", "30", Point1),
        wr_face_pnt("11", "21", "31", Point2),
        wr_face_pnt("12", "22", "32", Point3),
        wr_face_pnt("13", "23", "33", Point4)
    ].

wr_fcol(none) ->
    0;
wr_fcol({R,G,B}) ->
    rgb_to_coi(R, G, B).


write_dxf_header(Fp, Creator, L) ->
    ExportedFrom = lists:flatten(io_lib:format(
        "Exported from ~s (~p)", [Creator, ?MODULE])),
    PairsL = write_dxf_header_1(L),
    write_dxf(Fp, [
        {"999", ExportedFrom}
    ]),
    write_dxf_section(Fp, "HEADER", PairsL).
    

write_dxf_header_1(L) ->
    write_dxf_header_1(L, []).
write_dxf_header_1([{name, Name, C}|L], OL) ->
    OL1 = [[ {"9", Name}, {"1", C} ] | OL],
    write_dxf_header_1(L, OL1);
write_dxf_header_1([{coord, Name, X, Y, Z}|L], OL) ->
    OL1 = [[ {"9", Name},
        {"10", float_to_list(X, [{decimals,6},compact])},
        {"20", float_to_list(Y, [{decimals,6},compact])},
        {"30", float_to_list(Z, [{decimals,6},compact])}
     ] | OL],
    write_dxf_header_1(L, OL1);
write_dxf_header_1([{{int, Which}, Name, Num}|L], OL) ->
    OL1 = [[ {"9", Name},
        {integer_to_list(Which), integer_to_list(Num)}
     ] | OL],
    write_dxf_header_1(L, OL1);
write_dxf_header_1([], OL) ->
    lists:append(lists:reverse(OL)).


write_dxf_section(Fp, Str, L) ->
    write_dxf(Fp, [
        {"0", "SECTION"},
        {"2", Str},
        L,
        {"0", "ENDSEC"}
    ]).

wr_table(L) ->
    [
        {"0", "TABLE"},
        {"2", "LAYER"},
        L,
        {"0", "ENDTAB"}
    ].



write_dxf_file(Flname, DXFUnits, Objs, Creator) ->
    {{MinX,MaxX},{MinY,MaxY},{MinZ,MaxZ}} = wr_coord_range(Objs),
    {ok, Fp} = file:open(Flname, [write]),
    
    write_dxf_header(Fp, Creator, [
        {name, "$ACADVER", "AC1006"},
        {coord, "$INSBASE", 0.0, 0.0, 0.0},
        {coord, "$EXTMIN", MinX, MinY, MinZ},
        {coord, "$EXTMAX", MaxX, MaxY, MaxZ},
        {{int, 70}, "$INSUNITS", dxf_unit_int(DXFUnits)},
        {{int, 70}, "$MEASUREMENT", dxf_drw_unit_int(DXFUnits)}
    ]),
    
    write_dxf_section(Fp, "TABLES",
        wr_table([
            {"70", integer_to_list(length(Objs)+1)},
            [
                [
                    {"0", "LAYER"},
                    {"2", Name},
                    {"70", "0"},
                    {"62", "7"},
                    {"6", "CONTINUOUS"}
                ]
            || #o{layer=Name} <- Objs]
        ])),

    write_dxf_section(Fp, "BLOCKS", [
    ]),
    
    write_dxf_section(Fp, "ENTITIES",
        [
            [
                wr_face(Name, Verts)
            || #f{v=Verts} <- ObjFL ]
        || #o{f=ObjFL,layer=Name} <- Objs]),
    
    write_dxf(Fp, [{"0", "EOF"}]),
    
    file:close(Fp).


wr_vlist_for_objs(VNC, ObjName, BList) ->
    wr_vlist_for_objs(VNC, ObjName, BList, [], []).
wr_vlist_for_objs(_, ObjName, [], OMesh, VList) ->
    A1 = lists:append(lists:reverse(OMesh)),
    VList1 = [{B,A} || {A,B} <- VList],
    Sorted = lists:sort(VList1),
    A2 = [AC || {_,AC} <- Sorted],
    
    A1_1 = [{f, wr_vlist_1(L, A2)} || {f, L} <- A1],
    #o{f=A1_1,layer=to_layer_name(ObjName)};
wr_vlist_for_objs(VNC, ObjName, [#e3d_mesh{fs=Faces}=_|BList_1]=_, OMesh, VList) ->
    {Face_2, VList_2} = wr_vlist_for_face(VNC, Faces, VList),
    wr_vlist_for_objs(VNC, ObjName, BList_1, [Face_2|OMesh], VList_2).
    
wr_vlist_1(L, VA) ->
    VA_1 = array:from_list(VA),
    wr_vlist_1(L, VA_1, []).
wr_vlist_1([], _, OL) ->
    lists:reverse(OL);
wr_vlist_1([A1|L], VA, OL) ->
    V1 = array:get(A1, VA),
    wr_vlist_1(L, VA, [V1|OL]).


wr_vlist_for_face(VNC, Faces, VList) -> %% ??
    wr_vlist_for_face(VNC, Faces, [], VList).
wr_vlist_for_face(_, [], OEdges, VList) ->
    { lists:reverse(OEdges), VList};
wr_vlist_for_face(VNC, [#e3d_face{vs=Edges_V,ns=Edges_N,vc=Edges_C}|Faces_1], OEdges, VList) ->
    {AL_2, VList_2} = wr_vlist_for_edges_1(VNC, Edges_V, Edges_N, Edges_C, [], VList),
    wr_vlist_for_face(VNC, Faces_1, [ #f{v=AL_2} | OEdges], VList_2).


wr_vlist_for_edges_1(_, [], _E_N, _E_C, OEdge, VList) ->
    { lists:reverse(OEdge), VList };
wr_vlist_for_edges_1({AVs,ANs,ACs}=VNC, [Idx|E_V_1]=_E_V, E_N, E_C, OEdge, VList) ->
    Coord = lref(Idx, AVs),
    case E_N of
        [] -> 
            Normal = {0.0, 0.0, 0.0},
            E_N2 = [];
        [N|E_N2] ->
            Normal = lref(N, ANs)
    end,
    case E_C of
        [] ->
            Color = {0.7, 0.7, 0.7},
            E_C2 = [];
        [C|E_C2] ->
            Color = lref(C, ACs)
    end,
    Vert = #v{p=Coord, ns=Normal, co=Color},
    {Idx2, VList_2} = wr_vlist_index(VList, Vert),
    wr_vlist_for_edges_1(
        VNC, E_V_1, E_N2, E_C2, [Idx2|OEdge], VList_2).

lref(N, L) ->
    array:get(N, L).

wr_vlist_index(List, NewVert) ->
    case orddict:find(NewVert, List) of
        error ->
            NewIdx = length(List),
            {NewIdx, [{NewVert, NewIdx} | List]};
        {ok, Found_1} ->
            Idx = Found_1,
            {Idx, List}
    end.


wr_coord_range(Objs0) ->
    Objs1 = lists:append([OL || #o{f=OL} <- Objs0]),
    wr_coord_range(Objs1, {{0.0,0.0},{0.0,0.0},{0.0,0.0}}).
wr_coord_range([#f{v=Verts}|R], {RX,RY,RZ}) ->
    wr_coord_range(R, wr_coord_range_1(Verts, RX,RY,RZ));
wr_coord_range([], Range) ->
    Range.
wr_coord_range_1([#v{p={X,Y,Z}}|R], {MinX,MaxX},{MinY,MaxY},{MinZ,MaxZ}) ->
    wr_coord_range_1(R,
        wr_coord_range_2(X,MinX,MaxX),
        wr_coord_range_2(Y,MinY,MaxY),
        wr_coord_range_2(Z,MinZ,MaxZ));
wr_coord_range_1([], RX,RY,RZ) ->
    {RX,RY,RZ}.
wr_coord_range_2(X,MinX,MaxX)
  when X < MinX ->
    {X, MaxX};
wr_coord_range_2(X,MinX,MaxX)
  when X > MaxX ->
    {MinX, X};
wr_coord_range_2(_,MinX,MaxX) ->
    {MinX, MaxX}.

    
to_layer_name(L) ->
    to_layer_name(L, []).
to_layer_name([C|L], OL)
  when C >= $0, C =< $9;
       C >= $A, C =< $Z;
       C >= $a, C =< $z ->
    to_layer_name(L, [C|OL]);
to_layer_name([_|L], OL) ->
    to_layer_name(L, OL);
to_layer_name([], []) ->
    Unique = integer_to_list(abs(erlang:unique_integer())),
    string:slice(lists:reverse(Unique), 0, 12);
to_layer_name([], OL) ->
    lists:reverse(OL).


%%
%% DXF Colors
%%

rgb_to_coi(R, G, B)
  when abs(R - G) < 0.1,
       abs(G - B) < 0.1,
       abs(B - R) < 0.1 ->
    rgb_to_coi_gray((R + G + B) / 3.0);
rgb_to_coi(R, G, B)
  when R >= G, R >= B ->
    De = R - min(G,B),
    rgb_to_coi_1(R, De / R, ((G - B) / De) + 0.0);
rgb_to_coi(R, G, B)
  when G >= R, G >= B ->
    De = G - min(B,R),
    rgb_to_coi_1(G, De / G, ((B - R) / De) + 2.0);
rgb_to_coi(R, G, B)
  when B >= R, B >= G ->
    De = B - min(R,G),
    rgb_to_coi_1(B, De / B, ((R - G) / De) + 4.0);
rgb_to_coi(R, G, B) ->
    rgb_to_coi_gray((R + G + B) / 3.0).
rgb_to_coi_1(Peak, S, H)
  when H < 0.0 ->
    rgb_to_coi_1(Peak, S, H + 6.0);
rgb_to_coi_1(Peak, S, H)
  when S > 0.85, Peak > 0.85 ->
    %% Choose among the bright colors
    round(H + 1);
rgb_to_coi_1(Peak, S, H)
  when S < 0.5 ->
    MH = (round((H * 4.0)) * 10 + 10) + 1,
    MH + (5 - round(Peak * 5)) * 2;
rgb_to_coi_1(Peak, _, H) ->
    MH = (round((H * 4.0)) * 10 + 10),
    MH + (5 - round(Peak * 5)) * 2.
rgb_to_coi_gray(A) when A >= 0.05, A < 0.15 -> 7;
rgb_to_coi_gray(A) when A >= 0.25, A < 0.35 -> 8;
rgb_to_coi_gray(A) when A >= 0.45, A < 0.55 -> 9;
rgb_to_coi_gray(A) when A =< 0.2 -> 0;
rgb_to_coi_gray(A) when A > 0.2, A =< 1.0 ->
    round(A * 6) + 249;
rgb_to_coi_gray(_) ->
    0.

coi_to_rgb(Idx) when Idx < 10 ->
    case Idx of
        0 -> {0.0, 0.0, 0.0};
        1 -> {1.0, 0.0, 0.0};
        2 -> {1.0, 1.0, 0.0};
        3 -> {0.0, 1.0, 0.0};
        4 -> {0.0, 1.0, 1.0};
        5 -> {0.0, 0.0, 1.0};
        6 -> {1.0, 0.0, 1.0};
        7 -> {0.1, 0.1, 0.1};
        8 -> {0.3, 0.3, 0.3};
        9 -> {0.5, 0.5, 0.5}
    end;
coi_to_rgb(Idx) when Idx >= 250 ->
    A = (Idx - 249) / 6.0,
    {A, A, A};
coi_to_rgb(Idx) ->
    H0 = floor(Idx / 10),
    B = H0 * 10,
    H = (H0-1) / 4.0,
    SB = Idx rem 2,
    Val = (5 - ((Idx - B - SB) div 2)) / 5.0,
    
    Y = if
        H < 2.0 -> Val * (1 - abs(H - 1.0));
        H < 4.0 -> Val * (1 - abs(H - 2.0 - 1.0));
        true    -> Val * (1 - abs(H - 4.0 - 1.0))
    end,
    SB1 = if
        SB =:= 1 -> 0.6 * Y;
        true -> 0.0
    end,
    if
        H < 1.0 -> {Val, Y, SB1};
        H < 2.0 -> {Y, Val, SB1};
        H < 3.0 -> {SB1, Val, Y};
        H < 4.0 -> {SB1, Y, Val};
        H < 5.0 -> {Y, SB1, Val};
        true    -> {Val, SB1, Y}
    end.

%%%
%%%
%%%

%% About units: One DXF unit is one Wings Unit

%% LINE, CIRCLE, ARC are treated
%% as 2D linework drawing elements in this importer.
%% 

-define(D_LINE, 1).
-define(D_CIRCLE, 3).
-define(D_ARC, 4).

%% Depending on mesh flag, POLYLINE, VERTEX and SEQEND, can be
%% either 2D linework or 3D mesh
%%

-define(D_POLYLINE, 14).
-define(D_VERTEX, 15).
-define(D_SEQEND, 16).
-define(D_LWPOLYLINE, 19).

-define(D_SOLID, 6).      %% Weld common points and extruded surface
-define(D_3DFACE, 17).    %% Weld common points and make E3D objects
-define(D_NONE, 0).

-define(D_BLOCK, 9).      %% 
-define(D_INSERT, 11).    %% 
-define(D_VIEWPORT, 18).  %%

%% No plans to implement
-define(D_TRACE, 5).      %% ??
-define(D_SHAPE, 8).      %% 

-record(block_info, {
    base = {0.0, 0.0, 0.0} :: {float(), float(), float()},
    flags = false
}).

get_file_units(Cont) ->
    #dxfc{params=Hdr} = Cont,
    case orddict:find(units, Hdr) of
        {ok, UnitsH} ->
            {ok, Units} = orddict:find({integer_value,70}, UnitsH),
            dxf_units(Units);
        _ ->
            mm
    end.


read_dxf_file(FileName) ->
    case open_dxf_file(FileName) of
        {ok, DXF} ->
            % See if Binary
            
            {0, Section} = next_grouppair(DXF),
            case Section of
                <<"SECTION">> ->
                    Cont = read_dxf_file_header(DXF),
                    read_dxf_sections(DXF, Cont);
                _ ->
                    {ok, [eof]}
            end;
        {dxb, DXB} ->
            read_dxb_records(DXB);
        error -> {error, unspecfied}
    end.

read_dxf_file_header(DXF) ->
    {2, SectionName} = next_grouppair(DXF),
    case SectionName of
        <<"HEADER">> ->
            %% A header was found.
            read_dxf_file_header(DXF, [{hdr, none}]);
        _ ->
            %% This file did not have a header.
            read_unknown_section(DXF),
            #dxfc{}
    end.

read_dxf_file_header(DXF, Params) ->
    read_dxf_file_header(DXF, Params, []).
read_dxf_file_header(DXF, Params, HdrVals) ->
    case next_grouppair(DXF) of
        {0, <<"ENDSEC">>} ->
            #dxfc{params=
                orddict:store(
                    orddict:fetch(hdr, Params),
                    Params, HdrVals)};
        {GroupCode, GroupValue}
          when GroupCode =/= 9 ->
            read_dxf_file_header(DXF,
                orddict:store(groupcode(GroupCode), GroupValue, Params),
                HdrVals);
        {9, GroupValue} ->
            HdrName = case GroupValue of
                <<"$ACADVER">>  -> acadver;
                <<"$EXTMIN">>   -> extmin;
                <<"$EXTMAX">>   -> extmax;
                <<"$INSUNITS">> -> units;
                _ ->
                    none
            end,
            read_dxf_file_header(DXF,
                [{hdr, HdrName}],
                orddict:store(
                    orddict:fetch(hdr, Params),
                    Params, HdrVals));
        _ ->
            read_dxf_file_header(DXF, Params, HdrVals)
    end.

read_dxf_sections(DXF, Cont) ->
    {0, Section} = next_grouppair(DXF),
    case Section of
        <<"SECTION">> -> read_dxf_section(DXF, [], Cont);
        <<"EOF">>     -> {ok, [eof]};
        error         -> {error, unspecified};
        {error, Err}  -> {error, Err}
    end.

read_dxf_section(DXF, CommandsList, Cont) ->
    {2, SectionName} = next_grouppair(DXF),
    case SectionName of
    
        %% Entities is where the object drawings is
        <<"ENTITIES">> ->
            CEntities = read_entities_section_loop(DXF),
            CommandsList1 = CommandsList ++ CEntities,
            Cont1 = Cont;
        
        %% Ignore the tables section
        <<"TABLES">> ->
            CommandsList1 = CommandsList,
            Cont1 = Cont,
            read_tables_section(DXF);
        
        %% Blocks section
        <<"BLOCKS">> ->
            Blocks = read_blocks_section(DXF),
            CommandsList1 = CommandsList,
            Cont1 = Cont#dxfc{blocks=orddict:from_list(Blocks)};
        
        _ ->
            CommandsList1 = CommandsList,
            Cont1 = Cont,
            read_unknown_section(DXF)
    end,
    {0, Section} = next_grouppair(DXF),
    case Section of
        <<"SECTION">> -> read_dxf_section(DXF, CommandsList1, Cont1);
        <<"EOF">>     -> {ok, Cont1#dxfc{commands=CommandsList1}};
        _             -> {ok, Cont1#dxfc{commands=CommandsList1}}
    end.

read_unknown_section(DXF) ->
    case next_grouppair(DXF) of
        {0, <<"ENDSEC">>} -> done;
        _                 -> read_unknown_section(DXF)
    end.

read_tables_section(DXF) ->
    %% Ignore the tables section
    case next_grouppair(DXF) of
        {0, <<"ENDSEC">>} -> done;
        _                 -> read_tables_section(DXF)
    end.

read_blocks_section(DXF) ->
    read_blocks_section(DXF, []).
read_blocks_section(DXF, OL) ->
    case next_grouppair(DXF) of
        {0, <<"ENDSEC">>} ->
            lists:reverse(OL);
        {0, <<"BLOCK">>} ->
            CEntities = read_entities_section_loop(DXF, ?D_BLOCK),
            {BlockName, BlockInfo} = to_block_entry(CEntities),
            read_blocks_section(DXF, [{BlockName, BlockInfo}|OL]);
        _                 ->
            read_blocks_section(DXF)
    end.

read_entities_section_loop(DXF) ->
    read_entities_section_loop(DXF, none).
read_entities_section_loop(DXF, Which) ->
    read_entities_section_loop(DXF, Which, [], fun add_param/3, [], false).
read_entities_section_loop(DXF, Which, Params, AddParamF, CEntities, MeshMode) ->
    case next_grouppair(DXF) of
        {0, ReturnMarker}
          when ReturnMarker =:= <<"ENDSEC">>;
               ReturnMarker =:= <<"ENDBLK">>  ->
            lists:reverse(
                case Which of
                    none -> CEntities;
                    _ ->
                        NewCEnt = read_entities_section_set_ent(Which, Params, MeshMode),
                        [NewCEnt|CEntities]
                end);
        {0, WhichEnt} ->
            CEnt2 = case Which of
                none ->
                    MeshMode_1 = MeshMode,
                    CEntities;
                _ ->
                    NewCEnt = read_entities_section_set_ent(Which, Params, MeshMode),
                    case NewCEnt of
                        {line_seq_start, _} -> MeshMode_1 = false;
                        {mesh_start, _} -> MeshMode_1 = true;
                        _ -> MeshMode_1 = MeshMode
                    end,
                    [NewCEnt|CEntities]
            end,
            {NewWhich, NewParams, AddParamF_1} = case WhichEnt of
                %% Linework Elements
                <<"LINE">> ->       {?D_LINE, [], fun add_param/3};
                <<"CIRCLE">> ->     {?D_CIRCLE, [], fun add_param/3};
                <<"ARC">> ->        {?D_ARC, [], fun add_param/3};
                
                %% Linework or Mesh Elements
                <<"POLYLINE">> ->   {?D_POLYLINE , [], fun add_param/3}; %% POLYLINE goes with VERTEX and SEQEND
                <<"VERTEX">> ->     {?D_VERTEX, [], fun add_param/3};
                <<"SEQEND">> ->     {?D_SEQEND, [], fun add_param/3};
                <<"LWPOLYLINE">> -> {?D_LWPOLYLINE , [], fun add_param_lw/3};
                
                %% 2D Solids
                <<"SOLID">> ->      {?D_SOLID, [], fun add_param/3};
                
                %% 3D Quads
                <<"3DFACE">> ->     {?D_3DFACE, [], fun add_param/3};
                
                <<"TRACE">> ->      {?D_TRACE, [], fun add_param/3};
                <<"SHAPE">> ->      {?D_SHAPE, [], fun add_param/3};
                
                <<"INSERT">> ->     {?D_INSERT, [], fun add_param/3};
                <<"VIEWPORT">> ->   {?D_VIEWPORT, [], fun add_param/3};
                
                _              ->   {?D_NONE, [], fun add_param/3}
            end,
            read_entities_section_loop(DXF, NewWhich,
                NewParams, AddParamF_1, CEnt2, MeshMode_1);
        {GroupCode, GroupValue} ->
            read_entities_section_loop(DXF, Which,
                AddParamF(GroupCode, GroupValue, Params),
                AddParamF, CEntities, MeshMode)
    end.
    
add_param(GroupCode, GroupValue, Params) ->
    orddict:store(groupcode(GroupCode), GroupValue, Params).

%% For LWPOLYLINE
add_param_lw(GroupCode, GroupValue, Params) ->
    C = groupcode(GroupCode),
    case C of
        _ when C =:= x; C =:= y; C =:= z ->
            V1 = GroupValue,
            C_1 = {lwsequ, C},
            case orddict:find({lwsequ, C}, Params) of
                {ok, V_0} -> V_0;
                _ -> V_0 = []
            end,
            V = [V1|V_0];
        _ ->
            C_1 = C,
            V = GroupValue
    end,
    orddict:store(C_1, V, Params).
    
to_block_entry([{block_start, {Name, B}} | CEntities]) ->
    {Name, {B, CEntities}}.

groupcode(N) ->
    case N of
        0 -> start_entity;
        1 -> primary_text_value;
        2 -> name; %% Attribute tag, Block name, etc.
        3 -> {name, 3};
        4 -> {name, 4};
        5 -> entity_handle_hexstring;
        6 -> line_type_name;
        7 -> text_style_name;
        8 -> layer_name;
        9 -> variable_name_identifier; % used only in HEADER
        10 -> x;
        _  when N >= 11 andalso N =< 18  -> {x, N};
        20 -> y;
        _  when N >= 21 andalso N =< 28  -> {y, N};
        30 -> z;
        _  when N >= 31 andalso N =< 37  -> {z, N};
        38 -> z; % elevation
        39 -> thickness;
        _  when N >= 40 andalso N =< 48  -> {float_value, N};
        49 -> repeated_value;
        _  when N >= 50 andalso N =< 58  -> {angle, N};
        62 -> color_number;
        66 -> entities_follow;
        67 -> which_space;
        68 -> viewport_stat;
        69 -> viewport_number;
        _  when N >= 70 andalso N =< 78  -> {integer_value, N};
        210 -> x_extrude;
        220 -> y_extrude;
        230 -> z_extrude;
        999 -> comment;
        1000 -> {extended_entity, string};
        1001 -> {extended_entity, registered_app_name};
        1002 -> {extended_entity, data_control};
        1003 -> {extended_entity, layer_name};
        1004 -> {extended_entity, bytes};
        1005 -> {extended_entity, db_handle};
        1010 -> {extended_entity, x};
        1020 -> {extended_entity, y};
        1030 -> {extended_entity, z};
        1011 -> {extended_entity, x, position};
        1021 -> {extended_entity, y, position};
        1031 -> {extended_entity, z, position};
        1012 -> {extended_entity, x, displacement};
        1022 -> {extended_entity, y, displacement};
        1032 -> {extended_entity, z, displacement};
        1013 -> {extended_entity, x, direction};
        1023 -> {extended_entity, y, direction};
        1033 -> {extended_entity, z, direction};
        1040 -> {extended_entity, float_value};
        1041 -> {extended_entity, distance_value};
        1042 -> {extended_entity, scale_factor};
        1070 -> {extended_entity, integer_value};
        1071 -> {extended_entity, integer_value};

        100 -> {name, 100};
        _ -> unknown
    end.
    
read_entities_section_set_ent(Which, Params, MeshMode) ->
    case Which of
        %% Linework elements
        ?D_LINE -> dxf_set_ent_line(Params);
        ?D_CIRCLE -> dxf_set_ent_circle(Params);
        ?D_ARC -> dxf_set_ent_arc(Params);
        
        %% Linework or Mesh elements
        ?D_POLYLINE -> dxf_set_ent_polyline(Params);
        ?D_VERTEX -> dxf_set_ent_vertex(Params, MeshMode);
        ?D_SEQEND -> dxf_set_ent_seqend(Params, MeshMode);
        ?D_LWPOLYLINE -> dxf_set_ent_lwpolyline(Params);
        
        %% 2D Solids
        ?D_SOLID -> dxf_set_ent_solid(Params);
        
        %% 3D Solids
        ?D_3DFACE -> dxf_set_ent_3dface(Params);
        
        %% Unimplemented
        ?D_TRACE -> dxf_set_ent_trace(Params);
        ?D_SHAPE -> dxf_set_ent_shape(Params);
        
        ?D_INSERT -> dxf_set_ent_insert(Params);
        ?D_VIEWPORT -> dxf_set_ent_viewport(Params);
        ?D_BLOCK -> dxf_set_ent_block(Params);
        
        _ -> unused
    end.
    
merge_with_defaults(L1, L2) ->
    orddict:merge(fun (_, A1, _A2) -> A1 end, L1, L2).
orddict_zero_vals(L) ->
    lists:map(fun(A) -> 
        {A, case A of
            %% Strings
            start_entity -> <<"">>;
            primary_text_value -> <<"">>;
            name -> <<"">>;
            {name, 3} -> <<"">>;
            {name, 4} -> <<"">>;
            entity_handle_hexstring -> <<"">>;
            line_type_name -> <<"">>;
            text_style_name -> <<"">>;
            layer_name -> <<"">>;
            variable_name_identifier -> <<"">>;
            %% Float
            x -> 0.0;
            {x, _N} -> 0.0;
            y -> 0.0;
            {y, _N} -> 0.0;
            z -> 0.0;
            {z, _N} -> 0.0;
            thickness -> 0.0;
            {float_value, _N} -> 0.0;
            repeated_value -> 0.0;
            {angle, _N} -> 0.0;
            %% Integer
            color_number -> 0;
            entities_follow -> 0;
            which_space -> 0;
            viewport_stat -> 0;
            viewport_number -> 0;
            {integer_value, _N} -> 0;
            %% Float
            x_extrude -> 0.0;
            y_extrude -> 0.0;
            z_extrude -> 0.0;
            comment -> 0.0;
            %% String
            {extended_entity, string} -> <<"">>;
            {extended_entity, registered_app_name} -> <<"">>;
            {extended_entity, data_control} -> <<"">>;
            {extended_entity, layer_name} -> <<"">>;
            {extended_entity, bytes} -> <<"">>;
            {extended_entity, db_handle} -> <<"">>;
            %% Float
            {extended_entity, x} -> 0.0;
            {extended_entity, y} -> 0.0;
            {extended_entity, z} -> 0.0;
            {extended_entity, x, position} -> 0.0;
            {extended_entity, y, position} -> 0.0;
            {extended_entity, z, position} -> 0.0;
            {extended_entity, x, displacement} -> 0.0;
            {extended_entity, y, displacement} -> 0.0;
            {extended_entity, z, displacement} -> 0.0;
            {extended_entity, x, direction} -> 0.0;
            {extended_entity, y, direction} -> 0.0;
            {extended_entity, z, direction} -> 0.0;
            {extended_entity, float_value} -> 0.0;
            {extended_entity, distance_value} -> 0.0;
            {extended_entity, scale_factor} -> 0.0;
            %% Integer
            {extended_entity, integer_value} -> 0;
            _ -> 0
        end} end, lists:sort(L)).
orddict_one_vals(L) ->
    lists:map(fun(A) -> 
        {A, case A of
            {float_value, _N} -> 1.0;
            {integer_value, _N} -> 1;
            _ -> 1
        end} end, lists:sort(L)).

 %% Line entity
dxf_set_ent_line(Params0) ->
    Params = merge_with_defaults(Params0, orddict_zero_vals([
        x, y, z, {x, 11}, {y, 21}, {z, 31}])),
    % LINE
    % start point
    {ok, X1} = orddict:find(x, Params),
    {ok, Y1} = orddict:find(y, Params),
    {ok, _Z1} = orddict:find(z, Params),
    % endpoint
    {ok, X2} = orddict:find({x, 11}, Params),
    {ok, Y2} = orddict:find({y, 21}, Params),
    {ok, _Z2} = orddict:find({z, 31}, Params),
    Lines = [
        {{float(X1),float(Y1)},{float(X2),float(Y2)}}
    ],
    {lines, Lines}.


%% Generate Arc paths
%%
arc_into_lines(X1, Y1, Radius) ->
    arc_into_lines(X1, Y1, Radius, 0.0, 360.0).
arc_into_lines(X1, Y1, Radius, StartAngle_0, EndAngle_0) ->
    AngFromDeg = (math:pi() / 180.0),
    StartAngle = StartAngle_0 * AngFromDeg,
    EndAngle = EndAngle_0 * AngFromDeg,
    Arc = arc_open(X1 - Radius, Y1 - Radius,
        X1 + Radius, Y1 + Radius, StartAngle, EndAngle, []),
    seq_to_lines(Arc).
overAngle(Ang1, Ang2)
  when Ang2 < Ang1 ->
    overAngle(Ang1, Ang2 + (2.0 * math:pi()));
overAngle(_   , Ang2) -> Ang2.
arc_open(X1, Y1, X2, Y2, StartAngle, EndAngle_0, StartList)
  when StartAngle =:= EndAngle_0 ->
    arc_open(X1, Y1, X2, Y2, StartAngle, EndAngle_0 + math:pi() * 2.0, StartList);
arc_open(X1, Y1, X2, Y2, StartAngle, EndAngle_0, StartList) ->
    EndAngle = overAngle(StartAngle, EndAngle_0),
    WR = (X2 - X1) / 2.0,
    HR = (Y2 - Y1) / 2.0,
    XC = X1 + WR,
    YC = Y1 + HR,
    AngleDiff = EndAngle - StartAngle,
    NumPoints = round(abs(AngleDiff / math:pi() * 10)),
    arc_open({XC, YC}, 0, NumPoints, StartAngle, AngleDiff, WR, HR, StartList).
arc_open({XC, YC}, I, NumPoints, StartAngle, AngleDiff, WR, HR, OPath)
  when I =:= NumPoints ->
    lists:reverse([arcto_cos_sin({XC, YC}, StartAngle+AngleDiff, WR, HR) | OPath]);
arc_open({XC, YC}=Coord, I, NumPoints, StartAngle, AngleDiff, WR, HR, OPath)
  when I < NumPoints ->
    Ang = StartAngle + AngleDiff * (float(I) / NumPoints),
    OPath_1 = [arcto_cos_sin({XC, YC}, Ang, WR, HR) | OPath],
    arc_open(Coord, I+1, NumPoints, StartAngle, AngleDiff, WR, HR, OPath_1).
arcto_cos_sin({XC, YC}, Ang, WR, HR) ->
    X1 = XC + math:cos(Ang) * WR,
    Y1 = YC - math:sin(Ang) * HR,
    {X1, Y1}.


%% Circle entity
%%
dxf_set_ent_circle(Params0) ->
    Params = merge_with_defaults(Params0, orddict_zero_vals([
        x, y, z, {float_value, 40}])),
    % CIRCLE 
    % center
    {ok, X1} = orddict:find(x, Params),
    {ok, Y1} = orddict:find(y, Params),
    {ok, _Z1} = orddict:find(z, Params),
    % radius
    {ok, Radius} = orddict:find({float_value, 40}, Params),
    {lines, arc_into_lines(float(X1), float(Y1), float(Radius))}.

%% Arc entity
%%
dxf_set_ent_arc(Params0) ->
    Params = merge_with_defaults(Params0, orddict_zero_vals([
        x, y, z, {float_value, 40}, {angle, 50}, {angle, 51}])),
    % ARC
    % center
    {ok, X1} = orddict:find(x, Params),
    {ok, Y1} = orddict:find(y, Params),
    {ok, _Z1} = orddict:find(z, Params),
    % radius
    {ok, Radius} = orddict:find({float_value, 40}, Params),
    % start angle
    {ok, StartAngle} = orddict:find({angle, 50}, Params),
    % end angle
    {ok, EndAngle} = orddict:find({angle, 51}, Params),
    {lines, arc_into_lines(float(X1), float(Y1), float(Radius),
                           float(StartAngle), float(EndAngle))}.

%% Treat the same as SOLID (?)
dxf_set_ent_trace(Params0) ->
    Params = merge_with_defaults(Params0, orddict_zero_vals([
        x, y, z, {x, 11}, {y, 21}, {z, 31},
        {x, 12},  {y, 22}, {z, 32}, {x, 13}, {y, 23}, {z, 33}])),
    % TRACE
    % Four points defining the corners of the trace.
    {ok, X1} = orddict:find(x, Params),
    {ok, Y1} = orddict:find(y, Params),
    {ok, _Z1} = orddict:find(z, Params),
    {ok, X2} = orddict:find({x, 11}, Params),
    {ok, Y2} = orddict:find({y, 21}, Params),
    {ok, _Z2} = orddict:find({z, 31}, Params),
    {ok, X3} = orddict:find({x, 12}, Params),
    {ok, Y3} = orddict:find({y, 22}, Params),
    {ok, _Z3} = orddict:find({z, 32}, Params),
    {ok, X4} = orddict:find({x, 13}, Params),
    {ok, Y4} = orddict:find({y, 23}, Params),
    {ok, _Z4} = orddict:find({z, 33}, Params),
    Points = [
        {float(X1),float(Y1)},
        {float(X2),float(Y2)},
        {float(X3),float(Y3)},
        {float(X4),float(Y4)}
    ],
    {solid, remove_adj_duplicate_points(Points)}.

%% SOLID entity
%%
dxf_set_ent_solid(Params0) ->
    Params = merge_with_defaults(Params0, orddict_zero_vals([
        x, y, z, {x, 11}, {y, 21}, {z, 31},
        {x, 12}, {y, 22}, {z, 32}, {x, 13}, {y, 23}, {z, 33}])),
    % SOLID 
    % Four points defining the corners of the solid
    {ok, X1} = orddict:find(x, Params),
    {ok, Y1} = orddict:find(y, Params),
    {ok, _Z1} = orddict:find(z, Params),
    {ok, X2} = orddict:find({x, 11}, Params),
    {ok, Y2} = orddict:find({y, 21}, Params),
    {ok, _Z2} = orddict:find({z, 31}, Params),
    {ok, X3} = orddict:find({x, 12}, Params),
    {ok, Y3} = orddict:find({y, 22}, Params),
    {ok, _Z3} = orddict:find({z, 32}, Params),
    {ok, X4} = orddict:find({x, 13}, Params),
    {ok, Y4} = orddict:find({y, 23}, Params),
    {ok, _Z4} = orddict:find({z, 33}, Params),
    Points = [
        {float(X1),float(Y1)},
        {float(X2),float(Y2)},
        {float(X3),float(Y3)},
        {float(X4),float(Y4)}
    ],
    {solid, remove_adj_duplicate_points(Points)}.


dxf_set_ent_shape(Params0) ->
    Params = merge_with_defaults(Params0,
             merge_with_defaults(
                 orddict_one_vals([{float_value, 41}]),
                 orddict_zero_vals([
                         name, x, y, z, {float_value, 40},
                         {angle, 50}, {angle, 51}]))),
    % SHAPE
    
    % shape name
    {ok, _} = orddict:find(name, Params),
    % insertion point
    {ok, _} = orddict:find(x, Params),
    {ok, _} = orddict:find(y, Params),
    {ok, _} = orddict:find(z, Params),
    % size
    {ok, _} = orddict:find({float_value, 40}, Params),
    % relative X-scale factor (default 1)
    {ok, _} = orddict:find({float_value, 41}, Params),
    % rotation angle
    {ok, _} = orddict:find({angle, 50}, Params),
    % oblique angle
    {ok, _} = orddict:find({angle, 51}, Params),
    unused.

%% Polyline entity
%%
dxf_set_ent_polyline(Params0) ->
    Params = merge_with_defaults(Params0, orddict_zero_vals([
        x, y, z, {float_value, 40}, {float_value, 41},
        {integer_value, 70}, {integer_value, 71}, {integer_value, 72},
        {integer_value, 73}, {integer_value, 74}, {integer_value, 75}, entities_follow])),
    %% POLYLINE GOES WITH VERTEX AND SEQEND
    
    %% POLYLINE can be either 2D or 3D, if 70, 71 and 72 are defined it is 3D
    
    % POLYLINE
    % vertices-follow flag
    {ok, _} = orddict:find(entities_follow, Params),

    % Polyline flag, if this bitwise and's with 64 it
    % is a 3D mesh, definitions.
    {ok, Flag} = orddict:find({integer_value, 70}, Params),
    case (Flag band 64) > 0 of
        true ->
            %% Mesh vertices, the vertex sequence contains
            %% M vertex points followed by N triangular faces.
            %%
            % x, y and z are ignored.
            % polygon mesh M and N vertex counts
            {ok, NVertex} = orddict:find({integer_value, 71}, Params),
            {ok, NFaces} = orddict:find({integer_value, 72}, Params),
            % smooth surface M and N densities
            {ok, _} = orddict:find({integer_value, 73}, Params),
            {ok, _} = orddict:find({integer_value, 74}, Params),
            % curves and smooth surface type
            {ok, _} = orddict:find({integer_value, 75}, Params),
            {mesh_start, {round(NVertex), round(NFaces)}};
        
        false ->
            %% 2D Polyline
            % x and y are always set to zero, Z is ignored
            {ok, X1} = orddict:find(x, Params),
            {ok, Y1} = orddict:find(y, Params),
            {ok, _} = orddict:find(z, Params),
            case (Flag band 1) > 0 of
                true ->
                    {line_seq_start, {true, [{float(X1),float(Y1)}]}};
                false ->
                    {line_seq_start, {false, [{float(X1),float(Y1)}]}}
            end
    end.
    

%% Vertex entity
%%
dxf_set_ent_vertex(Params0, MeshMode) ->
    %% Follows after POLYLINE, before SEQEND
    Params = merge_with_defaults(Params0, orddict_zero_vals([
        x, y, z, {float_value, 40}, {float_value, 41}, {float_value, 42}, {angle, 50}, {integer_value, 70},
        {integer_value, 71}, {integer_value, 72}, {integer_value, 73}])),
    % VERTEX
    % vertex flags (default 0)
    {ok, Flag} = orddict:find({integer_value, 70}, Params),
    case (Flag band 128) > 0 of
        true
          when MeshMode =:= true ->
            %% Mesh related
            case (Flag band 64) > 0 of
                true ->
                    %% Vertex
                    % location
                    {ok, X1} = orddict:find(x, Params),
                    {ok, Y1} = orddict:find(y, Params),
                    {ok, Z1} = orddict:find(z, Params),
                    % bulge value
                    {ok, _} = orddict:find({float_value, 42}, Params),
                    % curve fit tangent direction
                    {ok, _} = orddict:find({angle, 50}, Params),
                    {mesh_vertex, {float(X1),float(Y1),float(Z1)}};
                false ->
                    %% Triangle face (vertex indices in 71, 72, 73)
                    {ok, V1} = orddict:find({integer_value, 71}, Params),
                    {ok, V2} = orddict:find({integer_value, 72}, Params),
                    {ok, V3} = orddict:find({integer_value, 73}, Params),
                    % location is ignored.
                    % bulge value
                    {ok, _} = orddict:find({float_value, 42}, Params),
                    % curve fit tangent direction
                    {ok, _} = orddict:find({angle, 50}, Params),
                    {mesh_face, [round(V1),round(V2),round(V3)]}
            end;
        false when MeshMode =:= false ->
            %% 2D Vertex
            % location
            {ok, X1} = orddict:find(x, Params),
            {ok, Y1} = orddict:find(y, Params),
            {ok, _Z1} = orddict:find(z, Params),
            % bulge value
            {ok, _} = orddict:find({float_value, 42}, Params),
            % curve fit tangent direction
            {ok, _} = orddict:find({angle, 50}, Params),
            {line_seq, [{float(X1),float(Y1)}]};
        _ ->
            io:format("~p: " ++ ?__(1,"NOTE: DXF POLYLINE unknown vertex") ++
                "~n", [?MODULE]),
            unused
    end.
 
dxf_set_ent_seqend(_Params, MeshMode) ->
    %% Follows after POLYLINE/VERTEX
    %%
    %% SEQEND
    case MeshMode of
        true ->
            {mesh_end, 0};
        false ->
            {line_seq_end, 0}
    end.

%% lwPolyline entity
%%
dxf_set_ent_lwpolyline(Params0) ->
    Params = merge_with_defaults(Params0, orddict_zero_vals([
        x, y, z, {float_value, 40}, {float_value, 41}, {integer_value, 70},
        {integer_value, 71}, {integer_value, 72},
        {integer_value, 73}, {integer_value, 74}, {integer_value, 75},
        entities_follow])),
    
    % LWPOLYLINE
    {ok, _} = orddict:find(entities_follow, Params),
    
    % Polyline flag
    {ok, Flag} = orddict:find({integer_value, 70}, Params),
    case (Flag band 64) > 0 of
        true ->
            unused;
        false ->
            %% 2D Polyline
            {ok, LWSequ_0_X} = orddict:find({lwsequ, x}, Params),
            {ok, LWSequ_0_Y} = orddict:find({lwsequ, y}, Params),
            LWSequ = lists:reverse(lists:zip(LWSequ_0_X, LWSequ_0_Y)),
            
            case (Flag band 1) > 0 of
                true ->
                    %% Closed shape
                    {path, LWSequ};
                false ->
                    %% Open shape
                    {lines, seq_to_lines(LWSequ)}
            end
    end.
    

 

%% 3D Faces will have their points welded and turned into objects.
%%
dxf_set_ent_3dface(Params0) ->
    Params = merge_with_defaults(Params0, orddict_zero_vals([
        x, y, z, {x, 11}, {y, 21}, {z, 31}, {x, 12}, {y, 22}, {z, 32},
        color_number, {integer_value, 70}])),
    
    % 3DFACE
    % invisible edge flags
    LayerName = "0", % TODO
    {ok, Color} = orddict:find(color_number, Params),
    {ok, _} = orddict:find({integer_value, 70}, Params),
    case orddict:find({x, 13}, Params) of
        {ok, _} ->
            %% Quad (or triangle)
            {ok, X1} = orddict:find(x, Params),
            {ok, Y1} = orddict:find(y, Params),
            {ok, Z1} = orddict:find(z, Params),
            {ok, X2} = orddict:find({x, 11}, Params),
            {ok, Y2} = orddict:find({y, 21}, Params),
            {ok, Z2} = orddict:find({z, 31}, Params),
            {ok, X3} = orddict:find({x, 12}, Params),
            {ok, Y3} = orddict:find({y, 22}, Params),
            {ok, Z3} = orddict:find({z, 32}, Params),
            {ok, X4} = orddict:find({x, 13}, Params),
            {ok, Y4} = orddict:find({y, 23}, Params),
            {ok, Z4} = orddict:find({z, 33}, Params),
            %% If the third and fourth are the same, it is a triangle.
            case X3 =:= X4 andalso Y3 =:= Y4 andalso Z3 =:= Z4 of
                false ->
                    Vertices = [
                        {float(X1),float(Y1),float(Z1)},
                        {float(X2),float(Y2),float(Z2)},
                        {float(X3),float(Y3),float(Z3)},
                        {float(X4),float(Y4),float(Z4)}];
                true ->
                    Vertices = [
                        {float(X1),float(Y1),float(Z1)},
                        {float(X2),float(Y2),float(Z2)},
                        {float(X3),float(Y3),float(Z3)}]
            end;
        _ ->
            %% Triangle
            {ok, X1} = orddict:find(x, Params),
            {ok, Y1} = orddict:find(y, Params),
            {ok, Z1} = orddict:find(z, Params),
            {ok, X2} = orddict:find({x, 11}, Params),
            {ok, Y2} = orddict:find({y, 21}, Params),
            {ok, Z2} = orddict:find({z, 31}, Params),
            {ok, X3} = orddict:find({x, 12}, Params),
            {ok, Y3} = orddict:find({y, 22}, Params),
            {ok, Z3} = orddict:find({z, 32}, Params),
            Vertices = [
                {float(X1),float(Y1),float(Z1)},
                {float(X2),float(Y2),float(Z2)},
                {float(X3),float(Y3),float(Z3)}]
    end,    
    {td_face, {Vertices, Color, LayerName}}.
    

%% Block entity
%%
dxf_set_ent_block(Params0) ->
    Params = merge_with_defaults(Params0, orddict_zero_vals([
        name, {name, 3}, x, y, z, {integer_value, 70}])),
    % BLOCK
    % Block name
    {ok, Name0} = orddict:find(name, Params),
    % this is also the Block name
    {ok, _Name1} = orddict:find({name, 3}, Params),
    % Block base point
    {ok, BaseX} = orddict:find(x, Params),
    {ok, BaseY} = orddict:find(y, Params),
    {ok, BaseZ} = orddict:find(z, Params),
    % Block type flag
    {ok, Flags} = orddict:find({integer_value, 70}, Params),
    {block_start, {Name0, #block_info{base={BaseX,BaseY,BaseZ}, flags=Flags}}}.


%% Insert (usually a block)
%%
dxf_set_ent_insert(Params0) ->
    Params = merge_with_defaults(Params0, 
             merge_with_defaults(
                 orddict_one_vals([{float_value, 41}, {float_value, 42},
                     {float_value, 43}, {integer_value, 70}, {integer_value, 71}]),
                 orddict_zero_vals([name, x, y, z, {float_value, 44},
                     {float_value, 45}, {angle, 50}, entities_follow]))),
    % INSERT
    % Block name
    {ok, Name} = orddict:find(name, Params),
    % insertion point
    {ok, X} = orddict:find(x, Params),
    {ok, Y} = orddict:find(y, Params),
    {ok, Z} = orddict:find(z, Params),
    % X- scale factor (default 1)
    {ok, ScaleX} = orddict:find({float_value, 41}, Params),
    % Y scale factor (default 1)
    {ok, ScaleY} = orddict:find({float_value, 42}, Params),
    % Z scale factor (default 1)
    {ok, ScaleZ} = orddict:find({float_value, 43}, Params),
    % column and row spacing
    {ok, _} = orddict:find({float_value, 44}, Params),
    {ok, _} = orddict:find({float_value, 45}, Params),
    % rotation angle
    {ok, _} = orddict:find({angle, 50}, Params),
    % column and row counts
    {ok, _} = orddict:find({integer_value, 70}, Params),
    {ok, _} = orddict:find({integer_value, 71}, Params),
    % Attributes follow flag
    {ok, _} = orddict:find(entities_follow, Params),
    
    Scale = {ScaleX,ScaleY,ScaleZ},
    {block_insert, {Name, {X,Y,Z}, Scale}}.


dxf_set_ent_viewport(Params0) ->
    Params = merge_with_defaults(Params0, orddict_zero_vals([
        x, y, z, {float_value, 40}, {float_value, 41}, viewport_stat, viewport_number])),
    % VIEWPORT
    % center point of entity in paper space coordinates
    {ok, _} = orddict:find(x, Params),
    {ok, _} = orddict:find(y, Params),
    {ok, _} = orddict:find(z, Params),
    % width in paper space units
    {ok, _} = orddict:find({float_value, 40}, Params),
    % height in paper space units
    {ok, _} = orddict:find({float_value, 41}, Params),
    % viewport status field
    {ok, _} = orddict:find(viewport_stat, Params),
    % viewport ID
    {ok, _} = orddict:find(viewport_number, Params),
    unused.



open_dxf_file(FileName) ->
    {ok, FilePort} = file:open(FileName, [read, binary]),
    case file:read(FilePort, 4) of
        {ok, <<"Auto">>} ->
            %% It is either a binary file or an error
            {ok, BeginningMore1} = file:read(FilePort, 15),
            {ok, BeginningMore2} = file:read(FilePort, 3), 
            case detect_binary_dxf(<<BeginningMore1/binary, BeginningMore2/binary>>) of
                true ->
                    {ok, {dxf, binary, FilePort}};
                false ->
                    case detect_binary_dxb(<<BeginningMore1/binary>>) of
                        true ->
                            %% found dxb
                            file:position(FilePort, {cur, -3}),
                            {dxb, {dxb, FilePort}};
                        false ->
                            file:close(FilePort),
                            error
                    end
            end;
        _ ->
            file:position(FilePort, {bof, 0}),
            {ok, {dxf, ascii, FilePort}}
    end.

detect_binary_dxf(Beginning) ->
    % A binary DXF file begins with a 22-byte signature.
    Beginning =:= <<"CAD Binary DXF", 13, 10, 26, 0 >>.

detect_binary_dxb(Beginning) ->
    % 19 bytes
    Beginning =:= <<"CAD DXB 1.0", 13, 10, 26, 0 >>.

%% Table of group code ranges
%%
dxf_groupcode_to_valuetype(N) ->
    if 
        N >= 0 andalso N =< 9 -> string;
        N >= 10 andalso N =< 59 -> float;
        N >= 60 andalso N =< 79 -> integer;
        N >= 90 andalso N =< 99 -> int32;
        N >= 100 andalso N =< 109 -> string;
        N >= 110 andalso N =< 149 -> float;
        N >= 170 andalso N =< 179 -> integer;
        N >= 210 andalso N =< 239 -> float;
        
        N >= 270 andalso N =< 279 -> integer;
        N >= 280 andalso N =< 289 -> integer;
        N >= 290 andalso N =< 299 -> integer;
        N >= 300 andalso N =< 309 -> string;
        N >= 310 andalso N =< 319 -> string; % hex encoded
        N >= 320 andalso N =< 369 -> string; % (handles)
        N >= 370 andalso N =< 389 -> integer;
        N >= 390 andalso N =< 399 -> string; % (hex)
        N >= 400 andalso N =< 409 -> integer;
        N >= 410 andalso N =< 419 -> string;
        N >= 420 andalso N =< 429 -> int32;
        N >= 430 andalso N =< 439 -> string;
        N >= 440 andalso N =< 449 -> int32;
        N >= 450 andalso N =< 459 -> int32;
        N >= 460 andalso N =< 469 -> float32;
        N >= 470 andalso N =< 479 -> string;
        N =:= 999 -> comment;
        N >= 1000 andalso N =< 1009 -> string;
        N >= 1010 andalso N =< 1059 -> float;
        N >= 1060 andalso N =< 1070 -> integer; % 16-bit signed
        N =:= 1071 -> int32; % 32-bit signed
        N >= 1072 andalso N =< 1079 -> integer;
        
        %% When there are newer DXF versions, they may create new group code ranges
        %% which will need to be added in this table, unknown group codes will
        %% be tagged as comments which are allowed in text DXF but not binary DXF.
        %% If a binary DXF causes an error during decoding it is probably because
        %% the group code is not yet categorized as an integer or string.
        true ->
            io:format("~p: " ++ ?__(1,"Unknown group code:") ++
                " ~p~n", [?MODULE, N]),
            comment
    end.


next_grouppair({dxf, ascii, FilePort}) ->
    case next_grouppair_ascii(FilePort) of
        %% Skip comments
        {999, _} -> next_grouppair({dxf, ascii, FilePort});
        {GroupCode, GroupValue} -> {GroupCode, GroupValue}
    end;
    
next_grouppair({dxf, binary, FilePort}) ->
    %% Reads the next binary group pair
    {ok, <<GroupCode1:8/little-unsigned-integer>>} = file:read(FilePort, 1),
    GroupCode = case GroupCode1 of
        255 -> {ok, <<GroupCode2:16/little-unsigned-integer>>} = file:read(FilePort, 2), GroupCode2;
        _   -> GroupCode1
    end,
    GroupValue = case dxf_groupcode_to_valuetype(GroupCode) of
        string ->
            {ok, Bin} = file_read_delimited_nul(FilePort),
            Bin;
        integer when GroupCode =:= 1004 ->
            {ok, <<LenVal1:8/little-unsigned-integer>>} = file:read(FilePort, 1),
            ChunkData = file:read(FilePort, LenVal1),
            ChunkData;
        integer ->
            {ok, <<Val1:16/little-unsigned-integer>>} = file:read(FilePort, 2),
            Val1;
        int32 ->
            {ok, <<Val1:32/little-unsigned-integer>>} = file:read(FilePort, 4),
            Val1;
        float ->
            {ok, <<Val1:64/little-float>>} = file:read(FilePort, 8),
            Val1;
        comment ->
            %% This is probably a newer DXF file, you will need to 
            %% add a group code range in dxf_groupcode_to_valuetype/1
            erlang:error(unexpected_group_code)
    end,
    {GroupCode, GroupValue}.
    
next_grouppair_ascii(FilePort) ->
    %% Read two lines
    {ok, GroupCode_S0} = file_read_delimited_nl(FilePort),
    GroupCode_S = string:trim(binary_to_list(GroupCode_S0)),
    {ok, GroupValue_S} = file_read_delimited_nl(FilePort),
    case string:to_integer(GroupCode_S) of
        {error,     _} -> error;
        {GroupCode, _} ->
            GroupValue = next_grouppair_ascii_val(GroupCode, GroupValue_S),
            {GroupCode, GroupValue}
    end.
    
next_grouppair_ascii_val(GroupCode, GroupValue_S) ->
    case dxf_groupcode_to_valuetype(GroupCode) of
        string ->
            GroupValue_S;
        integer ->
            next_grouppair_ascii_int(GroupValue_S);
        int32 ->
            next_grouppair_ascii_int(GroupValue_S);
        float ->
            next_grouppair_ascii_float(GroupValue_S);
        _ ->
            GroupValue_S
    end.

next_grouppair_ascii_int(GroupValue_S) ->
    GroupValue_S1 = string:trim(binary_to_list(GroupValue_S)),
    case string:to_float(GroupValue_S1) of
        {error, no_float} -> 
            case string:to_integer(GroupValue_S1) of
                {error, _} -> 0;
                {Num_1, _} -> Num_1
            end;
        {Num_1, _} -> Num_1
    end.

next_grouppair_ascii_float(GroupValue_S) ->
    GroupValue_S1 = string:to_lower(string:trim(
        binary_to_list(GroupValue_S))),
    case string:split(GroupValue_S1, "e") of
        [LExpNum_S, RExpNum_S] ->
            LExpNum = case string:to_float(LExpNum_S) of
                {error, no_float} -> 
                    case string:to_integer(LExpNum_S) of
                        {error, _} -> 0.0;
                        {Num_1, _} -> float(Num_1)
                    end;
                {Num_1, _} -> Num_1
            end,
            RExpNum = case string:to_float(RExpNum_S) of
                {error, no_float} -> 
                    case string:to_integer(RExpNum_S) of
                        {error, _} -> 0.0;
                        {Num_2, _} -> float(Num_2)
                    end;
                {Num_2, _} -> Num_2
            end,
            LExpNum * math:pow(10, RExpNum);
        [_NoExponent] ->
            case string:to_float(GroupValue_S1) of
                {error, no_float} -> 
                    case string:to_integer(GroupValue_S1) of
                        {error, _} -> 0.0;
                        {Num_1, _} -> float(Num_1)
                    end;
                {Num_1, _} -> Num_1
            end
    end.

file_read_delimited_nl(Fp) ->
    {ok, <<Line/binary>>} = file:read_line(Fp),
    {ok, string:trim(Line, trailing)}.

%% Read until NUL
file_read_delimited_nul(Fp) -> file_read_delimited_nul(Fp, []).
file_read_delimited_nul(Fp, Str) ->
    case file:read(Fp, 1) of
        {ok, <<0:8>>}    -> {ok, iolist_to_binary(lists:reverse(Str))};
        {ok, <<Byte:8>>} -> file_read_delimited_nul(Fp, [Byte|Str])
    end.


%% DXB Files
%% If the current file is auto detected to be a DXB the importer might
%% as well try to import lines from it.

-record(dxb_state, {
    polyline_closure,  %% Add a closing vertex implicitly if 1
    scale_factor,      %% Rescale integer coordinates with this number
    color_number,      %% Color palette number
    lname              %% Layer name
}).

-define(B_EOF, 0).
-define(B_POLYLINE, 19).
-define(B_SEQEND, 17).
-define(B_NEWLAYER, 129).
-define(B_SCALEFACTOR, 128).
-define(B_NUMBERMODE, 135).
-define(B_NEWCOLOR, 136).
-define(B_LINE, 1).
-define(B_POINT, 2).
-define(B_CIRCLE, 3).
-define(B_ARC, 8).
-define(B_TRACE, 9).
-define(B_SOLID, 11).
-define(B_VERTEX, 20).
-define(B_3DFACE, 22).
-define(B_LINEEXTENSION, 130).
-define(B_TRACEEXTENSION, 131).
-define(B_UNUSED_132, 132).
-define(B_UNUSED_133, 133).
-define(B_UNUSED_134, 134).
-define(B_3DLINEEXTENSION, 137).

-define(SINT, little-signed-integer).

read_dxb_records({dxb, F}) ->
    Cmds=read_dxb_records(F, integer, #dxb_state{
        polyline_closure=0,
        scale_factor=1.0,
        color_number=0,
        lname="0"}, []),
    #dxfc{commands=Cmds}.
read_dxb_records(F, NumberMode, #dxb_state{scale_factor=ScaleFactor}=DXBS, Paths) ->
    case file:read(F, 1) of %% 1 byte
        eof -> lists:reverse(Paths);
        {ok, <<ItemType:8/little-unsigned-integer>>} ->
            case {ItemType, NumberMode} of
                {?B_EOF, _} ->
                    {ok, lists:reverse(Paths)};
                
                {?B_POLYLINE, _} ->
                    %% Polyline
                    {ok, <<ClosureFlag:16/?SINT>>} = file:read(F, 3 - 1),
                    read_dxb_records(F, NumberMode, DXBS#dxb_state{polyline_closure=ClosureFlag},
                        [{line_seq_start, {false, []}} | Paths]);

                {?B_SEQEND, _} ->
                    %% Seqend
                    read_dxb_records(F, NumberMode, DXBS,
                        [{line_seq_end, 0} | Paths]);

                {?B_NEWLAYER, _} ->
                    %% New Layer
                    %% Null terminated string
                    LayerName = file_read_delimited_nul(F),
                    read_dxb_records(F, NumberMode, DXBS#dxb_state{lname=LayerName}, Paths);

                {?B_SCALEFACTOR, _} ->
                    %% Scale Factor
                    {ok, <<NewScaleFactor:64/little-float>>} = file:read(F, 9 - 1),
                    read_dxb_records(F, NumberMode, DXBS#dxb_state{scale_factor=NewScaleFactor}, Paths);
                
                {?B_NUMBERMODE, _} ->
                    %% Number Mode
                    {ok, <<NewNumberMode:16/?SINT>>} = file:read(F, 3 - 1),
                    case NewNumberMode of
                        0 -> read_dxb_records(F, integer, DXBS, Paths);
                        1 -> read_dxb_records(F, float, DXBS, Paths)
                    end;
                
                {?B_NEWCOLOR, _} ->
                    %% New Color
                    {ok, <<NewColorNumber:16/?SINT>>} = file:read(F, 3 - 1),
                    case NewColorNumber of
                        _ when NewColorNumber =< 255 ->
                            read_dxb_records(F, NumberMode, DXBS#dxb_state{color_number=NewColorNumber}, Paths);
                        _ ->
                            read_dxb_records(F, NumberMode, DXBS#dxb_state{color_number=0}, Paths)
                    end;
                
                {?B_LINE, integer} ->
                    %% Line
                    {ok, 
                    << N_Fromx_Int:16/?SINT, N_Fromy_Int:16/?SINT,
                       N_Tox_Int:16/?SINT, N_Toy_Int:16/?SINT
                       >>} = file:read(F, 13 - 1  - 4),
                       %N_Toz_Int = 0, N_Fromz_Int = 0,
                    N_Fromx = ScaleFactor * N_Fromx_Int,
                    N_Fromy = ScaleFactor * N_Fromy_Int,
                    N_Tox = ScaleFactor * N_Tox_Int,
                    N_Toy = ScaleFactor * N_Toy_Int,
                    read_dxb_records(F, NumberMode, DXBS,
                        [{lines, [{{N_Tox, N_Toy}, {N_Fromx, N_Fromy}}]} | Paths]);

                {?B_LINE, float} ->
                    %% Line
                    {ok, 
                    << N_Fromx:64/little-float, N_Fromy:64/little-float,
                       N_Tox:64/little-float, N_Toy:64/little-float
                       >>} = file:read(F, 48 - 1  - 16),
                    read_dxb_records(F, NumberMode, DXBS,
                        [{lines, [{{N_Tox, N_Toy}, {N_Fromx, N_Fromy}}]} | Paths]);
                
                {?B_CIRCLE, integer} ->
                    %% Circle
                    {ok,
                    << N_Ctrx_Int:16/?SINT,
                       N_Ctry_Int:16/?SINT,
                       N_Rad_Int:16/?SINT>>} = file:read(F, 7 - 1),
                    N_Ctrx = ScaleFactor * N_Ctrx_Int,
                    N_Ctry = ScaleFactor * N_Ctry_Int,
                    N_Rad = ScaleFactor * N_Rad_Int,
                    read_dxb_records(F, NumberMode, DXBS,
                        [{lines, arc_into_lines(N_Ctrx, N_Ctry, N_Rad, 0, 0)} | Paths]);

                {?B_CIRCLE, float} ->
                    %% Circle
                    {ok,
                    << N_Ctrx:64/little-float,
                       N_Ctry:64/little-float,
                       N_Rad:64/little-float>>} = file:read(F, 25 - 1),
                    read_dxb_records(F, NumberMode, DXBS,
                        [{lines, arc_into_lines(N_Ctrx, N_Ctry, N_Rad, 0, 0)} | Paths]);

                {?B_ARC, integer} ->
                    %% Arc
                    {ok, 
                    << N_Ctrx_Int:16/?SINT, N_Ctry_Int:16/?SINT, N_Rad_Int:16/?SINT,
                       A_Starta_Int:32/?SINT, A_Enda_Int:32/?SINT>>} = file:read(F, 19 - 1),
                    N_Ctrx = ScaleFactor * N_Ctrx_Int,
                    N_Ctry = ScaleFactor * N_Ctry_Int,
                    N_Rad = ScaleFactor * N_Rad_Int,
                    A_Starta = A_Starta_Int,
                    A_Enda = A_Enda_Int,
                    read_dxb_records(F, NumberMode, DXBS,
                        [{lines, arc_into_lines(N_Ctrx, N_Ctry, N_Rad, A_Starta, A_Enda)} | Paths]);

                {?B_ARC, float} ->
                    %% Arc
                    {ok, 
                    << N_Ctrx:64/little-float, N_Ctry:64/little-float, N_Rad:64/little-float,
                       A_Starta:64/little-float, A_Enda:64/little-float>>} = file:read(F, 41 - 1),
                    read_dxb_records(F, NumberMode, DXBS,
                        [{lines, arc_into_lines(N_Ctrx, N_Ctry, N_Rad, A_Starta, A_Enda)} | Paths]);

                {?B_TRACE, integer} ->
                    %% Trace
                    {ok, 
                    << _:16/?SINT, _N_Y1_Int:16/?SINT,
                       _:16/?SINT, _N_Y2_Int:16/?SINT,
                       _:16/?SINT, _N_Y3_Int:16/?SINT,
                       _:16/?SINT, _N_Y4_Int:16/?SINT>>} = file:read(F, 17 - 1),
                    read_dxb_records(F, NumberMode, DXBS,
                        Paths);

                {?B_TRACE, float} ->
                    %% Trace
                    {ok, 
                    << _N_X1:64/little-float, _N_Y1:64/little-float, _N_X2:64/little-float, _N_Y2:64/little-float,
                       _N_X3:64/little-float, _N_Y3:64/little-float, _N_X4:64/little-float, _N_Y4:64/little-float>>} = file:read(F, 65 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                {?B_TRACEEXTENSION, integer} ->
                    file:read(F, 9 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                {?B_TRACEEXTENSION, float} ->
                    file:read(F, 33 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                {?B_SOLID, integer} ->
                    %% Solid
                    {ok, 
                    << N_X1_Int:16/?SINT, N_Y1_Int:16/?SINT, N_X2_Int:16/?SINT, N_Y2_Int:16/?SINT,
                       N_X3_Int:16/?SINT, N_Y3_Int:16/?SINT, N_X4_Int:16/?SINT, N_Y4_Int:16/?SINT>>} = file:read(F, 17 - 1),
                    N_X1 = ScaleFactor * N_X1_Int,
                    N_Y1 = ScaleFactor * N_Y1_Int,
                    N_X2 = ScaleFactor * N_X2_Int,
                    N_Y2 = ScaleFactor * N_Y2_Int,
                    N_X3 = ScaleFactor * N_X3_Int,
                    N_Y3 = ScaleFactor * N_Y3_Int,
                    N_X4 = ScaleFactor * N_X4_Int,
                    N_Y4 = ScaleFactor * N_Y4_Int,
                    read_dxb_records(F, NumberMode, DXBS,
                        [{solid, remove_adj_duplicate_points([{N_X1, N_Y1}, {N_X2, N_Y2}, {N_X3, N_Y3}, {N_X4, N_Y4}])} | Paths]);

                {?B_SOLID, float} ->
                    %% Solid
                    {ok, 
                    << N_X1:64/little-float, N_Y1:64/little-float, N_X2:64/little-float, N_Y2:64/little-float,
                       N_X3:64/little-float, N_Y3:64/little-float, N_X4:64/little-float, N_Y4:64/little-float>>} = file:read(F, 65 - 1),
                    read_dxb_records(F, NumberMode, DXBS,
                        [{solid, remove_adj_duplicate_points([{N_X1, N_Y1}, {N_X2, N_Y2}, {N_X3, N_Y3}, {N_X4, N_Y4}])} | Paths]);

                {?B_VERTEX, integer} ->
                    %% Vertex
                    {ok, 
                    << _N_X_Int:16/?SINT, _N_Y_Int:16/?SINT>>} = file:read(F, 5 - 1),
                    %N_X = ScaleFactor * N_X_Int,
                    %N_Y = ScaleFactor * N_Y_Int,
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                {?B_VERTEX, float} ->
                    %% Vertex
                    {ok, 
                    << _N_X:64/little-float, _N_Y:64/little-float>>} = file:read(F, 17 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                {?B_3DFACE, integer} ->
                    %% 3Dface
                    {ok, 
                    << N_X1_Int:16/?SINT, N_Y1_Int:16/?SINT, N_Z1_Int:16/?SINT,
                       N_X2_Int:16/?SINT, N_Y2_Int:16/?SINT, N_Z2_Int:16/?SINT,
                       N_X3_Int:16/?SINT, N_Y3_Int:16/?SINT, N_Z3_Int:16/?SINT,
                       N_X4_Int:16/?SINT, N_Y4_Int:16/?SINT, N_Z4_Int:16/?SINT>>} = file:read(F, 25 - 1),
                    N_X1 = ScaleFactor * N_X1_Int,
                    N_Y1 = ScaleFactor * N_Y1_Int,
                    N_Z1 = ScaleFactor * N_Z1_Int,
                    N_X2 = ScaleFactor * N_X2_Int,
                    N_Y2 = ScaleFactor * N_Y2_Int,
                    N_Z2 = ScaleFactor * N_Z2_Int,
                    N_X3 = ScaleFactor * N_X3_Int,
                    N_Y3 = ScaleFactor * N_Y3_Int,
                    N_Z3 = ScaleFactor * N_Z3_Int,
                    N_X4 = ScaleFactor * N_X4_Int,
                    N_Y4 = ScaleFactor * N_Y4_Int,
                    N_Z4 = ScaleFactor * N_Z4_Int,
                    Color = DXBS#dxb_state.color_number,
                    LName = DXBS#dxb_state.lname,
                    Face = {td_face, {[
                        {N_X1, N_Y1, N_Z1},
                        {N_X2, N_Y2, N_Z2},
                        {N_X3, N_Y3, N_Z3},
                        {N_X4, N_Y4, N_Z4}
                    ], Color, LName}},
                    read_dxb_records(F, NumberMode, DXBS, [Face|Paths]);

                {?B_3DFACE, float} ->
                    %% 3Dface
                    {ok, 
                    << N_X1:64/little-float, N_Y1:64/little-float, N_Z1:64/little-float,
                       N_X2:64/little-float, N_Y2:64/little-float, N_Z2:64/little-float,
                       N_X3:64/little-float, N_Y3:64/little-float, N_Z3:64/little-float,
                       N_X4:64/little-float, N_Y4:64/little-float, N_Z4:64/little-float>>} = file:read(F, 97 - 1),
                    Color = DXBS#dxb_state.color_number,
                    LName = DXBS#dxb_state.lname,
                    Face = {td_face, {[
                        {N_X1, N_Y1, N_Z1},
                        {N_X2, N_Y2, N_Z2},
                        {N_X3, N_Y3, N_Z3},
                        {N_X4, N_Y4, N_Z4}
                    ], Color, LName}},
                    read_dxb_records(F, NumberMode, DXBS, [Face|Paths]);


                {?B_POINT, integer} ->
                    file:read(F, 5 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                {?B_POINT, float} ->
                    file:read(F, 17 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                {?B_LINEEXTENSION, integer} ->
                    file:read(F, 5 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);
                    
                {?B_LINEEXTENSION, float} ->
                    file:read(F, 17 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                {?B_UNUSED_132, integer} ->
                    file:read(F, 5 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                {?B_UNUSED_132, float} ->
                    file:read(F, 17 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);
                
                {?B_UNUSED_133, integer} ->
                    file:read(F, 5 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);
                    
                {?B_UNUSED_133, float} ->
                    file:read(F, 9 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                {?B_UNUSED_134,integer} ->
                    file:read(F, 5 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                {?B_UNUSED_134,float} ->
                    file:read(F, 17 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths); 
                
                {?B_3DLINEEXTENSION, integer} ->
                    file:read(F, 7 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);
                    
                {?B_3DLINEEXTENSION, float} ->
                    file:read(F, 25 - 1),
                    read_dxb_records(F, NumberMode, DXBS, Paths);

                _Unknown ->
                    io:format("~p: " ++ ?__(1,"Unrecognized DXB Record") ++ ": ~w~n",
                        [?MODULE, {ItemType, NumberMode}]),
                    {ok, lists:reverse(Paths)}
            end
    end.


adjust_commands(Commands)
  when is_list(Commands) ->
    adjust_commands(Commands, []).
adjust_commands([], OCommands) ->
    lists:reverse(OCommands);
adjust_commands([A|Commands], OCommands) ->
    case A of
        unused ->
            adjust_commands(Commands, OCommands);

        %% Loose lines
        {line_seq_start, {false, List}} ->
            {Commands_1, Seq} = adjust_commands_seq(Commands, List),
            OCommands_1 = [{lines, seq_to_lines(Seq)}|OCommands],
            adjust_commands(Commands_1, OCommands_1);
        {lines, Lines}
          when length(Lines) > 0 ->
            adjust_commands(Commands, [{lines, Lines} | OCommands]);
        {lines, []} ->
            adjust_commands(Commands, OCommands);

        %% Closed paths
        {line_seq_start, {true, List}} ->
            {Commands_1, Seq} = adjust_commands_seq(Commands, List),
            OCommands_1 = [{path, Seq}|OCommands],
            adjust_commands(Commands_1, OCommands_1);
        {path, _} ->
            adjust_commands(Commands, [A | OCommands]);
        
        {solid, Path}
          when length(Path) > 0 ->
            adjust_commands(Commands, [A | OCommands]);
        
        {mesh_start, _} -> adjust_commands(Commands, [A | OCommands]);
        {mesh_vertex, _Path} -> adjust_commands(Commands, [A | OCommands]);
        {mesh_face, _Path} -> adjust_commands(Commands, [A | OCommands]);
        {mesh_end, _} -> adjust_commands(Commands, [A | OCommands]);
        
        {td_face, _} -> adjust_commands(Commands, [A | OCommands])
    end.

adjust_commands_seq(Commands, Seq) ->
    adjust_commands_seq(Commands, [], [Seq]).
adjust_commands_seq([A|Commands], OCommands, Seq) ->
    case A of
        {line_seq, List} ->
            adjust_commands_seq(Commands, OCommands, [List|Seq]);
        {line_seq_end, _} ->
            { lists:reverse(OCommands) ++ Commands,
              lists:append(lists:reverse(Seq)) };
        _ ->
            adjust_commands_seq(Commands, [A | OCommands], Seq)
    end.



expand_inserts(#dxfc{commands=Cmds}=Cont) ->
    expand_inserts(Cmds, Cont, []).
expand_inserts([{block_insert, {Name, Pnt, Scale}}|L], #dxfc{blocks=Blocks}=Cont, OL) ->
    case orddict:find(Name, Blocks) of
        {ok, {B, Ent}} ->
            OL_1 = lists:reverse(position_insert(
                Ent, B, Pnt, Scale)) ++ OL,
            expand_inserts(L, Cont, OL_1);
        _ ->
            expand_inserts(L, Cont, OL)
    end;
expand_inserts([C|L], Cont, OL) ->
    expand_inserts(L, Cont, [C|OL]);
expand_inserts([], Cont, OL) ->
    Cont#dxfc{commands=lists:reverse(OL)}.

position_insert(Ent, #block_info{base=BPnt}=_, Pnt, Scl) ->
    position_insert_1(Ent, {BPnt, Pnt, Scl}, []).
position_insert_1([{line_seq, List}|Ent], PT, OL) ->
    C1 = {line_seq, [position_point(PT, P) || P <- List]},
    position_insert_1(Ent, PT, [C1|OL]);
position_insert_1([{line_seq_start, {Closed, List}}|Ent], PT, OL) ->
    C1 = {line_seq_start, {Closed, [position_point(PT, P) || P <- List]}},
    position_insert_1(Ent, PT, [C1|OL]);
position_insert_1([{lines, Lines}|Ent], PT, OL) ->
    C1 = {lines, [{position_point(PT, P1), position_point(PT, P2)}
            || {P1,P2} <- Lines]},
    position_insert_1(Ent, PT, [C1|OL]);
position_insert_1([{path, {List, C, LY}}|Ent], PT, OL) ->
    C1 = {path, {[position_point(PT, P) || P <- List], C, LY}},
    position_insert_1(Ent, PT, [C1|OL]);
position_insert_1([{solid, List}|Ent], PT, OL) ->
    C1 = {solid, [position_point(PT, P) || P <- List]},
    position_insert_1(Ent, PT, [C1|OL]);
position_insert_1([{mesh_vertex, _Path}|Ent], PT, OL) ->
    C1 = {mesh_vertex, _Path},
    position_insert_1(Ent, PT, [C1|OL]);
position_insert_1([{td_face, {List, C, LY}}|Ent], PT, OL) ->
    C1 = {td_face, {[position_point(PT, P) || P <- List], C, LY}},
    position_insert_1(Ent, PT, [C1|OL]);
position_insert_1([C|Ent], PT, OL) ->
    position_insert_1(Ent, PT, [C|OL]);
position_insert_1([], _PT, OL) ->
    lists:reverse(OL).
    
position_point({{XC,YC,ZC}, {XA,YA,ZA}, {XS,YS,ZS}}, {X,Y,Z}) ->
    {(X - XC) * XS + XA, (Y - YC) * YS + YA, (Z - ZC) * ZS + ZA};
position_point({{XC,YC,_}, {XA,YA,_}, {XS,YS,_}}, {X,Y}) ->
    {(X - XC) * XS + XA, (Y - YC) * YS + YA}.
    


rescale(Scaler, Paths) ->
    rescale(Scaler, Paths, []).
rescale(_, [], OPaths) ->
    lists:reverse(OPaths);
rescale(Scaler, [A|Paths], OPaths) ->
    case A of
        {lines, List}
          when length(List) > 0 ->
            List_1 = [
                {rescale_coord(Scaler, C1),
                 rescale_coord(Scaler, C2)}
                || {C1,C2} <- List],
            NewPath = {lines, List_1};
        {path, {List, C, LY}}
          when length(List) > 0 ->
            List_1 = [ rescale_coord(Scaler, C1) || C1 <- List],
            NewPath = {path, {List_1, C, LY}};
        {solid, List} -> 
            NewPath = {solid, [
                rescale_coord(Scaler, Coord)
                || Coord <- List]};
        {mesh_vertex, Coord}  -> 
            NewPath = {mesh_vertex, rescale_coord(Scaler, Coord)};
        {td_face, {List, C, LY}}  -> 
            NewPath = {td_face,
                {[rescale_coord(Scaler, Coord) || Coord <- List], C, LY}};
        Unk ->
            NewPath = Unk
    end,
    rescale(Scaler, Paths, [NewPath|OPaths]).
rescale_coord({XS,YS,_}, {X,Y}) ->
    {XS * X, YS * Y};
rescale_coord({XS,YS,ZS}, {X,Y,Z}) ->
    {XS * X, YS * Y, ZS * Z}.

remove_adj_duplicate_points(Points) ->
    remove_adj_duplicate_points(Points, []).
remove_adj_duplicate_points([], List) ->
    lists:reverse(List);
remove_adj_duplicate_points([Pnt | Points], [LPnt | _]=List)
  when Pnt =:= LPnt ->
    remove_adj_duplicate_points(Points, List);
remove_adj_duplicate_points([Pnt | Points], List) ->
    remove_adj_duplicate_points(Points, [Pnt | List]).
    
    
seq_to_lines(Seqs) ->
    seq_to_lines(Seqs, []).
seq_to_lines([P1|[P2|_]=R], OL) ->
    seq_to_lines(R, [{P1,P2}|OL]);
seq_to_lines([_], OL) ->
    lists:reverse(OL);
seq_to_lines([], OL) ->
    lists:reverse(OL).
    

edge_pairs([E|_]=Fs) ->
    edge_pairs(Fs, E, []).
edge_pairs([E1|[E2|_]=Fs], E0, OL) ->
    edge_pairs(Fs, E0, [{E1,E2}|OL]);
edge_pairs([E1], E0, OL) ->
    lists:reverse([{E1,E0}|OL]).

all_edges(FL) ->
    lists:append([edge_pairs(F) || F <- FL]).


%%
%% 2D Solids to E3D
%%

-define(DXF_2D_THICKNESS, 0.05).

solid_commands_to_objects(Commands, Scale) ->
    solid_commands_to_objects_f(Commands, Scale, [], []).

solid_commands_to_objects_f([], _Scale, [], Other) ->
    lists:reverse(Other);
solid_commands_to_objects_f([], Scale, Solids, Other) ->
    Thick = ?DXF_2D_THICKNESS,
    case solids_vxlist(Solids) of
        [] ->
            {ok, lists:reverse(Other)};
        VxList ->
            Polys = solids_edge_objects(Solids, VxList),
            OutsidePath = solid_path_around(0, unique_edge(lists:append([edge_loops(S) || S <- Polys]))),
            % io:format("~p~n", [OutsidePath]),
            
            FrontVs = [{X*Scale,Y*Scale,Thick}  || {X,Y} <- VxList],
            BackVs  = [{X*Scale,Y*Scale,-Thick} || {X,Y} <- VxList],
            BackVsOffset = length(VxList),
            Vs = FrontVs ++ BackVs,
            
            FrontHe = OutsidePath,
            BackHe = [{Idx1+BackVsOffset, Idx2+BackVsOffset} || {Idx1,Idx2} <- OutsidePath],
            FrontFs = [#e3d_face{vs=lists:reverse([Idx              || Idx <- Pl])} || Pl <- Polys],
            BackFs  = [#e3d_face{vs=lists:reverse([Idx+BackVsOffset || Idx <- Pl])} || Pl <- Polys],
            FillBetween = [#e3d_face{vs=lists:reverse([V1,V1+BackVsOffset,V2+BackVsOffset,V2])} || {V1, V2} <- OutsidePath],
            Fs = FrontFs ++ BackFs ++ FillBetween,
            He = FrontHe ++ BackHe,
            
            Mesh = #e3d_mesh{
                type=polygon,
                vs=Vs,
                fs=Fs,
                he=He
            },
            [{obj, #e3d_object{obj=Mesh}} | lists:reverse(Other)]
    end;
solid_commands_to_objects_f([{solid, _}=S | P], Scale, Solids, Other) ->
    solid_commands_to_objects_f(P, Scale, [S | Solids], Other);
solid_commands_to_objects_f([E | P], Scale, Solids, Other) ->
    solid_commands_to_objects_f(P, Scale, Solids, [E | Other]).
edge_loops([A,B|R]) when A < B -> [{A,B} | edge_loops([B|R],A)];
edge_loops([A,B|R])            -> [{B,A} | edge_loops([B|R],A)].
edge_loops([B],S) when S < B   -> [{S,B}];
edge_loops([B],S)              -> [{B,S}];
edge_loops([A,B|R],S) when A < B -> [{A,B} | edge_loops([B|R],S)];
edge_loops([A,B|R],S)            -> [{B,A} | edge_loops([B|R],S)].
unique_edge(R) ->
    unique_edge(R, gb_sets:new(), []).
unique_edge([], _, List) ->
    lists:reverse(List);
unique_edge([A|R], Sets, List) ->
    case gb_sets:is_member(A, Sets) of
        true ->
            unique_edge(R, Sets, lists:delete(A, List));
        false ->
            unique_edge(R, gb_sets:add_element(A, Sets), [A | List])
    end.

solids_edge_objects(Solids, VxList) ->
    Enumed = solids_numerate(VxList),
    solids_edge_objects(Solids, gb_trees:from_orddict(Enumed), []).
solids_edge_objects([], _, VIndices) ->
    lists:reverse(VIndices);
solids_edge_objects([{solid, PL} | R], VxList, VIndices) ->
    solids_edge_objects(R, VxList,
        [[ gb_trees:get(C, VxList) || C <- PL ] | VIndices]).
solids_numerate(VxList) ->
    %% NOTE: If array starts with 1, change 0 to 1
    solids_numerate(0, VxList, []).
solids_numerate(_Index, [], Enumed) ->
    orddict:from_list(lists:reverse(Enumed));
solids_numerate(Index, [C | VxList], Enumed) ->
    solids_numerate(Index+1, VxList, [{C, Index} | Enumed]).

solids_vxlist(Solids) ->
    solids_vxlist(Solids, gb_sets:new()).
solids_vxlist([], CoordSet) ->
    VertexList = gb_sets:to_list(CoordSet),
    VertexList;
solids_vxlist([{solid, PL} | R], CoordSet_0) ->
    CoordSet = lists:foldl(
        fun(C, CoordSet_1) ->
            gb_sets:add_element(C, CoordSet_1)
        end, CoordSet_0, PL),
    solids_vxlist(R, CoordSet).


solid_path_around(I, T) ->
    solid_path_around(I, T, gb_sets:new(), I, []).
solid_path_around(I, _ , _Visited, I, List)
  when length(List) > 0 ->
    lists:reverse(List);
solid_path_around(I, TT, Visited, I_1, List) ->
    Next = solid_path_around_next(I, TT, Visited, I_1),
    solid_path_around(Next, TT,
        gb_sets:add_element(Next, Visited), I_1, [{I,Next} | List]).

solid_path_around_next(I, TT, Visited, I_1) ->
    case solid_path_around_routes(I, TT, Visited) of
        [Next_1] -> Next_1;
        [I_1,Next_1] -> Next_1;
        [Next_1,I_1] -> Next_1;
        [A,B] when A < B -> A;
        [A,B] when A > B -> B
    end.

solid_path_around_routes(_I, [], _Visited) -> [];
solid_path_around_routes(I, [{I,V} | TTL], Visited) ->
    case gb_sets:is_member(V, Visited) of
        true  -> solid_path_around_routes(I, TTL, Visited);
        false -> [V | solid_path_around_routes(I, TTL, Visited)]
    end;
solid_path_around_routes(I, [{V,I} | TTL], Visited) ->
    case gb_sets:is_member(V, Visited) of
        true  -> solid_path_around_routes(I, TTL, Visited);
        false -> [V | solid_path_around_routes(I, TTL, Visited)]
    end;
solid_path_around_routes(I, [_ | TTL], Visited) ->
    solid_path_around_routes(I, TTL, Visited).


%%
%% 3DFACE to E3D
%%


face_commands_to_objects(Commands) ->
    face_commands_to_objects_1(Commands).

face_commands_to_objects_1(Commands) ->
    face_commands_to_objects_1(Commands, [], []).
face_commands_to_objects_1([], [], Other) ->
    lists:reverse(Other);
face_commands_to_objects_1([], Faces, Other) ->
    case face_vxlist(Faces) of
        [] ->
            {ok, lists:reverse(Other)};
        VxList ->
            {MtlList, _MtlMap} = mtl_list_from_faces(Faces),
            FaceVIdxAndMat = face_edge_objects(Faces, VxList),
            He = lists:append([face_all_edges(F) || {F,_Mat} <- FaceVIdxAndMat]),
            
            %% TODO: Change path directions if needed if the same edge pair
            %%       shows up between faces.
            
            Mesh = #e3d_mesh{
                type=polygon,
                vs=VxList,
                fs=[#e3d_face{vs=Vs,mat=[Mt]} || {Vs,Mt} <- FaceVIdxAndMat],
                he=He
            },
            [{obj, #e3d_object{obj=Mesh,mat=MtlList}} | lists:reverse(Other)]
    end;
face_commands_to_objects_1([{td_face, Tup} | P], Faces, Other) ->
    face_commands_to_objects_1(P, [{face, Tup} | Faces], Other);
face_commands_to_objects_1([E | P], Faces, Other) ->
    face_commands_to_objects_1(P, Faces, [E | Other]).

%% Get all edges to turn them into hard edges.
face_all_edges([A,B|R]) -> [{A,B} | face_all_edges([B|R],A)].
face_all_edges([B],S)   -> [{S,B}];
face_all_edges([A,B|R],S) -> [{A,B} | face_all_edges([B|R],S)].

face_edge_objects(Faces, VxList) ->
    Enumed = face_numerate(VxList),
    face_edge_objects(Faces, gb_trees:from_orddict(Enumed), []).
face_edge_objects([], _, VIndices) ->
    lists:reverse(VIndices);
face_edge_objects([{face, {PL, Color, _LName}} | R], VxList, VIndices) ->
    MatName = list_to_atom("dxf_mat_" ++ integer_to_list(Color)),
    face_edge_objects(R, VxList,
        [{[ gb_trees:get(C, VxList) || C <- PL ], MatName} | VIndices]).
face_numerate(VxList) ->
    face_numerate(0, VxList, []).
face_numerate(_Index, [], Enumed) ->
    orddict:from_list(lists:reverse(Enumed));
face_numerate(Index, [C | VxList], Enumed) ->
    face_numerate(Index+1, VxList, [{C, Index} | Enumed]).

face_vxlist(Faces) ->
    face_vxlist(Faces, gb_sets:new()).
face_vxlist([], CoordSet) ->
    VertexList = gb_sets:to_list(CoordSet),
    VertexList;
face_vxlist([{face, {PL, _Color, _LName}} | R], CoordSet_0) ->
    CoordSet = lists:foldl(
        fun(C, CoordSet_1) ->
            gb_sets:add_element(C, CoordSet_1)
        end, CoordSet_0, PL),
    face_vxlist(R, CoordSet).


mtl_list_from_faces(Faces) ->
    mtl_list_from_faces(Faces, gb_trees:empty()).
mtl_list_from_faces([], MtlMap) ->
    L1 = lists:usort([{B,A} || {A,B} <- gb_trees:to_list(MtlMap)]),
    MtlList = [mtl_list_from_faces_1(Col) || {_, Col} <- L1],
    {MtlList, MtlMap};
mtl_list_from_faces([{face, {_, Color, _}} | R], MtlMap_0) ->
    MtlMap_1 = case gb_trees:lookup(Color, MtlMap_0) of
        none ->
            gb_trees:insert(Color, gb_trees:size(MtlMap_0), MtlMap_0);
        {value, _} ->
            MtlMap_0
    end,
    mtl_list_from_faces(R, MtlMap_1).
mtl_list_from_faces_1(Col) ->
    {R,G,B} = coi_to_rgb(Col),
    MatName = list_to_atom("dxf_mat_" ++ integer_to_list(Col)),
    OpenGL = {opengl, [
        {ambient,  {0.0,0.0,0.0,1.0}},
        {specular, {1.0,1.0,1.0,0.2}},
        {shininess, 0.7},
        {diffuse,  {R, G, B, 1.0}},
        {emission, {R * 0.1,G * 0.1,B * 0.1, 1.0}},
        {metallic, 0.1},
        {roughness, 0.8},
        {vertex_colors, set}
    ]},
    {MatName, [OpenGL]}.



%%%
%%%  Vertex lists to E3D
%%%

mesh_sequences_to_objects(Commands) ->
    mesh_sequences_to_objects(Commands, false, []).
mesh_sequences_to_objects([], false, OL) ->
    lists:reverse(OL);
mesh_sequences_to_objects([{mesh_start, {NVertex, NFaces}} | R], false, OL) ->
    mesh_sequences_to_objects(R, {NVertex, NFaces, [], []}, OL);
mesh_sequences_to_objects([{mesh_vertex, {X1,Y1,Z1}}|R], {NVertex, NFaces, Vs, Fs}, OL) ->
    Vs_1 = [{X1,Y1,Z1}|Vs],
    mesh_sequences_to_objects(R, {NVertex, NFaces, Vs_1, Fs}, OL);
mesh_sequences_to_objects([{mesh_face, VI}|R], {NVertex, NFaces, Vs, Fs}, OL) ->
    Fs_1 = [[I-1 || I <- VI]|Fs],
    mesh_sequences_to_objects(R, {NVertex, NFaces, Vs, Fs_1}, OL);
mesh_sequences_to_objects([{mesh_end, _}|R], {NVertex, NFaces, Vs_0, Fs_0}, OL) ->
    Vs_1 = lists:reverse(Vs_0),
    Fs_1 = lists:reverse(Fs_0),
    case length(Vs_1) > NVertex of
        true ->
            io:format("~p: " ++ ?__(1,"NOTE: More vertices than expected") ++
                " (~w > ~w)~n",
                [?MODULE, length(Vs_1), NVertex]),
            Vs = lists:sublist(Vs_1, 1, NVertex);
        false ->
            Vs = Vs_1
    end,
    case length(Fs_1) > NFaces of
        true ->
            io:format("~p: " ++ ?__(2,"NOTE: More faces than expected") ++
                " (~w > ~w)~n",
                [?MODULE, length(Fs_1), NFaces]),
            Fs = lists:sublist(Fs_1, 1, NFaces);
        false ->
            Fs = Fs_1
    end,
    He = all_edges(Fs),
    
    Mesh = #e3d_mesh{
        type=polygon,
        vs=Vs,
        fs=[#e3d_face{vs=F} || F <- Fs],
        he=He
    },
    Obj = {obj, #e3d_object{obj=Mesh}},
    mesh_sequences_to_objects(R, false, [Obj|OL]);
mesh_sequences_to_objects([{mesh_vertex, _}|R], false, OL) ->
    mesh_sequences_to_objects(R, false, OL);
mesh_sequences_to_objects([{mesh_face, _}|R], false, OL) ->
    mesh_sequences_to_objects(R, false, OL);
mesh_sequences_to_objects([{mesh_end, _}|R], false, OL) ->
    mesh_sequences_to_objects(R, false, OL);
mesh_sequences_to_objects([P|R], S, OL) ->
    mesh_sequences_to_objects(R, S, [P|OL]).


%%%
%%% Contours to E3D
%%%

contours_to_objects(Commands, _Scale, ignore) ->
    Commands;
contours_to_objects(Commands, Scale, fill) ->
    contours_to_objects_f(Commands, Scale, [], []).
    
contours_to_objects_f([{path, Points}|Commands], Scale, Contours, OL) ->
    Objs = contours_to_objects_f_path(Scale, Points),
    OL_1 = Objs ++ OL,
    contours_to_objects_f(Commands, Scale, Contours, OL_1);
contours_to_objects_f([{lines, []}|Commands], Scale, Contours, OL) ->
    contours_to_objects_f(Commands, Scale, Contours, OL);
contours_to_objects_f([{lines, Lines}|Commands], Scale, Contours, OL) ->
    contours_to_objects_f(Commands, Scale, [Lines | Contours], OL);
contours_to_objects_f([P|Commands], Scale, Contours, OL) ->
    contours_to_objects_f(Commands, Scale, Contours, [P|OL]);
contours_to_objects_f([], Scale, Contours, OL) ->
    Objs = contours_to_objects_f_lines(Scale, Contours),
    Objs ++ lists:reverse(OL).


contours_to_objects_f_lines(Scale, Contours) ->
    Lines = lists:append(Contours),
    Shapes_0 = line_objects(Lines),
    %% Find empty markers among distinct shapes
    {Shapes, EmptyMarkers} = find_empty_points(Shapes_0),
    ShapeLists = [ fill(Shape, EmptyMarkers) || Shape <- Shapes],
    FLs0 = contours_to_objects_f_1(ShapeLists, Scale),
    FLs = [O || O <- FLs0, O =/= unused],
    [
        case contours_to_objects_obj(FL1) of
            {obj, _}=Obj -> Obj;
            unused -> false
        end
    || FL1 <- FLs ].

contours_to_objects_f_path(Scale, Points) ->
    FLs0 = contours_to_objects_f_1([[
        {path, Points},
        {hollow_out, Points}]], Scale),
    FLs = [O || O <- FLs0, O =/= unused],
    [
        case contours_to_objects_obj(FL1) of
            {obj, _}=Obj -> Obj;
            unused -> false
        end
    || FL1 <- FLs ].




contours_to_objects_f_1(ShapeLists, Scale) ->
    L = [contours_to_objects_f_2(P, Scale) || P <- ShapeLists],
    %% io:format("L=~p~n", [L]),
    L.
contours_to_objects_f_2(L, Scale)
  when is_float(Scale) ->
    contours_to_objects_f_2(L, Scale, []).
contours_to_objects_f_2([L|ShapeLists], Scale,  OL)
  when is_list(L) ->
    %%io:format("contours_to_objects_f_2 L=~p~n",[L]),
    OL_1 = contours_to_objects_f_2(L, Scale, OL),
    contours_to_objects_f_2(ShapeLists, Scale, OL_1);
contours_to_objects_f_2([{path, []}|ShapeLists], Scale, OL) ->
    contours_to_objects_f_2(ShapeLists, Scale, OL);
contours_to_objects_f_2([{path, L}|ShapeLists], Scale, OL) ->
    Thick = ?DXF_2D_THICKNESS,
    Color = 250,
    TopCoords = lists:reverse([{X*Scale,Y*Scale,Thick} || {X,Y} <- L]),
    TopFace = {face, {TopCoords, Color, "0"}},
    BottomCoords = lists:reverse([{X*Scale,Y*Scale,-Thick} || {X,Y} <- L]),
    BottomFace = {face, {BottomCoords, Color, "0"}},
    OL_1 = [TopFace,BottomFace|OL],
    contours_to_objects_f_2(ShapeLists, Scale, OL_1);
contours_to_objects_f_2([{hollow_out, L}|ShapeLists], Scale, OL) ->
    L_1 = contours_to_objects_wrap(L, Scale),
    %%io:format("L_1=~p~n", [L_1]),
    OL_1 = L_1 ++ OL,
    contours_to_objects_f_2(ShapeLists, Scale, OL_1);
contours_to_objects_f_2([{hollow_in, L}|ShapeLists], Scale, OL) ->
    L_1 = contours_to_objects_wrap(L, Scale),
    OL_1 = L_1 ++ OL,
    contours_to_objects_f_2(ShapeLists, Scale, OL_1);
contours_to_objects_f_2([], _Scale, OL) ->
    OL.

contours_to_objects_wrap([A|_]=L, Scale) ->
    contours_to_objects_wrap(L, Scale, A, []).
contours_to_objects_wrap([A1|[A2|_]=L], Scale, FP, OL) ->
    Thick = ?DXF_2D_THICKNESS,
    F = contours_to_objects_wrap_1(Thick, Scale, A1,A2),
    contours_to_objects_wrap(L, Scale, FP, [F|OL]);
contours_to_objects_wrap([A1], Scale, FP, OL) ->
    Thick = ?DXF_2D_THICKNESS,
    F = contours_to_objects_wrap_1(Thick, Scale, A1,FP),
    [F|OL].
contours_to_objects_wrap_1(Thick, Scale, {A1X,A1Y}, {A2X,A2Y}) ->
    Color = 250,
    V1 = {A1X*Scale,A1Y*Scale,Thick},
    V2 = {A2X*Scale,A2Y*Scale,Thick},
    V3 = {A2X*Scale,A2Y*Scale,-Thick},
    V4 = {A1X*Scale,A1Y*Scale,-Thick},
    {face, {lists:reverse([V1,V2,V3,V4]), Color, "0"}}.
    
    

contours_to_objects_obj(Commands) ->
    contours_to_objects_obj(Commands, []).
contours_to_objects_obj([{face, Tup} | P], Faces) ->
    contours_to_objects_obj(P, [{face, Tup} | Faces]);
contours_to_objects_obj([], []) ->
    unused;
contours_to_objects_obj([], Faces) ->
    case face_vxlist(Faces) of
        [] ->
            unused;
        VxList ->
            FaceVIdxAndMat = face_edge_objects(Faces, VxList),
            He = lists:append([face_all_edges(F) || {F,_Mat} <- FaceVIdxAndMat]),
            
            Mesh = #e3d_mesh{
                type=polygon,
                vs=VxList,
                fs=[#e3d_face{vs=Vs} || {Vs,_Mat} <- FaceVIdxAndMat],
                he=He
            },
            Mesh_1 = e3d_mesh:quadrangulate(Mesh),
            {obj, #e3d_object{obj=Mesh_1}}
    end.
    



%%%
%%%

-type line_coords() :: {{float(), float()}, {float(), float()}}.


-spec line_objects([line_coords()]) -> [[line_coords()]].
line_objects(Lines) ->
    case has_enough_lines(Lines) of
        true ->
            Shapes_1 = start_state(Lines),
            Shapes_2 = generate_missing_points(Shapes_1),
            Shapes_3 = distinct_shapes(Shapes_2),
            line_objects_back_to_list(Shapes_3);
        _ ->
            []
    end.

%% If there is only a lone line just return an empty shape.
%%
has_enough_lines([]) -> false;
has_enough_lines([_]) -> false;
has_enough_lines(_) -> true.


-record(point_arr, {
    ls = array:new() :: array:array(line_coords()),
    ls_shape = array:new() :: array:array(integer()), % which shape number
    count = 0 :: integer()
}).

start_state(Lines) ->
    {STATE, I} = start_state_ln(Lines, 0, #point_arr{}),
    STATE#point_arr{count=I}.
start_state_ln([{{AX1, AY1}, {AX2, AY2}} | R], I, #point_arr{ls=Ls}=STATE) ->
    start_state_ln(R, I+1, STATE#point_arr{
        ls=array:set(I, {{AX1, AY1}, {AX2, AY2}}, Ls)});
start_state_ln([], I, STATE) ->
    {STATE, I}.


-spec coord_same({float(), float()}, {float(), float()}) -> true | false.

coord_same({A1_X,A1_Y}=_A1, {A2_X,A2_Y}=_A2)
  when is_float(A1_X), is_float(A1_Y),
       is_float(A2_X), is_float(A2_Y) ->
    case round(100 * A1_X) == round(100 * A2_X) andalso
         round(100 * A1_Y) == round(100 * A2_Y) of
    true ->
        true;
    false ->
        false
    end.

-spec coord_of({{float(), float()}, {float(), float()}}, integer()) ->  {float(), float()}.

coord_of({A1_XY1, _A1_XY2}=_A1, 0) -> A1_XY1;
coord_of({_A1_XY1, A1_XY2}=_A1, 1) -> A1_XY2.


-spec line_intersect({float(), float()}, {float(), float()}, {float(), float()}, {float(), float()}) -> false | {float(), float()}.

line_intersect(_L1A1, L2A1_L1A2_Same, L2A1_L1A2_Same, _L2A2) -> false;
line_intersect(L2A1_L1A1_Same, _L1A2, L2A1_L1A1_Same, _L2A2) -> false;
line_intersect(L2A2_L1A1_Same, _L1A2, _L2A1, L2A2_L1A1_Same) -> false;
line_intersect(_L1A1, L2A2_L1A2_Same, _L2A1, L2A2_L1A2_Same) -> false;
line_intersect({_,L1A1_Y}=L1A1, {_,L1A2_Y}=L1A2, {_,L2A1_Y}=L2A1, {_,L2A2_Y}=L2A2) ->
    case (L1A1_Y < L1A2_Y) of
    true ->
        LeastY1 = L1A1_Y,
        MostY1 = L1A2_Y;
    false ->
        LeastY1 = L1A2_Y,
        MostY1 = L1A1_Y
    end,
    case (L2A1_Y < L2A2_Y) of
    true ->
        LeastY2 = L2A1_Y,
        MostY2 = L2A2_Y;
    false ->
        LeastY2 = L2A2_Y,
        MostY2 = L2A1_Y
    end,
    line_intersect_2(L1A1, L1A2, L2A1, L2A2, LeastY1,LeastY2,MostY1,MostY2).
line_intersect_2(_, _, _, _, _,LeastY2,MostY1,_)
  when MostY1 < LeastY2 ->
    false;
line_intersect_2(_, _, _, _, LeastY1,_,_,MostY2)
  when MostY2 < LeastY1 ->
    false;
line_intersect_2({L1A1_X,_}=L1A1, {L1A2_X,_}=L1A2,
                 {L2A1_X,_}=L2A1, {L2A2_X,_}=L2A2,
                 LeastY1,LeastY2,MostY1,MostY2) ->
    case (L1A1_X < L1A2_X) of
    true ->
        L1B1 = L1A1,
        L1B2 = L1A2;
    false ->
        L1B1 = L1A2,
        L1B2 = L1A1
    end,
    case (L2A1_X < L2A2_X) of
    true ->
        L2B1 = L2A1,
        L2B2 = L2A2;
    false ->
        L2B1 = L2A2,
        L2B2 = L2A1
    end,
    
    {L1B1_X,L1B1_Y} = L1B1,
    {L1B2_X,L1B2_Y} = L1B2,
    {L2B1_X,L2B1_Y} = L2B1,
    {L2B2_X,L2B2_Y} = L2B2,
    
    case ((L1B2_Y - L1B1_Y) == (L2B2_Y - L2B1_Y)) andalso
         ((L1B2_X - L1B1_X) == (L2B2_X - L2B1_X)) of
    true ->
        false;
        
    false when abs(L1B2_X - L1B1_X) == 0.0 ->
        case abs(L2B2_X - L2B1_X) == 0.0 of
        true ->
            false;
        false ->
            case (L2B1_X < L1B1_X) andalso (L2B2_X > L1B1_X) of
            true ->
                B = (L2B2_Y - L2B1_Y) / (L2B2_X - L2B1_X),
                D = L2B1_Y - (L2B1_X * B),
                MX = L1B1_X,
                MY = B * MX + D,
                line_intersect_y_bound({MX, MY}, LeastY1, MostY1, LeastY2, MostY2);
            false ->
                false
            end
        end;
    false when abs(L2B2_X - L2B1_X) == 0.0 ->
        case (L1B1_X < L2B1_X) andalso (L1B2_X > L2B1_X) of
        true ->
            A = (L1B2_Y - L1B1_Y) / (L1B2_X - L1B1_X),
            C = L1B1_Y - (L1B1_X * A),
            MX = L2B1_X,
            MY = A * MX + C,
            line_intersect_y_bound({MX, MY}, LeastY1, MostY1, LeastY2, MostY2);
        false ->
            false
        end;
    false ->
        A = (L1B2_Y - L1B1_Y) / (L1B2_X - L1B1_X),
        B = (L2B2_Y - L2B1_Y) / (L2B2_X - L2B1_X),
        C = L1B1_Y - (L1B1_X * A),
        D = L2B1_Y - (L2B1_X * B),

        case abs(A - B) == 0.0 of
        true ->
            false;
        false ->
            MX = (D - C) / (A - B),
            MY = A * MX + C,
            
            line_intersect_3(MX, MY, L1B1,L1B2,L2B1,L2B2, LeastY1,LeastY2,MostY1,MostY2)
        end
    end.
line_intersect_3(MX, MY, {L1B1_X,_}, {L1B2_X,_}, {L2B1_X,_}, {L2B2_X,_},
                 LeastY1,LeastY2,MostY1,MostY2) ->
    case MX > L1B1_X andalso MX < L1B2_X
        andalso MX > L2B1_X andalso MX < L2B2_X
    of
        true ->
            line_intersect_y_bound({MX, MY}, LeastY1, MostY1, LeastY2, MostY2);
        false ->
            false
    end.
line_intersect_y_bound({_, MY}=Point, LeastY1, MostY1, LeastY2, MostY2)
  when MY >= LeastY2 andalso MY =< MostY2 andalso
       MY >= LeastY1 andalso MY =< MostY1 ->
    Point;
line_intersect_y_bound(_, _, _, _, _) ->
    false.

-spec point_on_line({float(), float()}, {float(), float()}, {float(), float()}) -> false | true.

%% Find if a point is on a line
%%
point_on_line({_L1A1_X,L1A1_Y}=L1A1, {_L1A2_X,L1A2_Y}=L1A2, {_L2B1_X,_L2B1_Y}=L2B1) ->
    case (L1A1_Y < L1A2_Y) of
    true ->
        LeastY1 = L1A1_Y,
        MostY1 = L1A2_Y;
    false ->
        LeastY1 = L1A2_Y,
        MostY1 = L1A1_Y
    end,
    
    case coord_same(L1A1, L2B1) orelse coord_same(L1A2, L2B1) orelse coord_same(L1A1, L1A2) of
    true ->
        false;
    false ->
        point_on_line_1(L1A1, L1A2, L2B1, LeastY1,MostY1)
    end.
point_on_line_1(L1A1, L1A2, {_L2B1_X,L2B1_Y}=L2B1, LeastY1,MostY1) ->
    case MostY1 < L2B1_Y of
    true ->
        false;
    false ->
        point_on_line_2(L1A1, L1A2, L2B1, LeastY1,MostY1)
    end.
point_on_line_2(L1A1, L1A2, {_L2B1_X,L2B1_Y}=L2B1, LeastY1,MostY1) ->
    case LeastY1 > L2B1_Y of
    true ->
        false;
    false ->
        point_on_line_3(L1A1, L1A2, L2B1, LeastY1,MostY1)
    end.
    
point_on_line_3({L1A1_X,_L1A1_Y}=L1A1, {L1A2_X,_L1A2_Y}=L1A2, {L2B1_X,L2B1_Y}=_, _LeastY1,_MostY1) ->
    case (L1A1_X < L1A2_X) of
    true ->
        L1B1 = L1A1,
        L1B2 = L1A2;
    false ->
        L1B1 = L1A2,
        L1B2 = L1A1
    end,
    {L1B1_X,L1B1_Y}=L1B1,
    {L1B2_X,L1B2_Y}=L1B2,
    
    case abs(L1B2_X - L1B1_X) == 0.0 of
    true ->
        case (L2B1_X == L1B1_X) of
        true ->
            case L1B1_Y == L2B1_Y of
            true ->
                true;
            false ->
                false
            end;
        false ->
            false
        end;
    false -> 
        B = (L1B2_Y - L1B1_Y) / (L1B2_X - L1B1_X),
        D = L1B1_Y - (L1B1_X * B),
        MY = B * L2B1_X + D,
        case (L2B1_X > L1B1_X) andalso (L2B1_X < L1B2_X) of
        true ->
            MY == L2B1_Y;
        false ->
            false
        end
    end.

-spec angle_of_line({float(), float()}, {float(), float()}) -> float().

angle_of_line({A1_X,A1_Y}=_A1, {A2_X,A2_Y}=_A2)
  when is_float(A1_X), is_float(A1_Y), is_float(A2_X), is_float(A2_Y) ->
    case A1_X == A2_X of
    true ->
        case A2_Y > A1_Y of
        true ->
            0.5 * math:pi();
        false ->
            1.5 * math:pi()
        end;
    false ->
        R0 = (A2_Y - A1_Y) / (A2_X - A1_X),
        case abs(R0) == 0.0 of
        true ->
            case A2_X < A1_X of
            true ->
                math:pi();
            false ->
                0.0
            end;
        false ->
            Ang = math:atan(R0),
            case A2_X > A1_X of
            true ->
                Ang;
            false ->
                Ang + math:pi()
            end
        end
    end.

-spec outwards_angle_of_line(float(), float()) -> float().

%% How big the angle from the left hand side (grid system where Y is going up)
outwards_angle_of_line(OAng, XAng)
  when is_float(OAng), is_float(XAng) ->
    RAng = -(XAng - OAng + math:pi()),
    outwards_angle_of_line_1(OAng, XAng, RAng).
outwards_angle_of_line_1(OAng, XAng, RAng)
  when RAng < 0.0 ->
    RAng_1 = RAng + 2 * math:pi(),
    outwards_angle_of_line_1(OAng, XAng, RAng_1);
outwards_angle_of_line_1(_OAng, _XAng, RAng) ->
    RAng.

-spec next_line_ending_from({float(), float()}, {{float(), float()},{float(), float()}}) -> integer().

next_line_ending_from({_A1_X,_A1_Y}=A1, {A2L_XY1,_A2L_XY2}=_A2L) ->
    case coord_same(A1, A2L_XY1) of
        true  -> 1;
        false -> 0
    end.



-spec skip_null_line({{float(), float()}, {float(), float()}}) -> false | true.

skip_null_line({A1L_XY1,A1L_XY2}=_A1L) ->
    case coord_same(A1L_XY1, A1L_XY2) of
        true -> true;
        false -> false
    end.

%%%
%%%

-record(misspnt_r, {
    added_points,
    bcnt
}).

%% Generate missing points for edge crossings,
%% splitting the two edges into four edges
%%
generate_missing_points(STATE) ->
    generate_missing_points_1(STATE, #misspnt_r{added_points = true}).
generate_missing_points_1(#point_arr{count=Count}=STATE,
                          #misspnt_r{added_points=true}=MPL) ->
    MPL_1 = MPL#misspnt_r{added_points=false,bcnt=Count},
    {STATE_1, MPL_2} = generate_missing_points_1_forl_1(STATE, MPL_1),
    generate_missing_points_1(STATE_1, MPL_2);
generate_missing_points_1(STATE, #misspnt_r{added_points=false}=_MPL) ->
    STATE.

    
generate_missing_points_1_forl(I, I2, {#point_arr{ls=Ls,count=Count}=STATE,
                                       #misspnt_r{bcnt=BCnt}=MPL})
  when I2 =< (BCnt-1) ->
    Ls_I = array:get(I, Ls),
    Ls_I2 = array:get(I2, Ls),
    
    {Ls_I_XY1, Ls_I_XY2} = Ls_I,
    {Ls_I2_XY1, Ls_I2_XY2} = Ls_I2, 
    case line_intersect(Ls_I_XY1, Ls_I_XY2, Ls_I2_XY1, Ls_I2_XY2) of
    { _X, _Y } = IntersectPoint ->
        io:format("~p: Point at: ~w~n", [?MODULE, IntersectPoint]),
        MPL_1 = MPL#misspnt_r{added_points = true},
        Ls1 = array:set(I,
                {Ls_I_XY1, IntersectPoint},
                Ls),
        Ls2 = array:set(I2,
                {Ls_I2_XY1, IntersectPoint},
                Ls1),
        Ls3 = array:set(Count,
                {IntersectPoint, Ls_I_XY2},
                Ls2),
        Count1 = Count + 1,
        STATE_4 = STATE#point_arr{
            ls=array:set(Count1,
                {IntersectPoint, Ls_I2_XY2},
                Ls3),
            count=Count1 + 1
        },
        {STATE_4, MPL_1};
        
    false ->
        case generate_missing_points_point_on_line(I, I2, Ls_I_XY1, Ls_I_XY2, Ls_I2_XY1, Ls_I2_XY2) of
            {WhichLine, MPoint1,MPoint2,MPoint3,MPoint4} ->
                MPL_1 = MPL#misspnt_r{added_points = true},
                STATE_1 = STATE#point_arr{
                    ls=array:set(WhichLine, {MPoint1, MPoint2},
                       array:set(Count, {MPoint3, MPoint4}, Ls)),
                    count=Count + 1
                },
                {STATE_1, MPL_1};
            false ->
                generate_missing_points_1_forl(I, I2+1, {STATE, MPL})
        end
    end;
generate_missing_points_1_forl(_I, I2, {_STATE,#misspnt_r{bcnt=BCnt}=_MPL}=FT)
  when I2 > (BCnt-1) ->
    FT.

generate_missing_points_point_on_line(I, I2, Ls_I_XY1, Ls_I_XY2, Ls_I2_XY1, Ls_I2_XY2) ->
    case generate_missing_points_point_on_line_1(I, Ls_I_XY1, Ls_I_XY2, Ls_I2_XY1, Ls_I2_XY2) of
        false ->
            generate_missing_points_point_on_line_1(I2, Ls_I2_XY1, Ls_I2_XY2, Ls_I_XY1, Ls_I_XY2);
        Ret ->
            Ret
    end.
generate_missing_points_point_on_line_1(WhichLine, Ls_I_XY1, Ls_I_XY2, Ls_I2_XY1, Ls_I2_XY2) ->
    case point_on_line(Ls_I_XY1, Ls_I_XY2, Ls_I2_XY1) of true ->
        {WhichLine,
         Ls_I_XY1,
         Ls_I2_XY1,
         Ls_I2_XY1,
         Ls_I_XY2};
        
    false ->
        case point_on_line(Ls_I_XY1, Ls_I_XY2, Ls_I2_XY2) of true ->
            {WhichLine,
             Ls_I_XY1,
             Ls_I2_XY2,
             Ls_I2_XY2,
             Ls_I_XY2};
        false ->
            false
        end
    end.
    


generate_missing_points_1_forl_1(STATE, #misspnt_r{bcnt=_BCnt}=MPL) ->
    generate_missing_points_1_forl_1(STATE, MPL, 0, {}).
generate_missing_points_1_forl_1(STATE,#misspnt_r{bcnt=BCnt}=MPL, I, {})
  when I =< (BCnt-1) ->
    {STATE_1,MPL_1} = generate_missing_points_1_forl(I, I + 1, {STATE, MPL}),
    generate_missing_points_1_forl_1(STATE_1,MPL_1, I+1, {});
generate_missing_points_1_forl_1(STATE,#misspnt_r{bcnt=BCnt}=MPL, I, {})
  when I > (BCnt-1) ->
    {STATE,MPL}.

%%%
%%%

-record(distshp_r, {
    next_shp,
    assigned_shapes,
    all_done,
    use_i = 0 :: integer(),
    use_ls_i3
}).

%% Determine distinct untouching shapes
%%
distinct_shapes(STATE) ->
    DSS = #distshp_r{},
    {STATE_1, DSS_1} = distinct_shapes_forl_1(STATE, DSS),
    DSS_2 = DSS_1#distshp_r{
        next_shp = 0,
        all_done = false,
        use_i = 0
    },
    distinct_shapes(STATE_1, DSS_2).
distinct_shapes(#point_arr{ls_shape=LsShape}=STATE, #distshp_r{all_done=false}=DSS) ->
    I = DSS#distshp_r.use_i,
    case array:get(I,LsShape) =/= -1 of
    true ->
        DSS_1 = DSS#distshp_r{
            assigned_shapes = true
        },
        {STATE_1, DSS_2} = distinct_shapes_1(STATE, DSS_1);
    false ->
        STATE_1 = STATE,
        DSS_2 = DSS
    end,
    #point_arr{count=Count}=STATE_1,
    DSS_3 = DSS_2#distshp_r{
        all_done = true
    },
    {STATE_2, DSS_4} = distinct_shapes_forl(0, Count - 1, {STATE_1, DSS_3}),
    distinct_shapes(STATE_2, DSS_4);
distinct_shapes(STATE, #distshp_r{all_done=true}=_DSS) ->
    STATE.
    
    
distinct_shapes_forl_1(#point_arr{count=Count}=STATE, DSS) ->
    distinct_shapes_forl_1(STATE, DSS, 0, Count, {}).
distinct_shapes_forl_1(#point_arr{ls_shape=LsShape}=STATE, DSS, I, End, {})
  when I =< End ->
    STATE_2 = STATE#point_arr{
        ls_shape=array:set(I, -1, LsShape)
    },
    distinct_shapes_forl_1(STATE_2, DSS, I+1, End, {});
distinct_shapes_forl_1(STATE, DSS, I, End, {})
  when I > End ->
    {STATE, DSS}.

distinct_shapes_forl(I2, End, {#point_arr{ls_shape=LsShape}=STATE, DSS})
  when I2 =< End ->
    case array:get(I2,LsShape) == -1 of
    true ->
        io:format("~p: " ++ ?__(1,"New shape") ++ " ~w~n", [?MODULE, I2]),
        STATE_1 = STATE#point_arr{
            ls_shape=array:set(I2, DSS#distshp_r.next_shp, LsShape)
        },
        DSS_1 = DSS#distshp_r{
            next_shp = DSS#distshp_r.next_shp + 1,
            use_i = I2,
            all_done = false
        },
        {STATE_1, DSS_1};
    false ->
        distinct_shapes_forl(I2+1, End, {STATE, DSS})
    end;
distinct_shapes_forl(I2, End, FT)
  when I2 > End ->
    FT.
    
    
distinct_shapes_1(STATE, #distshp_r{assigned_shapes=true}=DSS) ->
    DSS_1 = DSS#distshp_r{
        assigned_shapes = false
    },
    {STATE_1, DSS_2} = distinct_shapes_1_forl_2(STATE, DSS_1),
    distinct_shapes_1(STATE_1, DSS_2);
distinct_shapes_1(STATE, #distshp_r{assigned_shapes=false}=DSS) ->
    {STATE, DSS}.


distinct_shapes_1_forl_2(#point_arr{count=Count}=STATE, DSS) ->
    distinct_shapes_1_forl_2(STATE, DSS, 0, Count - 1, {}).    
distinct_shapes_1_forl_2(#point_arr{ls_shape=LsShape,ls=Ls}=STATE, DSS, I3, End, {})
  when I3 =< End ->
    DSS_2 = DSS#distshp_r{
        use_ls_i3 = array:get(I3,Ls)
    },
    I = DSS_2#distshp_r.use_i,
    case array:get(I3,LsShape) == array:get(I,LsShape) of
    true ->
        {STATE_2, DSS_3} = distinct_shapes_1_forl_2_3(STATE, DSS_2);
    false ->
        STATE_2 = STATE,
        DSS_3 = DSS
    end,
    distinct_shapes_1_forl_2(STATE_2, DSS_3, I3+1, End, {});
distinct_shapes_1_forl_2(STATE, DSS, I3, End, {})
  when I3 > End ->
    {STATE, DSS}.
    

distinct_shapes_1_forl_2_3(#point_arr{count=Count}=STATE, DSS) ->
    distinct_shapes_1_forl_2_3(STATE, DSS, 0, Count - 1, {}).    
distinct_shapes_1_forl_2_3(#point_arr{ls=Ls,ls_shape=LsShape}=STATE, DSS, I2, End, {})
  when I2 =< End ->
    Ls_I2 = array:get(I2,Ls),
    {Ls_I2_XY1, Ls_I2_XY2} = Ls_I2,
    Ls_I3 = DSS#distshp_r.use_ls_i3,
    {Ls_I3_XY1, Ls_I3_XY2} = Ls_I3,
    case array:get(I2,LsShape) == -1 of
    true ->
        case (coord_same(Ls_I3_XY1, Ls_I2_XY1) orelse
              coord_same(Ls_I3_XY2, Ls_I2_XY1) orelse
              coord_same(Ls_I3_XY1, Ls_I2_XY2) orelse
              coord_same(Ls_I3_XY2, Ls_I2_XY2)) of
        true ->
            DSS_2 = DSS#distshp_r{
                assigned_shapes = true
            },
            I = DSS_2#distshp_r.use_i,
            STATE_2 = STATE#point_arr{
                ls_shape=array:set(I2, array:get(I, LsShape), LsShape)
            };
        false ->
            STATE_2 = STATE,
            DSS_2 = DSS
        end;
    false ->
        STATE_2 = STATE,
        DSS_2 = DSS
    end,
    distinct_shapes_1_forl_2_3(STATE_2, DSS_2, I2+1, End, {});
distinct_shapes_1_forl_2_3(STATE, DSS, I2, End, {})
  when I2 > End ->
    {STATE, DSS}.



-spec line_objects_back_to_list(#point_arr{}) -> [[line_coords()]].
line_objects_back_to_list(#point_arr{}=STATE) ->
    line_objects_back_to_list(STATE, 0, array:new()).
line_objects_back_to_list(#point_arr{count=Count}=_STATE, I, ArrShapes)
  when I =:= Count ->
    array:to_list(ArrShapes);
line_objects_back_to_list(#point_arr{ls=Ls,ls_shape=LsShape,count=Count}=STATE, I, ArrShapes)
  when I < Count ->
    Ln = array:get(I, Ls),
    ShapeNum = array:get(I, LsShape),
    case array:get(ShapeNum, ArrShapes) of
        undefined -> Ls1 = [];
        Ls1 -> Ls1
    end,
    ArrShapes_1 = array:set(ShapeNum, [Ln | Ls1], ArrShapes),
    line_objects_back_to_list(STATE, I+1, ArrShapes_1).


%%%
%%% Fill mode functions
%%%

%% About making hollow parts inside shapes:
%% Detect small "x" drawings (4 lines with one common point, each line 
%% going a different direction) as a special point in the DXF. Use the 
%% special point to remove paths (create a hole) and add an inner contour
%% where the special point is inside of, it should be ignored if the path 
%% to remove touches the outside contour.


-spec fill([line_coords()],[{float(),float()}]) -> [{path, [{float(),float()}]}|[_]].
fill(ShapeLines, EmptyMarkers) ->
    case has_enough_lines_to_fill(ShapeLines) of
        true ->
            %% Fill the lines as a solid mesh.
            ShapeLines_1 = additional_path_lines(ShapeLines),
            {Contour, OtherLines} = find_outside_edge_contours(ShapeLines_1),
            % io:format("Contour=~p OtherLines=~p~n", [Contour, OtherLines]),
            divide_contour_to_paths(Contour, OtherLines, EmptyMarkers);
        _ ->
            []
    end.


%% Check if there are enough lines to at least make a triangle.
%%
has_enough_lines_to_fill([]) ->
    false;
has_enough_lines_to_fill([_]) ->
    false;
has_enough_lines_to_fill(_) ->
    true.
    
%%%
%%%

%% Find small "x" shapes among the distinct shapes, these are used
%% to note the insides of shapes that should be empty.
%%
-spec find_empty_points([[line_coords()]]) ->
    {[[line_coords()]], [{float(),float()}]}.
find_empty_points(Shapes) ->
    find_empty_points(Shapes, [], []).
find_empty_points([S|Shapes], OL, EM)
  when length(S) =:= 4 ->
    case find_empty_points_1(S) of
        {_X,_Y}=Point ->
            find_empty_points(Shapes, OL, [Point|EM]);
        false ->
            find_empty_points(Shapes, [S|OL], EM)
    end;
find_empty_points([S|Shapes], OL, EM) ->
    find_empty_points(Shapes, [S|OL], EM);
find_empty_points([], OL, EM) ->
    {lists:reverse(OL), lists:reverse(EM)}.


find_empty_points_1(SL) ->
    case find_empty_points_center(SL) of
        {ok, Center} ->
            find_empty_points_2(SL, Center);
        false ->
            false
    end.

find_empty_points_center(SL) ->
    find_empty_points_center(SL, dict:new()).
find_empty_points_center([{Same,Same}|_], _SD) ->
    false;
find_empty_points_center([{S1,S2}|SR], SD)
  when S1 =/= S2 ->
    SD_1 = dict:update_counter(S1, 1, SD),
    SD_2 = dict:update_counter(S2, 1, SD_1),
    find_empty_points_center(SR, SD_2);
find_empty_points_center([], SD) ->
    dict:fold(
        fun
            (P, C, _Acc) when C =:= 4 ->
                {ok, P};
            (_, _, Acc) ->
                Acc
        end,
        false, SD).

find_empty_points_2(SL, Center) ->
    find_empty_points_2(SL, Center, array:from_list([false, false, false, false])).
find_empty_points_2([{{CX,CY},{CX,CY}}|_], _Center, _) ->
    false;
find_empty_points_2([{{CX,CY},{X,Y}}|SL], {CX,CY}=Center, Arr) ->
    find_empty_points_2([{{X,Y},{CX,CY}}|SL], Center, Arr);
find_empty_points_2([{{X,Y},{CX,CY}}=S|SL], {CX,CY}=Center, Arr) ->
    if 
        X < CX, Y < CY -> 
            Idx = 0;
        X > CX, Y < CY -> 
            Idx = 1;
        X < CX, Y > CY -> 
            Idx = 2;
        X > CX, Y > CY -> 
            Idx = 3;
        true ->
            Idx = 4
    end,
    find_empty_points_2(SL, Center, array:set(Idx, S, Arr));
find_empty_points_2([], Center, Arr_0) ->
    case array:to_list(Arr_0) of
        Arr_1 when length(Arr_1) =/= 4 ->
            false;
        Arr_1 ->
            find_empty_points_3(Center, Arr_1)
    end.

find_empty_points_3(Center, Arr_1) ->
    case not lists:any(
                fun(false) -> true; (_) -> false end,
                Arr_1)
    of
        true ->
            Center;
        false ->
            false
    end.



%%%
%%% Add lines to points with only one edge so all points form paths
%%%
additional_path_lines(Lines) ->
    Counts = count_common_points(Lines),
    add_if_needed(Lines, Counts).


count_common_points(Lns) ->
    Counts = dict:new(),
    count_common_points(Lns, Counts).
count_common_points([L|Lns], Counts) ->
    {P1, P2} = L,
    Counts_1 = dict:update_counter(P1, 1, Counts),
    Counts_2 = dict:update_counter(P2, 1, Counts_1),
    count_common_points(Lns, Counts_2);
count_common_points([], Counts) ->
    Counts.

%% Add every line coordinates to a dictionary, find the lines that have
%% a coordinate that aren't connected to another line (with coordinate as
%% key, the count value is 1 instead of 2 or more). Add a line to the
%% closest coordinate that has line of sight (no intersection) with that
%% with that coordinate.
%%
add_if_needed(Lns, Counts) ->
    add_if_needed(Lns, Counts, []).
add_if_needed([L|Lns]=ELn, Counts, OL) ->
    {P1, P2} = L,
    {Counts_1, NL_1} = add_if_needed_1(P1, P2, Counts, [ELn, OL]),
    {Counts_2, NL_2} = add_if_needed_1(P2, P1, Counts_1, [ELn, NL_1, OL]),
    OL_1 = NL_1 ++ NL_2 ++ OL,
    add_if_needed(Lns, Counts_2, [L|OL_1]);
add_if_needed([], _Counts, OL) ->
    lists:reverse(OL).
add_if_needed_1(Pnt, OP, Counts, ELine) ->
    {ok, Num} = dict:find(Pnt, Counts),
    case Num =:= 1 of
    true ->
        %% Find a nearby point that is not OP
        PointsAvail = points_with_no_intersecting_lines(Pnt, Counts, ELine),
        {P2, _Closest} = lists:foldl(
            add_if_needed_2(Pnt, OP),
            {none, 10.0e25}, PointsAvail),
        Counts_1 = dict:update_counter(Pnt, 1, Counts),
        Counts_2 = dict:update_counter(P2, 1, Counts_1),
        {Counts_2, [{Pnt,P2}]};
    false ->
        {Counts, []}
    end.
add_if_needed_2(Pnt, OP) ->
    fun
        (P2_0, {_, LRadius}=LPntR)
          when OP =/= P2_0 andalso Pnt =/= P2_0 ->
            NRadius = radius(Pnt, P2_0),
            case NRadius < LRadius of
                true ->
                    {P2_0, NRadius};
                _ -> LPntR
            end;
        (_, LPntR) -> LPntR
    end.
radius({X1,Y1},{X2,Y2}) ->
    XD = abs(X2-X1),
    YD = abs(Y2-Y1),
    math:sqrt(XD*XD + YD*YD).
points_with_no_intersecting_lines(Pnt, Counts, ELine_0) ->
    ELine = lists:append(ELine_0),
    dict:fold(
        fun(CPnt, _V, Points) ->
            case lists:any(
                points_with_no_intersecting_lines_1(Pnt, CPnt),
                ELine)
            of
                true -> Points;
                false -> [CPnt|Points]
            end
        end,
        [], Counts).
points_with_no_intersecting_lines_1(Pnt, CPnt) ->
    fun({EP1,EP2}) ->
        case line_intersect(Pnt, CPnt, EP1, EP2) of
            {_, _} -> true;
            false -> false
        end
    end.

%%%
%%% Divide contour into paths from lines
%%%

%% Determine faces from a contour and some free lines.
%%
%% First get the whole shape as a path (the contour).
%% Recursively subdivide the shape in two shapes as there are lines that are
%% found to be inside of the current shape. On return, each side returns a 
%% list of of faces where there are no more lines contained within the shape.
%%
%%
divide_contour_to_paths(ContLines, InLines, EmptyMarkers) ->
    %%io:format("divide_contour_to_paths~n", []),
    Path=lines_to_path(ContLines),
    Around = {hollow_out, Path},
    PathList = divide_shape(Path, InLines, EmptyMarkers),
    [Around,PathList].

lines_to_path([First|ListR]) ->
    {P1, P2} = lines_to_path_which_first(First, ListR),
    D = gb_trees:from_orddict(orddict:from_list(ListR)),
    lines_to_path(ListR, D, [P1], P2, P1).
lines_to_path(_List, _D, PL, PF, PF) ->
    PL; % Need to be reversed for divide_shape/3
lines_to_path([{P2,P3}|List], D, PL, P2, PF)
  when P2 =/= PF ->
    lines_to_path(List, D, [P2|PL], P3, PF);
lines_to_path([{P3,P2}|List], D, PL, P2, PF)
  when P2 =/= PF ->
    lines_to_path(List, D, [P2|PL], P3, PF);
lines_to_path([_|List], D, PL, P2, PF)
  when P2 =/= PF ->
    {value, P3} = gb_trees:lookup(P2, D),
    lines_to_path(List, D, [P2|PL], P3, PF).
lines_to_path_which_first({P1, P2}=_, List) ->
    [{O1,O2}|_] = lists:reverse(List),
    case P1 =:= O1 orelse P1 =:= O2 of
        true ->
            {P1, P2};
        false ->
            {P2, P1}
    end.
    
%% Divide the shape into paths while there are free lines within.
%% Contour is entered in reverse order to the order from
%% find_outside_edge_contours/1 which is done by lines_to_path/1
%%
divide_shape(Contour, FreeLines, EmptyMarkers) ->
    %% io:format("~nContour=~p~nFreeLines=~p~n", [Contour, FreeLines]),
    case find_div(Contour, FreeLines) of
        {L, FreeLines_1} ->
            {Contour_1A, Contour_1B} = div_2(Contour, L),
            {FreeLines_1A, FreeLines_2} =
                within(Contour_1A, FreeLines_1),
            {FreeLines_1B, _} =
                within(Contour_1A, FreeLines_2),
            %% Divide into two
            lists:flatten([
                divide_shape(Contour_1A, FreeLines_1A, EmptyMarkers),
                divide_shape(Contour_1B, FreeLines_1B, EmptyMarkers) ]);
        {_} ->
            divide_shape_1(Contour, EmptyMarkers)
    end.
divide_shape_1(Contour, EmptyMarkers) ->
    %% Now that the shape has no more free lines within, check if there
    %% are empty markers within the contour of the current shape.
    %% If there is, change this to a hollow_in path and don't subdivide further.
    %% If not, further subdivide into convex paths.
    case check_empty_markers(Contour, EmptyMarkers) of
        true ->
            {hollow_in, Contour};
        false ->
            %% further subdivide into convex paths.
            divide_shape_further(Contour)
    end.


%% Go through the list of empty markers, and return true if there are
%% any within the contour.
%%
check_empty_markers(Contour, EmptyMarkers) ->
    lists:any(
        fun(EMarker) ->
            within_inside(EMarker, Contour)
        end,
        EmptyMarkers).
    
%% Divide a path further into subpaths when there are no free lines left
%% and the shape might have concave angles.
%%
divide_shape_further(Contour)
  when length(Contour) < 3 ->
    %% Triangles
    {path, Contour};
divide_shape_further(Contour) ->
    div_concave(Contour).

div_2(Contour, L) ->
    %%io:format("Contour=~p L=~p~n", [Contour, L]),
    [A1|R] = L,
    [A2|_] = lists:reverse(R),
    div_2(Contour, L, A1, A2, [], []).

div_2([P|Contour], L, A1, A2, OL, O2)
  when P =:= A1 ->
    div_2(Contour, L, A1, A2, [], [lists:reverse(L) ++ OL|O2]);
div_2([P|Contour], L, A1, A2, OL, O2)
  when P =:= A2 ->
    div_2(Contour, L, A1, A2, [], [L ++ OL|O2]);
div_2([P|Contour], L, A1, A2, OL, O2) ->
    div_2(Contour, L, A1, A2, [P|OL], O2);
div_2([], _L, _, _, [], [O1,O2]) ->
    {lists:reverse(O1), lists:reverse(O2)};
div_2([], _L, _, _, [], [O1,O2,O3]) ->
    {lists:reverse(O2), lists:reverse(O1 ++ O3)};
div_2([], _L, _, _, [], _List) ->
    error;
div_2([], L, A1, A2, OL, O2) ->
    div_2([], L, A1, A2, [], [OL|O2]).
    

within(Contour, FreeLines) ->
    lists:partition(within_1(Contour), FreeLines).
within_1(Contour) ->
    Cont = sets:from_list(Contour),
    fun({P1,P2}) ->
        %%io:format("within Contour=~p~n  Points=~p~n", [Contour, {P1,P2}]),
        within_2(P1, Cont, Contour) andalso
        within_2(P2, Cont, Contour)
    end.
within_2(Pnt, Cont, Contour) ->
    case sets:is_element(Pnt, Cont) of
        true -> true;
        false ->
            within_inside(Pnt, Contour)
    end.
within_inside(Pnt, Contour) ->
    within_inside(Pnt, Contour, []).
within_inside(Pnt, [C1|_]=Contour, OL) ->
    within_inside(Pnt, C1, Contour, OL).
within_inside(Pnt, C2, [C1]=_Contour, OL) ->
    L1 = [W || W <- [within_inside_2(Pnt, C1, C2)|OL], W =/= false],
    L2 = [W || {_,W} <- L1],
    L3 = orddict:to_list(lists:foldl(
        fun(W, Ac) ->
            orddict:update_counter(W,1,Ac)
        end, orddict:new(), L2)),
    case L3 of
        [{_,_Count}] ->
            false;
        [{_,Count1},{_,Count2}]
          when (Count1 rem 2) =:= 1, (Count2 rem 2) =:= 1 ->
            true;
        _ ->
            false
    end;
within_inside(Pnt, First, [C1|[C2|_]=Contour], OL) ->
    within_inside(Pnt, First, Contour, [within_inside_2(Pnt, C1, C2)|OL]).
within_inside_2({_PX,PY}=Pnt, C1, C2) ->
    {_C1X,C1Y} = C1,
    {_C2X,C2Y} = C2,
    case C1Y < PY andalso C2Y >= PY of
        true ->
            Cmp_1 = comp_x(C1,C2,Pnt),
            %%io:format(" Cmp_1 ~p ~p ~p ~p~n", [C1,Pnt,C2,Cmp_1]),
            {1,Cmp_1};
        false ->
            case C1Y >= PY andalso C2Y < PY of
                true ->
                    Cmp_2 = comp_x(C1,C2,Pnt),
                    %%io:format(" Cmp_2 ~p ~p ~p ~p~n", [C1,Pnt,C2, Cmp_2]),
                    {-1,Cmp_2};
                false ->
                    false
            end
    end.
comp_x({C1X,_}, {C2X,_}, {PX,_})
  when C1X < PX, C2X < PX ->
    right;
comp_x({C1X,_}, {C2X,_}, {PX,_})
  when C1X > PX, C2X > PX ->
    left;
comp_x({C1X,_}, {C2X,_}, {PX,_})
  when C1X =:= C2X ->
    case C1X < PX of
        true -> left;
        _ -> right
    end;
comp_x({C1X,C1Y}=_C1, {C2X,C2Y}=_C2, {PX,PY}=_Pnt) ->
    RoR = (C2Y - C1Y) / (C2X - C1X),
    WhereY = RoR * (PX - C1X) + C1Y,
    case WhereY > PY of
        false ->
            %%io:format("RoR=~p~nWhereY >=~p ~p~n", [RoR, WhereY, PY]),
            right;
        true ->
            %i%o:format("RoR=~p~nWhereY <=~p ~p~n", [RoR, WhereY, PY]),
            left
    end.


find_div(Contour, FreeLines) ->
    Cont = sets:from_list(Contour),
    find_div(Cont, FreeLines, []).

find_div(Cont, [L|FreeLines], UL) ->
    %%io:format("find_div~n", []),
    {P1, P2} = L,
    case {sets:is_element(P1, Cont),
          sets:is_element(P2, Cont)} of
    {false, false} ->
        find_div(Cont, FreeLines, [L|UL]);
    {true, true} ->
        {[P1, P2], FreeLines};
    {true, _} ->
        find_div_1(Cont, P2, FreeLines ++ UL, [], [P2, P1]);
    {_, true} ->
        find_div_1(Cont, P1, FreeLines ++ UL, [], [P1, P2])
    end;
find_div(_Cont, [], _UL) ->
    {[]}.

find_div_1(Cont, LP, [L|FreeLines], UL, BL) ->
    %% io:format("find_div_1~n", []),
    {P1, P2} = L,
    case {LP =:= P1, LP =:= P2,
          sets:is_element(P1, Cont) orelse
          sets:is_element(P2, Cont)} of
    {false, false, _} ->
        find_div_1(Cont, LP, FreeLines, [L|UL], BL);
    
    {true, false, true} ->
        {lists:reverse([P2|BL]), FreeLines ++ UL};
    {false, true, true} ->
        {lists:reverse([P1|BL]), FreeLines ++ UL};
    
    {true, false, false} ->
        find_div_1(Cont, P2, FreeLines ++ UL, [], [P2|BL]);
    {false, true, false} ->
        find_div_1(Cont, P1, FreeLines ++ UL, [], [P1|BL])
    end;
find_div_1(_Cont, _LP, [], AL, BL) ->
    {AL ++ BL}.


%%%
%%%

p_angles(P1, P1, _) -> 0.0;
p_angles(P1, P2, P3) ->
    Ang = outwards_angle_of_line(
            angle_of_line(P1, P2),
            angle_of_line(P2, P3)),
    Ang * (180.0 / math:pi()).
    
get_angles([A|_]=B) ->
    get_angles(A, B, A, []).
get_angles(A, [B], F, OL) ->
    lists:reverse([{B, p_angles(A,B,F)}|OL]);
get_angles(A, [B|[C0|_]=C], F, OL) ->
    get_angles(B, C, F, [{B, p_angles(A,B,C0)}|OL]).

div_concave(Path) ->
    case div_concave_1(Path) of
        {Path} -> {path, Path};
        {Path1, Path2} ->
            [div_concave(Path1), div_concave(Path2)]
    end.

div_concave_1(Path) ->
    ELines = div_concave_path_to_lines(Path),
    Angles = get_angles(Path),
    %%io:format("Angles=~p~n", [Angles]),
    div_concave_1(Angles, ELines, []).

div_concave_1([{P,Ang}|Angles], ELines, OL)
  when Ang > 180.0 ->
    Input = [W || {W,_} <- Angles] ++ lists:reverse(OL),
    case closest_points(Input, P, ELines) of
        {[P0|_]=Angles_1, Angles_R, _} when length(Angles_1) >= 2, length(Angles_R) >= 1 ->
            Path1 = lists:reverse(Angles_1 ++ [P]),
            Path2 = Angles_R ++ [P,P0],
            %%io:format("Input=~p~nPath1=~p~nPath2=~p~n~n~n", [Input, Path1, Path2]),
            {Path1, Path2};
        _ ->
            div_concave_1(Angles, ELines, [P|OL])
    end;
div_concave_1([{P,_Ang}|Angles], ELines, OL) ->
    div_concave_1(Angles, ELines, [P|OL]);
div_concave_1([], _, OL) ->
    {lists:reverse(OL)}.

closest_points(Angles, Point, ELines) ->
    [A|Angles_1] = lists:reverse(Angles),
    closest_points_1(Angles_1, [A], Point, ELines,
        l_radius(Angles_1, Point, ELines, [A])).


closest_points_1([_], _OL, _Point, _ELines, LL) ->
    LL;
closest_points_1([Pnt2=A|AnglesR]=Angles, [_|_]=OL, Point, ELines, LL)
  when Point =/= Pnt2 ->
    case LL of
        false ->
            LRadius = 10.0e24;
        {_, _, LRadius} ->
            LRadius
    end,
    case l_radius(Angles, Point, ELines, OL) of
        {_, _, CRadius} = CL when CRadius < LRadius ->
            closest_points_1(Angles, OL, Point, ELines, CL);
        _ ->
            closest_points_1(AnglesR, [A|OL], Point, ELines, LL)
    end;
closest_points_1([A|Angles], OL, Point, ELines, LL) ->
    closest_points_1(Angles, [A|OL], Point, ELines, LL).

l_radius([P1|_]=Angles_1, Point, ELines, OL) ->
    case lists:any(l_radius_intersect(P1, Point), ELines) of
        true ->
            false;
        false ->
            Radius = radius(P1,Point),
            %%io:format("Radius=~p~n", [Radius]),
            {Angles_1, OL, Radius}
    end.
l_radius_intersect(P1, P2) -> 
    fun({E1,E2}) ->
        case line_intersect(P1, P2, E1,E2) of
        {X, Y} when is_float(X), is_float(Y) ->
            %%io:format("{~p,~p}-{~p,~p} intersect X=~p Y=~p~n", [P1, P2, E1,E2, X, Y]),
            true;
        false ->
            false
        end
    end.

div_concave_path_to_lines([P|_]=A) ->
    div_concave_path_to_lines(A, P, []).
div_concave_path_to_lines([P1|[P2|_]=A], F, OL) ->
    div_concave_path_to_lines(A, F, [{P1,P2}|OL]);
div_concave_path_to_lines([P1], P2, OL) ->
    lists:reverse([{P1,P2}|OL]).


%%%
%%%

-record(outsedge_r, {
    original_line = 0 :: integer(),
    current_line = 0 :: integer(),
    current_line_ending = 0 :: integer(),
    current_coord :: {float(), float()}|'undefined',
        
    current_line_angle = 0.0 :: float(),
        
    left_most_next_line :: integer()|'undefined',
    left_most_next_line_ending :: integer()|'undefined',
    left_most_next_line_outward_angle :: float()|'undefined',
        
    left_most_next_line_actual_angle :: float()|'undefined',
    have_next_line :: boolean()|'undefined',
    outward_angle = 0.0 :: float(),
    maximum_walk :: integer()|'undefined',
    found_other_line :: boolean()|'undefined',
    ls_visited = array:new() :: array:array(boolean()),
    has_one_convex_angle_line :: boolean()|'undefined',
    has_one_convex_angle_line_angle1 :: float()|'undefined',
    has_one_convex_angle_line_angle2 :: float()|'undefined',
    
    smallest_x :: float(),
    smallest_x_other_end :: float()
}).
-record(arrshape, {
    ls = array:new() :: array:array({{float(), float()}, {float(), float()}}),
    ls_outside = array:new() :: array:array(integer()), % outside edges 0 = no, 1 = yes
    ls_contour = [] :: [integer()],
    count = 0 :: integer()
}).

%% Get the top left most vertice
%% Follow a path along the left (when Y is going up)
%%
find_outside_edge_contours(Lines) ->
    #arrshape{count=Count}=STATE=find_outside_edge_contours_1(Lines),
    %%io:format("ls_outside=~p", [array:to_list(STATE#arrshape.ls_outside)]),
    find_outside_edge_contours_after(STATE, 0, Count - 1, []).
find_outside_edge_contours_after(#arrshape{ls=Ls,ls_outside=LsO}=STATE, I, End, EL)
  when I =< End ->
    Ls_I = array:get(I, Ls),
    case array:get(I, LsO) of
        1 ->
            find_outside_edge_contours_after(
                STATE, I+1, End, EL);
        _ ->
            find_outside_edge_contours_after(
                STATE, I+1, End, [Ls_I | EL])
    end;
find_outside_edge_contours_after(STATE, I, End, EL)
  when I > End ->
    { find_outside_edge_contours_after_1(STATE),
      lists:reverse(EL) }.
find_outside_edge_contours_after_1(#arrshape{ls_contour=LsC,ls=Ls}=_) ->
    find_outside_edge_contours_after_1(LsC, Ls, []).
find_outside_edge_contours_after_1([I|LsC], Ls, OL) ->
    Ls_I = array:get(I, Ls),
    find_outside_edge_contours_after_1(LsC, Ls, [Ls_I|OL]);
find_outside_edge_contours_after_1([],_Ls,OL) ->
    OL.

find_outside_edge_contours_1(Lines) ->

    STATE = find_outside_edge_contours_start(Lines),

    %% Find the left-most point from the perspective of the current point
    OSE = #outsedge_r{
        smallest_x=1.0e25,
        smallest_x_other_end=0.0,
        current_line_angle=math:pi() / 2.0
    },
    {#arrshape{ls=Ls,count=Count}=STATE_1, #outsedge_r{current_line=CurrLine}=OSE_1} = 
        find_outside_edge_contours_5(STATE, OSE),
    
    Ls_CurrentLine = array:get(CurrLine,Ls),
    {Ls_CurrentLine_XY1, Ls_CurrentLine_XY2} = Ls_CurrentLine,
    {_Ls_CurrentLine_XY1_X, Ls_CurrentLine_XY1_Y} = Ls_CurrentLine_XY1,
    {_Ls_CurrentLine_XY2_X, Ls_CurrentLine_XY2_Y} = Ls_CurrentLine_XY2,
    
    OSE_2 = OSE_1#outsedge_r{
        current_line_ending=
            case (Ls_CurrentLine_XY1_Y > Ls_CurrentLine_XY2_Y) of
                true -> 0;
                false -> 1
            end,
        original_line = CurrLine
    },
    %% A maximum walk of the number of lines we will try.
    %% if the algorithm doesn't finish in MaxWalk return an error.
    MaxWalk = Count + 1,
    OSE_3 = OSE_2#outsedge_r{
        maximum_walk=MaxWalk,
        have_next_line=true
    },
    find_outside_edge_contours_forl(0, MaxWalk, {STATE_1, OSE_3}).

find_outside_edge_contours_start(Lines) ->
    {STATE, I} = find_outside_edge_contours_start_ln(Lines, 0, #arrshape{}),
    STATE#arrshape{count=I}.
find_outside_edge_contours_start_ln([{{AX1, AY1}, {AX2, AY2}} | R], I, #arrshape{ls=Ls}=STATE) ->
    find_outside_edge_contours_start_ln(R, I+1, STATE#arrshape{ls=array:set(I, {{AX1, AY1}, {AX2, AY2}}, Ls)});
find_outside_edge_contours_start_ln([], I, STATE) ->
    {STATE, I}.


find_outside_edge_contours_5(STATE, OSE) ->
    find_outside_edge_contours_5_forl_1(STATE, OSE).
find_outside_edge_contours_5_forl_1(#arrshape{count=Count}=STATE, OSE) ->
    find_outside_edge_contours_5_forl_1(STATE, OSE, 0, Count - 1, {}).
find_outside_edge_contours_5_forl_1(#arrshape{ls=Ls,ls_outside=LsO_0}=STATE, OSE, I, End, {})
  when I =< End ->
    Ls_I = array:get(I, Ls),
    {Ls_I_XY1, Ls_I_XY2} = Ls_I,
    {Ls_I_XY1_X, _Ls_I_XY1_Y} = Ls_I_XY1,
    {Ls_I_XY2_X, _Ls_I_XY2_Y} = Ls_I_XY2,
    STATE_2 = STATE#arrshape{
        ls_outside=array:set(I, 0, LsO_0)
    },
    {STATE_3, OSE_2} = find_outside_edge_contours_5_2(
        Ls_I_XY2_X, Ls_I_XY1_X, I, 
        find_outside_edge_contours_5_1(
            Ls_I_XY1_X, Ls_I_XY2_X, I, 
            {STATE_2, OSE})),
    find_outside_edge_contours_5_forl_1(STATE_3, OSE_2, I+1, End, {});
find_outside_edge_contours_5_forl_1(STATE, OSE, I, End, {})
  when I > End ->
    {STATE, OSE}.

find_outside_edge_contours_5_1(Ls_I_XY1_X, Ls_I_XY2_X, I,
  {STATE, #outsedge_r{smallest_x=SmX,smallest_x_other_end=SmX2}=OSE})
  when Ls_I_XY1_X =< SmX ->
    case (Ls_I_XY1_X < SmX) orelse
         (Ls_I_XY2_X < SmX2) of
    true ->
        OSE_1 = OSE#outsedge_r{
            smallest_x=Ls_I_XY1_X,
            smallest_x_other_end=Ls_I_XY2_X,
            current_line=I,
            current_line_ending=0
        },
        {STATE, OSE_1};
    false ->
        {STATE, OSE}
    end;
find_outside_edge_contours_5_1(_Ls_I_XY1_X, _Ls_I_XY2_X, _I, {STATE, OSE}) ->
    {STATE, OSE}.

find_outside_edge_contours_5_2(Ls_I_XY2_X, Ls_I_XY1_X, I,
  {STATE, #outsedge_r{smallest_x=SmX,smallest_x_other_end=SmX2}=OSE})
  when Ls_I_XY2_X =< SmX ->
    case (Ls_I_XY2_X < SmX) orelse (Ls_I_XY1_X < SmX2) of
    true ->
        OSE_1 = OSE#outsedge_r{
            smallest_x=Ls_I_XY2_X,
            smallest_x_other_end=Ls_I_XY1_X,
            current_line=I,
            current_line_ending=1
        },
        {STATE, OSE_1};
    false ->
        {STATE, OSE}
    end;
find_outside_edge_contours_5_2(_Ls_I_XY2_X, _Ls_I_XY1_X, _I,
    {STATE, OSE}) ->
    {STATE, OSE}.

find_outside_edge_contours_forl(IX, End, {STATE, #outsedge_r{have_next_line=false}=_OSE})
  when IX =< End ->
    STATE;
find_outside_edge_contours_forl(IX, End, {_STATE, #outsedge_r{maximum_walk=0}=_OSE})
  when IX =< End ->
    io:format("~p: find_outside_edge_contours: " ++
        ?__(1,"ERROR: Maximum walk has been reached.") ++ "~n", [?MODULE]),
    %% Show Error
    {error, maximum_walk_reached};
find_outside_edge_contours_forl(IX, End, {
    #arrshape{ls=Ls,ls_outside=LsO_0,ls_contour=LsC_0}=STATE,
    #outsedge_r{current_line=CurrLine,current_line_ending=CurrLnE,current_line_angle=_CurrLnA}=OSE})
  when IX =< End ->
    Ls_CurrentLine = array:get(CurrLine, Ls),
    OSE_1 = OSE#outsedge_r{
        left_most_next_line=0,
        left_most_next_line_ending=0,
        left_most_next_line_outward_angle=1.0e25,
        current_coord=coord_of(Ls_CurrentLine, CurrLnE)
    },
    STATE_1 = STATE#arrshape{
        ls_outside=array:set(CurrLine, 1, LsO_0),
        ls_contour=[CurrLine | LsC_0]
    },
    
    % io:format("~n", []),
    % io:format("current_line=~w  current_line_angle=~w~n", [CurrLine, CurrLnA]),
    % io:format("Candidates:~n", []),
    %% Has one line that is within 180 degrees inward of the previous line
    #outsedge_r{outward_angle=_OutwardAngle,current_line_angle=CurrLnA_2}=OSE_2 = OSE_1#outsedge_r{
        has_one_convex_angle_line=false,
        found_other_line=false
    },
    {STATE_2, #outsedge_r{has_one_convex_angle_line=HasOneConvexAngle,outward_angle=OutwardAngle_1}=OSE_3} =
        find_outside_edge_contours_forl_2(STATE_1, OSE_2),
    case not HasOneConvexAngle of
    true ->
        A_Ang1 = CurrLnA_2 - math:pi(),
        A_Ang2 = CurrLnA_2 - ((math:pi() * 2) - OutwardAngle_1),
        
        LnAng1 = angle_over_zero(A_Ang1),
        LnAng2 = angle_over_zero(A_Ang2),
        #outsedge_r{current_coord=_CurrCoord_4}=OSE_4 = OSE_3#outsedge_r{
            has_one_convex_angle_line_angle1=LnAng1,
            has_one_convex_angle_line_angle2=LnAng2
        },
        %{CurrentCoord_X, CurrentCoord_Y} = CurrCoord_4,
        STATE_3 = STATE_2,
        #outsedge_r{found_other_line=FoundOtherLine}=OSE_5 = OSE_4;
    false ->
        STATE_3 = STATE_2,
        #outsedge_r{found_other_line=FoundOtherLine}=OSE_5 = OSE_3
    end,
    OSE_6 = case not FoundOtherLine of
    true ->
        %% Could not find another line to go to so back track by reversing the
        %% cursor line ending.
        OSE_5#outsedge_r{
            current_line_ending=
                case (OSE_5#outsedge_r.current_line_ending == 0) of
                    true -> 1;
                    false -> 0
                end
        };
    false ->
        OSE_5
    end,
    OSE_7 = case reached_original_line(OSE_6) of
    true ->
        %% We've reached the original line.
        OSE_6#outsedge_r{
            have_next_line=false
        };
    false ->
        next_current_line(OSE_6)
    end,
    OSE_8 = OSE_7#outsedge_r{
        maximum_walk=OSE_7#outsedge_r.maximum_walk - 1
    },
    
    find_outside_edge_contours_forl(IX+1, End, {STATE_3, OSE_8});
find_outside_edge_contours_forl(IX, End, {STATE, _}=_FT)
  when IX > End ->
    STATE.
reached_original_line(#outsedge_r{left_most_next_line=LeftMostNextLine,
  original_line=OriginalLine}=_) ->
    LeftMostNextLine == OriginalLine.
next_current_line(#outsedge_r{left_most_next_line=LeftMostNextLine,
  left_most_next_line_ending=LeftMostNextLE,
  left_most_next_line_actual_angle=LeftMostNextLAng,ls_visited=LsVisited}=OSE) ->
    CurrentLine = LeftMostNextLine,
    OSE#outsedge_r{
        current_line=CurrentLine,
        current_line_ending=LeftMostNextLE,
        current_line_angle=LeftMostNextLAng,
        ls_visited=array:set(CurrentLine, true, LsVisited)
    }.
find_outside_edge_contours_forl_2(#arrshape{count=Count}=STATE, OSE) ->
    find_outside_edge_contours_forl_2(STATE, OSE, 0, Count - 1, {}).    
find_outside_edge_contours_forl_2(#arrshape{ls=Ls}=STATE,
    #outsedge_r{current_line=CurrLine,current_line_angle=CurrLnA,current_coord=CurrCoord,ls_visited=LsV}=OSE,
    I2, End, {})
  when I2 =< End ->
    Ls_I2 = array:get(I2,Ls),
    {Ls_I2_XY1, Ls_I2_XY2} = Ls_I2,

    OSE_4 = case not skip_null_line(Ls_I2) of
    true ->
        case (I2 =/= CurrLine) andalso
             (array:get(I2, LsV) =/= true) andalso
             (coord_same(CurrCoord, Ls_I2_XY1) orelse
                 coord_same(CurrCoord, Ls_I2_XY2)) of
        true ->
            NextPossibleLineEnding = next_line_ending_from(CurrCoord, Ls_I2),
            NextPossibleCoord = coord_of(Ls_I2, NextPossibleLineEnding),
            NextPossibleAng = angle_of_line(CurrCoord, NextPossibleCoord),
            OutwardAngle = outwards_angle_of_line(CurrLnA, NextPossibleAng),
            #outsedge_r{has_one_convex_angle_line=HasOneConvexAngleLine_0,left_most_next_line_outward_angle=LMNLOutwardAngle}=OSE_2 = OSE#outsedge_r{
                outward_angle=OutwardAngle
            },
            % io:format("  I2=~w   (~w,~w)  ~w OAngle=~w~n", [I2, CurrCoord,NextPossibleCoord, NextPossibleAng, OutwardAngle]),
            case OutwardAngle > math:pi() of
            true ->
                %% There is at least one line within 180 degrees inward so there
                %% is no need to add another edge.
                HasOneConvexAngleLine_1 = true;
            false ->
                HasOneConvexAngleLine_1 = HasOneConvexAngleLine_0
            end,
            
            case OutwardAngle < LMNLOutwardAngle of
            true ->
                OSE_3 = OSE_2#outsedge_r{
                    left_most_next_line=I2,
                    left_most_next_line_ending=NextPossibleLineEnding,
                    left_most_next_line_actual_angle=NextPossibleAng,
                    left_most_next_line_outward_angle=OutwardAngle
                };
            false ->
                OSE_3 = OSE_2
            end,
            OSE_3#outsedge_r{
                found_other_line=true,
                has_one_convex_angle_line=HasOneConvexAngleLine_1
            };
        false ->
            OSE
        end;
    false ->
        OSE
    end,
    find_outside_edge_contours_forl_2(STATE, OSE_4, I2+1, End, {});
find_outside_edge_contours_forl_2(STATE, OSE, I2, End, {})
  when I2 > End ->
    {STATE, OSE}.

angle_over_zero(Ang)
  when Ang < 0 ->
    Ang_1 = Ang + (math:pi() * 2),
    angle_over_zero(Ang_1);
angle_over_zero(Ang) -> Ang.


%%%
%%%

dxf_units(N) ->
    case N of
        0 -> none; % actually unitless
        1 -> in;
        2 -> ft;
        3 -> mi;
        4 -> mm;
        5 -> cm;
        6 -> meter;
        7 -> km;
        8 -> microinch;
        9 -> thou; % 1000th of an inch
        10 -> yd;
        11 -> ang;
        12 -> nm;
        13 -> micron;
        14 -> dm;
        15 -> decameter;
        16 -> hectometer;
        17 -> gm; % gigameter
        18 -> au; % astronomical unit
        19 -> ly; % light year
        20 -> parsec;
        _ -> none
    end.
    
dxf_unit_int(Atom) ->
    case Atom of
        in -> 1;
        ft -> 2;
        mi -> 3;
        mm -> 4;
        cm -> 5;
        meter -> 6;
        km -> 7;
        microinch -> 8;
        thou -> 9; % 1000th of an inch
        yd -> 10;
        ang -> 11;
        nm -> 12;
        micron -> 13;
        dm -> 14;
        decameter -> 15;
        hectometer -> 16;
        gm -> 17; % gigameter
        au -> 18; % astronomical unit
        ly -> 19; % light year
        parsec -> 20;
        _ -> 0
    end.

dxf_drw_unit_int(Atom) ->
    case lists:member(Atom,
            [mm,cm,meter,km,ang,nm,micron,dm,
             decameter,hectometer,gm,au,ly,parsec]) of
        true ->
            1;
        false ->
            0
    end.


scale_objects(Objs, ScaleF) ->
    [ scale_objects_1(Obj, ScaleF) || Obj <- Objs].
scale_objects_1(#e3d_object{obj=#e3d_mesh{vs=Vs}=Mesh}=Obj, Scl) ->
    Obj#e3d_object{obj=Mesh#e3d_mesh{vs=[{X*Scl,Y*Scl,Z*Scl} || {X,Y,Z} <- Vs]}}.


%%%
%%% Unit conversion
%%%

scale_from_units(export, {WU, DXFUnit}) ->
    unit_ratio(WU, DXFUnit);
scale_from_units(import, Units) ->
    1.0 / scale_from_units(export, Units).

unit_scaled_mm(micron) ->     0.001 * unit_scaled_mm(mm);
unit_scaled_mm(mm) ->           1.0;
unit_scaled_mm(cm) ->          10.0 * unit_scaled_mm(mm);
unit_scaled_mm(dm) ->         100.0 * unit_scaled_mm(mm);
unit_scaled_mm(meter)  ->    1000.0 * unit_scaled_mm(mm);

unit_scaled_mm(in) ->           (1.0 / 0.03937008) * unit_scaled_mm(mm);
unit_scaled_mm(ft) ->          12.0 * unit_scaled_mm(in);
unit_scaled_mm(yd) ->           3.0 * unit_scaled_mm(ft);
unit_scaled_mm(gm) ->           1000000.0 * unit_scaled_mm(km); %Gigameter
unit_scaled_mm(km) ->           1000.0 * unit_scaled_mm(meter);  %Kilometer
unit_scaled_mm(hectometer) ->   100.0 * unit_scaled_mm(meter); %Hectometer
unit_scaled_mm(decameter) ->    10.0 * unit_scaled_mm(meter); %Decameter
unit_scaled_mm(nm) ->           0.000001 * unit_scaled_mm(mm); %Nanometer
unit_scaled_mm(ang) ->          0.1 * unit_scaled_mm(nm); %Angstrom
unit_scaled_mm(mi) ->           5280.0 * unit_scaled_mm(ft); %Mile
unit_scaled_mm(thou) ->         (1.0 / 1000.0) * unit_scaled_mm(in); %Thou (Mils)
unit_scaled_mm(microinch) ->    (1.0 / 1000.0) * unit_scaled_mm(thou); %Micro-inch
unit_scaled_mm(parsec) ->       3.26 * unit_scaled_mm(ly); %Parsec
unit_scaled_mm(ly) ->           9460730472580.8 * unit_scaled_mm(km); %Light year
unit_scaled_mm(au) ->           149597870.7 * unit_scaled_mm(km). %Astronomical Unit

unit_ratio(Unit1, Unit2)
  when Unit1 =:= Unit2 ->
    1.0;
unit_ratio(Unit1, Unit2) ->
    unit_scaled_mm(Unit1) / unit_scaled_mm(Unit2).



%%%
%%%

%% Add material colors to faces if there are no vertex color information
%%
mat_to_col(#e3d_file{objs=Objs,mat=MatList1}=Cont) ->
    Cont#e3d_file{objs=[mat_to_col_1(O, MatList1) || O <- Objs]}.
mat_to_col_1(#e3d_object{obj=#e3d_mesh{fs=Efs,vc=VcL}=Mesh,mat=MatList2}=Obj, MatList1) ->
    {VcL_1, MtC} = add_to_colors(VcL, MatList2++MatList1),
    Obj#e3d_object{obj=Mesh#e3d_mesh{fs=[mat_to_col_2(F, MtC) || F <- Efs],vc=VcL_1}}.
mat_to_col_2(#e3d_face{vc=[_Colors|_]}=F, _) ->
    F;
mat_to_col_2(#e3d_face{vs=Vs,vc=[],mat=[]}=F, _) ->
    F#e3d_face{vc=[0 || _ <- Vs]};
mat_to_col_2(#e3d_face{vs=Vs,vc=[],mat=[MatName]}=F, MtC) ->
    F#e3d_face{vc=[mat_to_col_3(MatName, MtC) || _ <- Vs]}.
mat_to_col_3(MatName, MtC) ->
    case gb_trees:lookup(MatName, MtC) of
        {value, Idx} -> Idx
    end.
add_to_colors(VcL_0, ML_0) ->
    add_to_colors(VcL_0, length(VcL_0), ML_0, gb_trees:empty()).
add_to_colors(VcL, Next, [{MatName, Mtl}|ML_0], MtC) ->
    OpenGL = proplists:get_value(opengl, Mtl, []),
    {R,G,B,_} = proplists:get_value(diffuse, OpenGL, {0.8,0.8,0.8,1.0}),
    add_to_colors([{R,G,B}|VcL], Next+1, ML_0, gb_trees:insert(MatName, Next, MtC));
add_to_colors(VcL, _, [], MtC) ->
    {lists:reverse(VcL), MtC}.


%%%
%%%

-ifdef(TEST).
%%
%% Test functions 
%%

t11() ->
    S=[
    {{0.0,0.0}, {1.0,0.0}},
    {{0.0,0.0}, {0.0,1.0}},
    {{1.0,0.0}, {2.0,0.5}},
    {{2.0,1.0}, {1.0,1.0}},
    {{1.0,1.0}, {0.0,1.0}}
    ],
    additional_path_lines(S).

t12() ->
    CLines = [
     {{0.0,1.0}, {1.0,0.0}},
     {{1.0,0.0}, {1.5,0.0}},
       {{1.5,0.0}, {2.0,1.0}},
       {{2.0,1.0}, {1.5,1.0}},
     {{1.5,1.0}, {1.0,2.0}},
     {{1.0,2.0}, {0.0,1.0}} ],
    InLines = [{{1.0,0.0}, {0.5,1.0}},
     {{0.5,1.0}, {1.0,2.0}},
     {{0.5,1.0}, {1.5,1.0}} ],
    EmptyMarkers = [],
     divide_contour_to_paths(CLines, InLines, EmptyMarkers).

t13() ->
    Lines = t11(),
    find_outside_edge_contours(Lines).

t14() ->
    L = [
        {0.4,0.0},
        {1.0,0.0},
        {0.6,1.0},
        {0.0,1.0}
    ],
    within_inside({0.5,0.5}, L).


t15() ->
    Path=[
      {0.0,0.0},
      {1.0,0.0},
      {1.0,1.0},
      {2.0,2.0},
      {2.0,3.0},
      {3.0,4.0},
      {0.5,4.0},
      {0.5,0.5}
    ],
    _Path1=[
      {0.0,0.0},
      {1.0,0.0},
      {1.0,1.0},
      {0.0,1.0}
    ],
    div_concave(Path).


t16() ->
    Shape1 = [
        {{-1.0,-1.0}, {1.0,-1.0}},
        {{1.0,-1.0}, {1.0,1.0}},
        {{1.0,1.0}, {-1.0,1.0}},
        {{-1.0,1.0}, {-1.0,-1.0}}
    ],
    Shape2 = [
        {{0.0,0.0}, {0.2,0.2}},
        {{0.4,0.0}, {0.2,0.2}},
        {{0.0,0.4}, {0.2,0.2}},
        {{0.4,0.4}, {0.2,0.2}}
    ],
    Shape3 = [
        {{10.0,0.0}, {10.3,0.3}},
        {{10.3,0.3}, {10.6,0.0}},
        {{10.0,0.6}, {10.3,0.3}},
        {{10.3,0.3}, {10.6,0.6}}
    ],
    Shapes = [
        Shape1,
        Shape2,
        Shape3
    ],
    find_empty_points(Shapes).

    
t() ->
    t_2("dxf/u2007-mm.dxf").%box.dxf").
-define(PX_RESCALE, 25.0). %% 40 pixels per unit
t_2(FileN) ->
    {ok, Cont_0} = read_dxf_file(FileN),
    #dxfc{commands=CommandsList} = expand_inserts(Cont_0),
    
    Scale = {?PX_RESCALE, ?PX_RESCALE, ?PX_RESCALE},
    Commands_0 = rescale(Scale, adjust_commands(CommandsList)),
    Commands_1 = solid_commands_to_objects(Commands_0, 0.1),
    Commands_2 = face_commands_to_objects(Commands_1),
    Commands_3 = mesh_sequences_to_objects(Commands_2),
    Objs = contours_to_objects(Commands_3, 0.1, fill),

    {ok, Fp3} = file:open("debug.mvg", [write]),
    file:write(Fp3, "push graphic-context\n"),
    file:write(Fp3, "viewbox 0 0 624 624\nfill white\nstroke black\n"),
    t_mvis(Fp3, Objs),
    file:write(Fp3, "pop graphic-context\n"),
    file:close(Fp3),
    os:cmd("magick convert mvg:debug.mvg debug.png").
t_mvis(_, []) -> done;
t_mvis(Fp3, [A|Commands]) ->
    case A of
        {obj, Obj} ->
            t_mvis_obj(Fp3, Obj)
    end,
    t_mvis(Fp3, Commands).
t_mvis_obj(Fp3, #e3d_object{obj=#e3d_mesh{vs=Vs,fs=Fs}=_}) ->
    Arr = array:from_list(Vs),
    t_mvis_path(Fp3, Fs, Arr).
t_mvis_path(Fp3, [#e3d_face{vs=L}=_|Fs], Arr) ->
    file:write(Fp3, "path '" ++ t_mvis_lines(L, Arr) ++ "'\n"),
    t_mvis_path(Fp3, Fs, Arr);
t_mvis_path(_, [], _Arr) ->
    ok.
t_mvis_lines([I|L], Arr) ->
    {X1,Y1} = t_mvis_2d(array:get(I, Arr)),
    "M" ++ lists:flatten(io_lib:format("~p,~p", [X1,Y1])) ++ "L" ++
    t_mvis_lines(L, Arr, I).
t_mvis_lines([I|R], Arr, F) ->
    {X2,Y2} = t_mvis_2d(array:get(I, Arr)),
    " " ++ lists:flatten(io_lib:format("~p,~p", [X2,Y2])) ++
    t_mvis_lines(R, Arr, F);
t_mvis_lines([], Arr, I) ->
    {X2,Y2} = t_mvis_2d(array:get(I, Arr)),
    " " ++ lists:flatten(io_lib:format("~p,~p", [X2,Y2])).
t_mvis_2d({X1,Y1,Z1}) ->
    {50.0 + X1 + Z1 * 0.1,
     50.0 + Y1 + Z1 * 0.1}.

%%%
%%%

t2() ->
    Add1 = fun(L) -> [ A+1 || A <- L] end,
    P = [
        {mesh_start, {8, 6}},
        {mesh_vertex, {-1.0,-1.0,-1.0}},
            {mesh_vertex, {-1.0,-1.0,1.0}},
            {mesh_vertex, {-1.0,1.0,-1.0}},
            {mesh_vertex, {-1.0,1.0,1.0}},
            {mesh_vertex, {1.0,-1.0,-1.0}},
            {mesh_vertex, {1.0,-1.0,1.0}},
            {mesh_vertex, {1.0,1.0,-1.0}},
            {mesh_vertex, {1.0,1.0,1.0}},
        {mesh_face, Add1([3,1,5,7])},
            {mesh_face, Add1([2,3,7,6])},
            {mesh_face, Add1([2,0,1,3])},
            {mesh_face, Add1([6,7,5,4])},
            {mesh_face, Add1([2,6,4,0])},
            {mesh_face, Add1([0,4,5,1])},
        {mesh_end, 0}
    ],
    mesh_sequences_to_objects(P).

t3() ->
    P1 = {-1.0,-1.0,-1.0},
    P2 = {-1.0,-1.0,1.0},
    P3 = {-1.0,1.0,-1.0},
    P4 = {-1.0,1.0,1.0},
    
    P5 = {1.0,-1.0,-1.0},
    P6 = {1.0,-1.0,1.0},
    P7 = {1.0,1.0,-1.0},
    P8 = {1.0,1.0,1.0},
    
    P = [
        unused,
        {td_face, {[P1, P5, P6, P2], 250, "0"}},
        {td_face, {[P3, P7, P5, P1], 250, "0"}},
        {td_face, {[P7, P8, P6, P5], 250, "0"}},
        unused,
        unused,
        {td_face, {[P3, P1, P2, P4], 250, "0"}},
        {td_face, {[P3, P4, P8, P7], 250, "0"}},
        {td_face, {[P4, P2, P6, P8], 250, "0"}}
    ],
    face_commands_to_objects(P).


t_solids() ->
    P = [
        unused,
        {solid, [{0,0}, {5,0}, {5,5}, {0,5}]},
        unused,
        unused,
        {solid, [{5,0}, {10, 0}, {10, 5}, {5, 5}]},
        {solid, [{0,5}, {5,5}, {5,10}, {0,10}]}
    ],
    solid_commands_to_objects(P, 1.0).

t4() ->
    P = [
        {lines, [{{-1.0,-1.0}, {1.0,-1.0}}]},
        {lines, [{{1.0,-1.0}, {1.0,1.0}}]},
        {lines, [{{1.0,1.0}, {-1.0,1.0}}]},
        {lines, [{{-1.0,1.0}, {-1.0,-1.0}}]}
    ],
    contours_to_objects(P, 1.0, fill).
    
t5() ->
    all_edges([[3,1,5,7],[2,3,7,6],[2,0,1,3],[6,7,5,4],[2,6,4,0],[0,4,5,1]]).



t0() ->
    t_2("g.dxf").
-endif().
