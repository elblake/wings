
##
##  Scripting for Shapes  (Scheme and Python)
##
##  Copyright 2025 Edward Blake
##
##  See the file "license.terms" for information on usage and redistribution
##  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
##
##     $Id$
##

from w3d_int import OutputList,wings_we__change,wings_we__get


def collapse__collapse_edge(arg1, *args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_collapse', 'collapse_edge', arg1) ## int
	elif argsize == 1:
		arg2 = args[0]
		return wings_we__change('wings_collapse', 'collapse_edge', arg1, arg2) ## int int
	else:
		raise 'invalid arguments'
def collapse__collapse_edges(arg1): ## Changes #we{}
	return wings_we__change('wings_collapse', 'collapse_edges', arg1) ## list
def collapse__collapse_faces(arg1): ## Changes #we{}
	return wings_we__change('wings_collapse', 'collapse_faces', arg1) ## gbset
def collapse__collapse_vertices(arg1): ## Changes #we{}
	return wings_we__change('wings_collapse', 'collapse_vertices', arg1) ## list
def collapse__fast_collapse_edge(arg1): ## Changes #we{}
	return wings_we__change('wings_collapse', 'fast_collapse_edge', arg1) ## int

def dissolve__complement(arg1): ## Changes #we{}
	return wings_we__change('wings_dissolve', 'complement', arg1) ## list
def dissolve__faces(arg1): ## Changes #we{}
	return wings_we__change('wings_dissolve', 'faces', arg1) ## ordset
def dissolve__outer_edge_partition(arg1):
	return wings_we__get('wings_dissolve', 'outer_edge_partition', arg1) ## ordset

def edge__cut(arg1, arg2): ## Changes #we{}
	return wings_we__change('wings_edge', 'cut', arg1, arg2) ## int int
def edge__dissolve_edge(arg1) ## Changes #we{}
	return wings_we__change('wings_edge', 'dissolve_edge', arg1) ## int
def edge__dissolve_edges(arg1, *args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_edge', 'dissolve_edges', arg1) ## list
	elif argsize == 1:
		arg2 = args[0]
		return wings_we__change('wings_edge', 'dissolve_edges', arg1, arg2) ## list list
	else:
		raise 'invalid arguments'
def edge__dissolve_isolated_vs(arg1): ## Changes #we{}
	return wings_we__change('wings_edge', 'dissolve_isolated_vs', arg1) ## list
def edge__fast_cut(arg1, arg2): ## Changes #we{}
	return wings_we__change('wings_edge', 'fast_cut', arg1, arg2) ## int vec3_or_atom
def edge__from_faces(arg1):
	return wings_we__get('wings_edge', 'from_faces', arg1) ## list
def edge__from_vs(arg1):
	return wings_we__get('wings_edge', 'from_vs', arg1) ## list
def edge__length(arg1):
	return wings_we__get('wings_edge', 'length', arg1) ## int
def edge__reachable_faces(arg1, arg2):
	return wings_we__get('wings_edge', 'reachable_faces', arg1, arg2) ## gbset gbset
def edge__screaming_cut(arg1, arg2): ## Changes #we{}
	return wings_we__change('wings_edge', 'screaming_cut', arg1, arg2) ## int vec3
def edge__select_region(arg1):
	return wings_we__get('wings_edge', 'select_region', arg1) ## gbset
def edge__to_vertices(arg1):
	return wings_we__get('wings_edge', 'to_vertices', arg1) ## list

def edge_cmd__loop_cut_partition(arg1):
	return wings_we__get('wings_edge_cmd', 'loop_cut_partition', arg1) ## list

def edge_loop__edge_links(arg1):
	return wings_we__get('wings_edge_loop', 'edge_links', arg1) ## gbset
def edge_loop__edge_loop_vertices(arg1):
	return wings_we__get('wings_edge_loop', 'edge_loop_vertices', arg1) ## gbset
def edge_loop__partition_edges(arg1):
	return wings_we__get('wings_edge_loop', 'partition_edges', arg1) ## gbset
def edge_loop__select_loop(arg1):
	return wings_we__get('wings_edge_loop', 'select_loop', arg1) ## ordset

def extrude_edge__extrude_edges(arg1, arg2): ## Changes #we{}
	return wings_we__change('wings_extrude_edge', 'extrude_edges', arg1, arg2) ## list float

def extrude_face__faces(arg1): ## Changes #we{}
	return wings_we__change('wings_extrude_face', 'faces', arg1) ## list
def extrude_face__regions(arg1): ## Changes #we{}
	return wings_we__change('wings_extrude_face', 'regions', arg1) ## list

def face__are_neighbors(arg1, arg2):
	return wings_we__get('wings_face', 'are_neighbors', arg1, arg2) ## int int
def face__area(arg1):
	return wings_we__get('wings_face', 'area', arg1) ## int
def face__center(arg1):
	return wings_we__get('wings_face', 'center', arg1) ## int
def face__delete_bad_faces(arg1): ## Changes #we{}
	return wings_we__change('wings_face', 'delete_bad_faces', arg1) ## list
def face__extend_border(arg1):
	return wings_we__get('wings_face', 'extend_border', arg1) ## gbset
def face__face_normal_ccw(arg1):
	return wings_we__get('wings_face', 'face_normal_ccw', arg1) ## list
def face__face_normal_cw(arg1):
	return wings_we__get('wings_face', 'face_normal_cw', arg1) ## list
def face__from_edges(arg1):
	return wings_we__get('wings_face', 'from_edges', arg1) ## list
def face__from_vs(arg1):
	return wings_we__get('wings_face', 'from_vs', arg1) ## list
def face__good_normal(arg1):
	return wings_we__get('wings_face', 'good_normal', arg1) ## int
def face__inner_edges(arg1):
	return wings_we__get('wings_face', 'inner_edges', arg1) ## list
def face__inner_outer_edges(arg1):
	return wings_we__get('wings_face', 'inner_outer_edges', arg1) ## list
def face__is_planar(arg1, arg2):
	return wings_we__get('wings_face', 'is_planar', arg1, arg2) ## float int
def face__is_thin(arg1):
	return wings_we__get('wings_face', 'is_thin', arg1) ## int
def face__mirror_matrix(arg1):
	return wings_we__get('wings_face', 'mirror_matrix', arg1) ## int
def face__normal(arg1, *args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_face', 'normal', arg1) ## int
	elif argsize == 1:
		arg2 = args[0]
		return wings_we__get('wings_face', 'normal', arg1, arg2) ## int int
	else:
		raise 'invalid arguments'
def face__outer_edges(arg1):
	return wings_we__get('wings_face', 'outer_edges', arg1) ## list
def face__to_edges(arg1):
	return wings_we__get('wings_face', 'to_edges', arg1) ## list
def face__to_vertices(arg1):
	return wings_we__get('wings_face', 'to_vertices', arg1) ## list
def face__vertex_positions(arg1, *args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_face', 'vertex_positions', arg1) ## int
	elif argsize == 1:
		arg2 = args[0]
		return wings_we__get('wings_face', 'vertex_positions', arg1, arg2) ## int int
	else:
		raise 'invalid arguments'
def face__vertices(arg1):
	return wings_we__get('wings_face', 'vertices', arg1) ## int
def face__vertices_ccw(arg1, *args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_face', 'vertices_ccw', arg1) ## int
	elif argsize == 1:
		arg2 = args[0]
		return wings_we__get('wings_face', 'vertices_ccw', arg1, arg2) ## int int
	else:
		raise 'invalid arguments'
def face__vertices_cw(arg1, *args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_face', 'vertices_cw', arg1) ## int
	elif argsize == 1:
		arg2 = args[0]
		return wings_we__get('wings_face', 'vertices_cw', arg1, arg2) ## int int
	else:
		raise 'invalid arguments'

def face_cmd__force_bridge(arg1, arg2, arg3, arg4): ## Changes #we{}
	return wings_we__change('wings_face_cmd', 'force_bridge', arg1, arg2, arg3, arg4) ## int int int int
def face_cmd__mirror_faces(arg1): ## Changes #we{}
	return wings_we__change('wings_face_cmd', 'mirror_faces', arg1) ## list

def facemat__all():
	return wings_we__get('wings_facemat', 'all')
def facemat__any_interesting_materials():
	return wings_we__get('wings_facemat', 'any_interesting_materials')
def facemat__assign(arg1, *args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_facemat', 'assign', arg1) ## list
	elif argsize == 1:
		arg2 = args[0]
		return wings_we__change('wings_facemat', 'assign', arg1, arg2) ## val list
	else:
		raise 'invalid arguments'
def facemat__delete_face(arg1): ## Changes #we{}
	return wings_we__change('wings_facemat', 'delete_face', arg1) ## int
def facemat__delete_faces(arg1): ## Changes #we{}
	return wings_we__change('wings_facemat', 'delete_faces', arg1) ## list
def facemat__face(arg1):
	return wings_we__get('wings_facemat', 'face', arg1) ## int
def facemat__gc(): ## Changes #we{}
	return wings_we__change('wings_facemat', 'gc')
def facemat__hide_faces(): ## Changes #we{}
	return wings_we__change('wings_facemat', 'hide_faces')
def facemat__is_material_used(arg1):
	return wings_we__get('wings_facemat', 'is_material_used', arg1) ## val
def facemat__keep_faces(arg1): ## Changes #we{}
	return wings_we__change('wings_facemat', 'keep_faces', arg1) ## list
def facemat__mat_faces(arg1):
	return wings_we__get('wings_facemat', 'mat_faces', arg1) ## list
def facemat__show_faces(arg1): ## Changes #we{}
	return wings_we__change('wings_facemat', 'show_faces', arg1) ## list
def facemat__used_materials():
	return wings_we__get('wings_facemat', 'used_materials')

def subdiv__get_proxy_info(arg1, arg2):
	return wings_we__get('wings_subdiv', 'get_proxy_info', arg1, arg2) ## list list
def subdiv__smooth(*args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_subdiv', 'smooth')
	elif argsize == 4:
		arg1 = args[0]
		arg2 = args[1]
		arg3 = args[2]
		arg4 = args[3]
		return wings_we__change('wings_subdiv', 'smooth', arg1, arg2, arg3, arg4) ## list list list list
	else:
		raise 'invalid arguments'
def subdiv__smooth_faces_htab():
	return wings_we__get('wings_subdiv', 'smooth_faces_htab')
def subdiv__subdiv(*args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_subdiv', 'subdiv')
	elif argsize == 4:
		arg1 = args[0]
		arg2 = args[1]
		arg3 = args[2]
		arg4 = args[3]
		return wings_we__change('wings_subdiv', 'subdiv', arg1, arg2, arg3, arg4) ## list list list list
	else:
		raise 'invalid arguments'

def tesselation__quadrangulate(*args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_tesselation', 'quadrangulate')
	elif argsize == 1:
		arg1 = args[0]
		return wings_we__change('wings_tesselation', 'quadrangulate', arg1) ## list
	else:
		raise 'invalid arguments'
def tesselation__triangulate(*args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_tesselation', 'triangulate')
	elif argsize == 1:
		arg1 = args[0]
		return wings_we__change('wings_tesselation', 'triangulate', arg1) ## gbset
	else:
		raise 'invalid arguments'

def util__add_vpos(arg1):
	return wings_we__get('wings_util', 'add_vpos', arg1) ## list
def util__update_vpos(arg1):
	return wings_we__get('wings_util', 'update_vpos', arg1) ## list

def va__all(arg1):
	return wings_we__get('wings_va', 'all', arg1) ## atom
def va__any_attributes():
	return wings_we__get('wings_va', 'any_attributes')
def va__any_colors():
	return wings_we__get('wings_va', 'any_colors')
def va__any_uvs():
	return wings_we__get('wings_va', 'any_uvs')
def va__del_edge_attrs(arg1): ## Changes #we{}
	return wings_we__change('wings_va', 'del_edge_attrs', arg1) ## int
def va__edge_attrs(arg1, arg2, *args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_va', 'edge_attrs', arg1, arg2) ## int atom
	elif argsize == 1:
		arg3 = args[0]
		return wings_we__get('wings_va', 'edge_attrs', arg1, arg2, arg3) ## int atom float
	else:
		raise 'invalid arguments'
def va__face_attr(arg1, arg2, *args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_va', 'face_attr', arg1, arg2) ## atom int
	elif argsize == 1:
		arg3 = args[0]
		return wings_we__get('wings_va', 'face_attr', arg1, arg2, arg3) ## atom int int
	else:
		raise 'invalid arguments'
def va__face_mixed_attrs(arg1):
	return wings_we__get('wings_va', 'face_mixed_attrs', arg1) ## int
def va__face_pos_attr(arg1, arg2, arg3):
	return wings_we__get('wings_va', 'face_pos_attr', arg1, arg2, arg3) ## atom int int
def va__gc(): ## Changes #we{}
	return wings_we__change('wings_va', 'gc))
def va__remove(arg1, *args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_va', 'remove', arg1) ## atom
	elif argsize == 1:
		arg2 = args[0]
		return wings_we__change('wings_va', 'remove', arg1, arg2) ## atom list
	else:
		raise 'invalid arguments'
def va__renumber(arg1): ## Changes #we{}
	return wings_we__change('wings_va', 'renumber', arg1) ## val
def va__set_body_color(arg1): ## Changes #we{}
	return wings_we__change('wings_va', 'set_body_color', arg1) ## vec3
def va__set_both_edge_attrs(arg1, arg2, arg3): ## Changes #we{}
	return wings_we__change('wings_va', 'set_both_edge_attrs', arg1, arg2, arg3) ## int val val
def va__set_edge_attrs(arg1, *args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_va', 'set_edge_attrs', arg1) ## list
	elif argsize == 2:
		arg2 = args[0]
		arg3 = Args[1]
		return wings_we__change('wings_va', 'set_edge_attrs', arg1, arg2, arg3) ## int atom val
	else:
		raise 'invalid arguments'
def va__set_edge_color(arg1, arg2, *args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0
		return wings_we__change('wings_va', 'set_edge_color', arg1, arg2) ## gbset vec3
	elif argsize == 1:
		arg3 = args[0]
		return wings_we__change('wings_va', 'set_edge_color', arg1, arg2, arg3) ## int vec3 vec3
	else:
		raise 'invalid arguments'
def va__set_edge_colors(arg1): ## Changes #we{}
	return wings_we__change('wings_va', 'set_edge_colors', arg1) ## list
def va__set_edge_uvs(arg1): ## Changes #we{}
	return wings_we__change('wings_va', 'set_edge_uvs', arg1) ## list
def va__set_face_attr_vs(arg1, arg2, arg3): ## Changes #we{}
	return wings_we__change('wings_va', 'set_face_attr_vs', arg1, arg2, arg3) ## atom int list
def va__set_face_attrs(arg1, arg2): ## Changes #we{}
	return wings_we__change('wings_va', 'set_face_attrs', arg1, arg2) ## int val
def va__set_face_color(arg1, arg2): ## Changes #we{}
	return wings_we__change('wings_va', 'set_face_color', arg1, arg2) ## gbset vec3
def va__set_vertex_color(arg1, arg2): ## Changes #we{}
	return wings_we__change('wings_va', 'set_vertex_color', arg1, arg2) ## gbset vec3
def va__set_vtx_face_uvs(arg1, arg2, arg3): ## Changes #we{}
	return wings_we__change('wings_va', 'set_vtx_face_uvs', arg1, arg2, arg3) ## int list vec2
def va__vtx_attrs(arg1, *args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_va', 'vtx_attrs', arg1) ## int
	elif argsize == 1:
		arg2 = args[0]
		return wings_we__get('wings_va', 'vtx_attrs', arg1, arg2) ## int int
	else:
		raise 'invalid arguments'

def vertex__bounding_box(*args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_vertex', 'bounding_box')
	elif argsize == 1:
		arg1 = args[0]
		return wings_we__get('wings_vertex', 'bounding_box', arg1) ## val
	elif argsize == 2:
		arg1 = args[0]
		arg2 = args[1]
		return wings_we__get('wings_vertex', 'bounding_box', arg1, arg2) ## list val
	else:
		raise 'invalid arguments'
def vertex__center(*args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_vertex', 'center')
	elif argsize == 1:
		arg1 = args[0]
		return wings_we__get('wings_vertex', 'center', arg1) ## gbset
	else:
		raise 'invalid arguments'
def vertex__connect(arg1, arg2): ## Changes #we{}
	return wings_we__change('wings_vertex', 'connect', arg1, arg2) ## int list
def vertex__connect_cut(arg1, arg2): ## Changes #we{}
	return wings_we__change('wings_vertex', 'connect_cut', arg1, arg2) ## int int
def vertex__dissolve_isolated(arg1): ## Changes #we{}
	return wings_we__change('wings_vertex', 'dissolve_isolated', arg1) ## list
def vertex__edge_through(arg1, arg2, *args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_vertex', 'edge_through', arg1, arg2) ## int int
	elif argsize == 1:
		arg3 = args[0]
		return wings_we__get('wings_vertex', 'edge_through', arg1, arg2, arg3) ## int int int
	else:
		raise 'invalid arguments'
def vertex__flatten(arg1, arg2, *args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_vertex', 'flatten', arg1, arg2) ## list vec3
	elif argsize == 1:
		arg3 = args[0]
		return wings_we__change('wings_vertex', 'flatten', arg1, arg2, arg3) ## list vec3 vec3
	else:
		raise 'invalid arguments'
def vertex__force_connect(arg1, arg2, arg3): ## Changes #we{}
	return wings_we__change('wings_vertex', 'force_connect', arg1, arg2, arg3) ## int int int
def vertex__from_edges(arg1):
	return wings_we__get('wings_vertex', 'from_edges', arg1) ## gbset
def vertex__from_faces(arg1):
	return wings_we__get('wings_vertex', 'from_faces', arg1) ## gbset
def vertex__isolated():
	return wings_we__get('wings_vertex', 'isolated')
def vertex__normal(arg1):
	return wings_we__get('wings_vertex', 'normal', arg1) ## int
def vertex__outer_vertices_ccw(arg1):
	return wings_we__get('wings_vertex', 'outer_vertices_ccw', arg1) ## list
def vertex__per_face(arg1):
	return wings_we__get('wings_vertex', 'per_face', arg1) ## list
def vertex__pos(arg1):
	return wings_we__get('wings_vertex', 'pos', arg1) ## int
def vertex__reachable(arg1):
	return wings_we__get('wings_vertex', 'reachable', arg1) ## list

def vertex_cmd__bevel_vertex(arg1): ## Changes #we{}
	return wings_we__change('wings_vertex_cmd', 'bevel_vertex', arg1) ## int
def vertex_cmd__connect(arg1): ## Changes #we{}
	return wings_we__change('wings_vertex_cmd', 'connect', arg1) ## list

def we__all_hidden():
	return wings_we__get('wings_we', 'all_hidden')
def we__break_mirror(): ## Changes #we{}
	return wings_we__change('wings_we', 'break_mirror')
def we__centroid():
	return wings_we__get('wings_we', 'centroid')
def we__create_holes(arg1): ## Changes #we{}
	return wings_we__change('wings_we', 'create_holes', arg1) ## list
def we__create_mirror(arg1): ## Changes #we{}
	return wings_we__change('wings_we', 'create_mirror', arg1) ## int
def we__fast_rebuild(): ## Changes #we{}
	return wings_we__change('wings_we', 'fast_rebuild')
def we__freeze_mirror(): ## Changes #we{}
	return wings_we__change('wings_we', 'freeze_mirror')
def we__fully_visible_edges(arg1):
	return wings_we__get('wings_we', 'fully_visible_edges', arg1) ## ordset
def we__hide_faces(arg1): ## Changes #we{}
	return wings_we__change('wings_we', 'hide_faces', arg1) ## gbset
def we__invert_normals(): ## Changes #we{}
	return wings_we__change('wings_we', 'invert_normals')
def we__is_consistent():
	return wings_we__get('wings_we', 'is_consistent')
def we__is_face_consistent(arg1):
	return wings_we__get('wings_we', 'is_face_consistent', arg1) ## int
def we__is_open():
	return wings_we__get('wings_we', 'is_open')
def we__mirror_projection():
	return wings_we__get('wings_we', 'mirror_projection')
def we__new_id(): ## Changes #we{}
	return wings_we__change('wings_we', 'new_id')
def we__new_ids(arg1): ## Changes #we{}
	return wings_we__change('wings_we', 'new_ids', arg1) ## int
def we__new_wrap_range(arg1, arg2):
	return wings_we__change('wings_we', 'new_wrap_range', arg1, arg2) ## int int
def we__normals(arg1, arg2):
	return wings_we__get('wings_we', 'normals', arg1, arg2) ## list val
def we__num_hidden():
	return wings_we__get('wings_we', 'num_hidden')
def we__perimeter():
	return wings_we__get('wings_we', 'perimeter')
def we__rebuild(): ## Changes #we{}
	return wings_we__change('wings_we', 'rebuild')
def we__renumber(arg1, *args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_we', 'renumber', arg1) ## int
	elif argsize == 1:
		arg2 = args[0]
		return wings_we__change('wings_we', 'renumber', arg1, arg2) ## int list
	else:
		raise 'invalid arguments'
def we__separate(): ## Changes #we{}
	return wings_we__change('wings_we', 'separate')
def we__show_faces(*args): ## Changes #we{}
	argsize = len(args)
	if argsize == 0:
		return wings_we__change('wings_we', 'show_faces')
	elif argsize == 1:
		arg1 = args[0]
		return wings_we__change('wings_we', 'show_faces', arg1) ## list
	else:
		raise 'invalid arguments'
def we__surface_area():
	return wings_we__get('wings_we', 'surface_area')
def we__transform_vs(arg1): ## Changes #we{}
	return wings_we__change('wings_we', 'transform_vs', arg1) ## matrix
def we__uv_mapped_faces():
	return wings_we__get('wings_we', 'uv_mapped_faces')
def we__uv_to_color(): ## Changes #we{}
	return wings_we__change('wings_we', 'uv_to_color')
def we__validate_mirror(): ## Changes #we{}
	return wings_we__change('wings_we', 'validate_mirror')
def we__visible(*args):
	argsize = len(args)
	if argsize == 0
		return wings_we__get('wings_we', 'visible')
	elif argsize == 1:
		arg1 = args[0]
		return wings_we__get('wings_we', 'visible', arg1) ## list
	else:
		raise 'invalid arguments'
def we__visible_edges(*args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_we', 'visible_edges')
	elif argsize == 1:
		arg1 = args[0]
		return wings_we__get('wings_we', 'visible_edges', arg1) ## gbset
	else:
		raise 'invalid arguments'
def we__visible_vs(*args):
	argsize = len(args)
	if argsize == 0:
		return wings_we__get('wings_we', 'visible_vs')
	elif argsize == 1:
		arg1 = args[0]
		return wings_we__get('wings_we', 'visible_vs', arg1) ## list
	else:
		raise 'invalid arguments'
def we__volume():
	return wings_we__get('wings_we', 'volume')




