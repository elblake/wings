#
#  Makefile --
#
#     Makefile for building command plug-ins.
#
#  Copyright (c) 2001-2013 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.21 2006/06/29 19:57:58 giniu Exp $
#
include ../../erl.mk

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
WINGS_INTL=../../intl_tools
EBIN=../../plugins/commands
WINGS_TOP=../../..
WINGS_E3D=../../e3d

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

MODULES= \
	wpc_absolute_move \
	wpc_absolute_scale \
	wpc_align_to_axis \
	wpc_ambocc \
	wpc_arc_intersect \
	wpc_bend \
	wpc_circularise \
	wpc_connect_tool \
	wpc_constraints \
	wpc_cylindrilize \
	wpc_corner \
	wpc_deselect_previous \
	wpc_doosabin \
	wpc_duplicate_radial \
	wpc_explode \
	wpc_flow_connect \
	wpc_greebles \
	wpc_grid_snap \
	wpc_inset \
	wpc_intersect_edge \
	wpc_intersect_vertex \
	wpc_isometric_view \
	wpc_magnet_mask \
	wpc_move_planar \
	wpc_numeric_camera \
	wpc_plane_cut \
	wpc_region \
	wpc_rotate_unconstrained \
	wpc_sculpt \
	wpc_sel_win \
	wpc_shear \
	wpc_shift \
	wpc_sweep_extrude \
	wpc_turnedge \
	wpc_untriangulate \
	wpc_views_win \
	wpc_weld

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -pa $(WINGS_INTL) -Werror -I $(WINGS_TOP) $(TYPE_FLAGS) +debug_info

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

template: opt
	$(ERL) -pa $(WINGS_INTL) -noinput -run tools generate_template_files $(EBIN)

lang: template
	cp *.lang $(EBIN)
	$(ERL) -pa $(WINGS_INTL) -noinput -run tools diff_lang_files $(EBIN)

common: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(EBIN)/wpc_constraints.beam: $(WINGS_E3D)/e3d.hrl
