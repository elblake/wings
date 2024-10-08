#
#  Makefile --
#
#     Makefile for building import/export plug-ins.
#
#  Copyright (c) 2001-2013 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.25 2006/09/06 22:52:18 antoneos Exp $
#

include ../../erl.mk

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
WINGS_INTL=../../intl_tools
EBIN=../../plugins/import_export
WINGS_E3D=../../e3d
WINGS_TOP=../../..

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

MODULES= \
	wpc_3ds \
	wpc_3mf \
	wpc_ai \
	wpc_amf \
	wpc_bzw \
	wpc_collada \
	collada_import \
	wpc_fbx_p \
	fbx_p_import \
	wpc_gltf \
	wpc_hlines \
	wpc_jscad \
	wpc_kerky \
	wpc_lwo \
	wpc_obj \
	wpc_ply \
	wpc_pov \
	wpc_ps \
	wpc_rwx \
	wpc_stl \
	wpc_svg_path \
	wpc_wmf_paths \
	wpc_wrl \
	wpc_x \
	x_import \
	wpc_yafaray \
	x3d_import \
	x3d_import__sgi

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -Werror -pa $(WINGS_INTL) \
  -I $(WINGS_TOP) $(TYPE_FLAGS) +debug_info

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

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------

$(TARGET_FILES): $(WINGS_E3D)/e3d.hrl

