
##
##  Scripting for Shapes  (Scheme and Python)
##
##  Copyright 2023 Edward Blake
##
##  See the file "license.terms" for information on usage and redistribution
##  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
##
##     $Id$
##

##
## This python code acts as a boot strap to define some useful functions
## and then load the actual script that is sent as a symbolic expression through
## standard input by the erlang plugin.
##

import w3d_int
import sys
from os import path
from runpy import run_path


sys.stdin.reconfigure(encoding="utf-8")
sys.stdout.reconfigure(encoding="utf-8")
sys.stderr.reconfigure(encoding="utf-8")

## Parameters being passed to the script from the script's
## parameter options window.
params = []

## Access parameters by key, for those that used an atom key
params_by_key = {}

## Extra parameters passed as the third argument
## These are parameters that are not set via a parameter
## window and may be auxiliary variables dependent on the
## type of script (e.g. "content" contains an e3d_file tuple
## for exporter plugins, parameters set with "script_params"
## also show up in here).
extra_params = {}

for line in sys.stdin:
	if (line == ""):
		pass
	cmd_0, _ = w3d_int.scm_parse(line)
	cmd = cmd_0[0]
	break
if cmd[0] == "run":
	params = cmd[2]
	## Add to params_by_key parameters that are keyed
	for p in params:
		if hasattr(p, "__len__"):
			params_by_key[p[0]] = p[1]
	## Grab the extra params
	extra_params_1 = cmd[3]
	for p in extra_params_1:
		extra_params[p[0]] = p[1]
	g = {}
	g["params"] = params
	g["params_by_key"] = params_by_key
	g["extra_params"] = extra_params
	g["scm_parse"] = w3d_int.scm_parse
	g["OutputList"] = w3d_int.OutputList
	# g["wings_set_var"] = w3d_int.wings_set_var
	# g["wings_query"] = w3d_int.wings_query
	script_file = cmd[1]
	sys.path.insert(0, path.dirname(script_file))
	run_path(script_file, g)
	print("")
	sys.stdout.flush()
	exit(0)
else:
	print("Not run")
