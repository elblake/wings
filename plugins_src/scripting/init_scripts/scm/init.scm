
;;
;;  Scripting for Shapes  (init for Scheme)
;;
;;  Copyright 2023 Edward Blake
;;
;;  See the file "license.terms" for information on usage and redistribution
;;  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
;;
;;     $Id$
;;

;;
;; This scheme code acts as a boot strap to define some useful functions
;; and then load the actual script. The actual script is sent by the erlang
;; plugin as a symbolic expression through standard input.
;;

;; Main function of script, can be called multiple times
;;
(define *scr-main-fun* #f)
(define (main-function a)
	(set! *scr-main-fun* a))

;; The full path of the script being run
(define *script-full-path* "")

;; The directory of the script being run, the script
;; might want to use this to load it's other files.
(define *script-directory* "")

;; Parameters being passed to the script from the script's
;; parameter options window.
(define *params* '())

;; Extra parameters passed as the third argument
;; These are parameters that are not set via a parameter
;; window and may be auxiliary variables dependent on the
;; type of script (e.g. "content" contains an e3d_file tuple
;; for exporter plugins, parameters set with "script_params"
;; also show up in here).
(define *extra-params* '())


(define **scr-langstrs '())

(define (set-lang-string List)
	(set! **scr-langstrs
		(map (lambda (A)
				(cons (vector-ref A 0)
					  (vector-ref A 1)) )
			List))
	)


(define (?__ Id Str)
	(define Found (assq Id **scr-langstrs))
	(if (eq? Found #f)
		Str
		(cdr Found))
	)


;; Used by the constructors for E3D objects
;;
(define (**loader-construct-from sc default names)
	(define s-name (car default))
	(define s-defaults (cdr default))
	(cons s-name
		(let loop ((sn names) (sd s-defaults))
			(if (equal? '() sn)
				'()
				(let ((res (assq (car sn) sc)))
				(if res
					(cons (list-ref res 1) (loop (cdr sn) (cdr sd)))
					(cons (car sd)         (loop (cdr sn) (cdr sd))))))
			)
		)
	)

;; Useful functions for e3d records
(define (e3d_transf? l) (equal? 'e3d_transf (list-ref l 0)))
(define (e3d_transf-mat l) (list-ref l 1)) ; 
(define (e3d_transf-inv l) (list-ref l 2)) ; 
(define (make-e3d_transf . sc)
	(define default (list 'e3d_transf '()  '()))
	(define names   (list             'mat 'inv))
	(**loader-construct-from sc default names)
	)

(define (ray? l) (equal? 'ray (list-ref l 0)))
(define (ray-o l) (list-ref l 1)) ; 
(define (ray-d l) (list-ref l 2)) ; 
(define (ray-n l) (list-ref l 3)) ; Near, far (or MinT MaxT)
(define (ray-f l) (list-ref l 4)) ; 
(define (ray-bfc l) (list-ref l 5)) ; Backface culling?
(define (make-ray . sc)
	(define default (list 'ray #(0 0) #(0 0) 0.0 1.0 #t))
	(define names   (list      'o  'd  'n  'f 'bfc))
	(**loader-construct-from sc default names)
	)

(define (e3d_face? l) (equal? 'e3d_face (list-ref l 0)))
(define (e3d_face-vs l) (list-ref l 1)) ; List of vertex indices.
(define (e3d_face-vc l) (list-ref l 2)) ; Vertex color indices.
(define (e3d_face-tx l) (list-ref l 3)) ; List of texture indices.
(define (e3d_face-ns l) (list-ref l 4)) ; List of normal indices.
(define (e3d_face-mat l) (list-ref l 5)) ; Materials for face.
(define (e3d_face-sg l) (list-ref l 6)) ; Smooth group for face.
(define (e3d_face-vis l) (list-ref l 7)) ; Visible edges (as in 3DS).
(define (make-e3d_face . sc)
	(define default (list 'e3d_face '() '() '() '() '() 1 -1))
	(define names   (list           'vs 'vc 'tx 'ns 'mat 'sg 'vis))
	(define sc_1 (map
		(lambda (b)
			(if (eq? (car b) 'mat)
				;; Ensure that mat stays a list of atoms
				(let ((matlist (list-ref b 1)))
					(list (car b) (cons "!list" matlist)))
				b
				)
			) sc))
	(**loader-construct-from sc_1 default names)
	)

(define (e3d_mesh? l) (equal? 'e3d_mesh (list-ref l 0)))
(define (e3d_mesh-type l) (list-ref l 1)) ; 'triangle | 'quad | 'polygon
(define (e3d_mesh-vs l) (list-ref l 2)) ; Vertex table (list).
(define (e3d_mesh-vc l) (list-ref l 3)) ; Vertex color table (list).
(define (e3d_mesh-tx l) (list-ref l 4)) ; Texture coordinates (list).
(define (e3d_mesh-ns l) (list-ref l 5)) ; Normal table (list).
(define (e3d_mesh-fs l) (list-ref l 6)) ; Face table (list of e3d_face).
(define (e3d_mesh-he l) (list-ref l 7)) ; List of chains of hard edges.
(define (e3d_mesh-matrix l) (list-ref l 8)) ; Local coordinate system.
(define (make-e3d_mesh . sc)
	(define default (list 'e3d_mesh 'poly '() '() '() '() '() '() 'identity))
	(define names   (list           'type  'vs  'vc  'tx  'ns  'fs 'he  'matrix))
	(**loader-construct-from sc default names)
	)

(define (e3d_object? l) (equal? 'e3d_object (list-ref l 0)))
(define (e3d_object-name l) (list-ref l 1)) ; Name of object (string)
(define (e3d_object-obj l) (list-ref l 2)) ; Object implementation.
(define (e3d_object-mat l) (list-ref l 3)) ; Materials for this object.
(define (e3d_object-attr l) (list-ref l 4)) ; List of attributes.
(define (make-e3d_object . sc)
	(define default (list 'e3d_object 'undefined #f '() '()))
	(define names   (list             'name 'obj  'mat  'attr))
	(**loader-construct-from sc default names)
	)

(define (e3d_file? l) (equal? 'e3d_file (list-ref l 0)))
(define (e3d_file-objs l) (list-ref l 1)) ; List of objects.
(define (e3d_file-mat l) (list-ref l 2)) ; List of materials.
(define (e3d_file-creator l) (list-ref l 3)) ; Creator string.
(define (e3d_file-dir l) (list-ref l 4)) ; Directory for file.
(define (make-e3d_file . sc)
	(define default (list 'e3d_file '() '() "" ""))
	(define names   (list           'objs 'mat 'creator 'dir))
	(**loader-construct-from sc default names)
	)
; (e3d_file `(objs ,objs) `(mat ,mat))
;   


(define (e3d_image? l) (equal? 'e3d_image (list-ref l 0)))
(define (e3d_image-type l) (list-ref l 1))
(define (e3d_image-bytes_pp l) (list-ref l 2))
(define (e3d_image-alignment l) (list-ref l 3))
(define (e3d_image-order l) (list-ref l 4))
(define (e3d_image-width l) (list-ref l 5))
(define (e3d_image-height l) (list-ref l 6))
(define (e3d_image-image l) (list-ref l 7))
(define (e3d_image-filename l) (list-ref l 8))
(define (e3d_image-name l) (list-ref l 9))
(define (e3d_image-extra l) (list-ref l 10))
(define (make-e3d_image . sc)
	(define default (list 'e3d_image 'r8g8b8 3 1 'lower_left 0 0 #f 'none '() '()))
	(define names   (list            'type   'bytes_pp 'alignment 'order 'width 'height 'image 'filename 'name 'extra))
	(**loader-construct-from sc default names)
	)


(define (**send-back-list l)
	(newline)
	(write l)
	(newline)
	(**flush-out))

(define (**send-back-list-ep l)
	(newline (current-error-port))
	(write l (current-error-port))
	(newline (current-error-port))
	(**flush-out))

(define (**returns-reply)
	(let ((reply (read)))
	  reply))

(define (**loader-get-script-directory path)
	;; Assuming only R5RS available, we need a function
	;; to get the directory.
	(let ((len (string-length path)))
		(let ((chr (string-ref path (- len 1)))
		      (sub (substring path 0 (- len 1))))
			(if (or (eq? chr #\\) (eq? chr #\/))
				(substring path 0 len)
				(**loader-get-script-directory sub)))))

;;
;; Returned replies from (**returns-reply) are always as a list, car
;; is used if there is only one value.
;;

(define (wings-set-variable! varname varval)
	(**send-back-list (list '%setvar varname varval))
	(car (**returns-reply)))
(define (wings-get-variable varname)
	(**send-back-list (list '%getvar varname))
	(car (**returns-reply)))
(define (wings-query str)
	(**send-back-list (list '%query str))
	(car (**returns-reply)))
(define (wings-we! . Args)
	(**send-back-list (list '%we! Args))
	(**returns-reply))
(define (wings-we . Args)
	(**send-back-list (list '%we Args))
	(car (**returns-reply)))
(define (wings-pb-message . Args)
	(define A1 (car Args))
	(define A2 (cdr Args))
	(cond
		((eq? A2 '())
			(wings-pb-message-1 A1))
		(#t
			(wings-pb-message-2 A1 (car A2)))))

(define (wings-pb-message-2 Prc Str)
	(**send-back-list-ep (list '%pbmessage Prc Str)))

(define (wings-pb-message-1 Str)
	(**send-back-list-ep (list '%pbmessage 1.0 Str)))

(define (wings-result-text List)
	(if (string? List)
		(wings-result-text (list List))
		(**send-back-list (list '%resulttext List))
		)
	)


;;
;;

(define (we:collapse:collapse_edge! Arg1 . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_collapse 'collapse_edge Arg1)) ;; int
        ((1) (let-values (((Arg2) (apply values Args)))
                (wings-we! 'wings_collapse 'collapse_edge Arg1 Arg2))) ;; int int
    ))
(define (we:collapse:collapse_edges! Arg1) (wings-we! 'wings_collapse 'collapse_edges Arg1)) ;; list
(define (we:collapse:collapse_faces! Arg1) (wings-we! 'wings_collapse 'collapse_faces Arg1)) ;; gbset
(define (we:collapse:collapse_vertices! Arg1) (wings-we! 'wings_collapse 'collapse_vertices Arg1)) ;; list
(define (we:collapse:fast_collapse_edge! Arg1) (wings-we! 'wings_collapse 'fast_collapse_edge Arg1)) ;; int

(define (we:dissolve:complement! Arg1) (wings-we! 'wings_dissolve 'complement Arg1)) ;; list
(define (we:dissolve:faces! Arg1) (wings-we! 'wings_dissolve 'faces Arg1)) ;; ordset
(define (we:dissolve:outer_edge_partition Arg1) (wings-we 'wings_dissolve 'outer_edge_partition Arg1)) ;; ordset

(define (we:edge:cut! Arg1 Arg2) (wings-we! 'wings_edge 'cut Arg1 Arg2)) ;; int int
(define (we:edge:dissolve_edge! Arg1) (wings-we! 'wings_edge 'dissolve_edge Arg1)) ;; int
(define (we:edge:dissolve_edges! Arg1 . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_edge 'dissolve_edges Arg1)) ;; list
        ((1) (let-values (((Arg2) (apply values Args) ))
                (wings-we! 'wings_edge 'dissolve_edges Arg1 Arg2))) ;; list list
    ))
(define (we:edge:dissolve_isolated_vs! Arg1) (wings-we! 'wings_edge 'dissolve_isolated_vs Arg1)) ;; list
(define (we:edge:fast_cut! Arg1 Arg2) (wings-we! 'wings_edge 'fast_cut Arg1 Arg2)) ;; int vec3_or_atom
(define (we:edge:from_faces Arg1) (wings-we 'wings_edge 'from_faces Arg1)) ;; list
(define (we:edge:from_vs Arg1) (wings-we 'wings_edge 'from_vs Arg1)) ;; list
(define (we:edge:length Arg1) (wings-we 'wings_edge 'length Arg1)) ;; int
(define (we:edge:reachable_faces Arg1 Arg2) (wings-we 'wings_edge 'reachable_faces Arg1 Arg2)) ;; gbset gbset
(define (we:edge:screaming_cut! Arg1 Arg2) (wings-we! 'wings_edge 'screaming_cut Arg1 Arg2)) ;; int vec3
(define (we:edge:select_region Arg1) (wings-we 'wings_edge 'select_region Arg1)) ;; gbset
(define (we:edge:to_vertices Arg1) (wings-we 'wings_edge 'to_vertices Arg1)) ;; list

(define (we:edge_cmd:loop_cut_partition Arg1) (wings-we 'wings_edge_cmd 'loop_cut_partition Arg1)) ;; list

(define (we:edge_loop:edge_links Arg1) (wings-we 'wings_edge_loop 'edge_links Arg1)) ;; gbset
(define (we:edge_loop:edge_loop_vertices Arg1) (wings-we 'wings_edge_loop 'edge_loop_vertices Arg1)) ;; gbset
(define (we:edge_loop:partition_edges Arg1) (wings-we 'wings_edge_loop 'partition_edges Arg1)) ;; gbset
(define (we:edge_loop:select_loop Arg1) (wings-we 'wings_edge_loop 'select_loop Arg1)) ;; ordset

(define (we:extrude_edge:extrude_edges! Arg1 Arg2) (wings-we! 'wings_extrude_edge 'extrude_edges Arg1 Arg2)) ;; list float

(define (we:extrude_face:faces! Arg1) (wings-we! 'wings_extrude_face 'faces Arg1)) ;; list
(define (we:extrude_face:regions! Arg1) (wings-we! 'wings_extrude_face 'regions Arg1)) ;; list

(define (we:face:are_neighbors Arg1 Arg2) (wings-we 'wings_face 'are_neighbors Arg1 Arg2)) ;; int int
(define (we:face:area Arg1) (wings-we 'wings_face 'area Arg1)) ;; int
(define (we:face:center Arg1) (wings-we 'wings_face 'center Arg1)) ;; int
(define (we:face:delete_bad_faces! Arg1) (wings-we! 'wings_face 'delete_bad_faces Arg1)) ;; list
(define (we:face:extend_border Arg1) (wings-we 'wings_face 'extend_border Arg1)) ;; gbset
(define (we:face:face_normal_ccw Arg1) (wings-we 'wings_face 'face_normal_ccw Arg1)) ;; list
(define (we:face:face_normal_cw Arg1) (wings-we 'wings_face 'face_normal_cw Arg1)) ;; list
(define (we:face:from_edges Arg1) (wings-we 'wings_face 'from_edges Arg1)) ;; list
(define (we:face:from_vs Arg1) (wings-we 'wings_face 'from_vs Arg1)) ;; list
(define (we:face:good_normal Arg1) (wings-we 'wings_face 'good_normal Arg1)) ;; int
(define (we:face:inner_edges Arg1) (wings-we 'wings_face 'inner_edges Arg1)) ;; list
(define (we:face:inner_outer_edges Arg1) (wings-we 'wings_face 'inner_outer_edges Arg1)) ;; list
(define (we:face:is_planar Arg1 Arg2) (wings-we 'wings_face 'is_planar Arg1 Arg2)) ;; float int
(define (we:face:is_thin Arg1) (wings-we 'wings_face 'is_thin Arg1)) ;; int
(define (we:face:mirror_matrix Arg1) (wings-we 'wings_face 'mirror_matrix Arg1)) ;; int
(define (we:face:normal Arg1 . Args)
    (case (length Args)
        ((0) (wings-we 'wings_face 'normal Arg1)) ;; int
        ((1) (let-values (((Arg2) (apply values Args) ))
                (wings-we 'wings_face 'normal Arg1 Arg2))) ;; int int
    ))
(define (we:face:outer_edges Arg1) (wings-we 'wings_face 'outer_edges Arg1)) ;; list
(define (we:face:to_edges Arg1) (wings-we 'wings_face 'to_edges Arg1)) ;; list
(define (we:face:to_vertices Arg1) (wings-we 'wings_face 'to_vertices Arg1)) ;; list
(define (we:face:vertex_positions Arg1 . Args)
    (case (length Args)
        ((0) (wings-we 'wings_face 'vertex_positions Arg1)) ;; int
        ((1) (let-values (((Arg2) (apply values Args) ))
                (wings-we 'wings_face 'vertex_positions Arg1 Arg2))) ;; int int
    ))
(define (we:face:vertices Arg1) (wings-we 'wings_face 'vertices Arg1)) ;; int
(define (we:face:vertices_ccw Arg1 . Args)
    (case (length Args)
        ((0) (wings-we 'wings_face 'vertices_ccw Arg1)) ;; int
        ((1) (let-values (((Arg2) (apply values Args) ))
                (wings-we 'wings_face 'vertices_ccw Arg1 Arg2))) ;; int int
    ))
(define (we:face:vertices_cw Arg1 . Args)
    (case (length Args)
        ((0) (wings-we 'wings_face 'vertices_cw Arg1)) ;; int
        ((1) (let-values (((Arg2) (apply values Args) ))
                (wings-we 'wings_face 'vertices_cw Arg1 Arg2))) ;; int int
    ))

(define (we:face_cmd:force_bridge! Arg1 Arg2 Arg3 Arg4) (wings-we! 'wings_face_cmd 'force_bridge Arg1 Arg2 Arg3 Arg4)) ;; int int int int
(define (we:face_cmd:mirror_faces! Arg1) (wings-we! 'wings_face_cmd 'mirror_faces Arg1)) ;; list

(define (we:facemat:all) (wings-we 'wings_facemat 'all))
(define (we:facemat:any_interesting_materials) (wings-we 'wings_facemat 'any_interesting_materials))
(define (we:facemat:assign! Arg1 . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_facemat 'assign Arg1)) ;; list
        ((1) (let-values (((Arg2) (apply values Args) ))
                (wings-we! 'wings_facemat 'assign Arg1 Arg2))) ;; val list
    ))
(define (we:facemat:delete_face! Arg1) (wings-we! 'wings_facemat 'delete_face Arg1)) ;; int
(define (we:facemat:delete_faces! Arg1) (wings-we! 'wings_facemat 'delete_faces Arg1)) ;; list
(define (we:facemat:face Arg1) (wings-we 'wings_facemat 'face Arg1)) ;; int
(define (we:facemat:gc!) (wings-we! 'wings_facemat 'gc))
(define (we:facemat:hide_faces!) (wings-we! 'wings_facemat 'hide_faces))
(define (we:facemat:is_material_used Arg1) (wings-we 'wings_facemat 'is_material_used Arg1)) ;; val
(define (we:facemat:keep_faces! Arg1) (wings-we! 'wings_facemat 'keep_faces Arg1)) ;; list
(define (we:facemat:mat_faces Arg1) (wings-we 'wings_facemat 'mat_faces Arg1)) ;; list
(define (we:facemat:show_faces! Arg1) (wings-we! 'wings_facemat 'show_faces Arg1)) ;; list
(define (we:facemat:used_materials) (wings-we 'wings_facemat 'used_materials))

(define (we:subdiv:get_proxy_info Arg1 Arg2) (wings-we 'wings_subdiv 'get_proxy_info Arg1 Arg2)) ;; list list
(define (we:subdiv:smooth! . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_subdiv 'smooth))
        ((4) (let-values (((Arg1 Arg2 Arg3 Arg4) (apply values Args)))
                (wings-we! 'wings_subdiv 'smooth Arg1 Arg2 Arg3 Arg4))) ;; list list list list
    ))
(define (we:subdiv:smooth_faces_htab) (wings-we 'wings_subdiv 'smooth_faces_htab))
(define (we:subdiv:subdiv! . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_subdiv 'subdiv))
        ((4) (let-values (((Arg1 Arg2 Arg3 Arg4) (apply values Args))) 
                (wings-we! 'wings_subdiv 'subdiv Arg1 Arg2 Arg3 Arg4))) ;; list list list list
    ))

(define (we:tesselation:quadrangulate! . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_tesselation 'quadrangulate))
        ((1) (let-values (((Arg1) (apply values Args) ))
                (wings-we! 'wings_tesselation 'quadrangulate Arg1))) ;; list
    ))
(define (we:tesselation:triangulate! . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_tesselation 'triangulate))
        ((1) (let-values (((Arg1) (apply values Args) ))
                (wings-we! 'wings_tesselation 'triangulate Arg1))) ;; gbset
    ))

(define (we:util:add_vpos Arg1) (wings-we 'wings_util 'add_vpos Arg1)) ;; list
(define (we:util:update_vpos Arg1) (wings-we 'wings_util 'update_vpos Arg1)) ;; list

(define (we:va:all Arg1) (wings-we 'wings_va 'all Arg1)) ;; atom
(define (we:va:any_attributes) (wings-we 'wings_va 'any_attributes))
(define (we:va:any_colors) (wings-we 'wings_va 'any_colors))
(define (we:va:any_uvs) (wings-we 'wings_va 'any_uvs))
(define (we:va:del_edge_attrs! Arg1) (wings-we! 'wings_va 'del_edge_attrs Arg1)) ;; int
(define (we:va:edge_attrs Arg1 Arg2 . Args)
    (case (length Args)
        ((0) (wings-we 'wings_va 'edge_attrs Arg1 Arg2)) ;; int atom
        ((1) (let-values (((Arg3) (apply values Args) ))
                (wings-we 'wings_va 'edge_attrs Arg1 Arg2 Arg3))) ;; int atom float
    ))
(define (we:va:face_attr Arg1 Arg2 . Args)
    (case (length Args)
        ((0) (wings-we 'wings_va 'face_attr Arg1 Arg2)) ;; atom int
        ((1) (let-values (((Arg3) (apply values Args) ))
                (wings-we 'wings_va 'face_attr Arg1 Arg2 Arg3))) ;; atom int int
    ))
(define (we:va:face_mixed_attrs Arg1) (wings-we 'wings_va 'face_mixed_attrs Arg1)) ;; int
(define (we:va:face_pos_attr Arg1 Arg2 Arg3) (wings-we 'wings_va 'face_pos_attr Arg1 Arg2 Arg3)) ;; atom int int
(define (we:va:gc!) (wings-we! 'wings_va 'gc))
(define (we:va:remove! Arg1 . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_va 'remove Arg1)) ;; atom
        ((1) (let-values (((Arg2) (apply values Args) ))
                (wings-we! 'wings_va 'remove Arg1 Arg2))) ;; atom list
    ))
(define (we:va:renumber! Arg1) (wings-we! 'wings_va 'renumber Arg1)) ;; val
(define (we:va:set_body_color! Arg1) (wings-we! 'wings_va 'set_body_color Arg1)) ;; vec3
(define (we:va:set_both_edge_attrs! Arg1 Arg2 Arg3) (wings-we! 'wings_va 'set_both_edge_attrs Arg1 Arg2 Arg3)) ;; int val val
(define (we:va:set_edge_attrs! Arg1 . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_va 'set_edge_attrs Arg1)) ;; list
        ((2) (let-values (((Arg2 Arg3) (apply values Args)))
                (wings-we! 'wings_va 'set_edge_attrs Arg1 Arg2 Arg3))) ;; int atom val
    ))
(define (we:va:set_edge_color! Arg1 Arg2 . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_va 'set_edge_color Arg1 Arg2)) ;; gbset vec3
        ((1) (let-values (((Arg3) (apply values Args)))
                (wings-we! 'wings_va 'set_edge_color Arg1 Arg2 Arg3))) ;; int vec3 vec3
    ))
(define (we:va:set_edge_colors! Arg1) (wings-we! 'wings_va 'set_edge_colors Arg1)) ;; list
(define (we:va:set_edge_uvs! Arg1) (wings-we! 'wings_va 'set_edge_uvs Arg1)) ;; list
(define (we:va:set_face_attr_vs! Arg1 Arg2 Arg3) (wings-we! 'wings_va 'set_face_attr_vs Arg1 Arg2 Arg3)) ;; atom int list
(define (we:va:set_face_attrs! Arg1 Arg2) (wings-we! 'wings_va 'set_face_attrs Arg1 Arg2)) ;; int val
(define (we:va:set_face_color! Arg1 Arg2) (wings-we! 'wings_va 'set_face_color Arg1 Arg2)) ;; gbset vec3
(define (we:va:set_vertex_color! Arg1 Arg2) (wings-we! 'wings_va 'set_vertex_color Arg1 Arg2)) ;; gbset vec3
(define (we:va:set_vtx_face_uvs! Arg1 Arg2 Arg3) (wings-we! 'wings_va 'set_vtx_face_uvs Arg1 Arg2 Arg3)) ;; int list vec2
(define (we:va:vtx_attrs Arg1 . Args)
    (case (length Args)
        ((0) (wings-we 'wings_va 'vtx_attrs Arg1)) ;; int
        ((1) (let-values (((Arg2) (apply values Args) ))
                (wings-we 'wings_va 'vtx_attrs Arg1 Arg2))) ;; int int
    ))

(define (we:vertex:bounding_box . Args)
    (case (length Args)
        ((0) (wings-we 'wings_vertex 'bounding_box))
        ((1) (let-values (((Arg1) (apply values Args) ))
                (wings-we 'wings_vertex 'bounding_box Arg1))) ;; val
        ((2) (let-values (((Arg1 Arg2) (apply values Args)))
                (wings-we 'wings_vertex 'bounding_box Arg1 Arg2))) ;; list val
    ))
(define (we:vertex:center . Args)
    (case (length Args)
        ((0) (wings-we 'wings_vertex 'center))
        ((1) (let-values (((Arg1) (apply values Args) ))
                (wings-we 'wings_vertex 'center Arg1))) ;; gbset
    ))
(define (we:vertex:connect! Arg1 Arg2) (wings-we! 'wings_vertex 'connect Arg1 Arg2)) ;; int list
(define (we:vertex:connect_cut! Arg1 Arg2) (wings-we! 'wings_vertex 'connect_cut Arg1 Arg2)) ;; int int
(define (we:vertex:dissolve_isolated! Arg1) (wings-we! 'wings_vertex 'dissolve_isolated Arg1)) ;; list
(define (we:vertex:edge_through Arg1 Arg2 . Args)
    (case (length Args)
        ((0) (wings-we 'wings_vertex 'edge_through Arg1 Arg2)) ;; int int
        ((1) (let-values (((Arg3) (apply values Args) ))
                (wings-we 'wings_vertex 'edge_through Arg1 Arg2 Arg3))) ;; int int int
    ))
(define (we:vertex:flatten! Arg1 Arg2 . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_vertex 'flatten Arg1 Arg2)) ;; list vec3
        ((1) (let-values (((Arg3) (apply values Args) ))
                (wings-we! 'wings_vertex 'flatten Arg1 Arg2 Arg3))) ;; list vec3 vec3
    ))
(define (we:vertex:force_connect! Arg1 Arg2 Arg3) (wings-we! 'wings_vertex 'force_connect Arg1 Arg2 Arg3)) ;; int int int
(define (we:vertex:from_edges Arg1) (wings-we 'wings_vertex 'from_edges Arg1)) ;; gbset
(define (we:vertex:from_faces Arg1) (wings-we 'wings_vertex 'from_faces Arg1)) ;; gbset
(define (we:vertex:isolated) (wings-we 'wings_vertex 'isolated))
(define (we:vertex:normal Arg1) (wings-we 'wings_vertex 'normal Arg1)) ;; int
(define (we:vertex:outer_vertices_ccw Arg1) (wings-we 'wings_vertex 'outer_vertices_ccw Arg1)) ;; list
(define (we:vertex:per_face Arg1) (wings-we 'wings_vertex 'per_face Arg1)) ;; list
(define (we:vertex:pos Arg1) (wings-we 'wings_vertex 'pos Arg1)) ;; int
(define (we:vertex:reachable Arg1) (wings-we 'wings_vertex 'reachable Arg1)) ;; list

(define (we:vertex_cmd:bevel_vertex! Arg1) (wings-we! 'wings_vertex_cmd 'bevel_vertex Arg1)) ;; int
(define (we:vertex_cmd:connect! Arg1) (wings-we! 'wings_vertex_cmd 'connect Arg1)) ;; list

(define (we:we:all_hidden) (wings-we 'wings_we 'all_hidden))
(define (we:we:break_mirror!) (wings-we! 'wings_we 'break_mirror))
(define (we:we:centroid) (wings-we 'wings_we 'centroid))
(define (we:we:create_holes! Arg1) (wings-we! 'wings_we 'create_holes Arg1)) ;; list
(define (we:we:create_mirror! Arg1) (wings-we! 'wings_we 'create_mirror Arg1)) ;; int
(define (we:we:fast_rebuild!) (wings-we! 'wings_we 'fast_rebuild))
(define (we:we:freeze_mirror!) (wings-we! 'wings_we 'freeze_mirror))
(define (we:we:fully_visible_edges Arg1) (wings-we 'wings_we 'fully_visible_edges Arg1)) ;; ordset
(define (we:we:hide_faces! Arg1) (wings-we! 'wings_we 'hide_faces Arg1)) ;; gbset
(define (we:we:invert_normals!) (wings-we! 'wings_we 'invert_normals))
(define (we:we:is_consistent) (wings-we 'wings_we 'is_consistent))
(define (we:we:is_face_consistent Arg1) (wings-we 'wings_we 'is_face_consistent Arg1)) ;; int
(define (we:we:is_open) (wings-we 'wings_we 'is_open))
(define (we:we:mirror_projection) (wings-we 'wings_we 'mirror_projection))
(define (we:we:new_id!) (wings-we! 'wings_we 'new_id))
(define (we:we:new_ids! Arg1) (wings-we! 'wings_we 'new_ids Arg1)) ;; int
(define (we:we:new_wrap_range Arg1 Arg2) (wings-we! 'wings_we 'new_wrap_range Arg1 Arg2)) ;; int int
(define (we:we:normals Arg1 Arg2) (wings-we 'wings_we 'normals Arg1 Arg2)) ;; list val
(define (we:we:num_hidden) (wings-we 'wings_we 'num_hidden))
(define (we:we:perimeter) (wings-we 'wings_we 'perimeter))
(define (we:we:rebuild!) (wings-we! 'wings_we 'rebuild))
(define (we:we:renumber! Arg1 . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_we 'renumber Arg1)) ;; int
        ((1) (let-values (((Arg2) (apply values Args) ))
                (wings-we! 'wings_we 'renumber Arg1 Arg2))) ;; int list
    ))
(define (we:we:separate!) (wings-we! 'wings_we 'separate))
(define (we:we:show_faces! . Args)
    (case (length Args)
        ((0) (wings-we! 'wings_we 'show_faces))
        ((1) (let-values (((Arg1) (apply values Args) ))
                (wings-we! 'wings_we 'show_faces Arg1))) ;; list
    ))
(define (we:we:surface_area) (wings-we 'wings_we 'surface_area))
(define (we:we:transform_vs! Arg1) (wings-we! 'wings_we 'transform_vs Arg1)) ;; matrix
(define (we:we:uv_mapped_faces) (wings-we 'wings_we 'uv_mapped_faces))
(define (we:we:uv_to_color!) (wings-we! 'wings_we 'uv_to_color))
(define (we:we:validate_mirror!) (wings-we! 'wings_we 'validate_mirror))
(define (we:we:visible . Args)
    (case (length Args)
        ((0) (wings-we 'wings_we 'visible))
        ((1) (let-values (((Arg1) (apply values Args) ))
                (wings-we 'wings_we 'visible Arg1))) ;; list
    ))
(define (we:we:visible_edges . Args)
    (case (length Args)
        ((0) (wings-we 'wings_we 'visible_edges))
        ((1) (let-values (((Arg1) (apply values Args) ))
                (wings-we 'wings_we 'visible_edges Arg1))) ;; gbset
    ))
(define (we:we:visible_vs . Args)
    (case (length Args)
        ((0) (wings-we 'wings_we 'visible_vs))
        ((1) (let-values (((Arg1) (apply values Args) ))
                (wings-we 'wings_we 'visible_vs Arg1))) ;; list
    ))
(define (we:we:volume) (wings-we 'wings_we 'volume))



;; Long running process
;;
;; Example:
;; 
;; (display "Started")(newline)
;; (long-running-process
;; 	(lambda ()
;; 		(let loop ((N 100000000))
;; 			(if (> N 0)
;; 				(loop (- N 1))
;; 				'ok))
;; 	))
;; (display "Done")(newline)
;; 

(define (long-running-process Fun)
	;; A thread that simply sends keep-alive messages back to the scripting
	;; plugin to keep it from timing out while the script processes something
	;; that takes a while.
	;;
	(define StandbyState #f)
	(define StandbySema  (make-semaphore 1))
	(define StandbyThread
		(make-thread (^[] (guard (e [else (report-error e) #f])
			(define (get_val)
				(semaphore-acquire! StandbySema)
				(let ((Val (if StandbyState #t #f)))
					(semaphore-release! StandbySema)
					Val))
			(let loop ()
				(if (get_val)
					'done
					(let ()
						;; The standard output seems to have some sort of buffering
						;; that prevents it from sending from this thread, current
						;; workaround is sending to standard error to bypass the buffering
						(**send-back-list-ep (list '%keepalive 0))
						(thread-sleep! 1)
						(loop))))
		))))

	(thread-start! StandbyThread)
	(unwind-protect (Fun)
		(semaphore-acquire! StandbySema)
		(set! StandbyState #t)
		(semaphore-release! StandbySema)
		(thread-join! StandbyThread)
		)
	)


(define (script-loop)
	(define cmd (read))
	(case (list-ref cmd 0)
		((run_init)
			(begin
				(set! *script-full-path* (list-ref cmd 1))
				(set! *script-directory* (**loader-get-script-directory *script-full-path*))
				(**add-to-load-path *script-directory*)
				(set-lang-string (list-ref cmd 2))
				(begin (load *script-full-path*))
				(if (equal? #f *scr-main-fun*)
					(begin
						(display "ERROR: main function not set" (current-error-port))
						(newline (current-error-port))
						(exit))
					(begin
						(**send-back-list'(ok))
						(script-loop))))
			)
		((run)
			(begin
				(let ((params (list-ref cmd 2))
					  (extra-params (list-ref cmd 3)))
					(define returned (*scr-main-fun* params extra-params))
                    (if (not (undefined? returned))
                        (begin
                            (newline)
                            (write returned)
                            )
                        (begin
                            )
                        )
                    )
				(newline)
				(**flush-out)
				(**send-back-list '(%ok))
				(script-loop))
			)
		(else
			(begin
				(display "Not run")
				(newline)
				(exit))))
	)

(script-loop)

