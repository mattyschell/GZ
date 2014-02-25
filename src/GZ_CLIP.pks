CREATE OR REPLACE PACKAGE GZ_CLIP
AUTHID CURRENT_USER
AS
   --Matt! 3/01/2010
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --TYPES-----------------------------------------------------------------------------
   --These should go in GZ_TYPES but I dont want to interrupt actual work
   TYPE nodearray    IS TABLE OF SDO_GEOMETRY INDEX BY PLS_INTEGER;
   TYPE geomarray    IS TABLE OF SDO_GEOMETRY INDEX BY PLS_INTEGER; --I know
   TYPE stringhash IS TABLE OF VARCHAR2(4000) INDEX BY VARCHAR2(4000);
   TYPE numberhash IS TABLE OF NUMBER INDEX BY VARCHAR2(4000);
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------
   FUNCTION PIPE_MODULES (
      p_masks           IN MDSYS.STRING_ARRAY
   ) RETURN GZ_TYPES.GEN_CLIP_MODULES PIPELINED;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GET_MASKS (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_clip_mask      IN VARCHAR2 DEFAULT NULL --optional state based
   ) RETURN GZ_TYPES.stringarray;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GET_MODULES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_modules        IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.stringarray;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GET_NODE_GEOMETRY (
      p_topo            IN VARCHAR2,
      p_node_id         IN VARCHAR2
   ) RETURN SDO_GEOMETRY;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GET_EDGE_GEOMETRY (
      p_topo            IN VARCHAR2,
      p_edge_id         IN VARCHAR2
   ) RETURN SDO_GEOMETRY;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GET_CLIP_PARAMETERS (
      p_release                  IN VARCHAR2,
      p_project_id               IN VARCHAR2,
      p_jobid                    IN VARCHAR2
   ) RETURN GZ_TYPES.GEN_CLIP_JOBRUNS_REC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GET_CLIP_PROJECT_PARAMETERS (
      p_release                  IN VARCHAR2,
      p_project_id               IN VARCHAR2
   ) RETURN GZ_TYPES.GEN_CLIP_PARAMETERS_REC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GET_SOURCE_SCHEMA (
      p_input                    IN VARCHAR2
   ) RETURN VARCHAR2 DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   
   FUNCTION GET_SOURCE_TOPOLOGY (
      p_input                    IN VARCHAR2
   ) RETURN VARCHAR2 DETERMINISTIC;
   
   FUNCTION GET_EDGE_TABLE (
      p_project_id            IN VARCHAR2,
      p_jobid                 IN VARCHAR2,
      p_edge_input_table      IN VARCHAR2
   ) RETURN VARCHAR2 DETERMINISTIC;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   PROCEDURE ADD_LINES_FROM_SPATIAL (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_jobid              IN VARCHAR2,
      p_toponame           IN VARCHAR2,
      p_featuretable       IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_featuretable_id    IN VARCHAR2,
      p_validate_geom      IN VARCHAR2 DEFAULT 'NO',
      p_validate_tol       IN NUMBER DEFAULT .05,
      p_geom_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_val         IN VARCHAR2 DEFAULT NULL,
      p_topomap_mbr        IN SDO_GEOMETRY DEFAULT NULL
   );
   
   PROCEDURE CONNECT_DANGLE_TO_NN_NODE (
      p_topo               IN VARCHAR2,
      p_source_table       IN VARCHAR2,
      p_source_pkc         IN VARCHAR2,
      p_source_value       IN NUMBER,
      p_tolerance          IN NUMBER,
      p_how_desperate      IN NUMBER DEFAULT 10
   );
   
    PROCEDURE ADD_NODES_AROUND_DANGLE(
      p_topo               IN VARCHAR2,
      p_source_table       IN VARCHAR2,
      p_source_pkc         IN VARCHAR2,
      p_source_value       IN NUMBER,
      p_tolerance          IN NUMBER
   );

   FUNCTION FLIP_INVERTED_FACE (
      p_release         IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_topo            IN VARCHAR2, --working topo
      newedgetab        IN VARCHAR2, --working edge table with L/R
      intopo            IN VARCHAR2, --original input topo
      inedgetab         IN VARCHAR2, --edge table from transfer attributes
      badface           IN VARCHAR2, --the face we IDd
      left_right_atts   IN VARCHAR2,
      p_face_mask       IN VARCHAR2
   ) RETURN VARCHAR2;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION BUILD_TOPO_FROM_TOPO (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_jobid              IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_table              IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_topo_type          IN NUMBER,
      p_topo_col           IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN VARCHAR2;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION REMOVE_NODE_AND_RESHAPE (
      p_schema          IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_mask            IN VARCHAR2,
      p_newtopo         IN VARCHAR2,
      p_old_edge        IN NUMBER,
      p_new_edge        IN NUMBER,
      p_old_node        IN NUMBER,
      p_new_node        IN NUMBER,
      p_tip             IN VARCHAR2
   ) RETURN VARCHAR2;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION FIND_FAR_EDGE (
      p_schema          IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_mask            IN VARCHAR2,
      p_newtopo         IN VARCHAR2,
      p_new_node        IN NUMBER,
      p_edge_1          IN NUMBER,
      p_edge_1_tip      IN VARCHAR2,
      p_edge_2          IN NUMBER,
      p_edge_2_tip      IN VARCHAR2,
      p_edge_3          IN NUMBER DEFAULT NULL,
      p_edge_3_tip      IN VARCHAR2 DEFAULT NULL
   ) RETURN NUMBER;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION EXTEND_DANGLING_V_NODES (
      p_schema          IN VARCHAR2,
      p_release         IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_mask            IN VARCHAR2,
      p_newtopo         IN VARCHAR2,
      p_oldtopo         IN VARCHAR2,
      p_newedgetable    IN VARCHAR2,
      p_oldedgetable    IN VARCHAR2,
      p_oldnode         IN NUMBER,
      p_srid            IN NUMBER,
      p_tolerance       IN NUMBER,
      p_debug           IN PLS_INTEGER
   ) RETURN NUMBER;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   PROCEDURE ADD_VERTEX_ON_EDGE (
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_edge            IN NUMBER,
      p_node            IN SDO_GEOMETRY,
      p_coord_index     IN NUMBER
   );
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION EXTEND_INTERIOR_DANGLES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_debug          IN PLS_INTEGER DEFAULT NULL
   ) RETURN VARCHAR2;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION NODE_EXISTS (
      p_schema         IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo           IN VARCHAR2,
      p_test_table     IN VARCHAR2,
      p_edge_id        IN NUMBER,
      p_tip            IN VARCHAR2
   ) RETURN PLS_INTEGER;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   --public for debug
   PROCEDURE DELETE_AN_EDGE (
      p_schema         IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_topo           IN VARCHAR2,
      p_feature_table  IN VARCHAR2,
      p_feature_edge   IN NUMBER,
      p_edge_id        IN NUMBER,
      p_tg_layer_id    IN NUMBER,
      p_topomap        IN VARCHAR2,
      p_retain_feature IN VARCHAR2 DEFAULT 'N',
      p_retain_prim    IN VARCHAR2 DEFAULT 'N',
      p_reverse_flag   IN VARCHAR2 DEFAULT NULL
   );

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION QUERY_DELIMITED_LIST (
      p_input     IN GZ_TYPES.stringarray,
      p_query     IN VARCHAR2
   ) RETURN NUMBER DETERMINISTIC;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION QUERY_DELIMITED_LIST (
      p_input     IN GZ_TYPES.stringarray,
      p_query     IN NUMBER
   ) RETURN NUMBER DETERMINISTIC;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   --public for debug

   FUNCTION ID_DANGLES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2
   ) RETURN VARCHAR2;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION DELETE_EXTERIOR_DANGLES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2
   ) RETURN VARCHAR2;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION DELETE_DANGLING_FACES (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2
   ) RETURN VARCHAR2;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   PROCEDURE UHAUL_NODE (
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_dangle_edge     IN NUMBER,
      p_dangle_node     IN NUMBER,
      p_percent         IN NUMBER DEFAULT .5
   );
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION POPULATE_MEASUREMENTS (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_mask           IN VARCHAR2,
      p_topo_out       IN VARCHAR2,
      p_validate_topo  IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge       IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge      IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa     IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   PROCEDURE REAREND_NODE (
      p_project_id      IN VARCHAR2,
      p_jobid           IN VARCHAR2,
      p_topo            IN VARCHAR2,
      p_dangle_edge     IN NUMBER,
      p_dangle_node     IN NUMBER,
      p_percent         IN NUMBER DEFAULT .5
   );
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION APPROX_GC_DISTANCE (
      x1 IN OUT NOCOPY NUMBER,
      y1 IN OUT NOCOPY NUMBER,
      x2 IN OUT NOCOPY NUMBER,
      y2 IN OUT NOCOPY NUMBER
   ) RETURN NUMBER DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION PERPENDICULAR (
      xin IN OUT NOCOPY NUMBER,
      yin IN OUT NOCOPY NUMBER,  -- the point
      x1 IN OUT NOCOPY NUMBER,
      y1 IN OUT NOCOPY NUMBER,  -- the line
      x2 IN OUT NOCOPY NUMBER,
      y2 IN OUT NOCOPY NUMBER,
      xnear IN OUT NOCOPY NUMBER,
      ynear IN OUT NOCOPY NUMBER,  -- the perpendicular
      always BOOLEAN default FALSE,  -- always return a distance
      meters BOOLEAN default FALSE
   )  -- when true return meters
   RETURN NUMBER deterministic;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   ---------------------------------------------
   --- Primary entry point  --
   ---------------------------------------------
   FUNCTION GENERALIZATION_CLIP (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_jobid_in           IN VARCHAR2,
      p_clip_edge_tab      IN VARCHAR2,
      p_gen_clip_mask      IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_topo_in            IN VARCHAR2 DEFAULT NULL,
      p_drop_module_tab    IN VARCHAR2 DEFAULT 'Y',
      p_transfer_atts      IN VARCHAR2 DEFAULT 'Y',
      p_validate_input     IN VARCHAR2 DEFAULT 'Y',
      p_modules            IN VARCHAR2 DEFAULT NULL,
      p_validate_topo      IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge           IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge          IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa         IN VARCHAR2 DEFAULT 'N'    
   ) RETURN NUMBER;
   ---------------------------------------------
   ---   --
   ---------------------------------------------
   ---------------------------------------------
   --- Create empty work tables  --
   ---------------------------------------------
   FUNCTION NEW_GEN_CLIP_PARAMETERS
   RETURN GZ_TYPES.GEN_CLIP_PARAMETERS PIPELINED;

   FUNCTION NEW_GEN_CLIP_MODULES_LOG
   RETURN GZ_TYPES.GEN_CLIP_MODULES PIPELINED;
   FUNCTION NEW_GEN_CLIP_EXPELLED
   RETURN GZ_TYPES.GEN_CLIP_EXPELLED PIPELINED;
   FUNCTION NEW_GEN_CLIP_JOBRUNS
   RETURN GZ_TYPES.GEN_CLIP_JOBRUNS PIPELINED;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   --One table per schema
   PROCEDURE CREATE_GEN_CLIP_PARAMETERS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_CLIP_PARAMETERS'
   );
   --one table per release
   PROCEDURE CREATE_GEN_CLIP_JOBRUNS (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_jobid          IN VARCHAR2,
      p_table_name     IN VARCHAR2 DEFAULT 'CLIP_JOBRUNS'
   );
   --one table per project
   PROCEDURE CREATE_GEN_CLIP_MODULE_LOG (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_CLIP_MODULES_XXX'
   );
   --one table per job
   PROCEDURE CREATE_GEN_CLIP_TRACKING_LOG (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2 DEFAULT 'CLIP_TRACKING'
   );
   --one table per job
   PROCEDURE CREATE_GEN_CLIP_EXPELLED (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_CLIP_EXPELLED_XXX'
   );
   ---------------------------------------------
    PROCEDURE CREATE_EDGE_ATTRIBUTE (
      p_schema             IN VARCHAR2,
      p_tab_name           IN VARCHAR2,
      p_type               IN VARCHAR2 DEFAULT NULL
   );
   ---------------------------------------------
   PROCEDURE BUILD_EDGE_ATTRIBUTE_TABLE (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_tab_name           IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_type               IN VARCHAR2 DEFAULT NULL,
      p_zref_table         IN VARCHAR2 DEFAULT NULL,
      p_special_use        IN VARCHAR2 DEFAULT NULL,
      p_calc_atts          IN VARCHAR2 DEFAULT 'Y',
      p_tolerance          IN VARCHAR2 DEFAULT .05,
      p_face_tab           IN VARCHAR2 DEFAULT NULL,
      p_log_tag            IN VARCHAR2
   );
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   --lower level utils
   FUNCTION GET_XYS(
      Geom              IN MDSYS.SDO_GEOMETRY,
      ring1_to_find     PLS_INTEGER,
      edge1_to_find     PLS_INTEGER,
      ring2_to_find     PLS_INTEGER DEFAULT NULL,
      edge2_to_find     PLS_INTEGER DEFAULT NULL
   ) RETURN sdo_geometry;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   PROCEDURE MAKE_ARROWHEAD(
      x1 number default 0.,
      y1 number default 0.,
      x2 number default 1.,
      y2 number default 1.,
      delta number default 1.
   );
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION MAKE_ARROWHEAD(
      p_geom            IN SDO_GEOMETRY,
      p_scalefactor     IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION SCALE2PERCENT(
      p_geom            IN SDO_GEOMETRY,
      p_scalefactor     IN NUMBER
   ) RETURN SDO_GEOMETRY;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION FIRSTSEG(
      p_geom            IN SDO_GEOMETRY,
      p_last            IN VARCHAR2 DEFAULT NULL
   ) RETURN sdo_geometry;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION FIND_COORD_INDEX (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY
   ) RETURN PLS_INTEGER DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION ACCURATE_FIND_COORD_INDEX (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY,
      p_debug          IN PLS_INTEGER DEFAULT 0
   ) RETURN PLS_INTEGER DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION ACCURATE_FIND_COORD_INDEX_OLD (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY,
      p_debug          IN PLS_INTEGER DEFAULT 0
   ) RETURN PLS_INTEGER;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GZ_SEGMENT_RANGE (
      p_seg_geom             IN SDO_GEOMETRY,
      p_start                IN NUMBER,
      p_end                  IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GZ_ADD_MIDPOINT (
      p_geometry        IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GZ_PROJECT_PT (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY,
      p_tolerance      IN NUMBER DEFAULT 0.00000001
   ) RETURN SDO_GEOMETRY DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GZ_FIND_MEASURE (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY
   ) RETURN NUMBER DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GZ_LOCATE_PT (
      p_edge           IN SDO_GEOMETRY,
      p_measure        IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GET_SEGMENT_FROM_INDEX (
      p_edge           IN SDO_GEOMETRY,
      p_index          IN PLS_INTEGER
   ) RETURN GZ_TYPES.stringarray DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION GET_SEGMENT_GEOM_FROM_INDEX (
      p_edge           IN SDO_GEOMETRY,
      p_index          IN PLS_INTEGER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION MAKE_GEOM_FROM_SIDEY_SEGMENT (
      p_coords          IN GZ_TYPES.stringarray,
      p_srid            IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION MAKE_GEOM_FROM_SIDEY_SEGMENT (
      p_x1              IN NUMBER,
      p_y1              IN NUMBER,
      p_x2              IN NUMBER,
      p_y2              IN NUMBER,
      p_srid            IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION SLOPE_CALCULATOR (
      p_x1                          IN NUMBER,
      p_y1                          IN NUMBER,
      p_x2                          IN NUMBER,
      p_y2                          IN NUMBER
   ) RETURN NUMBER DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION STRINGARRAY_TO_VARRAY (
      p_input     IN GZ_TYPES.stringarray
   ) RETURN MDSYS.STRING_ARRAY DETERMINISTIC;
   FUNCTION STRINGARRAY2HASH (
      p_input     IN GZ_TYPES.stringarray
   ) RETURN GZ_CLIP.stringhash DETERMINISTIC;
   FUNCTION STRINGARRAY_TO_VARCHAR (
      p_input     IN GZ_TYPES.stringarray,
      p_delim     IN VARCHAR2
   ) RETURN VARCHAR2 DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION SHOW_ME_COORD_INDEX (
      p_index     IN NUMBER,
      p_input     IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION ORDINATE_ROUNDER (
      p_geometry                IN SDO_GEOMETRY,
      p_places                  IN PLS_INTEGER DEFAULT 6
   ) RETURN SDO_GEOMETRY DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   PROCEDURE CLEAN_UP_GEOMETRIES (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2,
      p_gtype          IN NUMBER,
      p_column_name    IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_skip_dups      IN VARCHAR2 DEFAULT 'NO',
      p_precision_too  IN VARCHAR2 DEFAULT 'NO',
      p_digits_right   IN NUMBER DEFAULT 6,
      p_tolerance      IN NUMBER DEFAULT .05
   );
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION FIX_INVALID_GEOMETRIES (
      p_schema          IN VARCHAR2,
      p_table_name      IN VARCHAR2,
      p_pkccol          IN VARCHAR2,
      p_gtype           IN NUMBER,
      p_topo            IN VARCHAR2,
      p_column_name     IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_flag_col        IN VARCHAR2 DEFAULT 'QC',
      p_intersect_size  IN NUMBER DEFAULT NULL,
      p_intersect_ratio IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION PARSE_CRSWKT (
      p_srid          IN NUMBER,
      p_item_no       IN NUMBER,
      p_keyword1      IN VARCHAR2,
      p_keyword2      IN VARCHAR2 DEFAULT NULL,
      p_keyword3      IN VARCHAR2 DEFAULT NULL,
      p_keyword4      IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   FUNCTION FULL_DIST_CONVERT (
      p_input         IN NUMBER,
      p_input_units   IN VARCHAR2,
      p_output_units  IN VARCHAR2 DEFAULT NULL
   ) RETURN NUMBER DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
    FUNCTION TOLERANCE_CONVERTER (
      p_input         IN NUMBER,
      p_input_srid    IN VARCHAR2 DEFAULT '8265',
      p_output_srid   IN VARCHAR2 DEFAULT 'NULL',
      p_alaska_flag   IN VARCHAR2 DEFAULT NULL
   ) RETURN NUMBER DETERMINISTIC;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


END GZ_CLIP;
/
