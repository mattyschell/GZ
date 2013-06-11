CREATE OR REPLACE PACKAGE GZ_UTILITIES
AUTHID CURRENT_USER
AS

   -----------------------------------------------------------------------------
   -- Sidey potpourri
   -----------------------------------------------------------------------------

PROCEDURE CREATE_PROJECTION_SEQ(seq_name VARCHAR2 default 'GZ_PROJECT_SEQ');
PROCEDURE CREATE_GZ_PROJECTION_WRK2003(
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_PROJECT_WRK2003');
PROCEDURE CREATE_GZ_PROJECTION_WRKAREA(
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_PROJECT_WRKAREA');
PROCEDURE CREATE_GZ_PROJECTION_WRKOUT(
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_PROJECT_WRKOUT');
PROCEDURE CREATE_GZ_PROJECTION_OUTPUT(
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_PROJECT_OUTPUT');

-- Reshape an "L" shaped edge that is too close to another edge by moving the "L"
-- vertex away.
PROCEDURE RESHAPE_LEDGE(State VARCHAR2,ptopology VARCHAR2, face_no NUMBER, pUS_STATE_table VARCHAR2 default 'ACS09_SL040',pEntityfp VARCHAR2 default 'STATEFP');
-- Highly accurate distance alculation for geodetic coordinates
FUNCTION ACCURATE_GCD( x1 IN OUT NOCOPY NUMBER, y1 IN OUT NOCOPY NUMBER,
                       x2 IN OUT NOCOPY NUMBER, y2 IN OUT NOCOPY NUMBER)
                       RETURN NUMBER DETERMINISTIC;
-- Function to find the closest edge
FUNCTION FIND_CLOSEST_EDGE(pTopology VARCHAR2,face_id NUMBER) RETURN MDSYS.SDO_LIST_TYPE;
-- Function to create a reshaped edge that is not to close to the nearest edge.
FUNCTION GET_NEAREST_EDGE (pTopology VARCHAR2,face_id NUMBER,edge_id IN OUT NOCOPY NUMBER,pInSchema VARCHAR2 default 'GZDEC10ST',tolerance NUMBER default 0.05,decim_digits NUMBER default 7) RETURN MDSYS.SDO_GEOMETRY;

FUNCTION TABLE_EXISTS ( pInTable  IN VARCHAR2, pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN;

-- Function to test whether a point is on a line and its near point on the
-- perpendicular from th point to the line.
FUNCTION Is_pt_on_Segment(xtest IN OUT NOCOPY NUMBER,ytest IN OUT NOCOPY NUMBER, -- test point
                       x1 IN OUT NOCOPY NUMBER,y1 IN OUT NOCOPY NUMBER,  -- the line
                       x2 IN OUT NOCOPY NUMBER,y2 IN OUT NOCOPY NUMBER,
                       xnear IN OUT NOCOPY NUMBER,ynear IN OUT NOCOPY NUMBER,
                       pu IN OUT NOCOPY NUMBER) RETURN NUMBER DETERMINISTIC;

-- Function to perform a Cross-product and determine the clockwiseness of
-- 3 points.
FUNCTION ORIENT2D (paX   NUMBER,  paY   NUMBER,
                      pbX   NUMBER,  pbY   NUMBER,
                      pcX   NUMBER,  pcY   NUMBER)

            RETURN          NUMBER
            Deterministic;

-- Function to perform a simple 2-D line intersection with no glancing
-- distance calculated. It intersects or does not or is paralle.
FUNCTION SIMPLE_INTERSECT( X1 IN OUT NOCOPY NUMBER,
                              Y1 IN OUT NOCOPY NUMBER,
                              X2 IN OUT NOCOPY NUMBER,
                              Y2 IN OUT NOCOPY NUMBER,
                              X3 IN OUT NOCOPY NUMBER,
                              Y3 IN OUT NOCOPY NUMBER,
                              X4 IN OUT NOCOPY NUMBER,
                              Y4 IN OUT NOCOPY NUMBER,
                              Xi IN OUT NOCOPY NUMBER,
                              Yi IN OUT NOCOPY NUMBER
                              )
            RETURN NUMBER Deterministic;

-- Function to reshape an edge in the topology by calling SDO_TOPO_MAP.CHANGE_EDGE_COORDS
FUNCTION SWAP_EDGE_COORDS(Topology VARCHAR2,Edge_id NUMBER, Geometry IN OUT NOCOPY MDSYS.SDO_GEOMETRY) RETURN VARCHAR2;

FUNCTION before(pString VARCHAR2,
pSearchString VARCHAR2,pAppearance NUMBER DEFAULT 1)
RETURN VARCHAR2;



   -----------------------------------------------------------------------------
   -- Tracking tables
   -----------------------------------------------------------------------------
   

   PROCEDURE CREATE_GEN_XTEND_TRACKING_LOG (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE GEN_TRACKING_LOG (
     p_process        IN VARCHAR2,
     p_table_name     IN VARCHAR2 DEFAULT NULL,
     p_step           IN VARCHAR2 DEFAULT NULL,
     p_start_time     IN TIMESTAMP DEFAULT NULL,
     p_end_time       IN TIMESTAMP DEFAULT NULL,
     p_release        IN VARCHAR2 DEFAULT NULL,
     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
     p_deploy         IN VARCHAR2 DEFAULT NULL
   );

   PROCEDURE GEN_CLIP_TRACKING_LOG (
     p_jobrun         IN VARCHAR2,
     p_process        IN VARCHAR2,
     p_table_name     IN VARCHAR2 DEFAULT NULL,
     p_step           IN VARCHAR2 DEFAULT NULL,
     p_start_time     IN TIMESTAMP DEFAULT NULL,
     p_end_time       IN TIMESTAMP DEFAULT NULL,
     p_release        IN VARCHAR2 DEFAULT NULL,
     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
     p_deploy         IN VARCHAR2 DEFAULT NULL,
     p_error_msg      IN VARCHAR2 DEFAULT NULL,
     p_sdo_dump       IN SDO_GEOMETRY DEFAULT NULL
   );

   PROCEDURE GEN_EXTENDED_TRACKING_LOG (
     p_module         IN VARCHAR2,
     p_jobrun         IN VARCHAR2,
     p_process        IN VARCHAR2,
     p_table_name     IN VARCHAR2 DEFAULT NULL,
     p_step           IN VARCHAR2 DEFAULT NULL,
     p_start_time     IN TIMESTAMP DEFAULT NULL,
     p_end_time       IN TIMESTAMP DEFAULT NULL,
     p_release        IN VARCHAR2 DEFAULT NULL,
     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
     p_deploy         IN VARCHAR2 DEFAULT NULL,
     p_error_msg      IN VARCHAR2 DEFAULT NULL,
     p_sdo_dump       IN SDO_GEOMETRY DEFAULT NULL
   );

   PROCEDURE GEN_SP_TRACKING_LOG (
     p_jobrun         IN VARCHAR2,
     p_process        IN VARCHAR2,
     p_table_name     IN VARCHAR2 DEFAULT NULL,
     p_step           IN VARCHAR2 DEFAULT NULL,
     p_start_time     IN TIMESTAMP DEFAULT NULL,
     p_end_time       IN TIMESTAMP DEFAULT NULL,
     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
     p_message      IN VARCHAR2 DEFAULT NULL
   );
   -- FSL Tracking logger (copied from Matts)
   PROCEDURE GEN_FSL_TRACKING_LOG (
     p_jobrun         IN VARCHAR2,
     p_process        IN VARCHAR2,
     p_table_name     IN VARCHAR2 DEFAULT NULL,
     p_step           IN VARCHAR2 DEFAULT NULL,
     p_start_time     IN TIMESTAMP DEFAULT NULL,
     p_end_time       IN TIMESTAMP DEFAULT NULL,
     p_sqlstmt        IN VARCHAR2 DEFAULT NULL,
     p_message      IN VARCHAR2 DEFAULT NULL
   );


   -----------------------------------------------------------------------------
   -- Metadata and Index
   -----------------------------------------------------------------------------
   
   FUNCTION GET_SDO_TOPO_METADATA_CHAR (
      p_topo                     IN VARCHAR2,
      p_table                    IN VARCHAR2,
      p_what                     IN VARCHAR2,
      p_column                   IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN VARCHAR2;
   
   FUNCTION GET_SDO_TOPO_METADATA_NUM (
      p_topo                     IN VARCHAR2,
      p_table                    IN VARCHAR2,
      p_what                     IN VARCHAR2,
      p_column                   IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN NUMBER;
   
    FUNCTION GET_X_OF_THA_DOT (
      p_input                    IN VARCHAR2,
      p_x                        IN VARCHAR2 DEFAULT 'LEFT'
   ) RETURN VARCHAR2 DETERMINISTIC;
   

    PROCEDURE INSERT_SDOGEOM_METADATA (
      p_table_name      IN VARCHAR2,
      p_column_name     IN VARCHAR2,
      p_srid            IN NUMBER,
      p_tolerance       IN NUMBER DEFAULT .05
   );

   PROCEDURE ADD_SPATIAL_INDEX (
      p_table_name      IN VARCHAR2,
      p_column_name     IN VARCHAR2,
      p_srid            IN NUMBER,
      p_tolerance       IN NUMBER,
      p_local           IN VARCHAR2 DEFAULT NULL,
      p_parallel        IN NUMBER DEFAULT NULL,
      p_idx_name        IN VARCHAR2 DEFAULT NULL
   );

   PROCEDURE ADD_INDEX (
    p_table_name      IN VARCHAR2,
    p_index_name      IN VARCHAR2,
    p_column_name     IN VARCHAR2,
    p_type            IN VARCHAR2 DEFAULT NULL,
    p_parallel        IN NUMBER DEFAULT NULL,
    p_logging         IN VARCHAR2 DEFAULT NULL,
    p_local           IN VARCHAR2 DEFAULT NULL
   );

   PROCEDURE GATHER_TABLE_STATS (
    p_table_name      IN VARCHAR2
   );

   PROCEDURE GATHER_TOPO_STATS (
    p_topo_name      IN VARCHAR2
   );
   
   FUNCTION INDEX_EXISTS (
      p_table_name        IN VARCHAR2,
      p_column_name       IN VARCHAR2 DEFAULT NULL,
      p_index_type        IN VARCHAR2 DEFAULT NULL
   ) RETURN BOOLEAN;
   
   FUNCTION COLUMN_EXISTS (
      p_table_name        IN VARCHAR2,
      p_column_list       IN VARCHAR2,
      p_column_kount      IN PLS_INTEGER DEFAULT 1
   ) RETURN BOOLEAN;
   
   
   -----------------------------------------------------------------------------
   -- Types and converters
   -----------------------------------------------------------------------------
   

   FUNCTION SPLIT (
      p_str   IN VARCHAR2,
      p_regex IN VARCHAR2 DEFAULT NULL,
      p_match IN VARCHAR2 DEFAULT NULL,
      p_end   IN NUMBER DEFAULT 0
   ) RETURN GZ_TYPES.stringarray DETERMINISTIC;

   FUNCTION STRINGARRAY_TO_VARRAY (
      p_input     IN GZ_TYPES.stringarray
   ) RETURN MDSYS.STRING_ARRAY DETERMINISTIC;

    FUNCTION NUMARRAY_TO_VARRAY (
      p_input     IN GZ_TYPES.numberarray
   ) RETURN MDSYS.STRING_ARRAY DETERMINISTIC;

   FUNCTION NUMARRAY_TO_ORDARRAY (
      p_input     IN MDSYS.SDO_NUMBER_ARRAY
   ) RETURN MDSYS.SDO_ORDINATE_ARRAY DETERMINISTIC;

   FUNCTION STRINGARRAY_ADD (
      p_input_1           IN GZ_TYPES.stringarray,
      p_input_2           IN GZ_TYPES.stringarray
   ) RETURN GZ_TYPES.stringarray DETERMINISTIC;
   
   FUNCTION CLOB_TO_VARRAY (
      p_input           IN CLOB,
      p_delimiter       IN VARCHAR2 DEFAULT ','
   ) RETURN MDSYS.STRING_ARRAY DETERMINISTIC;
   
   FUNCTION CONVERT_COL_TO_CLOB (
      p_sql             IN VARCHAR2,
      p_delimiter       IN VARCHAR2 DEFAULT ','      
   ) RETURN CLOB;
   
   
   -----------------------------------------------------------------------------
   -- Topo creators
   -----------------------------------------------------------------------------
      

   PROCEDURE BUILD_TOPO_FROM_SPATIAL (
      p_toponame           IN VARCHAR2,
      p_featuretable       IN VARCHAR2,
      p_topo_type          IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_validate_geom      IN VARCHAR2 DEFAULT 'NO',
      p_validate_tol       IN NUMBER DEFAULT .05,
      p_fix_geom           IN VARCHAR2 DEFAULT 'NO',
      p_topo_col           IN VARCHAR2 DEFAULT 'TOPOGEOM',
      p_geom_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_oid_col            IN VARCHAR2 DEFAULT NULL,
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_val         IN VARCHAR2 DEFAULT NULL
   );

   PROCEDURE ADD_TOPO_FROM_SPATIAL (
      p_toponame           IN VARCHAR2,
      p_featuretable       IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_topo_type          IN VARCHAR2,
      p_log_type           IN VARCHAR2,
      p_geom_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_val         IN VARCHAR2 DEFAULT NULL,
      p_allow_splits       IN VARCHAR2 DEFAULT 'Y',
      p_new_layer          IN VARCHAR2 DEFAULT 'N',
      p_topomap_mbr        IN SDO_GEOMETRY DEFAULT NULL
   );
 
   

   FUNCTION MAKE_ARROWHEAD(
      p_geom            IN SDO_GEOMETRY,
      p_scalefactor     IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY;

   PROCEDURE CLOSED_LOOPS_HELPER (
      p_schema          IN VARCHAR2,
      p_table_name      IN VARCHAR2,
      p_log_table       IN VARCHAR2,
      p_state_code      IN VARCHAR2 DEFAULT NULL,
      p_srid            IN NUMBER DEFAULT 4269,
      p_tolerance       IN NUMBER DEFAULT .05,
      p_tidy_topo       IN VARCHAR2 DEFAULT 'N'
   );

   FUNCTION CLOSED_LOOPS (
      p_tablename       IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_edge_id         IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2;


   FUNCTION COUNT_OBSOLETE_NODES(
      p_topology       IN VARCHAR2
   ) RETURN NUMBER;

   FUNCTION GET_OBSOLETE_NODES(
      p_topology       IN VARCHAR2
   ) RETURN GZ_TYPES.numberarray;

   PROCEDURE UPDATE_FACE_MEASUREMENTS(
      p_table      IN VARCHAR2,
      p_join_key   IN VARCHAR2 DEFAULT 'FACE_ID',
      p_list       IN VARCHAR2 DEFAULT NULL
   );

    FUNCTION SHOW_ME_THE_VERTICES (
      p_geom            IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   FUNCTION SHOW_ME_THE_FACE_DUPES (
      p_geom            IN SDO_GEOMETRY,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY;

   FUNCTION SHOW_ME_EDGE_INTERSECTION (
      p_geom            IN SDO_GEOMETRY,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY;

   FUNCTION DROP_UPDATABLE_TOPOMAPS (
      p_current_topomap    IN VARCHAR2,
      p_which              IN VARCHAR2 DEFAULT 'UPDATABLE'
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION EZ_TOPOMAP_MANAGER (
      p_topomap_name       IN VARCHAR2,
      p_topology_name      IN VARCHAR2,
      p_create_it          IN NUMBER,
      p_xLL                IN NUMBER DEFAULT NULL,
      p_yLL                IN NUMBER DEFAULT NULL,
      p_xUR                IN NUMBER DEFAULT NULL,
      p_yUR                IN NUMBER DEFAULT NULL,
      p_delta              IN NUMBER DEFAULT 0.0001,
      p_memory_size        IN NUMBER DEFAULT NULL
   ) RETURN NUMBER;

  PROCEDURE remove_obs_nodes_one_state (
    topo_name VARCHAR2,
    state_table VARCHAR2,
    v_state VARCHAR2
  );

  FUNCTION GZ_REMOVE_OBSOLETE_NODES (
     p_topo          IN VARCHAR2
  ) RETURN NUMBER;


  PROCEDURE remove_isolated_nodes(
    p_topology VARCHAR2
  );

   FUNCTION REMOVE_HOLES (
      p_sql                IN VARCHAR2,
      p_area               IN NUMBER,
      p_tolerance          IN NUMBER
   ) RETURN sdo_geometry;


   FUNCTION REMOVE_HOLES (
      geom_in              IN SDO_GEOMETRY,
      p_id                 IN VARCHAR2,
      p_area               IN NUMBER,
      p_tolerance          IN NUMBER,
      p_log                IN VARCHAR2 DEFAULT 'N',
      p_logtable_name      IN VARCHAR2 DEFAULT NULL,
      p_validate_out       IN VARCHAR2 DEFAULT 'N'
   ) RETURN sdo_geometry;

    FUNCTION GET_XYS(
      Geom              IN MDSYS.SDO_GEOMETRY,
      ring1_to_find     PLS_INTEGER,
      edge1_to_find     PLS_INTEGER,
      ring2_to_find     PLS_INTEGER DEFAULT NULL,
      edge2_to_find     PLS_INTEGER DEFAULT NULL
   ) RETURN sdo_geometry;



   PROCEDURE DEREGISTER_FEATURE_TABLES (
      p_schema             IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_drop_tabs          IN VARCHAR2 DEFAULT 'Y',
      p_min_tg_layer_level IN NUMBER DEFAULT 0,
      p_likeclause         IN VARCHAR2 DEFAULT NULL
   );

   PROCEDURE REFRESH_REMOTE_PROD_SCHEMA (
      p_prod_schema        IN VARCHAR2,
      p_prod_db            IN VARCHAR2,
      p_prod_pwd           IN VARCHAR2,
      p_likeclause         IN VARCHAR2 DEFAULT 'REF%'
   );

   FUNCTION GET_REF_SCHEMAS (
      p_schema             IN VARCHAR2,
      p_ref_schema_tab     IN VARCHAR2 DEFAULT 'REFERENCE_SCHEMAS'
   ) RETURN GZ_TYPES.stringhash;

   FUNCTION GET_REFERENCE_FACE_FIELDS (
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_type           IN VARCHAR2 DEFAULT 'ATTRIBUTE', --or MEASUREMENT
      p_ref_table      IN VARCHAR2 DEFAULT 'REFERENCE_FACE_FIELDS'
   ) RETURN GZ_TYPES.stringarray;

   PROCEDURE GZ_PRIV_GRANTER (
      p_ref_schema_table   IN VARCHAR2 DEFAULT 'REFERENCE_SCHEMAS',
      p_like_clause        IN VARCHAR2 DEFAULT NULL
   );

   PROCEDURE CREATE_TABLE (
      p_table_type        IN VARCHAR2,
      p_table_name        IN VARCHAR2,
      p_global_temp       IN VARCHAR2 DEFAULT 'N'
   );

   FUNCTION GZ_TABLE_EXISTS (
      p_table_name        IN VARCHAR2,p_empty_exists BOOLEAN default FALSE
   ) RETURN BOOLEAN;

   PROCEDURE ALIGN_EDGES (
      geom_table_name         VARCHAR2,
      geom_column_name        VARCHAR2,
      output_table_name       VARCHAR2,
      tolerance               NUMBER
   );


   PROCEDURE GZ_DUMP_SDO_EDGE_ARRAY (
      p_input         IN MDSYS.SDO_EDGE_ARRAY
   );

   FUNCTION REMOVE_CLOSE_ORDINATES (
      p_geom          IN SDO_GEOMETRY,
      p_distance      IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   PROCEDURE GZ_CHANGE_EDGE_COORDS (
      p_topo          IN VARCHAR2,
      p_edge_id       IN NUMBER,
      p_edge_geom     IN SDO_GEOMETRY,
      p_clean_edge    IN NUMBER DEFAULT NULL,
      p_debug         IN NUMBER DEFAULT NULL
   );

   PROCEDURE GZ_MOVE_NODE (
      p_topo          IN VARCHAR2,
      p_node_id       IN NUMBER,
      p_move_point    IN SDO_GEOMETRY,
      p_debug         IN NUMBER DEFAULT NULL,
      p_topology      IN VARCHAR2 DEFAULT NULL
   );

   FUNCTION GZ_LOCATE_PT_DISTANCE (
      p_edge_geom       IN SDO_GEOMETRY,
      p_distance        IN NUMBER,
      p_tip             IN VARCHAR2,
      p_tolerance       IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;


   FUNCTION GZ_GET_FSL_FACES (
      p_fsl_tab       IN VARCHAR2,
      p_fsl_oid       IN NUMBER,
      p_fsl_oid_col   IN VARCHAR2 DEFAULT 'OID',
      p_topo_col      IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION GZ_GET_FSL_BDYEDGES (
      p_fsl_tab       IN VARCHAR2,
      p_fsl_oid       IN NUMBER,
      p_fsl_oid_col   IN VARCHAR2 DEFAULT 'OID',
      p_topo_col      IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.stringarray;
   
   FUNCTION GZ_COUNT_FSL_OVERLAPS (
      p_topology        IN VARCHAR2,
      p_fsl_tab         IN VARCHAR2,
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN NUMBER;

   FUNCTION GZ_COUNT_TOPO_OVERLAPS (
      p_topology        IN VARCHAR2,
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN NUMBER;   

   PROCEDURE GZ_POPULATE_MEASUREMENTS (
      p_face_tab      IN VARCHAR2,
      p_pkc_col       IN VARCHAR2,
      p_column        IN VARCHAR2 DEFAULT 'ALL',
      p_records       IN VARCHAR2 DEFAULT 'ALL', --or NULL, meaning sdo null
      p_tolerance     IN NUMBER DEFAULT .05,
      p_to_srid       IN NUMBER DEFAULT NULL,
      p_subset_col    IN VARCHAR2 DEFAULT NULL,
      p_subset_val    IN VARCHAR2 DEFAULT NULL
   );

   PROCEDURE POPULATE_EDIT_MEASUREMENTS (
      p_topo          IN VARCHAR2,
      p_face_tab      IN VARCHAR2,
      p_face_id       IN NUMBER,
      p_validate_sdo  IN VARCHAR2 DEFAULT 'Y',
      p_validate_topo IN VARCHAR2 DEFAULT 'N',
      p_tolerance     IN NUMBER DEFAULT .05,
      p_pkc_col       IN VARCHAR2 DEFAULT 'FACE_ID',
      p_column        IN VARCHAR2 DEFAULT 'ALL',
      p_to_srid       IN NUMBER DEFAULT NULL,
      p_debug         IN NUMBER DEFAULT NULL
   );

   FUNCTION WHERE_GEOM (
      g        IN SDO_GEOMETRY
   ) RETURN VARCHAR2 deterministic;

   FUNCTION ID_GEOM (
      g        IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY deterministic;

   PROCEDURE IMPORT_SDOTOPO11G (
      p_topo            IN VARCHAR2,
      p_feature_tabs    IN VARCHAR2 DEFAULT NULL
   );

   FUNCTION DUMP_STRING_ENDPOINTS (
      geom1        IN SDO_GEOMETRY,
      geom2        IN SDO_GEOMETRY DEFAULT NULL
   ) RETURN VARCHAR2;

   FUNCTION DUMP_SDO_SUBELEMENTS (
      geom         IN SDO_GEOMETRY,
      indent       IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB;

   FUNCTION DUMP_SDO (
      geom         IN SDO_GEOMETRY,
      indent       IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB;

   FUNCTION DUMP_SDO_POINT (
      geom         IN SDO_POINT_TYPE,
      indent       IN VARCHAR2 DEFAULT ''
   ) RETURN VARCHAR2;

   FUNCTION DUMP_SDO_ELEM (
      geom         IN SDO_ELEM_INFO_ARRAY,
      indent       IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB;

   FUNCTION DUMP_SDO_ORDS (
      geom         IN SDO_ORDINATE_ARRAY,
      indent       IN VARCHAR2 DEFAULT ''
   ) RETURN CLOB;

   TYPE DUMPREC IS RECORD (
      dumptext       VARCHAR2(4000)
   );

   TYPE DUMPTAB IS TABLE OF DUMPREC;

   FUNCTION DUMP_TEST_CASE (
      p_topo            IN VARCHAR2,
      p_feature_tab     IN VARCHAR2,
      p_edge_sql        IN VARCHAR2
   ) RETURN DUMPTAB PIPELINED;

   FUNCTION DUMP_TEST_CASE_PRC (
      p_topo            IN VARCHAR2,
      p_proc_name       IN VARCHAR2,
      p_type            IN VARCHAR2 DEFAULT 'LINE',
      p_geom_sql        IN VARCHAR2
   ) RETURN DUMPTAB PIPELINED;

   -- Used in Helper tool FACE_DIFFS (Stephanie)
   TYPE TY_ATTR_DIFF_REC IS RECORD (
      ATTR     VARCHAR2(100),
      FACE1    VARCHAR2(100),
      FACE2    VARCHAR2(100),
      STATUS   VARCHAR2(100));

   TYPE TY_ATTR_DIFF_TAB IS TABLE OF TY_ATTR_DIFF_REC;

   FUNCTION FACE_DIFFS (
      p_face1                     IN number,
      p_face2                     IN number,
      p_face_table_name           IN varchar2,
      p_reference_face_fields_tab IN varchar2,
      p_release_name              IN VARCHAR2 DEFAULT NULL,
      p_gen_project_id            IN VARCHAR2 DEFAULT NULL
   ) RETURN TY_ATTR_DIFF_TAB PIPELINED;

   FUNCTION GET_TG_LAYER_ID (
      p_topology                    IN VARCHAR2,
      p_table_name                  IN VARCHAR2,
      p_column_name                 IN VARCHAR2,
      p_tg_layer_type               IN VARCHAR2
   ) RETURN NUMBER;
   
   FUNCTION GET_TG_LAYER_LEVEL (
      p_topology                    IN VARCHAR2,
      p_table_name                  IN VARCHAR2,
      p_column_name                 IN VARCHAR2,
      p_tg_layer_type               IN VARCHAR2
   ) RETURN NUMBER;


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

   FUNCTION ORDINATE_ROUNDER (
      p_geometry                IN SDO_GEOMETRY,
      p_places                  IN PLS_INTEGER DEFAULT 6
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

    FUNCTION VALIDATE_TOPOLOGY_TILE (
      p_topo               IN VARCHAR2,
      p_tiles              IN GZ_TYPES.geomarray,
      p_delta              IN NUMBER DEFAULT 0.0001,
      p_edge_count         IN NUMBER DEFAULT 100000,
      p_memory_size        IN NUMBER DEFAULT NULL,
      p_log_type           IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2;

   FUNCTION VALIDATE_TOPOLOGY (
      p_topo               IN VARCHAR2,
      p_edge_count         IN NUMBER DEFAULT NULL,
      p_memory_size        IN NUMBER DEFAULT NULL,
      p_xmin               IN NUMBER DEFAULT NULL,
      p_ymin               IN NUMBER DEFAULT NULL,
      p_xmax               IN NUMBER DEFAULT NULL,
      p_ymax               IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2;

    FUNCTION GZ_VALIDATE_TOPOLOGY (
      p_topo            IN VARCHAR2,
      p_outline_table   IN VARCHAR2,
      p_column          IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_delta           IN NUMBER DEFAULT 0.0001,
      p_log_type        IN VARCHAR2 DEFAULT NULL,
      p_tile_target     IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2;

   FUNCTION QUERY_DELIMITED_LIST (
      p_input             IN GZ_TYPES.stringarray,
      p_query             IN VARCHAR2
   ) RETURN NUMBER DETERMINISTIC;

   FUNCTION QUERY_DELIMITED_LIST (
      p_input             IN GZ_TYPES.stringarray,
      p_query             IN NUMBER
   ) RETURN NUMBER DETERMINISTIC;

   PROCEDURE JAVA_MEMORY_MANAGER (
      p_table_name        IN VARCHAR2,
      p_sdo_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_error_msg         IN VARCHAR2 DEFAULT NULL,
      p_filter            IN SDO_GEOMETRY DEFAULT NULL
   );

   PROCEDURE CREATE_REFERENCE_SCHEMAS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'REFERENCE_SCHEMAS'
   );

   PROCEDURE CREATE_REFERENCE_FACE_FIELDS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'REFERENCE_FACE_FIELDS'
   );

   PROCEDURE CREATE_STATE_EDGES (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE CREATE_GEN_CLIP_PARAMETERS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_CLIP_PARAMETERS'
   );

   PROCEDURE CREATE_GEN_MERGE_PARAMETERS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_MERGE_PARAMETERS'
   );

   PROCEDURE CREATE_LUT_LSAD (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'LUT_LSAD'
   );

   --Alternate topo build module--

   PROCEDURE CREATE_GZ_LAYERS_IN (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_IN'
   );

   PROCEDURE CREATE_GZ_LAYER_INFO (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   );

    PROCEDURE CREATE_GZ_BUILD_POLY (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE CREATE_GZ_BUILD_EDGE (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   );

   --OUTPUT module--

   PROCEDURE CREATE_GZ_LAYERS_OUT (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_OUT'
   );

   PROCEDURE CREATE_GZ_LAYERS_AGGREGATE (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_AGGREGATE'
   );

   PROCEDURE CREATE_GZ_LAYERS_SPLIT (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_SPLIT'
   );

   PROCEDURE CREATE_GZ_LAYERS_HIERARCHICAL (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_HIERARCHICAL'
   );

   PROCEDURE CREATE_GZ_LAYERS_SUBSET (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_SUBSET'
   );

   PROCEDURE CREATE_GZ_LAYERS_FIELDS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_FIELDS'
   );

   PROCEDURE CREATE_GZ_LAYERS_CROSSWALK (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_CROSSWALK'
   );

   PROCEDURE CREATE_GZ_LAYERS_GEOID (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GZ_LAYERS_GEOID'
   );

   PROCEDURE CREATE_GZ_LAYERS_OUT_INFO (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE CREATE_GZ_LAYERS_OUT_GEOM (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2,
      p_srid           IN NUMBER
   );
   
   PROCEDURE CREATE_GZ_LAYERS_OUT_HELP (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE CREATE_GZ_FACE_MERGE (
      p_table_name     IN VARCHAR2
   );

   --Project management tools

   PROCEDURE GZ_RELEASE_COPIER (
      p_src_schema     IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_new_release    IN VARCHAR2 DEFAULT NULL
   );

   PROCEDURE GZ_RELEASE_DELETER (
      p_release        IN VARCHAR2
   );

   PROCEDURE GZ_LAYER_COPIER (
      p_src_schema         IN VARCHAR2,
      p_src_release        IN VARCHAR2,
      p_src_project_id     IN VARCHAR2,
      p_src_layer          IN VARCHAR2,
      p_dest_release       IN VARCHAR2,
      p_dest_project_id    IN VARCHAR2,
      p_dest_layer         IN VARCHAR2,
      p_src_x_out          IN VARCHAR2 DEFAULT NULL,
      p_clean_house        IN VARCHAR2 DEFAULT 'N'
   );

   --generic build script helper

   PROCEDURE COPY_TO_X (
      p_tablename     IN VARCHAR2,
      p_xxx           IN VARCHAR2 DEFAULT NULL
   );

    PROCEDURE CREATE_GZ_BUILD_TILE (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE CREATE_GZ_BUILD_GEOM (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE BACKUP_GZ_TABLES (
      p_schema          IN VARCHAR2 DEFAULT NULL
   );

   PROCEDURE CREATE_GZ_TABLES (
      p_drop            IN VARCHAR2 DEFAULT 'Y',
      p_new_schema      IN VARCHAR2 DEFAULT 'N'
   );

   PROCEDURE REBUILD_GZ_TABLES;

   PROCEDURE COPY_GZ_TABLES (
      p_srcschema       IN VARCHAR2,
      p_myschema        IN VARCHAR2 DEFAULT NULL
   );

   ------------------------------------------------------------------------------------
   --++LRS LRS  LRS  LRS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GZ_FIND_MEASURE (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY
   ) RETURN NUMBER DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GZ_LOCATE_PT (
      p_edge           IN SDO_GEOMETRY,
      p_measure        IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GZ_PROJECT_PT (
      p_point          IN SDO_GEOMETRY,
      p_edge           IN SDO_GEOMETRY,
      p_tolerance      IN NUMBER DEFAULT 0.00000001
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION NEW_SMALL_POLY_RSUV
   RETURN GZ_TYPES.SMALL_POLY_RSUV PIPELINED;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE CREATE_SMALL_POLY_RSUV (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION PIPE_EDGE_VERTICES (
      p_tab_name           IN VARCHAR2,
      p_sdo_col            IN VARCHAR2 DEFAULT 'SDOGEOMETRY'
   ) RETURN GZ_TYPES.SMALL_POLY_RSUV PIPELINED;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION AXE_UNMATCHED_VTXS (
      p_topo               IN VARCHAR2,
      p_match_tab          IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN NUMBER;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION REMOVE_SP_UNIVERSAL_VERTICES (
      p_topo            IN VARCHAR2,
      p_clip_tab        IN VARCHAR2,
      p_drop_work_tab   IN VARCHAR2 DEFAULT 'N',
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN NUMBER;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE UPDATE_SP_THINNED_FACES (
      p_topo            IN VARCHAR2,
      p_face_tab        IN VARCHAR2,
      p_tolerance       IN NUMBER DEFAULT .05
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION SP_VERTEX_THINNER (
      p_topo            IN VARCHAR2,
      p_clip_tab        IN VARCHAR2,
      p_face_tab        IN VARCHAR2 DEFAULT NULL,
      p_validate_sdo    IN VARCHAR2 DEFAULT 'N',
      p_validate_topo   IN VARCHAR2 DEFAULT 'N',
      p_tolerance       IN NUMBER DEFAULT .05,
      p_drop_work_tab   IN VARCHAR2 DEFAULT 'N'
   ) RETURN NUMBER;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE GZ_ADD_TOPO_GEOMETRY_LAYER (
      p_topo            IN VARCHAR2,
      p_table_name      IN VARCHAR2,
      p_column_name     IN VARCHAR2 DEFAULT 'TOPOGEOM',
      p_layer_type      IN VARCHAR2 DEFAULT 'POLYGON',
      p_child_tab       IN VARCHAR2 DEFAULT NULL
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE GZ_COPY_ATTR_FROM_FACE_TO_FACE (
      p_release            IN VARCHAR2,
      p_projectid         IN VARCHAR2,
      p_face_table_name     IN VARCHAR2,
      p_src_face            IN VARCHAR2,
      p_destination_face    IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE GZ_COPY_FACE_ATTR_FROM_TABLE (
      p_projectid                      IN VARCHAR2,
      p_faceid                        IN VARCHAR2,
      p_face_src_table_name           IN VARCHAR2,
      p_face_destination_table_name   IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION REMOVE_DUPE_VERTEX (
      XYs                     IN OUT NOCOPY MDSYS.SDO_ORDINATE_ARRAY
   ) RETURN PLS_INTEGER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE REMOVE_EXACT_TOPO_DUPES (
      p_topo                  IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE REBUILD_TOPO_DOMAIN_INDEXES (
      p_topo                  IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE REBUILD_REL_DOLLAR_INDEXES (
      p_topo                  IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE GZ_TOPO_TUNE_UP (
      p_topo                  IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
      FUNCTION VALIDATE_LINES_WITH_CONTEXT (
      p_line1                  IN SDO_GEOMETRY,
      p_line2                  IN SDO_GEOMETRY,
      p_tolerance             IN NUMBER DEFAULT .05
   ) RETURN VARCHAR2;

   FUNCTION VALIDATE_LINES_WITH_CONTEXT (
      p_line                  IN SDO_GEOMETRY,
      p_tolerance             IN NUMBER DEFAULT .05
   ) RETURN VARCHAR2;

    FUNCTION Concatenate_lines_Sans(pline1 IN MDSYS.SDO_GEOMETRY,
    pline2 IN MDSYS.SDO_GEOMETRY,
    p_tolerance NUMBER DEFAULT 0.05) RETURN MDSYS.SDO_GEOMETRY;



   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE UPDATE_HARD_CODED_TABLES (
      p_table_list            IN VARCHAR2,
      p_type                  IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE PRIV_GRANTS_ON_LIST (
      p_table_list            IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE GZ_DROP_TABLE (
      p_table                  IN VARCHAR2
   );

   PROCEDURE DROP_MODULE_WORK_TABLES (
      p_topo                  IN VARCHAR2,
      p_module                IN VARCHAR2,
      p_drop_tracking         IN VARCHAR2 DEFAULT 'N'
   );

   PROCEDURE DROP_MODULE_SEQUENCES (
      p_topo                  IN VARCHAR2,
      p_module                IN VARCHAR2
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE CREATE_VERTEX_TABLE (
      pGeom                   IN SDO_GEOMETRY,
      pOutputTable            IN VARCHAR2,
      pDropTable              IN VARCHAR2 DEFAULT 'N'
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE CPB_SLEEP (
      p_time_seconds    IN NUMBER
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION SUBDIVIDE_TILE (
      p_geom               IN SDO_GEOMETRY,
      p_subdivisions       IN NUMBER
   ) RETURN GZ_TYPES.geomarray;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GZ_TILE_TABLE (
      p_table           IN VARCHAR2,                     --can be schema.table
      p_tile_target     IN NUMBER,
      p_geom_col        IN VARCHAR2 DEFAULT 'GEOMETRY',
      p_whereclause     IN VARCHAR2 DEFAULT NULL,        --use a.xx syntax
      p_sdo_filter      IN SDO_GEOMETRY DEFAULT NULL,
      p_log_type        IN VARCHAR2 DEFAULT NULL,        --BUILD for ex
      p_log_tag         IN VARCHAR2 DEFAULT NULL,        --ex topology
      p_debug           IN NUMBER DEFAULT NULL
   ) RETURN GZ_TYPES.geomarray;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION EXPAND_MBR_PERCENT (
      p_mbr             IN SDO_GEOMETRY,
      p_percent         IN NUMBER DEFAULT 1
   ) RETURN SDO_GEOMETRY;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   PROCEDURE DBMS_SQL_HELPER (
      p_sql             IN CLOB
   );
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

END GZ_UTILITIES;
/
