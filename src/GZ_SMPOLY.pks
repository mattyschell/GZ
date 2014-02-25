CREATE OR REPLACE PACKAGE GZ_SMPOLY
AUTHID CURRENT_USER
AS
--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------
-- types specific to small interactive poly work, others should go in CPB/GZ_TYPES
TYPE my_comparison_rec IS RECORD (
 compare VARCHAR2(4000),
 value1 VARCHAR2(4000),
 value2 VARCHAR2(4000),
 status VARCHAR2(4000));
TYPE my_comparison_table IS TABLE OF my_comparison_rec;
--------------------------------------------
--- Create empty work tables --
---------------------------------------------
-- default name needs to change
PROCEDURE CREATE_GEN_SP_PARAMETERS(
      p_schema          VARCHAR2,
      p_table_name      VARCHAR2 DEFAULT 'SMALL_POLYGON_PARAMETERS'
);
FUNCTION NEW_SMALL_POLYGON_PARAMETERS
RETURN GZ_TYPES.SMALL_POLYGON_PARAMETERS PIPELINED;
FUNCTION NEW_DELETE_EDGES_INPUT
RETURN GZ_TYPES.DELETE_EDGES_INPUT PIPELINED;
FUNCTION NEW_GEN_SP_TRACKING
RETURN GZ_TYPES.GEN_SP_TRACKING PIPELINED;
FUNCTION NEW_EDGE_ATTRIBUTE
RETURN GZ_TYPES.EDGE_ATTRIBUTE PIPELINED;
FUNCTION START_SP_LOGGING (
 pSchema IN VARCHAR2,
 pJobRun IN VARCHAR2
)RETURN VARCHAR2;
--------------------------------------------------------------------------------
-- OTHER FUNCTIONS
--------------------------------------------------------------------------------
FUNCTION compare_faces (
 p_face1 NUMBER,
 p_face2 NUMBER,
 p_face_table VARCHAR2,
 p_columns_tab VARCHAR2 default 'GEOID_COMPONENTS',
 p_comp_column VARCHAR2 default 'FACE_ID'
 ) RETURN my_comparison_table PIPELINED;
--------------------------------------------------------------------------------
FUNCTION are_my_polys_valid (
   pTopology VARCHAR2,
   pFeatureTab VARCHAR2,
   pPrimaryKey VARCHAR2,
   pTolerance number DEFAULT 0.05,
   pFixValErr CHAR DEFAULT 'N'
 ) RETURN BOOLEAN;
--------------------------------------------------------------------------------
FUNCTION are_my_poly_features_valid (
 pFeatureTab VARCHAR2,
 pPrimaryKey VARCHAR2
 ) RETURN BOOLEAN;
--------------------------------------------------------------------------------
FUNCTION ARE_MY_SDO_GEOMETRIES_VALID (
 pTable varchar2,
 pPKColumn varchar2 DEFAULT 'GEO_ID',
 pGeomColumn varchar2 DEFAULT 'SDOGEOMETRY',
 pTolerance number DEFAULT 0.05
) Return BOOLEAN;
--------------------------------------------------------------------------------
--- PROCEDURES for creating tables
--------------------------------------------------------------------------------
PROCEDURE CREATE_DELETE_EDGES_INPUT (
 p_schema IN VARCHAR2,
 p_table_name IN VARCHAR2 DEFAULT 'DELETE_EDGES_INPUT'
);
PROCEDURE CREATE_GEN_SP_TRACKING (
 p_schema IN VARCHAR2,
 p_table_name IN VARCHAR2 DEFAULT 'GEN_SP_TRACKING'
);
PROCEDURE CREATE_EDGE_ATTRIBUTE (
 p_schema IN VARCHAR2,
 p_table_name IN VARCHAR2 DEFAULT 'GEN_EDGE_ATT'
);
--------------------------------------------------------------------------------
-- OTHER PROCEDURES
--------------------------------------------------------------------------------
PROCEDURE bigu_review (
 p_topology VARCHAR2,
 p_bigu_tab VARCHAR2,
 p_face_feature_tab VARCHAR2,
 p_sm_poly_edges_tab VARCHAR2,
 p_sumlev_fields_tab VARCHAR2,
 p_output_table VARCHAR2 default 'UNIQUE_SMALL_POLYS'
);
PROCEDURE LOGNOTE (
 pLogNote VARCHAR2
);
PROCEDURE GENERALIZATION_SP_REMOVAL (
 pSchema             IN VARCHAR2,
 pProjectId          IN VARCHAR2,
 pJobId              IN VARCHAR2,
 pTopo               IN VARCHAR2,
 pModules            IN VARCHAR2,
 pFromTopo           IN VARCHAR2,
 pTopoBk             IN VARCHAR2,
 pStateOutlineTable  IN VARCHAR2,
 pStateFP            IN VARCHAR2,
 pGeoIDColumn        IN VARCHAR2 default 'GEOID',
 pRunId              IN VARCHAR2 default '1',
 pCleanup            IN VARCHAR2 default 'N',
 p_validate_topo     IN VARCHAR2 DEFAULT 'Y',
 p_fix_edge          IN VARCHAR2 DEFAULT 'Y',
 p_fix_2edge         IN VARCHAR2 DEFAULT 'N',
 p_topofix_qa        IN VARCHAR2 DEFAULT 'N'    
);
PROCEDURE LOAD_EDGE_SIMPLE (
 pSchema VARCHAR2,
 pTopology VARCHAR2
);
PROCEDURE UPDATE_MEASURES_WHERE_NULL (
 pTable        IN VARCHAR2,
 pPrimaryKey   IN VARCHAR2,
 pparallel     IN NUMBER DEFAULT 4
);
PROCEDURE drop_dangles(
 pTopo VARCHAR2,
 pValFlag VARCHAR2 default 'TRUE',
 pDEBUG VARCHAR2 default 'FALSE'
);
PROCEDURE ELIMINATE_SMALL_POLYS(
 pInSchema VARCHAR2,
 pInMT_EDGE$ VARCHAR2,
 pInTable VARCHAR2,
 pEdgeTable VARCHAR2,
 pInGeoIDColumn VARCHAR2,
 pOutTable VARCHAR2,
 InScale NUMBER,
 Area_Cutoff NUMBER,
 InAreaTolerance NUMBER,
 pRunId VARCHAR2 default 'X',
 pCleanup VARCHAR2 default 'N');

   FUNCTION GET_SMPOLY_PROJECT_PARAMETERS (
      p_project_id         IN VARCHAR2,
      p_release             IN VARCHAR2
   ) RETURN GZ_TYPES.SMALL_POLYGON_PARAMETERS_REC;

   -----------------------------------------------------------------------------
   -- | Matt! Face Merge Code below here  |
   -- |                                   |
   ---V-----------------------------------V-------------------------------------

   FUNCTION STRINGHASH_ADD (
      p_input_1   IN GZ_TYPES.stringhash,
      p_input_2   IN GZ_TYPES.stringhash
   ) RETURN GZ_TYPES.stringhash DETERMINISTIC;

   FUNCTION STRINGHASH_SUBTRACT (
      p_input_1   IN GZ_TYPES.stringhash,
      p_input_2   IN GZ_TYPES.stringhash
   ) RETURN GZ_TYPES.stringhash DETERMINISTIC;

   FUNCTION IS_STRING_IN_ARRAY (
      p_string          IN VARCHAR2,
      p_array           IN GZ_TYPES.stringarray,
      p_like_flag       IN VARCHAR2 DEFAULT NULL  --put %s in input p_string
   ) RETURN BOOLEAN;

   FUNCTION NUMBERARRAY_ADD_UNIQUE (
     p_input_1   IN GZ_TYPES.numberarray,
     p_input_2   IN GZ_TYPES.numberarray
   ) RETURN GZ_TYPES.numberarray DETERMINISTIC;

   FUNCTION DUBNUMBERARRAY_ORDER (
     p_key           IN NUMBER,
     p_val           IN NUMBER,
     p_inputarray    IN GZ_TYPES.doublenumarray,
     p_order         IN VARCHAR2 DEFAULT 'ASC'
   ) RETURN GZ_TYPES.doublenumarray DETERMINISTIC;

   FUNCTION MANAGE_FSL_LIST (
      p_list            IN GZ_TYPES.stringhash,
      p_fsl_table       IN VARCHAR2,
      p_key_val         IN VARCHAR2,
      p_delimiter       IN VARCHAR2 DEFAULT ';'
   ) RETURN GZ_TYPES.stringhash;

   FUNCTION SUBTRACT_FSL_LIST (
      p_list            IN GZ_TYPES.stringhash,
      p_fsl_table       IN VARCHAR2,
      p_key_val         IN VARCHAR2,
      p_delimiter       IN VARCHAR2 DEFAULT ';'
   ) RETURN GZ_TYPES.stringhash;

   FUNCTION SUBTRACT_HASH_UNIQUE (
      p_list            IN GZ_TYPES.stringhash,
      p_subtract_list   IN GZ_TYPES.stringhash,
      p_delimiter       IN VARCHAR2 DEFAULT ';'
   ) RETURN GZ_TYPES.stringhash;

   FUNCTION GET_EDGE_MBR (
      p_topo            IN VARCHAR2,
      p_edge            IN NUMBER
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION GET_EDGE_NODE (
      p_topo            IN VARCHAR2,
      p_edge            IN NUMBER,
      p_tip             IN VARCHAR2 DEFAULT 'START'
   ) RETURN NUMBER;

   FUNCTION GET_FEATURE_FACE_GEOMETRY (
      p_face_table      IN VARCHAR2,
      p_face_id         IN NUMBER
   ) RETURN SDO_GEOMETRY;

   FUNCTION GET_NODE_GEOMETRY (
      p_topo            IN VARCHAR2,
      p_node_id         IN VARCHAR2
   ) RETURN SDO_GEOMETRY;

   FUNCTION GET_EDGES_BETWEEN_FACES (
      p_topology           IN VARCHAR2,
      p_face_id_1          IN NUMBER,
      p_face_id_2          IN NUMBER
   ) RETURN GZ_TYPES.numberarray;

   FUNCTION GET_EDGE_BETWEEN_FACES (
      p_topology           IN VARCHAR2,
      p_face_id_1          IN NUMBER,
      p_face_id_2          IN NUMBER
   ) RETURN NUMBER;

   FUNCTION GET_EDGES_BETWEEN_FACES_HINTED (
      p_topology           IN VARCHAR2,
      p_face_id_1          IN NUMBER,
      p_face_id_2          IN NUMBER,
      p_edge_hint          IN NUMBER,
      p_edge_hint_2        IN NUMBER DEFAULT NULL
   ) RETURN GZ_TYPES.numberarray;

   FUNCTION GET_EDGE_BETWEEN_FACES_HINTED (
      p_topology           IN VARCHAR2,
      p_face_id_1          IN NUMBER,
      p_face_id_2          IN NUMBER,
      p_edge_hint_1        IN NUMBER,
      p_edge_hint_2        IN NUMBER
   ) RETURN NUMBER;

   FUNCTION GET_OPPOSITE_FACE (
      p_topology           IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_edge_id            IN NUMBER
   ) RETURN NUMBER;

   FUNCTION GET_SINGLE_OPPOSITE_FACE (
      p_topology           IN VARCHAR2,
      p_face_id            IN NUMBER
   ) RETURN NUMBER;

   FUNCTION GET_NODES_FOR_EDGE (
      p_topo               IN VARCHAR2,
      p_edge_id            IN NUMBER
   ) RETURN GZ_TYPES.numberarray;

   FUNCTION GET_NODE_BETWEEN_EDGES (
      p_topology           IN VARCHAR2,
      p_edge_id_1          IN NUMBER,
      p_edge_id_2          IN NUMBER
   ) RETURN NUMBER;

   FUNCTION GET_NODE_TIP (
      p_topology           IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_node_id            IN NUMBER
   ) RETURN VARCHAR2;

   FUNCTION IS_EDGE_BOUNDED_BY_OBSOLETE (
      p_topology        IN VARCHAR2,
      p_edge_id         IN NUMBER
   ) RETURN BOOLEAN;

   FUNCTION IS_OBSOLETE_NODE_BETWEEN_FACES (
      p_topology        IN VARCHAR2,
      p_face_id_1       IN NUMBER,
      p_face_id_2       IN NUMBER
   ) RETURN BOOLEAN;

   FUNCTION ARE_EDGES_CONNECTED (
      p_topology        IN VARCHAR2,
      p_edge_id_1       IN NUMBER,
      p_edge_id_2       IN NUMBER
   ) RETURN BOOLEAN;

   FUNCTION ARE_EDGES_BTWN_FACES_CONNECTED (
      p_topology        IN VARCHAR2,
      p_face_id_1       IN NUMBER,
      p_face_id_2       IN NUMBER
   ) RETURN BOOLEAN;

   FUNCTION ARE_THREE_FACES_AT_A_POINT (
      p_topology        IN VARCHAR2,
      p_face_id_1       IN NUMBER,
      p_face_id_2       IN NUMBER,
      p_face_id_3       IN NUMBER
   ) RETURN BOOLEAN;

   FUNCTION ARE_3_EDGES_A_SIMPLE_CHAIN (
      p_topology        IN VARCHAR2,
      p_edges           IN GZ_TYPES.NUMBERARRAY
   ) RETURN BOOLEAN;

   FUNCTION IS_AC_BC_AC (
      p_topology        IN VARCHAR2,
      p_face_id_target  IN NUMBER,
      p_face_id_2       IN NUMBER,
      p_face_id_3       IN NUMBER
   ) RETURN BOOLEAN;

   FUNCTION COUNT_EDGES_BETWEEN_FACES (
      p_topology        IN VARCHAR2,
      p_face_id_1       IN NUMBER,
      p_face_id_2       IN NUMBER
   ) RETURN PLS_INTEGER;

   FUNCTION GET_TG_LAYER_LEVEL (
      p_topology           IN VARCHAR2,
      p_table_name         IN VARCHAR2
   ) RETURN NUMBER;

   FUNCTION GET_FSLS (
      p_topology           IN VARCHAR2,
      p_greater_level      IN NUMBER DEFAULT NULL,
      p_allow_nuthin       IN VARCHAR2 DEFAULT 'N'
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION GET_FSLS_HIERARCHY (
      p_topology           IN VARCHAR2,
      p_table              IN VARCHAR2
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION GET_PARENT_TABLES (
      p_topology           IN VARCHAR2,
      p_child_table        IN VARCHAR2,
      p_paternity_test     IN VARCHAR2 DEFAULT 'N'
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION GET_PARENT_OF_CHILD (
      p_topology           IN VARCHAR2,
      p_child_table        IN VARCHAR2,
      p_child_key_col      IN VARCHAR2,
      p_child_key          IN VARCHAR2,
      p_parent_table       IN VARCHAR2,
      p_parent_key_col     IN VARCHAR2,
      p_parent_exists      IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2;

   FUNCTION COUNT_PARENT_TGL_OBJECTS (
      p_topology           IN VARCHAR2,
      p_parent_table       IN VARCHAR2,
      p_parent_key_col     IN VARCHAR2,
      p_parent_key         IN VARCHAR2
   ) RETURN PLS_INTEGER;

   FUNCTION GET_CHILD_TGL_OBJECT (
      p_child_table        IN VARCHAR2,
      p_child_key_col      IN VARCHAR2,
      p_child_key          IN VARCHAR2
   ) RETURN SDO_TGL_OBJECT;

   FUNCTION GZ_GET_GEOID_FROM_FACE (
      p_topology        IN VARCHAR2,
      p_fsl_tab         IN VARCHAR2,
      p_layer_level     IN NUMBER,
      p_face_id         IN NUMBER,
      p_fsl_key_col     IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION GZ_GET_FSL_FACES (
      p_topology        IN VARCHAR2,
      p_fsl_tab         IN VARCHAR2,
      p_fsl_geo_id      IN VARCHAR2,
      p_tg_layer_level  IN NUMBER,
      p_fsl_pkc_col     IN VARCHAR2 DEFAULT 'GEO_ID',
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.NUMBERARRAY;

   FUNCTION GET_EXPENDABLE_FSLS (
      p_release         IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_expendable_tab  IN VARCHAR2,
      p_what            IN VARCHAR2 --SLIVER_EXEMPT or SLIVER_EXTINCT_CLAUSE
   ) RETURN GZ_TYPES.stringhash;

   FUNCTION IS_GEOID_EXPENDABLE (
      p_release         IN VARCHAR2,
      p_project_id      IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_layer           IN VARCHAR2,
      p_pkc_col         IN VARCHAR2,
      p_geoid           IN VARCHAR2,
      p_where_clause    IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION FSLS_ENDANGERED (
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_topology           IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_fsl_pkc            IN VARCHAR2 DEFAULT 'GEO_ID',
      p_expendable_tab     IN VARCHAR2 DEFAULT NULL,
      p_only_expendable    IN VARCHAR2 DEFAULT 'N',
      p_debug              IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2;

   PROCEDURE START_MERGEFACE_LOGGING (
      p_topo           IN VARCHAR2,
      p_log_type       IN VARCHAR2, --MERGEFACE if standalone
      transaction_id   IN VARCHAR2
   );

   PROCEDURE DELETE_PARENT_TGL_OBJECT (
      p_topology           IN VARCHAR2,
      p_child_table        IN VARCHAR2,
      p_child_key          IN VARCHAR2,
      p_child_key_col      IN VARCHAR2,
      p_parent_key_col     IN VARCHAR2,
      p_must_delete        IN VARCHAR2 DEFAULT 'Y',
      p_depth              IN NUMBER DEFAULT 0
   );

   FUNCTION GET_FSLS_FOR_FACE (
      p_topology           IN VARCHAR2,
      p_face_id            IN VARCHAR2,
      p_fsl_pkc            IN VARCHAR2 DEFAULT 'GEO_ID'
   ) RETURN GZ_TYPES.stringhash;

   FUNCTION DELETE_A_FACE (
      p_topology           IN VARCHAR2,
      p_feature_table      IN VARCHAR2,
      p_key                IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_key_col            IN VARCHAR2,  --caller should pass in GEO_ID or FACE_ID usually
      p_parent_key_col     IN VARCHAR2 DEFAULT 'GEO_ID'
   ) RETURN NUMBER;

   PROCEDURE ADD_A_FACE (
      p_topology           IN VARCHAR2,
      p_feature_table      IN VARCHAR2,
      p_key                IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_key_col            IN VARCHAR2  --caller should pass in GEO_ID or FACE_ID usually
   );

   FUNCTION SUBTRACT_FACE_FROM_FSLS (
      p_topology           IN VARCHAR2,
      p_face_id            IN VARCHAR2,
      p_fsl_hash           IN GZ_TYPES.stringhash,
      p_fsl_pkc            IN VARCHAR2 DEFAULT 'GEO_ID'
   ) RETURN GZ_TYPES.stringhash;

   FUNCTION ADD_FACE_TO_FSLS (
      p_topology           IN VARCHAR2,
      p_face_id            IN VARCHAR2,
      p_fsl_hash           IN GZ_TYPES.stringhash,
      p_fsl_pkc            IN VARCHAR2 DEFAULT 'GEO_ID',
      p_face_table         IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION DETERMINE_CONFIG (
      p_topology           IN VARCHAR2,
      p_transaction_id     IN VARCHAR2,
      p_log_type           IN VARCHAR2,
      p_small_face_id      IN NUMBER,
      p_big_face_id_1      IN NUMBER,
      p_big_face_id_2      IN NUMBER DEFAULT NULL,
      p_big_face_id_3      IN NUMBER DEFAULT NULL,
      p_debug              IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2;

    PROCEDURE GZ_REMOVE_EDGE (
      p_topo           IN VARCHAR2,
      p_edge_id        IN VARCHAR2
   );

   FUNCTION REMOVE_SHARED_EDGE (
      p_topology        IN VARCHAR2,
      p_dead_edge       IN NUMBER
   ) RETURN GZ_TYPES.numberarray;

   FUNCTION REMOVE_NODE_KINDLY (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   ) RETURN NUMBER;

   PROCEDURE REMOVE_OBSOLETE_NODES_LIST (
      p_topology           IN VARCHAR2,
      p_nodes              IN GZ_TYPES.numberarray,
      p_option             IN VARCHAR2 DEFAULT 'ZAP'
   );

   PROCEDURE REMOVE_OBSOLETE_BETWEEN_FACES (
      p_topology        IN VARCHAR2,
      p_face_id_1       IN NUMBER,
      p_face_id_2       IN NUMBER
   );

   FUNCTION GET_FACE_TABLE (
      p_topology           IN VARCHAR2,
      p_not_like_clause    IN VARCHAR2  --%FSL% maybe will change some day
   ) RETURN VARCHAR2;

    FUNCTION ADD_NEW_EDGE (
      p_topo            IN VARCHAR2,
      p_old_node_id     IN NUMBER,
      p_new_node_id     IN NUMBER,
      p_extend_edge     IN NUMBER
   ) RETURN NUMBER;

   FUNCTION ADD_NEW_NODE (
      p_topo               IN VARCHAR2,
      p_edge               IN NUMBER,
      p_coord_index        IN NUMBER,
      p_node               IN SDO_GEOMETRY,
      p_is_new_shape_pt    IN VARCHAR2
   ) RETURN NUMBER;

   FUNCTION GET_EXISTING_NODE (
      p_topo            IN VARCHAR2,
      p_edge            IN VARCHAR2,
      p_coord_index     IN NUMBER
   ) RETURN NUMBER;

   FUNCTION ADD_AN_OPPOSITE_NODE (
      p_topology           IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_node_id            IN NUMBER,
      p_face_table         IN VARCHAR2,
      p_lateral_edge_1     IN NUMBER,
      p_lateral_edge_2     IN NUMBER,
      p_lateral_edge_3     IN NUMBER DEFAULT NULL,
      p_feature_face_id    IN NUMBER DEFAULT NULL,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_testing            IN NUMBER DEFAULT 0
   ) RETURN NUMBER;

   PROCEDURE MERGE_FACE_GEOMETRY_WORK (
      p_topology           IN VARCHAR2,
      p_transaction_id     IN VARCHAR2,
      p_merge_case         IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_small_face_id      IN NUMBER,
      p_big_face_id_1      IN NUMBER,
      p_big_face_id_2      IN NUMBER DEFAULT NULL,
      p_big_face_id_3      IN NUMBER DEFAULT NULL,
      p_small_faces        IN OUT GZ_TYPES.NUMBERARRAY,
      p_big_faces          IN OUT GZ_TYPES.NUMBERARRAY,
      p_dead_edges         IN OUT GZ_TYPES.NUMBERARRAY,
      p_new_edges          IN OUT GZ_TYPES.NUMBERARRAY,
      p_dead_nodes         IN OUT GZ_TYPES.NUMBERARRAY,
      p_log_type           IN VARCHAR2
   );

   PROCEDURE UPDATE_SURVIVING_FACE (
      p_face_table         IN VARCHAR2,
      p_big_face           IN NUMBER,
      p_small_face         IN NUMBER,
      p_merge_case         IN VARCHAR2
   );

   PROCEDURE UPDATE_FEATURE_FACE (
      p_face_table         IN VARCHAR2,
      p_feature_face_id    IN NUMBER
   );

   PROCEDURE UPDATE_EDITED_FSLS (
      p_topology           IN VARCHAR2,
      p_transaction_id     IN VARCHAR2,
      p_fsl_hash           IN GZ_TYPES.stringhash,
      p_face_table         IN VARCHAR2,
      p_fsl_pkc_col        IN VARCHAR2,
      p_log_type           IN VARCHAR2  --want to log all of these
   );

   PROCEDURE MATCH_FSLS (
      p_topology           IN VARCHAR2,
      p_transaction_id     IN VARCHAR2,
      p_merge_case         IN VARCHAR2,
      p_log_type           IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_small_face_id      IN NUMBER,
      p_big_face_id        IN NUMBER,
      p_fsl_small_hash     IN GZ_TYPES.stringhash,
      p_fsl_big_hash       IN GZ_TYPES.stringhash,
      p_fsls_edited        IN OUT GZ_TYPES.stringhash,
      p_fsl_pkc_col        IN VARCHAR2,
      p_debug              IN NUMBER DEFAULT NULL
   );

   FUNCTION FACE_MERGE (
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_topology           IN VARCHAR2,
      p_log_type           IN VARCHAR2 DEFAULT NULL,
      p_small_face_id      IN NUMBER,
      p_big_face_id        IN NUMBER,
      p_big_face_id_2      IN NUMBER DEFAULT NULL,
      p_big_face_id_3      IN NUMBER DEFAULT NULL,
      p_allow_extinction   IN VARCHAR DEFAULT 'N',
      p_expendable_tab     IN VARCHAR2 DEFAULT NULL,
      p_fsl_pkc            IN VARCHAR2 DEFAULT 'GEO_ID',
      p_debug              IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2;

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

   FUNCTION UPDATE_SPE_RESULTS(
     p_area_tolerance            IN NUMBER,
     p_spe_table                 IN VARCHAR2,
     p_topo                      IN VARCHAR2,
     p_process                   IN VARCHAR2,
     p_stepnum                   IN VARCHAR2
   ) RETURN VARCHAR2;

END GZ_SMPOLY;
/
