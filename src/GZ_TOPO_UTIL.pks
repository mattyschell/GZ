CREATE OR REPLACE PACKAGE GZ_TOPO_UTIL AUTHID CURRENT_USER   AS

   PROCEDURE PURGE_TOPOLOGY(
      pTGT_SCHEMA IN VARCHAR2,
      pTGT_TOPOLOGY IN VARCHAR2,
      pFT_KEEP      IN VARCHAR2 DEFAULT NULL
   );

   PROCEDURE PURGE_TOPOLOGIES (
      p_schema        IN VARCHAR2,
      p_topo_like     IN VARCHAR2
   );

   FUNCTION CREATE_TOPOLOGY(
      TGT_SCHEMA        IN VARCHAR2,
      TGT_TOPOLOGY      IN VARCHAR2,
      TGT_TOLERANCE     IN NUMBER,
      TGT_SRID          IN NUMBER,
      TGT_DIGITS_RIGHT  IN NUMBER DEFAULT 16
   ) RETURN NUMBER;

   PROCEDURE COPY_PRIMITIVES(
      SRC_SCHEMA IN VARCHAR2,
      SRC_TOPOLOGY IN VARCHAR2,
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2
   );
   PROCEDURE CREATE_RELATION_SHELL(
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2
   );
   PROCEDURE CREATE_FEATURE_TABLE(
      SRC_SCHEMA IN VARCHAR2,
      SRC_FTABLE IN VARCHAR2,
      TGT_SCHEMA IN VARCHAR2,
      TGT_FTABLE IN VARCHAR2
   );
   PROCEDURE COPY_FEATURE_TABLE(
      SRC_SCHEMA IN VARCHAR2,
      SRC_FTABLE IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2,
      TGT_SCHEMA IN VARCHAR2,
      TGT_FTABLE IN VARCHAR2,
      TGT_LAYER_ID IN VARCHAR2
   );
   PROCEDURE UPDATE_TOPOLOGY_ID(
      TGT_SCHEMA IN VARCHAR2,
      TGT_FTABLE IN VARCHAR2,
      TOPOLOGY_ID IN NUMBER
   );
   PROCEDURE ADD_RELATION_PARTITION(
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2,
      TGLAYER_ID IN NUMBER
   );
   PROCEDURE COPY_RELATION_DATA(
      SRC_SCHEMA IN VARCHAR2,
      SRC_TOPOLOGY IN VARCHAR2,
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2,
      TGLAYER_ID IN NUMBER
   );
   PROCEDURE REGISTER_FEATURE_TABLE(
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2,
      FEATURE_TABLE IN VARCHAR2,
      TGLAYER_TYPE IN VARCHAR2,
      TGLAYER_ID IN NUMBER,
      CLAYER_ID NUMBER
   );
   PROCEDURE PROCESS_FEATURE(
      SRC_SCHEMA IN VARCHAR2,
      SRC_TOPOLOGY IN VARCHAR2,
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2,
      TOPOLOGY_ID IN NUMBER,
      SRC_FTABLE IN VARCHAR2,
      TGT_FTABLE IN VARCHAR2,
      TGLAYER_ID IN NUMBER,
      TGLAYER_TYPE IN VARCHAR2,
      CLAYER_ID NUMBER
   );
   PROCEDURE INIT_METADATA(
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2
   );
   PROCEDURE VALIDATE_TOPOLOGY(
      TGT_SCHEMA IN VARCHAR2,
      TGT_TOPOLOGY IN VARCHAR2
   );
   FUNCTION VALIDATE_FEATURE_TABLES(
      pSRC_SCHEMA IN VARCHAR2,
      pSRC_TOPOLOGY IN VARCHAR2
   ) RETURN BOOLEAN;
   
   PROCEDURE COPY_TOPOLOGY(
      pSRC_SCHEMA IN VARCHAR2,
      pSRC_TOPOLOGY IN VARCHAR2,
      pTGT_SCHEMA IN VARCHAR2,
      pTGT_TOPOLOGY IN VARCHAR2,
      pPURGE_TGT_TOPOLOGY IN CHAR DEFAULT 'N',
      pVERIFY_SRC_TOPOLOGY IN CHAR DEFAULT 'N',
      pGRANT_PRIVS IN CHAR DEFAULT 'Y'
   );
   
   PROCEDURE COPY_TOPOLOGY(
      pSRC_SCHEMA IN VARCHAR2,
      pSRC_TOPOLOGY IN VARCHAR2,
      pTGT_SCHEMA IN VARCHAR2,
      pTGT_TOPOLOGY IN VARCHAR2,
      pPURGE_TGT_TOPOLOGY IN CHAR,
      pVERIFY_SRC_TOPOLOGY IN CHAR,
      pGRANT_PRIVS IN CHAR DEFAULT 'Y',
      pCOPY_FTABLES IN CHAR
   );
   
   PROCEDURE VALIDATE_TGID(
      pSCHEMA IN VARCHAR2,
      pTOPOLOGY IN VARCHAR2,
      pFTABLE IN VARCHAR2,
      pPK IN VARCHAR2,
      pOUTTABLE IN VARCHAR2,
      pTG_ID IN NUMBER,
      pTGL_ID IN NUMBER
   );
   PROCEDURE VALIDATE_FTABLE(
      pSCHEMA IN VARCHAR2,
      pTOPOLOGY IN VARCHAR2,
      pFTABLE IN VARCHAR2,
      pPK IN VARCHAR2,
      pOUTTABLE IN VARCHAR2
   );
   PROCEDURE REBUILD_INDEXES(
      pTOPOLOGY IN VARCHAR2
   );

   PROCEDURE CREATE_INDEXES (
         pSRC_SCHEMA        IN VARCHAR2,
         pSRC_TOPOLOGY    IN VARCHAR2,
         pTGT_TOPOLOGY    IN VARCHAR2
   );

   FUNCTION GET_IND_COLUMNS (pSchema        IN VARCHAR2,
                             pTable_name    IN VARCHAR2,
                             pIndexl_name   IN VARCHAR2)
      RETURN VARCHAR2;

   PROCEDURE FIX_CnnctByLp_Err(
      pTOPOLOGY IN VARCHAR2,
      pTABLE_NAME IN VARCHAR2,
      pPKCol IN VARCHAR2,
      pPKVal IN NUMBER,
      pSDOTopoGeomCol IN VARCHAR2
   );
   PROCEDURE PURGE_TOPO_AND_TABLES(
      pTOPOLOGY IN VARCHAR2,
      pSCHEMA_NAME IN VARCHAR2
   );
   PROCEDURE DROP_TABLES_WITH_PREFIX(
      pPREFIX IN VARCHAR2,
      pSCHEMA_NAME IN VARCHAR2
   );
   PROCEDURE FIX_RELATION_DOLLAR_INDEXES(
      pTopology IN VARCHAR2
   );

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

   PROCEDURE GATHER_TOPO_STATS (
    p_topo_name      IN VARCHAR2
   );


   ---------------------------------------------------
   --Topo Parsers and Accessors and Primitive Changers
   ---------------------------------------------------

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


   FUNCTION COUNT_OBSOLETE_NODES(
      p_topology       IN VARCHAR2
   ) RETURN NUMBER;

   FUNCTION GET_OBSOLETE_NODES(
      p_topology       IN VARCHAR2
   ) RETURN GZ_TYPES.numberarray;

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

  PROCEDURE REMOVE_ISOLATED_NODES_CACHE (
    p_topo           IN VARCHAR2
  );

  PROCEDURE DEREGISTER_FEATURE_TABLES (
      p_schema             IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_drop_tabs          IN VARCHAR2 DEFAULT 'Y',
      p_min_tg_layer_level IN NUMBER DEFAULT 0,
      p_likeclause         IN VARCHAR2 DEFAULT NULL
   );


    PROCEDURE GZ_CHANGE_EDGE_COORDS (
      p_topo          IN VARCHAR2,
      p_edge_id       IN NUMBER,
      p_edge_geom     IN SDO_GEOMETRY,
      p_clean_edge    IN NUMBER DEFAULT NULL,
      p_delta         IN NUMBER DEFAULT 0,
      p_debug         IN NUMBER DEFAULT NULL
   );

   PROCEDURE GZ_MOVE_NODE (
      p_topo          IN VARCHAR2,
      p_node_id       IN NUMBER,
      p_move_point    IN SDO_GEOMETRY,
      p_debug         IN NUMBER DEFAULT NULL,
      p_topology      IN VARCHAR2 DEFAULT NULL
   );

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


   PROCEDURE GZ_ADD_TOPO_GEOMETRY_LAYER (
      p_topo            IN VARCHAR2,
      p_table_name      IN VARCHAR2,
      p_column_name     IN VARCHAR2 DEFAULT 'TOPOGEOM',
      p_layer_type      IN VARCHAR2 DEFAULT 'POLYGON',
      p_child_tab       IN VARCHAR2 DEFAULT NULL
   );


   -----------------------------------------------------------------------------
   -- Create Topo
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





   ------------------------------
   --Validation
   ----------------------------------


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



   ---------
   --Misc Weirdos
   --------

   FUNCTION CLOSED_LOOPS (
      p_tablename       IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_edge_id         IN NUMBER DEFAULT NULL
   ) RETURN VARCHAR2;

   PROCEDURE IMPORT_SDOTOPO11G (
      p_topo            IN VARCHAR2,
      p_feature_tabs    IN VARCHAR2 DEFAULT NULL
   );

   FUNCTION ADD_FAKE_FACE (
      p_topo            IN VARCHAR2,
      p_edge_id         IN NUMBER,
      p_dupe_geom       IN SDO_GEOMETRY,
      p_dupe_i          IN PLS_INTEGER,
      p_face_tab        IN VARCHAR2,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN NUMBER;

   FUNCTION GET_TEMP_EDGE(
      p_topo            IN VARCHAR2,
      p_dupepoint_geom  IN SDO_GEOMETRY,
      p_edge_id         IN NUMBER
   ) RETURN NUMBER;

   PROCEDURE REMOVE_FAKE_FACE (
      p_topo            IN VARCHAR2,
      p_face_feat_tab   IN VARCHAR2
   );


END GZ_TOPO_UTIL;
/
