CREATE OR REPLACE PACKAGE GZ_BUSINESS_UTILS
AUTHID CURRENT_USER
AS


   --Some Sidey creators

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

   FUNCTION TABLE_EXISTS ( pInTable  IN VARCHAR2, pInSchema IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN;


   -----------------------------------------------------------------------------
   -- Tracking table creator and inserters
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







   -------------------------------
   --All other table creators
   -------------------------------


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

   PROCEDURE CREATE_GZ_BUILD_TILE (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE CREATE_GZ_BUILD_GEOM (
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


   --coastal sliver transaction table
   PROCEDURE CREATE_GZ_FACE_SLIVERS (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   );

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











   -----------------------------------------------------------------------------
   -- Database Objects
   -----------------------------------------------------------------------------

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

   FUNCTION GET_PRIMARY_KEY_COLS (
      p_table_name      IN VARCHAR2,
      p_schema          IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.stringarray;

   --generic build script helper
   PROCEDURE COPY_TO_X (
      p_tablename     IN VARCHAR2,
      p_xxx           IN VARCHAR2 DEFAULT NULL
   );




   ------------------------
   --Module Utilities
   ------------------------

   PROCEDURE UPDATE_FACE_MEASUREMENTS(
      p_table      IN VARCHAR2,
      p_join_key   IN VARCHAR2 DEFAULT 'FACE_ID',
      p_list       IN VARCHAR2 DEFAULT NULL
   );

   FUNCTION GET_REF_SCHEMAS (
      p_schema             IN VARCHAR2,
      p_ref_schema_tab     IN VARCHAR2 DEFAULT 'REFERENCE_SCHEMAS'
   ) RETURN GZ_TYPES.stringhash;

   FUNCTION GET_REFERENCE_FACE_FIELDS (
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_type           IN VARCHAR2 DEFAULT 'ATTRIBUTE', --or MEASUREMENT
      p_ref_table      IN VARCHAR2 DEFAULT 'REFERENCE_FACE_FIELDS',
      p_ref_col        IN VARCHAR2 DEFAULT 'FIELD',
      p_ref_field      IN VARCHAR2 DEFAULT NULL
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
      p_table_name        IN VARCHAR2,
      p_empty_exists      BOOLEAN default FALSE
   ) RETURN BOOLEAN;

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

    PROCEDURE JAVA_MEMORY_MANAGER (
      p_table_name        IN VARCHAR2,
      p_sdo_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_error_msg         IN VARCHAR2 DEFAULT NULL,
      p_filter            IN SDO_GEOMETRY DEFAULT NULL
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

   PROCEDURE UPDATE_HARD_CODED_TABLES (
      p_table_list            IN VARCHAR2,
      p_type                  IN VARCHAR2
   );


   PROCEDURE PRIV_GRANTS_ON_LIST (
      p_table_list            IN VARCHAR2
   );

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

   PROCEDURE CPB_SLEEP (
      p_time_seconds    IN NUMBER
   );

   PROCEDURE DBMS_SQL_HELPER (
      p_sql             IN CLOB
   );




   ---------------------------------------------
   --Parsing and logic utils
   ---------------------------------------------



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

   FUNCTION LIST_TYPE_TO_NUMARRAY (
      p_input     IN MDSYS.SDO_LIST_TYPE
   ) RETURN GZ_TYPES.NUMBERARRAY DETERMINISTIC;

   FUNCTION NUMARRAY_TO_LIST_TYPE (
      p_input     IN GZ_TYPES.NUMBERARRAY
   ) RETURN MDSYS.SDO_LIST_TYPE DETERMINISTIC;
   
   FUNCTION STRINGLIST_TO_STRINGARRAY (
      p_input     IN MDSYS.STRINGLIST
   ) RETURN GZ_TYPES.stringarray DETERMINISTIC;
   
   FUNCTION STRINGARRAY_TO_STRINGLIST (
      p_input     IN GZ_TYPES.stringarray
   ) RETURN MDSYS.STRINGLIST DETERMINISTIC;

   FUNCTION STRINGARRAY_ADD (
      p_input_1           IN GZ_TYPES.stringarray,
      p_input_2           IN GZ_TYPES.stringarray
   ) RETURN GZ_TYPES.stringarray DETERMINISTIC;

   FUNCTION NUMBERARRAY_ADD (
      p_input_1           IN GZ_TYPES.numberarray,
      p_input_2           IN GZ_TYPES.numberarray
   ) RETURN GZ_TYPES.numberarray DETERMINISTIC;

   FUNCTION NUMBERARRAY_ADD_UNIQUE (
     p_input_1   IN GZ_TYPES.numberarray,
     p_input_2   IN GZ_TYPES.numberarray
   ) RETURN GZ_TYPES.numberarray DETERMINISTIC;

   FUNCTION LIST_TYPE_ADD_UNIQUE (
     p_input_1   IN MDSYS.SDO_LIST_TYPE,
     p_input_2   IN MDSYS.SDO_LIST_TYPE
   ) RETURN MDSYS.SDO_LIST_TYPE DETERMINISTIC;
   
   FUNCTION LIST_TYPE_ADD (
     p_input_1   IN MDSYS.SDO_LIST_TYPE,
     p_input_2   IN MDSYS.SDO_LIST_TYPE
   ) RETURN MDSYS.SDO_LIST_TYPE DETERMINISTIC;
   
   FUNCTION STRINGLIST_ADD (
     p_input_1   IN MDSYS.STRINGLIST,
     p_input_2   IN MDSYS.STRINGLIST
   ) RETURN MDSYS.STRINGLIST DETERMINISTIC;

   FUNCTION NUMBERARRAY_SUBTRACT (
      p_input_1           IN GZ_TYPES.numberarray,
      p_input_2           IN GZ_TYPES.numberarray
   ) RETURN GZ_TYPES.numberarray DETERMINISTIC;

   FUNCTION NUMBERARRAY_SUBTRACT (
      p_input_1           IN GZ_TYPES.numberarray,
      p_input_2           IN NUMBER
   ) RETURN GZ_TYPES.numberarray DETERMINISTIC;

   FUNCTION CLOB_TO_VARRAY (
      p_input           IN CLOB,
      p_delimiter       IN VARCHAR2 DEFAULT ','
   ) RETURN MDSYS.STRING_ARRAY DETERMINISTIC;


   FUNCTION QUERY_DELIMITED_LIST (
      p_input             IN GZ_TYPES.stringarray,
      p_query             IN VARCHAR2
   ) RETURN NUMBER DETERMINISTIC;

   FUNCTION QUERY_DELIMITED_LIST (
      p_input             IN GZ_TYPES.stringarray,
      p_query             IN NUMBER
   ) RETURN NUMBER DETERMINISTIC;

   FUNCTION QUERY_DELIMITED_LIST (
      p_input             IN MDSYS.STRINGLIST,
      p_query             IN VARCHAR2
   ) RETURN NUMBER DETERMINISTIC;

   FUNCTION QUERY_DELIMITED_LIST (
      p_input             IN MDSYS.SDO_LIST_TYPE,
      p_query             IN NUMBER
   ) RETURN NUMBER DETERMINISTIC;

   FUNCTION DUMP_NESTED_HASH (
      p_input             IN GZ_TYPES.nestedhash
   ) RETURN VARCHAR2 DETERMINISTIC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

END GZ_BUSINESS_UTILS;
/
