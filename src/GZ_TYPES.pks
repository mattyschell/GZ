CREATE OR REPLACE PACKAGE GZ_TYPES
AUTHID CURRENT_USER
AS
 ------------------------------------------------------------------------------------
 --++SHARED TYPES++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
 ------------------------------------------------------------------------------------
 --
 -----------------
 --Generic types--
 -----------------
 --
 TYPE stringarray IS TABLE OF VARCHAR2(4000)
 INDEX BY PLS_INTEGER;
 --
 TYPE stringarray1 IS TABLE OF VARCHAR2(1)
 INDEX BY PLS_INTEGER;
 --
 TYPE stringarray32 IS TABLE OF VARCHAR2(32)
 INDEX BY PLS_INTEGER;
 --
 TYPE numberarray IS TABLE OF NUMBER
 INDEX BY PLS_INTEGER;
 --
 TYPE geomarray IS TABLE OF SDO_GEOMETRY
 INDEX BY PLS_INTEGER;
 --
 TYPE stringhash IS TABLE OF VARCHAR2(4000)
 INDEX BY VARCHAR2(4000);
 --
 TYPE numberhash IS TABLE OF NUMBER
 INDEX BY VARCHAR2(4000);
 --
 TYPE geomhash IS TABLE OF SDO_GEOMETRY
 INDEX BY VARCHAR2(4000);

 TYPE doublenum IS RECORD (
    number1 NUMBER,
    number2 NUMBER
 );

 TYPE doublenumarray IS TABLE OF doublenum
 INDEX BY PLS_INTEGER;

 TYPE nestedhash IS TABLE OF GZ_TYPES.numberhash
 INDEX BY VARCHAR2(4000);

 --
 --
 --Used in eliminate small polygons
 TYPE SMALL_POLY_EDGES_REC IS RECORD (
 EDGE_ID NUMBER,
 LEFT_FACE_ID NUMBER,
 RIGHT_FACE_ID NUMBER,
 KEEP_FACE NUMBER,
 NOT_KEEP NUMBER,
 AREATOTAL NUMBER,
 MAX_AREA NUMBER
 );
 TYPE SMALL_POLY_EDGES IS TABLE OF SMALL_POLY_EDGES_REC;

 --Used in small poly vertex thinning
 TYPE RSUV_REC IS RECORD (
   id             NUMBER,
   sdogeometry    SDO_GEOMETRY
 );
 TYPE SMALL_POLY_RSUV IS TABLE OF RSUV_REC;
 --
 -- left/right and areas not actually used during delete edges
 -- really only need edge_id, keep_face, not_keep, DONE, SQLSTMT, and ERROR_MSG
 TYPE DELETE_EDGES_INPUT_REC IS RECORD (
 EDGE_ID NUMBER,
 LEFT_FACE_ID NUMBER,
 RIGHT_FACE_ID NUMBER,
 KEEP_FACE NUMBER,
 NOT_KEEP NUMBER,
 AREATOTAL NUMBER,
 MAX_AREA NUMBER,
 DONE VARCHAR2(100),
 SQLSTMT VARCHAR2(4000),
 ERROR_MSG VARCHAR2(4000)
 );
 TYPE DELETE_EDGES_INPUT IS TABLE OF DELETE_EDGES_INPUT_REC;
 --
 TYPE SMALL_POLYGON_PARAMETERS_REC IS RECORD(
 RELEASE             VARCHAR2(10),
 GEN_PROJECT_ID VARCHAR2(4),
 GEN_TOPOLOGY_NAME VARCHAR2(32),
 EDGE_ATT_TABLE VARCHAR2(32),
 FACE_FEATURE_TABLE VARCHAR2(32),
 CLIP_EDGE_FEATURE_TABLE VARCHAR2(32),
 AREA_TOLERANCE NUMBER,
 AREA_THRESHOLD NUMBER,
 SP_MODULES VARCHAR2(4000),
 USER_LAST_MODIFIED VARCHAR2(32),
 DATE_LAST_MODIFIED DATE
 );
 TYPE SMALL_POLYGON_PARAMETERS IS TABLE OF SMALL_POLYGON_PARAMETERS_REC;
 --
 TYPE FSLBUILD_PARAMETERS_REC IS RECORD(
 RELEASE             VARCHAR2(10),
 GEN_PROJECT_ID VARCHAR2(4),
 FACE_TAB_EXT VARCHAR2(20),
 TOPO_UNIVERSE VARCHAR2(30),
 TOPO_HIERARCHY VARCHAR2(30),
 STATE_EDGES_TABLE VARCHAR2(30),
 FSLBUILD_MODULES VARCHAR2(10),
 PROJECT_Z9 VARCHAR2(1),
 TRACKING_TABLE_SUFFIX VARCHAR2(20),
 USER_LAST_MODIFIED VARCHAR2(20),
 DATE_LAST_MODIFIED DATE
 );
 TYPE FSLBUILD_PARAMETERS IS TABLE OF FSLBUILD_PARAMETERS_REC;
 --
 TYPE QA_PARAMETERS_REC IS RECORD(
 RELEASE             VARCHAR2(10),
 GEN_PROJECT_ID VARCHAR2(4),
 CMP_TABLE VARCHAR2(32),
 COMPARE_GEN_COLUMN VARCHAR2(20),
 AREA_CHECK NUMBER,
 SHAPE_CHECK NUMBER,
 USER_LAST_MODIFIED VARCHAR2(32),
 DATE_LAST_MODIFIED DATE
 );
 TYPE QA_PARAMETERS IS TABLE OF QA_PARAMETERS_REC;
 --
 TYPE GZ_SHAPEFILE_PARAMETERS_REC IS RECORD(
 RELEASE VARCHAR2(64),
 GEN_PROJECT_ID VARCHAR2(4),
 SUM_LEV VARCHAR2(4),
 POLYGON VARCHAR2(1),
 LINE VARCHAR2(1),
 DROP_OUTLINE VARCHAR2(1),
 NATION VARCHAR2(1),
 FANOUT VARCHAR2(1),
 GV_CODE VARCHAR2(2),
 UNIT_FIELD_NAME VARCHAR2(100),
 UNIT_DIR_PREFIX VARCHAR2(100),
 MODTAG VARCHAR2(5),
 WAVE NUMBER,
 USER_LAST_MODIFIED VARCHAR2(20),
 DATE_LAST_MODIFIED DATE
 );
 TYPE GZ_SHAPEFILE_PARAMETERS IS TABLE OF GZ_SHAPEFILE_PARAMETERS_REC;
 --
 TYPE GZ_JOB_PARAMETERS_REC IS RECORD(
 RELEASE VARCHAR2(10),
 GEN_PROJECT_ID VARCHAR2(4),
 BENCH_MARK_PRCS_UNIT VARCHAR2(1),
 TOPOBUILD_PRCS_UNIT VARCHAR2(1),
 GZ_PRCS_UNIT VARCHAR2(1),
 FINAL_TOPO_PRCS_UNIT VARCHAR2(1),
 SHAPE_FILE_PRCS_UNIT VARCHAR2(1),
 GZ_MODULES VARCHAR2(20),
 UNIVERSE_TABLE    VARCHAR2(32),
 HIERARCHY_TABLE   VARCHAR2(32),
 STATE_TABLE VARCHAR2(30),
 GEN_CLIP_TABLE VARCHAR2(32),
 TARGET_SCALE   NUMBER,
 STATE_EDGES_TBL VARCHAR2(30),
 REFERENCE_SCHEMAS VARCHAR2(30),
 REFERENCE_FACE_FIELDS VARCHAR2(30),
 STATE_ENTITY_TBL VARCHAR2(30),
 SOURCE_SCHEMA   VARCHAR2(30),
 SOURCE_TOPOLOGY   VARCHAR2(32),
 COMMENTS VARCHAR2(400),
 USER_LAST_MODIFIED VARCHAR2(20),
 DATE_LAST_MODIFIED DATE);
 TYPE GZ_JOB_PARAMETERS IS TABLE OF GZ_JOB_PARAMETERS_REC;
 --
 --Stub of the edge attribute table
 --used in small poly removal and expanded for clipping
 --
 TYPE EDGE_ATTRIBUTE_REC IS RECORD (
 EDGE_ID number,
 EDGELEN number,
 EDGE_CNTRLFLG VARCHAR2(100),
 LEFT_FACE_ID number,
 RIGHT_FACE_ID number
 );
 TYPE EDGE_ATTRIBUTE IS TABLE OF EDGE_ATTRIBUTE_REC;
 ---
 TYPE LINE_SIM_PARAMETERS_REC IS RECORD(
 RELEASE VARCHAR2(10),
 GEN_PROJECT_ID VARCHAR2(4),
 GEN_TOPOLOGY_NAME VARCHAR2(32),
 METHOD VARCHAR2(32),
  NICE NUMBER,
 EDGES_TABLE VARCHAR2(32),
 SKIP_EDGES_TABLE VARCHAR2(4000),
 DONE_EDGES_TABLE VARCHAR2(32),
 BAD_TABLE VARCHAR2(32),
 STATE_EDGE_TABLE VARCHAR2(32),
 ENTITYFP VARCHAR2(32),
 EDGE_TABLE VARCHAR2(32),
 SRCEDGE_TABLE VARCHAR2(32),
 FACETABLE VARCHAR2(32),
 LS_MODULES VARCHAR2(32),
 UNGENQALIMITPCT NUMBER,
 CAV_MATCH_LENGTH INTEGER,
 AREA_CHECK NUMBER,
 PERCENT_UNGEN NUMBER,
 MAX_CONSEC_MATCH NUMBER,
 CAV_INC_ST_EDGES VARCHAR2(3),
 CAV_PPRINT VARCHAR2(3),
 USER_LAST_MODIFIED VARCHAR2(32),
 DATE_LAST_MODIFIED DATE
 );
 TYPE LINE_SIM_PARAMETERS IS TABLE OF LINE_SIM_PARAMETERS_REC;
 --
TYPE SHAPEFILE_PARAMETERS_REC IS RECORD(
 RELEASE             VARCHAR2(10),
 GEN_PROJECT_ID      VARCHAR2(4),
 QA_STRING           VARCHAR2(10),
 FSL_STRING          VARCHAR2(10),
 YEAR                VARCHAR2(20),
 GEN_LEVEL           VARCHAR2(4),
 GENCODE             VARCHAR2(1),
 PROJECTION          VARCHAR2(50),
 QA_UNGEN_OUT_DIR    VARCHAR2(200),
 QA_OUT_DIR          VARCHAR2(200),
 PRD_OUT_DIR         VARCHAR2(200),
 STATE_TOPO_FLAG     VARCHAR2(1),
 DEL_CPG_FLAG        VARCHAR2(3),
 MAPPING_FILE_DIR    VARCHAR2(200),
 USER_LAST_MODIFIED  VARCHAR2(32),
 DATE_LAST_MODIFIED  DATE
);
 TYPE SHAPEFILE_PARAMETERS IS TABLE OF SHAPEFILE_PARAMETERS_REC;
 --
 --Expanded edge attribute type for line simplification
 --
 TYPE EDGE_ATTRIBUTE_SIMP_REC IS RECORD (
 EDGE_ID NUMBER,
 START_NODE_ID NUMBER,
 END_NODE_ID NUMBER,
 LEFT_FACE_ID NUMBER,
 RIGHT_FACE_ID NUMBER,
 STATE VARCHAR2(4000),
 UNIQ NUMBER,
 MT_LENGTH NUMBER,
 XLL NUMBER,
 YLL NUMBER,
 XUR NUMBER,
 YUR NUMBER,
 NEARBY_EDGES SDO_LIST_TYPE,
 VERTICES NUMBER,
 IGNORE NUMBER,
 GEOMETRY SDO_GEOMETRY,
 NEW_GEOMETRY SDO_GEOMETRY
 );
 TYPE EDGE_ATTRIBUTE_SIMPLIFY IS TABLE OF EDGE_ATTRIBUTE_SIMP_REC;
 --
 --
 ---------------------------------------------
 --- Functions to create empty work tables --
 ---------------------------------------------
 --
 FUNCTION NEW_EDGE_ATTRIBUTE
 RETURN GZ_TYPES.EDGE_ATTRIBUTE PIPELINED;
 --
 FUNCTION NEW_EDGE_ATTRIBUTE_SIMPLIFY
 RETURN GZ_TYPES.EDGE_ATTRIBUTE_SIMPLIFY PIPELINED;
 --
 -----------------------------------
 --Historical generalization types--
 -----------------------------------
 --Not sure if this is used by anything
 TYPE CDB_PRM_ARRAY IS VARRAY(10) OF VARCHAR2(100);
 --Used in FIND_SLITS
 TYPE SDO_GEOMETRY_ARRAY IS VARRAY(1048576) OF MDSYS.SDO_GEOMETRY;
 --Used in GZ_TOPO_EDIT
 --And GZ_TOPO_EDIT_0
 TYPE SLIVER_REC IS RECORD (
 FACE_ID NUMBER,
 STATEFP VARCHAR2(2 BYTE),
 SLIVER_LENGTH NUMBER,
 SLIVER_START NUMBER,
 SLIVER_END NUMBER,
 SLIVER_MID NUMBER,
 START_ON VARCHAR2(5 BYTE),
 XSTART NUMBER,
 YSTART NUMBER,
 XEND NUMBER,
 YEND NUMBER,
 XMID NUMBER,
 YMID NUMBER,
 NEW_GEOMETRY MDSYS.SDO_GEOMETRY,
 DONE CHAR(5 BYTE),
 EFFDT DATE
 );
 TYPE SLIVER_EDITS IS TABLE OF SLIVER_REC;
 -----------
 --Logging--
 -----------
 --generic
 TYPE GEN_EXTENDED_TRACKING_REC IS RECORD (
 table_name VARCHAR2(32),
 process VARCHAR2(100),
 step VARCHAR2(4000),
 start_time timestamp,
 end_time timestamp,
 elapsed_time interval day(5) to second(6),
 release VARCHAR2(100),
 sqlstmt VARCHAR2(4000),
 deploy VARCHAR2(32),
 luser VARCHAR2(32),
 error_msg VARCHAR2(4000),
 sdo_dump SDO_GEOMETRY
 );
 TYPE GEN_EXTENDED_TRACKING IS TABLE OF GEN_EXTENDED_TRACKING_REC;

 TYPE GEN_TRACKING_REC IS RECORD (
 table_name VARCHAR2(30),
 process VARCHAR2(100),
 step VARCHAR2(4000),
 start_time        timestamp with time zone,
      end_time          timestamp with time zone,
      elapsed_time      interval day(5) to second(6),
      release           VARCHAR2(100),
      sqlstmt           VARCHAR2(4000),
      deploy            VARCHAR2(32)
   );
   TYPE GEN_TRACKING IS TABLE OF GEN_TRACKING_REC;

   TYPE LUT_LSAD_REC IS RECORD (
      RELEASE               VARCHAR2(10),
      LSAD                    VARCHAR2(2),
      PUBLICATION             VARCHAR2(100),
      STANDARD                VARCHAR2(100),
      SHORT                   VARCHAR2(100),
      PRESUF                  VARCHAR2(1),
      VERSION                 VARCHAR2(4),
      PUBL_NAME               VARCHAR2(600),
      STND_NAME               VARCHAR2(600),
      SHRT_NAME               VARCHAR2(600)
   );

   TYPE LUT_LSAD IS TABLE OF LUT_LSAD_REC;

 -----------------------------------
 --Topo Build Types - are these types implemented or used anywhere?--
 -----------------------------------

   TYPE TOPO_UNIVERSE_REC IS RECORD (
    EXEC_ORDER              NUMBER,
    TOPOLOGY                VARCHAR2(32),
    TABLE_NAME              VARCHAR2(32),
    TABLE_TYPE              VARCHAR2(2),
    SUM_LEVEL               VARCHAR2(7),
    SUM_LEVEL_NAME          VARCHAR2(250),
    SOURCE_LOCATION         VARCHAR2(250),
    WHERE_CLAUSE            VARCHAR2(1000),
    TBL_KEYS                VARCHAR2(250),
    EXCEPT_COLS             VARCHAR2(4000),
    ADD_CODE1               VARCHAR2(4000),
    ADD_CODE2               VARCHAR2(4000),
    DEPLOY                  VARCHAR2(32),
    RELEASE                 VARCHAR2(20),
    TBL_PF                  VARCHAR2(32),
    ADD_FACE_COLS           VARCHAR2(32)
   );

   TYPE TOPO_UNIVERSE IS TABLE OF TOPO_UNIVERSE_REC;

   TYPE TOPO_HIERARCHY_REC IS RECORD (
    TOPOLOGY                VARCHAR2(20),
    TABLE_NAME              VARCHAR2(32),
    COLUMN_NAME             VARCHAR2(32),
    CHILD_LAYER_TBL         VARCHAR2(32),
    NAME                    VARCHAR2(250),
    GTYPE                   VARCHAR2(10),
    SEQ_NUM                 NUMBER
   );

   TYPE TOPO_HIERARCHY IS TABLE OF TOPO_HIERARCHY_REC;

   TYPE TOPO_FIELD_DEF_REC IS RECORD (
    MTCOLDEF                VARCHAR2(32),
    SPECCOLDEF              VARCHAR2(32),
    COLLEN                  NUMBER
   );

   TYPE TOPO_FIELD_DEF IS TABLE OF TOPO_FIELD_DEF_REC;


   -----------------------------------
   --"Alternate" Topo Build Types --
   -----------------------------------

   TYPE GZ_LAYERS_IN_REC IS RECORD (
      release                 VARCHAR2(64),
      gen_project_id          VARCHAR2(4),
      layer                   VARCHAR2(30),
      source_table            VARCHAR2(30),
      source_key              VARCHAR2(28),
      alt_source_schema       VARCHAR2(30),
      where_clause            VARCHAR2(4000),
      boundary_definition     VARCHAR2(4000),
      topo_extent_lyr         VARCHAR2(1),
      join_tables             VARCHAR2(4000),
      notes                   VARCHAR2(4000),
      user_last_modified      VARCHAR2(32),
      date_last_modified      DATE
   );
   TYPE GZ_LAYERS_IN IS TABLE OF GZ_LAYERS_IN_REC;


   TYPE GZ_LAYERS_IN_INFO_REC IS RECORD (
      release                 VARCHAR2(64),
      gen_project_id          VARCHAR2(4),
      layer                   VARCHAR2(30),
      jobid                   VARCHAR2(20),
      source_schema           VARCHAR2(30),
      source_table            VARCHAR2(30),
      source_topology         VARCHAR2(30),
      source_key              VARCHAR2(28),
      where_clause            VARCHAR2(4000),
      layer_sql               VARCHAR2(4000),
      boundary_definition     VARCHAR2(4000),
      topo_extent_lyr         VARCHAR2(1),
      sdo_filter              SDO_GEOMETRY
   );
   TYPE GZ_LAYERS_IN_INFO IS TABLE OF GZ_LAYERS_IN_INFO_REC;

   TYPE GZ_BUILD_POLY_REC IS RECORD (
      source_key              VARCHAR2(4000),
      edge_id                 NUMBER,
      layer                   VARCHAR2(30)
    );
    TYPE GZ_BUILD_POLY IS TABLE OF GZ_BUILD_POLY_REC;

    TYPE GZ_BUILD_EDGE_REC IS RECORD (
      edge_id                 NUMBER,
      source_edge_id          NUMBER,
      tile_number             NUMBER
    );
    TYPE GZ_BUILD_EDGE IS TABLE OF GZ_BUILD_EDGE_REC;

    TYPE GZ_BUILD_TILE_REC IS RECORD (
      tile_number             NUMBER,
      sdogeometry             SDO_GEOMETRY,
      status_clue             VARCHAR2(32)
    );
    TYPE GZ_BUILD_TILE IS TABLE OF GZ_BUILD_TILE_REC;

    TYPE GZ_BUILD_GEOM_REC IS RECORD (
      edge_id                 NUMBER,
      geometry                SDO_GEOMETRY,
      processed               NUMBER
    );
    TYPE GZ_BUILD_GEOM IS TABLE OF GZ_BUILD_GEOM_REC;

   -----------------------------------
   --Module specific tracking table types --
   -----------------------------------
      TYPE GEN_SP_TRACKING_REC IS RECORD (
         topology          VARCHAR2(32),
         process           VARCHAR2(100),
         step              VARCHAR2(4000),
         start_time        timestamp,
         end_time          timestamp,
         elapsed_time      interval day(5) to second(6),
         sqlstmt           VARCHAR2(4000),
         deploy            VARCHAR2(32),
         message           VARCHAR2(4000),
         luser             VARCHAR2(32)
      );
      TYPE GEN_SP_TRACKING IS TABLE OF GEN_SP_TRACKING_REC;
   -- Line Simplification Tracking Record
   TYPE GEN_LS_TRACKING_REC IS RECORD (
      topology          VARCHAR2(32),
      process           VARCHAR2(100),
      step              VARCHAR2(4000),
      start_time        timestamp,
      end_time          timestamp,
      elapsed_time      interval day(5) to second(6),
      sqlstmt           VARCHAR2(4000),
      deploy            VARCHAR2(32),
      message           VARCHAR2(4000),
      luser             VARCHAR2(32)
   );
   TYPE GEN_LS_TRACKING IS TABLE OF GEN_LS_TRACKING_REC;
   -- FSL table creation Tracking Record
   TYPE GEN_FSL_TRACKING_REC IS RECORD (
      topology          VARCHAR2(32),
      process           VARCHAR2(100),
      step              VARCHAR2(4000),
      start_time        timestamp,
      end_time          timestamp,
      elapsed_time      interval day(5) to second(6),
      sqlstmt           VARCHAR2(4000),
      deploy            VARCHAR2(32),
      message           VARCHAR2(4000),
      luser             VARCHAR2(32)
   );
   TYPE GEN_FSL_TRACKING IS TABLE OF GEN_FSL_TRACKING_REC;
   ---------------------
   --Unique Clip Logging--
   ---------------------
   TYPE GEN_CLIP_MODULES_REC IS RECORD (
      gen_clip_mask        VARCHAR2(4000),  --For each clip mask
      module_1             VARCHAR2(1),     --CREATE_EMPTY_TOPOLOGY
      module_2             VARCHAR2(1),     --ADD_INTERIOR_LINES
      module_3             VARCHAR2(1),     --ADD_CLIP_MASK
      module_4             VARCHAR2(1),     --ID_DANGLES
      module_5             VARCHAR2(1),     --DELETE_EXTERIOR_DANGLES
      module_6             VARCHAR2(1),     --DELETE_DANGLING_FACES
      module_7             VARCHAR2(1),     --EXTEND_INTERIOR_DANGLES
      module_8             VARCHAR2(1),     --CREATE_NEW_FACE_TABLE
      module_9             VARCHAR2(1),     --POPULATE_FACE_TABLE
      module_10            VARCHAR2(1)      --POPULATE_MEASUREMENTS
   );
   TYPE GEN_CLIP_MODULES IS TABLE OF GEN_CLIP_MODULES_REC;
   ---------------------
   --Clip Output--
   ---------------------
   TYPE GEN_CLIP_EXPELLED_REC IS RECORD (
      expelled_id          NUMBER,
      gen_clip_mask        VARCHAR2(32),
      geography            VARCHAR2(4000),
      expelled_value       VARCHAR2(4000),
      psql                 VARCHAR2(4000),
      note                 VARCHAR2(4000)
   );
   TYPE GEN_CLIP_EXPELLED IS TABLE OF GEN_CLIP_EXPELLED_REC;

   TYPE GZ_BUILD_SOURCE_PARAMETERS_REC IS RECORD(
     RELEASE            VARCHAR2(20),
     GEN_PROJECT_ID       VARCHAR2(4),
     TILE_COUNT          NUMBER,
     SDO_FILTER           SDO_GEOMETRY,
     MODULES                   VARCHAR(20),
     RESTART_FLAG         VARCHAR2(1),
     SRID                        NUMBER,
     TOLERANCE             NUMBER,
     SNAPPING_DIGITS    NUMBER,
     DROP_TABLES         VARCHAR2(1),
     USER_LAST_MODIFIED         VARCHAR2(32),
     DATE_LAST_MODIFIED         DATE
   );
   TYPE GZ_BUILD_SOURCE_PARAMETERS IS TABLE OF GZ_BUILD_SOURCE_PARAMETERS_REC;

   TYPE GZ_OUTPUT_PARAMETERS_REC IS RECORD(
     RELEASE               VARCHAR2(20),
     GEN_PROJECT_ID        VARCHAR2(4),
     MODULES               VARCHAR(20),
     RESTART_FLAG          VARCHAR2(1),
     SINGLE_LAYER          VARCHAR2(1),
     TILE_COUNT_STATE      NUMBER,
     TILE_COUNT_NATION     NUMBER,
     SLIVER_PRCS_UNIT      VARCHAR2(1),
     SLIVER_WIDTH          NUMBER,
     SEGMENT_LENGTH        NUMBER,
     SRID                  NUMBER,
     TOLERANCE             NUMBER,
     DROP_TABLES           VARCHAR2(1),
     USER_LAST_MODIFIED    VARCHAR2(32),
     DATE_LAST_MODIFIED    DATE
   );
   TYPE GZ_OUTPUT_PARAMETERS IS TABLE OF GZ_OUTPUT_PARAMETERS_REC;
   ------------------------
   --Matts clipping types--
   ------------------------
   TYPE GEN_CLIP_PARAMETERS_REC IS RECORD (
      release                    VARCHAR2(10),
      gen_project_id             VARCHAR2(4),
      gen_topology_name          VARCHAR2(64),
      edge_input_table           VARCHAR2(64),
      face_input_table           VARCHAR2(64),
      edge_output_table          VARCHAR2(32),
      face_output_table          VARCHAR2(32),
      face_output_measurements   VARCHAR2(4000),
      left_right_attributes      VARCHAR2(4000),
      face_feature_mask_col      VARCHAR2(30),
      clip_edge_mask_col         VARCHAR2(30),
      gen_clip_modules           VARCHAR2(4000),
      gen_clip_tolerance         NUMBER,
      gen_clip_job_srid          NUMBER,
      gen_clip_output_srid       NUMBER,
      gen_clip_snapping_digits   NUMBER,
      user_last_modified         VARCHAR2(32),
      date_last_modified         DATE
   );
   TYPE GEN_CLIP_PARAMETERS IS TABLE OF GEN_CLIP_PARAMETERS_REC;
   --
   TYPE GEN_CLIP_PARAMETERS_PLSINT IS TABLE OF GEN_CLIP_PARAMETERS_REC
      INDEX BY PLS_INTEGER;
   --
   TYPE GEN_CLIP_JOBRUNS_REC IS RECORD (
      gen_job_id                 VARCHAR2(6),
      gen_project_id             VARCHAR2(4),
      gen_topology_name          VARCHAR2(64),
      edge_input_table           VARCHAR2(64),
      face_input_table           VARCHAR2(64),
      edge_output_table          VARCHAR2(32),
      face_output_table          VARCHAR2(32),
      face_output_measurements   VARCHAR2(4000),
      left_right_attributes      VARCHAR2(4000),
      face_feature_mask_col      VARCHAR2(32),
      clip_edge_mask_col         VARCHAR2(32),
      gen_clip_table             VARCHAR2(32),
      gen_clip_modules           VARCHAR2(4000),
      gen_clip_tolerance         NUMBER,
      gen_clip_job_srid          NUMBER,
      gen_clip_output_srid       NUMBER,
      gen_clip_snapping_digits   NUMBER,
      user_last_modified         VARCHAR2(32),
      date_last_modified         DATE
   );
   TYPE GEN_CLIP_JOBRUNS IS TABLE OF GEN_CLIP_JOBRUNS_REC;
   --
   TYPE GEN_CLIP_JOBRUNS_PLSINT IS TABLE OF GEN_CLIP_JOBRUNS_REC
      INDEX BY PLS_INTEGER;
   --
   --Matt uses these in gz_utilities.build_topo_from_spatial
   TYPE SDO_TOPO_GEOMETRY_ARRAY IS TABLE OF SDO_TOPO_GEOMETRY;
   --
   TYPE SDO_TOPO_GEOMETRY_ARRAY_PLSINT IS TABLE OF SDO_TOPO_GEOMETRY
                                       INDEX BY PLS_INTEGER;
   ------------------------
   --  Topo merge types  --
   ------------------------
   TYPE GEN_CUTTER_REC IS RECORD (
      cutter_id                  NUMBER,
      source_topo                VARCHAR2(4000),
      topo_name                  VARCHAR2(4000),  --jinx!
      face_table                 VARCHAR2(4000),
      unaligned_sdo              SDO_GEOMETRY,
      sdogeometry                SDO_GEOMETRY,
      topogeom                   SDO_TOPO_GEOMETRY,
      is_dead_flag               VARCHAR2(1)      --hat tip: NP
   );
   TYPE GEN_CUTTER IS TABLE OF GEN_CUTTER_REC;
   --
   TYPE GEN_CUTTER_PLSINT IS TABLE OF GEN_CUTTER_REC
      INDEX BY PLS_INTEGER;
   --
   TYPE GEN_ALFACE_REC IS RECORD (
      face_id                    VARCHAR2(4000),
      source_topo                VARCHAR2(4000),
      unaligned_sdo              SDO_GEOMETRY,
      sdogeometry                SDO_GEOMETRY
   );
   --
   TYPE GEN_ALFACE IS TABLE OF GEN_ALFACE_REC;
   --
   TYPE GEN_ALFACE_PLSINT IS TABLE OF GEN_ALFACE_REC
      INDEX BY PLS_INTEGER;
   TYPE GEN_TOPO_MERGE_PARAMETERS_REC IS RECORD (
      release                        VARCHAR2(10),
      gen_project_id             VARCHAR2(4),
      gen_merge_face_input_table VARCHAR2(4000),
      gen_merge_face_fields      VARCHAR2(4000),
      gen_merge_tolerance        NUMBER,
      gen_merge_null_tolerance   NUMBER,
      gen_merge_srid             NUMBER,
      gen_merge_snapping_digits  NUMBER,
      gen_merge_error_chasm      NUMBER,
      gen_merge_top_layer        VARCHAR2(4000),
      gen_merge_face_out         VARCHAR2(4000),
      user_last_modified         VARCHAR2(32),
      date_last_modified         DATE
   );
   TYPE GEN_TOPO_MERGE_PARAMETERS IS TABLE OF GEN_TOPO_MERGE_PARAMETERS_REC;
   --
   TYPE GEN_TOPO_MERGE_PARMS_PLSINT IS TABLE OF GEN_TOPO_MERGE_PARAMETERS_REC
      INDEX BY PLS_INTEGER;
   TYPE GEN_TOPO_MERGE_AGGR_REC IS RECORD (
      step                       VARCHAR2(4000),
      sdogeometry                SDO_GEOMETRY
   );
   TYPE GEN_TOPO_MERGE_AGGR IS TABLE OF GEN_TOPO_MERGE_AGGR_REC;
   --
   TYPE GEN_TOPO_MERGE_AGGR_PLSINT IS TABLE OF GEN_TOPO_MERGE_AGGR_REC
      INDEX BY PLS_INTEGER;
   --
   TYPE GEN_TOPO_MERGE_PHONY_FSL_REC IS RECORD (
      sdogeometry                SDO_GEOMETRY,
      topogeom                   SDO_TOPO_GEOMETRY
   );
   TYPE GEN_TOPO_MERGE_PHONY_FSL IS TABLE OF GEN_TOPO_MERGE_PHONY_FSL_REC;

   TYPE GZ_ENTITY_TABLE_REC IS RECORD(
      RELEASE                                            VARCHAR2(10),
      GEN_PROJECT_ID                                     VARCHAR2(4),
      ENTITY_CODE                                        VARCHAR2(4),
      ENTITY                                             VARCHAR2(10),
      ENTITY_TYPE                                        VARCHAR2(10),
      DESCRIPTION                                        VARCHAR2(100),
      USER_LAST_MODIFIED                                 VARCHAR2(32),
      DATE_LAST_MODIFIED                                 DATE
   );
   TYPE GZ_ENTITY_TABLE IS TABLE OF GZ_ENTITY_TABLE_REC;

   --
   TYPE GZ_JOB_SETUP_REC IS RECORD(
      JOBID            VARCHAR2(20),
      DESCR            VARCHAR2(200),
      RELEASE          VARCHAR2(10),
      GEN_PROJECT_ID       VARCHAR2(4),
      ENTITY           VARCHAR2(10),
      STEP             INTEGER,
      DEPENDENCY       INTEGER,
      USE_FOR_MERGE    VARCHAR2(1),
      SRC_TYPE         VARCHAR2(3),
      PRD_TYPE         VARCHAR2(3),
      TOPO_TAG         VARCHAR2(20),
      STATE_TABLE    VARCHAR2(30),
      GEN_CLIP_TABLE VARCHAR2(32),
      TARGET_SCALE   NUMBER,
      PRE        VARCHAR2(1),
      PRE_STATUS VARCHAR2(1),
      TOPOBUILD        VARCHAR2(1),
      TOPOBUILD_STATUS VARCHAR2(1),
      CLIP             VARCHAR2(1),
      CLIP_STATUS      VARCHAR2(1),
      SMPOLY           VARCHAR2(1),
      SMPOLY_STATUS    VARCHAR2(1),
      LINESIM          VARCHAR2(1),
      LINESIM_STATUS   VARCHAR2(1),
      MERGE            VARCHAR2(1),
      MERGE_STATUS     VARCHAR2(1),
      FSLBUILD         VARCHAR2(1),
      FSLBUILD_STATUS  VARCHAR2(1),
      CLIP_ATTR               VARCHAR2(1),
      CLIP_ATTR_STATUS        VARCHAR2(1),
      QA               VARCHAR2(1),
      QA_STATUS        VARCHAR2(1),
      SHAPEFILE        VARCHAR2(1),
      SHAPEFILE_STATUS VARCHAR2(1),
      CURR_MOD         VARCHAR2(20),
      RUN_STATUS       VARCHAR2(1),
      START_TIME       DATE,
      END_TIME         DATE,
      COMMENTS         VARCHAR2(4000),
      USER_LAST_MODIFIED    VARCHAR2(32),
      DATE_LAST_MODIFIED    DATE
   );
   TYPE GZ_JOB_SETUP IS TABLE OF GZ_JOB_SETUP_REC;

   TYPE GZ_PRE_SETUP_REC IS RECORD(
      JOBID             VARCHAR2(20),
      STATEMENT         CLOB,
      STEP              NUMBER,
      STATUS            VARCHAR2(32),
      START_TIME        DATE,
      END_TIME          DATE,
      COMMENTS          VARCHAR2(4000),
      USER_LAST_MODIFIED     VARCHAR2(20),
      DATE_LAST_MODIFIED     DATE
   );
   TYPE GZ_PRE_SETUP IS TABLE OF GZ_PRE_SETUP_REC;

   TYPE GZ_TOPOBUILD_SETUP_REC IS RECORD(
     JOBID             VARCHAR2(20),
     UNIVERSE_TABLE    VARCHAR2(32),
     HIERARCHY_TABLE   VARCHAR2(32),
     TOPO_OUT          VARCHAR2(20),
     STATEFP           VARCHAR2(2),
     FEATURE_MBR_ONLY  VARCHAR2(20),
     TOPO_VALIDATION    VARCHAR2(1),
     BUILD_OPTION      INTEGER,
     CLEANUP           VARCHAR2(1),
     STATUS            VARCHAR2(32),
     START_TIME       DATE,
     END_TIME         DATE,
     COMMENTS          VARCHAR2(4000),
     USER_LAST_MODIFIED    VARCHAR2(32),
     DATE_LAST_MODIFIED    DATE
   );
   TYPE GZ_TOPOBUILD_SETUP IS TABLE OF GZ_TOPOBUILD_SETUP_REC;


TYPE GZ_BUILD_SOURCE_SETUP_REC IS RECORD(
     JOBID              VARCHAR2(20),
     SOURCE_SCHEMA      VARCHAR2(30),
     SOURCE_TOPOLOGY    VARCHAR2(32),
     OUTPUT_TOPOLOGY    VARCHAR2(32),
     TILE_COUNT         NUMBER,
     SDO_FILTER         SDO_GEOMETRY ,
     MODULES            VARCHAR(20),
     RESTART_FLAG       VARCHAR2(1),
     SRID               NUMBER,
     TOLERANCE          NUMBER,
     SNAPPING_DIGITS    NUMBER,
     VALIDATE_TOPO      VARCHAR2(1),
     TOPOFIX_EDGE       VARCHAR2(1),
     TOPOFIX_2EDGE      VARCHAR2(1),
     TOPOFIX_QA         VARCHAR2(1),
     DROP_TABLES        VARCHAR2(1),
     STATUS             VARCHAR2(32),
     START_TIME         DATE,
     END_TIME           DATE,
     COMMENTS           VARCHAR2(4000),
     USER_LAST_MODIFIED    VARCHAR2(32),
     DATE_LAST_MODIFIED    DATE
   );

TYPE GZ_BUILD_SOURCE_SETUP IS TABLE OF GZ_BUILD_SOURCE_SETUP_REC;

   TYPE GZ_OUTPUT_SETUP_REC IS RECORD(
     JOBID                 VARCHAR2(20),
     SOURCE_SCHEMA         VARCHAR2(30),
     SOURCE_TOPOLOGY       VARCHAR2(32),
     OUTPUT_TOPOLOGY       VARCHAR2(32),
     MODULES               VARCHAR(20),
     RESTART_FLAG          VARCHAR2(1),
     SINGLE_LAYER          VARCHAR2(10),
     TILE_COUNT            NUMBER,
     PRCS_SLIVERS          VARCHAR2(1),
     SLIVER_RESTART_FLAG   VARCHAR2(1),
     SLIVER_WIDTH          NUMBER,
     SEGMENT_LENGTH        NUMBER,
     EXPENDABLE_REVIEW     VARCHAR2(4),
     RESHAPE_REVIEW        VARCHAR2(4),
     SRID                  NUMBER,
     TOLERANCE             NUMBER,
     VALIDATE_TOPO         VARCHAR2(1),
     TOPOFIX_EDGE          VARCHAR2(1),
     TOPOFIX_2EDGE         VARCHAR2(1),
     TOPOFIX_QA            VARCHAR2(1),
     DROP_TABLES           VARCHAR2(1),
     STATUS                VARCHAR2(32),
     START_TIME            DATE,
     END_TIME              DATE,
     COMMENTS              VARCHAR2(4000),
     USER_LAST_MODIFIED    VARCHAR2(32),
     DATE_LAST_MODIFIED    DATE
   );
   TYPE  GZ_OUTPUT_SETUP IS TABLE OF  GZ_OUTPUT_SETUP_REC;

   TYPE GZ_CLIP_SETUP_REC IS RECORD(
      JOBID                VARCHAR2(20),
      GEN_CLIP_MASK        VARCHAR2(32),
      CL_JOBID             VARCHAR2(10),
      TOPO_IN              VARCHAR2(20),
      TOPO_OUT             VARCHAR2(20),
      GEN_CLIP_MODULES     VARCHAR2(20),
      DROP_TABLES          VARCHAR2(1),
      TRANSFER_ATTS        VARCHAR2(1),
      VALIDATE_TOPO        VARCHAR2(1),
      TOPOFIX_EDGE         VARCHAR2(1),
      TOPOFIX_2EDGE        VARCHAR2(1),
      TOPOFIX_QA           VARCHAR2(1),
      STATUS               VARCHAR2(32),
      START_TIME           DATE,
      END_TIME             DATE,
      COMMENTS             VARCHAR2(4000),
      USER_LAST_MODIFIED   VARCHAR2(32),
      DATE_LAST_MODIFIED   DATE
   );
   TYPE GZ_CLIP_SETUP IS TABLE OF GZ_CLIP_SETUP_REC;

   TYPE GZ_SMPOLY_SETUP_REC IS RECORD(
      JOBID             VARCHAR2(20),
      SP_MODULES        VARCHAR2(32),
      TOPO_IN           VARCHAR2(20),
      TOPO_OUT          VARCHAR2(20),
      STATEFP           VARCHAR2(2),
      GEOID_COL         VARCHAR2(30),
      RUNID             VARCHAR2(5),
      CLEANUP           VARCHAR2(1),
      VALIDATE_TOPO     VARCHAR2(1),
      TOPOFIX_EDGE      VARCHAR2(1),
      TOPOFIX_2EDGE     VARCHAR2(1),
      TOPOFIX_QA        VARCHAR2(1),
      STATUS            VARCHAR2(32),
      START_TIME        DATE,
      END_TIME          DATE,
      COMMENTS          VARCHAR2(4000),
      USER_LAST_MODIFIED    VARCHAR2(32),
      DATE_LAST_MODIFIED    DATE
   );
   TYPE GZ_SMPOLY_SETUP IS TABLE OF GZ_SMPOLY_SETUP_REC;

   TYPE GZ_LINESIM_SETUP_REC IS RECORD(
      JOBID                VARCHAR2(32),
      LS_MODULES           VARCHAR2(32),
      TOPO_IN              VARCHAR2(20),
      TOPO_OUT             VARCHAR2(20),
      STATEFP              VARCHAR2(2),
      SKIP_EDGES_TABLE     VARCHAR2(4000),
      VALIDATE_TOPO        VARCHAR2(1),
      TOPOFIX_EDGE         VARCHAR2(1),
      TOPOFIX_2EDGE        VARCHAR2(1),
      TOPOFIX_QA           VARCHAR2(1),
      STATUS               VARCHAR2(32),
      START_TIME           DATE,
      END_TIME             DATE,
      COMMENTS             VARCHAR2(4000),
      USER_LAST_MODIFIED   VARCHAR2(32),
      DATE_LAST_MODIFIED   DATE
   );
   TYPE GZ_LINESIM_SETUP IS TABLE OF GZ_LINESIM_SETUP_REC;

   TYPE GZ_MERGE_SETUP_REC IS RECORD(
      JOBID             VARCHAR2(32),
      TOPO_IN_TBL       VARCHAR2(30),
      TOPO_OUT          VARCHAR2(20),
      TOP_LAYER_TABLE   VARCHAR2(30),
      FACE_OUT_TBL_EXT  VARCHAR2(20),
      MRG_MODULES       VARCHAR2(10),
      RESTART           VARCHAR2(1),
      VALIDATE_TOPO     VARCHAR2(1),
      TOPOFIX_EDGE      VARCHAR2(1),
      TOPOFIX_2EDGE     VARCHAR2(1),
      TOPOFIX_QA        VARCHAR2(1),
      STATUS            VARCHAR2(32),
      START_TIME        DATE,
      END_TIME          DATE,
      COMMENTS          VARCHAR2(4000),
      USER_LAST_MODIFIED    VARCHAR2(32),
      DATE_LAST_MODIFIED    DATE
   );
   TYPE GZ_MERGE_SETUP IS TABLE OF GZ_MERGE_SETUP_REC;

   TYPE GZ_FSLBUILD_SETUP_REC IS RECORD(
      JOBID             VARCHAR2(32),
      TOPO_IN           VARCHAR2(20),
      UNGEN_TOPO   VARCHAR2(20),
      FACE_TABLE_EXT    VARCHAR2(30),
      UNIVERSE_TABLE    VARCHAR2(30),
      HIERARCHY_TABLE   VARCHAR2(30),
      PROJECT_Z9        VARCHAR2(1),
      RELEASE           VARCHAR2(20),
      STATE_EDGES_TABLE VARCHAR2(30),
      DROP_TABLES       VARCHAR2(1),
      STATUS            VARCHAR2(32),
      START_TIME       DATE,
      END_TIME         DATE,
      COMMENTS          VARCHAR2(4000),
      USER_LAST_MODIFIED    VARCHAR2(32),
      DATE_LAST_MODIFIED    DATE
   );
   TYPE GZ_FSLBUILD_SETUP IS TABLE OF GZ_FSLBUILD_SETUP_REC;

   TYPE GZ_QA_SETUP_REC IS RECORD(
      JOBID             VARCHAR2(32),
      GEN_TOPO          VARCHAR2(20),
      GEN_SDOGEOM_COL   VARCHAR2(30),
      UNGEN_SCHEMA      VARCHAR2(30),
      UNGEN_TOPO        VARCHAR2(20),
      UNGEN_SDOGEOM_COL VARCHAR2(30),
      OUT_TBL_SUFFIX    VARCHAR2(30),
      TARGET_SCALE      NUMBER,
      COMPARE_GEN_COLUMN VARCHAR2(20),
      AREA_CHECK NUMBER,
      SHAPE_CHECK NUMBER,
      ENTIRE_TOPO       VARCHAR2(1),
      GEN_TBL_NAME      VARCHAR2(30),
      UNGEN_TBL_NAME    VARCHAR2(30),
      MISSING_REC_TBL VARCHAR2(30),
      MISSING_REC_KEY VARCHAR2(30),
      MISSING_REC_FEAT_TBL VARCHAR2(30),
      STATUS            VARCHAR2(32),
      START_TIME       DATE,
      END_TIME         DATE,
      COMMENTS          VARCHAR2(4000),
      USER_LAST_MODIFIED    VARCHAR2(32),
      DATE_LAST_MODIFIED    DATE
   );
   TYPE GZ_QA_SETUP IS TABLE OF GZ_QA_SETUP_REC;

   TYPE GZ_SHAPEFILE_SETUP_REC IS RECORD(
      JOBID            VARCHAR2(32),
      UNGEN_TOPO_NAME  VARCHAR2(20),
      TOPO_NAME        VARCHAR2(20),
      QA_STRING        VARCHAR2(10),
      FSL_STRING       VARCHAR2(10),
      YEAR             VARCHAR2(20),
      GEN_LEVEL        VARCHAR2(4),
      GENCODE          VARCHAR2(1),
      PROJECTION       VARCHAR2(50),
      QA_UNGEN_OUT_DIR VARCHAR2(200),
      QA_OUT_DIR       VARCHAR2(200),
      STATE_TOPO_FLAG  VARCHAR2(1),
      DEL_CPG_FLAG     VARCHAR2(3),
      SHP_UNIT_REQUEST VARCHAR2(100),
      STATUS           VARCHAR2(32),
      START_TIME       DATE,
      END_TIME         DATE,
      COMMENTS         VARCHAR2(4000),
      USER_LAST_MODIFIED    VARCHAR2(32),
      DATE_LAST_MODIFIED    DATE
   );
   TYPE GZ_SHAPEFILE_SETUP IS TABLE OF GZ_SHAPEFILE_SETUP_REC;

   TYPE GZ_SHP_METADATA_REC IS RECORD(
      JOBID                    VARCHAR2(20),
      SHP_NAME              VARCHAR2(1000),
      SHP_TYPE              VARCHAR2(5),
      SHP_CLASS             VARCHAR2(2),
      SOURCE                VARCHAR2(20),
      SUM_LEV                VARCHAR2(3),
      PROJECTION            VARCHAR2(50),
      UNIT                  VARCHAR2(6),
      MODTAG                VARCHAR2(5),
      RESOLUTION            VARCHAR2(2),
      DEL_FLAG              VARCHAR2(1),
      WAVE                  NUMBER,
      OUTPUT_DIR            VARCHAR2(1000),
      LOGFILE               VARCHAR2(1000),
      OGC_VALID             VARCHAR2(1000),
      SHP_SIZE_BYTES        NUMBER,
      SHP_CREATION_DATE        DATE,
      MISSING_GEOID_LIST    VARCHAR2(4000),
      FME_RESULT            VARCHAR2(1000),
      FEATURE_COUNT         NUMBER,
      FME_VERSION           VARCHAR2(100),
      QA_STATUS                VARCHAR2(14),
      QA_DATE                DATE,
      NOTES                    VARCHAR2(4000),
      USER_LAST_MODIFIED    VARCHAR2(32),
      DATE_LAST_MODIFIED    DATE,
      RELEASE_VERSION        VARCHAR2(100)
   );
   TYPE GZ_SHP_METADATA IS TABLE OF GZ_SHP_METADATA_REC;

   TYPE GZ_MISSING_GEOIDS_REC IS RECORD(
      TOPOLOGY1             VARCHAR2(20),
      TOPOLOGY2        VARCHAR2(20),
      FEATURE_TABLE          VARCHAR2(30),
      MISSING_FROM_1   VARCHAR2(60),
      MISSING_FROM_2      VARCHAR2(60)
   );
   TYPE GZ_MISSING_GEOIDS IS TABLE OF GZ_MISSING_GEOIDS_REC;
   --
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   --Reference table types

   TYPE REFERENCE_SCHEMAS_REC IS RECORD(
      schema_name             VARCHAR2(32),
      grants                  VARCHAR2(100),
      user_last_modified      VARCHAR2(32),
      date_last_modified      DATE
   );
   TYPE REFERENCE_SCHEMAS IS TABLE OF REFERENCE_SCHEMAS_REC;

   TYPE REFERENCE_FACE_FIELDS_REC IS RECORD(
      field                   VARCHAR2(100),
      field_type              VARCHAR2(32),
      field_length            NUMBER(22),
      bench_table             VARCHAR2(32),
      field_use               VARCHAR2(100),
      notes                   VARCHAR2(4000),
      field_order             NUMBER(22),
      gen_project_id          VARCHAR2(4),
      release                 VARCHAR2(10),
      user_last_modified      VARCHAR2(32),
      date_last_modified      DATE
   );
   TYPE REFERENCE_FACE_FIELDS IS TABLE OF REFERENCE_FACE_FIELDS_REC;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   --misc table types

   TYPE STATE_EDGES_REC IS RECORD(
      stedge_id               NUMBER,
      src                     VARCHAR2(32),
      type                    VARCHAR2(32),
      r_statefp               VARCHAR2(32),
      l_statefp               VARCHAR2(32),
      sdogeometry             SDO_GEOMETRY
   );
   TYPE STATE_EDGES IS TABLE OF STATE_EDGES_REC;

   --topofix types

   TYPE TOPOFIX_CONSTELLATION IS RECORD (
      fix_class               NUMBER,
      fix_constellation       VARCHAR2(32),
      edge_id                 NUMBER,
      edge_start_node_id      NUMBER,
      edge_end_node_id        NUMBER,
      edge_vertex_count       NUMBER,
      edge_nodes_univ_count   NUMBER,
      shorty_index            NUMBER,
      edge_universal          VARCHAR2(1),
      segment_obsolete_nodes  NUMBER
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   --OUTPUT module types

   TYPE GZ_LAYERS_OUT_REC IS RECORD (
      release                    VARCHAR2(64),
      gen_project_id             VARCHAR2(4),
      layer                      VARCHAR2(30),
      description                VARCHAR2(4000),
      layer_type                 VARCHAR2(64),
      add_to_face                VARCHAR2(30), --fill in layer value?
      sliver_extinct_clause      VARCHAR2(4000),
      sliver_exempt              VARCHAR2(1),
      notes                      VARCHAR2(4000),
      user_last_modified         VARCHAR2(32),
      date_last_modified         DATE
   );
   TYPE GZ_LAYERS_OUT IS TABLE OF GZ_LAYERS_OUT_REC;

   TYPE GZ_LAYERS_AGGREGATE_REC IS RECORD (
      release                 VARCHAR2(64),
      layer                   VARCHAR2(30),
      source                  VARCHAR2(4000),
      where_clause            VARCHAR2(4000),
      notes                   VARCHAR2(4000),
      user_last_modified      VARCHAR2(32),
      date_last_modified      DATE
   );
   TYPE GZ_LAYERS_AGGREGATE IS TABLE OF GZ_LAYERS_AGGREGATE_REC;

   TYPE GZ_LAYERS_SPLIT_REC IS RECORD (
      release                 VARCHAR2(64),
      layer                   VARCHAR2(30),
      base_layer              VARCHAR2(30),
      superior_layer          VARCHAR2(30),
      create_remainders       VARCHAR2(1),
      remainder_code          VARCHAR2(4000),
      remainder_name_prefix   VARCHAR2(4000),
      remainder_code_field    VARCHAR2(30),
      notes                   VARCHAR2(4000),
      user_last_modified      VARCHAR2(32),
      date_last_modified      DATE
   );
   TYPE GZ_LAYERS_SPLIT IS TABLE OF GZ_LAYERS_SPLIT_REC;

   TYPE GZ_LAYERS_HIERARCHICAL_REC IS RECORD (
      release                 VARCHAR2(64),
      gen_project_id          VARCHAR2(4),
      layer                   VARCHAR2(30),
      source                  VARCHAR2(4000),  --bigger since multiples
      where_clause            VARCHAR2(4000),
      source_key              VARCHAR2(30),
      source_nesting_field    VARCHAR2(256),
      nesting_layer           VARCHAR2(30),
      nesting_layer_field     VARCHAR2(256),
      notes                   VARCHAR2(4000),
      user_last_modified      VARCHAR2(32),
      date_last_modified      DATE
   );
   TYPE GZ_LAYERS_HIERARCHICAL IS TABLE OF GZ_LAYERS_HIERARCHICAL_REC;


   TYPE GZ_LAYERS_SUBSET_REC IS RECORD (
      release                 VARCHAR2(64),
      gen_project_id          VARCHAR2(4),
      layer                   VARCHAR2(30),
      source                  VARCHAR2(30),
      where_clause            VARCHAR2(4000),
      oid_clob                CLOB,
      notes                   VARCHAR2(4000),
      user_last_modified      VARCHAR2(32),
      date_last_modified      DATE
   );
   TYPE GZ_LAYERS_SUBSET IS TABLE OF GZ_LAYERS_SUBSET_REC;

   TYPE GZ_LAYERS_FIELDS_REC IS RECORD (
      release                 VARCHAR2(64),
      layer                   VARCHAR2(30),
      fields                  VARCHAR2(4000),
      update_fields           VARCHAR2(4000),
      update_fields_delimiter VARCHAR2(1),
      notes                   VARCHAR2(4000),
      user_last_modified      VARCHAR2(32),
      date_last_modified      DATE
   );
   TYPE GZ_LAYERS_FIELDS IS TABLE OF GZ_LAYERS_FIELDS_REC;

   TYPE GZ_LAYERS_CROSSWALK_REC IS RECORD (
      release                 VARCHAR2(64),
      output_field            VARCHAR2(30),
      output_length           NUMBER,
      bench_field             VARCHAR2(30),
      notes                   VARCHAR2(4000),
      user_last_modified      VARCHAR2(32),
      date_last_modified      DATE
   );
   TYPE GZ_LAYERS_CROSSWALK IS TABLE OF GZ_LAYERS_CROSSWALK_REC;

   TYPE GZ_LAYERS_GEOID_REC IS RECORD (
      release                 VARCHAR2(64),
      sum_lev                 VARCHAR2(30),  --same as layer? so 30
      gv                      VARCHAR2(2),
      gc                      VARCHAR2(2),
      separator               VARCHAR2(2),
      geocodes                VARCHAR2(4000),
      notes                   VARCHAR2(4000),
      user_last_modified      VARCHAR2(32),
      date_last_modified      DATE
   );
   TYPE GZ_LAYERS_GEOID IS TABLE OF GZ_LAYERS_GEOID_REC;

 TYPE GZ_LAYERS_OUT_INFO_REC IS RECORD (
      release                 VARCHAR2(64),
      gen_project_id          VARCHAR2(4),
      layer                   VARCHAR2(30),
      description             VARCHAR2(4000),
      layer_type              VARCHAR2(64),
      add_to_face             VARCHAR2(30),
      layer_sequence          NUMBER,
      layer_level             NUMBER,
      module_status           NUMBER
   );
   TYPE GZ_LAYERS_OUT_INFO IS TABLE OF GZ_LAYERS_OUT_INFO_REC;

   TYPE GZ_LAYERS_OUT_GEOM_REC IS RECORD (
      oid                     VARCHAR2(4000),
      sdogeometry             SDO_GEOMETRY
   );
   TYPE GZ_LAYERS_OUT_GEOM IS TABLE OF GZ_LAYERS_OUT_GEOM_REC;

   TYPE GZ_LAYERS_OUT_HELP_REC IS RECORD (
      oid_child               VARCHAR2(4000),
      oid_parent              VARCHAR2(4000)
   );
   TYPE GZ_LAYERS_OUT_HELP IS TABLE OF GZ_LAYERS_OUT_HELP_REC;

   TYPE GZ_PROJECTION_WRK2003_REC IS RECORD (
      compid                     NUMBER,
      ID                         VARCHAR2(100),
      geometry                   SDO_GEOMETRY,
      num_pieces                 NUMBER
   );
   TYPE GZ_PROJECTION_WRK2003 IS TABLE OF GZ_PROJECTION_WRK2003_REC;

   TYPE GZ_PROJECTION_WRKAREA_REC IS RECORD (
      compid                     NUMBER,
      area                       NUMBER,
      LLX                        NUMBER,
      LLY                        NUMBER,
      URX                        NUMBER,
      URY                        NUMBER
   );
   TYPE GZ_PROJECTION_WRKAREA IS TABLE OF GZ_PROJECTION_WRKAREA_REC;

   TYPE GZ_PROJECTION_WRKOUT_REC IS RECORD (
      compid                     NUMBER,
      ID                         VARCHAR2(100),
      geometry                   SDO_GEOMETRY
   );
   TYPE GZ_PROJECTION_WRKOUT IS TABLE OF GZ_PROJECTION_WRKOUT_REC;

   TYPE GZ_PROJECTION_OUTPUT_REC IS RECORD (
      compid                     NUMBER,
      Id                         VARCHAR2(100),
      geometry                   SDO_GEOMETRY
   );
   TYPE GZ_PROJECTION_OUTPUT IS TABLE OF GZ_PROJECTION_OUTPUT_REC;

   TYPE GZ_FACE_SLIVERS_REC IS RECORD (
      sliver_id               NUMBER,
      sliver_type             VARCHAR2(32),
      face_id                 NUMBER,
      status                  VARCHAR2(32),
      review_adjudication     VARCHAR2(1),
      fsl_geoids              VARCHAR2(4000),
      geoid_extinction        VARCHAR2(4000),
      sdogeometry             SDO_GEOMETRY,
      partial_sdogeometry     SDO_GEOMETRY
   );
   TYPE GZ_FACE_SLIVERS IS TABLE OF GZ_FACE_SLIVERS_REC;

   TYPE GZ_FACE_MERGE_REC IS RECORD (
      release                 VARCHAR2(64),
      gen_project_id          VARCHAR2(4),
      layer                   VARCHAR2(30),
      sliver_exempt           VARCHAR2(1),
      sliver_extinct_clause   VARCHAR2(4000)
   );
   TYPE GZ_FACE_MERGE IS TABLE OF GZ_FACE_MERGE_REC;

   TYPE GZ_SUPER_DONE_REC IS RECORD(
       EDGE_ID NUMBER,
       STATE VARCHAR2(5));

   TYPE GZ_SUPER_DONE is TABLE OF GZ_SUPER_DONE_REC;

   TYPE GZ_SUPER_STATE_REC IS RECORD(
       EDGE_ID NUMBER,RIGHT_STATE VARCHAR2(100),LEFT_STATE VARCHAR2(100),LOWEST_STATE VARCHAR2(100));

   TYPE GZ_SUPER_STATE is TABLE OF GZ_SUPER_STATE_REC;

   TYPE GZ_SUPER_EDGES_REC IS RECORD(
       EDGE_ID NUMBER,
       START_NODE_ID NUMBER,END_NODE_ID NUMBER,
       LEFT_FACE_ID  NUMBER,RIGHT_FACE_ID NUMBER,
       STATE   VARCHAR2(100),
       UNIQ  NUMBER, MT_LENGTH NUMBER,
       XLL  NUMBER,YLL NUMBER,XUR NUMBER,YUR NUMBER,
       NEARBY_EDGES  MDSYS.SDO_LIST_TYPE,
       VERTICES      NUMBER,
       IGNORE NUMBER,
       GEOMETRY MDSYS.SDO_GEOMETRY, NEW_GEOMETRY MDSYS.SDO_GEOMETRY);

   TYPE GZ_SUPER_EDGES is TABLE OF GZ_SUPER_EDGES_REC;
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   --Procedures and Functions
   FUNCTION NEW_GEN_EXTENDED_TRACKING_LOG
   RETURN GZ_TYPES.GEN_EXTENDED_TRACKING PIPELINED;

   FUNCTION LEGAL_REFERENCE_TABLES
   RETURN GZ_TYPES.stringarray DETERMINISTIC;

   FUNCTION LEGAL_GZ_TABLES
   RETURN GZ_TYPES.stringarray DETERMINISTIC;

   FUNCTION LEGAL_GZ_RELEASE_TABLES
   RETURN GZ_TYPES.stringarray DETERMINISTIC;

   FUNCTION LEGAL_GZ_OTHER_TABLES
   RETURN GZ_TYPES.stringarray DETERMINISTIC;

   FUNCTION LEGAL_GZ_ALL_TABLES
   RETURN GZ_TYPES.stringarray DETERMINISTIC;

   FUNCTION LEGAL_GZ_XTENDED_KEYS
   RETURN GZ_TYPES.stringhash DETERMINISTIC;

END GZ_TYPES;
/
