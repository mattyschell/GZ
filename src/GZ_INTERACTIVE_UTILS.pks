CREATE OR REPLACE PACKAGE GZ_INTERACTIVE_UTILS
AUTHID CURRENT_USER
AS


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
   
   PROCEDURE REFRESH_REMOTE_PROD_SCHEMA (
      p_prod_schema        IN VARCHAR2,
      p_prod_db            IN VARCHAR2,
      p_prod_pwd           IN VARCHAR2,
      p_likeclause         IN VARCHAR2 DEFAULT 'REF%'
   );
   
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

   FUNCTION FACE_DIFFS_NAMES (
      p_face1                       IN NUMBER,
      p_face2                       IN NUMBER,
      p_face_table_name             IN VARCHAR2,
      p_reference_face_fields_tab   IN VARCHAR2,
      p_release_name                IN VARCHAR2 DEFAULT NULL,
      p_gen_project_id              IN VARCHAR2 DEFAULT NULL
   ) RETURN TY_ATTR_DIFF_TAB PIPELINED;
   
   FUNCTION CONVERT_COL_TO_CLOB (
      p_sql             IN VARCHAR2,
      p_delimiter       IN VARCHAR2 DEFAULT ','
   ) RETURN CLOB;
   
    --Project management tools
   
   FUNCTION GZ_COPIER_CHECKS (
      p_src_schema         IN VARCHAR2,               --always
      p_release            IN VARCHAR2,               --source release
      p_new_release        IN VARCHAR2 DEFAULT NULL,  --release copier always has this, may be same as p_release
      p_project_id         IN VARCHAR2 DEFAULT NULL,  --only project copier has this (always)
      p_new_project_id     IN VARCHAR2 DEFAULT NULL   --only project copier has this (always, may be same as p_project_id)
   ) RETURN VARCHAR2;

   PROCEDURE GZ_RELEASE_COPIER (
      p_src_schema     IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_new_release    IN VARCHAR2 DEFAULT NULL
   );
   
   PROCEDURE GZ_PROJECT_COPIER (
      p_src_schema         IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_new_project_id     IN VARCHAR2 DEFAULT NULL,
      p_nuke_conflicts     IN VARCHAR2 DEFAULT 'N'
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
   
   PROCEDURE CREATE_VERTEX_TABLE (
      pGeom                   IN SDO_GEOMETRY,
      pOutputTable            IN VARCHAR2,
      pDropTable              IN VARCHAR2 DEFAULT 'N'
   );
   
   PROCEDURE NULL_A_JOBID (
      p_jobid                 IN VARCHAR2
   );
   
   PROCEDURE DIFF_GZ_OUTPUT (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_base_schema           IN VARCHAR2,
      p_compare_schema        IN VARCHAR2,
      p_topo                  IN VARCHAR2 DEFAULT 'ALL',
      p_layer_type            IN VARCHAR2 DEFAULT 'ALL',
      p_tracking_table_suffix IN VARCHAR2 DEFAULT '_DIFF',
      p_use_intpoint          IN VARCHAR2 DEFAULT 'N',
      p_tolerance             IN NUMBER DEFAULT .05      
   );
   
   PROCEDURE DIFF_GZ_BUILD (
      p_source_schema         IN VARCHAR2,
      p_source_topo           IN VARCHAR2,
      p_compare_schema        IN VARCHAR2,
      p_compare_topo          IN VARCHAR2,
      p_output_table          IN VARCHAR2 DEFAULT 'BUILDCOMPARE',
      p_tolerance             IN NUMBER DEFAULT .05,  
      p_require_geoid_idx     IN NUMBER DEFAULT 1      
   );

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION CONVERT_DEGREES_TO_METERS (
      p_degrees               IN NUMBER,
      p_latitude              IN NUMBER DEFAULT 45,
      p_round                 IN NUMBER DEFAULT 2
   ) RETURN NUMBER DETERMINISTIC;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION CONVERT_METERS_TO_DEGREES (
      p_meters                IN NUMBER,
      p_latitude              IN NUMBER DEFAULT 45,
      p_round                 IN NUMBER DEFAULT 6
   ) RETURN NUMBER DETERMINISTIC;
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------


END GZ_INTERACTIVE_UTILS;
/
