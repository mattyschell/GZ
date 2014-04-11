CREATE OR REPLACE PACKAGE GZ_SLIVER
AUTHID CURRENT_USER
AS

   PROCEDURE DBUG_THIS (
      p_dbug               IN VARCHAR2,
      p_sql                IN VARCHAR2
   );

   FUNCTION MEASURE_FACE_TO_UNIVERSAL (
      p_direction          IN VARCHAR2,
      p_cutoff_distance    IN NUMBER,
      p_FeatureFace        IN GzFeatureFace
   ) RETURN NUMBER;

   FUNCTION MEASURE_UNIVERSAL_TO_FACE (
      p_direction          IN VARCHAR2,
      p_cutoff_distance    IN NUMBER,
      p_FeatureFace        IN GzFeatureFace
   ) RETURN NUMBER;

    FUNCTION MEASURE_EDGE_TO_UNIVERSAL (
      p_TopoEdge           IN GzTopoEdge,
      p_cutoff_distance    IN NUMBER,
      p_reverse_flag       IN VARCHAR2
   ) RETURN NUMBER;

    PROCEDURE COASTAL_SLIVER_DETERMINATION (
      p_FeatureFace        IN OUT GzFeatureFace,
      p_cutoff_distance    IN NUMBER,
      p_partial_length     IN NUMBER
   );

   FUNCTION GET_COASTAL_OPPOSING_EDGES (
      p_FeatureFace        IN GzFeatureFace,
      p_direction          IN VARCHAR2,
      p_percent            IN NUMBER DEFAULT 1
   ) RETURN MDSYS.SDO_LIST_TYPE;


   PROCEDURE COLLAPSE_FACE_FROM_EDGES (
      p_FeatureFace        IN OUT GzFeatureFace,
      p_edges              IN MDSYS.SDO_LIST_TYPE,
      p_depth              IN NUMBER DEFAULT 0
   );

   PROCEDURE COLLAPSE_FACE_TO_UNIVERSAL (
      p_FeatureFace        IN OUT GzFeatureFace
   );
   
   PROCEDURE UPDATE_A_FEATURE (
      p_feature_table         IN VARCHAR2,
      p_feature_geoid         IN VARCHAR2,
      p_tracking_hash         IN OUT GZ_TYPES.nestedhash,
      p_featurekey            IN VARCHAR2,
      p_tolerance             IN NUMBER
   );

   PROCEDURE UPDATE_EDITED_MEASUREMENTS (
      p_face_list             IN MDSYS.SDO_LIST_TYPE,
      p_feature_table_list    IN MDSYS.STRINGLIST,
      p_feature_geoid_list    IN MDSYS.STRINGLIST, 
      p_topology              IN GzTopology
   );

   FUNCTION VERIFY_CS_INPUTS (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_topology              IN GzTopology,
      p_sliver_restart_flag   IN VARCHAR2,
      p_sliver_width          IN NUMBER,
      p_segment_length        IN NUMBER,
      p_expendable_review     IN VARCHAR2,
      p_reshape_review        IN VARCHAR2,
      p_update_feature_geom   IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION PROCESS_COASTAL_SLIVERS (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_topo                  IN VARCHAR2,
      p_log_type              IN VARCHAR2,
      p_sliver_width          IN NUMBER,
      p_segment_length        IN NUMBER,
      p_sliver_restart_flag   IN VARCHAR2 DEFAULT 'N',
      p_expendable_review     IN VARCHAR2 DEFAULT 'N',
      p_reshape_review        IN VARCHAR2 DEFAULT 'Y',
      p_update_feature_geom   IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2;




END GZ_SLIVER;

/