CREATE OR REPLACE PACKAGE GZ_TOPO_MERGE
AUTHID CURRENT_USER
AS

   --Matt! 5/20/2011
   --Code to merge subnational topologies into a national topo
   --See GENERALIZATION_TOPO_MERGE For main package entry point

   FUNCTION GET_GALACTIC_TOPOS (
      p_topo_out              IN VARCHAR2,
      p_column                IN VARCHAR2 DEFAULT 'TOPO_NAME',
      p_is_dead_flag          IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION GET_SIMPLE_FACE_GEOMETRY (
      p_topo                 IN VARCHAR2,
      p_face_id              IN NUMBER,
      p_tolerance            IN NUMBER DEFAULT .05,
      p_valid_out            IN VARCHAR2 DEFAULT 'Y'
   ) RETURN SDO_GEOMETRY;

   FUNCTION COUNT_TOPO_GAPS (
      p_topo                 IN VARCHAR2,
      p_feature_face_tab     IN VARCHAR2
   ) RETURN NUMBER;

   FUNCTION GET_TOPO_GAPS (
      p_topo                 IN VARCHAR2,
      p_feature_face_tab     IN VARCHAR2
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION COUNT_TOPO_OVERLAPS (
      p_topo                 IN VARCHAR2,
      p_feature_face_tab     IN VARCHAR2
   ) RETURN NUMBER;

   FUNCTION GAP_MANAGER (
      p_topo                 IN VARCHAR2,
      p_feature_face_tab     IN VARCHAR2,
      p_null_tolerance       IN NUMBER,
      p_max_chasm            IN NUMBER,
      p_tolerance            IN NUMBER DEFAULT .05,
      p_log_type             IN VARCHAR2 DEFAULT 'MERGE'
   ) RETURN NUMBER;

   FUNCTION PIPE_UNALIGNED_RECS (
      p_project_id         IN VARCHAR2,
      p_job_id             IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_topo               IN VARCHAR2,
      p_face_tab           IN VARCHAR2,
      p_cutter_tab         IN VARCHAR2,
      p_srid               IN NUMBER
   ) RETURN GZ_TYPES.GEN_ALFACE PIPELINED;


   FUNCTION GENERALIZATION_TOPO_MERGE (
      p_schema             IN VARCHAR2,
      p_gz_jobid           IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id_in      IN VARCHAR2,
      p_topo_in_table      IN VARCHAR2,
      p_topo_out           IN VARCHAR2,
      p_face_out           IN VARCHAR2,
      p_modules            IN VARCHAR2 DEFAULT 'YYYYYYYYYY',
      p_restart_flag       IN VARCHAR2 DEFAULT 'N',
      p_fix_edge           IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2;


   FUNCTION EXTRACT_HOLES (
      geom_in           IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

    FUNCTION MEASURE_SLIVER_WIDTH (
      geom_in           IN SDO_GEOMETRY,
      p_sample_kount    IN PLS_INTEGER DEFAULT 100,
      p_tolerance       IN NUMBER DEFAULT .00000005,
      p_debug           IN NUMBER DEFAULT 0
   ) RETURN NUMBER DETERMINISTIC;

   PROCEDURE GZ_ALIGN_EDGES (
      p_tab_name              IN VARCHAR2,
      p_geom_col_name         IN VARCHAR2,
      p_out_tab_name          IN VARCHAR2,
      p_tolerance             IN NUMBER DEFAULT .0000005,
      p_drop_out_tab          IN VARCHAR2 DEFAULT 'N'
   );

    FUNCTION MAX_GAP_EVALUATION_2 (
      p_topo_out      IN VARCHAR2,
      p_tab           IN VARCHAR2,
      p_step          IN VARCHAR2,
      p_work_tab      IN VARCHAR2,
      p_sdo_col       IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_null_tol      IN NUMBER DEFAULT .0000005,
      p_sample_count  IN NUMBER DEFAULT 100
   ) RETURN NUMBER;

    FUNCTION MAX_GAP_EVALUATION (
      p_topo_out      IN VARCHAR2,
      p_tab           IN VARCHAR2,
      p_step          IN VARCHAR2,
      p_work_tab      IN VARCHAR2,
      p_sdo_col       IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_null_tol      IN NUMBER DEFAULT .0000005,
      p_sample_count  IN NUMBER DEFAULT 100
   ) RETURN NUMBER;

   FUNCTION MAX_OVERLAP_EVALUATION (
      p_topo_out      IN VARCHAR2,
      p_tab           IN VARCHAR2,
      p_step          IN VARCHAR2,
      p_work_tab      IN VARCHAR2,
      p_sdo_col       IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_null_tol      IN NUMBER DEFAULT .0000005,
      p_sample_count  IN NUMBER DEFAULT 100
   ) RETURN NUMBER;

   FUNCTION GZ_AGGR_UNION (
      p_tab_name      IN VARCHAR2,
      p_col_name      IN VARCHAR2,
      p_tolerance     IN NUMBER
   ) RETURN SDO_GEOMETRY;

   FUNCTION GZ_INTERSECTION (
      p_incoming      IN SDO_GEOMETRY,
      p_clipper       IN SDO_GEOMETRY,
      p_tolerance     IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

    FUNCTION GZ_LINE_INTERSECTION (
      p_incoming      IN SDO_GEOMETRY,
      p_clipper       IN SDO_GEOMETRY,
      p_tolerance     IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

    FUNCTION GZ_LINE_SDO (
      p_incoming      IN SDO_GEOMETRY,
      p_clipper       IN SDO_GEOMETRY,
      p_tolerance     IN NUMBER,
      p_operation     IN VARCHAR2
   ) RETURN GZ_TYPES.geomarray DETERMINISTIC;

   FUNCTION GZ_POLY_INTERSECTION (
      p_incoming      IN SDO_GEOMETRY,
      p_clipper       IN SDO_GEOMETRY,
      p_tolerance     IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

    --Object accessor
   FUNCTION GET_MERGE_PARAMETERS (
      p_release                  IN VARCHAR2,
      p_project_id               IN VARCHAR2
   ) RETURN GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;

   ---------------------------------------------
   --- Create empty work tables  --
   ---------------------------------------------
   FUNCTION NEW_GEN_CUTTER
   RETURN GZ_TYPES.GEN_CUTTER PIPELINED;

   FUNCTION NEW_GEN_ALFACE
   RETURN GZ_TYPES.GEN_ALFACE PIPELINED;

   FUNCTION NEW_GEN_AGGR
   RETURN GZ_TYPES.GEN_TOPO_MERGE_AGGR PIPELINED;

   FUNCTION NEW_GEN_MERGE_PARAMETERS
   RETURN GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS PIPELINED;

   FUNCTION NEW_GEN_MERGE_PHONY_FSL
   RETURN GZ_TYPES.GEN_TOPO_MERGE_PHONY_FSL PIPELINED;

   PROCEDURE CREATE_GEN_CUTTER (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE CREATE_GEN_ALFACE (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE CREATE_GEN_MERGE_PHONY_FSL (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE CREATE_MERGE_FACE (
      p_schema         IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE CREATE_GEN_AGGR (
      p_schema         IN VARCHAR2,
      p_table_name     IN VARCHAR2
   );

   PROCEDURE CREATE_GEN_MERGE_PARAMETERS (
      p_schema         IN VARCHAR2 DEFAULT NULL,
      p_table_name     IN VARCHAR2 DEFAULT 'GEN_MERGE_PARAMETERS'
   );

   PROCEDURE ADD_A_POLY_FROM_SPATIAL (
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

    PROCEDURE ADD_POLYS_FROM_SPATIAL (
      p_toponame           IN VARCHAR2,
      p_featuretable       IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_featuretable_id    IN VARCHAR2,
      p_validate_geom      IN VARCHAR2 DEFAULT 'NO',
      p_validate_tol       IN NUMBER DEFAULT .05,
      p_geom_col           IN VARCHAR2 DEFAULT 'SDOGEOMETRY',
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_val         IN VARCHAR2 DEFAULT NULL,
      p_topomap_mbr        IN SDO_GEOMETRY DEFAULT NULL,
      p_mitosis_col        IN VARCHAR2 DEFAULT 'SOURCE_FACE_ID'
   );


END GZ_TOPO_MERGE;
/
