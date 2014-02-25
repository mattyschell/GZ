CREATE OR REPLACE PACKAGE GZ_OUTPUT
AUTHID CURRENT_USER
AS

   --uses the shapes that are output from the initial topology build process
   --and creates topologically integrated layers that will be output as products
   --See GENERALIZATION_OUTPUT for main package entry point


   FUNCTION VERIFY_OUTPUT_PARMS (
      p_release               IN VARCHAR2,
      p_schema                IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2;

   FUNCTION GZ_TOPO_EXISTS (
      p_topo         IN VARCHAR2,
      p_schema       IN VARCHAR2 DEFAULT NULL
   ) RETURN BOOLEAN;

   PROCEDURE UPDATE_TOPOGEOM (
      p_topo                  IN VARCHAR2,
      p_table                 IN VARCHAR2,
      p_table_oid             IN VARCHAR2,
      p_topo_type             IN NUMBER,
      p_tg_layer_id           IN NUMBER,
      p_topo_object_array     IN MDSYS.SDO_TOPO_OBJECT_ARRAY,
      p_tgl_object_array      IN MDSYS.SDO_TGL_OBJECT_ARRAY,
      p_oid_column            IN VARCHAR2 DEFAULT 'OID',
      p_column_name           IN VARCHAR2 DEFAULT 'TOPOGEOM'
   );

   FUNCTION IS_LAYER_THROUGH_MODULE (
      p_topo         IN VARCHAR2,
      p_layer        IN VARCHAR2,
      p_module       IN NUMBER
   ) RETURN BOOLEAN;

   FUNCTION GET_RELEASE_PROJECTS (
      p_release                  IN VARCHAR2
   ) RETURN GZ_TYPES.STRINGARRAY;

   FUNCTION GET_ALL_LAYERS (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2
   ) RETURN GZ_TYPES.STRINGHASH;

   FUNCTION GET_LAYERS_OUT_INFO (
      p_topo                     IN VARCHAR2,
      p_layer_type               IN VARCHAR2 DEFAULT NULL,
      p_layer                    IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.GZ_LAYERS_OUT_INFO;

   FUNCTION GET_LAYER_OUT_INFO (
      p_topo                     IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_OUT_INFO_REC;

   FUNCTION GET_FACE_TABLE (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_topology                 IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION GET_LAYER_IN (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_IN_REC;

   FUNCTION GET_LAYER_TYPE (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION GET_LAYER_TYPE (
      p_topo                     IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION GET_LAYERS_OUT (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer_type               IN VARCHAR2 DEFAULT NULL,
      p_layer                    IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.GZ_LAYERS_OUT;

   FUNCTION GET_HIERARCHICAL_LAYER (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_HIERARCHICAL_REC;

   FUNCTION GET_SUBSET_LAYER (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_SUBSET_REC;

   FUNCTION GET_SPLIT_LAYER (
      p_release                  IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_SPLIT_REC;

   FUNCTION GET_AGGREGATE_LAYER (
      p_release                  IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_AGGREGATE_REC;

   FUNCTION GET_FIELDS (
      p_release                  IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION GET_UPDATE_FIELDS (
      p_release                  IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.stringhash;

   FUNCTION GET_CROSSWALK_HASH (
      p_release                  IN VARCHAR2,
      p_what                     IN VARCHAR2 DEFAULT 'OUTPUT_LENGTH'
   ) RETURN GZ_TYPES.stringhash;

   FUNCTION GET_MISSING_FIELDS (
      p_layer                    IN VARCHAR2,
      p_source                   IN VARCHAR2,
      p_all_fields               IN GZ_TYPES.stringarray,
      p_crosswalk                IN GZ_TYPES.stringhash
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION GET_LAYER_LEVEL (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_output_topology          IN VARCHAR2,
      p_layer                    IN VARCHAR2,
      p_recursive                IN NUMBER DEFAULT 0
   ) RETURN NUMBER;

   FUNCTION GET_CHILD_LAYER (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_output_topology          IN VARCHAR2,
      p_layer                    IN VARCHAR2,
      p_which                    IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2;

   FUNCTION GET_DEEP_SOURCE (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_output_topology          IN VARCHAR2,
      p_layer                    IN VARCHAR2,
      p_what                     IN VARCHAR2 DEFAULT 'TABLE',
      p_depth                    IN NUMBER DEFAULT 0,
      p_which                    IN NUMBER DEFAULT NULL --1 or 2 for aggregate
   ) RETURN VARCHAR2;

   FUNCTION GET_TOPO_TABLE_HIERARCHY (
      p_topology                 IN VARCHAR2,
      p_table_name               IN VARCHAR2,
      p_column_name              IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION GET_OUTPUT_TILES (
      p_topology                 IN VARCHAR2,
      p_release                  IN VARCHAR2,
      p_project_id               IN VARCHAR2,
      p_tile_kount               IN NUMBER,
      p_face_table               IN VARCHAR2
   ) RETURN GZ_TYPES.geomarray;

   FUNCTION GET_LAYER_SOURCES (
      p_topology                 IN VARCHAR2,
      p_layer                    IN VARCHAR2,
      p_which                    IN VARCHAR2 DEFAULT 'BASE'
   ) RETURN GZ_TYPES.stringhash;

   PROCEDURE SET_LAYER_INFO_MODULE (
      p_topo                     IN VARCHAR2,
      p_module                   IN NUMBER,
      p_layer                    IN VARCHAR2 DEFAULT NULL
   );

   PROCEDURE SET_LAYER_LEVEL (
      p_topo                     IN VARCHAR2,
      p_layer                    IN VARCHAR2,
      p_level                    IN NUMBER
   );

   PROCEDURE ADD_COLUMN_TO_TABLE (
      p_tab                      IN VARCHAR2,
      p_col                      IN VARCHAR2,
      p_type                     IN VARCHAR2 DEFAULT 'VARCHAR2(4000)',
      p_replace                  IN VARCHAR2 DEFAULT 'Y'
   );

   FUNCTION LAYER_IS_INITIAL (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN BOOLEAN;

   FUNCTION LAYER_IS_HIERARCHICAL (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN BOOLEAN;

   FUNCTION LEGAL_LAYER_TYPES
   RETURN GZ_TYPES.stringarray DETERMINISTIC;

   FUNCTION LEGAL_WORK_COLUMNS
   RETURN GZ_TYPES.stringarray DETERMINISTIC;

   FUNCTION STRINGARRAY_SUBTRACT (
      p_input1          GZ_TYPES.stringarray,
      p_input2          GZ_TYPES.stringarray
   ) RETURN GZ_TYPES.stringarray;

   FUNCTION VERIFY_INPUTS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION PIPE_LAYERS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_description        IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_add_to_face        IN VARCHAR2,
      p_sequence           IN NUMBER
   ) RETURN GZ_TYPES.GZ_LAYERS_OUT_INFO PIPELINED;

   FUNCTION SET_UP (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_output_srid        IN NUMBER DEFAULT 8265,
      p_restart_flag       IN VARCHAR2 DEFAULT 'N',
      p_single_layer       IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2;

   PROCEDURE CREATE_LAYER_TABLE (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2
   );

   FUNCTION CREATE_LAYER (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_face_table         IN VARCHAR2
   ) RETURN VARCHAR2;

   PROCEDURE INSERT_SUBSET_OIDS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_face_table         IN VARCHAR2
   );

   PROCEDURE INSERT_HIERARCHICAL_OIDS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_face_table         IN VARCHAR2
   );

   PROCEDURE INSERT_AGGREGATE_OIDS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_face_table         IN VARCHAR2
   );

   PROCEDURE INSERT_SPLIT_OIDS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_face_table         IN VARCHAR2
   );

   PROCEDURE PROCESS_SPLITS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_post_clip_flag     IN VARCHAR2 DEFAULT 'Y'
   );

   PROCEDURE UPDATE_SUBSET_TOPOGEOM (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_face_table         IN VARCHAR2
   );

   FUNCTION REPLACE_A_ALIAS (
      p_string                IN VARCHAR2,
      p_replace               IN VARCHAR2 DEFAULT 'b'
   ) RETURN VARCHAR2;

   PROCEDURE UPDATE_HIERARCHICAL_TOPOGEOM (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_face_table         IN VARCHAR2
   );

   PROCEDURE UPDATE_AGGREGATE_TOPOGEOM (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_face_table         IN VARCHAR2
   );

   PROCEDURE UPDATE_SPLIT_TOPOGEOM (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_face_table         IN VARCHAR2
   );

   FUNCTION BUILD_LAYER_TOPOGEOM (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_face_table         IN VARCHAR2
   ) RETURN VARCHAR2;

   PROCEDURE POPULATE_REMAINDER_ATTRIBUTES (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2
   );

   FUNCTION POPULATE_LAYER_ATTRIBUTES (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_face_table         IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION ADD_STATE_TO_AIA_NAME (
      p_oid                IN NUMBER,
      p_name               IN VARCHAR2,
      p_source_tab         IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION ADD_TO_NAME_BASED_ON_SOURCE (
      p_oid                IN NUMBER,
      p_name               IN VARCHAR2,
      p_source_tab         IN VARCHAR2,
      p_alt_oid            IN NUMBER DEFAULT NULL,
      p_alt_source_tab     IN VARCHAR2 DEFAULT NULL,
      p_text_to_add        IN VARCHAR2 DEFAULT ' (state)',
      p_side_to_add        IN VARCHAR2 DEFAULT 'END',
      p_source_col         IN VARCHAR2 DEFAULT 'aiannhfsr',
      p_add_when_source    IN VARCHAR2 DEFAULT 'S',
      p_source_pkc         IN VARCHAR2 DEFAULT 'oid'
   ) RETURN VARCHAR2;
   
   FUNCTION GET_CDB_LSAD_ABB (
      p_lsad               IN VARCHAR2,
      p_cdb_lsad_table     IN VARCHAR2   --ex acs13cdb.lut_lsad
   ) RETURN VARCHAR2 DETERMINISTIC;

   FUNCTION GET_STANDARD_LSAD_ABB (
      p_lsad               IN VARCHAR2,
      p_release            IN VARCHAR2 DEFAULT NULL,
      p_release_col        IN VARCHAR2 DEFAULT 'RELEASE'
   ) RETURN VARCHAR2 DETERMINISTIC;

   FUNCTION GET_RELATED_VALUE_FROM_LUT (
      p_value              IN VARCHAR2,
      p_lut_table          IN VARCHAR2,
      p_lut_col_we_want    IN VARCHAR2,
      p_lut_col_we_match   IN VARCHAR2,
      p_allow_null         IN VARCHAR2 DEFAULT 'N',
      p_value2             IN VARCHAR2 DEFAULT NULL,
      p_lut_col_we_match2  IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2;

   FUNCTION REPLACE_APOSTROPHES (
      p_string VARCHAR2
   ) RETURN VARCHAR2;
   
   FUNCTION GET_PUBL_NAME (
     pLsad       IN VARCHAR2,
     pBaseName   IN VARCHAR2,
     pLUT_LSAD   IN VARCHAR2 DEFAULT 'LUT_LSAD',
     pType       IN VARCHAR2 DEFAULT 'publ',
     p_release   IN VARCHAR2 DEFAULT NULL,
     p_release_col IN VARCHAR2 DEFAULT 'RELEASE'
   ) RETURN VARCHAR2;

   FUNCTION DROP_POSTAL_ABB (
      p_name               IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION FORCE_SUPERIOR_ATTRIBUTE (
      p_source_superior       IN VARCHAR2,
      p_oid_superior          IN VARCHAR2,
      p_fsl_col               IN VARCHAR2 DEFAULT 'CBSA',
      p_bench_col             IN VARCHAR2 DEFAULT 'CBSAFP',
      p_bench_key             IN VARCHAR2 DEFAULT 'OID'
   ) RETURN VARCHAR2;

   FUNCTION COLUMN_IS_NESTED_ON_SOURCE (
      p_oid                   IN VARCHAR2,
      p_column_value          IN VARCHAR2,
      p_source_table          IN VARCHAR2,
      p_source_column         IN VARCHAR2,
      p_source_pkc_col        IN VARCHAR2 DEFAULT 'OID'
   ) RETURN NUMBER;

   FUNCTION COUSUB_ESTIMATES_UNIV (
      p_oid                          IN VARCHAR2,
      p_source_table            IN VARCHAR2,
      p_source_pkc_col         IN VARCHAR2 DEFAULT 'OID'
   ) RETURN NUMBER;

   PROCEDURE DELETE_SOME_REMAINDERS (
      p_table                 IN VARCHAR2,
      p_base_count            IN NUMBER,
      p_base_count_inequality IN VARCHAR2 DEFAULT '=',
      p_b_oid_column          IN VARCHAR2 DEFAULT 'OID_BASE',
      p_s_oid_column          IN VARCHAR2 DEFAULT 'OID_SUPERIOR'
   );

   PROCEDURE ADD_LAYER_TO_FACE (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_add_to_face        IN VARCHAR2,
      p_face_table         IN VARCHAR2
   );

   PROCEDURE UPDATE_FIELDS (
      p_release            IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2
   );

   PROCEDURE UPDATE_GEO_ID (
      p_release            IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2
   );

   FUNCTION SPECIAL_HANDLING (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_single_layer       IN VARCHAR2,
      p_face_table         IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION POPULATE_MEASUREMENTS (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_output_topology       IN VARCHAR2,
      p_single_layer          IN VARCHAR2,
      p_face_table            IN VARCHAR2,
      p_prcs_slivers          IN VARCHAR2 DEFAULT 'N',
      p_sliver_restart_flag   IN VARCHAR2 DEFAULT 'N',
      p_sliver_width          IN NUMBER DEFAULT NULL,
      p_segment_length        IN NUMBER DEFAULT NULL,
      p_expendable_review     IN VARCHAR2 DEFAULT 'N',
      p_reshape_review        IN VARCHAR2 DEFAULT 'Y',
      p_tolerance             IN NUMBER DEFAULT .05,
      p_srid                  IN NUMBER DEFAULT 8265,
      p_restart_flag          IN VARCHAR2 DEFAULT 'N',
      p_topofix_edge          IN VARCHAR2 DEFAULT 'Y',
      p_topofix_2edge         IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa            IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2;

   PROCEDURE DROP_WORK_COLUMNS (
      p_table              IN VARCHAR2
   );

   FUNCTION PREPARE_FOR_SHAPES (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_single_layer       IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_drop_work_tables   IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION VALIDATE_OUTPUT (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_single_layer       IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_tile_kount         IN NUMBER DEFAULT 10,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_validate_topo      IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2;

   FUNCTION TIDY_EXIT (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_drop_work_tables   IN VARCHAR2,
      p_single_layer       IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_fixes_retval       IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION GENERALIZATION_OUTPUT (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_source_schema         IN VARCHAR2,
      p_source_topology       IN VARCHAR2,
      p_output_topology       IN VARCHAR2,
      p_modules               IN VARCHAR2 DEFAULT 'YYYYYYYYYY',
      p_restart_flag          IN VARCHAR2 DEFAULT 'N',
      p_single_layer          IN VARCHAR2 DEFAULT NULL,
      p_tile_kount            IN NUMBER DEFAULT 10,
      p_prcs_slivers          IN VARCHAR2 DEFAULT 'N',
      p_sliver_restart_flag   IN VARCHAR2 DEFAULT 'N',
      p_sliver_width          IN NUMBER DEFAULT NULL,
      p_segment_length        IN NUMBER DEFAULT NULL,
      p_expendable_review     IN VARCHAR2 DEFAULT 'N',
      p_reshape_review        IN VARCHAR2 DEFAULT 'Y',
      p_srid                  IN NUMBER DEFAULT 8265,
      p_tolerance             IN NUMBER DEFAULT .05,
      p_drop_work_tables      IN VARCHAR2 DEFAULT 'Y',
      p_validate_topo         IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge              IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge             IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa            IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2;


END GZ_OUTPUT;
/
