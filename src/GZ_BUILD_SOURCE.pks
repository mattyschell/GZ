CREATE OR REPLACE PACKAGE GZ_BUILD_SOURCE
AUTHID CURRENT_USER
AS

   --Code to create initial GZ topology
   --See GENERALIZATION_TOPO_BUILD For main package entry point




   FUNCTION GET_LAYERS (
      p_release                  IN VARCHAR2,
      p_project_id               IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_IN;

   FUNCTION GET_LAYER_INFO (
      p_topo               IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_IN_INFO;

   FUNCTION GET_EXTENT_LAYER (
      p_topo               IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_IN_INFO_REC;

   FUNCTION PIPE_LAYERS (
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_table       IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_source_key         IN VARCHAR2,
      p_where_clause       IN VARCHAR2,
      p_boundary_def       IN VARCHAR2,
      p_sdo_filter         IN SDO_GEOMETRY DEFAULT NULL,
      p_alt_source_schema  IN VARCHAR2 DEFAULT NULL,
      p_topo_extent_lyr    IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.GZ_LAYERS_IN_INFO PIPELINED;

   FUNCTION CREATE_LAYER_INFO (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_sdo_filter         IN SDO_GEOMETRY DEFAULT NULL
   ) RETURN VARCHAR2;

   FUNCTION PIPE_BUILD_POLY (
      p_layer                 IN VARCHAR2,
      p_source_schema         IN VARCHAR2,
      p_source_table          IN VARCHAR2,
      p_source_topology       IN VARCHAR2,
      p_output_topology       IN VARCHAR2,
      p_source_key            IN VARCHAR2,
      p_where_clause          IN VARCHAR2,
      p_boundary_definition   IN VARCHAR2,
      p_sdo_filter            IN SDO_GEOMETRY DEFAULT NULL,
      p_rigorous_filter       IN PLS_INTEGER DEFAULT 0
   ) RETURN GZ_TYPES.GZ_BUILD_POLY PIPELINED;

   FUNCTION CREATE_BUILD_POLY (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_sdo_filter         IN SDO_GEOMETRY DEFAULT NULL
   ) RETURN VARCHAR2;

   PROCEDURE POPULATE_BUILD_TILE (
      p_topology           IN VARCHAR2,
      p_tiles              IN GZ_TYPES.GEOMARRAY,
      p_status_clue        IN VARCHAR2 DEFAULT '0'
   );

   FUNCTION GET_BUILD_TILE (
      p_topology           IN VARCHAR2,
      p_status             IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.GEOMARRAY;

   FUNCTION IS_TILE_PROCESSED (
      p_topology           IN VARCHAR2,
      p_tile_number        IN VARCHAR2,
      p_status_clue        IN VARCHAR2
   ) RETURN BOOLEAN;

   PROCEDURE MARK_TILE_STATUS (
      p_topology           IN VARCHAR2,
      p_tile               IN NUMBER,
      p_status_clue        IN VARCHAR2
   );

   FUNCTION CREATE_EMPTY_TOPOLOGY (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_srid               IN NUMBER DEFAULT 8265,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_snapping_digits    IN NUMBER DEFAULT 16
   ) RETURN VARCHAR2;

   FUNCTION REMOVE_BADDY_EDGE (
      p_output_topology    IN VARCHAR2,
      p_errm               IN VARCHAR2
   ) RETURN NUMBER;

   PROCEDURE LOAD_TOPO_TILE (
      p_input_schema       IN VARCHAR2,
      p_input_topology     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tile               IN SDO_GEOMETRY,
      p_tile_kount         IN NUMBER,
      p_tile_total         IN NUMBER,
      p_srid               IN NUMBER DEFAULT 8265,
      p_src_srid           IN NUMBER DEFAULT 8265,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_delta              IN NUMBER DEFAULT .05,
      p_mask_override      IN VARCHAR2 DEFAULT NULL
   );

   FUNCTION GET_GENESIS_EDGE_ID (
      p_topology           IN VARCHAR2,
      p_current_edge_geom  IN SDO_GEOMETRY,
      p_dbug               IN NUMBER DEFAULT 0
   ) RETURN NUMBER;

   FUNCTION FIX_MYSTERY_FACE (
      p_topology           IN VARCHAR2,
      p_mystery_face       IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN NUMBER;

   FUNCTION FACE_HAS_KNOWN_EDGES (
      p_topology           IN VARCHAR2,
      p_mystery_face       IN NUMBER,
      p_mystery_edges      IN GZ_TYPES.numberarray
   ) RETURN VARCHAR2;

   FUNCTION LOAD_OUTPUT_TOPOLOGY (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_input_topology     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tile_kount         IN NUMBER,
      p_srid               IN NUMBER DEFAULT 8265,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_restart_flag       IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2;

   PROCEDURE REMOVE_OBSOLETE_NODES_EDGE (
      p_topology           IN VARCHAR2,
      p_sdo_window         IN SDO_GEOMETRY,
      p_tile_number        IN NUMBER,
      p_delta              IN NUMBER DEFAULT .05,
      p_tries              IN NUMBER DEFAULT 2
   );

   PROCEDURE REMOVE_OBSOLETE_NODES (
      p_topology           IN VARCHAR2,
      p_sdo_window         IN SDO_GEOMETRY,
      p_tile_number        IN NUMBER,
      p_delta              IN NUMBER DEFAULT .05
   );

   FUNCTION CLEAN_OUTPUT_TOPOLOGY (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tile_kount         IN NUMBER,
      p_srid               IN NUMBER DEFAULT 8265,
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN VARCHAR2;

   FUNCTION CREATE_FACE_TABLE (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION CONSTRUCT_FACE_TABLE (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2
   ) RETURN VARCHAR2;

   PROCEDURE POPULATE_BUILD_GEOID (
      p_output_topology    IN VARCHAR2,
      p_column_name        IN VARCHAR2 DEFAULT 'GEOID',
      p_delim              IN VARCHAR2 DEFAULT ',',
      p_null_fill          IN VARCHAR2 DEFAULT ' '
   );

    FUNCTION UPDATE_FACE_LAYERS (
      p_schema             IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2
   ) RETURN VARCHAR2;

   FUNCTION POPULATE_MEASUREMENTS (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tolerance          IN NUMBER,
      p_srid               IN NUMBER,
      p_topofix_qa         IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge           IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge          IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2;

   FUNCTION FINALIZE_AND_VALIDATE (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tile_kount         IN NUMBER,
      p_drop_tables        IN VARCHAR2 DEFAULT 'Y',
      p_validate_topo      IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2;

   FUNCTION GENERALIZATION_TOPO_BUILD (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tile_kount         IN NUMBER DEFAULT 1,
      p_sdo_filter         IN SDO_GEOMETRY DEFAULT NULL,
      p_modules            IN VARCHAR2 DEFAULT 'YYYYYYYYYY',
      p_restart_flag       IN VARCHAR2 DEFAULT 'N',
      p_srid               IN NUMBER DEFAULT 8265,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_snapping_digits    IN NUMBER DEFAULT 16,
      p_drop_tables        IN VARCHAR2 DEFAULT 'Y',
      p_validate_topo      IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge           IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge          IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa         IN VARCHAR2 DEFAULT 'Y'      
   ) RETURN VARCHAR2;

   FUNCTION GZ_GET_FACES (
      p_owner           IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_table           IN VARCHAR2,
      p_oid             IN NUMBER,
      p_oid_col         IN VARCHAR2 DEFAULT 'OID',
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.stringarray;

    FUNCTION GZ_GET_FACES_SQL (
      p_owner           IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_table           IN VARCHAR2,
      p_oid             IN NUMBER,
      p_oid_col         IN VARCHAR2 DEFAULT 'OID',
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN VARCHAR2;

    FUNCTION GET_TG_LAYER_LEVEL (
      p_owner                       IN VARCHAR2,
      p_topology                    IN VARCHAR2,
      p_table_name                  IN VARCHAR2,
      p_column_name                 IN VARCHAR2,
      p_tg_layer_type               IN VARCHAR2 --point, line, polygon
   ) RETURN NUMBER;

   FUNCTION GET_BDYEDGES_SQL (
      p_schema            IN VARCHAR2,
      p_feature_tab       IN VARCHAR2,
      p_topo              IN VARCHAR2,
      p_oid_col           IN VARCHAR2 DEFAULT 'OID'
   ) RETURN VARCHAR2;

   FUNCTION GET_FACE_TILES (
      p_topo               IN VARCHAR2
   ) RETURN GZ_TYPES.STRINGARRAY;

   FUNCTION GZ_GET_BDYEDGES (
      p_owner           IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_table           IN VARCHAR2,
      p_oid             IN NUMBER,
      p_oid_col         IN VARCHAR2 DEFAULT 'OID'
   ) RETURN MDSYS.SDO_LIST_TYPE;

   FUNCTION GZ_GET_OID_FROM_FACE (
      p_owner              IN VARCHAR2,
      p_topology           IN VARCHAR2,
      p_table              IN VARCHAR2,
      p_tg_layer_level     IN NUMBER,
      p_face_id            IN NUMBER,
      p_whereclause        IN VARCHAR2,
      p_oid_col            IN VARCHAR2 DEFAULT 'OID'
   ) RETURN VARCHAR2;

   FUNCTION SDO_GET_OID_FROM_FACE (
      p_owner              IN VARCHAR2,
      p_topology           IN VARCHAR2,
      p_table              IN VARCHAR2,
      p_tg_layer_level     IN NUMBER,
      p_face_id            IN NUMBER,
      p_whereclause        IN VARCHAR2,
      p_oid_col            IN VARCHAR2 DEFAULT 'OID',
      p_sdo                IN SDO_GEOMETRY
   ) RETURN VARCHAR2;

   FUNCTION TIDY_TILES (
      p_tiles                IN GZ_TYPES.geomarray
   ) RETURN GZ_TYPES.geomarray;

   FUNCTION GET_GEODETIC_MBR_WIDTH (
     p_mbr              IN SDO_GEOMETRY,
     p_tolerance        IN NUMBER DEFAULT .05
   ) RETURN NUMBER DETERMINISTIC;

   FUNCTION GZ_TILE_TABLE (
      p_table           IN VARCHAR2,
      p_tile_target     IN NUMBER,
      p_geom_col        IN VARCHAR2 DEFAULT 'GEOMETRY',
      p_whereclause     IN VARCHAR2 DEFAULT NULL,
      p_sdo_filter      IN SDO_GEOMETRY DEFAULT NULL,
      p_log_type        IN VARCHAR2 DEFAULT NULL,
      p_log_tag         IN VARCHAR2 DEFAULT NULL,
      p_debug           IN NUMBER DEFAULT NULL
   ) RETURN GZ_TYPES.geomarray;

   FUNCTION ISOLATED_NODES_EXIST (
      p_topo               IN VARCHAR2
   ) RETURN BOOLEAN;

   FUNCTION DANGLE_EDGES_EXIST (
      p_topo               IN VARCHAR2
   ) RETURN BOOLEAN;

   FUNCTION SHIFT_AT_DATELINE (
      p_tile                IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   FUNCTION SPLIT_AT_DATELINE (
      p_tile                IN SDO_GEOMETRY,
      p_hemisphere          IN VARCHAR2 DEFAULT 'WESTERN'
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

   FUNCTION GEODETIC_MBR_CONSIDERATIONS (
      p_tile                IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY;

   FUNCTION AGGR_MBR_NEAR_DATELINE (
      p_tab                IN VARCHAR2,
      p_pkc_col            IN VARCHAR2,
      p_keys               IN GZ_TYPES.stringarray,
      p_geom_col           IN VARCHAR2 DEFAULT 'GEOMETRY',
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY;

   FUNCTION GET_MBR_NEAR_DATELINE (
      p_geom               IN SDO_GEOMETRY,
      p_mbr                IN SDO_GEOMETRY DEFAULT NULL,
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY;



END GZ_BUILD_SOURCE;
/
