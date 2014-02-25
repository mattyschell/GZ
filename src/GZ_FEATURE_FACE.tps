CREATE OR REPLACE
TYPE gz_feature_face
AUTHID CURRENT_USER
AS OBJECT (

   --Matt! 7/12/13
   --Attempt to wrap the whole sliver ID and removal process around a face feature object

   topology_name              VARCHAR2(20),
   table_name                 VARCHAR2(32),
   face_id                    NUMBER,
   tolerance                  NUMBER,
   null_tolerance             NUMBER,
   tg_layer_id                NUMBER,
   tg_id                      NUMBER,
   srid                       NUMBER,
   feature_tables             MDSYS.STRINGLIST,
   feature_table_geoids       MDSYS.STRINGLIST,
   boundary_edges             MDSYS.SDO_LIST_TYPE,
   signed_boundary_edges      MDSYS.SDO_LIST_TYPE,
   boundary_faces             MDSYS.SDO_LIST_TYPE,
   coastal                    VARCHAR2(1),
   coastal_edges              MDSYS.SDO_LIST_TYPE,
   sdogeometry                SDO_GEOMETRY,
   coastal_sliver             VARCHAR2(8),  --PARTIAL, SLIVER, NO
   coastal_sliver_clock_d     NUMBER,       --If partial clockwise, how far
   coastal_sliver_c_clock_d   NUMBER,       --If partial counterclockwise, how far
   dbug                       NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   CONSTRUCTOR FUNCTION gz_feature_face
   RETURN SELF AS RESULT,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   CONSTRUCTOR FUNCTION gz_feature_face(
      p_topology_name      IN VARCHAR2,
      p_table_name         IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_dbug               IN NUMBER DEFAULT NULL
   ) RETURN SELF AS RESULT,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_topo_info,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_feature_layers,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_feature_layers(
      p_feature_tables           IN MDSYS.STRINGLIST,
      p_feature_table_geoids     IN MDSYS.STRINGLIST
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_face_id(
      p_face_id                  IN NUMBER
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_feature_tables
   RETURN MDSYS.STRINGLIST,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_feature_table_geoids
   RETURN MDSYS.STRINGLIST,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE match_features(
      p_neighbor_face      IN NUMBER
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE merge_face(
      p_neighbor_face      IN NUMBER
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION valid(
      p_tolerance       IN NUMBER
   ) RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_boundary_edges,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_boundary_edges
   RETURN MDSYS.SDO_LIST_TYPE,
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_boundary_faces,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_boundary_faces
   RETURN MDSYS.SDO_LIST_TYPE,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_signed_boundary_edges
   RETURN MDSYS.SDO_LIST_TYPE,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_coastal_sliver(
      p_sliver_width       IN NUMBER,
      p_partial_length     IN NUMBER
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_coastal_sliver
   RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_coastal_edges,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_coastal_edges
   RETURN MDSYS.SDO_LIST_TYPE,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_signed_interior_edges (
      p_direction             IN VARCHAR2
   ) RETURN MDSYS.SDO_LIST_TYPE,
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_interior_edges (
      p_direction             IN VARCHAR2
   ) RETURN MDSYS.SDO_LIST_TYPE,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_sdogeometry
   RETURN SDO_GEOMETRY,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE update_measurements,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_feature_layers
   RETURN MDSYS.SDO_LIST_TYPE,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE collapse_to_universal,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_edge_geometry(
      p_edge_id            IN NUMBER
   ) RETURN SDO_GEOMETRY,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_node_geometry(
      p_node_id            IN NUMBER
   ) RETURN SDO_GEOMETRY,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_partial_sliver_cutoff(
      p_direction          IN VARCHAR2
   )RETURN SDO_GEOMETRY,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_sliver_opposing_edges(
      p_direction          IN VARCHAR2
   )RETURN MDSYS.SDO_LIST_TYPE,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION measure_edge_to_universal(
      p_edge_id            IN NUMBER,
      p_edge_geometry      IN SDO_GEOMETRY,
      p_cutoff_distance    IN NUMBER
   ) RETURN NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION measure_face_to_universal(
      p_direction          IN VARCHAR2,   --CLOCKWISE or COUNTERCLOCKWISE
      p_cutoff_distance    IN NUMBER
   ) RETURN NUMBER,   --return the length of the segments traversed or -1 if full traversal
   
   MEMBER FUNCTION measure_universal_to_face(
      p_direction          IN VARCHAR2,   --CLOCKWISE or COUNTERCLOCKWISE
      p_cutoff_distance    IN NUMBER
   ) RETURN NUMBER,   --return the length of the segments traversed or -1 if full traversal

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE describe_face_object,

   MEMBER FUNCTION describe_face_object
   RETURN VARCHAR2

);
/