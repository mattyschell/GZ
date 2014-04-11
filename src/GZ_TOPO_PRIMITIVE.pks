CREATE OR REPLACE PACKAGE GZ_TOPO_PRIMITIVE
AUTHID CURRENT_USER
AS

   PROCEDURE DBUG_THIS (
      p_dbug               IN NUMBER,
      p_sql                IN VARCHAR2
   );
   
   --face features and parents...

   PROCEDURE ADD_A_FACE (
      p_topology           IN VARCHAR2,
      p_feature_table      IN VARCHAR2,
      p_key                IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_tg_layer_id        IN NUMBER,                      --new, not in smpoly version
      p_key_col            IN VARCHAR2                    --caller should pass in GEO_ID or FACE_ID usually
   );

    FUNCTION DELETE_A_FACE (
      p_topology           IN VARCHAR2,
      p_feature_table      IN VARCHAR2,
      p_key                IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_tg_layer_id        IN NUMBER,                      --new, not in smpoly version
      p_key_col            IN VARCHAR2,                    --caller should pass in GEO_ID or FACE_ID usually
      p_dbug               IN NUMBER DEFAULT 0
   ) RETURN NUMBER;

   PROCEDURE DELETE_PARENT_TGL_OBJECT (
      p_topology           IN VARCHAR2,
      p_child_table        IN VARCHAR2,
      p_child_key          IN VARCHAR2,
      p_child_key_col      IN VARCHAR2,
      p_depth              IN NUMBER DEFAULT 0,
      p_dbug               IN NUMBER DEFAULT 0
   );

   FUNCTION GET_PARENT_OF_CHILD (
      p_topology           IN VARCHAR2,
      p_child_table        IN VARCHAR2,
      p_child_key_col      IN VARCHAR2,
      p_child_key          IN VARCHAR2,
      p_parent_table       IN VARCHAR2,
      p_parent_key_col     IN VARCHAR2 DEFAULT 'GEO_ID'
   ) RETURN VARCHAR2;
   
   --edges...

   FUNCTION GET_EDGE_MBR (
      p_topo            IN VARCHAR2,
      p_edge            IN NUMBER
   ) RETURN GZ_TYPES.numberarray;

   FUNCTION ADD_NEW_NODE (
      p_topo               IN VARCHAR2,
      p_edge               IN NUMBER,
      p_coord_index        IN NUMBER,
      p_node               IN SDO_GEOMETRY,
      p_is_new_shape_pt    IN VARCHAR2
   ) RETURN NUMBER;
   
   FUNCTION ADD_NEW_EDGE (
      p_topo               IN VARCHAR2,
      p_start_node_id      IN NUMBER,
      p_end_node_id        IN NUMBER,
      p_edge_geom          IN SDO_GEOMETRY DEFAULT NULL
   ) RETURN NUMBER;

   PROCEDURE REMOVE_EDGE (
      p_topo           IN VARCHAR2,
      p_edge_id        IN VARCHAR2
   );

   PROCEDURE UPDATE_MERGED_FACE_FEATURES (
      p_face_tab        IN VARCHAR2,
      p_face_1          IN NUMBER,
      p_face_2          IN NUMBER
   );
   
   --nodes...
   
   FUNCTION GET_NODE_GEOMETRY (
      p_topo            IN VARCHAR2,
      p_node_id         IN VARCHAR2
   ) RETURN SDO_GEOMETRY;
   
   FUNCTION GET_NODE_TIP (
      p_topology           IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_node_id            IN NUMBER
   ) RETURN VARCHAR2;
   
   FUNCTION REMOVE_NODE_KINDLY (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   ) RETURN NUMBER;
   
   PROCEDURE ZAP_NODE (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   );
   
   FUNCTION IS_NODE_UNIVERSAL (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   ) RETURN BOOLEAN;



END GZ_TOPO_PRIMITIVE;

/