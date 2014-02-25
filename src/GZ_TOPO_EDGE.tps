CREATE OR REPLACE
TYPE gz_topo_edge FORCE
AUTHID CURRENT_USER
AS OBJECT 
(

   --Matt! 9/06/13
   --Instantiate wrt a face
   

   topology_name              VARCHAR2(32),
   face_id                    NUMBER,
   edge_id                    NUMBER,
   face_side                  VARCHAR2(1),  
   start_node_id              NUMBER,
   end_node_id                NUMBER,
   left_face_id               NUMBER,
   right_face_id              NUMBER,
   geometry                   SDO_GEOMETRY,
   tolerance                  NUMBER,
   null_tolerance             NUMBER,
   dbug                       NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   CONSTRUCTOR FUNCTION gz_topo_edge
   RETURN SELF AS RESULT,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   CONSTRUCTOR FUNCTION gz_topo_edge(
      p_topology_name         IN VARCHAR2,
      p_edge_id               IN NUMBER,
      p_face_id               IN NUMBER,
      p_tolerance             IN NUMBER DEFAULT .05,
      p_dbug                  IN NUMBER DEFAULT NULL
   ) RETURN SELF AS RESULT,
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   
   MEMBER FUNCTION add_node(
      p_sdogeometry           IN SDO_GEOMETRY,
      p_must_be_existing      IN VARCHAR2 DEFAULT NULL
   ) RETURN NUMBER,
    
   
   
   --MEMBER PROCEDURE erase
   
   --MEMBER PROCEDURE match_fsls 
   
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE describe_edge_object (
      p_indent IN VARCHAR2 DEFAULT NULL
   ),

   MEMBER FUNCTION describe_edge_object (
      p_indent IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2

);
/