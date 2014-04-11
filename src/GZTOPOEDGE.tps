CREATE OR REPLACE
TYPE GzTopoEdge FORCE
AUTHID CURRENT_USER
AS OBJECT
(

   --Matt! 3/14/14
   --see body for sample usages

   edgeId                  NUMBER,
   dirEdgeId               NUMBER,
   faceId                  NUMBER,
   faceSide                VARCHAR2(1),
   startNodeId             NUMBER,
   endNodeId               NUMBER,
   topology                GzTopology,
   neighborFaceId          NUMBER,
   coastal                 VARCHAR2(1),
   geometry                MDSYS.SDO_GEOMETRY,
   dbug                    NUMBER,


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzTopoEdge
   RETURN SELF AS RESULT,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzTopoEdge (
      p_edge_id            IN NUMBER,
      p_face_id            IN NUMBER,
      p_GzTopology         IN GzTopology
   ) RETURN SELF AS RESULT,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION WithinDistance (
      p_geometry           IN MDSYS.SDO_GEOMETRY,
      p_distance           IN NUMBER
   ) RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetEdgeId
   RETURN NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetCoordIndex (
      p_pt_geometry         IN MDSYS.SDO_GEOMETRY
   ) RETURN PLS_INTEGER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetProjectPt (
      p_pt_geometry         IN MDSYS.SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY,

   ---------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetGeometry
   RETURN MDSYS.SDO_GEOMETRY,

   ---------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetNodeGeometry (
      p_which           IN VARCHAR2
   ) RETURN MDSYS.SDO_GEOMETRY,

   ---------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetNodeId (
      p_which        IN VARCHAR2
   ) RETURN NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Split (
      p_pt_geometry        IN MDSYS.SDO_GEOMETRY
   ) RETURN NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Describe (
      p_indent             IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER Procedure Remove,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetDbug,
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE Describe (
      p_indent          IN VARCHAR2 DEFAULT NULL
   )



);
/