CREATE OR REPLACE
TYPE GzFeatureFace FORCE
AUTHID CURRENT_USER
AS OBJECT
(

   --Matt!
   --see body for sample usages

   faceId                  NUMBER,
   topoLayer               GzTopoLayer,
   tgId                    NUMBER,
   topology                GzTopology,
   topoEdges               GZ_TOPO_EDGE_LIST,
   featureTables           MDSYS.stringlist,
   featureTableGeoids      MDSYS.stringlist,
   boundaryEdges           MDSYS.sdo_list_type,
   signedBoundaryEdges     MDSYS.sdo_list_type,
   boundaryFaces           MDSYS.sdo_list_type,
   coastal                 VARCHAR2(1),
   coastalEdges            MDSYS.sdo_list_type,
   sdoGeometry             MDSYS.sdo_geometry,
   coastalSliver           VARCHAR2(8),
   coastalSliverClockD     NUMBER,
   coastalSliverCClockD    NUMBER,
   ringCount               NUMBER,
   dbug                    NUMBER,


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzFeatureFace
   RETURN SELF AS RESULT,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzFeatureFace (
      p_face_id               IN NUMBER,
      p_topology              IN GzTopology
   ) RETURN SELF AS RESULT,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzFeatureFace (
      p_face_id               IN NUMBER,
      p_topology              IN GzTopology,
      p_featureTables         IN MDSYS.stringlist,
      p_featureTableGeoids    IN MDSYS.stringlist
   ) RETURN SELF AS RESULT,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFaceId
   RETURN NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFeatureTables
   RETURN MDSYS.STRINGLIST,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFeatureTableGeoids
   RETURN MDSYS.STRINGLIST,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetBoundaryEdges
   RETURN MDSYS.SDO_LIST_TYPE,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetInteriorEdges(
      p_direction          IN VARCHAR2        --CLOCKWISE or COUNTERCLOCKWISE
   ) RETURN MDSYS.SDO_LIST_TYPE,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetSignedInteriorEdges(
      p_direction          IN VARCHAR2        --CLOCKWISE or COUNTERCLOCKWISE
   ) RETURN MDSYS.SDO_LIST_TYPE,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetBoundaryFaces
   RETURN MDSYS.SDO_LIST_TYPE,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetSdogeometry
   RETURN MDSYS.SDO_GEOMETRY,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetCoastalEdges
   RETURN MDSYS.SDO_LIST_TYPE,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetTopoEdge (
      p_edge_id            IN NUMBER
   ) RETURN GzTopoEdge,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetCoastalSliver
   RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetCoastalSliverDistance (
      p_direction       IN VARCHAR2
   ) RETURN NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetPartialSliverCutoff (
      p_direction          IN VARCHAR2
   ) RETURN MDSYS.SDO_GEOMETRY,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Split (
      self                 IN OUT GzFeatureFace,
      p_sdogeometry        IN MDSYS.SDO_GEOMETRY,
      p_depth              IN NUMBER DEFAULT 0
   ) RETURN NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetDbug
   RETURN NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Describe (
      p_indent          IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetBoundaryEdges,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetCoastalEdges,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetFeatureLayers,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetCoastalSliver (
      p_value              IN VARCHAR2,
      p_direction          IN VARCHAR2 DEFAULT NULL,
      p_distance           IN NUMBER DEFAULT NULL
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE DuplicateFaceRecord (
      p_face_id            IN NUMBER
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE MatchFeatures (
      p_neighbor_face      IN NUMBER
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE InitializeFaceKernel (
      p_face_id               IN NUMBER,
      p_topology              IN GzTopology
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SplitFaceObject (
      p_sdogeometry        IN MDSYS.SDO_GEOMETRY,
      left_face            OUT GzFeatureFace,
      right_face           OUT GzFeatureFace
   ),


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