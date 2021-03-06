CREATE OR REPLACE
TYPE GzTopology FORCE
AUTHID CURRENT_USER
AS OBJECT
(

   --Matt! 2/28/14  Slept
   --see body for sample usages

   owner                    VARCHAR2(20),
   name                     VARCHAR2(20),
   srid                     NUMBER,
   id                       NUMBER,
   tolerance                NUMBER,
   nullTolerance            NUMBER,
   layerCount               NUMBER,
   featureTables            MDSYS.STRINGLIST,
   featureTableKey          VARCHAR2(20),
   faceFeatureTable         VARCHAR2(20),
   dbug                     NUMBER,


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzTopology
   RETURN SELF AS RESULT,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzTopology (
      p_topology_owner        IN VARCHAR2,
      p_topology_name         IN VARCHAR2,
      p_null_tolerance        IN NUMBER DEFAULT 0.00000005
   ) RETURN SELF AS RESULT,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION EdgeTable
   RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION NodeTable
   RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION FaceTable
   RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION RelationTable
   RETURN VARCHAR2,
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetTopoName
   RETURN VARCHAR2,
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetSrid
   RETURN NUMBER,
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetTolerance
   RETURN NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetNullTolerance
   RETURN NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFeatureTables
   RETURN MDSYS.STRINGLIST,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFeatureTableKey
   RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFaceFeatureTable
   RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Valid
   RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Describe (
      p_indent          IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetNullTolerance(
      p_null_tolerance        IN NUMBER
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetFeatureTables,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetDbug,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE Purge,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE Copy(
      p_target_name         IN VARCHAR2
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE AddLayer(
      p_table_name         IN VARCHAR2,
      p_column_name        IN VARCHAR2 DEFAULT 'TOPOGEOM',
      p_layer_type         IN VARCHAR2 DEFAULT 'POLYGON',
      p_child_table        IN NUMBER DEFAULT NULL
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE DeleteLayer(
      p_table_name         IN VARCHAR2,
      p_column_name        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE Describe (
      p_indent          IN VARCHAR2 DEFAULT NULL
   )

);
/