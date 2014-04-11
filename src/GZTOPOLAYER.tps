CREATE OR REPLACE
TYPE GzTopoLayer FORCE
AUTHID CURRENT_USER
AS OBJECT
(

   --Matt! 3/11/14
   --see body for sample usages

   tableName               VARCHAR2(20),
   tableKey                VARCHAR2(20),
   columnName              VARCHAR2(20),
   topology                GzTopology,
   tgLayerId               NUMBER,
   tgLayerType             VARCHAR2(10),
   tgLayerLevel            NUMBER,
   dbug                    NUMBER,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzTopoLayer
   RETURN SELF AS RESULT,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzTopoLayer (
      p_table_name         IN VARCHAR2,
      p_table_key          IN VARCHAR2,
      p_column_name        IN VARCHAR2,
      p_GzTopology         IN GzTopology
   ) RETURN SELF AS RESULT,
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetTableName
   RETURN VARCHAR2,  

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetTableKey
   RETURN VARCHAR2,  
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Describe (
      p_indent             IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2,

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE AddFace (
      p_record_key         IN VARCHAR2,
      p_face_id            IN NUMBER
   ),

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE DeleteFace (
      p_record_key         IN VARCHAR2,
      p_face_id            IN NUMBER
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