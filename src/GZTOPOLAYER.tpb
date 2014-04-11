CREATE OR REPLACE
TYPE BODY GzTopoLayer
AS

   --Matt! 3/11/14

   --SAMPLE, common usage

   --   declare
   --      thistopo  GzTopology  := GzTopology();
   --      thislayer GzTopoLayer := GzTopoLayer();
   --   begin
   --      thistopo  := GzTopology('SCHEL010','Z899IN');
   --      thislayer := GzTopoLayer('Z899IN_FSL050V','GEO_ID','TOPOGEOM',thistopo);
   --      thislayer.describe;
   --   end;

   --DBMS_OUTPUT
   --   tableName: Z899IN_FSL050V
   --   tableKey: GEO_ID
   --   columnName: TOPOGEOM
   --   topology:
   --      owner: SCHEL010
   --      name: Z899IN
   --      srid: 8265
   --      id: 17003
   --      tolerance: .05
   --      nullTolerance:
   --      layerCount: 13
   --      featureTables: ( Z899IN_FACE,Z899IN_FSL060V,Z899IN_FSL250V,Z899IN_FSL500V,Z899IN_FSL050V,Z899IN_FSL160V,Z899IN_FSL040V,Z899IN_FSL310V,Z899IN_FSL314V,Z899IN_FSL330V,Z899IN_FSL950V,Z899IN_FSL960V,Z899IN_FSL970V )
   --      dbug:
   --   tgLayerId: 74
   --   tgLayerType: POLYGON
   --   tgLayerLevel: 0
   --   dbug:

   --SAMPLE, delete a face from a FSL

   --   declare
   --      thistopo  GzTopology  := GzTopology();
   --      thislayer GzTopoLayer := GzTopoLayer();
   --   begin
   --      thistopo  := GzTopology('SCHEL010','Z899IN');
   --      thislayer := GzTopoLayer('Z899IN_FSL050V','GEO_ID','TOPOGEOM',thistopo);
   --      thislayer.deleteface('0500000US25025',871);
   --   end;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --Default constructor

   CONSTRUCTOR FUNCTION GzTopoLayer
   RETURN SELF AS RESULT
   AS
   BEGIN

      RETURN;

   END GzTopoLayer;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzTopoLayer(
      p_table_name         IN VARCHAR2,
      p_table_key          IN VARCHAR2,
      p_column_name        IN VARCHAR2,
      p_GzTopology         IN GzTopology
   ) RETURN SELF AS RESULT
   AS

      psql              VARCHAR2(4000);

   BEGIN

      self.tableName :=   UPPER(p_table_name);
      self.tableKey  :=   UPPER(p_table_key);
      self.columnName :=  UPPER(p_column_name);
      self.topology  :=   p_GzTopology;

      psql := 'SELECT a.tg_layer_id, a.tg_layer_type, a.tg_layer_level '
           || 'FROM '
           || 'all_sdo_topo_info a '
           || 'WHERE a.topology = :p1 AND '
           || 'a.owner = :p2 AND '
           || 'a.table_name = :p3 AND '
           || 'a.column_name = :p4 ';

      EXECUTE IMMEDIATE psql INTO self.tgLayerId,
                                  self.tgLayerType,
                                  self.tgLayerLevel USING self.topology.name,
                                                          self.topology.owner,
                                                          self.tableName,
                                                          self.columnName;

      RETURN;

   END GzTopoLayer;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetTableName
   RETURN VARCHAR2
   AS
   
   BEGIN
   
     RETURN self.TableName;   
   
   END GetTableName; 
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetTableKey
   RETURN VARCHAR2
   AS
   
   BEGIN
   
     RETURN self.TableKey;   
   
   END GetTableKey; 

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Describe (
      p_indent             IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      output         VARCHAR2(8000);

   BEGIN

      output := output || p_indent || 'tableName: ' || self.tableName || chr(10);
      output := output || p_indent || 'tableKey: ' || self.tableKey || chr(10);
      output := output || p_indent || 'columnName: ' || self.columnName || chr(10);
      output := output || p_indent || 'topology: ' || chr(10) || self.topology.Describe(p_indent || '   ');
      output := output || p_indent || 'tgLayerId: ' || self.tgLayerId || chr(10);
      output := output || p_indent || 'tgLayerType: ' || self.tgLayerType || chr(10);
      output := output || p_indent || 'tgLayerLevel: ' || self.tgLayerLevel || chr(10);
      output := output || p_indent || 'dbug: ' || self.dbug || chr(10);

      RETURN output;

   END Describe;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE AddFace (
      p_record_key         IN VARCHAR2,
      p_face_id            IN NUMBER
   )
   AS


   BEGIN

      IF self.tgLayerLevel > 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry player, can''t add faces to layers with tg_layer_level greater than 0. '
                                         || self.tableName || ' is at tg_layer_level ' || self.tgLayerLevel || '. '
                                         || 'Instead, add the face to the child layer and the parent will use it automagically.');

      END IF;

      GZ_TOPO_PRIMITIVE.ADD_A_FACE(self.topology.name,
                                   self.tableName,
                                   p_record_key,
                                   p_face_id,
                                   self.tgLayerId,
                                   self.tableKey);

   END AddFace;


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE DeleteFace (
      p_record_key         IN VARCHAR2,
      p_face_id            IN NUMBER
   )
   AS

      record_extinction       NUMBER;


   BEGIN

      IF self.tgLayerLevel > 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry player, can''t delete faces to layers with tg_layer_level greater than 0. '
                                         || self.tableName || ' is at tg_layer_level ' || self.tgLayerLevel || '. '
                                         || 'Instead, delete the face to the child layer and the parent will use it automagically.');

      END IF;

      --dont care
      record_extinction := GZ_TOPO_PRIMITIVE.DELETE_A_FACE(self.topology.name,
                                                           self.tableName,
                                                           p_record_key,
                                                           p_face_id,
                                                           self.tgLayerId,
                                                           self.tableKey,
                                                           self.dbug);

   END DeleteFace;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetDbug
   AS

   BEGIN

      --different levels of dbug.. could get carried away
      self.dbug := 1;

   END SetDbug;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE Describe (
      p_indent          IN VARCHAR2 DEFAULT NULL
   ) AS

   BEGIN

      dbms_output.put_line(Describe(p_indent));

   END Describe;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

END;
/