CREATE OR REPLACE
TYPE BODY GzTopology
AS

   --Matt! 3/11/14

   --SAMPLE, common usage
   --
   --      declare
   --         thistopo  GzTopology := GzTopology();
   --      begin
   --         thistopo := GzTopology('SCHEL010','Z899IN');
   --         thistopo.Describe;
   --      end;

   --SAMPLE's DBMS_OUTPUT
   --
   --owner: SCHEL010
   --name: Z899IN
   --srid: 8265
   --id: 17822
   --tolerance: .05
   --nullTolerance:
   --layerCount: 13
   --featureTables: ( Z899IN_FACE,Z899IN_FSL060V,Z899IN_FSL250V,Z899IN_FSL500V,Z899IN_FSL050V,Z899IN_FSL160V,Z899IN_FSL040V,Z899IN_FSL310V,Z899IN_FSL314V,Z899IN_FSL330V,Z899IN_FSL950V,Z899IN_FSL960V,Z899IN_FSL970V )
   --featureTableKey: GEO_ID
   --faceFeatureTable: Z899IN_FACE
   --dbug:

   --OR SAMPLE from SQL prompt
   --   select GzTopology('SCHEL010','Z899IN') from dual

   --OR SAMPLE remote topology
   --   select GzTopology('ACS13','MT') from dual

   --OR SAMPLE in dynamic sql
   --      declare
   --         thistopo   GzTopology := GzTopology();
   --         psql       varchar2(4000);
   --      begin
   --         psql := 'select GzTopology(:p1,:p2) from dual';
   --         execute immediate psql into thistopo using 'SCHEL010', 'Z899IN';
   --         thistopo.Describe;
   --      end;


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --Default constructor

   CONSTRUCTOR FUNCTION GzTopology
   RETURN SELF AS RESULT
   AS
   BEGIN

      RETURN;

   END GzTopology;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzTopology(
      p_topology_owner        IN VARCHAR2,
      p_topology_name         IN VARCHAR2,
      p_null_tolerance        IN NUMBER DEFAULT 0.00000005
   ) RETURN SELF AS RESULT
   AS

      psql              VARCHAR2(4000);

   BEGIN

      self.owner := UPPER(p_topology_owner);
      self.name  := UPPER(p_topology_name);
      self.SetNullTolerance(p_null_tolerance);

      psql := 'SELECT a.table_name '
           || 'FROM '
           || 'all_sdo_topo_info a '
           || 'WHERE a.topology = :p1 AND '
           || 'a.owner = :p2 '
           || 'ORDER BY a.tg_layer_id ';  --fix this order

      EXECUTE IMMEDIATE psql BULK COLLECT INTO self.featureTables USING self.name,
                                                                        self.owner;

      self.featureTableKey := GetFeatureTableKey;

      IF self.featureTables.COUNT = 0
      THEN

         --Need to decide the dealio here. Ever allow work on a topology with no feature tables?
         RETURN;

      ELSE

         self.layerCount := self.featureTables.COUNT;

      END IF;

      psql := 'SELECT a.topology_id, a.tolerance, a.srid '
           || 'FROM all_sdo_topo_info a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.owner = :p2 AND '
           || 'a.table_name = :p3 AND '
           || 'rownum = 1 ';  --in case of boundaryedges or something

      EXECUTE IMMEDIATE psql INTO self.id,
                                  self.tolerance,
                                  self.srid USING self.name,
                                                  self.owner,
                                                  self.featureTables(1);

      self.faceFeatureTable := GetFaceFeatureTable;

      RETURN;

   END GzTopology;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION EdgeTable
   RETURN VARCHAR2
   AS

   BEGIN

      RETURN self.name || '_EDGE$';

   END EdgeTable;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION NodeTable
   RETURN VARCHAR2
   AS

   BEGIN

      RETURN self.name || '_NODE$';

   END NodeTable;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION FaceTable
   RETURN VARCHAR2
   AS

   BEGIN

      RETURN self.name || '_FACE$';

   END FaceTable;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION RelationTable
   RETURN VARCHAR2
   AS

   BEGIN

      RETURN self.name || '_RELATION$';

   END RelationTable;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetTopoName
   RETURN VARCHAR2
   AS

   BEGIN

      RETURN self.name;

   END GetTopoName;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetSrid
   RETURN NUMBER
   AS

   BEGIN

      RETURN self.srid;

  END GetSrid;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetTolerance
   RETURN NUMBER
   AS

   BEGIN

      --no setter for this one, sources from the topology
      RETURN self.Tolerance;

   END GetTolerance;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetNullTolerance
   RETURN NUMBER
   AS

   BEGIN

      IF self.nullTolerance IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'No null tolerance set!');

      END IF;

      RETURN self.nullTolerance;

   END GetNullTolerance;


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFeatureTables
   RETURN MDSYS.STRINGLIST
   AS

   BEGIN

      RETURN self.featureTables;

   END GetFeatureTables;


 ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFeatureTableKey
   RETURN VARCHAR2
   AS

      --sweep this under the rug
      --layers will use topology.getFeatureTableKey instead of hard coding GEO_ID
      --   all over the place


   BEGIN

      IF self.featureTableKey IS NOT NULL
      THEN

         --SOP usage from the API after instantiation
         RETURN self.featureTableKey;

      ELSIF self.name = 'MT'
      THEN

         --ha ha ha
         RETURN 'OID';

      ELSE

         --call from the constructor
         --assumption that key is the same for all non-face layers within a topology
         --  know that for some pub products this isn't true
         RETURN 'GEO_ID';

      END IF;


   END GetFeatureTableKey;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFaceFeatureTable
   RETURN VARCHAR2
   AS

      --hide all embarassing details about table name conventions like
      --FSL and FACE here.  If these conventions change... fingers crossed...
      --change here only

   BEGIN

      IF self.faceFeatureTable IS NOT NULL
      THEN

         --SOP usage from the API after instantiation
         RETURN self.faceFeatureTable;

      ELSE

         --call from the constructor

         FOR i IN 1 .. self.featureTables.COUNT
         LOOP

            IF  self.featureTables(i) LIKE '%FACE%'
            AND self.featureTables(i) NOT LIKE '%FSL%'
            THEN

               RETURN self.featureTables(i);

            END IF;

         END LOOP;

         --allow this?  Like skeleton topos and MT topos
         --RAISE_APPLICATION_ERROR(-20001, 'Couldnt find a feature table like FACE and not like FSL');

      END IF;


   END GetFaceFeatureTable;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Valid
   RETURN VARCHAR2
   AS

   BEGIN

      IF  self.name IS NULL
      AND self.owner IS NULL
      THEN

         RETURN 'FALSE';

      ELSE

         RETURN 'TRUE';

      END IF;

   END Valid;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Describe (
      p_indent          IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      output         VARCHAR2(8000);
      stringy        VARCHAR2(4000);

   BEGIN

      output := output || p_indent || 'owner: ' || self.owner || chr(10);
      output := output || p_indent || 'name: ' || self.name || chr(10);
      output := output || p_indent || 'srid: ' || TO_CHAR(self.srid) || chr(10);
      output := output || p_indent || 'id: ' || TO_CHAR(self.id) || chr(10);
      output := output || p_indent || 'tolerance: ' || TO_CHAR(self.tolerance) || chr(10);
      output := output || p_indent || 'nullTolerance: ' || TO_CHAR(self.nullTolerance) || chr(10);
      output := output || p_indent || 'layerCount: ' || TO_CHAR(self.layerCount) || chr(10);

      IF self.featureTables IS NOT NULL
      THEN

         FOR i IN 1 .. self.featureTables.COUNT
         LOOP

            stringy := stringy || featureTables(i);

            IF i <> self.featureTables.COUNT
            THEN

               stringy := stringy || ',';

            END IF;

         END LOOP;

      END IF;

      output := output || p_indent || 'featureTables: ( ' || stringy || ' )' || chr(10);
      output := output || p_indent || 'featureTableKey: ' || self.featureTableKey || chr(10);
      output := output || p_indent || 'faceFeatureTable: ' || self.faceFeatureTable || chr(10);
      output := output || p_indent || 'dbug: ' || TO_CHAR(self.dbug) || chr(10);

      RETURN output;

   END Describe;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetNullTolerance (
      p_null_tolerance        IN NUMBER
   )
   AS

   BEGIN

      self.nullTolerance := p_null_tolerance;

   END SetNullTolerance;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetFeatureTables
   AS

      psql              VARCHAR2(4000);

   BEGIN

      psql := 'SELECT a.table_name '
           || 'FROM '
           || 'all_sdo_topo_info a '
           || 'WHERE a.topology = :p1 AND '
           || 'a.owner = :p2 '
           || 'ORDER BY a.tg_layer_id ';  --fix this order

      EXECUTE IMMEDIATE psql BULK COLLECT INTO self.featureTables USING self.name,
                                                                        self.owner;

      self.layerCount := self.featureTables.COUNT;

   END SetFeatureTables;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetDbug
   AS

   BEGIN

      --oooh different levels of dbug?
      self.dbug := 1;

   END SetDbug;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE Purge
   AS

   BEGIN

      GZ_TOPO_UTIL.PURGE_TOPOLOGY(self.owner,
                                  self.name);

      --make "invalid"
      self.owner := NULL;
      self.name := NULL;

   END Purge;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE Copy(
      p_target_name         IN VARCHAR2
   )
   AS

   BEGIN

      IF self.valid() = 'TRUE'
      THEN

         BEGIN

            GZ_TOPO_UTIL.copy_topology(self.owner,
                                       self.name,
                                       SYS_CONTEXT('USERENV','CURRENT_SCHEMA'),
                                       p_target_name);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%TARGET TOPOLOGY EXISTS%'
            THEN

               RAISE_APPLICATION_ERROR(-20001, 'Topology ' || p_target_name || ' exists.  Instantiate and xx.Purge it first');

            ELSE

               RAISE_APPLICATION_ERROR(-20001, SQLERRM || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Check ur workflow, topo object instance not valid');

      END IF;

   END Copy;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE AddLayer(
      p_table_name         IN VARCHAR2,
      p_column_name        IN VARCHAR2 DEFAULT 'TOPOGEOM',
      p_layer_type         IN VARCHAR2 DEFAULT 'POLYGON',
      p_child_table        IN NUMBER DEFAULT NULL
   )
   AS

   BEGIN

      GZ_TOPO_UTIL.gz_add_topo_geometry_layer(self.name,
                                              p_table_name,
                                              p_column_name,
                                              p_layer_type,
                                              p_child_table);

      self.SetFeatureTables;


   END AddLayer;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE DeleteLayer(
      p_table_name         IN VARCHAR2,
      p_column_name        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   )
   AS

   BEGIN

      --probably should catch an error here about dependent child layers
      SDO_TOPO.delete_topo_geometry_layer(self.name,
                                          p_table_name,
                                          p_column_name);

      GZ_BUSINESS_UTILS.GZ_DROP_TABLE(p_table_name);


   END DeleteLayer;

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