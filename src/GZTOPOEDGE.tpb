CREATE OR REPLACE
TYPE BODY GzTopoEdge
AS

   --Matt! 3/11/14

   --SAMPLE, play usage
   --   declare
   --      thistopo  GzTopology  := GzTopology();
   --      thisedge  GzTopoEdge := GzTopoEdge();
   --   begin
   --      thistopo  := GzTopology('SCHEL010','Z899IN');
   --      thisedge := GzTopoEdge(3318,41,thistopo);
   --      thisedge.describe;
   --   end;

   --sample, remove an edge from the topology
   --   declare
   --      thistopo  GzTopology  := GzTopology();
   --      thisedge  GzTopoEdge := GzTopoEdge();
   --   begin
   --      thistopo := GzTopology('SCHEL010','Z899IN');
   --      thisedge := GzTopoEdge(31242,539,thistopo);
   --      thisedge.remove;
   --   end;


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --Default constructor

   CONSTRUCTOR FUNCTION GzTopoEdge
   RETURN SELF AS RESULT
   AS
   BEGIN

      RETURN;

   END GzTopoEdge;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzTopoEdge(
      p_edge_id            IN NUMBER,
      p_face_id            IN NUMBER,
      p_GzTopology         IN GzTopology
   ) RETURN SELF AS RESULT
   AS

      psql              VARCHAR2(4000);
      left_face_id      NUMBER;
      right_face_id     NUMBER;

   BEGIN

      self.edgeId   := ABS(p_edge_id);  --negative if we are passing in directed edges
      self.faceId   := p_face_id;
      self.topology  :=  p_GzTopology;

      psql := 'SELECT e.start_node_id, e.end_node_id, '
           || 'e.left_face_id, e.right_face_id, '
           || 'e.geometry '
           || 'FROM ' || self.topology.EdgeTable || ' e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO self.startNodeId,
                                  self.endNodeId,
                                  left_face_id,
                                  right_face_id,
                                  self.geometry USING ABS(p_edge_id);

      IF p_face_id = left_face_id
      THEN

         self.faceSide := 'L';
         self.neighborFaceId := right_face_id;

      ELSIF p_face_id = right_face_id
      THEN

         self.faceSide := 'R';
         self.neighborFaceId := left_face_id;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Face ' || p_face_id || ' doesnt match any of the L/R faces of '
                                         || self.topology.EdgeTable || ' edge_id ' || ABS(p_edge_id));

      END IF;
      
       --set coastal      
      IF left_face_id = -1 
      OR right_face_id = -1
      THEN
      
         self.coastal := 'Y';
         
      ELSE
      
         self.coastal := 'N';
      
      END IF;
      
      --set directed edge id if we dont already know it to be negative
      --in the cases where we know its positive and pass in positive this is duplicate work
      
      IF p_edge_id > 0
      THEN
      
         psql := 'SELECT t.column_value FROM '
              || 'TABLE(SDO_TOPO.GET_FACE_BOUNDARY(:p1, :p2, :p3)) t '
              || 'WHERE ABS(t.column_value) = :p4 ';
         
         EXECUTE IMMEDIATE psql INTO self.dirEdgeId USING self.Topology.GetTopoName,
                                                          self.faceId,
                                                          'FALSE',
                                                          self.edgeId;
                                                          
      ELSE
      
         self.dirEdgeId := p_edge_id;
      
      END IF;     

      RETURN;

   END GzTopoEdge;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION WithinDistance (
      p_geometry           IN MDSYS.SDO_GEOMETRY,
      p_distance           IN NUMBER
   ) RETURN VARCHAR2
   AS

      --KISS
      --input geom expected to be a vtx in my planned usage
      --but any gtype should work

      output         VARCHAR2(5);

   BEGIN

      output := SDO_GEOM.WITHIN_DISTANCE(self.geometry,
                                         p_distance,
                                         p_geometry,
                                         self.topology.tolerance);

      --'TRUE' or 'FALSE'
      RETURN output;

   END WithinDistance;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetEdgeId
   RETURN NUMBER
   AS

   BEGIN

      RETURN self.GetEdgeId;

   END GetEdgeId;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetCoordIndex (
      p_pt_geometry         IN MDSYS.SDO_GEOMETRY
   ) RETURN PLS_INTEGER
   AS

      --For a given point, return its coordinate index on the self edge
      --Just the object based wrapper to GZ_GEOM_UTILS.get_coord_index
      --Input point should be spatially ON the edge.  If it's far off there will be much mess
      --this guy does its operations in NULL srid space for both geoms

      edge_geom            SDO_GEOMETRY;
      pt_geom              SDO_GEOMETRY;

   BEGIN

      IF p_pt_geometry.sdo_gtype <> 2001
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Expected input gtype 2001, got ' || TO_CHAR(p_pt_geometry.sdo_gtype));

      END IF;

      --Return start if just one segment
      IF self.geometry.sdo_ordinates.COUNT = 4
      THEN

         RETURN 0;  --oracle coord_index

      END IF;

      pt_geom := p_pt_geometry;
      pt_geom.sdo_srid := NULL;

      edge_geom := self.GetGeometry;
      edge_geom.sdo_srid := NULL;

      RETURN GZ_GEOM_UTILS.get_coord_index(pt_geom,
                                           edge_geom);

   END GetCoordIndex;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetProjectPt (
      p_pt_geometry         IN MDSYS.SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY
   AS

      --Object based method wrapper to gz_geom_utils.gz_project_pt
      --Returns the projection point of the input point onto the self.edge
      --Always works in NULL srid because thats how we roll

      line               SDO_GEOMETRY;
      pt                 SDO_GEOMETRY;
      output             SDO_GEOMETRY;

   BEGIN

      line := self.GetGeometry;
      line.sdo_srid := NULL;

      pt := p_pt_geometry;
      pt.sdo_srid := NULL;

      output := GZ_GEOM_UTILS.GZ_PROJECT_PT(pt,
                                            line,
                                            self.topology.GetNullTolerance);

      output.sdo_srid := self.Topology.GetSrid;

      RETURN output;

   END GetProjectPt;

   ---------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetGeometry
   RETURN MDSYS.SDO_GEOMETRY
   AS

   BEGIN

      RETURN self.geometry;

   END GetGeometry;

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

   MEMBER FUNCTION GetNodeGeometry (
      p_which           IN VARCHAR2
   ) RETURN MDSYS.SDO_GEOMETRY
   AS

      psql           VARCHAR2(4000);
      output         SDO_GEOMETRY;

   BEGIN

      psql := 'SELECT n.geometry '
           || 'FROM ' || self.topology.NodeTable || ' n '
           || 'WHERE n.node_id = :p1 ';

      IF UPPER(p_which) = 'START'
      THEN

         EXECUTE IMMEDIATE psql INTO output USING self.startNodeId;

      ELSIF UPPER(p_which) = 'END'
      THEN

         EXECUTE IMMEDIATE psql INTO output USING self.endNodeId;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Which ' || p_which || '?');

      END IF;

      RETURN output;

   END GetNodeGeometry;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------


   MEMBER FUNCTION GetNodeId (
      p_which        IN VARCHAR2
   ) RETURN NUMBER
   AS

   BEGIN

      IF UPPER(p_which) = 'START'
      THEN

         RETURN self.startNodeId;

      ELSIF UPPER(p_which) = 'END'
      THEN

         RETURN self.endNodeId;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Which p_which is ' || p_which || '?');

      END IF;

   END GetNodeId;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Split (
      p_pt_geometry           IN MDSYS.SDO_GEOMETRY
   ) RETURN NUMBER
   AS

      --Return new node id
      --caller must know to ditch the current instance of the edge
      --and find new edge ids based on the returned node id
      --This guy works in real srid

      coord_index             PLS_INTEGER;
      new_node_id             NUMBER;
      new_shapepoint_flag     VARCHAR2(5) := 'TRUE';

   BEGIN

      IF p_pt_geometry.sdo_gtype <> 2001
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Expected input gtype 2001, got ' || TO_CHAR(p_pt_geometry.sdo_gtype));

      END IF;

      --could check if the input pt is at the start or end of the edge
      --or later based on the coord index
      --for now caller must be smart, or respond to the error messages thrown below

      --sub works in NULL srid
      coord_index := self.GetCoordIndex(p_pt_geometry);

      IF self.dbug = 1
      THEN
         dbms_output.put_line('found coord index ' || coord_index);
      END IF;

      --Are we close to an existing shape point?  Gonna risk it

      BEGIN

         new_node_id := GZ_TOPO_PRIMITIVE.ADD_NEW_NODE(self.topology.name,
                                                       self.edgeId,
                                                       coord_index,
                                                       p_pt_geometry,
                                                       new_shapepoint_flag);  --default TRUE

      EXCEPTION
      WHEN OTHERS
      THEN

         IF self.dbug = 1
         THEN
            dbms_output.put_line('first add_new_node threw ' || SQLERRM);
         END IF;

         --Look at this idiocy, try again with existing shape point flag, dont even test
         IF LOWER(SQLERRM) LIKE '%add node results in self-intersection of line string%'
         OR LOWER(SQLERRM) LIKE '%add node results in two pieces of split edge intersecting each other%'
         OR LOWER(SQLERRM) LIKE '%results in an intersection with another edge%'
         THEN

            new_shapepoint_flag := 'FALSE';
            --coord_index := coord_index + 1;

            IF self.dbug = 1
            THEN
               dbms_output.put_line('error adding node, trying again with new shapepoint flag set to FALSE');
            END IF;

            BEGIN

               --note coord_index + 1.  Segments always own their start coord index
               --meaning end of the segment we were on, push on to the next
               new_node_id := GZ_TOPO_PRIMITIVE.ADD_NEW_NODE(self.topology.name,
                                                             self.edgeId,
                                                             coord_index,
                                                             p_pt_geometry,
                                                             new_shapepoint_flag);

            EXCEPTION
            WHEN OTHERS
            THEN

               IF LOWER(SQLERRM) LIKE '%attempted to add a node at an edge terminus or outside the range of the vertex array%'
               OR LOWER(SQLERRM) LIKE '%add node results in self-intersection of line string%'
               OR LOWER(SQLERRM) LIKE '%add node results in two pieces of split edge intersecting each other%'
               OR LOWER(SQLERRM) LIKE '%results in an intersection with another edge%'
               THEN

                  --most likely
                  --Add node results in self-intersection of line string
                  --Followed by
                  --Attempted to add a node at an edge terminus or outside the range of the vertex array

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace || ' on '
                                      || 'GZ_TOPO_PRIMITIVE.ADD_NEW_NODE(''' ||  self.topology.name || ''','
                                      || TO_CHAR(self.edgeId) || ','
                                      || coord_index || ',' || TO_CHAR(gz_geom_utils.dump_sdo(p_pt_geometry)) || ','
                                      || '''' || new_shapepoint_flag || ''')');

               ELSE

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace || ' on '
                                      || 'GZ_TOPO_PRIMITIVE.ADD_NEW_NODE(''' ||  self.topology.name || ''','
                                      || TO_CHAR(self.edgeId) || ','
                                      || coord_index || ',' || TO_CHAR(gz_geom_utils.dump_sdo(p_pt_geometry)) || ','
                                      || '''' || new_shapepoint_flag || ''')');

               END IF;

            END;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace || ' on '
                                      || 'GZ_TOPO_PRIMITIVE.ADD_NEW_NODE(''' ||  self.topology.name || ''','
                                      || TO_CHAR(self.edgeId) || ','
                                      || coord_index || ',' || TO_CHAR(gz_geom_utils.dump_sdo(p_pt_geometry)) || ','
                                      || '''' || new_shapepoint_flag || ''')');

         END IF;

      END;

      RETURN new_node_id;

   END Split;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Describe (
      p_indent             IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      output         VARCHAR2(8000);

   BEGIN

      output := output || p_indent || 'edgeId: ' || TO_CHAR(self.edgeId) || chr(10);
      output := output || p_indent || 'dirEdgeId: ' || TO_CHAR(self.dirEdgeId) || chr(10);
      output := output || p_indent || 'faceId: ' || TO_CHAR(self.faceId) || chr(10);
      output := output || p_indent || 'faceSide: ' || self.faceSide || chr(10);
      output := output || p_indent || 'startNodeId: ' || TO_CHAR(self.startNodeId) || chr(10);
      output := output || p_indent || 'endNodeId: ' || TO_CHAR(self.endNodeId) || chr(10);
      output := output || p_indent || 'topology: ' || chr(10) || self.topology.Describe(p_indent || '   ');
      output := output || p_indent || 'neighborFaceId: ' || TO_CHAR(self.neighborFaceId) || chr(10);
      output := output || p_indent || 'coastal: ' || self.coastal || chr(10);
      output := output || p_indent || 'geometry: SDO_GEOMETRY( ' || TO_CHAR(self.geometry.sdo_gtype) || ','  || TO_CHAR(self.geometry.sdo_srid) || ','
                                   || ' NULL, SDO_ELEM_INFO_ARRAY(1,2,1), '
                                   || ' SDO_ORDINATE_ARRAY(' || self.geometry.sdo_ordinates(1) || ',' || self.geometry.sdo_ordinates(2) || '..<SNIP>..'
                                   || self.geometry.sdo_ordinates(self.geometry.sdo_ordinates.COUNT - 1) || ',' || self.geometry.sdo_ordinates(self.geometry.sdo_ordinates.COUNT)
                                   ||  '))' || chr(10);
      output := output || p_indent || 'dbug: ' || self.dbug || chr(10);

      RETURN output;

   END Describe;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER Procedure Remove
   AS

      --removes the edge with respect to the instantiating face
      --ie kills that face's attributes
      --all fsl table topogeom will be updated correctly (no measurements however)
      --if a face feature table exists it will return that face table to its
      --   "correct" state for the two faces on either side of the edge
      --   creating 1:1 match between the face feature record face_id and primitives

      --will cause any registered feature layers on the neighbor face to
      --   take over territory on the face belonging to the self edge
      --subroutines take care of any FSL deletions, including almost certain
      --   deletion on the face feature table


      self_face         GzFeatureFace := GzFeatureFace();

   BEGIN

      IF self.faceId = self.neighborFaceId
      THEN

        RAISE_APPLICATION_ERROR(-20001, 'Not prepared to remove a dangling edge, ' || self.edgeId || '. Good idea?');

      END IF;

      --instantiate this edge's face
      self_face := GzFeatureFace(self.faceId,
                                 self.topology);

      --match features
      self_face.matchFeatures(self.neighborFaceId);

      --kill the edge in the topo
      GZ_TOPO_PRIMITIVE.remove_edge(self.topology.name,
                                    self.edgeId);

      IF self.topology.GetFaceFeatureTable IS NOT NULL
      THEN

         GZ_TOPO_PRIMITIVE.update_merged_face_features(self.topology.GetFaceFeatureTable,
                                                       self.faceId,
                                                       self.neighborFaceId);

      END IF;

   END Remove;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetDbug
   AS

   BEGIN

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