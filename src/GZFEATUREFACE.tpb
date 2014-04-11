CREATE OR REPLACE
TYPE BODY GzFeatureFace
AS

   --Matt! 3/19/14
   --Rewrite of original gz_feature_face type

   --SAMPLE, play usage
   --   declare
   --      thistopo   GzTopology  := GzTopology();
   --      thisface   GzFeatureFace  := GzFeatureFace();
   --   begin
   --      thistopo  := GzTopology('SCHEL010','Z899IN');
   --      thisface := GzFeatureFace(352, thistopo);
   --      thisface.describe;
   --   end;



   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --Default constructor

   CONSTRUCTOR FUNCTION GzFeatureFace
   RETURN SELF AS RESULT
   AS
   BEGIN

      RETURN;

   END GzFeatureFace;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzFeatureFace(
      p_face_id               IN NUMBER,
      p_topology              IN GzTopology
   ) RETURN SELF AS RESULT
   AS

   BEGIN

      InitializeFaceKernel(p_face_id,
                           p_topology);

      --worry about the performance of this if it actually is a problem, not in advance
      self.SetFeatureLayers;

      RETURN;

   END GzFeatureFace;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzFeatureFace(
      p_face_id               IN NUMBER,
      p_topology              IN GzTopology,
      p_featureTables         IN MDSYS.stringlist,
      p_featureTableGeoids    IN MDSYS.stringlist
   ) RETURN SELF AS RESULT
   AS

      --this one used when reinstantiating a face that's been edited in shape
      --but the feature tables are the same. Pass them in from the original
      --object to save a hot millisecond


   BEGIN

      InitializeFaceKernel(p_face_id,
                           p_topology);

      self.FeatureTables      := p_featureTables;
      self.FeatureTableGeoids := p_featureTableGeoids;

      RETURN;

   END GzFeatureFace;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFaceId
   RETURN NUMBER
   AS

   BEGIN

      RETURN self.faceId;

   END GetFaceId;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFeatureTables
   RETURN MDSYS.STRINGLIST
   AS

   BEGIN

      RETURN self.FeatureTables;

   END GetFeatureTables;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetFeatureTableGeoids
   RETURN MDSYS.STRINGLIST
   AS

   BEGIN

      RETURN self.FeatureTableGeoids;

   END GetFeatureTableGeoids;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetBoundaryEdges
   RETURN MDSYS.SDO_LIST_TYPE
   AS

   BEGIN

      RETURN self.BoundaryEdges;

   END GetBoundaryEdges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetInteriorEdges (
      p_direction          IN VARCHAR2        --CLOCKWISE or COUNTERCLOCKWISE
   )RETURN MDSYS.SDO_LIST_TYPE
   AS

      --Return ordered but unsigned array of interior edges starting at the universal edge
      --   either from clockwise side or counterclockwise
      --Requires specific definition of a coastal face as one that has a single edge on the universal

      ordered_signed_edges             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      ordered_edges                    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

   BEGIN

      IF p_direction NOT IN ('CLOCKWISE','COUNTERCLOCKWISE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Invalid p_direction ' || p_direction);

      END IF;

      IF self.coastalEdges.COUNT <> 1
      OR self.ringCount > 1
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Only valid for faces touching universal on one edge and with no inner rings');

      END IF;

      ordered_signed_edges := self.getSignedInteriorEdges(p_direction);
      ordered_edges.EXTEND(ordered_signed_edges.COUNT);

      FOR i IN 1 .. ordered_signed_edges.COUNT
      LOOP

         ordered_edges(i) := ABS(ordered_signed_edges(i));

      END LOOP;

      RETURN ordered_edges;

   END GetInteriorEdges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetSignedInteriorEdges(
      p_direction          IN VARCHAR2        --CLOCKWISE or COUNTERCLOCKWISE
   ) RETURN MDSYS.SDO_LIST_TYPE
   AS

      --Return ordered and signed array of interior edges starting at the universal face
      --   either from clockwise side or counterclockwise
      --Requires specific definition of a coastal face as one that has a single edge on the universal

      ordered_signed_edges             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      reversed_signed_edges            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      found_coastal_pos                PLS_INTEGER;
      kount                            PLS_INTEGER := 0;

   BEGIN

      IF p_direction NOT IN ('CLOCKWISE','COUNTERCLOCKWISE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Invalid p_direction ' || p_direction);

      END IF;

      IF self.coastalEdges.COUNT <> 1
      OR self.ringCount <> 1
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Only valid for faces touching universal on one edge and with no inner rings');

      END IF;

      ordered_signed_edges.EXTEND(self.signedBoundaryEdges.COUNT - 1);

      --perform counterclockwise walk, matches the signed_boundary_edges
      FOR i IN 1 .. self.signedBoundaryEdges.COUNT
      LOOP

         --find the coastal edge
         IF ABS(self.signedBoundaryEdges(i)) = self.coastalEdges(1)
         THEN

            found_coastal_pos := i;
            EXIT;

         END IF;

      END LOOP;

      IF found_coastal_pos <> self.signedBoundaryEdges.COUNT
      THEN

         --walk inward from coastal .. end of the array
         FOR i IN (found_coastal_pos + 1) .. self.signedBoundaryEdges.COUNT
         LOOP

            kount := kount + 1;
            ordered_signed_edges(kount) := self.signedBoundaryEdges(i);

         END LOOP;

      END IF;

      IF found_coastal_pos <> 1  --if it was first we are done in the first loop
      THEN

         --walk from the beginning of the array back to the coast
         FOR i IN 1 .. (found_coastal_pos - 1)
         LOOP

            kount := kount + 1;
            ordered_signed_edges(kount) := self.signedBoundaryEdges(i);

         END LOOP;

      END IF;

      IF p_direction = 'COUNTERCLOCKWISE'
      THEN

         --Done
         RETURN ordered_signed_edges;

      ELSE

         --Reverse the array.  Easier this way than trying to work the loops above individually

         reversed_signed_edges.EXTEND(ordered_signed_edges.COUNT);
         kount := 0;

         FOR i IN REVERSE 1 .. ordered_signed_edges.COUNT
         LOOP

            kount := kount + 1;
            reversed_signed_edges(kount) := ordered_signed_edges(i);

         END LOOP;

         RETURN reversed_signed_edges;

      END IF;

   END GetSignedInteriorEdges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetBoundaryFaces
   RETURN MDSYS.SDO_LIST_TYPE
   AS
   
      --this one is rarely used, just a getter, no set attribute
      
      psql           VARCHAR2(4000);
      bdy_faces      MDSYS.SDO_LIST_TYPE;

   BEGIN

      psql := 'SELECT e.left_face_id FROM ' || self.topology.EdgeTable || ' e '
           || 'WHERE e.right_face_id = :p1 '
           || 'AND e.left_face_id <> :p2 '
           || 'UNION '
           || 'SELECT e.right_face_id FROM ' || self.topology.EdgeTable || ' e '
           || 'WHERE e.left_face_id = :p3 '
           || 'AND e.right_face_id <> :p4 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO bdy_faces USING self.GetFaceId,
                                                               -1,
                                                               self.GetFaceId,
                                                               -1;
                                                               
      RETURN bdy_faces;

   END GetBoundaryFaces;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetSdogeometry
   RETURN MDSYS.SDO_GEOMETRY
   AS

   BEGIN

      RETURN self.sdoGeometry;

   END GetSdogeometry;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetCoastalEdges
   RETURN MDSYS.SDO_LIST_TYPE
   AS

   BEGIN

      RETURN self.coastalEdges;

   END GetCoastalEdges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetTopoEdge (
      p_edge_id            IN NUMBER
   ) RETURN GzTopoEdge
   AS

   BEGIN

      FOR i IN 1 .. self.topoEdges.COUNT
      LOOP

         IF self.topoEdges(i).edgeID = p_edge_id
         THEN

            RETURN self.topoEdges(i);

         END IF;

      END LOOP;

      RAISE_APPLICATION_ERROR(-20001, 'Didnt find edge ' || p_edge_id);

   END GetTopoEdge;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetCoastalSliver
   RETURN VARCHAR2
   AS

   BEGIN

      IF self.CoastalSliver IS NOT NULL
      THEN

         RETURN self.coastalSliver;

      ELSE

         --hmmm
         RETURN NULL;

      END IF;

   END GetCoastalSliver;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetCoastalSliverDistance (
      p_direction       IN VARCHAR2
   ) RETURN NUMBER
   AS

   BEGIN

      IF UPPER(p_direction) IN ('CLOCKWISE', 'COUNTERCLOCKWISE')
      THEN

         IF self.coastalSliver = 'PARTIAL'
         THEN

            IF UPPER(p_direction) = 'CLOCKWISE'
            THEN

               IF self.coastalSliverClockD > 0
               THEN

                  RETURN self.coastalSliverClockD;

               ELSE

                  RETURN 0;

               END IF;

            ELSIF UPPER(p_direction) = 'COUNTERCLOCKWISE'
            THEN

               IF self.coastalSliverCClockD > 0
               THEN

                  RETURN self.coastalSliverCClockD;

               ELSE

                  RETURN 0;

               END IF;

            END IF;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, 'Face ' || self.faceID || ' has a coastalSliver value of ' || self.coastalSliver);

         END IF;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Unknown sliver direction ' || p_direction);

      END IF;

   END GetCoastalSliverDistance;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetPartialSliverCutoff (
      p_direction          IN VARCHAR2
   ) RETURN MDSYS.SDO_GEOMETRY
   AS

      --Mainly a helper to visualize

      ordered_signed_edges             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      edge_geometry                    SDO_GEOMETRY;
      cutoff                           NUMBER;
      running_length                   NUMBER := 0;
      vtx_geometry                     SDO_GEOMETRY;
      prev_geometry                    SDO_GEOMETRY;

   BEGIN

      IF self.coastalSliver <> 'PARTIAL'
      THEN

         RETURN NULL;

      END IF;

      IF p_direction = 'CLOCKWISE'
      THEN

         cutoff := coastalSliverClockD;

      ELSIF p_direction = 'COUNTERCLOCKWISE'
      THEN

         cutoff := coastalSliverCClockD;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Bunk direction ' || p_direction);

      END IF;

      ordered_signed_edges := self.getSignedInteriorEdges(p_direction);

      FOR i IN 1 .. ordered_signed_edges.COUNT
      LOOP


         --measure each geometry up to self.coastalsliverclockd or coastalslivercclockd

         edge_geometry := self.GetTopoEdge(ABS(ordered_signed_edges(i))).geometry;

         --measure the length of the edge and decide if we need to dive into it

         IF (running_length + SDO_GEOM.SDO_LENGTH(edge_geometry, self.topology.tolerance)) >= cutoff
         THEN

            --this is the girl

            IF (p_direction = 'COUNTERCLOCKWISE' AND ordered_signed_edges(i) < 0)
            OR (p_direction = 'CLOCKWISE' AND ordered_signed_edges(i) > 0)
            THEN

               --reverse edges that run opposite to our current walk
               edge_geometry := SDO_UTIL.REVERSE_LINESTRING(edge_geometry);

            END IF;

            FOR jj IN 1 .. edge_geometry.SDO_ORDINATES.COUNT/2
            LOOP

               vtx_geometry := SDO_GEOMETRY(
                               2001,
                               self.topology.srid,
                               SDO_POINT_TYPE(
                                              edge_geometry.SDO_ORDINATES( (jj * 2) - 1),
                                              edge_geometry.SDO_ORDINATES( (jj * 2)),
                                              NULL),
                               NULL,
                               NULL);

               IF jj = 1
               THEN

                  --nothing to measure yet, store the position
                  prev_geometry := vtx_geometry;

               ELSE

                  running_length := running_length + SDO_GEOM.SDO_DISTANCE(prev_geometry,
                                                                           vtx_geometry,
                                                                           self.topology.tolerance);


               END IF;

               --dbms_output.put_line(jj || ' running length ' || running_length);

               IF running_length > cutoff
               THEN

                  --think its best to always go to the next vertex in case rounding and stuff
                  --makes it impossible to precisely locate the vtx at the cutoff distance

                  --Returning the vertex at the start of the segment that we just measured as being too far
                  RETURN prev_geometry;

               ELSE

                  prev_geometry := vtx_geometry;

               END IF;

            END LOOP;

         ELSE

            running_length := running_length + SDO_GEOM.SDO_LENGTH(edge_geometry, self.topology.tolerance);

         END IF;

      END LOOP;

      RAISE_APPLICATION_ERROR(-20001, 'Didnt find the cutoff, check some stuff yo');

   END GetPartialSliverCutoff;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Split (
      self                 IN OUT GzFeatureFace,  --dumb.  Allows call to self.DuplicateFaceRecord
      p_sdogeometry        IN MDSYS.SDO_GEOMETRY,
      p_depth              IN NUMBER DEFAULT 0
   ) RETURN NUMBER
   AS

      --3/24/14!
      --Dont call this one.  Use SplitFaceObject procedure which is a wrapper to this

      --split the face using the input geometry
      --return the edge_id that separates the two resulting faces
      --Duplicate the row in the face feature table and update face_id and topogeom

      --the code here will recurse until operating on a static face with no new nodes added
      --   in the present recursion

      --All work is in NULL srid with topology.nullTolerance
      --Since that's how the Oracle topology rolls, so also we must roll

      --SAMPLE
      --      declare
      --         thistopo    GzTopology     := GzTopology();
      --         thisface    GzFeatureFace  := GzFeatureFace();
      --         splitgeom   sdo_geometry := sdo_geometry(.....);
      --         new_edge_id number;
      --      begin
      --         thistopo  := GzTopology('SCHEL010','Z899IN');
      --         thisface := GzFeatureFace(756, thistopo);
      --         new_edge_id := thisface.split(splitgeom);
      --      end;

      start_pt_geom        SDO_GEOMETRY;
      end_pt_geom          SDO_GEOMETRY;
      split_geom           SDO_GEOMETRY;
      self_geom            SDO_GEOMETRY;
      test_edge_geom       SDO_GEOMETRY;
      start_node_id        NUMBER;
      end_node_id          NUMBER;
      node_geom            SDO_GEOMETRY;
      recursed_face        GzFeatureFace := GzFeatureFace();
      new_edge_id          NUMBER;
      new_face_id          NUMBER;
      psql                 VARCHAR2(4000);


   BEGIN

      IF p_depth > 2
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Too many recursions');

      ELSIF self.dbug = 1
      THEN

         dbms_output.put_line(p_depth || ' fathom(s)');

      END IF;

      IF self.ringCount > 1
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Not ready to work on faces with interior rings. Can be done shonuff!');

      END IF;

      IF p_sdogeometry.sdo_gtype <> 2002
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Sorry, can only split faces using lines. Input gtype is ' || p_sdogeometry.sdo_gtype);

      ELSE

         split_geom := p_sdogeometry;

         --The great nulling
         split_geom.sdo_srid := NULL;

         self_geom := self.GetSdogeometry;
         self_geom.sdo_srid := NULL;

      END IF;

      IF self.dbug = 1
      THEN
         dbms_output.put_line('Starting GzFeatureFace.split with split_geom ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(split_geom)));
      END IF;

      --check that the input sdogeometry comes correct

      IF p_depth = 0
      AND SDO_GEOM.relate(split_geom,
                         'mask=COVEREDBY',
                         self_geom,
                         self.topology.GetNullTolerance) = 'FALSE'
      THEN

         IF self.dbug = 1
         THEN
            dbms_output.put_line('Input line isnt covered by the face shape. Line: '
                                 || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(split_geom)));
         END IF;

         --Going to let slivers own their inside bits - sliver code must catch this
         RAISE_APPLICATION_ERROR(-20001, 'Input line isnt covered by the face shape');

      END IF;


      start_pt_geom := SDO_GEOMETRY(2001,
                                    split_geom.sdo_srid,
                                    SDO_POINT_TYPE(split_geom.sdo_ordinates(1),
                                                   split_geom.sdo_ordinates(2),
                                                   NULL),
                                    NULL,NULL);

      end_pt_geom := SDO_GEOMETRY(2001,
                                  split_geom.sdo_srid,
                                  SDO_POINT_TYPE(split_geom.sdo_ordinates(split_geom.sdo_ordinates.COUNT - 1),
                                                 split_geom.sdo_ordinates(split_geom.sdo_ordinates.COUNT),
                                                 NULL),
                                  NULL,NULL);

      IF SDO_GEOM.relate(start_pt_geom,
                         'mask=EQUAL',
                         end_pt_geom,
                         self.topology.GetNullTolerance) <> 'FALSE'
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Geometry input to .split is a loop');

      END IF;

      ------------------------------------------------
      --PART 1: AT THE START OF THE INPUT SPLIT GEOM--
      ------------------------------------------------

      -------------------------------------------------------------------
      --Plan A: check for an exact x,y match with an edge start/end node
      -------------------------------------------------------------------

      FOR i IN 1 .. self.topoEdges.COUNT
      LOOP

         --check if we are splitting starting from an existing node exactly - start or end of the edge
         --this only really matches if the inputs from the caller are literally taken from node$. Which we do below

         IF  self.topoEdges(i).geometry.sdo_ordinates(1) = start_pt_geom.sdo_point.X
         AND self.topoEdges(i).geometry.sdo_ordinates(2) = start_pt_geom.sdo_point.Y
         THEN

            start_node_id := self.topoEdges(i).startNodeId;

         ELSIF self.topoEdges(i).geometry.sdo_ordinates(self.topoEdges(i).geometry.sdo_ordinates.COUNT - 1) = start_pt_geom.sdo_point.X
         AND   self.topoEdges(i).geometry.sdo_ordinates(self.topoEdges(i).geometry.sdo_ordinates.COUNT) = start_pt_geom.sdo_point.Y
         THEN

            start_node_id := self.topoEdges(i).endNodeId;

         END IF;

         --IF plan A succeeds, on to Part 2

      END LOOP;

      ---------------------------------------------
      --Plan B: Use sdo_geom.relate to find an edge
      ---------------------------------------------

      IF start_node_id IS NULL
      THEN

         --must split an existing edge
         --roll through and find who touches the start

         FOR i IN 1 .. self.topoEdges.COUNT  --exit loop when we get a start_node_id
         LOOP

            ----------------------------------------------------
            --Plan B Option 1: Splits an existing edge interior
            ----------------------------------------------------

            test_edge_geom := self.topoEdges(i).geometry;
            test_edge_geom.sdo_srid := NULL;

            IF SDO_GEOM.relate(test_edge_geom,
                               'mask=CONTAINS',   --this is the relationship 0------x------0
                               start_pt_geom,
                               self.topology.GetNullTolerance) <> 'FALSE'
            THEN

               --this is the edge
               --is this cool with everyone?

               start_pt_geom.sdo_srid := self.topology.GetSrid;
               start_node_id := self.topoEdges(i).Split(start_pt_geom);
               ------
               EXIT;
               ------

            ----------------------------------------------------------
            --Plan B Option 2: Is close to the start or end of an edge
            ----------------------------------------------------------

            ELSIF SDO_GEOM.relate(test_edge_geom,
                                  'mask=TOUCH',   --this is the relationship  0----------X0
                                  start_pt_geom,
                                  self.topology.GetNullTolerance) <> 'FALSE'
            THEN

               node_geom := GZ_TOPO_PRIMITIVE.get_node_geometry(self.topology.name,
                                                                self.topoEdges(i).GetNodeId('START'));

               node_geom.sdo_srid := NULL;

               IF SDO_GEOM.relate(node_geom,
                                  'mask=EQUAL',
                                  start_pt_geom,
                                  self.topology.GetNullTolerance) <> 'FALSE'
               THEN

                  start_node_id := self.topoEdges(i).GetNodeId('START');
                  ------
                  EXIT;
                  ------

               ELSE

                  node_geom := GZ_TOPO_PRIMITIVE.get_node_geometry(self.topology.name,
                                                                   self.topoEdges(i).GetNodeId('END'));

                  node_geom.sdo_srid := NULL;

                  --this is a mess
                  IF SDO_GEOM.relate(node_geom,
                                     'mask=EQUAL',
                                     start_pt_geom,
                                     self.topology.GetNullTolerance) <> 'FALSE'
                  THEN

                     start_node_id := self.topoEdges(i).GetNodeId('END');
                     ------
                     EXIT;
                     ------

                  ELSE

                     RAISE_APPLICATION_ERROR(-20001, 'Saw relationship ON but couldnt nail it down');

                  END IF;

               END IF;

            ELSIF self.dbug = 1
            THEN

               dbms_output.put_line('start relate to edge_id ' || self.topoEdges(i).edgeId || ' is '
                                    || SDO_GEOM.relate(test_edge_geom, 'mask=DETERMINE',start_pt_geom,self.topology.GetNullTolerance));

            END IF;

         END LOOP;

         IF start_node_id IS NULL
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Failed to determine the start point in the face');

         END IF;

         --We built a node at the start or we found a "close enough" node
         --munge the input geom to match whatever the topo thinks is out in the nether-digits
         node_geom := GZ_TOPO_PRIMITIVE.get_node_geometry(self.topology.name,
                                                          start_node_id);

         split_geom.sdo_ordinates(1) := node_geom.sdo_point.X;
         split_geom.sdo_ordinates(2) := node_geom.sdo_point.Y;

         -------------------------------------------
         --IF we chose plan B, start recursion here
         -------------------------------------------

         recursed_face := GzFeatureFace(self.faceId,
                                        self.topology,
                                        self.FeatureTables,
                                        self.FeatureTableGeoids);

         new_edge_id := recursed_face.Split(split_geom,
                                            p_depth + 1);

         -------------------
         RETURN new_edge_id;
         -------------------

      END IF;


      ------------------------------------------------
      --PART 2: AT THE END OF THE INPUT SPLIT GEOM--
      ------------------------------------------------

      ------------------------------------------------------------------
      --Plan A: check for an exact x,y match with an edge start/end node
      ------------------------------------------------------------------

      --end of the splitter
      --check for coincidence with an existing node



      FOR i IN 1 .. self.topoEdges.COUNT
      LOOP

         --check if we are splitting starting from an existing node - start or end of the edge

         IF  self.topoEdges(i).geometry.sdo_ordinates(1) = end_pt_geom.sdo_point.X
         AND self.topoEdges(i).geometry.sdo_ordinates(2) = end_pt_geom.sdo_point.Y
         THEN

            end_node_id := self.topoEdges(i).startNodeId;

         ELSIF self.topoEdges(i).geometry.sdo_ordinates(self.topoEdges(i).geometry.sdo_ordinates.COUNT - 1) = end_pt_geom.sdo_point.X
         AND   self.topoEdges(i).geometry.sdo_ordinates(self.topoEdges(i).geometry.sdo_ordinates.COUNT) = end_pt_geom.sdo_point.Y
         THEN

            end_node_id := self.topoEdges(i).endNodeId;

         END IF;

         --IF plan A succeeds, on to Part 3

      END LOOP;

      ---------------------------------------------
      --Plan B: Use sdo_geom.relate to find an edge
      ---------------------------------------------

      IF end_node_id IS NULL
      THEN

         FOR i IN 1 .. self.topoEdges.COUNT
         LOOP

            test_edge_geom := self.topoEdges(i).geometry;
            test_edge_geom.sdo_srid := NULL;

            ----------------------------------------------------
            --Plan B Option 1: Splits an existing edge interior
            ----------------------------------------------------

            IF SDO_GEOM.relate(test_edge_geom,
                               'mask=CONTAINS',
                               end_pt_geom,
                               self.topology.GetNullTolerance) <> 'FALSE'
            THEN

               --this is the edge
               --is this cool with everyone?

               end_pt_geom.sdo_srid := self.topology.GetSrid;
               end_node_id := self.topoEdges(i).Split(end_pt_geom);
               ------
               EXIT;
               ------

            ----------------------------------------------------------
            --Plan B Option 2: Is close to the start or end of an edge
            ----------------------------------------------------------

            ELSIF SDO_GEOM.relate(test_edge_geom,
                                  'mask=TOUCH',   --this is the relationship  0----------X0
                                  end_pt_geom,
                                  self.topology.GetNullTolerance) <> 'FALSE'
            THEN

               node_geom := GZ_TOPO_PRIMITIVE.get_node_geometry(self.topology.name,
                                                                self.topoEdges(i).GetNodeId('START'));

               node_geom.sdo_srid := NULL;

               IF SDO_GEOM.relate(node_geom,
                                  'mask=EQUAL',
                                  end_pt_geom,
                                  self.topology.GetNullTolerance) <> 'FALSE'
               THEN

                  end_node_id := self.topoEdges(i).GetNodeId('START');
                  ------
                  EXIT;
                  ------

               ELSE

                  node_geom := GZ_TOPO_PRIMITIVE.get_node_geometry(self.topology.name,
                                                                   self.topoEdges(i).GetNodeId('END'));
                  node_geom.sdo_srid := NULL;

                  --this is a mess
                  IF SDO_GEOM.relate(node_geom,
                                     'mask=EQUAL',
                                     end_pt_geom,
                                     self.topology.GetNullTolerance) <> 'FALSE'
                  THEN

                     end_node_id := self.topoEdges(i).GetNodeId('END');
                     ------
                     EXIT;
                     ------

                  ELSE

                     RAISE_APPLICATION_ERROR(-20001, 'Saw relationship ON but couldnt nail it down');

                  END IF;

               END IF;

            ELSIF self.dbug = 1
            THEN

               dbms_output.put_line('end relate to edge_id ' || self.topoEdges(i).edgeId || ' is '
                                    || SDO_GEOM.relate(test_edge_geom, 'mask=DETERMINE',end_pt_geom,self.topology.GetNullTolerance));

            END IF;

         END LOOP;

         IF end_node_id IS NULL
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Failed to determine the end point in the face');

         END IF;

         --We built a node at the end or we found a "close enough" node
         --munge the input geom to match whatever the topo decided on out in the nether-digits
         node_geom := GZ_TOPO_PRIMITIVE.get_node_geometry(self.topology.name,
                                                          end_node_id);

         split_geom.sdo_ordinates(split_geom.sdo_ordinates.COUNT - 1) := node_geom.sdo_point.X;
         split_geom.sdo_ordinates(split_geom.sdo_ordinates.COUNT) := node_geom.sdo_point.Y;


         --this self.face is toast, recurse
         --goes in with null srid in the recursion
         recursed_face := GzFeatureFace(self.faceId,
                                        self.topology,
                                        self.FeatureTables,
                                        self.FeatureTableGeoids);

         new_edge_id := recursed_face.Split(split_geom,
                                            p_depth + 1);

         RETURN new_edge_id;

      END IF;

      ---------------------------------------------------------
      --PART 3: ADD THE EDGE  In this level of recursion
      --
      --1. The face object is built on unedited topo
      --2. The split geom start/end matches a node in the topo
      ----------------------------------------------------------

      IF  start_node_id IS NOT NULL
      AND end_node_id IS NOT NULL
      THEN

         --just in case
         split_geom.sdo_srid := self.topology.GetSrid;

         --ready to split from node to node with new geom
         new_edge_id := GZ_TOPO_PRIMITIVE.ADD_NEW_EDGE(self.topology.name,
                                                       start_node_id,
                                                       end_node_id,
                                                       split_geom);

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Failed to split!');

      END IF;

      --get the new face_id

      psql := 'SELECT e.left_face_id FROM ' || self.topology.EdgeTable || ' e '
           || 'WHERE e.edge_id = :p1 AND e.left_face_id <> :p2 '
           || 'UNION ALL '
           || 'SELECT e.right_face_id FROM ' || self.topology.EdgeTable || ' e '
           || 'WHERE e.edge_id = :p3 AND e.right_face_id <> :p4 ';

      EXECUTE IMMEDIATE psql INTO new_face_id USING new_edge_id, self.faceId,
                                                    new_edge_id, self.faceId;

      --duplicate the face feature table record. Topogeom is null
      self.DuplicateFaceRecord(new_face_id);

      --add the new primitive face to the new record topogeom
      self.topoLayer.AddFace(new_face_id,
                             new_face_id);

      --remove the new primitive face from the original face_id
      self.topoLayer.DeleteFace(self.faceId,
                                new_face_id);

      --pass back up to the surface
      RETURN new_edge_id;

   END Split;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------


   MEMBER FUNCTION GetDbug
   RETURN NUMBER
   AS

   BEGIN

      RETURN self.dbug;

   END GetDbug;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Describe (
      p_indent          IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      output         VARCHAR2(32000);
      stringy        VARCHAR2(32000);
      stringytoo     VARCHAR2(32000);

   BEGIN

      output := output || p_indent || 'faceId: ' || TO_CHAR(self.faceId) || chr(10);
      output := output || p_indent || 'topoLayer: ' || chr(10) || self.topoLayer.Describe(p_indent || '   ');
      output := output || p_indent || 'tgId: ' || TO_CHAR(self.tgId) || chr(10);

      --too much text to fully describe every edge object

      FOR i IN 1 .. self.topoEdges.COUNT
      LOOP

            stringy := stringy || self.topoEdges(i).edgeId;

            IF i <> self.topoEdges.COUNT
            THEN

               stringy := stringy || ',';

            END IF;

      END LOOP;

      output := output || p_indent || 'topoEdges: ( ' || stringy || ' )' || chr(10);

      stringy := '';

      IF self.featureTables IS NOT NULL
      THEN

         FOR i IN 1 .. self.featureTables.COUNT
         LOOP

            stringy := stringy || featureTables(i);
            stringytoo := stringytoo || featureTableGeoids(i);

            IF i <> self.featureTables.COUNT
            THEN

               stringy := stringy || ',';
               stringytoo := stringytoo || ',';

            END IF;

         END LOOP;

      END IF;

      output := output || p_indent || 'featureTables: ( ' || stringy || ' )' || chr(10);
      output := output || p_indent || 'featureTableGeoids: ( ' || stringytoo || ' )' || chr(10);

      stringy := '';
      stringytoo := '';

      FOR i IN 1 .. self.boundaryEdges.COUNT
      LOOP

         stringy := stringy || TO_CHAR(boundaryEdges(i));
         stringytoo := stringytoo || TO_CHAR(signedBoundaryEdges(i));

         IF i <> self.boundaryEdges.COUNT
         THEN

            stringy := stringy || ',';
            stringytoo := stringytoo || ',';

         END IF;

      END LOOP;

      output := output || p_indent || 'boundaryEdges: ( ' || stringy || ' )' || chr(10);
      output := output || p_indent || 'signedBoundaryEdges: ( ' || stringytoo || ' )' || chr(10);

      output := output || p_indent || 'coastal: ' || self.coastal || chr(10);

      stringy := '';

      FOR i IN 1 .. self.coastalEdges.COUNT
      LOOP

         stringy := stringy || TO_CHAR(self.coastalEdges(i));

         IF i <> self.coastalEdges.COUNT
         THEN

            stringy := stringy || ',';

         END IF;

      END LOOP;

      output := output || p_indent || 'coastalEdges: ( ' || stringy || ' )' || chr(10);

      output := output || p_indent || 'geometry: SDO_GEOMETRY( ' || TO_CHAR(self.sdogeometry.sdo_gtype) || ','  || TO_CHAR(self.sdogeometry.sdo_srid) || ','
                                   || ' NULL, <snip>... '
                                   || ' SDO_ORDINATE_ARRAY(' || self.sdogeometry.sdo_ordinates(1) || ',' || self.sdogeometry.sdo_ordinates(2) || '..<SNIP>..'
                                   || self.sdogeometry.sdo_ordinates(self.sdogeometry.sdo_ordinates.COUNT - 1) || ','
                                   || self.sdogeometry.sdo_ordinates(self.sdogeometry.sdo_ordinates.COUNT)
                                   ||  '))' || chr(10);

      output := output || p_indent || 'coastalSliver: ' || self.coastalSliver || chr(10);
      output := output || p_indent || 'coastalSliverClockD: ' || TO_CHAR(self.coastalSliverClockD) || chr(10);
      output := output || p_indent || 'coastalSliverCClockD: ' || TO_CHAR(self.coastalSliverCClockD) || chr(10);
      output := output || p_indent || 'ringCount: ' || TO_CHAR(self.ringCount) || chr(10);
      output := output || p_indent || 'dbug: ' || TO_CHAR(self.dbug) || chr(10);

      RETURN output;

   END Describe;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetBoundaryEdges
   AS

      bdy_edges         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

   BEGIN

      --this includes interior ring edges!
      --if the shape is complex, like an interior ring or a ring that touches the exterior
      --   at a point (not technically an interior ring) the edges won't be ordered nicely
      --   the way I'm assuming
      --will need to separate the 2 sets at some point

      self.signedBoundaryEdges := SDO_TOPO.GET_FACE_BOUNDARY(self.topology.name,
                                                             self.faceId,
                                                             'FALSE');  --default, in case of internal dangles, shouldnt be there

      bdy_edges.EXTEND(self.signedBoundaryEdges.COUNT);

      FOR i IN 1 .. self.signedBoundaryEdges.COUNT
      LOOP

         bdy_edges(i) := ABS(self.signedBoundaryEdges(i));

      END LOOP;

      self.boundaryEdges := bdy_edges;

   END SetBoundaryEdges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetCoastalEdges
   AS

      psql        VARCHAR2(4000);

   BEGIN

      IF self.boundaryEdges IS NULL
      OR self.boundaryEdges.COUNT = 0
      THEN

         self.SetBoundaryEdges();

      END IF;

      psql := 'SELECT e.edge_id '
           || 'FROM ' || self.topology.EdgeTable || ' e '
           || 'WHERE '
           || '(e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
           || 'e.edge_id IN (SELECT * FROM TABLE(:p3)) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO self.coastalEdges USING -1, -1,
                                                                        self.boundaryEdges;

      IF self.coastalEdges.COUNT > 0
      THEN

         self.coastal := 'Y';

      ELSE

         self.coastal := 'N';

      END IF;

   END SetCoastalEdges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetFeatureLayers
   AS

      --does NOT include face feature table.  Its a special table
      --   associated with self.Topolayer

      --klugey nomenclature - set featureLAYERS
      --results in tables and geoids attributes

      psql                    VARCHAR2(4000);
      all_layers              MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      feature_tables          MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      feature_table_geoids    MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      kount                   PLS_INTEGER := 0;
      current_layer_level     NUMBER;
      current_key             VARCHAR2(4000);

   BEGIN

      --get all poly layers in the topo that aren't the face feature table
      psql := 'SELECT a.table_name FROM user_sdo_topo_info a '
           || 'WHERE a.topology = :p1 AND '
           || 'a.tg_layer_type = :p2 AND '
           || 'a.table_name <> :p3 '
           || 'ORDER BY a.tg_layer_level, a.table_name ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO all_layers USING self.topology.name,
                                                                'POLYGON',
                                                                self.topoLayer.tableName;

      --loop through each and determine if the face participates in the layer, and what geoid

      FOR i IN 1 .. all_layers.COUNT
      LOOP

         current_layer_level := GZ_TOPO_UTIL.GET_TG_LAYER_LEVEL(self.topology.name,
                                                                all_layers(i),
                                                                'TOPOGEOM',
                                                                'POLYGON');

         BEGIN

            current_key := GZ_BUILD_SOURCE.GZ_GET_OID_FROM_FACE(USER,
                                                                self.topology.name,
                                                                all_layers(i),
                                                                current_layer_level,
                                                                self.faceId,
                                                                '1=1',      --always expects whereclause
                                                                'GEO_ID');  --calling this fixed now

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%no data found%'
            THEN

               --ORA-20001: ORA-01403: no data found
               current_key := NULL;

            ELSE

               RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;

         IF current_key IS NOT NULL
         THEN

            kount := kount + 1;
            feature_tables.EXTEND(1);
            feature_table_geoids.EXTEND(1);
            feature_tables(kount) := all_layers(i);
            feature_table_geoids(kount) := current_key;

         END IF;

      END LOOP;

      self.featureTables      := feature_tables;
      self.featureTableGeoids := feature_table_geoids;

   END SetFeatureLayers;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetCoastalSliver (
      p_value              IN VARCHAR2,
      p_direction          IN VARCHAR2 DEFAULT NULL,
      p_distance           IN NUMBER DEFAULT NULL
   )
   AS

      --Matt! 7/16/13
      --The original object method (circa 7/16/13)
      --    was a messy kitchen sink.
      --This is a simple setter.  See GZ_SLIVER for methods to
      --   determine if a face is a sliver, and what type

   BEGIN

      IF  self.coastal <> 'Y'
      AND self.coastalEdges.COUNT <> 1
      AND p_value <> 'NO'
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Lo siento amigo, current requirement is that coastal slivers must be coastal '
                                      || 'and have but one edge touching the coast.');

      END IF;

      IF UPPER(p_value) NOT IN ('SLIVER','PARTIAL','NO')
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Unknown sliver descriptor ' || p_value);

      ELSIF UPPER(p_value) IN ('SLIVER', 'NO')
      THEN

         self.coastalSliver := UPPER(p_value);
         self.coastalSliverClockD  := NULL;
         self.coastalSliverCClockD := NULL;

      ELSIF UPPER(p_value) = 'PARTIAL'
      THEN

         self.coastalSliver := UPPER(p_value);

         IF UPPER(p_direction) = 'CLOCKWISE'
         AND p_distance > 0
         THEN

            self.coastalSliverClockD := p_distance;

         ELSIF UPPER(p_direction) = 'COUNTERCLOCKWISE'
         AND p_distance > 0
         THEN

            self.coastalSliverCClockD := p_distance;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, 'Uknown sliver direction ' || p_direction || ' or distance ' || p_distance);

         END IF;

      END IF;

   END SetCoastalSliver;


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE DuplicateFaceRecord (
      p_face_id            IN NUMBER
   )
   AS

      --For this feature face, duplicate the values in the face feature table
      --using a new face_id (and NULL topogeom)

      psql              VARCHAR2(4000);
      cols              MDSYS.STRINGLIST := MDSYS.STRINGLIST();

   BEGIN

      psql := 'SELECT column_name FROM user_tab_cols '
           || 'WHERE table_name = :p1 AND '
           || 'column_name NOT LIKE :p2 '
           || 'ORDER BY column_id ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO cols USING self.topoLayer.tableName,
                                                          '%$';

      psql := 'INSERT INTO ' || self.topoLayer.tableName || '('
           || 'SELECT ';

      FOR i IN 1 .. cols.COUNT
      LOOP

         IF cols(i) = self.topoLayer.tableKey --face_id
         THEN

            psql := psql || 'CAST(:p1 AS NUMBER)';

         ELSIF cols(i) = 'TOPOGEOM'
         THEN

            psql := psql || 'NULL';

         ELSE

            psql := psql || 'a.' || cols(i);

         END IF;

         IF i <> cols.COUNT
         THEN

            psql := psql || ', ';

         ELSE

            psql := psql || ' ';

         END IF;

      END LOOP;

      psql := psql || 'FROM ' || self.topoLayer.tableName || ' a WHERE a.face_id = :p2) ';

      IF self.dbug = 1
      THEN
         dbms_output.put_line(psql || ' using ' || p_face_id || ',' || self.faceId);
      END IF;

      EXECUTE IMMEDIATE psql USING p_face_id,
                                   self.faceId;

      COMMIT;

   END DuplicateFaceRecord;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE MatchFeatures (
      p_neighbor_face      IN NUMBER
   )
   AS

      --M! 7/25/13
      --3/20/14 copied and modified from original feature face object

      --For any face feature pair, manage the FSLS built on top to allow a merge
      --The neighbor face is to be the winner.
      --In typical usage the self.face is a little guy getting swallowed

      --For 0-level FSLS:
      --  When geoids both exist for a fsl but value is diff across the face divide
      --     +/- Add the self.face to the neighbor fsl, subtract the self.face from the self FSL (may delete it)
      --  When neighbor fsl exists and self doesnt, add the self.face to the neighbor FSL
      --  When self fsl exists and neighbor doesnt, subtract the self.face from the self FSL (may delete it)
      --For 1+ level FSLS:
      --  Do nothing.  0 level should take care of it
      --  However caller must be aware of possible measurement changes in hierarchy if
      --     child FSL features were edited in the above

      neighbor_face        GzFeatureFace := GzFeatureFace();
      neighbor_fsls        MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      neighbor_geoids      MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      neighbor_layer       GzTopoLayer := GzTopoLayer();
      self_layer           GzTopoLayer := GzTopoLayer();
      self_position        NUMBER;
      neighbor_position    NUMBER;

   BEGIN

      --instantiate a face object for the neighbor
      neighbor_face := GzFeatureFace(p_neighbor_face,
                                     self.topology);

      --these are ordered and paired by position
      --and do not include the face feature table
      neighbor_fsls   := neighbor_face.GetFeatureTables;
      neighbor_geoids := neighbor_face.GetFeatureTableGeoids;

      IF self.dbug = 1
      THEN
         dbms_output.put_line('self before match: ');
         dbms_output.put_line(self.describe);
         dbms_output.put_line('neighbor before match: ');
         dbms_output.put_line(neighbor_face.describe);
      END IF;

      --do a future brother a solid and make a helpful schematic:

      --Part 1------------------------------------------------
      --NEIGHBOR_A | SELF_B   becomes   NEIGHBOR_A | SELF_A
      --NEIGHBOR_A | empty    becomes   NEIGHBOR_A | SELF_A
      --------------------------------------------------------

      FOR i IN 1 .. neighbor_fsls.COUNT
      LOOP

         --instantiate this layer that exists on the neighbor face
         neighbor_layer := GzTopoLayer(neighbor_fsls(i),
                                       self.topology.featureTableKey,
                                       'TOPOGEOM',
                                       self.topology);

         --overarching strategy
         --  When geoids exist for both fsls but value is diff across the face divide
         --     +/- Add the self.face to the neighbor fsl, subtract the self.face from the self FSL (may delete it)
         --  When neighbor fsl exists and self doesnt, add the self.face to the self FSL

         IF neighbor_layer.tgLayerLevel = 0  --higher levels ignored, they inherit
         THEN

            --does this table exist in self face layer hierarchy?
            self_position := GZ_BUSINESS_UTILS.QUERY_DELIMITED_LIST(self.featureTables,
                                                                    neighbor_fsls(i));

            IF self_position > 0  --Yes
            THEN

               --so instantiate the self layer too, we need it
                self_layer := GzTopoLayer(self.featureTables(self_position),
                                          self.topology.featureTableKey,
                                          'TOPOGEOM',
                                          self.topology);

               --geoids exist for both of the fsls - what are the values?

               IF self.featureTableGeoids(self_position) <>  neighbor_geoids(i)
               THEN

                  --Geoids dont match
                  --Add the self.face to the neighbor fsl - the neighbor takes over the self territory

                  neighbor_layer.AddFace(neighbor_geoids(i),  --the actual geo_id
                                         self.faceId);        --the face id to add to it

                  --subtract the self face from the self FSL (may delete it).  The self abandons its territory

                  self_layer.DeleteFace(self.featureTableGeoids(self_position),
                                        self.faceId);

               END IF;

            ELSE

               --neighbor fsl exists and self doesnt
               --Add the self face to the neighbor fsl - the neighbor takes over the self territory
               --same call as above, seems clearerer to separate the logic

               neighbor_layer.AddFace(neighbor_geoids(i),  --the actual geo_id
                                      self.faceId);        --the face id to add to it

            END IF;

         END IF;

      END LOOP;


      --Part 2------------------------------------------------
      -- empty | SELF_B   becomes   empty | empty
      --------------------------------------------------------

      FOR i IN 1 .. self.featureTables.COUNT
      LOOP

         --overarching strategy
         --  When self fsl exists and neighbor doesnt, subtract the self.face from the self FSL (may delete it)

         self_layer := GzTopoLayer(self.featureTables(i),
                                   self.topology.featureTableKey,
                                   'TOPOGEOM',
                                   self.topology);

         IF self_layer.tgLayerLevel = 0  --0 level only work
         THEN

            neighbor_position := GZ_BUSINESS_UTILS.QUERY_DELIMITED_LIST(neighbor_fsls,
                                                                        self.featureTables(i));

            IF neighbor_position = 0 --No, not in the list
            THEN

               --this feature table is not repped in the neighbor
               --subtract the self.face from the self FSL (may delete it).  The self abandons its territory

               self_layer.DeleteFace(self.featureTableGeoids(i),
                                     self.faceId);

            END IF;

         END IF;

      END LOOP;

      --Part 3--------------------------------------------------------------
      --NEIGHBORFACE_A | SELFFACE_B  becomes  NEIGHBORFACE_A | NEIGHBORFACE_B
      ---------------------------------------------------------------------

      --now that the the standard feature layers are done, take care of the feature faces themselves
      --Add the small face_id to the neighbor face feature

      --the self topo layer is the face feature table
      self.topoLayer.AddFace(TO_CHAR(neighbor_face.faceId),
                             self.faceId);


      --subtract the small face from the small feature face (will delete it usually)
      --   but sometimes during multi-step face collapses there's mitosis and the small face feature remains
      --   in increasingly smaller bits of territory
      --at this point our self object is a figment, we built it on the fly from pieces of the original
      --its face id doesnt correspond to anything in the feature face table,
      --and the primitive face already got subtracted from the remaining real feature face id

      BEGIN

         self.topoLayer.DeleteFace(TO_CHAR(self.faceId),
                                   self.faceId);



      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLERRM LIKE '%Got a face kount of 0%' --from gz_topo_primitive.delete_a_face
         THEN

            --When collapsing to universal the first time through here
            --removed the small face from the feature face record
            --this is sloppy
            --lets go back to raising here in the rewrite - see if it can be managed better in the caller
            RAISE_APPLICATION_ERROR(-20001, 'Revived this error: ' || SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

         END IF;

      END;

   END MatchFeatures;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE InitializeFaceKernel (
      p_face_id               IN NUMBER,
      p_topology              IN GzTopology
   )
   AS

      psql              VARCHAR2(4000);
      an_edge           GzTopoEdge;
      several_edges     GZ_TOPO_EDGE_LIST;
      kount             PLS_INTEGER;

   BEGIN

      self.faceId    := p_face_id;
      self.topology  := p_topology;

      self.topoLayer := GzTopoLayer(self.topology.GetFaceFeatureTable,
                                    'FACE_ID',
                                    'TOPOGEOM',
                                    self.topology);

      psql := 'SELECT a.topogeom.tg_id, a.topogeom.get_geometry() '
           || 'FROM ' || self.topoLayer.tableName || ' a '
           || 'WHERE '
           || 'a.face_id = :p1 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO self.tgId,
                                     self.sdoGeometry USING self.faceId;

      EXCEPTION
      WHEN OTHERS
      THEN

         --for instantiating temporary faces that arent in the face feature table
         --explicit
         self.tgId := NULL;
         self.sdoGeometry := NULL;

      END;

      psql := 'SELECT COUNT(*) FROM '
           || self.topology.FaceTable || ' f, '
           || 'TABLE(f.island_edge_id_list) t '
           || 'WHERE f.face_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO kount USING self.faceId;

      --face is always a 2003, add interior rings to total
      self.ringCount := kount + 1;
      
      --my dumb plan to use get_face_boundary to chain edges falls apart when there is a 
      --ring that touches the exterior at a point (technically not interior in OGC speak)
      --get_face_boundary returns edges in a jumbled order.  (same if there are true interior rings)
      --So test for this condition, call it a "ring" in this dumb world of mine (for now)
      --   and the code that just says no to interior rings will do the same for these
      --Need to write a true edge chainer soon
      
      psql := 'SELECT COUNT(*) FROM ('
           || 'SELECT node_id FROM ('
           || 'SELECT start_node_id node_id FROM ' || self.topology.EdgeTable || ' e '
           || 'WHERE e.left_face_id = :p1 OR e.right_face_id = :p2 '
           || 'UNION ALL '
           || 'SELECT end_node_id node_id FROM ' || self.topology.EdgeTable || ' e '
           || 'WHERE e.left_face_id = :p3 OR e.right_face_id = :p4 '
           || ') GROUP BY node_id '
           || 'HAVING COUNT(node_id) > :p5)';
      
      BEGIN
      
         EXECUTE IMMEDIATE psql INTO kount USING self.faceId,
                                                 self.faceId,
                                                 self.faceId,
                                                 self.faceId,
                                                 2;
                                                 
      EXCEPTION
      
         WHEN NO_DATA_FOUND
         THEN
            kount := 0;
         WHEN OTHERS
         THEN
            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace || ' on ' || psql);
         
      END;
                  
      --note that if interior rings also create a 3+ connect we get too many total                           
      self.ringCount := self.ringCount + kount;

      self.SetBoundaryEdges;

      self.SetCoastalEdges;

      several_edges := gz_topo_edge_list();

      FOR i IN 1 .. self.boundaryEdges.COUNT
      LOOP

        an_edge := GzTopoEdge(self.boundaryEdges(i),
                              self.faceId,
                              self.topology);

        several_edges.EXTEND();
        several_edges(i) := an_edge;

      END LOOP;

      self.topoEdges := several_edges;

   END InitializeFaceKernel;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SplitFaceObject (
      p_sdogeometry        IN MDSYS.SDO_GEOMETRY,
      left_face            OUT GzFeatureFace,
      right_face           OUT GzFeatureFace
   )
   AS

      --Best call this one usually, ala
      --      declare
      --         thistopo   GzTopology := GzTopology();
      --         thisface   GzFeatureFace := GzFeatureFace();
      --         leftface   GzFeatureFace;
      --         rightface  GzFeatureFace;
      --         splitter sdo_geometry;
      --      BEGIN
      --         splitter := SDO_GEOMETRY ( ...);
      --         thistopo := GzTopology ('SCHEL010', 'Z899IN');
      --         thisface := GzFeatureFace (432, thistopo);
      --         thisface.SplitFaceObject(splitter,leftface,rightface);
      --         leftface.describe; --etc
      --      END;

      new_edge_id          NUMBER;
      psql                 VARCHAR2(4000);
      left_face_id         NUMBER;
      right_face_id        NUMBER;

   BEGIN


      new_edge_id := self.split(p_sdogeometry,
                                0);

      psql := 'SELECT e.left_face_id, e.right_face_id '
           || 'FROM ' || self.topology.EdgeTable || ' e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO left_face_id,
                                  right_face_id USING new_edge_id;

      left_face := GzFeatureFace(left_face_id,
                                 self.topology,
                                 self.GetFeatureTables,
                                 self.GetFeatureTableGeoids);

      right_face := GzFeatureFace(right_face_id,
                                  self.topology,
                                  self.GetFeatureTables,
                                  self.GetFeatureTableGeoids);


   END SplitFaceObject;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE SetDbug
   AS

   BEGIN

      --different levels?
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