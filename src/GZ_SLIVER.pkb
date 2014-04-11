CREATE OR REPLACE PACKAGE BODY GZ_SLIVER
AS

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE DBUG_THIS (
      p_dbug               IN VARCHAR2,
      p_sql                IN VARCHAR2
   )
   AS

      psql           VARCHAR2(4000);

      --1 is standard usage from an object.
      --2 is for code debugging, ie compiling new test code on the fly
      --    GZ_SLIVER.DBUG_THIS(2,'boo');
      --ELSE is for dealing with super fubars where dbms_output isnt getting returned
      --   ex dbug = 'MYFOO', get dbms outputs in this table
      --   create table myfoo (psql varchar2(4000), thetime TIMESTAMP)

   BEGIN

      IF p_dbug = '1'
      THEN

         dbms_output.put_line(p_sql);

      ELSIF p_dbug = '2'
      THEN

         RAISE_APPLICATION_ERROR(-20001, p_sql);

      ELSE

         psql := 'INSERT /*+ APPEND */ INTO ' || p_dbug || ' VALUES(:p1,:p2)';
         EXECUTE IMMEDIATE psql USING p_sql,
                                      systimestamp;

         COMMIT;

      END IF;

   END DBUG_THIS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION MEASURE_FACE_TO_UNIVERSAL (
      p_direction          IN VARCHAR2,
      p_cutoff_distance    IN NUMBER,
      p_FeatureFace        IN GzFeatureFace
   ) RETURN NUMBER
   AS

      --3/21/14 copied out and modified from the original feature face code
      --return the length of the segments traversed or -1 if full traversal

      --Walk the faces' edges inward, vertex by vertex (or node, if a new edge)
      --Measure the width from the vertex/node to the UF
      --If the face width at a vertex/node <= p_cutoff_distance
      --   Tally the running length of the segments traversed from the UF, continue...
      --If the width becomes greater than the cutoff
      --   Return the current tally of the measurement - this may be a partial sliver
      --If the walk reaches the universal face on the opposite side, return -1 - total sliver

      ordered_signed_edges             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      TopoEdge                         GzTopoEdge;
      reverse_flag                     VARCHAR2(1);
      running_length                   NUMBER := 0;
      current_edge_length              NUMBER := 0;

   BEGIN

      IF p_direction NOT IN ('CLOCKWISE','COUNTERCLOCKWISE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Invalid p_direction ' || p_direction);

      END IF;

      IF p_FeatureFace.coastalEdges.COUNT <> 1
      OR p_FeatureFace.coastalEdges IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Need exactly one coastal edge in current implementation');

      END IF;

      --get the edges in this direction
      ordered_signed_edges := p_FeatureFace.GetSignedInteriorEdges(p_direction);

      FOR i IN 1 .. ordered_signed_edges.COUNT
      LOOP

         --measure each edge

         TopoEdge := p_featureface.GetTopoEdge(ABS(ordered_signed_edges(i)));

         IF (p_direction = 'COUNTERCLOCKWISE' AND ordered_signed_edges(i) < 0)
         OR (p_direction = 'CLOCKWISE' AND ordered_signed_edges(i) > 0)
         THEN

            --must reverse edges that run opposite to our current walk
            reverse_flag := 'Y';

         ELSE

            reverse_flag := 'N';

         END IF;

         current_edge_length := GZ_SLIVER.MEASURE_EDGE_TO_UNIVERSAL(TopoEdge,
                                                                    p_cutoff_distance,
                                                                    reverse_flag);

         --pulling some Sidey tricks
         --if the return value is positive, we havent blown p_cutoff_distance on the edge and continue
         --if negative (or zero, for nothing tallied) we exceeded the sliver cutoff and tally what we have + what we got back on this one

         IF current_edge_length > 0
         THEN

            --didnt exceed cutoff anywhere on the edge
            running_length := running_length + current_edge_length;

         ELSE

            --exceeded the cutoff somewhere midway along the edge
            --add it to the total and done

            running_length := running_length + ABS(current_edge_length);

            RETURN running_length;

         END IF;

      END LOOP;

      --if we made it this far then no edges on the walk exceeded the p_cutoff_distance
      --and we have a full sliver

      RETURN -1;

   END MEASURE_FACE_TO_UNIVERSAL;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION MEASURE_UNIVERSAL_TO_FACE (
      p_direction          IN VARCHAR2,
      p_cutoff_distance    IN NUMBER,
      p_FeatureFace        IN GzFeatureFace
   ) RETURN NUMBER
   AS


      --walk the coastal edge vertex by vertex
      --measure the face width from the vertex to the nearest face interior edge
      --If the face_width at a vertex/node <= p_cutoff_distance
      --    Tally the running length of segments traversed on the UF
      --If the width becomes greater
      --   Return the current tally of the length.  This value isnt used for anything
      --If the walk reaches the end of the edge return -1

      --20140321 copied and modified from original in gz_feature_face
      --Only realized I needed to measure this way too after seeing some bad results
      --return the length of the segments traversed or -1 if full traversal

      ordered_edges                    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      uf_edge_geometry                 SDO_GEOMETRY;
      vtx_geometry                     SDO_GEOMETRY;
      psql                             VARCHAR2(4000);
      edge_found                       PLS_INTEGER;
      prev_geometry                    SDO_GEOMETRY;
      running_length                   NUMBER := 0;
      coastal_edges                    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

   BEGIN

      IF p_direction NOT IN ('CLOCKWISE','COUNTERCLOCKWISE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Invalid p_direction ' || p_direction);

      END IF;

      coastal_edges := p_FeatureFace.GetCoastalEdges;

      IF coastal_edges.COUNT <> 1
      OR coastal_edges IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Need exactly one coastal edge in current implementation');

      END IF;

      --get the edges in this direction, order doesnt actually matter
      ordered_edges := p_FeatureFace.GetInteriorEdges(p_direction);

      uf_edge_geometry := p_FeatureFace.GetTopoEdge(coastal_edges(1)).GetGeometry;

      FOR i IN 1 .. uf_edge_geometry.SDO_ORDINATES.COUNT/2
      LOOP

         IF i = 1
         OR i = uf_edge_geometry.SDO_ORDINATES.COUNT/2
         THEN

            --just one edge to measure on the UF
            --so the first and last vtx are always within distance of an interior
            edge_found := 1;

         ELSE

            --measure each vtx
            vtx_geometry := SDO_GEOMETRY(2001,
                                         p_FeatureFace.topology.srid,
                                         SDO_POINT_TYPE(uf_edge_geometry.SDO_ORDINATES( (i * 2) - 1),
                                                        uf_edge_geometry.SDO_ORDINATES( (i * 2)),
                                                        NULL),
                                          NULL,
                                          NULL);

            psql := 'SELECT COUNT(edge_id) FROM ('
                 || 'SELECT /*+ FIRST_ROWS */ e.edge_id, sdo_nn_distance(1) dist '
                 || 'FROM ' || p_FeatureFace.topology.EdgeTable || ' e '
                 || 'WHERE e.edge_id IN (SELECT * FROM TABLE(:p1)) AND '
                 || 'SDO_NN(e.geometry, :p2, :p3, 1) = :p4 '
                 || 'ORDER BY DIST '
                 || ') WHERE rownum = 1 ';

            EXECUTE IMMEDIATE psql INTO edge_found USING ordered_edges,
                                                         vtx_geometry,
                                                         'sdo_batch_size=2 dist=' || p_cutoff_distance,
                                                         'TRUE';

         END IF;

         IF edge_found = 0
         THEN

            --there are no edges in the interior within p_cutoff_distance of this vtx on the UF
            --so this distance is to be to be a sliver
            RETURN (0 - running_length);  --may be zero

         ELSE

            --this vertex is still within p_cutoff_distance

            IF i = 1
            THEN

               --nothing to measure yet, store the position
               prev_geometry := vtx_geometry;

            ELSE

               running_length := running_length + SDO_GEOM.SDO_DISTANCE(prev_geometry,
                                                                        vtx_geometry,
                                                                        p_FeatureFace.topology.tolerance);

               prev_geometry := vtx_geometry;

            END IF;

         END IF;

      END LOOP;

      --if we made it this far then no edges on the walk exceeded the p_cutoff_distance
      --and we have a full sliver

      RETURN -1;

   END MEASURE_UNIVERSAL_TO_FACE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION MEASURE_EDGE_TO_UNIVERSAL (
      p_TopoEdge           IN GzTopoEdge,
      p_cutoff_distance    IN NUMBER,
      p_reverse_flag       IN VARCHAR2
   ) RETURN NUMBER
   AS

      --20140314 Matt! copied and modified from the original gz_feature_face object

      --for a single edge and a given cutoff distance where we stop measuring
      --measure at each edge vertex the projection to the universal
      --if its less than the cutoff distance, keep a running tally of how far we have walked
      --if we exceed the cutoff distance tally up the current length of the walk and return it negative
      --   to indicate the cutoff distance was exceeded

      running_length          NUMBER := 0;
      vtx_geometry            SDO_GEOMETRY;
      prev_geometry           SDO_GEOMETRY;
      psql                    VARCHAR2(4000);
      edge_found              PLS_INTEGER;
      edge_geometry           SDO_GEOMETRY;

   BEGIN

      IF p_reverse_flag = 'Y'
      THEN

         edge_geometry := SDO_UTIL.REVERSE_LINESTRING(p_TopoEdge.geometry);

      ELSE

         edge_geometry := p_TopoEdge.geometry;

      END IF;

      FOR i IN 1 .. edge_geometry.SDO_ORDINATES.COUNT/2
      LOOP

         vtx_geometry := SDO_GEOMETRY(
                         2001,
                         p_TopoEdge.topology.srid,
                         SDO_POINT_TYPE(
                                        edge_geometry.SDO_ORDINATES( (i * 2) - 1),
                                        edge_geometry.SDO_ORDINATES( (i * 2)),
                                        NULL),
                         NULL,
                         NULL);

         IF p_TopoEdge.dbug = 1
         THEN
            dbms_output.put_line('measuring edge ' || p_TopoEdge.edgeId || ' from ');
            dbms_output.put_line(gz_geom_utils.dump_sdo(vtx_geometry));
         END IF;

         psql := 'SELECT COUNT(edge_id) FROM ('
              || 'SELECT /*+ FIRST_ROWS */ e.edge_id, sdo_nn_distance(1) dist '
              || 'FROM ' || p_TopoEdge.topology.EdgeTable || ' e '
              || 'WHERE (e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
              || 'SDO_NN(e.geometry, :p3, :p4, 1) = :p5 '
              || 'ORDER BY DIST '
              || ') WHERE rownum = 1 ';


         IF p_TopoEdge.dbug = 1
         THEN
            dbms_output.put_line(psql || ' ' || ' using ' || '-1,-1,see above,'
                                      || 'sdo_batch_size=2 dist=' || p_cutoff_distance ||',TRUE');
         END IF;

         EXECUTE IMMEDIATE psql INTO edge_found USING -1, -1,
                                                       vtx_geometry,
                                                       'sdo_batch_size=2 dist=' || p_cutoff_distance,
                                                       'TRUE';

         IF p_TopoEdge.dbug = 1
         THEN
            dbms_output.put_line('Found ' || edge_found || ' universal edges');
         END IF;

         IF edge_found = 0
         THEN

            --there are no edges on the universal face within p_cutoff_distance at this location
            RETURN (0 - running_length);  --may be zero

         ELSE

            --this vertex is still within p_cutoff_distance

            IF i = 1
            THEN

               --nothing to measure yet, store the position
               prev_geometry := vtx_geometry;

            ELSE

               running_length := running_length + SDO_GEOM.SDO_DISTANCE(prev_geometry,
                                                                        vtx_geometry,
                                                                        p_TopoEdge.topology.tolerance);

               prev_geometry := vtx_geometry;

            END IF;

            IF p_TopoEdge.dbug = 1
            THEN
               dbms_output.put_line('running length is ' || running_length);
            END IF;

         END IF;

      END LOOP;

      --made it all the way across the edge without exceeding p_cutoff_distance
      --return the distance we traversed

      IF running_length <> 0
      THEN

         RETURN running_length;

      ELSE

         --we measured 2 or more vertices as within distance
         --but the running length of that traversal is 0
         --suggests edges that are below tolerance, ie tiny coastal sliver triangles
         --sdo_geom.sdo_length seems to be oblivious to .05 tolerance, unlike distance
         RETURN SDO_GEOM.SDO_LENGTH(edge_geometry, p_TopoEdge.topology.tolerance);

      END IF;

   END MEASURE_EDGE_TO_UNIVERSAL;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

    PROCEDURE COASTAL_SLIVER_DETERMINATION (
      p_FeatureFace        IN OUT GzFeatureFace,
      p_cutoff_distance    IN NUMBER,
      p_partial_length     IN NUMBER
   )
   AS

      --is this face a coastal sliver according to the input parameters?
      --if so what type?
      --Set the IN/OUT face object properties correctly
      --20140321 - rewrite of original in gz_feature_face

      --SAMPLE stub
      --declare
      --   thistopo   GzTopology  := GzTopology();
      --   thisface   GzFeatureFace  := GzFeatureFace();
      --begin
      --   thistopo  := GzTopology('SCHEL010','Z899IN');
      --   thisface := GzFeatureFace(510, thistopo);
      --
      --   GZ_SLIVER.COASTAL_SLIVER_DETERMINATION(thisface,
      --                                          1270,
      --                                          2540);
      --   thisface.describe;
      --end;


     --Steps
      --1. Check if face has only 1 edge on the universal face
      --      If not, exit, this is not a coastal sliver by current defs
      --2. Start counterclockwise, measure the face distance to the universal
      --3. Decide what to do with the counterclockwise direction
      --      If the measurer returns -1 then this is a sliver.  Set SLIVER
      --      If the measurer returns zed or a positive number, compare to p_partial length
      --         If the returned value is greater then p_partial_length, set PARTIAL
      --         set coastal_sliver_dir to COUNTERCLOCKWISE
      --4. If not total sliver determined, walk in a clockwise direction
      --      Should not get a sliver in this direction unless something is wack
      --      If the measurer returns zed or a positive number compare to p_partial length
      --         If the returned value (clockwise) is greater than p_partial_length, set PARTIAL
      --            set coastal_sliver_dir to CLOCKWISE


       measure_cclock         NUMBER;
       measure_clock          NUMBER;
       measure_u_2face        NUMBER;
       coastal_edges          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
       boundary_edges         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

   BEGIN

      coastal_edges  := p_FeatureFace.GetCoastalEdges;
      boundary_edges := p_FeatureFace.GetBoundaryEdges;

      IF coastal_edges.COUNT <> 1
      THEN

         --if 0 edges shouldnt be here.  If 2 edges on coast no code to deal (yet)
         p_FeatureFace.SetCoastalSliver('NO');
         RETURN;

      ELSIF coastal_edges.COUNT = 1
      AND boundary_edges.COUNT = 1
      THEN

         --cant do anything for islands with no interior edges
         p_FeatureFace.SetCoastalSliver('NO');
         RETURN;

      ELSIF p_FeatureFace.ringCount > 1
      THEN

         --present code cant handle slivers that contain inner rings
         p_FeatureFace.SetCoastalSliver('NO');
         RETURN;

      END IF;

      --returns the distance, or -1 if traverse the interior edges
      measure_cclock := GZ_SLIVER.MEASURE_FACE_TO_UNIVERSAL('COUNTERCLOCKWISE',
                                                            p_cutoff_distance,
                                                            p_FeatureFace);

      IF measure_cclock < 0
      THEN

         --Usually this is a sliver, but need to check the other way, in case the
         --vertices on the interior edges are sparse

         --Direction isnt really important.  CC matches the edge definitions
         measure_u_2face := GZ_SLIVER.MEASURE_UNIVERSAL_TO_FACE('COUNTERCLOCKWISE',
                                                                p_cutoff_distance,
                                                                p_FeatureFace);

         IF measure_u_2face < 0
         THEN

            p_FeatureFace.SetCoastalSliver('SLIVER');

            --done
            RETURN;

         ELSE

            --Just gonna measure a false sliver from the other direction
            --get out
            p_FeatureFace.SetCoastalSliver('NO');
            RETURN;

         END IF;

      END IF;

      --either a partial sliver, or a nothing if we get here

      IF measure_cclock >= 0
      AND measure_cclock > p_partial_length  --something, at least partial in this dir
      THEN

         p_FeatureFace.SetCoastalSliver('PARTIAL',
                                        'COUNTERCLOCKWISE',
                                        measure_cclock);
         --dont return, check other dir

      ELSE

         --set it to nothing.  Clockwise check below will reset to PARTIAL if it finds it
         p_FeatureFace.SetCoastalSliver('NO');

      END IF;


      --returns the distance, or -1 if traverse the interior edges
      measure_clock := GZ_SLIVER.MEASURE_FACE_TO_UNIVERSAL('CLOCKWISE',
                                                            p_cutoff_distance,
                                                            p_FeatureFace);


      IF measure_clock < 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Howd we get a sliver walk clockwise but not counterclockwise?');

      ELSE

         IF measure_clock > p_partial_length  --ie yup
         THEN

            --if already a partial in the other dir, this will simply add the clockwise distance to the attributes
            p_FeatureFace.SetCoastalSliver('PARTIAL',
                                           'CLOCKWISE',
                                           measure_clock);

         END IF;

      END IF;

   END COASTAL_SLIVER_DETERMINATION;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------


   FUNCTION GET_COASTAL_OPPOSING_EDGES (
      p_FeatureFace        IN GzFeatureFace,
      p_direction          IN VARCHAR2,
      p_percent            IN NUMBER DEFAULT 1
   ) RETURN MDSYS.SDO_LIST_TYPE
   AS

      --! 3/26/14
      --Moved here and modified from the original gz_feature_face

      --Get edges opposite the universal face - only those "inside" the sliver
      --   that can be used to chomp down from a neighboring face
      --Edges are returned with their signs
      --"Inside" is defined as having all vertices that project onto the universal edge
      --   within p_percent of the UF start/end points

      --Will be using a simplified understanding of chomping
      --  The nodes bordering an opposing edge are either both droppable, fully interiors (then edge removal between the nodes)
      --  Or 1 node is droppable for either the UF-touching edge or the next edge (then edge removal)
      --This means that the only edges we will test for dropability are the 2 that touch the UF,
      --  all others are assumed to chomp in some fashion

      --If there is just one edge, its the one
      --If there are two edges and no node is droppable, go with the longest

      --This isn't ready for partials yet, though its close
      --needs to know when to stop traversing

      --SAMPLE tester
      --declare
      --   thistopo   GzTopology  := GzTopology();
      --   thisface   GzFeatureFace  := GzFeatureFace();
      --   oppedges   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      --begin
      --   thistopo  := GzTopology('SCHEL010','Z899IN');
      --   thisface := GzFeatureFace(297, thistopo);
      --   thisface.SetCoastalSliver('SLIVER');
      --   oppedges := GZ_SLIVER.GET_COASTAL_OPPOSING_EDGES(thisface, 'COUNTERCLOCKWISE');
      --   for i IN 1 .. oppedges.count
      --   loop
      --      dbms_output.put_line(oppedges(i));
      --   end loop;
      --end;

      output                  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      output_kount            PLS_INTEGER := 0;
      signed_interior_edges   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      universal_edges         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      universal_geom          SDO_GEOMETRY;
      testing_node_pos        VARCHAR2(5);
      testing_node_geom       SDO_GEOMETRY;
      testing_measure         NUMBER;
      psql                    VARCHAR2(4000);
      max_length              NUMBER := 0;
      max_i                   PLS_INTEGER;
      dbug                    VARCHAR2(32);

   BEGIN

      IF p_FeatureFace.GetDbug IS NOT NULL
      THEN

         dbug := TO_CHAR(p_FeatureFace.GetDbug);
         ----dbug := 'MYFOO';

      END IF;


      IF p_percent >= 50
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Percentage is measured from both ends, so dont go bigger than 50');

      END IF;

      --Work counterclockwise usually for vanilla slivers

      --Returns ordered and signed array of interior edges starting at the universal edge
      --either from clockwise side or counterclockwise
      signed_interior_edges := p_FeatureFace.GetSignedInteriorEdges(p_direction);

      IF signed_interior_edges.COUNT = 1
      THEN

         --if theres just one interior edge bounding the sliver, its the one
         --this is often the case
         output.EXTEND(1);
         output(1) := signed_interior_edges(1);
         RETURN output;

      END IF;

      --get the edge to use in the LRS
      universal_edges := p_FeatureFace.GetCoastalEdges;
      universal_geom :=  p_FeatureFace.GetTopoEdge(universal_edges(1)).GetGeometry;

      --null the srid
      universal_geom.sdo_srid := NULL;

      FOR i IN 1 .. (signed_interior_edges.COUNT)
      LOOP

         IF i = 1
         OR i = (signed_interior_edges.COUNT)
         THEN

            --1st and last, the UF touchers, get special testing treatment to see if they
            --really hang over the sliver and can chomp down on it

            IF (p_direction = 'COUNTERCLOCKWISE' AND signed_interior_edges(i) < 0)
            OR (p_direction = 'CLOCKWISE' AND signed_interior_edges(i) > 0)
            THEN

               IF i = 1
               THEN
                  --get start node for the edge for new drop test
                  testing_node_pos := 'start';
               ELSE
                  --opposite for the last edge coming back to the UF
                  testing_node_pos := 'end';
               END IF;

            ELSE

               IF i = 1
               THEN
                  --get end node for new drop test
                  testing_node_pos := 'end';
               ELSE
                  testing_node_pos := 'start';
               END IF;

            END IF;

            psql := 'SELECT n.geometry '
                 || 'FROM '
                 || p_FeatureFace.topology.NodeTable() || ' n, '
                 || p_FeatureFace.topology.EdgeTable() || ' e '
                 || 'WHERE '
                 || 'e.edge_id = :p1 AND '
                 || 'e.' || testing_node_pos || '_node_id = n.node_id ';

            EXECUTE IMMEDIATE psql INTO testing_node_geom USING ABS(signed_interior_edges(i));

            testing_node_geom.sdo_srid := NULL;

            --perform projection and locate the measure on the Universal Face edge
            --my LRS measures are always between 0 and 1000
            testing_measure := GZ_GEOM_UTILS.GZ_FIND_MEASURE_PERCENT(GZ_GEOM_UTILS.GZ_PROJECT_PT(testing_node_geom,
                                                                                                 universal_geom,
                                                                                                 p_FeatureFace.topology.GetNullTolerance),
                                                                     universal_geom);

            IF dbug IS NOT NULL
            THEN
               GZ_SLIVER.DBUG_THIS(dbug, 'measure is ' || testing_measure);
            END IF;

            IF (testing_measure > p_percent) AND        --usually between 1 and 99 is the deal here
               (testing_measure < (100 - p_percent))
            THEN

               IF dbug IS NOT NULL
               THEN
                  GZ_SLIVER.DBUG_THIS(dbug, 'keeping ' || signed_interior_edges(i));
               END IF;

               output_kount := output_kount + 1;
               output.EXTEND(1);
               output(output_kount) := signed_interior_edges(i);

            ELSE

               IF dbug IS NOT NULL
               THEN
                  GZ_SLIVER.DBUG_THIS(dbug, 'rejecting ' || signed_interior_edges(i));
               END IF;

            END IF;

         ELSE

            IF dbug IS NOT NULL
            THEN
               GZ_SLIVER.DBUG_THIS(dbug, 'edge ' || signed_interior_edges(i) || ' goes in the bucket no matter what');
            END IF;

            --all others go in the bucket no matter what
            --how they actually chomp gets determined later
            output_kount := output_kount + 1;
            output.EXTEND(1);
            output(output_kount) := signed_interior_edges(i);

         END IF;

      END LOOP;

      IF output.COUNT = 0
      THEN

         IF dbug IS NOT NULL
         THEN
            GZ_SLIVER.DBUG_THIS(dbug, 'in length measurer');
         END IF;


         --take the longest edge bordering the sliver
         --  Most likely this a sliver with 2 edges bordering it
         --  and the 1 node between them is offset from the UF edge
         --  A skinny right triangle, basically
         FOR i IN 1 .. (signed_interior_edges.COUNT)
         LOOP

            IF SDO_GEOM.SDO_LENGTH(p_FeatureFace.GetTopoEdge(ABS(signed_interior_edges(i))).GetGeometry,
                                   p_FeatureFace.Topology.GetTolerance) > max_length
            THEN

               --max length initialized to 0 so theres always a winner
               max_length := SDO_GEOM.SDO_LENGTH(p_FeatureFace.GetTopoEdge(ABS(signed_interior_edges(i))).GetGeometry,
                                                 p_FeatureFace.Topology.GetTolerance);
               max_i      := i;

            END IF;

         END LOOP;

         output.EXTEND(1);
         output(1) := signed_interior_edges(max_i);

      END IF;

      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Didnt come up with an opposing edge');

      ELSE

         RETURN output;

      END IF;


   END GET_COASTAL_OPPOSING_EDGES;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE COLLAPSE_FACE_FROM_EDGES (
      p_FeatureFace        IN OUT GzFeatureFace,
      p_edges              IN MDSYS.SDO_LIST_TYPE,
      p_depth              IN NUMBER DEFAULT 0
   )
   AS

      --takes in a face and a list of counterclockwise directed edges that are gonna chomp the face
      --but works on just one edge at a time, calling itself recursively

      --split the face at the "end" (counterclockwise) of the first edge
      --remove the first edge from the topology
      --remove the first edge from the list
      --take the right face from the face split, and pass it and the updated edge list on to another level
      --the input p_FeatureFace should always come correct (tm), representing the real state of the face to be split

      --when (or if, first time) theres just one edge in the list
      --dont call split, just remove it and return back up the stack

      --sample stub
      --declare
      --   thistopo   GzTopology := GzTopology();
      --   thisface   GzFeatureFace := GzFeatureFace();
      --   edgez      mdsys.sdo_list_type := mdsys.sdo_list_type();
      --BEGIN
      --   thistopo := GzTopology ('SCHEL010', 'Z899IN');
      --   thisface := GzFeatureFace (466, thistopo);
      --   thisface.setdbug;
      --   edgez.extend(3);
      --   edgez(1) := -27240;
      --   edgez(2) := -27239;
      --   edgez(3) := 26872;
      --   gz_sliver.collapse_face_from_edges(thisface, edgez);
      --end;

      coastal_edges           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      opposing_edge           NUMBER;
      node_pos                VARCHAR2(5);
      drop_node_id            NUMBER;
      drop_node_geom          SDO_GEOMETRY;
      uf_edge                 GzTopoEdge := GzTopoEdge();
      remove_edge             GzTopoEdge := GzTopoEdge();
      new_node_geom           SDO_GEOMETRY;
      LeftFace                GzFeatureFace;
      RightFace               GzFeatureFace;
      new_edge_geom           SDO_GEOMETRY;
      next_edges              MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      dbug                    VARCHAR2(32);
      bail_out                PLS_INTEGER := 0;
      bail_msg                VARCHAR2(4000);

   BEGIN

      IF p_depth > 100
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Prove that a face has 100+ edges to collapse');

      END IF;

      IF p_FeatureFace.GetDbug IS NOT NULL
      THEN

         dbug := TO_CHAR(p_FeatureFace.GetDbug);
         ----dbug := 'MYFOO';

      END IF;

      coastal_edges := p_FeatureFace.GetCoastalEdges;

      opposing_edge := p_edges(1);

      --establish factoids

      IF opposing_edge > 0
      THEN

         --going counterclockwise, so edge dir matches
         node_pos         := 'END';

      ELSE

         --edge is reversed relative to counterclockwise walk
         node_pos         := 'START';

      END IF;

      IF dbug IS NOT NULL
      THEN
         GZ_SLIVER.DBUG_THIS(dbug, 'at depth ' || p_depth || ' opposing edge is ' || opposing_edge || ' node pos ' || node_pos);
      END IF;

      IF p_edges.COUNT > 1
      THEN

         --split the face at the "end" (counterclockwise) of the first edge
         --This just returns either the start or end node for the opposing edge
         drop_node_id := p_FeatureFace.GetTopoEdge(ABS(opposing_edge)).GetNodeId(node_pos);

         IF dbug IS NOT NULL
         THEN
            GZ_SLIVER.DBUG_THIS(dbug, 'drop node id is ' || drop_node_id);
         END IF;

         drop_node_geom := p_FeatureFace.GetTopoEdge(ABS(opposing_edge)).GetNodeGeometry(node_pos);

         --this is the coastal sliver shortcut that prevents direct reuse in other edits
         --_this_ is the reason why I should be getting poor performance reviews!
         uf_edge := p_FeatureFace.GetTopoEdge(coastal_edges(1));

         new_node_geom := uf_edge.GetProjectPt(drop_node_geom);

         new_edge_geom := SDO_GEOMETRY(2002,
                                       p_FeatureFace.Topology.GetSrid,
                                       NULL,
                                       SDO_ELEM_INFO_ARRAY(1,2,1),
                                       SDO_ORDINATE_ARRAY(drop_node_geom.sdo_point.X,
                                                          drop_node_geom.sdo_point.Y,
                                                          new_node_geom.sdo_point.X,   --face.split should add this node
                                                          new_node_geom.sdo_point.Y)
                                       );

         IF dbug IS NOT NULL
         THEN
            GZ_SLIVER.DBUG_THIS(dbug, 'Splitting face ' ||  p_FeatureFace.GetFaceId || ' with '
                                || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(new_edge_geom)));
         END IF;

         BEGIN

            p_FeatureFace.SplitFaceObject(new_edge_geom,
                                          LeftFace,    --OUT
                                          RightFace);  --OUT

         EXCEPTION
         WHEN OTHERS
         THEN

            bail_out := 1;
            bail_msg := SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace;

            IF dbug IS NOT NULL
            THEN
               GZ_SLIVER.DBUG_THIS(dbug, 'Failed to split face. Error and trace: '
                                   || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(new_edge_geom)));
            END IF;

         END;

      END IF;

       --remove the first (or final/only) edge from the topology

      IF p_edges.COUNT > 1
      AND bail_out = 0
      THEN

         --dont take him from the original p_featureface list, that face is toasted
         remove_edge := GzTopoEdge(ABS(opposing_edge),
                                   LeftFace.GetFaceId,
                                   LeftFace.Topology);

      ELSE

         --remove the final edge, or the edge we are on if the drop failed
         remove_edge := GzTopoEdge(ABS(opposing_edge),
                                   p_FeatureFace.GetFaceId,
                                   p_FeatureFace.Topology);

      END IF;

      IF dbug IS NOT NULL
      THEN
         GZ_SLIVER.DBUG_THIS(dbug, 'removing edge ' || ABS(opposing_edge));
      END IF;

      remove_edge.Remove;

      IF p_edges.COUNT > 1
      AND bail_out = 0
      THEN

         --remove the first edge from the list

         next_edges.EXTEND(p_edges.COUNT - 1);

         FOR i IN 1 .. (p_edges.COUNT - 1)
         LOOP

            next_edges(i) := p_edges(i+1);

         END LOOP;

         --take the right face from the face split, and pass it and the list on to another level

         IF dbug IS NOT NULL
         THEN
            GZ_SLIVER.DBUG_THIS(dbug, 'calling recursively, now with face ' || RightFace.GetFaceId);
            --send it down
            RightFace.SetDbug;
         END IF;

         GZ_SLIVER.COLLAPSE_FACE_FROM_EDGES(RightFace,
                                            next_edges,
                                            (p_depth + 1));

      ELSIF bail_out = 1
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Failed in adding new nodes and edges and tidied up. Error was ' || bail_msg);

      ELSE

         --success and done
         RETURN;

      END IF;

   END COLLAPSE_FACE_FROM_EDGES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE COLLAPSE_FACE_TO_UNIVERSAL (
      p_FeatureFace        IN OUT GzFeatureFace
   )
   AS

      --high level overview
      --1. Get the counterclockwise "opposing edges" for the face
      --2. Get all start/end nodes of the opposing edges.  Set them aside
      --3. Call GZ_SLIVER.COLLAPSE_FACE_FROM_EDGES, giving it the face and the opposing edge(s)
      --     Which will split the face, and remove the first(left side) edge
      --     If there are more opposing edges, it will recursively call itself
      --         giving itself the right side face from the split and the remaining opposing edges
      --     If there's only one opposing edge remaining it simply removes the edge
      --4. Remove the nodes stored from 2.  Kindly for the UF nodes, Zap for interior

      dbug                          VARCHAR2(32);
      coastal_edges                 MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      opposing_directed_edges       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      all_nodes                     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      temp_nodes                    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      removed_node                  NUMBER;
      bail_out                      PLS_INTEGER := 0;
      bail_msg                      VARCHAR2(4000);

   BEGIN

      IF p_FeatureFace.GetDbug IS NOT NULL
      THEN

         dbug := TO_CHAR(p_FeatureFace.GetDbug);
         --dbug := 'MYFOO';

      END IF;

      coastal_edges := p_FeatureFace.GetCoastalEdges;

      IF coastal_edges.COUNT <> 1
      THEN

         --I think this should work for any single-edge coastal face. Doesnt have to be a sliver
         RAISE_APPLICATION_ERROR(-20001, 'Sorry peaches, I only know how to collapse coastal faces with 1 edge on the coast');

      END IF;

      --1. Get the counterclockwise "opposing edges" for the face
      --Counterclockwise so direction matches the signs in the array
      opposing_directed_edges := GZ_SLIVER.get_coastal_opposing_edges(p_FeatureFace,
                                                                      'COUNTERCLOCKWISE');

      --2. Get all start/end nodes of the opposing edges.  Set them aside

      FOR i IN 1 .. opposing_directed_edges.COUNT
      LOOP

         temp_nodes.DELETE;
         temp_nodes.EXTEND(2);
         temp_nodes(1) := p_FeatureFace.GetTopoEdge(ABS(opposing_directed_edges(i))).GetNodeId('START');
         temp_nodes(2) := p_FeatureFace.GetTopoEdge(ABS(opposing_directed_edges(i))).GetNodeId('END');

         --oink oink
         all_nodes := GZ_BUSINESS_UTILS.LIST_TYPE_ADD_UNIQUE(all_nodes,
                                                             temp_nodes);

      END LOOP;

      --3. Call GZ_SLIVER.COLLAPSE_FACE_FROM_EDGES, giving it the face and the opposing edge(s)

      BEGIN

         GZ_SLIVER.COLLAPSE_FACE_FROM_EDGES(p_FeatureFace,
                                            opposing_directed_edges);

      EXCEPTION
      WHEN OTHERS
      THEN

         bail_out := 1;
         bail_msg := SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace;

      END;


      --4. Remove the nodes stored from 2.  Kindly for the UF nodes, Zap for interior

      FOR i IN 1 .. all_nodes.COUNT
      LOOP

         IF GZ_TOPO_PRIMITIVE.IS_NODE_UNIVERSAL(p_FeatureFace.topology.GetTopoName,
                                                all_nodes(i))
         THEN

            --remove obsolete, leave vertex
            removed_node := GZ_TOPO_PRIMITIVE.REMOVE_NODE_KINDLY(p_FeatureFace.topology.GetTopoName,
                                                                 all_nodes(i));

            IF removed_node = 0
            AND dbug IS NOT NULL
            THEN
               GZ_SLIVER.DBUG_THIS(dbug, 'Failed to remove supposedly obsolete node ' || all_nodes(i));
            END IF;

         ELSE

            --remove obs node and ghost vertex
            BEGIN

               GZ_TOPO_PRIMITIVE.ZAP_NODE(p_FeatureFace.topology.GetTopoName,
                                          all_nodes(i));

            EXCEPTION
            WHEN OTHERS
            THEN

               IF SQLERRM LIKE '%Zap node expects node to be bound by 2 edges%'
               THEN

                  --this is my error in zap node
                  --most likely either a dropped node that actually started as a 3-way connect
                  --or a bail out edit where we're just pointlessly attempting to process interior
                  --nodes that never got dropped
                  --gonna call this harmless mistaken fun, like a magar

                  IF dbug IS NOT NULL
                  THEN
                     GZ_SLIVER.DBUG_THIS(dbug, 'Failed to zap node ' || all_nodes(i));
                  END IF;

               ELSE

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

               END IF;

            END;

         END IF;

      END LOOP;

      IF bail_out = 1
      THEN

          RAISE_APPLICATION_ERROR(-20001, 'Failed in face collapse, tidied nodes, failure was ' || bail_msg);

      END IF;

   END COLLAPSE_FACE_TO_UNIVERSAL;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------
   
   PROCEDURE UPDATE_A_FEATURE (
      p_feature_table         IN VARCHAR2,
      p_feature_geoid         IN VARCHAR2,
      p_tracking_hash         IN OUT GZ_TYPES.nestedhash,
      p_featurekey            IN VARCHAR2,
      p_tolerance             IN NUMBER
   ) 
   AS
   
      --! 4/10/14
      
      temp_hash            GZ_TYPES.numberhash;
      update_dis_guy       PLS_INTEGER := 0;
   
   BEGIN
   
      --each call reps a single feature table record built on a face

      --kooky IN/OUT nested hash looks like
      --T848LS_FSL050V    (0500000US48427 = 1
      --                   0500000US48427 = 1)
      --T848LS_FSL310V    (310M300US21340 = 1)

      IF p_tracking_hash.EXISTS(p_feature_table)
      THEN

         --have seen this feature table before

         IF p_tracking_hash(p_feature_table).EXISTS(p_feature_geoid)
         THEN

            --table and geoid already hashed and updated. Skip
            update_dis_guy := 0;

         ELSE

            update_dis_guy := 1;

            --table is in there, but geoid is new.
            --add geoid to the tracking hash
            --and update the feature table record

            --pull the previously seen tables worth into a temp. Its a simple hash
            --(0500000US48427 = 1
            -- 0500000US48427 = 1)

            temp_hash := p_tracking_hash(p_feature_table);

            --add the new geoid
            temp_hash(p_feature_geoid) := 1;

            --replace the nested hash value for this table with the new set of geoids
            p_tracking_hash(p_feature_table) := temp_hash;

         END IF;

      ELSE

         --add the table, first time, and the geoid
         --and update dis guys record

         update_dis_guy := 1;

         temp_hash(p_feature_geoid) := 1;

         p_tracking_hash(p_feature_table) := temp_hash;

      END IF;

      IF update_dis_guy = 1
      THEN

         --Update.  Note that this feature may be extinct

         BEGIN

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_feature_table,
                                                       p_featurekey,
                                                       'SDOGEOMETRY',  --just this column
                                                       'ALL',
                                                        p_tolerance,
                                                        p_subset_col => p_featurekey,    --subset col
                                                        p_subset_val => p_feature_geoid);     --subset val to update

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%No records found in%'
            THEN

               --This was a waterey piece of fsl allowed to go extinct
               --Should maybe consider returning these to the caller to verify matches in sliver transaction table
               NULL;

            ELSE

               RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;

      END IF;
   
   END UPDATE_A_FEATURE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE UPDATE_EDITED_MEASUREMENTS (
      p_face_list             IN MDSYS.SDO_LIST_TYPE,
      p_feature_table_list    IN MDSYS.STRINGLIST,
      p_feature_geoid_list    IN MDSYS.STRINGLIST, 
      p_topology              IN GzTopology
   )
   AS

      --04/03/14!
      --This is pure sliver business logic
      --Only to be used when measurements on faces and fsls were good before sliver processing
      --   and we wish to keep them maintained after sliver work

      --Spell it out for me again brother
      --I'm ready to move on out from Squaresville and live in Editing Edge City
      --  A sliver face is our target for editing.  All of its neighbor faces may also get reshaped
      --  After editing any of those original face ids may exist (edited or not) or they may be gone
      --  There may also be new face_ids generated, some of which may even ID territory of an OG face
      --    but because of the way the face object code works any new faces will have NULL sdogeometry and can be IDd
      --The faces in the wall of text above will represent all possible FSLs edited, except one--
      --   The initial sliver face rep'd some FSLs, and they clearly got edited.  So must store those 
      --   FLSs and pass them in separately

      --Overall plan
      --Keep track of all faces and all neighbor faces as editing takes place
      --When editing is complete, also add any new face ids based in their null sdo
      --Add all faces to a list, and **remove any that got toasted and no longer exist**
      --Send that list of faces to this sub
      --It will update all measurements for the input face features
      --   and also for any higher level FSLS associated with the face
      --   tracking fsl keys to ensure no duplication of work <-- Thats most of the mess here

      WorkFace             GzFeatureFace := GzFeatureFace();
      feature_tables       MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      feature_geoids       MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      tracking_hash        GZ_TYPES.nestedhash;

   BEGIN

      --Do the sliver face FSLs first
      
      FOR i IN 1 .. p_feature_table_list.COUNT
      LOOP
      
         --p_feature_table_list and p_feature_geoid_list are populated in parallel
         --and likely chock full of dupes.  No checking in process_slivers
         --the FSL may also not exist
         
         GZ_SLIVER.UPDATE_A_FEATURE(p_feature_table_list(i),
                                    p_feature_geoid_list(i),
                                    tracking_hash,                 --IN OUT
                                    p_topology.GetFeatureTableKey,
                                    p_topology.GetTolerance);
         
      END LOOP;

      FOR i IN 1 .. p_face_list.COUNT
      LOOP

         WorkFace := GzFeatureFace(p_face_list(i),
                                   p_topology);

         --update all measurements on the face
         GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_topology.GetFaceFeatureTable,
                                                    'FACE_ID',
                                                    'SDOGEOMETRY',
                                                    'ALL',
                                                    p_topology.GetTolerance,
                                                    p_subset_col => 'FACE_ID',
                                                    p_subset_val => WorkFace.GetFaceId);

         feature_tables := WorkFace.GetFeatureTables;
         feature_geoids := WorkFace.GetFeatureTableGeoids;

         FOR jj IN 1 .. feature_tables.COUNT
         LOOP
         
            GZ_SLIVER.UPDATE_A_FEATURE(feature_tables(jj),
                                       feature_geoids(jj),
                                       tracking_hash,           --IN OUT
                                       p_topology.GetFeatureTableKey,
                                       p_topology.GetTolerance);

         END LOOP;  --end loop over feature tables repped by this face

      END LOOP;  --end loop over all faces

   END UPDATE_EDITED_MEASUREMENTS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------------

   FUNCTION VERIFY_CS_INPUTS (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_topology              IN GzTopology,
      p_sliver_restart_flag   IN VARCHAR2,
      p_sliver_width          IN NUMBER,
      p_segment_length        IN NUMBER,
      p_expendable_review     IN VARCHAR2,
      p_reshape_review        IN VARCHAR2,
      p_update_feature_geom   IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --M@! 8/01/13
      --Moved and modified from gz_topofix 4/4/14

      --Being a little bit OCD about this since we are putting our nice topology under the knife
      --      Values are legal
      --      Face table exists
      --      Layers in the output parameter table for this release/project are built and in the topo
      --      If restarting face_slivers transaction table should exist

      output            VARCHAR2(4000);
      layers_out        GZ_TYPES.gz_layers_out;

   BEGIN

      --      Values are legal

      IF UPPER(p_sliver_restart_flag) NOT IN ('Y', 'N')
      THEN

         output := output || 'Sliver_Restart_Flag unknown value ' ||  p_sliver_restart_flag || ' | ';

      END IF;

      IF p_sliver_width IS NULL
      THEN

         output := output || 'Sliver_Width cant be NULL | ';

      END IF;

      IF UPPER(p_expendable_review) NOT IN ('Y', 'N')
      THEN

         output := output || 'expendable_review unknown value ' ||  p_expendable_review || ' | ';

      END IF;

      IF UPPER(p_reshape_review) NOT IN ('Y', 'N')
      THEN

         output := output || 'reshape_review unknown value ' ||  p_reshape_review || ' | ';

      END IF;

      IF UPPER(p_update_feature_geom) NOT IN ('Y', 'N')
      THEN

         output := output || 'p_update_feature_geom unknown value ' ||  p_update_feature_geom || ' | ';

      END IF;

      --      Face table exists

      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_topology.GetFaceFeatureTable)
      THEN

         output := output || p_topology.GetFaceFeatureTable || ' doesnt exist or is empty | ';

      END IF;

      --      Layers in the output parameter table for this release/project are built

      layers_out := GZ_OUTPUT.GET_LAYERS_OUT(p_release,
                                             p_gen_project_id);

      IF layers_out.COUNT = 0
      THEN

         output := output || 'Cant find any records in gz_layers_out for release ' || p_release
                          || ' and project id ' || p_gen_project_id || ' | ';

      END IF;


      FOR i IN 1 .. layers_out.COUNT
      LOOP

         IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_topology.GetTopoName || '_FSL' || layers_out(i).layer || 'V',
                                                  TRUE) --empty exists
         THEN

            output := output || 'Output table ' || p_topology.GetTopoName || '_FSL' || layers_out(i).layer || 'V'
                             || ' doesnt exist | ';

         END IF;

         --check that layer is in the topo

         IF GZ_BUSINESS_UTILS.QUERY_DELIMITED_LIST(p_topology.GetFeatureTables,
                                                   p_topology.GetTopoName || '_FSL' || layers_out(i).layer || 'V') = 0
         THEN

            output := output || 'Couldnt find layer ' || p_topology.GetTopoName || '_FSL' || layers_out(i).layer || 'V'
                             || ' in topo ' || p_topology.GetTopoName || ' | ';

         END IF;

      END LOOP;

      --      If restarting face_slivers transaction table should exist
      IF p_sliver_restart_flag = 'Y'
      AND NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_topology.GetTopoName || '_FACE_SLIVERS')
      THEN

         output := output || 'Sliver restart but table ' || p_topology.GetTopoName || '_FACE_SLIVERS' || ' doesnt exist or is empty | ';

      END IF;

      RETURN output;

   END VERIFY_CS_INPUTS;

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   PROCEDURE WRITE_SLIVER_TRANSACTIONS (
      p_topo                  IN VARCHAR2,
      p_sliver_type           IN VARCHAR2,
      p_face_id               IN NUMBER,
      p_status                IN VARCHAR2,
      p_fsl_tables            IN MDSYS.STRINGLIST,
      p_fsl_geoids            IN MDSYS.STRINGLIST,
      p_geoid_extinction      IN VARCHAR2,
      p_sdogeometry           IN SDO_GEOMETRY,
      p_sliver_id             IN NUMBER DEFAULT NULL,     --restart, update an existing record
      p_partial_sdogeometry   IN SDO_GEOMETRY DEFAULT NULL
   )
   AS

      --Matt! 8/5/13
      --Moved from gz_topofix 4/4/14
      --Writer for the xx_face_slivers table

      psql              VARCHAR2(4000);
      sliver_id         NUMBER;
      fsl_geoids        VARCHAR2(8000);

   BEGIN

      IF p_sliver_id IS NULL
      THEN

         --standard, insert on initial run
         psql := 'SELECT MAX(a.sliver_id) + 1 FROM '
              || p_topo || '_face_slivers a ';

         EXECUTE IMMEDIATE psql INTO sliver_id;

         IF sliver_id IS NULL
         THEN

            --first, duh
            sliver_id := 1;

         END IF;

      END IF;

      FOR i IN 1 .. p_fsl_tables.COUNT
      LOOP

         --Z606LS_FSL160V|1600000US0608058;Z606LS_FSL976V|9760000US0603199997...
         fsl_geoids := fsl_geoids || p_fsl_tables(i) || '|' || p_fsl_geoids(i) || ';';

      END LOOP;

      IF LENGTH(fsl_geoids) > 4000
      THEN

         fsl_geoids := SUBSTR(fsl_geoids, 1, 3950) || ' <SNIP!>';

      END IF;

      IF p_sliver_id IS NULL
      THEN

         psql := 'INSERT /*+ APPEND */ INTO ' || p_topo || '_face_slivers '
              || '(sliver_id, sliver_type, face_id, status, fsl_geoids, geoid_extinction, sdogeometry, partial_sdogeometry) '
              || 'VALUES(:p1, :p2, :p3, :p4, :p5, :p6, :p7, :p8) ';

         EXECUTE IMMEDIATE psql USING sliver_id,
                                      p_sliver_type,
                                      p_face_id,
                                      p_status,
                                      fsl_geoids,
                                      p_geoid_extinction,
                                      p_sdogeometry,
                                      p_partial_sdogeometry;

      ELSE

         --restart and update of an existing transaction record
         --only thing that should change is the status message
         --I'll update geoids and extinction too, just in case I'm forgetting something
         --If anything, don't want to change stuff like sdo and partial sdo, want it to be a window back to the
         --original input topo

         psql := 'UPDATE ' || p_topo || '_face_slivers a '
              || 'SET '
              || 'a.status = :p1, '
              || 'a.fsl_geoids = :p2, '
              || 'a.geoid_extinction = :p3 '
              || 'WHERE '
              || 'a.sliver_id = :p4 AND '
              || 'a.face_id = :p5 ';

         EXECUTE IMMEDIATE psql USING p_status,
                                      fsl_geoids,
                                      p_geoid_extinction,
                                      p_sliver_id,
                                      p_face_id;

      END IF;

      COMMIT;


   END WRITE_SLIVER_TRANSACTIONS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION PROCESS_COASTAL_SLIVERS (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_topo                  IN VARCHAR2,
      p_log_type              IN VARCHAR2,
      p_sliver_width          IN NUMBER,
      p_segment_length        IN NUMBER,
      p_sliver_restart_flag   IN VARCHAR2 DEFAULT 'N',  --switched out
      p_expendable_review     IN VARCHAR2 DEFAULT 'N',  --switched out
      p_reshape_review        IN VARCHAR2 DEFAULT 'Y',  --switched out
      p_update_feature_geom   IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2
   AS

      --M@! 8/1/13
      --Modified and moved from gz_topofix to gz_sliver 4/4/14

      --Specs to Self:
      --0. Replace explicit NULL inputs with defaults
      --1. Check inputs in a sub
      --      Face table exists
      --      Values are legal
      --      Layers in the output parameter table for this release/project are built
      --2. Create or replace or check in on existence of transaction table
      --
      --LOOP while slivers are being removed
      --
      --3. Sort out universe of slivers for removal
      --      Sort faces on universal face by area asc. Loop over the ids
      --      If face id still exists, check for sliverishness
      --      If sliver, determine if removing the face results in endangerment, checking expendability
      --        IF endangered and not expendable, or endangered and p_expendable_review is Y, skip the face but note that this scenario exists
      --    If go ahead is OK and p_update_feature_geom is Y record face and all neighbor faces
      --4. Remove the sliver
      --
      --END LOOP

      --5. IF p_update_feature_geom is Y update measurements. Best to avoid this step
      --              In standard output processing coastal slivers happen before any sdo is calculated
      --              On restarts of coastal slivers the caller in output processing will need to switch this to Y
      --
      --6. When slivers no longer being removed, if non-expendables were found roll through one more time
      --  Get their IDs, whatever they currently are
      --  Write to the transaction table
      --7. If segment_length is not null Roll through again and get partials
      --  For now just write to the transaction table
      --8. Reshape and restart stuff will go here eventually

      Topology                GzTopology := GzTopology();
      WorkFace                GzFeatureFace := GzFeatureFace();
      FaceLayer               GzTopoLayer;
      all_touched_faces       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      null_touched_faces      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      remaining_touched_faces MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      sliver_restart_flag     VARCHAR2(1);
      expendable_review       VARCHAR2(4);
      reshape_review          VARCHAR2(4);
      output                  VARCHAR2(4000);
      slivers_removed         PLS_INTEGER := 1;
      deadman                 PLS_INTEGER := 0;
      face_sql                VARCHAR2(4000);
      psql                    VARCHAR2(4000);
      facez                   GZ_TYPES.numberarray;
      sliver_idz              GZ_TYPES.numberarray;
      kount                   PLS_INTEGER;
      endangered              VARCHAR2(4000);
      expendable              VARCHAR2(4000);
      sliver_status           VARCHAR2(32);
      endangered_exist        PLS_INTEGER := 0;
      fails_may_exist         PLS_INTEGER := 0;
      final_removed_kount     PLS_INTEGER;
      start_removed_kount     PLS_INTEGER;
      all_sliver_tables       MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      all_sliver_geoids       MDSYS.STRINGLIST := MDSYS.STRINGLIST();

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 1');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_SLIVER.PROCESS_COASTAL_SLIVERS: Start ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      Topology := GzTopology(SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA'),
                             p_topo);

      facelayer := GzTopoLayer(Topology.GetFaceFeatureTable,
                               'FACE_ID',
                               'TOPOGEOM',
                               Topology);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                  Topology.GetTopoName,
                                                  'PROCESS_COASTAL_SLIVERS',
                                                  Topology.GetFaceFeatureTable,
                                                  'Start: GZ_SLIVER.PROCESS_COASTAL_SLIVERS('
                                                  || p_release || ',' || p_gen_project_id || ','
                                                  || p_topo || ',' || p_log_type || ','
                                                  || p_sliver_restart_flag || ',' || p_sliver_width || ','
                                                  || p_segment_length || ',' || p_expendable_review || ','
                                                  || p_reshape_review || ',' || p_update_feature_geom || ')');

      -------------------------------------------------------------------------
      --0. Replace explicit NULL inputs with defaults
      --   This can happen if the setup tables get NULLed by hand
      -------------------------------------------------------------------------

      IF p_sliver_restart_flag IS NULL
      THEN
         sliver_restart_flag := 'N';
      ELSE
         sliver_restart_flag := UPPER(p_sliver_restart_flag);
      END IF;

      IF p_expendable_review IS NULL
      THEN
         expendable_review := 'N';
      ELSE
         expendable_review := UPPER(p_expendable_review);
      END IF;

      IF p_reshape_review IS NULL
      THEN
         reshape_review := 'Y';
      ELSE
         reshape_review := UPPER(p_reshape_review);
      END IF;

      IF Topology.GetSrid <> 8265 OR Topology.GetSrid IS NULL
      THEN

         --Code and remove this soonish
         RETURN 'Sorry (kinda), I havent coded coastal sliver removal for Z9 yet';

      END IF;

      -------------------------------------------------------------------------
      --1. Check inputs in a sub
      -------------------------------------------------------------------------

      output := GZ_SLIVER.VERIFY_CS_INPUTS(p_release,
                                           p_gen_project_id,
                                           Topology,
                                           sliver_restart_flag,
                                           p_sliver_width,
                                           p_segment_length,
                                           expendable_review,
                                           reshape_review,
                                           p_update_feature_geom);

      IF output IS NOT NULL
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                     Topology.GetTopoName,
                                                     'PROCESS_COASTAL_SLIVERS',
                                                     Topology.GetFaceFeatureTable,
                                                     'PROCESS_COASTAL_SLIVERS input check failed with: ' || output);

         RETURN 'PROCESS_COASTAL_SLIVERS input check failed with: ' || output;

      END IF;

      -------------------------------------------------------------------------
      --2. Create or replace transaction table
      -------------------------------------------------------------------------

      IF sliver_restart_flag = 'N'
      THEN

         GZ_BUSINESS_UTILS.CREATE_GZ_FACE_SLIVERS(NULL,
                                                  Topology.GetTopoName || '_FACE_SLIVERS');

      END IF;

      --LOOP while slivers are being removed

      --get count for tracking
      psql := 'SELECT COUNT(*) '
           || 'FROM '  || topology.GetTopoName || '_face_slivers '
           || 'WHERE status = :p1 ';

      EXECUTE IMMEDIATE psql INTO start_removed_kount USING 'REMOVED';

      WHILE slivers_removed = 1
      LOOP

         -------------------------------------------------------------------------
         --3. Sort out universe of slivers for removal
         -------------------------------------------------------------------------

         IF sliver_restart_flag = 'N'
         THEN

            face_sql := 'SELECT a.face_id FROM ' || FaceLayer.GetTableName || ' a '
                     || 'WHERE a.face_id IN ( '
                     || 'SELECT e.right_face_id FROM ' || Topology.GetTopoName || '_edge$ e '
                     || 'WHERE e.left_face_id = :p1 '
                     || 'UNION '
                     || 'SELECT e.left_face_id FROM ' || Topology.GetTopoName || '_edge$ e '
                     || 'WHERE e.right_face_id = :p2 '
                     || ') ORDER BY SDO_GEOM.SDO_AREA(a.sdogeometry,:p3) ASC ';

            --even for final national topos
            --dont think this will ever be more than a few tens of thousands

            EXECUTE IMMEDIATE face_sql BULK COLLECT INTO facez USING -1,
                                                                     -1,
                                                                     Topology.GetTolerance;

         ELSE

            --meh, keep the whole mess separate
            face_sql := 'SELECT a.face_id, b.sliver_id FROM ' || FaceLayer.GetTableName || ' a, '
                     || Topology.GetTopoName || '_FACE_SLIVERS b '
                     || 'WHERE a.face_id IN ( '
                     || 'SELECT e.right_face_id FROM ' || Topology.GetTopoName || '_edge$ e '
                     || 'WHERE e.left_face_id = :p1 '
                     || 'UNION '
                     || 'SELECT e.left_face_id FROM ' || Topology.GetTopoName || '_edge$ e '
                     || 'WHERE e.right_face_id = :p2 '
                     || ') AND a.face_id = b.face_id '
                     || 'AND b.sliver_type = :p3 AND b.status = :p4 AND b.review_adjudication = :p5 '
                     || 'ORDER BY SDO_GEOM.SDO_AREA(a.sdogeometry,:p3) ASC ';

            --need sliver ids to uniquely ID the record
            --This should only happen if expendable review is set to Y and we want to review water junk before removal
            --which is unlikely

            EXECUTE IMMEDIATE face_sql BULK COLLECT INTO facez, sliver_idz USING -1,
                                                                                 -1,
                                                                                 'SLIVER',
                                                                                 'IN REVIEW',
                                                                                 'Y',
                                                                                 Topology.GetTolerance;

         END IF;


         IF facez.COUNT > 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        Topology.GetTopoName,
                                                        'PROCESS_COASTAL_SLIVERS',
                                                        Topology.GetTopoName || '_FACE_SLIVERS' ,
                                                        'Starting loop ' || TO_CHAR(deadman + 1) || ' over ' || facez.COUNT
                                                        || ' faces on the universal face. See ' || Topology.GetTopoName
                                                        || '_FACE_SLIVERS for details');

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        Topology.GetTopoName,
                                                        'PROCESS_COASTAL_SLIVERS',
                                                        Topology.GetTopoName || '_FACE_SLIVERS' ,
                                                        'No sliver faces to process');

         END IF;

         --      Sort faces on universal face by area asc. Loop over the ids

         --set to exit the outer loop if nothing happens
         slivers_removed := 0;

         FOR i IN 1 .. facez.COUNT
         LOOP

            -- If face id still exists on this loop, check for sliverishness

            psql := 'SELECT COUNT(*) '
                 || 'FROM ' ||  FaceLayer.GetTableName || ' a '
                 || 'WHERE a.face_id = :p1 ';

            EXECUTE IMMEDIATE psql INTO kount USING facez(i);

            IF kount = 1
            THEN

               --------------------------------------------------------------------
               --Much logic buried here, instantiate face and check if its a sliver
               
               BEGIN
               
                  WorkFace := GzFeatureFace(facez(i),
                                            Topology);

                  GZ_SLIVER.COASTAL_SLIVER_DETERMINATION(WorkFace,       --IN OUT
                                                         p_sliver_width,
                                                         p_segment_length);  --consider updating type to take a NULL value here
                                                      
               EXCEPTION
               WHEN OTHERS
               THEN
               
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                              Topology.GetTopoName,
                                                              'PROCESS_COASTAL_SLIVERS',
                                                              Topology.GetTopoName || '_FACE_SLIVERS' ,
                                                              'Total failure to instantiate and determine sliver of face ' || facez(i),
                                                              p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

               
               END;
               --------------------------------------------------------------------


               IF WorkFace.GetCoastalSliver = 'SLIVER'
               THEN

                  -- If sliver, determine if removing the face results in endangerment, checking expendability

                  endangered := NULL;

                  IF expendable_review = 'N'
                  OR sliver_restart_flag = 'Y' --if expendable review = Y on a restart, still need to get real endangerment
                  THEN

                     --usually here. No need to review when expendable watery stuff is endangered
                     endangered := GZ_SMPOLY.FSLS_ENDANGERED(p_release,
                                                             p_gen_project_id,
                                                             Topology.GetTopoName,
                                                             facez(i),
                                                             Topology.GetFeatureTableKey,
                                                             'GZ_LAYERS_OUT');

                  ELSE

                     --wish to review if anything at all is endangered, including water junk
                     endangered := GZ_SMPOLY.FSLS_ENDANGERED(p_release,
                                                             p_gen_project_id,
                                                             Topology.GetTopoName,
                                                             facez(i),
                                                             Topology.GetFeatureTableKey,
                                                             'GZ_LAYERS_OUT');

                     --Add water junk.  All we are doing at this point is kicking
                     --out any face that can't be removed
                     --After the looping and face removing we will get these once again
                     endangered := endangered || GZ_SMPOLY.FSLS_ENDANGERED(p_release,
                                                                           p_gen_project_id,
                                                                           Topology.GetTopoName,
                                                                           facez(i),
                                                                           Topology.GetFeatureTableKey,
                                                                           'GZ_LAYERS_OUT',
                                                                           'Y');  --special reverse flag

                  END IF;


                  IF endangered_exist = 0
                  AND endangered IS NOT NULL
                  THEN

                     --first time weve seen something is about to get extinct
                     --set this.  Cant log these guys now because face_ids may change
                     endangered_exist := 1;

                  ELSIF endangered IS NULL
                  THEN

                     -- If go ahead is OK and p_update_feature_geom is Y record all FSLs on the face and on neighbor faces

                     IF p_update_feature_geom = 'Y'
                     THEN
                        --add all neighbor faces and self face to our running list of faces
                        all_touched_faces := GZ_BUSINESS_UTILS.LIST_TYPE_ADD_UNIQUE(all_touched_faces,
                                                                                    WorkFace.GetBoundaryFaces);
                        
                        --add current face
                        all_touched_faces.EXTEND(1);
                        all_touched_faces(all_touched_faces.COUNT) := WorkFace.GetFaceId; 
                        
                        --add current face fsls since no faces above rep them after sliver removal
                        all_sliver_tables := GZ_BUSINESS_UTILS.STRINGLIST_ADD(all_sliver_tables,
                                                                              WorkFace.GetFeatureTables);
                                                                              
                        all_sliver_geoids := GZ_BUSINESS_UTILS.STRINGLIST_ADD(all_sliver_geoids,
                                                                              WorkFace.GetFeatureTableGeoids);

                     END IF;

                     BEGIN

                        --call the fsl getter again with reverse from standard use above, cant do it after face collapse
                        --gets any ugly extinct water geoids.  So we can write them to the transaction table. usually null
                        endangered := GZ_SMPOLY.FSLS_ENDANGERED(p_release,
                                                                p_gen_project_id,
                                                                Topology.GetTopoName,
                                                                facez(i),
                                                                Topology.GetFeatureTableKey,
                                                                'GZ_LAYERS_OUT',
                                                                'Y');   ---special only get stuff we allow to be extinct


                        --+++++++++++++++++++++++++++++++++++++++++++--
                        --Tha workhorse--------------------------------
                        GZ_SLIVER.COLLAPSE_FACE_TO_UNIVERSAL(WorkFace);
                        -----------------------------------------------
                        --+++++++++++++++++++++++++++++++++++++++++++--

                        --no error
                        slivers_removed := 1;

                        IF sliver_restart_flag = 'N'
                        THEN

                           --SOP
                           GZ_SLIVER.WRITE_SLIVER_TRANSACTIONS(Topology.GetTopoName,
                                                               'SLIVER',
                                                               facez(i),
                                                               'REMOVED',
                                                                WorkFace.GetFeatureTables,
                                                                WorkFace.GetFeatureTableGeoids,
                                                                endangered,  --should be null here
                                                                WorkFace.GetSdogeometry);

                        ELSE

                           --rare, possibly never called, restart and removal of expendable territory after review
                           GZ_SLIVER.WRITE_SLIVER_TRANSACTIONS(Topology.GetTopoName,
                                                               'SLIVER',
                                                               facez(i),
                                                               'REMOVED',         --should go from IN REVIEW to REMOVED
                                                               WorkFace.GetFeatureTables,
                                                               WorkFace.GetFeatureTableGeoids,
                                                               endangered,
                                                               WorkFace.GetSdogeometry,
                                                               sliver_idz(i));     --update instead of insert based on primary key

                        END IF;


                     EXCEPTION
                     WHEN OTHERS
                     THEN

                        --Dont write to the transaction table. Rationale:
                        --The face and other info written here may well be bogus by the time of investigation
                        --The face may have partially been fixed, turning a sliver into a partial sliver that will be totally fixed
                        --If it wasnt fixed and is still a sliver, final transaction loop will catch and record it

                        fails_may_exist := 1;
                        --Do write to the tracking table for developer investigation
                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                                    Topology.GetTopoName,
                                                                    'PROCESS_COASTAL_SLIVERS',
                                                                    Topology.GetTopoName || '_FACE_SLIVERS' ,
                                                                    'FAILURE: collapse_to_universal on ' || facez(i),
                                                                    p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace,
                                                                    p_sdo_dump => WorkFace.GetSdogeometry);

                     END;

                  END IF;  --end if endangered

               END IF; --end if, this face is SLIVER

            END IF;  --end if, does this face even exist

         END LOOP; --end loop over this current set of all -1 faces in the topo


         IF deadman <= 5
         THEN

            deadman := deadman + 1;

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                        Topology.GetTopoName,
                                                        'PROCESS_COASTAL_SLIVERS',
                                                        FaceLayer.GetTableName,
                                                        '5 loops and still cant remove all slivers?  '
                                                        || 'Somethings wrong, continuing with what we have ');

            output := output || 'Warning: Cant remove all slivers | ';
            slivers_removed := 0;

         END IF;


      END LOOP;

      psql := 'SELECT COUNT(*) '
           || 'FROM '  || Topology.GetTopoName || '_face_slivers '
           || 'WHERE status = :p1 ';

      EXECUTE IMMEDIATE psql INTO final_removed_kount USING 'REMOVED';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                  Topology.GetTopoName,
                                                  'PROCESS_COASTAL_SLIVERS',
                                                  FaceLayer.GetTableName,
                                                  'Removed ' || TO_CHAR(final_removed_kount - start_removed_kount) || ' coastal slivers ');


      -------------------------------------------------------------------------
      --5. IF p_update_feature_geom is Y update measurements for all touched FSLS
      -------------------------------------------------------------------------

      IF p_update_feature_geom = 'Y'
      AND all_touched_faces.COUNT > 0
      THEN

         --First remove those that no longer exist

         psql := 'SELECT a.face_id FROM ' || FaceLayer.GetTableName || ' a '
              || 'WHERE a.face_id IN (SELECT * FROM TABLE(:p1))';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO remaining_touched_faces USING all_touched_faces;

         --Add any null sdo face ids.  These face ids were generated during processing
         psql := 'SELECT a.face_id FROM ' || FaceLayer.GetTableName || ' a '
              || 'WHERE a.sdogeometry IS NULL ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO null_touched_faces;

         all_touched_faces := GZ_BUSINESS_UTILS.LIST_TYPE_ADD_UNIQUE(remaining_touched_faces,
                                                                     null_touched_faces);

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                     Topology.GetTopoName,
                                                     'PROCESS_COASTAL_SLIVERS',
                                                     Topology.GetTopoName || '_FACE_SLIVERS' ,
                                                     'Updating measurements and sdo for ' || remaining_touched_faces.COUNT
                                                     || ' remaining faces and any fsls touched by sliver removal');

         GZ_SLIVER.UPDATE_EDITED_MEASUREMENTS(remaining_touched_faces,
                                              all_sliver_tables,
                                              all_sliver_geoids,
                                              Topology);


      END IF;

      -------------------------------------------------------------------------
      --6. When slivers no longer being removed, if non-expendables were found roll through one more time
      --   Get their IDs, whatever they currently are
      --   Write to the transaction table
      --   If expendable review is also requested, get those too
      -------------------------------------------------------------------------
      -------------------------------------------------------------------------
      --7. If segment_length is not null and this is an initial run, get partial reshapes
      --  For now just write to the transaction table
      -------------------------------------------------------------------------

      IF endangered_exist = 1            --these three evaluate to YES pretty much always
      OR fails_may_exist = 1
      OR p_segment_length IS NOT NULL
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                     Topology.GetTopoName,
                                                     'PROCESS_COASTAL_SLIVERS',
                                                     Topology.GetTopoName || '_FACE_SLIVERS' ,
                                                     'Making a final loop to get endangered, failed, or partial sliver records to add to '
                                                     || Topology.GetTopoName || '_FACE_SLIVERS');

         --face_sql stored above
         IF sliver_restart_flag = 'N'
         THEN

            --SOP
            EXECUTE IMMEDIATE face_sql BULK COLLECT INTO facez USING -1,
                                                                     -1,
                                                                     Topology.GetTolerance;
         ELSE

            --rare review of expendable water and there are fails
            --will only get faces in review that didnt get fixed up yonder
            EXECUTE IMMEDIATE face_sql BULK COLLECT INTO facez, sliver_idz USING -1,
                                                                                 -1,
                                                                                 'SLIVER',
                                                                                 'IN REVIEW',
                                                                                 'Y',
                                                                                 Topology.GetTolerance;
         END IF;

         FOR i IN 1 .. facez.COUNT
         LOOP

            -- Dont need to check for face existence this loop, no transmogrifying

             --------------------------------------------------------------------
            --Much logic buried here, instantiate face and check if its a sliver
            BEGIN
            
               WorkFace := GzFeatureFace(facez(i),
                                         Topology);

               GZ_SLIVER.COASTAL_SLIVER_DETERMINATION(WorkFace,           --IN OUT
                                                      p_sliver_width,
                                                      p_segment_length);  --consider updating type to take a NULL value here
            
            EXCEPTION
               WHEN OTHERS
               THEN
               
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                              Topology.GetTopoName,
                                                              'PROCESS_COASTAL_SLIVERS',
                                                              Topology.GetTopoName || '_FACE_SLIVERS' ,
                                                              'Total failure to instantiate and determine sliver of face ' || facez(i),
                                                              p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

               
            END;
            --------------------------------------------------------------------


            IF WorkFace.GetCoastalSliver = 'SLIVER'
            THEN

               -- If sliver, determine if removing the face results in endangerment, checking expendability

               endangered := NULL;

               --usually just this. Get any important endangered stuff
               endangered := GZ_SMPOLY.FSLS_ENDANGERED(p_release,
                                                       p_gen_project_id,
                                                       Topology.GetTopoName,
                                                       facez(i),
                                                       Topology.GetFeatureTableKey,
                                                       'GZ_LAYERS_OUT');


               --also get any watery junk, put it in the table too
               expendable := GZ_SMPOLY.FSLS_ENDANGERED(p_release,
                                                       p_gen_project_id,
                                                       Topology.GetTopoName,
                                                       facez(i),
                                                       Topology.GetFeatureTableKey,
                                                       'GZ_LAYERS_OUT',
                                                       'Y');  --special reverse flag


               IF expendable_review = 'N'
               THEN

                  IF endangered IS NOT NULL
                  THEN

                     sliver_status := 'NOT EXPENDABLE';

                  ELSE

                     --if it was totally ok to remove this face but its still here, assume a problem
                     sliver_status := 'REMOVAL FAILED';

                  END IF;

               ELSE --expendable_review = 'Y'

                  IF endangered IS NOT NULL
                  THEN

                     --existence of non expendable trumps
                     sliver_status := 'NOT EXPENDABLE';

                  ELSIF endangered IS NULL
                  AND expendable IS NOT NULL
                  THEN

                     --This is what we are trying to catch here

                     IF sliver_restart_flag = 'N'
                     THEN

                        sliver_status := 'IN REVIEW';

                     ELSIF sliver_restart_flag = 'Y'
                     THEN

                        sliver_status := 'REMOVAL FAILED';

                     END IF;

                  ELSIF endangered IS NULL
                  AND expendable IS NULL
                  THEN

                     sliver_status := 'REMOVAL FAILED';

                  ELSE

                     RAISE_APPLICATION_ERROR(-20001, 'Logic 101 bustah');

                  END IF;

               END IF;

               IF sliver_restart_flag = 'N'
               THEN

                  GZ_SLIVER.WRITE_SLIVER_TRANSACTIONS(Topology.GetTopoName,
                                                      'SLIVER',
                                                      facez(i),
                                                      sliver_status,                  --the variable
                                                      WorkFace.GetFeatureTables,
                                                      WorkFace.GetFeatureTableGeoids,
                                                      endangered || ';' || expendable,       --one or both may be NULL
                                                      WorkFace.GetSdogeometry);

               ELSE

                  GZ_SLIVER.WRITE_SLIVER_TRANSACTIONS(Topology.GetTopoName,
                                                      'SLIVER',
                                                      facez(i),
                                                      sliver_status,                  --the variable
                                                      WorkFace.GetFeatureTables,
                                                      WorkFace.GetFeatureTableGeoids,
                                                      endangered || ';' || expendable,       --one or both may be NULL
                                                      WorkFace.GetSdogeometry,
                                                      sliver_idz(i));

               END IF;

            ELSIF WorkFace.GetCoastalSliver = 'PARTIAL'
            THEN

               --Make this explicit in the table.  Not checked, doesnt matter.
               endangered := 'NA';

               sliver_status := 'IN REVIEW';

               IF WorkFace.GetCoastalSliverDistance('CLOCKWISE') > 0
               THEN

                  /*Go for the gusto
                  --better protect this
                  BEGIN

                     cutoff_pt := WorkFace.GetPartialSliverCutoff('CLOCKWISE');

                  EXCEPTION
                  WHEN OTHERS
                  THEN

                     cutoff_pt := NULL;

                  END;
                  */

                  GZ_SLIVER.WRITE_SLIVER_TRANSACTIONS(Topology.GetTopoName,
                                                      'PARTIAL',
                                                      facez(i),
                                                      sliver_status,
                                                      WorkFace.GetFeatureTables,
                                                      WorkFace.GetFeatureTableGeoids,
                                                      endangered,
                                                      WorkFace.GetSdogeometry,
                                                      p_partial_sdogeometry => WorkFace.GetPartialSliverCutoff('CLOCKWISE'));

               END IF;

               IF WorkFace.GetCoastalSliverDistance('COUNTERCLOCKWISE') > 0
               THEN

                  /*
                  BEGIN

                    cutoff_pt := WorkFace.GetPartialSliverCutoff('COUNTERCLOCKWISE');

                  EXCEPTION
                  WHEN OTHERS
                  THEN
                     --just in case, not extensively tested as of adding this
                     cutoff_pt := NULL;

                  END;
                  */

                  GZ_SLIVER.WRITE_SLIVER_TRANSACTIONS(Topology.GetTopoName,
                                                      'PARTIAL',
                                                      facez(i),
                                                      sliver_status,
                                                      WorkFace.GetFeatureTables,
                                                      WorkFace.GetFeatureTableGeoids,
                                                      endangered,
                                                      WorkFace.GetSdogeometry,
                                                      p_partial_sdogeometry => WorkFace.GetPartialSliverCutoff('COUNTERCLOCKWISE'));

               END IF;

            END IF; --end if, this face is a SLIVER

         END LOOP; --end loop over this current set of all -1 faces in the topo

      END IF;  --end IF did some endangered exist


      ------------------------------------------------------------------------
      --8. Reshape restart stuff will go here eventually
      -------------------------------------------------------------------------



      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG(p_log_type,
                                                  Topology.GetTopoName,
                                                  'PROCESS_COASTAL_SLIVERS',
                                                  FaceLayer.GetTableName,
                                                  'End GZ_SLIVER.PROCESS_COASTAL_SLIVERS');

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GZ_SLIVER.PROCESS_COASTAL_SLIVERS: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;

   END PROCESS_COASTAL_SLIVERS;


END GZ_SLIVER;

/