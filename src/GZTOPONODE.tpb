CREATE OR REPLACE
TYPE BODY GzTopoNode
AS

   --Matt! 8/21/14

   --SAMPLE, play usage
   
   --declare
   --   thisnode GzTopoNode := GzTopoNode();
   --begin
   --   thisnode := GzTopoNode(2328469, 'V699IN');
   --   thisnode.describe;
   --   thisnode.zap;
   --end;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --Default constructor

   CONSTRUCTOR FUNCTION GzTopoNode
   RETURN SELF AS RESULT
   AS
   BEGIN

      RETURN;

   END GzTopoNode;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   CONSTRUCTOR FUNCTION GzTopoNode (
      p_node_id            IN NUMBER,
      p_topology           IN VARCHAR2
   ) RETURN SELF AS RESULT
   AS

      psql              VARCHAR2(4000);

   BEGIN

      self.nodeId    := ABS(p_node_id);  
      self.topology  := UPPER(p_topology);


      psql := 'SELECT n.geometry '
           || 'FROM ' || self.topology || '_node$ n '
           || 'WHERE n.node_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO self.geometry USING self.nodeId;
      
      psql := 'SELECT e.edge_id FROM ' || self.topology || '_edge$ e '
           || 'WHERE e.start_node_id = :p1 OR e.end_node_id = :p2 ';
           
      EXECUTE IMMEDIATE psql BULK COLLECT INTO self.edges USING self.nodeId,
                                                                self.nodeId;

      RETURN;

   END GzTopoNode;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetNodeId
   RETURN NUMBER
   AS

   BEGIN

      RETURN self.NodeId;

   END GetNodeId;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetGeometry
   RETURN SDO_GEOMETRY
   AS

   BEGIN

      RETURN self.geometry;

   END GetGeometry;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetEdges
   RETURN MDSYS.SDO_LIST_TYPE
   AS

   BEGIN

      RETURN self.edges;

   END GetEdges;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetIsUniversal
   RETURN VARCHAR2
   AS
   
      --duplicates gz_topo_primitive.is_node_universal 
      
      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(1) '
           || 'FROM '
           || self.topology || '_edge$ e '
           || 'WHERE '
           || '(e.start_node_id = :p1 OR e.end_node_id = :p2) AND '
           || '(e.left_face_id = :p3 or e.right_face_id = :p4) ';

      EXECUTE IMMEDIATE psql INTO kount USING self.GetNodeId, self.GetNodeId,
                                              -1, -1;

      IF kount = 0
      THEN

         RETURN 'FALSE';

      ELSIF kount = 2
      THEN

         RETURN 'TRUE';

      ELSIF kount = 1
      THEN

         --This is an island edge that forms a full ring
         --Any outlier cases to worry about here?
         RETURN 'TRUE';

      ELSIF kount > 2
      THEN

         RAISE_APPLICATION_ERROR(-20001,'You have a bowtie topology! Remove this error if thats cool');


      END IF;

   END GetIsUniversal;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetEdgeCount
   RETURN NUMBER
   AS
   
      edges          MDSYS.SDO_LIST_TYPE;

   BEGIN
   
      edges := self.GetEdges;
      
      RETURN edges.COUNT;

   END GetEdgeCount;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetTip (
      p_edge_id         IN NUMBER
   ) RETURN VARCHAR2
   AS

      psql     VARCHAR2(4000);
      output   VARCHAR2(5);
      
   BEGIN

       psql := 'SELECT node '
            || 'FROM ' || self.topology || '_edge$ UNPIVOT (nodes '
            || 'FOR node '
            || 'IN (start_node_id AS :p1, end_node_id AS :p2)) '
            || 'WHERE '
            || 'edge_id = :p3 AND '
            || 'nodes = :p4 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING 'START',
                                                  'END',
                                                  p_edge_id,
                                                  self.GetNodeId;

      EXCEPTION
      WHEN OTHERS
      THEN

          RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' on edge ' || p_edge_id || ', node' || self.GetNodeId
                                         || '-- ' || psql );

      END;

      RETURN output;

   END GetTip;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetIsObsolete
   RETURN VARCHAR2
   AS
   
   BEGIN
   
      IF self.GetEdgeCount = 2
      THEN
      
         RETURN 'TRUE';
         
      ELSE
      
         RETURN 'FALSE';
      
      END IF;
   
   END GetIsObsolete;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION GetDbug
   RETURN NUMBER
   AS

   BEGIN

      RETURN self.Dbug;

   END GetDbug;
   

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER FUNCTION Describe (
      p_indent             IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      output         VARCHAR2(8000);

   BEGIN

      output := output || p_indent || 'NodeId: ' || TO_CHAR(self.GetNodeId) || chr(10);
      output := output || p_indent || 'Topology: ' || self.topology || chr(10);
      output := output || p_indent || 'geometry: ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(self.GetGeometry)) || chr(10);
      output := output || p_indent || 'dbug: ' || self.GetDbug || chr(10);

      RETURN output;

   END Describe;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   MEMBER PROCEDURE Move (
      to_geom           IN SDO_GEOMETRY
   )
   AS
   
      --wrapper to SDO_TOPO_MAP.MOVE_NODE which is absurdly hard to use by humans
      --based on older gz_topo_util.GZ_MOVE_NODE
      
      psql              VARCHAR2(4000);
      edge_ids          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      pos_edge_ids      MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      edge_geoms        MDSYS.SDO_GEOMETRY_ARRAY := MDSYS.SDO_GEOMETRY_ARRAY();
      newnumarray       SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY();
      ordkount          PLS_INTEGER := 0;
      wack_edge_array   SDO_EDGE_ARRAY := SDO_EDGE_ARRAY();
      window_geom       SDO_GEOMETRY;
      create_it         NUMBER;
      
   BEGIN

      --get our bucket, must maintain the order
      psql := 'SELECT tt.edge_id, ABS(tt.edge_id), e.geometry '
           || 'FROM '
           || '  (SELECT rownum myorder, t.column_value edge_id '
           || '   FROM TABLE(SDO_TOPO_MAP.GET_NODE_STAR(:p1, NULL, :p2)) t '
           || '  ) tt, '
           || self.topology || '_edge$ e '
           || 'WHERE '
           || 'ABS(tt.edge_id) = e.edge_id '
           || 'ORDER BY tt.myorder ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_ids,
                                               pos_edge_ids,
                                               edge_geoms USING self.topology,
                                                                self.GetNodeId;

      --edge ids are in correct order for move_node
      --positive edge_ids mean the geometry runs from our changing spot --> end
      --negative edge_ids mean the geometry runs from start node --> our change

      FOR i IN 1 .. edge_ids.COUNT
      LOOP

         newnumarray.EXTEND(edge_geoms(i).sdo_ordinates.COUNT);

         FOR j IN 1 .. (edge_geoms(i).sdo_ordinates.COUNT)/2
         LOOP

            IF edge_ids(i) > 0
            AND j = 1
            THEN

               --move start
               ordkount := ordkount + 1;
               newnumarray(ordkount) := to_geom.sdo_point.X;
               ordkount := ordkount + 1;
               newnumarray(ordkount) := to_geom.sdo_point.Y;

            ELSIF edge_ids(i) < 0
            AND j = (edge_geoms(i).sdo_ordinates.COUNT)/2
            THEN

               --move end
               ordkount := ordkount + 1;
               newnumarray(ordkount) := to_geom.sdo_point.X;
               ordkount := ordkount + 1;
               newnumarray(ordkount) := to_geom.sdo_point.Y;

            ELSE

               --transfer ordinates unchanged
               ordkount := ordkount + 1;
               newnumarray(ordkount) := edge_geoms(i).sdo_ordinates((j * 2) - 1);
               ordkount := ordkount + 1;
               newnumarray(ordkount) := edge_geoms(i).sdo_ordinates(j * 2);

            END IF;

         END LOOP;

         --this edge has a num array with an altered tip
         wack_edge_array.EXTEND;
         wack_edge_array(i) := newnumarray;

         --reset for next loop
         newnumarray.DELETE;
         ordkount := 0;

      END LOOP;

      --SOP
      psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
           || 'FROM '
           || self.topology || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id IN (SELECT * FROM TABLE(:p1)) ';

      EXECUTE IMMEDIATE psql INTO window_geom USING pos_edge_ids;

      IF window_geom.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Bad window. Bad. ');

      END IF;

      --create the topomap with window

      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(self.topology || 'MAP',
                                                   self.topology,
                                                   2,
                                                   window_geom.sdo_ordinates(1),
                                                   window_geom.sdo_ordinates(2),
                                                   window_geom.sdo_ordinates(3),
                                                   window_geom.sdo_ordinates(4)
                                                   );

      BEGIN

         SDO_TOPO_MAP.MOVE_NODE(NULL,
                                self.GetNodeId,
                                wack_edge_array);

         --consider other signature for bug management as well
                                --moved_iso_nodes,
                                --moved_iso_edges,
                                --'TRUE'); or FALSE


      EXCEPTION
      WHEN OTHERS
      THEN

         --cleanliness is something
         --commit and drop topomap
         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(self.topology || 'MAP',
                                                      self.topology,
                                                      3);


         IF SQLERRM LIKE '%Moved node has not moved%'
         THEN

            --probably a tolerance issue.  Maybe ignore it?
            GZ_GEOM_UTILS.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
            
            RAISE_APPLICATION_ERROR(-20001, SQLERRM || DBMS_UTILITY.format_error_backtrace);

         ELSIF SQLERRM LIKE '%An edge does not maintain the position of the opposite end node%'
         THEN

            --Fracked up the order probably
            GZ_GEOM_UTILS.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
            
            RAISE_APPLICATION_ERROR(-20001, SQLERRM || DBMS_UTILITY.format_error_backtrace);

         ELSIF SQLERRM LIKE '%not found in cache%'
         THEN

            --oracle.spatial.topo.TopoEntityNotFoundException: Edge ID 337 not found in cache

            --This appears to be a bug
            --Get it when not using a topomap.  Manual workaround: use a topomap

            --Also get it when using a topomap.  Adding the edge returned to the window just
            --   chases the error to a new edge ad infinitum
            --Automated workaround possible here: Make a topomap that includes the whole topology

            GZ_GEOM_UTILS.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
            
            RAISE_APPLICATION_ERROR(-20001, 'Got the not found in cache error '
                                    || 'SDO_TOPO_MAP.MOVE_NODE(NULL,' || self.GetNodeId || ', <edge array>);'
                                    || SQLERRM);


         ELSE

               GZ_GEOM_UTILS.GZ_DUMP_SDO_EDGE_ARRAY(wack_edge_array);
                  
               RAISE_APPLICATION_ERROR(-20001, 'Got the not found in cache error, consider using a topomap '
                                    || 'SDO_TOPO_MAP.MOVE_NODE(NULL,' || self.GetNodeId || ', <edge array>);'
                                    || SQLERRM);

         END IF;

      END;

      --commit and drop topomap
      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(self.topology || 'MAP',
                                                   self.topology,
                                                   3);

   END Move;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   
   MEMBER PROCEDURE RemoveKindly
   AS
   
      window_geom       SDO_GEOMETRY;
      edges             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      create_it         NUMBER;
      psql              VARCHAR2(4000);
   
   BEGIN

      IF self.GetIsObsolete = 'FALSE'
      THEN

         --not obsolete, either 0 or 1(ring) or  2+
         --so cannot remove the node
         --Gentle return
         RETURN;

      END IF;
      
      --edge count is 2
      edges := self.GetEdges;

      --set up topomap to cover the extent of both edges

      psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
           || 'FROM '
           || self.topology || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 OR '
           || 'e.edge_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO window_geom USING edges(1),
                                                    edges(2);

      --create the topomap with window

      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(self.topology || 'MAP',
                                                   self.topology,
                                                   2,
                                                   window_geom.sdo_ordinates(1),
                                                   window_geom.sdo_ordinates(2),
                                                   window_geom.sdo_ordinates(3),
                                                   window_geom.sdo_ordinates(4)
                                                   );

      BEGIN

         SDO_TOPO_MAP.REMOVE_NODE(NULL,
                                  self.GetNodeId);

      EXCEPTION
      WHEN OTHERS
      THEN

         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(self.topology || 'MAP',
                                                      self.topology,
                                                      3);

         RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

      END;

      --commit and drop temp topomap
      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(self.topology || 'MAP',
                                                   self.topology,
                                                   3);

   
   END RemoveKindly;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   
   MEMBER PROCEDURE Zap
   AS
   
      --Removes node from the topology. Node must be attached to two edges
      --Then also removes all trace of the ghost vertex
      
      edges                MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      remaining_edge_id    NUMBER;
      remaining_edge_geom  SDO_GEOMETRY;
      new_ordinates        SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();
      ordkount             PLS_INTEGER := 0;
      skipvertex           PLS_INTEGER := 0;
      reshaped_edge_geom   SDO_GEOMETRY;
      psql                 VARCHAR2(4000);
      nodegeom             SDO_GEOMETRY;
      
   BEGIN
   
      edges := self.GetEdges;
   
      IF self.GetIsObsolete = 'FALSE'
      THEN

         --not gentle
         RAISE_APPLICATION_ERROR(-20001,'Zap node expects node to be bound by 2 edges ');

      END IF;

      --safe to use, calls sdo_topo_map.remove_node
      self.RemoveKindly;
      
      --one of the edges should now be gone, find which edge remains
      psql := 'SELECT e.edge_id, e.geometry '
           || 'FROM '
           || self.topology || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 OR '
           || 'e.edge_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO remaining_edge_id,
                                  remaining_edge_geom USING edges(1),
                                                            edges(2);

      --Reshape the remaining edge to not include the position we saved
      
      nodegeom := self.GetGeometry;

      new_ordinates.EXTEND(remaining_edge_geom.sdo_ordinates.COUNT - 2);

      FOR i IN 1 .. remaining_edge_geom.sdo_ordinates.COUNT/2
      LOOP

         IF remaining_edge_geom.sdo_ordinates( (i*2) - 1 ) = nodegeom.SDO_POINT.X
         AND remaining_edge_geom.sdo_ordinates(i*2) = nodegeom.SDO_POINT.Y
         THEN

            --this is the node removed, we dont love it no more
            skipvertex := skipvertex + 1;

         ELSE

            ordkount := ordkount + 1;
            new_ordinates(ordkount) := remaining_edge_geom.sdo_ordinates( (i*2) - 1 );
            ordkount := ordkount + 1;
            new_ordinates(ordkount) := remaining_edge_geom.sdo_ordinates(i*2);

         END IF;

      END LOOP;

      IF skipvertex <> 1
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Didnt find a match using removed node_geom ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(nodegeom))
                                      || ' against edge id ' || remaining_edge_id);

      END IF;

      reshaped_edge_geom := remaining_edge_geom;
      reshaped_edge_geom.sdo_ordinates := new_ordinates;

      --hmm why not use edge.removevertex method??
      --Dont have a topo object and face id to work with
      
      GZ_TOPO_UTIL.GZ_CHANGE_EDGE_COORDS(self.topology,
                                         remaining_edge_id,
                                         reshaped_edge_geom);
   
   END Zap;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   
   MEMBER PROCEDURE ZapIsolated
   AS
      
   BEGIN
   
      --could verify its isolated
      --not gonna bother since we are usually de-assembling some junk at this point
      --and the original object may be dirty
      
      --let oracle handle the topomap since I really dont know whats needed for isolated nodes
      SDO_TOPO_MAP.REMOVE_NODE(self.topology,
                               self.GetNodeId);
   
   END ZapIsolated;
   
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