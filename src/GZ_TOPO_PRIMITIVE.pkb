CREATE OR REPLACE PACKAGE BODY GZ_TOPO_PRIMITIVE
AS

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE DBUG_THIS (
      p_dbug               IN NUMBER,
      p_sql                IN VARCHAR2
   )
   AS

   BEGIN

      IF p_dbug = 1
      THEN

         dbms_output.put_line(p_sql);

      ELSIF p_dbug = 2
      THEN

         RAISE_APPLICATION_ERROR(-20001, p_sql);

      END IF;

   END DBUG_THIS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE ADD_A_FACE (
      p_topology           IN VARCHAR2,
      p_feature_table      IN VARCHAR2,
      p_key                IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_tg_layer_id        IN NUMBER,   --new, not in smpoly version
      p_key_col            IN VARCHAR2  --caller should pass in GEO_ID or FACE_ID
   )
   AS

      --Matt! 10/15/12
      --Copied and modified from GZ_SMPOLY 3/12/14

      --Deletes a face from a feature table
      --Table must be 0-level since its a face
      --Note that higher level parents built on these 0-levels will automatically
      --   include the new child face

      psql                 VARCHAR2(4000);

   BEGIN

      --Use the constructor

      psql := 'UPDATE ' ||  p_feature_table || ' a '
           || 'SET '
           || 'a.topogeom = '  --Topology name, topo geom type, tg_layer_id, topo to add (face_id, type), no topo to delete
           || 'SDO_TOPO_GEOMETRY(:p1, :p2, :p3, SDO_TOPO_OBJECT_ARRAY(SDO_TOPO_OBJECT(:p4,:p5)), NULL) '
           || 'WHERE a.' || p_key_col || ' = :p6 ';

      EXECUTE IMMEDIATE psql USING p_topology,
                                   3,
                                   p_tg_layer_id,
                                   p_face_id,
                                   3,
                                   p_key;

      COMMIT;

   END ADD_A_FACE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION DELETE_A_FACE (
      p_topology           IN VARCHAR2,
      p_feature_table      IN VARCHAR2,
      p_key                IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_tg_layer_id        IN NUMBER,                      --new, not in smpoly version
      p_key_col            IN VARCHAR2,                    --caller should pass in GEO_ID or FACE_ID
      p_dbug               IN NUMBER DEFAULT 0
   ) RETURN NUMBER
   AS

      --Matt! 10/15/12
      --Copied and modified from GZ_SMPOLY 3/12/14

      --This one returns 1 for deleted a face, 0 for feature extinction
      --Table should be 0-level since it works on a face
      --Either the face feature table or some other 0-level table

      --Like add_a_face coded above, higher level feature table parents are automatically handled
      --   If a face is simply removed from this 0-level layer, the parents will reshape automatically
      --   If the record on this 0-level layer goes extinct, the code here will call recursively up
      --      to parent layers, either removing the tgl_object pointing to children, or deleting the parent
      --      record entirely if it too goes extinct.

      psql                 VARCHAR2(4000);
      face_kount           PLS_INTEGER;
      output               NUMBER;

   BEGIN

      psql := 'SELECT COUNT(t.topo_id) FROM '
           || p_feature_table || ' a, '
           || 'TABLE(a.topogeom.get_topo_elements()) t '
           || 'WHERE '
           || 'a.' || p_key_col || ' = :p1 ';

      EXECUTE IMMEDIATE psql INTO face_kount USING p_key;

      IF face_kount > 1
      THEN

         output := 1;

         --we may use the constructor, leaving at least one face with the feature
         --other than the one we are deleting

         psql := 'UPDATE ' ||  p_feature_table || ' a '
              || 'SET '
              || 'a.topogeom = '  --Topology name, topo geom type, tg_layer_id, no topo to add, topo to delete (face_id, type)
              || 'SDO_TOPO_GEOMETRY(:p1, :p2, :p3, NULL, SDO_TOPO_OBJECT_ARRAY(SDO_TOPO_OBJECT(:p4,:p5))) '
              || 'WHERE a.' || p_key_col || ' = :p6 ';

         EXECUTE IMMEDIATE psql USING p_topology,
                                      3,
                                      p_tg_layer_id,
                                      p_face_id,
                                      3,
                                      p_key;

         COMMIT;

      ELSIF face_kount = 1
      THEN

         output := 0;

         --as far as I know this is best practice
         --Just delete the feature and the topogeom takes care of itself

         psql := 'DELETE FROM ' || p_feature_table || ' a '
              || 'WHERE a.' || p_key_col || ' = :p1 ';

         BEGIN

            EXECUTE IMMEDIATE psql USING p_key;

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%Cannot delete a TG object with dependent parent objects%'
            THEN

               --For simple shape changes we keep track of higher level parents
               --And update their measurements
               --In this case though we must remove the child tgl_object from the parent
               --before we delete it in the child

               --This sub will work recursively up through the grandparent and etc layers as necessary

               GZ_TOPO_PRIMITIVE.DELETE_PARENT_TGL_OBJECT(p_topology,
                                                          p_feature_table,
                                                          p_key,
                                                          p_key_col,
                                                          0,            --recursive depth 0 to start
                                                          p_dbug);

               --delete child record again now that we are clear
               EXECUTE IMMEDIATE psql USING p_key;

            ELSE

               RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;

         COMMIT;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Got a face kount of ' || face_kount || ' for ' || p_feature_table || ' ' || p_key);

      END IF;

      RETURN output;

   END DELETE_A_FACE;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   PROCEDURE DELETE_PARENT_TGL_OBJECT (
      p_topology           IN VARCHAR2,
      p_child_table        IN VARCHAR2,
      p_child_key          IN VARCHAR2,
      p_child_key_col      IN VARCHAR2,
      p_depth              IN NUMBER DEFAULT 0,
      p_dbug               IN NUMBER DEFAULT 0
   )
   AS

      --Matt! 10/23/12
      --3/12/14 Copied and modified from gz_smpoly

      --We wish to delete an entire record in a low level of the topo hierarchy (no faces will remain)
      --   Delete requires we pay attention to parent feature layers
      --who a single level up in the hierarchy has a relationship to our child?  May be several layers
      --which specific record at that one level up has the topo_id, topo_type matching the child?
      --   If more than one record on a feature layer, definitely an error in GZ business. Indicates overlap
      --If one parent record per layer, either
      --  use constructor with special SDO_TGL_OBJECT_ARRAY to subtract the relationship to the child
      --  if theres only one sdo_tgl_object in the parent we are deleting, call recursively to next parent up
      --     then delete the current parent

       --assumption
      --child key can be anything - ie either face_id or geo_id
      --parent key, ie anything level 1 and above, will be geo_id

      psql                    VARCHAR2(4000);
      parent_tables           MDSYS.STRINGLIST;
      parent_objects_kount    PLS_INTEGER;
      child_tgl               MDSYS.SDO_TGL_OBJECT;
      parent_tg_layer_id      NUMBER;
      parent_key              VARCHAR2(4000);


   BEGIN

      IF p_depth > 10
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Topology prob doesnt have ' || p_depth || ' layer levels');

      END IF;

      --who a single level up in the hierarchy has a relationship to our child?  May be several layers

      psql := 'SELECT b.table_name '
           || 'FROM '
           || 'user_sdo_topo_info a, '
           || 'user_sdo_topo_info b '
           || 'WHERE '
           || 'a.table_name = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.tg_layer_type = :p3 AND '
           || 'a.column_name = :p4 AND '
           || 'b.topology = :p5 AND '
           || 'b.tg_layer_type = :p6 AND '
           || 'b.column_name = :p7 AND '
           || 'a.tg_layer_id = b.child_layer_id ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO parent_tables USING UPPER(p_child_table),
                                                                   UPPER(p_topology),
                                                                   'POLYGON',
                                                                   'TOPOGEOM',
                                                                   UPPER(p_topology),
                                                                   'POLYGON',
                                                                   'TOPOGEOM';

      FOR i IN 1 .. parent_tables.COUNT
      LOOP

         --set to 0, if it remains 0 theres a problem
         parent_objects_kount := 0;

         --which specific record at that one level up has the topo_id, topo_type matching the child?

         parent_key := GZ_TOPO_PRIMITIVE.GET_PARENT_OF_CHILD(p_topology,
                                                             p_child_table,
                                                             p_child_key_col,
                                                             p_child_key,
                                                             parent_tables(i),
                                                             'GEO_ID');  --always level 1 and above is geoid in our business

         --count how many tg_ids

         psql := 'SELECT COUNT(t.tg_id) '
              || 'FROM ' || parent_tables(i) || ' a, '
              || 'TABLE(a.topogeom.get_tgl_objects()) t '
              || 'WHERE '
              || 'a.geo_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO parent_objects_kount USING parent_key;

         IF parent_objects_kount > 1
         THEN

            --use constructor with special SDO_TGL_OBJECT_ARRAY to subtract the relationsip
            --the rest of the pointers in this parent record will remain alive

            --REMEMBER we are deleting the child, not reshaping it, thats why we are here
            --so we know theres just one tgl pointer

            psql := 'SELECT SDO_TGL_OBJECT(a.topogeom.tg_layer_id, a.topogeom.tg_id) '
                 || 'FROM ' || p_child_table || ' a '
                 || 'WHERE '
                 || 'a.' || p_child_key_col || ' = :p1 ';

            GZ_TOPO_PRIMITIVE.DBUG_THIS(p_dbug, psql);

            EXECUTE IMMEDIATE psql INTO child_tgl USING p_child_key;

            parent_tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_topology,
                                                               parent_tables(i),
                                                               'TOPOGEOM',
                                                               'POLYGON');

            --constructor subtraction on parent
            psql := 'UPDATE ' || parent_tables(i) || ' b '
                 || 'SET b.topogeom = SDO_TOPO_GEOMETRY( '
                 || ':p1, '                                 --topology
                 || ':p2, '                                 --topology geometry type
                 || ':p3, '                                 --parent tg_layer_id
                 || 'NULL, '                                --No ids to add
                 || 'SDO_TGL_OBJECT_ARRAY(:p4) '            --one id to delete
                 || ') '
                 || 'WHERE b.geo_id = :p5 ';                 --always geo_id bustah!

            GZ_TOPO_PRIMITIVE.DBUG_THIS(p_dbug, psql);

            EXECUTE IMMEDIATE psql USING p_topology,
                                         3,
                                         parent_tg_layer_id,
                                         child_tgl,
                                         parent_key;

            COMMIT;

         ELSIF parent_objects_kount = 1
         THEN

            --if theres only one sdo_tgl_object in the guy we are deleting, call recursively to next parent up

            GZ_SMPOLY.DELETE_PARENT_TGL_OBJECT(p_topology,
                                               parent_tables(i),  --parent becomes --> child
                                               parent_key,
                                               'GEO_ID',          --GEOID and turtles all the way up
                                               (p_depth + 1));    --protect from inifinite recursion

            --Now should be safe to delete the entire record wherever we are in the recursion
            psql := 'DELETE FROM ' || parent_tables(i) || ' a '
                 || 'WHERE a.geo_id = :p1 ';

            EXECUTE IMMEDIATE psql USING parent_key;

            COMMIT;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, 'Woops, somethings wrong. Saw a parent but then didnt get a tg_id');

         END IF;

      END LOOP;


   END DELETE_PARENT_TGL_OBJECT;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GET_PARENT_OF_CHILD (
      p_topology           IN VARCHAR2,
      p_child_table        IN VARCHAR2,
      p_child_key_col      IN VARCHAR2,
      p_child_key          IN VARCHAR2,
      p_parent_table       IN VARCHAR2,
      p_parent_key_col     IN VARCHAR2 DEFAULT 'GEO_ID'
   ) RETURN VARCHAR2
   AS

      --Matt! 10/23/12
      --3/12/14 Moved and modified from GZ_SMPOLY

      --Returns the geo_id of the parent layer for a given child
      --assumption: always GEO_ID key as a varchar key up in the hierarchy, and TOPOGEOM topo

      psql              VARCHAR2(4000);
      geo_id             VARCHAR2(4000);

   BEGIN

      --theres probably an equivalent relation$ SQL

      psql := 'SELECT a.' || p_parent_key_col || ' '
           || 'FROM ' || p_parent_table || ' a, '
           || 'TABLE(a.topogeom.get_tgl_objects()) t, '
           || p_child_table || ' b '
           || 'WHERE '
           || 't.tgl_id = b.topogeom.tg_layer_id AND '
           || 't.tg_id = b.topogeom.tg_id AND '
           || 'b.' || p_child_key_col || ' = :p1 ';

      EXECUTE IMMEDIATE psql INTO geo_id USING p_child_key;

      RETURN geo_id;

   END GET_PARENT_OF_CHILD;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_EDGE_MBR (
      p_topo            IN VARCHAR2,
      p_edge            IN NUMBER
   ) RETURN GZ_TYPES.numberarray
   AS

      --Matt! 9/6/10
      --Copied from clip 11/06/12
      --Copied and modified from GZ_SMPOLY 3/18/14

      psql              VARCHAR2(4000);
      output            GZ_TYPES.numberarray;

   BEGIN

      psql := 'SELECT '
           || 'SDO_GEOM.SDO_MIN_MBR_ORDINATE(e.geometry,1) llx, '
           || 'SDO_GEOM.SDO_MIN_MBR_ORDINATE(e.geometry,2) lly, '
           || 'SDO_GEOM.SDO_MAX_MBR_ORDINATE(e.geometry,1) urx, '
           || 'SDO_GEOM.SDO_MAX_MBR_ORDINATE(e.geometry,2) ury '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.edge_id = :p1';
      EXECUTE IMMEDIATE psql INTO output(1),
                                  output(2),
                                  output(3),
                                  output(4) USING p_edge;

      RETURN output;


   END GET_EDGE_MBR;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

    FUNCTION ADD_NEW_NODE (
      p_topo               IN VARCHAR2,
      p_edge               IN NUMBER,
      p_coord_index        IN NUMBER,
      p_node               IN SDO_GEOMETRY,
      p_is_new_shape_pt    IN VARCHAR2
   ) RETURN NUMBER
   AS

      --Matt! 4/03/10
      --Copied from clip and modified 11/06/12
      --Copied from GZ_SMPOLY and modified 3/18/14
      --Add a node to the topology
      --Return the new node id
      --Handle errors

      output            NUMBER;
      topomap           VARCHAR2(4000);
      edge_mbr          GZ_TYPES.numberarray;
      create_it         NUMBER;

   BEGIN


      output := SDO_TOPO_MAP.ADD_NODE(p_topo,
                                      p_edge,
                                      p_node,
                                      p_coord_index,
                                      p_is_new_shape_pt);


      RETURN output;

   EXCEPTION
      WHEN OTHERS
      THEN


         IF SQLCODE = -29532
         THEN


            IF SQLERRM LIKE '%add node results in an intersection with another edge%'
            THEN

               RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            ELSIF SQLERRM LIKE '%is not on the boundary of one or two of the faces it links%'
            OR SQLERRM LIKE '%an updatable TopoMap object already exists%'
            THEN

               --use an explicit topomap
               --or someone else has been using one and we need to clean house

               topomap := p_topo || '_TOPOMAP';

               edge_mbr := GZ_TOPO_PRIMITIVE.GET_EDGE_MBR(p_topo,
                                                          p_edge);

               create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topomap,
                                                            p_topo,
                                                            2,
                                                            edge_mbr(1),
                                                            edge_mbr(2),
                                                            edge_mbr(3),
                                                            edge_mbr(4));

               output := SDO_TOPO_MAP.ADD_NODE(NULL,  --topomap
                                               p_edge,
                                               p_node,
                                               p_coord_index,
                                               p_is_new_shape_pt);

               create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topomap,
                                                            p_topo,
                                                            create_it);


               RETURN output;

            ELSIF SQLERRM LIKE '%Add node results in two pieces of split edge intersecting each other%'
            THEN

               --This means we probably are working with the wrong coord index
               --No error handling here per se (yet) requires fix in the caller

               RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            ELSE

               RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            END IF;


         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

         END IF;



   END ADD_NEW_NODE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION ADD_NEW_EDGE (
      p_topo            IN VARCHAR2,
      p_start_node_id   IN NUMBER,
      p_end_node_id     IN NUMBER,
      p_edge_geom       IN SDO_GEOMETRY DEFAULT NULL
   ) RETURN NUMBER
   AS

      --Matt! 3/24/14 copied and modified from gz_smpoly version
      --Add a new edge
      --Return the new edge id
      --If you leave off the sdogeometry the function will build a straight line
      --   from start node to end node
      --Handle errors somehow

      --sample
      --   declare
      --      new_edge_id number;
      --   begin
      --      new_edge_id := gz_topo_primitive.add_new_edge('Z899IN', 42773, 42216);
      --   end;

      new_edge_id       NUMBER;
      psql              VARCHAR2(4000);
      start_node_geom   SDO_GEOMETRY;
      end_node_geom     SDO_GEOMETRY;
      new_edge_geom     SDO_GEOMETRY;
      kount             PLS_INTEGER;
      connect_edges     GZ_TYPES.numberarray;

   BEGIN

      IF p_edge_geom IS NULL
      THEN

         --Build the new edge, start node --> end node
         --We dont trust any geometry except what we see living in the now

         start_node_geom := GZ_TOPO_PRIMITIVE.GET_NODE_GEOMETRY(p_topo,
                                                                p_start_node_id);

         end_node_geom := GZ_TOPO_PRIMITIVE.GET_NODE_GEOMETRY(p_topo,
                                                              p_end_node_id);

         --we want direction of new node to old node
         new_edge_geom := SDO_GEOMETRY(2002,
                                       start_node_geom.SDO_SRID,
                                       NULL,
                                       SDO_ELEM_INFO_ARRAY(1, 2, 1),
                                       SDO_ORDINATE_ARRAY(start_node_geom.sdo_point.X,
                                                          start_node_geom.sdo_point.Y,
                                                          end_node_geom.sdo_point.X,
                                                          end_node_geom.sdo_point.Y
                                                          )
                                   );

      ELSIF p_edge_geom.sdo_gtype = 2002
      THEN

         new_edge_geom := p_edge_geom;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Got a weird gtype for the input ' || p_edge_geom.sdo_gtype);

      END IF;


      --Find the edges in the topology that are supposed to touch this new edge
      --
      --             |  <--- Who knows whats connected outside the face
      --             |
      --      -------0--------
      --             |<-- New edge, doesnt exist in topo yet
      --             |
      --           --|--------   <-- An unexpected edge HERE is bad
      --             |
      --      -------0-------
      --            / \  <-- More edges I dont care about


      psql := 'SELECT DISTINCT e.edge_id '
           || 'FROM ' || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.start_node_id IN (:p1,:p2) OR '
           || 'end_node_id IN (:p3,:p4) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO connect_edges USING p_end_node_id, p_start_node_id,
                                                                   p_end_node_id, p_start_node_id;

      --Check to see if our new planned edge crosses some other edges
      --I suppose the alternative here would be to try the add and respond to errors

      psql := 'SELECT count(*) '
           || 'FROM ' || p_topo || '_EDGE$ e '
           || 'WHERE e.edge_id NOT IN (SELECT * FROM TABLE(:p1)) AND '
           || 'SDO_RELATE(e.geometry,:p2,:p3) = :p4 ';

      EXECUTE IMMEDIATE psql INTO kount USING GZ_BUSINESS_UTILS.NUMARRAY_TO_VARRAY(connect_edges),
                                              new_edge_geom,
                                              'mask=ANYINTERACT',
                                              'TRUE';

      IF kount > 0
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'New edge intersects with ' || kount || ' unexpected edges. ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(new_edge_geom)));

      END IF;

      BEGIN

         new_edge_id := SDO_TOPO_MAP.ADD_EDGE(p_topo,
                                              p_start_node_id,
                                              p_end_node_id,
                                              new_edge_geom);

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -29532
         THEN

            --Lets see what we scare up here
            RAISE_APPLICATION_ERROR(-20001, 'Old node: ' || p_start_node_id || ' New node: ' || p_end_node_id
                                            || ' Edge geom: ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(new_edge_geom))
                                            || ' ' || SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

         END IF;

      END;

     RETURN new_edge_id;

   END ADD_NEW_EDGE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE REMOVE_EDGE (
      p_topo           IN VARCHAR2,
      p_edge_id        IN VARCHAR2
   )
   AS

      --Matt! 9/10/12
      --caller must manage feature layers - this is just the topo util wrapper

      --Copied verbatim from the version in gz_clip
      --Recopied from gz_smpoly to here 3/18/14

      psql              VARCHAR2(4000);
      window_geom       SDO_GEOMETRY;
      create_it         NUMBER;


   BEGIN

      psql := 'SELECT SDO_GEOM.SDO_MBR(e.geometry) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO window_geom USING p_edge_id;

      IF window_geom.sdo_ordinates.COUNT != 4
      THEN

        RAISE_APPLICATION_ERROR(-20001,'Bad window. Bad!');

      END IF;

      --create the topomap with window

      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                   p_topo,
                                                   2,
                                                   window_geom.sdo_ordinates(1),
                                                   window_geom.sdo_ordinates(2),
                                                   window_geom.sdo_ordinates(3),
                                                   window_geom.sdo_ordinates(4)
                                                   );

      --no error handler here, let the caller trap and release
      SDO_TOPO_MAP.REMOVE_EDGE(NULL,p_edge_id);

      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);


   END REMOVE_EDGE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   PROCEDURE UPDATE_MERGED_FACE_FEATURES (
      p_face_tab        IN VARCHAR2,
      p_face_1          IN NUMBER,
      p_face_2          IN NUMBER
   )
   AS

      --Matt! 3/19/14
      --Removed an edge between two faces
      --Update the face_feature table face_id of the remaining
      --  feature to match the primitive face_id (if necessary)

      psql              VARCHAR2(4000);
      face_id           NUMBER;
      topo_id           NUMBER;

   BEGIN

      psql := 'SELECT a.face_id, t.topo_id '
           || 'FROM ' || p_face_tab || ' a, '
           || 'TABLE(a.topogeom.get_topo_elements()) t '
           || 'WHERE a.face_id IN (:p1, :p2) ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO face_id,
                                     topo_id USING p_face_1,
                                                   p_face_2;

      EXCEPTION
      WHEN OTHERS
      THEN

         --probably two topos or two faces, this is an error
         --we dont allow such a state to exist except in the thick of editing
         RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace || ' on '
                                         || psql || ' using ' || p_face_1 || ',' || p_face_1);

      END;

      IF face_id <> topo_id
      THEN

         psql := 'UPDATE ' || p_face_tab || ' a '
              || 'SET a.face_id = :p1 '
              || 'WHERE a.face_id = :p2 ';

         EXECUTE IMMEDIATE psql USING topo_id,
                                      face_id;

         COMMIT;

      END IF;

   END UPDATE_MERGED_FACE_FEATURES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_NODE_GEOMETRY (
      p_topo            IN VARCHAR2,
      p_node_id         IN VARCHAR2
   ) RETURN SDO_GEOMETRY
   AS

      psql     VARCHAR2(4000);
      output   SDO_GEOMETRY;

   BEGIN

      psql := 'SELECT n.geometry '
           || 'FROM ' || p_topo || '_NODE$ n '
           || 'WHERE n.node_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO output USING p_node_id;


      RETURN output;

   END GET_NODE_GEOMETRY;

------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION GET_NODE_TIP (
      p_topology           IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_node_id            IN NUMBER
   ) RETURN VARCHAR2
   AS

      --this was called by add_new_edge to maintain directionality of the added edge
      --decided for now that I dont need it, so this may be refuse code

      --Matt! copied and modified from gz_smpoly version 3/24/14
      --Is the node the START or the END of the edge
      --no loops allowed

      psql                 VARCHAR2(4000);
      output               VARCHAR2(8);

   BEGIN

      psql := 'SELECT node '
           || 'FROM ' || p_topology || '_edge$ UNPIVOT (nodes '
           || 'FOR node '
           || 'IN (start_node_id AS :p1, end_node_id AS :p2)) '
           || 'WHERE '
           || 'edge_id = :p3 AND '
           || 'nodes = :p4 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING 'START',
                                                  'END',
                                                  p_edge_id,
                                                  p_node_id;

      EXCEPTION
      WHEN OTHERS
      THEN

          RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' on edge ' || p_edge_id || ', node' || p_node_id
                                         || '-- ' || psql );

      END;

      RETURN output;

   END GET_NODE_TIP;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION REMOVE_NODE_KINDLY (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 3/28/14 copied and modified from version in gz_smpoly

      --Exits cleanly if node has already been removed, or is simply not obsolete
      --Otherwise removes the obsolete node, and thats it.  Leaves vertex

      edge_ids             GZ_TYPES.numberarray;
      psql                 VARCHAR2(4000);
      window_geom          SDO_GEOMETRY;
      create_it            NUMBER;


   BEGIN

      psql := 'SELECT e.edge_id '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.start_node_id = :p1 OR e.end_node_id = :p2 ';


      EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_ids USING p_node_id,
                                                              p_node_id;

      IF edge_ids.COUNT > 2
      OR edge_ids.COUNT = 0
      THEN

         --not obsolete, either 0 or 2+
         RETURN 0;

      ELSIF edge_ids.COUNT = 1
      THEN

         --Maybe this is a ring edge?  Let's leave it be for now
         RETURN 0;
         --RAISE_APPLICATION_ERROR(-20001,'How is node ' || p_node_id || ' connected to just one edge?');

      END IF;

      --set up topomap to cover the extent of both edges

      psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 OR '
           || 'e.edge_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO window_geom USING edge_ids(1),
                                                    edge_ids(2);

      --create the topomap with window

      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                   p_topo,
                                                   2,
                                                   window_geom.sdo_ordinates(1),
                                                   window_geom.sdo_ordinates(2),
                                                   window_geom.sdo_ordinates(3),
                                                   window_geom.sdo_ordinates(4)
                                                   );

      BEGIN

         SDO_TOPO_MAP.REMOVE_NODE(NULL,
                                  p_node_id);

      EXCEPTION
      WHEN OTHERS
      THEN

         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

         RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

      END;

      --commit and drop temp topomap
      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

      --not guaranteed buddy
      RETURN 1;


   END REMOVE_NODE_KINDLY;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC----------------------------------------------------------------------------

   PROCEDURE ZAP_NODE (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   )
   AS

      --Matt! 12/12/11
      --Removes a node from the topology. Node must be attached to two edges
      --Then also removes all trace of the ghost vertex

      node_geom            SDO_GEOMETRY;
      edge_ids             GZ_TYPES.numberarray;
      psql                 VARCHAR2(4000);
      window_geom          SDO_GEOMETRY;
      create_it            NUMBER;
      remaining_edge_id    NUMBER;
      remaining_edge_geom  SDO_GEOMETRY;
      reshaped_edge_geom   SDO_GEOMETRY;
      new_ordinates        SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();
      ordkount             PLS_INTEGER := 0;
      skipvertex           PLS_INTEGER := 0;

   BEGIN

      node_geom := GET_NODE_GEOMETRY(p_topo,
                                     p_node_id);

      psql := 'SELECT e.edge_id '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.start_node_id = :p1 OR e.end_node_id = :p2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_ids USING p_node_id,
                                                              p_node_id;

      IF edge_ids.COUNT != 2
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Zap node expects node to be bound by 2 edges ');

      END IF;

      --set up topomap to cover the extent of both edges. This necessary?

      psql := 'SELECT SDO_AGGR_MBR(e.geometry) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 OR '
           || 'e.edge_id = :p2 ';

      EXECUTE IMMEDIATE psql INTO window_geom USING edge_ids(1),
                                                    edge_ids(2);

      --create the topomap with window

      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',
                                                   p_topo,
                                                   2,
                                                   window_geom.sdo_ordinates(1),
                                                   window_geom.sdo_ordinates(2),
                                                   window_geom.sdo_ordinates(3),
                                                   window_geom.sdo_ordinates(4)
                                                   );

      BEGIN

         SDO_TOPO_MAP.REMOVE_NODE(NULL,
                                  p_node_id);

      EXCEPTION
      WHEN OTHERS
      THEN

         create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

         RAISE_APPLICATION_ERROR(-20001,SQLERRM || Chr(10) || DBMS_UTILITY.format_error_backtrace);

      END;

      --commit and drop temp topomap
      create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topo || 'MAP',p_topo,3);

      --one of the edges should now be gone, find which edge remains
      psql := 'SELECT e.edge_id, e.geometry '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || 'e.edge_id = :p1 OR '
           || 'e.edge_id = :p2 ';
      
      EXECUTE IMMEDIATE psql INTO remaining_edge_id,
                                  remaining_edge_geom USING edge_ids(1),
                                                            edge_ids(2);

      --Reshape the remaining edge to not include the position we saved

      new_ordinates.EXTEND(remaining_edge_geom.sdo_ordinates.COUNT - 2);

      FOR i IN 1 .. remaining_edge_geom.sdo_ordinates.COUNT/2
      LOOP
         
         IF remaining_edge_geom.sdo_ordinates( (i*2) - 1 ) = node_geom.SDO_POINT.X
         AND remaining_edge_geom.sdo_ordinates(i*2) = node_geom.SDO_POINT.Y
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

      IF skipvertex != 1
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Didnt find a match using removed node_geom ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(node_geom))
                                      || ' against edge id ' || remaining_edge_id);

      END IF;

      reshaped_edge_geom := remaining_edge_geom;
      reshaped_edge_geom.sdo_ordinates := new_ordinates;

      GZ_TOPO_UTIL.GZ_CHANGE_EDGE_COORDS(p_topo,
                                         remaining_edge_id,
                                         reshaped_edge_geom);


   END ZAP_NODE;

    ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PUBLIC---------------------------------------------------------------------------

   FUNCTION IS_NODE_UNIVERSAL (
      p_topo               IN VARCHAR2,
      p_node_id            IN NUMBER
   ) RETURN BOOLEAN
   AS

      --MATT! 11/22/11

      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(1) '
           || 'FROM '
           || p_topo || '_edge$ e '
           || 'WHERE '
           || '(e.start_node_id = :p1 OR e.end_node_id = :p2) AND '
           || '(e.left_face_id = :p3 or e.right_face_id = :p4) ';

      EXECUTE IMMEDIATE psql INTO kount USING p_node_id, p_node_id,
                                              -1, -1;

      IF kount = 0
      THEN

         RETURN FALSE;

      ELSIF kount = 2
      THEN

         RETURN TRUE;

      ELSIF kount = 1
      THEN

         --This is an island edge that forms a full ring
         --Any outlier cases to worry about here?
         RETURN TRUE;

      ELSIF kount > 2
      THEN

         RAISE_APPLICATION_ERROR(-20001,'You have a bowtie topology! Remove this error if thats cool');


      END IF;


   END IS_NODE_UNIVERSAL;



END GZ_TOPO_PRIMITIVE;
/
