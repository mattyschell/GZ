CREATE OR REPLACE
TYPE BODY gz_feature_face
AS


   --Matt! 7/12/13
   --Attempt to wrap the whole coastal sliver ID and removal process around a face feature object
   --Transient objects are instances of objects that aren't stored in the database. They
   --   have a lifetime limited to their use in a PL/SQL block

   --SAMPLE USAGE
   --declare
   --   work_face GZ_FEATURE_FACE;
   --
   --begin
   --
   --   work_face := gz_feature_face('Z606LS','Z606LS_CLIP_FACE',7453, p_dbug=>1);
   --   work_face.set_coastal_sliver(250,500);
   --   --work_face.describe_face_object;
   --
   --    if work_face.coastal_sliver = 'SLIVER'
   --    then
   --       work_face.collapse_to_universal;
   --    end if;
   --
   --end;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --Default constructor
   CONSTRUCTOR FUNCTION gz_feature_face
   RETURN SELF AS RESULT
   AS
   BEGIN
      RETURN;

   END gz_feature_face;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   CONSTRUCTOR FUNCTION gz_feature_face(
      p_topology_name      IN VARCHAR2,
      p_table_name         IN VARCHAR2,
      p_face_id            IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_dbug               IN NUMBER DEFAULT NULL
   ) RETURN SELF AS RESULT
   AS

   BEGIN

      self.topology_name := UPPER(p_topology_name);
      self.table_name    := UPPER(p_table_name);
      self.face_id       := p_face_id;
      self.tolerance     := p_tolerance;

      self.set_topo_info();

      IF self.srid = 8265
      AND self.tolerance = .05
      THEN
         self.null_tolerance := 0.0000005;
      ELSIF self.srid = 8265
      AND self.tolerance <> .05
      THEN
         self.null_tolerance := GZ_CLIP.TOLERANCE_CONVERTER(self.tolerance, self.srid, 'NULL');
      ELSE
         self.null_tolerance := self.tolerance;
      END IF;

      self.set_boundary_edges();

      self.set_coastal_edges();

      IF p_dbug IS NOT NULL
      THEN

         self.dbug := 1;

      END IF;

      RETURN;

   END gz_feature_face;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_topo_info
   AS

      psql           VARCHAR2(4000);

   BEGIN

      psql := 'SELECT tg_layer_id, srid '
           || 'FROM user_sdo_topo_info a '
           || 'WHERE '
           || 'a.table_name = :p1 AND '
           || 'a.topology = :p2 ';

      EXECUTE IMMEDIATE psql INTO self.tg_layer_id,
                                  self.srid USING self.table_name,
                                                  self.topology_name;

      psql := 'SELECT a.topogeom.tg_id, a.sdogeometry '
           || 'FROM ' || self.table_name || ' a '
           || 'WHERE '
           || 'a.face_id = :p1 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO self.tg_id,
                                     self.sdogeometry USING self.face_id;

      EXCEPTION
      WHEN OTHERS
      THEN

         --for instantiating temporary faces that arent in the face feature table
         self.tg_id := NULL;
         self.sdogeometry := NULL;

      END;


   END set_topo_info;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_feature_layers
   AS

      psql                    VARCHAR2(4000);
      all_layers              MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      feature_tables          MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      feature_table_geoids    MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      kount                   PLS_INTEGER := 0;
      current_layer_level     NUMBER;
      current_key             VARCHAR2(4000);

   BEGIN

      --get all poly layers in the topo
      psql := 'SELECT a.table_name FROM user_sdo_topo_info a '
           || 'WHERE a.topology = :p1 AND '
           || 'a.tg_layer_type = :p2 AND '
           || 'a.table_name <> :p3 '
           || 'ORDER BY a.tg_layer_level, a.table_name ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO all_layers USING self.topology_name,
                                                                'POLYGON',
                                                                self.table_name;

      --loop through each and determine if the face participates in the layer, and what geoid

      FOR i IN 1 .. all_layers.COUNT
      LOOP

         current_layer_level := GZ_SMPOLY.GET_TG_LAYER_LEVEL(self.topology_name,
                                                             all_layers(i));

         BEGIN

            current_key := GZ_BUILD_SOURCE.GZ_GET_OID_FROM_FACE(USER,
                                                                self.topology_name,
                                                                all_layers(i),
                                                                current_layer_level,
                                                                self.face_id,
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

      self.feature_tables       := feature_tables;
      self.feature_table_geoids := feature_table_geoids;

   END set_feature_layers;

    ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_feature_layers(
      p_feature_tables           IN MDSYS.STRINGLIST,
      p_feature_table_geoids     IN MDSYS.STRINGLIST
   ) AS

      --M@! 7/26/13
      --overloaded for passing feature layer info on to a sibling during face mitosis

   BEGIN

      self.feature_tables       := p_feature_tables;
      self.feature_table_geoids := p_feature_table_geoids;

   END set_feature_layers;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_face_id(
      p_face_id                  IN NUMBER
   )

   AS

      --during geometry processing faces get split
      --On one side we have territory that is moments away from being annihilated
      --On the other side we have remaining territory of the original face, with a possibly
      --  new face_id
      psql              VARCHAR2(4000);

   BEGIN

      IF p_face_id <> self.face_id
      THEN

         self.face_id := p_face_id;

      END IF;

   END set_face_id;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_feature_tables
   RETURN MDSYS.STRINGLIST
   AS

   BEGIN

      RETURN self.feature_tables;

   END get_feature_tables;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_feature_table_geoids
   RETURN MDSYS.STRINGLIST
   AS

   BEGIN

      RETURN self.feature_table_geoids;

   END get_feature_table_geoids;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE match_features(
      p_neighbor_face      IN NUMBER
   )
   AS

      --M@! 7/25/13
      --For any face pair, manage the FSLS built on top to allow a merge
      --The neighbor face is to be the winner.
      --In typical usage the self.face is a little guy getting swallowed

      --For 0-level FSLS:
      --  When geoids both exist for a fsl but value is diff across the face divide
      --     +/- Add the self.face to the neighbor fsl, subtract the self.face from the self FSL (may delete it)
      --  When neighbor fsl exists and self doesnt, add the self.face to the neighbor FSL
      --  When self fsl exists and neighbor doesnt, subtract the self.face from the self FSL (may delete it)
      --For 1+ level FSLS:
      --  Do nothing.  0 level should take care of it
      --  However caller must be aware of possible measurement changes in hierarchy if child FSL features were edited in the above

      neighbor_face        GZ_FEATURE_FACE := gz_feature_face();
      neighbor_fsls        MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      neighbor_geoids      MDSYS.STRINGLIST := MDSYS.STRINGLIST();
      self_position        NUMBER;
      neighbor_position    NUMBER;
      edited_record        NUMBER;

   BEGIN

      --instantiate a face object for the neighbor. wooooot
      neighbor_face := gz_feature_face(self.topology_name,
                                       self.table_name,
                                       p_neighbor_face,
                                       self.tolerance);

      --doesnt include _FACE table
      neighbor_face.set_feature_layers;

      neighbor_fsls   := neighbor_face.get_feature_tables;
      neighbor_geoids := neighbor_face.get_feature_table_geoids;

      IF self.feature_tables IS NULL
      THEN

         --in case caller hasnt done it
         self.set_feature_layers;

      END IF;

      IF self.dbug = 1
      THEN
         dbms_output.put_line('self before match dets: ');
         dbms_output.put_line(self.describe_face_object);
         dbms_output.put_line('neighbor before match dets: ');
         dbms_output.put_line(neighbor_face.describe_face_object);
      END IF;

      FOR i IN 1 .. neighbor_fsls.COUNT
      LOOP

         --  When geoids both exist for a fsl but value is diff across the face divide
         --     +/- Add the self.face to the neighbor fsl, subtract the self.face from the self FSL (may delete it)
         --  When neighbor fsl exists and self doesnt, add the self.face to the self FSL

         IF GZ_TOPO_UTIL.GET_TG_LAYER_LEVEL(neighbor_face.topology_name,
                                            neighbor_fsls(i),
                                            'TOPOGEOM',
                                            'POLYGON') = 0
         THEN

            self_position := GZ_BUSINESS_UTILS.QUERY_DELIMITED_LIST(self.feature_tables,
                                                                    neighbor_fsls(i));

            IF self_position > 0
            THEN

               --geoids both exist for the fsls

               IF self.feature_table_geoids(self_position) <>  neighbor_geoids(i)
               THEN

                  --and the geoids dont match
                  --Add the self.face to the neighbor fsl - the neighbor takes over the self territory
                  GZ_SMPOLY.ADD_A_FACE(neighbor_face.topology_name,
                                       neighbor_fsls(i),
                                       neighbor_geoids(i),  --the actual geo_id
                                       self.face_id,
                                       'GEO_ID');

                  --subtract the self.face from the self FSL (may delete it).  The self abandons its territory
                  --return value in edited_record not used. 1 for deleted a face, 0 for feature extinction
                  edited_record := GZ_SMPOLY.DELETE_A_FACE(self.topology_name,
                                                           self.feature_tables(self_position),
                                                           self.feature_table_geoids(self_position),
                                                           self.face_id,
                                                           'GEO_ID',    --key on this layer
                                                           'GEO_ID');   --key on any parent too

               END IF;

            ELSE

               --neighbor fsl exists and self doesnt
               --Add the self.face to the neighbor fsl - the neighbor takes over the self territory
               --same call as above, seems clearerer to separate the logic
               GZ_SMPOLY.ADD_A_FACE(neighbor_face.topology_name,
                                    neighbor_fsls(i),
                                    neighbor_geoids(i),  --the actual geo_id
                                    self.face_id,
                                    'GEO_ID');

            END IF;

         END IF;

      END LOOP;


      FOR i IN 1 .. self.feature_tables.COUNT
      LOOP

         --  When self fsl exists and neighbor doesnt, subtract the self.face from the self FSL (may delete it)
         IF GZ_TOPO_UTIL.GET_TG_LAYER_LEVEL(self.topology_name,
                                            self.feature_tables(i),
                                            'TOPOGEOM',
                                            'POLYGON') = 0
         THEN

            neighbor_position := GZ_BUSINESS_UTILS.QUERY_DELIMITED_LIST(neighbor_fsls,
                                                                        self.feature_tables(i));

            IF neighbor_position = 0
            THEN

               --this feature table is not repped in the neighbor
               --subtract the self.face from the self FSL (may delete it).  The self abandons its territory
               --return value in edited_record not used. 1 for deleted a face, 0 for feature extinction
               edited_record := GZ_SMPOLY.DELETE_A_FACE(self.topology_name,
                                                        self.feature_tables(i),
                                                        self.feature_table_geoids(i),
                                                        self.face_id,
                                                        'GEO_ID',    --key on this layer
                                                        'GEO_ID');   --key on any parent too

            END IF;

         END IF;

      END LOOP;

      --now that the the standard feature layers are done, take care of the feature faces themselves
      --Add the small face_id to the neighbor face feature
      GZ_SMPOLY.ADD_A_FACE(neighbor_face.topology_name,
                           neighbor_face.table_name,
                           neighbor_face.face_id,
                           self.face_id,
                           'FACE_ID');


      --subtract the small face from the small feature face (will delete it usually)
      --   but sometimes during multi-step face collapses there's mitosis and the small face feature remains
      --   in increasingly smaller bits of territory
      --at this point our self object is a figment, we built it on the fly from pieces of the original
      --its face id doesnt correspond to anything in the feature face table,
      --and the primitive face already got subtracted from the remaining real feature face id

      BEGIN

         edited_record := GZ_SMPOLY.DELETE_A_FACE(self.topology_name,
                                                  self.table_name,
                                                  self.face_id,
                                                  self.face_id,
                                                  'FACE_ID',    --key on this layer
                                                  'GEO_ID');    --key on any parent too



      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLERRM LIKE '%Got a face kount of 0%'
         THEN

            --When collapsing to universal the first time through here
            --removed the small face from the feature face record
            --this is sloppy
            NULL;

         ELSE

            RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

         END IF;

      END;

   END match_features;


    ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE merge_face(
      p_neighbor_face      IN NUMBER
   )
   AS

      --M@! 7/25/13
      --! 8/23/13 added handling for faces that touch more than once
      --Resolves any fsl and face feature differences
      --Removes edge between self and neighbor
      --updates face feature table with surviving primitive face id
      --Self feature face becomes a figment at this point.  Wasnt it always though

      psql              VARCHAR2(4000);
      edge_id           NUMBER;
      edge_idz          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      survivor_face     NUMBER;
      seppuku           PLS_INTEGER := 0;

   BEGIN

      IF self.coastal_edges.COUNT > 1
      THEN

         --need to deal with face switcheroo in a more robust manner if this is an interior merge
         RAISE_APPLICATION_ERROR(-20001, 'Sorry kiddo, this jawn is only written for faces with one edge on the universal face at the moment');

      END IF;

      --The big feature table work
      self.match_features(p_neighbor_face);

      --get the edge between the 2 faces
      psql := 'SELECT e.edge_id FROM ' || self.topology_name || '_edge$ e '
           || 'WHERE (e.left_face_id = :p1 AND e.right_face_id = :p2) '
           || 'OR (e.right_face_id = :p3 AND e.left_face_id = :p4) ';

      BEGIN
      
         EXECUTE IMMEDIATE psql INTO edge_id USING p_neighbor_face, self.face_id,
                                                   p_neighbor_face, self.face_id;
                                                   
         edge_idz.EXTEND;
         edge_idz(1) := edge_id;
                                                   
      EXCEPTION
      WHEN OTHERS
      THEN
      
         IF SQLCODE = -1422  
         THEN
         
            seppuku := 1;
            
            --Pretty darn rare
            --ORA-01422: exact fetch returns more than requested number of rows
            --These two faces touch at more than one place
            --Safest option is to remove all edges between them but stop the rest of the sliver processing
            
            EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_idz USING p_neighbor_face, self.face_id,
                                                                    p_neighbor_face, self.face_id;
         
         ELSE
         
            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);
         
         END IF;
      
      
      END;
      
      FOR i IN 1 .. edge_idz.COUNT
      LOOP
      
         IF self.dbug = 1
         THEN
            dbms_output.put_line('removing edge ' || edge_idz(i));
         END IF;


         GZ_SMPOLY.GZ_REMOVE_EDGE(self.topology_name,
                                  edge_idz(i));
                                  
      END LOOP;

      --Need to be careful here
      --The feature face table is potentially junk if we are rolling through
      --a series of edges to collapse to the UF
      --1nd, the self.face_id may remain even after match_features
      --  because it still hangs on to territory after mitosis
      --2rd, the neighbor face may no longer have a face_id matching its primitive face

      --The only constant is the UF edge of the self, primitive-style
      --Get whatever is to its side and set the big surviving face to match

      psql := 'SELECT e.left_face_id FROM ' || self.topology_name || '_edge$ e '
           || 'WHERE e.edge_id = :p1 AND e.left_face_id <> :p2 '
           || 'UNION ALL '
           || 'SELECT e.right_face_id FROM ' || self.topology_name || '_edge$ e '
           || 'WHERE e.edge_id = :p3 AND e.right_face_id <> :p4 ';

      EXECUTE IMMEDIATE psql INTO survivor_face USING self.coastal_edges(1), -1,
                                                      self.coastal_edges(1), -1;

      IF self.dbug = 1
      THEN
         dbms_output.put_line('survivor face is ' || survivor_face || '. Will update face ' || p_neighbor_face);
      END IF;

      --This is just the value for face_id
      --All other face feature values are correct, the big face feature is the survivor,
      --   only the corresponding primitive face ids may change

      IF survivor_face <> p_neighbor_face
      THEN

         psql := 'UPDATE ' || self.table_name || ' a '
              || 'SET a.face_id = :p1 '
              || 'WHERE a.face_id = :p2 ';

         EXECUTE IMMEDIATE psql USING survivor_face,
                                      p_neighbor_face;

         COMMIT;

      END IF;
      
      IF seppuku = 1
      THEN
      
         RAISE_APPLICATION_ERROR(-20001,'SEPPUKU');
      
      END IF;

   END merge_face;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION valid(
      p_tolerance       IN NUMBER
   ) RETURN VARCHAR2
   AS
      --not using this but it seems like a good idea

   BEGIN

      IF self.sdogeometry IS NOT NULL
      AND sdo_geom.validate_geometry_with_context(self.sdogeometry, p_tolerance) = 'TRUE'
      AND self.sdogeometry.sdo_gtype = 2003
      THEN

         RETURN 'TRUE';

      ELSE

         RETURN 'FALSE';

      END IF;

   END valid;


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_boundary_edges
   AS

      bdy_edges         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

   BEGIN

      self.signed_boundary_edges := SDO_TOPO.GET_FACE_BOUNDARY(self.topology_name,
                                                               self.face_id,
                                                               'FALSE');  --default, in case of internal dangles, shouldnt be there

      bdy_edges.EXTEND(self.signed_boundary_edges.COUNT);

      FOR i IN 1 .. self.signed_boundary_edges.COUNT
      LOOP

         bdy_edges(i) := ABS(self.signed_boundary_edges(i));

      END LOOP;

      self.boundary_edges := bdy_edges;

   END set_boundary_edges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_boundary_edges
   RETURN MDSYS.SDO_LIST_TYPE
   AS

   BEGIN

      IF self.boundary_edges IS NOT NULL
      AND self.boundary_edges.COUNT > 1
      THEN

         RETURN self.boundary_edges;

      ELSE

         --why?
         RETURN NULL;

      END IF;

   END get_boundary_edges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_boundary_faces
   AS

      bdy_faces         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      psql              VARCHAR2(4000);

   BEGIN


      psql := 'SELECT face_id FROM ('
           || 'SELECT e.left_face_id face_id FROM ' || self.topology_name || '_edge$ e '
           || 'WHERE e.edge_id IN (SELECT * FROM TABLE(:p1)) '
           || 'UNION '
           || 'SELECT e.right_face_id face_id FROM ' || self.topology_name || '_edge$ e '
           || 'WHERE e.edge_id IN (SELECT * FROM TABLE(:p2)) '
           || ') WHERE face_id NOT IN (:p3,:p4) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO bdy_faces USING self.boundary_edges,
                                                               self.boundary_edges,
                                                               self.face_id,
                                                               -1;

      self.boundary_faces := bdy_faces;

   END set_boundary_faces;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_boundary_faces
   RETURN MDSYS.SDO_LIST_TYPE
   AS

   BEGIN

      IF self.boundary_edges IS NOT NULL
      AND self.boundary_edges.COUNT > 1
      THEN

         RETURN self.boundary_faces;

      ELSE

         --why?
         RETURN NULL;

      END IF;

   END get_boundary_faces;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_signed_boundary_edges
   RETURN MDSYS.SDO_LIST_TYPE
   AS

   BEGIN

      IF self.signed_boundary_edges IS NOT NULL
      AND self.signed_boundary_edges.COUNT > 1
      THEN

         RETURN self.signed_boundary_edges;

      ELSE

         --why?
         RETURN NULL;

      END IF;

   END get_signed_boundary_edges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_coastal_sliver(
      p_sliver_width       IN NUMBER,
      p_partial_length     IN NUMBER
   )
   AS

      --Matt! 7/16/13

      --Steps
      --1. Check if face has only 1 edge on the universal face
      --      If not, exit, this is not a coastal sliver
      --2. Start counterclockwise, measure the face distance to the universal
      --3. Decide what to do with the counterclockwise direction
      --      If the measurer returns -1 then this is a sliver.  Set SLIVER
      --         set coastal_sliver_dir to NA
      --      If the measurer returns zed or a positive number, compare to p_partial length
      --         If the returned value is greater then p_partial_length, set PARTIAL
      --         set coastal_sliver_dir to COUNTERCLOCKWISE
      --4. If neither sliver or partial were determined, walk in a clockwise direction
      --      Should not get a sliver in this direction unless something is wack
      --      If the measurer returns zed or a positive number compare to p_partial length
      --         If the returned value (clockwise) is greater than p_partial_length, set PARTIAL
      --            set coastal_sliver_dir to CLOCKWISE
      --      If measurer is less than p_partial_length set to NO
      --            set coastal_sliver_dir to NA

      measurement          NUMBER;


   BEGIN



      IF self.coastal_edges.COUNT <> 1
      THEN

         --if 0 edges shouldnt be here.  If 2 edges on coast no code to deal (yet)
         self.coastal_sliver := 'NO';
         RETURN;

      ELSIF self.coastal_edges.COUNT = 1
      AND self.boundary_edges.COUNT = 1
      THEN

         --cant do anything for islands with no interior edges
         self.coastal_sliver := 'NO';
         RETURN;

      END IF;

      --returns the distance, or -1 if traverse the interior edges
      measurement := self.measure_face_to_universal('COUNTERCLOCKWISE',p_sliver_width);

      IF measurement < 0
      THEN

         --Usually this is a sliver, but need to check the other way, in case the 
         --vertices on the interior edges are sparse
         
         measurement := self.measure_universal_to_face('COUNTERCLOCKWISE',p_sliver_width);
         
         IF measurement < 0
         THEN
         
            self.coastal_sliver     := 'SLIVER';

            --done
            RETURN;
            
         ELSE
         
            --Just gonna measure a false sliver from the other direction
            --get out
            self.coastal_sliver     := 'NO';
            RETURN;
            
         END IF;

      ELSE

         IF measurement > p_partial_length
         THEN

            self.coastal_sliver           := 'PARTIAL';
            self.coastal_sliver_c_clock_d := measurement;

         END IF;

      END IF;

      measurement := self.measure_face_to_universal('CLOCKWISE',p_sliver_width);

      IF measurement < 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Howd we get a sliver walk clockwise but not counterclockwise?');

      ELSE


         IF measurement > p_partial_length
         THEN

            --may rewrite the above if both ways
            self.coastal_sliver         := 'PARTIAL';
            self.coastal_sliver_clock_d := measurement;

         ELSE

            IF self.coastal_sliver <> 'PARTIAL'
            OR self.coastal_sliver IS NULL
            THEN

               --only set to No if not already a partial sliver in counterclockwise dir
               self.coastal_sliver     := 'NO';

            END IF;

         END IF;

      END IF;


   END set_coastal_sliver;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_coastal_sliver
   RETURN VARCHAR2
   AS

   BEGIN

      IF self.coastal_sliver IS NOT NULL
      THEN

         RETURN self.coastal_sliver;

      ELSE

         --why?
         RETURN NULL;

      END IF;

   END get_coastal_sliver;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE set_coastal_edges
   AS

      psql        VARCHAR2(4000);

   BEGIN

      IF self.boundary_edges IS NULL
      OR self.boundary_edges.COUNT = 0
      THEN

         self.set_boundary_edges();

      END IF;

      psql := 'SELECT e.edge_id '
           || 'FROM ' || self.topology_name || '_edge$ e '
           || 'WHERE '
           || '(e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
           || 'e.edge_id IN (SELECT * FROM TABLE(:p3)) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO self.coastal_edges USING -1, -1,
                                                                        self.boundary_edges;

      IF self.coastal_edges.COUNT > 0
      THEN

         self.coastal := 'Y';

      ELSE

         self.coastal := 'N';

      END IF;

   END set_coastal_edges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_coastal_edges
   RETURN MDSYS.SDO_LIST_TYPE
   AS

   BEGIN

      IF self.coastal_edges IS NOT NULL
      AND self.coastal_edges.COUNT > 1
      THEN

         RETURN self.coastal_edges;

      ELSE

         --why?
         RETURN NULL;

      END IF;

   END get_coastal_edges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_signed_interior_edges(
      p_direction          IN VARCHAR2        --CLOCKWISE or COUNTERCLOCKWISE
   )RETURN MDSYS.SDO_LIST_TYPE
   AS

      --Matt! 7/15/13
      --Return ordered and signed array of interior edges starting at the universal edge
      --   either from clockwise side or counterclockwise
      --Require specific definition of a coastal face as one that has a single edge on the universal

      ordered_signed_edges             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      reversed_signed_edges            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      found_coastal_pos                PLS_INTEGER;
      kount                            PLS_INTEGER := 0;

   BEGIN

      IF p_direction NOT IN ('CLOCKWISE','COUNTERCLOCKWISE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Invalid p_direction ' || p_direction);

      END IF;

      IF self.coastal_edges.COUNT <> 1
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Only can get an interior ring for faces touching universal on one edge ');

      END IF;

      ordered_signed_edges.EXTEND(self.signed_boundary_edges.COUNT - 1);

      --perform counterclockwise walk, matches the signed_boundary_edges
      FOR i IN 1 .. self.signed_boundary_edges.COUNT
      LOOP

         --find the coastal edge
         IF ABS(self.signed_boundary_edges(i)) = self.coastal_edges(1)
         THEN

            found_coastal_pos := i;
            EXIT;

         END IF;

      END LOOP;


      IF found_coastal_pos <> self.signed_boundary_edges.COUNT
      THEN

         --walk inward from coastal .. end of the array
         FOR i IN (found_coastal_pos + 1) .. self.signed_boundary_edges.COUNT
         LOOP

            kount := kount + 1;
            ordered_signed_edges(kount) := self.signed_boundary_edges(i);

         END LOOP;

      END IF;

      IF found_coastal_pos <> 1  --if it was first we are done in the first loop
      THEN

         --walk from the beginning of the array back to the coast
         FOR i IN 1 .. (found_coastal_pos - 1)
         LOOP

            kount := kount + 1;
            ordered_signed_edges(kount) := self.signed_boundary_edges(i);

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

   END get_signed_interior_edges;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_interior_edges(
      p_direction          IN VARCHAR2        --CLOCKWISE or COUNTERCLOCKWISE
   )RETURN MDSYS.SDO_LIST_TYPE
   AS

      --Matt! 7/15/13
      --Return ordered but unsigned array of interior edges starting at the universal edge
      --   either from clockwise side or counterclockwise
      --Require specific definition of a coastal face as one that has a single edge on the universal

      ordered_signed_edges             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      ordered_edges                    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();

   BEGIN

      IF p_direction NOT IN ('CLOCKWISE','COUNTERCLOCKWISE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Invalid p_direction ' || p_direction);

      END IF;

      IF self.coastal_edges.COUNT <> 1
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Only can get an interior ring for faces touching universal on one edge ');

      END IF;
      
      ordered_signed_edges := self.get_signed_interior_edges(p_direction);
      ordered_edges.EXTEND(ordered_signed_edges.COUNT);

      --perform counterclockwise walk, matches the signed_boundary_edges
      FOR i IN 1 .. ordered_signed_edges.COUNT
      LOOP

         ordered_edges(i) := ABS(ordered_signed_edges(i));

      END LOOP;

      RETURN ordered_edges;     

   END get_interior_edges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_sdogeometry
   RETURN sdo_geometry
   AS

   BEGIN

      RETURN self.sdogeometry;

   END get_sdogeometry;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE update_measurements
   AS

   BEGIN

      GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(self.table_name,
                                                 'FACE_ID',
                                                 'SDOGEOMETRY',
                                                 'ALL',
                                                 self.tolerance,
                                                 p_subset_col => 'FACE_ID',
                                                 p_subset_val => self.face_id);

   END update_measurements;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_feature_layers
   RETURN MDSYS.SDO_LIST_TYPE
   AS

      --M@! 7/23/13

      output         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      psql           VARCHAR2(4000);

   BEGIN

      psql := 'SELECT a.table_name '
           || 'FROM user_sdo_topo_info a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.tg_layer_type = :p2 AND '
           || 'a.table_name <> :p3 '
           ||  'ORDER BY a.tg_layer_level DESC ';  --constancy.  Sweet constancy

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING self.topology_name,
                                                            'POLYGON',
                                                            self.table_name;

      RETURN output;

   END get_feature_layers;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE collapse_to_universal
   AS

      --M@! 7/23/13
      --Collapse a feature face - targets coastal slivers, but probably works for any single-edge-on-coast face
      --Caller should know about endangerment checks and whatnot, and be wise
      --   before calling collapse_to_universal
      --   see GZ_SMPOLY.FSLS_ENDANGERED
      --Caller should also record the fsls and geo_ids that border the sliver
      --   as well as the fsls and geoids that are the sliver
      --   and recalculate all potentially impacted measurements after calling face expunge
      --   see GZ_SMPOLY.GET_FSLS_FOR_FACE
      --Details, for each sliver "opposing edge":
      --Geometry work: (If this is the final (or only) edge on the Counterclockwise walk, skip this step)
      --Drop the "next" node to the UF by creating a new node on the UF and a new edge connecting
      --   Record the interior node id for later
      --   Record the node_id on the UF for later
      --   Figure out if the face_id reversed inside the sliver
      --   Figure out if the UF edge for the current sliver is the old id or new
      --Match the FSLs on either side of the opposing edge
      --Record the node id at the start and end of the opposing edge
      --Delete the opposing edge
      --   Update face feature table with surviving face id if necessary
      --When done processing all edges
      --   Zap node attempt to remove the node and vtx, if possible, at each dropped interior node
      --   Simple obsolete node removal attempt for all nodes on the UF and start/end interiors
      --   Update meas           urements for all face features that appear to have been touched

      --Some day could be modified to collapse_to_*edge* and replace the UF edge with any interior edge


      psql                          VARCHAR2(4000);
      opposing_directed_edges       MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      all_feature_faces             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      all_feature_faces_live        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      zap_nodes                     MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      zap_nodes_bulk                MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      kindly_nodes                  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      kindly_nodes_bulk             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      node_pos                      VARCHAR2(5);
      sliver_face_side              VARCHAR2(5);
      big_face_side                 VARCHAR2(5);
      drop_node                     NUMBER;
      drop_node_geom                SDO_GEOMETRY;
      universal_geom                SDO_GEOMETRY;
      universal_edge_id             NUMBER;
      new_node_id                   NUMBER;
      new_node_geom                 SDO_GEOMETRY;
      new_edge_id                   NUMBER;
      coord_index                   NUMBER;
      node_star                     MDSYS.SDO_NUMBER_ARRAY := MDSYS.SDO_NUMBER_ARRAY();
      extending_edge                NUMBER;
      sliver_face_now               NUMBER;
      big_face_id                   NUMBER;
      sliver_face_next              NUMBER;
      uf_edge_now                   NUMBER;
      uf_edge_next                  NUMBER;
      node_kount                    NUMBER;
      working_face                  GZ_FEATURE_FACE := gz_feature_face();
      edited_record                 NUMBER;
      bail_out                      PLS_INTEGER := 0;
      bail_msg                      VARCHAR2(4000);

   BEGIN

      IF self.coastal_edges.COUNT <> 1
      THEN

         --I think this should work for any single-edge coastal face. Doesnt have to be a sliver
         RAISE_APPLICATION_ERROR(-20001, 'Sorry peaches, I only know how to collapse coastal faces with 1 edge on the coast');

      END IF;

      --Get the edges to traverse. Counterclockwise so direction matches the signs in the array, no real reason though
      opposing_directed_edges := self.get_sliver_opposing_edges('COUNTERCLOCKWISE');

      --the small face always reps the same set of FSLS
      --get it once and then pass on to twins during mitosis
      self.set_feature_layers;

      --original_face_id := self.face_id;

      --about to go into la la land on our original face feature
      --clean it out of the face feature table before it starts mitosis
      edited_record := GZ_SMPOLY.DELETE_A_FACE(self.topology_name,
                                               self.table_name,
                                               self.face_id,    --the key on the face feature table
                                               self.face_id,
                                               'FACE_ID',       --key on this layer
                                               'GEO_ID');       --key on any parent too

      FOR i IN 1 .. opposing_directed_edges.COUNT
      LOOP

         IF self.dbug = 1
         THEN
            dbms_output.put_line('on opposing edge ' || opposing_directed_edges(i));
         END IF;

         --establish some factoids
         IF opposing_directed_edges(i) > 0
         THEN

            --going counterclockwise, so edge dir matches
            node_pos         := 'end';
            sliver_face_side := 'left';
            big_face_side    := 'right';

         ELSE

            --edge is reversed relative to counterclockwise walk
            node_pos         := 'start';
            sliver_face_side := 'right';
            big_face_side    := 'left';

         END IF;

         IF i = 1
         THEN

            --Things that have to happen at the start go here
            --Theres also an ELSE down below for end work

            --This is True for singles and the first in a series
            --In geom processing code this only executes in i>1
            universal_edge_id := self.coastal_edges(1);

            --Start the (or create the only) running distinct list of all uf nodes on either side of the uf edges
            psql := 'SELECT e.start_node_id FROM ' || self.topology_name || '_edge$ e '
                 || 'WHERE e.edge_id = :p1 '
                 || 'UNION '
                 || 'SELECT e.end_node_id FROM ' || self.topology_name || '_edge$ e '
                 || 'WHERE e.edge_id = :p2 ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO kindly_nodes USING universal_edge_id,
                                                                        universal_edge_id;

         END IF;

         IF i = 1
         OR i = opposing_directed_edges.COUNT
         THEN

            --This is a bit sloppy, but some first and last interior nodes dont get dropped and recorded in geometry work
            --but need to be made not obsolete
            --Just gather them all, deduplicating with union and let the kindly call determine what can be done
            --some of these also got into zap nodes (correctly) and will be NA by the time kindly runs

            psql := 'SELECT e.start_node_id FROM ' || self.topology_name || '_edge$ e '
                 || 'WHERE e.edge_id = :p1 '
                 || 'UNION '
                 || 'SELECT e.end_node_id FROM ' || self.topology_name || '_edge$ e '
                 || 'WHERE e.edge_id = :p2 '
                 || 'UNION '
                 || 'SELECT * FROM TABLE(:p3) ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO kindly_nodes_bulk USING ABS(opposing_directed_edges(i)),
                                                                             ABS(opposing_directed_edges(i)),
                                                                             kindly_nodes;

            --I'm clearly doing this wrong.  Cant get the above to work without 2 arrays
            kindly_nodes.DELETE;
            kindly_nodes := kindly_nodes_bulk;
            kindly_nodes_bulk.DELETE;

         END IF;

         IF i <> opposing_directed_edges.COUNT --skip geom work for final opposing edge and also singles
         THEN

             --Drop the "next" node to the UF by creating a new node on the UF and a new edge connecting

             psql := 'SELECT n.node_id, n.geometry FROM '
                  || self.topology_name || '_node$ n, '
                  || self.topology_name || '_edge$ e '
                  || 'WHERE e.edge_id = :p1 AND '
                  || 'e.' || node_pos || '_node_id = n.node_id ';

            EXECUTE IMMEDIATE psql INTO drop_node,
                                        drop_node_geom USING ABS(opposing_directed_edges(i));

            IF self.dbug = 1
            THEN
               dbms_output.put_line('drop node: ' || drop_node || ' from ' || psql);
            END IF;

            --Record the interior node id for later - it should always be zapped
            zap_nodes.EXTEND(1);
            zap_nodes(zap_nodes.COUNT) := drop_node;

            --null the drop node srid
            drop_node_geom.sdo_srid := NULL;

            IF i > 1
            THEN

               --increment to next universal on loops. May be the same ID, but diff geom as we process
               universal_edge_id := uf_edge_next;

               --Maintain the running distinct list of all uf nodes on either side of the uf edges
               psql := 'SELECT e.start_node_id FROM ' || self.topology_name || '_edge$ e '
                    || 'WHERE e.edge_id = :p1 '
                    || 'UNION '
                    || 'SELECT e.end_node_id FROM ' || self.topology_name || '_edge$ e '
                    || 'WHERE e.edge_id = :p2 '
                    || 'UNION '
                    || 'SELECT * FROM TABLE(:p3) ';

               EXECUTE IMMEDIATE psql BULK COLLECT INTO kindly_nodes_bulk USING universal_edge_id,
                                                                                universal_edge_id,
                                                                                kindly_nodes;
               --doing it wrong
               kindly_nodes.DELETE;
               kindly_nodes := kindly_nodes_bulk;
               kindly_nodes_bulk.DELETE;

            END IF;

            IF self.dbug = 1
            THEN
               dbms_output.put_line('projecting to universal edge ' || universal_edge_id);
            END IF;

            --get the uf geom and null the srid
            universal_geom := self.get_edge_geometry(universal_edge_id);
            universal_geom.sdo_srid := NULL;

            --could be some tests here before going for it

            new_node_geom := GZ_CLIP.GZ_PROJECT_PT(drop_node_geom,
                                                   universal_geom,
                                                   self.null_tolerance);

            coord_index := GZ_CLIP.ACCURATE_FIND_COORD_INDEX(new_node_geom,
                                                             universal_geom);

            --done with the LRS stuff, back to real srids
            new_node_geom.sdo_srid := self.srid;

            --Are we close to an existing shape point?  Gonna risk it
            --Should not be near a node.  If we are then the get_opposing_edges LRS code needs to be revised
            --May temporarily be very close to an existing node, but its going away soon

            --dont need to record the new node id, it has a long and healthy life ahead of it
            IF self.dbug = 1
            THEN
               dbms_output.put_line('adding a new node at ');
               dbms_output.put_line(gz_geom_utils.dump_sdo(new_node_geom));
            END IF;

            BEGIN

               new_node_id := GZ_SMPOLY.ADD_NEW_NODE(self.topology_name,
                                                     universal_edge_id,
                                                     coord_index,
                                                     new_node_geom,
                                                     'TRUE');

            EXCEPTION
            WHEN OTHERS
            THEN

               --Look at this idiocy, try again with existing shape point flag, dont even test
               IF LOWER(SQLERRM) LIKE '%add node results in self-intersection of line string%'
               OR LOWER(SQLERRM) LIKE '%add node results in two pieces of split edge intersecting each other%'
               OR LOWER(SQLERRM) LIKE '%results in an intersection with another edge%'
               THEN

                  IF self.dbug = 1
                  THEN
                     dbms_output.put_line('error adding node, trying again with new shapepoint flag set to FALSE');
                  END IF;

                  BEGIN
                  
                     new_node_id := GZ_SMPOLY.ADD_NEW_NODE(self.topology_name,
                                                           universal_edge_id,
                                                           coord_index + 1,
                                                           new_node_geom,
                                                           'FALSE');
                                                           
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

                        --The feature face for the sliver is gone, need to merge something
                        --Best option is to simply merge whats left of the sliver with the
                        --current opposing edge.  Will most likely turn it into a partial sliver
                        bail_out := 1;
                        bail_msg := SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace || ' on '
                                            || 'GZ_SMPOLY.ADD_NEW_NODE(''' ||  self.topology_name || ''',' || universal_edge_id || ','
                                            || coord_index || ',' || TO_CHAR(gz_geom_utils.dump_sdo(new_node_geom)) || ','
                                            || '''FALSE'')';
                     
                     ELSE
                     
                        RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace || ' on '
                                             || 'GZ_SMPOLY.ADD_NEW_NODE(''' ||  self.topology_name || ''',' || universal_edge_id || ','
                                             || coord_index || ',' || TO_CHAR(gz_geom_utils.dump_sdo(new_node_geom)) || ','
                                             || '''FALSE'')');
                     
                     END IF;
                  
                  END;

               ELSE
            
                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace || ' on '
                                             || 'GZ_SMPOLY.ADD_NEW_NODE(''' ||  self.topology_name || ''',' || universal_edge_id || ','
                                             || coord_index || ',' || TO_CHAR(gz_geom_utils.dump_sdo(new_node_geom)) || ','
                                             || '''TRUE'')');

               END IF;

            END;

            --get the edge that bounds the manifest destiny face and is gonna extend
            node_star := SDO_TOPO_MAP.GET_NODE_STAR(self.topology_name,
                                                    NULL,
                                                    drop_node);

            FOR jj IN 1 .. node_star.COUNT
            LOOP

               IF ABS(node_star(jj)) = ABS(opposing_directed_edges(i))
               THEN

                  --node star is clockwise, so the extend edge is the one before our working edge
                  IF jj = 1
                  THEN

                     extending_edge := ABS(node_star(node_star.COUNT));

                  ELSE

                     extending_edge := ABS(node_star(jj-1));

                  END IF;

                  EXIT;

               END IF;

            END LOOP;

            --new edge connecting. From here the self.feature_face is no longer accurate, split into two primitives
            
            IF bail_out = 0
            THEN
            
               BEGIN

                  new_edge_id := GZ_SMPOLY.ADD_NEW_EDGE(self.topology_name,
                                                        drop_node,
                                                        new_node_id,
                                                        extending_edge);

               EXCEPTION
               WHEN OTHERS
               THEN

                  IF UPPER(SQLERRM) LIKE '%JAVA%'
                  OR UPPER(SQLERRM) LIKE '%GOT EXTRA CONNECTIONS%' --my check in add_new_edge
                  OR UPPER(SQLERRM) LIKE '%EXACT FETCH RETURNS MORE THAN REQUESTED NUMBER OF ROWS%' --saw this on ACS13 V839 did not investigate
                  THEN

                     --most likely
                     --ORA-29532: Java call terminated by uncaught Java exception:
                     --oracle.spatial.topo.InvalidTopoOperationException: add edge coordi...

                     --This is the dangerous spot, if adding an edge that somehow bumps existing edges
                     --The sliver face is already gone from the face feature table
                     --Best option is to simply merge whats left of the sliver with the
                     --current opposing edge.  Will most likely turn it into a partial sliver
                     bail_out := 1;
                     bail_msg := SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace;

                     --remove the obsolete node on the UF. Ordinarily it isnt tracked since its a 3 way on successes
                     GZ_TOPOFIX.ZAP_NODE(self.topology_name,
                                         new_node_id);

                  ELSE

                     RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

                  END IF;

               END;

            END IF;
            
            --At this point Ima call the new section of sliver the SELF sliver
            --The old section that we just walled off will get a working_face temporary object
            --and go on to merge_face

            --find the face id that borders our opposing edge now
            --also get the manifest destiny face while at it
            psql := 'SELECT e.' || sliver_face_side || '_face_id, e.' || big_face_side || '_face_id '
                 || 'FROM ' || self.topology_name || '_edge$ e '
                 || 'WHERE e.edge_id = :p1 ';

            EXECUTE IMMEDIATE psql INTO sliver_face_now,
                                        big_face_id USING ABS(opposing_directed_edges(i));

            IF bail_out = 0
            THEN

               IF sliver_face_now <> self.face_id
               THEN

                  --starting face pushed on to next
                  sliver_face_next := self.face_id;

               ELSE

                  --find out the next
                  psql := 'SELECT e.left_face_id FROM ' || self.topology_name || '_edge$ e '
                       || 'WHERE e.edge_id = :p1 AND e.left_face_id <> :p2 '
                       || 'UNION ALL '
                       || 'SELECT e.right_face_id FROM ' || self.topology_name || '_edge$ e '
                       || 'WHERE e.edge_id = :p3 AND e.right_face_id <> :p4 ';

                  EXECUTE IMMEDIATE psql INTO sliver_face_next USING new_edge_id, self.face_id,
                                                                     new_edge_id, self.face_id;

               END IF;

               --Set the self object face id
               IF self.dbug = 1
               THEN
                  dbms_output.put_line('setting self face id to ' || sliver_face_next);
               END IF;

               self.set_face_id(sliver_face_next);

               --   Figure out if the UF edge for the now sliver is the old id or new

               psql := 'SELECT e.edge_id FROM ' || self.topology_name || '_edge$ e '
                    || 'WHERE (e.left_face_id = :p1 AND e.right_face_id = :p2) '
                    || 'OR (e.right_face_id = :p3 AND e.left_face_id = :p4) ';

               EXECUTE IMMEDIATE psql INTO uf_edge_now USING sliver_face_now, -1,
                                                             sliver_face_now, -1;

               IF self.dbug = 1
               THEN
                  dbms_output.put_line('uf edge now is ' || uf_edge_now || ' based on sliver face now ' || sliver_face_now);
               END IF;

               IF uf_edge_now = universal_edge_id
               THEN

                  --figure it out using next face id
                  EXECUTE IMMEDIATE psql INTO uf_edge_next USING self.face_id, -1,
                                                                 self.face_id, -1;

                  IF self.dbug = 1
                  THEN
                     dbms_output.put_line('1 set uf edge next ' || uf_edge_next );
                  END IF;

               ELSE

                  --pushed on to the next, no need for another sql
                  uf_edge_next := universal_edge_id;

                  IF self.dbug = 1
                  THEN
                     dbms_output.put_line('2 set uf edge next ' || uf_edge_next );
                  END IF;

               END IF;

               IF self.dbug = 1
               THEN
                  dbms_output.put_line('after geom processing i ' || i);
                  dbms_output.put_line('uf_edge_now: ' || uf_edge_now || ' uf_edge_next: ' || uf_edge_next);
               END IF;

            ELSE

               --bail out is 1, set just what we need to merge the current face with the
               --sliver that hasnt been split by new edges
               --should have sliver_face_now and big_face_id
               --anything else?  Yeah, don't attempt to zap the interior extension node on the failure spot
               --Removing the edge on one side of it makes it go from a T to an obsolete
               --But that shape point needs to stay
               zap_nodes.TRIM(1);

            END IF;

         ELSE       --i = opposing_directed_edges.COUNT

            --this is either the only edge and theres no geom work
            --or this is the final edge in a series of chomps (again, no geom work)
            --If final, previous roll through the loop took note of the edge and face for this time

            sliver_face_now := self.face_id;

            --get big face
            psql := 'SELECT e.' || big_face_side || '_face_id '
                 || 'FROM ' || self.topology_name || '_edge$ e '
                 || 'WHERE e.edge_id = :p1 ';

            EXECUTE IMMEDIATE psql INTO big_face_id USING ABS(opposing_directed_edges(i));

            IF opposing_directed_edges.COUNT > 1
            THEN

               --final in a series
               --sliver_face_now := sliver_face_next;  --taken care with self object face update
               uf_edge_now     := uf_edge_next;

            ELSE

               --anything unique for finals?
               NULL;

            END IF;

         END IF; -- end skip geom work for single edge slivers and final edge in a list of them

         --------------------------------------------
         ---THE BIG FSL WORK AND FACE MERGE IS HERE--
         --------------------------------------------

         IF self.dbug = 1
         THEN
            dbms_output.put_line('instantiating ' || sliver_face_now);
         END IF;

         --make a new object with the current configuration
         --could be the same face id
         working_face := gz_feature_face(self.topology_name,
                                         self.table_name,
                                         sliver_face_now,
                                         self.tolerance);

         --pass the FSL info on to the working face, whatever it is
         working_face.set_feature_layers(self.feature_tables,
                                         self.feature_table_geoids);


         IF self.dbug = 1
         THEN
            dbms_output.put_line('description of working face ');
            working_face.describe_face_object;
            dbms_output.put_line('big face is ' || big_face_id);
         END IF;

         --Match the FSLs on either side of the opposing edge and merge
         --this method also corrects any face switcheroo on the feature face table
         
         BEGIN
         
            working_face.merge_face(big_face_id);
            
         EXCEPTION
         WHEN OTHERS
         THEN
         
            --only known error here is a small face that borders a big face more than once.  Merge them and bail
            IF self.dbug = 1
            THEN
               dbms_output.put_line(SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace || ' on merge_face');
            END IF;
            
            bail_out := 1;
         
         END;

         --tuck all feature faces that have been edited. Not all will survive
         all_feature_faces.EXTEND(2);
         all_feature_faces(all_feature_faces.COUNT - 1) := working_face.face_id;
         all_feature_faces(all_feature_faces.COUNT) := big_face_id;

         IF bail_out = 1
         THEN

            --Failed on the edge add or node add, stop the loop over opposing edges
            --note the next big face since it was also possibly edited a little in the aborted work
            IF opposing_directed_edges(i+1) > 0
            THEN

               big_face_side    := 'right';

            ELSE

               big_face_side    := 'left';

            END IF;

            --get next big face
            psql := 'SELECT e.' || big_face_side || '_face_id '
                 || 'FROM ' || self.topology_name || '_edge$ e '
                 || 'WHERE e.edge_id = :p1 ';

            EXECUTE IMMEDIATE psql INTO big_face_id USING ABS(opposing_directed_edges(i+1));

            all_feature_faces.EXTEND(1); --just the one big, theres no more sliver chomping
            all_feature_faces(all_feature_faces.COUNT) := big_face_id;

            EXIT;

         END IF;

      END LOOP;


      --Zap node attempt to remove the node and vtx, if possible at each interior node
      --Simple obsolete node removal attempt for all nodes on the UF

      FOR i IN 1 .. zap_nodes.COUNT
      LOOP

         --Node stuff above is sloppy as sh$t
         --ensure that this node is actually interior, otherwise removes shape points on UF
         --I dont think this is necessary but just in case

         IF NOT GZ_TOPOFIX.IS_NODE_UNIVERSAL(self.topology_name,
                                             zap_nodes(i))
         THEN

            IF self.dbug = 1
            THEN
               dbms_output.put_line('zap node is ' || zap_nodes(i));
            END IF;

            BEGIN

               GZ_TOPOFIX.ZAP_NODE(self.topology_name,
                                   zap_nodes(i));

            EXCEPTION
            WHEN OTHERS
            THEN

               IF SQLERRM LIKE '%Zap node expects node to be bound by 2 edges%'
               THEN

                  --Not obsolete
                  NULL;

               ELSE

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

               END IF;

            END;

         ELSE

            IF self.dbug = 1
            THEN
               dbms_output.put_line('Not zapping universal ' || zap_nodes(i));
            END IF;

         END IF;

      END LOOP;

      FOR i IN 1 ..kindly_nodes.COUNT
      LOOP

         --does nothing at all if node doesnt exist, or is not obsolete
         --otherwise opens tiny topomap and remvoes the obsolete node, leaving vertex behind
         --return 0 or 1, not using at the moment

         --may have put some interior nodes in here
         --Never hurts to kindly remove an obsolete node, will fail silently
         --if not obsolete

         IF self.dbug = 1
         THEN
            dbms_output.put_line('kindly node is ' || kindly_nodes(i));
         END IF;

         node_kount := GZ_SMPOLY.REMOVE_NODE_KINDLY(self.topology_name,
                                                    kindly_nodes(i));


      END LOOP;

      --Update measurements
      --determine which faces on each side of the opposing edges remain

      psql := 'SELECT a.face_id FROM ' || self.table_name || ' a '
           || 'WHERE a.face_id IN (SELECT * FROM TABLE(:p1)) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO all_feature_faces_live USING all_feature_faces;

      FOR i IN 1 .. all_feature_faces_live.COUNT
      LOOP

         IF self.dbug = 1
         THEN
            dbms_output.put_line('Updating measurements for feature face ' || all_feature_faces_live(i));
         END IF;

         working_face := gz_feature_face(self.topology_name,
                                         self.table_name,
                                         all_feature_faces_live(i),
                                         self.tolerance);

         working_face.update_measurements;

      END LOOP;

      IF bail_out = 1
      THEN

         --hopefully cleaned up the topo and measurments back to a consistent state after the add edge failure
         --Must still error out to say this was a fail
         RAISE_APPLICATION_ERROR(-20001,'Failed in adding new nodes and edges and tidied up. Error was ' || bail_msg);

      END IF;

   END collapse_to_universal;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_edge_geometry(
      p_edge_id            IN NUMBER
   ) RETURN SDO_GEOMETRY
   AS

      psql           VARCHAR2(256);
      geometry       SDO_GEOMETRY;

   BEGIN

      psql := 'SELECT e.geometry '
           || 'FROM ' || self.topology_name || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO geometry USING ABS(p_edge_id);

      IF geometry IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Didnt get a geometry for edge id ' || ABS(p_edge_id));

      END IF;

      RETURN geometry;

   END get_edge_geometry;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_node_geometry(
      p_node_id            IN NUMBER
   ) RETURN SDO_GEOMETRY
   AS

      psql           VARCHAR2(256);
      geometry       SDO_GEOMETRY;

   BEGIN

      psql := 'SELECT n.geometry '
           || 'FROM ' || self.topology_name || '_node$ n '
           || 'WHERE n.node_id = :p1 ';

      EXECUTE IMMEDIATE psql INTO geometry USING ABS(p_node_id);

      IF geometry IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Didnt get a geometry for edge id ' || ABS(p_node_id));

      END IF;

      RETURN geometry;

   END get_node_geometry;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_sliver_opposing_edges(
      p_direction             IN VARCHAR2
   )RETURN MDSYS.SDO_LIST_TYPE
   AS

      --M@! 7/18/13
      --Get edges opposite the universal face - only those "inside" the sliver
      --That can be used to chomp down from a neighboring face

      --Will be using a simplified understanding of chomping
      --  The nodes bordering an opposing edge are either both droppable, fully interiors (then edge removal between the nodes)
      --  Or 1 node is droppable for either the UF-touching edge or the next edge (then edge removal)
      --This means that the only edges we will test for dropability are the 2 that touch the UF,
      --  all others are assumed to chomp in some fashion

      --If there is just one edge, its the one
      --If there are two edges and no node is droppable, go with the longest

      --This isn't ready for partials yet, though its close
      --needs to know when to stop traversing

      output                  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      output_kount            PLS_INTEGER := 0;
      signed_interior_edges   MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      universal_geom          SDO_GEOMETRY;
      testing_node_pos        VARCHAR2(5);
      testing_node_geom       SDO_GEOMETRY;
      testing_measure         NUMBER;
      psql                    VARCHAR2(4000);
      max_length              NUMBER := 0;
      max_i                   PLS_INTEGER;

   BEGIN

      IF self.coastal_sliver IS NULL
      OR self.coastal_sliver = 'N'
      THEN

         --why are we here?
         RETURN output;

      END IF;

      --Work counterclockwise usually for vanilla slivers

      --Returns ordered and signed array of interior edges starting at the universal edge
      --either from clockwise side or counterclockwise
      signed_interior_edges := self.get_signed_interior_edges(p_direction);

      IF signed_interior_edges.COUNT = 1
      THEN

         --if theres just one interior edge bounding the sliver, its the one
         --this is often the case
         output.EXTEND(1);
         output(1) := signed_interior_edges(1);
         RETURN output;

      END IF;

      --get the edge to use in the LRS
      universal_geom := self.get_edge_geometry(self.coastal_edges(1));
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
                 || self.topology_name || '_node$ n, '
                 || self.topology_name || '_edge$ e '
                 || 'WHERE '
                 || 'e.edge_id = :p1 AND '
                 || 'e.' || testing_node_pos || '_node_id = n.node_id ';

            EXECUTE IMMEDIATE psql INTO testing_node_geom USING ABS(signed_interior_edges(i));

            testing_node_geom.sdo_srid := NULL;

            --perform projection and locate the measure on the Universal Face edge
            testing_measure := GZ_GEOM_UTILS.GZ_FIND_MEASURE(GZ_GEOM_UTILS.GZ_PROJECT_PT(testing_node_geom,
                                                                                         universal_geom,
                                                                                         self.null_tolerance),
                                                             universal_geom);
            IF self.dbug = 1
            THEN
               dbms_output.put_line('measure is ' || testing_measure);
            END IF;

            IF (testing_measure > 10) AND (testing_measure < 990)
            THEN

               IF self.dbug = 1
               THEN
                  dbms_output.put_line('keeping ' || signed_interior_edges(i));
               END IF;

               output_kount := output_kount + 1;
               output.EXTEND(1);
               output(output_kount) := signed_interior_edges(i);

            ELSE

               IF self.dbug = 1
               THEN
                  dbms_output.put_line('rejecting ' || signed_interior_edges(i));
               END IF;

            END IF;

         ELSE

            --all others go in the bucket no matter what
            --how they actually chomp gets determined later
            output_kount := output_kount + 1;
            output.EXTEND(1);
            output(output_kount) := signed_interior_edges(i);

         END IF;

      END LOOP;

      IF output.COUNT = 0
      THEN

         IF self.dbug = 1
         THEN
            dbms_output.put_line('in length measurer');
         END IF;

         --take the longest edge bordering the sliver
         --  Most likely this a sliver with 2 edges bordering it
         --  and the 1 node between them is offset from the UF edge
         --  A skinny right triangle, basically
         FOR i IN 1 .. (signed_interior_edges.COUNT)
         LOOP

            IF SDO_GEOM.SDO_LENGTH(self.get_edge_geometry(signed_interior_edges(i)), self.tolerance) > max_length
            THEN

               --max length initialized to 0 so theres always a winner
               max_length := SDO_GEOM.SDO_LENGTH(self.get_edge_geometry(signed_interior_edges(i)), self.tolerance);
               max_i      := i;

            END IF;

         END LOOP;

         output.EXTEND(1);
         output(1) := signed_interior_edges(max_i);

      END IF;

      RETURN output;

   END get_sliver_opposing_edges;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION get_partial_sliver_cutoff(
      p_direction          IN VARCHAR2
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 7/17/13
      --Mainly a helper to visualize

      ordered_signed_edges             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      edge_geometry                    SDO_GEOMETRY;
      cutoff                           NUMBER;
      running_length                   NUMBER := 0;
      vtx_geometry                     SDO_GEOMETRY;
      prev_geometry                    SDO_GEOMETRY;

   BEGIN

      IF self.coastal_sliver <> 'PARTIAL'
      THEN

         RETURN NULL;

      END IF;

      IF p_direction = 'CLOCKWISE'
      THEN

         cutoff := coastal_sliver_clock_d;

      ELSIF p_direction = 'COUNTERCLOCKWISE'
      THEN

         cutoff := coastal_sliver_c_clock_d;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Bunk direction ' || p_direction);

      END IF;

      ordered_signed_edges := self.get_signed_interior_edges(p_direction);

      FOR i IN 1 .. ordered_signed_edges.COUNT
      LOOP

         --measure each geometry up to self.coastal_sliver_clock_d or coastal_sliver_c_clock_d

         edge_geometry := self.get_edge_geometry(ordered_signed_edges(i));

         --measure the length of the edge and decide if we need to dive into it

         IF (running_length + SDO_GEOM.SDO_LENGTH(edge_geometry, self.tolerance)) >= cutoff
         THEN

            --this is the edge

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
                               self.srid,
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
                                                                           self.tolerance);


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

            running_length := running_length + SDO_GEOM.SDO_LENGTH(edge_geometry, self.tolerance);

         END IF;

      END LOOP;

      RAISE_APPLICATION_ERROR(-20001, 'Didnt find the cutoff, check some stuff yo');

   END get_partial_sliver_cutoff;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION measure_edge_to_universal(
      p_edge_id            IN NUMBER,
      p_edge_geometry      IN SDO_GEOMETRY,
      p_cutoff_distance    IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 7/16/13
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

   BEGIN

      FOR i IN 1 .. p_edge_geometry.SDO_ORDINATES.COUNT/2
      LOOP

         vtx_geometry := SDO_GEOMETRY(
                         2001,
                         self.srid,
                         SDO_POINT_TYPE(
                                        p_edge_geometry.SDO_ORDINATES( (i * 2) - 1),
                                        p_edge_geometry.SDO_ORDINATES( (i * 2)),
                                        NULL),
                         NULL,
                         NULL);

         IF self.dbug = 1
         THEN
            dbms_output.put_line('measuring edge ' || p_edge_id || ' from ');
            dbms_output.put_line(gz_geom_utils.dump_sdo(vtx_geometry));
         END IF;

         psql := 'SELECT COUNT(edge_id) FROM ('
              || 'SELECT /*+ FIRST_ROWS */ e.edge_id, sdo_nn_distance(1) dist '
              || 'FROM ' || self.topology_name || '_edge$ e '
              || 'WHERE (e.left_face_id = :p1 OR e.right_face_id = :p2) AND '
              || 'SDO_NN(e.geometry, :p3, :p4, 1) = :p5 '
              || 'ORDER BY DIST '
              || ') WHERE rownum = 1 ';


         IF self.dbug = 1
         THEN
            dbms_output.put_line(psql || ' ' || ' using ' || '-1,-1,see above,'
                                     || 'sdo_batch_size=2 dist=' || p_cutoff_distance ||',TRUE');
         END IF;

         EXECUTE IMMEDIATE psql INTO edge_found USING -1, -1,
                                                       vtx_geometry,
                                                       'sdo_batch_size=2 dist=' || p_cutoff_distance,
                                                       'TRUE';
         IF self.dbug = 1
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
                                                                        self.tolerance);

               prev_geometry := vtx_geometry;

            END IF;

            IF self.dbug = 1
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
         RETURN SDO_GEOM.SDO_LENGTH(p_edge_geometry, self.tolerance);
      
      END IF;

   END measure_edge_to_universal;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION measure_face_to_universal(
      p_direction          IN VARCHAR2,   --CLOCKWISE or COUNTERCLOCKWISE
      p_cutoff_distance    IN NUMBER      --stop measuring when we hit this distance. Out of sliver zone
   ) RETURN NUMBER
   AS

      --return the length of the segments traversed or -1 if full traversal

      --Walk the faces edges inward, vertex by vertex (or node, if a new edge)
      --Measure the face width from the vertex/node to the UF
      --If the face_width at a vertex/node <= p_cutoff_distance
      --   Tally the running length of the segments traversed from the UF
      --If the width becomes greater
      --   Return the current tally of the length
      --If the walk reaches the universal face on the opposite side, return -1

      ordered_signed_edges             MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      edge_geometry                    SDO_GEOMETRY;
      running_length                   NUMBER := 0;
      current_edge_length              NUMBER := 0;

   BEGIN

      IF p_direction NOT IN ('CLOCKWISE','COUNTERCLOCKWISE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Invalid p_direction ' || p_direction);

      END IF;

      IF self.coastal_edges.COUNT <> 1
      OR self.coastal_edges IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Need exactly one coastal edge in current implementation.  Did you set coastal edges?');

      END IF;

      --get the edges in this direction
      ordered_signed_edges := self.get_signed_interior_edges(p_direction);

      FOR i IN 1 .. ordered_signed_edges.COUNT
      LOOP

         --measure each geometry

         edge_geometry := self.get_edge_geometry(ordered_signed_edges(i));

         IF (p_direction = 'COUNTERCLOCKWISE' AND ordered_signed_edges(i) < 0)
         OR (p_direction = 'CLOCKWISE' AND ordered_signed_edges(i) > 0)
         THEN

            --reverse edges that run opposite to our current walk
            edge_geometry := SDO_UTIL.REVERSE_LINESTRING(edge_geometry);

         END IF;

         current_edge_length := self.measure_edge_to_universal(ABS(ordered_signed_edges(i)),
                                                               edge_geometry,
                                                               p_cutoff_distance);

         --pulling some Sidey tricks
         --if the return value is positive, we havent blown p_cutoff_distance on the edge and continue
         --if negative (or zero, for nothing tallied) we exceeded the sliver cutoff and tally what we have + what we got back on this one

         IF current_edge_length > 0
         THEN

            --didnt exceed cutoff anywhere on the edge
            running_length := running_length + current_edge_length;

         ELSE

            running_length := running_length + ABS(current_edge_length);
            RETURN running_length;

         END IF;

      END LOOP;

      --if we made it this far then no edges on the walk exceeded the p_cutoff_distance
      --and we have a full sliver

      RETURN -1;

   END measure_face_to_universal;
   
    ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION measure_universal_to_face(
      p_direction          IN VARCHAR2,   --CLOCKWISE or COUNTERCLOCKWISE, pointless, just to mirror other calls
      p_cutoff_distance    IN NUMBER      --stop measuring when we hit this distance. Out of sliver zone
   ) RETURN NUMBER
   AS

      --M@! 8/30/13
      --Only realized I needed to measure this way too after seeing some bad results
      --return the length of the segments traversed or -1 if full traversal

      --walk the coastal edge vertex by vertex
      --measure the face width from the vertex to the nearest face interior edge
      --If the face_width at a vertex/node <= p_cutoff_distance
      --    Tally the running length of segments traversed on the UF
      --If the width becomes greater
      --   Return the current tally of the length.  This value isnt used for anything
      --If the walk reaches the end of the edge return -1

      ordered_edges                    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      uf_edge_geometry                 SDO_GEOMETRY;
      vtx_geometry                     SDO_GEOMETRY;
      psql                             VARCHAR2(4000);
      edge_found                       PLS_INTEGER;
      prev_geometry                    SDO_GEOMETRY;  
      running_length                   NUMBER := 0;

   BEGIN

      IF p_direction NOT IN ('CLOCKWISE','COUNTERCLOCKWISE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Invalid p_direction ' || p_direction);

      END IF;

      IF self.coastal_edges.COUNT <> 1
      OR self.coastal_edges IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Need exactly one coastal edge in current implementation.  Did you set coastal edges?');

      END IF;

      --get the edges in this direction, ordere doesnt actually matter
      ordered_edges := self.get_interior_edges(p_direction);
      
      uf_edge_geometry := self.get_edge_geometry(self.coastal_edges(1));

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
                                         self.srid,
                                         SDO_POINT_TYPE(uf_edge_geometry.SDO_ORDINATES( (i * 2) - 1),
                                                        uf_edge_geometry.SDO_ORDINATES( (i * 2)),
                                                        NULL),
                                          NULL,
                                          NULL);

            psql := 'SELECT COUNT(edge_id) FROM ('
                 || 'SELECT /*+ FIRST_ROWS */ e.edge_id, sdo_nn_distance(1) dist '
                 || 'FROM ' || self.topology_name || '_edge$ e '
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
                                                                        self.tolerance);

               prev_geometry := vtx_geometry;

            END IF;

         END IF;       

      END LOOP;

      --if we made it this far then no edges on the walk exceeded the p_cutoff_distance
      --and we have a full sliver

      RETURN -1;

   END measure_universal_to_face;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE describe_face_object
   AS

   BEGIN

      dbms_output.put_line(describe_face_object);

   END describe_face_object;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION describe_face_object
   RETURN VARCHAR2
   AS

      stringy        VARCHAR2(8000);
      stringy_too    VARCHAR2(8000) := '';
      output         VARCHAR2(8000) := '';

   BEGIN

      output := output || 'topology_name: ' || self.topology_name || chr(10);
      output := output || 'table_name: ' || self.table_name || chr(10);
      output := output || 'face_id: ' || self.face_id || chr(10);
      output := output || 'tolerance: ' || self.tolerance || chr(10);
      output := output || 'tg_layer_id: ' || self.tg_layer_id || chr(10);
      output := output || 'tg_id: ' || self.tg_id || chr(10);
      output := output || 'srid: ' || self.srid || chr(10);

      IF self.feature_tables IS NOT NULL
      THEN

         FOR i IN 1 .. self.feature_tables.COUNT
         LOOP

            IF length(stringy_too) > 7000
            THEN

               stringy := stringy || ' <snip!>';
               stringy_too := stringy_too || ' <snip!>';
               EXIT;

            END IF;

            stringy := stringy || feature_tables(i);
            stringy_too := stringy_too || feature_table_geoids(i);

            IF i <> self.feature_tables.COUNT
            THEN
               stringy     := stringy || ',';
               stringy_too := stringy_too || ',';
            END IF;

         END LOOP;

      END IF;

      output := output || 'feature_tables: ( ' || stringy || ' )' || chr(10);
      output := output || 'feature_table_geoids: ( ' || stringy_too || ' )' || chr(10);

      IF self.boundary_edges IS NOT NULL
      AND self.boundary_edges.COUNT > 0
      THEN

         FOR i IN 1 .. self.boundary_edges.COUNT
         LOOP

            IF i <> 1
            THEN

               stringy := stringy || ',' || self.boundary_edges(i);

            ELSE

               stringy := self.boundary_edges(i);

            END IF;

         END LOOP;

      END IF;

      output := output || 'boundary_edges: ( ' || stringy || ' )' || chr(10);

      stringy := '';

      IF self.signed_boundary_edges IS NOT NULL
      AND self.signed_boundary_edges.COUNT > 0
      THEN

         FOR i IN 1 .. self.signed_boundary_edges.COUNT
         LOOP

            IF i <> 1
            THEN

               stringy := stringy || ',' || self.signed_boundary_edges(i);

            ELSE

               stringy := self.signed_boundary_edges(i);

            END IF;

         END LOOP;

      END IF;

      output := output || 'signed_boundary_edges: ( ' || stringy || ' )' || chr(10);

      stringy := '';

      IF self.boundary_faces IS NOT NULL
      AND self.boundary_faces.COUNT > 0
      THEN

         FOR i IN 1 .. self.boundary_faces.COUNT
         LOOP

            IF i <> 1
            THEN

               stringy := stringy || ',' || self.boundary_faces(i);

            ELSE

               stringy := self.boundary_faces(i);

            END IF;

         END LOOP;

      END IF;

      output := output || 'boundary_faces: ( ' || stringy || ' )' || chr(10);

      output := output || 'coastal: ' || self.coastal || chr(10);

      stringy := '';

      IF self.coastal_edges IS NOT NULL
      AND self.coastal_edges.COUNT > 0
      THEN

         FOR i IN 1 .. self.coastal_edges.COUNT
         LOOP

            IF i <> 1
            THEN

               stringy := stringy || ',' || self.coastal_edges(i);

            ELSE

               stringy := self.coastal_edges(i);

            END IF;

         END LOOP;

      END IF;

      output := output || 'coastal_edges: ( ' || stringy || ' )' || chr(10);

      output := output || 'coastal_sliver: ' || self.coastal_sliver || chr(10);
      output := output || 'coastal_sliver_clock_d: ' || self.coastal_sliver_clock_d || chr(10);
      output := output || 'coastal_sliver_c_clock_d: ' || self.coastal_sliver_c_clock_d || chr(10);

      --too big?
      --dbms_output.put_line('sdogeometry: ' || TO_CHAR(GZ_GEOM_UTILS.DUMP_SDO(self.sdogeometry)));

      RETURN output;

   END describe_face_object;

END;
/