CREATE OR REPLACE
TYPE BODY gz_topo_edge
AS

   --Matt! 9/06/13
   --instantiate wrt a face
   
--   --standalone sample.  Typically only manage from within the feature face object
--   declare
--
--      work_edge    gz_topo_edge;
--   
--   begin
--
--      work_edge := gz_topo_edge('Z653LS', 8637, 13017);   
--      work_edge.describe_edge_object;
--
--   end;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --Default constructor
   CONSTRUCTOR FUNCTION gz_topo_edge
   RETURN SELF AS RESULT
   AS
   BEGIN
      RETURN;

   END gz_topo_edge;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   CONSTRUCTOR FUNCTION gz_topo_edge(
      p_topology_name      IN VARCHAR2,
      p_edge_id            IN NUMBER,
      p_face_id            IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_dbug               IN NUMBER DEFAULT NULL
   ) RETURN SELF AS RESULT
   AS
   
      psql              VARCHAR2(4000);
      
   BEGIN

      self.topology_name := UPPER(p_topology_name);
      self.face_id       := p_face_id;
      self.edge_id       := p_edge_id;
      self.tolerance     := p_tolerance;
      
      
      psql := 'SELECT e.start_node_id, e.end_node_id, e.left_face_id, e.right_face_id, e.geometry '
           || 'FROM ' || p_topology_name || '_edge$ e '
           || 'WHERE e.edge_id = :p1 ';
           
      EXECUTE IMMEDIATE psql INTO self.start_node_id, self.end_node_id,
                                  self.left_face_id, self.right_face_id,
                                  self.geometry USING p_edge_id;
      
      IF self.left_face_id = p_face_id
      THEN
      
         self.face_side := 'L';
         
      ELSIF self.right_face_id = p_face_id
      THEN
      
         self.face_side := 'R';
         
      ELSE
      
         RAISE_APPLICATION_ERROR(-20001, 'Edge ' || p_edge_id || ' doesnt bound ' || p_face_id);
      
      END IF;
      

      IF p_dbug IS NOT NULL
      THEN

         self.dbug := 1;

      END IF;
      
      IF self.geometry.sdo_srid = 8265
      AND self.tolerance = .05
      THEN
         self.null_tolerance := 0.0000005;
      ELSIF self.geometry.sdo_srid = 8265
      AND self.tolerance <> .05
      THEN
         self.null_tolerance := GZ_CLIP.TOLERANCE_CONVERTER(self.tolerance, self.geometry.sdo_srid, 'NULL');
      ELSE
         self.null_tolerance := self.tolerance;
      END IF;

      RETURN;

   END gz_topo_edge;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION add_node(
      p_sdogeometry           IN SDO_GEOMETRY,
      p_must_be_existing      IN VARCHAR2 DEFAULT NULL
   ) RETURN NUMBER AS
   
      temp_segment            SDO_GEOMETRY;
      coord_index             NUMBER;
      output                  NUMBER;
      new_shape_pt            VARCHAR2(5);
   
   BEGIN
   
      IF p_sdogeometry.sdo_gtype <> 2001
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Cant add a node with gtype ' || p_sdogeometry.sdo_gtype);
      
      END IF;
   
      FOR i IN 1 .. self.geometry.SDO_ORDINATES.COUNT/2
      LOOP
        
         temp_segment := SDO_GEOMETRY(2002,
                                      NULL,   --always use null srid for topo work, and this is an edge$ edge
                                      NULL,
                                      SDO_ELEM_INFO_ARRAY(1,2,1),
                                      SDO_ORDINATE_ARRAY( self.geometry.SDO_ORDINATES( (i*2) - 1),
                                                          self.geometry.SDO_ORDINATES( (i*2)),
                                                          self.geometry.SDO_ORDINATES( (i*2) + 1),
                                                          self.geometry.SDO_ORDINATES( (i*2) + 2)
                                                         )
                                       );
         
         --use anyinteract for brutish force approach
         
         IF sdo_geom.relate(temp_segment, 'mask=ANYINTERACT', p_sdogeometry, self.null_tolerance) = 'TRUE'   --true for anyinteract only
         THEN 
         
            --check if its an existing shape point at the end, first time it appeared
            IF  p_sdogeometry.SDO_POINT.X = self.geometry.SDO_ORDINATES( (i*2) + 1)
            AND p_sdogeometry.SDO_POINT.X = self.geometry.SDO_ORDINATES( (i*2) + 2)
            THEN
            
               new_shape_pt := 'FALSE';
               --Increment to next index by not subtracting 1
               coord_index := i;
               
            ELSE
            
               IF i = 1
               AND p_sdogeometry.SDO_POINT.X = self.geometry.SDO_ORDINATES( (i*2) - 1)
               AND p_sdogeometry.SDO_POINT.Y = self.geometry.SDO_ORDINATEs( (i*2))
               THEN
               
                   --if this is the first segment be extra careful that this isnt the first node
                   RAISE_APPLICATION_ERROR(-20001, 'Cant add a node at the beginning of edge ' || self.edge_id);

               END IF;
               
               new_shape_pt := 'TRUE';
               coord_index  := (i - 1);
            
            END IF;
         
            IF p_must_be_existing IS NOT NULL
            AND new_shape_pt = 'TRUE'
            THEN
           
               RAISE_APPLICATION_ERROR(-20001, 'Didnt find an existing shape point on edge ' || self.edge_id);
               
            END IF;
            
         END IF;

      END LOOP;
      
      IF coord_index IS NULL
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Failed to find the location on edge ' || self.edge_id);
      
      END IF;
      
      --what about error handlers?  Use explicit topomap and whatnot
      output := SDO_TOPO_MAP.ADD_NODE(self.topology_name,
                                      self.edge_id,
                                      p_sdogeometry,
                                      coord_index,
                                      new_shape_pt);
      
      
      RETURN output;
   
   END add_node;
   
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER PROCEDURE describe_edge_object (
      p_indent             IN VARCHAR2 DEFAULT NULL
   )
   AS

   BEGIN

      dbms_output.put_line(describe_edge_object);

   END describe_edge_object;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   MEMBER FUNCTION describe_edge_object (
      p_indent             IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS


      output         VARCHAR2(8000) := '';

   BEGIN

      output := output || p_indent || 'topology_name: ' || self.topology_name || chr(10);
      output := output || p_indent || 'face_id: ' || self.face_id || chr(10);
      output := output || p_indent || 'edge_id: ' || self.edge_id || chr(10);
      output := output || p_indent || 'face_side: ' || self.face_side || chr(10);
      output := output || p_indent || 'start_node_id: ' || self.start_node_id || chr(10);
      output := output || p_indent || 'end_node_id: ' || self.end_node_id || chr(10);
      output := output || p_indent || 'left_face_id: ' || self.left_face_id || chr(10);
      output := output || p_indent || 'right_face_id: ' || self.right_face_id || chr(10);
      
      IF self.geometry IS NOT NULL
      THEN
      
         output := output || p_indent || 'geometry: ' || 'SELECT GZ_GEOM_UTILS.DUMP_SDO(geometry) from '
                                      || self.topology_name || '_edge$ where edge_id = ' || self.edge_id || chr(10);
      
      END IF;
      
      output := output || p_indent || 'tolerance: ' || self.tolerance || chr(10);
      output := output || p_indent || 'null_tolerance: ' || self.null_tolerance || chr(10);

      RETURN output;

   END describe_edge_object;
   
   
END;
/