CREATE OR REPLACE PACKAGE BODY GZ_TOPO_EDIT AS
/******************************************************************************
   NAME:       GZ_TOPO_EDIT
   PURPOSE:
   REVISIONS:
   Ver        Date        Author           Description
   ---------  ----------  ---------------  ------------------------------------
   1.00       10/07/2009  Sreeni Karpurapu 1. Created this package body.
   1.01       10/21/2009  Sreeni Karpurapu Procedure: Delete Edges
                                           Now deleting the face that topology
                                             removed, not the one provided.
   1.02       10/30/2009  Sreeni Karpurapu Modified code to make it more generic
                                           Feature Attribute columns are
                                             processed dynamically.
   1.10       11/09/2009  Sreeni Karpurapu Added code to fix slivers.
******************************************************************************/
FUNCTION GET_STATE_FIPS RETURN  MDSYS.SDO_LIST_TYPE AS
-- Generate State Fips codes
   Fips            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
BEGIN
   Fips.extend(52);
   Fips(1) := 1;
   Fips(2) := 2;
   Fips(3) := 4;
   Fips(4) := 5;
   Fips(5) := 6;
   Fips(6) := 8;
   Fips(7) := 9;
   Fips(8) := 10;
   Fips(9) := 11;
   Fips(10) := 12;
   Fips(11) := 13;
   Fips(12) := 15;
   Fips(13) := 16;
   Fips(14) := 17;
   Fips(15) := 18;
   Fips(16) := 19;
   Fips(17) := 20;
   Fips(18) := 21;
   Fips(19) := 22;
   Fips(20) := 23;
   Fips(21) := 24;
   Fips(22) := 25;
   Fips(23) := 26;
   Fips(24) := 27;
   Fips(25) := 28;
   Fips(26) := 29;
   Fips(27) := 30;
   Fips(28) := 31;
   Fips(29) := 32;
   Fips(30) := 33;
   Fips(31) := 34;
   Fips(32) := 35;
   Fips(33) := 36;
   Fips(34) := 37;
   Fips(35) := 38;
   Fips(36) := 39;
   Fips(37) := 40;
   Fips(38) := 41;
   Fips(39) := 42;
   Fips(40) := 44;
   Fips(41) := 45;
   Fips(42) := 46;
   Fips(43) := 47;
   Fips(44) := 48;
   Fips(45) := 49;
   Fips(46) := 50;
   Fips(47) := 51;
   Fips(48) := 53;
   Fips(49) := 54;
   Fips(50) := 55;
   Fips(51) := 56;
   Fips(52) := 72;
   RETURN FIPS;
END GET_STATE_FIPS;
  FUNCTION IS_NODE(topology IN VARCHAR2, ordX IN NUMBER, ordY IN NUMBER)
  RETURN NUMBER AS
     sql1 VARCHAR2(1000);
     nodeID NUMBER;
     dist  NUMBER;
  BEGIN
     sql1 := 'SELECT n.node_id, sdo_nn_distance(1) '
          || ' FROM  ' || topology || '_NODE$ n '
          ||' WHERE SDO_NN(n.GEOMETRY, sdo_geometry(2001, 8265, '
          ||                   ' SDO_POINT_TYPE(:1,:2, NULL), NULL, NULL), ''sdo_batch_size=10'',1) = ''TRUE'' '
          || ' AND ROWNUM=1 ';
     --DBMS_OUTPUT.PUT_LINE(sql1);
     EXECUTE IMMEDIATE sql1 INTO nodeID, dist USING ordX, ordY;
     IF (dist = 0) THEN
        RETURN nodeID;
     ELSE
        RETURN NULL;
     END IF;
  END;
  FUNCTION GET_CLOSEST_EDGE(topology IN VARCHAR2, ordX IN NUMBER, ordY IN NUMBER)
  RETURN NUMBER AS
      -- Use this function only if the vertex is not a node
      -- otherwise it may return inaccurate data.
      edgID NUMBER := NULL;
      dist  NUMBER;
      sql1 VARCHAR2(1000);
   BEGIN
      sql1 := 'SELECT e.edge_id, sdo_nn_distance(1) '
            || ' FROM  ' || topology || '_EDGE$ e '
            ||' WHERE SDO_NN(e.GEOMETRY, sdo_geometry(2001, 8265, '
            ||                   ' SDO_POINT_TYPE(:1,:2, NULL), NULL, NULL), ''sdo_batch_size=10'',1) = ''TRUE'' '
            || ' AND ROWNUM=1 ';
      --DBMS_OUTPUT.PUT_LINE(sql1);
      EXECUTE IMMEDIATE sql1 INTO edgID, dist USING ordX, ordY;
      --DBMS_OUTPUT.PUT_LINE('edgID: ' || edgID);
      --DBMS_OUTPUT.PUT_LINE('dist: ' || dist);
      IF (dist = 0) THEN
         RETURN edgID;
      ELSE
         RETURN NULL;
      END IF;
  END;
  FUNCTION ADD_NODE(topology IN VARCHAR2, tplgy IN VARCHAR2, edgeID IN NUMBER, ordX IN NUMBER, ordY IN NUMBER)
  RETURN NUMBER AS
      nodePos NUMBER;
      nodeID NUMBER;
      dist  NUMBER;
      foundNode BOOLEAN := FALSE;
      sql1 VARCHAR2(1000);
      EdgeOrdArray MDSYS.SDO_ORDINATE_ARRAY;
      isNewShapePoint VARCHAR2(10); -- Oracle Defined this as a Varchar2 not as Boolean
   BEGIN
      --DBMS_OUTPUT.PUT_LINE('TOPOLOGY: ' || topology || ' tplgy: ' || tplgy || ' edgeID: ' || edgeID || ' ordX: ' || ordX || ' ordY: ' || ordY);
      EdgeOrdArray := MDSYS.SDO_ORDINATE_ARRAY();
      sql1 := 'SELECT e.geometry.sdo_ordinates '
            || ' FROM ' || topology || '_edge$ e '
            ||' WHERE edge_id = :1';
      EXECUTE IMMEDIATE sql1 INTO EdgeOrdArray USING edgeID;
      --DBMS_OUTPUT.PUT_LINE('EdgeID: ' || edgeID || ' EdgeOrdArray.count = ' || EdgeOrdArray.count);
      isNewShapePoint := is_new_shape_point(topology, edgeID, ordX, ordY);
      IF isNewShapePoint = 'TRUE' THEN
         --A value of TRUE lets you add a node at a new point,
         --breaking an edge segment at the coordinates specified in the point parameter.
         nodePos := null;
         nodePos := get_node_pos_TRUE(topology, edgeID, ordX, ordY);
      ELSE
         --A value of FALSE causes the coordinates in the point parameter to be ignored,
         --and causes the node to be added at the existing shape point associated with the coord_index value.
         nodePos := null;
         nodePos := get_node_pos(topology, edgeID, ordX, ordY);
      END IF;
      DBMS_OUTPUT.PUT_LINE('nodePos: ' || nodePos || ' EdgeID: ' || edgeID || ' ordX: ' || ordX || ' ordY: ' || ordY || ' isNewShapePoint: ' || isNewShapePoint);
/*
      For i in 1..EdgeOrdArray.count
      Loop
         --DBMS_OUTPUT.PUT_LINE('EdgeOrdArray(' || i || ') = ' || EdgeOrdArray(i));
         --if (round(EdgeOrdArray(i),6) = ordX and round(EdgeOrdArray(i+1),6) = ordY) and not(foundNode) then
         if (round(EdgeOrdArray(i),6) = round(ordX,6) and round(EdgeOrdArray(i+1),6) = round(ordY,6)) and not(foundNode) then
         --if (EdgeOrdArray(i) = ordX and EdgeOrdArray(i+1) = ordY) and not(foundNode) then
            --DBMS_OUTPUT.PUT_LINE('***** Found vt at ' || i || ' *****');
            --DBMS_OUTPUT.PUT_LINE('***** Vertex # ' || (i+1)/2 || ' *****');
            --DBMS_OUTPUT.PUT_LINE('ordX: ' || EdgeOrdArray(i));
            --DBMS_OUTPUT.PUT_LINE('ordY: ' || EdgeOrdArray(i+1));
            nodePos := (i-1)/2;  --not (i+1)/2
            --ordX1 := EdgeOrdArray(i);
            --ordY1 := EdgeOrdArray(i+1);
            foundNode := true;
         end if;
      End Loop;
      EdgeOrdArray.delete;
      --DBMS_OUTPUT.PUT_LINE('nodePos: ' || nodePos);
*/
      --IF (nodePos IS NOT NULL)
      --THEN
         --nodeID := sdo_topo_map.add_node(topology, edgeID, ordX, ordY, nodePos, 'FALSE'); -- TRUE if Node is a shape point
         nodeID := sdo_topo_map.add_node(tplgy, edgeID, ordX, ordY, nodePos, isNewShapePoint);
         --nodeID := 9999;
         DBMS_OUTPUT.PUT_LINE('Newly added NodeID = ' || nodeID);
         SDO_TOPO_MAP.UPDATE_TOPO_MAP;
      --ELSE
      --   DBMS_OUTPUT.PUT_LINE('add_node: Unable to locate nodePosition.  Please verify vertex coordinates');
      --END IF;
      RETURN nodeID;
  END;
  FUNCTION ADD_NODE2(topology IN VARCHAR2, edgeID IN NUMBER, ordX IN NUMBER, ordY IN NUMBER)
  RETURN NUMBER AS
      nodePos NUMBER;
      nodeID NUMBER;
      dist  NUMBER;
      foundNode BOOLEAN := FALSE;
      sql1 VARCHAR2(1000);
      EdgeOrdArray MDSYS.SDO_ORDINATE_ARRAY;
      isNewShapePoint VARCHAR2(10); -- Oracle Defined this as a Varchar2 not as Boolean
   BEGIN
      EdgeOrdArray := MDSYS.SDO_ORDINATE_ARRAY();
      sql1 := 'SELECT e.geometry.sdo_ordinates '
            || ' FROM ' || topology || '_edge$ e '
            ||' WHERE edge_id = :1';
      EXECUTE IMMEDIATE sql1 INTO EdgeOrdArray USING edgeID;
      --DBMS_OUTPUT.PUT_LINE('EdgeID: ' || edgeID || ' EdgeOrdArray.count = ' || EdgeOrdArray.count);
      nodePos := null;
      nodePos := get_node_pos(topology, edgeID, ordX, ordY);
      isNewShapePoint := is_new_shape_point(topology, edgeID, ordX, ordY);
      DBMS_OUTPUT.PUT_LINE('nodePos: ' || nodePos || ' EdgeID: ' || edgeID || ' ordX: ' || ordX || ' ordY: ' || ordY || ' isNewShapePoint: ' || isNewShapePoint);
/*
      For i in 1..EdgeOrdArray.count
      Loop
         --DBMS_OUTPUT.PUT_LINE('EdgeOrdArray(' || i || ') = ' || EdgeOrdArray(i));
         --if (round(EdgeOrdArray(i),6) = ordX and round(EdgeOrdArray(i+1),6) = ordY) and not(foundNode) then
         if (round(EdgeOrdArray(i),6) = round(ordX,6) and round(EdgeOrdArray(i+1),6) = round(ordY,6)) and not(foundNode) then
         --if (EdgeOrdArray(i) = ordX and EdgeOrdArray(i+1) = ordY) and not(foundNode) then
            --DBMS_OUTPUT.PUT_LINE('***** Found vt at ' || i || ' *****');
            --DBMS_OUTPUT.PUT_LINE('***** Vertex # ' || (i+1)/2 || ' *****');
            --DBMS_OUTPUT.PUT_LINE('ordX: ' || EdgeOrdArray(i));
            --DBMS_OUTPUT.PUT_LINE('ordY: ' || EdgeOrdArray(i+1));
            nodePos := (i-1)/2;  --not (i+1)/2
            --ordX1 := EdgeOrdArray(i);
            --ordY1 := EdgeOrdArray(i+1);
            foundNode := true;
         end if;
      End Loop;
      EdgeOrdArray.delete;
      --DBMS_OUTPUT.PUT_LINE('nodePos: ' || nodePos);
*/
      IF (nodePos IS NOT NULL)
      THEN
         --nodeID := sdo_topo_map.add_node(topology, edgeID, ordX, ordY, nodePos, 'FALSE'); -- TRUE if Node is a shape point
         nodeID := sdo_topo_map.add_node(topology, edgeID, ordX, ordY, nodePos, isNewShapePoint);
         --nodeID := 9999;
         DBMS_OUTPUT.PUT_LINE('Newly added NodeID = ' || nodeID);
      ELSE
         DBMS_OUTPUT.PUT_LINE('add_node: Unable to locate nodePosition.  Please verify vertex coordinates');
      END IF;
      RETURN nodeID;
   END;
   FUNCTION GET_NODE_POS(topology IN VARCHAR2, edgeID IN NUMBER, ordX IN NUMBER, ordY IN NUMBER)
   RETURN NUMBER AS
      nodePos NUMBER;
      nodeID NUMBER;
      dist  NUMBER;
      foundNode BOOLEAN := FALSE;
      sql1 VARCHAR2(1000);
      EdgeOrdArray MDSYS.SDO_ORDINATE_ARRAY;
      isNewShapePoint VARCHAR2(10); -- Oracle Defined this as a Varchar2 not as Boolean
   BEGIN
      EdgeOrdArray := MDSYS.SDO_ORDINATE_ARRAY();
      sql1 := 'SELECT e.geometry.sdo_ordinates '
            || ' FROM ' || topology || '_edge$ e '
            ||' WHERE edge_id = :1';
      EXECUTE IMMEDIATE sql1 INTO EdgeOrdArray USING edgeID;
      For i in 1..EdgeOrdArray.count
      Loop
         --DBMS_OUTPUT.PUT_LINE('EdgeOrdArray(' || i || ') = ' || EdgeOrdArray(i));
         if (round(EdgeOrdArray(i),6) = round(ordX,6) and round(EdgeOrdArray(i+1),6) = round(ordY,6)) and not(foundNode) then
         --SK**** if (EdgeOrdArray(i) = ordX and EdgeOrdArray(i+1) = ordY) and not(foundNode) then
         --if ((round(EdgeOrdArray(i),6) = round(ordX,6)) and (round(EdgeOrdArray(i+1),6) = round(ordY,6))) then
         --if (EdgeOrdArray(i) = ordX and EdgeOrdArray(i+1) = ordY) and not(foundNode) then
            --DBMS_OUTPUT.PUT_LINE('***** Found vt at ' || i || ' *****');
            --DBMS_OUTPUT.PUT_LINE('***** Vertex # ' || (i+1)/2 || ' *****');
            --DBMS_OUTPUT.PUT_LINE('ordX: ' || EdgeOrdArray(i));
            --DBMS_OUTPUT.PUT_LINE('ordY: ' || EdgeOrdArray(i+1));
            nodePos := (i-1)/2;  --not (i+1)/2
            --ordX1 := EdgeOrdArray(i);
            --ordY1 := EdgeOrdArray(i+1);
            foundNode := true;
         end if;
      End Loop;
      EdgeOrdArray.delete;
      RETURN NodePos;
   END;
   FUNCTION GET_NODE_POS_TRUE(topology IN VARCHAR2, edgeID IN NUMBER, ordX IN NUMBER, ordY IN NUMBER)
   RETURN NUMBER AS
     -- Use this function to get the node position if IS_NEW_SHAPE_POINT is TRUE in ADD_NODE
     tplgy  VARCHAR2(20) := topology;
     inpEdgeID NUMBER := edgeID;
     inpVx NUMBER := ordX;
     inpVy NUMBER := ordY;
     sql1 VARCHAR2(1000) := NULL;
     /*cursor curGeom is
        select f.sdogeometry.sdo_ordinates
          from face3 f
         where face_id = inpFaceID;
     */
     OrdArray MDSYS.SDO_ORDINATE_ARRAY;
     LoopCtr NUMBER := NULL;
     i      PLS_INTEGER := NULL;
     POINT_SEG SDO_GEOMETRY;
     LINE_SEG SDO_GEOMETRY;
     tmpDist NUMBER := null;
     NodePos NUMBER := null;
   BEGIN
      OrdArray := MDSYS.SDO_ORDINATE_ARRAY();
      sql1 := 'SELECT e.geometry.sdo_ordinates FROM ' || tplgy || '_EDGE$ e WHERE edge_id = :1';
      EXECUTE IMMEDIATE sql1 INTO OrdArray USING inpEdgeID;
      DBMS_OUTPUT.PUT_LINE('EdgeID: ' || inpEdgeID || ' OrdArray.count = ' || OrdArray.COUNT);
      POINT_SEG := sdo_geometry(2001, 8265, SDO_POINT_TYPE(inpVx,inpVy, NULL), NULL, NULL);
      /*
      LINE_SEG := SDO_GEOMETRY(2002, 8265, NULL,SDO_ELEM_INFO_ARRAY(1,2,1),
                          SDO_ORDINATE_ARRAY(
     -90.31070800000000531326804775744676589966,
     41.74221399999999704277797718532383441925
     -90.31521999999999650299287168309092521667,
     41.73426400000000313639247906394302845001
                            ));
      Select sdo_geom.sdo_distance(POINT_SEG, LINE_SEG, 0.05, 'UNIT=mile') into tmpDist from dual;
      DBMS_OUTPUT.PUT_LINE('tmpDist: ' || tmpDist);
      */
      NodePos := NULL;
      LoopCtr := OrdArray.COUNT-3;  -- We need 4 ordinates at a time
      --DBMS_OUTPUT.PUT_LINE('OrdArray.Count: ' || OrdArray.count);
      i := 1;
      WHILE i <= LoopCtr
      --For i in 1..LoopCtr
      LOOP
         --DBMS_OUTPUT.PUT_LINE('OrdArray(' || i || ') = ' || OrdArray(i));
         --DBMS_OUTPUT.PUT_LINE('OrdArray(' || to_char(i+1) || ') = ' || OrdArray(i+1));
         --DBMS_OUTPUT.PUT_LINE('OrdArray(' || to_char(i+2) || ') = ' || OrdArray(i+2));
         --DBMS_OUTPUT.PUT_LINE('OrdArray(' || to_char(i+3) || ') = ' || OrdArray(i+3));
         LINE_SEG := SDO_GEOMETRY(2002, 8265, NULL,
                                 SDO_ELEM_INFO_ARRAY(1,2,1),
                                 SDO_ORDINATE_ARRAY(OrdArray(i), OrdArray(i+1), OrdArray(i+2), OrdArray(i+3)));
         SELECT sdo_geom.sdo_distance(POINT_SEG, LINE_SEG, 0.05, 'UNIT=meter') INTO tmpDist FROM dual;
         --DBMS_OUTPUT.PUT_LINE('i: ' || i || ' tmpDist: ' || tmpDist);
         IF (tmpDist = 0)
         THEN
            --DBMS_OUTPUT.PUT_LINE('******************************');
            --DBMS_OUTPUT.PUT_LINE('i: ' || i || ' tmpDist: ' || tmpDist);
            --DBMS_OUTPUT.PUT_LINE('NodePos: ' || to_char((i+1)/2) );
            --DBMS_OUTPUT.PUT_LINE('NodePos: ' || i);
            --DBMS_OUTPUT.PUT_LINE('Line Seg: ' || OrdArray(i) || ', ' || OrdArray(i+1) || ', ' || OrdArray(i+2) || ', ' || OrdArray(i+3));
            --DBMS_OUTPUT.PUT_LINE('******************************');
            --NodePos := (i+1)/2; -- Starting Pos is 0 not 1
            NodePos := (i+1)/2 -1;
            DBMS_OUTPUT.PUT_LINE('GET_NODE_POS: ' || NodePos);
            OrdArray.DELETE;
            RETURN(NodePos);
         ELSE
            --DBMS_OUTPUT.PUT_LINE('i: ' || i || ' tmpDist: ' || tmpDist);
            --DBMS_OUTPUT.PUT_LINE('NodePos: ' || to_char((i+1)/2) );
            --DBMS_OUTPUT.PUT_LINE('Line Seg: ' || OrdArray(i) || ', ' || OrdArray(i+1) || ', ' || OrdArray(i+2) || ', ' || OrdArray(i+3));
            NULL;
         END IF;
         i := i+2; -- Account for x & y ordinates
      END LOOP;
      OrdArray.DELETE;
      DBMS_OUTPUT.PUT_LINE('GET_NODE_POS: NULL');
      RETURN NodePos;
   END;
FUNCTION IS_NEW_SHAPE_POINT(topology IN VARCHAR2, edgeID IN NUMBER, ordX IN NUMBER, ordY IN NUMBER)
RETURN VARCHAR2 AS
/*
  is_new_shape_point
  ==================
  A value of TRUE lets you add a node at a new point,
    breaking an edge segment at the coordinates specified in the point parameter.
  A value of FALSE causes the coordinates in the point parameter to be ignored,
    and causes the node to be added at the existing shape point associated with the coord_index value.
*/
  tplgy  VARCHAR2(20) := topology;
  inpEdgeID NUMBER := edgeID;
  inpVx NUMBER := ordX;
  inpVy NUMBER := ordY;
  sql1 VARCHAR2(1000) := NULL;
  /*cursor curGeom is
     select f.sdogeometry.sdo_ordinates
       from face3 f
      where face_id = inpFaceID;
  */
  OrdArray MDSYS.SDO_ORDINATE_ARRAY;
  LoopCtr NUMBER := NULL;
  i      PLS_INTEGER := NULL;
BEGIN
  OrdArray := MDSYS.SDO_ORDINATE_ARRAY();
  sql1 := 'SELECT e.geometry.sdo_ordinates FROM ' || tplgy || '_EDGE$ e WHERE edge_id = :1';
  EXECUTE IMMEDIATE sql1 INTO OrdArray USING inpEdgeID;
   --DBMS_OUTPUT.PUT_LINE('EdgeID: ' || inpEdgeID || ' OrdArray.count = ' || OrdArray.COUNT);
   LoopCtr := OrdArray.COUNT;
   --DBMS_OUTPUT.PUT_LINE('OrdArray.Count: ' || OrdArray.count);
   i := 1;
   WHILE i <= LoopCtr
   --For i in 1..LoopCtr
   LOOP
      --DBMS_OUTPUT.PUT_LINE('OrdArray(' || i || ') = ' || OrdArray(i));
      --DBMS_OUTPUT.PUT_LINE('OrdArray(' || to_char(i+1) || ') = ' || OrdArray(i+1));
      IF (round(ordArray(i),6) = round(inpVx,6)) and (round(ordArray(i+1),6) = round(inpVy,6))
      THEN
         DBMS_OUTPUT.PUT_LINE('IS_NEW_SHAPE_POINT: FALSE');
         RETURN 'FALSE';
      ELSE
         NULL;
      END IF;
      i := i+2; -- Account for x & y ordinates
   END LOOP;
   OrdArray.DELETE;
   DBMS_OUTPUT.PUT_LINE('IS_NEW_SHAPE_POINT: TRUE');
   RETURN 'TRUE';
END;
  FUNCTION GET_EDGE(pTplgy IN varchar, pN1 IN NUMBER, pN2 IN NUMBER)
  RETURN edgTyp AS
      tplgy VARCHAR2(50) := pTplgy;
      edgeID NUMBER := NULL;
      startNode NUMBER := NULL;
      endNode NUMBER := NULL;
      sql1 VARCHAR2(1000);
      edgCnt NUMBER := NULL;
      theEdge edgTyp;
   BEGIN
      sql1 := 'SELECT count(edge_id) '
            || ' FROM ' || tplgy || '_edge$ e '
            ||' WHERE (start_node_id = :1 AND end_node_id = :2) OR (start_node_id = :3 AND end_node_id = :4)';
      EXECUTE IMMEDIATE sql1 INTO edgCnt USING pN1, pN2, PN2, PN1;
      IF (edgCnt = 1) THEN
         sql1 := 'SELECT edge_id, start_node_id, end_node_id '
               || ' FROM ' || tplgy || '_edge$ e '
               ||' WHERE (start_node_id = :1 AND end_node_id = :2) OR (start_node_id = :3 AND end_node_id = :4)';
         --where (start_node_id = 99416 and end_node_id = 80166) or (start_node_id = 80166 and end_node_id = 99416 )
         EXECUTE IMMEDIATE sql1 INTO edgeID, startNode, endNode USING pN1, pN2, PN2, PN1;
         theEdge.ID := edgeID;
         theEdge.sNode := startNode;
         theEdge.eNode := endNode;
      ELSE
         DBMS_OUTPUT.PUT_LINE('Edge count is not equal to one.  It is : ' || edgCnt);
         theEdge.ID := NULL;
         theEdge.sNode := NULL;
         theEdge.eNode := NULL;
      END IF;
      RETURN theEdge;
  END;
PROCEDURE GET_FACE_MBR(pTplgy IN VARCHAR2, pFaceID IN NUMBER, pXmin IN OUT NUMBER, pYmin IN OUT NUMBER, pXmax IN OUT NUMBER, pYmax IN OUT NUMBER) AS
  tblName varchar2(50) := pTplgy || '_FACE$';
  inpFaceID number := pFaceID;
  sql1 varchar2(1000) := null;
  OrdArray MDSYS.SDO_ORDINATE_ARRAY;
BEGIN
  OrdArray := MDSYS.SDO_ORDINATE_ARRAY();
  sql1 := 'SELECT f.mbr_geometry.sdo_ordinates FROM ' || tblName || ' f WHERE face_id = :1';
  EXECUTE IMMEDIATE sql1 INTO OrdArray USING inpFaceID;
   --DBMS_OUTPUT.PUT_LINE('FaceID: ' || inpFaceID || ' OrdArray.count = ' || OrdArray.count);
   pXmin := OrdArray(1);
   pYmin := OrdArray(2);
   pXmax := OrdArray(3);
   pYmax := OrdArray(4);
   --DBMS_OUTPUT.PUT_LINE('pXmin: ' || pXmin || ' pYmin: ' || pYmin || ' pXmax: ' || pXmax || ' pYmax: ' || pYmax);
   OrdArray.delete;
END;
PROCEDURE DELETE_EDGES(topology VARCHAR2, ftrTable VARCHAR2, inpTable VARCHAR2) AS
/*
   NAME:       DELETE_EDGES
   PURPOSE:    Deletes the edges identified in the input table from the
               topology to merge faces.
               The input table identifies which face attributes to save
               from the specified zero-level feature table.
   INPUT TABLE STRUCTURE AND REQUIREMENTS:
               Topology must have one and only one feature table.  Before
               and after running the delete_edges routine, this feature
               table should have a one-to-one face$ to feature relationship.
               Measurements stored in the feature table will be out of sync
               after this program runs, recalculate them before moving on.
              (uses the results of the identify small polygons routine,
              called "ELIMINATE_SMALL_POLYS")
              input table structure...
              EDGE_ID,
              LEFT_FACE_ID,
              RIGHT_FACE_ID,
              KEEP_FACE, (face id whose attributes we will keep)
              NOT_KEEP, (face id whose attributes will be lost)
              AREATOTAL,
              MAX_AREA,
              DONE     (to track what has been processed)
              If the faces to merge have different left and right state
              values... WHAT DOES IT DO?
              This program will create obsolete nodes that should be removed
              prior to line simplification.
     Output:
         Tracks progress by using the "DONE" field in the input table
         'Y' = completed sucessfully,
         '??' = could not complete,
         'QA" = did something, but needs review by a human,
         NULL = unprocessed.
     Author: Sreeni Karpurapu
*/
   psql        VARCHAR2(4000);
   psql1       VARCHAR2(4000);
   psql2       VARCHAR2(4000);
   sql1        VARCHAR2(4000);
   sql2        VARCHAR2(4000);
   sql3        VARCHAR2(4000);
   ctr         pls_integer;
   arrEdge     MDSYS.SDO_LIST_TYPE;
   arrLyr_ID   MDSYS.SDO_NUMBER_ARRAY;
   arrTG_ID    MDSYS.SDO_NUMBER_ARRAY;
   arrTOPO_ID  MDSYS.SDO_NUMBER_ARRAY;
   updKeepFace NUMBER;
   updDelFace  NUMBER;
   v_tg_layer_id NUMBER;
   v_Edge_ID   NUMBER;
   v_Keep_Face NUMBER;
   v_del_face  NUMBER;
   --
   tmpsql    VARCHAR2(1000);
   tmpStr  VARCHAR2(1000);
   atrStr  VARCHAR2(1000);
   TYPE RefCursorType IS REF CURSOR;
   AtrCursor     RefCursorType;
   --
   nodeCursor RefCursorType;
   curNode     NUMBER;
   v_code         NUMBER;
   v_errm         VARCHAR2(1000);
  BEGIN
   -- Get zero level "face" feature table's tg_layer_id from the
   -- topology metadata
   SELECT tg_layer_id INTO v_tg_layer_id
     FROM user_sdo_topo_info
    WHERE table_name = ftrTable;
  -- Get a list of unprocessed egdes from the input table
   psql := 'SELECT Edge_id '
         || ' FROM ' || inpTable
         || ' WHERE done IS NULL';
   --DBMS_OUTPUT.PUT_LINE('psql = ' || psql);
   EXECUTE IMMEDIATE psql BULK COLLECT INTO arrEdge;
   -- Count Topo Geometry Layers..... should be only one layer (FACE)
   --DBMS_OUTPUT.PUT_LINE(arrEdge.LAST);
   -- Loop through each unprocessed edge in the input table
   FOR i IN arrEdge.FIRST..arrEdge.LAST
   LOOP
      v_Edge_ID := arrEdge(i);
      -- get relevant face ids
      psql1 := 'SELECT keep_face, not_keep FROM ' || inpTable || ' WHERE edge_id = :1';
      EXECUTE IMMEDIATE psql1 INTO v_Keep_Face, v_del_face USING v_Edge_ID;
      DBMS_OUTPUT.PUT_LINE('Processing Edge_ID: ' || v_Edge_ID);
      --DBMS_OUTPUT.PUT_LINE('i: ' || i || ' Edg: ' || v_Edge_ID || ' Fc1: ' || v_keep_face || ' Fc2: ' || v_del_face);
      -- Get relation$ info for both faces
      psql1 := 'SELECT TG_LAYER_ID, TG_ID, TOPO_ID '
             ||'  FROM ' || topology || '_RELATION$ '
             ||' WHERE TOPO_ID IN (:1, :2) AND tg_layer_id = :3';
      --DBMS_OUTPUT.PUT_LINE('psql1: ' || psql1);
      DBMS_OUTPUT.PUT_LINE('v_keep_face: ' || v_keep_face || ' v_del_face: ' || v_del_face || ' v_tg_layer_id: ' || v_tg_layer_id);
      EXECUTE IMMEDIATE psql1 BULK COLLECT INTO arrLyr_ID, arrTG_ID, arrTOPO_ID USING v_keep_face, v_del_face, v_tg_layer_id;
      -- Check to make sure both faces still exist in relation$, and if so,
          -- create duplicate pointers, so that face a and face b in face$ both
          -- point to both face a and b in the primitive table (this makes the
          -- values on both sides of the edge the same, allowing us to delete
          -- the edge.
          -- then remove the unecessary nodes you left behind
          -- Oracle picks which face id to keep after the edge is deleted,
          -- so if it keeps the one we want to drop, we copy the attributes
          -- from the keep face to the not keep face, and then drop the
          -- feature record for the face that oracle dropped
          -- to remove the duplicate attribute record from the feature table
      -- If both faces do not exist, then mark this edge with an 'X", skip it,
      -- and try to process the next edge.
      IF arrTG_ID.COUNT = 2 THEN
         sql1 := 'INSERT INTO ' || topology || '_RELATION$ VALUES (:1, :2, :3, :4, :5)';
         EXECUTE IMMEDIATE sql1 USING arrLyr_ID(1), arrTG_ID(1), arrTOPO_ID(2), 3, '';
         --DBMS_OUTPUT.PUT_LINE('sql1: ' || sql1);
         --DBMS_OUTPUT.PUT_LINE('arrLyr_ID(1): ' || arrLyr_ID(1) || ' arrTG_ID(1): ' || arrTG_ID(1) || ' arrTOPO_ID(2): ' || arrTOPO_ID(2) || ' 3 BLANK');
         sql2 := 'INSERT INTO ' || topology || '_RELATION$ VALUES (:1, :2, :3, :4, :5)';
         EXECUTE IMMEDIATE sql2 USING arrLyr_ID(1), arrTG_ID(2), arrTOPO_ID(1), 3, '';
         --DBMS_OUTPUT.PUT_LINE('sql2: ' || sql2);
         --DBMS_OUTPUT.PUT_LINE('arrLyr_ID(1): ' || arrLyr_ID(1) || ' arrTG_ID(2): ' || arrTG_ID(2) || ' arrTOPO_ID(1): ' || arrTOPO_ID(1) || ' 3 BLANK');
         BEGIN
            SDO_TOPO_MAP.REMOVE_EDGE(topology, v_Edge_ID);
         EXCEPTION
            WHEN OTHERS THEN
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               DBMS_OUTPUT.PUT_LINE('Unable to remove edge... skipping record ' || v_code || ': ' || v_errm);
               ROLLBACK;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'X', v_edge_id;
               COMMIT;
               GOTO CONT;
         END;
         tmpSql := 'SELECT node_id FROM ' || topology || '_node$ WHERE edge_id = 0';
         OPEN nodeCursor FOR tmpSql;
         LOOP
            FETCH nodeCursor INTO curNode;
            EXIT WHEN nodeCursor%NOTFOUND;
            BEGIN
              --DBMS_OUTPUT.PUT_LINE('CurNode: ' || curNode);
              SDO_TOPO_MAP.REMOVE_NODE(topology, curNode);
            EXCEPTION
              WHEN OTHERS THEN
                 v_code := SQLCODE;
                 v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
                 DBMS_OUTPUT.PUT_LINE('Unable to remove CurNode: ' || curNode || ' ErrCode/Msg ' || v_code || ': ' || v_errm);
            END;
         END LOOP;
         CLOSE nodeCursor;
         --SK 03/24/2010
         /*
         --It was our intention to remove the current NOT_KEEP FACE.
         --    Since we are deleting the NOT_KEEP FACE, it should not be listed
         --    as KEEP_FACE in any of the unprocessed records
         --    Mark them as Q?
            sql2 := 'UPDATE ' || inpTable
                    || ' SET done = ''Q?'''
                    || ' WHERE done IS NULL '
                    || '   AND edge_id <> :1 AND keep_face = :2 ';
            --DBMS_OUTPUT.PUT_LINE('Updating face_id.... ' || sql2);
            EXECUTE IMMEDIATE sql2 USING v_edge_id, v_Del_Face;
         --Also we are deleting the NOT_KEEP Face, mark any unprocessed records
         --    with the same NOT_KEEP FACE as completed (YD - Yes Duplicate)
            sql2 := 'UPDATE ' || inpTable
                    || ' SET done = ''YD'''
                    || ' WHERE done IS NULL '
                    || '   AND edge_id <> :1 AND not_keep = :2 ';
            --DBMS_OUTPUT.PUT_LINE('Updating face_id.... ' || sql2);
            EXECUTE IMMEDIATE sql2 USING v_edge_id, v_Del_Face;
         */
         -- END SK 03/24/2010
         -- Drop the correct face. it might not be the given one.
         -- Select from face$ to see which face is left.
         -- There should be only one face
         sql1 := 'SELECT face_id FROM ' || topology || '_FACE$ WHERE face_id IN (:1, :2)';
         EXECUTE IMMEDIATE sql1 INTO updKeepFace USING v_Keep_Face, v_Del_Face;
         -- Now that we know updKeepFace the other one should be updDelFace
         If updKeepFace = v_Keep_face Then
            updDelFace := v_Del_Face;
         Elsif updKeepFace = v_Del_Face Then
            updDelFace := v_Keep_Face;
         Else
            -- Invalid condition
            DBMS_OUTPUT.PUT_LINE('***** Reached invalid Else condition, please verify *****');
         End If;
         -- reassign attributes to the face Oracle kept if necessary
         IF updKeepFace <> v_Keep_Face THEN
            DBMS_OUTPUT.PUT_LINE('Topology decided to remove the "keep_face": ' || v_Keep_Face || ' and keep the "not_keep" face: ' || updKeepFace);
            DBMS_OUTPUT.PUT_LINE('........ will process accordingly!');
            --DBMS_OUTPUT.PUT_LINE('updKeepFace = ' || updKeepFace || ' oldKeepFace = ' || arrKeepFace(i));
            -- Making an assumption that all REQUIRED attribute columns are of type VARCHAR2
               -- Not sure this assumption is a good one, but it is true for
               -- ACS09 - maybe we need a specific list (Stephanie)
            tmpSql := 'SELECT COLUMN_NAME FROM USER_TAB_COLUMNS WHERE TABLE_NAME = ''' || ftrTable || ''' AND DATA_TYPE = ''VARCHAR2'' ';
            --DBMS_OUTPUT.PUT_LINE(tmpsql);
            OPEN AtrCursor FOR tmpsql;
            atrStr := NULL;
            LOOP
               FETCH AtrCursor INTO tmpStr;
               EXIT WHEN AtrCursor%NOTFOUND;
               IF atrStr IS NULL THEN
                  atrStr := tmpStr;
               ELSE
                  atrStr := atrStr || ', ' || tmpStr;
               END IF;
            END LOOP;
            CLOSE AtrCursor;
            --DBMS_OUTPUT.PUT_LINE(atrStr);
            sql1 := 'UPDATE ' || ftrTable
                     || ' SET (' || atrStr || ') = (SELECT ' || atrStr || ' FROM ' || ftrTable
                     || ' WHERE face_id = :1) WHERE face_id = :2';
            execute immediate sql1 using v_Keep_Face, v_Del_Face;
         /*   Let's expand this logic and handle this after we update the done flag to Y
            --SK3
            -- Update any upprocessed records with the correct face_id
            sql2 := 'UPDATE ' || inpTable
                    || ' SET keep_face = ' || v_Del_Face
                    || ' WHERE done IS NULL AND keep_face = :1 '
                    || '   AND edge_id <> :2';
            DBMS_OUTPUT.PUT_LINE('Updating face_id.... ' || sql2);
            EXECUTE IMMEDIATE sql2 USING v_Keep_Face, v_edge_id;
         */
         /*  20100330  Dont need this.  Handled above
            updDelFace := v_Keep_Face;
         ELSE
            --DBMS_OUTPUT.PUT_LINE('updKeepFace = arrKeepFace');
            --DBMS_OUTPUT.PUT_LINE('updKeepFace = ' || updKeepFace || ' KeepFace = ' || arrKeepFace(i));
            updDelFace := v_Del_Face;
         */
         END IF;
         -- remove the redundant feature record you created
         EXECUTE IMMEDIATE 'DELETE FROM ' || ftrTable || ' WHERE face_id = ' || updDelFace;
         -- null out face mesurements in the feature table since they no
         -- longer match the geometry
         EXECUTE IMMEDIATE 'UPDATE ' || ftrTable || ' f '
                          --|| ' SET sdogeometry= f.topogeom.get_geometry(), '
                          || ' SET sdogeometry= NULL, '
                          || '     areatotal = NULL, '
                          || '     perimeter = NULL, '
                          || '     llx = NULL, '
                          || '     lly = NULL, '
                          || '     urx = NULL, '
                          || '     ury = NULL, '
                          || '     pa_ratio = NULL '
                          --|| '     mbr = NULL '
                          || ' WHERE face_id = ' || updKeepFace;
         -- mark edge in the input table complete
         sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
         EXECUTE IMMEDIATE sql3 USING 'Y', v_edge_id;
         /*
         When processing Edge1 Oracle deleted either A or B
         updKeepFace and updDelFace will have the right values
         If updDelFace exists in any of the unprocessed Keep_Face or Not_Keep,
             replace that value with updKeepFace
         Example:
         Edge ID    Keep_Face    Not_Keep
         EDGE1      A            B
         EDGE2      A            B
         EDGE3      A            C
         EDGE4      D            A
         If Oracle kept B and Deleted A
         Edge ID    Keep_Face    Not_Keep
         EDGE1      A            B
         EDGE2      B            B  update keep_face from A to B
         EDGE3      B            C  update keep_face from A to B
         EDGE4      D            B  update not_keep from A to B
         If Oracle kept A and Deleted B
         Edge ID    Keep_Face    Not_Keep
         EDGE1      A            B
         EDGE2      A            A  update Not_keep from B to A
         EDGE3      A            C  No changes
         EDGE4      D            A  No changes
         */
            -- Update any upprocessed records with the correct face_id
            sql2 := 'UPDATE ' || inpTable
                    || ' SET keep_face = ' || updKeepFace
                    || ' WHERE done IS NULL AND keep_face = :1 '
                    || '   AND edge_id <> :2';
            DBMS_OUTPUT.PUT_LINE('Updating face_id.... ' || sql2);
            EXECUTE IMMEDIATE sql2 USING updDelFace, v_edge_id;
            -- Update any upprocessed records with the correct face_id
            sql2 := 'UPDATE ' || inpTable
                    || ' SET not_keep = ' || updKeepFace
                    || ' WHERE done IS NULL AND not_keep = :1 '
                    || '   AND edge_id <> :2';
            DBMS_OUTPUT.PUT_LINE('Updating face_id.... ' || sql2);
            EXECUTE IMMEDIATE sql2 USING updDelFace, v_edge_id;
         COMMIT;
      ELSIF (arrTG_ID.COUNT = 1) AND (v_Keep_Face = v_del_face) THEN
         -- Keep Face is same as delete face; Just delete the edge
         BEGIN
            SDO_TOPO_MAP.REMOVE_EDGE(topology, v_Edge_ID);
            sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
            EXECUTE IMMEDIATE sql3 USING 'Y', v_edge_id;
            COMMIT;
         EXCEPTION
            WHEN OTHERS THEN
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               DBMS_OUTPUT.PUT_LINE('Unable to remove edge... skipping record ' || v_code || ': ' || v_errm);
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'X', v_edge_id;
               COMMIT;
               GOTO CONT;
         END;
      ELSE
         -- mark edge in the input table as not processable
         sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
         EXECUTE IMMEDIATE sql3 USING 'X', v_edge_id;
         COMMIT;
         DBMS_OUTPUT.PUT_LINE('********************************************* ERROR *********************************************');
         DBMS_OUTPUT.PUT_LINE('TG_ID.COUNT is not equal to 2: ' || arrtg_id.count);
         --DBMS_OUTPUT.PUT_LINE('There are two possible causes for this error.');
         DBMS_OUTPUT.PUT_LINE('  1) DELETE_EDGES requires there be one and only one feature table in the topology.');
         DBMS_OUTPUT.PUT_LINE('     Please verify if any additional topo geometry layers have to be removed and rerun this program' );
         --DBMS_OUTPUT.PUT_LINE('  2) This could also mean that the keep and not_keep face ids are the same.  They must be different.' );
         DBMS_OUTPUT.PUT_LINE('********************************************* ERROR *********************************************');
      END IF;
      <<CONT>>
      arrLyr_ID.DELETE;
      arrTG_ID.DELETE;
      arrTOPO_ID.DELETE;
      --null;
   END LOOP;
   arrEdge.DELETE;
   DBMS_OUTPUT.PUT_LINE('');
   DBMS_OUTPUT.PUT_LINE('==============================================================================');
   DBMS_OUTPUT.PUT_LINE('Done processing all records from ' || inpTable);
   DBMS_OUTPUT.PUT_LINE('Please update columns AREATOTAL, PERIMETER etc in feature table ' || ftrTable);
   DBMS_OUTPUT.PUT_LINE('==============================================================================');
   commit;
END DELETE_EDGES;
PROCEDURE DELETE_EDGES_2(topology VARCHAR2,
ftrTable VARCHAR2,
inpTable VARCHAR2,
valFlg VARCHAR2 default 'FALSE') AS
/*  **** THIS PROCEDURE USES EXPLICIT TOPOMAP TO REMOVE EDGES + Utilizes
    Oracle Topo Constructors ****
   NAME:       DELETE_EDGES 2
   PURPOSE:    Deletes the edges identified in the input table from the
               topology to merge faces.
               The input table identifies which face attributes to save
               from the specified zero-level feature table.
   INPUT TABLE STRUCTURE AND REQUIREMENTS:
               Topology must have one and only one feature table.  Before
               and after running the delete_edges routine, this feature
               table should have a one-to-one face$ to feature relationship.
               Measurements stored in the feature table will be out of sync
               after this program runs, recalculate them before moving on.
              (uses the results of the identify small polygons routine,
              called "ELIMINATE_SMALL_POLYS")
              input table structure...
              EDGE_ID,
              LEFT_FACE_ID,
              RIGHT_FACE_ID,
              KEEP_FACE, (face id whose attributes we will keep)
              NOT_KEEP, (face id whose attributes will be lost)
              AREATOTAL,
              MAX_AREA,
              DONE     (to track what has been processed)
              If the faces to merge have different left and right state
              values... WHAT DOES IT DO?
              This program will create obsolete nodes that should be removed
              prior to line simplification.
     Output:
         Tracks progress by using the "DONE" field in the input table
         'Y' = completed sucessfully,
         '??' = could not complete,
         'QA" = did something, but needs review by a human,
         'XSelInpTbl' = There are duplicate edge ids in the input table
         'XInsRel$' = Error inserting records into Rel$ table (possible
                      unique constraint error)
         'XF' = Unable to select updated keep Face from Face$ table
         NULL = unprocessed.
     Author: Sreeni Karpurapu
     Recent Revision: 08/09/10 - Salman - Added Topo Constructors
                                          to Update the Relation$.
               Replaced Insert statements with Update statements for Relation$.
     20100812: Added Error reporting to log file (in addition to table)
*/
   psql        VARCHAR2(4000);
   psql1       VARCHAR2(4000);
   psql2       VARCHAR2(4000);
   sql1        VARCHAR2(4000);
   sql2        VARCHAR2(4000);
   sql3        VARCHAR2(4000);
   ctr         pls_integer;
   arrEdge     MDSYS.SDO_LIST_TYPE;
   arrLyr_ID   MDSYS.SDO_NUMBER_ARRAY;
   arrTG_ID    MDSYS.SDO_NUMBER_ARRAY;
   arrTOPO_ID  MDSYS.SDO_NUMBER_ARRAY;
   updKeepFace NUMBER;
   updDelFace  NUMBER;
   v_tg_layer_id NUMBER;
   v_Edge_ID   NUMBER;
   v_Keep_Face NUMBER;
   v_del_face  NUMBER;
   --
   tmpsql      VARCHAR2(1000);
   tmpStr      VARCHAR2(1000);
   atrStr      VARCHAR2(1000);
   TYPE RefCursorType IS REF CURSOR;
   AtrCursor   RefCursorType;
   --
   nodeCursor  RefCursorType;
   curNode     NUMBER;
   v_code      NUMBER;
   v_errm      VARCHAR2(1000);
   TPLGY       VARCHAR2(50) := NULL;
   TPMP        VARCHAR2(50) := topology || '_TOPOMAP';
   DELTA       NUMBER(4,3)  := 0.001;
   xMin        NUMBER;
   yMin        NUMBER;
   xMax        NUMBER;
   yMax        NUMBER;
   x1Min       NUMBER;
   y1Min       NUMBER;
   x1Max       NUMBER;
   y1Max       NUMBER;
   x2Min       NUMBER;
   y2Min       NUMBER;
   x2Max       NUMBER;
   y2Max       NUMBER;
   res         VARCHAR2(10);
   --RecCtr         NUMBER := 0;
   vErrorCounter NUMBER := 0;
  BEGIN
   -- Get zero level "face" feature table's tg_layer_id from the
   -- topology metadata
   SELECT tg_layer_id INTO v_tg_layer_id
     FROM user_sdo_topo_info
    WHERE table_name = ftrTable;
  -- Get a list of unprocessed egdes from the input table
   psql := 'SELECT Edge_id '
         || ' FROM ' || inpTable
         || ' WHERE done IS NULL';
   --DBMS_OUTPUT.PUT_LINE('psql = ' || psql);
   EXECUTE IMMEDIATE psql BULK COLLECT INTO arrEdge;
   -- Count Topo Geometry Layers..... should be only one layer (FACE)
   --DBMS_OUTPUT.PUT_LINE(arrEdge.LAST);
   SDO_TOPO_MAP.CREATE_TOPO_MAP(topology, TPMP);
   -- Loop through each unprocessed edge in the input table
   FOR i IN arrEdge.FIRST..arrEdge.LAST
   LOOP
      v_Edge_ID := arrEdge(i);
      --DBMS_OUTPUT.PUT_LINE('Processing Edge_ID: ' || v_Edge_ID);
      --DBMS_OUTPUT.PUT_LINE('i: ' || i || ' Edg: ' || v_Edge_ID || ' Fc1: ' || v_keep_face || ' Fc2: ' || v_del_face);
      -- get relevant face ids
      -- SK 04/06/2010 Handle "fetch returns more than requested number of rows" exception
      BEGIN
         psql1 := 'SELECT keep_face, not_keep FROM ' || inpTable || ' WHERE edge_id = :1';
         EXECUTE IMMEDIATE psql1 INTO v_Keep_Face, v_del_face USING v_Edge_ID;
      EXCEPTION
         WHEN OTHERS THEN
            v_code := SQLCODE;
            v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
            vErrorCounter := vErrorCounter + 1;
            --DBMS_OUTPUT.PUT_LINE('Select does not return one record ' || v_code || ': ' || v_errm);
            sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
            EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
            COMMIT;
            sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
            EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
            COMMIT;
            sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
            EXECUTE IMMEDIATE sql3 USING 'XSelInpTbl', v_edge_id;
            COMMIT;
            GOTO CONT;
      END;
      -- Get relation$ info for both faces
      psql1 := 'SELECT TG_LAYER_ID, TG_ID, TOPO_ID '
             ||'  FROM ' || topology || '_RELATION$ '
             ||' WHERE TOPO_ID IN (:1, :2) AND tg_layer_id = :3';
      --DBMS_OUTPUT.PUT_LINE('psql1: ' || psql1);
      --DBMS_OUTPUT.PUT_LINE('v_keep_face: ' || v_keep_face || ' v_del_face: ' || v_del_face || ' v_tg_layer_id: ' || v_tg_layer_id);
      EXECUTE IMMEDIATE psql1 BULK COLLECT INTO arrLyr_ID, arrTG_ID, arrTOPO_ID USING v_keep_face, v_del_face, v_tg_layer_id;
      IF arrTG_ID.COUNT = 2 THEN
         -- GET FACE MBRs
         get_face_mbr(topology, v_Keep_Face, x1Min, y1Min, x1Max, y1Max);
         get_face_mbr(topology, v_Del_Face, x2Min, y2Min, x2Max, y2Max);
         -- Compute the MBR of the two faces and add a buffer (DELTA)
         IF x1Min < x2Min THEN
            xMin := x1Min - DELTA;
         ELSE
            xMin := x2Min - DELTA;
         END IF;
         IF y1Min < y2Min THEN
            yMin := y1Min - DELTA;
         ELSE
            yMin := y2Min - DELTA;
         END IF;
         IF x1Max > x2Max THEN
            xMax := x1Max + DELTA;
         ELSE
            xMax := x2Max + DELTA;
         END IF;
         IF y1Max > y2Max THEN
            yMax := y1Max + DELTA;
         ELSE
            yMax := y2Max + DELTA;
         END IF;
         BEGIN
            res := SDO_TOPO_MAP.LOAD_TOPO_MAP(TPMP, xMin, yMin, xMax, yMax, 'TRUE');
         EXCEPTION
            WHEN OTHERS THEN
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               vErrorCounter := vErrorCounter + 1;
               --DBMS_OUTPUT.PUT_LINE('Unable to Load Topo Map ' || v_code || ': ' || v_errm);
               SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'X Unable to load topo map', v_edge_id;
               COMMIT;
               GOTO CONT;
         END;
      -- Check to make sure both faces still exist in relation$, and if so,
          -- create duplicate pointers, so that face a and face b in face$ both
          -- point to both face a and b in the primitive table (this makes the
          -- values on both sides of the edge the same, allowing us to delete
          -- the edge.
          -- then remove the unecessary nodes you left behind
          -- Oracle picks which face id to keep after the edge is deleted,
          -- so if it keeps the one we want to drop, we copy the attributes
          -- from the keep face to the not keep face, and then drop the
          -- feature record for the face that oracle dropped
          -- to remove the duplicate attribute record from the feature table
      -- If both faces do not exist, then mark this edge with an 'X", skip it,
      -- and try to process the next edge.
      -- 08/09/10 Update Topology using Topo Constructors
         BEGIN
            sql1 := 'UPDATE '||ftrTable||' b SET b.topogeom = SDO_TOPO_GEOMETRY(:1,:2,:3,SDO_TOPO_OBJECT_ARRAY (SDO_TOPO_OBJECT (:4,:5)),null)
                     WHERE b.face_id = :6';
            EXECUTE IMMEDIATE sql1 USING topology, 3, arrLyr_ID(1), arrTOPO_ID(1), 3, arrTOPO_ID(2);
            COMMIT;
            --DBMS_OUTPUT.PUT_LINE('sql1: ' || sql1);
            --DBMS_OUTPUT.PUT_LINE('arrLyr_ID(1): ' || arrLyr_ID(1) || ' arrTG_ID(1): ' || arrTG_ID(1) || ' arrTOPO_ID(2): ' || arrTOPO_ID(2) || ' 3 BLANK');
         EXCEPTION
            WHEN OTHERS THEN
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               vErrorCounter := vErrorCounter + 1;
               --DBMS_OUTPUT.PUT_LINE('Error while inserting into Rel$ table ' || v_code || ': ' || v_errm);
               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'XInsRel$', v_edge_id;
               COMMIT;
               GOTO CONT;
         END;
         BEGIN
            sql2 := 'UPDATE '||ftrTable||' b SET b.topogeom = SDO_TOPO_GEOMETRY(:1,:2,:3,SDO_TOPO_OBJECT_ARRAY (SDO_TOPO_OBJECT (:4,:5)),null)
                     WHERE b.face_id = :6';
            EXECUTE IMMEDIATE sql2 USING topology, 3, arrLyr_ID(1), arrTOPO_ID(2), 3, arrTOPO_ID(1);
            COMMIT;
         EXCEPTION
            WHEN OTHERS THEN
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               vErrorCounter := vErrorCounter + 1;
               --DBMS_OUTPUT.PUT_LINE('Error while inserting into Rel$ table ' || v_code || ': ' || v_errm);
               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'XInsRel$', v_edge_id;
               COMMIT;
               GOTO CONT;
         END;
         --DBMS_OUTPUT.PUT_LINE('sql2: ' || sql2);
         --DBMS_OUTPUT.PUT_LINE('arrLyr_ID(1): ' || arrLyr_ID(1) || ' arrTG_ID(2): ' || arrTG_ID(2) || ' arrTOPO_ID(1): ' || arrTOPO_ID(1) || ' 3 BLANK');
         BEGIN
            SDO_TOPO_MAP.REMOVE_EDGE(TPLGY, v_Edge_ID);
            IF valFlg = 'TRUE' THEN
               RES := SDO_TOPO_MAP.VALIDATE_TOPO_MAP(TPMP);
               IF RES <> 'TRUE' THEN
                  ROLLBACK;
                  SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
                  sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
                  EXECUTE IMMEDIATE sql3 USING 'InvldTPMP', v_edge_id;
                  COMMIT;
                  GOTO CONT;
               END IF;
            END IF;
         EXCEPTION
            WHEN OTHERS THEN
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               vErrorCounter := vErrorCounter + 1;
              --DBMS_OUTPUT.PUT_LINE('Unable to remove edge... skipping record ' || v_code || ': ' || v_errm);
               ROLLBACK;
               SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'X Unable to remove edge', v_edge_id;
               COMMIT;
               GOTO CONT;
         END;
         /*
         tmpSql := 'SELECT node_id FROM ' || topology || '_node$ WHERE edge_id = 0';
         OPEN nodeCursor FOR tmpSql;
         LOOP
            FETCH nodeCursor INTO curNode;
            EXIT WHEN nodeCursor%NOTFOUND;
            BEGIN
              --DBMS_OUTPUT.PUT_LINE('CurNode: ' || curNode);
              SDO_TOPO_MAP.REMOVE_NODE(TPLGY, curNode);
            EXCEPTION
              WHEN OTHERS THEN
                 v_code := SQLCODE;
                 v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
                 DBMS_OUTPUT.PUT_LINE('Unable to remove CurNode: ' || curNode || ' ErrCode/Msg ' || v_code || ': ' || v_errm);
            END;
         END LOOP;
         CLOSE nodeCursor;
         */
         SDO_TOPO_MAP.UPDATE_TOPO_MAP;
         SDO_TOPO_MAP.COMMIT_TOPO_MAP;
         -- Drop the correct face. it might not be the given one.
         -- Select from face$ to see which face is left.
         -- There should be only one face
         -- SK 04/06/2010
         -- Sometimes deleting an edge does not automatically result in deleting one of the faces
         Begin
            sql1 := 'SELECT face_id FROM ' || topology || '_FACE$ WHERE face_id IN (:1, :2)';
            EXECUTE IMMEDIATE sql1 INTO updKeepFace USING v_Keep_Face, v_Del_Face;
         EXCEPTION
            WHEN OTHERS THEN
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               vErrorCounter := vErrorCounter + 1;
               --DBMS_OUTPUT.PUT_LINE('Error while selecting updKeepFace ' || v_code || ': ' || v_errm);
               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'XF', v_edge_id;
               COMMIT;
               GOTO CONT;
         END;
         -- Now that we know updKeepFace the other one should be updDelFace
         If updKeepFace = v_Keep_face Then
            updDelFace := v_Del_Face;
         Elsif updKeepFace = v_Del_Face Then
            updDelFace := v_Keep_Face;
         Else
            NULL;
            -- Invalid condition
            --DBMS_OUTPUT.PUT_LINE('***** Reached invalid Else condition, please verify *****');
         End If;
         -- reassign attributes to the face Oracle kept if necessary
         IF updKeepFace <> v_Keep_Face THEN
            --DBMS_OUTPUT.PUT_LINE('Topology decided to remove the "keep_face": ' || v_Keep_Face || ' and keep the "not_keep" face: ' || updKeepFace);
            --DBMS_OUTPUT.PUT_LINE('........ will process accordingly!');
            --DBMS_OUTPUT.PUT_LINE('updKeepFace = ' || updKeepFace || ' oldKeepFace = ' || arrKeepFace(i));
            -- Making an assumption that all REQUIRED attribute columns are of type VARCHAR2
               -- Not sure this assumption is a good one, but it is true for
               -- ACS09 - maybe we need a specific list (Stephanie)
            tmpSql := 'SELECT COLUMN_NAME FROM USER_TAB_COLUMNS WHERE TABLE_NAME = ''' || ftrTable || ''' AND DATA_TYPE = ''VARCHAR2'' ';
            --DBMS_OUTPUT.PUT_LINE(tmpsql);
            OPEN AtrCursor FOR tmpsql;
            atrStr := NULL;
            LOOP
               FETCH AtrCursor INTO tmpStr;
               EXIT WHEN AtrCursor%NOTFOUND;
               IF atrStr IS NULL THEN
                  atrStr := tmpStr;
               ELSE
                  atrStr := atrStr || ', ' || tmpStr;
               END IF;
            END LOOP;
            CLOSE AtrCursor;
            --DBMS_OUTPUT.PUT_LINE(atrStr);
            sql1 := 'UPDATE ' || ftrTable
                     || ' SET (' || atrStr || ') = (SELECT ' || atrStr || ' FROM ' || ftrTable
                     || ' WHERE face_id = :1) WHERE face_id = :2';
            execute immediate sql1 using v_Keep_Face, v_Del_Face;
         /*   Let's expand this logic and handle this after we update the done flag to Y
            --SK3
            -- Update any upprocessed records with the correct face_id
            sql2 := 'UPDATE ' || inpTable
                    || ' SET keep_face = ' || v_Del_Face
                    || ' WHERE done IS NULL AND keep_face = :1 '
                    || '   AND edge_id <> :2';
            DBMS_OUTPUT.PUT_LINE('Updating face_id.... ' || sql2);
            EXECUTE IMMEDIATE sql2 USING v_Keep_Face, v_edge_id;
         */
         /*  20100330  Dont need this.  Handled above
            updDelFace := v_Keep_Face;
         ELSE
            --DBMS_OUTPUT.PUT_LINE('updKeepFace = arrKeepFace');
            --DBMS_OUTPUT.PUT_LINE('updKeepFace = ' || updKeepFace || ' KeepFace = ' || arrKeepFace(i));
            updDelFace := v_Del_Face;
         */
         END IF;
         -- remove the redundant feature record you created
         EXECUTE IMMEDIATE 'DELETE FROM ' || ftrTable || ' WHERE face_id = ' || updDelFace;
         -- null out face mesurements in the feature table since they no
         -- longer match the geometry
         EXECUTE IMMEDIATE 'UPDATE ' || ftrTable || ' f '
                          --|| ' SET sdogeometry= f.topogeom.get_geometry(), '
                          || ' SET sdogeometry= NULL, '
                          || '     areatotal = NULL, '
                          || '     perimeter = NULL, '
                          || '     llx = NULL, '
                          || '     lly = NULL, '
                          || '     urx = NULL, '
                          || '     ury = NULL, '
                          || '     pa_ratio = NULL '
                          --|| '     mbr = NULL '
                          || ' WHERE face_id = ' || updKeepFace;
         -- mark edge in the input table complete
         sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
         EXECUTE IMMEDIATE sql3 USING 'Y', v_edge_id;
         /*
         When processing Edge1 Oracle deleted either A or B
         updKeepFace and updDelFace will have the right values
         If updDelFace exists in any of the unprocessed Keep_Face or Not_Keep,
             replace that value with updKeepFace
         Example:
         Edge ID    Keep_Face    Not_Keep
         EDGE1      A            B
         EDGE2      A            B
         EDGE3      A            C
         EDGE4      D            A
         If Oracle kept B and Deleted A
         Edge ID    Keep_Face    Not_Keep
         EDGE1      A            B
         EDGE2      B            B  update keep_face from A to B
         EDGE3      B            C  update keep_face from A to B
         EDGE4      D            B  update not_keep from A to B
         If Oracle kept A and Deleted B
         Edge ID    Keep_Face    Not_Keep
         EDGE1      A            B
         EDGE2      A            A  update Not_keep from B to A
         EDGE3      A            C  No changes
         EDGE4      D            A  No changes
         */
            -- Update any upprocessed records with the correct face_id
            sql2 := 'UPDATE ' || inpTable
                    || ' SET keep_face = ' || updKeepFace
                    || ' WHERE done IS NULL AND keep_face = :1 '
                    || '   AND edge_id <> :2';
            --DBMS_OUTPUT.PUT_LINE('Updating face_id.... ' || sql2 || ' updDelFace: ' || updDelFace);
            EXECUTE IMMEDIATE sql2 USING updDelFace, v_edge_id;
            -- Update any upprocessed records with the correct face_id
            sql2 := 'UPDATE ' || inpTable
                    || ' SET not_keep = ' || updKeepFace
                    || ' WHERE done IS NULL AND not_keep = :1 '
                    || '   AND edge_id <> :2';
            --DBMS_OUTPUT.PUT_LINE('Updating face_id.... ' || sql2 || ' updDelFace: ' || updDelFace);
            EXECUTE IMMEDIATE sql2 USING updDelFace, v_edge_id;
         COMMIT;
      ELSIF (arrTG_ID.COUNT = 1) AND (v_Keep_Face = v_del_face) THEN
         -- Keep Face is same as delete face; Just delete the edge
         get_face_mbr(topology, v_Keep_Face, xMin, yMin, xMax, yMax);
         xMin := xMin - DELTA;
         yMin := yMin - DELTA;
         xMax := xMax + DELTA;
         yMax := yMax + DELTA;
         BEGIN
            res := SDO_TOPO_MAP.LOAD_TOPO_MAP(TPMP, xMin, yMin, xMax, yMax, 'TRUE');
         EXCEPTION
            WHEN OTHERS THEN
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               vErrorCounter := vErrorCounter + 1;
               --DBMS_OUTPUT.PUT_LINE('Unable to Load Topo Map ' || v_code || ': ' || v_errm);
               SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'X unable to load topo map', v_edge_id;
               COMMIT;
               GOTO CONT;
         END;
         BEGIN
            SDO_TOPO_MAP.REMOVE_EDGE(TPLGY, v_Edge_ID);
            IF valFlg = 'TRUE' THEN
               RES := SDO_TOPO_MAP.VALIDATE_TOPO_MAP(TPMP);
               IF RES <> 'TRUE' THEN
                  ROLLBACK;
                  SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
                  sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
                  EXECUTE IMMEDIATE sql3 USING 'InvldTPMP', v_edge_id;
                  COMMIT;
                  GOTO CONT;
               END IF;
            END IF;
            SDO_TOPO_MAP.UPDATE_TOPO_MAP;
            SDO_TOPO_MAP.COMMIT_TOPO_MAP;
            sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
            EXECUTE IMMEDIATE sql3 USING 'Y', v_edge_id;
            COMMIT;
         EXCEPTION
            WHEN OTHERS THEN
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               --DBMS_OUTPUT.PUT_LINE('Unable to remove edge... skipping record ' || v_code || ': ' || v_errm);
               SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
               vErrorCounter := vErrorCounter + 1;
               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'X unable to remove edge', v_edge_id;
               COMMIT;
               GOTO CONT;
         END;
      ELSE
         -- mark edge in the input table as not processable
         sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
         EXECUTE IMMEDIATE sql3 USING 'X', v_edge_id;
         COMMIT;
         --DBMS_OUTPUT.PUT_LINE('********************************************* ERROR *********************************************');
         --DBMS_OUTPUT.PUT_LINE('TG_ID.COUNT is not equal to 2: ' || arrtg_id.count);
         --DBMS_OUTPUT.PUT_LINE('There are two possible causes for this error.');
         --DBMS_OUTPUT.PUT_LINE('  1) DELETE_EDGES requires there be one and only one feature table in the topology.');
         --DBMS_OUTPUT.PUT_LINE('     Please verify if any additional topo geometry layers have to be removed and rerun this program' );
         --DBMS_OUTPUT.PUT_LINE('  2) This could also mean that the keep and not_keep face ids are the same.  They must be different.' );
         --DBMS_OUTPUT.PUT_LINE('********************************************* ERROR *********************************************');
      END IF;
      <<CONT>>
      SDO_TOPO_MAP.CLEAR_TOPO_MAP(TPMP);
      BEGIN
         arrLyr_ID.DELETE;
         arrTG_ID.DELETE;
         arrTOPO_ID.DELETE;
      EXCEPTION
         WHEN OTHERS THEN
             NULL;
      END;
      --null;
      /*
      RecCtr := RecCtr + 1;
      if (mod(RecCtr, 500) = 0) then
         DBMS_OUTPUT.PUT_LINE('Processed : ' || RecCtr || ' records....');
      end if;
      */
   END LOOP;
   --DBMS_OUTPUT.PUT_LINE('Total Number of Records Processed : ' || RecCtr);
   arrEdge.DELETE;
   SDO_TOPO_MAP.DROP_TOPO_MAP(TPMP);
   DBMS_OUTPUT.PUT_LINE('');
   DBMS_OUTPUT.PUT_LINE('==============================================================================');
   DBMS_OUTPUT.PUT_LINE('Done processing all records from ' || inpTable);
   DBMS_OUTPUT.PUT_LINE('There were '||vErrorCounter||' erorrs.');
   DBMS_OUTPUT.PUT_LINE('Please update columns AREATOTAL, PERIMETER etc in feature table ' || ftrTable);
   DBMS_OUTPUT.PUT_LINE('==============================================================================');
   commit;
END DELETE_EDGES_2;


PROCEDURE DELETE_EDGES_TPMP(
   topology VARCHAR2,
   ftrTable VARCHAR2,
   inpTable VARCHAR2,
   valFlg VARCHAR2 default 'FALSE') AS
   
   --Matt! 8/16/13
   --Changed to use ez_topomap_mgr slash and burn approach to topomaps.
   --Resolves an unexplained java.lang.OutOfMemoryError
   
/*  **** THIS PROCEDURE USES EXPLICIT TOPOMAP TO REMOVE EDGES ****
   NAME:       DELETE_EDGES
   PURPOSE:    Deletes the edges identified in the input table from the
               topology to merge faces.
               The input table identifies which face attributes to save
               from the specified zero-level feature table.
   INPUT TABLE STRUCTURE AND REQUIREMENTS:
               Topology must have one and only one feature table.  Before
               and after running the delete_edges routine, this feature
               table should have a one-to-one face$ to feature relationship.
               Measurements stored in the feature table will be out of sync
               after this program runs, recalculate them before moving on.
              (uses the results of the identify small polygons routine,
              called "ELIMINATE_SMALL_POLYS")
              input table structure...
              EDGE_ID,
              LEFT_FACE_ID,
              RIGHT_FACE_ID,
              KEEP_FACE, (face id whose attributes we will keep)
              NOT_KEEP, (face id whose attributes will be lost)
              AREATOTAL,
              MAX_AREA,
              DONE     (to track what has been processed)
              If the faces to merge have different left and right state
              values... WHAT DOES IT DO?
              This program will create obsolete nodes that should be removed
              prior to line simplification.
     Output:
         Tracks progress by using the "DONE" field in the input table
         'Y' = completed sucessfully,
         '??' = could not complete,
         'QA" = did something, but needs review by a human,
         'XSelInpTbl' = There are duplicate edge ids in the input table
         'XInsRel$' = Error inserting records into Rel$ table (possible unique constraint error)
         'XF' = Unable to select updated keep Face from Face$ table
         NULL = unprocessed.
     Author: Sreeni Karpurapu
*/

   psql        VARCHAR2(4000);
   psql1       VARCHAR2(4000);
   psql2       VARCHAR2(4000);
   sql1        VARCHAR2(4000);
   sql2        VARCHAR2(4000);
   sql3        VARCHAR2(4000);
   ctr         pls_integer;
   arrEdge     MDSYS.SDO_LIST_TYPE;
   arrLyr_ID   MDSYS.SDO_NUMBER_ARRAY;
   arrTG_ID    MDSYS.SDO_NUMBER_ARRAY;
   arrTOPO_ID  MDSYS.SDO_NUMBER_ARRAY;
   updKeepFace NUMBER;
   updDelFace  NUMBER;
   v_tg_layer_id NUMBER;
   v_Edge_ID   NUMBER;
   v_Keep_Face NUMBER;
   v_del_face  NUMBER;
   --
   tmpsql      VARCHAR2(1000);
   tmpStr      VARCHAR2(1000);
   atrStr      VARCHAR2(1000);
   TYPE RefCursorType IS REF CURSOR;
   AtrCursor   RefCursorType;
   --
   nodeCursor  RefCursorType;
   curNode     NUMBER;
   v_code      NUMBER;
   v_errm      VARCHAR2(1000);
   TPLGY       VARCHAR2(50) := NULL;
   TPMP        VARCHAR2(50) := topology || '_TOPOMAP';
   DELTA       NUMBER(4,3)  := 0.001;
   xMin        NUMBER;
   yMin        NUMBER;
   xMax        NUMBER;
   yMax        NUMBER;
   x1Min       NUMBER;
   y1Min       NUMBER;
   x1Max       NUMBER;
   y1Max       NUMBER;
   x2Min       NUMBER;
   y2Min       NUMBER;
   x2Max       NUMBER;
   y2Max       NUMBER;
   res         VARCHAR2(10);
   --RecCtr         NUMBER := 0;
   vErrorCounter NUMBER := 0;
   create_it            NUMBER;
   
  BEGIN
  
   -- Get zero level "face" feature table's tg_layer_id from the
   -- topology metadata
   SELECT tg_layer_id INTO v_tg_layer_id
     FROM user_sdo_topo_info
    WHERE table_name = ftrTable;
  -- Get a list of unprocessed egdes from the input table
  -- with the smallest faces first.
   psql :=  '   SELECT Edge_id '
         || '     FROM ' || inpTable
         || '    WHERE done IS NULL '
         || ' ORDER BY areatotal';
   --DBMS_OUTPUT.PUT_LINE('psql = ' || psql);
   EXECUTE IMMEDIATE psql BULK COLLECT INTO arrEdge;
   -- Count Topo Geometry Layers..... should be only one layer (FACE)
   --DBMS_OUTPUT.PUT_LINE(arrEdge.LAST);

   
   -- Loop through each unprocessed edge in the input table
   If arrEdge.Count > 0 Then
   
      FOR i IN arrEdge.FIRST..arrEdge.LAST
      LOOP
      
         IF MOD(i,500) = 0
         THEN
         
            --periodic logging for Matt. Sweaty palms without positive feedback
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('SP', topology, 'DELETE_EDGES_TPMP',
                                                        'Removed another 500 edges');
                                 
         END IF;
         
      v_Edge_ID := arrEdge(i);
      --DBMS_OUTPUT.PUT_LINE('Processing Edge_ID: ' || v_Edge_ID);
      --DBMS_OUTPUT.PUT_LINE('i: ' || i || ' Edg: ' || v_Edge_ID || ' Fc1: ' || v_keep_face || ' Fc2: ' || v_del_face);
      -- get relevant face ids
      -- SK 04/06/2010 Handle "fetch returns more than requested number of rows" exception
      
      BEGIN
         psql1 := 'SELECT keep_face, not_keep FROM ' || inpTable || ' WHERE edge_id = :1';
         EXECUTE IMMEDIATE psql1 INTO v_Keep_Face, v_del_face USING v_Edge_ID;
      EXCEPTION
         WHEN OTHERS THEN
            v_code := SQLCODE;
            v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
            vErrorCounter := vErrorCounter + 1;
            sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
            EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
            COMMIT;
            sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
            EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
            COMMIT;
            --DBMS_OUTPUT.PUT_LINE('Select does not return one record ' || v_code || ': ' || v_errm);
            sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
            EXECUTE IMMEDIATE sql3 USING 'XSelInpTbl', v_edge_id;
            COMMIT;
            GOTO CONT;
      END;
      
      -- Get relation$ info for both faces
      psql1 := 'SELECT TG_LAYER_ID, TG_ID, TOPO_ID '
             ||'  FROM ' || topology || '_RELATION$ '
             ||' WHERE TOPO_ID IN (:1, :2) AND tg_layer_id = :3';
      --DBMS_OUTPUT.PUT_LINE('psql1: ' || psql1);
      --DBMS_OUTPUT.PUT_LINE('v_keep_face: ' || v_keep_face || ' v_del_face: ' || v_del_face || ' v_tg_layer_id: ' || v_tg_layer_id);
      EXECUTE IMMEDIATE psql1 BULK COLLECT INTO arrLyr_ID, arrTG_ID, arrTOPO_ID USING v_keep_face, v_del_face, v_tg_layer_id;
      
      IF arrTG_ID.COUNT = 2 THEN
         -- GET FACE MBRs
         
         get_face_mbr(topology, v_Keep_Face, x1Min, y1Min, x1Max, y1Max);
         get_face_mbr(topology, v_Del_Face, x2Min, y2Min, x2Max, y2Max);
         -- Compute the MBR of the two faces and add a buffer (DELTA)
         IF x1Min < x2Min THEN
            xMin := x1Min - DELTA;
         ELSE
            xMin := x2Min - DELTA;
         END IF;
         IF y1Min < y2Min THEN
            yMin := y1Min - DELTA;
         ELSE
            yMin := y2Min - DELTA;
         END IF;
         IF x1Max > x2Max THEN
            xMax := x1Max + DELTA;
         ELSE
            xMax := x2Max + DELTA;
         END IF;
         IF y1Max > y2Max THEN
            yMax := y1Max + DELTA;
         ELSE
            yMax := y2Max + DELTA;
         END IF;
         
--         BEGIN
--            dbms_output.put_line('xmin ' || xmin || ' ymin ' || ymin || ' xmax ' || xmax || ' ymax ' || ymax);
--            res := SDO_TOPO_MAP.LOAD_TOPO_MAP(TPMP, xMin, yMin, xMax, yMax, 'TRUE');
--         EXCEPTION
--            WHEN OTHERS THEN
--               v_code := SQLCODE;
--               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
--               vErrorCounter := vErrorCounter + 1;
--               DBMS_OUTPUT.PUT_LINE('heres the real error before the rollback' || SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);
--               DBMS_OUTPUT.PUT_LINE('Unable to Load Topo Map ' || v_code || ': ' || v_errm);
--               SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
--               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
--               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
--               COMMIT;
--               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
--               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
--               COMMIT;
--               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
--               EXECUTE IMMEDIATE sql3 USING 'X Unable to load topo map', v_edge_id;
--               COMMIT;
--               GOTO CONT;
--         END;
      -- Check to make sure both faces still exist in relation$, and if so,
          -- create duplicate pointers, so that face a and face b in face$ both
          -- point to both face a and b in the primitive table (this makes the
          -- values on both sides of the edge the same, allowing us to delete
          -- the edge.
          -- then remove the unecessary nodes you left behind
          -- Oracle picks which face id to keep after the edge is deleted,
          -- so if it keeps the one we want to drop, we copy the attributes
          -- from the keep face to the not keep face, and then drop the
          -- feature record for the face that oracle dropped
          -- to remove the duplicate attribute record from the feature table
      -- If both faces do not exist, then mark this edge with an 'X", skip it,
      -- and try to process the next edge.
         --04/06/2010 Handle any fallouts from inserting data
         BEGIN
         
            sql1 := 'INSERT INTO ' || topology || '_RELATION$ VALUES (:1, :2, :3, :4, :5)';
            EXECUTE IMMEDIATE sql1 USING arrLyr_ID(1), arrTG_ID(1), arrTOPO_ID(2), 3, '';
            --DBMS_OUTPUT.PUT_LINE('sql1: ' || sql1);
            --DBMS_OUTPUT.PUT_LINE('arrLyr_ID(1): ' || arrLyr_ID(1) || ' arrTG_ID(1): ' || arrTG_ID(1) || ' arrTOPO_ID(2): ' || arrTOPO_ID(2) || ' 3 BLANK');
            sql2 := 'INSERT INTO ' || topology || '_RELATION$ VALUES (:1, :2, :3, :4, :5)';
            EXECUTE IMMEDIATE sql2 USING arrLyr_ID(1), arrTG_ID(2), arrTOPO_ID(1), 3, '';
            
         EXCEPTION
         
            WHEN OTHERS THEN
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               vErrorCounter := vErrorCounter + 1;
               --DBMS_OUTPUT.PUT_LINE('Error while inserting into Rel$ table ' || v_code || ': ' || v_errm);
               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'XInsRel$', v_edge_id;
               COMMIT;
               GOTO CONT;
               
         END;
         
         --DBMS_OUTPUT.PUT_LINE('sql2: ' || sql2);
         --DBMS_OUTPUT.PUT_LINE('arrLyr_ID(1): ' || arrLyr_ID(1) || ' arrTG_ID(2): ' || arrTG_ID(2) || ' arrTOPO_ID(1): ' || arrTOPO_ID(1) || ' 3 BLANK');
         
         BEGIN
         
            create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topology || 'MAP',
                                                        topology,
                                                        2,
                                                        xMin,
                                                        yMin,
                                                        xMax,
                                                        yMax,
                                                        0);  --no delta, this code manages it above
                                                        
            SDO_TOPO_MAP.REMOVE_EDGE(TPLGY, v_Edge_ID);
            
            create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topology || 'MAP',topology,3);
            
            
         EXCEPTION
         
            WHEN OTHERS THEN
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               vErrorCounter := vErrorCounter + 1;
               --DBMS_OUTPUT.PUT_LINE('Unable to remove edge... skipping record ' || v_code || ': ' || v_errm);
               ROLLBACK;
               
               create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topology || 'MAP',topology,3);
               
               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'X Unable to remove edge', v_edge_id;
               COMMIT;
               GOTO CONT;
               
         END;
         
         /*
         tmpSql := 'SELECT node_id FROM ' || topology || '_node$ WHERE edge_id = 0';
         OPEN nodeCursor FOR tmpSql;
         LOOP
            FETCH nodeCursor INTO curNode;
            EXIT WHEN nodeCursor%NOTFOUND;
            BEGIN
              --DBMS_OUTPUT.PUT_LINE('CurNode: ' || curNode);
              SDO_TOPO_MAP.REMOVE_NODE(TPLGY, curNode);
            EXCEPTION
              WHEN OTHERS THEN
                 v_code := SQLCODE;
                 v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
                 DBMS_OUTPUT.PUT_LINE('Unable to remove CurNode: ' || curNode || ' ErrCode/Msg ' || v_code || ': ' || v_errm);
            END;
         END LOOP;
         CLOSE nodeCursor;
         */
         
         -- Drop the correct face. it might not be the given one.
         -- Select from face$ to see which face is left.
         -- There should be only one face
         -- SK 04/06/2010
         -- Sometimes deleting an edge does not automatically result in deleting one of the faces
         
         
         Begin
         
            sql1 := 'SELECT face_id FROM ' || topology || '_FACE$ WHERE face_id IN (:1, :2)';
            EXECUTE IMMEDIATE sql1 INTO updKeepFace USING v_Keep_Face, v_Del_Face;
            
         EXCEPTION
            WHEN OTHERS THEN
            
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               vErrorCounter := vErrorCounter + 1;
               --DBMS_OUTPUT.PUT_LINE('Error while selecting updKeepFace ' || v_code || ': ' || v_errm);
               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'XF', v_edge_id;
               COMMIT;
               GOTO CONT;
               
         END;
         
         -- Now that we know updKeepFace the other one should be updDelFace
         If updKeepFace = v_Keep_face Then
            updDelFace := v_Del_Face;
         Elsif updKeepFace = v_Del_Face Then
            updDelFace := v_Keep_Face;
         Else
         
            NULL;
            -- Invalid condition
            --DBMS_OUTPUT.PUT_LINE('***** Reached invalid Else condition, please verify *****');
            
         End If;
         
         -- reassign attributes to the face Oracle kept if necessary
         IF updKeepFace <> v_Keep_Face THEN
         
            --DBMS_OUTPUT.PUT_LINE('Topology decided to remove the "keep_face": ' || v_Keep_Face || ' and keep the "not_keep" face: ' || updKeepFace);
            --DBMS_OUTPUT.PUT_LINE('........ will process accordingly!');
            --DBMS_OUTPUT.PUT_LINE('updKeepFace = ' || updKeepFace || ' oldKeepFace = ' || arrKeepFace(i));
            -- Making an assumption that all REQUIRED attribute columns are of type VARCHAR2
               -- Not sure this assumption is a good one, but it is true for
               -- ACS09 - maybe we need a specific list (Stephanie)
            tmpSql := 'SELECT COLUMN_NAME FROM USER_TAB_COLUMNS WHERE TABLE_NAME = ''' || ftrTable || ''' AND DATA_TYPE = ''VARCHAR2'' ';
            --DBMS_OUTPUT.PUT_LINE(tmpsql);
            OPEN AtrCursor FOR tmpsql;
            atrStr := NULL;
            LOOP
               FETCH AtrCursor INTO tmpStr;
               EXIT WHEN AtrCursor%NOTFOUND;
               IF atrStr IS NULL THEN
                  atrStr := tmpStr;
               ELSE
                  atrStr := atrStr || ', ' || tmpStr;
               END IF;
            END LOOP;
            CLOSE AtrCursor;
            --DBMS_OUTPUT.PUT_LINE(atrStr);
            
            sql1 := 'UPDATE ' || ftrTable
                     || ' SET (' || atrStr || ') = (SELECT ' || atrStr || ' FROM ' || ftrTable
                     || ' WHERE face_id = :1) WHERE face_id = :2';
            execute immediate sql1 using v_Keep_Face, v_Del_Face;
            
         /*   Let's expand this logic and handle this after we update the done flag to Y
            --SK3
            -- Update any upprocessed records with the correct face_id
            sql2 := 'UPDATE ' || inpTable
                    || ' SET keep_face = ' || v_Del_Face
                    || ' WHERE done IS NULL AND keep_face = :1 '
                    || '   AND edge_id <> :2';
            DBMS_OUTPUT.PUT_LINE('Updating face_id.... ' || sql2);
            EXECUTE IMMEDIATE sql2 USING v_Keep_Face, v_edge_id;
         */
         /*  20100330  Dont need this.  Handled above
            updDelFace := v_Keep_Face;
         ELSE
            --DBMS_OUTPUT.PUT_LINE('updKeepFace = arrKeepFace');
            --DBMS_OUTPUT.PUT_LINE('updKeepFace = ' || updKeepFace || ' KeepFace = ' || arrKeepFace(i));
            updDelFace := v_Del_Face;
         */
         
         END IF;
         
         -- remove the redundant feature record you created
         EXECUTE IMMEDIATE 'DELETE FROM ' || ftrTable || ' WHERE face_id = ' || updDelFace;
         
         -- null out face mesurements in the feature table since they no
         -- longer match the geometry
         EXECUTE IMMEDIATE 'UPDATE ' || ftrTable || ' f '
                          --|| ' SET sdogeometry= f.topogeom.get_geometry(), '
                          || ' SET sdogeometry= NULL, '
                          || '     areatotal = NULL, '
                          || '     perimeter = NULL, '
                          || '     llx = NULL, '
                          || '     lly = NULL, '
                          || '     urx = NULL, '
                          || '     ury = NULL, '
                          || '     pa_ratio = NULL '
                          --|| '     mbr = NULL '
                          || ' WHERE face_id = ' || updKeepFace;
                          
         -- mark edge in the input table complete
         sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
         EXECUTE IMMEDIATE sql3 USING 'Y', v_edge_id;
         
         /*
         When processing Edge1 Oracle deleted either A or B
         updKeepFace and updDelFace will have the right values
         If updDelFace exists in any of the unprocessed Keep_Face or Not_Keep,
             replace that value with updKeepFace
         Example:
         Edge ID    Keep_Face    Not_Keep
         EDGE1      A            B
         EDGE2      A            B
         EDGE3      A            C
         EDGE4      D            A
         If Oracle kept B and Deleted A
         Edge ID    Keep_Face    Not_Keep
         EDGE1      A            B
         EDGE2      B            B  update keep_face from A to B
         EDGE3      B            C  update keep_face from A to B
         EDGE4      D            B  update not_keep from A to B
         If Oracle kept A and Deleted B
         Edge ID    Keep_Face    Not_Keep
         EDGE1      A            B
         EDGE2      A            A  update Not_keep from B to A
         EDGE3      A            C  No changes
         EDGE4      D            A  No changes
         */
         
            -- Update any upprocessed records with the correct face_id
            sql2 := 'UPDATE ' || inpTable
                    || ' SET keep_face = ' || updKeepFace
                    || ' WHERE done IS NULL AND keep_face = :1 '
                    || '   AND edge_id <> :2';
            --DBMS_OUTPUT.PUT_LINE('Updating face_id.... ' || sql2 || ' updDelFace: ' || updDelFace);
            EXECUTE IMMEDIATE sql2 USING updDelFace, v_edge_id;
            -- Update any upprocessed records with the correct face_id
            sql2 := 'UPDATE ' || inpTable
                    || ' SET not_keep = ' || updKeepFace
                    || ' WHERE done IS NULL AND not_keep = :1 '
                    || '   AND edge_id <> :2';
            --DBMS_OUTPUT.PUT_LINE('Updating face_id.... ' || sql2 || ' updDelFace: ' || updDelFace);
            EXECUTE IMMEDIATE sql2 USING updDelFace, v_edge_id;
         COMMIT;
         
      ELSIF (arrTG_ID.COUNT = 1) AND (v_Keep_Face = v_del_face) THEN
      
         -- Keep Face is same as delete face; Just delete the edge
         get_face_mbr(topology, v_Keep_Face, xMin, yMin, xMax, yMax);
         xMin := xMin - DELTA;
         yMin := yMin - DELTA;
         xMax := xMax + DELTA;
         yMax := yMax + DELTA;
         
         BEGIN
         
            create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topology || 'MAP',
                                                        topology,
                                                        2,
                                                        xMin,
                                                        yMin,
                                                        xMax,
                                                        yMax,
                                                        0); --no delta, this code manages it above
                                                        
         EXCEPTION
            WHEN OTHERS THEN
            
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               vErrorCounter :=  vErrorCounter + 1;
               --DBMS_OUTPUT.PUT_LINE('Unable to Load Topo Map ' || v_code || ': ' || v_errm);


               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'X unable to load topo map', v_edge_id;
               COMMIT;
               GOTO CONT;
               
         END;
         
         BEGIN
         
            SDO_TOPO_MAP.REMOVE_EDGE(TPLGY, v_Edge_ID);
            
            create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topology || 'MAP',topology,3);

            sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
            EXECUTE IMMEDIATE sql3 USING 'Y', v_edge_id;
            COMMIT;
            
         EXCEPTION
            WHEN OTHERS THEN
            
               v_code := SQLCODE;
               v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
               vErrorCounter := vErrorCounter + 1;
               --DBMS_OUTPUT.PUT_LINE('Unable to remove edge... skipping record ' || v_code || ': ' || v_errm);
               
               create_it := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(topology || 'MAP',topology,3);
               
               sql3 := 'UPDATE ' || inpTable || ' SET sqlstmt = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLCODE, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET error_msg = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING SQLERRM, v_edge_id;
               COMMIT;
               sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
               EXECUTE IMMEDIATE sql3 USING 'X unable to remove edge', v_edge_id;
               COMMIT;
               GOTO CONT;
               
         END;
         
      ELSE
         -- mark edge in the input table as not processable
         sql3 := 'UPDATE ' || inpTable || ' SET done = :1 WHERE edge_id = :2';
         EXECUTE IMMEDIATE sql3 USING 'X', v_edge_id;
         COMMIT;
         
         --DBMS_OUTPUT.PUT_LINE('********************************************* ERROR *********************************************');
         --DBMS_OUTPUT.PUT_LINE('TG_ID.COUNT is not equal to 2: ' || arrtg_id.count);
         --DBMS_OUTPUT.PUT_LINE('There are two possible causes for this error.');
         --DBMS_OUTPUT.PUT_LINE('  1) DELETE_EDGES requires there be one and only one feature table in the topology.');
         --DBMS_OUTPUT.PUT_LINE('     Please verify if any additional topo geometry layers have to be removed and rerun this program' );
         --DBMS_OUTPUT.PUT_LINE('  2) This could also mean that the keep and not_keep face ids are the same.  They must be different.' );
         --DBMS_OUTPUT.PUT_LINE('********************************************* ERROR *********************************************');
         
      END IF;

      <<CONT>>

      BEGIN
         arrLyr_ID.DELETE;
         arrTG_ID.DELETE;
         arrTOPO_ID.DELETE;
      EXCEPTION
         WHEN OTHERS THEN
             NULL;
      END;
      
      --null;
      /*
      RecCtr := RecCtr + 1;
      if (mod(RecCtr, 500) = 0) then
         DBMS_OUTPUT.PUT_LINE('Processed : ' || RecCtr || ' records....');
      end if;
      */
      
   END LOOP;
   
   --DBMS_OUTPUT.PUT_LINE('Total Number of Records Processed : ' || RecCtr);
   
   Else
   
      DBMS_OUTPUT.PUT_LINE('Did not find any edges to delete');
      
   End If;

   arrEdge.DELETE;


   DBMS_OUTPUT.PUT_LINE('');
   DBMS_OUTPUT.PUT_LINE('==============================================================================');
   DBMS_OUTPUT.PUT_LINE('Done processing all records from ' || inpTable);
   DBMS_OUTPUT.PUT_LINE('There were '||vErrorCounter||' errors');
   DBMS_OUTPUT.PUT_LINE('Please update columns AREATOTAL, PERIMETER etc in feature table ' || ftrTable);
   DBMS_OUTPUT.PUT_LINE('==============================================================================');
   commit;
   
END DELETE_EDGES_TPMP;



  PROCEDURE GET_NODE_XY(pTplgy IN VARCHAR2, pNode IN NUMBER, pX IN OUT NOCOPY NUMBER, pY IN OUT NOCOPY NUMBER) IS
      sql1 VARCHAR2(1000);
   BEGIN
      pX := NULL;
      pY := NULL;
      sql1 := 'SELECT n.geometry.sdo_point.x, n.geometry.sdo_point.y '
             ||' FROM ' || pTplgy || '_node$ n WHERE node_id = :1';
      EXECUTE IMMEDIATE sql1 INTO pX, pY USING pNode;
  END;
  PROCEDURE MK_ST_EDGE(topology IN VARCHAR2, edgeID IN NUMBER, sX IN NUMBER, sY IN NUMBER, eX IN NUMBER, eY IN NUMBER) IS
  BEGIN
     SDO_TOPO_MAP.CHANGE_EDGE_COORDS(topology,edgeID,SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_ORDINATE_ARRAY(sX,sY,eX,eY)));
  END;
  PROCEDURE MOVE_EDGE(topology IN VARCHAR2, edgeID IN NUMBER,
                       srcNode IN NUMBER, tgtNode IN NUMBER,
                       ordX1 IN NUMBER, ordY1 IN NUMBER,
                       ordX2 IN NUMBER, ordY2 IN NUMBER) IS
      EdgeNumArray MDSYS.SDO_NUMBER_ARRAY;
   BEGIN
      EdgeNumArray := MDSYS.SDO_NUMBER_ARRAY();
      EdgeNumArray.Extend(4);
      EdgeNumArray(1) := ordX1;
      EdgeNumArray(2) := ordY1;
      EdgeNumArray(3) := ordX2;
      EdgeNumArray(4) := ordY2;
      SDO_TOPO_MAP.MOVE_EDGE(topology, edgeID, srcNode, tgtNode, EdgeNumArray);
      EdgeNumArray.DELETE;
  END;
  PROCEDURE UPDATE_STATUS(pInpTable IN VARCHAR2, pFaceID IN NUMBER, pStatus IN VARCHAR2) IS
     --PRAGMA AUTONOMOUS_TRANSACTION;
     sql1 VARCHAR2(1000);
  BEGIN
     DBMS_OUTPUT.PUT_LINE('Face_ID: ' || pFaceID || ' Status: ' || pStatus);
     sql1 := 'UPDATE ' || pInpTable || ' SET done = :1 WHERE face_id = :2 AND done IS NULL';
     EXECUTE IMMEDIATE sql1 USING pStatus, pFaceID;
     COMMIT;
  END;
  PROCEDURE FIX_SLIVERS(topology IN VARCHAR2, inpTable IN VARCHAR2, ftrTable IN VARCHAR2) IS
     TYPE RefCursorType IS REF CURSOR;
     sliverEditsCur RefCursorType;
     sliverRec      GZ_TYPES.SLIVER_REC;
     sql1           VARCHAR2(1000) := NULL;
     Tplgy          VARCHAR2(50) := NULL;
     TpMp           VARCHAR2(50) := topology || '_TOPOMAP';
     res            VARCHAR2(10);
     vtxStart       vtxTyp;
     vtxMid         vtxTyp;
     vtxEnd         vtxTyp;
     delta          NUMBER := 0;
     --tmpEdge edgTyp;
     othNode        NUMBER;
     fxdNode        NUMBER;
     sX             NUMBER;
     sY             NUMBER;
     eX             NUMBER;
     eY             NUMBER;
     oX             NUMBER;
     oY             NUMBER;
     x1             NUMBER;
     y1             NUMBER;
     x2             NUMBER;
     y2             NUMBER;
     xMin           NUMBER;
     yMin           NUMBER;
     xMax           NUMBER;
     yMax           NUMBER;
     leftFace       NUMBER;
     rightFace      NUMBER;
     tmpNode        NUMBER;
     tmpEdge        NUMBER;
     edge2Move      edgTyp;
     v_code         NUMBER;
     v_errm         VARCHAR2(1000);
     leftFaceID     NUMBER;
     rightFaceID    NUMBER;
     debugX         NUMBER;
     debugY         NUMBER;
     --State_Fips            MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
  BEGIN
     -- 1) Read inpTable
     --State_Fips := GET_STATE_FIPS;
     --FOR i in 1..State_Fips.count LOOP   -- StateFP Loop
         -- We dont need this; if we sort the states by statefp, it should be sufficient.
        --sql1 := 'SELECT * FROM ' || inpTable || ' WHERE done IS NULL AND statefp = :1';
        --OPEN sliverEditsCur FOR sql1 USING state_fips(i);
        sql1 := 'SELECT * FROM ' || inpTable || ' WHERE done IS NULL ORDER BY statefp';
        OPEN sliverEditsCur FOR sql1;
/* TOPOMAP STUFF*/
       SDO_TOPO_MAP.CREATE_TOPO_MAP(TOPOLOGY, TPMP);
        LOOP
          --SK DBMS_OUTPUT.PUT_LINE('Begin LOOP');
          FETCH sliverEditsCur INTO sliverRec;
          EXIT WHEN sliverEditsCur%NOTFOUND;
          vtxStart.X := sliverRec.xStart;
          vtxStart.Y := sliverRec.yStart;
          DBMS_OUTPUT.PUT_LINE('FaceID: ' || sliverRec.face_id);
          --SK DBMS_OUTPUT.PUT_LINE('FaceID: ' || sliverRec.face_id || ' SX: ' || vtxStart.X || ' SY: ' || vtxStart.Y);
          --tmpNode := null;
          BEGIN
             get_face_mbr(topology, sliverRec.face_id, xMin, yMin, xMax, yMax);
             xMin := xMin - delta;
             yMin := yMin - delta;
             xMax := xMax + delta;
             yMax := yMax + delta;
             res := SDO_TOPO_MAP.LOAD_TOPO_MAP(TPMP, xMin, yMin, xMax, yMax, 'TRUE');
             DBMS_OUTPUT.PUT_LINE('Result: ' || res);
             --DBMS_OUTPUT.PUT_LINE('tmpNode Begin vtxStart.X ' || vtxStart.X || ' vtxStart.Y: ' || vtxStart.Y);
             tmpNode := is_node(TOPOLOGY, vtxStart.X, vtxStart.Y);
             --DBMS_OUTPUT.PUT_LINE('tmpNode Start: ' || tmpNode);
             IF tmpNode is NULL THEN
                debugX := vtxStart.X;
                debugY := vtxStart.Y;
                tmpEdge := get_closest_edge(TOPOLOGY, vtxStart.X, vtxStart.Y);
                IF tmpEdge IS NULL THEN
                   DBMS_OUTPUT.PUT_LINE('Please verify.... Vertex not on edge (' || vtxStart.X || ', ' || vtxStart.Y);
                   UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'QE');
                   SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
                   GOTO END_LOOP;
                END IF;
                vtxStart.node_id := add_node(TOPOLOGY, NULL, tmpEdge, vtxStart.X, vtxStart.Y);
                IF vtxStart.node_id IS NULL THEN
                   DBMS_OUTPUT.PUT_LINE('Unable to add node on edge ' || tmpEdge || ' (' || vtxStart.X || ', ' || vtxStart.Y);
                   UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'QN');
                   SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
                   GOTO END_LOOP;
                END IF;
             ELSE
                vtxStart.node_id := tmpNode;
             END IF;
             vtxMid.X := sliverRec.xMid;
             vtxMid.Y := sliverRec.yMid;
             --SK DBMS_OUTPUT.PUT_LINE('FaceID: ' || sliverRec.face_id || ' MX: ' || vtxMid.X || ' MY: ' || vtxMid.Y);
             tmpNode := is_node(TOPOLOGY, vtxMid.X, vtxMid.Y);
             --SK DBMS_OUTPUT.PUT_LINE('tmpNode Mid: ' || tmpNode);
             IF tmpNode is NULL THEN
                debugX := vtxMid.X;
                debugY := vtxMid.Y;
                tmpEdge := get_closest_edge(TOPOLOGY, vtxMid.X, vtxMid.Y);
                IF tmpEdge IS NULL THEN
                   DBMS_OUTPUT.PUT_LINE('Please verify.... Vertex not on edge (' || vtxMid.X || ', ' || vtxMid.Y);
                   UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'QE');
                   SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
                   GOTO END_LOOP;
                END IF;
                vtxMid.node_id := add_node(TOPOLOGY, NULL, tmpEdge, vtxMid.X, vtxMid.Y);
                IF vtxMid.node_id IS NULL THEN
                   DBMS_OUTPUT.PUT_LINE('Unable to add node on edge ' || tmpEdge || ' (' || vtxMid.X || ', ' || vtxMid.Y);
                   UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'QN');
                   SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
                   GOTO END_LOOP;
                END IF;
             ELSE
                vtxMid.node_id := tmpNode;
             END IF;
             vtxEnd.X := sliverRec.xEnd;
             vtxEnd.Y := sliverRec.yEnd;
             --SK DBMS_OUTPUT.PUT_LINE('FaceID: ' || sliverRec.face_id || ' EX: ' || vtxEnd.X || ' EY: ' || vtxEnd.Y);
             tmpNode := is_node(TOPOLOGY, vtxEnd.X, vtxEnd.Y);
             --SK DBMS_OUTPUT.PUT_LINE('tmpNode End: ' || tmpNode);
             IF tmpNode is NULL THEN
                debugX := vtxEnd.X;
                debugY := vtxEnd.Y;
                tmpEdge := get_closest_edge(TOPOLOGY, vtxEnd.X, vtxEnd.Y);
                IF tmpEdge IS NULL THEN
                   DBMS_OUTPUT.PUT_LINE('Please verify.... Vertex not on edge (' || vtxEnd.X || ', ' || vtxEnd.Y);
                   UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'QE');
                   SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
                   GOTO END_LOOP;
                END IF;
                vtxEnd.node_id := add_node(TOPOLOGY, NULL, tmpEdge, vtxEnd.X, vtxEnd.Y);
                IF vtxEnd.node_id IS NULL THEN
                   DBMS_OUTPUT.PUT_LINE('Unable to add node on edge ' || tmpEdge || ' (' || vtxEnd.X || ', ' || vtxEnd.Y);
                   UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'QN');
                   SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
                   GOTO END_LOOP;
                END IF;
             ELSE
                vtxEnd.node_id := tmpNode;
             END IF;
             DBMS_OUTPUT.PUT_LINE('Start Node: ' || vtxStart.node_id || ' Mid Node: ' || vtxMid.node_id|| ' End Node: ' || vtxEnd.node_id);
             IF (vtxStart.node_id = vtxMid.node_id) OR (vtxMid.node_id = vtxEnd.node_id) OR (vtxStart.node_id  = vtxEnd.node_id) THEN
                DBMS_OUTPUT.PUT_LINE('Bad data in input table. Duplicate nodes...  Please verify start, mid and end nodes.');
                UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'QDUPN');
                --sql1 := 'Update Sliver_Edits set done = ''QD'' where face_id = :1';
                --execute immediate sql1 using sliverRec.Face_ID;
                SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
                GOTO END_LOOP;
             END IF;
          EXCEPTION
             WHEN OTHERS THEN
                v_code := SQLCODE;
                v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
                DBMS_OUTPUT.PUT_LINE('curVx = ' || debugX || ' curVy = ' || debugY);
                DBMS_OUTPUT.PUT_LINE('Unable to add nodes... skipping record ' || v_code || ': ' || v_errm);
                UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'QN');
                SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
                GOTO END_LOOP;
          END;
          -- 2) Verify and Add Nodes
          -- 3) get Edges between nodes
           IF sliverRec.start_on = 'TRUE' THEN
              -- Move Edge between Mid and End Nodes
              edge2move := GET_EDGE(TOPOLOGY, vtxMid.node_id, vtxEnd.node_id);
              fxdNode := vtxEnd.node_id;
              othNode := vtxStart.node_id;
              DBMS_OUTPUT.PUT_LINE('edge2Move: ' || edge2Move.ID || ' sNode: ' || edge2Move.sNode || ' eNode: ' || edge2Move.eNode || ' othNode: ' || othNode);
           ELSIF sliverRec.start_on = 'FALSE' THEN
              -- Move Edge between Start and Mid Nodes
              edge2Move := GET_EDGE(TOPOLOGY, vtxStart.node_id, vtxMid.node_id);
              fxdNode := vtxStart.node_id;
              othNode := vtxEnd.node_id;
              DBMS_OUTPUT.PUT_LINE('edge2Move: ' || edge2Move.ID || ' sNode: ' || edge2Move.sNode || ' eNode: ' || edge2Move.eNode || ' othNode: ' || othNode);
           ELSE
              -- Bad Data
              DBMS_OUTPUT.PUT_LINE('Bad start_on *' || sliverRec.start_on || '*');
              NULL;
           END IF;
           IF (edge2Move.id is NULL) OR (othNode = vtxMid.node_id) THEN
               IF edge2Move.id is NULL THEN
                  DBMS_OUTPUT.PUT_LINE('edge2Move.id is null, handle this manually');
               ELSE
                  DBMS_OUTPUT.PUT_LINE('MidNode is same as othNode.... please verify');
               END IF;
               UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'QX');
               SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
               GOTO END_LOOP;
               --sql1 := 'Update Sliver_Edits set done = ''QX'' where face_id = :1';
               --execute immediate sql1 using sliverRec.Face_ID;
               DBMS_OUTPUT.PUT_LINE('******************************************');
               DBMS_OUTPUT.PUT_LINE('Fix this record manually.  Face_ID: ' || sliverRec.Face_ID);
               DBMS_OUTPUT.PUT_LINE('******************************************');
           ELSE
              -- 4) straighten edge
              -- simplify/straighten edge
              DBMS_OUTPUT.PUT_LINE('EDGE2MOVE is: ' || edge2Move.id);
              DBMS_OUTPUT.PUT_LINE('fxdNode: ' || fxdNode || ' midNode: ' || vtxMid.node_id || ' othNode: ' || othNode);
              get_Node_XY(TOPOLOGY, edge2Move.sNode, sX, sY);
              --DBMS_OUTPUT.PUT_LINE('Node: ' || edge2Move.sNode || ' sX: ' || sX || ' sY: ' || sY);
              get_Node_XY(TOPOLOGY, edge2Move.eNode, eX, eY);
              --DBMS_OUTPUT.PUT_LINE('Node: ' || edge2Move.eNode || ' eX: ' || eX || ' eY: ' || eY);
              --DBMS_OUTPUT.PUT_LINE('-----');
/*  Using topomap don't need this
              BEGIN
                 SDO_TOPO_MAP.CHANGE_EDGE_COORDS(tplgy,edge2Move.ID,SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_ORDINATE_ARRAY(sX,sY,eX,eY)));
              EXCEPTION
                 WHEN OTHERS THEN
                    v_code := SQLCODE;
                    v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
                    DBMS_OUTPUT.PUT_LINE('CHANGE_EDGE_COORDS Error code ' || v_code || ': ' || v_errm);
                    UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'QCEC');
                    --sql1 := 'Update Sliver_Edits set done = ''QE'' where face_id = :1';
                    --execute immediate sql1 using sliverRec.Face_ID;
                    GOTO END_LOOP;
              END;
*/
              -- 5) move edge
              --DBMS_OUTPUT.PUT_LINE('MidNode (src): ' || vtxMid.node_id || ' othNode (tgt): ' || othNode || ' fxdNode: ' || fxdNode);
              get_Node_XY(TOPOLOGY, fxdNode, x1, y1);
              --DBMS_OUTPUT.PUT_LINE('fxdNode: ' || fxdNode || ' x1: ' || x1 || ' y1: ' || y1);
              get_Node_XY(TOPOLOGY, othNode, x2, y2);
              --DBMS_OUTPUT.PUT_LINE('othNode: ' || othNode || ' x2: ' || x2 || ' y2: ' || y2);
              BEGIN
                 IF edge2Move.sNode = fxdNode THEN
                    --DBMS_OUTPUT.PUT_LINE(topology ||' ' || edge2Move.ID ||' ' || vtxMid.node_id ||' ' || othNode ||' ' || x1 ||' ' || y1 ||' ' || x2 ||' ' || y2);
                    move_Edge(tplgy, edge2Move.ID, vtxMid.node_id, othNode, x1, y1, x2, y2);
                 ELSIF edge2Move.sNode = vtxMid.node_id THEN
                    --DBMS_OUTPUT.PUT_LINE(topology ||' ' || edge2Move.ID ||' ' || vtxMid.node_id ||' ' || othNode ||' ' || x2 ||' ' || y2 ||' ' || x1 ||' ' || y1);
                    move_Edge(tplgy, edge2Move.ID, vtxMid.node_id, othNode, x2, y2, x1, y1);
                 ELSE
                    DBMS_OUTPUT.PUT_LINE('ERROR ****');
                 END IF;
              EXCEPTION
                 WHEN OTHERS THEN
                    v_code := SQLCODE;
                    v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
                    DBMS_OUTPUT.PUT_LINE('MOVE_EDGE Error code ' || v_code || ': ' || v_errm);
                    UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'QME');
                    --sql1 := 'Update Sliver_Edits set done = ''QM'' where face_id = :1';
                    --execute immediate sql1 using sliverRec.Face_ID;
                    SDO_TOPO_MAP.ROLLBACK_TOPO_MAP;
                    GOTO END_LOOP;
              END;
              -- We Don't want short edges, so try to remove the fixed Node.  This would eliminate short edges.
              BEGIN
                 SDO_TOPO_MAP.REMOVE_NODE(tplgy, fxdNode);
                 DBMS_OUTPUT.PUT_LINE('Removed Node: ' || fxdNode);
              EXCEPTION
                 WHEN OTHERS THEN
                    v_code := SQLCODE;
                    v_errm := SQLERRM; --SUBSTR(SQLERRM, 1 , 64);
                    DBMS_OUTPUT.PUT_LINE('Unable to remove Node: ' || fxdNode || ' ErrCode/Msg ' || v_code || ': ' || v_errm);
              END;
              -- 6) update feature table
              sql1 := 'SELECT left_face_id, right_face_id FROM ' || topology || '_EDGE$ WHERE edge_id = :1';
              EXECUTE IMMEDIATE sql1 INTO leftFaceID, rightFaceID USING edge2Move.ID;
              DBMS_OUTPUT.PUT_LINE('Updating SDOGEOMETRY.... Edge_ID: ' || edge2Move.ID || ' leftFaceID: ' || leftFaceID || ' rightFaceID: ' || rightFaceID);
              SDO_TOPO_MAP.UPDATE_TOPO_MAP;
              SDO_TOPO_MAP.COMMIT_TOPO_MAP;
              SQL1 := 'UPDATE ' || ftrTable || ' f '
                     || ' SET sdogeometry= f.topogeom.get_geometry() '
                     || ' WHERE f.face_id = :1';
              DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
              EXECUTE IMMEDIATE SQL1 using leftFaceID;
              EXECUTE IMMEDIATE SQL1 using rightFaceID;
                             --|| ', '
                             --|| '     areatotal = NULL, '
                             --|| '     perimeter = NULL, '
                             --|| '     llx = NULL, '
                             --|| '     lly = NULL, '
                             --|| '     urx = NULL, '
                             --|| '     ury = NULL, '
                             --|| '     pa_ratio = NULL '
                             --|| '     mbr = NULL '
              COMMIT;
              --EXECUTE IMMEDIATE 'UPDATE ' || ftrTable || ' f '
              --               || ' SET sdogeometry= f.topogeom.get_geometry() '
                             --|| ', '
                             --|| '     areatotal = NULL, '
                             --|| '     perimeter = NULL, '
                             --|| '     llx = NULL, '
                             --|| '     lly = NULL, '
                             --|| '     urx = NULL, '
                             --|| '     ury = NULL, '
                             --|| '     pa_ratio = NULL '
                             --|| '     mbr = NULL '
              --               || ' WHERE face_id = ' || rightFaceID;
              UPDATE_STATUS(inpTable, sliverRec.Face_ID, 'Y');
              --sql1 := 'Update ' || inpTable || ' set done = ''Y'' where face_id = :1';
              --execute immediate sql1 using sliverRec.Face_ID;
           END IF;
        <<END_LOOP>>
           --XSDO_TOPO_MAP.ROLLBACK_TOPO_MAP;  -- Rollback changes since previous commit;
           SDO_TOPO_MAP.CLEAR_TOPO_MAP(TPMP);
           COMMIT;
           DBMS_OUTPUT.PUT_LINE('-----');
        END LOOP;
        CLOSE sliverEditsCur;
        SDO_TOPO_MAP.DROP_TOPO_MAP(TpMp);
        NULL;
     --END LOOP; StateFP loop
  END;
END GZ_TOPO_EDIT;
/
