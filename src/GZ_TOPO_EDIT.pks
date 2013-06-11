CREATE OR REPLACE PACKAGE GZ_TOPO_EDIT AUTHID CURRENT_USER AS
/******************************************************************************
   NAME:       GZ_TOPO_EDIT
   PURPOSE:

   REVISIONS:
   Ver        Date        Author           Description
   ---------  ----------  ---------------  ------------------------------------
   1.0        10/7/2009   Sreeni Karpurapu 1. Created this package.
******************************************************************************/

--  FUNCTION MyFunction(Param1 IN NUMBER) RETURN NUMBER;

   TYPE vtxTyp IS RECORD (
      X NUMBER,
      Y NUMBER,
      edge_id NUMBER,
      node_id number
   );
   
  TYPE edgTyp IS RECORD (
     ID NUMBER,
     sNode NUMBER,
     eNode NUMBER
  );
  
  FUNCTION GET_STATE_FIPS RETURN  MDSYS.SDO_LIST_TYPE;
     
  FUNCTION IS_NODE(topology IN VARCHAR2, ordX IN NUMBER, ordY IN NUMBER) RETURN NUMBER;
  
  FUNCTION GET_CLOSEST_EDGE(topology IN VARCHAR2, ordX IN NUMBER, ordY IN NUMBER) RETURN NUMBER;
  
  FUNCTION ADD_NODE(topology IN VARCHAR2, tplgy IN VARCHAR2, edgeID IN NUMBER, ordX IN NUMBER, ordY IN NUMBER) RETURN NUMBER;

  FUNCTION ADD_NODE2(topology IN VARCHAR2, edgeID IN NUMBER, ordX IN NUMBER, ordY IN NUMBER) RETURN NUMBER;
  
  FUNCTION GET_NODE_POS(topology IN VARCHAR2, edgeID IN NUMBER, ordX IN NUMBER, ordY IN NUMBER) RETURN NUMBER;
  
  FUNCTION GET_NODE_POS_TRUE(topology IN VARCHAR2, edgeID IN NUMBER, ordX IN NUMBER, ordY IN NUMBER) RETURN NUMBER;

  FUNCTION IS_NEW_SHAPE_POINT(topology IN VARCHAR2, edgeID IN NUMBER, ordX IN NUMBER, ordY IN NUMBER) RETURN VARCHAR2;
  
  FUNCTION GET_EDGE(pTplgy IN varchar, pN1 IN NUMBER, pN2 IN NUMBER) RETURN edgTyp;

  PROCEDURE GET_FACE_MBR(pTplgy IN VARCHAR2, pFaceID IN NUMBER, pXmin IN OUT NUMBER, pYmin IN OUT NUMBER, pXmax IN OUT NUMBER, pYmax IN OUT NUMBER); 
   
  PROCEDURE DELETE_EDGES(topology IN VARCHAR2, ftrTable IN VARCHAR2, inpTable IN VARCHAR2);
  
  PROCEDURE DELETE_EDGES_2(topology VARCHAR2, ftrTable VARCHAR2, inpTable VARCHAR2, valFlg VARCHAR2 default 'FALSE');
  
  PROCEDURE DELETE_EDGES_TPMP(topology IN VARCHAR2, ftrTable IN VARCHAR2, inpTable IN VARCHAR2, valFlg VARCHAR2 default 'FALSE');

  PROCEDURE GET_NODE_XY(pTplgy IN VARCHAR2, pNode IN NUMBER, pX IN OUT NOCOPY NUMBER, pY IN OUT NOCOPY NUMBER);
  
  PROCEDURE MK_ST_EDGE(topology IN VARCHAR2, edgeID IN NUMBER, sX IN NUMBER, sY IN NUMBER, eX IN NUMBER, eY IN NUMBER);
  
  PROCEDURE MOVE_EDGE(topology IN VARCHAR2, edgeID IN NUMBER, srcNode IN NUMBER, tgtNode IN NUMBER, ordX1 IN NUMBER, ordY1 IN NUMBER, ordX2 IN NUMBER, ordY2 IN NUMBER);

  PROCEDURE UPDATE_STATUS(pInpTable IN VARCHAR2, pFaceID IN NUMBER, pStatus IN VARCHAR2);

  PROCEDURE FIX_SLIVERS(topology IN VARCHAR2, inpTable IN VARCHAR2, ftrTable IN VARCHAR2);
  
  
    
END GZ_TOPO_EDIT;
/

