CREATE OR REPLACE PACKAGE BODY GZ_PROJECTION  AS
-- Updated: 01/04/2012 Added ID_is_NUMBER (may be 'TRUE' or 'FALSE') so user
--                     may get NUMBER type for output table when ID is number!
-- Updated: 08/10/2011 Ensured output table dropped if it exists.
-- Updated: 05/26/2011 Dropped defaults from column and renamed arguments more clearly
-- Updated: 05/12/2011 Dropped extra column capability since it caused confusion
--     This package is just used for the Z9 resolution to produce a custom projection with Alaska,
--     Puerto Rico and Hawaii in a suitable position with respect to the contiguous USA states.

--     The result is an Albers projection in US feet. It uses SRID 1000081 because of an error in
--     the way we defined 1000082. That has since been fixed. The rubber sheeting to place Alaska
--     and the islands was done by similarity transforms using legacy information. 

--     There is no use of distance in this package and although only tested with SRID input set to
--     8265, other input SRIDs should be seemlessly handled by Oracle's SDO_CS.TRANSFORM function.


--     The input arguments to PROJECT2007_TO_ALBERS (the user's entrypoint) which performs
--     projection on polygons (either 2007s or 2003s or both) are:

--                       pTableName?            - the input table to be projected
--                       pGeometry_Column      - the geometry column in that table
--                       pId_Column            - the geo Id column in that table
--                       pSchema_Name          - the output Schema
--                       pOutput_Table_Name    - the output Table to be created

--

PROCEDURE PROJECT_2007_TO_ALBERS(pTopology VARCHAR2,pTable_name VARCHAR2,pGeometry_Column VARCHAR2,pID_Column VARCHAR2,pSCHEMA_NAME VARCHAR2,
pOutput_Table VARCHAR2,pNew_Geom_Column VARCHAR2 default 'NEW_GEOMETRY',EXCLUDE VARCHAR2 DEFAULT NULL,
pID_is_NUMBER VARCHAR2 default 'TRUE',plog_type   VARCHAR2 DEFAULT 'PROJECTION'
,seq_name VARCHAR2  default 'GZ_PROJECT_SEQ',Wrk_table2003 VARCHAR2 DEFAULT 'GZ_PROJECT_WRK2003',Wrk_tablearea VARCHAR2 DEFAULT 'GZ_PROJECT_WRKAREA',
Wrk_tableout VARCHAR2 DEFAULT 'GZ_PROJECT_WRKOUT') AS


-- Procedure to project the input Table name in Schema to a Projected_Table.
-- PID is OID or face_id
-- Pnew_GEOMETRY is the name of the geometry column in the new Table.
-- pExclude : either NULL or 'OID<>-1'
--

   Topology               VARCHAR2(30)  := UPPER(pTopology);
   InputTable_name        VARCHAR2(100) := UPPER(pTable_Name);
   Schema_name            VARCHAR2(100) := NVL(UPPER(pSchema_Name),USER);
   WrkTable2003s          VARCHAR2(100) := UPPER(Wrk_table2003);
   WrkArea_Table          VARCHAR2(100) := UPPER(Wrk_tablearea);
   WrkOut_Table           VARCHAR2(100) := UPPER(Wrk_Tableout);
   Output_Table           VARCHAR2(100) := UPPER(pOutput_Table);
   Id_column              VARCHAR2(100) := UPPER(pID_COLUMN);
   Geometry_column        VARCHAR2(100) := UPPER(pGeometry_column);
   New_Geom_Column        VARCHAR2(100) := NVL(UPPER(pNew_Geom_Column),'NEW_GEOMETRY');
   Ids                    VCHAR100_ARRAY;
   SRID                   NUMBER;
   ID                     VARCHAR2(100);
   ID_Is_Number           VARCHAR2(10) := NVL(UPPER(pID_is_Number),'TRUE');
   kount_of_2007s         NUMBER;
   log_type               VARCHAR2(30) := plog_type;
   error_msg              VARCHAR2(1000);
   sql_stmt               VARCHAR2(4000);
BEGIN
 dbms_output.put_line('name ' || wrktable2003s);
------------------------------SETUP for a programmer run -----------------------  
-- This is just for programmer use, creating a tracking table  
   
   if log_type is NULL then
      log_type := 'PROJECTION';
   end if;
   
   IF log_type = 'PROJECTION'
      THEN

         --new standalone projection job, make a log table
         GZ_BUSINESS_UTILS.CREATE_GEN_XTEND_TRACKING_LOG(SYS_CONTEXT('USERENV', 'CURRENT_USER'),
                                                    Topology || '_'||log_type||'_TRACKING');
   END IF;

--------------------------------------START check paramters---------------------
        error_msg := ':START:Projection:';
        GZ_TOPOFIX.Track_App(error_msg,Topology,log_type); 
 

   if pTopology is NULL or pTable_name is NULL or pGeometry_Column is NULL or pId_Column is NULL or pSchema_Name is NULL or pOutput_Table is NULL  
      then
      error_msg := ':FATAL Error:Projection: At least one NULL Input parameter to PROJECT_2007_TO_ALBERS, Topology:'||pTopology||': ,pTable_Name:'||pTable_Name||': ,pGeometry_column:' ||pGeometry_Column || ': ,pID_column:' || pID_Column ||': ,pSchema_Name:'||pSchema_Name||': ,pOutput_Table:'||pOutput_Table||':';
      if Topology is NULL then
        raise_application_error(-20001,'Topology may NOT BE NULL');
      else
        GZ_TOPOFIX.Track_App_Error(error_msg,Topology,log_type); 
      end if;
   end if;  


   error_msg := ':Begin:Projection:Parameters - 1) Topology='||Topology||
                ', 2) Input table='||InputTable_name||', 3) geometry column='||Geometry_column||', 4) Id column='||
                pId_column||',5) Schema '||pSchema_name||',6) Output table '||pOutput_table ||',7) new geometry column='||
                pNew_Geom_column||', 8) EXclude='||exclude||',9) Id is a number ' ||pid_is_number||', 10) log_type='||plog_type;
   GZ_TOPOFIX.Track_App(error_msg,Topology,log_type);             
                 
---------------------------------Step A, Converting geometries to 2003s --------

   error_msg := ':Converting geometries:Projection: to 2003s';
   GZ_TOPOFIX.Track_App(error_msg,Topology,log_type); 
   kount_of_2007s := CONVERT_2007_TO_2003(Topology,InputTable_name,pGeometry_Column,pID_Column,WrkTable2003s,'COMPID',seq_name);
 
   error_msg := ':Finished converting:Projection:, input count is:' || kount_of_2007s;
   GZ_TOPOFIX.Track_App(error_msg,Topology,log_type); 
 
 --------------------------------Failure - no data !!  
   if kount_of_2007s <=0 then
     error_msg := ':FATAL ERROR:Projection:  no input data';
     GZ_TOPOFIX.Track_App_Error(error_msg,Topology,log_type);    
   end if;
 
   
 -------------------------------Step B Projecting ------------------------------  
   error_msg := ':Projecting geometries:Projection:';
   GZ_TOPOFIX.Track_App(error_msg,Topology,log_type); 
   
   Project_to_Albers(WrkTable2003s,Geometry_column,ID_Column,WrkArea_Table,pSCHEMA_NAME,WrkOut_Table,pNew_Geom_Column,EXCLUDE);
  
   error_msg := ':Finished projection:Projection:';
   GZ_TOPOFIX.Track_App(error_msg,Topology,log_type); 
 
 -------------------------------Step C Aggregating output----------------------- 
   
   sql_stmt := 'SELECT DISTINCT('||ID_Column||') FROM '|| InputTable_Name;
   EXECUTE IMMEDIATE sql_stmt BULK COLLECT into Ids;
   
   error_msg := ':Aggregating geometries:Projection: and writing output table '|| Output_Table;
   GZ_TOPOFIX.Track_App(error_msg,Topology,log_type); 
 

    if Id_column <> 'ID' then
       sql_stmt := 'ALTER Table ' || Output_table || ' rename column ID to ' || pID_column;
       
       BEGIN
          EXECUTE IMMEDIATE sql_stmt;
       EXCEPTION 
       WHEN OTHERS THEN
       
          IF SQLCODE = -957
          THEN
             
             ---ORA-00957: duplicate column name
             --Evil Matt likes to reuse these work tables over and over
             NULL;
             
          ELSE
          
             RAISE;
             
          END IF;
       
       END;
       
    end if;
    if pNew_Geom_column <> 'GEOMETRY' then
       sql_stmt := 'ALTER Table ' || Output_table || ' rename column geometry to ' || pNew_Geom_column;
       
       BEGIN
          EXECUTE IMMEDIATE sql_stmt;
       EXCEPTION 
       WHEN OTHERS THEN
       
          IF SQLCODE = -957
          THEN
          
             NULL;
             
          ELSE
          
             RAISE;
             
          END IF;
       
       END;
       
    end if;
    commit;
   
   For ii in 1..Ids.count Loop 
     ID := Ids(ii);
     sql_stmt := 'INSERT INTO /*+ APPEND */ ' || Output_Table ||' ('||Id_Column||','||
                  pNew_GEOM_Column||') SELECT MAX(t.ID),' ||  
                  'SDO_AGGR_UNION(SDOAGGRTYPE(t.geometry,0.05)) ' ||
                  ' FROM ' || WrkOut_table ||' t where t.ID= :1';
                  
--dbms_output.put_line(SUBSTR(sql_stmt,1,100));
-- dbms_output.put_line(SUBSTR(sql_stmt,101,100));
   EXECUTE IMMEDIATE sql_stmt using ID;
   END LOOP;
   
   

--------------------------------Done !! ----------------------------------------   
   error_msg := ':COMPLETE:Projection:PROJECT_2007_TO_ALBERS is Complete';
   GZ_TOPOFIX.Track_App(error_msg,Topology,log_type);
END;

PROCEDURE PROJECT_TO_ALBERS(pTable_name VARCHAR2,pGeometry_Column VARCHAR2,pID_Column VARCHAR2,WrkArea_Table VARCHAR2,pSCHEMA_NAME VARCHAR2,pOutput_Table VARCHAR2,pNew_Geom_Column VARCHAR2 default 'NEW_GEOMETRY',EXCLUDE VARCHAR2 DEFAULT NULL) AS

    TYPE                   TblCursorType   IS REF CURSOR;
    deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.  
    Table_name             VARCHAR2(100) := UPPER(pTable_Name);
    Schema_name            VARCHAR2(100) := UPPER(pSchema_Name);
    Geometry_Column        VARCHAR2(100) := UPPER(pGeometry_Column);
    OutputProject_Table    VARCHAR2(100) := UPPER(pOutput_Table);
    sql_stmt               VARCHAR2(4000);
    sql_stmt2              VARCHAR2(4000);

     Table_cursor          TblCursorType ;
     Geometry_Array        GZ_TYPES.SDO_GEOMETRY_ARRAY;
     ProjGeometry_Array    GZ_TYPES.SDO_GEOMETRY_ARRAY;
     Empty_GArray          GZ_TYPES.SDO_GEOMETRY_ARRAY := GZ_TYPES.SDO_GEOMETRY_ARRAY();
     Ids                   VCHAR100_ARRAY;
     CompIds               MDSYS.SDO_LIST_TYPE;

     Areas                 MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
     LLxs                  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
     LLys                  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
     URxs                  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
     URys                  MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
     row_limit             PLS_INTEGER := 100;
     area                  NUMBER;
     SRID                  NUMBER;
     loop_count            PLS_INTEGER := 0;
     LLx                   NUMBER;
     LLy                   NUMBER;
     URx                   NUMBER;
     URy                   NUMBER;
     xshift                NUMBER;
     yshift                NUMBER;
     scale                 NUMBER;
     theta                 NUMBER;
     sin0                  NUMBER :=0.;
     cos0                  NUMBER :=0.;
     x                     NUMBER;
     y                     NUMBER;
     x1                    NUMBER;
     y1                    NUMBER;
     xnew                  NUMBER;
     ynew                  NUMBER;
 
     TEST_IT               NUMBER := 0.;
BEGIN


     LLxs.extend(4);
     LLys.extend(4);
     URxs.extend(4);
     URys.extend(4);
     Areas.extend(row_limit);
 
     
  -- Most of the data is from the contiguous USA - area 2

-- Choose area numbers of 1 for ALASKA, 2 for CONTIGUOUS USA, 3 for HAWAII
-- and 4 for PUERTO RICO (North to south).


-- Now project in order:                   LLx      LLy           URx      URy
--                       1 Alaska,                  > 50.N                < 72.N
--                       2 USA            >-125W    > 19.N      <-65W     < 50.N
--                       3 Hawaii         >-162.W   > 19.N      <-154W    < 23.N
--                       4 Puerto Rico    >-68.W    > 17.N      <-64W     < 19N

     LLxs(1) := 0.;   -- zeros not used
     LLys(1) := 50.;
     URxs(1) := 0.;
     URys(1) := 72.;
     LLxs(2) := -125.;
     LLys(2) := 19.;
     URxs(2) := -65.;
     URys(2) := 50.;
     LLxs(3) := -161.; -- -161.; gets rid of small islands or -- -179. -- all
     LLys(3) := 18.;
     URxs(3) := -152.; 
     URys(3) := 29.; --23.;
     LLxs(4) := -68.;
     LLys(4) := 17.;
     URxs(4) := -64.;
     URys(4) := 19.;


     sql_stmt:= 'INSERT INTO ' || WrkArea_Table || ' SELECT t.COMPID,0,SDO_GEOM.SDO_MIN_MBR_ORDINATE(t.geometry,1),' ||
     'SDO_GEOM.SDO_MIN_MBR_ORDINATE(t.geometry,2),SDO_GEOM.SDO_MAX_MBR_ORDINATE(t.geometry,1),SDO_GEOM.SDO_MAX_MBR_ORDINATE(t.geometry,2) FROM ' ||
      Table_name || ' t ';
      if EXCLUDE is NOT NULL then
         sql_stmt := sql_stmt || 'where t.' || EXCLUDE;
      end if;
 --    dbms_output.put_line(SUBSTR(sql_stmt,1,100));
     EXECUTE IMMEDIATE sql_stmt;
   

     FOR varea in 1..4 LOOP
     
       area := varea;
       -- dbms_output.put_line(varea);
       LLx := LLxs(varea);
       LLy := LLys(varea);
       URx := URxs(varea);
       URy := URys(varea);
       
       if varea = 1 then
         sql_stmt:= 'UPDATE ' || WrkArea_Table || ' SET AREA=1 where LLy>:1 AND URy <:2'; 
         EXECUTE IMMEDIATE sql_stmt USING LLy,URy;
       else
       
         sql_stmt:= 'UPDATE ' || WrkArea_Table || ' SET AREA=:1 where LLx>:2 AND URx <:3 AND LLy>:4 AND URy <:5';
--         sql_stmt:= 'UPDATE ' || Area_Table || ' p SET p.AREA= (SELECT :1 AREA FROM ' ||
--                  Schema_name ||'.' || Table_Name || ' a where a.LLx>:2 AND a.LLy>:3 AND a.URx<:4 AND a.URy <:5 and a.FACE_ID = p.FACE_ID)' ||
--                  ' where exists (select 1 from ' || Schema_name ||'.' || Table_Name || ' a where a.LLx>:6 AND a.LLy>:7 AND a.URx<:8 AND a.URy <:9 and a.FACE_ID = p.FACE_ID)';
      
         EXECUTE IMMEDIATE sql_stmt USING area,LLx,URx,LLy,URy;
         
       end if;
       commit;
     END LOOP;
    
-- Now project in order:                   LLx      LLy           URx      URy
--                       1 Alaska,                  > 50.N                < 72.N
--                       2 USA                      > 19.N                < 50.N
--                       3 Hawaii         >-162.W   > 19.N      <-155W    < 23.N
--                       4 Puerto Rico    >-68.W    > 17.N      <-64W     < 19N

-- 
 
   FOR varea in 1..4 LOOP
     -- dbms_output.put_line('varea is ' || varea);
      if varea <> 1  then
        Close Table_cursor;
      end if;
 
      sql_stmt2 := 'SELECT p.compid,p.Id,p.geometry FROM ' ||Table_Name || ' p, '|| WrkArea_Table ||
                  ' a WHERE p.compid = a.compid and a.area =:1';
      OPEN Table_cursor FOR sql_stmt2 USING varea;
   --------------------------------------------------------------------------
  
      Geometry_Array := Empty_GArray;
      sin0 := 0.;
-- Special SRIDs in feet, Albers 
      if varea = 1 then
         SRID := 1000081;
         scale := 0.500063;
         theta := 0.41260 * deg2rad; --+0.317895 * deg2rad;-- +0.317895 * deg2rad;
         sin0 := sin(theta);
         cos0 := cos(theta);
         xshift := -7405403.;
         yshift := 10182450.;
         x := -6351486.5;
         y := 1391585.25;
         x1 := 6546663.;
         y1 := 1482540.125;
 
      /*
         SRID := 1000002;
         scale := 0.499; --0.4998; --0.500019286;
         theta := +0.48 * deg2rad;-- +0.317895 * deg2rad;
         xcenter := 538187.4117;
         ycenter := 1536409.8557;  --1539409.8557;
         sin0 := sin(theta);
         cos0 := cos(theta);
         xshift := -6877633.;  --  -7410251.2; ---7410633.25;
         yshift := 11729398.6; --  10186398.6;    --10186398.6;  --10159752.54;
         x := -6351486.5;
         y := 1391585.25;
         x1 := 6546663.;
         y1 := 1482540.125;
*/         
--         SRID := 1000002;
--         scale := 0.4998; --0.500019286;
--         theta := +0.317895 * deg2rad;-- +0.317895 * deg2rad;
--         sin0 := sin(theta);
--         cos0 := cos(theta);
--         xshift := -7410251.2; ---7410633.25;
--         yshift := 10186398.6;    --10186398.6;  --10159752.54;
--         x := -6351486.5;
--         y := 1391585.25;
--         x1 := 6546663.;
--         y1 := 1482540.125;
      elsif varea =2 then
         SRID := 1000099.;
         scale := 1.;
         xshift := 0.;
         yshift := 0.;
         x := 1.;
         y := 1.;
         x1 := 1.;
         y1 := 1.;
      elsif varea = 3 then
         SRID := 1000015.;
         scale := 1.49250665;
         xshift := -13307164.42;
         yshift := 122757.154;
         x := -4097905.75;
         y := 170073.469;
         x1 := 4097728.75;
         y1 := 170073.469;
      elsif varea = 4 then
         SRID := 1000072;
         scale := 2.48; --2.49; --3.076923077;
         xshift := 6651000.; -- 6655600.; --6944300.4;
         yshift := -86800; ---88000.; -- -259507.434;
         x := -492000.;
         y := 5600.;
         x1 := 483000.;
         y1 := 5600.;
      end if;
      If test_IT = 1 then
      
        xnew := x*scale + xshift;
        ynew := y*scale + yshift;
        -- dbms_output.put_line('Xnew ' || ROUND(xnew,6) || ' ynew ' || ROUND(ynew,6));
        xnew := x1*scale + xshift;
        ynew := y1*scale + yshift;
        -- dbms_output.put_line('Xnew ' || ROUND(xnew,6) || ' ynew ' || ROUND(ynew,6));
      ELSE
      LOOP
        loop_count := loop_count + 1;       
-- Free memory to avoid a PGA memory spiral    
        if loop_count <> 1 then
           Geometry_Array.delete; 
           dbms_session.free_unused_user_memory; 
           Geometry_Array := Empty_GArray;    
         End if;

   -- Pull the Ids,and the geometries themselvces
         FETCH_GEOMETRIES(Table_cursor,Compids,Ids,Geometry_Array,row_limit);

         Exit when Ids.count = 0;
       
--  Now Project the geometries using the desired parameters.
         area := varea;
         ProjGeometry_Array := PROJECT_GEOMETRIES(SRID,scale,xshift,yshift,sin0,cos0,Ids,Geometry_Array);
         For ii in 1..row_limit Loop
            Areas(ii) := area; 
         end Loop;
         
         FORALL ii IN 1..Ids.count
             EXECUTE IMMEDIATE
             'INSERT INTO /*+ APPEND */ ' || OutputProject_table ||
              ' (Compid,Id,geometry) VALUES (:1,:2,:3)'
              USING Compids(ii),Ids(ii),ProjGeometry_Array(ii);

       commit;
      END LOOP;
 
      END IF;
    END LOOP;
 

END;
--
FUNCTION PROJECT_GEOMETRIES(SRID NUMBER,scale NUMBER,xshift NUMBER, yshift NUMBER,sin0 NUMBER,cos0 NUMBER,Face_ids IN OUT NOCOPY VCHAR100_ARRAY,
                           Geometry_Array IN OUT NOCOPY GZ_TYPES.SDO_GEOMETRY_ARRAY) RETURN GZ_TYPES.SDO_GEOMETRY_ARRAY IS

-- Method: Use 1000082 becuase of problems in definition of 1000081 (feet)

-- Reference: State Plane Coordinate Systems and the U.S. Survey Foot
--             http://www.vterrain.org/Projections/sp_feet.html
                           
    ProjGeometry_Array     GZ_TYPES.SDO_GEOMETRY_ARRAY := GZ_TYPES.SDO_GEOMETRY_ARRAY();
    Geometry               MDSYS.SDO_GEOMETRY;
    ProjGeometry           MDSYS.SDO_GEOMETRY;
    ProjXys                MDSYS.SDO_ORDINATE_ARRAY;
    sql_stmt               VARCHAR2(4000);
    to_srid                NUMBER := SRID;
    to_USFT                NUMBER := 39.37/12. * scale;
    x                      NUMBER;
    y                      NUMBER;
    totalx                 NUMBER := 0.;
    totaly                 NUMBER := 0.;
    
BEGIN
--   If SRID = 1000081. then
--     to_USFT := scale;
--   end if;
   ProjGeometry_Array.extend(Geometry_Array.count);
   
   For ii in 1..Geometry_Array.count Loop
     Geometry := Geometry_Array(ii);
     ProjGeometry := SDO_CS.TRANSFORM(Geometry,to_srid);
    
--     If SRID <> 1000082. then
      ProjXys := ProjGeometry.SDO_ORDINATES;
     For jj in 1..ProjXys.count Loop
       if MOD(jj,2) = 1 then
          x:= ProjXys(jj)*to_USFT;
          y := ProjXys(jj+1)*to_USFT;
          totalx := totalx + x;
          totaly := totaly + y;
          if sin0 <> 0. then
            ProjXys(jj) := x*cos0 -sin0*y + xshift;
            ProjXys(jj+1) := x*sin0 + cos0*y + yshift;
          else
            ProjXys(jj) := x + xshift;
            ProjXys(jj+1) := y + yshift;
          end if;
       end if;
     End Loop;
--     dbms_output.put_line('totalx ' || ROUND(totalx/ProjXys.count*2.,6) || ' totaly ' ||  ROUND(totaly/ProjXys.count*2.,6));
     --ProjGeometry_Array(ii) := MDSYS.SDO_GEOMETRY(ProjGeometry.SDO_GTYPE,ProjGeometry.SDO_SRID,NULL,
                              -- ProjGeometry.SDO_ELEM_INFO,ProjXys);
      ProjGeometry_Array(ii) := MDSYS.SDO_GEOMETRY(ProjGeometry.SDO_GTYPE,1000082,NULL,
                                ProjGeometry.SDO_ELEM_INFO,ProjXys);
 --    else
-- US 1000082 already in feet
--       ProjGeometry_Array(ii) := ProjGeometry;
--     end if;
   End Loop;
   
   RETURN ProjGeometry_Array;

END PROJECT_GEOMETRIES;
--
FUNCTION ANGLE(x0 NUMBER,y0 NUMBER,x1 NUMBER,y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER) RETURN NUMBER DETERMINISTIC IS

/*
**************************************************************************************
--Program Name: Angle
--Author: Sidey Timmins
--Creation Date: 08/01/2009
--Usage:
  -- Call this function from inside another PL/SQL program.  There are 6 parameters:
  --
  --   REQUIRED Parameters: 
  --           (x0,y0): coordinates in degrees of a vertex
  --           (x1,y1): coordinates in degrees of center vertex
  --           (x2,y2): coordinates in degrees of 3rd vertex
--
--                    (x1,y1) +---------------+ (x0,y0)
--                            |   included angle
--                            |
--                    (x2,y2) +
--
--Purpose:  Measure angles on the earth's surface between nearby points.    
-- Method:  Computes bearings and then calculates difference.     
-- Accuracy: typically less than ?? degrees.
--Dependencies: geo_bearing
***************************************************************************************
*/
--  rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   bearing1              NUMBER;
   bearing2              NUMBER;
   included_angle        NUMBER;
   oldb                  NUMBER;
BEGIN

        bearing2 := geo_bearing(x1,y1,x2,y2,oldb,SRID);

        if bearing2 < 0. then
          bearing2 := 360. + bearing2;
        end if;

-- Work out bearings all going towards x2,y2        
        bearing1 := geo_bearing(x0,y0,x1,y1,oldb,SRID);
        
        if bearing1 < 0. then
          bearing1 := 360. + bearing1;
        end if;
-- Now adding 180 here reverses bearing1 to its real sense (from point 1 to point 0)  
        included_angle := abs(mod(bearing1+180.,360.) - bearing2);
--        dbms_output.put_line('B1 ' || round(bearing1,3) || ' B2 ' || round(bearing2,3) || ' ia ' ||round(included_angle,3));
-- dbms_output.put_line('x0 ' || round(x0,6) || ' y0 ' || round(y0,6) );
-- dbms_output.put_line('x1 ' || round(x1,6) || ' y1 ' || round(y1,6) );
-- dbms_output.put_line('x2 ' || round(x2,6) || ' y2 ' || round(y2,6) );
        if included_angle > 180.0 then
          included_angle := 360. - included_angle;
        end if;
--   oldb := fast_atan2(y1-y0,x1-x0)* rad2deg;
--         dbms_output.put_line('atan1 ' || round(oldb,6) );
--          oldb := fast_atan2(y2-y1,x2-x1)* rad2deg;
--         dbms_output.put_line('atan2 ' || round(oldb,6) );
        RETURN included_angle;

END ANGLE;
--
FUNCTION Remove_Too_Straight(Geom IN OUT NOCOPY MDSYS.SDO_GEOMETRY,
                      Straight_len NUMBER default 1000000.,
                      Straight_angle NUMBER default 175.) RETURN PLS_INTEGER AS
 
-- This function removes vertices from a geometry that are less than straight_len apart and have angles greater than
-- Straight_angle. Defaults preserve lines 10 km long and remove vertices whose angles are greater than 175 degrees.
                     
   rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;
   Xys             MDSYS.SDO_ORDINATE_ARRAY := Geom.SDO_ORDINATES;

   xlast           NUMBER;
   ylast           NUMBER;
   x               NUMBER;
   y               NUMBER;
   y0              NUMBER;
   xtest           NUMBER;
   ytest           NUMBER;
   xycheck         NUMBER;
   xcheck          NUMBER;
   ycheck          NUMBER;
   distance        NUMBER;
   included_angle  NUMBER;
   includ_angle    NUMBER;
   last_angle      NUMBER := 0.;
   b1              NUMBER := NULL;
   b2              NUMBER;
   b3              NUMBER;
   xdistance       NUMBER;
   ydistance       NUMBER;
   dx              NUMBER;
   dy              NUMBER;
   b22             NUMBER;
   b33             NUMBER;
   f               NUMBER;
   len             NUMBER;
   delta           NUMBER := 0.1;
   SRID            NUMBER := Geom.SDO_SRID;
   wiggles         BOOLEAN;
   dropped         PLS_INTEGER := 0;
   kk              PLS_INTEGER;
   n               PLS_INTEGER;
   next            PLS_INTEGER := 2;
BEGIN

    n := TRUNC(Xys.count/2);
    xlast := Xys(1);
    ylast := Xys(2);
 /*   y0 := ylast;
    for ii in 1..2 Loop
      xtest := xlast + delta;
      ytest := ylast + delta;
      xdistance := GZ_UTIL_ZONE.accurate_gcd(xlast,ylast,xtest,ylast);
      ydistance := GZ_UTIL_ZONE.accurate_gcd(xlast,ylast,xlast,ytest);
      f := xdistance/ydistance;
      if ii = 1 then
        xycheck := sqrt(0.1*0.1*f*f + 0.1*0.1) * straight_len/ydistance*0.98;
      end if;
      delta := 0.005;
    end loop;
    */
--    dbms_output.put_line('f ' || round(f,8) || ' xycheck ' || round(xycheck,8));
   
    xtest := Xys(3);
    ytest := Xys(4);
    For ii in 2..n-1 Loop
          x := xtest;
          y := ytest;
          kk := (ii+1)*2;
          
          xtest := Xys(kk-1);
          ytest := Xys(kk);

          if xtest <> x or ytest <> y then
            dx := (xtest-xlast) * f;
            dy := ytest-ylast;
            len := sqrt(dx*dx  + dy*dy);
           distance := distance_fcn(xlast,ylast,xtest,ytest,SRID);
--            if len >= xycheck and distance < Straight_len then
 --            dbms_output.put_line('dist ' || round(distance,4) || ' len ' || Round(len,5));
--            end if;

            included_angle := angle(xlast,ylast,x,y,xtest,ytest,SRID);
--            b2 := GZ_UTIL_ZONE.geo_bearing(xlast,ylast,x,y);
--            b3 := GZ_UTIL_ZONE.geo_bearing(x,y,xtest,ytest);
 
--           included_angle := abs(mod(b2+180.,360.) - b3);
/*
--
--            if includ_angle > 180.0 then
--              includ_angle := 360. - includ_angle;
--            end if;
            if ABS(y - y0) > 0.05 then
                 y0 := y;
                 xcheck := x + delta;
                 ycheck := y + delta;
                 xdistance := GZ_UTIL_ZONE.accurate_gcd(x,y,xcheck,y);
                 ydistance := GZ_UTIL_ZONE.accurate_gcd(x,y,x,ycheck);
                 f := xdistance/ydistance;
            end if;
            b22 := GZ_UTIL_ZONE.fast_atan2(y-ylast,(x-xlast)*f) *rad2deg;
            if b22 < 0. then
              b22 := b22 + 360.;
            end if;

            b33 := GZ_UTIL_ZONE.fast_atan2(ytest-y,(xtest-x)*f) *rad2deg;
            if b33 < 0. then
              b33 := b33 + 360.;
            end if;
            included_angle := abs(mod(b22+180.,360.) - b33);
--*/
            if included_angle > 180.0 then
              included_angle := 360. - included_angle;
            end if;
--            b2 := b22;
--            b3 := b33;
--            dbms_output.put_line('b2 ' || round(b2,6) || ' b22 ' || round(b22,6) || ' D ' || ROUND(b2-b22,6));
--            dbms_output.put_line('b3 ' || round(b3,6) || ' b33 ' || round(b33,6) || ' D ' || ROUND(b3-b33,6));

            if distance < Straight_len then --len < xycheck then --distance < Straight_len then

-- Does the line wiggle back and forth or are we following a slow curve?
--            \                            ----
--             \-------                         \
--                      \                        |
--        
               wiggles := FALSE;
               if b1 is NOT NULL then
                  wiggles := TRUE;
                  if (b1 > b2 and b2 > b3) or (b1 < b2 and b2 < b3) then
-- But the line can be still very straight so ignore half of these
                    if MOD(ii,2) = 1 then
                     wiggles := FALSE;   -- we are following a curve.
                    end if;
                  end if; 
               end if;
-- Remove this point
-- dbms_output.put_line('dist ' || round(distance,4) || ' angle ' || ROUND(included_angle,6) || ' at ' || ii);
               if included_angle > straight_angle or     -- 178.2
               (wiggles and included_angle >= Straight_angle and last_angle >= Straight_angle) then
--                 dbms_output.put_line('RMoved dist ' || round(distance,4) || ' angle ' || ROUND(included_angle,6) || ' x ' ||x || ' at ' || ii);
                  dropped := dropped + 2;
--                  dbms_output.put_line('dropped ' || ii);
               else
                  next := next + 2;
                  if next <> kk then
                    Xys(next-1) := x;
                    Xys(next) := y;
                  end if;
                  xlast := x;
                  ylast := y;
               end if;               
            else
                next := next + 2;
                if next <> kk then
                 Xys(next-1) := x;
                 Xys(next) := y;
                end if;
                xlast := x;
                ylast := y;
            end if;
--            dbms_output.put_line('next ' || next || ' kk ' || kk);

            b1 := b2;
            last_angle := included_angle;
          end if;

    End Loop;
    next := next + 2;
    Xys(next-1) := Xys(XYs.count-1);
    Xys(next) := Xys(Xys.count);
    If dropped <> 0 then
       Xys.trim(dropped);
--         dbms_output.put_line('dropped ' || dropped || ' count ' || Xys.count || ' next ' || next);
       Geom.SDO_ORDINATES := Xys;
    End If;
   
       RETURN dropped;  

END;
PROCEDURE try_remove_too_straight(Geom MDSYS.SDO_GEOMETRY default NULL, pstraight_angle NUMBER default 179.9,px NUMBER default NULL,py NUMBER default 40.,pxdelta NUMBER default NULL, pydelta NUMBER default NULL)
AS
     Geom2    MDSYS.SDO_GEOMETRY;
     Xys      MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY(); 
     xdelta   NUMBER := pxdelta;
     ydelta   NUMBER := pydelta;
     x        NUMBER;
     y        NUMBER := py;
     SRID     NUMBER := 82650.0;
     Straight_len NUMBER := 1000000.;
     Straight_angle NUMBER := pStraight_angle;
     dropped  PLS_INTEGER;
     str      VARCHAR2(32000);
     p        PLS_INTEGER := -99;

BEGIN
     if Geom is NULL then
        Xys.extend(800);
        xys(1) := -100000.;
        xys(2) := y * 100000;
        xdelta := 0.01 * 10000.;
        ydelta := 0.000008 * 10000.;
        Straight_angle := 179; --179.98; --999984;
-- First try a shallow curve going horizontal (ydelta about the same)
        for ii in  3..Xys.count Loop
           if MOD(ii,2) = 1 then
             Xys(ii) := Xys(ii-2); 
           else
             if ii <= 400 then
               ydelta := 0.00008*abs(ii-200)*10000;
             else
               ydelta := 0.00008*abs(ii-600)*1000;
             end if;
             Xys(ii) := y *100000 + ydelta;
           end if;
 
        end loop;
     else
        Xys := Geom.sdo_ordinates;
     end if;

--     For ii in 1..Xys.count Loop
--        dbms_output.put_line('II ' || ii || ' Bxy ' || xys(ii));
--     end loop;
 
     Geom2 := MDSYS.SDO_GEOMETRY(2002,8265,NULL,mdsys.sdo_elem_info_array(1,2,1),Xys);
     dropped := Remove_Too_Straight(Geom2,Straight_len,Straight_angle); 
     dbms_output.put_line('dropped ' || dropped );
     Xys := Geom2.sdo_ordinates;
     str := 'MDSYS.SDO_GEOMETRY(2002,8265,NULL,SDO_ELEM_INFO_ARRAY(1,2,1),SDO_ORDINATE_ARRAY(';
     For ii in 1..Xys.count Loop
--        dbms_output.put_line('ii '|| ii || ' xy ' || xys(ii));
        str := str || ',' ||xys(ii);
     end loop;
     str :=str || '))';
     for ii in 1..TRUNC(Length(str)/100)+1 loop
        p := p + 100;
        dbms_output.put_line(SUBSTR(str,p,100));
     end loop;
END try_remove_too_straight;
--

--
PROCEDURE FETCH_GEOMETRIES(Table_cursor IN OUT NOCOPY SYS_REFCURSOR,
                           Compid_Array IN OUT NOCOPY MDSYS.SDO_LIST_TYPE,
                           OID_Array IN OUT NOCOPY VCHAR100_ARRAY,
                           Geometry_Array IN OUT NOCOPY GZ_TYPES.SDO_GEOMETRY_ARRAY,                    
                           row_limit PLS_INTEGER)
/**
********************************************************************************
* Program Name: fetch_geometries
* Author: Sidey Timmins
* Creation Date: 09/12/08
* Usage: 
*   
*     REQUIRED Parameters:
*        table_cursor - an open cursor to fetch 
*        Oid_Array  - an array to receive the Oids of the fetched geometries
*        Geometry_Array - An SDO geometry array.
*        row_limit  - a limit for the fetch (must not be too big) 100 to 1000
*                     suggested
* Purpose: 
*         Fetches geometries into a geometry array. Avoid creating this
*         array at the callers level. It is believed this enables memory to be freed.
* Dependencies: none      
*     
********************************************************************************
*/ 
 AS

BEGIN

   FETCH Table_cursor BULK COLLECT INTO Compid_Array,OID_Array,Geometry_ARRAY LIMIT row_limit;
   

END FETCH_GEOMETRIES;
--
FUNCTION GEO_BEARING(px0 NUMBER,py0 NUMBER,px1 NUMBER,py1 NUMBER,old_bearing IN OUT NOCOPY NUMBER,SRID NUMBER)
                RETURN NUMBER DETERMINISTIC IS
/*
**************************************************************************************
--Program Name: Geo_Bearing
--Author: Sidey Timmins
--Creation Date: 03/19/2009
--Updated:  04/07/2010 To use same method as chainer with Great circle bearings
--Usage:
  -- Call this function from inside another PL/SQL program.  
  -- There are 2 ordered pairs describing a vector:
  --
  --   REQUIRED Parameters: 
  --           (px1,py1): coordinates in degrees of 1st vertex
  --           (px2,py2): coordinates in degrees of 2nd vertex
--
--                                 + (x2,y2)
--                               /
--                             /    Bearing is counter clockwise angle from East
--                    (x1,y1) +---------------+ X axis
--                           

--
--Purpose:  Approximates horizontal angles on the earth's surface between nearby 
--          points with geodetic coordinates. It does not calculate spherical angles.
--
-- Method:  Computes x and y and then calculates bearing. Always computes
--          going North or East and then reverses direction if necessary.
--
-- Accuracy: Typically less than 1.e-12 degrees compared to the azimuth formula.
-- This is the initial bearing (sometimes referred to as forward azimuth) which  
--if followed in a straight line along a great-circle arc will take you from the
--start point to the end point:
-- ¿ = atan2( sin(¿long).cos(lat2),cos(lat1).sin(lat2) ¿ sin(lat1).cos(lat2).cos(¿long) ) 

* Reference: http://www.movable-type.co.uk/scripts/latlong.html
*    Personal communication from Peter H. Dana
*    http://www.colorado.edu/geography/gcraft/notes/mapproj/mapproj_f.html
*
* Dependencies:
*    GEN_UTIL_ZONE.atan2,GEN_UTIL_ZONE.sincos
***************************************************************************************
*/
   deg2rad   CONSTANT NUMBER   :=0.0174532925199432957692369076848861271344;  -- pi/180.  
   rad2deg   CONSTANT NUMBER   := 57.29577951308232087679815481410517033235;

--   a2          NUMBER   := 40680631590769.;               -- ellipsoid radius ^2
--   b2          NUMBER   := 40408299983328.76931725432420763255772;
   e2          NUMBER := 0.006694380022900787625359114703055206936; --1. - b2/a2;
   x0                    NUMBER := px0;
   y0                    NUMBER := py0;
   x1                    NUMBER := px1;
   y1                    NUMBER := py1;

   dx                    NUMBER;
   dy                    NUMBER;
   x1_0                  NUMBER;
   cosx1_0               NUMBER;
   sinx1_0               NUMBER;
   siny0                 NUMBER;
   siny1                 NUMBER;
   cosy1                 NUMBER;
   cosy0                 NUMBER;
   tany0                 NUMBER;
   Bearing               NUMBER;
   Bearingc              NUMBER;
   cotphi                NUMBER;
   Lambda12              NUMBER;
   tanpsi2               NUMBER;
   flip                  PLS_INTEGER := 0;

BEGIN

-- To get consistent bearings independent of direction,always choose the smallest y
     if ABS(y1) < ABS(y0) then
        flip := 1;
        x0 := x1;
        y0 := y1;
        x1 := px0;
        y1 := py0;
     end if;
     
-- Here we setup the change in x and y from the most recent vertex     
     dx := x1 - x0;
     dy := y1 - y0;

     if dx = 0. and dy = 0. then
        RETURN NULL;
     end if;

    If SRID <> 8265. Then
       if flip = 1 then
         dy := -dy;
         dx := -dx;
       end if;
       bearing := fast_atan2(dy,dx) * rad2deg;
    ELSE
    -- Convert to radians and then do a local secant projection
--        dbms_output.put_line('x0 ' || round(x0,7) || ' y0 ' || round(y0,7));
--         dbms_output.put_line('x1 ' || round(x1,7) || ' y1 ' || round(y1,7));
      y0 := y0* deg2rad;
      x1_0 := (x1-x0)* deg2rad;
      y1 := y1 * deg2rad;

-- Set up constants for the projection at sin_center_lat and cos_center_lat:
-- Note: since we just want the arctan of Y/X,  the multiplication by
--       ksp cancels. So we don't need to calculate g and ksp !
--==     Commented code has 2 dashes and 2 == below.

--        g = sin_center_lat * sin(lat) + cos_center_lat * cos(lat) * cos(dlon)
--==      g := sin(y0) * sin(y1) + cos(y0) * cos(y1) * cos(x1-x0);
--==      ksp := 2. / (1.0 + g);

-- Project X1, Y1
-- X = ksp * cos(lat) * sin(dlon)
-- Y = ksp * ( cos_center_lat * sin(lat) - sin_center_lat * cos(lat) * cos(dlon)

        siny0 := sincos(y0,cosy0);
        siny1 := sincos(y1,cosy1);
        sinx1_0 := sincos(x1_0,cosx1_0);

        dx := cosy1 * sinx1_0;
        dy := (cosy0 * siny1 - siny0 * cosy1 * cosx1_0);

-- Maintain the way the line was going, from zero to 1 to 2;
       if flip = 1 then
         dy := -dy;
         dx := -dx;
       end if;

-- Cunningham's formula (page 120 from G. Bomford)
       
       bearing := fast_atan2(dy,dx) * rad2deg;
       if sinx1_0 = 0.0 or cosy1 = 0.0  then
          bearing := fast_atan2(dy,dx) * rad2deg;
--          dbms_output.put_line('= zero  ' || bearing);
       else
--         tany0 := tan(y0);
--         lambda12 := (1.- e2) * tan(y1)/tany0 + e2 * (cosy0/cosy1);
--         cotphi := (lambda12  - cosx1_0) * siny0/sinx1_0;
         tanpsi2 := (1.- e2) * tan(y1) + e2 * (siny0/cosy1);
         cotphi := (cosy0*tanpsi2  - siny0*cosx1_0)/sinx1_0;
         if cotphi <> 0.0 then
           bearingc := 90.-fast_atan2(1./cotphi,1.) *rad2deg;
           if dy < 0. then
             bearingc := bearingc -180.;
           end if;
           if abs(bearingc - bearing) > 0.2 then           
              dbms_output.put_line('bc ' || round(bearingc,6) || ' b1 ' || round(bearing,6));
           end if;
           if bearing < 0. then
             bearing := 360. + bearing;
           end if;
           old_bearing := bearing;
           bearing := bearingc;
         else
-- Atan2 is very slow (remember 38 digits of precision) so use
-- a very accurate approximation and convert to degrees.
           bearing := fast_atan2(dy,dx) * rad2deg;
         end if;
       end if;
      END IF;
      if bearing < 0. then
          bearing := 360. + bearing;
      end if;

      RETURN Bearing;

 
END GEO_BEARING;
--
FUNCTION CONVERT_2007_TO_2003(Topology VARCHAR2,pInTable VARCHAR2,pInSDOGeomColumn VARCHAR2,pIdColumn VARCHAR2, pOutTable VARCHAR2,Compid_fld VARCHAR2 default 'COMPID',seq_name VARCHAR2) RETURN NUMBER AS
/**
********************************************************************************
* Program Name: convert_2007_to_2003 
* Author: Sidey Timmins
* Creation Date: 07/18/08
* Usage: 
* Example usage:
* convert_2007_to_2003('COUNTY','SDOGEOMETRY','FACE_ID','OUTPUT_TABLE')
*
* REQUIRED Parameters:
*             pInTable - The name of a table with a sdogeometry
*             pInSDOGeomcolumn - the name of the input SDO Geometry column 
*             pInFace_IdColumn - name of the input Face_Id column
*             poutTable - The name of the output table
*             Compid_Fld - The name to give the unique ID column
* Purpose: 
*           Converts 2007 polygon geometries to 2003 polygon geometries. Assigns
*           Assigns each record a unique Id (COMPID) and a Num_Pieces count.
*
* Method: Moves the 2003 geometries to the output table and then extracts just
*         the 2007s.
* Called_by: 
* Dependencies: none
********************************************************************************
*/

 TYPE TblCursorType IS REF CURSOR;

 Table_cursor TblCursorType;

 Geometry_Array         GZ_TYPES.SDO_GEOMETRY_ARRAY := GZ_TYPES.SDO_GEOMETRY_ARRAY();
 Out_Geometry_Array     GZ_TYPES.SDO_GEOMETRY_ARRAY := GZ_TYPES.SDO_GEOMETRY_ARRAY();
 Compids                MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
 Ids                    VCHAR100_ARRAY := VCHAR100_ARRAY();
 Out_ids                VCHAR100_ARRAY := VCHAR100_ARRAY();
 NumPieces_Array        MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
 Out_NumPieces          MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE(); 
 Empty_GArray           GZ_TYPES.SDO_GEOMETRY_ARRAY := GZ_TYPES.SDO_GEOMETRY_ARRAY();
 Empty_NArray           MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
 
 
 InSDOGeomColumn        VARCHAR2(100) := UPPER(pInSDOGeomColumn);
 IdColumn               VARCHAR2(100) := UPPER(pIdColumn);
 InTable                VARCHAR2(100) := UPPER(pInTable);
 out_Table              VARCHAR2(100) := UPPER(pOutTable);
 sql_stmt               VARCHAR2(4000);
 
 
 SRID            NUMBER := -1;
 row_limit       PLS_INTEGER := 100;
 compid          PLS_INTEGER;
 extend_amount   PLS_INTEGER := row_limit*2;
 geom            MDSYS.SDO_GEOMETRY;
 geom2           MDSYS.SDO_GEOMETRY;
 k               PLS_INTEGER;
 num_elements    PLS_INTEGER;
 loop_count      PLS_INTEGER;
 kount           NUMBER := 0.;

BEGIN

-- Sequence is already created
   
-- make a table without 2007 geometries. 
--  The long dissolve field is eplace with a number and a columns determining
-- whether the geometry needs to be dissolved is added
-- COMPID         OID       MTFCC           SDOGEOMETRY         NEEDS_DISSOLVE
---     1       4441234     1               MDSYS.SDO_GEOMETR(...            1
---     2       4441356     3               MDSYS.SDO_GEOMETR(...            1
---     3       4441355     2               MDSYS.SDO_GEOMETR(...            0



   sql_stmt := 'INSERT INTO /*+ APPEND */ ' || Out_Table ||' (COMPID,ID,GEOMETRY,NUM_PIECES) SELECT ' || 
               seq_name ||'.nextval,t.'||IdColumn||
               ',t.' || InSDOgeomColumn ||',1 FROM ' ||Intable||' t WHERE ' || 
               ' sdo_util.GetNumElem(t.' ||InSDOgeomColumn ||') = 1';
    dbms_output.put_line(substr(sql_stmt,1,100));
    dbms_output.put_line(substr(sql_stmt,101,100));
   EXECUTE IMMEDIATE sql_stmt; 

 
   sql_stmt := 'SELECT ' || seq_name ||'.nextval from dual';
   EXECUTE IMMEDIATE sql_stmt into compid;
   compid := compid -1;
   kount := compid;
   
-- Now select just the 2007s and break them apart 
   sql_stmt := 'SELECT t.'||IdColumn||',1 NUM_PIECES,t.' || InSDOgeomColumn ||' FROM ' ||
                Intable||' t WHERE ' ||' sdo_util.GetNumElem(t.' ||InSDOgeomColumn ||') > 1';


   OPEN Table_cursor FOR sql_stmt;

   Compids.extend(extend_amount);
   Out_ids.extend(extend_amount);
   Out_NumPieces.extend(extend_amount);
   Out_Geometry_Array.extend(extend_amount);
   
 LOOP 
   loop_count := loop_count + 1;

-- Free memory to avoid a PGA memory spiral
   if loop_count <> 1 then 
     Geometry_Array := Empty_Garray;
     dbms_session.free_unused_user_memory;
   End if;
  
   FETCH Table_cursor BULK COLLECT INTO Ids,NumPieces_Array,Geometry_ARRAY LIMIT ROW_LIMIT;
---123
   Exit when ids.count = 0;

   k := 0;
   For i in 1..Geometry_Array.count Loop
     geom := Geometry_Array(i);
     SRID := geom.sdo_srid;
     num_elements := sdo_util.GetNumElem(geom);
     kount := kount + 1.;
     If k +num_elements > Out_Geometry_Array.count then
        if num_elements > extend_amount then
           extend_amount := num_elements;
        end if;
        Compids.extend(extend_amount);
        Out_Geometry_Array.extend(extend_amount);
        Out_ids.extend(extend_amount);
        Out_NumPieces.extend(extend_amount);
     End If;
-- extract the other rings (if any) and give each one a separate and unique
-- compid


   For j in 1..num_elements Loop
     k := k + 1;
     compid := compid +1;
     Compids(k) := compid;
     Out_ids(k) := ids(i);
     Out_NumPieces(k) := NumPieces_Array(i);
     If num_elements <> 1 then
       geom2 := SDO_UTIL.EXTRACT(geom,j);
       Out_Geometry_Array(k) := geom2;
     Else
-- Is it possible to have a single ring 2007?
       Out_Geometry_Array(k) := geom;
     End If;
   End Loop;
---123
 End Loop;

-- Single transaction to output the 2003 geometries from one 2007 and then commit every so often 

-- usual case 
 FORALL i IN 1..k
    EXECUTE IMMEDIATE
    'INSERT INTO /*+ APPEND */ ' || Out_Table || '(COMPID,ID,' ||
                                    'NUM_PIECES,GEOMETRY) VALUES (:1,:2,:3,:4)'
    USING Compids(i),Out_ids(i),Out_NumPieces(i),Out_Geometry_Array(i);
 
 If MOD(loop_count,6) = 5 then
   commit;
 End If;
 
 END LOOP;
 commit;
 

 Geometry_Array := Empty_Garray; 
 Out_Geometry_Array := Empty_Garray;
 Compids := Empty_Narray;
 Out_ids := VCHAR100_ARRAY();
 
 If SRID = -1 then
   EXECUTE IMMEDIATE 'SELECT t.geometry.SDO_SRID FROM '|| Out_Table || 
                     ' t WHERE ROWNUM = 1' into SRID;
 End If;
 RETURN kount;
 

END CONVERT_2007_TO_2003;
--
PROCEDURE Add_geom_metadata(pTableName VARCHAR2,pColumnName VARCHAR2,pInSchema VARCHAR2,SRID NUMBER,xLL NUMBER default -180,yLL NUMBER default -90.,xUR NUMBER default 180.,yUR NUMBER default 90.,tolerance NUMBER default 0.005) AS
/*****************************************************************************************************************
-- Program Name: add_geom_metadata_8265
-- Author: Nick Padfield
-- Updated: 10/20/2010 Sidey Timmins to handle other SRIDs when user has the extent
--                     and not add dummy entries for geometry columns that don't exist!!
-- Creation Date: 5/31/2006

-- Usage: (How do I call this program and what parameters does it need?)
     -- Call this program from inside another PL/SQL program.  This program
     -- requires two parameters:
     --   pTableName  - a Table Name
     --   pColumnName - a column name that you want to register with the USER_SDO_GEOM_METADATA table

-- Purpose: (What does this program do or what values does it return?)
     -- The purpose of this table is to register a spatial column with the USER_SDO_GEOM_METADATA table (with SRID = 8265).

-- Dependencies: NOTE!! This procedure expects the column to exist and doesn't 
--           check if the table has the column!
*****************************************************************************************************************
*/
   RowsSelected   PLS_INTEGER := 0;
   InSchema       VARCHAR2(100) := UPPER(pInSchema);
   TableName      VARCHAR2(100) := UPPER(pTableName);
   ColumnName     VARCHAR2(100) := UPPER(pColumnName);
   sql_stmt       VARCHAR2(4000);
   xname          VARCHAR2(20) :='X';
   yname          VARCHAR2(20) :='Y';
   RecordCount    NUMBER;
BEGIN
   ---------------------------------------------------------------------------------------
   
-- Check to see if the COLUMN exists!
   sql_stmt := 'SELECT COUNT(*) FROM all_tab_columns WHERE owner = :1 AND table_name = :2 AND column_name = :3';
   EXECUTE IMMEDIATE sql_stmt INTO RecordCount USING InSchema,TableName,ColumnName;
   IF (RecordCount = 0) THEN
      dbms_output.put_line('WARNING: the '||ColumnName||' column does not exist in the '||InSchema||'.'||TableName||' table');
      RETURN;
    END IF;
   
   -- Check to see if this geometry is already registered
   SELECT COUNT(rownum)
   INTO RowsSelected
   FROM user_sdo_geom_metadata
   WHERE table_name = TableName AND column_name = ColumnName;
   -----------------------------------
   IF (RowsSelected > 0) THEN
      dbms_output.put_line('There were ' || RowsSelected || ' pre-existing records found in the USER_SDO_GEOM_METADATA table.');
      DELETE FROM user_sdo_geom_metadata
      WHERE table_name = TableName AND column_name = ColumnName;
      COMMIT;
      dbms_output.put_line('These pre-existing records were deleted');
   END IF;
 
   RowsSelected := 0;
   ---------------------------------------------------------------------------------------
   -- Add a record to the metadata table
   /*Additional notes about insertion:
   If v_SRID is that of a geodetic coordinate system (ie. 8265) then v_Xmin must equal -180,
   v_Xmax must equal 180, v_Ymin must equal -90, and v_Ymax must equal 90.  Additionally,
   the units for the tolerance for geodetic SRID's is METERS.  Hence, a tolerance value of
   0.005 is precise to 5 millimeters.
   */
   IF SRID = 8265 then
      xname := 'Longitude';
      yname := 'Latitude';
   end if;
   INSERT INTO USER_SDO_GEOM_METADATA
      (TABLE_NAME, COLUMN_NAME, DIMINFO, SRID)
      VALUES (
         TableName,
         ColumnName,
         MDSYS.SDO_DIM_ARRAY (
            MDSYS.SDO_DIM_ELEMENT(xname,xLL,xUR,tolerance),
            MDSYS.SDO_DIM_ELEMENT(yname,yLL,yUR,tolerance)),
            SRID);
   COMMIT;
   ----------------------------------------------------------------------------------------
   -- Check to see if the geometry was registered correctly
   SELECT COUNT(rownum)
   INTO RowsSelected
   FROM user_sdo_geom_metadata
   WHERE table_name = TableName AND column_name = ColumnName;
   IF (RowsSelected = 1) THEN
      dbms_output.put_line(TableName || '.' || ColumnName || ' was successfully registered!');
   ELSE
      dbms_output.put_line('WARNING: ' || TableName || '.' || ColumnName || ' was NOT successfully registered!');
   END IF;
   ----------------------------------------------------------------------------------------
END Add_geom_metadata;
--
Function Distance_Fcn(x1 NUMBER, y1 NUMBER,x2 NUMBER,y2 NUMBER,SRID NUMBER default 8265.) Return NUMBER IS

/*
********************************************************************************
--Program Name: distance_fcn
--Author: Sidey Timmins
--Creation Date: 04/15/2011
--Usage:
  -- Call this function from SQL or inside a PL/SQL program.
  
  --   REQUIRED Parameter: 
  --        INPUT
  --             x1,y1:  vertex coordinates in degrees (or some other units
  --                     if SRID <> 8265).
  --             x2,y2:  2nd vertex coordinates in degrees (or some other units
  --                     if SRID <> 8265.)
  --
--Purpose:   -- Calculate the great circle distance in meters between two close 
--              vertices with geodetic coordinates ( if the SRID is not 8265 it
--              calculates the Pythagorean hypotenuse for 2 vertices).
--              Note it closely approximates the values given by the Vincenty 
--              formula - not those given by Oracle length - although they 
--              should be the same. Absolute accuracy:
--               For differences < 0.02 degrees, error is < 0.0004 meters.
--                               <= 0.1 degrees, error is < 0.02 meters 
--              Relative accuracy which is < 0.00001 at .1 degrees improves
--              to < 0.0000005 at 0.000001 degrees.
-- Method:      Uses empirically derived constants to approximate the distance
--              using the arcsin formula for the GCD. The constants were
--              derived in part by Rosenbrock optimization and a HP15c calculator!
-- Reference:   http://en.wikipedia.org/wiki/Great_circle_distance
--
--Dependencies: sincos  (a fast sin and cos function)
********************************************************************************
*/                                        
   deg2rad              NUMBER   :=0.0174532925199432957692369076848861271344;
   dx       NUMBER;
   dy       NUMBER;
   ysin     NUMBER;
   ycos     NUMBER;
   ycos2    NUMBER;
   ysin2    NUMBER;
   cosdy    NUMBER;
   sinysq   NUMBER;
   cos3y    NUMBER;
   cos6y    NUMBER;

   a1       NUMBER :=  0.99330564310853556;
   a2       NUMBER :=  0.0099739720896542635;
   a3       NUMBER :=  0.0000844513348186908111;
   a4       NUMBER := -0.0000000207345260587865496;
   delta    NUMBER;
   sindelta NUMBER;
   yfactor  NUMBER;  --   0.9933;
   xfactor  NUMBER;  --  1.0
   dist_in_meters NUMBER;
   dist_factor NUMBER := 111319.490793274;
   
Begin 
    
  dx := x2-x1;
  dy := y2-y1;
  
  If SRID = 8265. Then
-- This code is derived from Accurate_GCD and has been tested against 
-- Charles Karney's code in the geodesic package. The results are amazing.
-- For lat/long differences < 0.1 degrees, the error is < 0.01 meters.
-- For differences < 0.02 degrees, the distance error is < 0.0004 meters.
-- Provided lat/long differences are < 0.1 degrees, the relative error is 
-- less than 1 part in 1 million.
          ysin := sincos(y1*deg2rad,ycos);
          delta := (y2-y1)*0.5*deg2rad;           -- calculate sin((y1+y2)*0.5)
          ycos2 := ycos - ysin * delta*2.;        -- small angle approximation for cos(y2)
          cosdy := 1. - delta*delta*0.5;          -- using sin(a+eps) = sin(a)cos(eps) + cos(a)sin(eps)
          sindelta := delta - delta*delta*delta/6.; -- make sin of delta using Taylor series
          ysin2 := ysin*cosdy + ycos*sindelta;    -- small angle approximation for formula above
          delta := dy*deg2rad;
          ycos2 := ycos - ysin * delta;           -- small angle approximation for cos(y2) 

--------------------------------------------------------------------------------
--        NEW The 4 a coefficents replace all of the code above for a1,a2
          sinysq := ysin2*ysin2;
          cos3y := ycos*(1.-4.*sinysq);
          cos6y := 2.*cos3y*cos3y -1.;
          dy := dy * (a1+ a2*sinysq + a3*sinysq*sinysq + a4*cos6y);
--------------------------------------------------------------------------------
--          yfactor := 0.9933 + 0.010025*ysin2*ysin2;
--          dy := (y2-y1)*yfactor;
--          xfactor := (1. + 0.00335*ysin*ysin);
          xfactor := 0.00335;
          if abs(y1) > 25. then
               xfactor := xfactor + 2.8E-07*(abs(y1)-25.);
          else
             xfactor := xfactor - 1.5E-07*(25.-abs(y1));
          end if;
--          if abs(x2-x1) > 0.04 then
--               xfactor := xfactor + (y2-y1)/abs(x2-x1) * 7.E-06;
--          end if;
            if abs(x2-x1) > 0.04 and abs(y2-y1) > 0.04 then
               xfactor := 1. + xfactor*ysin*ysin;
               if abs(y1) > 10. then
                 xfactor := xfactor + ysin*abs(x2-x1) * 3.5E-05;
               end if;
            else
          xfactor := (1. + xfactor*ysin*ysin);
            end if;
          dx := dx*xfactor;
          dist_in_meters := sqrt(dx*dx*ycos*ycos2 + dy*dy) * dist_factor;
  Else
          dist_in_meters := sqrt(dx*dx+ dy*dy);
  End if;
  
  Return dist_in_meters;
 
End;
--
FUNCTION FAST_ATAN2(YIn NUMBER, XIn NUMBER) RETURN NUMBER Deterministic IS
/*
**************************************************************************************
--Program Name: fast_atan2
--Author: Sidey Timmins
--Creation Date: 11/16/2006
--Updated: 8/19/09 comments and added constants
--Usage:
  -- Call this function from inside another PL/SQL program.  There are 2 parameters:
  --
  --   REQUIRED Parameters: 
  --            Y:  Y ordinate (vertical coordinate) - as per convention Y comes first!
  --            X:  X abscissa (horizontal coordinate)
  --
--Purpose:   -- Calculates the arctan function (angle that has the tangent =y/x)
--                             atan2(y,x)
--                                            +
--                               angle        |  y
--                           +________________+
--                                   x
--              as fast as possible (>20 times faster than the built in 
--              function atan2 which takes 4 seconds for 20000 values) with 
--              an accuracy of about 10 digits.
--              The domain is unlimited. (Note this function gives no error
--              and returns zero when both x and y are zero. The result should
--              be undefined).
-- Method:      where x = ABS(Yin/XIn) the raw atan is:
--         atan(x)= x(c1 + c2*x**2 )/(c3+c2)
--         and then it its adjusted for the quadrant.

-- Accuracy:  The maximum error in the range - two pi to two pi is less than
--            4.6E-13. Relative error is less than 3E-12.
                 
-- Reference: http://www.ganssle.com/approx/approx.pdf
--            "A Guide to Approximations" by Jack G Ganssle
--     -      "A Sequence of Polynomials for Approximating Arctangent" by Herbert A Medina
--Dependencies: None but see c_test_atan2 for rigorous testing.
***************************************************************************************
*/
  
  pi        NUMBER             := 3.1415926535897932384626433832795028842;
  piByTwo   NUMBER             := 1.5707963267948966192313216916397514421;
  piBy6     NUMBER             := 0.5235987755982988730771072305465838140;
  tanpiby6  NUMBER             := 0.5773502691896257645091487805019574557;
  tanpiby12 NUMBER             := 0.2679491924311227064725536584941276331;
  piBy12    CONSTANT NUMBER    := 0.2617993877991494365385536152732919070167;
  tanpiby24 CONSTANT NUMBER    := 0.1316524975873958534715264574097171035924;  
  c1        NUMBER             := 1.6867629106;
  c2        NUMBER             := 0.4378497304;
  c3        NUMBER             := 1.6867633134;
  a3        NUMBER  := 0.3333333333333333333333333333333333333333;
  a5        NUMBER  := -0.2;
  a7        NUMBER  := 0.1428571428571428571428571428571428571429;
--  a9        NUMBER  := -0.1041666666666666666666666666666666666667;  -- 5/48
   a9        NUMBER  := -0.109825;   -- by trial and error
  --a10       NUMBER  := 0.05;  -- Medina's h2(x) coeffs
  --a11       NUMBER  := 0.2443181818181818181818181818181818181818;
  --a12       NUMBER  := 0.25;
  --a13       NUMBER  := 0.12980769230769230769230769230769230769230;
  --a14       NUMBER  := 0.0357142857142857142857142857142857142857;
  --a15       NUMBER  := 0.004166666666666666666666666666666666666667;
  x         NUMBER; 
  x2        NUMBER;
  x3        NUMBER;
  x4        NUMBER;
  x5        NUMBER;
  result    NUMBER;

  complement BOOLEAN           := FALSE;
  region    NUMBER             := 0.0;
  region2   NUMBER             := 0.0;
  
BEGIN

/* arctan2(Y,X) is the quadrant-correct arc tangent atan(Y/X).  If the 
   denominator X is zero, then the numerator Y must not be zero.  All
   arguments are legal except Y = X = 0. This code returns zero for this case */
 
-- check for error conditions 
  IF XIn = 0.0 THEN
    IF YIn = 0.0 THEN 
      RETURN  0.0;    -- We return zero although arctan(0,0) is undefined
    ELSIF YIn < 0.0 THEN 
      RETURN -pibyTwo;
    ELSE 
      RETURN pibyTwo;
    END IF;
  END IF;
  

  IF ABS(YIn) > ABS(XIn) THEN
     x   := ABS(XIn)/ABS(YIn);
     complement := TRUE;
  ELSE
     x   := ABS(YIn/XIN);
  END IF;


-- reduce arg to under tan(pi/12)
  if (x > tanpiby12) THEN
     x := (x-tanpiby6)/(1.0 +tanpiby6*x); 
     region := 1;
  end if;

-- reduce arg to under tan(pi/24)

   if (x > tanpiby24) THEN
     x := (x-tanpiby12)/(1.0 +tanpiby12*x); 
     region2 := 1.;
  elsif  ( x < -tanpiby24) THEN
     x := (x+tanpiby12)/(1.0 -tanpiby12*x); 
     region2 := -1.;
  end if; 
   

  x2 := x * x;

  if x >-0.15 and x <= 0.15 then
    x3 := x * x2;
-- This is the usual path: use Medina's h2(x) sequence truncated 
     result := x -x3*(a3 + x2*(a5 + x2*(a7 + x2*a9))); --  +x5*x4*(x*a10 -x2*a11+ x3*a12-x4*a13 +x5*a14 - x3*x3*a15);
  else
-- Use Horner's rule to evaluate 
    dbms_output.put_line('x ' || round(x,12));
    result := (x*(c1 + x2*c2))/(c3 + x2);
  end if;
  IF region <> 0.0 THEN
     result  := result + piby6;
  END IF;
    IF region2 <> 0.0 THEN
     result  := result + piby12*region2;
  END IF;
  
  IF complement = TRUE THEN
     result := pibyTwo -result;
  END IF;
  
  IF XIn < 0.0 THEN 
      result := pi-result;
  END IF;
  
  IF YIn < 0.0 THEN 
      RETURN -result;
  ELSE 
      RETURN result;
  END IF;
  

END FAST_ATAN2;
--
FUNCTION sincos(InX NUMBER,COSX IN OUT NOCOPY NUMBER) RETURN NUMBER DETERMINISTIC AS
/**
********************************************************************************
* Program Name: sincos
* Author: Sidey Timmins
* Creation Date: 11/16/2006
*
* Usage:
*    Call this program from inside another PL/SQL program.  This program
*    has 2 parameters:
*  
*     REQUIRED Parameters: 
*              Input:            X:  X input value (in radians)
*              Output:        cosx: will contain cosine of X on output.
*  
* Purpose:    
*              Calculate simultaneously sin(x) and cos(x) (the sine and cosine 
*              functions) as fast as possible (3 times faster than the built in
*              functions that take 1 second for 20000 sine and cosine values)
*              with about 19 digits of accuracy. Worst is 15 digits of accuracy 
*              near zero and pi.
*
* Accuracy: 
*    Over the range - two pi to two pi by 1/10000 the maximum errors are:
*           sine       -.001884954475928113753023025004501920291813 correct
*           sincos     -.001884954475928117129897194923190991656619 very close
*           error       .000000000000000003376874169918689071364806
*           cosine      .8846460864518815658418479233550142026277  correct
*           cosx        .8846460864518815658354808691145121305329  very close
*           error       .0000000000000000000063670542405020720948
*    Near pi
*           sine       -.00150796390221547396236390642315296548 cos
*           sincos     -.001507963902215478183896385939205188475162
*           cosine      -.99999886302178844781346870679287144982
*           cosx        -.999998863021788447807102780963812085428
*
* Reference: 
*    http://cache-www.intel.com/cd/00/00/29/37/293747_293747.pdf
*    ?originalurl=http://www.intel.com/cd/ids/developer/asmo-na/eng/293747.htm
*    The title of the pdf is:
*    "Slerping Clock Cycles" JMP van Waveren, Id Software, Inc
*    For cosine code
*    http://www.ganssle.com/approx/approx.pdf
*    "A Guide to Approximations" by Jack G Ganssle
*
* Updates: 
*    with better approximations for cosine on 11/22 
*    allowed range to be the real number line
*
* Dependencies: 
*    None but see c_test_sincos for rigorous testing.
*
* Modification History:
*    11/28/2006, Sidey Timmins
*    09/14/2007, Nick Padfield - Ported code into the CDB_POLYSIMPLIFY2 Package
*    09/17/2007 Sidey Timmins Added decimal points to literals.
********************************************************************************
*/
  X         NUMBER             := Abs(InX);
  
  twopi     CONSTANT NUMBER             := 6.2831853071795864769252867665590057684;
  pi        CONSTANT NUMBER             := 3.1415926535897932384626433832795028842;                                 
  piByTwo   CONSTANT NUMBER             := 1.5707963267948966192313216916397514421;
  pi3by2    CONSTANT NUMBER             := 4.7123889803846898576939650749192543263;
 
  -- these coefficients are optimized to give accurate results throughout
  -- the range [-2pi to 2pi]
    c1        CONSTANT NUMBER             := 0.9999999999999999999936329;
    c2        CONSTANT NUMBER             :=-0.49999999999999999948362843;
    c3        CONSTANT NUMBER             := 0.04166666666666665975670054;
    c4        CONSTANT NUMBER             :=-0.00138888888888885302082298;
    c5        CONSTANT NUMBER             := 0.000024801587301492746422297;
    c6        CONSTANT NUMBER             :=-0.00000027557319209666748555;
    c7        CONSTANT NUMBER             := 0.0000000020876755667423458605;
    c8        CONSTANT NUMBER             :=-0.0000000000114706701991777771;
    c9        CONSTANT NUMBER             := 0.0000000000000477687298095717;
    c10       CONSTANT NUMBER             :=-0.00000000000000015119893746887;

  x2          NUMBER;
  xx          NUMBER;
  sinx        NUMBER; 
BEGIN
-- Adjust to range -twopi to +twopi
   IF x > twopi THEN
     xx := Mod(Inx,twopi);
     x := Abs(xx);
   ELSE
     xx := Inx;
   END IF;

  If x > pi THEN
    x   := MOD(x,pi);
  END IF;
  
  IF x > piByTwo THEN
     x := pi -x;
  END IF;

  x2 := x * x;
  
  -- This approximation is better for small angles (1st reference) and gets
  -- a zero argument right
-- near zero 
--sine   error .000000000000000003376874169918689071364806
--cosine error .0000000000000000000063670542405020720948 
-- near pi
--sine   error .000000000000000004221532479516052222995162
--cosine error .0000000000000000000063659258290593643911

 
   IF (x < 1.5E-3 ) THEN
   
 -- Use Horner's rule to evaluate the sine
  
    sinx :=  x*(x2*(x2*(x2*(x2 *(-2.39E-08 * x2 + 2.7526E-06) - 1.98409E-04) + 8.3333315E-03)
           -1.666666664E-01) + 1.0); 
 
     IF (sinx > 1.0) THEN
       sinx := 1.0;
     END IF;
 
-- Calculate cosine
     cosx := sqrt(1.0 - sinx*sinx);
    
  ELSE 
 -- Use Horner's Rule to evaluate the cosine (2nd reference)
 
   cosx   := c1 + x2*(c2 + x2*(c3 + x2*(c4 + x2*(c5 + x2*(c6 +x2*(c7 + x2*(c8 + 
              x2*(c9 + x2*c10))))))));

    IF cosx > 1.0 THEN
      cosx := 1.0;
    END IF;
  
-- Calculate sine. This could be done another way (sine = cosine(pi/2 - x)
-- but sqrt is very fast.

    sinx := sqrt(1.0 - cosx*cosx);
   
  END IF;
 
 -- Sine is an odd function
 
  IF (xx > pi and xx < twopi) or (xx < 0.0 and xx > -pi) THEN 
      sinx := -sinx;
  END IF;
  
 -- The cosine is an even function  
 
  IF Abs(xx) > pibyTwo and Abs(xx) < (pi3by2) THEN
      cosx := -cosx;
  END IF;
  
  RETURN sinx;

END sincos;
--
END GZ_PROJECTION;
/
