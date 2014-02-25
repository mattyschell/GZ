CREATE OR REPLACE PACKAGE BODY GZ_INTERACTIVE_UTILS
AS


   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   FUNCTION MAKE_ARROWHEAD(
      p_geom            IN SDO_GEOMETRY,
      p_scalefactor     IN NUMBER DEFAULT NULL
   ) RETURN SDO_GEOMETRY
   AS

     --Sidey!! and Matt! 3/5/10

     --Just a helper, not called in production
     --use like:
     --select GZ_utilities.make_arrowhead(a.geom) from
     --   newfeaturetype a where a.st99_hi_edges_ = 1

     --Way too big, or small?  Add the scalefactor parameter
     --select GZ_utilities.make_arrowhead(a.geom,5) from
     --   newfeaturetype a where a.st99_hi_edges_ = 1

     --Color and fill in mapviewer

      twopi     CONSTANT NUMBER  := 6.2831853071795864769252867665590057684; -- pi*2

      angle            NUMBER;
      sin_angle        NUMBER;
      cos_angle        NUMBER;
      x1new            NUMBER;
      y1new            NUMBER;
      x2new            NUMBER;
      y2new            NUMBER;
      xmid             NUMBER;
      ymid             NUMBER;
      xnew             NUMBER;
      ynew             NUMBER;
      x3               NUMBER;
      y3               NUMBER;
      x4               NUMBER;
      y4               NUMBER;
      x1               NUMBER;
      y1               NUMBER;
      x2               NUMBER;
      y2               NUMBER;
      delta            NUMBER := 1;

      ordinates       MDSYS.SDO_ORDINATE_ARRAY := MDSYS.SDO_ORDINATE_ARRAY();
      kount           PLS_INTEGER;
      mystart         PLS_INTEGER;
      triangle        SDO_GEOMETRY;


   BEGIN



      IF p_geom.sdo_gtype != '2002'
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Sorry, gtype is ' || p_geom.sdo_gtype);
      END IF;

      IF p_scalefactor IS NOT NULL
      THEN

         IF p_scalefactor <= 0
         THEN
            RAISE_APPLICATION_ERROR(-20001,'Scale factor must be greater than zero');
         END IF;

         delta := delta * (p_scalefactor / 100);

      END IF;

      --get roughly the middle segment
      kount := p_geom.sdo_ordinates.COUNT;
      --Dont let Sidey see this
      mystart := (kount/2 - 1);
      IF mod(mystart,2) = 0
      THEN
         --must start on an X
         mystart := mystart + 1;
      END IF;

      x1 := p_geom.sdo_ordinates(mystart);
      --dbms_output.put_line('kount is ' || kount);
      --dbms_output.put_line('x1 ' || x1);
      y1 := p_geom.sdo_ordinates(mystart+1);
      --dbms_output.put_line('y1 ' || y1);
      x2 := p_geom.sdo_ordinates(mystart+2);
      --dbms_output.put_line('x2 ' || x2);
      y2 := p_geom.sdo_ordinates(mystart+3);
      --dbms_output.put_line('y2 ' || y2);

      angle :=  atan2(y2-y1,x2-x1);
      if angle < 0. then
         angle :=  (twopi + angle);
      end if;
      sin_angle := sin(angle);
      cos_angle := cos(angle);
      --dbms_output.put_line('sin ' || sin_angle || ' cos ' || cos_angle);
      ymid := x1;
      xmid := y1;

       -- Fist rotate x2,y2 clockwise about (x1,y1)
       x2new := ROUND((y2 -ymid),8) * sin_angle + ROUND((x2 -xmid),8)* cos_angle;
       y2new :=  ROUND((y2 -ymid),8) * cos_angle - ROUND((x2 -xmid),8)* sin_angle;

       --dbms_output.put_line('x1new ' || x1new || ' y1new ' || y1new);
       --dbms_output.put_line('x2new ' || x2new || ' y2new ' || y2new);
       -- then rotate counterclockwise about x1,y1 and add back on the translation
       x3 := (x2new -delta) * cos_angle - (y2new+delta) * sin_angle + xmid;
       y3 := (x2new -delta) * sin_angle + (y2new+delta) * cos_angle + ymid;
       x4 := (x2new -delta) * cos_angle - (y2new-delta) * sin_angle + xmid;
       y4 := (x2new -delta) * sin_angle + (y2new-delta) * cos_angle + ymid;
       --dbms_output.put_line('x3 ' || x3 || ' y3 ' || y3);
       --dbms_output.put_line('x4 ' || x4 || ' y4 ' || y4);

       triangle := SDO_GEOMETRY(2003,
                                p_geom.SDO_SRID,
                                NULL,
                                SDO_ELEM_INFO_ARRAY (1,1003,1),
                                SDO_ORDINATE_ARRAY (x1,y1,
                                                    x3,y3,
                                                    x4,y4,
                                                    x1,y1)
                                );

      --This doesn't work too well since the scaling may move the triangle
      --completely off the line
      --IF p_scalefactor IS NOT NULL
      --THEN
         --triangle := GZ_CLIP.scale2percent(triangle, p_scalefactor);
      --END IF;


      RETURN triangle;

   END MAKE_ARROWHEAD;

   ------------------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------------------

   PROCEDURE CLOSED_LOOPS_HELPER (
      p_schema          IN VARCHAR2,
      p_table_name      IN VARCHAR2,
      p_log_table       IN VARCHAR2,
      p_state_code      IN VARCHAR2 DEFAULT NULL,
      p_srid            IN NUMBER DEFAULT 4269,
      p_tolerance       IN NUMBER DEFAULT .05,
      p_tidy_topo       IN VARCHAR2 DEFAULT 'N'
   )
   AS

      --Matt! 1/20/11
      --Swithced to add_linear_geometry 5/23/12
      --Helper to wrap the closed loop checker
      --Meant for standalone investigations
      --Written kinda on the cheap, not ready for prime time
      --sample: check all states
      --   BEGIN
      --   GZ_UTILITIES.CLOSED_LOOPS_HELPER('GZCPB_1','GZDEC10ST99.STATE_EDGES_Z6_V2','LOOPZ_TRACKER20100120');
      --   END;
      --   -----> See table LOOPZ_TRACKER20100120 for results
      --
      --sample: check one state, and clean up the temp topology
      --   BEGIN
      --   GZ_UTILITIES.CLOSED_LOOPS_HELPER('GZCPB_1','GZDEC10ST99.STATE_EDGES_Z6_V2','LOOPZ_TRACKER44','44',4269,.05,'Y');
      --   END;
      --   -----> See table LOOPZ_TRACKER44 for results


      ptopo       varchar2(32);
      ptabname    varchar2(32);
      states      gz_types.stringarray;
      psql        varchar2(4000);
      retval      varchar2(4000);

   BEGIN

      --set up logger
      BEGIN
         psql := 'create table ' || p_log_table || ' (state varchar2(4000), message varchar2(4000)) ';
         execute immediate psql;

         EXCEPTION
            when others then
            execute immediate 'drop table ' || p_log_table || ' ';
            execute immediate psql;

      END;

      IF p_state_code IS NULL
      THEN

         --get universe of states from source
         psql := 'select distinct state from ('
            || 'select l_statefp state from ' || p_table_name || ' '
            || 'union all '
            || 'select r_statefp state from ' || p_table_name || ' '
            || ') '
            || 'where state is not null '
            || 'order by state ';

         execute immediate psql bulk collect into states;

      ELSE

         states(1) := p_state_code;

      END IF;



      --start loop on states

      for i in 1 .. states.count
      loop

         --   IF i = 3
         --   THEN
         --      raise_application_error(-20001, 'peppahs!');
         --   end if;


         ptopo := 'LOOPZ_' || states(i);
         ptabname := 'LOOPZ_TAB' || states(i);


         --blow away anything topology related if rerunning
         BEGIN
            gz_topo_util.purge_topology(p_schema ,ptopo);

            EXCEPTION
            WHEN OTHERS THEN
               IF UPPER(SQLERRM) LIKE '%DOES NOT EXIST IN%'
               THEN
                  NULL;
               ELSE
                  RAISE;
               END IF;
         END;

         --drop the work table if there
         BEGIN
            execute immediate 'drop table ' || ptabname || ' ';

            EXCEPTION
               when others then
               NULL;
         END;


         --create table for this state
         psql := 'create table ' || ptabname || ' as select '
               || 'CAST(rownum AS NUMBER) ID, '
               || 'sdogeometry, '
               || 'CAST(NULL AS SDO_TOPO_GEOMETRY) TOPOGEOM '
               || 'from ' || p_table_name || ' '
               || 'where l_statefp = ' || states(i) || ' or r_statefp = ' || states(i) || ' ';
         execute immediate psql;

         --create topology
         SDO_TOPO.create_topology(ptopo,
                                   p_tolerance,
                                   p_srid,
                                   NULL,
                                   NULL,
                                   NULL,
                                   NULL,
                                   16);

         psql := 'INSERT INTO ' || ptopo || '_FACE$ '
              || 'VALUES (:p1, null, :p2, :p3, null)';
         EXECUTE IMMEDIATE psql USING -1,
                                            sdo_list_type(),
                                            sdo_list_type();
         commit;


         --build topogeom for this state

         --generic add_linear_geometry + constructors call

         GZ_TOPO_UTIL.ADD_TOPO_FROM_SPATIAL(ptopo,
                                            ptabname,
                                            'ID',
                                            'LINE',
                                            NULL,      --no logging
                                            'SDOGEOMETRY',
                                            NULL,         --no subset
                                            NULL,
                                            'N',          --no allow splits. Should intersect nothing at this point
                                            'Y');         --yes new layer

         /* BEAT
         GZ_UTILITIES.BUILD_TOPO_FROM_SPATIAL(ptopo,
                                               ptabname,
                                                  'LINE',
                                                  'ID',
                                                  'YES',
                                                  .05,
                                                  'YES',
                                                  'TOPOGEOM',
                                                  'SDOGEOMETRY',
                                                  NULL);
         */

         --see what the loop checker thinks
         retval := '';

         BEGIN
            retval :=  gz_topo_util.closed_loops(ptabname, ptopo);

            IF retval != 'Y'
            THEN
               psql := 'INSERT INTO ' || p_log_table || ' VALUES(:p1,:p2) ';
               EXECUTE IMMEDIATE psql USING states(i), retval;
               commit;
            ELSE
               psql := 'INSERT INTO ' || p_log_table || ' VALUES(:p1,:p2) ';
               EXECUTE IMMEDIATE psql USING states(i), 'OK';
               commit;
            END IF;

            --lets clean up if successful?
            --gz_topo_util.purge_topology(pschema ,ptopo);

         EXCEPTION
            WHEN OTHERS THEN

               psql := 'INSERT INTO ' || p_log_table || ' VALUES(:p1,:p2) ';
               execute immediate psql using states(i), SQLERRM;
               commit;
         END;

         IF p_tidy_topo = 'Y'
         THEN

            gz_topo_util.purge_topology(p_schema ,ptopo);

         END IF;



      end loop;



   END CLOSED_LOOPS_HELPER;
   
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   FUNCTION SHOW_ME_THE_VERTICES (
      p_geom            IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 5/21/10

      --This is just a helper, not actually called by anything in production
      --I want to see every x,y in mapviewer that makes up my geometry
      --Ex
      --select GZ_UTILITIES.show_me_the_vertices(e.geometry)
      --       FROM statefp02_edge$ e
      --       WHERE e.edge_id = 1

      output            SDO_GEOMETRY;
      out_ordinates     SDO_ORDINATE_ARRAY := SDO_ORDINATE_ARRAY();

   BEGIN

      --Lets be complete simpletons
      --Make a multipoint (dont tell) and put the ordinates in it

      output := SDO_GEOMETRY(2005,
                             p_geom.sdo_srid,
                             NULL,
                             SDO_ELEM_INFO_ARRAY(1,1,p_geom.sdo_ordinates.COUNT/2),
                             NULL);


      out_ordinates.EXTEND(p_geom.sdo_ordinates.COUNT);

      FOR i in 1 .. p_geom.sdo_ordinates.COUNT
      LOOP

         out_ordinates(i) := p_geom.sdo_ordinates(i);

      END LOOP;

      output.sdo_ordinates := out_ordinates;

      RETURN output;


   END SHOW_ME_THE_VERTICES;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION SHOW_ME_THE_FACE_DUPES (
      p_geom            IN SDO_GEOMETRY,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 2/11/11
      --sick this on a face that you know has duplicate vertices
      --it will return just the segment bounded by the dupes

      --sample:
      -- select GZ_UTILITIES.SHOW_ME_THE_FACE_DUPES(a.sdogeometry)
      -- from Z920LS_clip_face a
      -- where a.face_id = 152

      output            SDO_GEOMETRY;
      first_carrot      NUMBER;
      second_carrot     NUMBER;
      vertex_loc        NUMBER;
      validate_msg      VARCHAR2(4000);
      my_segment        SDO_GEOMETRY;

   BEGIN

      IF p_geom.sdo_gtype != 2003
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Sorry bubkins, this FN is face friendly only.  Got gtype ' || p_geom.sdo_gtype);
      END IF;

      --13356 [Element <1>] [Coordinate <220>][Ring <1>]
      validate_msg := SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(p_geom,
                                                              p_tolerance);

      IF validate_msg NOT LIKE '%13356%'
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry bubkins, this FN expects duplicate vertices, got ' || validate_msg);

      END IF;

      --Where is my fracking validate parser?
      --location of second <
      first_carrot := regexp_instr(validate_msg, '<',1,2);
      --location of second >
      second_carrot := regexp_instr(validate_msg, '>',1,2);
      vertex_loc := substr(validate_msg, (first_carrot + 1), (second_carrot - first_carrot - 1));

      my_segment := GZ_CLIP.GET_XYS(p_geom,1,vertex_loc);

      IF my_segment.SDO_ORDINATES.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Something went wrong, got ' || my_segment.SDO_ORDINATES.COUNT || ' ordinates');

      END IF;

      --Evil 2004 in the house.  May require an exorcism
      --can get away with this because I know exactly what the segment looks like
      --and this is just a helper fn
      output := SDO_GEOMETRY(2004,
                             my_segment.sdo_srid,
                             NULL,
                             SDO_ELEM_INFO_ARRAY(1,1,1, 3,1,1, 7,2,1),
                             SDO_ORDINATE_ARRAY(my_segment.sdo_ordinates(1), my_segment.sdo_ordinates(2),
                                                my_segment.sdo_ordinates(3), my_segment.sdo_ordinates(4),
                                                my_segment.sdo_ordinates(1),
                                                my_segment.sdo_ordinates(2),
                                                my_segment.sdo_ordinates(3),
                                                my_segment.sdo_ordinates(4)
                                                )
                             );

      RETURN output;


   END SHOW_ME_THE_FACE_DUPES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION SHOW_ME_EDGE_INTERSECTION (
      p_geom            IN SDO_GEOMETRY,
      p_tolerance       IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 7/12/12

      output            SDO_GEOMETRY;
      psql              VARCHAR2(4000);
      xes               GZ_TYPES.stringarray;
      yes               GZ_TYPES.stringarray;
      kounts            GZ_TYPES.stringarray;
      this_pt           SDO_GEOMETRY;

   BEGIN

      IF p_geom.sdo_gtype != 2002
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Sorry bubkins, this FN is edge friendly only.  Got gtype ' || p_geom.sdo_gtype);
      END IF;

      psql := 'SELECT xx, yy, kount FROM ( '
           || 'SELECT t.x xx, t.y yy, COUNT(*) kount '
           || 'FROM '
           || 'TABLE(SDO_UTIL.GETVERTICES((select sdo_geom.sdo_intersection(:p1, :p2, :p3) FROM DUAL '
           || '))) t '
           || 'GROUP BY t.x, t.y '
           || ') WHERE kount > 1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO xes, yes, kounts USING p_geom,
                                                                      p_geom,
                                                                      p_tolerance;

      IF xes.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'No intersection at ' || p_tolerance);

      END IF;

      --Evil 2004 in the house.  May require an exorcism
      --and this is just a helper fn

      output := p_geom;

      FOR i IN 1 .. xes.COUNT
      LOOP

         this_pt := SDO_GEOMETRY(2001,
                                 p_geom.sdo_srid,
                                 SDO_POINT_TYPE(xes(i),
                                                yes(i),
                                                NULL),
                                 NULL,NULL);

         output := SDO_UTIL.APPEND(output, this_pt);


      END LOOP;

      RETURN output;


   END SHOW_ME_EDGE_INTERSECTION;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE REFRESH_REMOTE_PROD_SCHEMA (
      p_prod_schema        IN VARCHAR2,
      p_prod_db            IN VARCHAR2,
      p_prod_pwd           IN VARCHAR2,
      p_likeclause         IN VARCHAR2 DEFAULT 'REF%'
   )
   AS

      --Matt! 11/01/10
      --Cheap little utility to take all the tables in GZDEC10ST99
      --  (or wherever the production schema parameter tables live) and
      -- mirror them on Devbench (or wherever)

      --As of 8/1/11 we no longer have privvies to create db links
      --Need to revise this guy to expect an input database link name, pre-created

      --Usage Sample. Logged into GZCPB1@Devbench
      --BEGIN
      --GZ_UTILITIES.REFRESH_REMOTE_PROD_SCHEMA('GZDEC10ST99','PRODBNCH','XXXXX');
      --END;

      prod_schema    VARCHAR2(32) := UPPER(p_prod_schema);
      prod_db        VARCHAR2(32) := UPPER(p_prod_db);
      prod_pwd       VARCHAR2(32) := p_prod_pwd;
      tabs           GZ_TYPES.stringarray;
      psql           VARCHAR2(4000);

   BEGIN

      --create DB link

      psql := 'CREATE DATABASE LINK ' || prod_schema || ' CONNECT TO ' || prod_schema
           || ' IDENTIFIED BY ' || prod_pwd || ' USING ''' || prod_db || '''';

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS THEN

         IF SQLCODE = -02011
         THEN
            --ORA-02011: duplicate database link name
            NULL;
         ELSE
            RAISE;
         END IF;

      END;


      --get all tables
      psql := 'SELECT table_name FROM '
           || 'USER_TABLES@' || prod_schema || ' '
           || 'WHERE table_name LIKE :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING p_likeclause;

      FOR i IN 1 .. tabs.COUNT
      LOOP

         psql := 'CREATE TABLE ' || tabs(i) || ' AS '
              || 'SELECT * FROM ' || prod_schema || '.' || tabs(i) || '@' || prod_schema || ' ';

         BEGIN

            EXECUTE IMMEDIATE psql;

         EXCEPTION

            WHEN OTHERS THEN

               IF SQLCODE = -00955
               THEN

                  --ORA-00955: name is already used by an existing object
                  EXECUTE IMMEDIATE 'DROP TABLE ' || tabs(i) || ' PURGE ';

                  EXECUTE IMMEDIATE psql;

               ELSE

                  RAISE;

               END IF;

         END;

         EXECUTE IMMEDIATE 'GRANT SELECT ON ' || tabs(i) || ' TO "PUBLIC" ';

      END LOOP;


      --clean up
      EXECUTE IMMEDIATE 'ALTER SESSION CLOSE DATABASE LINK ' || prod_schema;

      psql := 'DROP DATABASE LINK ' || prod_schema || ' ';
      EXECUTE IMMEDIATE psql;


   END REFRESH_REMOTE_PROD_SCHEMA;
   
    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   FUNCTION DUMP_TEST_CASE (
      p_topo            IN VARCHAR2,
      p_feature_tab     IN VARCHAR2,
      p_edge_sql        IN VARCHAR2
   ) RETURN DUMPTAB PIPELINED
   AS

      --Matt! 2/18/11 No more picking apart a topology piece by piece to build test cases
      --This is for simple test case builds from small to medium-sized edge$ records
      --It will produce a script, which, if possible, is best for test cases
      --See GZ_UTILITIES.DUMP_TEST_CASE_PRC for polys and more complex geoms

      --sample usage: make a topo with all of the edges in the state clip outline
      --select dumptext from
      --table(mattool.dump_test_case('DUMPTEST2','DUMPTESTTAB2',
      --      'select e.geometry from Z955LS_STATE_EDGES_Z9_V1 a, Z955LS_relation$ r, Z955LS_edge$ e where a.topogeom.tg_id = r.tg_id and a.topogeom.tg_layer_id = r.tg_layer_id and r.topo_id = e.edge_id'))
      --Then right click on TOAD and export to flat file

      output         CLOB;
      kount          PLS_INTEGER := 0;

      TYPE sdoarray  IS TABLE OF SDO_GEOMETRY
                     INDEX BY PLS_INTEGER;
      my_sdoarray    sdoarray;

      varpipe        DUMPREC;
      startpt        NUMBER;
      endpt          NUMBER;
      lengthpt       NUMBER;
      my_cursor      SYS_REFCURSOR;
      itstheend      PLS_INTEGER := 0;
      ordklump       PLS_INTEGER := 0;



   BEGIN

      varpipe.dumptext :=                     '--This is a test of ??' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--SR ??' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--Submitted by Matt Schell' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--Contact Matt Schell (matthew.c.schell@census.gov)' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--Create topology' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO.CREATE_TOPOLOGY(''' || p_topo || ''' ,.05,8265,NULL,NULL,NULL,NULL,16); ' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--insert universal face' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'INSERT INTO ' || p_topo || '_FACE$ VALUES (-1, null, sdo_list_type(), sdo_list_type(), null); ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'commit;' || chr(10);

      PIPE ROW(varpipe);

      varpipe.dumptext := Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--create dummy feature table' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'CREATE TABLE ' || p_feature_tab || '(id NUMBER, topogeom MDSYS.sdo_topo_geometry);' || chr(10);
      varpipe.dumptext := varpipe.dumptext || '--add table to topology' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO.add_topo_geometry_layer(''' || p_topo || ''',''' || p_feature_tab || ''',''TOPOGEOM'',''LINE'');' || chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--load a topomap' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO_MAP.DROP_TOPO_MAP(''' || p_topo || '_MAP'');' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXCEPTION ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'WHEN OTHERS THEN NULL; ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END;' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO_MAP.CREATE_TOPO_MAP(''' || p_topo || ''',''' || p_topo || '_MAP'');' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO_MAP.LOAD_TOPO_MAP(''' || p_topo || '_MAP'', ''TRUE'',''TRUE'');' || chr(10);

      PIPE ROW(varpipe);


      output := '';

      --get edge geometries


      OPEN my_cursor FOR p_edge_sql;
      LOOP

         FETCH my_cursor BULK COLLECT INTO my_sdoarray LIMIT 50;
         EXIT WHEN my_sdoarray.COUNT = 0;

         --insert lines

         FOR i IN 1 .. my_sdoarray.COUNT
         LOOP

            kount := kount + 1;

            --varpipe.dumptext := Chr(10);
            varpipe.dumptext := 'INSERT INTO ' || p_feature_tab
                             || ' VALUES(' || kount ||',SDO_TOPO_MAP.CREATE_FEATURE(''' || p_topo || ''',''' || p_feature_tab || ''',''TOPOGEOM'','; --no NL
            PIPE ROW(varpipe);


            --manage CLOBs here

            output := GZ_GEOM_UTILS.DUMP_SDO(my_sdoarray(i)); --no NL
            output := output || '));' || Chr(10);
            output := output || 'commit;' || Chr(10);


            endpt := 0;
            ordklump := 0;

            LOOP


               startpt := endpt + 1;

               --jump ahead 3000 chars, find a comma
               endpt := REGEXP_INSTR(output,',', (startpt + 3000));


               IF endpt = 0
               THEN

                  itstheend := 1;
                  --endpt := DBMS_LOB.GETLENGTH(output);
                  endpt := length(output);

               END IF;

               lengthpt := endpt - startpt + 1;


               varpipe.dumptext := TO_CHAR(substr(output,startpt,lengthpt));

               IF ordklump > 0
               THEN

                  --remove newline character at the start (after comma) in next klump
                  varpipe.dumptext := substr(varpipe.dumptext,2);

               END IF;

               PIPE ROW(varpipe);

               ordklump := ordklump + 1;

               IF itstheend = 1
               THEN

                  itstheend := 0;
                  EXIT;

               END IF;

            END LOOP;

            output := '';


         END LOOP;

      END LOOP;


      --clean up
      varpipe.dumptext := Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--clean up and initialize ' ||  Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO_MAP.COMMIT_TOPO_MAP(); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO_MAP.DROP_TOPO_MAP(''' || p_topo || '_MAP''); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXEC SDO_TOPO.INITIALIZE_METADATA(''' || p_topo || '''); ' || Chr(10);

      PIPE ROW(varpipe);



   END DUMP_TEST_CASE;



   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

    FUNCTION DUMP_TEST_CASE_PRC (
      p_topo            IN VARCHAR2,
      p_proc_name       IN VARCHAR2,
      p_type            IN VARCHAR2 DEFAULT 'LINE',
      p_geom_sql        IN VARCHAR2
   ) RETURN DUMPTAB PIPELINED
   AS

      --Matt! 05/06/11 No more picking apart a topology piece by piece to build test cases
      --This is the version that builds a procedure that you then compile and execute
      --This one is intended for bigger geometries (> 999 arguments in insert statement, ie 999 ordinates, I think)
      --See GZ_UTILITIES.DUMP_TEST_CASE for a simpler version that produces a script


      --sample usage: make a procedure that builds a topology from the SDO in the select statement
      --select dumptext from
      --table(gz_utilities.dump_test_case_prc('CFTEST2','CFTESTPROC','POLYGON','select a.sdogeometry from tab10_sl040 a')) --Then right click on TOAD and export to flat file


      output         CLOB;
      kount          PLS_INTEGER := 0;

      TYPE sdoarray  IS TABLE OF SDO_GEOMETRY
                     INDEX BY PLS_INTEGER;
      my_sdoarray    sdoarray;

      varpipe        DUMPREC;
      startpt        NUMBER;
      endpt          NUMBER;
      lengthpt       NUMBER;
      my_cursor      SYS_REFCURSOR;
      itstheend      PLS_INTEGER := 0;
      ordklump       PLS_INTEGER := 0;
      v_type         VARCHAR2(32) := UPPER(p_type);



   BEGIN

      --checks
      IF v_type NOT IN ('LINE','POLYGON')
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Sorry, p_type must be either LINE or POLYGON, you entered: ' || v_type);
      END IF;


      --As the kids say, this is so meta....

      varpipe.dumptext := 'CREATE OR REPLACE PROCEDURE ' || p_proc_name || ' (p_table_name VARCHAR2) ' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || 'AUTHID CURRENT_USER AS  ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   table_name     VARCHAR2(32) := UPPER(p_table_name); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   psql           VARCHAR2(4000); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   geom           SDO_GEOMETRY; ' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '  --This is a test of ??' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '  --SR ??' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '  --Submitted by ??' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '  --Contact ?? (??.?.??@census.gov)' || Chr(10);

      PIPE ROW(varpipe);

      varpipe.dumptext := Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || Chr(10);


      varpipe.dumptext := varpipe.dumptext || '--deregister table if it already exists from a previous run' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   SDO_TOPO.DELETE_TOPO_GEOMETRY_LAYER(''' || p_topo || ''',table_name,''TOPOGEOM'' ); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXCEPTION ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'WHEN OTHERS THEN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   NULL; ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END; ' || Chr(10);


      varpipe.dumptext := varpipe.dumptext || '--drop topo too if it already exists from a previous run' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   SDO_TOPO.DROP_TOPOLOGY(''' || p_topo || '''); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXCEPTION ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'WHEN OTHERS THEN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   NULL; ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END; ' || Chr(10);


      varpipe.dumptext := varpipe.dumptext || '--drop table if it already exists from a previous run' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'psql := ''DROP TABLE '' || table_name ;' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   EXECUTE IMMEDIATE psql; ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXCEPTION ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'WHEN OTHERS THEN ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '   NULL; ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END; ' || Chr(10);



      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--Create topology' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO.CREATE_TOPOLOGY(''' || p_topo || ''' ,.05,8265,NULL,NULL,NULL,NULL,16); ' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--insert universal face' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXECUTE IMMEDIATE ''INSERT INTO ' || p_topo || '_FACE$ VALUES (-1, null, sdo_list_type(), sdo_list_type(), null) ''; ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'commit;' || chr(10);

      PIPE ROW(varpipe);

      varpipe.dumptext := Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--create dummy feature table' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXECUTE IMMEDIATE ''CREATE TABLE '' || table_name || ''(id NUMBER, topogeom MDSYS.sdo_topo_geometry) ''; ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || '--add table to topology' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO.add_topo_geometry_layer(''' || p_topo || ''',table_name,''TOPOGEOM'',''' || v_type || ''');' || chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--load a topomap' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'BEGIN ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || '   SDO_TOPO_MAP.DROP_TOPO_MAP(''' || p_topo || '_MAP'');' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'EXCEPTION ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || '   WHEN OTHERS THEN NULL; ' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END;' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO_MAP.CREATE_TOPO_MAP(''' || p_topo || ''',''' || p_topo || '_MAP'');' || chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO_MAP.LOAD_TOPO_MAP(''' || p_topo || '_MAP'', ''TRUE'',''TRUE'');' || chr(10);

      PIPE ROW(varpipe);


      output := '';

      --get geometries

      OPEN my_cursor FOR p_geom_sql;
      LOOP

         FETCH my_cursor BULK COLLECT INTO my_sdoarray LIMIT 25;
         EXIT WHEN my_sdoarray.COUNT = 0;

         --insert geoms

         FOR i IN 1 .. my_sdoarray.COUNT
         LOOP

            kount := kount + 1;

            --varpipe.dumptext := Chr(10);
            --varpipe.dumptext := 'INSERT INTO ' || p_feature_tab
                             --|| ' VALUES(' || kount ||',SDO_TOPO_MAP.CREATE_FEATURE(''' || p_topo || ''',''' || p_feature_tab || ''',''TOPOGEOM'','; --no NL

            varpipe.dumptext := 'geom := '; --no newline I think
            PIPE ROW(varpipe);


            --manage CLOBs here

            output := GZ_GEOM_UTILS.DUMP_SDO(my_sdoarray(i)) || ';';
            --output := output || '));' || Chr(10);
            --output := output || 'commit;' || Chr(10);


            endpt := 0;
            ordklump := 0;

            LOOP


               startpt := endpt + 1;

               --jump ahead 3000 chars, find a comma
               endpt := REGEXP_INSTR(output,',', (startpt + 3000));


               IF endpt = 0
               THEN

                  itstheend := 1;
                  --endpt := DBMS_LOB.GETLENGTH(output);
                  endpt := length(output);

               END IF;

               lengthpt := endpt - startpt + 1;


               varpipe.dumptext := TO_CHAR(substr(output,startpt,lengthpt));

               IF ordklump > 0
               THEN

                  --remove newline character at the start (after comma) in next klump
                  varpipe.dumptext := substr(varpipe.dumptext,2);

               END IF;

               PIPE ROW(varpipe);

               ordklump := ordklump + 1;

               IF itstheend = 1
               THEN

                  itstheend := 0;
                  EXIT;

               END IF;

            END LOOP;

            output := '';

            varpipe.dumptext := Chr(10);

            varpipe.dumptext := varpipe.dumptext || 'psql := ''INSERT INTO '' || table_name || '' VALUES('' ' || Chr(10);
            varpipe.dumptext := varpipe.dumptext || '   || '':p1,SDO_TOPO_MAP.CREATE_FEATURE(:p2,:p3,:p4,:p5)) ''; ' || Chr(10);
            varpipe.dumptext := varpipe.dumptext || 'EXECUTE IMMEDIATE psql USING ' || kount || ',''' || p_topo || ''',table_name,''TOPOGEOM'',geom; ' || Chr(10);
            varpipe.dumptext := varpipe.dumptext || 'COMMIT; ' || Chr(10);

            PIPE ROW(varpipe);

         END LOOP;

      END LOOP;


      --clean up
      varpipe.dumptext := Chr(10);
      varpipe.dumptext := varpipe.dumptext || '--clean up and initialize ' ||  Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO_MAP.COMMIT_TOPO_MAP(); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO_MAP.DROP_TOPO_MAP(''' || p_topo || '_MAP''); ' || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'SDO_TOPO.INITIALIZE_METADATA(''' || p_topo || '''); ' || Chr(10);

      varpipe.dumptext := varpipe.dumptext || Chr(10);
      varpipe.dumptext := varpipe.dumptext || 'END ' || p_proc_name || '; ' || Chr(10);

      PIPE ROW(varpipe);



   END DUMP_TEST_CASE_PRC;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

function FACE_DIFFS (
      p_face1            IN number,
      p_face2            IN number,
      p_face_table_name  IN varchar2,
      p_reference_face_fields_tab IN varchar2,
      p_release_name     IN VARCHAR2 DEFAULT NULL,
      p_gen_project_id   IN VARCHAR2 DEFAULT NULL
   ) RETURN ty_attr_diff_tab PIPELINED AS

   -- Stephanie:  July 29, 2011

   -- Interactive function designed to help figure out what attributes are different
   -- in a generalized topology's face feature table if you know two face ids
   -- and have a list of attributes to compare (in a REFERENCE_FACE_FIELDS...
   -- style table.)

   -- Parameters:
   --  p_face1: face_id for a face in p_face_table_name
   --  p_face2: face_id for a face in p_face_table_name
   --  p_face_table_name: A face feature table with "face_id" column and some attribute fields
   --  p_reference_face_fields_tab: a "reference_face_fields.." style table that contains a list of
   --                               attribute field names that exist in your p_face_table_name.
   --                               needs a field called "FIELD", and a field called "FIELD_USE"
   --                               For each column name in 'FIELD' that where 'FIELD_USE' = 'attribute'
   --                               this function will compare the values for that column between the two faces.
   --                               See the Generalization wiki for the layout of this table.

   --                   Duplicate field names in this table are not allowed.  If you are using a
   --                   reference_face_fields table that has duplicate records for a field name,
   --                   you will need to pass the optional "p_release_name" and "gen_project_id"
   --                   values (and columns called 'release' and 'gen_project_id' will need to be
   --                   in the reference_face_fields table
   --                   in order to get the correct result from the function.  If you are using a
   --                   reference face_fields table from production, pass in the last two parameters.

   --  p_release_name: The name in the "release" column of the
   --                  reference_face_fields_tab to select by.  For example,
   --                  "ACS12", or "DEC10W6"
   --                  if you enter this parameter, you MUST also enter a gen_project_id

   --  p_gen_project_id: The gen_project_id in the "gen_project_id" column of
   --                    the reference_face_fields_tab to select by. For
   --                    example 'z6' or 'z9'  In ptoduction reference face_fields
   --                    tables this is usually the lowercase version of the
   --                    resolution code.  This parameter is case sensitive.
   --                  if you enter this parameter, you MUST also enter a release name

   -- example calls...

   -- get a report for all fields...
      -- select * from table(GZ_UTILTITIES.FACE_DIFFS(123,456,'Z899PB_FACE','REFERENCE_FACE_FIELDS_Z8', 'ACS12W3','z6')) ;

   -- how many match and how many are mismatches...
      -- select count(*),STATUS from table(GZ_UTILTITIES.FACE_DIFFS(123,456,'Z899PB_FACE','REFERENCE_FACE_FIELDS_Z8','ACS12W3','z6')) GROUP BY STATUS;

   -- just give me the fields that do not match...
      -- select * from table(GZ_UTILTITIES.FACE_DIFFS(123,456,'Z899PB_FACE','REFERENCE_FACE_FIELDS_Z8','ACS12W3','z6')) where status = 'MISMATCH' ;

   -- February 21, 2013:  Updated to allow use of the new reference
   --                     face fields format and added "release_name" and
   --                     "gen_project_id" are passed as arguments)

   -- variable declarations section
   vsql             varchar2(4000);
   vattribute_list  gz_types.stringarray;
   vresult          gz_interactive_utils.ty_attr_diff_rec;
   vrelease         varchar2(4000) := UPPER(p_release_name);
   -- currently reference face fields is forced to lowercase in the production ref face fields talbe
   vgpid            varchar2(4000) := (p_gen_project_id);

   -- Do the work in this block...
   BEGIN

   -- get a list of fields from the reference table...
   IF vrelease IS NOT NULL
   THEN
      IF vgpid IS NULL THEN
          RAISE_APPLICATION_ERROR (
               -20001,
               'If p_release_name is passed as a parameter, you must also pass p_gen_project_id.');
      END IF;
      -- This works with a release name and gen_project ID
      vsql :=
         'select FIELD from ' || p_reference_face_fields_tab
         || ' where FIELD_USE = :p1 and release = :p2 and gen_project_id = :p3';

      BEGIN
         EXECUTE IMMEDIATE vsql
            BULK COLLECT INTO vattribute_list
            USING 'attribute', vrelease, vgpid;
      EXCEPTION
         WHEN OTHERS
         THEN
            RAISE_APPLICATION_ERROR (
               -20001,
               'Trouble selecting a list of attribute fields from reference_face_fields table, '
               || p_reference_face_fields_tab
               || '.  SQL = '
               || vsql
               || ' USING ''attribute'','''||vrelease||''''||vgpid||'''.');
      END;
   ELSE
        -- This works without a release name and gen_project_id
      vsql :=
            'select FIELD from '
         || p_reference_face_fields_tab
         || ' where FIELD_USE = :p1';

      BEGIN
         EXECUTE IMMEDIATE vsql
            BULK COLLECT INTO vattribute_list
            USING 'attribute';
      EXCEPTION
         WHEN OTHERS
         THEN
            RAISE_APPLICATION_ERROR (
               -20001,
               'Trouble selecting a list of attribute fields from reference_face_fields table, '
               || p_reference_face_fields_tab
               || '.  SQL = '
               || vsql
               || ' USING ''attribute''.');
      END;
   END IF;



   vresult.ATTR := 'FACE_ID';
   vresult.FACE1 := p_face1;
   vresult.FACE2 := p_face2;
   vresult.STATUS := NULL;

   PIPE ROW(vresult);

       for i IN 1..vattribute_list.COUNT LOOP
           vresult.ATTR := NULL;
           vresult.FACE1 := NULL;
           vresult.FACE2 := NULL;
           vresult.STATUS := NULL;

           -- First caputure the FACE_IDs...

           vresult.ATTR := vattribute_list(i);
           vsql := 'select '||vresult.ATTR||' from '||p_face_table_name||
                   ' where face_id = :p1';
           BEGIN
              EXECUTE IMMEDIATE vsql INTO vresult.FACE1 USING p_face1;
                EXCEPTION
                WHEN OTHERS THEN
                   RAISE_APPLICATION_ERROR(-20001,'Trouble selecting attribute value ('||vresult.ATTR||') for face '||p_face1||' from '||p_face_table_name||'.  Are you sure that face_id exists? SQL = '||vsql||' USING '||p_face1);
           END;

           BEGIN
              EXECUTE IMMEDIATE vsql INTO vresult.FACE2 USING p_face2;
                EXCEPTION
                WHEN OTHERS THEN
                   RAISE_APPLICATION_ERROR(-20001,'Trouble selecting attribute value ('||vresult.ATTR||') for face '||p_face2||' from '||p_face_table_name||'.  Are you sure that face_id exists?  SQL = '||vsql||' USING '||p_face2);
           END;

           IF vresult.FACE1 = vresult.FACE2 THEN
              vresult.STATUS := 'OK';
           ELSE
              vresult.STATUS := 'MISMATCH';
           END IF;

           IF ( vresult.FACE1 IS NULL AND vresult.FACE2 IS NULL) THEN
              vresult.STATUS := 'OK';
           END IF;

           PIPE ROW(vresult);

       END LOOP;

   END FACE_DIFFS;
   
   ------------------------------------------------------------------------------------
 
   FUNCTION FACE_DIFFS_NAMES (
      p_face1                       IN NUMBER,
      p_face2                       IN NUMBER,
      p_face_table_name             IN VARCHAR2,
      p_reference_face_fields_tab   IN VARCHAR2,
      p_release_name                IN VARCHAR2 DEFAULT NULL,
      p_gen_project_id              IN VARCHAR2 DEFAULT NULL)
      RETURN ty_attr_diff_tab
      PIPELINED
   AS
      -- Stephanie:  November 8, 2013

      -- see face_diffs.  Does the same thing, but returns names
      -- from the benchmark instead of the value (OID) stored in
      -- the face feature table
   
      -- NEEDS WORK!
      -- this isn't great and assumes a lot instead of looking stuff up.
      -- It is junky because I needed to get it to work quickly for ACS13
      -- interactive work.
      -- For example, it assumes all face feature table values
      -- are benchmark table names.
      -- If the attribute is STATE value, it assumes it is the STATEFP and VINTAGE 90
      -- It doesn't return AIANNHCOMP names, just tosses back the OIDs
      -- It assumes the "RELEASE" name is also the benchmark name.

      -- variable declarations section
      vsql              VARCHAR2 (4000);
      vsql2              VARCHAR2 (4000);
      vattribute_list   gz_types.stringarray;
      vresult           gz_interactive_utils.ty_attr_diff_rec;
      vresult_names     gz_interactive_utils.ty_attr_diff_rec;
      vrelease          VARCHAR2 (4000) := UPPER (p_release_name);
      -- currently reference face fields is forced to lowercase in the production ref face fields talbe
      vgpid             VARCHAR2 (4000) := (p_gen_project_id);
   -- Do the work in this block...
   BEGIN
      -- get a list of attr names...
          vsql :=
         'select FIELD from ' || p_reference_face_fields_tab
         || ' where FIELD_USE = :p1 and release = :p2 and gen_project_id = :p3';
      BEGIN
         EXECUTE IMMEDIATE vsql
            BULK COLLECT INTO vattribute_list
            USING 'attribute', vrelease, vgpid;
      EXCEPTION
         WHEN OTHERS
         THEN
            RAISE_APPLICATION_ERROR (
               -20001,
               'Trouble selecting a list of attribute fields from reference_face_fields table, '
               || p_reference_face_fields_tab
               || '.  SQL = '
               || vsql
               || ' USING ''attribute'','''||vrelease||''''||vgpid||'''.');
      END;


      -- create first table row
   
      vresult_names.ATTR := 'FACE_ID';
      vresult_names.FACE1 := p_face1;
      vresult_names.FACE2 := p_face2;
      vresult_names.STATUS := NULL;

      PIPE ROW (vresult_names);

     
     -- call face diffs and get values of both faces
     vsql := 'SELECT * FROM TABLE (gz_interactive_utils.FACE_DIFFS ('||
     p_face1||','||p_face2||','''||p_face_table_name||''','''||p_reference_face_fields_tab||''','''||p_release_name||''','''||p_gen_project_id||''')'||
             ')  WHERE ATTR = :p1';
   
    For i in 1..vattribute_list.count loop

     BEGIN
         EXECUTE IMMEDIATE vsql
            INTO vresult
            USING vattribute_list(i);
      EXCEPTION
         WHEN OTHERS
         THEN
            RAISE_APPLICATION_ERROR (
               -20001,
               'Trouble selecting '||vattribute_list(i)||
               ' row from face_diffs table.  SQL = '
               || vsql
               || ' USING '||vattribute_list(i)||'.');
      END;
     
         vresult_names.ATTR := vresult.ATTR;
         vresult_names.FACE1 := NULL;
         vresult_names.FACE2 := NULL;
         vresult_names.STATUS := vresult.STATUS;

        -- EXCEPTION FOR aiannhcomp (NO NAME IN BENCH)     
        -- EXCEPTION FOR STATE (NOT THE oid IN THE TABLE, BUT THE STATEFP)
        IF vresult.ATTR = 'AIANNHCOMP' THEN
            -- NEEDS DEVLOPMENT...just returning OIDs for now -- 
            vresult_names.FACE1 := vresult.face1;
            vresult_names.FACE2 := vresult.face1;
        ELSE
        
            IF vresult.ATTR = 'STATE' THEN
                vsql2 :=
                       'SELECT name '
                    || ' from '||p_release_name ||'.' --- || ' from <benchmark>.'
                    || vresult.ATTR
                    || ' where vintage = ''90'' and statefp = :p1';
            ELSE
                 vsql2 :=
                       'SELECT name '
                    || ' from '||p_release_name ||'.' --- || ' from <benchmark>.'
                    || vresult.ATTR
                    || ' where OID = :p1';
                
              END IF;

             IF vresult.FACE1 IS NOT NULL THEN 

                 BEGIN
                    EXECUTE IMMEDIATE vsql2 INTO vresult_names.FACE1 USING vresult.face1;
                 EXCEPTION
                    WHEN OTHERS
                    THEN
                       RAISE_APPLICATION_ERROR (
                          -20001,
                             'Trouble selecting name value for face '
                          || p_face1
                          || ' from '
                          ||p_release_name ||'.'|| vresult.ATTR
                          ||' USING OID =  '||vresult.face1
                          ||' SQL = '|| vsql2
                          || ' USING '
                          || vresult.face1);
                 END;

            ELSE 
            
                vresult_names.FACE1 := NULL;
            
            END IF;

             IF vresult.FACE2 IS NOT NULL THEN 

                 BEGIN
                    EXECUTE IMMEDIATE vsql2 INTO vresult_names.FACE2 USING vresult.face2;
                 EXCEPTION
                    WHEN OTHERS
                    THEN
                       RAISE_APPLICATION_ERROR (
                          -20001,
                             'Trouble selecting name value for face '
                          || p_face1
                          || ' from '
                          ||p_release_name ||'.'|| vresult.ATTR
                          ||' USING OID =  '||vresult.face2
                          ||' SQL = '|| vsql2
                          || ' USING '
                          || vresult.face2);
                 END;

              ELSE 
                  vresult_names.FACE2 := NULL;
              END IF;
              
          
         END IF;
         
         PIPE ROW (vresult_names);
      END LOOP;
      
   END FACE_DIFFS_NAMES;
   

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public----------------------------------------------------------------------------

   FUNCTION CONVERT_COL_TO_CLOB (
      p_sql             IN VARCHAR2,
      p_delimiter       IN VARCHAR2 DEFAULT ','
   ) RETURN CLOB
   AS

      --Matt! 1/25/12
      --Helper for use in populating the gz_layers_subset.oid_clob column

      --Sample:
      --update gz_layers_subset
      --set oid_clob = gz_utilities.CONVERT_COL_TO_CLOB('select oid_base from gzcpb1.z699tm_fsl061v')
      --where release = 'ACS122' and gen_project_id = 'Z6' and layer = '061'

      output            CLOB := empty_clob();  --initialize object
      my_cursor         SYS_REFCURSOR;
      valz              GZ_TYPES.stringarray;


   BEGIN

      OPEN my_cursor FOR p_sql;

      LOOP

         FETCH my_cursor BULK COLLECT INTO valz LIMIT 10000;
         EXIT WHEN valz.COUNT = 0;

         FOR i in 1 .. valz.COUNT
         LOOP

            output := output || valz(i) || p_delimiter;

         END LOOP;

      END LOOP;


      RETURN output;

   END CONVERT_COL_TO_CLOB;
   
    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   
   FUNCTION GZ_COPIER_CHECKS (
      p_src_schema         IN VARCHAR2,               --always
      p_release            IN VARCHAR2,               --source release
      p_new_release        IN VARCHAR2 DEFAULT NULL,  --release copier always has this, may be same as p_release
      p_project_id         IN VARCHAR2 DEFAULT NULL,  --only project copier has this (always)
      p_new_project_id     IN VARCHAR2 DEFAULT NULL   --only project copier has this (always, may be same as p_project_id)
   ) RETURN VARCHAR2
   AS
   
      --Matt! 6/11/13
      --Private fn. Shared checks for gz_release_copier and gz_project_copier and maybe more
      
      output               VARCHAR2(4000) := '0';
      legalreleasetabs     GZ_TYPES.stringarray;
      layer_type           VARCHAR2(64);
      psql                 VARCHAR2(4000);
      kount                PLS_INTEGER;
      missing_stash        GZ_TYPES.stringarray;
      missing_string       VARCHAR2(4000);
      
   BEGIN
   
      IF p_new_release IS NOT NULL
      AND LENGTH(p_new_release) > 64
      THEN

         output := output || '| Check length of ' || p_new_release || '; elsewhere GZ code has a 64 char limit ';

      END IF;
      

      IF p_project_id IS NULL
      THEN
      
         --Release copier
         IF p_src_schema = SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA')
         AND p_release = p_new_release
         THEN

            output := output || '|Sorry boss, cant replace a release with itself in your schema';

         END IF;
         
      ELSE 
      
         --Project copier
         IF p_src_schema = SYS_CONTEXT('USERENV', 'CURRENT_SCHEMA')
         AND p_project_id = p_new_project_id
         THEN

             output := output || '|Sorry boss, cant replace a project id with itself in your schema';

         END IF;
     
     
      END IF;
      
      
      --Check to make sure that previous release exists, and if copying to new release, new release does not

      legalreleasetabs := GZ_TYPES.LEGAL_GZ_RELEASE_TABLES();

      --check source schema

      FOR i IN 1 .. legalreleasetabs.COUNT
      LOOP

         --meh, clever this up later
         IF legalreleasetabs(i) LIKE '%HIERARCHICAL'
         OR legalreleasetabs(i) LIKE '%SUBSET'
         OR legalreleasetabs(i) LIKE '%SPLIT'
         OR legalreleasetabs(i) LIKE '%AGGREGATE'
         THEN

            --These tables only have records for the release if GZ_LAYERS_OUT says so

            --gz_layers_aggregate --> AGGREGATE
            layer_type := REGEXP_REPLACE(legalreleasetabs(i),'GZ_LAYERS_','');


            --sum should be 0 for neither, or 2 for both

            psql := 'SELECT SUM(kount) FROM ('
                 || 'SELECT count(*) kount '
                 || 'FROM ' || p_src_schema || '.gz_layers_out a '
                 || 'WHERE ';

            IF layer_type = 'SUBSET'
            THEN

               psql := psql || '(a.layer_type = ''INITIAL'' OR a.layer_type = :p1) AND ';

            ELSE

               psql := psql || 'a.layer_type = :p1 AND ';

            END IF;

            psql := psql
                 || 'a.release = :p2 AND ';
                 
            IF p_project_id IS NOT NULL
            AND (legalreleasetabs(i) LIKE '%SUBSET' OR legalreleasetabs(i) LIKE '%HIERARCHICAL') --only these 2 have gen_project_id
            THEN
            
               --cant test for specific project codes for aggregate and split.  Just check the whole release
               psql := psql || 'UPPER(a.gen_project_id) = ''' || p_project_id || ''' AND ';
            
            END IF;
            
            psql := psql 
                 || 'rownum = 1 '
                 || 'UNION ALL '
                 || 'SELECT COUNT(*) kount '
                 || 'FROM ' || p_src_schema || '.' || legalreleasetabs(i) || ' a '
                 || 'WHERE '
                 || 'a.release = :p3 AND ';
                 
            IF p_project_id IS NOT NULL
            AND (legalreleasetabs(i) LIKE '%SUBSET' OR legalreleasetabs(i) LIKE '%HIERARCHICAL') --only these 2 have gen_project_id
            THEN
            
               psql := psql || 'UPPER(a.gen_project_id) = ''' || p_project_id || ''' AND ';
            
            END IF;
            
            psql := psql
                 || 'rownum = 1 '
                 || ')';

            dbms_output.put_line(psql);
            EXECUTE IMMEDIATE psql INTO kount USING layer_type,
                                                    p_release,
                                                    p_release;

            IF kount <> 0
            AND kount <> 2
            THEN

               missing_stash(missing_stash.COUNT + 1) := legalreleasetabs(i);

            END IF;

         ELSIF legalreleasetabs(i) IN ('QA_PARAMETERS')
         THEN

            --these tables are feral critters
            NULL;

         ELSE

            --These tables should always have a record or more for a release

            psql := 'SELECT COUNT(*) '
                 || 'FROM ' || p_src_schema || '.' || legalreleasetabs(i) || ' a '
                 || 'WHERE a.release = :p1 AND ';
                 
            IF p_project_id IS NOT NULL
            AND (legalreleasetabs(i) NOT IN ('GZ_LAYERS_FIELDS','GZ_LAYERS_CROSSWALK','GZ_LAYERS_GEOID'))
            THEN
            
               --if copying a project code it needs to be present
               psql := psql || 'UPPER(a.gen_project_id) = ''' || p_project_id || ''' AND ';
                            
            END IF;
            
            psql := psql || 'rownum = 1';            

            BEGIN

               dbms_output.put_line(psql);
               EXECUTE IMMEDIATE psql INTO kount USING p_release;

            EXCEPTION
            WHEN OTHERS THEN

               IF SQLERRM LIKE '%table or view does not exist%'
               THEN

                  missing_stash(missing_stash.COUNT + 1) := legalreleasetabs(i);

               ELSE

                  RAISE;

               END IF;

            END;

            IF kount != 1
            THEN

               --check them all before reporting back
               missing_stash(missing_stash.COUNT + 1) := legalreleasetabs(i);

            END IF;

         END IF;

      END LOOP;
      
      
      IF missing_stash.COUNT > 0
      THEN

         FOR i IN 1 .. missing_stash.COUNT
         LOOP

            missing_string := missing_string || ' ' || missing_stash(i);

         END LOOP;
         
         IF p_project_id IS NULL
         THEN

            output := output || '|Dude, release ' || p_release || ' '
                             || 'doesnt exist or is extraneous in ' || p_src_schema || ' table(s) ' || missing_string || '.';
                                        
         ELSE
         
            output := output || '|Dude, release ' || p_release || ' and project id ' || p_project_id
                             || 'doesnt exist or is extraneous in ' || p_src_schema || ' table(s) ' || missing_string || '.';
         END IF;

      END IF;

      missing_stash.DELETE;
      missing_string := '';
      
      
      --check destination schema
      --For release copier check if table exists and if release exists
      --For project copier just check for table exists


      FOR i IN 1 .. legalreleasetabs.COUNT
      LOOP
      
         IF p_project_id IS NULL
         THEN
         
            --release copier
            
            psql := 'SELECT COUNT(*) '
                 || 'FROM ' || legalreleasetabs(i) || ' a '
                 || 'WHERE a.release = :p1 AND '
                 || 'rownum = 1 ';

            BEGIN

               EXECUTE IMMEDIATE psql INTO kount USING p_new_release;  

            EXCEPTION
            WHEN OTHERS THEN

               IF SQLERRM LIKE '%table or view does not exist%'
               THEN

                  output := output || '|Dude, you dont have a ' || legalreleasetabs(i) || ' table ';

               ELSE

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' from ' || psql);

               END IF;

            END;

            IF kount <> 0
            THEN

               --check them all before reporting back
               missing_stash(missing_stash.COUNT + 1) := legalreleasetabs(i);

            END IF;
            
         ELSE
         
            psql := 'SELECT COUNT(*) '
                 || 'FROM ' || legalreleasetabs(i) || ' a '
                 || 'WHERE a.release = :p1 AND '
                 || 'a.gen_project_id = :p2 AND '
                 || 'rownum = 1 ';

            BEGIN

               EXECUTE IMMEDIATE psql INTO kount USING p_release,
                                                       p_new_project_id;  

            EXCEPTION
            WHEN OTHERS THEN

               IF SQLCODE = -904   --ORA-00904: "GEN_PROJECT_ID": invalid identifier
               AND UPPER(SQLERRM) LIKE '%GEN_PROJECT_ID%'
               THEN
               
                  --Not gonna bother with these weirdos, like gz_layers_aggregate
                  --They will be dealt with in the NUKE y/n code below
                  kount := 0;
                  dbms_output.put_line(SQLERRM || ' on ' || psql);
                  
               ELSIF SQLERRM LIKE '%table or view does not exist%'
               THEN

                  output := output || '|Dude, you dont have a ' || legalreleasetabs(i) || ' table ';

               ELSE

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' from ' || psql);

               END IF;

            END;

            IF kount <> 0
            THEN

               --check them all before reporting back
               missing_stash(missing_stash.COUNT + 1) := legalreleasetabs(i);

            END IF;
         
         END IF;
      
      END LOOP;

      IF missing_stash.COUNT > 0
      THEN

         FOR i IN 1 .. missing_stash.COUNT
         LOOP

            missing_string := missing_string || ' ' || missing_stash(i);

         END LOOP;

         IF p_project_id IS NULL
         THEN
         
            output := output || '|Dude, release ' || p_new_release || ' '
                             || 'already exists in these tables of yours-> ' || missing_string || ' <- '
                             || 'Use the GZ_RELEASE_DELETER ';
                             
         ELSE
         
            output := output || '|Dude, release ' || p_release || ' and project_id ' || p_new_project_id || ' '
                             || 'already exists in these tables of yours-> ' || missing_string || ' <- '
                             || 'Use the GZ_RELEASE_DELETER ';
                             
         END IF;
                                        
      END IF;
      
      RETURN output;
   
   END GZ_COPIER_CHECKS;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

    PROCEDURE GZ_RELEASE_COPIER (
      p_src_schema     IN VARCHAR2,
      p_release        IN VARCHAR2,
      p_new_release    IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 4/24/12
      --WIP, for now just a personal helper for working on alternative topo build and alternative output
      --Matt! 6/11/13 better handling when columns mismatch

      release              VARCHAR2(4000) := UPPER(p_release);
      new_release          VARCHAR2(4000);
      legalreleasetabs     GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);
      ezcounter            PLS_INTEGER := 1;
      ezpick               PLS_INTEGER := 0;
      staging_table        VARCHAR2(64);
      insert_fails         VARCHAR2(4000) := '';
      cheker               VARCHAR2(4000);


   BEGIN

      IF p_new_release IS NULL
      THEN

         new_release := UPPER(release);

      ELSE

         new_release := UPPER(p_new_release);

      END IF;
      
      --Do all the checks in subroutine shared by project copier
      cheker := GZ_INTERACTIVE_UTILS.GZ_COPIER_CHECKS(p_src_schema,
                                              p_release,
                                              new_release);

      IF cheker <> '0'
      THEN
      
         RAISE_APPLICATION_ERROR(-20001,cheker);
      
      END IF;
      
      legalreleasetabs := GZ_TYPES.LEGAL_GZ_RELEASE_TABLES();


      FOR i IN 1 .. legalreleasetabs.COUNT
      LOOP

         IF release <> new_release
         THEN

            --make temp staging table
            --this is either a remote schema or own schema

            ezcounter := 1;
            FOR j in 1 .. 99
            LOOP

               EXIT WHEN ezcounter = 0;

               psql := 'SELECT count(*) FROM USER_TABLES '
                    || 'WHERE TABLE_NAME = :p1';
               EXECUTE IMMEDIATE psql INTO ezcounter USING legalreleasetabs(i) || '_EZ' || j;

               IF ezcounter = 0
               THEN
                  ezpick := j;
               END IF;

            END LOOP;

            staging_table := legalreleasetabs(i) || '_EZ' || ezpick;

            --create temp table from source. Could be own schema if changing release codes

            psql := 'CREATE TABLE ' || staging_table || ' '
                 || 'AS SELECT * FROM ' || p_src_schema || '.' || legalreleasetabs(i) || ' a '
                 || 'WHERE a.release = ''' || release || ''' ';   --no binds in DDL

            EXECUTE IMMEDIATE psql;

            psql := 'UPDATE ' || staging_table || ' a '
                 || 'SET a.release = :p1 ';

            EXECUTE IMMEDIATE psql USING new_release;
            COMMIT;

         ELSE

            --this is ALWAYS a remote schema with no release code change
            staging_table :=  p_src_schema || '.' || legalreleasetabs(i);

         END IF;


         psql := 'INSERT INTO ' || legalreleasetabs(i) || ' '
              || 'SELECT * FROM ' || staging_table || ' '
              || 'WHERE release = ''' || new_release || ''' ';

         dbms_output.put_line(psql);

         BEGIN

            EXECUTE IMMEDIATE psql;
            COMMIT;

         EXCEPTION
         WHEN OTHERS
         THEN
         
            IF SQLCODE = -947    -- ORA-00947: not enough values
            OR SQLCODE = -913    -- ORA-00913: too many values
            THEN
            
               --Column mismatch between source and target.  Try to keep going and error after all are attempted
               insert_fails := insert_fails || '|' || SQLERRM || ' on ' || psql;
               
            ELSE
            
               RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' on ' || psql);
            
            END IF;

         END;

         IF release <> new_release
         THEN

            EXECUTE IMMEDIATE 'DROP TABLE ' || staging_table || ' PURGE ';

         END IF;


      END LOOP;
      
      IF LENGTH(insert_fails) > 0
      THEN
      
         --Report any tables that failed due to column mismatch
         RAISE_APPLICATION_ERROR(-20001,'Copied into as many tables as possible.  Failed on these ones due to column mismatches: ' 
                                       || insert_fails);
      
      END IF;


   END GZ_RELEASE_COPIER;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   
   PROCEDURE GZ_PROJECT_COPIER (
      p_src_schema         IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_new_project_id     IN VARCHAR2 DEFAULT NULL,
      p_nuke_conflicts     IN VARCHAR2 DEFAULT 'N'
   )
   AS
   
      --Matt! 6/11/13
      --Made an executive decision to write this for selfish use even though its not high priority
      --Like the gz_release_copier, except only one gen_project_id.  
      --"For tables without a project_id the copier should accept an option to either overwrite or 
      --  keep pre-existing records with the same keys"
      
      --EXAMPLES
      --   Copy a nice pristine Z6 project to a Z1 project for tire burning
      --   Typically on self-copies nuke_conflicts is irrelevant since I'd just be overwriting my own 
      --   records with the same records
      --   begin
      --      GZ_UTILITIES.GZ_PROJECT_COPIER('SCHEL010', 'ACS122','Z6','Z1');
      --   end;
      --
      --   Copy from some other schema to me, just one project
      --   begin
      --      GZ_UTILITIES.GZ_PROJECT_COPIER('GZCPB2','ACS12','V6','V6');
      --   end;
      
      cheker               VARCHAR2(4000);
      new_project_id       VARCHAR2(4);
      legalreleasetabs     GZ_TYPES.stringarray;
      ezcounter            PLS_INTEGER := 1;
      ezpick               PLS_INTEGER := 0;
      psql                 VARCHAR2(4000);
      psql2                VARCHAR2(4000);
      staging_table        VARCHAR2(64);
      insert_fails         VARCHAR2(4000) := '';
      nuke_conflicts       VARCHAR2(1);
      pkcs                 GZ_TYPES.stringarray;
      pkc_commas           VARCHAR2(4000);
      
   BEGIN
   
      IF p_new_project_id IS NOT NULL AND
      LENGTH(p_new_project_id) > 4
      THEN
      
         RAISE_APPLICATION_ERROR(-20001,'Sorry buttercup, project ids can only be 4 chars, your new one is ' || p_new_project_id);   
         
      END IF;
      
      IF UPPER(p_nuke_conflicts) NOT IN ('Y','N')
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Whats a p_nuke_conflicts of ' || p_nuke_conflicts || '?');
       
      ELSE
      
         nuke_conflicts := UPPER(p_nuke_conflicts);
           
      END IF;
      
      IF p_new_project_id IS NULL
      THEN

         new_project_id := UPPER(p_project_id);

      ELSE

         new_project_id := UPPER(p_new_project_id);

      END IF;
   
       --Do all the checks in subroutine shared by release copier
      cheker := GZ_INTERACTIVE_UTILS.GZ_COPIER_CHECKS(p_src_schema,
                                              p_release,
                                              NULL,
                                              p_project_id,
                                              new_project_id);
                                              
      IF cheker <> '0'
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, cheker);
      
      END IF;
      
      legalreleasetabs := GZ_TYPES.LEGAL_GZ_RELEASE_TABLES();

      FOR i IN 1 .. legalreleasetabs.COUNT
      LOOP

         IF p_project_id <> new_project_id
         THEN

            --make temp staging table for update
            --this is either a remote schema or own schema

            ezcounter := 1;
            FOR j in 1 .. 99
            LOOP

               EXIT WHEN ezcounter = 0;

               psql := 'SELECT count(*) FROM USER_TABLES '
                    || 'WHERE TABLE_NAME = :p1';
               EXECUTE IMMEDIATE psql INTO ezcounter USING legalreleasetabs(i) || '_EZ' || j;

               IF ezcounter = 0
               THEN
                  ezpick := j;
               END IF;

            END LOOP;

            staging_table := legalreleasetabs(i) || '_EZ' || ezpick;

            --create temp table from source. Could be own schema if changing release codes

            psql := 'CREATE TABLE ' || staging_table || ' '
                 || 'AS SELECT * FROM ' || p_src_schema || '.' || legalreleasetabs(i) || ' a '
                 || 'WHERE a.release = ''' || p_release || ''' ';   --no binds in DDL
            
            IF legalreleasetabs(i) NOT IN ('GZ_LAYERS_FIELDS','GZ_LAYERS_CROSSWALK','GZ_LAYERS_GEOID',
                                           'GZ_LAYERS_AGGREGATE','GZ_LAYERS_SPLIT')
            THEN
            
               --no binds in ddl
               psql := psql || 'AND UPPER(a.gen_project_id) = ''' || p_project_id || '''';
            
            END IF;

            --stay verbose, its a utility
            dbms_output.put_line(psql);
            EXECUTE IMMEDIATE psql;

            IF legalreleasetabs(i) NOT IN ('GZ_LAYERS_FIELDS','GZ_LAYERS_CROSSWALK','GZ_LAYERS_GEOID',
                                           'GZ_LAYERS_AGGREGATE','GZ_LAYERS_SPLIT')
            THEN
            
               psql := 'UPDATE ' || staging_table || ' a '
                    || 'SET a.gen_project_id = :p1 ';

               EXECUTE IMMEDIATE psql USING new_project_id;
               COMMIT;
               
            END IF;

         ELSE

            --this is ALWAYS a remote schema with no update
            staging_table :=  p_src_schema || '.' || legalreleasetabs(i);

         END IF;

         
         psql := 'INSERT INTO ' || legalreleasetabs(i) || ' '
              || 'SELECT * FROM ' || staging_table || ' a '
              || 'WHERE a.release = :p1 ';
              
         IF legalreleasetabs(i) NOT IN ('GZ_LAYERS_FIELDS','GZ_LAYERS_CROSSWALK','GZ_LAYERS_GEOID',
                                        'GZ_LAYERS_AGGREGATE','GZ_LAYERS_SPLIT')
         THEN
         
            psql := psql || 'AND UPPER(a.gen_project_id) = ''' || new_project_id || '''';
         
         END IF;


         dbms_output.put_line(psql);

         BEGIN

            EXECUTE IMMEDIATE psql USING p_release;
            COMMIT;

         EXCEPTION
         WHEN OTHERS
         THEN
         
            IF SQLCODE = -947    -- ORA-00947: not enough values
            OR SQLCODE = -913    -- ORA-00913: too many values
            THEN
            
               --Column mismatch between source and target.  Try to keep going and error after all are attempted
               insert_fails := insert_fails || '|' || SQLERRM || ' on ' || psql;
               
            ELSIF SQLCODE = -1      -- ORA-00001: unique constraint (GZCPB1.GZ_LAYERS_AGGREGATEPKC) violated on ...
            AND nuke_conflicts = 'N'
            THEN
            
               --ignore the conflict
               --bad form to do nothing, Ill dbms_output like a chump
               dbms_output.put_line('Conflicting inserts for ' || legalreleasetabs(i) || ' ignored since p_nuke_conflicts is N. '
                                    || 'SQL: ' || psql);
               
            ELSIF SQLCODE = -1       -- ORA-00001: unique constraint
            AND nuke_conflicts = 'Y'
            THEN
            
               --This is intended for tables without a project code, like gz_layers_aggregate
               --Not for tables like gz_job_parameters
               --Some of the parameter tables (like gz_job_parameters, annoyingly) lack primary keys and so could get dupes in here
               --The checker should catch this
               
               --DELETE first
               --Example
               --delete from gz_layers_crosswalk where (release, output_field) IN 
               --(select release, output_field from 
               --GZ_LAYERS_CROSSWALK_EZ1 where release = 'ACS123')
               
               pkcs := GZ_BUSINESS_UTILS.GET_PRIMARY_KEY_COLS(legalreleasetabs(i));
               
               IF pkcs.COUNT = 0
               THEN
               
                  RAISE_APPLICATION_ERROR(-20001, 'Howd we get a unique constraint without a pkc for ' || legalreleasetabs(i));
               
               END IF;
               
               psql2 := 'DELETE FROM ' || legalreleasetabs(i) || ' WHERE (';
               
               FOR i IN 1 .. pkcs.COUNT
               LOOP
               
                  pkc_commas := pkcs(i);
                  
                  IF i <> pkcs.COUNT
                  THEN
                  
                     pkc_commas := pkc_commas || ', ';
                     
                  END IF;
                  
               END LOOP;
               
               psql2 := psql2 || pkc_commas || ') IN ('
                              || 'SELECT ' || pkc_commas || ' FROM '
                              || staging_table || ' WHERE release = :p1) ';
                              
               BEGIN
               
                  EXECUTE IMMEDIATE psql2 USING p_release;               
                  
                  EXECUTE IMMEDIATE psql USING p_release;
               
               EXCEPTION
               WHEN OTHERS 
               THEN
               
                  ROLLBACK;
                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' on ' || psql2 || chr(10) || psql);
                  
               END;
               
               COMMIT;
               
            ELSE
            
               RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' on ' || psql);
            
            END IF;

         END;

         IF p_project_id <> new_project_id
         THEN

            NULL;
            EXECUTE IMMEDIATE 'DROP TABLE ' || staging_table || ' PURGE ';

         END IF;


      END LOOP;
      
      IF LENGTH(insert_fails) > 0
      THEN
      
         --Report any tables that failed due to column mismatch
         RAISE_APPLICATION_ERROR(-20001,'Copied into as many tables as possible.  Failed on these ones due to column mismatches: ' 
                                       || insert_fails);
      
      END IF;
   
      
   END GZ_PROJECT_COPIER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE GZ_RELEASE_DELETER (
      p_release        IN VARCHAR2
   )
   AS

      --Matt! 4/26/12
      --WIP, for now just a personal helper for working on alternate topo build and alternate output

      legalreleasetabs     GZ_TYPES.stringarray;
      psql                 VARCHAR2(4000);
      kount                PLS_INTEGER;

   BEGIN

      --checks

      --should at least be in gz_layers_out and gz_layers_in right?

      psql := 'SELECT SUM(kount) FROM ( '
           || 'SELECT COUNT(*) kount FROM gz_layers_out '
           || 'WHERE release = :p1 AND '
           || 'rownum = 1 '
           || 'UNION ALL '
           || 'SELECT COUNT(*) kount FROM gz_layers_in '
           || 'WHERE release = :p2 AND '
           || 'rownum = 1 '
           || ') ';

      EXECUTE IMMEDIATE psql INTO kount USING UPPER(p_release),
                                              UPPER(p_release);

      IF kount <> 2
      THEN

         RAISE_APPLICATION_ERROR(-20001,'I dont see release ' || p_release || ' in either gz_layers_in or gz_layers_out '
                                     || 'Maybe you just want to wipe out some other layers junk?  Lets remove this error ');

      END IF;

      legalreleasetabs := GZ_TYPES.LEGAL_GZ_RELEASE_TABLES();


      FOR i IN 1 .. legalreleasetabs.COUNT
      LOOP

         psql := 'DELETE FROM ' || legalreleasetabs(i) || ' a '
              || 'WHERE a.release = :p1 ';

         EXECUTE IMMEDIATE psql USING UPPER(p_release);
         COMMIT;

      END LOOP;


   END GZ_RELEASE_DELETER;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   PROCEDURE GZ_LAYER_COPIER (
      p_src_schema         IN VARCHAR2,
      p_src_release        IN VARCHAR2,
      p_src_project_id     IN VARCHAR2,
      p_src_layer          IN VARCHAR2,
      p_dest_release       IN VARCHAR2,
      p_dest_project_id    IN VARCHAR2,
      p_dest_layer         IN VARCHAR2,
      p_src_x_out          IN VARCHAR2 DEFAULT NULL,
      p_clean_house        IN VARCHAR2 DEFAULT 'N'
   )
   AS

      --Matt! 9/04/12

      --p_clean_house means copier has permission to delete any conflicts in the destination

      --WIP WIP WIP WIP

      psql              VARCHAR2(4000);
      psql2             VARCHAR2(4000);
      temp_tab_out      VARCHAR2(30);
      temp_tab_type     VARCHAR2(30);
      temp_tab_geoid    VARCHAR2(30);
      temp_tab_fields   VARCHAR2(30);
      temp_tab_xwalk    VARCHAR2(30);
      layer_type        VARCHAR2(64);
      fieldz            GZ_TYPES.stringarray;
      kount             PLS_INTEGER;
      new_kount         PLS_INTEGER := 0;


   BEGIN

      ----------------------
      --Cleanup
      ----------------------


      temp_tab_out    := 'GZ_LAYERS_OUT_' || p_dest_layer;
      temp_tab_type   := 'GZ_LAYERS_TYPE_' || p_dest_layer;
      temp_tab_geoid  := 'GZ_LAYERS_GEOID_' || p_dest_layer;
      temp_tab_fields := 'GZ_LAYERS_FIELDS_' || p_dest_layer;
      temp_tab_xwalk  := 'GZ_LAYERS_XWALK_' || p_dest_layer;


      BEGIN
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || temp_tab_out;
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || temp_tab_type;
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || temp_tab_geoid;
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || temp_tab_fields;
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || temp_tab_xwalk;
      EXCEPTION
      WHEN OTHERS
      THEN

         NULL;

      END;


      ----------------------
      --GZ_LAYERS_OUT
      ----------------------

      psql := 'CREATE GLOBAL TEMPORARY TABLE ' || temp_tab_out || ' '
           || 'ON COMMIT PRESERVE ROWS '
           || 'AS '
           || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_OUT a '
           || 'WHERE '
           || 'a.release = ''' || p_src_release || p_src_x_out || ''' AND '  --assumption x out only here
           || 'a.gen_project_id = ''' || p_src_project_id || ''' AND '
           || 'a.layer = ''' || p_src_layer || ''' ';

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -955
         THEN

            psql := 'INSERT INTO ' || temp_tab_out || ' '
                 || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_OUT a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || p_src_x_out || ''' AND '  --assumption x out only here
                 || 'a.gen_project_id = ''' || p_src_project_id || ''' AND '
                 || 'a.layer = ''' || p_src_layer || ''' ';

            EXECUTE IMMEDIATE psql;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ON ' || psql);

         END IF;

      END;

      --verify
      psql2 := 'SELECT COUNT(*) FROM ' || temp_tab_out;
      EXECUTE IMMEDIATE psql2 INTO kount;

      IF kount = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Didnt get any records from ' || psql);

      END IF;

      --update all for simplicity

      psql := 'UPDATE ' || temp_tab_out || ' a '
           || 'SET '
           || 'a.release = :p1, '
           || 'a.gen_project_id = :p2, '
           || 'a.layer = :p3 ';

      EXECUTE IMMEDIATE psql USING p_dest_release,
                                   p_dest_project_id,
                                   p_dest_layer;


      psql := 'SELECT layer_type FROM ' || temp_tab_out;
      EXECUTE IMMEDIATE psql INTO layer_type;

      IF p_clean_house = 'Y'
      THEN

         psql := 'DELETE FROM gz_layers_out a '
              || 'WHERE '
              || 'a.release = :p1 AND '
              || 'a.gen_project_id = :p2 AND '
              || 'a.layer = :p3 ';

         EXECUTE IMMEDIATE psql USING p_dest_release,
                                      p_dest_project_id,
                                      p_dest_layer;

      END IF;

      psql := 'INSERT INTO gz_layers_out '
           || 'SELECT * FROM ' || temp_tab_out || ' ';

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLERRM LIKE ('%unique constraint%')
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Primary key violation inserting into gz_layers_out. '
                                        || ' Consider the p_clean_house flag. ' || SQLERRM || ' on ' || psql);

         ELSE

            RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' on ' || psql);

         END IF;

      END;

      COMMIT;


      IF layer_type NOT IN ('INITIAL','SUBSET','SPLIT','HIERARCHICAL','AGGREGATE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'WTW is layer type ' || layer_type);

      END IF;

      ----------------------
      --GZ_LAYERS_<LAYER_TYPE>
      ----------------------

      IF layer_type = 'INITIAL'
      THEN

         layer_type := 'SUBSET';

      END IF;

      psql := 'CREATE GLOBAL TEMPORARY TABLE ' || temp_tab_type || ' '
           || 'ON COMMIT PRESERVE ROWS '
           || 'AS '
           || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_' || layer_type || ' a '
           || 'WHERE '
           || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
           || 'a.layer = ''' || p_src_layer || ''' ';

           IF layer_type NOT IN ('SPLIT','AGGREGATE')
           THEN

              psql := psql || 'AND a.gen_project_id = ''' || p_src_project_id || ''' ';

           END IF;

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -955
         THEN

            psql := 'INSERT INTO ' || temp_tab_type || ' '
                 || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_' || layer_type || ' a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                 || 'a.layer = ''' || p_src_layer || ''' ';

            IF layer_type NOT IN ('SPLIT','AGGREGATE')
            THEN

               psql := psql || 'AND a.gen_project_id = ''' || p_src_project_id || ''' ';

            END IF;

            EXECUTE IMMEDIATE psql;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ON ' || psql);

         END IF;

      END;

      --update all for simplicity

      psql := 'UPDATE ' || temp_tab_type || ' a '
           || 'SET '
           || 'a.release = :p1, '
           || 'a.layer = :p2 ';

      IF layer_type NOT IN ('SPLIT','AGGREGATE')
      THEN

         psql := psql || ', a.gen_project_id = ''' || p_dest_project_id || ''' ';

      END IF;


      EXECUTE IMMEDIATE psql USING p_dest_release,
                                   p_dest_layer;

      IF p_clean_house = 'Y'
      THEN

         psql := 'DELETE FROM gz_layers_' || layer_type || ' a '
              || 'WHERE '
              || 'a.release = :p1 AND '
              || 'a.layer = :p2 ';

         IF layer_type NOT IN ('SPLIT','AGGREGATE')
         THEN

            psql := psql || 'AND a.gen_project_id = ''' || p_dest_project_id || ''' ';

         END IF;

         EXECUTE IMMEDIATE psql USING p_dest_release,
                                      p_dest_layer;

      END IF;

      psql := 'INSERT INTO  GZ_LAYERS_' || layer_type || ' '
           || 'SELECT * FROM ' || temp_tab_type;

      EXECUTE IMMEDIATE psql;
      COMMIT;


      ----------------------
      --GZ_LAYERS_GEOID
      ----------------------

      psql := 'CREATE GLOBAL TEMPORARY TABLE ' || temp_tab_geoid || ' '
           || 'ON COMMIT PRESERVE ROWS '
           || 'AS '
           || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_GEOID a '
           || 'WHERE '
           || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
           || 'a.sum_lev = ''' || p_src_layer || ''' ';

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -955
         THEN

            psql := 'INSERT INTO ' || temp_tab_geoid || ' '
                 || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_GEOID a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                 || 'a.sum_lev = ''' || p_src_layer || ''' ';

            EXECUTE IMMEDIATE psql;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ON ' || psql);

         END IF;

      END;

      psql := 'UPDATE ' || temp_tab_geoid || ' a '
           || 'SET '
           || 'a.release = :p1, '
           || 'a.sum_lev = :p2 ';

      EXECUTE IMMEDIATE psql USING p_dest_release,
                                   p_dest_layer;

      IF p_clean_house = 'Y'
      THEN

         psql := 'DELETE FROM gz_layers_geoid a '
              || 'WHERE '
              || 'a.release = :p1 AND '
              || 'a.sum_lev = :p2 ';


         EXECUTE IMMEDIATE psql USING p_dest_release,
                                      p_dest_layer;

      END IF;

      psql := 'INSERT INTO GZ_LAYERS_GEOID '
           || 'SELECT * FROM ' || temp_tab_geoid;

      EXECUTE IMMEDIATE psql;
      COMMIT;


      ----------------------
      --GZ_LAYERS_FIELDS
      ----------------------

      psql := 'CREATE GLOBAL TEMPORARY TABLE ' || temp_tab_fields || ' '
           || 'ON COMMIT PRESERVE ROWS '
           || 'AS SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_FIELDS a '
           || 'WHERE '
           || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
           || 'a.layer = ''' || p_src_layer || ''' ';

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -955
         THEN

            psql := 'INSERT INTO ' || temp_tab_fields || ' '
                 || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_GEOID a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                 || 'a.sum_lev = ''' || p_src_layer || ''' ';

            EXECUTE IMMEDIATE psql;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ON ' || psql);

         END IF;

      END;

      psql := 'UPDATE ' || temp_tab_fields || ' a '
           || 'SET '
           || 'a.release = :p1, '
           || 'a.layer = :p2 ';

      EXECUTE IMMEDIATE psql USING p_dest_release,
                                   p_dest_layer;

      IF p_clean_house = 'Y'
      THEN

         psql := 'DELETE FROM gz_layers_fields a '
              || 'WHERE '
              || 'a.release = :p1 AND '
              || 'a.layer = :p2 ';

         EXECUTE IMMEDIATE psql USING p_dest_release,
                                      p_dest_layer;

      END IF;

      psql := 'INSERT INTO GZ_LAYERS_FIELDS '
           || 'SELECT * FROM ' || temp_tab_fields;

      EXECUTE IMMEDIATE psql;
      COMMIT;


      ----------------------
      --GZ_LAYERS_CROSSWALK - only add the missing
      ----------------------

      fieldz := GZ_OUTPUT.GET_FIELDS(p_dest_release,
                                     p_dest_layer);


      FOR i IN 1 .. fieldz.COUNT
      LOOP

         psql := 'SELECT COUNT(*) FROM '
              || 'GZ_LAYERS_CROSSWALK a '
              || 'WHERE '
              || 'a.release = :p1 AND '
              || 'a.output_field = :p2 ';

         EXECUTE IMMEDIATE psql INTO kount USING p_dest_release,
                                                 fieldz(i);

         IF kount = 0
         AND new_kount = 0
         THEN

            new_kount := new_kount + 1;

            psql := 'CREATE GLOBAL TEMPORARY TABLE ' || temp_tab_xwalk || ' '
                 || 'ON COMMIT PRESERVE ROWS '
                 || 'AS SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_CROSSWALK a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                 || 'a.output_field = ''' || fieldz(i) || ''' ';

            BEGIN

               EXECUTE IMMEDIATE psql;

            EXCEPTION
            WHEN OTHERS
            THEN

               IF SQLCODE = -955
               THEN

                  psql := 'INSERT INTO ' || temp_tab_xwalk || ' '
                       || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_CROSSWALK a '
                       || 'WHERE '
                       || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                       || 'a.output_field = ''' || fieldz(i) || ''' ';

                 EXECUTE IMMEDIATE psql;

               ELSE

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ON ' || psql);

               END IF;

            END;

         ELSIF kount = 0
         AND new_kount > 0
         THEN

            psql := 'INSERT INTO ' || temp_tab_xwalk || ' '
                 || 'SELECT * FROM ' || p_src_schema || '.GZ_LAYERS_CROSSWALK a '
                 || 'WHERE '
                 || 'a.release = ''' || p_src_release || ''' AND '  --assumption no x here
                 || 'a.output_field = ''' || fieldz(i) || ''' ';

            EXECUTE IMMEDIATE psql;

         END IF;

      END LOOP;

      IF new_kount > 0
      THEN

         psql := 'UPDATE ' || temp_tab_xwalk || ' a '
              || 'SET a.release = :p1 ';

         EXECUTE IMMEDIATE psql USING p_dest_release;

         psql := 'INSERT INTO GZ_LAYERS_CROSSWALK '
              || 'SELECT * FROM ' || temp_tab_xwalk;

         EXECUTE IMMEDIATE psql;
         COMMIT;

      END IF;


   END GZ_LAYER_COPIER;


-----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE GZ_COPY_ATTR_FROM_FACE_TO_FACE (
      p_release                IN VARCHAR2,
      p_projectid              IN VARCHAR2,
      p_face_table_name        IN VARCHAR2,
      p_src_face               IN VARCHAR2,
      p_destination_face       IN VARCHAR2
   )
   AS

     --Suzanne 1/3/12 When updating face attributes to merge sliver faces into their larger counterparts, we should
     --change ALL attributes on the sliver face so let's standardize how we do that

     --p_src_face is large face
     --p_destination_face is sliver face

        vsql varchar2(4000);
        vsql2 varchar2(4000);
        vsql3 varchar2(4000);

        vcolumn_list GZ_TYPES.stringarray;
        vCount NUMBER;

   BEGIN

        vcolumn_list := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release, p_projectid,'ATTRIBUTE','REFERENCE_FACE_FIELDS');

        vSql := 'update '||p_face_table_name||' set ';
        vSql3:= 'where face_id = '||p_destination_face;

        vCount := vcolumn_list.COUNT;

        FOR i in 1..vcolumn_list.COUNT LOOP
            IF i = vCount THEN
                vSql2 := vSql2 ||vcolumn_list(i)||' = (select '||vcolumn_list(i)||' from '||p_face_table_name||' where face_id = '||p_src_face||') ';
            ELSE
                vSql2 := vSql2 ||vcolumn_list(i)||' = (select '||vcolumn_list(i)||' from '||p_face_table_name||' where face_id = '||p_src_face||'),';
            END IF;
        END LOOP;

        vsql := vsql ||vSql2||vsql3;

        execute immediate vsql;
        commit;
        --dbms_output.put_line(vsql);

   END GZ_COPY_ATTR_FROM_FACE_TO_FACE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE GZ_COPY_FACE_ATTR_FROM_TABLE (
      p_projectid                      IN VARCHAR2,
      p_faceid                        IN VARCHAR2,
      p_face_src_table_name           IN VARCHAR2,
      p_face_destination_table_name   IN VARCHAR2
   )
   AS

     --Suzanne 1/4/12 A helper tool to restore the attributes of a face from a previous version of a face_table
     --in case manual work requires restoring the original face attributes; no measurements are copied over

        vsql varchar2(4000);
        vsql2 varchar2(4000);
        vsql3 varchar2(4000);

        vcolumn_list GZ_TYPES.stringarray;
        vCount NUMBER;

   BEGIN

        vcolumn_list := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_projectid,'ATTRIBUTE','REFERENCE_FACE_FIELDS');

        vSql := 'update '||p_face_destination_table_name||' set ';
        vSql3:= 'where face_id = '||p_faceid;

        vCount := vcolumn_list.COUNT;

        FOR i in 1..vcolumn_list.COUNT LOOP
            IF i = vCount THEN
                vSql2 := vSql2 ||vcolumn_list(i)||' = (select '||vcolumn_list(i)||' from '||p_face_src_table_name||' where face_id = '||p_faceid||') ';
            ELSE
                vSql2 := vSql2 ||vcolumn_list(i)||' = (select '||vcolumn_list(i)||' from '||p_face_src_table_name||' where face_id = '||p_faceid||'),';
            END IF;
        END LOOP;

        vsql := vsql ||vSql2||vsql3;

        execute immediate vsql;
        commit;
        --dbms_output.put_line(vsql);

   END GZ_COPY_FACE_ATTR_FROM_TABLE;
   
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

    PROCEDURE CREATE_VERTEX_TABLE (
       pGeom           SDO_GEOMETRY,
       pOutputTable    VARCHAR2,
       pDropTable      VARCHAR2 DEFAULT 'N')
    AS
       /*

       Stephanie 4/2/2012

       Creates a table with a vertex id and point sdo geometry from an
       edge sdo geometry.  (primarily for labeling vertexes in mapviewer).

       pgeom = an edge sdo geometry
       poutputtable = the name of a new table to store the vertexes in
       pDrop Table = 'Y' allows the program to drop a pre-exisitng table
                      with the same name as Output Table.

       */

       vSql                  VARCHAR2 (4000);
       vDropSql              VARCHAR2 (4000);
       vVertex_count         NUMBER;
       vlon                  NUMBER;
       vlat                  NUMBER;
       vPointGeom            SDO_GEOMETRY;
       vDropFlag             VARCHAR2 (1) := UPPER (pDropTable);
       eTableAlreadyExists   EXCEPTION;
       PRAGMA EXCEPTION_INIT (eTableAlreadyExists, -00955);
       vmsg                  VARCHAR2 (4000);

    BEGIN
           vVertex_count := 0;
           vlon := 0;
           vlat := 0;

           vsql :=
                 'Create table '
              || pOutputTable
              || ' (vid number, geom sdo_geometry) '
              || 'noparallel nologging';

           BEGIN
              EXECUTE IMMEDIATE vsql;
           EXCEPTION
              WHEN eTableAlreadyExists
              THEN
                 IF vDropFlag = 'Y'
                 THEN
                    DBMS_OUTPUT.put_line (
                       'Dropped exisitng table, ' || pOutputTable || '.');
                    vdropsql :=
                       'DROP table ' || pOutputTable || ' cascade constraints purge';

                    EXECUTE IMMEDIATE vDropSQL;

                    EXECUTE IMMEDIATE vSql;
                 ELSE
                    vmsg :=
                          'A table called '
                       || pOutputTable
                       || ' already exists. Call with pDropTable = ''Y'' to '
                       || 'drop this table.';
                    raise_application_error (-20101, vMsg);
                 END IF;
              WHEN OTHERS
              THEN
                 RAISE;
           END;


        -- loop though each ordinate pair

        FOR i IN 1 .. (pGeom.sdo_ordinates.COUNT/2)
           LOOP

             -- create sdogeometry for the vertex point

              vVertex_count := i;
              vlon := pGeom.sdo_ordinates (i * 2 -1);
              vlat := pGeom.sdo_ordinates (i * 2);

              vPointGeom :=
                       sdo_geometry (2001,
                                     pGeom.sdo_srid,
                                     SDO_POINT_TYPE (vlon, vlat, NULL),
                                     NULL,
                                     NULL);

                    vSQL :=
                          'INSERT INTO '
                       || pOutputTable
                       || ' (vid,geom) values (:p1,:p2)';

                    BEGIN
                       EXECUTE IMMEDIATE vSQL USING vVertex_count, vPointGeom;
                    EXCEPTION
                       WHEN OTHERS
                       THEN
                          RAISE;
                    END;

                    COMMIT;

        END LOOP;

    END CREATE_VERTEX_TABLE;
    
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   PROCEDURE NULL_A_JOBID (
      p_jobid                 IN VARCHAR2
   )
   AS
   
      --Matt!  10/17/13
      --Never typing any of this SQL again
      
      --usage
      --execute gz_interactive_utils.null_a_jobid('ACT13_Z8_ST10_2');
      
      psql        VARCHAR2(4000);
      
   BEGIN
   
      psql := 'update gz_job_setup '
           || 'set '
           || 'topobuild_status = NULL, clip_status = NULL, smpoly_status = NULL, linesim_status = NULL, '
           || 'merge_status = NULL, fslbuild_status = NULL, clip_attr_status = NULL, qa_status = NULL, '
           || 'shapefile_status = NULL, curr_mod = NULL, run_status = NULL '
           || 'where jobid = :p1 ';
           
           
      BEGIN
      
         EXECUTE IMMEDIATE psql USING p_jobid;
         COMMIT;
      
      EXCEPTION
      WHEN OTHERS
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' on ' || psql || ' using ' || p_jobid);
      
      END;
   
   END NULL_A_JOBID;


   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   
   PROCEDURE DIFF_GZ_OUTPUT (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_base_schema           IN VARCHAR2,                 --use this one for parameters, the driver, usually "mine"
      p_compare_schema        IN VARCHAR2,                 --the other schema.  Usually "production" or "last week"
      p_topo                  IN VARCHAR2 DEFAULT 'ALL',   --ALL or name a specific topo, like Z610LS
      p_layer_type            IN VARCHAR2 DEFAULT 'ALL',   --ALL or name a valid gz_layers_out.layer_type
      p_tracking_table_suffix IN VARCHAR2 DEFAULT '_DIFF', -->yields ACS13_Z6_DIFF
      p_use_intpoint          IN VARCHAR2 DEFAULT 'N',     --Just check if intpoint of base record is inside compare record
      p_tolerance             IN NUMBER   DEFAULT .05      --If use intpoint N, recommend 10. Have to fudge, but not too much, since LS is not consistent 
   )
   AS
   
      --Matt!  10/24/13
      --       Compare OUTPUT module shapes and record counts in 2 schemas
      --       Like to compare old code vs new code
      --       Or parameter that shouldnt have changed anything in a new run
      --See results in table like ACS13_Z6_DIFF
      --Expect there to always be some values even on a good "identical" run, since LS doesn't produce consistent results
      --   Will require some poring over the output table in mapviewer or arcgis
      
      --ex check just the split layers in Z610LS
      -- BEGIN
      --    gz_interactive_utils.diff_gz_output('ACS13','Z6','GZCPB1','GZCPB9','Z610LS','SPLIT','_DIFF','N',10);
      -- END;
      
      --ex check intpoint inside sdo, just the split layers in Z610LS. Note tolerance change
      -- BEGIN
      --    gz_interactive_utils.diff_gz_output('ACS13','Z6','GZCPB1','GZCPB9','Z610LS','SPLIT','_DIFF','Y',.05);
      -- END;
      
      --Check everything sdo to sdo, all output topologies, all layers
      -- BEGIN
      --    gz_interactive_utils.diff_gz_output('ACS13','Z6','GZCPB1','GZCPB9','ALL','ALL','_DIFF',10);
      -- END;
      
      --Review strategy. 1) Check for mismatched record counts, the obvious
      --   select * from ACS13_Z6_DIFF where table_or_msg like 'mismatched%'
      --2) Put the compare on top of the base in mapviewer, then switch
      --   Top color black, bottom red, to see peek-outs.  No fill
      --    select base_sdo from ACS13_Z6_DIFF
      --    select compare_sdo from ACS13_Z6_DIFF

      
      
      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      topos             gz_types.stringarray;
      tabs              gz_types.stringarray;
      traker            VARCHAR2(32);
      base_geoidz       gz_types.stringarray;
      compare_geoidz    gz_types.stringarray;
      relatez           gz_types.stringarray;
      base_sdoz         gz_types.geomarray;
      compare_sdoz      gz_types.geomarray;
      
   
   BEGIN
   
      traker := p_release || '_' || p_gen_project_id || p_tracking_table_suffix;
      
      IF GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(traker)
      THEN
      
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || traker;
         
      ELSE
      
         EXECUTE IMMEDIATE 'CREATE TABLE ' || traker 
                        || '(table_or_msg VARCHAR2(256), geo_id VARCHAR2(4000), determine VARCHAR2(32), '
                        || 'base_sdo SDO_GEOMETRY, compare_sdo SDO_GEOMETRY, the_time timestamp) ';
      
      END IF;
      
      IF p_topo <> 'ALL'
      THEN
      
         psql := 'SELECT DISTINCT topology FROM all_sdo_topo_info '
              || 'WHERE owner = :p1 AND '
              || 'topology = :p2 ORDER BY 1';
              
         EXECUTE IMMEDIATE psql BULK COLLECT INTO topos USING UPPER(p_base_schema),
                                                              UPPER(p_topo);
              
      ELSE
      
         psql := 'SELECT DISTINCT topology FROM all_sdo_topo_info '
              || 'WHERE owner = :p1 AND '
              || 'topology LIKE :p2 ORDER BY 1';
              
         EXECUTE IMMEDIATE psql BULK COLLECT INTO topos USING UPPER(p_base_schema),
                                                              UPPER(p_gen_project_id) || '%LS';      
      
      END IF;
        
      IF topos.COUNT = 0
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'No topos! ' || psql );
      
      END IF;
      
      FOR i IN 1 .. topos.count
      LOOP
      
         psql := 'INSERT INTO ' || traker || ' VALUES (:p1,:p2,:p3,NULL,NULL,:p4)';
         EXECUTE IMMEDIATE psql USING 'start ' || topos(i), 
                                      p_base_schema, 
                                      p_compare_schema, 
                                      systimestamp; 
         COMMIT;
      
         IF p_layer_type <> 'ALL'
         THEN
         
            psql := 'SELECT a.table_name FROM all_sdo_topo_info a, '
                 || p_base_schema || '.gz_layers_out b '
                 || 'WHERE a.topology = :p1 AND '
                 || 'a.owner = :p2 AND '
                 || 'b.release = :p3 AND b.gen_project_id = :p4 AND b.layer_type = :p5 '
                 || 'and a.table_name = a.topology || ''_FSL'' || b.layer || ''V'' '
                 || 'order by 1 ';
         
            EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING topos(i),
                                                                UPPER(p_base_schema),
                                                                p_release,
                                                                p_gen_project_id,
                                                                p_layer_type;
                                                                
         ELSE
         
            psql := 'SELECT a.table_name FROM all_sdo_topo_info a, '
                 || p_base_schema || '.gz_layers_out b '
                 || 'WHERE a.topology = :p1 AND '
                 || 'a.owner = :p2 AND '
                 || 'b.release = :p3 AND b.gen_project_id = :p4 '
                 || 'and a.table_name = a.topology || ''_FSL'' || b.layer || ''V'' '
                 || 'order by 1 ';
         
            EXECUTE IMMEDIATE psql BULK COLLECT INTO tabs USING topos(i),
                                                                UPPER(p_base_schema),
                                                                p_release,
                                                                p_gen_project_id;
         
         END IF;
         
         FOR j IN 1 .. tabs.count
         LOOP
         
            --Compare record counts
            
            psql := 'select a.kounta - b.kountb from '
                 || '(select count(*) kounta from '
                 || p_base_schema || '.' || tabs(j) || ' a) a, '
                 || '(select count(*) kountb from '
                 || p_compare_schema || '.' || tabs(j) || ' b) b ';
                 
            EXECUTE IMMEDIATE psql INTO kount;
            
            IF kount <> 0
            THEN
            
               psql := 'INSERT INTO ' || traker || ' VALUES (:p1,NULL,NULL,NULL,NULL,:p2)';
               
               EXECUTE IMMEDIATE psql USING 'mismatched record counts on ' || tabs(j), systimestamp; 
               COMMIT;
            
            END IF;
            
            --the biggie, compare shapes, geo_id<-->geo_id
            
            IF p_use_intpoint = 'N'
            THEN
            
               psql := 'SELECT a.geo_id, sdo_geom.relate(a.sdogeometry, :p1, b.sdogeometry, :p2), a.sdogeometry, b.sdogeometry '
                    || 'FROM '
                    || p_base_schema || '.' || tabs(j) || ' a, '
                    || p_compare_schema || '.' || tabs(j) || ' b '
                    || 'WHERE a.geo_id = b.geo_id '
                    || 'AND sdo_geom.relate(a.sdogeometry, :p3, b.sdogeometry, :p4) <> :p5 ';
                    
               EXECUTE IMMEDIATE psql BULK COLLECT INTO base_geoidz, relatez,
                                                        base_sdoz, compare_sdoz
                                                   USING 'MASK=DETERMINE', p_tolerance,
                                                         'MASK=EQUAL', p_tolerance, 'MASK=EQUAL';
                                                         
            ELSE
            
               --avoid having to review hundreds of similar shapes that just differ in their LS outputs here and there
               --and check if the intpoint of the base is inside the compare sdo
               
               psql := 'SELECT a.geo_id, SDO_GEOM.RELATE(SDO_UTIL.INTERIOR_POINT(a.sdogeometry,:p1) , :p2, b.sdogeometry, :p3), a.sdogeometry, b.sdogeometry '
                    || 'FROM '
                    || p_base_schema || '.' || tabs(j) || ' a, '
                    || p_compare_schema || '.' || tabs(j) || ' b '
                    || 'WHERE a.geo_id = b.geo_id '
                    || 'AND SDO_GEOM.RELATE(SDO_UTIL.INTERIOR_POINT(a.sdogeometry,:p4) , :p5, b.sdogeometry, :p6) <> :p7 ';
                    
               EXECUTE IMMEDIATE psql BULK COLLECT INTO base_geoidz, relatez,
                                                        base_sdoz, compare_sdoz
                                                   USING p_tolerance, 'MASK=DETERMINE', p_tolerance,
                                                         p_tolerance, 'MASK=INSIDE', p_tolerance, 'MASK=INSIDE';
            
            
            END IF;
            
            IF base_geoidz.COUNT > 0
            THEN
            
               --dbms_output.put_line(psql);
            
               FORALL ii IN i .. base_geoidz.COUNT
                  EXECUTE IMMEDIATE 'INSERT INTO ' || traker || ' VALUES (:p1,:p2,:p3,:p4,:p5,:p5)'
                  USING tabs(j), base_geoidz(ii), relatez(ii), base_sdoz(ii), compare_sdoz(ii), systimestamp; 
               
               COMMIT;
            
            END IF;
         
         
         END LOOP;
         
      END LOOP;   
      
      
      psql := 'INSERT INTO ' || traker || ' VALUES (:p1,:p2,:p3,NULL,NULL,:p4)';
      EXECUTE IMMEDIATE psql USING 'Done checking ' || topos.COUNT || ' topos', 
                                   p_base_schema, 
                                   p_compare_schema, 
                                   systimestamp; 
      COMMIT; 
                                  
   
   END DIFF_GZ_OUTPUT;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   PROCEDURE DIFF_GZ_BUILD (
      p_source_schema         IN VARCHAR2,
      p_source_topo           IN VARCHAR2,
      p_compare_schema        IN VARCHAR2,
      p_compare_topo          IN VARCHAR2,
      p_output_table          IN VARCHAR2 DEFAULT 'BUILDCOMPARE',
      p_tolerance             IN NUMBER DEFAULT .05,
      p_require_geoid_idx     IN NUMBER DEFAULT 1    
   )
   AS
   
      --Matt! 1/29/14
      --Hacky comparer for two initial topos
      --Useful when I change some code and want to check that a new topo build run
      --  completely matches face-table-wise for
      --  record counts, geoid universe, and sdo match face<-->face
      
      --sample usage
      --begin
      --   gz_interactive_utils.diff_gz_build('SCHEL010','Z999IN','GZCPB9','Z999IN','Z999IN_BUILDCOMPARE');
      --end;
      
      --Note: Adding an index on geoid in both face tables can make the 
      --      difference between a minute and a lifetime here
      --      In the case of large topos copying a remote topo locally then building the index may actually be faster
      --      than going without the geoid index
      -- begin
      --  gz_business_utils.add_index('Z699IN_FACE','Z699IN_FACE_GIDX','GEOID');
      -- end;
      --
      --OR, if the topo is truly huge just copy over the relevant face table columns
      --instead of the entire topo
      --   CREATE TABLE zx99in_face as
      --   SELECT face_id, geoid, sdogeometry from gzcpb9.z699in_face;
      --
      --   ALTER TABLE zx99in_face
      --   ADD CONSTRAINT zx99in_facepkc PRIMARY KEY (face_id);
      --
      --   begin
      --    gz_business_utils.add_index('ZX99IN_FACE','ZX99IN_FACE_GIDX','GEOID');
      --   end;
      
      --otherwise, set p_require_geoid_idx to 0 if willing to thumb twiddle
   
      psql                 VARCHAR2(4000);
      badc_psql            VARCHAR2(4000);
      bads_psql            VARCHAR2(4000);
      message              VARCHAR2(4000) := '';
      geoidz               GZ_TYPES.stringarray;
      kountz               GZ_TYPES.numberarray;
      kountz2              GZ_TYPES.numberarray;
      kount                PLS_INTEGER;
      kount_sidxs          PLS_INTEGER;
      kount_cidxs          PLS_INTEGER;
      source_geomz         GZ_TYPES.geomarray;
      compare_geomz        GZ_TYPES.geomarray;
      source_facez         GZ_TYPES.numberarray;
      compare_facez        GZ_TYPES.numberarray;
      compare_found_iis    GZ_TYPES.numberhash;
      found_match          PLS_INTEGER := 0;
      null_geom            sdo_geometry;
      start_time           TIMESTAMP;
      
   BEGIN
   
      start_time := systimestamp;
      
      -----------------------
      --1) set up output table
      -----------------------
      
      IF GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_output_table, TRUE)
      THEN
      
         EXECUTE IMMEDIATE 'TRUNCATE TABLE ' || p_output_table; 
         
      ELSE 

         EXECUTE IMMEDIATE 'CREATE TABLE ' || p_output_table 
                        || '(geoid VARCHAR2(4000), is_problem VARCHAR2(4000), source_schema VARCHAR2(32), compare_schema VARCHAR2(32), '
                        || 'source_face_id NUMBER, compare_face_id NUMBER, source_geometry SDO_GEOMETRY, compare_geometry SDO_GEOMETRY)';
                        
      END IF;
      
      -------------------------------
      -- 2) Check for index on geoid
      -------------------------------
      
      psql := 'SELECT COUNT(*) FROM all_ind_columns a '
           || 'WHERE '
           || 'a.table_owner = :p1 AND '
           || 'a.table_name = :p2 AND '
           || 'a.column_name = :p3 ';
           
      EXECUTE IMMEDIATE psql INTO kount_sidxs USING UPPER(p_source_schema),
                                                    UPPER(p_source_topo) || '_FACE',
                                                    'GEOID';
                                                    
      EXECUTE IMMEDIATE psql INTO kount_cidxs USING UPPER(p_compare_schema),
                                                    UPPER(p_compare_topo) || '_FACE',
                                                    'GEOID';                                             
      
      IF kount_sidxs = 0
      OR kount_cidxs = 0
      THEN
      
         message := 'No index on geoid in ';
      
         IF kount_sidxs = 0
         THEN
         
            message := message || p_source_schema || '.' || p_source_topo || '_FACE ';
            
            IF kount_cidxs = 0
            THEN
            
               message := message || 'and ';
             
            END IF;
          
         END IF;
         
         IF kount_cidxs = 0
         THEN
         
            message := message || p_compare_schema || '.' || p_compare_topo || '_FACE ';
         
         END IF;
         
         IF p_require_geoid_idx = 1
         THEN
      
            RAISE_APPLICATION_ERROR(-20001, message || '. This could take forever to run. '
                                                    || 'Set p_require_geoid_idx to 0 if you wanna go for it anyway');
                                           
         ELSE
         
            psql := 'INSERT INTO ' ||  p_output_table || '(geoid, is_problem) '
                 || 'VALUES(:p1,:p2)';
            
            EXECUTE IMMEDIATE psql USING 'Geoid column itself', 'WARNING: ' || message;
            COMMIT;
         
         END IF;
      
      END IF;

                
      ----------------------------
      --3) compare geoid universe 
      ----------------------------   

      psql := 'SELECT geoid FROM ' || p_source_schema || '.' || p_source_topo || '_face '
           || 'MINUS '
           || 'SELECT geoid FROM ' || p_compare_schema || '.' || p_compare_topo || '_face ';
      
      EXECUTE IMMEDIATE psql BULK COLLECT INTO geoidz;
      
      psql := 'INSERT INTO ' ||  p_output_table || '(geoid, is_problem, source_schema, compare_schema) '
           || 'VALUES(:p1,:p2,:p3,:p4)';
      
      FORALL ii IN 1 ..geoidz.COUNT
         EXECUTE IMMEDIATE psql USING geoidz(ii),
                                      'geoid in source schema only',
                                      p_source_schema,
                                      p_compare_schema;
                                      
      COMMIT;
      
      psql := 'SELECT geoid FROM ' || p_compare_schema || '.' || p_compare_topo || '_face '
           || 'MINUS '
           || 'SELECT geoid FROM ' || p_source_schema || '.' || p_source_topo || '_face ';
      
      EXECUTE IMMEDIATE psql BULK COLLECT INTO geoidz;
      
      psql := 'INSERT INTO ' ||  p_output_table || '(geoid, is_problem, source_schema, compare_schema) '
           || 'VALUES(:p1,:p2,:p3,:p4)';
      
      FORALL ii IN 1 ..geoidz.COUNT
         EXECUTE IMMEDIATE psql USING geoidz(ii),
                                      'geoid in compare schema only',
                                      p_source_schema,
                                      p_compare_schema;
                                      
      COMMIT;
      
      -------------------------------------
      --4) compare counts by existing geoids
      -------------------------------------   

      psql := 'SELECT a.geoid, a.kount, b.kount FROM '
           || '(SELECT DISTINCT geoid, COUNT(geoid) kount '
           || 'FROM ' || p_source_schema || '.' || p_source_topo || '_face '
           || 'GROUP BY geoid) a, '
           || '(SELECT DISTINCT geoid, COUNT(geoid) kount '
           || 'FROM ' || p_compare_schema || '.' || p_source_topo || '_face '
           || 'GROUP BY geoid) b '
           || 'WHERE a.geoid = b.geoid '
           || 'AND a.kount <> b.kount ';
              
      EXECUTE IMMEDIATE psql BULK COLLECT INTO geoidz, 
                                               kountz,
                                               kountz2;
         
      psql := 'INSERT INTO ' ||  p_output_table || '(geoid, is_problem, source_schema, compare_schema) '
           || 'VALUES(:p1,:p2,:p3,:p4)';
                                                       
      FORALL ii IN 1 .. geoidz.COUNT
         EXECUTE IMMEDIATE psql USING geoidz(ii),
                                      'mismatched geoid count',
                                      kountz(ii),
                                      kountz2(ii);
                                      
      COMMIT;
      
      ----------------------------
      --5) compare shapes by geoid
      ----------------------------
      
      --set this one time
      bads_psql := 'INSERT INTO ' || p_output_table || ' '
                || '(geoid, is_problem, source_schema, compare_schema, source_face_id, source_geometry) '
                || 'VALUES (:p1, :p2, :p3, :p4, :p5, :p6) ';
               
      --set this one time
      badc_psql := 'INSERT INTO ' || p_output_table || ' '
                || '(geoid, is_problem, source_schema, compare_schema, compare_face_id, compare_geometry) '
                || 'VALUES (:p1, :p2, :p3, :p4, :p5, :p6) ';
                
      psql := 'SELECT DISTINCT a.geoid '
           || 'FROM ' 
           || p_source_schema || '.' || p_source_topo || '_face a ';
              
      EXECUTE IMMEDIATE psql BULK COLLECT INTO geoidz;
      
      FOR i IN 1 .. geoidz.COUNT
      LOOP
      
         --get source face_ids and sdo for this geoid.  May be several
         
         psql := 'SELECT a.face_id, a.sdogeometry '
              || 'FROM ' || p_source_schema || '.' || p_source_topo || '_face a '
              || 'WHERE a.geoid = :p1 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO source_facez,
                                                  source_geomz USING geoidz(i);
                                                                               
         --get the compare sdos
         
         psql := 'SELECT a.face_id, a.sdogeometry '
              || 'FROM ' || p_compare_schema || '.' || p_compare_topo || '_face a '
              || 'WHERE a.geoid = :p1 ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO compare_facez,
                                                  compare_geomz USING geoidz(i);
         
         --loop thru source and look for compares that are EQUAL within .05
         
         FOR j IN 1 .. source_facez.COUNT
         LOOP
         
            FOR ii IN 1 .. compare_facez.COUNT
            LOOP
            
               IF NOT compare_found_iis.EXISTS(TO_CHAR(ii))  --if this compare has already been matched, skip the relate
               THEN
            
                  IF sdo_geom.relate(source_geomz(j), 'EQUAL', compare_geomz(ii), p_tolerance) = 'EQUAL'
                  THEN
                  
                     --note so we dont have to compare on another loop
                     compare_found_iis(to_char(ii)) := 1;
                     found_match := 1;
                     
                     EXIT;
                  
                  END IF;
                  
               END IF;
            
            END LOOP;
            
            IF found_match = 0
            THEN
            
               --bad, never found a match for this source geoid/face
                    
               EXECUTE IMMEDIATE bads_psql USING geoidz(i),
                                                 'no matching geometry',
                                                 UPPER(p_source_schema),
                                                 UPPER(p_compare_schema),
                                                 source_facez(j),
                                                 source_geomz(j);
               COMMIT;
               
            ELSE
            
               --good, reset for next source face in the geoid loop
               found_match := 0;
            
            END IF;
         
         END LOOP;
         
         --now make sure that all of the compares for this geoid got matched
         FOR j IN 1 .. compare_facez.COUNT
         LOOP
         
            --order is maintained            
            IF NOT compare_found_iis.EXISTS(TO_CHAR(j))  
            THEN
            
               EXECUTE IMMEDIATE badc_psql USING geoidz(i),
                                                 'no matching geometry',
                                                 UPPER(p_source_schema),
                                                 UPPER(p_compare_schema),
                                                 compare_facez(j),
                                                 compare_geomz(j);
                                                
            END IF;
         
         END LOOP;
         
         compare_found_iis.DELETE;
      
         --log breadcrumbs
         IF mod(i,1000) = 0
         THEN
         
            psql := 'INSERT INTO ' || p_output_table 
                 || '(geoid, is_problem) '
                 || 'VALUES (:p1, :p2) ';
               
            EXECUTE IMMEDIATE psql USING 'None', 
                                         'Processed another 1000 geoids';
                                         
            COMMIT;
         
         END IF;
      
      END LOOP;   
      
      psql := 'INSERT INTO ' || p_output_table 
               || '(geoid, is_problem) '
               || 'VALUES (:p1, :p2) ';
               
      EXECUTE IMMEDIATE psql USING 'None', 
                                   'Dun! ' || to_char(start_time) || ' to ' || to_char(systimestamp);
      
      COMMIT;
      
   END DIFF_GZ_BUILD;
   
   
   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
   FUNCTION CONVERT_DEGREES_TO_METERS (
      p_degrees               IN NUMBER,
      p_latitude              IN NUMBER DEFAULT 45,
      p_round                 IN NUMBER DEFAULT 2
   ) RETURN NUMBER DETERMINISTIC
   AS
   
      --Matt! 2/25/14
      --Only appropriate for cocktail napkin-type work. Saves me googling the 111.whut number
      --Returns "Minumum" meters, north-south direction
      --Don't EVER let Sidey see this, he'd smack me with a slide rule
      
      --select gz_interactive_utils.convert_degrees_to_meters(.0000005) from dual
      --returns .04
      
      mypi                    NUMBER := asin(1)*2;
      
   BEGIN
   
      IF ABS(p_latitude) >= 90
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Your latitude ' || p_latitude || ' is in outer space!'); 
      
      END IF;
   
      RETURN ROUND((p_degrees * cos(p_latitude * mypi/180) * 111320), p_round);

   END CONVERT_DEGREES_TO_METERS;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION CONVERT_METERS_TO_DEGREES (
      p_meters                IN NUMBER,
      p_latitude              IN NUMBER DEFAULT 45,
      p_round                 IN NUMBER DEFAULT 6
   ) RETURN NUMBER DETERMINISTIC
   AS
   
      --Matt! 2/25/14
      --Only appropriate for cocktail napkin-type work.  Saves me from googling the 111.whut number again
      --Returns "Minumum" meters, north-south direction
      --Don't EVER let Sidey see this, he'd smack me with a slide rule
      
      --select gz_interactive_utils.convert_meters_to_degrees(.05) from dual
      --returns 0.000001
      
      mypi                    NUMBER := asin(1)*2;

   BEGIN

      IF ABS(p_latitude) >= 90
      THEN
      
         RAISE_APPLICATION_ERROR(-20001, 'Your latitude ' || p_latitude || ' is in outer space!'); 
      
      END IF;
      
      RETURN ROUND((p_meters / cos(p_latitude * mypi/180) / 111320), p_round);
      
   END CONVERT_METERS_TO_DEGREES;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------
   
END GZ_INTERACTIVE_UTILS;
/
