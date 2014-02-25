CREATE OR REPLACE PACKAGE BODY GZ_BUILD_SOURCE
AS
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   --SEE
   --GENERALIZATION_TOPO_BUILD
   --For entry point to this package
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   PROCEDURE START_BUILD_LOGGING (
      p_schema         IN VARCHAR2,
      p_project_id     IN VARCHAR2,
      p_topo_out       IN VARCHAR2
   )
   AS

      --Matt! 2/6/12
      --Create logging table for this job

   BEGIN

      GZ_BUSINESS_UTILS.CREATE_GEN_XTEND_TRACKING_LOG(p_schema,
                                                 p_topo_out || '_BUILD_TRACKING');

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',
                                             p_topo_out,
                                             'START_BUILD_LOGGING',
                                             'STARTING JOB: ' || p_topo_out);


   END START_BUILD_LOGGING;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   PROCEDURE SET_UP (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2
   )
   AS

      --Matt! 2/06/12
      --Create all work tables and drop any topology (gotta do this before table creation)


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('SET_UP: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --drop topology

      BEGIN
         GZ_TOPO_UTIL.purge_topology(p_schema, p_output_topology);
      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLERRM LIKE '%does not exist in%'
         THEN

            --ORA-20001: Topology JUNK does not exist in GZCPB1
            NULL;

         ELSE

            RAISE;

         END IF;

      END;

      --create and drop (if they exist) work tables

      --note procedure name and table name mismatch
      --made it BUILD_LAYERS to be consistent
      GZ_BUSINESS_UTILS.CREATE_GZ_LAYER_INFO(p_schema, p_output_topology || '_BUILD_LAYERS');

      --this one gets created in advance, its columns are fixed
      GZ_BUSINESS_UTILS.CREATE_GZ_BUILD_POLY(p_schema, p_output_topology || '_BUILD_POLY');

      --this one gets created in advance, its columns are fixed
      GZ_BUSINESS_UTILS.CREATE_GZ_BUILD_EDGE(p_schema, p_output_topology || '_BUILD_EDGE');

      --this one gets created in advance, its columns are fixed
      GZ_BUSINESS_UTILS.CREATE_GZ_BUILD_TILE(p_schema, p_output_topology || '_BUILD_TILE');

      --ditto
      GZ_BUSINESS_UTILS.CREATE_GZ_BUILD_GEOM(p_schema, p_output_topology || '_BUILD_GEOM');


      --probably some more...

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('SET_UP: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END SET_UP;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION VERIFY_BUILD_INPUTS (
      p_schema                      IN VARCHAR2,
      p_release                     IN VARCHAR2,
      p_project_id                  IN VARCHAR2,
      p_source_schema               IN VARCHAR2,
      p_source_topology             IN VARCHAR2,
      p_output_topology             IN VARCHAR2,
      p_sdo_filter                  IN SDO_GEOMETRY
   ) RETURN VARCHAR2
   AS

      --Matt 2/6/12
      --! 8/2/13 added check that layer name doesnt match add_to_face on gz_layers_out

      psql              VARCHAR2(4000);
      output            VARCHAR2(4000);
      build_layers      GZ_TYPES.GZ_LAYERS_IN;
      ref_tables        GZ_TYPES.stringarray;
      kount             PLS_INTEGER;
      measurements      GZ_TYPES.stringarray;
      add_to_face       VARCHAR2(32);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_BUILD_INPUTS: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'VERIFY_BUILD_INPUTS',NULL,'STARTING ' || p_output_topology);

      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS('GZ_LAYERS_IN')
      THEN

         output := output || 'Dude, GZ_LAYERS_IN doesnt exist or is empty | ';
         --fatal, cant continue
         return output;

      END IF;


      --get input layers
      build_layers := GZ_BUILD_SOURCE.GET_LAYERS(p_release, p_project_id);

      IF build_layers.COUNT = 0
      THEN

         output := output || 'Dude GZ_LAYERS_IN doesnt have any records for project id ' || p_project_id;

      END IF;

      --any reference tables. Just reference_schemas  and reference face fields at the moment
      ref_tables := GZ_TYPES.LEGAL_REFERENCE_TABLES();

      FOR i IN 1 .. ref_tables.COUNT
      LOOP

         IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(ref_tables(i))
         THEN

            output := output || 'Dude, reference table ' || ref_tables(i) || ' doesnt exist | ';

         END IF;

      END LOOP;

      --check that we have measurements for this project and release now
      --error is in final module if not

      BEGIN

         --throws an error
         measurements := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                                p_project_id,
                                                               'MEASUREMENT',
                                                               'REFERENCE_FACE_FIELDS');

      EXCEPTION
      WHEN OTHERS
      THEN

         output := output || 'Dude, REFERENCE_FACE_FIELDS doesnt have any measurements for '
                          || 'release ' || p_release || ', project_id ' || p_project_id;

      END;



      --check that input topo exists
      psql := 'SELECT count(*) '
           || 'FROM all_sdo_topo_info a '
           || 'WHERE '
           || 'a.owner = :p1 AND '
           || 'a.topology = :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING UPPER(p_source_schema),
                                              UPPER(p_source_topology);

      IF kount = 0
      THEN

         output := output || 'Dude, theres no topology called ' || p_source_topology || ' owned by schema ' || p_source_schema;

      END IF;

      --first round of checks
      IF output IS NOT NULL
      THEN

         RETURN output;

      END IF;

      --check that filter is reasonable

      IF p_sdo_filter IS NOT NULL
      THEN

         psql := 'SELECT COUNT(*) '
              || 'FROM '
              || p_source_schema || '.' || p_source_topology || '_edge$ e '
              || 'WHERE '
              || 'SDO_RELATE(e.geometry, :p1, :p2) = :p3 ';

         BEGIN

            EXECUTE IMMEDIATE psql INTO kount USING p_sdo_filter,
                                                    'mask=ANYINTERACT',
                                                    'TRUE';

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%window SRID does not match layer SRID%'
            THEN

               output := output || 'Dude, your input window SRID doesnt match the primitives SRID in '
                                || p_source_schema || ' topology ' || p_source_topology;
            ELSE

               RAISE;

            END IF;

         END;


         IF kount = 0
         THEN

            output := output || 'Dude, input window doesnt touch any primitives in '
                             || p_source_schema || ' topology ' || p_source_topology;

         END IF;



      END IF;

      --check that none of the layer names match the add_to_face value on gz_layers_out
      --this can happen if there is a STATE layer and we want to add a statefp value to
      --the face table in a column also called STATE, for example
      --The add_to_face column ends up replacing the input layer in the output build module

      psql := 'SELECT a.add_to_face FROM gz_layers_out a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 AND '
           || 'a.add_to_face IS NOT NULL ';

      EXECUTE IMMEDIATE psql INTO add_to_face USING p_release,
                                                    p_project_id;

      IF add_to_face IS NOT NULL
      THEN

         FOR i IN 1 .. build_layers.COUNT
         LOOP

            IF UPPER(build_layers(i).layer) = UPPER(add_to_face)
            THEN

               output := output || 'Dude, initial layer ' || build_layers(i).layer || ' will get wiped from the face table '
                                || 'by the output build gz_layers_out.add_to_face value of the same name ';

            END IF;

         END LOOP;

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_BUILD_INPUTS: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'VERIFY_BUILD_INPUTS',NULL,'FINISHED ' || p_output_topology);

      IF output IS NULL
      THEN
         output := '0';
      END IF;

      RETURN output;

   END VERIFY_BUILD_INPUTS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   FUNCTION STRINGARRAY2HASH (
      p_input       IN GZ_TYPES.stringarray
   )
   RETURN GZ_TYPES.stringhash DETERMINISTIC
   AS

     splithash     GZ_TYPES.stringhash;

   BEGIN

        FOR i IN 1 .. p_input.COUNT
        LOOP
           splithash(p_input(i)) := '1';
        END LOOP;

        RETURN splithash;

   END STRINGARRAY2HASH;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_LAYERS (
      p_release                  IN VARCHAR2,
      p_project_id               IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_IN
   AS

      --Matt! 02/06/12

      psql     VARCHAR2(4000);
      output   GZ_TYPES.GZ_LAYERS_IN;

   BEGIN

      psql := 'SELECT a.* FROM GZ_LAYERS_IN a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 ';

      BEGIN

         EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_release),
                                                               UPPER(p_project_id);

      EXCEPTION

         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Yo, no layers in GZ_LAYERS_IN for project id ' || p_project_id || ' and release ' || p_release);
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table GZ_LAYERS_IN does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if GZ_LAYERS_IN table matches GZ_LAYERS_IN type');
            ELSE
               RAISE;
            END IF;

      END;

      RETURN output;

   END GET_LAYERS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_LAYER_INFO (
      p_topo               IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_IN_INFO
   AS

      --Matt! 02/06/12

      psql     VARCHAR2(4000);
      output   GZ_TYPES.GZ_LAYERS_IN_INFO;

   BEGIN

      psql := 'SELECT a.* FROM ' || p_topo || '_BUILD_LAYERS a ';

      BEGIN

         EXECUTE IMMEDIATE psql BULK COLLECT INTO output;

      EXCEPTION

         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Yo, no layers in ' || p_topo || '_BUILD_LAYERS ');
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table ' || p_topo || '_LAYERS does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if ' || p_topo || '_BUILD_LAYERS table matches GZ_LAYER_INFO type');
            ELSE
               RAISE;
            END IF;

      END;

      RETURN output;

   END GET_LAYER_INFO;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_EXTENT_LAYER (
      p_topo               IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_IN_INFO_REC
   AS

      --Matt! 02/13/12

      psql     VARCHAR2(4000);
      output   GZ_TYPES.GZ_LAYERS_IN_INFO_REC;

   BEGIN

      psql := 'SELECT a.* FROM ' || p_topo || '_BUILD_LAYERS a '
           || 'WHERE a.topo_extent_lyr = :p1';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING 'Y';

      EXCEPTION

         WHEN NO_DATA_FOUND THEN
            --allow a return of NULL here, there may be no extent layer populated
            RETURN output;
            --RAISE_APPLICATION_ERROR(-20001,'Yo, no layers in ' || p_topo || '_LAYER_INFO ');
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table ' || p_topo || '_BUILD_LAYERS does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if ' || p_topo || '_BUILD_LAYERS table matches GZ_LAYER_INFO type');
            ELSE
               RAISE;
            END IF;

      END;

      RETURN output;

   END GET_EXTENT_LAYER;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION PIPE_LAYERS (
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_table       IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_source_key         IN VARCHAR2,
      p_where_clause       IN VARCHAR2,
      p_boundary_def       IN VARCHAR2,
      p_sdo_filter         IN SDO_GEOMETRY DEFAULT NULL,
      p_alt_source_schema  IN VARCHAR2 DEFAULT NULL,
      p_topo_extent_lyr    IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.GZ_LAYERS_IN_INFO PIPELINED
   AS

      --Matt! 2/6/12

      output         GZ_TYPES.GZ_LAYERS_IN_INFO_REC;

   BEGIN

      --really just returning a single rec, not a table (for now at least)
      --simple for now, expecting more

      output.release        := UPPER(p_release);
      output.gen_project_id := UPPER(p_project_id);
      output.layer          := UPPER(p_layer);
      output.jobid          := NULL; --never seems relevant, dont ever know it

      IF p_alt_source_schema IS NOT NULL
      THEN

         --from parameter table
         output.source_schema := UPPER(p_alt_source_schema);

      ELSE

         --otherwise same as all other layers, from call
         output.source_schema := UPPER(p_source_schema);

      END IF;

      output.source_table    := UPPER(p_source_table);
      output.source_topology := UPPER(p_source_topology);
      output.source_key      := UPPER(p_source_key);
      output.where_clause    := p_where_clause;

      --not messing with table joins for now

      output.layer_sql := 'SELECT a.' || output.source_key || ' '
                       || 'FROM '
                       || output.source_schema || '.' || output.source_table || ' a '
                       || 'WHERE '
                       || p_where_clause || ' ';

      output.boundary_definition := UPPER(p_boundary_def);

      output.sdo_filter :=  p_sdo_filter;


      IF p_topo_extent_lyr IS NULL
      THEN

         output.topo_extent_lyr := 'N';

      ELSE

         output.topo_extent_lyr := 'Y';

      END IF;

      PIPE ROW(output);


   END PIPE_LAYERS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION CREATE_LAYER_INFO (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_sdo_filter         IN SDO_GEOMETRY DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! 2/6/12

      layers         GZ_TYPES.GZ_LAYERS_IN;
      psql           VARCHAR2(4000);
      layer_info     GZ_TYPES.GZ_LAYERS_IN_INFO;
      got_xtent      VARCHAR2(32);

   BEGIN

      --get input layers
      layers := GZ_BUILD_SOURCE.GET_LAYERS(p_release,
                                           p_project_id);

      --layer info table got created in SET_UP
      --named like Z601IN_BUILD_LAYERS

      FOR i IN 1 .. layers.COUNT
      LOOP

         --check
         IF layers(i).topo_extent_lyr IS NOT NULL
         AND got_xtent IS NULL
         THEN

             got_xtent := layers(i).layer;

         ELSIF layers(i).topo_extent_lyr IS NOT NULL
         AND got_xtent IS NOT NULL
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Sorry, but both ' || got_xtent || ' and ' || layers(i).layer || ' are flagged as topo_extent_lyr');

         END IF;

         --probably just returns a single record

         psql := 'INSERT INTO ' || p_output_topology || '_BUILD_LAYERS '
              || 'SELECT * FROM TABLE(GZ_BUILD_SOURCE.PIPE_LAYERS(:p1,:p2,:p3,:p4,:p5,:p6,:p7,:p8,:p9,:p10,:p11,:p12)) ';

         EXECUTE IMMEDIATE psql USING p_release,
                                      p_project_id,
                                      layers(i).layer,
                                      p_source_schema,
                                      layers(i).source_table,
                                      p_source_topology,
                                      layers(i).source_key,
                                      layers(i).where_clause,
                                      layers(i).boundary_definition,
                                      p_sdo_filter,
                                      layers(i).alt_source_schema,
                                      layers(i).topo_extent_lyr;

         COMMIT;

      END LOOP;


      --quickie test, accessor could error

      layer_info := GZ_BUILD_SOURCE.GET_LAYER_INFO(p_output_topology);

      --double check
      IF layer_info.COUNT > 0
      THEN

         RETURN '0';

      ELSE

         RETURN p_output_topology || '_build_layers table has no layers!';

      END IF;

   END CREATE_LAYER_INFO;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION PIPE_BUILD_POLY (
      p_layer                 IN VARCHAR2,
      p_source_schema         IN VARCHAR2,
      p_source_table          IN VARCHAR2,
      p_source_topology       IN VARCHAR2,
      p_output_topology       IN VARCHAR2,
      p_source_key            IN VARCHAR2,
      p_where_clause          IN VARCHAR2,
      p_boundary_definition   IN VARCHAR2,
      p_sdo_filter            IN SDO_GEOMETRY DEFAULT NULL,
      p_rigorous_filter       IN PLS_INTEGER DEFAULT 0
   ) RETURN GZ_TYPES.GZ_BUILD_POLY PIPELINED
   AS

      --Matt! 2/9/12
      --2/4/13 Added code for exact filter.  Not parameterized in the workflow,
      --       hardcoded to NO in create_build_poly and passed here

      build_tab            GZ_TYPES.GZ_BUILD_POLY;
      psql                 VARCHAR2(4000);
      my_cursor            SYS_REFCURSOR;
      stack                VARCHAR2(4000);
      psql_filtered        VARCHAR2(4000);
      filter_masks         GZ_TYPES.stringarray;
      bkount               PLS_INTEGER := 1;

   BEGIN

      IF p_sdo_filter IS NOT NULL
      AND p_rigorous_filter = 1
      THEN

         --almost hard coded for the development only filter option
         filter_masks(1) := 'INSIDE+COVEREDBY';
         filter_masks(2) := 'COVERS';
         filter_masks(3) := 'OVERLAPBDYINTERSECT';
         filter_masks(4) := 'OVERLAPBDYDISJOINT';
         filter_masks(5) := 'EQUAL';

      END IF;


      psql := q'~SELECT a.~' || p_source_key || q'~, ~';

      --just gonna go with several cursor open rabbit holes, too complicated to manage binds in 10g

      --wrap the entire deal in an exception handler

      BEGIN

         IF p_boundary_definition <> 'TOPOGEOM'
         THEN

            --This should be boundaryedges in a benchmark.  But I suppose the column
            --could be called anything and still work
            --sample
            --select a.oid, t.topo_id edge_id from
            --tab10st10.county a,
            --TABLE(a.boundaryedges.get_topo_elements()) t
            --where a.vintage = '90'

            psql := psql || q'~t.topo_id edge_id, NULL ~'
                         || q'~FROM ~'
                         || p_source_schema || q'~.~' || p_source_table || q'~ a, ~'
                         || q'~TABLE(a.boundaryedges.get_topo_elements()) t ~';

            psql := psql || q'~WHERE ~' || p_where_clause || q'~ ~';

            IF p_sdo_filter IS NULL
            THEN

               --SOP
               OPEN my_cursor FOR psql;

            ELSIF p_sdo_filter IS NOT NULL
            AND p_rigorous_filter = 0
            THEN

               --standard development use filter.

               psql := psql || q'~AND SDO_RELATE(a.sdogeometry, :p5, :p6) = :p7 ~';

               OPEN my_cursor FOR psql USING p_sdo_filter,
                                             'mask=INSIDE+COVEREDBY',
                                             'TRUE';

            ELSIF p_sdo_filter IS NOT NULL
            AND p_rigorous_filter = 1
            THEN

               --unused exact filter
               --Use UNION ALL on each mask for slightly better performance

               FOR i IN 1 .. filter_masks.COUNT
               LOOP

                  IF i <> filter_masks.COUNT
                  THEN

                     psql_filtered := psql_filtered || psql
                                                    || q'~AND SDO_RELATE(a.sdogeometry, ~'
                                                    || q'~:p~' || TO_CHAR(bkount) || q'~, :p~' || TO_CHAR(bkount + 1) || q'~) = :p~' || TO_CHAR(bkount + 2) || q'~ ~'
                                                    || q'~ UNION ALL ~';

                     bkount := bkount + 3;

                  ELSE

                     psql_filtered := psql_filtered || psql
                                                    || q'~AND SDO_RELATE(a.sdogeometry, ~'
                                                    || q'~:p~' || TO_CHAR(bkount) || q'~, :p~' || TO_CHAR(bkount + 1) || q'~) = :p~' || TO_CHAR(bkount + 2) || q'~ ~';

                  END IF;

               END LOOP;

               --switch back for the log
               psql := psql_filtered;

               --masks correspond to all polys that have some L or R edges in the build universe
               --Will get further trimmed (if necessary) later

               OPEN my_cursor FOR psql USING p_sdo_filter, 'mask=' || filter_masks(1), 'TRUE',
                                             p_sdo_filter, 'mask=' || filter_masks(2), 'TRUE',
                                             p_sdo_filter, 'mask=' || filter_masks(3), 'TRUE',
                                             p_sdo_filter, 'mask=' || filter_masks(4), 'TRUE',
                                             p_sdo_filter, 'mask=' || filter_masks(5), 'TRUE';

            ELSE

               RAISE_APPLICATION_ERROR(-20001,'Oops, bad options');

            END IF;

         ELSIF p_boundary_definition = 'TOPOGEOM'
         THEN

             --sub code expects column to be called topogeom
             --sample
             --select a.oid, t.column_value edge_id from
             --tab10st10.county a,
             --TABLE(GZ_BUILD_SOURCE.GZ_GET_BDYEDGES('TAB10ST10','MT','COUNTY',a.oid,'OID')) t
             --where a.vintage = '90'

            psql := psql || q'~t.column_value edge_id, NULL ~'
                         || q'~FROM ~'
                         || p_source_schema || q'~.~' || p_source_table || q'~ a, ~'
                         || q'~TABLE(GZ_BUILD_SOURCE.GZ_GET_BDYEDGES(:p1,:p2,:p3,a.~' || p_source_key || q'~,:p4)) t ~';

            psql := psql || q'~WHERE ~' || p_where_clause || q'~ ~';

            IF p_sdo_filter IS NULL
            THEN

               OPEN my_cursor FOR psql USING p_source_schema,
                                             p_source_topology,
                                             p_source_table,
                                             p_source_key;

            ELSIF p_sdo_filter IS NOT NULL
            AND p_rigorous_filter = 0
            THEN

               psql := psql || q'~AND SDO_RELATE(a.sdogeometry, :p5, :p6) = :p7 ~';

               OPEN my_cursor FOR psql USING p_source_schema,
                                             p_source_topology,
                                             p_source_table,
                                             p_source_key,
                                             p_sdo_filter,
                                             'mask=INSIDE+COVEREDBY',
                                             'TRUE';

            ELSIF p_sdo_filter IS NOT NULL
            AND p_rigorous_filter = 1
            THEN

               --unused exact filter
               --Use UNION ALL on each mask for slightly better performance

               FOR i IN 1 .. filter_masks.COUNT
               LOOP

                  IF i <> filter_masks.COUNT
                  THEN

                     psql_filtered := psql_filtered || psql
                                                    || q'~AND SDO_RELATE(a.sdogeometry, ~'
                                                    || q'~:p~' || TO_CHAR(bkount) || q'~, :p~' || TO_CHAR(bkount + 1) || q'~) = :p~' || TO_CHAR(bkount + 2) || q'~ ~'
                                                    || q'~ UNION ALL ~';

                     bkount := bkount + 3;

                  ELSE

                     psql_filtered := psql_filtered || psql
                                                    || q'~AND SDO_RELATE(a.sdogeometry, ~'
                                                    || q'~:p~' || TO_CHAR(bkount) || q'~, :p~' || TO_CHAR(bkount + 1) || q'~) = :p~' || TO_CHAR(bkount + 2) || q'~ ~';

                  END IF;

               END LOOP;

               --switch back for the log
               psql := psql_filtered;

               --masks correspond to all polys that have some L or R edges in the build universe
               --Will get further trimmed (if necessary) later

               OPEN my_cursor FOR psql USING p_source_schema,
                                             p_source_topology,
                                             p_source_table,
                                             p_source_key,
                                             p_sdo_filter, 'mask=' || filter_masks(1), 'TRUE',
                                             p_sdo_filter, 'mask=' || filter_masks(2), 'TRUE',
                                             p_sdo_filter, 'mask=' || filter_masks(3), 'TRUE',
                                             p_sdo_filter, 'mask=' || filter_masks(4), 'TRUE',
                                             p_sdo_filter, 'mask=' || filter_masks(5), 'TRUE';

            ELSE

               RAISE_APPLICATION_ERROR(-20001,'oops, bad options');

            END IF;

         END IF;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'PIPE_BUILD_POLY',NULL,
                                                'Opened pipe_build_poly with sql--> ',NULL,NULL,NULL,psql,NULL,NULL);

      EXCEPTION
      WHEN OTHERS
      THEN

         stack := DBMS_UTILITY.format_error_backtrace;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'PIPE_BUILD_POLY',NULL,
                                                'ERROR--> see SQL and ERRM ',NULL,NULL,NULL,psql,NULL,SQLERRM);

         RAISE_APPLICATION_ERROR(-20001,stack);


      END;


      LOOP

         FETCH my_cursor BULK COLLECT INTO build_tab LIMIT 1000;
         EXIT WHEN build_tab.COUNT = 0;

         FOR i IN 1 .. build_tab.COUNT
         LOOP

            build_tab(i).layer := p_layer;

            PIPE ROW(build_tab(i));

         END LOOP;

      END LOOP;

   END PIPE_BUILD_POLY;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION CREATE_BUILD_POLY (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_sdo_filter         IN SDO_GEOMETRY DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! 2/7/12
      --2/04/13 Added exact filter option, hard coded to be turned off

      layers            GZ_TYPES.GZ_LAYERS_IN_INFO;
      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;

      --bad form!  change to 1 to get slower, exact filter
      rigorous_filter   PLS_INTEGER := 0;

   BEGIN

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_BUILD_POLY: Starting ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                             'Starting ' || p_output_topology || '_build_poly ');

      --get input layers
      layers := GZ_BUILD_SOURCE.GET_LAYER_INFO(p_output_topology);

      --build poly table got created in SET_UP
      --named like Z610IN_build_poly

      FOR i IN 1 .. layers.COUNT
      LOOP

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                                'Starting ' || layers(i).layer || ' insert into ' || p_output_topology || '_build_poly ');

         psql := 'INSERT INTO ' || p_output_topology || '_build_poly '
              || 'SELECT * FROM TABLE(GZ_BUILD_SOURCE.PIPE_BUILD_POLY(:p1,:p2,:p3,:p4,:p5,:p6,:p7,:p8,:p9,:p10)) ';

         BEGIN

            EXECUTE IMMEDIATE psql USING layers(i).layer,
                                         layers(i).source_schema,
                                         layers(i).source_table,
                                         p_source_topology,
                                         p_output_topology,
                                         layers(i).source_key,
                                         layers(i).where_clause,
                                         layers(i).boundary_definition,
                                         layers(i).sdo_filter,   --idea that each layer could have unique filter is bogus for now
                                         rigorous_filter;

         EXCEPTION
         WHEN OTHERS
         THEN

            IF UPPER(SQLERRM) LIKE '%UNIQUE CONSTRAINT%'
            THEN

               --log us a clue
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                                      'INSERT SQL to reproduce error -->', NULL,NULL,NULL,
                                                       psql || ' ' || layers(i).layer || ',' || layers(i).source_schema || ','
                                                       || layers(i).source_table || ',' || p_source_topology || ','
                                                       || p_output_topology || ',' || layers(i).source_key || ','
                                                       || layers(i).where_clause || ',' || layers(i).boundary_definition);

               RAISE;

            ELSE

               RAISE;

            END IF;

         END;

         COMMIT;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                                'Completed ' || layers(i).layer || ' insert into ' || p_output_topology || '_build_poly ');

      END LOOP;

      --checks?
      --lets make sure theres something in there

      psql := 'SELECT COUNT(*) '
           || 'FROM '
           || p_output_topology || '_build_poly '
           || 'WHERE rownum = 1';

      EXECUTE IMMEDIATE psql INTO kount;

      IF kount > 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                                'Complete ' || p_output_topology || '_build_poly ');

      ELSE

         RETURN 'Didnt get any records in ' || p_output_topology || '_build_poly ';

      END IF;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                             'Gathering stats on ' || p_output_topology || '_build_poly',
                                             NULL,NULL,NULL,NULL);

      GZ_BUSINESS_UTILS.GATHER_TABLE_STATS(p_output_topology || '_BUILD_POLY');

      --create the gz_build_geom table
      --probably deserves its own module, or at least a sub
      --I have concerns about doing this in one SQL
      --the 0 is "processed" -> Default 0 is no

      psql := 'INSERT /*+ APPEND */ INTO '
           || p_output_topology || '_build_geom '
           || 'SELECT e.edge_id, e.geometry, 0 '
           || 'FROM '
           || p_source_schema || '.' || p_source_topology || '_edge$ e '
           || 'WHERE e.edge_id IN ('
           || 'SELECT DISTINCT edge_id FROM ' || p_output_topology || '_build_poly) ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                             'Inserting data into ' || p_output_topology || '_build_geom',
                                             NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                                  'Committing insert into ' || p_output_topology || '_build_geom',
                                                  NULL,NULL,NULL,NULL);

      COMMIT;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                                  'Building spatial index on ' || p_output_topology || '_build_geom',
                                                  NULL,NULL,NULL,NULL);

      --no index on "processed" access will be entirely by primary key or spatial.  Processed is secondary
      GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(p_output_topology || '_BUILD_GEOM',
                                     'GEOMETRY',
                                      8265,
                                      .05);

      IF p_sdo_filter IS NOT NULL
      AND rigorous_filter= 1
      THEN

         --This code not really used
         --development mode, running with a filter and I want a precise filter
         --this isnt parameterized, just hard coded in the declaration above

         --in order to get everything exactly correct inside the filter the build_poly code inserts
         --   polygon records and all associated edges for polygons that are partially inside the filter
         --Now further prune out edges to just those edges that are truly in play

         --Im aware that it makes far more sense to prune in the initial SQL above against edge$
         --But the performance of the relate with detailed masks against mt_edge$ is absurd
         --As a general rule I learnding to makes relates against the smallest set possible, like in a local work table

         psql := 'DELETE FROM ' || p_output_topology || '_build_geom aa '
              || 'WHERE aa.rowid NOT IN ('
              || 'SELECT a.rowid FROM ' || p_output_topology || '_build_geom a '
              || 'WHERE SDO_RELATE(a.geometry, :p1, :p2 ) = :p3 '
              || 'UNION ALL '
              || 'SELECT a.rowid FROM ' || p_output_topology || '_build_geom a '
              || 'WHERE SDO_RELATE(a.geometry, :p4, :p5 ) = :p6 '
              || ')';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                                'Deleting filtered out edge geometries from ' || p_output_topology || '_build_geom',
                                                NULL,NULL,NULL,psql,NULL,NULL,p_sdo_filter);

         EXECUTE IMMEDIATE psql USING p_sdo_filter,
                                      'mask=INSIDE+COVEREDBY',  --optimized to be lovers
                                      'TRUE',
                                      p_sdo_filter,
                                      'mask=ON',                --hater
                                      'TRUE';
         COMMIT;

         --Now delete any outside-the-filter records from the build_poly table
         --It definitely makes logical sense to do this differently, like with two sdo_relates in the initial
         --insert into the build_poly table.  Not performative though

         psql := 'DELETE FROM ' || p_output_topology || '_build_poly '
              || 'WHERE edge_id NOT IN '
              || '(SELECT edge_id from ' || p_output_topology || '_build_geom) ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                                'Deleting any outside the filter edges from ' || p_output_topology || '_build_poly',
                                                NULL,NULL,NULL,NULL);

         EXECUTE IMMEDIATE psql;

         COMMIT;

      END IF;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                             'Completed populating ' || p_output_topology || '_build_geom',
                                             NULL,NULL,NULL,NULL);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                             'Gathering stats on ' || p_output_topology || '_build_geom',
                                             NULL,NULL,NULL,NULL);

      GZ_BUSINESS_UTILS.GATHER_TABLE_STATS(p_output_topology || '_BUILD_GEOM');

      --lets make sure theres something in there

      psql := 'SELECT COUNT(*) '
           || 'FROM '
           || p_output_topology || '_build_geom '
           || 'WHERE rownum = 1';

      EXECUTE IMMEDIATE psql INTO kount;


      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_BUILD_POLY: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      IF kount > 0
      THEN

         RETURN '0';

      ELSE

         RETURN 'Didnt get any records in ' || p_output_topology || '_build_geom ';

      END IF;


   END CREATE_BUILD_POLY;


    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   PROCEDURE BUILD_CREATE_TABLE (
      p_table_name     IN VARCHAR2,
      p_sql            IN VARCHAR2,
      p_pkc_col        IN VARCHAR2 DEFAULT NULL,
      p_uqc_col        IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 2/9/12
      --Similar to code edge att table builder in clip
      --For tables with dynamic cols this is as locked down as I know how to be

      psql             VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Create the table using passed sql');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      psql := p_sql;

      BEGIN

         EXECUTE IMMEDIATE psql;


      EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -60
            OR SQLCODE = -18014
            THEN

               --ORA-00060: deadlock detected while waiting for resource
               --Usually bogus, just heavy production
               --can't use dbms_lock, it's not available on all databases (yet)
               --DBMS_LOCK.sleep(5);
               --just try again like fools
               EXECUTE IMMEDIATE psql;

            ELSIF SQLCODE = -955
            THEN

               --table already exists
               EXECUTE IMMEDIATE 'DROP TABLE ' || UPPER(p_table_name) || ' PURGE';
               EXECUTE IMMEDIATE psql;

            ELSE

               RAISE;

            END IF;

      END;

     ----------------------------------------------------------------------------------
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
     DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Add constraints and keys to ' || p_table_name);
     --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
     ----------------------------------------------------------------------------------

      IF p_pkc_col IS NOT NULL
      AND p_uqc_col IS NULL
      THEN
         --Does EDGE_ID need to be parameterized?  Think this is ok
         EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
                        || 'ADD ('
                        || '   CONSTRAINT ' || p_table_name || 'PKC '
                        || '      PRIMARY KEY(' || p_pkc_col || ') '
                        || ')';



      END IF;

      IF p_uqc_col IS NOT NULL
      AND p_pkc_col IS NULL
      THEN

         EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
                        || 'ADD ('
                        || '   CONSTRAINT ' || p_table_name || 'UQC '
                        || '      UNIQUE(' || p_uqc_col || ') '
                        || ')';


      END IF;

      IF p_uqc_col IS NOT NULL
      AND p_pkc_col IS NOT NULL
      THEN

         EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
                        || 'ADD ('
                        || '   CONSTRAINT ' || p_table_name || 'PKC '
                        || '      PRIMARY KEY(' || p_pkc_col || '), '
                        || '   CONSTRAINT ' || p_table_name || 'UQC '
                        || '      UNIQUE(' || p_uqc_col || ') '
                        || ')';


      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('Grant privileges on ' || p_table_name);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);


   END BUILD_CREATE_TABLE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE POPULATE_BUILD_TILE (
      p_topology           IN VARCHAR2,
      p_tiles              IN GZ_TYPES.GEOMARRAY,
      p_status_clue        IN VARCHAR2 DEFAULT '0'
   )
   AS

     --Matt! 3/14/12

     iis                GZ_TYPES.stringarray;

   BEGIN

      --prep the iis first
      FOR i IN 1 .. p_tiles.COUNT
      LOOP

         --beautiful dude
         iis(i) := i;

      END LOOP;

      --even more so...iis think

      FORALL ii IN 1 .. iis.COUNT
         EXECUTE IMMEDIATE 'INSERT INTO ' || p_topology || '_build_tile '
                        || 'VALUES(:p1,:p2,:p3) ' USING iis(ii),
                                                         p_tiles(ii),
                                                         p_status_clue;

      COMMIT;

   END POPULATE_BUILD_TILE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_BUILD_TILE (
      p_topology           IN VARCHAR2,
      p_status             IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.GEOMARRAY
   AS

      --Matt! 3/14/12

      psql              VARCHAR2(4000);
      output            GZ_TYPES.geomarray;

   BEGIN

      psql := 'SELECT a.sdogeometry '
           || 'FROM '
           || p_topology || '_build_tile a ';

      IF p_status IS NOT NULL
      THEN

         psql := psql || 'WHERE '
                      || 'a.status_clue = :p1 '
                      || 'ORDER BY a.tile_number ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING p_status;

      ELSE

         psql := psql || 'ORDER BY a.tile_number ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO output;

      END IF;

      RETURN output;

   END GET_BUILD_TILE;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION IS_TILE_PROCESSED (
      p_topology           IN VARCHAR2,
      p_tile_number        IN VARCHAR2,
      p_status_clue        IN VARCHAR2
   ) RETURN BOOLEAN
   AS

      --Matt!  3/14/12

      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_topology || '_build_tile a '
           || 'WHERE '
           || 'a.status_clue = :p1 AND '
           || 'a.tile_number = :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_status_clue,
                                              p_tile_number;

      IF kount = 1
      THEN

         RETURN TRUE;

      ELSIF kount = 0
      THEN

         RETURN FALSE;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'oops');

      END IF;

   END IS_TILE_PROCESSED;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE MARK_TILE_STATUS (
      p_topology           IN VARCHAR2,
      p_tile               IN NUMBER,
      p_status_clue        IN VARCHAR2
   )
   AS

      --Matt!  3/14/12
      psql           VARCHAR2(4000);

   BEGIN

      psql := 'UPDATE ' || p_topology || '_build_tile a '
           || 'SET '
           || 'a.status_clue = :p1 '
           || 'WHERE '
           || 'a.tile_number = :p2 ';

      EXECUTE IMMEDIATE psql USING p_status_clue,
                                   p_tile;

      COMMIT;

   END MARK_TILE_STATUS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------


   FUNCTION CREATE_EMPTY_TOPOLOGY (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_srid               IN NUMBER DEFAULT 8265,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_snapping_digits    IN NUMBER DEFAULT 16
   ) RETURN VARCHAR2
   AS

      --Matt! 2/13/12

      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;
      output         VARCHAR2(4000) := '0';


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EMPTY_TOPOLOGY: Drop topo ' || p_output_topology || ' if it exists');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_EMPTY_TOPOLOGY',NULL,
                                             'Starting ' || p_output_topology);

      --Purger will error if topo doesnt exist

      psql := 'SELECT COUNT(*) '
           || 'FROM user_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'rownum = 1 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_output_topology;

      IF kount = 1
      THEN

         -- burn it all
         GZ_TOPO_UTIL.PURGE_TOPOLOGY(p_schema,p_output_topology);


      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EMPTY_TOPOLOGY: Create topology ' || p_output_topology);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      SDO_TOPO.create_topology(p_output_topology,
                               p_tolerance,
                               p_srid,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               p_snapping_digits);

      --have to get sidx on face$ in order to use limited topomap shortly
      SDO_TOPO.INITIALIZE_METADATA(p_output_topology);


      --insert universal face
      psql := 'INSERT INTO ' || p_output_topology || '_FACE$ '
           || 'VALUES (:p1, NULL, :p2, :p3, NULL)';

      EXECUTE IMMEDIATE psql USING -1,
                                   sdo_list_type(),
                                   sdo_list_type();


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EMPTY_TOPOLOGY: Grant selects on ' || p_output_topology || '$s');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --relation$ doesnt exist at this point

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_output_topology || '_EDGE$');

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_output_topology || '_FACE$');

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_output_topology || '_NODE$');


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_EMPTY_TOPOLOGY: Peace out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_EMPTY_TOPOLOGY',NULL,
                                             'Completed ' || p_output_topology);

      RETURN output;

   END CREATE_EMPTY_TOPOLOGY;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   PROCEDURE ROLL_BACK_A_TILE (
      p_output_topology       IN VARCHAR2,
      p_tile_number           IN NUMBER,
      p_restart               IN VARCHAR2 DEFAULT 'N'
   )
   AS

      --Matt!
      --just code reuse, rather than cluttering up load_topo_tile below

      psql           VARCHAR2(4000);

   BEGIN

      psql :=  'DELETE FROM '
            || p_output_topology || '_build_edge a '
            || 'WHERE '
            || 'a.tile_number = :p1 ';

      IF p_restart = 'Y'
      THEN

         --on restarts we sometimes get situations where, on the initial run,
         --the edge_ids got periodically committed to build_edge
         --but the database plug is pulled before we can commit the active topomap
         --theres probably no roll back a tile in that case
         --And its actually safe to delete ALL of the edges in build_Edge with this tile number
         --But just in case, only do the ones where we are sure they never made it into the topo

         psql := psql || 'AND a.edge_id NOT IN  '
                      || '(SELECT e.edge_id FROM ' || p_output_topology || '_edge$ e) ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_number,NULL,
                                                'Cleaning on restart all ' || p_output_topology ||
                                                '_build_edge recs for tile ' || p_tile_number,NULL,NULL,NULL,psql);

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_number,NULL,
                                                'Error, deleting all ' || p_output_topology ||
                                                 '_build_edge recs for tile ' || p_tile_number,NULL,NULL,NULL,psql);

      END IF;

      EXECUTE IMMEDIATE psql USING p_tile_number;
      COMMIT;  --THIS MUST REMAIN

   END ROLL_BACK_A_TILE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION REMOVE_BADDY_EDGE (
      p_output_topology    IN VARCHAR2,
      p_errm               IN VARCHAR2
   ) RETURN NUMBER
   AS

      --Matt! 6/11/12
      --Sometimes we get random weird errors loading a tile
      --Indicates an oracle bug.  Something went wrong but undetected when adding edges
      --Here we will try to parse the error and find a baddy edge
      --Removing the edge and re-adding it will likely fix the problem

      --return the tile to rework

      edge_id           NUMBER;
      start_pos         NUMBER;
      end_pos           NUMBER;
      psql              VARCHAR2(4000);
      source_edge_id    NUMBER;
      tile_number       NUMBER;

   BEGIN

      IF p_errm LIKE '%is not on the boundary of one or two of the faces it links%'
      THEN

         --ORA-29532: Java call terminated by uncaught Java exception: oracle.spatial.topo.TopoValidationException:
         --Edge 3150111 is not on the boundary of one or two of the faces it links

         --spelling it out for easy rethink
         start_pos := INSTR(p_errm,'Edge');
         end_pos   := INSTR(p_errm,'is not');

         edge_id := TO_NUMBER(SUBSTR(p_errm,(start_pos + 5), (end_pos - (start_pos + 5 + 1))));

         --no topomap, you figure it out genious
         SDO_TOPO_MAP.REMOVE_EDGE(p_output_topology, edge_id);

         psql := 'SELECT source_edge_id, tile_number '
              || 'FROM ' || p_output_topology || '_build_edge '
              || 'WHERE edge_id = :p1 ';

         EXECUTE IMMEDIATE psql INTO source_edge_id,
                                     tile_number USING edge_id;

         psql := 'DELETE FROM ' || p_output_topology || '_build_edge '
              || 'WHERE '
              || 'edge_id = :p1 AND '
              || 'source_edge_id = :p2 AND '
              || 'tile_number = :p3 ';

          EXECUTE IMMEDIATE psql USING edge_id,
                                       source_edge_id,
                                       tile_number;

          COMMIT;

         --Return the tile number
         --Rerunning this tile should catch just this missing edge
         RETURN tile_number;


      ELSE

         --Just add more
         RAISE_APPLICATION_ERROR(-20001, 'Dont know how to handle error ' || p_errm);

      END IF;



   END REMOVE_BADDY_EDGE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE LOAD_TOPO_TILE (
      p_input_schema       IN VARCHAR2,
      p_input_topology     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tile               IN SDO_GEOMETRY,
      p_tile_kount         IN NUMBER,
      p_tile_total         IN NUMBER,
      p_srid               IN NUMBER DEFAULT 8265,
      p_src_srid           IN NUMBER DEFAULT 8265,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_delta              IN NUMBER DEFAULT .05,
      p_mask_override      IN VARCHAR2 DEFAULT NULL
   )
   AS

       --Matt! 2/14/12
       --8/22/12  Additional wack-a-code for dateline crossing
       --6/19/13  reworked to not add the <topo>_build_edge records until after the topomap is committed
       --11/26/13 Moved obsolete node removal into this step

       psql                   VARCHAR2(4000);
       newtopomap             VARCHAR2(4000) := p_output_topology || '_TOPOMAP';
       my_cursor              SYS_REFCURSOR;
       ez_topo_mgr            NUMBER;
       stupid_number_array    SDO_NUMBER_ARRAY;
       p_window               SDO_GEOMETRY;
       p_window_eastern       SDO_GEOMETRY;
       TYPE toporec           IS RECORD (
                                         id NUMBER,
                                         sdogeometry SDO_GEOMETRY);
       TYPE topot             IS TABLE OF toporec;
       topotab                topot;
       insert_id              PLS_INTEGER := 0;
       feature_ids            GZ_TYPES.numberarray;
       edge_ids               GZ_TYPES.numberarray;
       src_srid               NUMBER;
       topo_window            SDO_GEOMETRY;
       topo_window_eastern    SDO_GEOMETRY;
       opened                 PLS_INTEGER := 0;
       zzztracker             PLS_INTEGER := 0;
       mbr_special_edges      GZ_TYPES.stringarray;
       window_kount           PLS_INTEGER := 1;
       mask                   VARCHAR2(256);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('LOAD_TOPO_TILE: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --what all to check

      IF p_tile.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Tile should be optimized, ordinate count is ' || p_tile.sdo_ordinates.COUNT);

      ELSE

         --to be safe, expand our tile just like in topomap manager
         --Optimized rectangles with geodetic are always dangerous - need to test this
         --Yeah, this can sometimes cause dateline crossing

         p_window := p_tile;

         p_window.sdo_ordinates(1) := p_window.sdo_ordinates(1) - p_delta;
         p_window.sdo_ordinates(2) := p_window.sdo_ordinates(2) - p_delta;
         p_window.sdo_ordinates(3) := p_window.sdo_ordinates(3) + p_delta;
         p_window.sdo_ordinates(4) := p_window.sdo_ordinates(4) + p_delta;

      END IF;


      --figure out topomap
      --REMEMBER: edges can slop over the tile boundaries, and the topology cares about this
      --this is a repeat of below, maybe consolidate later

      --just get the edges slopping

      /*
      psql := 'SELECT SDO_AGGR_MBR(ee.geometry) '
           || 'FROM ( '
           || 'SELECT DISTINCT a.edge_id '
           || 'FROM '
           || p_output_topology || '_build_poly a, '
           || p_input_schema || '.' || p_input_topology || '_edge$ e '
           || 'WHERE '
           || 'a.edge_id = e.edge_id AND '
           || 'SDO_RELATE(e.geometry, :p1,:p2) = :p3 AND '
           || 'NOT EXISTS (SELECT source_edge_id FROM ' || p_output_topology || '_build_edge b where b.source_edge_id = e.edge_id) '
           || ') aa, '
           || p_input_schema || '.' || p_input_topology || '_edge$ ee '
           || 'WHERE aa.edge_id = ee.edge_id ';
      */

      psql := 'SELECT SDO_AGGR_MBR(a.geometry) '
           || 'FROM '
           || p_output_topology || '_build_geom a '
           || 'WHERE '
           || 'SDO_RELATE(a.geometry, :p1,:p2) = :p3 AND '
           || 'a.processed = :p4 ';

           --old pre-obsolete node overhaul method
           --AND '
           --|| 'NOT EXISTS '
           --|| '(SELECT source_edge_id FROM ' || p_output_topology || '_build_edge b where b.source_edge_id = a.edge_id) ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                             'Get MBR of edges slopping over tile',
                                             NULL,NULL,NULL,psql,NULL,NULL,p_window);

      IF p_mask_override IS NULL
      THEN

         mask := 'mask=OVERLAPBDYDISJOINT';

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                     'Using special mask override: ' || p_mask_override);

         --special override mask to deal with flakey sdo_relate
         --probably 'mask=OVERLAPBDYDISJOINT+OVERLAPBDYINTERSECT'
         mask := p_mask_override;

      END IF;

      EXECUTE IMMEDIATE psql INTO topo_window USING p_window,
                                                    mask,
                                                    'TRUE',
                                                    0;


      IF topo_window IS NULL
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                     'No slopping, either a rerun or an island in a tile',
                                                     NULL,NULL,NULL,psql,NULL,NULL,topo_window);

         --this window is clean, its from the tiler
         topo_window := p_window;
         window_kount := 1;

      ELSE


         IF SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(topo_window, p_tolerance) != 'TRUE'
         THEN

            --sometimes sdo_aggr_mbr returns an invalid geometry.
            --saw this on a vertical edge MBR in AK
            --when calling union below the invalid bit can get ignored in the resulting window creation
            --   Thats just how sdo_union rolls
            --If our topo window is invalid, throw it at rectify.  Shouldnt hurt I hope, we just want a rough but full extent

            topo_window := SDO_UTIL.RECTIFY_GEOMETRY(topo_window, p_tolerance);

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                   'Topo window from sdo_aggr_mbr was invalid. Maybe rectified--> ',
                                                    NULL,NULL,NULL,NULL,NULL,NULL,topo_window);

            --For Gods sake dont check again!  I dont want to know

         END IF;


         --special dateline handling wack-a-code first

         IF p_tile_total > 1
         AND topo_window.sdo_srid = 8265
         AND topo_window.sdo_ordinates(1) < -140   --roughly eastern AK to...
         AND topo_window.sdo_ordinates(3) > 0      --somewhere past London
         THEN

            --Oracle likes to wrap the world with MBRs near the dateline
            --But dont do this if we just have one tile total, it could be the entire US
            --partial SQL from above

            /*
            psql := 'SELECT DISTINCT a.edge_id '
                 || 'FROM '
                 || p_output_topology || '_build_poly a, '
                 || p_input_schema || '.' || p_input_topology || '_edge$ e '
                 || 'WHERE '
                 || 'a.edge_id = e.edge_id AND '
                 || 'SDO_RELATE(e.geometry, :p1,:p2) = :p3 AND '
                 || 'NOT EXISTS (SELECT source_edge_id FROM ' || p_output_topology || '_build_edge b where b.source_edge_id = e.edge_id) ';
            */
           psql := 'SELECT a.edge_id '
                || 'FROM '
                || p_output_topology || '_build_geom a '
                || 'WHERE '
                || 'SDO_RELATE(a.geometry, :p1,:p2) = :p3 AND '
                || 'a.processed = :p4 ';


                -- AND '
                --|| 'NOT EXISTS '
                --|| '(SELECT source_edge_id FROM ' || p_output_topology || '_build_edge b where b.source_edge_id = a.edge_id) ';


            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                   'Calling bespoke sdo aggr mbr utility near dateline',
                                                   NULL,NULL,NULL,psql,NULL,NULL,p_window);

            EXECUTE IMMEDIATE psql BULK COLLECT INTO mbr_special_edges USING p_window,
                                                                             'mask=OVERLAPBDYDISJOINT',
                                                                             'TRUE',
                                                                             0;

            topo_window := GZ_BUILD_SOURCE.AGGR_MBR_NEAR_DATELINE(p_input_schema || '.' || p_input_topology || '_edge$',
                                                                  'EDGE_ID',
                                                                  mbr_special_edges,
                                                                  'GEOMETRY',
                                                                  p_tolerance);

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                   'Bespoke sdo aggr mbr is',NULL,NULL,NULL,NULL,NULL,NULL,topo_window);

            --special dateline MBR handling
            --will internally union and get tidy all negative MBR
            topo_window := GZ_BUILD_SOURCE.GET_MBR_NEAR_DATELINE(topo_window,
                                                                 p_window,
                                                                 p_tolerance);

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                   'Bespoke topo window is',NULL,NULL,NULL,NULL,NULL,NULL,topo_window);

            --now all window mitosis to handle each side of the dateline

            --put positive side in special commie-friendly window
            topo_window_eastern := GZ_BUILD_SOURCE.SPLIT_AT_DATELINE(topo_window, 'EASTERN');
            --replace regular window with chopped version
            topo_window := GZ_BUILD_SOURCE.SPLIT_AT_DATELINE(topo_window, 'WESTERN');
            --split the edge collection window too
            p_window_eastern := GZ_BUILD_SOURCE.SPLIT_AT_DATELINE(p_window, 'EASTERN');
            p_window := GZ_BUILD_SOURCE.SPLIT_AT_DATELINE(p_window, 'WESTERN');

            window_kount := 2;


         ELSE

            --SOP
            --then add the original window in case no slopping on one or more sides
            topo_window := SDO_GEOM.SDO_MBR(SDO_GEOM.SDO_UNION(topo_window, p_window, p_tolerance));

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                        'Window after union',NULL,NULL,NULL,NULL,NULL,NULL,topo_window);

            IF (topo_window.sdo_ordinates(1) > 0 AND topo_window.sdo_ordinates(1) < 180)
            AND topo_window.sdo_ordinates(3) < 0
            THEN

               --RARE dateline handling mess #22.5
               -- ex 179.232779622 to  -179.997101,
               --We typically rely on the aggr_mbr section above to trigger dateline crossing
               --But sometimes the slopping edges are all on one side of the dateline and being all cool
               --and its the base window itself that crosses and is totally uncool
               --We discover this after the MBR on the UNION just above

               --make all negative since my code expects it
               topo_window := gz_build_source.shift_at_dateline(topo_window);

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                      'Splitting into two windows at dateline, heres topo window post shift',
                                                      NULL,NULL,NULL,NULL,NULL,NULL,topo_window);

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                      'Splitting into two windows at dateline, the starter pwindow again',
                                                      NULL,NULL,NULL,NULL,NULL,NULL,p_window);

               --replace regular window with chopped version
               topo_window_eastern := GZ_BUILD_SOURCE.SPLIT_AT_DATELINE(topo_window, 'EASTERN');
               topo_window := GZ_BUILD_SOURCE.SPLIT_AT_DATELINE(topo_window, 'WESTERN');

               --split the edge collection window too. Assuming that this guy crosses, if not error here, something else going down
               p_window_eastern := GZ_BUILD_SOURCE.SPLIT_AT_DATELINE(p_window, 'EASTERN');
               p_window := GZ_BUILD_SOURCE.SPLIT_AT_DATELINE(p_window, 'WESTERN');

               window_kount := 2;

            ELSE

               --SOP
               window_kount := 1;

            END IF;

         END IF;

      END IF;


      FOR jj IN 1 .. window_kount
      LOOP

         --99.9% of the time just one window, one loop

         /*
         psql := 'SELECT aa.edge_id, ';

         IF p_src_srid = p_srid
         THEN

             psql := psql || 'ee.geometry ';

         ELSE

            --transform.  Should try this some fine day

            psql := psql || 'SDO_CS.TRANSFORM(ee.geometry,' || p_srid || ') ';

         END IF;

         psql := psql || 'FROM ( '
              || 'SELECT DISTINCT a.edge_id '
              || 'FROM '
              || p_output_topology || '_build_poly a, '
              || p_input_schema || '.' || p_input_topology || '_edge$ e '
              || 'WHERE '
              || 'a.edge_id = e.edge_id AND '
              || 'SDO_RELATE(e.geometry, :p1,:p2) = :p3 AND '
              || 'NOT EXISTS (SELECT source_edge_id FROM ' || p_output_topology || '_build_edge b where b.source_edge_id = e.edge_id) '
              || ') aa, '
              || p_input_schema || '.' || p_input_topology || '_edge$ ee '
              || 'WHERE aa.edge_id = ee.edge_id ';

         */

         psql := 'SELECT a.edge_id, ';

         IF p_src_srid = p_srid
         THEN

             psql := psql || 'a.geometry ';

         ELSE

            --transform.  Should try this some fine day

            psql := psql || 'SDO_CS.TRANSFORM(a.geometry,' || p_srid || ') ';

         END IF;

         psql := psql || 'FROM '
              || p_output_topology || '_build_geom a '
              || 'WHERE '
              || 'SDO_RELATE(a.geometry, :p1,:p2) = :p3 AND '
              || 'a.processed = :p4 ';

              --pre obsolete node overhaul strategy
              -- AND '
              --|| 'NOT EXISTS ('
              --|| 'SELECT source_edge_id FROM ' || p_output_topology || '_build_edge b where b.source_edge_id = a.edge_id) ';

         IF jj = 1
         THEN

            --SOP
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                        'Opening cursor with SQL and TILE window-->',NULL,NULL,NULL,psql,NULL,NULL,p_window);

            --original window
            OPEN my_cursor FOR psql USING p_window,
                                          'mask=INSIDE+COVEREDBY+OVERLAPBDYDISJOINT+ON',
                                          'TRUE',
                                          0;

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                        'Opening cursor with SQL and EASTERN TILE window-->',NULL,NULL,NULL,psql,NULL,NULL,p_window_eastern);

            --original window
            OPEN my_cursor FOR psql USING p_window_eastern,
                                          'mask=INSIDE+COVEREDBY+OVERLAPBDYDISJOINT+ON',
                                          'TRUE',
                                          0;

         END IF;

         LOOP

            FETCH my_cursor BULK COLLECT INTO topotab LIMIT 25;
            EXIT WHEN topotab.COUNT = 0;

            FOR i IN 1 .. topotab.COUNT
            LOOP


               IF opened = 0
               THEN

                  IF jj = 1
                  THEN

                     --tuck this in here
                     --in case of reruns with no edges to process, dont want to open a pointless topomap

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                                 'Opening topomap with expanded TOPO window-->',
                                                                 NULL,NULL,NULL,psql,NULL,NULL,topo_window);

                     --potentially expanded topo window
                     ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(newtopomap,
                                                                    p_output_topology,
                                                                    2,
                                                                    topo_window.sdo_ordinates(1),
                                                                    topo_window.sdo_ordinates(2),
                                                                    topo_window.sdo_ordinates(3),
                                                                    topo_window.sdo_ordinates(4)
                                                                    );

                  ELSE

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                                 'Opening topomap with expanded EASTERN TOPO window-->'
                                                                 ,NULL,NULL,NULL,psql,NULL,NULL,topo_window_eastern);

                     --potentially expanded topo window
                     ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(newtopomap,
                                                                    p_output_topology,
                                                                    2,
                                                                    topo_window_eastern.sdo_ordinates(1),
                                                                    topo_window_eastern.sdo_ordinates(2),
                                                                    topo_window_eastern.sdo_ordinates(3),
                                                                    topo_window_eastern.sdo_ordinates(4)
                                                                    );


                  END IF;

                  opened := 1;

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                              'Topomap opened. Starting calls to add_linear_geometry Zzz...');

               END IF;


               BEGIN

                  --THE CALL----
                  --**********--
                  stupid_number_array := SDO_TOPO_MAP.ADD_LINEAR_GEOMETRY(NULL,topotab(i).sdogeometry);
                  --------------
                  --------------

               EXCEPTION
               WHEN OTHERS
               THEN

                  --none of the edges that we added to the build_edge table will actually be added to the topology
                  --since the topomap cant be committed.  We consider the topomap to be hosed for this tile
                  --remove them from the build_edge table for easier debugging and restart
                  --NO! shouldnt be added in the first place unless the topomap and its commit below succeed
                  --GZ_BUILD_SOURCE.ROLL_BACK_A_TILE(p_output_topology,
                                                   --p_tile_kount);

                  IF (UPPER(SQLERRM) LIKE '%OUTOFMEMORYERROR%'
                  OR UPPER(SQLERRM) LIKE '%JAVA OUT OF MEMORY CONDITION%')  --WTF causes this instead of the first?
                  THEN

                     --handle?

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                            'ADD_LINEAR_GEOMETRY error on ' || topotab(i).id,
                                                             NULL,NULL,NULL,NULL,NULL,SQLERRM);


                     CLOSE my_cursor;
                     COMMIT;

                     RAISE_APPLICATION_ERROR(-20001,'MEMORY');

                  ELSE

                     --this is where '...Attempted to add linear geometry outside the update window..' throws
                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                                 'ADD_LINEAR_GEOMETRY error on ' || topotab(i).id ,
                                                                 NULL,NULL,NULL,NULL,NULL,SQLERRM);


                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                                 'ADD_LINEAR_GEOMETRY error on ' || topotab(i).id ,
                                                                 NULL,NULL,NULL,NULL,NULL,DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

                     CLOSE my_cursor;
                     COMMIT;

                     RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);

                  END IF;

               END;

               IF stupid_number_array.COUNT = 1
               THEN

                  --keep separate counter in here to be safe
                  insert_id := insert_id + 1;

                  edge_ids(insert_id) := stupid_number_array(1);
                  stupid_number_array.DELETE;
                  feature_ids(insert_id) := topotab(i).id;

               ELSE

                  --none of the edges that we added to the build_edge table will actually be added to the topology
                  --since the topomap wont be committed after we raise the error below.
                  --We consider the topomap to be hosed for this tile
                  --remove edges from the build_edge table for easier debugging and restart

                  --I think this only applies on a double loop dateline tile??  Who wrote this
                  --Dangerous now
                  --GZ_BUILD_SOURCE.ROLL_BACK_A_TILE(p_output_topology,
                                                   --p_tile_kount);

                  CLOSE my_cursor;
                  COMMIT;

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                         'Error - got ' || stupid_number_array.COUNT || ' primitives for edge_id ' || topotab(i).id);

                  RAISE_APPLICATION_ERROR(-20001,'Error - got ' || stupid_number_array.COUNT || ' primitives for edge_id ' || topotab(i).id);


               END IF;

            END LOOP; --end loop over one bucket of edges

            --log us a bread crumb trail on really long waits here
            IF zzztracker < 1000
            THEN

               --as in zzz.....
               zzztracker := zzztracker + 1;

            ELSE

               zzztracker := 0;
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                           'Added (another) 1000 edges');

            END IF;

            --Magic commit, moved to within each error handler
            --Avoids "fetch out of sequence error" on catch and retries, never really got to the bottom of the cause
            --Overkill version on all loops would be here
            --COMMIT;
            --------------

         END LOOP;  --end loop over entire cursor of edges for this tile

         CLOSE my_cursor;

         IF opened = 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                        'Didnt add anything on tile ' || p_tile_kount || '. Skipping obsolete node removal');

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                        'Completed add_linear_geometry, removing obsolete nodes');

            --------------------------------
            --Obsolete node removal
            --------------------------------

            BEGIN

               SDO_TOPO_MAP.REMOVE_OBSOLETE_NODES(NULL);

            EXCEPTION
            WHEN OTHERS
            THEN
            
              IF UPPER(SQLERRM) LIKE '%JAVA.LANG.OUTOFMEMORYERROR%'
              THEN
              
                 --call the magic java memory manager!
                 --should voodoo the unused user memory and set_max_memory_size
                 --if I see this on the regular should consider sub-tiling the tile
                 
                 GZ_BUSINESS_UTILS.JAVA_MEMORY_MANAGER(p_output_topology || '_edge$', --not used
                                                       'GEOMETRY', --not used
                                                       SQLERRM);
                                                       
                 SDO_TOPO_MAP.REMOVE_OBSOLETE_NODES(NULL);
              
              ELSE

                  --All of the other obsolete node removal error poop goes here if we still see it
                  
                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);
                  
              END IF;

            END;

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                        'Completed removing obsolete nodes');

         END IF;



         BEGIN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                        'Committing topomap');

            ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(newtopomap,
                                                           p_output_topology,
                                                           3);

         EXCEPTION
         WHEN OTHERS
         THEN

            --Seen the edge x is not on the boundary of one or two of the faces it links here

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount, NULL,
                                                        'ADD_LINEAR_GEOMETRY unhandled topomap commit exception',
                                                        NULL,NULL,NULL,NULL,NULL,SQLERRM);


            --I dont think this is true dude, maybe an old workflow
            --Potentially dangerous on a partially processed dateline-crossing tile
            --xxIf the topomap doesnt commit we are in a state here where the build_edge table is populated
            --xxBut the topology doesnt match
            --GZ_BUILD_SOURCE.ROLL_BACK_A_TILE(p_output_topology,
                                             --p_tile_kount);

            --make sure to include the SQLERRM in my thrown error or else no error message to scan in the caller
            RAISE_APPLICATION_ERROR(-20001,SQLERRM || chr(10) || DBMS_UTILITY.format_error_backtrace);

         END;

         --If the topomap commit succeeded, deal with the work tables
         --There should be a 1:1 relationship between all work successfully completed in the topomap
         --and the processing in the work tables, including on dateline-split tiles
         --If the topomap work fails, in either add_linear_geometry or remove_obsolete_nodes
         --Then none of the below should happen
         -- xx_build_edge output: insert everything processed
         -- xx_build_geom input : update processed to 1
         -- xx_build_edge output: delete obsolete node removed edges

         BEGIN

            --insert all the edge_ids, source_edge_ids, and tile_numbers processed in add_linear_geometry
            --Many of these disappeared in remove obsolete nodes, they are added here for just a sec

            FORALL ii IN 1 .. feature_ids.COUNT
               EXECUTE IMMEDIATE 'INSERT INTO ' || p_output_topology || '_build_edge a '
                              || 'VALUES (:p1,:p2,:p3) '
               USING edge_ids(ii),     --new edge ids
                     feature_ids(ii),  --original maftiger edge_ids
                     p_tile_kount;

            COMMIT;

         EXCEPTION
         WHEN OTHERS
         THEN

            --attempt to trap the exact edge for informational purposes

            IF SQLERRM LIKE '%EDGEPKC%'
            THEN

                --this is my primary key constraint on the primitive edge ids (new topology edge ids)
                GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                            'ADD_LINEAR_GEOMETRY PKC exception on forall insert into ' || p_output_topology || '_build_edge',
                                                            NULL,NULL,NULL,NULL,NULL,SQLERRM);

                FOR j IN 1 .. feature_ids.COUNT
                LOOP

                   GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                               'Inserting ' || edge_ids(j) || ' ' || feature_ids(j) || ' ' || p_tile_kount);

                   BEGIN

                      EXECUTE IMMEDIATE 'INSERT INTO ' || p_output_topology || '_build_edge a '
                                     || 'VALUES (:p1,:p2,:p3) ' USING edge_ids(j),
                                                                      feature_ids(j),
                                                                      p_tile_kount;
                   EXCEPTION
                   WHEN OTHERS
                   THEN

                      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                                  'THERE: ' || edge_ids(j) || ' ' || feature_ids(j) || ' ' || p_tile_kount);

                      --roll the tile back no topomap commit
                      --   but it is committed???
                      --just rollback the one at a time insert
                      ROLLBACK;
                      --GZ_BUILD_SOURCE.ROLL_BACK_A_TILE(p_output_topology,
                                                       --p_tile_kount);

                   END;

                END LOOP;

            ELSE

               --This shouldn't do anything, unless Im forgetting a weird reason. No edges for this tile are committed
               --GZ_BUILD_SOURCE.ROLL_BACK_A_TILE(p_output_topology,
                                                --p_tile_kount);

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                           'ADD_LINEAR_GEOMETRY unhandled exception on forall insert into ' || p_output_topology || '_build_edge',
                                                           NULL,NULL,NULL,NULL,NULL,SQLERRM);

               RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            END IF;


         END;

         --Now update the input xx_build_geom table all the benchmark
         --edges added in this topomap. Processed = 1
         --faster? where a.edge_id IN (select edge_id from  z899in_build_edge)

         psql := 'UPDATE ' || p_output_topology || '_build_geom a '
              || 'SET a.processed = :p1 '
              || 'WHERE a.edge_id = :p2 ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                     'Deleting processed recs from ' || p_output_topology || '_build_geom',
                                                     p_sqlstmt => psql);

         FORALL ii IN 1 .. feature_ids.COUNT
            EXECUTE IMMEDIATE psql
            USING 1,
                  feature_ids(ii);

         COMMIT;

         --Now delete from build_edge any that disappeared in remove_obsolete nodes
         --Well not really disappeared, they joined with a neighboring edge and are now rep'd by it

         --dont use tile number since obsoletes can merge across tiles
         psql := 'DELETE FROM ' || p_output_topology || '_build_edge a '
              || 'WHERE NOT EXISTS '
              || '(SELECT e.edge_id FROM '
              || p_output_topology || '_edge$ e '
              || 'WHERE e.edge_id = a.edge_id) ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_TOPO_TILE ' || p_tile_kount,NULL,
                                                     'Deleting obsolete node absorbed recs from ' || p_output_topology || '_build_edge',
                                                     p_sqlstmt => psql);

         EXECUTE IMMEDIATE psql;
         COMMIT;

         --must do this because of dateline code that does two loops thru here
         edge_ids.DELETE;
         feature_ids.DELETE;
         insert_id := 0;

      END LOOP;  --end loop over very rare double dateline split tile


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('LOAD_TOPO_TILE: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END LOAD_TOPO_TILE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_GENESIS_EDGE_ID (
      p_topology           IN VARCHAR2,
      p_current_edge_geom  IN SDO_GEOMETRY,
      p_dbug               IN NUMBER DEFAULT 0
   ) RETURN NUMBER
   AS

      --Matt! 12/23/13
      --Sub of FIX_MYSTERY_FACE
      --Return 0 for fails

      psql                 VARCHAR2(4000);
      genesis_edges        GZ_TYPES.NUMBERARRAY;
      genesis_geoms        GZ_TYPES.geomarray;
      current_edge_geom    SDO_GEOMETRY;
      lrs_geom             SDO_GEOMETRY;
      start_measure        NUMBER;
      end_measure          NUMBER;
      best_edge            NUMBER := 0;


   BEGIN

      psql := 'SELECT e.edge_id, e.geometry FROM '
           || p_topology || '_build_geom e '
           || 'WHERE SDO_RELATE(e.geometry, :p1, :p2) = :p3 '
           || 'UNION ALL '
           || 'SELECT e.edge_id, e.geometry FROM '
           || p_topology || '_build_geom e '
           || 'WHERE SDO_RELATE(e.geometry, :p4, :p5) = :p6 '
           || 'UNION ALL '
           || 'SELECT e.edge_id, e.geometry FROM '
           || p_topology || '_build_geom e '
           || 'WHERE SDO_RELATE(e.geometry, :p7, :p8) = :p9 ';

      --get all edges from the <topo>_build_geom table that align with our mystery edge
      --most come back as INSIDE

      EXECUTE IMMEDIATE psql BULK COLLECT INTO genesis_edges,
                                               genesis_geoms USING p_current_edge_geom, 'mask=EQUAL', 'TRUE',
                                                                   p_current_edge_geom, 'mask=INSIDE', 'TRUE',
                                                                   p_current_edge_geom, 'mask=COVEREDBY', 'TRUE';

      IF genesis_edges.COUNT = 0
      THEN

         --never actually seen this but who knows, protect ya neck
         RETURN 0;

      END IF;


      FOR i IN 1 .. genesis_edges.COUNT
      LOOP

         IF p_dbug = 1
         THEN
            dbms_output.put_line('genesis ' || genesis_edges(i));
         END IF;

         --find an edge with direction that matches the current mystery edge

         --use start and end of the genesis edge candidate
         --my find measure wrapper builds measures in range 0 to 1000 for the current big mystery edge
         ---   (second input)

         start_measure := GZ_GEOM_UTILS.GZ_FIND_MEASURE(SDO_GEOMETRY(2001,
                                                                     p_current_edge_geom.SDO_SRID,
                                                                     SDO_POINT_TYPE (genesis_geoms(i).sdo_ordinates(1),
                                                                                     genesis_geoms(i).sdo_ordinates(2),
                                                                                     NULL),
                                                                      NULL,
                                                                      NULL),
                                                         p_current_edge_geom
                                                         );

         end_measure := GZ_GEOM_UTILS.GZ_FIND_MEASURE(SDO_GEOMETRY(2001,
                                                                   p_current_edge_geom.SDO_SRID,
                                                                   SDO_POINT_TYPE (genesis_geoms(i).sdo_ordinates(genesis_geoms(i).sdo_ordinates.COUNT - 1),
                                                                                   genesis_geoms(i).sdo_ordinates(genesis_geoms(i).sdo_ordinates.COUNT),
                                                                                   NULL),
                                                                   NULL,
                                                                   NULL),
                                                      p_current_edge_geom
                                                      );



         IF p_dbug = 1
         THEN
            dbms_output.put_line('start measure ' || start_measure);
            dbms_output.put_line('end_measure ' || end_measure);
            --sample for the cluephone. This is a troubling one with
            --two starter edges that are untrustworthy since they mis-measure
            --at the head/tail of the loop
            --genesis 58884378
            --start measure 183.43076038674
            --end_measure 999.99999987449
            --genesis 99286336
            --start measure 974.090043878689
            --end_measure 999.99999987449
            --genesis 58885610
            --start measure 549.241786935728
            --end_measure 568.992470076802
         END IF;

         IF start_measure < end_measure  --ordinarily this is what we want
         THEN

            --Having problems with loops
            --the base edge is a loop
            --the test edge, which in reality has wrong dir from 500 -back-> 0 is actually getting the end measure. 500 -> 1000.
            --Looks like the correct dir

            --If a measure is too close to 1000 this is not an ideal edge candidate
            --only accept it at the end if no others are better

            IF start_measure > 10 AND start_measure < 990
            AND end_measure > 10 AND end_measure < 990
            THEN

                --somewhere in the middle, trust it, go with it
                RETURN genesis_edges(i);

            ELSE

               --dont trust it
               --only use if nothing better
               best_edge := genesis_edges(i);

            END IF;
         
         ELSIF start_measure > end_measure  --backwards!
         AND ROUND(end_measure,0) = 0       --measures are 0 to 1000, ignore decimal places. But do round ordinates just in case 
         AND ROUND(genesis_geoms(i).sdo_ordinates(1),6) = ROUND(genesis_geoms(i).sdo_ordinates(genesis_geoms(i).sdo_ordinates.COUNT - 1),6)
         AND ROUND(genesis_geoms(i).sdo_ordinates(2),6) = ROUND(genesis_geoms(i).sdo_ordinates(genesis_geoms(i).sdo_ordinates.COUNT),6)
         THEN
         
            --dont like it but just in case its our only option
            --probably a loop like
            --start measure 570.037815125252
            --end_measure   .00000000246232752670853
            --where the end measure is basically 1000 but getting fudged past the top of the dial
            best_edge := genesis_edges(i);
         
         END IF;

      END LOOP;

      IF best_edge = 0
      THEN

         --Getting here is a fail. Found no edges with the correct direction
         --not even an untrustworthy one at the head or tail
         RETURN 0;

      ELSE

         --this is potentially dangerous, but Ive never seen this reached
         --The only way we even get into this rabbit hole is if the mystery edge
         --is some V6/Z8 etc topology where 10-100 edges in the input get merged
         --always plenty of candidates above
         RETURN best_edge;

      END IF;

   END GET_GENESIS_EDGE_ID;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION FIX_MYSTERY_FACE (
      p_topology           IN VARCHAR2,
      p_mystery_face       IN NUMBER,
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN NUMBER
   AS

      --Matt! Unassailable documentation of IQ in freefall as of 12/23/13

      --Return edge_id of the current topo chosen to bound the face. Or 0 for fail

      --In the process of loading the topology and *simultaneously* removing obsolete nodes (same cache)
      --   Oracle has completely made up an edge_id in obsolete node removal. Example
      --
      --    0---34----->0------35------>0<-----36----0
      --
      --usually, 99.9+% of the time we aren't in this fixer and the result is something like
      --
      --    0----------------34--------------------->0
      --
      --... where one of the edges wins with maintained direction.
      --But in this rare case Oracle has generated a new edge_id.
      --Direction does appear to be maintained for some edge_id however
      --
      --    0<---------------99234-------------------0
      --
      --(In all cases of these shenanigans I believe the mystery edge is a full loop around a face)
      --(and the loop touches some other edge/face at a point)
      --
      --The goal of this function, in an example like the one above, is to determine that
      --edge_id 36 is the genesis edge for 99234
      --And insert this into the <topo>_build_edge table
      --   edge_id  source_edge_id   tile_number
      --    99234     36              xx

      output                  NUMBER := 0;
      candidate_edge_ids      GZ_TYPES.NUMBERARRAY;
      candidate_edge_geoms    GZ_TYPES.GEOMARRAY;
      source_edge_id          NUMBER := 0;
      tile_number             NUMBER;
      psql                    VARCHAR2(4000);
      the_edge_id             NUMBER;


   BEGIN

      --Find the primitive edges in our working topo that bound the face with no known source edges

      psql := 'SELECT e.edge_id, e.geometry FROM '
           || p_topology || '_edge$ e WHERE e.left_face_id = :p1 '
           || 'UNION ALL '
           || 'SELECT e.edge_id, e.geometry FROM '
           || p_topology || '_edge$ e WHERE e.right_face_id = :p2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO candidate_edge_ids,
                                               candidate_edge_geoms USING p_mystery_face,
                                                                          p_mystery_face;


      FOR i IN 1 .. candidate_edge_ids.COUNT
      LOOP

         --if an island, just one candidate

         --return 0 when nothing found
         source_edge_id := GZ_BUILD_SOURCE.GET_GENESIS_EDGE_ID(p_topology,
                                                               candidate_edge_geoms(i));

         IF source_edge_id <> 0
         THEN

            the_edge_id := candidate_edge_ids(i);

            EXIT;

         END IF;

      END LOOP;


      IF source_edge_id <> 0
      THEN

         --insert

         --get tile number.  Not too important which one.  No sidx on tile table, not great
         psql := 'SELECT a.tile_number FROM ' || p_topology || '_build_tile a '
              || 'WHERE '
              || 'SDO_GEOM.RELATE(a.sdogeometry, :p1, (SELECT geometry FROM  ' || p_topology || '_build_geom WHERE edge_id = :p2), :p3) <> :p4 '
              || 'AND rownum = 1 ';

         EXECUTE IMMEDIATE psql INTO tile_number USING 'mask=ANYINTERACT',
                                                       source_edge_id,
                                                       p_tolerance,
                                                       'FALSE';

         psql := 'INSERT INTO ' || p_topology || '_build_edge VALUES (:p1,:p2,:p3)';

         EXECUTE IMMEDIATE psql USING the_edge_id,
                                      source_edge_id,
                                      tile_number;

         COMMIT;

         RETURN the_edge_id;

      ELSE

         --if we get here return 0
         RETURN output;

      END IF;


   END FIX_MYSTERY_FACE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION FACE_HAS_KNOWN_EDGES (
      p_topology           IN VARCHAR2,
      p_mystery_face       IN NUMBER,
      p_mystery_edges      IN GZ_TYPES.numberarray
   ) RETURN VARCHAR2
   AS

      --Matt! 11/27/13
      --Silly helper to determine that a face still has bounding edges with known attributes
      --This test is used in LOAD_OUTPUT_TOPOLOGY below where we get a few edges in edge$ that
      --   dont appear in the xx_build_edge work table (after obsolete node removal)
      --Returns 'TRUE' or 'FALSE'

      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(edge_id) FROM '
           || '(SELECT e.edge_id FROM ' || p_topology || '_edge$ e '
           || 'WHERE e.left_face_id = :p1 '
           || 'UNION '
           || 'SELECT e.edge_id FROM ' || p_topology || '_edge$ e '
           || 'WHERE e.right_face_id = :p2) '
           || 'WHERE edge_id NOT IN (SELECT * FROM TABLE(:p3)) ';

      EXECUTE IMMEDIATE psql INTO kount USING p_mystery_face,
                                              p_mystery_face,
                                              GZ_BUSINESS_UTILS.NUMARRAY_TO_VARRAY(p_mystery_edges);

      IF kount = 0
      THEN

         RETURN 'FALSE';

      ELSE

         RETURN 'TRUE';

      END IF;

   END FACE_HAS_KNOWN_EDGES;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------
   
   FUNCTION LIQUIDATE_MIGRATED_EDGES (
      p_topo               IN VARCHAR2,
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN NUMBER
   AS
   
      output               NUMBER;
      psql                 VARCHAR2(4000);
      delete_psql          VARCHAR2(4000);
      tilez                GZ_TYPES.numberarray;
      edge_idz             GZ_TYPES.numberarray;
      total_liquidated     PLS_INTEGER := 0;
   
   BEGIN
   
      psql := 'SELECT a.tile_number '
           || 'FROM ' || p_topo || '_build_tile a ';
      
      EXECUTE IMMEDIATE psql BULK COLLECT INTO tilez;
      
      --use sdo_geom.relate with no spatial index
      --know which edge geom <--> edge geom to compare already
      
      --current edges should be the same as or bigger than genesis edges
      
      --    0---34----->0------35------>0<-----36----0  before obs node removal, 1:1 between my edge$ and mt_edge$
      --
      --
      --    0----------------34--------------------->0  after obs node removal, my 34 still has a source edge_id
      --                                                of size in the 1st diagram, aka COVERS
      
      psql := 'SELECT e.edge_id FROM '
           || p_topo || '_build_edge e, '
           || p_topo || '_build_geom g, '
           || p_topo || '_edge$ ee '
           || 'WHERE '
           || 'e.tile_number = :p1 AND '
           || 'e.source_edge_id = g.edge_id AND '
           || 'e.edge_id = ee.edge_id AND '
           || 'SDO_GEOM.RELATE(ee.geometry, :p2, g.geometry, :p3) <> :p4 ';
           
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topo,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                  'Calling this SQL for each tile ', p_sqlstmt => psql);
                                                  
      --set this once
      delete_psql := 'DELETE FROM ' || p_topo || '_build_edge e '
                  || 'WHERE e.edge_id IN (SELECT * FROM TABLE(:p1))';
      
      FOR i IN 1 .. tilez.COUNT
      LOOP
      
         --processing tile by tile with indexes seems to be faster
         --than letting exadata table access storage full do its dumb $hit
  
         EXECUTE IMMEDIATE psql BULK COLLECT INTO edge_idz USING tilez(i),
                                                                 'MASK=COVERS+CONTAINS+EQUAL',
                                                                 p_tolerance,
                                                                 'MASK=COVERS+CONTAINS+EQUAL';

         IF edge_idz.COUNT > 0
         THEN
         
            total_liquidated := total_liquidated + edge_idz.COUNT;
            
            IF edge_idz.COUNT > 1
            THEN
            
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topo,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                           'Deleting ' ||  edge_idz.COUNT || ' edge ids from ' 
                                                           || p_topo || '_build_edge for tile ' || tilez(i),
                                                           p_sqlstmt => delete_psql);
                                                           
            ELSE
            
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topo,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                           'Deleting edge_id ' ||  edge_idz(1) || ' from '
                                                           || p_topo || '_build_edge for tile ' || tilez(i),
                                                           p_sqlstmt => delete_psql);
            
            END IF;
                                                        
            EXECUTE IMMEDIATE delete_psql USING GZ_BUSINESS_UTILS.NUMARRAY_TO_VARRAY(edge_idz);
            COMMIT;
         
         END IF;
      
         --may want some sort of mod(i) tracker in here
         --hopefully the delete log will kick in enough
      
      END LOOP;   
   
      RETURN total_liquidated;
      
   END LIQUIDATE_MIGRATED_EDGES;
   
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION LOAD_OUTPUT_TOPOLOGY (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_input_topology     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tile_kount         IN NUMBER,
      p_srid               IN NUMBER DEFAULT 8265,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_restart_flag       IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2
   AS

      --Matt! 2/13/12
      --6/17/13  Marked bad tiles incomplete on
      --         'is not on the boundary of one or two of the faces it links' wack-a-errors
      --         Was not getting caught on full job restarts
      --6/19/13  Added recurring loop over tiles instead of failure when any tile cant be processed
      --11/26/13 Moved obsolete node removal into this module

      psql                    VARCHAR2(4000);
      kount                   PLS_INTEGER;
      output                  VARCHAR2(4000) := '0';
      extent_layer            GZ_TYPES.GZ_LAYERS_IN_INFO_REC;
      tiles                   GZ_TYPES.geomarray;
      tiles_active            GZ_TYPES.geomarray;
      input_edge_kount        PLS_INTEGER;
      output_edge_kount       PLS_INTEGER;
      output_work_kount       PLS_INTEGER;
      build_geom_kount        PLS_INTEGER;
      raise_the_roof          NUMBER := 2147483648;
      src_srid                NUMBER;
      tile_rerun              NUMBER;
      deadman                 PLS_INTEGER := 0;
      restart_flag            VARCHAR2(1);
      mystery_edges           GZ_TYPES.numberarray;
      mystery_faces           GZ_TYPES.numberarray;
      added_edge              NUMBER;
      liquidated_edges        NUMBER;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('LOAD_OUTPUT_TOPOLOGY: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                  'Starting ' || p_output_topology);


      --get inputs
      extent_layer := GZ_BUILD_SOURCE.GET_EXTENT_LAYER(p_output_topology);

      IF p_restart_flag = 'Y'
      THEN

         restart_flag := p_restart_flag;

      ELSE

         restart_flag := 'N';

      END IF;


      IF restart_flag = 'N'
      OR NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_output_topology || '_BUILD_TILE')
      THEN

         IF extent_layer.source_table IS NOT NULL
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                        'Getting tiles for ' || p_output_topology || ' using ' || extent_layer.source_table);

            tiles := GZ_BUILD_SOURCE.GZ_TILE_TABLE(extent_layer.source_schema || '.' || extent_layer.source_table,
                                                   p_tile_kount,
                                                   'SDOGEOMETRY',  --parameterize?
                                                   extent_layer.where_clause,
                                                   extent_layer.sdo_filter,
                                                   'BUILD',
                                                   p_output_topology);

         ELSE

            --can be null if no extent layer exists
            --got to go with edge$.  Much slower, mainly on the aggr step to get full extent

            tiles := GZ_BUILD_SOURCE.GZ_TILE_TABLE(p_source_schema || '.' || p_input_topology || '_EDGE$',
                                                   p_tile_kount,
                                                   'GEOMETRY',
                                                   'a.edge_id IN (SELECT edge_id FROM ' || p_output_topology || '_build_poly)',
                                                   NULL,  --spatial filter already applies to edge whereclause
                                                   'BUILD',
                                                   p_output_topology);

         END IF;

         --put these in tile table
         GZ_BUILD_SOURCE.POPULATE_BUILD_TILE(p_output_topology,
                                             tiles,
                                             '0');   --no status

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                     'Getting tiles for ' || p_output_topology || ' using tile table');

         --restart, but still get all tiles here, skip later
         tiles := GZ_BUILD_SOURCE.GET_BUILD_TILE(p_output_topology);

      END IF;


      --set this so the looping will work
      tiles_active := tiles;

       --get source srid just once
      psql := 'SELECT a.srid '
           || 'FROM all_sdo_topo_info a '
           || 'WHERE '
           || 'a.owner = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.table_schema = :p3 AND '
           || 'rownum = 1 ';

      EXECUTE IMMEDIATE psql INTO src_srid USING p_source_schema,
                                                 p_input_topology,
                                                 p_source_schema;

      LOOP

         EXIT WHEN tiles_active.COUNT = 0;

         --roll through all the tiles, hopefully just one time

         FOR i IN 1 .. tiles.COUNT
         LOOP

            --all the work in here

            IF restart_flag = 'Y'
            AND GZ_BUILD_SOURCE.IS_TILE_PROCESSED(p_output_topology,i,'4')
            THEN


               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                           'Already loaded tile ' || i);

            ELSE


               BEGIN

                  GZ_BUILD_SOURCE.LOAD_TOPO_TILE(p_source_schema,
                                                 p_input_topology,
                                                 p_output_topology,
                                                 tiles(i),
                                                 i,
                                                 tiles.COUNT,
                                                 p_srid,
                                                 src_srid,
                                                 p_tolerance);

                  --no error? Mark tile cool
                  GZ_BUILD_SOURCE.MARK_TILE_STATUS(p_output_topology,
                                                   i,
                                                   '4'); --module 4

               EXCEPTION
               WHEN OTHERS
               THEN

                  IF SQLERRM LIKE '%MEMORY%' --my error
                  THEN

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                                'Got a memory error on tile ' || i
                                                             || ' Dont like this (consider smaller tiles) but Im gonna raise the roof');

                      --Sidey voodoo first
                      --This may not work - ORA-29550: Java session state cleared
                      --If the out of memory is ORA-29554: unhandled Java out of memory condition
                     dbms_session.free_unused_user_memory;
                     SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(raise_the_roof);  --applies to session even with no topomap open?

                     --any work in tile(i) should be cleaned up in lower error handlers

                     BEGIN

                        GZ_BUILD_SOURCE.LOAD_TOPO_TILE(p_source_schema,
                                                       p_input_topology,
                                                       p_output_topology,
                                                       tiles(i),
                                                       i,
                                                       p_srid,
                                                       src_srid,
                                                       p_tolerance);

                        --no error? Mark tile cool
                        GZ_BUILD_SOURCE.MARK_TILE_STATUS(p_output_topology,
                                                         i,
                                                         '4'); --module 4

                     EXCEPTION
                     WHEN OTHERS
                     THEN

                        --tile is dead to me, continue to next
                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                                    'Retry after memory error on tile ' || i || ' got another error (see error_msg). Skipping tile ',
                                                                    p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);


                     END;

                  ELSIF UPPER(SQLERRM) LIKE UPPER('%Attempted to add linear geometry outside the update window%')
                  THEN

                     --I think this handler always handles this error these days
                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                                 'Got a bogus add linear geometry outside the update window on tile ' || i
                                                              || ' Gonna call with a funky relate mask and double delta');

                     --Oracle is a little buggy with the overlapbdyintersect mask
                     --seeing cases where sdo_geom.relate (and eyes) see an edge that overlaps the window
                     --But sdo_relate misses it.  Result is a short topo window
                     --Here we will add additional masks that seem to trick sdo_relate into giving the right answer
                     --Adding OVERLAPBDYINTERSECT to the mask seems to usually resolve the problem. Slows down a bit
                     --Found 1 case requiring expanding the window in addition to mask change was necessary

                     BEGIN

                        GZ_BUILD_SOURCE.LOAD_TOPO_TILE(p_source_schema,
                                                       p_input_topology,
                                                       p_output_topology,
                                                       tiles(i),
                                                       i,
                                                       tiles.COUNT,
                                                       p_srid,
                                                       src_srid,
                                                       p_tolerance,
                                                       (p_tolerance * 2),  --double the delta
                                                       'mask=OVERLAPBDYDISJOINT+OVERLAPBDYINTERSECT');

                        --no error? Mark tile cool
                        GZ_BUILD_SOURCE.MARK_TILE_STATUS(p_output_topology,
                                                         i,
                                                         '4'); --module 4

                     EXCEPTION
                     WHEN OTHERS
                     THEN

                        --tile is dead to me, continue to next
                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                                    'Retry on tile ' || i || ' got another error (see error_msg). Skipping tile ',
                                                                    p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);


                     END;

                  ELSIF UPPER(SQLERRM) LIKE UPPER('%is not on the boundary of one or two of the faces it links%')
                  THEN

                     --Havent seen this error for a while
                     --This approach will work with obsolete node removal incorporated
                     --So just skip the tile for now
                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                                 'Got an -is not on the boundary- error (see error_msg). Skipping tile(s) ',
                                                                  p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

                     /* Remove this junk soon

                     --ORA-20001: ORA-29532: Java call terminated by uncaught Java exception: oracle.spatial.topo.TopoValidationException:
                     --Edge 3150111 is not on the boundary of one or two of the faces it links

                     --mystery error. Topomap didnt commit, and we rolled back the tile in load_topo_tile
                     --If the error indicates a bad edge in a neighboring tile
                     --remove the one baddy edge and rerun its tile.  Should catch just the one missing edge and add back in successfully
                     --This error occurs frequently, and this workaround usually works
                     --Sometimes it doesnt, and failure and restarting the job seems to work
                     --May want to add other trickery here. Like expanding the window, or changing the order of the edges, or setting
                     --   aside the tiles and returning to them at the end of processing

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                                 'Got a bogus is not on the boundary of one or two of the faces it links on tile ' || i
                                                              || ' Gonna attempt to remove and re-add the offending edge');

                     --remove the edge indicated and get its tile number
                     tile_rerun := GZ_BUILD_SOURCE.REMOVE_BADDY_EDGE(p_output_topology,
                                                                     SQLERRM);

                     --Before doing anything mark the baddy tile as incomplete.  If we fail this is important for reruns
                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                                 'Marking tile ' || tile_rerun || ' incomplete ' );

                     GZ_BUILD_SOURCE.MARK_TILE_STATUS(p_output_topology,
                                                      tile_rerun,
                                                      '0');


                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                                 'Removed an edge on tile ' || tile_rerun || '. Gonna rerun the tile we started on');

                     BEGIN

                        --original tile
                        GZ_BUILD_SOURCE.LOAD_TOPO_TILE(p_source_schema,
                                                       p_input_topology,
                                                       p_output_topology,
                                                       tiles(i),
                                                       i,
                                                       tiles.COUNT,
                                                       p_srid,
                                                       src_srid,
                                                       p_tolerance);

                        --no error? Mark tile cool
                        GZ_BUILD_SOURCE.MARK_TILE_STATUS(p_output_topology,
                                                         i,
                                                         '4'); --module 4

                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                               'Finished tile ' || i || ' rerun. Now back to the tile we messed with: ' || tile_rerun);


                        GZ_BUILD_SOURCE.LOAD_TOPO_TILE(p_source_schema,
                                                       p_input_topology,
                                                       p_output_topology,
                                                       tiles(tile_rerun),
                                                       tile_rerun,
                                                       tiles.COUNT,
                                                       p_srid,
                                                       src_srid,
                                                       p_tolerance);

                        --dont forget the rerun tile status (other tile is marked above)
                        GZ_BUILD_SOURCE.MARK_TILE_STATUS(p_output_topology,
                                                         tile_rerun,
                                                         '4');

                     EXCEPTION
                     WHEN OTHERS
                     THEN

                        --tiles are dead to me, continue to next
                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                               'Retry threw another error (see error_msg). Skipping tile(s) ',
                                                               p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);


                     END;

                     */

                     --end handling of is not on the boundary


                  ELSIF UPPER(SQLERRM) LIKE UPPER('%links a face but is not on its boundary%')
                  THEN

                     --bogus, mess with the topomap extent
                     --this should still work with obsolete nodes in the mix
                     --consider this approach for other errors too, like
                     --  '%is not on the boundary of one or two of the faces it links%' directly above

                     BEGIN

                        GZ_BUILD_SOURCE.LOAD_TOPO_TILE(p_source_schema,
                                                       p_input_topology,
                                                       p_output_topology,
                                                       tiles(i),
                                                       i,
                                                       tiles.COUNT,
                                                       p_srid,
                                                       src_srid,
                                                       p_tolerance,
                                                       .02);  --smaller delta

                        --no error? Mark tile cool
                        GZ_BUILD_SOURCE.MARK_TILE_STATUS(p_output_topology,
                                                         i,
                                                         '4'); --module 4

                     EXCEPTION
                     WHEN OTHERS
                     THEN

                        --tile is dead to me, continue to next
                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                               'Retry on tile ' || i || ' with smaller extent threw another error (see error_msg). Skipping tile ',
                                                               p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);


                     END;


                  ELSIF UPPER(SQLERRM) LIKE UPPER('%fetch out of sequence%')
                  THEN

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                                 'Tile ' || i || ' threw fetch out of sequence ',
                                                                  p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

                     RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);


                  ELSE

                     RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

                  END IF;

               END;


            END IF;

         END LOOP; --loop over all tiles

         --in case we ditched some that threw errors, get a new batch
         --this is just for the count, the geometry is wasted overhead
         tiles_active := GZ_BUILD_SOURCE.GET_BUILD_TILE(p_output_topology, '0');

         IF tiles_active.COUNT > 0
         THEN

            restart_flag := 'Y';

            --make sure we dont do this forever. 3 times max
            deadman := deadman + 1;

            IF deadman < 4
            THEN

               --consider a parameter change here to the topomap delta

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                           'Making another loop to try to load ' || tiles_active.COUNT || ' failed tiles');

            ELSE

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                           'Quitting. Tried three loops and ' || tiles_active.COUNT || ' tiles are still unable');

            END IF;

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                        'All tiles appear to have loaded successfully on loop ' || deadman);

         END IF;

      END LOOP;  --Outer loop over batches of incomplete tiles, usually just once. EXIT WHEN tiles_active.COUNT = 0

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('LOAD_OUTPUT_TOPOLOGY: Verifying');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                  'Completed load, verifying record counts ');

      --Checks

      --1st, the build_geom table should show all edges as processed.  Each tile should have sucked records in,
      --added them to the topology, and set processed to 1
      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_output_topology || '_build_geom a '
           || 'WHERE a.processed = :p1 ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                  'Verifying all recs in ' || p_output_topology || '_build_geom are processed',
                                                  p_sqlstmt => psql);

      EXECUTE IMMEDIATE psql INTO build_geom_kount USING 0;

      IF build_geom_kount <> 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                     'Oops: Some records remain unprocessed in ' || p_output_topology || '_build_geom');

         RAISE_APPLICATION_ERROR(-20001,'Oops: Some records remain unprocessed in ' || p_output_topology || '_build_geom');

      END IF;
      
      --obsolete node removal will occasionally bogus up some edges, making the records in xx_build_edge untrustworthy
      --in addition to the clearly bogus ones discovered and fixed (as necessary) below, that correspond to nothing in build_edge
      --sometimes the edge ids still match existing values in the universe. But current edge id and source edge id are no longer spatially related
      --usually the edge id of a new mystery spot migrates to a neighboring face
      --results in bad face attributes, and worst of all, no way of knowing about it
      
      --determine if any are spatially bad and if so delete the records from xx_build_edge
      --forces them into the mystery universe a moment later
      
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                  'Remove edges from ' || p_output_topology || 
                                                  '_build_edge that have no spatial relationship with edge$');
                                                  
      liquidated_edges := GZ_BUILD_SOURCE.LIQUIDATE_MIGRATED_EDGES(p_output_topology,
                                                                   p_tolerance);
                                                                   
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                  'Removed ' || liquidated_edges || ' edges from ' || p_output_topology || '_build_edge ');

      --There *should* in a rainbows and unicorns world be a 1:1 relationship between
      --output xx_build_edges and output primitive edge ids in xx_edge$
      --And thanks to obsolete node removal a less than or equal number of edges in those two tables
      --    than the input build_poly table

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_output_topology || '_build_edge ';

      EXECUTE IMMEDIATE psql INTO output_work_kount;

      IF output_work_kount = 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                     'Oops: no records in ' || p_output_topology || '_build_edge');

         RAISE_APPLICATION_ERROR(-20001,'Oops: no records in ' || p_output_topology || '_build_edge');

      END IF;

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_output_topology || '_edge$ ';

      EXECUTE IMMEDIATE psql INTO output_edge_kount;

      IF output_edge_kount = 0
      THEN

         --This is fatal, no topo edges
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                     'Oops: no records in ' || p_output_topology || '_edge$');

         RAISE_APPLICATION_ERROR(-20001,'Oops: no records in ' || p_output_topology || '_edge$');

      END IF;


      IF output_work_kount != output_edge_kount
      THEN

         --1:1 between build_edge (as its deleted from due to obsolete nodes) and new edge$
         --Unfortunately in about .0023% of obsolete node removals along a string of edges, Oracle generates a new
         --   unknown edge id.  This is not fatal so long as other edges around the face bounded by the mystery edge
         --   Can continue to represent the geographies and perform the face update in the later module

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                     'WARNING: mismatched record counts. Will check each face for good edges '
                                                     || p_output_topology || '_build_edge:' || output_work_kount || ' '
                                                     || p_output_topology || '_edge$:' || output_edge_kount);

         IF output_work_kount < output_edge_kount
         THEN

            --this we can potentially work with so long as each face still has a good known edge
            --Gonna log the H E hockeysticks outta this

            psql := 'SELECT e.edge_id FROM ' || p_output_topology || '_edge$ e '
                 || 'MINUS '
                 || 'SELECT a.edge_id FROM ' || p_output_topology || '_build_edge a ';

            --These are new edges where we know nothing about their L/R geographies
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                        'Getting new edges in ' || p_output_topology || '_edge$ where we know nothing about L/R values',
                                                        p_sqlstmt => psql);

            EXECUTE IMMEDIATE psql BULK COLLECT INTO mystery_edges;

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                        'Found ' || mystery_edges.COUNT || ' primitive $ edges where we know nothing about L/R values');

            psql := 'WITH mystery_edges '
                 || '       AS (SELECT edge_id FROM ' || p_output_topology || '_edge$ '
                 || '          MINUS '
                 || '          SELECT edge_id FROM ' || p_output_topology || '_build_edge) '
                 || ' SELECT e.left_face_id '
                 || '   FROM ' || p_output_topology || '_edge$ e, mystery_edges a '
                 || '  WHERE e.edge_id = a.edge_id AND e.left_face_id <> :p1 '
                 || ' UNION '
                 || ' SELECT e.right_face_id '
                 || '   FROM ' || p_output_topology || '_edge$ e, mystery_edges a '
                 || '  WHERE e.edge_id = a.edge_id AND e.right_face_id <> :p2 ';

            --Get faces bound by new questionable edges
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                        'Getting faces bound by at least one new (unknown L/R values) edge id in ' || p_output_topology || '_edge$',
                                                        p_sqlstmt => psql);

            EXECUTE IMMEDIATE psql BULK COLLECT INTO mystery_faces USING -1, -1;

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                        'Found ' || mystery_faces.COUNT || ' primitive faces bound by a mystery edge_id ');

            FOR i IN 1 .. mystery_faces.COUNT
            LOOP

               IF GZ_BUILD_SOURCE.FACE_HAS_KNOWN_EDGES(p_output_topology,
                                                       mystery_faces(i),
                                                       mystery_edges) = 'FALSE'
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                             'Must take evasive maneuvers, face '|| mystery_faces(i) || ' is bounded by '
                                                          || ' no remaining good edges');

                  --Find an edge in xx_build_geom that can be inserted into xx_build_edge to rep this face
                  --Returns either the current topo edge (not the _geom edge) added as <topo>_build_edge.edge_id, or 0 for fail
                  added_edge := GZ_BUILD_SOURCE.FIX_MYSTERY_FACE(p_output_topology,
                                                                 mystery_faces(i));

                  IF added_edge <> 0
                  THEN

                     --dont forget, the added edge is no longer a mystery, it now points back to a _geom edge
                     --with known L/R vals in the bench
                     mystery_edges := GZ_BUSINESS_UTILS.NUMBERARRAY_SUBTRACT(mystery_edges,
                                                                             added_edge);

                     --check again
                     IF GZ_BUILD_SOURCE.FACE_HAS_KNOWN_EDGES(p_output_topology,
                                                             mystery_faces(i),
                                                             mystery_edges) = 'FALSE'
                     THEN

                        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                                   'After attempted fix, face '|| mystery_faces(i) || ' is bounded by '
                                                                || ' no remaining good edges');

                        RAISE_APPLICATION_ERROR(-20001,'After attempted fix, mystery face '|| mystery_faces(i) || ' is bounded by '
                                                    || ' no remaining good edges');

                     END IF;


                  ELSE


                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                                'Gotta fail, couldnt fix face '|| mystery_faces(i) || ' which is bounded by '
                                                             || ' no remaining good edges');

                     RAISE_APPLICATION_ERROR(-20001,'Gotta fail, couldnt fix mystery face '|| mystery_faces(i) || ' which is bounded by '
                                                 || ' no remaining good edges');

                  END IF;

               ELSE

                  --Tis ok, theres some other edge id that bounds the face and has the necessary L/R values
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                              'Pass: Face ' || mystery_faces(i) || ' is still bound by a known good edge_id ');

               END IF;


            END LOOP;

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                        'Will continue, all ' || mystery_faces.COUNT || ' faces bounded by '
                                                       || mystery_edges.COUNT || ' mystery edges are now bound by at least one good edge ');

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Gotta Fail, this is unexpected: More edges in '|| p_output_topology || '_build_edge than '
                                           || p_output_topology || '_edge$.');

         END IF;

      END IF;


      psql := 'SELECT count(DISTINCT edge_id) '
           || 'FROM ' || p_output_topology || '_build_poly ';

      EXECUTE IMMEDIATE psql INTO input_edge_kount;

      IF input_edge_kount <= output_work_kount
      THEN

         --Cant imagine this is really possible
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                     'Oops: somehow more edges in the output than the input. '
                                                     || p_output_topology || '_build_edge:' || output_work_kount || ' '
                                                     || p_output_topology || '_build_poly:' || input_edge_kount);

         RAISE_APPLICATION_ERROR(-20001,'Oops: somehow more edges in the output than the input. '
                                        || p_output_topology || '_build_edge:' || output_work_kount || ' '
                                        || p_output_topology || '_build_poly:' || input_edge_kount);


      END IF;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                  'Gathering stats on ' || p_output_topology || '_build_edge',
                                                  NULL,NULL,NULL,NULL);

      --messed with this table a lot
      GZ_BUSINESS_UTILS.GATHER_TABLE_STATS(p_output_topology || '_BUILD_EDGE');

      --que mas?


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                  'Load and verification complete ' || p_output_topology);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('LOAD_OUTPUT_TOPOLOGY: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      RETURN output;


   END LOAD_OUTPUT_TOPOLOGY;

    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE REMOVE_OBSOLETE_NODES_EDGE (
      p_topology           IN VARCHAR2,
      p_sdo_window         IN SDO_GEOMETRY,
      p_tile_number        IN NUMBER,
      p_delta              IN NUMBER DEFAULT .05,
      p_tries              IN NUMBER DEFAULT 2
   )
   AS

      --Matt! 4/02/12
      --This tile (or tiles) is bumming me out.  It keeps throwing the weird
      --"a portion of the boundary of face x is unreachable... " error
      --or some other error we are trapping over and over
      --Usually rerunning again later resolves it, but not this bad boy
      --Will remove obsolete nodes edge by edge within the tile

      --For the a portion of the boundary of face x is unreachable error...
      --Usually what will happen is that the error will trigger again on one of the edges
      --But then clear up when returning to the edge later

      p_window             SDO_GEOMETRY;
      ez_topo_mgr          NUMBER;
      before_kount         NUMBER;
      after_kount          NUMBER;
      psql                 VARCHAR2(4000);
      edge_ids             GZ_TYPES.stringarray;
      edge_geoms           GZ_TYPES.geomarray;
      my_cursor            SYS_REFCURSOR;
      edge_mbr             SDO_GEOMETRY;

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('REMOVE_OBSOLETE_NODES_EDGE: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --what all to check?

      IF p_sdo_window.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Tile should be optimized, ordinate count is ' || p_sdo_window.sdo_ordinates.COUNT);

      ELSE

         --to be safe, expand our tile just like in topomap manager
         --Optimized rectangles with geodetic are always dangerous - need to test this

         p_window := p_sdo_window;

         p_window.sdo_ordinates(1) := p_window.sdo_ordinates(1) - p_delta;
         p_window.sdo_ordinates(2) := p_window.sdo_ordinates(2) - p_delta;
         p_window.sdo_ordinates(3) := p_window.sdo_ordinates(3) + p_delta;
         p_window.sdo_ordinates(4) := p_window.sdo_ordinates(4) + p_delta;

      END IF;

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_topology || '_node$ n ';

      EXECUTE IMMEDIATE psql INTO before_kount;


      psql := 'SELECT e.edge_id, e.geometry '
           || 'FROM ' || p_topology || '_edge$ e '
           || 'WHERE '
           || 'SDO_RELATE(e.geometry, :p1, :p2) = :p3 ';


      FOR j IN 1 .. p_tries
      LOOP

         OPEN my_cursor FOR psql USING p_window,
                                       'mask=ANYINTERACT',
                                       'TRUE';

         LOOP

            FETCH my_cursor BULK COLLECT INTO edge_ids, edge_geoms LIMIT 10;
            EXIT WHEN edge_ids.COUNT = 0;

            FOR i IN 1 .. edge_ids.COUNT
            LOOP

               edge_mbr := SDO_GEOM.SDO_MBR(edge_geoms(i));

               edge_mbr.sdo_ordinates(1) := edge_mbr.sdo_ordinates(1) - p_delta;
               edge_mbr.sdo_ordinates(2) := edge_mbr.sdo_ordinates(2) - p_delta;
               edge_mbr.sdo_ordinates(3) := edge_mbr.sdo_ordinates(3) + p_delta;
               edge_mbr.sdo_ordinates(4) := edge_mbr.sdo_ordinates(4) + p_delta;


               ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topology || '_MAP',
                                                              p_topology,
                                                              2,
                                                              edge_mbr.sdo_ordinates(1),
                                                              edge_mbr.sdo_ordinates(2),
                                                              edge_mbr.sdo_ordinates(3),
                                                              edge_mbr.sdo_ordinates(4)
                                                              );

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topology,'REMOVE_OBSOLETE_NODES_EDGE edge_id ' || edge_ids(i),NULL,
                                                      'Removing nodes',NULL,NULL,NULL,NULL,NULL,NULL,NULL);
               BEGIN

                  SDO_TOPO_MAP.REMOVE_OBSOLETE_NODES(NULL);

               EXCEPTION
               WHEN OTHERS
               THEN

                  IF SQLERRM LIKE '%is unreachable from the face boundary edge or island list%'
                  AND j < p_tries
                  THEN

                     --mystery.  This error is why we are in edge by edge processing to begin with
                     --A portion of the boundary of face ID 31 is unreachable from the face boundary edge or island list
                     --Actually I think the error throws on the topomap commit below

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                            'Edge ' || edge_ids(i) || ' obsolete node removal threw->',
                                                             NULL,NULL,NULL,NULL,NULL,SQLERRM);

                  ELSIF SQLERRM LIKE '%is unreachable from the face boundary edge or island list%'
                  AND j = p_tries
                  THEN

                     RAISE_APPLICATION_ERROR(-20001,'Out of tries and got the unreachable madness error');

                  ELSE

                     RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

                  END IF;


               END;

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topology,'REMOVE_OBSOLETE_NODES_EDGE edge_id ' || edge_ids(i),NULL,
                                                      'Commit and drop topomap',NULL,NULL,NULL,NULL,NULL,NULL,NULL);

               BEGIN

                  ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topology || '_MAP',
                                                                 p_topology,
                                                                 3);

               EXCEPTION
               WHEN OTHERS
               THEN

                  IF SQLERRM LIKE '%is unreachable from the face boundary edge or island list%'
                  THEN

                     --mystery.  This error is why we are in edge by edge processing to begin with
                     --A portion of the boundary of face ID 31 is unreachable from the face boundary edge or island list

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                            'Edge ' || edge_ids(i) || ' obsolete node removal threw->',
                                                             NULL,NULL,NULL,NULL,NULL,SQLERRM);

                  ELSE

                     RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

                  END IF;

               END;

            END LOOP;

         END LOOP;


         edge_ids.DELETE;
         edge_geoms.DELETE;
         CLOSE my_cursor;

         IF j < p_tries
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topology,'REMOVE_OBSOLETE_NODES_EDGE ',NULL,
                                                   'Going back for another loop over the edges',NULL,NULL,NULL,NULL,NULL,NULL,NULL);

         END IF;

      END LOOP;


      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_topology || '_node$ n ';

      EXECUTE IMMEDIATE psql INTO after_kount;

      IF after_kount = before_kount
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Didnt remove any nodes!');

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topology,'REMOVE_OBSOLETE_NODES_EDGE ' || p_tile_number,NULL,
                                                'Complete.  Removed ' || (before_kount - after_kount) || ' obsoletes');

      END IF;


   END REMOVE_OBSOLETE_NODES_EDGE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE REMOVE_OBSOLETE_NODES (
      p_topology           IN VARCHAR2,
      p_sdo_window         IN SDO_GEOMETRY,  --could allow this to be null
      p_tile_number        IN NUMBER,
      p_delta              IN NUMBER DEFAULT .05
   )
   AS

      --Matt! 2/17/12

      p_window             SDO_GEOMETRY;
      ez_topo_mgr          NUMBER;
      before_kount         NUMBER;
      after_kount          NUMBER;
      psql                 VARCHAR2(4000);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('REMOVE_OBSOLETE_NODES: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --what all to check?

      IF p_sdo_window.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Tile should be optimized, ordinate count is ' || p_sdo_window.sdo_ordinates.COUNT);

      ELSE

         --to be safe, expand our tile just like in topomap manager
         --Optimized rectangles with geodetic are always dangerous - need to test this

         p_window := p_sdo_window;

         p_window.sdo_ordinates(1) := p_window.sdo_ordinates(1) - p_delta;
         p_window.sdo_ordinates(2) := p_window.sdo_ordinates(2) - p_delta;
         p_window.sdo_ordinates(3) := p_window.sdo_ordinates(3) + p_delta;
         p_window.sdo_ordinates(4) := p_window.sdo_ordinates(4) + p_delta;

      END IF;

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_topology || '_node$ n ';

      EXECUTE IMMEDIATE psql INTO before_kount;


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topology,'REMOVE_OBSOLETE_NODES ' || p_tile_number,NULL,
                                             'Opening topomap with window-->',NULL,NULL,NULL,NULL,NULL,NULL,p_window);


      ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topology || '_MAP',
                                                     p_topology,
                                                     2,
                                                     p_window.sdo_ordinates(1),
                                                     p_window.sdo_ordinates(2),
                                                     p_window.sdo_ordinates(3),
                                                     p_window.sdo_ordinates(4)
                                                     );

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topology,'REMOVE_OBSOLETE_NODES ' || p_tile_number,NULL,
                                             'Removing nodes',NULL,NULL,NULL,NULL,NULL,NULL,NULL);

      SDO_TOPO_MAP.REMOVE_OBSOLETE_NODES(NULL);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topology,'REMOVE_OBSOLETE_NODES ' || p_tile_number,NULL,
                                             'Commit and drop topomap',NULL,NULL,NULL,NULL,NULL,NULL,NULL);

      ez_topo_mgr := GZ_TOPO_UTIL.EZ_TOPOMAP_MANAGER(p_topology || '_MAP',
                                                     p_topology,
                                                     3);

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_topology || '_node$ n ';

      EXECUTE IMMEDIATE psql INTO after_kount;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topology,'REMOVE_OBSOLETE_NODES ' || p_tile_number,NULL,
                                             'Complete.  Removed ' || (before_kount - after_kount) || ' obsoletes');


   END REMOVE_OBSOLETE_NODES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION CLEAN_OUTPUT_TOPOLOGY (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tile_kount         IN NUMBER,
      p_srid               IN NUMBER DEFAULT 8265,
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN VARCHAR2
   AS

      --Matt! 2/16/12
      --! 11/26/13 Moved obsolete node removal to load_output_topology

      --1. Check for dangles
      --2. Check for isolated nodes
      --3. Check for obsolete nodes ?and remove if found? For now error
      --4. Check for 1:1 between xx_build_edge and xx_edge$


      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      output            VARCHAR2(4000) := '0';
      extent_layer      GZ_TYPES.GZ_LAYERS_IN_INFO_REC;
      tiles             GZ_TYPES.geomarray;
      status_clue       VARCHAR2(1);
      deadman           PLS_INTEGER := 0;
      tile_ids          GZ_TYPES.stringarray;
      raise_the_roof    NUMBER := 2147483648;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CLEAN_OUTPUT_TOPOLOGY: Starting checks');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                  'Starting ' || p_output_topology || ' checks for isolated nodes or dangles');

      --checks

      --1. isolated

      IF GZ_BUILD_SOURCE.ISOLATED_NODES_EXIST(p_output_topology)
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                     'Quitting.  We have isolated nodes in ' || p_output_topology);

         RAISE_APPLICATION_ERROR(-20001, 'Quitting.  We have isolated nodes in ' || p_output_topology);

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                     'Sweet, no isolated nodes in ' || p_output_topology);

      END IF;

      --2. dangles

      IF GZ_BUILD_SOURCE.DANGLE_EDGES_EXIST(p_output_topology)
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                     'Quitting.  We have dangling edges in ' || p_output_topology);

         RAISE_APPLICATION_ERROR(-20001, 'Quitting.  We have dangling in ' || p_output_topology);

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                     'Sweet, no dangling edges in ' || p_output_topology);

      END IF;


      --3. CHECK FOR OBSOLETES HERE
      --With possible single obsolete node removal code, see below for old examples
      --Could call gz_topo_util.GET_OBSOLETE_NODES
      --Assume for a moment that an obsolete node or 2 gets through
      --Does any processing that follow choke on them?  I think clip at least will be cool
      --And if not maybe removing from the smaller, parallelized clip/sp/ls topos is more efficient anyway
      --Verdict not arrived at

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                  'Checking ' || p_output_topology || ' for obsolete nodes, could take a minute(or 20)');

      kount := GZ_TOPO_UTIL.COUNT_OBSOLETE_NODES(p_output_topology);

      IF kount = 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                     'Marvelous, no obsolete nodes in ' || p_output_topology);

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                     'This can be dealt with but I want to see it -- '
                                                     || kount || ' obsolete nodes in ' || p_output_topology);

         RAISE_APPLICATION_ERROR(-20001,'This can be dealt with but I wanna see it -- '
                                         || kount || ' obsolete nodes in ' || p_output_topology);

      END IF;


      /*  Saving all of this defunct obsolete node code for error handling code
          in case I need to pull it into load_topo_tile

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CLEAN_OUTPUT_TOPOLOGY: Removing obsolete nodes ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


      LOOP

         EXIT WHEN tiles.COUNT = 0;

         FOR i IN 1 .. tiles.COUNT
         LOOP

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                   'Tile ' || tile_ids(i) || ' obsolete node removal ');

            BEGIN

               status_clue := '5';

               IF deadman = 0
               THEN

                  --SOP. First try
                  GZ_BUILD_SOURCE.REMOVE_OBSOLETE_NODES(p_output_topology,
                                                        tiles(i),
                                                        tile_ids(i));

               ELSIF deadman = 1
               THEN

                  --second try, reduce topomap to 1/2 the overlap of try 1
                  --suspicion is that the problem is an edge right on the boundary of the topomap
                  --where Oracle is getting the face but not the edge
                  GZ_BUILD_SOURCE.REMOVE_OBSOLETE_NODES(p_output_topology,
                                                        tiles(i),
                                                        tile_ids(i),
                                                        .25);


               ELSE

                  --3rd and 4th try
                  --desperate edge by edge last ditch effort
                  --extremely slow, just builds a little topomap for each edge in the tile and calls remove obsolete

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                         'Desperate, sending tile ' || tile_ids(i) ||
                                                         ' to edge by edge obsolete node removal');

                  GZ_BUILD_SOURCE.REMOVE_OBSOLETE_NODES_EDGE(p_output_topology,
                                                             tiles(i),
                                                             tile_ids(i));

               END IF;


            EXCEPTION
            WHEN OTHERS
            THEN

               IF SQLERRM LIKE '%is unreachable from the face boundary edge or island list%'
               OR SQLERRM LIKE '%is not on the boundary of one or two of the faces it links%'
               OR (SQLERRM LIKE '%Face ID%' AND SQLERRM LIKE '%not found in cache%')
               THEN

                  --mystery 1
                  --A portion of the boundary of face ID 31 is unreachable from the face boundary edge or island list
                  --mystery 2
                  --Edge x is not on the boundary of one or two of the faces it links
                  --   Seems to be a topomap bug. The edge mentioned is in a neighboring tile right on the edge of the current topomap
                  --mystery 3
                  --Face ID 218850 not found in cache ORA-06512: at "MDSYS.SDO_TOPO_MAP", line 342

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                         'Weird: Tile ' || tile_ids(i) || ' obsolete node removal threw->',
                                                         NULL,NULL,NULL,NULL,NULL,SQLERRM);

                  --update as 4 so we can potentially return to this tile
                  status_clue := '4';

               ELSIF UPPER(SQLERRM) LIKE '%JAVA.LANG.OUTOFMEMORYERROR%'
               THEN

                  --ORA-20001: ORA-29532: Java call terminated by uncaught Java exception:
                  --java.lang.OutOfMemoryError ORA-06512: at "GZACS12.GZ_UTILITIES", line 4921

                  --only seen this once in remove obsolete nodes.  Happened on topomap commit
                  --As of coding have never seen this trap succeed
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                         'Gonna raise the roof and return to this tile: Tile ' || tile_ids(i) || ' obsolete node removal threw->',
                                                          NULL,NULL,NULL,NULL,NULL,SQLERRM);

                  SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(raise_the_roof);

                  --update as 4 so we can return to this tile
                  status_clue := '4';

               ELSE

                   --what else?
                   --my own errors in remove_obsolete_nodes_edge will fall into this bucket
                   RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

               END IF;

            END;

            --mark tile with 5 unless known failure to return to
            GZ_BUILD_SOURCE.MARK_TILE_STATUS(p_output_topology,
                                             tile_ids(i),
                                             status_clue);

         END LOOP;

         --tries are 0 - normal
         --          1 - reduced delta used for topomap
         --          2 - edge by edge try 1
         --          3 - edge by edge try 2
         deadman := deadman + 1;

         IF deadman < 4
         THEN

            --no accessor for this style

            psql := 'SELECT a.tile_number, a.sdogeometry '
                 || 'FROM ' || p_output_topology || '_build_tile a '
                 || 'WHERE a.status_clue = :p1 '
                 || 'ORDER by a.tile_number ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO tile_ids,
                                                     tiles USING '4';


            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                   'Going back for another loop over ' || tiles.COUNT || ' failed tiles ');

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Yo, we cant seem to complete obsolete node removal');

         END IF;


         --delete edges here by tile maybe if performance below gets bad

      END LOOP;
      */

      --no tile-based obsolete node removal to do this tile-by-tile now
      --just set em all

      psql := 'UPDATE ' || p_output_topology || '_build_tile a '
           || 'SET '
           || 'a.status_clue = :p1 ';

      EXECUTE IMMEDIATE psql USING '5';
      COMMIT;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CLEAN_OUTPUT_TOPOLOGY: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                  'Complete ' || p_output_topology);

      RETURN output;

   END CLEAN_OUTPUT_TOPOLOGY;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   PROCEDURE CREATE_BUILD_FACE (
      p_table_name               IN VARCHAR2,
      p_release                  IN VARCHAR2,
      p_project_id               IN VARCHAR2,
      p_topology                 IN VARCHAR2
   )
   AS

      --Matt! 2/21/12
      --! Updated 9/30/13 to respect reference_face_fields.field_length

      psql                 VARCHAR2(4000);
      measurements         GZ_TYPES.stringarray;
      layers               GZ_TYPES.GZ_LAYERS_IN_INFO;
      lengths              GZ_TYPES.stringarray;

   BEGIN

      measurements := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                             p_project_id,
                                                             'MEASUREMENT');

      layers := GZ_BUILD_SOURCE.GET_LAYER_INFO(p_topology);

      --Build SQL string for face table
      psql := 'CREATE TABLE ' || p_table_name || ' ('
           || 'FACE_ID NUMBER, '
           || 'TILE_NUMBER NUMBER, '
           || 'TOPOGEOM SDO_TOPO_GEOMETRY, '
           || 'GEOID VARCHAR2(4000), '; --really bub?


      FOR i IN 1 .. layers.COUNT
      LOOP

         --return a single length, as a character
         lengths := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                                p_project_id,
                                                                'ATTRIBUTE',
                                                                'REFERENCE_FACE_FIELDS',
                                                                'FIELD_LENGTH',
                                                                layers(i).layer);

         psql := psql || layers(i).layer || ' VARCHAR2(' || lengths(1) || '), ';

      END LOOP;

      FOR i in 1 .. measurements.COUNT
      LOOP

         --includes QC col?

         IF measurements(i) LIKE '%GEOMETRY%' --ICK
         OR measurements(i) LIKE '%MBR%'      --Double ICK
         THEN

            psql := psql || measurements(i) || ' SDO_GEOMETRY';

         ELSE

            psql := psql || measurements(i) || ' NUMBER';

         END IF;

         IF i != measurements.COUNT
         THEN

            psql := psql || ',';

         ELSE

            psql := psql || ' ) NOPARALLEL NOLOGGING ';

         END IF;

      END LOOP;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_topology,'CREATE_BUILD_FACE',NULL,
                                             'SQL for ' || p_topology || '_face ',NULL,NULL,NULL,psql);

      BEGIN

         EXECUTE IMMEDIATE psql;


      EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -60
            OR SQLCODE = -18014
            THEN

               --ORA-00060: deadlock detected while waiting for resource
               --Usually bogus, just heavy production
               --can't use dbms_lock, it's not available on all databases (yet)
               --DBMS_LOCK.sleep(5);
               --just try again like fools
               EXECUTE IMMEDIATE psql;

            ELSIF SQLCODE = -955
            THEN

               --table already exists
               EXECUTE IMMEDIATE 'DROP TABLE ' || UPPER(p_table_name) || ' PURGE';

               EXECUTE IMMEDIATE psql;

            ELSE

               RAISE;

            END IF;


      END;


      --see if we can get away with this
      EXECUTE IMMEDIATE 'ALTER TABLE ' || p_table_name || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || p_table_name || 'PKC '
              || '      PRIMARY KEY(FACE_ID) '
              || ')';


      --bitmap index on handful of tiles

      GZ_BUSINESS_UTILS.ADD_INDEX(p_table_name,
                             p_table_name || '_TN',
                            'TILE_NUMBER',
                            'BITMAP');

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);


   END CREATE_BUILD_FACE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

    FUNCTION CREATE_FACE_TABLE (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 2/17/12

      --Create the table
      --register topo
      --insert face_ids
      --update tile numbers (messy)



      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      output            VARCHAR2(4000) := '0';
      tilez             GZ_TYPES.stringarray;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_FACE_TABLE: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_FACE_TABLE',NULL,
                                             'Starting ' || p_output_topology || '_face creation');


      --create it

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_FACE_TABLE',NULL,
                                             'Creating ' || p_output_topology || '_face');

      GZ_BUILD_SOURCE.CREATE_BUILD_FACE(p_output_topology || '_FACE',
                                        p_release,
                                        p_project_id,
                                        p_output_topology);


      --register

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_FACE_TABLE',NULL,
                                             'Registering ' || p_output_topology || '_face topogeom');

      SDO_TOPO.add_topo_geometry_layer(p_output_topology,
                                       p_output_topology || '_FACE',
                                       'TOPOGEOM',
                                       'POLYGON');

      --insert face ids

      psql := 'INSERT INTO '
            || p_output_topology || '_FACE (face_id) '
            || 'SELECT f.face_id '
            || 'FROM ' || p_output_topology || '_face$ f '
            || 'WHERE f.face_id != :p1 ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_FACE_TABLE',NULL,
                                             'Inserting ' || p_output_topology || '_face face_ids',
                                             NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql USING -1;
      COMMIT;

      --update tile numbers

      psql := 'SELECT DISTINCT '
           || 'a.tile_number '
           || 'FROM ' || p_output_topology || '_build_edge a ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO tilez;

      psql := 'UPDATE ' || p_output_topology || '_face aa '
              || 'SET aa.tile_number = :p1 '
              || 'WHERE aa.face_id IN '
              || '(SELECT DISTINCT f.face_id FROM '
              || p_output_topology || '_edge$ e, '
              || p_output_topology || '_face$ f, '
              || p_output_topology || '_build_edge ee '
              || 'WHERE ee.tile_number = :p2 AND '
              || 'e.edge_id = ee.edge_id AND '
              || '(e.left_face_id = f.face_id OR e.right_face_id = f.face_id) AND '
              || 'f.face_id != :p3) '
              || 'AND aa.tile_number IS NULL ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_FACE_TABLE',NULL,
                                             'Updating ' || p_output_topology || '_face tile_number ' || tilez.COUNT || ' times ',
                                             NULL,NULL,NULL,psql);

      FOR i IN 1 .. tilez.COUNT
      LOOP

         --this is a huge mess
         --should be rewritten in a single SQL
         --but better to do this inefficently now than let the following steps be tile free

          EXECUTE IMMEDIATE psql USING tilez(i),
                                       tilez(i),
                                       -1;

      END LOOP;

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_output_topology || '_face a '
           || 'WHERE a.tile_number IS NULL ';

      EXECUTE IMMEDIATE psql INTO kount;

      IF kount > 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_FACE_TABLE',NULL,
                                                'Bailing out, got ' || kount || ' NUll tile_numbers in ' || p_output_topology || '_face',
                                                 NULL,NULL,NULL,psql);

         RETURN 'Got ' || kount || ' NUll tile_numbers in ' || p_output_topology || '_face';

      END IF;


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_FACE_TABLE',NULL,
                                             'Gathering stats on ' || p_output_topology || '_face',
                                             NULL,NULL,NULL,NULL);

      GZ_BUSINESS_UTILS.GATHER_TABLE_STATS(p_output_topology || '_FACE');

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_FACE_TABLE: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_FACE_TABLE',NULL,
                                             'Complete ' || p_output_topology || '_face creation');

      RETURN output;

   END CREATE_FACE_TABLE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   PROCEDURE BUILD_TOPO_FROM_TOPO (
      p_topo               IN VARCHAR2,
      p_table              IN VARCHAR2,
      p_featuretable_pkc   IN VARCHAR2,
      p_topo_type          IN NUMBER,
      p_topo_col           IN VARCHAR2 DEFAULT 'TOPOGEOM',
      p_subset_col         IN VARCHAR2 DEFAULT NULL,
      p_subset_value       IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 2/21/11
      --Modified from versions in clip and merge


      psql              VARCHAR2(4000);
      tg_layer_type     VARCHAR2(32);
      tg_layer_id       NUMBER;

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_TOPO: Verify Inputs ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_topo_type NOT IN (1,2,3)
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'What the heck is a topo type of ' || p_topo_type);

      ELSE

         IF p_topo_type = 1
         THEN
            tg_layer_type := 'POINT';
         ELSIF p_topo_type = 2
         THEN
            tg_layer_type := 'LINE';
         ELSIF p_topo_type = 3
         THEN
            tg_layer_type := 'POLYGON';
         END IF;

      END IF;

      --???????????????????????????
      --What else should we check??
      --???????????????????????????


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_TOPO: Construct features for ' || p_table);
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_topo,
                                                  p_table,
                                                  p_topo_col,
                                                  tg_layer_type);

      psql := 'UPDATE ' || p_table || ' a '
           || 'SET '
           || 'a.topogeom = SDO_TOPO_GEOMETRY(:p1,:p2,:p3,SDO_TOPO_OBJECT_ARRAY(SDO_TOPO_OBJECT (a.' || p_featuretable_pkc || ',:p4))) ';


      BEGIN

         IF p_subset_col IS NOT NULL
         THEN

            psql := psql || 'WHERE '
                         || 'a.' || p_subset_col || ' = :p5 ';

            EXECUTE IMMEDIATE psql USING p_topo,
                                         p_topo_type,
                                         tg_layer_id,
                                         p_topo_type,
                                         p_subset_value;

         ELSE

            EXECUTE IMMEDIATE psql USING p_topo,
                                         p_topo_type,
                                         tg_layer_id,
                                         p_topo_type;

         END IF;

         COMMIT;

      EXCEPTION
      WHEN OTHERS
      THEN

         RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

      END;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_TOPO_FROM_TOPO: Peace Out ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END BUILD_TOPO_FROM_TOPO;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

    FUNCTION CONSTRUCT_FACE_TABLE (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 2/21/12


      psql              VARCHAR2(4000);
      kount             PLS_INTEGER;
      output            VARCHAR2(4000) := '0';
      tilez             GZ_TYPES.stringarray;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CONSTRUCT_FACE_TABLE: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CONSTRUCT_FACE_TABLE',NULL,
                                             'Starting ' || p_output_topology || '_face topogeom construction');

      --work by tile in order

       tilez := GZ_BUILD_SOURCE.GET_FACE_TILES(p_output_topology);

      FOR i IN 1 .. tilez.COUNT
      LOOP

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CONSTRUCT_FACE_TABLE',NULL,
                                                 'Calling build_topo_from_topo for tile ' || tilez(i));


         GZ_BUILD_SOURCE.BUILD_TOPO_FROM_TOPO(p_output_topology,
                                              p_output_topology || '_FACE',
                                              'FACE_ID',
                                              3,
                                              'TOPOGEOM',
                                              'TILE_NUMBER',
                                              tilez(i));

      END LOOP;

      --anything to check??

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_output_topology || '_face a '
           || 'WHERE a.topogeom IS NULL ';

      EXECUTE IMMEDIATE psql INTO kount;

      IF kount > 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CONSTRUCT_FACE_TABLE',NULL,
                                                'Bailing out, got ' || kount || ' NUll topogeoms in ' || p_output_topology || '_face',
                                                 NULL,NULL,NULL,psql);

         RETURN 'Got ' || kount || ' NUll topogeoms in ' || p_output_topology || '_face';

      END IF;

      --tune up now, have relation$
      --includes stats

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CONSTRUCT_FACE_TABLE',NULL,
                                             'Calling topo tune up for stats and index tidying ');

      GZ_TOPO_UTIL.GZ_TOPO_TUNE_UP(p_output_topology);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CONSTRUCT_FACE_TABLE: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CONSTRUCT_FACE_TABLE',NULL,
                                             'Complete ' || p_output_topology || '_face topogeom construction');

      RETURN output;

   END CONSTRUCT_FACE_TABLE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE POPULATE_BUILD_GEOID (
      p_output_topology    IN VARCHAR2,
      p_column_name        IN VARCHAR2 DEFAULT 'GEOID',
      p_delim              IN VARCHAR2 DEFAULT ',',
      p_null_fill          IN VARCHAR2 DEFAULT ' '
   )
   AS

      --Matt! 5/2/12
      --This is just some silly code to populate a geoid on the face feature table
      --Real geoid population comes some day from the alternate FSL build module
      --Or some other solution

      layerz               GZ_TYPES.GZ_LAYERS_IN_INFO;
      psql                 VARCHAR2(4000);

   BEGIN


      layerz := GZ_BUILD_SOURCE.GET_LAYER_INFO(p_output_topology);

      --update z609in_face a
      --set a.geoid = NVL(a.statefp, ' ') || ',' || NVL(a.uace, ' ')


      psql := 'UPDATE ' || p_output_topology || '_FACE a '
           || 'SET a.' || p_column_name || ' = ';

      FOR i IN 1 .. layerz.COUNT
      LOOP

         psql := psql || 'NVL(a.' || layerz(i).layer || ', ''' || p_null_fill || ''') ';

         IF i <> layerz.COUNT
         THEN

            psql := psql || ' || ''' || p_delim || ''' || ';

         END IF;

      END LOOP;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_BUILD_GEOID',NULL,
                                             'Executing build geoid sql -->', NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql;
      COMMIT;

   END POPULATE_BUILD_GEOID;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

    FUNCTION UPDATE_FACE_LAYERS (
      p_schema             IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 2/21/12
      --Switch to sdo_get_oid_from_face
      --! 11/26/13 New obsolete node on the fly removal
      --           kills off the xx_build_geom records
      --           Switch this fun to use the current topo edge$ for the SDO
      --! Also 11/27/13 New obsolete node plan results in a handful of edges
      --                that cant be tied back to the benchmark
      --                So they must be ignored in the L/R updates
      --! Performance issues in big Z6 with L/R ignore edges. Hopefully fixed 1/6/13


      psql                 VARCHAR2(4000);
      lupdate_sql          VARCHAR2(4000);
      rupdate_sql          VARCHAR2(4000);
      kount                PLS_INTEGER;
      output               VARCHAR2(4000) := '0';
      tilez                GZ_TYPES.stringarray;
      layerz               GZ_TYPES.GZ_LAYERS_IN_INFO;
      layerz_layer_levelz  GZ_TYPES.stringarray;
      faces_r              GZ_TYPES.numberarray;
      edges_r              GZ_TYPES.numberarray;
      faces_l              GZ_TYPES.numberarray;
      edges_l              GZ_TYPES.numberarray;  
      tile_kount           PLS_INTEGER;
      ignore_edges         MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE();
      
   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('UPDATE_FACE_LAYERS: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                  'Starting ' || p_output_topology || '_face layer updates ');

      --work by tile and layer

      layerz := GZ_BUILD_SOURCE.GET_LAYER_INFO(p_output_topology);

      --spin through the layers and get matching tg_layer_level. One time only dealie
      FOR i IN 1 .. layerz.COUNT
      LOOP

         layerz_layer_levelz(i) := GZ_BUILD_SOURCE.GET_TG_LAYER_LEVEL(layerz(i).source_schema,
                                                                      layerz(i).source_topology,
                                                                      layerz(i).source_table,
                                                                      'TOPOGEOM',
                                                                      'POLYGON');

      END LOOP;

      --one time only dealie, get the edges that we cant use for L/R updates
      --For some reason collecting these edges once and then re-using in the updates is slow
      --will still get them for the kount only      
      
      psql := 'SELECT e.edge_id FROM ' || p_output_topology || '_edge$ e '
           || 'MINUS '
           || 'SELECT a.edge_id FROM ' || p_output_topology || '_build_edge a ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                  'Will ignore these edges in update since they cant be tied back to the bench',
                                                  p_sqlstmt => psql);

      EXECUTE IMMEDIATE psql BULK COLLECT INTO ignore_edges;

      --set up the update sql one time
      --sample ... a little out of date
      /*UPDATE Z899IN_face a
         SET (a.CD, a.COUNTY) =
                (SELECT GZ_BUILD_SOURCE.SDO_GET_OID_FROM_FACE (
                           'ACS13',
                           'MT',
                           'CD',
                           0,
                           e.right_face_id,
                           q'`a.vintage = q'^90^' and a.cdsessn = q'^113^' and a.cdtyp = q'^O^' and a.statefp NOT IN (q'^74^')`',
                           'OID',
                           c.geometry),
                        GZ_BUILD_SOURCE.SDO_GET_OID_FROM_FACE (
                           'ACS13',
                           'MT',
                           'COUNTY',
                           1,
                           e.right_face_id,
                           q'`a.vintage = q'^90^' and a.statefp NOT IN (q'^74^')`',
                           'OID',
                           c.geometry)
                   FROM ACS13.MT_edge$ e, Z899IN_build_edge b, Z899IN_edge$ c
                  WHERE     b.edge_id = :p1
                        AND b.source_edge_id = e.edge_id
                        AND b.edge_id = c.edge_id)
         WHERE a.face_id = :p2
       */

      psql := q'~UPDATE ~' || p_output_topology || q'~_face a ~'
           || q'~SET (~';

      FOR i IN 1 .. layerz.COUNT
      LOOP

         IF i != layerz.COUNT
         THEN

            psql := psql || q'~a.~' || layerz(i).layer || q'~, ~';

         ELSE

            psql := psql || q'~a.~' || layerz(i).layer || q'~) ~';

         END IF;

      END LOOP;

      psql := psql || q'~ = (SELECT~';

      rupdate_sql := psql;
      lupdate_sql := psql;
      psql := '';

      FOR i IN 1 .. layerz.COUNT
      LOOP

         --come on TOAD~!~
         rupdate_sql := rupdate_sql || q'~ GZ_BUILD_SOURCE.SDO_GET_OID_FROM_FACE(~'  --SDO
                                    || q'~'~' || layerz(i).source_schema || q'~',~'
                                    || q'~'~' || layerz(i).source_topology || q'~',~'
                                    || q'~'~' || layerz(i).source_table ||  q'~',~'
                                    || layerz_layer_levelz(i) || ', '
                                    || q'~e.right_face_id,~';

         lupdate_sql := lupdate_sql || q'~ GZ_BUILD_SOURCE.SDO_GET_OID_FROM_FACE(~'  --SDO
                                    || q'~'~' || layerz(i).source_schema || q'~',~'
                                    || q'~'~' || layerz(i).source_topology || q'~',~'
                                    || q'~'~' || layerz(i).source_table ||  q'~',~'
                                    || layerz_layer_levelz(i) || ', '
                                    || q'~e.left_face_id,~';

         rupdate_sql := rupdate_sql || q'~q'`~' || layerz(i).where_clause || q'~`',~'
                                    || q'~'~' || layerz(i).source_key || q'~',~'             
                                    || q'~c.geometry)~';                                     

         lupdate_sql := lupdate_sql || q'~q'`~' || layerz(i).where_clause || q'~`',~'
                                    || q'~'~' || layerz(i).source_key || q'~',~'             
                                    || q'~c.geometry)~';                                     


         IF i != layerz.COUNT
         THEN
            rupdate_sql := rupdate_sql || q'~,~';
            lupdate_sql := lupdate_sql || q'~,~';
         ELSE
            rupdate_sql := rupdate_sql || q'~ ~';
            lupdate_sql := lupdate_sql || q'~ ~';
         END IF;

      END LOOP;

      psql := psql || q'~FROM ~'
                   || p_source_schema || q'~.~' || p_source_topology || q'~_edge$ e, ~'
                   || p_output_topology || q'~_build_edge b, ~'
                   || p_output_topology || q'~_build_geom c ~'  
                   || q'~WHERE ~'
                   || q'~b.edge_id = :p1 AND ~'
                   || q'~b.source_edge_id = e.edge_id AND ~'
                   || q'~b.source_edge_id = c.edge_id) ~'  
                   || q'~WHERE a.face_id = :p2 ~';

      rupdate_sql := rupdate_sql || psql;
      lupdate_sql := lupdate_sql || psql;

      psql := '';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                  'SQL for right update-->',NULL,NULL,NULL,rupdate_sql);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                  'SQL for left update-->',NULL,NULL,NULL,lupdate_sql);


      --get tile universe from pre tagged col in face table. Ordered by tile

      tilez := GZ_BUILD_SOURCE.GET_FACE_TILES(p_output_topology);


      FOR i IN 1 .. tilez.COUNT
      LOOP

         --check the status clue in case this is a restart
         psql := 'SELECT COUNT(*) FROM '
               || p_output_topology || '_build_tile a '
               || 'WHERE a.tile_number = :p1 AND a.status_clue = :p2 ';

         EXECUTE IMMEDIATE psql INTO tile_kount USING TO_NUMBER(tilez(i)),
                                                      '8';

         IF tile_kount = 0
         THEN

            --get the universe of rights first
            --sample
            /*SELECT f.face_id, e.edge_id
              FROM z210in_edge$ e, z210in_face f
             WHERE     f.tile_number = 1
                   AND e.right_face_id = f.face_id
                   AND e.edge_id = (SELECT MAX (ee.edge_id)
                                      FROM z210in_edge$ ee
                                     WHERE ee.right_face_id = f.face_id)
            */

            psql := 'SELECT f.face_id, e.edge_id '
                 || 'FROM '
                 || p_output_topology || '_edge$ e, '
                 || p_output_topology || '_face f '
                 || 'WHERE '
                 || 'f.tile_number = :p1 AND '
                 || 'e.right_face_id = f.face_id AND '
                 || 'e.edge_id = (SELECT MAX(ee.edge_id) FROM '
                 || p_output_topology || '_edge$ ee WHERE ee.right_face_id = f.face_id ';

            IF ignore_edges.COUNT = 0
            THEN

               psql := psql || ') ';

            ELSE

               --make sure the mystery edges dont get bubbled up as the max to use
               --I bet this makes zero sense in the future.  Trust
               
               psql := psql || 'AND ee.edge_id NOT IN '
                            || '(SELECT e.edge_id FROM ' || p_output_topology || '_edge$ e '
                            || ' MINUS '
                            || ' SELECT a.edge_id FROM ' || p_output_topology || '_build_edge a'
                            || ')) ';

            END IF;

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                        'Entering right update loop for tile ' || tilez(i), NULL,NULL,NULL,psql);


            --Removed the cursoring structure here
            --should never be more than a few thousand to collect per tile

            EXECUTE IMMEDIATE psql BULK COLLECT INTO faces_r, 
                                                     edges_r USING tilez(i);


            --here we have 1 face and 1 edge in the new topo
            --that will uniquely ID the geogs on the right
            
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                        'Calling right update SQL for ' || faces_r.COUNT || ' faces', NULL,NULL,NULL,rupdate_sql);

            FORALL jj IN 1 .. faces_r.COUNT
               EXECUTE IMMEDIATE rupdate_sql USING edges_r(jj),
                                                   faces_r(jj);
            COMMIT;

            faces_r.DELETE;
            edges_r.DELETE;
            
            --now do the universe of lefts

            --sample ..out of date
            /*SELECT f.face_id, e.edge_id
                 FROM Z210IN_edge$ e, Z210IN_face f
                WHERE     f.tile_number = 1
                      AND e.left_face_id = f.face_id
                      AND e.edge_id = (SELECT MAX (ee.edge_id)
                                         FROM Z210IN_edge$ ee
                                        WHERE ee.left_face_id = f.face_id)
                      AND f.face_id NOT IN
                             (SELECT DISTINCT (fff.face_id)               --The max edge join is not necessary in the sub
                                FROM Z210IN_edge$ eee, Z210IN_face fff    --Dont do it, results in full table scan
                               WHERE fff.tile_number = 1
                                     AND eee.right_face_id = fff.face_id)
            */


            psql := 'SELECT f.face_id, e.edge_id '
                 || 'FROM '
                 || p_output_topology || '_edge$ e, '
                 || p_output_topology || '_face f '
                 || 'WHERE '
                 || 'f.tile_number = :p1 '
                 || 'AND e.left_face_id = f.face_id AND '
                 || 'e.edge_id = (SELECT MAX(ee.edge_id) FROM '
                 || p_output_topology || '_edge$ ee WHERE ee.left_face_id = f.face_id ';

            IF ignore_edges.COUNT = 0
            THEN

               psql := psql || ') AND '
                            || 'f.face_id NOT IN ('
                            || 'SELECT DISTINCT (fff.face_id) '
                            || 'FROM '
                            || p_output_topology || '_edge$ eee, '
                            || p_output_topology || '_face fff '
                            || 'WHERE '
                            || 'fff.tile_number = :p2 AND '
                            || 'eee.right_face_id = fff.face_id) ';

            ELSE

               --If there are edges to ignore, ignore them both in the current
               --update, and also pay attention to them in the NOT IN sub clause
               --(which throws lots of faces out from this update since they already got
               --handled in the right update )
               --Any face that had an ignore edge in the right update, go ahead and hit it
               --again (usually) or for the first time (if the right ignore was the only edge)
               --Here in the left update
               
               --Oddly doing NOT IN (select * from table(:p1)) with the list of ignore_edges is slow

               psql := psql || 'AND ee.edge_id NOT IN '
                            || '(SELECT e.edge_id FROM ' || p_output_topology || '_edge$ e '   --(SELECT * FROM TABLE(:p2)) is slow!
                            || ' MINUS '
                            || ' SELECT a.edge_id FROM ' || p_output_topology || '_build_edge a'
                            || ') '
                            || ') AND '
                            || 'f.face_id NOT IN ('
                            || 'SELECT DISTINCT (fff.face_id) '
                            || 'FROM '
                            || p_output_topology || '_edge$ eee, '
                            || p_output_topology || '_face fff '
                            || 'WHERE '
                            || 'fff.tile_number = :p3 AND '
                            || 'eee.right_face_id = fff.face_id AND '
                            || 'eee.edge_id NOT IN '
                            || '(SELECT e.edge_id FROM ' || p_output_topology || '_edge$ e '
                            || ' MINUS '
                            || ' SELECT a.edge_id FROM ' || p_output_topology || '_build_edge a'
                            || '))';


            END IF;

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                        'Entering left update loop for tile ' || tilez(i), NULL,NULL,NULL,psql);

            EXECUTE IMMEDIATE psql BULK COLLECT INTO faces_l, 
                                                     edges_l USING tilez(i),
                                                                   tilez(i);

            --here we have 1 face and 1 edge in the new topo
            --that will uniquely ID the geogs on the left
            
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                        'Calling left update SQL for ' || faces_l.COUNT || ' faces', NULL,NULL,NULL,lupdate_sql);

            FORALL jj IN 1 .. faces_l.COUNT
               EXECUTE IMMEDIATE lupdate_sql USING edges_l(jj),
                                                   faces_l(jj);
            
            COMMIT;

            edges_l.DELETE;
            faces_l.DELETE;

            --finish loop on this tile
            --tile source is the distinct tiles in face table
            --but mark on the tile table since this loop is a big one, may need to restart

            --mark tile with 8
            GZ_BUILD_SOURCE.MARK_TILE_STATUS(p_output_topology,
                                             tilez(i),
                                             8);

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                        'Skipping previously processed tile ' || tilez(i), NULL,NULL,NULL,NULL);

         END IF;


      END LOOP;

      --create phony geoid.  This may be temporary, just to keep clip and SP happy in 2012

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                             'Calling korny GEOID builder');

      GZ_BUILD_SOURCE.POPULATE_BUILD_GEOID(p_output_topology,
                                           'GEOID');


      --There better be some checks here
      --I dont know what to check though - seems like almost anything can be NULL
      --Even an entire column, right?  Say aiannh in some states

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('UPDATE_FACE_LAYERS: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                  'Complete ' || p_output_topology || '_face layer updates');

      RETURN output;

   END UPDATE_FACE_LAYERS;


    -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

  FUNCTION POPULATE_MEASUREMENTS (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tolerance          IN NUMBER,
      p_srid               IN NUMBER,
      p_topofix_qa         IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge           IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge          IN VARCHAR2 DEFAULT 'N'
   ) RETURN VARCHAR2
   AS

      --Matt! 3/01/12
      --Matt! 6/10/13 p_topofix_qa option

      output            VARCHAR2(4000) := '0';
      measurements      GZ_TYPES.stringarray;
      measurehash       GZ_TYPES.stringhash;
      tilez             GZ_TYPES.stringarray;
      psql              VARCHAR2(4000);
      badfaces          GZ_TYPES.stringarray;
      fix_val           VARCHAR2(4000);
      invalidfacekount  PLS_INTEGER;
      edgefix_val       VARCHAR2(1);



   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                             'Starting ' || p_output_topology);


      measurements := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                             p_project_id,
                                                            'MEASUREMENT',
                                                            'REFERENCE_FACE_FIELDS'); --fixed, right?

      --I think there is a QC in this assoc array but we will never touch it below
      measurehash := GZ_BUILD_SOURCE.STRINGARRAY2HASH(measurements);

      --get tile universe from pre tagged col in face table

      tilez := GZ_BUILD_SOURCE.GET_FACE_TILES(p_output_topology);



      IF measurehash.EXISTS('SDOGEOMETRY')
      THEN

         IF p_fix_edge = 'Y'
         THEN

            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 15');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Call gz_fix_edge');
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                        'Call gz_fix_edge on the full ' || p_output_topology || ' topology ');

            --return char '1' for some still bad, '0' for all good

            edgefix_val := GZ_TOPOFIX.GZ_FIX_EDGE(p_output_topology,  --fake job id
                                                  p_output_topology,
                                                  'BUILD', --log type
                                                  'Y', --hold univeral
                                                   p_tolerance,
                                                   NULL, --continue looping over edges as long as they are being fixed
                                                   p_fix_2edge); --fix pairs of close edges, usually N. Slow

            IF edgefix_val = '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                           'GZ_FIX_EDGE success');


            ELSIF edgefix_val = '1'
            AND p_topofix_qa = 'Y'
            THEN

               --SOP on a failed fix.  We want to see it
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                      'GZ_FIX_EDGE not successful, we will fail the build job to be safe ');

               output := output || '|Failed to fix all edges in gz_fix_edge';

            ELSIF edgefix_val = '1'
            AND p_topofix_qa = 'N'
            THEN

               --overridden to ignore this unfixed edge(s) from gz_build_source_setup.topofix_qa
               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                      'GZ_FIX_EDGE not successful, but we we wont fail the build job since topofix_qa is N');

            ELSE

                RAISE_APPLICATION_ERROR(-20001,'Unknown GZ_FIX_EDGE result ' || edgefix_val);

            END IF;


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'NOT Calling gz_fix_edge on the full ' || p_output_topology || ' topology ');

         END IF;

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || p_output_topology || '_face geometry ');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         --no allowance to change SRIDs here

         --loop over each tile to try to avoid 24 hour hang up
         --not efficient on small jobs, but avocado son

         FOR i IN 1 .. tilez.COUNT
         LOOP

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'Calc ' || p_output_topology || '_face geometry on tile ' || tilez(i));

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_FACE',
                                                  'FACE_ID',
                                                  'SDOGEOMETRY',
                                                  'ALL',
                                                  p_tolerance,
                                                  NULL, --TO SRID is null, no translate
                                                  'TILE_NUMBER',
                                                  tilez(i));


            ----------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Check for NULL or bad gtype geometries in ' || p_output_topology || '_face');
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            ----------------------------------------------------------------------------------

            --this is still in the loop over tilez
            --I guess thats ok

            psql := 'SELECT a.face_id '
                 || 'FROM ' || p_output_topology || '_face a '
                 || 'WHERE '
                 || 'a.tile_number = :p1 AND '
                 || '( a.sdogeometry IS NULL '
                 || '  OR (a.sdogeometry.sdo_gtype != :p1 AND a.sdogeometry.sdo_gtype != :p2) ) ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO badfaces USING tilez(i),
                                                                    2003,
                                                                    2007;

            IF badfaces.COUNT > 0
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                      'Got ' || badfaces.COUNT || ' NULL sdogeometry or bad gtypes in tile ' || tilez(i),
                                                       NULL,NULL,NULL,psql,NULL,NULL,NULL);

               --update these to QC 2. Auto fixer doesnt handle

               psql := 'UPDATE ' || p_output_topology || '_face a '
                    || 'SET '
                    || 'a.qc = :p1 '
                    || 'WHERE a.face_id IN (SELECT * FROM TABLE(:p2)) ';

               EXECUTE IMMEDIATE psql USING '2',
                                             GZ_BUSINESS_UTILS.stringarray_to_varray(badfaces);

               output := output || '|From tile ' || tilez(i) || ' we have ' || badfaces.COUNT || ' NULL or wrong gtype sdogeometries ';

               badfaces.DELETE;

            ELSE

               --Im tempted to be nice and round ordinates here but clip comes next
               --The clip developer can suck it

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                      'Cool: ' || badfaces.COUNT || ' NULL sdogeometry or bad gtypes in tile ' || tilez(i),
                                                       NULL,NULL,NULL,psql,NULL,NULL,NULL);

            END IF;


         END LOOP; --end loop over topos

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS_ETC: Index ' || p_output_topology || '_face sdogeometry');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         --seems polite to do this no matter what
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                'Index ' || p_output_topology || '_face sdogeometry ');

         --this is actually a drop and rebuild
         GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(p_output_topology || '_face',
                                        'SDOGEOMETRY',
                                        p_srid,
                                        p_tolerance);

         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Call topofix on ' || p_output_topology || '_face');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                'Always: call topofix on  ' || p_output_topology || '_face');

         --will update QC for all that it can't fix
         --and update sdo for those it does fix
         --Returns '0' for success, '1' for something bad
         --QC 1 is 13349, 13356 or some other validate_Geometry_with_context error
         --QC 2 is NULL sdogeometry
         --QC 3 is non-2003 gtype

         fix_val := GZ_TOPOFIX.CHECK_FACE_TAB(p_output_topology,  --phony job id
                                              p_output_topology,
                                              p_output_topology || '_FACE',
                                              'FACE_ID',
                                              'BUILD',
                                               p_tolerance,
                                               p_srid);

         /*
         fix_val := GZ_TOPOFIX.GZ_FIX_FACE(p_output_topology,  --phony job id, dont care
                                           p_output_topology,
                                           p_output_topology || '_FACE',
                                           'BUILD',            --add to our current log
                                           'Y',                --Yes hold universal
                                           'N',                --No calc sdo in advance
                                            p_tolerance,
                                           'QC',
                                           'SDOGEOMETRY',
                                           p_srid);
         */

         IF fix_val <> '0'
         AND p_topofix_qa = 'Y'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'We cant get a valid geometry in ' || p_output_topology || '_face for some faces',
                                                    NULL,NULL,NULL,NULL,NULL,NULL,NULL);


            output := output || '| We have some invalid face sdogeometries ';

         ELSIF fix_val <> '0'
         AND p_topofix_qa = 'N'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'We cant get a valid geometry in ' || p_output_topology || '_face for some faces'
                                                   || ' but are not going to fail since topofix_qa is N',
                                                    NULL,NULL,NULL,NULL,NULL,NULL,NULL);

         ELSE


            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'Good: We have a valid geometry in ' || p_output_topology || '_face for all of our faces',
                                                   NULL,NULL,NULL,psql,NULL,NULL,NULL);

         END IF;

      END IF; --BIG IF on if we have sdogeometry


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || p_output_topology || '_face MBR and and index it');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --FROM HERE on we calc all fields if we are good

      FOR i IN 1 .. tilez.COUNT
      LOOP

         IF measurehash.EXISTS('MBR')
         AND measurehash.EXISTS('SDOGEOMETRY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'Calc MBR and index it ' || p_output_topology || '_face');



            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_face',
                                                  'FACE_ID',
                                                  'MBR',
                                                  'ALL',
                                                   p_tolerance,
                                                   NULL,
                                                   'TILE_NUMBER',
                                                   tilez(i));

           --Does anyone give a fudge if the MBRs are invalid?

           psql := 'SELECT a.face_id '
                || 'FROM ' || p_output_topology || '_face a '
                || 'WHERE SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT(a.mbr, :p1) != :p2 AND '
                || 'a.tile_number = :p2 ';

            EXECUTE IMMEDIATE psql BULK COLLECT INTO badfaces USING p_tolerance,
                                                                    'TRUE',
                                                                    tilez(i);

            IF badfaces.COUNT > 0
            THEN

               --nobody gives a pudding
               --just log and update QC val of 4

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                      'We cant get a valid geometry in tile ' || tilez(i) || ' for '
                                                      || badfaces.COUNT || 'of our MBRs',
                                                      NULL,NULL,NULL,psql,NULL,NULL,NULL);

                psql := 'UPDATE ' || p_output_topology || '_face a '
                     || 'SET '
                     || 'a.qc = :p1 '
                     || 'WHERE a.face_id IN (SELECT * FROM TABLE(:p2)) AND '
                     || 'a.qc IS NULL ';

                EXECUTE IMMEDIATE psql USING '4',
                                              GZ_BUSINESS_UTILS.stringarray_to_varray(badfaces);

                COMMIT;

                badfaces.DELETE;

            END IF;


         END IF;

          ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 70');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || p_output_topology || '_face areatotal');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         IF measurehash.EXISTS('AREATOTAL')
         AND measurehash.EXISTS('SDOGEOMETRY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'Calc areatotal for tile ' || tilez(i));

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_face',
                                                  'FACE_ID',
                                                  'AREATOTAL',
                                                  'ALL',
                                                   p_tolerance,
                                                   NULL,
                                                   'TILE_NUMBER',
                                                   tilez(i));

            --Sometimes invalid geometries that are very small slivers make areas < 0
            --utility handles this

         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 80');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || p_output_topology || '_face perimeter');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         IF measurehash.EXISTS('PERIMETER')
         AND measurehash.EXISTS('SDOGEOMETRY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'Calc perimeter for tile ' || tilez(i));

            -- 'unit=meter' is hard coded into utility  !

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_face',
                                                  'FACE_ID',
                                                  'PERIMETER',
                                                  'ALL',
                                                   p_tolerance,
                                                   NULL,
                                                   'TILE_NUMBER',
                                                   tilez(i));


         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 90');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || p_output_topology || '_face pa_ratio');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------

         IF measurehash.EXISTS('PA_RATIO')
         AND measurehash.EXISTS('SDOGEOMETRY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'Calc pa_ratio for tile ' || tilez(i));


            --DECODEs when area is 0 to avoid divide by 0
            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_face',
                                                  'FACE_ID',
                                                  'PA_RATIO',
                                                  'ALL',
                                                   p_tolerance,
                                                   NULL,
                                                   'TILE_NUMBER',
                                                   tilez(i));

         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 100');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || p_output_topology || '_face LLX');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------


         IF measurehash.EXISTS('MBR')
         AND measurehash.EXISTS('SDOGEOMETRY')
         AND measurehash.EXISTS('LLX')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'Calc llX for tile ' || tilez(i));

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_face',
                                                  'FACE_ID',
                                                  'LLX',
                                                  'ALL',
                                                   p_tolerance,
                                                   NULL,
                                                   'TILE_NUMBER',
                                                   tilez(i));

         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 110');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || p_output_topology || '_face LLY');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------


         IF measurehash.EXISTS('MBR')
         AND measurehash.EXISTS('SDOGEOMETRY')
         AND measurehash.EXISTS('LLY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'Calc lly for tile ' || tilez(i));

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_face',
                                                  'FACE_ID',
                                                  'LLY',
                                                  'ALL',
                                                   p_tolerance,
                                                   NULL,
                                                   'TILE_NUMBER',
                                                   tilez(i));

         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 120');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || p_output_topology || '_face URX');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------


         IF measurehash.EXISTS('MBR')
         AND measurehash.EXISTS('SDOGEOMETRY')
         AND measurehash.EXISTS('URX')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'Calc urx for tile ' || tilez(i));

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_face',
                                                  'FACE_ID',
                                                  'URX',
                                                  'ALL',
                                                   p_tolerance,
                                                   NULL,
                                                   'TILE_NUMBER',
                                                   tilez(i));

         END IF;


         ----------------------------------------------------------------------------------
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         DBMS_APPLICATION_INFO.SET_ACTION('Step 120');
         DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Calc ' || p_output_topology || '_face URY');
         --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         ----------------------------------------------------------------------------------


         IF measurehash.EXISTS('MBR')
         AND measurehash.EXISTS('SDOGEOMETRY')
         AND measurehash.EXISTS('URY')
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'Calc ury for tile ' || tilez(i));

            GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_face',
                                                  'FACE_ID',
                                                  'URY',
                                                  'ALL',
                                                   p_tolerance,
                                                   NULL,
                                                   'TILE_NUMBER',
                                                   tilez(i));

         END IF;

      END LOOP;   --end loop over tiles calcing secondary measurements



       --index the MBR
      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                             'Calc sidx on ' || p_output_topology || '_face MBR ');

      GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(p_output_topology || '_FACE',
                                     'MBR',
                                     p_srid,
                                     p_tolerance,
                                     NULL,
                                     NULL,
                                     p_output_topology || '_FACE' || '_MIDX');


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                             'Complete ' || p_output_topology);

      RETURN output;

   END POPULATE_MEASUREMENTS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION FINALIZE_AND_VALIDATE (
      p_schema             IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tile_kount         IN NUMBER,
      p_drop_tables        IN VARCHAR2 DEFAULT 'Y',
      p_validate_topo      IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2
   AS

      --Matt! 3/2/12
      --Last module, tidy up and validate

      output               VARCHAR2(4000) := '0';
      extent_layer         GZ_TYPES.GZ_LAYERS_IN_INFO_REC;
      tiles                GZ_TYPES.geomarray;
      validstr             VARCHAR2(4000);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FINALIZE_AND_VALIDATE: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                             'Starting ' || p_output_topology);


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                             'Calling topo tune up ');

      GZ_TOPO_UTIL.GZ_TOPO_TUNE_UP(p_output_topology);

      IF p_validate_topo = 'Y'
      THEN
      
         --Get tiles for validation

         --get input layers
         extent_layer := GZ_BUILD_SOURCE.GET_EXTENT_LAYER(p_output_topology);

         IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_output_topology || '_BUILD_TILE')
         THEN

            --this code really only here if we want to manually restart
            --drop the tile table
            --and request a new tile number

            IF extent_layer.source_table IS NOT NULL
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                      'Getting tiles for ' || p_output_topology || ' using ' || extent_layer.source_table);

               tiles := GZ_BUILD_SOURCE.GZ_TILE_TABLE(extent_layer.source_schema || '.' || extent_layer.source_table,
                                                      p_tile_kount,
                                                      'SDOGEOMETRY',  --parameterize?
                                                      extent_layer.where_clause,
                                                      extent_layer.sdo_filter,
                                                      'BUILD',
                                                      p_output_topology);

            ELSE

               --can be null if no extent layer exists
               --got to go with edge$.  Much slower, mainly on the aggr step to get full extent

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                      'Getting tiles for ' || p_output_topology || ' using edge$ extent');


               tiles := GZ_BUILD_SOURCE.GZ_TILE_TABLE(p_output_topology || '_EDGE$',
                                                      p_tile_kount,
                                                      'GEOMETRY',
                                                      'a.left_face_id = -1 or a.right_face_id = -1',
                                                      NULL,  --whole topo, no filter
                                                      'BUILD',
                                                      p_output_topology);

            END IF;

         ELSE

             GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                    'Getting tiles from ' || p_output_topology || '_build_tile');

            tiles := GZ_BUILD_SOURCE.GET_BUILD_TILE(p_output_topology);

         END IF;

         BEGIN

            validstr := GZ_TOPO_UTIL.VALIDATE_TOPOLOGY_TILE(p_output_topology,
                                                            tiles,
                                                            p_log_type=>'BUILD');

         EXCEPTION
         WHEN OTHERS
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                   'Boo: ' || p_output_topology || ' is not valid ');

            --this is a FAIL but we'll continue
            output := output || ' VALIDATE_TOPOLOGY_TILE threw ' || SQLERRM || ' |';

         END;

         IF validstr = 'TRUE'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                   'Sweet: ' || p_output_topology || ' is valid ');

         END IF;
         
      ELSE
      
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                     'Skipping topo validation since validate_topo param is ' || p_validate_topo);
                                                   
      END IF;


      --drop tables ?
      --if yes and success

      IF p_drop_tables = 'Y'
      AND output = '0'
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                'Dropping work tables ');

         BEGIN

            GZ_BUSINESS_UTILS.DROP_MODULE_WORK_TABLES(p_output_topology,
                                                 'BUILD',
                                                 'N'); --no drop tracking, how else we write?


         EXCEPTION
         WHEN OTHERS
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                   'Error dropping work tables: ' || SQLERRM);
         END;

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                'NOT Dropping work tables ');

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('FINALIZE_AND_VALIDATE: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                  'Complete ' || p_output_topology);

      RETURN output;

   END FINALIZE_AND_VALIDATE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GENERALIZATION_TOPO_BUILD (
      p_schema             IN VARCHAR2,
      p_release            IN VARCHAR2,
      p_project_id         IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_tile_kount         IN NUMBER DEFAULT 1,
      p_sdo_filter         IN SDO_GEOMETRY DEFAULT NULL,
      p_modules            IN VARCHAR2 DEFAULT 'YYYYYYYYYY',
      p_restart_flag       IN VARCHAR2 DEFAULT 'N',
      p_srid               IN NUMBER DEFAULT 8265,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_snapping_digits    IN NUMBER DEFAULT 16,
      p_drop_tables        IN VARCHAR2 DEFAULT 'Y',
      p_validate_topo      IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge           IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge          IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa         IN VARCHAR2 DEFAULT 'Y'    
   ) RETURN VARCHAR2
   AS

      --Matt! 02/06/12
      --Matt! 06/10/13 added topofix_qa option
      --M!    12/30/13 p_validate_topo, p_fix_edge, p_fix_2edge, and p_topofix_qa

      --testing sample

      --declare
      --retval varchar2(4000);
      --begin
      --retval := gz_build_source.generalization_topo_build('GZCPB1','DEC10W5','Z2','TAB10ST10','MT','Z210IN',4);
      --end;


      psql              VARCHAR2(4000);
      retval            VARCHAR2(4000) := '1';  --set to fail, must set to pass in sub modules
      stack             VARCHAR2(4000);
      errm              VARCHAR2(8000) := 'ERROR:'; --default for line 1 in log error message, if no SQLERRM
      topofix_qa        VARCHAR2(1);

   BEGIN

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_TOPO_BUILD: Let the logging begin');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------


      IF p_restart_flag = 'N'
      THEN

         BEGIN

            --We are starting from the beginning for this job
            GZ_BUILD_SOURCE.START_BUILD_LOGGING(p_schema,
                                                p_project_id,
                                                p_output_topology);

         EXCEPTION
         WHEN OTHERS
         THEN
            --send special exception to generic handler at the end
            RAISE_APPLICATION_ERROR(-20002,'Failed before we could even create a log.  Check the basics, like schema name');
         END;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'SET_UP',NULL,
                                                'Inputs are (' || p_schema || ',' || p_release || ','
                                                || p_project_id || ',' || p_source_schema || ','
                                                || p_source_topology || ',' || p_output_topology || ',' || p_tile_kount || ','
                                                || 'see sdo_dump->' || ',' ||  p_modules || ',' || p_restart_flag || ','
                                                || p_srid || ',' || p_tolerance || ',' || p_snapping_digits || ','
                                                || p_drop_tables || ',' || p_validate_topo || ','
                                                || p_fix_edge || ',' || p_fix_2edge || ','
                                                || p_topofix_qa ||  ')',
                                                 NULL,NULL,NULL,NULL,NULL,NULL,p_sdo_filter);


         --make work tables
         --need to give this more thought later

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'SET_UP',NULL,
                                                'Starting table set up for ' || p_output_topology,
                                                 NULL,NULL,NULL,NULL,NULL,NULL );
         GZ_BUILD_SOURCE.SET_UP(p_schema,
                                p_project_id,
                                p_output_topology);

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'SET_UP',NULL,
                                                 'Finished table set up for ' || p_output_topology,
                                                 NULL,NULL,NULL,NULL,NULL,NULL );

      END IF;


      IF p_topofix_qa IS NULL
      OR p_topofix_qa = 'Y'
      THEN

         --default topofix qa is YES, should not have invalid geoms at initial build.
         --Try to fix any and fail if we cant
         --Have to do this since setup table is typically null

         topofix_qa := 'Y';

      ELSIF p_topofix_qa = 'N'
      THEN

         topofix_qa := 'N';

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Unknown p_topofix_qa value of ' || p_topofix_qa);

      END IF;

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_BUILD_SOURCE: Verify and collect build inputs');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      retval := GZ_BUILD_SOURCE.VERIFY_BUILD_INPUTS(p_schema,
                                                    p_release,
                                                    p_project_id,
                                                    p_source_schema,
                                                    p_source_topology,
                                                    p_output_topology,
                                                    p_sdo_filter);

      IF retval != '0'
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'VERIFY_BUILD_INPUTS',NULL,
                                                'UNRECOVERABLE ERROR: Problem in verify build_inputs: ' || retval,
                                                 NULL,NULL,NULL,NULL,NULL,substr(retval , 1, 4000) );

         --kick it down to the generic error handler
         RAISE_APPLICATION_ERROR(-20001,'Problem with build inputs: ' || retval);

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_BUILD_SOURCE: Create and assess layer info table ');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --When hitting java.lang.outofmemory there can be scenarios where a topomap leaves a topology in a
      --broken, unrecoverable state if the memory failure occurs on the commit
      --Ive decided to be a hog and always call this for the session
      --Raising the limit allows the session to use more pga memory as needed
      --If theres a leak in the java code leading to out of process memory then its in the oracle code
      --Im not gonna worry about it
      SDO_TOPO_MAP.SET_MAX_MEMORY_SIZE(999999999999999999);

      IF substr(p_modules,1,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_BUILD_SOURCE.CREATE_LAYER_INFO(p_schema,
                                                        p_release,
                                                        p_project_id,
                                                        p_source_schema,
                                                        p_source_topology,
                                                        p_output_topology,
                                                        p_sdo_filter);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -01013
            THEN

               --ORA-01013: user requested cancel of current operation
               RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');

            ELSE

               errm := SQLERRM;
               stack := DBMS_UTILITY.format_error_backtrace;

            END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_LAYER_INFO',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_LAYER_INFO',NULL,
                                                   'Complete for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;


      IF substr(p_modules,2,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_BUILD_SOURCE.CREATE_BUILD_POLY(p_schema,
                                                        p_project_id,
                                                        p_source_schema,
                                                        p_source_topology,
                                                        p_output_topology,
                                                        p_sdo_filter);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -01013
            THEN

               --ORA-01013: user requested cancel of current operation
               RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');

            ELSE

               errm := SQLERRM;
               stack := DBMS_UTILITY.format_error_backtrace;

            END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_BUILD_POLY',NULL,
                                                   'Complete for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;

      IF substr(p_modules,3,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_BUILD_SOURCE.CREATE_EMPTY_TOPOLOGY(p_schema,
                                                            p_project_id,
                                                            p_output_topology,
                                                            p_srid,
                                                            p_tolerance,
                                                            p_snapping_digits);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -01013
            THEN

               --ORA-01013: user requested cancel of current operation
               RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');

            ELSE

               errm := SQLERRM;
               stack := DBMS_UTILITY.format_error_backtrace;

            END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_EMPTY_TOPOLOGY',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_EMPTY_TOPOLOGY',NULL,
                                                   'Complete for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;


      IF substr(p_modules,4,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_BUILD_SOURCE.LOAD_OUTPUT_TOPOLOGY(p_schema,
                                                           p_project_id,
                                                           p_source_schema,
                                                           p_source_topology,
                                                           p_output_topology,
                                                           p_tile_kount,
                                                           p_srid,
                                                           p_tolerance,
                                                           p_restart_flag);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -01013
            THEN

               --ORA-01013: user requested cancel of current operation
               RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');

            ELSE

               errm := SQLERRM;
               stack := DBMS_UTILITY.format_error_backtrace;

            END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'LOAD_OUTPUT_TOPOLOGY',NULL,
                                                   'Complete for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;



      IF substr(p_modules,5,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_BUILD_SOURCE.CLEAN_OUTPUT_TOPOLOGY(p_schema,
                                                            p_project_id,
                                                            p_output_topology,
                                                            p_tile_kount,
                                                            p_srid,
                                                            p_tolerance);

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -01013
            THEN

               --ORA-01013: user requested cancel of current operation
               RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');

            ELSE

               errm := SQLERRM;
               stack := DBMS_UTILITY.format_error_backtrace;

            END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CLEAN_OUTPUT_TOPOLOGY',NULL,
                                                   'Complete for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;


      IF substr(p_modules,6,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_BUILD_SOURCE.CREATE_FACE_TABLE(p_schema,
                                                        p_release,
                                                        p_project_id,
                                                        p_output_topology);


         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -01013
            THEN

               --ORA-01013: user requested cancel of current operation
               RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');

            ELSE

               errm := SQLERRM;
               stack := DBMS_UTILITY.format_error_backtrace;

            END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_FACE_TABLE',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CREATE_FACE_TABLE',NULL,
                                                   'Complete for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;



      IF substr(p_modules,7,1) = 'Y'
      THEN

         --load face table topogeom

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_BUILD_SOURCE.CONSTRUCT_FACE_TABLE(p_schema,
                                                           p_project_id,
                                                           p_output_topology);


         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -01013
            THEN

               --ORA-01013: user requested cancel of current operation
               RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');

            ELSE

               errm := SQLERRM;
               stack := DBMS_UTILITY.format_error_backtrace;

            END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CONSTRUCT_FACE_TABLE',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'CONSTRUCT_FACE_TABLE',NULL,
                                                   'Complete for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;


      IF substr(p_modules,8,1) = 'Y'
      THEN

         --update layers

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_BUILD_SOURCE.UPDATE_FACE_LAYERS(p_schema,
                                                         p_source_schema,
                                                         p_source_topology,
                                                         p_output_topology);


         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -01013
            THEN

               --ORA-01013: user requested cancel of current operation
               RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');

            ELSE

               errm := SQLERRM;
               stack := DBMS_UTILITY.format_error_backtrace;

            END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'UPDATE_FACE_LAYERS',NULL,
                                                   'Complete for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;


      IF substr(p_modules,9,1) = 'Y'
      THEN

         --update layers

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_BUILD_SOURCE.POPULATE_MEASUREMENTS(p_schema,
                                                            p_release,
                                                            p_project_id,
                                                            p_output_topology,
                                                            p_tolerance,
                                                            p_srid,
                                                            topofix_qa,
                                                            p_fix_edge,
                                                            p_fix_2edge);


         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -01013
            THEN

               --ORA-01013: user requested cancel of current operation
               RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');

            ELSE

               errm := SQLERRM;
               stack := DBMS_UTILITY.format_error_backtrace;

            END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'Complete for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;


      IF substr(p_modules,10,1) = 'Y'
      THEN

         --Validate and tidy

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_BUILD_SOURCE.FINALIZE_AND_VALIDATE(p_schema,
                                                            p_project_id,
                                                            p_output_topology,
                                                            p_tile_kount,
                                                            p_drop_tables,
                                                            p_validate_topo);


         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLCODE = -01013
            THEN

               --ORA-01013: user requested cancel of current operation
               RAISE_APPLICATION_ERROR(-20001,'You wanted to kill this thing, right?');

            ELSE

               errm := SQLERRM;
               stack := DBMS_UTILITY.format_error_backtrace;

            END IF;

         END;


         IF retval != '0'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || retval || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'FINALIZE_AND_VALIDATE',NULL,
                                                   'Complete for ' || p_output_topology,
                                                   NULL,NULL,NULL,NULL,NULL,NULL );

         END IF;

      END IF;


      ---------------------------------------------------------------
      ---------------------------------------------------------------

      IF retval = '0'
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'GENERALIZATION_TOPO_BUILD',NULL,
                                                'WOOT: Returning success code 0');

         RETURN retval;

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'GENERALIZATION_TOPO_BUILD',NULL,
                                                'ANTIwoot: Returning failure code 1');

         RETURN '1';

      END IF;

      ---------------------------------------------------------------
      ---------------------------------------------------------------



      --This is the general unhandled exception area
      --usually a totally flubbed up input near the very beginning of the job

      RETURN '0';

   EXCEPTION
   WHEN OTHERS
   THEN

      IF SQLCODE = -20002  --my made up code
      THEN

         errm := SQLERRM || DBMS_UTILITY.format_error_backtrace;
         RAISE_APPLICATION_ERROR(-20001, 'Doh! Unhandled exception.  We dont even have a log to write to. ' || errm);

      ELSE

         -- Word up to "good practice"

         errm := SQLERRM || DBMS_UTILITY.format_error_backtrace;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('BUILD',p_output_topology,'EXCEPTION HANDLER',NULL,
                                                'UNRECOVERABLE ERROR: Build source caught this exception and has no clue ',
                                                 NULL,NULL,NULL,NULL,NULL,substr(errm, 1, 4000) );

         RETURN '1';


      END IF;

   END GENERALIZATION_TOPO_BUILD;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GZ_GET_FACES (
      p_owner           IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_table           IN VARCHAR2,
      p_oid             IN NUMBER,
      p_oid_col         IN VARCHAR2 DEFAULT 'OID',
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt!  2/07/12
      --Modified from my GZ_UTILITIES.GZ_GET_FSL_FACES
      --Only difference is that this version reaches out to an external schema (can also be internal)

      --DECLARE
      -- face_ids gz_types.stringarray;
      --BEGIN
      -- face_ids := GZ_BUILD_SOURCE.GZ_GET_FACES('TAB10ST10','MT','COUNTY',27590404589427);
      --END;

      psql              VARCHAR2(4000);
      topology          VARCHAR2(4000);
      tg_layer_level    NUMBER;
      output            GZ_TYPES.stringarray;


   BEGIN

      IF NOT (GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_owner || '.' || p_table))
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Partner, table ' || p_owner || '.' || p_table || ' doesnt exist ');

      END IF;

      psql := 'SELECT a.topology, a.tg_layer_level '
           || 'FROM '
           || 'all_sdo_topo_info a '
           || 'WHERE '
           || 'a.table_name = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.owner = :p3 AND '
           || 'a.tg_layer_type = :p4 AND '
           || 'a.column_name = :p5 ';

      EXECUTE IMMEDIATE psql INTO topology,
                                  tg_layer_level USING UPPER(p_table),
                                                       UPPER(p_topology),
                                                       UPPER(p_owner),
                                                       'POLYGON',
                                                       UPPER(p_topo_col);

      CASE tg_layer_level

         WHEN 0 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         WHEN 1 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         WHEN 2 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         WHEN 3 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         WHEN 4 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4, '
                 || p_owner || '.' || p_topology || '_relation$ r5, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = r5.tg_layer_id AND '
                 || 'r4.topo_type = r5.tg_id AND '
                 || 'r5.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         WHEN 5 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4, '
                 || p_owner || '.' || p_topology || '_relation$ r5, '
                 || p_owner || '.' || p_topology || '_relation$ r6, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = r5.tg_layer_id AND '
                 || 'r4.topo_type = r5.tg_id AND '
                 || 'r5.topo_id = r6.tg_layer_id AND '
                 || 'r5.topo_type = r6.tg_id AND '
                 || 'r6.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Hierarchy level ' || tg_layer_level || ' is yet to be implemented!');

      END CASE;

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING p_oid;


      --is output of nothing OK?  Lets say no for now

      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Didnt find any faces for ' || p_table || ' ' || p_oid_col || ' ' || p_oid);

      END IF;

      RETURN output;


   END GZ_GET_FACES;

  -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GZ_GET_FACES_SQL (
      p_owner           IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_table           IN VARCHAR2,
      p_oid             IN NUMBER,
      p_oid_col         IN VARCHAR2 DEFAULT 'OID',
      p_topo_col        IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN VARCHAR2
   AS

      psql              VARCHAR2(4000);
      topology          VARCHAR2(4000);
      tg_layer_level    NUMBER;

   BEGIN

      IF NOT (GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_owner || '.' || p_table))
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Partner, table ' || p_owner || '.' || p_table || ' doesnt exist ');

      END IF;

      psql := 'SELECT a.topology, a.tg_layer_level '
           || 'FROM '
           || 'all_sdo_topo_info a '
           || 'WHERE '
           || 'a.table_name = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.owner = :p3 AND '
           || 'a.tg_layer_type = :p4 AND '
           || 'a.column_name = :p5 ';

      EXECUTE IMMEDIATE psql INTO topology,
                                  tg_layer_level USING UPPER(p_table),
                                                       UPPER(p_topology),
                                                       UPPER(p_owner),
                                                       'POLYGON',
                                                       UPPER(p_topo_col);

      CASE tg_layer_level

         WHEN 0 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         WHEN 1 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         WHEN 2 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         WHEN 3 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         WHEN 4 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4, '
                 || p_owner || '.' || p_topology || '_relation$ r5, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = r5.tg_layer_id AND '
                 || 'r4.topo_type = r5.tg_id AND '
                 || 'r5.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         WHEN 5 THEN

            psql := 'SELECT f.face_id '
                 || 'FROM '
                 || p_owner || '.' || p_table || ' a, '
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4, '
                 || p_owner || '.' || p_topology || '_relation$ r5, '
                 || p_owner || '.' || p_topology || '_relation$ r6, '
                 || p_owner || '.' || p_topology || '_face$ f '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = r5.tg_layer_id AND '
                 || 'r4.topo_type = r5.tg_id AND '
                 || 'r5.topo_id = r6.tg_layer_id AND '
                 || 'r5.topo_type = r6.tg_id AND '
                 || 'r6.topo_id = f.face_id AND '
                 || 'a.' || p_oid_col || ' = :p1 ';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Hierarchy level ' || tg_layer_level || ' is yet to be implemented!');

      END CASE;

      RETURN psql;

   END GZ_GET_FACES_SQL;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

    FUNCTION GET_TG_LAYER_LEVEL (
      p_owner                       IN VARCHAR2,
      p_topology                    IN VARCHAR2,
      p_table_name                  IN VARCHAR2,
      p_column_name                 IN VARCHAR2,
      p_tg_layer_type               IN VARCHAR2 --point, line, polygon
   ) RETURN NUMBER
   AS


      psql           VARCHAR2(4000);
      tg_layer_id    NUMBER;

   BEGIN

      IF UPPER(p_tg_layer_type) NOT IN ('POINT','LINE','POLYGON')
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'What the heck is a tg_layer_type of ' || p_tg_layer_type);

      END IF;

      psql := 'SELECT a.tg_layer_level FROM '
           || 'all_sdo_topo_metadata a '
           || 'WHERE '
           || 'a.owner = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.table_name = :p3 AND '
           || 'a.column_name = :p4 AND '
           || 'a.tg_layer_type = :p5 ';

      BEGIN
         EXECUTE IMMEDIATE psql INTO tg_layer_id USING UPPER(p_owner),
                                                       UPPER(p_topology),
                                                       UPPER(p_table_name),
                                                       UPPER(p_column_name),
                                                       UPPER(p_tg_layer_type);


      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR(-20001,'Error! No matching records found in user_sdo_topo_metadata');
         WHEN TOO_MANY_ROWS THEN
            RAISE_APPLICATION_ERROR(-20001,'Error! Found more than one record in user_sdo_topo_metadata');
         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Table user_sdo_topo_metadata does not exist! How wack is that?');
            ELSE
               RAISE;
            END IF;
      END;

      RETURN tg_layer_id;

   END GET_TG_LAYER_LEVEL;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION GET_FACE_TILES (
      p_topo               IN VARCHAR2
   ) RETURN GZ_TYPES.STRINGARRAY
   AS

      --Matt! 3/1/12

      psql              VARCHAR2(4000);
      tilez             GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT DISTINCT a.tile_number '
           || 'FROM '
           || p_topo || '_face a '
           || 'ORDER BY a.tile_number ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO tilez;

      IF tilez.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Donde estan los tiles?');

      END IF;

      RETURN tilez;

   END;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION GET_BDYEDGES_SQL (
      p_schema            IN VARCHAR2,
      p_feature_tab       IN VARCHAR2,
      p_topo              IN VARCHAR2,
      p_oid_col           IN VARCHAR2 DEFAULT 'OID'
   ) RETURN VARCHAR2
   AS

      --Matt! 2/8/12
      --Stole and modified the logic in here from CTS.GETBESQL
      --Thanks whoever you are!
      --select gz_build_source.GET_BDYEDGES_SQL('TAB10ST10','COUNTY','MT') from dual

        psql            VARCHAR2(4000);
        where_sql       VARCHAR2(1000);
        rel_table       VARCHAR2(32) := p_topo || '_relation$';
        edge_table      VARCHAR2(32) := p_topo || '_edge$';
        p_layer_level   NUMBER;

    BEGIN

       p_layer_level := gz_build_source.GET_TG_LAYER_LEVEL(p_schema,
                                                           p_topo,
                                                           p_feature_tab,
                                                           'TOPOGEOM',
                                                           'POLYGON');

        /*  SAMPLE level1 county
              SELECT e.edge_id
                FROM TAB10ST10.COUNTY a,
                     TAB10ST10.MT_edge$ e,
                     TAB10ST10.MT_relation$ r1,
                     TAB10ST10.MT_relation$ r2
               WHERE     a.topogeom.tg_id = r1.tg_id
                     AND a.topogeom.tg_layer_id = r1.tg_layer_id
                     AND r2.tg_layer_id = r1.topo_id
                     AND r2.tg_id = r1.topo_type
                     AND e.left_face_id != e.right_face_id
                     AND (e.left_face_id = r2.topo_id OR e.right_face_id = r2.topo_id)
                     AND a.OID = :p1
            GROUP BY r1.tg_id, e.edge_id
              HAVING COUNT (*) = 1
       */

        psql := 'SELECT e.edge_id '
             || 'FROM '
             || p_schema || '.' || p_feature_tab || ' a, '
             || p_schema || '.' || edge_table || ' e, '
             || p_schema || '.' || rel_table || ' r1 ';

        where_sql := 'WHERE '
                  || 'a.topogeom.tg_id = r1.tg_id AND '
                  || 'a.topogeom.tg_layer_id = r1.tg_layer_id ';

        FOR i IN 0 .. (p_layer_level - 1)
        LOOP

            psql := psql || ', ' || p_schema || '.' || rel_table || ' r' || (i+2) || ' ';

            where_sql := where_sql || ' '
                                   || 'AND r' || (i+2) || '.tg_layer_id = r' || (i+1) || '.topo_id '
                                   || 'AND r' || (i+2) || '.tg_id = r' || (i+1) || '.topo_type ';

        END LOOP;

        where_sql := where_sql || ' '
                               || 'AND e.left_face_id != e.right_face_id '                         --avoid dangling edges
                               || 'AND (e.left_face_id  = r' || (p_layer_level + 1) || '.topo_id ' --L_ or R_ correspond to lowest topoid in rel$
                               || 'OR e.right_face_id = r' || (p_layer_level + 1) || '.topo_id) '
                               || 'AND a.' || p_oid_col || ' = :p1 ';

        psql := psql || where_sql || ' '
                     || 'GROUP BY r1.tg_id,e.edge_id HAVING COUNT(*) = 1 ';  --group by highest in rel$ + edge_id

        RETURN psql;

    END GET_BDYEDGES_SQL;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION GZ_GET_BDYEDGES (
      p_owner           IN VARCHAR2,
      p_topology        IN VARCHAR2,
      p_table           IN VARCHAR2,
      p_oid             IN NUMBER,
      p_oid_col         IN VARCHAR2 DEFAULT 'OID'
   ) RETURN MDSYS.SDO_LIST_TYPE
   AS

      --Matt! 2/8/12
      --Final optimized version
      --really just a wrapper to get_bdyedges_sql
      --edges := gz_build_source.GZ_GET_BDYEDGES('TAB10ST10','MT','COUNTY',27590404589427);

      psql              VARCHAR2(4000);
      subquery          VARCHAR2(4000);
      --output            GZ_TYPES.stringarray;
      output            MDSYS.SDO_LIST_TYPE;

   BEGIN


      IF NOT (GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_owner || '.' || p_table))
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Partner, table ' || p_owner || '.' || p_table || ' doesnt exist ');

      END IF;

      psql := GZ_BUILD_SOURCE.GET_BDYEDGES_SQL(p_owner,p_table,p_topology,p_oid_col);

      --dbms_output.put_line(psql);

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING p_oid;

      --error?
      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Couldnt find any edges for ' || p_table || ' ' || p_oid_col || ' ' || p_oid);

      END IF;

      RETURN output;

   END GZ_GET_BDYEDGES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION GZ_GET_OID_FROM_FACE (
      p_owner              IN VARCHAR2,
      p_topology           IN VARCHAR2,
      p_table              IN VARCHAR2,
      p_tg_layer_level     IN NUMBER,
      p_face_id            IN NUMBER,
      p_whereclause        IN VARCHAR2,
      p_oid_col            IN VARCHAR2 DEFAULT 'OID'
   ) RETURN VARCHAR2
   AS

      --Matt! 2/21/11
      --I dont like this
      --3/9/12 switch to varchar2 return in case of gz GEOID input
      --4/13/12 Suggestion from WWU to remove redundant face$ join

      psql              VARCHAR2(4000);
      output            VARCHAR2(4000);

   BEGIN


      psql := 'SELECT a.' || p_oid_col || ' '
           || 'FROM '
           || p_owner || '.' || p_table || ' a, ';


      CASE p_tg_layer_level

         WHEN 0 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = :p1 ';

         WHEN 1 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = :p1 ';

         WHEN 2 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = :p1 ';

         WHEN 3 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = :p1 ';

         WHEN 4 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4, '
                 || p_owner || '.' || p_topology || '_relation$ r5 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = r5.tg_layer_id AND '
                 || 'r4.topo_type = r5.tg_id AND '
                 || 'r5.topo_id = :p1 ';

         WHEN 5 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4, '
                 || p_owner || '.' || p_topology || '_relation$ r5, '
                 || p_owner || '.' || p_topology || '_relation$ r6 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = r5.tg_layer_id AND '
                 || 'r4.topo_type = r5.tg_id AND '
                 || 'r5.topo_id = r6.tg_layer_id AND '
                 || 'r5.topo_type = r6.tg_id AND '
                 || 'r6.topo_id = :p1 ';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Hierarchy level ' || p_tg_layer_level || ' is yet to be implemented!');

      END CASE;

      psql := psql || 'AND ' || p_whereclause || ' ';

      --dbms_output.put_line(psql);

      EXECUTE IMMEDIATE psql INTO output USING p_face_id;

      RETURN output;


   END GZ_GET_OID_FROM_FACE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION SDO_GET_OID_FROM_FACE (
      p_owner              IN VARCHAR2,
      p_topology           IN VARCHAR2,
      p_table              IN VARCHAR2,
      p_tg_layer_level     IN NUMBER,
      p_face_id            IN NUMBER,
      p_whereclause        IN VARCHAR2,
      p_oid_col            IN VARCHAR2 DEFAULT 'OID',
      p_sdo                IN SDO_GEOMETRY
   ) RETURN VARCHAR2
   AS

      --Matt! 7/16/11
      --When joining against large tables in the original gz_get_oid_from_face
      --   the results were slow.  The relation$ joins were good, but then
      --   each call resulted in a table access storage full to all of blkgrp (for ex)
      --   with no index, even if one was nominally available
      --By adding the edge geometry driving the call into this function it forces the plan
      --to use the spatial index on the big bench table and then just
      --topogeom.xx_id join against a few records

      psql              VARCHAR2(4000);
      output            VARCHAR2(4000);

   BEGIN


      psql := 'SELECT a.' || p_oid_col || ' '
           || 'FROM '
           || p_owner || '.' || p_table || ' a, ';


      CASE p_tg_layer_level

         WHEN 0 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = :p1 ';

         WHEN 1 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = :p1 ';

         WHEN 2 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = :p1 ';

         WHEN 3 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = :p1 ';

         WHEN 4 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4, '
                 || p_owner || '.' || p_topology || '_relation$ r5 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = r5.tg_layer_id AND '
                 || 'r4.topo_type = r5.tg_id AND '
                 || 'r5.topo_id = :p1 ';

         WHEN 5 THEN

            psql := psql
                 || p_owner || '.' || p_topology || '_relation$ r1, '
                 || p_owner || '.' || p_topology || '_relation$ r2, '
                 || p_owner || '.' || p_topology || '_relation$ r3, '
                 || p_owner || '.' || p_topology || '_relation$ r4, '
                 || p_owner || '.' || p_topology || '_relation$ r5, '
                 || p_owner || '.' || p_topology || '_relation$ r6 '
                 || 'WHERE '
                 || 'a.topogeom.tg_id = r1.tg_id AND '
                 || 'a.topogeom.tg_layer_id = r1.tg_layer_id AND '
                 || 'r1.topo_id = r2.tg_layer_id AND '
                 || 'r1.topo_type = r2.tg_id AND '
                 || 'r2.topo_id = r3.tg_layer_id AND '
                 || 'r2.topo_type = r3.tg_id AND '
                 || 'r3.topo_id = r4.tg_layer_id AND '
                 || 'r3.topo_type = r4.tg_id AND '
                 || 'r4.topo_id = r5.tg_layer_id AND '
                 || 'r4.topo_type = r5.tg_id AND '
                 || 'r5.topo_id = r6.tg_layer_id AND '
                 || 'r5.topo_type = r6.tg_id AND '
                 || 'r6.topo_id = :p1 ';

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Hierarchy level ' || p_tg_layer_level || ' is yet to be implemented!');

      END CASE;

      psql := psql || 'AND (' || p_whereclause || ') '
                   || 'AND SDO_FILTER(a.sdogeometry,:p2) = :p3 ';

      --dbms_output.put_line(psql);

      EXECUTE IMMEDIATE psql INTO output USING p_face_id,
                                               p_sdo,
                                               'TRUE';

      RETURN output;


   END SDO_GET_OID_FROM_FACE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION TIDY_TILES (
      p_tiles                IN GZ_TYPES.geomarray
   ) RETURN GZ_TYPES.geomarray
   AS

      --9/28/12 Moved all tiling code to GZ_UTILITIES, dont call or modify this

      output            GZ_TYPES.geomarray;


   BEGIN


      FOR i IN 1 .. p_tiles.COUNT
      LOOP

         --Cant cross more than 119 degrees longitude
         --or have an area larger than or equal to one-half the surface of the Earth

         output(i) := GZ_BUILD_SOURCE.GEODETIC_MBR_CONSIDERATIONS(p_tiles(i));

      END LOOP;

      FOR i IN 1 .. output.COUNT
      LOOP

         output(i) := GZ_BUILD_SOURCE.SHIFT_AT_DATELINE(output(i));

      END LOOP;

      --anything else?


      RETURN output;

   END TIDY_TILES;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private-------------------------------------------------------------------------

   FUNCTION SUBDIVIDE_TILE (
      p_geom               IN SDO_GEOMETRY,
      p_subdivisions       IN NUMBER
   ) RETURN GZ_TYPES.geomarray
   AS

      --Matt! 2/10/12
      --9/28/12 Moved all tiling code to GZ_UTILITIES, dont call or modify this

      output            GZ_TYPES.geomarray;
      tile_size         NUMBER;
      llx               NUMBER;
      lly               NUMBER;
      urx               NUMBER;
      ury               NUMBER;
      current_window    SDO_GEOMETRY;
      deadman           PLS_INTEGER := 0;

   BEGIN

      IF p_geom.SDO_ORDINATES.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, gots ta be an optimized rectangle');

      END IF;

      IF  ROUND(ABS(p_geom.sdo_ordinates(1) - p_geom.sdo_ordinates(3)),6)
       != ROUND(ABS(p_geom.sdo_ordinates(2) - p_geom.sdo_ordinates(4)),6)
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Danger danger, I dont think your tile is squarish');

      END IF;

      tile_size := ABS(p_geom.sdo_ordinates(1) - p_geom.sdo_ordinates(3))/p_subdivisions;

      current_window := p_geom;

      --start at lower left
      llx := p_geom.sdo_ordinates(1);
      lly := p_geom.sdo_ordinates(2);
      urx := p_geom.sdo_ordinates(1);
      ury := p_geom.sdo_ordinates(2);

      LOOP

         --go up one grid in y dimension
         lly := ury;
         ury := ury + tile_size;

         LOOP

            --travel along the X dimension ---->
            llx := urx;
            urx := urx + tile_size;

            --we now have a tile position
            current_window.sdo_ordinates(1) := llx;
            current_window.sdo_ordinates(2) := lly;
            current_window.sdo_ordinates(3) := urx;
            current_window.sdo_ordinates(4) := ury;

            output(output.COUNT + 1) := current_window;


            --always go past the X end before exiting
            IF urx >= p_geom.sdo_ordinates(3)
            THEN

              --exit inner X loop
              EXIT;

            END IF;

            --check for deadman here, inner loop

            IF deadman < 10000
            THEN

               deadman := deadman + 1;

            ELSE

               RAISE_APPLICATION_ERROR(-20001,'Deadman switch');

            END IF;

         END LOOP;

         --carriage return the X
         llx := p_geom.sdo_ordinates(1);
         urx := p_geom.sdo_ordinates(1);


         IF ury >= p_geom.sdo_ordinates(4)
         THEN

            --exit the top right hopefully
            EXIT;

         END IF;

      END LOOP;

      IF output.COUNT = 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Oopsie');

      END IF;

      RETURN output;

   END SUBDIVIDE_TILE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION GET_GEODETIC_MBR_WIDTH (
     p_mbr              IN SDO_GEOMETRY,
     p_tolerance        IN NUMBER DEFAULT .05
   ) RETURN NUMBER DETERMINISTIC
   AS

      --Matt! 5/2/12
      --9/28/12 Moved all tiling code to GZ_UTILITIES, dont call or modify this

      --Southern side this is just a rough estimate


      output            NUMBER;
      pt1               SDO_GEOMETRY;
      pt2               SDO_GEOMETRY;

   BEGIN

      IF p_mbr.sdo_ordinates.COUNT <> 4
      OR p_mbr.sdo_srid <> 8265
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Well this is embarassing');

      END IF;

      pt1 := SDO_GEOMETRY(2001,
                          p_mbr.sdo_srid,
                          SDO_POINT_TYPE(p_mbr.sdo_ordinates(1), p_mbr.sdo_ordinates(2), NULL),
                          NULL,
                          NULL);

      pt2 := SDO_GEOMETRY(2001,
                          p_mbr.sdo_srid,
                          SDO_POINT_TYPE(p_mbr.sdo_ordinates(3), p_mbr.sdo_ordinates(2), NULL),
                          NULL,
                          NULL);

      output := SDO_GEOM.SDO_DISTANCE(pt1,pt2,p_tolerance);

      IF output < 0
      THEN
         RAISE_APPLICATION_ERROR(-20001,'Cheezburgers, width is ' || output);
      END IF;

      output := output/111000; --very rough, at equator

      --return in degrees longitude at the equator
      RETURN output;


   END GET_GEODETIC_MBR_WIDTH;


   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION GZ_TILE_TABLE (
      p_table           IN VARCHAR2,                  --can be schema.table
      p_tile_target     IN NUMBER,
      p_geom_col        IN VARCHAR2 DEFAULT 'GEOMETRY',
      p_whereclause     IN VARCHAR2 DEFAULT NULL,     --use a.xx syntax
      p_sdo_filter      IN SDO_GEOMETRY DEFAULT NULL,
      p_log_type        IN VARCHAR2 DEFAULT NULL, --BUILD for me here
      p_log_tag         IN VARCHAR2 DEFAULT NULL, --ex topology
      p_debug           IN NUMBER DEFAULT NULL
   ) RETURN GZ_TYPES.geomarray
   AS

      --Matt! 2/10/12
      --9/28/12 Moved all tiling code to GZ_UTILITIES
      --This remains only for backwards compatibility

   BEGIN

      RETURN GZ_GEOM_UTILS.GZ_TILE_TABLE(p_table,
                                        p_tile_target,
                                        p_geom_col,
                                        p_whereclause,
                                        p_sdo_filter,
                                        p_log_type,
                                        p_log_tag,
                                        p_debug);


   END GZ_TILE_TABLE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION ISOLATED_NODES_EXIST (
      p_topo               IN VARCHAR2
   ) RETURN BOOLEAN
   AS

      --Matt! 2/16/12

      --so which is it? Isolated nodes have edge_id of 0 or NULL
      --we always seem to get 0 in GZ
      --benchmark and oracle documentation show NULL

      --The documentation is clearly not true before feature tables are registered in the toplogy--
      --For each node, the EDGE_ID or FACE_ID value (but not both) must be null:
      --If the EDGE_ID value is null, the node is an isolated node (that is, isolated in a face).
      --If the FACE_ID value is null, the node is not an isolated node, but rather the start node or end node of an edge.


      psql                 VARCHAR2(4000);
      kount                PLS_INTEGER;

   BEGIN

      --this is my best check at the moment.  Time will tell...

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_topo || '_node$ n '
           || 'WHERE '
           || '(n.edge_id = :p1 OR n.edge_id IS NULL) OR '
           || '(n.face_id IS NOT NULL AND face_id != :p2) ';  --redundant, I know I know

      EXECUTE IMMEDIATE psql INTO kount USING 0, 0;

      IF kount > 0
      THEN

         RETURN TRUE;

      ELSE

         RETURN FALSE;

      END IF;

   END ISOLATED_NODES_EXIST;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION DANGLE_EDGES_EXIST (
      p_topo               IN VARCHAR2
   ) RETURN BOOLEAN
   AS

      --Matt! 2/16/12


      psql                 VARCHAR2(4000);
      kount                PLS_INTEGER;

   BEGIN

      --this right?

      psql := 'SELECT COUNT(*) FROM ( '
           || 'SELECT e.start_node_id node_id FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.start_node_id != e.end_node_id '
           || 'UNION ALL '
           || 'SELECT e.end_node_id node_id FROM '
           || p_topo || '_edge$ e '
           || 'WHERE e.start_node_id != end_node_id '
           || ') '
           || 'GROUP BY node_id HAVING COUNT(node_id) = :p1 ';


      BEGIN

         EXECUTE IMMEDIATE psql INTO kount USING 1;

      EXCEPTION
      WHEN NO_DATA_FOUND
      THEN

         RETURN FALSE;

      WHEN OTHERS
      THEN

         RAISE;

      END;

      IF kount > 0
      THEN

         RETURN TRUE;

      ELSE

         RETURN FALSE;

      END IF;

   END DANGLE_EDGES_EXIST;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public-------------------------------------------------------------------------

   FUNCTION SHIFT_AT_DATELINE (
      p_tile                IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 3/5/12 Shift any tiles at the dateline
      --9/28/12 Moved all tiling code to GZ_UTILITIES, dont call or modify this

      --to whatever magic style works best
      --This is a mess

      output         SDO_GEOMETRY;

   BEGIN

      IF p_tile.sdo_srid != 8265
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, srid is ' || p_tile.sdo_srid);

      END IF;

      IF p_tile.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Well this is embarassing.');

      END IF;

      output := p_tile;

      IF (p_tile.sdo_ordinates(1) < 0 AND
          p_tile.sdo_ordinates(3) < 0 AND
          p_tile.sdo_ordinates(1) <= -180 AND
          p_tile.sdo_ordinates(3) >= -180)
      THEN

         -- -182  -178   <---Earth---
         --anything?
         RETURN output;

      ELSIF (p_tile.sdo_ordinates(1) > 0 AND
             p_tile.sdo_ordinates(3) > 0 AND
             p_tile.sdo_ordinates(1) <= 180 AND
             p_tile.sdo_ordinates(3) >= 180)
      THEN

         -- 178   182   ---Earth-->
         --anything?
         RETURN output;

      ELSIF (p_tile.sdo_ordinates(1) > 0 AND
             p_tile.sdo_ordinates(3) < 0 AND
             p_tile.sdo_ordinates(1) <= 180 AND
             p_tile.sdo_ordinates(3) >= -180)
      THEN

         -- 177   -178    --->180<---
         --make it entirely negative
         output.sdo_ordinates(1) := -180 - (180 - p_tile.sdo_ordinates(1));

         RETURN output;

      ELSE

         --doesnt cross or concern us

         RETURN output;

      END IF;


   END SHIFT_AT_DATELINE;

   --------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-
   --Public------------------------------------------------------------------------

   FUNCTION SPLIT_AT_DATELINE (
      p_tile                IN SDO_GEOMETRY,
      p_hemisphere          IN VARCHAR2 DEFAULT 'WESTERN'
   ) RETURN SDO_GEOMETRY DETERMINISTIC
   AS

      --Matt! 3/15/12

      --For an optimized rectangle return just the part in the hemisphere requested
      --Change signs of longitude ordinates to match standard globe

      output         SDO_GEOMETRY;

   BEGIN

      IF UPPER(p_hemisphere) NOT IN ('WESTERN','EASTERN')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Talk to Henry Kissinger');

      END IF;

      IF p_tile.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry auntie, expecting optimized rectangles');

      END IF;

      output := p_tile;

      IF p_tile.sdo_ordinates(1) > 0
      OR p_tile.sdo_ordinates(3) > 0
      THEN

         IF p_tile.sdo_ordinates(3) < 180
         THEN
            RAISE_APPLICATION_ERROR(-20001,'Why bothering me?');
         END IF;

         --probably one of my tile windows
         --ex 175  185

         IF UPPER(p_hemisphere) = 'WESTERN'
         THEN

            --make all negative
            output.sdo_ordinates(1) := -180;
            --  185 becomes -175
            output.sdo_ordinates(3) := -180 + (p_tile.sdo_ordinates(3) - 180);

         ELSIF UPPER(p_hemisphere) = 'EASTERN'
         THEN

            --make all positive
            --ordinate 1 is already cool at 170something
            output.sdo_ordinates(3) := 180;

         END IF;

      ELSIF p_tile.sdo_ordinates(1) < 0
      AND p_tile.sdo_ordinates(3) < 0
      THEN

         --ex -185    -175

         IF p_tile.sdo_ordinates(1) > -180
         THEN
            RAISE_APPLICATION_ERROR(-20001,'Why bothering me?');
         END IF;

         --probably been through shift_at_dateline
         IF UPPER(p_hemisphere) = 'WESTERN'
         THEN

            --make all negative
            output.sdo_ordinates(1) := -180;
            --ordinate 3 is already cool at -170something

         ELSIF UPPER(p_hemisphere) = 'EASTERN'
         THEN

            --make all positive
            --ex -185 becomes 175
            output.sdo_ordinates(1) := 180 - ABS(p_tile.sdo_ordinates(1) + 180);
            output.sdo_ordinates(3) := 180;

         END IF;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Sorry amigo, we dont use mixed sign rectangles here');

      END IF;

      RETURN output;

   END SPLIT_AT_DATELINE;

   --------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-
   --Public------------------------------------------------------------------------

   FUNCTION GEODETIC_MBR_CONSIDERATIONS (
      p_tile                IN SDO_GEOMETRY
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 5/01/12
      --9/28/12 Moved all tiling code to GZ_UTILITIES, dont call or modify this

      --MBR of US data tends to go from -179 to 179 (or worse) if we have just one tile
      --I'm just gonna hard code the longitude to 140 -60, Guam to US Virgin Islands to make closer
      --Shift_at_dateline will make it all negative, if necessary
      --GZ_TILE_TABLE will split into multiple tiles if necessary

      --1. When an optimized rectangle spans more than 119 degrees in longitude, it is
      --internally divided into three rectangles; and as a result, these three rectangles
      --share an edge that is the common boundary between them. If you validate the geometry
      --of such an optimized rectangle, error code 13351 is returned because the internal
      --rectangles have a shared edge. You can use such an optimized rectangle for queries
      --with only the following: SDO_ANYINTERACT operator, SDO_RELATE operator with the
      --ANYINTERACT mask, or SDO_GEOM.RELATE function with the ANYINTERACT mask.
      --(Any other queries on such an optimized rectangle may return incorrect results.)
      --2. No polygon element can have an area larger than or equal to one-half the surface of the Earth.


      output         SDO_GEOMETRY;

   BEGIN

      IF p_tile.sdo_srid != 8265
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, srid is ' || p_tile.sdo_srid);

      END IF;

      IF p_tile.sdo_ordinates.COUNT != 4
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Well isnt this embarassing?');

      END IF;

      --typically we get
      -- -179.281086,
      -- -14.651813,
      -- 179.909681,
      -- 71.491059

      output := p_tile;

      IF p_tile.sdo_ordinates(1) < -175
      AND p_tile.sdo_ordinates(3) > 0
      THEN

         output.sdo_ordinates(1) := 140;  --144 Guam
         output.sdo_ordinates(3) := -60;  --64 US Virgin

      END IF;


      RETURN output;

   END GEODETIC_MBR_CONSIDERATIONS;

   --------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-
   --Public------------------------------------------------------------------------


   FUNCTION AGGR_MBR_NEAR_DATELINE (
      p_tab                IN VARCHAR2,
      p_pkc_col            IN VARCHAR2,
      p_keys               IN GZ_TYPES.stringarray,
      p_geom_col           IN VARCHAR2 DEFAULT 'GEOMETRY',
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 3/14/12
      --wrapper for GET_MBR_NEAR_DATELINE

      --Oracle MBRs near the dateline will wrap the globe even if the geoms are tiny
      --Hopefully this mess doesnt have to get called too often
      --I dont really want to talk about it any more

      --usage sample
      /*declare

         edges    gz_types.stringarray;
         mbr      sdo_geometry;
         psql     varchar2(4000);

         begin

            psql := 'SELECT sdo_aggr_mbr(e.geometry) from tab10.mt_edge$ e where e.edge_id IN (3300329,3297849,3297867)';
            execute immediate psql into mbr;

            dbms_output.put_line('before');
            dbms_output.put_line(gz_utilities.dump_sdo(mbr));

            IF mbr.sdo_ordinates(1) < -140   --roughly eastern AK to
            AND mbr.sdo_ordinates(3) >= 0    --past London
            THEN

               edges(1) := 3300329;
               edges(2) := 3297849;
               edges(3) := 3297867;

               mbr := GZ_BUILD_SOURCE.AGGR_MBR_NEAR_DATELINE('TAB10.MT_EDGE$','EDGE_ID',edges);

               dbms_output.put_line('after');
               dbms_output.put_line(gz_utilities.dump_sdo(mbr));

            END IF;

         end;*/

      psql                 VARCHAR2(4000);
      curr_geoms           GZ_TYPES.geomarray;
      output               SDO_GEOMETRY;
      my_cursor            SYS_REFCURSOR;

   BEGIN

      psql := 'SELECT ' || p_geom_col || ' '
           || 'FROM ' || p_tab || ' '
           || 'WHERE '
           || p_pkc_col || ' IN (SELECT * FROM TABLE(:p1)) ';

      OPEN my_cursor FOR psql USING GZ_BUSINESS_UTILS.stringarray_to_varray(p_keys);

      LOOP

         FETCH my_cursor BULK COLLECT INTO curr_geoms LIMIT 5;
         EXIT WHEN curr_geoms.COUNT = 0;

         FOR i IN 1 .. curr_geoms.COUNT
         LOOP

            IF output IS NULL
            THEN

                --get first MBR
                output := GZ_BUILD_SOURCE.GET_MBR_NEAR_DATELINE(curr_geoms(i));

            ELSE

               --add current geom to running MBR
               output := GZ_BUILD_SOURCE.GET_MBR_NEAR_DATELINE(curr_geoms(i),
                                                               output,
                                                               p_tolerance);

            END IF;

         END LOOP;

      END LOOP;

      CLOSE my_cursor;

      IF output.sdo_ordinates(1) < -140
      AND output.sdo_ordinates(3) >= 0
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Failsauce');

      END IF;

      RETURN output;

   END AGGR_MBR_NEAR_DATELINE;

   --------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-
   --Public------------------------------------------------------------------------

   FUNCTION GET_MBR_NEAR_DATELINE (
      p_geom               IN SDO_GEOMETRY,
      p_mbr                IN SDO_GEOMETRY DEFAULT NULL,
      p_tolerance          IN NUMBER DEFAULT .05
   ) RETURN SDO_GEOMETRY
   AS

      --Matt! 3/14/12

      --Oracle MBRs near the dateline will wrap the globe even if the geoms are tiny
      --Hopefully this mess doesnt have to get called too often
      --I dont really want to talk about it

      output               SDO_GEOMETRY;
      min_x                NUMBER := 1000; --force to be overwritten
      max_x                NUMBER := -1000;
      current_x            NUMBER;

   BEGIN

      IF p_mbr IS NULL
      THEN

         output := SDO_GEOM.SDO_MBR(p_geom);

      ELSE

         output := SDO_GEOM.SDO_MBR(SDO_GEOM.SDO_UNION(p_geom,p_mbr,p_tolerance));

      END IF;

      --only need to start with the klugefest if we are wrapping
      -- xs: -177,  178

      IF output.sdo_ordinates(1) < -140   --roughly eastern AK
      AND output.sdo_ordinates(3) >= 0    --London
      THEN

         --we got an MBR from the Pacific to the eastern Hemisphere again
         --btw we are cool with the Y coordinate

         FOR i IN 1 .. p_geom.sdo_ordinates.COUNT/2
         LOOP

            current_x := p_geom.sdo_ordinates( (i*2) - 1);

            IF current_x >= 0
            THEN

               --shift a 178 to a -182 for example
               current_x := -180 - (180 - current_x);

            END IF;

            IF current_x < min_x
            THEN

               min_x := current_x;

            END IF;

            IF current_x > max_x
            THEN

               max_x := current_x;

            END IF;

         END LOOP;

         IF p_mbr IS NOT NULL
         THEN

            FOR i IN 1 .. p_mbr.sdo_ordinates.COUNT/2
            LOOP

               current_x := p_mbr.sdo_ordinates( (i*2) - 1);

               IF current_x >= 0
               THEN

                  --shift a 178 to a -182 for example
                  current_x := -180 - (180 - current_x);

               END IF;

               IF current_x < min_x
               THEN

                  min_x := current_x;

               END IF;

               IF current_x > max_x
               THEN

                  max_x := current_x;

               END IF;

            END LOOP;

         END IF;

         output.sdo_ordinates(1) := min_x;
         output.sdo_ordinates(3) := max_x;

         --ouput MBR is good at this line for most purposes
         --but if it crosses dateline ordinate(1) to ordinate(3) positive to negative
         --we need to flip to all negative for topomap calls

         output := GZ_BUILD_SOURCE.SHIFT_AT_DATELINE(output);

         RETURN output;

      ELSE

         output := GZ_BUILD_SOURCE.SHIFT_AT_DATELINE(output);

         RETURN output;

      END IF;

   END GET_MBR_NEAR_DATELINE;

   ---------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ---------------------------------------------------------------------------------

END GZ_BUILD_SOURCE;
/
