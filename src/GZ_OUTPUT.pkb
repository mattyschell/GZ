CREATE OR REPLACE PACKAGE BODY GZ_OUTPUT
AS
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------
   --SEE
   --GENERALIZATION_OUTPUT
   --For entry point to this package
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION VERIFY_OUTPUT_PARMS (
      p_release               IN VARCHAR2,
      p_schema                IN VARCHAR2 DEFAULT NULL  --if checking a remote schema
   ) RETURN VARCHAR2
   AS

      --Matt! 4/26/12
      --return 0 if good
      --or return a list of errors in pipe-delimited fashion

      --sample standalone check
      --declare
      --output varchar2(4000);
      --begin
      --output := GZ_OUTPUT.VERIFY_OUTPUT_PARMS('A12');
      --dbms_output.put_line(output);
      --end;

      output                     VARCHAR2(4000) := '0';
      psql                       VARCHAR2(4000);
      v_schema                   VARCHAR2(64);
      v_gz_layers_out            VARCHAR2(64);
      v_gz_layers_in             VARCHAR2(64);
      v_gz_layers_aggregate      VARCHAR2(64);
      v_gz_layers_crosswalk      VARCHAR2(64);
      v_gz_layers_fields         VARCHAR2(64);
      v_gz_layers_geoid          VARCHAR2(64);
      v_gz_layers_hierarchical   VARCHAR2(64);
      v_gz_layers_split          VARCHAR2(64);
      v_gz_layers_subset         VARCHAR2(64);
      kount                      PLS_INTEGER;
      kount2                     PLS_INTEGER;
      all_tables                 GZ_TYPES.stringarray;
      v_release                  VARCHAR2(64) := UPPER(p_release);
      layer_hash                 GZ_TYPES.stringhash;
      projects                   GZ_TYPES.stringarray;
      layers                     GZ_TYPES.stringarray;
      child_layers               GZ_TYPES.stringarray;
      child_projects             GZ_TYPES.stringarray;
      all_layers                 GZ_TYPES.stringhash;
      layers_out                 GZ_TYPES.gz_layers_out;
      layer_aggr                 GZ_TYPES.GZ_LAYERS_AGGREGATE_REC;
      layer_hier                 GZ_TYPES.GZ_LAYERS_HIERARCHICAL_REC;
      layer_split                GZ_TYPES.GZ_LAYERS_SPLIT_REC;
      deep_split                 GZ_TYPES.GZ_LAYERS_SPLIT_REC;
      layer_subset               GZ_TYPES.GZ_LAYERS_SUBSET_REC;
      fields                     GZ_TYPES.stringarray;
      length_hash                GZ_TYPES.stringhash;


   BEGIN

      IF p_schema IS NULL
      THEN

         --usually here
         v_schema := SYS_CONTEXT('USERENV', 'CURRENT_USER');

      ELSE

         --maybe a call from the release copier on a remote schema?
         v_schema := UPPER(p_schema);

      END IF;


      v_gz_layers_out          := v_schema || '.GZ_LAYERS_OUT';
      all_tables(1)            := v_gz_layers_out;
      v_gz_layers_in           := v_schema || '.GZ_LAYERS_IN';
      all_tables(2)            := v_gz_layers_in;
      v_gz_layers_aggregate    := v_schema || '.GZ_LAYERS_AGGREGATE';
      all_tables(3)            := v_gz_layers_aggregate;
      v_gz_layers_crosswalk    := v_schema || '.GZ_LAYERS_CROSSWALK';
      all_tables(4)            := v_gz_layers_crosswalk;
      v_gz_layers_fields       := v_schema || '.GZ_LAYERS_FIELDS';
      all_tables(5)            := v_gz_layers_fields;
      v_gz_layers_geoid        := v_schema || '.GZ_LAYERS_GEOID';
      all_tables(6)            := v_gz_layers_geoid;
      v_gz_layers_hierarchical := v_schema || '.GZ_LAYERS_HIERARCHICAL';
      all_tables(7)            := v_gz_layers_hierarchical;
      v_gz_layers_split        := v_schema || '.GZ_LAYERS_SPLIT';
      all_tables(8)            := v_gz_layers_split;
      v_gz_layers_subset       := v_schema || '.GZ_LAYERS_SUBSET';
      all_tables(9)            := v_gz_layers_subset;



      --start with basic, if found we get out right away, checks

      --these two have to exist and have records

      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(v_gz_layers_out)
      THEN

         output := output || '| Table ' || v_gz_layers_out || ' doesnt appear to exist ';

      END IF;


      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(v_gz_layers_in)
      THEN

         output := output || '| Table ' || v_gz_layers_in || ' doesnt appear to exist ';

      END IF;

      IF output <> ''
      THEN

         RETURN output;

      END IF;


      --the rest can be empty

      psql := 'SELECT COUNT(*) '
           || 'FROM all_tables a '
           || 'WHERE a.owner = :p1 AND '
           || 'a.table_name IN (:p2,:p3,:p4,:p5,:p6,:p7,:p8)';

      EXECUTE IMMEDIATE psql INTO kount USING v_schema,
                                              'GZ_LAYERS_AGGREGATE',
                                              'GZ_LAYERS_CROSSWALK',
                                              'GZ_LAYERS_FIELDS',
                                              'GZ_LAYERS_GEOID',
                                              'GZ_LAYERS_HIERARCHICAL',
                                              'GZ_LAYERS_SPLIT',
                                              'GZ_LAYERS_SUBSET';

      IF kount < 7
      THEN

         output := output || '| One or more of the GZ_LAYERS_<layer_type> tables is missing ';

      END IF;

      --only do this one if its our schema

      IF p_schema = SYS_CONTEXT('USERENV', 'CURRENT_USER')
      THEN

         FOR i IN 1 .. all_tables.COUNT
         LOOP

            psql := 'SELECT count(*) '
                 || 'FROM user_constraints '
                 || 'WHERE table_name = :p1 ';

            EXECUTE IMMEDIATE psql INTO kount USING all_tables(i);

            IF kount = 0
            THEN

               output := output || '| Table ' || all_tables(i) || ' is missing its keys or constraints ';

            END IF;

         END LOOP;

      END IF;


      --check that we have this release

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || v_gz_layers_out || ' a '
           || 'WHERE a.release = :p1 AND '
           || 'rownum = 1 ';

      EXECUTE IMMEDIATE psql INTO kount USING v_release;

      IF kount = 0
      THEN

         output := output || '| ' || v_gz_layers_out || ' doesnt contain release ' || v_release || ' ';

      END IF;

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || v_gz_layers_in || ' a '
           || 'WHERE a.release = :p1 AND '
           || 'rownum = 1 ';

      EXECUTE IMMEDIATE psql INTO kount USING v_release;

      IF kount = 0
      THEN

         output := output || '| ' || v_gz_layers_out || ' doesnt contain release ' || v_release || ' ';

      END IF;


      IF output <> ''
      THEN

         RETURN output;

      END IF;

      ---------------------------------------------------------
      --check each required relationship based on gz_layers_out
      ---------------------------------------------------------


      --aggregate doesnt have project

      psql := 'SELECT a.layer '
           || 'FROM ' || v_gz_layers_out || ' a '
           || 'WHERE a.release = :p1 AND '
           || 'a.layer_type = :p2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO layers USING v_release,
                                                            'AGGREGATE';


      psql := 'SELECT a.layer '
           || 'FROM ' || v_gz_layers_aggregate || ' a '
           || 'WHERE a.release = :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO child_layers USING v_release;

      FOR i IN 1 .. child_layers.COUNT
      LOOP

         layer_hash(child_layers(i)) := '1';

      END LOOP;

      FOR i IN 1 .. layers.COUNT
      LOOP


         IF NOT layer_hash.EXISTS(layers(i))
         THEN

            output := output || '| ' || v_gz_layers_aggregate || ' is missing layer ' || layers(i) || ' ';

         END IF;

      END LOOP;

      child_layers.DELETE;
      layers.DELETE;
      layer_hash.DELETE;


      --fields applies to all types

      psql := 'SELECT a.layer FROM ' || v_gz_layers_out || ' a '
           || 'WHERE a.release = :p1 '
           || 'MINUS '
           || 'SELECT b.layer FROM ' || v_gz_layers_fields || ' b '
           || 'WHERE b.release = :p2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO layers USING v_release,
                                                            v_release;

      FOR i IN 1 .. layers.COUNT
      LOOP

         output := output || '| ' || v_gz_layers_fields || ' is missing layer ' || layers(i) || ' ';

      END LOOP;

      layers.DELETE;


      --geoid applies to all types

      psql := 'SELECT a.layer FROM ' || v_gz_layers_out || ' a '
           || 'WHERE a.release = :p1 '
           || 'MINUS '
           || 'SELECT b.sum_lev FROM ' || v_gz_layers_geoid || ' b '
           || 'WHERE b.release = :p2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO layers USING v_release,
                                                            v_release;

      FOR i IN 1 .. layers.COUNT
      LOOP

         output := output || '| ' || v_gz_layers_geoid || ' is missing layer ' || layers(i) || ' ';

      END LOOP;

      layers.DELETE;


      --hierarchical has project and layer

      psql := 'SELECT a.gen_project_id, a.layer '
           || 'FROM ' || v_gz_layers_out || ' a '
           || 'WHERE a.release = :p1 AND '
           || 'a.layer_type = :p2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO projects,
                                               layers USING v_release,
                                                            'HIERARCHICAL';


      psql := 'SELECT a.gen_project_id, a.layer '
           || 'FROM ' || v_gz_layers_hierarchical || ' a '
           || 'WHERE a.release = :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO child_projects,
                                               child_layers USING v_release;

      FOR i IN 1 .. child_projects.COUNT
      LOOP

         layer_hash(child_projects(i) || ',' || child_layers(i)) := '1';

      END LOOP;

      FOR i IN 1 .. projects.COUNT
      LOOP

         IF NOT layer_hash.EXISTS(projects(i) || ',' || layers(i))
         THEN

            output := output || '| ' || v_gz_layers_hierarchical || ' is missing project,layer ' || projects(i) || ',' || layers(i) || ' ';

         END IF;

      END LOOP;

      projects.DELETE;
      layers.DELETE;
      layer_hash.DELETE;
      child_projects.DELETE;
      child_layers.DELETE;


      --split has layer only

      psql := 'SELECT a.layer '
           || 'FROM ' || v_gz_layers_out || ' a '
           || 'WHERE a.release = :p1 AND '
           || 'a.layer_type = :p2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO layers USING v_release,
                                                            'SPLIT';

      psql := 'SELECT a.layer '
           || 'FROM ' || v_gz_layers_split || ' a '
           || 'WHERE a.release = :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO child_layers USING v_release;

      FOR i IN 1 .. child_layers.COUNT
      LOOP

         layer_hash(child_layers(i)) := '1';

      END LOOP;

      FOR i IN 1 .. layers.COUNT
      LOOP

         IF NOT layer_hash.EXISTS(layers(i))
         THEN

            output := output || '| ' || v_gz_layers_split || ' is missing layer ' || layers(i) || ' ';

         END IF;

      END LOOP;

      layers.DELETE;
      layer_hash.DELETE;
      child_layers.DELETE;


      --subset has project and layer
      --dont lets forget, INITIAL is also listed in SUBSET

      psql := 'SELECT a.gen_project_id, a.layer '
           || 'FROM ' || v_gz_layers_out || ' a '
           || 'WHERE a.release = :p1 AND '
           || 'a.layer_type IN (:p2,:p3) ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO projects,
                                               layers USING v_release,
                                                            'SUBSET',
                                                            'INITIAL';

      psql := 'SELECT a.gen_project_id, a.layer '
           || 'FROM ' || v_gz_layers_subset || ' a '
           || 'WHERE a.release = :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO child_projects,
                                               child_layers USING v_release;

      FOR i IN 1 .. child_projects.COUNT
      LOOP

         layer_hash(child_projects(i) || ',' || child_layers(i)) := '1';

      END LOOP;

      FOR i IN 1 .. projects.COUNT
      LOOP

         IF NOT layer_hash.EXISTS(projects(i) || ',' || layers(i))
         THEN

            output := output || '| ' || v_gz_layers_subset || ' is missing project,layer ' || projects(i) || ',' || layers(i) || ' ';

         END IF;

      END LOOP;

      projects.DELETE;
      layers.DELETE;
      layer_hash.DELETE;


      ----------------------------------------------------------------------
      --check each child gz_layers_layertype table for required sub children
      ----------------------------------------------------------------------

      projects := GZ_OUTPUT.GET_RELEASE_PROJECTS(p_release);

      FOR i IN 1 .. projects.COUNT
      LOOP

         --first, check that no layers are listed in both
         --gz_layers_in and gz_layers_out

         psql := 'SELECT layer FROM gz_layers_in i '
              || 'WHERE '
              || 'i.release = :p1 AND '
              || 'i.gen_project_id = :p2 AND '
              || 'EXISTS ( '
              || 'SELECT o.layer FROM gz_layers_out o '
              || 'WHERE '
              || 'o.release = :p3 AND '
              || 'o.gen_project_id = :p4 AND '
              || 'i.layer = o.layer) ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO layers USING p_release,
                                                               projects(i),
                                                               p_release,
                                                               projects(i);

         FOR j IN 1 .. layers.COUNT
         LOOP

             output := output || '| Layer ' || layers(j) || ' on project ' || projects(i) || ' is listed on both gz_layers_in and gz_layers_out ';

         END LOOP;

         --get all valid layers once
         all_layers := GZ_OUTPUT.GET_ALL_LAYERS(p_release,
                                                 projects(i));


         --aggregate

         layers_out := GET_LAYERS_OUT(p_release,
                                      projects(i),
                                      'AGGREGATE');

         FOR j IN 1 .. layers_out.COUNT
         LOOP

            layer_aggr := GZ_OUTPUT.GET_AGGREGATE_LAYER(p_release,
                                                        layers_out(j).layer);

            --aggregate source is comma delimited
            layers := GZ_BUSINESS_UTILS.SPLIT(layer_aggr.source,',');

            IF layers.COUNT < 2
            THEN

               output := output || '| Project ' || projects(i) || ' aggregate layer ' || layers_out(j).layer || ' doesnt appear to have enough SOURCEs in gz_layers_aggregate ';

            END IF;

            FOR ii IN 1 .. layers.COUNT
            LOOP

               --check that each layer exists
               IF NOT all_layers.EXISTS(layers(ii))
               THEN

                  output := output || '| Project ' || projects(i) || ' aggregate layer ' || layer_aggr.layer || 's SOURCE of ' || layers(ii) || ', listed in gz_layers_aggregate doesnt exist ';

               END IF;

            END LOOP;

         END LOOP;   --end aggregate


         --hierarchical

         layers_out := GET_LAYERS_OUT(p_release,
                                      projects(i),
                                      'HIERARCHICAL');

         FOR j IN 1 .. layers_out.COUNT
         LOOP

            layer_hier := GZ_OUTPUT.GET_HIERARCHICAL_LAYER(p_release,
                                                           projects(i),
                                                           layers_out(j).layer);

            --check that each nesting layer exists
            IF NOT all_layers.EXISTS(layer_hier.nesting_layer)
            THEN

               output := output || '| Project ' || projects(i) || ' hierarchical layer ' || layer_hier.layer || ' nesting_layer ' || layer_hier.nesting_layer || ' listed in gz_layers_hierarchical doesnt exist ';

            END IF;

            --check that each nesting column is a aliased

            IF layer_hier.source_nesting_field NOT LIKE ('a.%')
            THEN

               output := output || '| Project ' || projects(i) || ' hierarchical layer ' || layer_hier.layer
                                || ' source_nesting_field ' || layer_hier.source_nesting_field || ' isnt a-aliased ';

            END IF;

            IF layer_hier.nesting_layer_field NOT LIKE ('a.%')
            THEN

               output := output || '| Project ' || projects(i) || ' hierarchical layer ' || layer_hier.layer
                                || ' nesting_layer_field ' || layer_hier.nesting_layer_field || ' isnt a-aliased ';

            END IF;

         END LOOP;   --end hierarchical


         --split

         layers_out := GET_LAYERS_OUT(p_release,
                                      projects(i),
                                      'SPLIT');

         FOR j IN 1 .. layers_out.COUNT
         LOOP

            layer_split := GZ_OUTPUT.GET_SPLIT_LAYER(p_release,
                                                     layers_out(j).layer);


            IF NOT all_layers.EXISTS(layer_split.base_layer)
            THEN

               --check that each base layer exists

               output := output || '| Project ' || projects(i) || ' split layer ' || layer_split.layer || ' base_layer ' || layer_split.base_layer || ' listed in gz_layers_split doesnt exist ';

            ELSE

               --special checks on what's allowable for base layers of splits
               --generally, base layers of splits can be anything


               IF NOT GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                                 projects(i),
                                                 layer_split.base_layer)
               THEN

                  IF GZ_OUTPUT.GET_LAYER_TYPE(p_release,
                                              projects(i),
                                              layer_split.base_layer) = 'SPLIT'
                  THEN

                     --enforce any special rules for splits of splits
                     --Current code cant handle remainders. Maybe some day if theres a need I will program it
                     --But if not kicked out here the result could be a seeming success thats actually totally wrong

                     IF layer_split.create_remainders = 'Y'
                     THEN

                        output := output || '| Project ' || projects(i) || ' split layer ' || layer_split.layer || ' is a split of a split and it requests remainders. '
                                         || ' Code to build remainders for splits of splits isn''t implemented.';

                     END IF;

                  END IF;

               END IF;

            END IF;

            IF NOT all_layers.EXISTS(layer_split.superior_layer)
            THEN

               --check that each superior layer exists

               output := output || '| Project ' || projects(i) || ' split layer ' || layer_split.layer || ' superior_layer ' || layer_split.superior_layer || ' listed in gz_layers_split doesnt exist ';

            ELSE

               --special checks on whats allowable for superior layers
               --cant be aggregate or split as superior
               IF NOT GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                                 projects(i),
                                                 layer_split.superior_layer)
               THEN

                  IF GZ_OUTPUT.GET_LAYER_TYPE(p_release,
                                              projects(i),
                                              layer_split.superior_layer) IN ('SPLIT','AGGREGATE')
                  THEN

                     output := output || '| Project ' || projects(i) || ' split layer ' || layer_split.layer || ' superior_layer ' || layer_split.superior_layer || ' with layer type of '
                                      || GZ_OUTPUT.GET_LAYER_TYPE(p_release,
                                                                  projects(i),
                                                                  layer_split.superior_layer) || ' isnt currently programmed ';

                  END IF;

               END IF;

            END IF;



         END LOOP;   --end split


         --subset

         layers_out := GET_LAYERS_OUT(p_release,
                                      projects(i),
                                      'SUBSET');

         FOR j IN 1 .. layers_out.COUNT
         LOOP

            layer_subset := GZ_OUTPUT.GET_SUBSET_LAYER(p_release,
                                                       projects(i),
                                                       layers_out(j).layer);

            --check that source layer exists
            IF NOT all_layers.EXISTS(layer_subset.source)
            THEN

               output := output || '| Project ' || projects(i) || ' subset layer ' || layer_subset.layer || ' base_layer ' || layer_subset.source || ' listed in gz_layers_subset doesnt exist ';

            END IF;


         END LOOP;   --end subset


      END LOOP; --loop on projects


      --------------------------------------------------
      --Checks that all gz_layers_fields <xxx> <yyy> have a match in gz_layers_crosswalk
      --------------------------------------------------

      --get all values for known fields to the gz_layers_crosswalk table

      length_hash := GZ_OUTPUT.GET_CROSSWALK_HASH(p_release);

      --we already have a hash with all layers for this release

      --no GETter for this

      psql := 'SELECT a.layer '
           || 'FROM '
           || 'gz_layers_fields a '
           || 'WHERE a.release = :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO layers USING p_release;

      FOR i IN 1 .. layers.COUNT
      LOOP


         fields := GZ_OUTPUT.GET_FIELDS(p_release,
                                        layers(i));

         FOR j IN 1 .. fields.COUNT
         LOOP


            IF NOT length_hash.EXISTS(fields(j))
            THEN

               output := output || '| Layer ' || layers(i) || ' field ' || fields(j) || ' is missing from gz_layers_crosswalk ';

            END IF;

         END LOOP;

      END LOOP;

      --------------------------------------------------------------------
      --Checks that gz_layers_fields <xxx> <yyy> sorta match any add_to_face
      --------------------------------------------------------------------

      FOR i IN 1 .. projects.COUNT
      LOOP

         psql := 'SELECT COUNT(*) FROM '
              || v_gz_layers_out || ' o '
              || 'WHERE '
              || 'o.release = :p1 AND '
              || 'o.gen_project_id = :p2 AND '
              || 'o.add_to_face IS NOT NULL ';

         EXECUTE IMMEDIATE psql INTO kount USING p_release,
                                                 projects(i);

         IF kount = 1
         THEN

            psql := 'SELECT COUNT(*) FROM '
                 || v_gz_layers_out || ' o, '
                 || v_gz_layers_fields || ' f '
                 || 'WHERE '
                 || 'o.release = :p1 AND '
                 || 'o.gen_project_id = :p2 AND '
                 || 'o.add_to_face IS NOT NULL AND '
                 || 'f.release = :p3 AND '
                 || 'f.layer = o.layer AND '
                 || 'UPPER(f.fields) LIKE ''%'' || UPPER(o.add_to_face) || ''%'' ';

            EXECUTE IMMEDIATE psql INTO kount2 USING  p_release,
                                                      projects(i),
                                                      p_release;

            IF kount <> kount2
            THEN

                output := output || '| ' || v_gz_layers_out || ' Project ' || projects(i) || ' add_to_face value doesnt appear to match '
                                 || 'the fields of the same layer on ' || v_gz_layers_fields;

            END IF;

         ELSIF kount > 1
         THEN

            --Theres some code in initial topo build and the QA module that expect a single add_to_face value
            --Weve never had more than one in a project.  Could be handled if theres a good reason for it
            output := output || '| ' || v_gz_layers_out || ' Project ' || projects(i) || ' has more than one '
                             || 'add_to_face value. Theres some code that assumes only one value - lets talk if this is intentional';

         ELSIF kount = 0
         THEN

            --Im gonna allow this.  Could be possible theres no add to state because we are running nation-nation-nation
            --without a clip. Probably suspicious though, leaving this here as a reminder
            NULL;

         END IF;

      END LOOP;



      --What else?

      --dbms_output.put_line('output iz -' || output || '-');


      RETURN output;


   END VERIFY_OUTPUT_PARMS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE---------------------------------------------------------------------------------

   PROCEDURE START_OUTPUT_LOGGING (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_output_topology       IN VARCHAR2
   )
   AS

      --Matt! 5/3/12
      --Create logging table for this job

   BEGIN

      GZ_BUSINESS_UTILS.CREATE_GEN_XTEND_TRACKING_LOG(sys_context('USERENV','CURRENT_SCHEMA'),
                                                 p_output_topology || '_OUTPUT_TRACKING');

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',
                                             p_output_topology,
                                             'START_OUTPUT_LOGGING', NULL,
                                             'STARTING RELEASE:' || p_release
                                                  || ' PROJECT:' || p_gen_project_id
                                                  || ' JOB:' || p_output_topology);


   END START_OUTPUT_LOGGING;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GZ_TOPO_EXISTS (
      p_topo         IN VARCHAR2,
      p_schema       IN VARCHAR2 DEFAULT NULL
   ) RETURN BOOLEAN
   AS

      --Matt! 5/3/12

      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;

   BEGIN

      --kick out junk right away
      IF LENGTH(p_topo) = 0
      THEN
         RETURN FALSE;
      END IF;


      BEGIN

         IF p_schema IS NOT NULL
         THEN

            psql := 'SELECT COUNT(*) FROM '
                 || 'all_sdo_topo_info a '
                 || 'WHERE '
                 || 'a.owner = :p1 AND '
                 || 'a.topology = :p2 AND '
                 || 'rownum = 1 ';

            EXECUTE IMMEDIATE psql INTO kount USING UPPER(p_schema),
                                                    UPPER(p_topo);


         ELSE

            psql := 'SELECT COUNT(*) FROM '
                 || 'user_sdo_topo_info a '
                 || 'WHERE '
                 || 'a.topology = :p1 AND '
                 || 'rownum = 1 ';

            EXECUTE IMMEDIATE psql INTO kount USING UPPER(p_topo);


         END IF;

      EXCEPTION
      WHEN OTHERS
      THEN

          --anything specific here?
          RETURN FALSE;

      END;

      IF kount = 0
      THEN

         RETURN FALSE;

      END IF;

      RETURN TRUE;

   END GZ_TOPO_EXISTS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE UPDATE_TOPOGEOM (
      p_topo                  IN VARCHAR2,
      p_table                 IN VARCHAR2,
      p_table_oid             IN VARCHAR2,
      p_topo_type             IN NUMBER,
      p_tg_layer_id           IN NUMBER,
      p_topo_object_array     IN MDSYS.SDO_TOPO_OBJECT_ARRAY,
      p_tgl_object_array      IN MDSYS.SDO_TGL_OBJECT_ARRAY,
      p_oid_column            IN VARCHAR2 DEFAULT 'OID',
      p_column_name           IN VARCHAR2 DEFAULT 'TOPOGEOM'
   )
   AS

      --Matt! 1/2/13
      --Only adds
      --See GZ_SMPOLY.DELETE_PARENT_TGL_OBJECT for deletes

      --Syntax sample reminder
      --
      --declare
      --tgl_arr  MDSYS.SDO_TGL_OBJECT_ARRAY := MDSYS.SDO_TGL_OBJECT_ARRAY();
      --begin
      --tgl_arr.EXTEND(5);
      --tgl_arr(1) := SDO_TGL_OBJECT(2,15234);
      --tgl_arr(2) := SDO_TGL_OBJECT(2,15195);
      --tgl_arr(3) := SDO_TGL_OBJECT(2,15470);
      --tgl_arr(4) := SDO_TGL_OBJECT(2,15699);
      --tgl_arr(5) := SDO_TGL_OBJECT(2,15698);
      --GZ_OUTPUT.UPDATE_TOPOGEOM('Z699TM','Z699TM_FSL140V','207901230958021',3,3,NULL,tgl_arr,'OID_BASE');
      --end;

      psql              VARCHAR2(4000);

   BEGIN

      IF p_topo_object_array IS NOT NULL
      AND p_tgl_object_array IS NOT NULL
      THEN

          RAISE_APPLICATION_ERROR(-20001,'Should only be adding primitives or children, not both');

      ELSIF p_topo_object_array IS NULL
      AND p_tgl_object_array IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Didnt find any array elements to add');

      END IF;


      psql := 'UPDATE ' || p_table || ' a '
           || 'SET a.' || p_column_name || ' = SDO_TOPO_GEOMETRY( '
           || ':p1, '                                 --topology
           || ':p2, '                                 --topology geometry type
           || ':p3, '                                 --tg_layer_id
           || ':p4, '                                 --ids to add
           || 'NULL '                                 --No delete
           || ') '
           || 'WHERE a.' || p_oid_column || ' = :p5 ';


      BEGIN

         IF p_topo_object_array IS NOT NULL
         THEN

            EXECUTE IMMEDIATE psql USING p_topo,
                                         p_topo_type,
                                         p_tg_layer_id,
                                         p_topo_object_array,
                                         p_table_oid;

         ELSE

            EXECUTE IMMEDIATE psql USING p_topo,
                                         p_topo_type,
                                         p_tg_layer_id,
                                         p_tgl_object_array,
                                         p_table_oid;

         END IF;

      EXCEPTION
      WHEN OTHERS
      THEN

         RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' on ' || psql);

      END;

      COMMIT;


   END UPDATE_TOPOGEOM;
   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION IS_LAYER_THROUGH_MODULE (
      p_topo         IN VARCHAR2,
      p_layer        IN VARCHAR2,
      p_module       IN NUMBER
   ) RETURN BOOLEAN
   AS

      --Matt! 10/23/12
      --Late add to help with restarts

      psql           VARCHAR2(4000);
      kount          PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(*) '
           || 'FROM ' || p_topo || '_layers_out_info a '
           || 'WHERE a.layer = :p1 AND '
           || 'a.module_status < :p2 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_layer,
                                              p_module;

      IF kount = 1
      THEN

         RETURN FALSE;

      ELSIF kount = 0
      THEN

         RETURN TRUE;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'I was a stranger in a foreign land until I met thee');

      END IF;

   END;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_RELEASE_PROJECTS (
      p_release                  IN VARCHAR2
   ) RETURN GZ_TYPES.STRINGARRAY
   AS

      --Matt! 5/4/12

      psql           VARCHAR2(4000);
      output         GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT DISTINCT a.gen_project_id '
           || 'FROM gz_layers_out a '
           || 'WHERE a.release = :p1 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_release);

      --no bother with error handlers yet

      RETURN output;

   END GET_RELEASE_PROJECTS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_ALL_LAYERS (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2
   ) RETURN GZ_TYPES.STRINGHASH
   AS

      --Matt! 5/4/12

      psql              VARCHAR2(4000);
      temput            GZ_TYPES.stringarray;
      output            GZ_TYPES.stringhash;

   BEGIN

      psql := 'SELECT layer FROM gz_layers_out a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 '
           || 'UNION ALL '
           || 'SELECT b.layer FROM gz_layers_in b '
           || 'WHERE '
           || 'b.release = :p3 AND '
           || 'b.gen_project_id = :p4 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO temput USING UPPER(p_release),
                                                            UPPER(p_gen_project_id),
                                                            UPPER(p_release),
                                                            UPPER(p_gen_project_id);

      FOR i IN 1 .. temput.COUNT
      LOOP

         output(temput(i)) := '1';

      END LOOP;

      RETURN output;

   END GET_ALL_LAYERS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_LAYERS_OUT_INFO (
      p_topo                     IN VARCHAR2,
      p_layer_type               IN VARCHAR2 DEFAULT NULL,
      p_layer                    IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.GZ_LAYERS_OUT_INFO
   AS

      --Matt! 05/04/12

      psql     VARCHAR2(4000);
      output   GZ_TYPES.GZ_LAYERS_OUT_INFO;

   BEGIN

      psql := 'SELECT a.* FROM ' || p_topo || '_LAYERS_OUT_INFO a ';

      IF p_layer_type IS NOT NULL
      THEN

         psql := psql || 'WHERE a.layer_type = :p1 ';

      END IF;

      IF p_layer IS NOT NULL
      THEN

         --rare, single layer processing

         IF p_layer_type IS NOT NULL
         THEN

            psql := psql || 'AND a.layer = ''' || p_layer || ''' ';

         ELSE

            psql := psql || 'WHERE a.layer = ''' || p_layer || ''' ';

         END IF;

      END IF;

      BEGIN

         IF p_layer_type IS NULL
         THEN

            EXECUTE IMMEDIATE psql BULK COLLECT INTO output;

         ELSE

            EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_layer_type);

         END IF;

      EXCEPTION

         WHEN NO_DATA_FOUND THEN

            IF p_layer_type IS NULL
            THEN

                RAISE_APPLICATION_ERROR(-20001,'Yo, found no layers in ' || p_topo || '_LAYERS_OUT_INFO');

            ELSE

               RETURN output;

            END IF;

         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table ' || p_topo || '_LAYERS_OUT_INFO does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if '
                                              || p_topo || '_LAYERS_OUT_INFO table matches GZ_LAYERS_OUT_INFO type');
            ELSE
               RAISE;
            END IF;

      END;

      RETURN output;

   END GET_LAYERS_OUT_INFO;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_LAYER_OUT_INFO (
      p_topo                     IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_OUT_INFO_REC
   AS

      --Matt! 5/7/12


      psql     VARCHAR2(4000);
      output   GZ_TYPES.GZ_LAYERS_OUT_INFO_REC;

   BEGIN

      psql := 'SELECT a.* FROM ' || p_topo || '_LAYERS_OUT_INFO a '
           || 'WHERE a.layer = :p1 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING UPPER(p_layer);

      EXCEPTION

         WHEN NO_DATA_FOUND THEN

            RAISE_APPLICATION_ERROR(-20001,'Yo, no layers in ' || p_topo || '_LAYERS_OUT_INFO for layer ' ||  p_layer );

         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table ' || p_topo || '_LAYERS_OUT_INFO does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if ' || p_topo || '_LAYERS_OUT_INFO table matches GZ_LAYERS_OUT_INFO type');
            ELSE
               RAISE;
            END IF;

      END;

      RETURN output;

   END GET_LAYER_OUT_INFO;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_FACE_TABLE (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_topology                 IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 6/1/12
      --called to get the face table, since it could be named anything
      --its the poly layer in the topology which is either solo (on first time thru)
      --or isnt one of the layers we are building
      --Will also call this from the verification step

      --The concept of running with 10 layers, then rerunning with 8 is a squiggly one
      --For now it requires manually intervention. Prob for the best, could make a mess

      psql                 VARCHAR2(4000);
      output               VARCHAR2(32);


   BEGIN

      psql := 'SELECT a.table_name '
           || 'FROM user_sdo_topo_info a '
           || 'WHERE '
           || 'a.topology = :p1 AND '
           || 'a.tg_layer_type = :p2 AND '
           || 'a.table_name NOT IN ('
           || 'SELECT ''' || p_topology || '_FSL'' || b.layer || ''V'' '
           || 'FROM gz_layers_out b '
           || 'WHERE '
           || 'b.release = :p3 AND '
           || 'b.gen_project_id = :p4 '
           || ') ';

           --dbms_output.put_line(psql);

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING UPPER(p_topology),
                                                  'POLYGON',
                                                  p_release,
                                                  p_gen_project_id;

      EXCEPTION
      WHEN OTHERS THEN
         IF SQLCODE = -1422
         THEN

            --ORA-01422: exact fetch returns more than requested number of rows
            RAISE_APPLICATION_ERROR(-20001,'There are more layers in the topology than face + gz_layers_out. Are you rerunning with fewer layers?');

         ELSE

            RAISE_APPLICATION_ERROR(-20001,'Cant find a face table');

         END IF;


      END;

      RETURN output;


   END GET_FACE_TABLE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_LAYER_IN (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_IN_REC
   AS

      --Matt! 5/7/12

      psql           VARCHAR2(4000);
      output         GZ_TYPES.GZ_LAYERS_IN_REC;

   BEGIN

      psql := 'SELECT a.* FROM gz_layers_in a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 AND '
           || 'a.layer = :p3 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING UPPER(p_release),
                                                  UPPER(p_gen_project_id),
                                                  UPPER(p_layer);

      EXCEPTION

         WHEN NO_DATA_FOUND THEN

            RAISE_APPLICATION_ERROR(-20001,'Yo, no layers in GZ_LAYERS_IN for layer ' ||  p_layer );

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

   END GET_LAYER_IN;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_LAYER_TYPE (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 8/16/12
      --This is the version for general parameter table work
      --No active job

      psql                    VARCHAR2(4000);
      output                  VARCHAR2(32);

   BEGIN

      psql := 'SELECT a.layer_type '
           || 'FROM '
           || 'gz_layers_out a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 AND '
           || 'a.layer = :p3 ';

      EXECUTE IMMEDIATE psql INTO output USING UPPER(p_release),
                                               UPPER(p_gen_project_id),
                                               UPPER(p_layer);

      RETURN output;

   END GET_LAYER_TYPE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_LAYER_TYPE (
      p_topo                     IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/7/12
      --This is the version for an active job
      --See just above for generic parameter tables

      psql                    VARCHAR2(4000);
      output                  VARCHAR2(32);

   BEGIN

      psql := 'SELECT a.layer_type '
           || 'FROM ' || p_topo || '_LAYERS_OUT_INFO a '
           || 'WHERE a.layer = :p1 ';

      EXECUTE IMMEDIATE psql INTO output USING UPPER(p_layer);

      RETURN output;

   END GET_LAYER_TYPE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_LAYERS_OUT (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer_type               IN VARCHAR2 DEFAULT NULL,
      p_layer                    IN VARCHAR2 DEFAULT NULL
   ) RETURN GZ_TYPES.GZ_LAYERS_OUT
   AS

      --Matt! 05/03/12

      psql     VARCHAR2(4000);
      output   GZ_TYPES.GZ_LAYERS_OUT;

   BEGIN

      psql := 'SELECT a.* FROM GZ_LAYERS_OUT a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 ';

      IF p_layer_type IS NOT NULL
      THEN

         psql := psql || ' AND a.layer_type = :p3 ';

      END IF;

      IF p_layer IS NOT NULL
      THEN

         --rare, single layer processing
         psql := psql || ' AND a.layer = ''' || p_layer || ''' ';

      END IF;

      BEGIN

         IF p_layer_type IS NULL
         THEN

            EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_release),
                                                                  UPPER(p_gen_project_id);

         ELSE

            EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_release),
                                                                  UPPER(p_gen_project_id),
                                                                  UPPER(p_layer_type);

         END IF;

      EXCEPTION

         WHEN NO_DATA_FOUND THEN

            IF p_layer_type IS NULL
            THEN

                RAISE_APPLICATION_ERROR(-20001,'Yo, no layers in GZ_LAYERS_OUT for project id ' || p_gen_project_id || ' and release ' || p_release);

            ELSE

               RETURN output;

            END IF;

         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table GZ_LAYERS_OUT does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if GZ_LAYERS_OUT table matches GZ_LAYERS_OUT type');
            ELSE
               RAISE;
            END IF;

      END;

      RETURN output;

   END GET_LAYERS_OUT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_HIERARCHICAL_LAYER (
      p_release                  IN VARCHAR2,
      p_gen_project_id               IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_HIERARCHICAL_REC
   AS

      --Matt! 05/03/12

      psql              VARCHAR2(4000);
      output            GZ_TYPES.GZ_LAYERS_HIERARCHICAL_REC;

   BEGIN


      psql := 'SELECT * FROM gz_layers_hierarchical a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 AND '
           || 'a.layer = :p3 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING p_release,
                                                  p_gen_project_id,
                                                  p_layer;
      EXCEPTION

         WHEN NO_DATA_FOUND
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Yo, no layers in GZ_LAYERS_HIERARCHICAL for '
                                        || 'project id ' || p_gen_project_id || ' and release ' || p_release || ' and layer ' || p_layer);

         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table GZ_LAYERS_HIERARCHICAL does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if GZ_LAYERS_HIERARCHICAL table matches GZ_LAYERS_HIERARCHICAL type');
            ELSE
               RAISE;
            END IF;

      END;


      RETURN output;

   END GET_HIERARCHICAL_LAYER;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_SUBSET_LAYER (
      p_release                  IN VARCHAR2,
      p_gen_project_id               IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_SUBSET_REC
   AS

      --Matt! 05/03/12

      psql              VARCHAR2(4000);
      output            GZ_TYPES.GZ_LAYERS_SUBSET_REC;

   BEGIN


      psql := 'SELECT * FROM gz_layers_subset a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 AND '
           || 'a.layer = :p3 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING p_release,
                                                  p_gen_project_id,
                                                  p_layer;
      EXCEPTION

         WHEN NO_DATA_FOUND
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Yo, no layers in GZ_LAYERS_SUBSET for '
                                        || 'project id ' || p_gen_project_id || ' and release ' || p_release || ' and layer ' || p_layer);

         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table GZ_LAYERS_SUBSET does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if GZ_LAYERS_SUBSET table matches GZ_LAYERS_SUBSET type');
            ELSE
               RAISE;
            END IF;

      END;


      RETURN output;

   END GET_SUBSET_LAYER;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_SPLIT_LAYER (
      p_release                  IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_SPLIT_REC
   AS

      --Matt! 05/03/12

      psql              VARCHAR2(4000);
      output            GZ_TYPES.GZ_LAYERS_SPLIT_REC;

   BEGIN


      psql := 'SELECT * FROM gz_layers_split a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.layer = :p2 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING p_release,
                                                  p_layer;
      EXCEPTION

         WHEN NO_DATA_FOUND
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Yo, no layers in GZ_LAYERS_SPLIT for '
                                        || 'release ' || p_release || ' and layer ' || p_layer);

         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table GZ_LAYERS_SPLIT does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if GZ_LAYERS_SPLIT table matches GZ_LAYERS_SPLIT type');
            ELSE
               RAISE;
            END IF;

      END;


      RETURN output;

   END GET_SPLIT_LAYER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_AGGREGATE_LAYER (
      p_release                  IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.GZ_LAYERS_AGGREGATE_REC
   AS

      --Matt! 05/03/12

      psql              VARCHAR2(4000);
      output            GZ_TYPES.GZ_LAYERS_AGGREGATE_REC;

   BEGIN


      psql := 'SELECT * FROM gz_layers_aggregate a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.layer = :p2 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING p_release,
                                                  p_layer;
      EXCEPTION

         WHEN NO_DATA_FOUND
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Yo, no layers in GZ_LAYERS_AGGREGATE for '
                                        || 'release ' || p_release || ' and layer ' || p_layer);

         WHEN OTHERS THEN
            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table GZ_LAYERS_AGGREGATE does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if GZ_LAYERS_AGGREGATE table matches GZ_LAYERS_AGGREGATE type');
            ELSE
               RAISE;
            END IF;

      END;


      RETURN output;

   END GET_AGGREGATE_LAYER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_FIELDS (
      p_release                  IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 05/04/12

      psql              VARCHAR2(4000);
      field_string      VARCHAR2(4000);
      output            GZ_TYPES.stringarray;
      first_carrot      NUMBER;
      second_carrot     NUMBER;
      tokens            PLS_INTEGER := 0;
      deadman           PLS_INTEGER := 1000;

   BEGIN


      psql := 'SELECT a.fields '
           || 'FROM gz_layers_fields a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.layer = :p2 ';

      EXECUTE IMMEDIATE psql INTO field_string USING UPPER(p_release),
                                                     UPPER(p_layer);


      --<GEO_ID> <REGION> <NAME> <LSAD>


         first_carrot  := REGEXP_INSTR(field_string, '<',1,1);
         second_carrot := REGEXP_INSTR(field_string, '>',1,1);
         tokens := tokens + 1;

         LOOP

            EXIT WHEN first_carrot = 0;

            output(tokens) := SUBSTR(field_string, (first_carrot + 1), (second_carrot - first_carrot - 1));

            tokens := tokens + 1;
            first_carrot  := REGEXP_INSTR(field_string, '<',1,tokens);
            second_carrot := REGEXP_INSTR(field_string, '>',1,tokens);

            IF tokens > deadman
            THEN

               RAISE_APPLICATION_ERROR(-20001, 'deadman switch on ' || field_string);

            END IF;

         END LOOP;


      RETURN output;

   END GET_FIELDS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_UPDATE_FIELDS (
      p_release                  IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN GZ_TYPES.stringhash
   AS

      --Matt! 05/10/12

      psql              VARCHAR2(4000);
      output            GZ_TYPES.stringhash;
      update_fields     VARCHAR2(4000);
      update_delimiter  VARCHAR2(1);

   BEGIN

      psql := 'SELECT a.update_fields, a.update_fields_delimiter '
           || 'FROM '
           || 'gz_layers_fields a '
           || 'WHERE '
           || 'a.layer = :p1 AND '
           || 'a.release = :p2 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO update_fields,
                                     update_delimiter USING UPPER(p_layer),
                                                            UPPER(p_release);

      EXCEPTION

         WHEN NO_DATA_FOUND
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Yo, no update_fields in GZ_LAYERS_FIELDS for '
                                           || 'release ' || p_release || ' and layer ' || p_layer);

         WHEN OTHERS THEN

            IF SQLCODE = -942
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Yo, table GZ_LAYERS_FIELDS does not exist!');
            ELSIF SQLCODE = -1007
            THEN
               RAISE_APPLICATION_ERROR(-20001,'Variable not in select list, check if GZ_LAYERS_FIELDS table matches GZ_LAYERS_FIELDS type');
            ELSE
               RAISE;
            END IF;

      END;

      IF update_fields IS NOT NULL
      THEN

          output(update_fields) := update_delimiter;

      END IF;

      RETURN output;

   END GET_UPDATE_FIELDS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_CROSSWALK_HASH (
      p_release                  IN VARCHAR2,
      p_what                     IN VARCHAR2 DEFAULT 'OUTPUT_LENGTH'
   ) RETURN GZ_TYPES.stringhash
   AS

      --Matt! 5/4/12

      output            GZ_TYPES.stringhash;
      psql              VARCHAR2(4000);
      fields            GZ_TYPES.stringarray;
      whats             GZ_TYPES.stringarray;

   BEGIN

      IF p_what NOT IN ('OUTPUT_LENGTH','BENCH_FIELD')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'What the what is a ' || p_what || '?');

      END IF;

      psql := 'SELECT a.output_field, a.' || p_what || ' '
           || 'FROM gz_layers_crosswalk a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.' || p_what || ' IS NOT NULL ';  --avoid getting GEOID for bench_field and etc

      EXECUTE IMMEDIATE psql BULK COLLECT INTO fields,
                                               whats USING UPPER(p_release);

      FOR i IN 1 .. fields.COUNT
      LOOP

         output(fields(i)) := whats(i);

      END LOOP;

      RETURN output;

   END GET_CROSSWALK_HASH;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_MISSING_FIELDS (
      p_layer                    IN VARCHAR2,  --not used
      p_source                   IN VARCHAR2,
      p_all_fields               IN GZ_TYPES.stringarray,
      p_crosswalk                IN GZ_TYPES.stringhash
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 6/18/12

      --For some list of fields for this layer
      --(which must be filtered through the crosswalk to find benchmark column equivalents)
      --Which fields are not in the current passed in source, like TAB10ST09.CDP
      --Return those without matches. These are the superior-only fields, for example

      output               GZ_TYPES.stringarray;
      table_cols           GZ_TYPES.stringarray;
      table_col_hash       GZ_TYPES.stringhash;
      psql                 VARCHAR2(4000);
      schema_dot_table     GZ_TYPES.stringarray;
      output_kount         PLS_INTEGER := 0;


   BEGIN

      schema_dot_table := GZ_BUSINESS_UTILS.SPLIT(p_source, '\.');

      IF schema_dot_table.COUNT <> 2
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Got ' || schema_dot_table.COUNT || ' splits from ' || p_source);

      END IF;

      psql := 'SELECT column_name FROM '
           || 'all_tab_columns '
           || 'WHERE '
           || 'owner = :p1 AND '
           || 'table_name = :p2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO table_cols USING schema_dot_table(1),
                                                                schema_dot_table(2);

      FOR i IN 1 .. table_cols.COUNT
      LOOP

         table_col_hash(table_cols(i)) := '1';

      END LOOP;


      FOR i IN 1 .. p_all_fields.COUNT
      LOOP

         IF p_crosswalk.EXISTS(p_all_fields(i))
         THEN

            --only examine fields with crosswalk matches, not geoid for ex

            IF table_col_hash.EXISTS(p_crosswalk(p_all_fields(i)))
            THEN

               --has a match
               NULL;

            ELSE

               output_kount := output_kount + 1;
               output(output_kount) := p_all_fields(i);

            END IF;

         END IF;

      END LOOP;


      RETURN output;

   END GET_MISSING_FIELDS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_LAYER_LEVEL (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_output_topology          IN VARCHAR2,
      p_layer                    IN VARCHAR2,
      p_recursive                IN NUMBER DEFAULT 0
   ) RETURN NUMBER
   AS

      --Matt! 5/7/12
      --This is all half written for now

      layer_info                 GZ_TYPES.GZ_LAYERS_OUT_INFO_REC;
      layer_hierarchical         GZ_TYPES.GZ_LAYERS_HIERARCHICAL_REC;
      layer_subset               GZ_TYPES.GZ_LAYERS_SUBSET_REC;
      layer_aggregate            GZ_TYPES.GZ_LAYERS_AGGREGATE_REC;
      aggregate_sources          GZ_TYPES.stringarray;
      output                     NUMBER;
      recursivity                NUMBER;



   BEGIN

      IF p_recursive > 3
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Too deep!');

      ELSE

        recursivity := p_recursive + 1;

      END IF;

      layer_info := GZ_OUTPUT.GET_LAYER_OUT_INFO(p_output_topology,
                                                 p_layer);

      IF layer_info.layer_level IS NOT NULL
      THEN

         RETURN layer_info.layer_level;

      END IF;

      --unknown, so we try to find it
      --if we find it, set it in the info table and return the number
      --If not successful, like not enough children are known should return an error

      IF layer_info.layer_type = 'INITIAL'
      THEN

         --raise error?
         RETURN 0;

      ELSIF layer_info.layer_type = 'HIERARCHICAL'
      THEN

         layer_hierarchical := GET_HIERARCHICAL_LAYER(p_release,
                                                      p_gen_project_id,
                                                      p_layer);


         --should be 1 up from the nesting layer
         --unless initial, then there is no nesting layer in the topo
         --just faces to construct

         IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                       p_gen_project_id,
                                       layer_hierarchical.nesting_layer)
         THEN

            --set it while we are at it
            GZ_OUTPUT.SET_LAYER_LEVEL(p_output_topology,
                                      p_layer,
                                      0);

            RETURN 0;

         ELSE

            --scary recursive call
            output := GZ_OUTPUT.GET_LAYER_LEVEL(p_release,
                                                p_gen_project_id,
                                                p_output_topology,
                                                layer_hierarchical.nesting_layer,
                                                recursivity);

            --up 1 from nesting dummy
            output := output + 1;

            GZ_OUTPUT.SET_LAYER_LEVEL(p_output_topology,
                                      p_layer,
                                      output);

            RETURN output;

         END IF;

      ELSIF layer_info.layer_type = 'SUBSET'
      THEN

         --For now lets say one up from

         layer_subset := GZ_OUTPUT.GET_SUBSET_LAYER(p_release,
                                                   p_gen_project_id,
                                                   p_layer);

         IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                       p_gen_project_id,
                                       layer_subset.source)
         THEN

            --zero, must build on faces
            --set it while we are at it
            GZ_OUTPUT.SET_LAYER_LEVEL(p_output_topology,
                                      p_layer,
                                      0);

            RETURN 0;

         ELSE

            --scary recursive call
            output := GZ_OUTPUT.GET_LAYER_LEVEL(p_release,
                                                p_gen_project_id,
                                                p_output_topology,
                                                layer_subset.source,
                                                recursivity);


            output := output + 1;

            GZ_OUTPUT.SET_LAYER_LEVEL(p_output_topology,
                                      p_layer,
                                      output);

            RETURN output;

         END IF;

      ELSIF layer_info.layer_type = 'SPLIT'
      THEN

         --For the time being lets always bust these components down to faces

         --set it while we are at it
         GZ_OUTPUT.SET_LAYER_LEVEL(p_output_topology,
                                   p_layer,
                                   0);

         RETURN 0;

      ELSIF layer_info.layer_type = 'AGGREGATE'
      THEN

         layer_aggregate := GZ_OUTPUT.GET_AGGREGATE_LAYER(p_release,
                                                          p_layer);


         aggregate_sources := GZ_BUSINESS_UTILS.SPLIT(layer_aggregate.source,',');

         IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                       p_gen_project_id,
                                       aggregate_sources(1))
         OR GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                       p_gen_project_id,
                                       aggregate_sources(2))
         THEN

            --if either is 0, combine their faces
            --This is the only use we really see for this layer type as of today

            GZ_OUTPUT.SET_LAYER_LEVEL(p_output_topology,
                                      p_layer,
                                      0);

            RETURN 0;

         ELSE

            --aggregate is always 0
            --cant source a higher level feature table to 2 child tables

            GZ_OUTPUT.SET_LAYER_LEVEL(p_output_topology,
                                      p_layer,
                                      0);

            RETURN 0;

            /*
            --assume both are the same level then, like both 1, both 2?
            --better add more checks for this somewhere

            --scary recursive call
            output := GZ_OUTPUT.GET_LAYER_LEVEL(p_release,
                                                p_gen_project_id,
                                                p_output_topology,
                                                aggregate_sources(1),
                                                recursivity);

            IF output <> GZ_OUTPUT.GET_LAYER_LEVEL(p_release,
                                                   p_gen_project_id,
                                                   p_output_topology,
                                                   aggregate_sources(2),
                                                   recursivity)
            THEN

               RAISE_APPLICATION_ERROR(-20001,'Subset sources are at different levels! I guess this could be ok but its not expected');

            ELSE

               --still set to 0
               --no topo way to build higher level off of two child layers, must be registered on one

               GZ_OUTPUT.SET_LAYER_LEVEL(p_output_topology,
                                         p_layer,
                                         0);

               RETURN 0;

            END IF;
            */

         END IF;


      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Oops');

      END IF;


   END GET_LAYER_LEVEL;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_CHILD_LAYER (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_output_topology          IN VARCHAR2,
      p_layer                    IN VARCHAR2,
      p_which                    IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! 5/7/12

      layer_info                 GZ_TYPES.GZ_LAYERS_OUT_INFO_REC;
      layer_hierarchical         GZ_TYPES.GZ_LAYERS_HIERARCHICAL_REC;
      layer_subset               GZ_TYPES.GZ_LAYERS_SUBSET_REC;
      layer_split                GZ_TYPES.GZ_LAYERS_SPLIT_REC;

   BEGIN

      layer_info := GZ_OUTPUT.GET_LAYER_OUT_INFO(p_output_topology,
                                                 p_layer);

      IF (layer_info.layer_level = 0 AND layer_info.layer_type <> 'SPLIT')
      OR layer_info.layer_type IN ('INITIAL','AGGREGATE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Why you pestering me, no children');

      END IF;

      IF layer_info.layer_type = 'HIERARCHICAL'
      THEN

         layer_hierarchical := GET_HIERARCHICAL_LAYER(p_release,
                                                      p_gen_project_id,
                                                      p_layer);

         RETURN layer_hierarchical.nesting_layer;


      ELSIF layer_info.layer_type = 'SUBSET'
      THEN

         layer_subset := GZ_OUTPUT.GET_SUBSET_LAYER(p_release,
                                                    p_gen_project_id,
                                                    p_layer);

         RETURN layer_subset.source;

      ELSIF layer_info.layer_type = 'SPLIT'
      THEN

         layer_split := GZ_OUTPUT.GET_SPLIT_LAYER(p_release,
                                                  p_layer);

         IF p_which = 'BASE_LAYER'
         THEN

            RETURN layer_split.base_layer;

         ELSIF p_which = 'SUPERIOR_LAYER'
         THEN

            RETURN layer_split.superior_layer;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, 'Which p_which is a ' || p_which || '?');

         END IF;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'oops, whats layer type ' || layer_info.layer_type);

      END IF;

   END GET_CHILD_LAYER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_DEEP_SOURCE (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_output_topology          IN VARCHAR2,
      p_layer                    IN VARCHAR2,
      p_what                     IN VARCHAR2 DEFAULT 'TABLE', --or KEY
      p_depth                    IN NUMBER DEFAULT 0,
      p_which                    IN NUMBER DEFAULT NULL --1 or 2 for aggregate
   ) RETURN VARCHAR2
   AS

      --Matt! 5/8/12
      --For any given layer that is built on a lower level source
      --Return info (table, key) about the deep source
      --There is no such thing as a deep where clause.  Where clauses only limit the surface layer


      output               VARCHAR2(4000);
      layer_out            GZ_TYPES.GZ_LAYERS_OUT_INFO_REC;
      layer_subset         GZ_TYPES.GZ_LAYERS_SUBSET_REC;
      layer_in             GZ_TYPES.GZ_LAYERS_IN_REC;
      layer_hierarchical   GZ_TYPES.GZ_LAYERS_HIERARCHICAL_REC;
      layer_aggregate      GZ_TYPES.GZ_LAYERS_AGGREGATE_REC;
      layer_aggr_sources   GZ_TYPES.stringarray;
      v_table              VARCHAR2(32);
      v_key                VARCHAR2(32);
      v_where_clause       VARCHAR2(4000);
      v_depth              NUMBER;

   BEGIN

      IF p_what NOT IN ('TABLE','KEY')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'What the what is a ' || p_what || ' ? ');

      END IF;

      IF p_depth > 10
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Yo, depth is ' || p_depth || '. Thats too deep man!');

      ELSE

         v_depth := p_depth + 1;

      END IF;

      IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                    p_gen_project_id,
                                    p_layer)
      THEN

            --A FLOOR
            layer_in := GZ_OUTPUT.GET_LAYER_IN(p_release,
                                               p_gen_project_id,
                                               p_layer);

            v_table        := layer_in.source_table;
            v_key          := layer_in.source_key;

            --ever allow where clause here?

      ELSE

         layer_out := GZ_OUTPUT.GET_LAYER_OUT_INFO(p_output_topology,
                                                   p_layer);


         IF layer_out.layer_type = 'INITIAL'
         OR layer_out.layer_type = 'SUBSET'
         THEN

            --This layer is always built from "something else" deeper
            --Either an initial layer or some other output layer

            layer_subset := GZ_OUTPUT.GET_SUBSET_LAYER(p_release,
                                                       p_gen_project_id,
                                                       p_layer);

            IF layer_out.layer_type = 'INITIAL'
            THEN

               --THE FLOOR. Simple enough to get it here without the rabbit hole

               layer_in := GZ_OUTPUT.GET_LAYER_IN(p_release,
                                                  p_gen_project_id,
                                                  layer_subset.source); --subset.source --> in.layer

               v_table        := layer_in.source_table;
               v_key          := layer_in.source_key;

            ELSIF layer_out.layer_type = 'SUBSET'
            THEN

               --recursivityity

               v_table := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                    p_gen_project_id,
                                                    p_output_topology,
                                                    layer_subset.source,
                                                    'TABLE',
                                                    v_depth);

               v_key   := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                    p_gen_project_id,
                                                    p_output_topology,
                                                    layer_subset.source,
                                                    'KEY',
                                                    v_depth);

            END IF;

         ELSIF layer_out.layer_type = 'HIERARCHICAL'
         THEN

            --Hierarchical layers are built right at the surface

            layer_hierarchical := GZ_OUTPUT.GET_HIERARCHICAL_LAYER(p_release,
                                                                   p_gen_project_id,
                                                                   p_layer);

            v_table        := layer_hierarchical.source;
            v_key          := layer_hierarchical.source_key;

            /*Not the nester dummy
            IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                          p_gen_project_id,
                                          layer_hierarchical.nesting_layer)
            THEN

               --A FLOOR

               layer_in := GZ_OUTPUT.GET_LAYER_IN(p_release,
                                                  p_gen_project_id,
                                                  layer_hierarchical.nesting_layer);

               v_table        := layer_in.source_table;
               v_key          := layer_in.source_key;
               v_where_clause := layer_in.where_clause;


            ELSIF GZ_OUTPUT.LAYER_IS_HIERARCHICAL(p_release,
                                                  p_gen_project_id,
                                                  layer_hierarchical.nesting_layer)
            THEN

               --For hierarchical layers the depth should only ever be one down '
               --   if the nesting layer is also hierarchical
               --Ex For Divisions nesting on States, we dont want to recursively go down to
               --   the Counties on which the states rest and look for county.regionce

               layer_hierarchical := GZ_OUTPUT.GET_HIERARCHICAL_LAYER(p_release,
                                                                      p_gen_project_id,
                                                                      layer_hierarchical.nesting_layer);

               v_table        := layer_hierarchical.source;
               v_key          := layer_hierarchical.source_key;
               v_where_clause := layer_hierarchical.where_clause;

            ELSE

               --down the rabbit hole

               v_table := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                    p_gen_project_id,
                                                    p_output_topology,
                                                    layer_hierarchical.nesting_layer,
                                                    'TABLE',
                                                    v_depth);

               v_key :=   GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                    p_gen_project_id,
                                                    p_output_topology,
                                                    layer_hierarchical.nesting_layer,
                                                    'KEY',
                                                    v_depth);

               v_where_clause := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                           p_gen_project_id,
                                                           p_output_topology,
                                                           layer_hierarchical.nesting_layer,
                                                           'WHERE_CLAUSE',
                                                           v_depth);


            END IF;

            */

         ELSIF layer_out.layer_type = 'AGGREGATE'
         THEN

            layer_aggregate := GZ_OUTPUT.GET_AGGREGATE_LAYER(p_release,
                                                             p_layer);

            --source is comma delimited
            layer_aggr_sources := GZ_BUSINESS_UTILS.SPLIT(layer_aggregate.source, ',');

            IF layer_aggr_sources.COUNT <> 2
            THEN

               RAISE_APPLICATION_ERROR(-20001,'Layer ' || p_layer || ' source ' ||
                                              ' doesnt appear to have two comma delimited sources in gz_layers_aggregate');

            END IF;

            IF p_which NOT IN (1,2)
            THEN

               RAISE_APPLICATION_ERROR(-20001,'Bad p_which of ' || p_which);

            END IF;


            IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                          p_gen_project_id,
                                          layer_aggr_sources(p_which))
            THEN

               --A FLOOR. Most of the time here for aggregate

               layer_in := GZ_OUTPUT.GET_LAYER_IN(p_release,
                                                  p_gen_project_id,
                                                  layer_aggr_sources(p_which));

               v_table        := layer_in.source_table;
               v_key          := layer_in.source_key;

            ELSE

               --scary. Aggregate not built on initial

               v_table := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                    p_gen_project_id,
                                                    p_output_topology,
                                                    layer_aggr_sources(p_which),
                                                    'TABLE',
                                                    v_depth);

               v_key   := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                    p_gen_project_id,
                                                    p_output_topology,
                                                    layer_aggr_sources(p_which),
                                                    'KEY',
                                                    v_depth);


            END IF;


         ELSE

            --most likely a split layer got in here
            RAISE_APPLICATION_ERROR(-20001,'Not programmed');

         END IF;

      END IF;

      IF p_what = 'TABLE'
      THEN

         RETURN v_table;

      ELSIF p_what = 'KEY'
      THEN

         RETURN v_key;

      END IF;


   END GET_DEEP_SOURCE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_TOPO_TABLE_HIERARCHY (
      p_topology                 IN VARCHAR2,
      p_table_name               IN VARCHAR2,
      p_column_name              IN VARCHAR2 DEFAULT 'TOPOGEOM'
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 5/8/12
      --Think I want from high to low for purposes of deregistering a single hierarchy
      --Maybe parameterize

      psql                 VARCHAR2(4000);
      output               GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT a.table_name '
           || 'FROM user_sdo_topo_info a '
           || 'START WITH '
           || 'a.table_name = :p1 AND '
           || 'a.topology = :p2 AND '
           || 'a.column_name = :p3 '
           || 'CONNECT BY PRIOR '
           || 'a.tg_layer_id = a.child_layer_id AND '
           || 'a.topology = :p4 AND '
           || 'a.column_name = :p5 '
           || 'ORDER BY tg_layer_level DESC ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output USING UPPER(p_table_name),
                                                            UPPER(p_topology),
                                                            UPPER(p_column_name),
                                                            UPPER(p_topology),
                                                            UPPER(p_column_name);

      RETURN output;

   END GET_TOPO_TABLE_HIERARCHY;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_OUTPUT_TILES (
      p_topology                 IN VARCHAR2,
      p_release                  IN VARCHAR2,
      p_project_id               IN VARCHAR2,
      p_tile_kount               IN NUMBER,
      p_face_table               IN VARCHAR2
   ) RETURN GZ_TYPES.geomarray
   AS

      --Matt! 5/15/12
      --Output module isnt as tile friendly as input topo build
      --Grovel about here to find tiles
      --8/17/12 Dont use edge$, just get tiles around the perimeter.
      --        Switch to face feature
      --8/24/12 Dont use extent layer either, could be projected
      --9/05/12 Fix to use passed in table name, not topology_face

      output               GZ_TYPES.geomarray;
      psql                 VARCHAR2(4000);

   BEGIN

      --xxslower if no other optionxx best option
      --build tiles based on face table
      --doesnt seem too slow actually.  We will see

      output := GZ_BUILD_SOURCE.GZ_TILE_TABLE(p_face_table,
                                              p_tile_kount,
                                              'SDOGEOMETRY',
                                              NULL,  --no where clause
                                              NULL,  --whole topo, no filter
                                              'OUTPUT',
                                              p_topology);


      RETURN output;

   END GET_OUTPUT_TILES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_LAYER_SOURCES (
      p_topology                 IN VARCHAR2,
      p_layer                    IN VARCHAR2,
      p_which                    IN VARCHAR2 DEFAULT 'BASE'
   ) RETURN GZ_TYPES.stringhash
   AS

      --Matt! 1/07/13
      --Some complex layers are hodgepodges of sources and keys
      --And these layers are in turn sources for another layer
      --This helper gets the distinct set of sources/keys for a layer thats already built


      output               GZ_TYPES.stringhash;
      psql                 VARCHAR2(4000);
      sourcez              GZ_TYPES.stringarray;
      keyz                 GZ_TYPES.stringarray;

   BEGIN

      IF UPPER(p_which) NOT IN ('BASE','SUPERIOR')
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Which p_which is ' || p_which || '?');

      END IF;


      psql := 'SELECT DISTINCT SOURCE_' || p_which || ', key_' || p_which || ' '
           || 'FROM ' || p_topology || '_FSL' || p_layer || 'V';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO sourcez,
                                               keyz;

      FOR i IN 1 .. sourcez.COUNT
      LOOP

         output(sourcez(i)) := keyz(i);

      END LOOP;

      --Dont think I can error here when finding nothing, sometimes state runs or whatever
      --can be sparse for certain layers

      RETURN output;

   END GET_LAYER_SOURCES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE SET_LAYER_INFO_MODULE (
      p_topo                     IN VARCHAR2,
      p_module                   IN NUMBER,
      p_layer                    IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 5/4/12

      psql                 VARCHAR2(4000);

   BEGIN

      psql := 'UPDATE ' || p_topo || '_layers_out_info a '
           || 'SET '
           || 'a.module_status = :p1 ';

      IF p_layer IS NOT NULL
      THEN

         psql := psql || 'WHERE a.layer = :p2 ';

         EXECUTE IMMEDIATE psql USING p_module,
                                      UPPER(p_layer);

      ELSE

         EXECUTE IMMEDIATE psql USING p_module;

      END IF;

      COMMIT;

   END SET_LAYER_INFO_MODULE;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE SET_LAYER_LEVEL (
      p_topo                     IN VARCHAR2,
      p_layer                    IN VARCHAR2,
      p_level                    IN NUMBER
   )
   AS

      --Matt! 5/7/12

      psql                 VARCHAR2(4000);

   BEGIN

      psql := 'UPDATE ' || p_topo || '_layers_out_info a '
           || 'SET '
           || 'a.layer_level = :p1 '
           || 'WHERE '
           || 'a.layer = :p2 ';

      EXECUTE IMMEDIATE psql USING p_level,
                                   UPPER(p_layer);

      COMMIT;

   END SET_LAYER_LEVEL;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE ADD_COLUMN_TO_TABLE (
      p_tab                      IN VARCHAR2,
      p_col                      IN VARCHAR2,
      p_type                     IN VARCHAR2 DEFAULT 'VARCHAR2(4000)',
      p_replace                  IN VARCHAR2 DEFAULT 'Y'
   )
   AS

      --Matt! 5/15/12
      --No Like

      psql                    VARCHAR2(4000);

   BEGIN

      psql := 'ALTER TABLE ' || p_tab || ' ADD '
           || p_col || ' ' || p_type || ' ';

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS THEN

         IF SQLCODE = -1430
         AND p_replace = 'Y'
         THEN

            --This feels cheap
            --ORA-01430: column being added already exists in table

            EXECUTE IMMEDIATE 'ALTER TABLE ' || p_tab || ' DROP COLUMN ' || p_col;

            EXECUTE IMMEDIATE psql;

         ELSE

            RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

         END IF;

      END;

   END ADD_COLUMN_TO_TABLE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION LAYER_IS_INITIAL (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN BOOLEAN
   AS

      --Matt! 5/3/12

      psql                 VARCHAR2(4000);
      kount                PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(*) '
           || 'FROM gz_layers_in a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 AND '
           || 'a.layer = :p3 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_release,
                                              p_gen_project_id,
                                              p_layer;

      IF kount = 1
      THEN

         RETURN TRUE;

      ELSE

         RETURN FALSE;

      END IF;

   END LAYER_IS_INITIAL;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION LAYER_IS_HIERARCHICAL (
      p_release                  IN VARCHAR2,
      p_gen_project_id           IN VARCHAR2,
      p_layer                    IN VARCHAR2
   ) RETURN BOOLEAN
   AS

      --Matt! 5/30/12

      psql                 VARCHAR2(4000);
      kount                PLS_INTEGER;

   BEGIN

      psql := 'SELECT COUNT(*) '
           || 'FROM gz_layers_hierarchical a '
           || 'WHERE '
           || 'a.release = :p1 AND '
           || 'a.gen_project_id = :p2 AND '
           || 'a.layer = :p3 ';

      EXECUTE IMMEDIATE psql INTO kount USING p_release,
                                              p_gen_project_id,
                                              p_layer;

      IF kount = 1
      THEN

         RETURN TRUE;

      ELSE

         RETURN FALSE;

      END IF;

   END LAYER_IS_HIERARCHICAL;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION LEGAL_LAYER_TYPES
   RETURN GZ_TYPES.stringarray DETERMINISTIC
   AS

      --Matt! 5/3/12
      --The order is probably important

      output      GZ_TYPES.stringarray;

   BEGIN

      output(1) := 'INITIAL';
      output(2) := 'HIERARCHICAL';
      output(3) := 'SUBSET';
      output(4) := 'SPLIT';
      output(5) := 'AGGREGATE';


      RETURN output;

   END LEGAL_LAYER_TYPES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION LEGAL_WORK_COLUMNS
   RETURN GZ_TYPES.stringarray DETERMINISTIC
   AS

      --Matt! 5/11/12

      output      GZ_TYPES.stringarray;

   BEGIN

      output(1) := 'oid_base';
      output(2) := 'oid_superior';
      output(3) := 'source_base';
      output(4) := 'source_superior';
      output(5) := 'key_base';
      output(6) := 'split_primitives';

      RETURN output;

   END LEGAL_WORK_COLUMNS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION STRINGARRAY_SUBTRACT (
      p_input1          GZ_TYPES.stringarray,
      p_input2          GZ_TYPES.stringarray
   ) RETURN GZ_TYPES.stringarray
   AS

      --Matt! 6/18/12

      psql              VARCHAR2(4000);
      output            GZ_TYPES.stringarray;

   BEGIN

      psql := 'SELECT t1.column_value '
           || 'FROM TABLE(:p1) t1 '
           || 'MINUS '
           || 'SELECT t2.column_value '
           || 'FROM TABLE(:p2) t2 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO output
                                          USING GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(p_input1),
                                                GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(p_input2);

      --ok if output is empty?  Guess so

      RETURN output;

   END STRINGARRAY_SUBTRACT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION VERIFY_INPUTS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/3/12

      output               VARCHAR2(4000) := '0';
      cheker               VARCHAR2(4000);
      face_table           VARCHAR2(32);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_INPUTS: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VERIFY_INPUTS',NULL,'STARTING ' || p_output_topology);

      --do checks not part of parm checker first

      IF NOT GZ_TOPO_EXISTS(p_source_topology,p_source_schema)
      THEN

         output := output || 'Topo ' || p_source_schema || '.' || p_source_topology || ' doesnt exist| ';

      END IF;

      IF NOT GZ_TOPO_EXISTS(p_output_topology)
      THEN

         output := output || '|GZ topo ' || p_output_topology || ' doesnt exist| ';

      END IF;


      BEGIN

         --Check that we have a face table
         --and its the only non-layers_out table registered in the topology (in case of reruns)
         face_table := GZ_OUTPUT.GET_FACE_TABLE(p_release,
                                                p_gen_project_id,
                                                p_output_topology);

      EXCEPTION
      WHEN OTHERS
      THEN

         output := output || '|Attempt to get a face table returned ' || SQLERRM || '|';

      END;

      --What else

      --call the table parameter checker
      --It will check the entire release, not just the project
      --gonna say this is best parctice for now

      cheker := VERIFY_OUTPUT_PARMS(p_release);

      IF cheker <> '0'
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VERIFY_INPUTS',NULL,
                                                'VERIFY_OUTPUT_PARMS(''' || p_release || ''') returned bad--> ',
                                                NULL,NULL,NULL,NULL,NULL,cheker);

         output := output || ' ' || cheker;

      END IF;




      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VERIFY_INPUTS: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF output <> '0'
      THEN

         RETURN 'Dude, ' || output;

      ELSE

         RETURN '0';

      END IF;


   END VERIFY_INPUTS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION PIPE_LAYERS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_description        IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_add_to_face        IN VARCHAR2,
      p_sequence           IN NUMBER
   ) RETURN GZ_TYPES.GZ_LAYERS_OUT_INFO PIPELINED
   AS

      --Matt! 5/3/12


      output            GZ_TYPES.GZ_LAYERS_OUT_INFO_REC;
      hierarchical_rec  GZ_TYPES.GZ_LAYERS_HIERARCHICAL_REC;
      subset_rec        GZ_TYPES.GZ_LAYERS_SUBSET_REC;
      split_rec         GZ_TYPES.GZ_LAYERS_SPLIT_REC;
      aggregate_rec     GZ_TYPES.GZ_LAYERS_AGGREGATE_REC;
      sources           GZ_TYPES.stringarray;

   BEGIN

      --really just returning a single rec, not a table (for now at least)


      output.release        := UPPER(p_release);
      output.gen_project_id := UPPER(p_gen_project_id);
      output.layer          := UPPER(p_layer);
      output.description    := p_description;
      output.layer_type     := UPPER(p_layer_type);
      output.add_to_face    := p_add_to_face;

      --set to current module
      output.module_status := 2;

      --take a crack at the sequence and layer level

      IF p_layer_type = 'INITIAL'
      THEN

         output.layer_level    := 0;
         output.layer_sequence := p_sequence;

      ELSIF p_layer_type = 'HIERARCHICAL'
      THEN

         hierarchical_rec := GET_HIERARCHICAL_LAYER(p_release,p_gen_project_id,p_layer);

         IF LAYER_IS_INITIAL(p_release,p_gen_project_id,hierarchical_rec.nesting_layer)
         THEN

            --Hierarchical nesting on an initial, hierarchical is level 0
            --There is no layer in the topo to build on, just faces to construct
            output.layer_level := 0;
            --Good to go on sequence, good to go whenever
            output.layer_sequence := p_sequence;

         ELSE

             --Will be one above the nesting layer
             --But we may not have sussed out the nesting layer yet
             --Let the random try layer looper handle it layter
             --just to be clear I didnt forget
             output.layer_level := NULL;
             output.layer_sequence := NULL;

         END IF;

      ELSIF p_layer_type = 'SUBSET'
      THEN

         subset_rec := GET_SUBSET_LAYER(p_release,p_gen_project_id,p_layer);

         IF LAYER_IS_INITIAL(p_release,p_gen_project_id,subset_rec.source)
         THEN

            output.layer_level := NULL;
            --output.layer_level := 0; --think this is right
            output.layer_sequence := p_sequence;

         ELSE

            output.layer_level := NULL;
            output.layer_sequence := NULL;

         END IF;

      ELSIF p_layer_type = 'SPLIT'
      THEN

         split_rec := GET_SPLIT_LAYER(p_release,p_layer);

         IF  LAYER_IS_INITIAL(p_release,p_gen_project_id,split_rec.base_layer)
         AND LAYER_IS_INITIAL(p_release,p_gen_project_id,split_rec.superior_layer)
         THEN

            output.layer_level := NULL;
            --output.layer_level := 0;    --face-based set logic
            output.layer_sequence := p_sequence;

         ELSE

            output.layer_level := NULL;
            output.layer_sequence := NULL;

         END IF;

      ELSIF p_layer_type = 'AGGREGATE'
      THEN

         aggregate_rec := GET_AGGREGATE_LAYER(p_release,p_layer);
         sources       := GZ_BUSINESS_UTILS.SPLIT(aggregate_rec.source,',');

         IF  LAYER_IS_INITIAL(p_release,p_gen_project_id,sources(1))
         AND LAYER_IS_INITIAL(p_release,p_gen_project_id,sources(2))
         THEN

            output.layer_level := NULL;
            --output.layer_level := 1;    --also not sure if this is best
            output.layer_sequence := p_sequence;

         ELSE

            output.layer_level := NULL;
            output.layer_sequence := NULL;

         END IF;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'oops');

      END IF;


      PIPE ROW(output);

   END PIPE_LAYERS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION CREATE_OUTPUT_TYPES (
      p_release            IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 6/5/12
      --Two special types for use in split layers
      --Should exist except for very first time a job gets run in a schema

      psql           VARCHAR2(4000);
      output         VARCHAR2(4000) := '0';
      obj_new        PLS_INTEGER := 0; --assume no
      set_new        PLS_INTEGER := 0;

   BEGIN

      psql := 'CREATE OR REPLACE TYPE output_prim_obj AS OBJECT (face_id NUMBER)';

      BEGIN

         EXECUTE IMMEDIATE psql;

         --only get here on first run in a schema
         obj_new := 1;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -2303
         THEN

            --ORA-02303: cannot drop or replace a type with type or table dependents
            NULL;

         ELSE

            output := 'CREATE_OUTPUT_TYPES ''CREATE OR REPLACE TYPE output_prim_obj AS OBJECT (face_id NUMBER)'' '
                   || 'threw ' || SQLERRM;

            --get out
            RETURN output;

         END IF;

      END;

      psql := 'CREATE OR REPLACE TYPE output_prim_set AS TABLE OF output_prim_obj';

      BEGIN

         EXECUTE IMMEDIATE psql;

         --only get here on first run in a schema
         set_new := 1;

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -2303
         THEN

            --ORA-02303: cannot drop or replace a type with type or table dependents
            NULL;

         ELSE

            output := 'CREATE_OUTPUT_TYPES ''CREATE OR REPLACE TYPE output_prim_obj AS OBJECT (face_id NUMBER)'' '
                   || 'threw ' || SQLERRM;

            RETURN output;

         END IF;

      END;

      IF obj_new = 1
      OR set_new = 1
      THEN

         --if new grant execute
         --allows a remote schema to select * from the output tables when partially complete

         psql := 'GRANT EXECUTE ON output_prim_set TO "PUBLIC" ';

         BEGIN

            EXECUTE IMMEDIATE psql;

         EXCEPTION
         WHEN OTHERS
         THEN

            NULL;

         END;

         psql := 'GRANT EXECUTE ON output_prim_obj TO "PUBLIC" ';

         BEGIN

            EXECUTE IMMEDIATE psql;

         EXCEPTION
         WHEN OTHERS
         THEN

            NULL;

         END;

      END IF;

      RETURN output;


   END CREATE_OUTPUT_TYPES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION SET_UP (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_output_srid        IN NUMBER DEFAULT 8265,
      p_restart_flag       IN VARCHAR2 DEFAULT 'N',
      p_single_layer       IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! 5/3/12
      --Added gz_projection table and sequence creation 8/29/12

      output               VARCHAR2(4000) := '0';
      layers               GZ_TYPES.GZ_LAYERS_OUT;
      psql                 VARCHAR2(4000);
      layer_types          GZ_TYPES.stringarray;
      layer_sequence       PLS_INTEGER := 0;
      srid                 NUMBER;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('SET_UP: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SET_UP',NULL,'STARTING setup for ' || p_output_topology);

      IF p_restart_flag = 'N'
      THEN

         psql := 'SELECT srid FROM all_sdo_topo_info '
              || 'WHERE '
              || 'topology = :p1 AND '
              || 'owner = :p2 AND '
              || 'rownum = 1 ';

         EXECUTE IMMEDIATE psql INTO srid USING UPPER(p_source_topology),
                                                UPPER(p_source_schema);

         --create and drop work tables

         GZ_BUSINESS_UTILS.CREATE_GZ_LAYERS_OUT_GEOM(NULL, p_output_topology || '_LAYERS_OUT_SUPE', srid);

         GZ_BUSINESS_UTILS.CREATE_GZ_LAYERS_OUT_GEOM(NULL, p_output_topology || '_LAYERS_OUT_BASE', srid);

         GZ_BUSINESS_UTILS.CREATE_GZ_LAYERS_OUT_INFO(NULL, p_output_topology || '_LAYERS_OUT_INFO');

         GZ_BUSINESS_UTILS.CREATE_GZ_LAYERS_OUT_HELP(NULL, p_output_topology || '_LAYERS_OUT_HELP');

         IF p_output_srid IS NULL
         THEN

            --indicates Z9 albers 1000082 srid
            --plop out the pile of gz_projection work tables. We will at least re-use these for each layer now

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SET_UP',NULL,
                                                   'Looks like Z9. Creating buku tables for GZ_PROJECTION');

            GZ_BUSINESS_UTILS.CREATE_GZ_PROJECTION_WRK2003(SYS_CONTEXT('USERENV','CURRENT_SCHEMA'),p_output_topology || '_OUT_2003');
            GZ_BUSINESS_UTILS.CREATE_GZ_PROJECTION_WRKAREA(SYS_CONTEXT('USERENV','CURRENT_SCHEMA'),p_output_topology || '_OUT_AREA');
            GZ_BUSINESS_UTILS.CREATE_GZ_PROJECTION_WRKOUT(SYS_CONTEXT('USERENV','CURRENT_SCHEMA'),p_output_topology || '_OUT_Z9TMP');
            GZ_BUSINESS_UTILS.CREATE_GZ_PROJECTION_OUTPUT(SYS_CONTEXT('USERENV','CURRENT_SCHEMA'),p_output_topology || '_OUT_Z9OUT');

            BEGIN
               GZ_BUSINESS_UTILS.CREATE_PROJECTION_SEQ(p_output_topology || '_OUT_SEQ');
            EXCEPTION
            WHEN OTHERS
            THEN

               IF SQLCODE = -955
               THEN
                  --name is already used by an existing object
                  --no problem, Sidey says he can start at whatever the sequence is
                  NULL;
               ELSE
                  RAISE;
               END IF;

            END;

         END IF;

      END IF;

      --Populate tables as necessary

      layer_types := GZ_OUTPUT.LEGAL_LAYER_TYPES();

      psql := 'INSERT INTO ' || p_output_topology || '_LAYERS_OUT_INFO '
           || 'SELECT * FROM TABLE(GZ_OUTPUT.PIPE_LAYERS(:p1,:p2,:p3,:p4,:p5,:p6,:p7)) ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SET_UP',NULL,
                                             'Inserting records into work table ' || p_output_topology || '_LAYERS_OUT_INFO',
                                             NULL,NULL,NULL,psql);

      FOR i IN 1 .. layer_types.COUNT
      LOOP

         --get input layers
         --layer types is already ordered initial, hierarchical, subset, split, aggregate


         layers := GZ_OUTPUT.GET_LAYERS_OUT(p_release,
                                            p_gen_project_id,
                                            layer_types(i),
                                            p_single_layer);


         --Populate layer info

         FOR j IN 1 .. layers.COUNT
         LOOP

            layer_sequence := layer_sequence + 1;

            EXECUTE IMMEDIATE psql USING p_release,
                                         p_gen_project_id,
                                         layers(j).layer,
                                         layers(j).description,
                                         layers(j).layer_type,
                                         layers(j).add_to_face,
                                         layer_sequence;

         END LOOP;

         COMMIT;
         layers.DELETE;

      END LOOP;

      --create special object types if they dont exist
      --most always they should always exist in the schema here

      output := GZ_OUTPUT.CREATE_OUTPUT_TYPES(p_release);


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('SET_UP: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SET_UP',NULL,'COMPLETED setup for ' || p_output_topology);

      RETURN output;

   END SET_UP;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --PRIVATE--------------------------------------------------------------------------------

   PROCEDURE OUTPUT_CREATE_TABLE (
      p_table_name     IN VARCHAR2,
      p_sql            IN VARCHAR2,
      p_idx_col        IN VARCHAR2 DEFAULT NULL,
      p_pkc_col        IN VARCHAR2 DEFAULT NULL,
      p_uqc_col        IN VARCHAR2 DEFAULT NULL
   )
   AS

      --Matt! 5/4/12
      --Not sure if well be able to use indexes or keys

      psql             VARCHAR2(4000);


   BEGIN

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
               EXECUTE IMMEDIATE 'DROP TABLE ' || UPPER(p_table_name) || ' PURGE ';
               EXECUTE IMMEDIATE psql;

            ELSE

               RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.format_error_backtrace || chr(10) || psql );

            END IF;


      END;



      IF p_pkc_col IS NOT NULL
      AND p_uqc_col IS NULL
      THEN

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

      IF p_idx_col IS NOT NULL
      THEN

         GZ_BUSINESS_UTILS.ADD_INDEX(p_table_name,
                                p_table_name || 'IDX',
                                p_idx_col);

      END IF;

      GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS',p_table_name);



   END OUTPUT_CREATE_TABLE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE CREATE_LAYER_TABLE (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2
   )
   AS

      --Matt! 5/4/12

      psql                 VARCHAR2(4000);
      fields               GZ_TYPES.stringarray;
      length_hash          GZ_TYPES.stringhash;


   BEGIN


      --start
      psql := 'CREATE TABLE ' || p_output_topology || '_FSL' || p_layer || 'V ';

      --add my temp working columns

      psql := psql || '(oid_base VARCHAR2(4000), oid_superior VARCHAR2(4000), '
                   || 'source_base VARCHAR2(64), source_superior VARCHAR2(64), '
                   || 'key_base VARCHAR2(32), '  --schema.table
                   || 'split_primitives OUTPUT_PRIM_SET, ';

      fields := GZ_OUTPUT.GET_FIELDS(p_release,
                                     p_layer);

      length_hash := GZ_OUTPUT.GET_CROSSWALK_HASH(p_release);

      FOR i IN 1 .. fields.COUNT
      LOOP

         --GEO_ID VARCHAR2(60),

         psql := psql || fields(i) || ' VARCHAR2(' || length_hash(fields(i)) || '), ';

      END LOOP;

      --sdo and topo

      psql := psql || 'SDOGEOMETRY SDO_GEOMETRY, QA_SDOGEOMETRY SDO_GEOMETRY, TOPOGEOM SDO_TOPO_GEOMETRY) '
                   || 'NESTED TABLE split_primitives STORE AS ' || p_output_topology || '_FSL' || p_layer || 'V_STO ';

      --are there more columns? QC?  Dunno where to look

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                             'DDL for layer ' || p_layer,
                                             NULL,NULL,NULL,psql);

      GZ_OUTPUT.OUTPUT_CREATE_TABLE(p_output_topology || '_FSL' || p_layer || 'V',
                                    psql,
                                    'oid_base');


      --additional chucked in index on oid_superior

      GZ_BUSINESS_UTILS.ADD_INDEX(p_output_topology || '_FSL' || p_layer || 'V',
                             p_output_topology || '_FSL' || p_layer || 'V' || 'IDX2',
                             'OID_SUPERIOR');

      --additional chucked in bitmap indexes on source_base and source_superior

      GZ_BUSINESS_UTILS.ADD_INDEX(p_output_topology || '_FSL' || p_layer || 'V',
                             p_output_topology || '_FSL' || p_layer || 'V' || 'IDX3',
                             'SOURCE_BASE',
                             'BITMAP');

      GZ_BUSINESS_UTILS.ADD_INDEX(p_output_topology || '_FSL' || p_layer || 'V',
                             p_output_topology || '_FSL' || p_layer || 'V' || 'IDX4',
                             'SOURCE_SUPERIOR',
                             'BITMAP');

      --special index on storage name of the nested split_primitives table
      GZ_BUSINESS_UTILS.ADD_INDEX(p_output_topology || '_FSL' || p_layer || 'V_STO',    --nested table name, see create above
                             p_output_topology || '_FSL' || p_layer || 'V_STOIDX', --our name for idx
                             'FACE_ID');                                           --the (only) column in the output_prim_obj is defined as this


   END CREATE_LAYER_TABLE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION CREATE_LAYER (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_face_table         IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/4/12
      --Create the empty table with columns for this layer
      --Register in topology

      output               VARCHAR2(4000) := '0';
      psql                 VARCHAR2(4000);
      layer_level          NUMBER;
      child_layer          VARCHAR2(32);
      table_hierarchy      GZ_TYPES.stringarray;
      base_layer           VARCHAR2(32);
      superior_layer       VARCHAR2(32);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_LAYER: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,'STARTING create_layer for ' || p_layer);


      --Get layer level, should always be possible to GET it
      --maybe just not actually to make this layer yet, if its dependencies arent complete

      layer_level := GZ_OUTPUT.GET_LAYER_LEVEL(p_release,
                                               p_gen_project_id,
                                               p_output_topology,
                                               p_layer);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                             'Decided ' || p_layer || ' layer level is ' || layer_level);

      --drop from topo first, in case of rerun
      --must also drop any parents too

      --table_hierarchy will be ordered from high to low
      --From highest parent down to our current layer (if any)
      --Should not include any children down from the current layer
      table_hierarchy := GZ_OUTPUT.GET_TOPO_TABLE_HIERARCHY(p_output_topology,
                                                            p_output_topology || '_FSL' || p_layer || 'V');

      FOR i IN 1 .. table_hierarchy.COUNT
      LOOP

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                                'Deregister and drop ' || table_hierarchy(i),
                                                NULL,NULL,p_release);

         BEGIN

            GZ_TOPO_UTIL.DEREGISTER_FEATURE_TABLES(SYS_CONTEXT('USERENV','CURRENT_SCHEMA'),
                                                   p_output_topology,
                                                   'Y', --yeah, drop it
                                                   0,   --just go with 0, any greater than
                                                   table_hierarchy(i));
         EXCEPTION
         WHEN OTHERS
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                                   'Caught error deregistering ' || table_hierarchy(i) || ', must not be a rerun',
                                                   NULL,NULL,NULL,NULL,NULL, DBMS_UTILITY.format_error_backtrace);

         END;

      END LOOP;


      --Build the empty table

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                             'Creating table ' ||  p_output_topology || '_FSL' || p_layer || 'V');

      IF layer_level = 0
      AND GZ_OUTPUT.GET_LAYER_TYPE(p_output_topology,p_layer) = 'SPLIT'  --why not other layer types?
      THEN

         --however some split layers are dependent on hierarchical or other split layers (ick)
         --so kick them out early if necessary
         --for hierarchical layers we wait till below and rely on add_topo_geometry_layer to trigger kick
         base_layer := GZ_OUTPUT.GET_CHILD_LAYER(p_release,
                                                 p_gen_project_id,
                                                 p_output_topology,
                                                 p_layer,
                                                 'BASE_LAYER');

         superior_layer := GZ_OUTPUT.GET_CHILD_LAYER(p_release,
                                                     p_gen_project_id,
                                                     p_output_topology,
                                                     p_layer,
                                                     'SUPERIOR_LAYER');

         --Both base and superior must be either initial or in existence

         IF (NOT GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                            p_gen_project_id,
                                            base_layer)
             AND NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_output_topology || '_FSL' || base_layer || 'V',TRUE)) --allow empty tables
         OR (NOT GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                            p_gen_project_id,
                                            superior_layer)
             AND NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_output_topology || '_FSL' || superior_layer || 'V',TRUE)) --allow empty tables
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                                   'KICKING out, split dependency ' || base_layer ||
                                                   ' or ' || superior_layer || ' isnt built yet');

            RETURN '-1';

         END IF;

      END IF;

      GZ_OUTPUT.CREATE_LAYER_TABLE(p_release,
                                   p_gen_project_id,
                                   p_output_topology,
                                   p_layer);


      --register it

      IF layer_level = 0  --all 0s this should be set already from within GET_LAYER_LEVEL
      THEN


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                                'Adding to topology 0-level table ' ||  p_output_topology || '_FSL' || p_layer || 'V');

         GZ_TOPO_UTIL.GZ_ADD_TOPO_GEOMETRY_LAYER(p_output_topology,
                                                 p_output_topology || '_FSL' || p_layer || 'V',
                                                 'TOPOGEOM',
                                                 'POLYGON');

      ELSE

         --This is our key spot for kicking out layers that aren't ready yet
         --The child layer may not be registered in the topology

         --we at least get the name for sure
         child_layer := GZ_OUTPUT.GET_CHILD_LAYER(p_release,
                                                  p_gen_project_id,
                                                  p_output_topology,
                                                  p_layer);

         BEGIN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                                   'Adding to topology table ' ||  p_output_topology || '_FSL' || p_layer || 'V'
                                                || ' with child layer ' || child_layer);

            GZ_TOPO_UTIL.GZ_ADD_TOPO_GEOMETRY_LAYER(p_output_topology,
                                                    p_output_topology || '_FSL' || p_layer || 'V',
                                                    'TOPOGEOM',
                                                    'POLYGON',
                                                     p_output_topology || '_FSL' || child_layer || 'V');

         EXCEPTION
         WHEN OTHERS
         THEN

            IF SQLERRM LIKE '%No matching records found in user_sdo_topo_metadata%' --my error in GET_TG_LAYER_ID
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                                      'KICKING out, Child layer ' || child_layer || ' isnt built yet');

               RETURN '-1';

            ELSE

               RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.format_error_backtrace);

            END IF;

         END;

      END IF;


      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('CREATE_LAYER: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,'COMPLETE create_layer for ' || p_layer);

      RETURN output;

   END CREATE_LAYER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE INSERT_SUBSET_OIDS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,  --SUBSET or INITIAL
      p_face_table         IN VARCHAR2
   )
   AS

      --Matt! 5/7/12
      --For subset and initial layers
      --Insert the record oids into the output table
      --No topogeom yet

      --2/4/13 Added oid_clob functionality, but commented it since mid-production
      --6/10/13 turned on oid_clob

      psql                 VARCHAR2(4000);
      layer_subset         GZ_TYPES.GZ_LAYERS_SUBSET_REC;
      source_table         VARCHAR2(32);
      source_key           VARCHAR2(32);
      source_where_clause  VARCHAR2(4000);

   BEGIN

      IF p_layer_type NOT IN ('SUBSET','INITIAL')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Wut iz layur tipe ' || p_layer_type);

      END IF;

      layer_subset := GZ_OUTPUT.GET_SUBSET_LAYER(p_release,
                                                 p_gen_project_id,
                                                 p_layer);

      source_table := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                p_gen_project_id,
                                                p_output_topology,
                                                p_layer,
                                                'TABLE');

      source_key   := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                p_gen_project_id,
                                                p_output_topology,
                                                p_layer,
                                                'KEY');

      source_where_clause := layer_subset.where_clause;



      psql := 'INSERT /*+ APPEND */ INTO '
            || p_output_topology || '_FSL' || p_layer || 'V '
            || '(oid_base, source_base, key_base) '
            || 'SELECT DISTINCT ';

      --REMEMBER: Go to the layer we already have. Its universe is our input universe
      --However, if there's a whereclause just on THIS layer, then join back to the source

      IF p_layer_type = 'INITIAL'
      THEN

         --source key from FACE table, like COUNTY
         --from z609in_face

         psql := psql || 'f.' || layer_subset.source || ', '
                      || 'CAST(''' || p_source_schema || ''' || ''.'' || ''' || source_table || ''' AS VARCHAR2(64)), '
                      || 'CAST(''' || source_key || ''' AS VARCHAR2(32)) '
                      || 'FROM '
                      || p_face_table || ' f ';

      ELSIF p_layer_type = 'SUBSET'
      THEN

         --This is some other feature layer we have already built
         --usually has a where clause, but sometimes we get a 100 pct subset
         --ex z609in_050

         psql := psql || 'f.oid_base, '
                      || 'CAST(''' || p_source_schema || ''' || ''.'' || ''' || source_table || ''' AS VARCHAR2(64)), '
                      || 'CAST(''' || source_key || ''' AS VARCHAR2(32)) '
                      || 'FROM '
                      || p_output_topology || '_FSL' || layer_subset.source || 'V f ';

      END IF;

      --ex tab10st09.county a
      --must enforce the a alias on parameters


      IF source_where_clause IS NOT NULL    --means we are joining to bench source
      OR layer_subset.oid_clob IS NOT NULL  --ditto, we are matching bench source oids to this list
      OR p_layer_type = 'INITIAL'           --we at least need a NOT NULL where clause to the face table
      THEN

         --Some sort of where clause

         IF source_where_clause IS NOT NULL
         OR layer_subset.oid_clob IS NOT NULL
         THEN

            --must join to deep source

            psql := psql || ', '
                         || p_source_schema || '.' || source_table || ' a ';

         END IF;

         --always a where
         psql := psql || 'WHERE ';

         IF p_layer_type = 'INITIAL'
         AND source_where_clause IS NOT NULL
         THEN

            --initial + where clause
            --ensure face column is not null
            --and add where clause

            psql := psql || 'a.' || source_key || ' = f.' || layer_subset.source || ' AND '
                         || 'f.' || layer_subset.source || ' IS NOT NULL AND '
                         || '(' || source_where_clause || ') ';

         ELSIF p_layer_type = 'INITIAL'
         AND source_where_clause IS NULL
         THEN

             --initial, no where clause
             --just ensure face col is not null

              psql := psql || 'f.' || layer_subset.source || ' IS NOT NULL ';

              IF layer_subset.oid_clob IS NOT NULL
              THEN

                 --otherwise cartesian join
                 psql := psql || 'AND a.' || source_key || ' = f.' || layer_subset.source || ' ';

              END IF;

         ELSIF p_layer_type = 'SUBSET'
         AND source_where_clause IS NOT NULL
         THEN

            --subset with where clause
            --join on source key and add where clause

            psql := psql || 'a.' || source_key || ' = f.oid_base AND '
                         || '(' || source_where_clause || ') ';

         ELSIF p_layer_type = 'SUBSET'
         AND source_where_clause IS NULL
         THEN

            --subset with oid_clob, no where clause
            psql := psql || 'a.' || source_key || ' = f.oid_base ';

         ELSE

            RAISE_APPLICATION_ERROR(-20001, 'Woops');

         END IF;

         IF layer_subset.oid_clob IS NOT NULL
         THEN

            psql := psql || 'AND a.' || source_key || ' IN (SELECT * FROM TABLE(:p1))';

         END IF;


      END IF;


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'INSERT_SUBSET_OIDS',NULL,
                                             'Inserting oids for ' || p_layer_type || ' layer ' || p_layer,
                                             NULL,NULL,p_release,psql);

      IF layer_subset.oid_clob IS NULL
      THEN

         EXECUTE IMMEDIATE psql;
         COMMIT;

      ELSE

         EXECUTE IMMEDIATE psql USING GZ_BUSINESS_UTILS.CLOB_TO_VARRAY(layer_subset.oid_clob);
         COMMIT;

      END IF;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'INSERT_SUBSET_OIDS',NULL,
                                             'Completed inserting oids for ' || p_layer_type || ' layer ' || p_layer,
                                             NULL,NULL,p_release);


   END INSERT_SUBSET_OIDS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE INSERT_HIERARCHICAL_OIDS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,  --Hierarchical
      p_face_table         IN VARCHAR2
   )
   AS

      --Matt! 5/21/12
      --For hierarchical layers
      --Insert the record oids into the output table
      --No topogeom yet, thats next step

      psql                 VARCHAR2(4000);
      nesting_table        VARCHAR2(32);
      nesting_key          VARCHAR2(32);
      layer_hierarchical   GZ_TYPES.GZ_LAYERS_HIERARCHICAL_REC;

   BEGIN

      IF p_layer_type NOT IN ('HIERARCHICAL')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Wut iz layur tipe ' || p_layer_type);

      END IF;

      layer_hierarchical := GZ_OUTPUT.GET_HIERARCHICAL_LAYER(p_release,
                                                             p_gen_project_id,
                                                             p_layer);

      --make look like the other insert procedures
      --maybe combine some day

      nesting_table := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                 p_gen_project_id,
                                                 p_output_topology,
                                                 layer_hierarchical.nesting_layer,
                                                 'TABLE');

      nesting_key := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                               p_gen_project_id,
                                               p_output_topology,
                                               layer_hierarchical.nesting_layer,
                                               'KEY');

      --SOP
      --SELECT a.statefp, CAST ('TAB10ST09.STATE' AS VARCHAR2 (64))
      --FROM tab10st09.state a
      --WHERE a.vintage = '90'
      --and a.statefp IN (
      --select distinct a.statefp from
      --tab10st09.county a,
      --z609in_fsl050v b
      --where a.oid = b.oid_base
      --AND a.vintage = '90')

      --build on initial
      --INSERT   /*+ APPEND */
      --INTO   Z609IN_FSL040V (oid_base, source_base, key_base)
      --SELECT a.oid,
      --CAST ('TAB10ST09' || '.' || 'STATE' AS VARCHAR2 (64)),
      --CAST ('oid' AS VARCHAR2 (32))
      --FROM TAB10ST09.STATE a WHERE a.vintage=q'^90^'
      --AND a.statefp IN
      --(SELECT DISTINCT a.statefp FROM TAB10ST09.COUNTY a, Z609IN_FACE b WHERE a.oid = b.COUNTY)


      psql := 'INSERT /*+ APPEND */ INTO '
            || p_output_topology || '_FSL' || p_layer || 'V '
            || '(oid_base, source_base, key_base) '
            || 'SELECT a.' || layer_hierarchical.source_key || ', '
            || 'CAST(''' || p_source_schema || ''' || ''.'' || ''' || layer_hierarchical.source || ''' AS VARCHAR2(64)), '
            || 'CAST(''' || layer_hierarchical.source_key || ''' AS VARCHAR2(32)) '
            || 'FROM '
            || p_source_schema || '.' || layer_hierarchical.source || ' a ';

      IF layer_hierarchical.where_clause IS NOT NULL
      THEN

         psql := psql || 'WHERE '
                      || layer_hierarchical.where_clause || ' ';

      END IF;

      --Only grab oids from the original source for this layer
      --when we see a nesting match existing in the nester (either face or other layer)

      psql := psql || ' AND ' || layer_hierarchical.source_nesting_field || ' IN '  --a aliased in parms. SOURCE
                   || '(SELECT DISTINCT ' || layer_hierarchical.nesting_layer_field || ' '  --same, already aliased NESTING
                   || 'FROM '
                   || p_source_schema || '.' || nesting_table || ' a, ';


      IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                    p_gen_project_id,
                                    layer_hierarchical.nesting_layer)
      THEN

         psql := psql || p_face_table || ' b '
                      || 'WHERE '
                      || 'a.' || nesting_key || ' = b.' || layer_hierarchical.nesting_layer || ')';

                      --no worries about face table having NULL here

      ELSE

         psql := psql || p_output_topology || '_FSL' || layer_hierarchical.nesting_layer || 'V b '
                      || 'WHERE '
                      || 'a.' || nesting_key || ' = b.oid_base)';

      END IF;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'INSERT_HIERARCHICAL_OIDS',NULL,
                                             'Inserting oids for ' || p_layer_type || ' layer ' || p_layer,
                                             NULL,NULL,p_release,psql);

      EXECUTE IMMEDIATE psql;
      COMMIT;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'INSERT_HIERARCHICAL_OIDS',NULL,
                                             'Completed inserting oids for ' || p_layer_type || ' layer ' || p_layer,
                                             NULL,NULL,p_release);


   END INSERT_HIERARCHICAL_OIDS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE INSERT_AGGREGATE_OIDS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,  --Aggregate
      p_face_table         IN VARCHAR2
   )
   AS

      --Matt! 5/29/12
      --For aggregate layers
      --Insert the record oids into the output table
      --No topogeom yet, thats next step

      psql                 VARCHAR2(4000);
      source_tablez        GZ_TYPES.stringarray;
      source_keyz          GZ_TYPES.stringarray;
      source_whereclause   VARCHAR2(4000);
      layer_aggregate      GZ_TYPES.GZ_LAYERS_AGGREGATE_REC;
      sourcez              GZ_TYPES.stringarray;
      agg_where_clause     VARCHAR2(4000);

   BEGIN


      IF p_layer_type NOT IN ('AGGREGATE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Wut iz layur tipe ' || p_layer_type);

      END IF;

      layer_aggregate := GZ_OUTPUT.GET_AGGREGATE_LAYER(p_release,
                                                       p_layer);

      sourcez := GZ_BUSINESS_UTILS.SPLIT(layer_aggregate.source,',');

      IF sourcez.COUNT <> 2
      THEN

         --better have checked this somewhere else bub
         RAISE_APPLICATION_ERROR(-20001, 'Aggregate layer ' || p_layer || ' has ' || sourcez.COUNT || ' sources ');

      END IF;

      --make look like the other insert procedures
      --maybe combine some day

      --aggregate has no independent source, always built on the deep
      source_tablez(1) := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                    p_gen_project_id,
                                                    p_output_topology,
                                                    p_layer,
                                                    'TABLE',
                                                    0,
                                                    1);  --first one

      source_keyz(1) := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                  p_gen_project_id,
                                                  p_output_topology,
                                                  p_layer,
                                                  'KEY',
                                                  0,
                                                  1);


      source_tablez(2) := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                    p_gen_project_id,
                                                    p_output_topology,
                                                    p_layer,
                                                    'TABLE',
                                                    0,
                                                    2);

      source_keyz(2) := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                  p_gen_project_id,
                                                  p_output_topology,
                                                  p_layer,
                                                  'KEY',
                                                  0,
                                                  2);

      --just one shared where clause
      --I think its usually null. If not, has to apply to both sources
      source_whereclause := layer_aggregate.where_clause;


      --REMEMBER: Go to the .source layer we already have built.
      --Its universe is our input universe here
      --However, if there's an additional whereclause just on THIS layer, then join back to the deep source

      --INSERT   /*+ APPEND */
      --INTO  Z609IN_FSL160V (oid_base, source_base)
      --SELECT DISTINCT
      --    f.INCPLACE,
      --    CAST ('TAB10ST09' || '.' || 'INCPLACE' AS VARCHAR2 (64))
      --FROM Z609IN_FACE f

      FOR i IN 1 .. sourcez.COUNT
      LOOP

         --should just be 2 loops

         psql := 'INSERT /*+ APPEND */ INTO '
               || p_output_topology || '_FSL' || p_layer || 'V '
               || '(oid_base, source_base, key_base) '
               || 'SELECT DISTINCT ';

         --REMEMBER: Go to the layer we already have. It's universe is our input universe
         --However, if there's a whereclause just on THIS layer, then join back to the source

         IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                       p_gen_project_id,
                                       sourcez(i))
         THEN

            --source key from FACE table, like INCPLACE
            --from z609in_face

            psql := psql || 'f.' || sourcez(i) || ', '
                         || 'CAST(''' || p_source_schema || ''' || ''.'' || ''' || source_tablez(i) || ''' AS VARCHAR2(64)), '
                         || 'CAST(''' || source_keyz(i) || ''' AS VARCHAR2(32)) '
                         || 'FROM '
                         || p_face_table || ' f ';

         ELSE

            --This is some other feature layer we have already built
            --ex z609in_050

            psql := psql || 'f.oid_base, '
                         || 'CAST(''' || p_source_schema || ''' || ''.'' || ''' || source_tablez(i) || ''' AS VARCHAR2(64)), '
                         || 'CAST(''' || source_keyz(i) || ''' AS VARCHAR2(32)) ' --should be same as f.key_base, right?
                         || 'FROM '
                         || p_output_topology || '_FSL' || sourcez(i) || 'V f ';

         END IF;



         IF source_whereclause IS NOT NULL
         OR GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                       p_gen_project_id,
                                       sourcez(i))
         THEN

            --Go back to the source table
            --Join on key column AND whatever the user wants for just this layer


            IF source_whereclause IS NOT NULL
            THEN

               psql := psql || ', '
                            || p_source_schema || '.' || source_tablez(i) || ' a ';

            END IF;

            psql := psql || 'WHERE ';

            IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                          p_gen_project_id,
                                          sourcez(i))
            AND source_whereclause IS NOT NULL
            THEN

               --initial so not null on face
               --+ where clause

               psql := psql || 'a.' || source_keyz(i) || ' = f.' || sourcez(i) || ' '
                            || 'AND f.' || sourcez(i) || ' IS NOT NULL '
                            || 'AND (' || source_whereclause || ') ';

            ELSIF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                             p_gen_project_id,
                                             sourcez(i))
            AND source_whereclause IS NULL
            THEN

               --initial so not null on face
               --no other where clause, no join

               psql := psql || 'f.' || sourcez(i) || ' IS NOT NULL ';

            ELSE

               --not initial + where clause
               --always join

               psql := psql || 'a.' || source_keyz(i) || ' = f.oid_base '
                            || 'AND (' || source_whereclause || ') ';

            END IF;

         END IF;


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'INSERT_AGGREGATE_OIDS',NULL,
                                                'Inserting set ' || i || ' of  aggregate ' || sourcez(i) || ' oids for layer ' || p_layer,
                                                NULL,NULL,p_release,psql);

         EXECUTE IMMEDIATE psql;
         COMMIT;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'INSERT_AGGREGATE_OIDS',NULL,
                                                'Completed inserting aggregate ' || sourcez(i) || ' oids for layer ' || p_layer,
                                                NULL,NULL,p_release);

      END LOOP;


   END INSERT_AGGREGATE_OIDS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE INSERT_SPLIT_OIDS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,  --Split
      p_face_table         IN VARCHAR2
   )
   AS

      --Matt! 6/5/12
      --Insert base oids for split layers
      --8/17/12 updated for splits of splits
      --        Add phony base geo_id temporary pass through to split work table geo_id for processing assist

      --1/28/13 - Allow splits of splits to split deep split remainders
      --          By transferring oid_superior (onlys) --> oid_base at this step

      psql                 VARCHAR2(4000);
      layer_split          GZ_TYPES.GZ_LAYERS_SPLIT_REC;
      source_table         VARCHAR2(32);
      source_key           VARCHAR2(32);
      superior_source_key  VARCHAR2(32);
      deep_split           GZ_TYPES.GZ_LAYERS_SPLIT_REC;


   BEGIN



      IF p_layer_type NOT IN ('SPLIT')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Wut iz layur tipe ' || p_layer_type);

      END IF;

      layer_split := GZ_OUTPUT.GET_SPLIT_LAYER(p_release,
                                               p_layer);


      --no where clause for splits? Apparently never needed

      psql := 'INSERT /*+ APPEND */ INTO '
            || p_output_topology || '_FSL' || p_layer || 'V '
            || '(oid_base, source_base, key_base, geo_id) ' --temp phony geo_id
            || 'SELECT ';

      IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                    p_gen_project_id,
                                    layer_split.base_layer)
      THEN


         --get distinct oids from face

         --dont put this outside the IF. No work for aggregate
         source_table := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                   p_gen_project_id,
                                                   p_output_topology,
                                                   layer_split.base_layer,
                                                   'TABLE');

         source_key   := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                   p_gen_project_id,
                                                   p_output_topology,
                                                   layer_split.base_layer,
                                                   'KEY');

         psql := psql || 'DISTINCT '
                      || 'f.' || layer_split.base_layer || ', '
                      || 'CAST( ''' || p_source_schema || '.' || source_table || ''' AS VARCHAR2(64)), '   --already has the schema.table
                      || 'CAST(''' || source_key || ''' AS VARCHAR2(32)), '
                      || 'f.' || layer_split.base_layer || ' '  --doubly phony geoid.  Just stick the face value in there for consistency
                      || 'FROM '
                      || p_face_table || ' f '
                      || 'WHERE f.' || layer_split.base_layer || ' IS NOT NULL ' ;

      ELSE

         --dont bother with the deep source mess
         --if this is an aggregate layer (or another split) we need exactly whats in the table we created

         psql := psql || 'f.oid_base, '
                      || 'f.source_base, '
                      || 'f.key_base, '
                      || 'f.geo_id '
                      || 'FROM '
                      || p_output_topology || '_FSL' || layer_split.base_layer || 'V f '
                      || 'WHERE f.oid_base IS NOT NULL ';  --in case the input base is a split layer, we deal with remainders below


      END IF;


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'INSERT_SPLIT_OIDS',NULL,
                                             'Inserting split oids for ' || p_layer_type || ' layer ' || p_layer,
                                             NULL,NULL,p_release,psql);

      EXECUTE IMMEDIATE psql;
      COMMIT;


      IF NOT GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                        p_gen_project_id,
                                        layer_split.base_layer)
      THEN

         IF GZ_OUTPUT.GET_LAYER_TYPE(p_output_topology,
                                   layer_split.base_layer) = 'SPLIT'
         THEN

            --dreaded split of a split.
            --Any superior-only remainders from the deep split
            --need to be shifted over into base records for current split processing

            --get the deep split layer itself
            deep_split := GZ_OUTPUT.GET_SPLIT_LAYER(p_release,
                                                    layer_split.base_layer);

            --need to get the KEY for these records.  Not explicitly stored in the processing table
            superior_source_key := GZ_OUTPUT.GET_DEEP_SOURCE (p_release,
                                                              p_gen_project_id,
                                                              p_output_topology,
                                                              deep_split.superior_layer,
                                                              'KEY');

            psql := 'INSERT /*+ APPEND */ INTO '
               || p_output_topology || '_FSL' || p_layer || 'V '
               || '(oid_base, source_base, key_base, geo_id) ' --temp phony geo_id
               || 'SELECT ';

            --just to make it look like above, note however superior values are being selected and transferred to base

            psql := psql || 'f.oid_superior, '
                         || 'f.source_superior, '
                         || 'CAST(''' || superior_source_key || ''' AS VARCHAR2(32)), '
                         || 'f.geo_id '
                         || 'FROM '
                         || p_output_topology || '_FSL' || layer_split.base_layer || 'V f '
                         || 'WHERE f.oid_base IS NULL ';  --remainder records only

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'INSERT_SPLIT_OIDS',NULL,
                                                   'Inserting split of split remainder oids for ' || p_layer_type || ' layer ' || p_layer,
                                                   NULL,NULL,p_release,psql);

            EXECUTE IMMEDIATE psql;
            COMMIT;

         END IF;

      END IF;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'INSERT_SPLIT_OIDS',NULL,
                                             'Completed inserting oids for ' || p_layer_type || ' layer ' || p_layer,
                                             NULL,NULL,p_release);

   END INSERT_SPLIT_OIDS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE PROCESS_SPLITS (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,  --Split
      p_face_table         IN VARCHAR2,
      p_post_clip_flag     IN VARCHAR2 DEFAULT 'Y' --Future flag! not implemented
   )
   AS

      --Matt! 6/5/12
      --Splits and remainders are so special that they get an interim processing step
      --between insert oids and update topogeom

      --8/10/12
      --Updates for monstrosity that is a split of a split
      --Must use GEO_ID (now moved into layer-based processing) to uniquely ID
      -- a row in the superior or base source layer.  Base_oid and superior_oid
      -- values may be duplicated if theyve been splitted

      --8/17/12 updated for splits of splits. Sample SQL may no longer be up to date
      --        mainly involved adding geo_id to some of the where clauses
      --        In insert_split_oids we throw in the geo_id of the base along with
      --        its oid_base.  This allows for 1:1 joins back to the base layer
      --        When the base includes duplicate base_oids (because theyve been split)

      --8/30/12 Allow superior layers to be input.  Oversight

      --08/31/12 Allow BASES IN SPACES

      --January 2013 work on performance - helper tables and sdo_join haymaking
      --             added temp idx on face table when base or superior is initial
      --             Reworked split of split logic

      --1/28/13 - Allow splits of splits to split deep split remainders

      --10/23/13 - Getting incorrect buggy results on the multiset except.  Its clearing out 22,000 primitives
      --              from a superior record based on a handful of base primitives
      --           Rewrote both multiset excepts as MINUSes

      --Summary of steps. The numbers no longer match the comments in the code below

      --1. All base layer oids are already in our work table
      --   Fill helper tables
      --      xx_layers_out_supe gets all superior oids and sdogeometry from bench
      --      xx_layers_out_base gets all base oids and sdogeometry from bench
      --      xx_layers_out_help gets spatial cross product of oid<-->oid pairs that interact (inside, contains, etc)
      --2. Process record by record through the base oids
      --      Use xx_layers_out_help to determine oids of superior layers in the geom work table that overlap this base oid
      --      Fill the base oid split_primitives object with face_ids
      --      For any needed superior oid that isn't already in the processing table, add it and its primitives. Give it a null oid_base
      --      Multiset intersect the base record primitives which each of the superior oid primitives
      --       Each intersection results in an oid_base, oid_superior output with shared primitives in split_primitives
      --       Remove shared primitives from the base record split_primitives
      --       Remove shared primitives from the superior records split_primitives
      --      After intersecting all superiors for this base, it (base) should have empty primitives
      --       If empty, delete the starter base record (with null superior oid)
      --       If not empty, delete any way.  This happens a lot with splits of convenience
      --      Delete any superior records (Null base_oid, xxx superior_oid) record with empty primitives
      --3. If no remainders requested, delete all superior only records (Null base_oid, xxx superior_oid)
      --   If yes remainders, nothing, work is done later


      dbug                    PLS_INTEGER := 0;

      psql                    VARCHAR2(4000);
      layer_split             GZ_TYPES.GZ_LAYERS_SPLIT_REC;
      deep_split              GZ_TYPES.GZ_LAYERS_SPLIT_REC;
      superior_source_table   VARCHAR2(32);
      superior_source_key     VARCHAR2(32);
      deep_source_table       VARCHAR2(64) := 'XXXX';
      base_oidz               GZ_TYPES.stringarray;
      base_sourcez            GZ_TYPES.stringarray;
      base_keyz               GZ_TYPES.stringarray;
      base_geoidz             GZ_TYPES.stringarray;
      superior_oidz           GZ_TYPES.stringarray;
      new_superior_oidz       GZ_TYPES.stringarray;
      base_is_initial         PLS_INTEGER;
      superior_is_initial     PLS_INTEGER;
      card                    PLS_INTEGER;
      kount                   PLS_INTEGER;
      clipped_sqls            GZ_TYPES.stringarray;
      bases_in_spaces         PLS_INTEGER := 0;
      base_hash               GZ_TYPES.stringhash;
      pkey                    VARCHAR2(4000);
      temp_idxs               GZ_TYPES.stringarray;
      split_of_split          PLS_INTEGER := 0;
      insert_kount            PLS_INTEGER;
      --Detailed logging variables
      start_time              TIMESTAMP;
      end_time                TIMESTAMP;
      elapsed2AA              interval DAY(5) TO second (2) := NUMTODSINTERVAL (0,'Minute');
      elapsed3C               interval DAY(5) TO second (2) := NUMTODSINTERVAL (0,'Minute');
      elapsed3C1              interval DAY(5) TO second (2) := NUMTODSINTERVAL (0,'Minute');
      elapsed3D               interval DAY(5) TO second (2) := NUMTODSINTERVAL (0,'Minute');
      elapsed3E               interval DAY(5) TO second (2) := NUMTODSINTERVAL (0,'Minute');
      elapsed3F               interval DAY(5) TO second (2) := NUMTODSINTERVAL (0,'Minute');


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('PROCESS_SPLITS: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_layer_type NOT IN ('SPLIT')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Wut iz layur tipe ' || p_layer_type);

      END IF;

      layer_split := GZ_OUTPUT.GET_SPLIT_LAYER(p_release,
                                               p_layer);

      --do this once
      IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                    p_gen_project_id,
                                    layer_split.base_layer)
      THEN

         base_is_initial := 1;
         split_of_split := 0;  --cant be a split of a split

         --add an index on the face column that we are gonna be hitting on each loop
         --ex z699tm_merge_face.SDUNI

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                'Creating temp idx on ' || p_face_table || ' column ' || layer_split.base_layer,
                                                 NULL,NULL,p_release);

         GZ_BUSINESS_UTILS.ADD_INDEX(p_face_table,
                                p_face_table || SUBSTR(layer_split.base_layer,1,2) || 'X', --ex z699tm_merge_facesdx
                                UPPER(layer_split.base_layer));

         temp_idxs(temp_idxs.COUNT + 1) := p_face_table || SUBSTR(layer_split.base_layer,1,2) || 'X';

      ELSE

         base_is_initial := 0;

         IF GZ_OUTPUT.GET_LAYER_TYPE(p_output_topology,
                                     layer_split.base_layer) = 'SPLIT'
         THEN

            --dreaded split of a split. Gird loins and stuff
            split_of_split := 1;

            --if we are splitting remainders we need to join from an oid_base on this split
            --   (this is how we set it up in insert_split_oids)
            --to an oid_superior populated, oid_base null back in the deep split
            --Of course, most often We arent splitting remainders (either not requested or nonexistent)
            --if so we'll never match this source in the main loop below

            --get the deep split layer itself
            deep_split := GZ_OUTPUT.GET_SPLIT_LAYER(p_release,
                                                    layer_split.base_layer);

            --Note the source for the deep split superior layer in case it comes up in the loop
            deep_source_table := p_source_schema || '.' || GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                                                     p_gen_project_id,
                                                                                     p_output_topology,
                                                                                     deep_split.superior_layer,
                                                                                     'TABLE');

         END IF;

      END IF;

      --do this once
      IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                    p_gen_project_id,
                                    layer_split.superior_layer)
      THEN

         --Not sure if a superior as initial (face table only, no FSL made) has ever existed.

         superior_is_initial := 1;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                'Creating temp idx on ' || p_face_table || ' column ' || layer_split.superior_layer,
                                                 NULL,NULL,p_release);

         GZ_BUSINESS_UTILS.ADD_INDEX(p_face_table,
                                p_face_table || SUBSTR(layer_split.superior_layer,1,2) || 'X',
                                UPPER(layer_split.superior_layer));

         temp_idxs(temp_idxs.COUNT + 1) := p_face_table || SUBSTR(layer_split.superior_layer,1,2) || 'X';

      ELSE

         superior_is_initial := 0;

      END IF;

      --also these once. Superior layer is a vanilla layer, so we can do this deep source
      --back to the bench
      superior_source_table := GZ_OUTPUT.GET_DEEP_SOURCE (p_release,
                                                          p_gen_project_id,
                                                          p_output_topology,
                                                          layer_split.superior_layer,
                                                          'TABLE');

      superior_source_key   := GZ_OUTPUT.GET_DEEP_SOURCE (p_release,
                                                          p_gen_project_id,
                                                          p_output_topology,
                                                          layer_split.superior_layer,
                                                          'KEY');


      ---------------------------------------------------------------------------------
      --populate 2 work tables with superior and base geoms that are in play
      --this is for performance, so we dont sdo_relate against the nation repeatedly
      --And allows for sdo_join...
      --Then perform sdo_join to fill the 3rd helper table with cross product of oids
      ---------------------------------------------------------------------------------


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                             'Emptying helper tables',
                                             NULL,NULL,p_release);

      psql := 'DELETE FROM ' || p_output_topology || '_LAYERS_OUT_SUPE';

      EXECUTE IMMEDIATE psql;
      COMMIT;

      psql := 'DELETE FROM ' || p_output_topology || '_LAYERS_OUT_BASE';

      EXECUTE IMMEDIATE psql;
      COMMIT;

      psql := 'DELETE FROM ' || p_output_topology || '_LAYERS_OUT_HELP';

      EXECUTE IMMEDIATE psql;
      COMMIT;

      --MUST use the benchmark sdo
      --If a superior/base layer relationship has been clipped off
      --To the extent that the superior has no relationship at all to any base post-clip
      --We still need the superior (whatever faces remain in current topo) for remainders

      --------------------------------
      --Insert the superior geometries
      --------------------------------

      IF superior_is_initial = 0
      THEN

         --No, superior is not initial
         --Recall that the superior layer used as the source here may not have unique
         --oid values.  Only the geo_id is guaranteed to be unique.  This is why the DISTINCT subclause
         --before the join back to the benchmark

         --INSERT /* + APPEND */
         --INTO Z699TM_layers_out_supe
         --SELECT c.OID, c.sdogeometry
         --FROM ACS12.TRACT c where c.oid IN (
         --SELECT DISTINCT b.oid
         --FROM Z699TM_FSL140V a, ACS12.TRACT b
         --WHERE a.oid_base = b.OID)

         psql := 'INSERT /* + APPEND */ INTO '
               || p_output_topology || '_layers_out_supe '
               || 'SELECT c.' || superior_source_key || ', c.sdogeometry '
               || 'FROM ' || p_source_schema || '.' || superior_source_table || ' c '
               || 'WHERE c.' || superior_source_key || ' IN ('
               || 'SELECT DISTINCT b.' || superior_source_key || ' FROM '
               || p_output_topology || '_FSL' || layer_split.superior_layer || 'V a, '
               || p_source_schema || '.' || superior_source_table || ' b '
               || 'WHERE '
               || 'a.oid_base = b.' || superior_source_key || ') ';

      ELSE

         --yes superior is initial
         /* INSERT
               INTO   Z699IN_layers_out_sup
            SELECT b.OID, b.sdogeometry
              FROM ACS12.COUSUB b
             WHERE b.oid IN (SELECT DISTINCT cousub FROM Z699IN_FACE)
         */

         psql := 'INSERT /* + APPEND */ INTO '
               || p_output_topology || '_layers_out_supe '
               || 'SELECT b.' || superior_source_key || ', b.sdogeometry '
               || 'FROM '
               || p_source_schema || '.' || superior_source_table || ' b '
               || 'WHERE '
               || 'b.' || superior_source_key || ' IN '
               || '(SELECT DISTINCT ' || layer_split.superior_layer || ' FROM ' || p_face_table || ') ';

      END IF;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                             'Inserting superior geometries into ' || p_output_topology || '_layers_out_supe ',
                                             NULL,NULL,p_release,psql);

      EXECUTE IMMEDIATE psql;
      COMMIT;

      --------------------------------
      --Insert the base geometries
      --------------------------------

      --Base values are already in our current layer table. If initial or vanilla just one loop here
      --It may not be vanilla (like an aggregate of INCPLACE and CDP),
      --so have to get all possible sources and keys

      base_hash := GZ_OUTPUT.GET_LAYER_SOURCES(p_output_topology,
                                               p_layer,
                                               'BASE');

      --base_hash is like BAS13.ANRC=OID

      pkey := base_hash.FIRST;

      LOOP

         --usually just one loop
         EXIT WHEN NOT base_hash.EXISTS(pkey);

         --INSERT /* + APPEND */
         --INTO  Z699TM_layers_out_base
         --SELECT c.OID, c.sdogeometry
         --FROM ACS12.CDP c
         --WHERE c.oid IN
         --(SELECT DISTINCT b.oid
         --       FROM Z699TM_FSL080V a, ACS12.CDP b
         --      WHERE a.oid_base = b.OID AND a.source_base = 'ACS12.CDP')

         --Recall that the superior layer used as the source here may not have unique
         --oid values.  Only the geo_id is guaranteed to be unique.  This is why the DISTINCT subclause
         --for the join back to the benchmark

         psql := 'INSERT /* + APPEND */ INTO '
              || p_output_topology || '_layers_out_base '
              || 'SELECT c.' || base_hash(pkey) || ', c.sdogeometry '
              || 'FROM ' || pkey || ' c '
              || 'WHERE c.' || base_hash(pkey) || ' IN ('
              || 'SELECT DISTINCT b.' || base_hash(pkey) || ' '
              || 'FROM '
              || p_output_topology || '_FSL' || p_layer || 'V a, '
              || pkey || ' b '
              || 'WHERE '
              || 'a.oid_base = b.' || base_hash(pkey) || ' AND '
              || 'a.source_base = :p1 )';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                'Inserting base geometries into ' || p_output_topology || '_layers_out_base for source ' || pkey,
                                                NULL,NULL,p_release,psql);

         EXECUTE IMMEDIATE psql USING pkey;
         COMMIT;

         pkey  := base_hash.NEXT(pkey);

      END LOOP;

      --perform our sdo_join to make a cross product of all base to all superior sdogeometries
      --insert oid pairs into the helper table
      --put the base oid in the "oid_child" column since its indexed.  Kinda misleading
      --Recall that these are the spatial relationships between the benchmark shapes


      --insert /*+ APPEND */ into t699in_layers_out_help
      --SELECT /*+ ordered  */ b.oid oid_base, a.oid oid_superior
      --FROM TABLE(SDO_JOIN('T699IN_layers_out_supe', 'SDOGEOMETRY',
      --                'T699IN_layers_out_base', 'SDOGEOMETRY',
      --                'mask=INSIDE+COVEREDBY+CONTAINS+COVERS+OVERLAPBDYINTERSECT+EQUAL+OVERLAPBDYDISJOINT')) c,
      -- T699IN_layers_out_supe a,
      -- T699IN_layers_out_base b
      --WHERE c.rowid1 = a.rowid AND c.rowid2 = b.rowid


      psql := 'INSERT /*+ APPEND */ INTO ' || p_output_topology || '_layers_out_help '
           || 'SELECT /*+ ORDERED */ b.oid oid_base, a.oid oid_superior '
           || 'FROM '
           || 'TABLE(SDO_JOIN(:p1, :p2, :p3, :p4, :p5)) c, '
           || p_output_topology || '_layers_out_supe a, '
           || p_output_topology || '_layers_out_base b '
           || 'WHERE '
           || 'c.rowid1 = a.rowid AND '
           || 'c.rowid2 = b.rowid ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                             'Inserting sdo_join cross product of base and superior into ' || p_output_topology || '_layers_out_help',
                                             NULL,NULL,p_release,psql);

      EXECUTE IMMEDIATE psql USING p_output_topology || '_layers_out_supe',
                                   'SDOGEOMETRY',
                                   p_output_topology || '_layers_out_base',
                                   'SDOGEOMETRY',
                                   'mask=INSIDE+COVEREDBY+CONTAINS+COVERS+OVERLAPBDYINTERSECT+EQUAL+OVERLAPBDYDISJOINT';

      COMMIT;


      -------------------------------------SPLITS OF SPLITS NOTE---------------------------------------------------
      --   Gets klugey below on splits of splits.
      --   For example, an incplace may be split by cousubs and become a deep split input to the current processing
      --   In current processing that incplace/cousub is now being split that by tracts
      --   The original incplace oid spatially relates to some number of tracts, lets say 20
      --   But the deep split layer has that original incplace divided into some number of pieces, lets say 5.
      --       Each of those 5 pieces contains a subset of the total faces and territory representing the incplace
      --   As a result that incplace oid will get looped 5 times below.
      --   On each loop only some of the superior oids will actually contain faces that result in real overlap output
      --   So on the first base oid loop the code will process faces against all 20 tracts.
      --      But only some tract set intersections will result in output faces. The processed tracts will get deleted
      --      And then stupidly on the next oid_base loop repopulated AGAIN
      --      This is why splits of splits cant have remainders
      --   Proposed solution: Process by DISTINCT oid_base
      --                      Then an inner loop over all records with the current distinct oid_base
      --                      Holding the universe of superior oids fixed within that inner loop
      --------------------------------------SPLITS OF SPLITS NOTE---------------------------------------------------


      --Get all records in the base layer with enough info to uniquely ID them all


      psql := 'SELECT a.oid_base, a.source_base, a.key_base, a.geo_id '   --geo_id is temporarily filled with base-y values
           || 'FROM ' || p_output_topology || '_FSL' || p_layer || 'V a '
           || 'ORDER BY a.oid_base, a.source_base, a.geo_id ';  --constancy
                                                                --The order is good for splits of splits too.  May have oid_base dupes


      --Get our initial set.
      --Worry about size of these arrays later, but shouldnt ever be more than a few thousand right?
      --Also tricky to cursor since we are inserting and updating the table source in the looping below

      EXECUTE IMMEDIATE psql BULK COLLECT INTO base_oidz,
                                               base_sourcez,
                                               base_keyz,
                                               base_geoidz;

      FOR i IN 1 .. base_oidz.COUNT
      LOOP

         --Moved 3A ahead of 2

         --   3A. Use populated values in helper table to determine oids of superior layers
         --       That overlap this base
         --       REMEMBER:
         --       If a superior/base layer relationship has been clipped off
         --       To the extent that the superior has no relationship at all to any base post-clip
         --       We still need the superior (whatever faces remain in current topo) for remainders

         --Misleadingly, base is in oid_child of the helper, superior is in oid_parent
         --This table was originally set up for hierarchical helpers, and index is on oid_child


         psql := 'SELECT a.oid_parent '
              || 'FROM ' || p_output_topology ||'_layers_out_help a '
              || 'WHERE '
              || 'a.oid_child = :p1';

         IF i = 1
         OR dbug = 1
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                   'Logging on first loop, determine oids of superior layers that overlap this base oid ' || base_oidz(i),
                                                   NULL,NULL,p_release,psql);

         ELSIF MOD(i,1000) = 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                   'Processed 1000 more ' || p_layer || 's',
                                                    NULL,NULL,p_release);


         END IF;

         EXECUTE IMMEDIATE psql BULK COLLECT INTO superior_oidz USING base_oidz(i);

         IF superior_oidz.COUNT = 0
         THEN

            --I originally didnt think this was valid, but in ACS12 layer 172 split PLACES with CONCITYs
            --Concities are almost nonexistent.  Almost every place in the base is gonna be tossed
            --Ideally I'd like a flag indicating that this is OK and not an error
            --RAISE_APPLICATION_ERROR(-20001,'Didnt find any superior oids for base ' || base_oidz(i));

            bases_in_spaces := bases_in_spaces + 1;


            psql := 'DELETE FROM ' || p_output_topology || '_FSL' || p_layer || 'V v '
                 || 'WHERE '
                 || 'v.oid_base = :p1 AND '
                 || 'v.source_base = :p2 AND '
                 || 'v.geo_id = :p3 AND '
                 || 'v.oid_superior IS NULL ';

            IF bases_in_spaces < 25
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                      'WARNING: BASES IN SPACES: Base oid ' || base_oidz(i) || ' intersects no superior. Tossing it',
                                                      NULL,NULL,p_release,psql);

            ELSIF bases_in_spaces = 25
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                      'OK we''ve just tossed 25 bases in spaces, lets stop logging, there are probably lots',
                                                      NULL,NULL,p_release,psql);

            END IF;

            EXECUTE IMMEDIATE psql USING base_oidz(i),
                                         base_sourcez(i),
                                         base_geoidz(i);

            --Ive always wanted to use the new continue
            CONTINUE;

         END IF;


         --2. Process record by record through the base oids

         --2AA. Fill the base oid split_primitives object with face_ids

         start_time := systimestamp;

         IF base_is_initial = 0
         THEN

            --NOT initial, get faces from base layer

            IF split_of_split = 0                       --not split of a split
            OR deep_source_table <> base_sourcez(i)     --or if split of a split, still a standard join back to oid_base
            THEN

               --SOP 99.9 pct

               psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V v '
                    || 'SET v.split_primitives = '
                    || '(SELECT CAST ('
                    || '           MULTISET (SELECT sub.topo_id'
                    || '                         FROM (SELECT a.oid_base, t.topo_id '
                    || '                               FROM ' || p_output_topology || '_FSL' || layer_split.base_layer || 'V a, '
                    || '                                    TABLE (a.topogeom.get_topo_elements ()) t '
                    || '                              WHERE a.oid_base = :p1 AND a.geo_id = :p2) sub) AS OUTPUT_PRIM_SET) '
                    || '   FROM DUAL) '
                    || 'WHERE v.oid_base = :p3 AND v.geo_id = :p4 ';  --oid base + base geo_id temporarily in the geo_id column
                                                                      --is the only unique ID for complex splits as inputs

            ELSE

               --rare split of a split and this record is a remainder in the deep split
               --join btwn current base oid and oid_superior (+ geo_id) to get the deep split primitives

               psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V v '
                    || 'SET v.split_primitives = '
                    || '(SELECT CAST ('
                    || '           MULTISET (SELECT sub.topo_id'
                    || '                         FROM (SELECT a.oid_superior, t.topo_id '
                    || '                               FROM ' || p_output_topology || '_FSL' || layer_split.base_layer || 'V a, '
                    || '                                    TABLE (a.topogeom.get_topo_elements ()) t '
                    || '                              WHERE a.oid_superior = :p1 AND a.geo_id = :p2) sub) AS OUTPUT_PRIM_SET) '   -- <-- deep superior
                    || '   FROM DUAL) '
                    || 'WHERE v.oid_base = :p3 AND v.geo_id = :p4 ';  -- <-- join to current oid base


            END IF;


            IF i = 1 OR dbug = 1
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                      'Logging on first loop base oid ' || base_oidz(i) || ' geo_id ' || base_geoidz(i)
                                                      || ', populating split primitives for ' || p_layer || ' using base ' || layer_split.base_layer,
                                                       NULL,NULL,p_release,psql);

            END IF;

            BEGIN

               EXECUTE IMMEDIATE psql USING base_oidz(i),
                                            base_geoidz(i),
                                            base_oidz(i),
                                            base_geoidz(i);

               COMMIT;

            EXCEPTION
            WHEN OTHERS
            THEN

               --Very once in a million get connect by loop in user data on the get_topo_elements call to the child
               --This is almost impossible to mine out for a user, particularly since the layer with the problem is not the active layer
               --so leave breadcrumbs

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                           'Error with bind variables :p1 ' || base_oidz(i) || ' :p2 ' || base_geoidz(i)
                                                           || ' :p3 ' || base_oidz(i) || ' :p4 ' || base_geoidz(i),
                                                           NULL,NULL, p_release, psql,
                                                           p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

               RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            END;

         ELSE

            --initial, get faces from face table
            --this is not a complicated input (like split of split)


            psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V v '
                 || 'SET v.split_primitives = '
                 || '       (SELECT CAST ( '
                 || '                  MULTISET (SELECT f.face_id '
                 || '                              FROM ' || p_face_table || ' f '
                 || '                             WHERE f.' || layer_split.base_layer || ' = :p1) AS OUTPUT_PRIM_SET) '
                 || '          FROM DUAL) '
                 || 'WHERE v.oid_base = :p2 AND v.geo_id = :p3 ';

            IF i = 1 OR dbug = 1
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                      'Logging on first loop, populating split primitives for ' || p_layer || ' using base ' || layer_split.base_layer,
                                                       NULL,NULL,p_release,psql);

            END IF;

            BEGIN

               EXECUTE IMMEDIATE psql USING base_oidz(i),
                                            base_oidz(i),
                                            base_geoidz(i); --for initial bases this is just a duplicate of the base_oidz(i) value

               COMMIT;

            EXCEPTION
            WHEN OTHERS
            THEN

               --get topo elements connect by loop - possible breadcrumbs

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                           'Error with bind variables :p1 ' || base_oidz(i) || ' :p2 ' || base_oidz(i)
                                                           || ' :p3 ' || base_geoidz(i),
                                                           NULL,NULL, p_release, psql,
                                                           p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

               RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            END;

         END IF;

         -------------------------------------------------
         end_time := systimestamp;
         elapsed2AA := elapsed2AA + (end_time - start_time);
         IF MOD(i,1000) = 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                   'Step 2AA 1000 base recs cost ' || TO_CHAR(elapsed2AA),
                                                   NULL,NULL,p_release);

            elapsed2AA := NUMTODSINTERVAL (0,'Minute');

         END IF;
         ------------------------------------------------


         --   3C. If a superior oid isn't already in the processing table, add it and its primitives. Give it a null oid_base

         start_time := systimestamp;

         psql := 'SELECT t.column_value '
              || 'FROM TABLE(:p1) t '
              || 'WHERE '
              || 't.column_value NOT IN '
              || '(SELECT oid_superior FROM ' || p_output_topology || '_FSL' || p_layer || 'V '
              || ' WHERE oid_superior IS NOT NULL AND oid_base IS NULL) ';

         --seems like I could keep a running list of these instead of querying over and over
         --actually this is best because the list of oid superiors is constantly getting trimmed near the end of the loop
         EXECUTE IMMEDIATE psql BULK COLLECT INTO new_superior_oidz USING GZ_BUSINESS_UTILS.STRINGARRAY_to_varray(superior_oidz);

         ------------------------------------------------
         end_time := systimestamp;
         elapsed3C := elapsed3C + (end_time - start_time);
         IF MOD(i,1000) = 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                   'Step 3C 1000 base recs cost ' || TO_CHAR(elapsed3C),
                                                   NULL,NULL,p_release);

            elapsed3C := NUMTODSINTERVAL (0,'Minute');

         END IF;
         ------------------------------------------------



         --INSERT INTO Z609IN_FSL155V (oid_superior, source_superior, split_primitives)
         --VALUES (
         --         27590334171251,
         --         'TAB10ST09.COUNTY',
         --         (SELECT CAST (
         --                    MULTISET (
         --                       SELECT t.topo_id
         --                         FROM Z609IN_FSL050V a,
         --                              TABLE (a.topogeom.get_topo_elements ()) t
         --                        WHERE a.oid_base = 27590334171251) AS OUTPUT_PRIM_SET)
         --            FROM DUAL));

         IF new_superior_oidz.COUNT > 0
         THEN

            start_time := systimestamp;

            IF superior_is_initial = 0
            THEN

               psql := 'INSERT INTO ' || p_output_topology || '_FSL' || p_layer || 'V '
                    || '(oid_superior, source_superior, split_primitives) '
                    || '     VALUES (:p1,:p2,'
                    || '             (SELECT CAST ('
                    || '                     MULTISET ('
                    || '                     SELECT t.topo_id '
                    || '                     FROM ' || p_output_topology || '_FSL' || layer_split.superior_layer || 'V a, '
                    || '                          TABLE (a.topogeom.get_topo_elements ()) t '
                    || '                     WHERE a.oid_base = :p3) AS OUTPUT_PRIM_SET) FROM DUAL))';

            ELSE

               psql := 'INSERT INTO ' || p_output_topology || '_FSL' || p_layer || 'V '
                    || '(oid_superior, source_superior, split_primitives) '
                    || '     VALUES (:p1,:p2,'
                    || '             (SELECT CAST ('
                    || '                     MULTISET ('
                    || '                     SELECT f.face_id '
                    || '                     FROM ' || p_face_table || ' f '
                    || '                     WHERE f.' || layer_split.superior_layer || ' = :p3) AS OUTPUT_PRIM_SET) FROM DUAL))';


            END IF;

            IF i = 1 OR dbug = 1
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                      'Logging on first loop, add all superior primitives First superior is ' ||new_superior_oidz(1),
                                                       NULL,NULL,p_release,psql);

            END IF;


            FORALL ii IN 1 .. new_superior_oidz.COUNT
              EXECUTE IMMEDIATE psql USING new_superior_oidz(ii),
                                           p_source_schema || '.' || superior_source_table,  --not schema.table yet
                                           new_superior_oidz(ii);

            COMMIT;

            new_superior_oidz.DELETE;

            ------------------------------------------------
            end_time := systimestamp;
            elapsed3C1 := elapsed3C1 + (end_time - start_time);
            IF MOD(i,1000) = 0
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                      'Step 3C1 1000 base recs cost ' || TO_CHAR(elapsed3C1),
                                                      NULL,NULL,p_release);

               elapsed3C1 := NUMTODSINTERVAL (0,'Minute');

            END IF;
            ------------------------------------------------

         END IF;


         --   3D. Multiset intersect the base record primitives with each of the superior oid primitives
         --       Each intersection results in an oid_base, oid_superior output with shared primitives in split_primitives
         --       Or more rarely, nothing at all if the spatial overlap territory got clipped off (or is a split of a split territory)

         --   This is the sub step that takes most of the processing time
         --   Especially when the superior layer has lots (relatively, like 10s of thousands)
         --   of primitives, like when the superior is a state
         --   I havent come up with a strategy to improve it.  Thinking that maybe the set operators werent a good idea
         --   performance-wise



         ------------------------------------
         -- Start Inner loop over superiors
         ------------------------------------

         start_time := systimestamp;

         FOR ii IN 1 .. superior_oidz.COUNT
         LOOP

            --      INSERT INTO Z699TM_FSL080V (oid_base,
            --                            oid_superior,
            --                            source_base,
            --                            source_superior,
            --                            key_base,
            --                            geo_id,
            --                            split_primitives)
            --     (SELECT
            --               '27890102597693',
            --               '20790102600246',
            --               'ACS12.INCPLACE',
            --               'ACS12.TRACT',
            --               'OID',
            --               '0700000US210679203446027',
            --               (SELECT (SELECT split_primitives
            --                          FROM Z699TM_FSL080V
            --                         WHERE     oid_base = '27890102597693'
            --                               AND geo_id = '0700000US210679203446027'
            --                               AND oid_superior IS NULL)
            --                          MULTISET INTERSECT (SELECT split_primitives
            --                                                FROM Z699TM_FSL080V
            --                                               WHERE oid_superior = '20790102600246'
            --                                                     AND oid_base IS NULL)
            --                  FROM DUAL)     from dual)


            psql := 'INSERT INTO ' || p_output_topology || '_FSL' || p_layer || 'V '
                 || '(oid_base,oid_superior,source_base,source_superior,key_base,geo_id,split_primitives) '
                 || '(SELECT'
                 || ':p1,'
                 || ':p2,'
                 || ':p3,'
                 || ':p4,'
                 || ':p5,'
                 || ':p6,'
                 || '(SELECT (SELECT split_primitives '
                 || '     FROM ' || p_output_topology || '_FSL' || p_layer || 'V '
                 || '    WHERE     oid_base = :p7 '
                 || '          AND geo_id = :p8 '
                 || '          AND oid_superior IS NULL) '
                 || '     MULTISET INTERSECT (SELECT split_primitives '
                 || '                           FROM ' || p_output_topology || '_FSL' || p_layer || 'V '
                 || '                          WHERE oid_superior = :p9 '
                 || '                                AND oid_base IS NULL) '
                 || 'FROM DUAL) '
                 || 'FROM DUAL ';

            IF split_of_split = 0
            THEN

               --SOP
               psql := psql || ')';

            ELSE

               --for splits of splits go to the extra effort of only inserting
               --when the WHERE EXISTS evaluates to true
               --avoids inserting records where there is no actual spatial overlap (ie primitives no INTERSECT)
               --This (inserting null primitives) can rarely happen in non splits of splits above, but those are true clipped entities

               psql := psql || ' WHERE EXISTS ( '
                            || '    SELECT * FROM TABLE( '
                            || '    SELECT (SELECT split_primitives '
                            || '              FROM ' || p_output_topology || '_FSL' || p_layer || 'V '
                            || '             WHERE     oid_base = :p10 '
                            || '                   AND geo_id = :p11 '
                            || '                   AND oid_superior IS NULL) '
                            || '              MULTISET INTERSECT (SELECT split_primitives '
                            || '                                     FROM ' || p_output_topology || '_FSL' || p_layer || 'V '
                            || '                                   WHERE oid_superior = :p12 '
                            || '                                         AND oid_base IS NULL) '
                            || '      FROM DUAL) '
                            || '    ) '
                            || ') ';

            END IF;

            IF (i = 1
            AND ii = 1 ) OR dbug = 1
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                      'Logging on first loop, Multiset intersect the base record primitives with each of the superior oid primitives'
                                                      || ' base oid ' || base_oidz(i) || ', geo_id ' || base_geoidz(i)
                                                      || ', superior oid ' || superior_oidz(ii) ,
                                                       NULL,NULL,p_release,psql);

            END IF;


            IF split_of_split = 0
            THEN

               EXECUTE IMMEDIATE psql USING base_oidz(i),
                                            superior_oidz(ii),
                                            base_sourcez(i),                                  --schemadottable
                                            p_source_schema || '.' || superior_source_table,  --not schema dot, new
                                            base_keyz(i),
                                            base_geoidz(i), --temporary, from base layer
                                            base_oidz(i),
                                            base_geoidz(i),
                                            superior_oidz(ii);

               --Will assume this is always true for standard layers
               insert_kount := 1;

               COMMIT;

            ELSE

               EXECUTE IMMEDIATE psql USING base_oidz(i),
                                            superior_oidz(ii),
                                            base_sourcez(i),                                  --schemadottable
                                            p_source_schema || '.' || superior_source_table,  --not schema dot, new
                                            base_keyz(i),
                                            base_geoidz(i), --temporary, from base layer
                                            base_oidz(i),
                                            base_geoidz(i),
                                            superior_oidz(ii),
                                            base_oidz(i),
                                            base_geoidz(i),
                                            superior_oidz(ii);

               --is this cheap?
               --Must happen before the commit
               --Dont really care about the value (note that its limited to PLS_INTEGERS, goes negative if too big, weird
               --Just 0 or not zero to me
               insert_kount := SQL%ROWCOUNT;

               COMMIT;

            END IF;

            --remove those primitives from the superior record
            --that we just moved to the shared record


            --UPDATE z609in_fsl155v v
            --   SET v.split_primitives =
            --          (SELECT (SELECT split_primitives
            --                     FROM z609in_fsl155v
            --                    WHERE oid_superior = 27590334171251 AND oid_base IS NULL)
            --                     MULTISET EXCEPT (SELECT split_primitives
            --                                        FROM z609in_fsl155v
            --                                       WHERE oid_base = 28090334230863
            --                                             AND oid_superior = 27590334171251)
            --             FROM DUAL)
            -- WHERE v.oid_superior = 27590334171251 AND v.oid_base IS NULL

            IF insert_kount <> 0
            THEN

               /* This was working fine and then one day in one case it didnt
                  Result set after subtracting a handful of primitives from 22,000 primitives was null
                  Killing off multiset except and tabling() the primitives before a MINUS

               psql := 'UPDATE  ' || p_output_topology || '_FSL' || p_layer || 'V v '
                    || 'SET v.split_primitives = '
                    || '(SELECT (SELECT split_primitives '
                    || '         FROM ' || p_output_topology || '_FSL' || p_layer || 'V '
                    || '         WHERE oid_superior = :p1 AND oid_base IS NULL) '
                    || '      MULTISET EXCEPT '
                    || '         (SELECT split_primitives '
                    || '          FROM ' || p_output_topology || '_FSL' || p_layer || 'V '
                    || '          WHERE oid_base = :p2 AND geo_id = :p3 AND oid_superior = :p4) '
                    || 'FROM DUAL) '
                    || 'WHERE v.oid_superior = :p5 AND v.oid_base IS NULL ';
               */

               psql := 'UPDATE  ' || p_output_topology || '_FSL' || p_layer || 'V v '
                    || 'SET v.split_primitives = '
                    || '(SELECT CAST ( '
                    || '   MULTISET (SELECT f.face_id '
                    || '      FROM (SELECT b.face_id '
                    || '            FROM ' || p_output_topology || '_FSL' || p_layer || 'V a, '
                    || '                      TABLE (a.split_primitives) b '
                    || '               WHERE a.oid_superior = :p1 AND a.oid_base IS NULL '
                    || '            MINUS '
                    || '            SELECT b.face_id '
                    || '            FROM ' || p_output_topology || '_FSL' || p_layer || 'V a, '
                    || '                      TABLE (a.split_primitives) b '
                    || '               WHERE a.oid_base = :p2 AND a.geo_id = :p3 AND a.oid_superior = :p4 '
                    || '            ) f '
                    || '    ) AS OUTPUT_PRIM_SET) '
                    || 'FROM DUAL) '
                    || 'WHERE v.oid_superior = :p5 AND v.oid_base IS NULL ';

               IF (i = 1
               AND ii = 1) OR dbug = 1
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                         'Logging on first loop, Multiset except to remove the primitives from the superior record',
                                                          NULL,NULL,p_release,psql);

               END IF;


               EXECUTE IMMEDIATE psql USING superior_oidz(ii),
                                            base_oidz(i),
                                            base_geoidz(i),
                                            superior_oidz(ii),
                                            superior_oidz(ii);

               COMMIT;


               --       Remove shared primitives from the base record

               --delete from seed base using what weve added to the split record
               --UPDATE z609in_fsl155v v
               --   SET v.split_primitives =
               --          (SELECT (SELECT split_primitives
               --                     FROM z609in_fsl155v
               --                    WHERE oid_base = 28090334230863 AND oid_superior IS NULL)
               --                     MULTISET EXCEPT (SELECT split_primitives
               --                                        FROM z609in_fsl155v
               --                                       WHERE oid_superior = 27590334171251
               --                                             AND oid_base = 28090334230863)
               --             FROM DUAL)
               -- WHERE v.oid_base = 28090334230863 AND v.oid_superior IS NULL


               -- Didnt experience a problem with this one, like the above
               -- but rewrote with MINUS just to be safe
               /*
               psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V v '
                    || 'SET v.split_primitives = '
                    || '(SELECT (SELECT split_primitives '
                    || '         FROM ' || p_output_topology || '_FSL' || p_layer || 'V '
                    || '         WHERE oid_base = :p1 AND geo_id = :p2 AND oid_superior IS NULL) '
                    || '      MULTISET EXCEPT '
                    || '         (SELECT split_primitives '
                    || '          FROM ' || p_output_topology || '_FSL' || p_layer || 'V '
                    || '          WHERE oid_superior = :p3 AND oid_base = :p4 AND geo_id = :p5) '
                    || 'FROM DUAL) '
                    || 'WHERE '
                    || 'v.oid_base = :p6 AND v.geo_id = :p7 AND v.oid_superior IS NULL ';
               */

               psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V v '
                    || 'SET v.split_primitives = '
                    || '       (SELECT CAST ( '
                    || '                  MULTISET (SELECT f.face_id '
                    || '                              FROM (SELECT b.face_id '
                    || '                                      FROM ' || p_output_topology || '_FSL' || p_layer || 'V a, '
                    || '                                           TABLE (a.split_primitives) b '
                    || '                                     WHERE a.oid_base = :p1 AND a.geo_id = :p2 AND a.oid_superior IS NULL '
                    || '                                    MINUS '
                    || '                                    SELECT b.face_id '
                    || '                                      FROM ' || p_output_topology || '_FSL' || p_layer || 'V a, '
                    || '                                           TABLE (a.split_primitives) b '
                    || '                                     WHERE a.oid_superior = :p3 AND a.oid_base = :p4 AND a.geo_id = :p5 '
                    || '                                    ) f ) AS OUTPUT_PRIM_SET) '
                    || '          FROM DUAL) '
                    || 'WHERE v.oid_base = :p6 AND v.geo_id = :p7 AND v.oid_superior IS NULL ';


               IF (i = 1
               AND ii = 1) OR dbug = 1
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                         'Logging on first loop, subtract ie multiset except the base record primitives the superior oid primitives',
                                                          NULL,NULL,p_release,psql);

               END IF;

               EXECUTE IMMEDIATE psql USING base_oidz(i),
                                            base_geoidz(i),
                                            superior_oidz(ii),
                                            base_oidz(i),
                                            base_geoidz(i),
                                            base_oidz(i),
                                            base_geoidz(i);

               COMMIT;

            END IF;

         END LOOP;


         ----------------------------------
         --End inner loop over superiors
         ----------------------------------


         ------------------------------------------------
         end_time := systimestamp;
         elapsed3D := elapsed3D + (end_time - start_time);
         IF MOD(i,1000) = 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                   'Step 3D 1000 base recs cost ' || TO_CHAR(elapsed3D),
                                                   NULL,NULL,p_release);

            elapsed3D := NUMTODSINTERVAL (0,'Minute');

         END IF;
         ------------------------------------------------



         --   3E. After intersecting all superiors for this base, the base should usually have empty primitives
         --       delete the starter base record (with null superior oid)
         --       XXXIf not empty, raise an error (we may allow this some day, if bases extend outside of superiors)XXX
         --       Have to allow this for 321, where base places extend outside of their CBSAs
         --       Change to log warning, then delete as usual.  Would like a flag for this.
         --       This is different from BASES IN SPACES that got deleted above.  BASES IN SPACES have no relationship
         --          even in the benchmark
         --       New plan 12/28/12: This happens a lot with faux-split layers, aka "splits of convenience"
         --                 Ill leave the code commented as a reminder.  But trimming any repetitive SQL in this mess is best

         start_time := systimestamp;

         psql := 'DELETE FROM ' || p_output_topology || '_FSL' || p_layer || 'V v '
              || 'WHERE '
              || 'v.oid_base = :p1 AND '
              || 'v.geo_id = :p2 AND '
              || 'v.oid_superior IS NULL ';

         EXECUTE IMMEDIATE psql USING base_oidz(i),
                                      base_geoidz(i);
         COMMIT;

         ------------------------------------------------
         end_time := systimestamp;
         elapsed3E := elapsed3E + (end_time - start_time);
         IF MOD(i,1000) = 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                   'Step 3E 1000 base recs cost ' || TO_CHAR(elapsed3E),
                                                   NULL,NULL,p_release);

            elapsed3E := NUMTODSINTERVAL (0,'Minute');

         END IF;
         ------------------------------------------------


         --   3F. Delete any of this group of superior records (Null base_oid, xxx superior_oid) record with emptied out primitives

         start_time := systimestamp;

         psql := 'DELETE FROM ' || p_output_topology || '_FSL' || p_layer || 'V v '
              || 'WHERE '
              || 'v.oid_superior = :p1 AND '
              || 'v.oid_base IS NULL AND '
              || '(CARDINALITY(v.split_primitives) = :p2 OR '
              || 'CARDINALITY(v.split_primitives) IS NULL) ';

         FORALL ii IN 1 .. superior_oidz.COUNT
            EXECUTE IMMEDIATE psql USING superior_oidz(ii),
                                         0;

         COMMIT;


         ------------------------------------------------
         end_time := systimestamp;
         elapsed3E := elapsed3F + (end_time - start_time);
         IF MOD(i,1000) = 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                   'Step 3F 1000 base recs cost ' || TO_CHAR(elapsed3F),
                                                   NULL,NULL,p_release);

            elapsed3F := NUMTODSINTERVAL (0,'Minute');

         END IF;
         -------------------------------------------------



      END LOOP;


      -------------------------------
      ---END MAIN LOOP OVER BASE OIDS
      -------------------------------


      --4. If no remainders requested, delete all superior only records (Null base_oid, xxx superior_oid)
      --   If yes remainders, populate remainder columns for the superior-only records --> Actually lets save this for attributes

      IF layer_split.create_remainders = 'N'
      THEN

         psql := 'DELETE FROM ' || p_output_topology || '_FSL' || p_layer || 'V v '
              || 'WHERE v.oid_superior IS NOT NULL AND '
              || 'v.oid_base IS NULL ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                'Deleting superior-only remainders from ' || p_output_topology || '_FSL' || p_layer || 'V v ',
                                                 NULL,NULL,p_release,psql);

         EXECUTE IMMEDIATE psql;
         COMMIT;

      ELSIF layer_split.create_remainders = 'Y'
      THEN

         --Later I think
         NULL;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Sorry, I shoulda caught this sooner.  Unknown ' || p_layer || ' create_remainders option ' || layer_split.create_remainders);

      END IF;


      --checks?

      --ensure we have no records without primitives
      --these will be null topogeoms and null sdo messes in a hot minute
      psql := 'SELECT count(*) FROM ' || p_output_topology || '_FSL' || p_layer || 'V v '
           || 'WHERE '
           || 'CARDINALITY(v.split_primitives) = :p1 OR '
           || 'CARDINALITY(v.split_primitives) IS NULL ';

      EXECUTE IMMEDIATE psql INTO kount USING 0;

      IF kount > 0
      AND p_post_clip_flag = 'N'
      THEN

         --Never in here at the moment.  This flag is not implemented
         --In the future, if we see a base oid that didn't match any superior faces
         --And its pre-clip, then this is a mistake

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                'Error, we have split records with no primitives in ' || p_output_topology || '_FSL' || p_layer || 'V ',
                                                 NULL,NULL,p_release,psql);

         RAISE_APPLICATION_ERROR(-20001,'Error, we have split records with no primitives in ' || p_output_topology || '_FSL' || p_layer || 'V ');

      ELSIF kount > 0
      AND p_post_clip_flag = 'Y'
      THEN

         --We do in fact have some base-superior records that we thought were related in the bench
         --But at this moment after intersecting faces, we came up with nothing
         --This could/should be a relationship that exists in the benchmark but has been clipped off of our current face x face
         --Default behavior is this section.  This flag is not implemented as of SD and ACS12 production
         --Eventually we can even have fancy "is the missing relationship outside the clip outline" checks

         psql := 'SELECT '
              || '''SELECT sdogeometry FROM '' || v.source_base || '' WHERE '' || v.key_base || '' = '' || v.oid_base '
              || ' || '' | '' || '
              || '''SELECT sdogeometry FROM '' || v.source_superior || '' WHERE oid = '' || v.oid_superior '
              || 'FROM ' || p_output_topology || '_FSL' || p_layer || 'V v '
              || 'WHERE '
              || 'CARDINALITY(v.split_primitives) = :p1 OR '
              || 'CARDINALITY(v.split_primitives) IS NULL ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO clipped_sqls USING 0;

         FOR i IN 1 .. clipped_sqls.COUNT
         LOOP

            --Write special log record. Put the clipped off SQLs in the sql_stmt column
            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                   '<clipped entity>',
                                                    NULL,NULL,p_release,clipped_sqls(i));

         END LOOP;

         --Go ahead and delete.  Hopefully no damage, but this could be a huge mistake too

         psql := 'DELETE FROM ' || p_output_topology || '_FSL' || p_layer || 'V v '
              || 'WHERE '
              || 'CARDINALITY(v.split_primitives) = :p1 OR '
              || 'CARDINALITY(v.split_primitives) IS NULL ';

         EXECUTE IMMEDIATE psql USING 0;
         COMMIT;


      END IF;

      --if we made any temp idxs on the face table drop them

      FOR i IN 1 .. temp_idxs.COUNT
      LOOP

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PROCESS_SPLITS',NULL,
                                                'Dropping temp idx on ' || p_face_table || ', ' || temp_idxs(i),
                                                 NULL,NULL,p_release);

         EXECUTE IMMEDIATE 'DROP INDEX ' || temp_idxs(i);

      END LOOP;


      --leave the temporary base geo_ids in the work table since we may need them in attribute update

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('PROCESS_SPLITS: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END PROCESS_SPLITS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE UPDATE_SUBSET_TOPOGEOM (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,  --SUBSET or INITIAL
      p_face_table         IN VARCHAR2
   )
   AS

      --Matt! 5/8/12
      --This is a bit of a mess
      --For subset and initial layers
      --Update topogeom for already populated oids
      --Using constructors or create_feature

      psql                 VARCHAR2(4000);
      psql2                VARCHAR2(4000);
      layer_subset         GZ_TYPES.GZ_LAYERS_SUBSET_REC;
      my_cursor            SYS_REFCURSOR;
      oidz                 GZ_TYPES.stringarray;
      oidz2                GZ_TYPES.stringarray;
      facez                GZ_TYPES.stringarray;
      tg_layer_id          NUMBER;
      id_kount             PLS_INTEGER := 0;
      prim_kount           PLS_INTEGER := 0;
      kurrent_kount        PLS_INTEGER := 0;
      current_id           NUMBER;
      --
      TYPE my_group_rec    IS RECORD (
                           feat_id  NUMBER,
                           prim_ids GZ_TYPES.stringarray
      );
      TYPE my_group_arr    IS TABLE of my_group_rec INDEX BY PLS_INTEGER;
      my_groups            my_group_arr;
      --
      TYPE stupidrec       IS RECORD (
                           feat_id NUMBER,
                           prim_ids SDO_TOPO_OBJECT_ARRAY );
      TYPE stupid_arr      IS TABLE OF stupidrec INDEX BY PLS_INTEGER;
      stupid_array         stupid_arr;

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('UPDATE_SUBSET_TOPOGEOM: starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_layer_type NOT IN ('SUBSET','INITIAL')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Wut iz layur tipe ' || p_layer_type);

      END IF;

      layer_subset := GZ_OUTPUT.GET_SUBSET_LAYER(p_release,
                                                 p_gen_project_id,
                                                 p_layer);

      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_output_topology,
                                                  p_output_topology ||  '_FSL' || p_layer || 'V',
                                                  'TOPOGEOM',
                                                  'POLYGON');

      IF p_layer_type = 'INITIAL'
      THEN

         psql := 'SELECT a.oid_base, f.face_id  '
              || 'FROM '
              || p_output_topology || '_FSL' || p_layer || 'V a, '
              || p_face_table || ' f '
              || 'WHERE '
              || 'a.oid_base = f.' || layer_subset.source || ' ' --must include since we may have limited
              || 'ORDER BY a.oid_base ';                         --the universe in previous step with a where clause

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_SUBSET_TOPOGEOM',NULL,
                                                'Opening cursor to collect face ids for ' || p_layer,
                                                NULL,NULL,p_release,psql);

         OPEN my_cursor FOR psql;

         LOOP

            FETCH my_cursor BULK COLLECT INTO oidz, facez LIMIT 10000;  --Bumping this up, its faces, not features
            EXIT WHEN oidz.COUNT = 0;

            FOR i in 1 .. oidz.COUNT
            LOOP

               --Have to set this up first so the stupid array can get EXTENDED
               --with the correct count
               --Theres SQL to do this, return soon

               IF i = 1
               THEN

                   --counter and loop cleanup
                   id_kount := 1;
                   prim_kount := 1;
                   current_id := oidz(i);
                   my_groups.DELETE;

                   my_groups(id_kount).feat_id := oidz(i);
                   my_groups(id_kount).prim_ids(prim_kount) := facez(i);

               ELSIF oidz(i) = current_id
               THEN

                  prim_kount := prim_kount + 1;

                  my_groups(id_kount).prim_ids(prim_kount) := facez(i);

               ELSIF oidz(i) <> current_id
               THEN

                  id_kount := id_kount + 1;
                  prim_kount := 1;
                  current_id := oidz(i);

                  my_groups(id_kount).feat_id := oidz(i);
                  my_groups(id_kount).prim_ids(prim_kount) := facez(i);

               ELSE

                  RAISE_APPLICATION_ERROR(-20001,'woops');

               END IF;

            END LOOP;

            FOR j IN 1 .. my_groups.COUNT
            LOOP

               IF j = 1
               THEN

                  --cleanup here for re loops
                  kurrent_kount := 0;
                  stupid_array.DELETE;

               END IF;

               kurrent_kount := kurrent_kount + 1;

               --place the feature id in the outer record id
               stupid_array(kurrent_kount).feat_id := my_groups(j).feat_id;
               --stupid extend the nested object
               stupid_array(kurrent_kount).prim_ids := SDO_TOPO_OBJECT_ARRAY();
               stupid_array(kurrent_kount).prim_ids.EXTEND(my_groups(j).prim_ids.COUNT);

               --stupid objects
               FOR jj IN 1 .. my_groups(j).prim_ids.COUNT
               LOOP

                  --prim_ids: SDO_TOPO_OBJECT_ARRAY is VARRAY (1000000) of SDO_TOPO_OBJECT
                  stupid_array(kurrent_kount).prim_ids(jj) := SDO_TOPO_OBJECT(my_groups(j).prim_ids(jj), 3);

               END LOOP;

            END LOOP;


            --Update this batch
            --Note that there may be overlaps on base_oids between the outer cursor BULK COLLECTs
            --This is ok, just another few face_id adds to the same oid_base


            psql2 := 'UPDATE ' || p_output_topology ||  '_FSL' || p_layer || 'V a '
                  || 'SET '
                  || 'a.topogeom = SDO_TOPO_GEOMETRY(:p1,:p2,:p3,:p4,NULL) '  --MUST have explicit null in delete position
                  || 'WHERE a.oid_base = :p5 ';

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_SUBSET_TOPOGEOM',NULL,
                                                   'Calling constructor for ' || stupid_array.COUNT || ' records on ' || p_layer,
                                                   NULL,NULL,p_release,psql2);

            FORALL ii IN 1 .. stupid_array.COUNT
               EXECUTE IMMEDIATE psql2
               USING p_output_topology,
                     3,
                     tg_layer_id,
                     stupid_array(ii).prim_ids,
                     stupid_array(ii).feat_id;

            COMMIT;


         END LOOP;

         CLOSE my_cursor;

      ELSIF p_layer_type = 'SUBSET'
      THEN

         --there should be a direct relationship
         --between a child table with some universe of oid_base
         --and our current parent table with a subset of those oid_bases BELONG TO US

         psql := 'SELECT a.oid_base '
              || 'FROM ' || p_output_topology ||  '_FSL' || p_layer || 'V a ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_SUBSET_TOPOGEOM',NULL,
                                                'Opening cursor to collect oids for ' || p_layer,
                                                NULL,NULL,p_release,psql);

         --set up parent type create feature
         psql2 := 'UPDATE ' || p_output_topology ||  '_FSL' || p_layer || 'V a '
               || 'SET '
               || 'a.topogeom = SDO_TOPO_MAP.CREATE_FEATURE(:p1,:p2,:p3,:p4) '
               || 'WHERE a.oid_base = :p5 ';


         OPEN my_cursor FOR psql;

         LOOP

            FETCH my_cursor BULK COLLECT INTO oidz LIMIT 1000;
            EXIT WHEN oidz.COUNT = 0;

            FOR i in 1 .. oidz.COUNT
            LOOP

               --silly PLS-00440: FORALL bulk IN-bind variables cannot be used here

               --dml to implicit child
               oidz2(i) := 'oid_base=''' || oidz(i) || '''';

            END LOOP;

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_SUBSET_TOPOGEOM',NULL,
                                                   'Calling parents-style create_feature for ' || oidz.COUNT || ' records on ' || p_layer,
                                                   NULL,NULL,p_release,psql2);

            FORALL ii IN 1 .. oidz.COUNT
               EXECUTE IMMEDIATE psql2 USING p_output_topology,
                                             p_output_topology ||  '_FSL' || p_layer || 'V',  --current table
                                             'TOPOGEOM',
                                             oidz2(ii),     --dml to implicit child
                                             oidz(ii);
            COMMIT;

         END LOOP;

         CLOSE my_cursor;

      END IF;


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_SUBSET_TOPOGEOM',NULL,
                                             'Complete for ' || p_layer,
                                             NULL,NULL,p_release);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('UPDATE_SUBSET_TOPOGEOM: peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END UPDATE_SUBSET_TOPOGEOM;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   FUNCTION REPLACE_A_ALIAS (
      p_string                IN VARCHAR2,
      p_replace               IN VARCHAR2 DEFAULT 'b'
   ) RETURN VARCHAR2
   AS

      --Matt! 1/3/13

      tokenz            GZ_TYPES.stringarray;
      output            VARCHAR2(4000) := '';

   BEGIN

      --split on ||s first.  May not be any, tis fine

      tokenz := GZ_BUSINESS_UTILS.SPLIT(p_string,'\|\|');

      FOR i IN 1 .. tokenz.COUNT
      LOOP

         IF i > 1
         THEN

            output := output || '||';

         END IF;

         IF INSTR(tokenz(i),'a.') = 1
         THEN

            output := output || REPLACE(tokenz(i), 'a.', p_replace || '.');


         ELSE

            --do no harm. Could get more clever
            output := output || tokenz(i);

         END IF;

      END LOOP;

      RETURN output;

   END REPLACE_A_ALIAS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   PROCEDURE POPULATE_HIERARCHICAL_HELPER (
      p_output_topology       IN VARCHAR2,
      p_parent_fsl            IN VARCHAR2,
      p_child_fsl             IN VARCHAR2,
      p_parent_nesting        IN VARCHAR2,
      p_child_nesting         IN VARCHAR2
   )
   AS

      --Matt! 1/03/13

      --Higher level hierarchical is not performative (for big table joins) without use of this special helper table
      -- xx_LAYERS_OUT_HELP
      --contains oid_child, oid_parent
      --    ex   BLKGRP_A    TRACT_Z
      --         BLKGRP_B    TRACT_Z
      --         BLKGRP_C    TRACT_Y

      psql                    VARCHAR2(4000);
      parent_source_table     VARCHAR2(64);
      child_source_table      VARCHAR2(64);
      child_nesting           VARCHAR2(4000);

   BEGIN

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                             'POPULATE_HIERARCHICAL_HELPER starting with some cleanup',
                                              NULL,NULL,NULL);

      --Truncate in case someone else has been through here
      psql := 'TRUNCATE TABLE ' || p_output_topology || '_LAYERS_OUT_HELP';
      EXECUTE IMMEDIATE psql;

      --update the child table to all null
      psql := 'UPDATE ' || p_child_fsl || ' a '
           || 'SET a.oid_superior = NULL ';

      EXECUTE IMMEDIATE psql;
      COMMIT;

      --Make sure just one source for this current layer
      --This is an assumption for hierarchical, that the source is initial, input, subset, or hierarchical
      --If multiple sources for the base are possible, new code here

      psql := 'SELECT DISTINCT a.source_base '
           || 'FROM ' || p_parent_fsl || ' a ';

      EXECUTE IMMEDIATE psql INTO parent_source_table;

      --Get the child source
      --Same caveats as above

      psql := 'SELECT DISTINCT a.source_base '
           || 'FROM ' || p_child_fsl || ' a ';

      EXECUTE IMMEDIATE psql INTO child_source_table;

      --since the both the nesting parameters are a-aliased, need to switcheroo child to B
      child_nesting := GZ_OUTPUT.REPLACE_A_ALIAS(p_child_nesting, 'b');


      --insert into z699tm_LAYERS_OUT_HELP
      --select d.oid_base oid_parent, c.oid_base oid_child from
      --acs12.tract a,
      --acs12.blkgrp b,
      --z699tm_fsl140v c,
      --z699tm_fsl150v d
      --where
      --c.oid_base = a.oid and
      --d.oid_base = b.oid and
      --a.statefp || a.countyfp || a.tractce = b.statefp || b.countyfp || b.tractce

      psql := 'INSERT INTO ' || p_output_topology || '_LAYERS_OUT_HELP '
           || 'SELECT d.oid_base, c.oid_base '  --order is important, child oids are unique
           || 'FROM '
           || parent_source_table || ' a, '
           || child_source_table || ' b, '
           || p_parent_fsl || ' c, '
           || p_child_fsl || ' d '
           || 'WHERE '
           || 'c.oid_base = a.oid AND '
           || 'd.oid_base = b.oid AND '
           || p_parent_nesting || ' = ' || child_nesting;  --a to b aliasing

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                             'Inserting records into ' || p_output_topology || '_LAYERS_OUT_HELP',
                                              NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql;

      COMMIT;

      --Next update the nesting child table with slumming parent values
      --I could not get this update to complete in under an hour (tracts and blkgrps)
      --When combining the joins above with the update below
      --without going this route with a temporary helper table with an index
      --update z699tm_fsl150v a
      --set (a.oid_superior) = (select v.oid_parent
      --from
      --z699tm_layers_out_help v
      --where v.oid_child = a.oid_base);

      psql := 'UPDATE ' || p_child_fsl || ' a '
           || 'SET '
           || '(a.oid_superior) = (SELECT b.oid_parent '
           || 'FROM '
           || p_output_topology || '_LAYERS_OUT_HELP b '
           || 'WHERE b.oid_child = a.oid_base)';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                             'Updating entire child table ' || p_child_fsl || ' with superior oids',
                                              NULL,NULL,NULL,psql);

      EXECUTE IMMEDIATE psql;

      COMMIT;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                             'Completed one time POPULATE_HIERARCHICAL_HELPER',
                                              NULL,NULL,NULL,psql);

   END POPULATE_HIERARCHICAL_HELPER;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE UPDATE_HIERARCHICAL_TOPOGEOM (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,  --SUBSET or INITIAL
      p_face_table         IN VARCHAR2
   )
   AS

      --Matt! 5/21/12
      --! 12/28/12 Noodling on performance
      --           Changed logic into a series of tighter FORALL executions. Tight yo
      --Update topogeom for already populated oids of hierarchical layers
      --Using constructors or create_feature (depending on nesting layer)

      psql                  VARCHAR2(4000);
      psql2                 VARCHAR2(4000);
      psql3                 VARCHAR2(4000);
      psql4                 VARCHAR2(4000);
      layer_hierarchical    GZ_TYPES.GZ_LAYERS_HIERARCHICAL_REC;
      nesting_layer_in      GZ_TYPES.GZ_LAYERS_IN_REC;
      oidz                  GZ_TYPES.stringarray;
      sourcez               GZ_TYPES.stringarray;
      keyz                  GZ_TYPES.stringarray;
      my_cursor             SYS_REFCURSOR;
      nesting_valz          GZ_TYPES.stringarray;
      nesting_table         VARCHAR2(32);
      nesting_key_col       VARCHAR2(32);
      kount                 PLS_INTEGER;
      nesting_layer_initial PLS_INTEGER;
      tg_layer_id           NUMBER;
      --For build on faces
      facez                 GZ_TYPES.stringarray;
      prim_ids              SDO_TOPO_OBJECT_ARRAY;
      TYPE prim_ids_array_t IS TABLE OF SDO_TOPO_OBJECT_ARRAY INDEX BY PLS_INTEGER;
      prim_ids_array        prim_ids_array_t;
      prim_ids_array_kount  PLS_INTEGER := 0;
      --For build on children
      tgl_idz               GZ_TYPES.numberarray;
      tg_idz                GZ_TYPES.numberarray;
      child_ids             SDO_TGL_OBJECT_ARRAY;
      TYPE child_ids_array_t IS TABLE OF SDO_TGL_OBJECT_ARRAY INDEX BY PLS_INTEGER;
      child_ids_array       child_ids_array_t;
      child_ids_array_kount PLS_INTEGER := 0;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('UPDATE_HIERARCHICAL_TOPOGEOM: starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF NOT GZ_BUSINESS_UTILS.GZ_TABLE_EXISTS(p_output_topology ||  '_FSL' || p_layer || 'V') --no exist if no rows
      THEN

         --Get out if no oids to work with
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                'Exiting for layer ' || p_layer || ', no records',
                                                NULL,NULL,p_release,NULL);

         RETURN;

      END IF;

      IF p_layer_type NOT IN ('HIERARCHICAL')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Wut iz layur tipe ' || p_layer_type);

      END IF;

      layer_hierarchical := GZ_OUTPUT.GET_HIERARCHICAL_LAYER(p_release,
                                                             p_gen_project_id,
                                                             p_layer);

      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_output_topology,
                                                  p_output_topology ||  '_FSL' || p_layer || 'V',
                                                  'TOPOGEOM',
                                                  'POLYGON');


       --Do a whole mess of one time only setups
       --So we dont do them repeatedly in the loop
      IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                    p_gen_project_id,
                                    layer_hierarchical.nesting_layer)
      THEN

         --building at 0 setups
         nesting_layer_initial := 1;

         nesting_layer_in := GZ_OUTPUT.GET_LAYER_IN(p_release,
                                                    p_gen_project_id,
                                                    layer_hierarchical.nesting_layer);

         --make parallel to variable usage in both sections
         nesting_table        := p_source_schema || '.' || nesting_layer_in.source_table;
         nesting_key_col      := nesting_layer_in.source_key;



         --set up 0 level constructor SQL one time -----------------------------
         psql4 := 'UPDATE ' || p_output_topology ||  '_FSL' || p_layer || 'V a '
               || 'SET '
               || 'a.topogeom = SDO_TOPO_GEOMETRY(:p1,:p2,:p3,:p4,NULL) '  --Must have NULL delete explicit
               || 'WHERE a.oid_base = :p5 ';
         -----------------------------------------------------------------------

      ELSE

         --building at higher setups

         nesting_layer_initial := 0;

         --one time only stuff first before the loop

         --like TAB10ST09.COUNTY
         nesting_table := p_source_schema || '.' || GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                                              p_gen_project_id,
                                                                              p_output_topology,
                                                                              layer_hierarchical.nesting_layer,
                                                                              'TABLE');

         --oid or statefp, that sort of thing
         --NB This is the key joiner for the child table
         --Not the nesting VALUE (nesting_layer_field) on which we are aggregating
         --Stop switching it future Matt
         nesting_key_col := GZ_OUTPUT.GET_DEEP_SOURCE(p_release,
                                                      p_gen_project_id,
                                                      p_output_topology,
                                                      layer_hierarchical.nesting_layer,
                                                      'KEY');

         --populate hierarchical helper table in sub
         GZ_OUTPUT.POPULATE_HIERARCHICAL_HELPER(p_output_topology,
                                                p_output_topology ||  '_FSL' || p_layer || 'V',
                                                p_output_topology ||  '_FSL' || layer_hierarchical.nesting_layer || 'V',
                                                layer_hierarchical.source_nesting_field,
                                                layer_hierarchical.nesting_layer_field);


      END IF;


      --no special accessor for the hierarchical simple source
      --I think source_base will always be the same but just in case,  XX THIS IS A FIXED ASSUMPTION FOR NOW
      --    increasingly we are going the other way with inputs that are crossed
      --This is our cursored outer loop for everything else that follows
      psql := 'SELECT a.oid_base, a.source_base, a.key_base '
           || 'FROM ' || p_output_topology ||  '_FSL' || p_layer || 'V a ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                             'Opening cursor to collect oids and sources for hierarchical layer ' || p_layer,
                                              NULL,NULL,p_release,psql);

      OPEN my_cursor FOR psql;

      LOOP

         FETCH my_cursor BULK COLLECT INTO oidz, sourcez, keyz LIMIT 2000; --Bumping this higher to get more SQL per FORALL on slow guys
         EXIT WHEN oidz.COUNT = 0;


         IF nesting_layer_initial = 1 --Yes
         THEN

            FOR i in 1 .. oidz.COUNT
            LOOP

               --Must go to original source for nesting value on parent
               --Will inefficiently do this on each loop (must because of a-aliasing on all inputs and possibility
               --                                         of the source_base changing anywhere in this loop
               --If performance gets to be a problem could get the distinct sourcez and bulk collect for each. So far never a prob
               --select a.statefp from tab10st09.state a
               --where a.oid = 27490331955805


               --expect layer_hierarchical.source_nesting_field to be fully a aliased

               psql2 := 'SELECT ' || layer_hierarchical.source_nesting_field || ' '
                     || 'FROM ' || sourcez(i) || ' a '
                     || 'WHERE a.' || keyz(i) || ' = :p1 ';


               IF i = 1
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                         'Heres the SQL to get the nesting val for ' || oidz.COUNT || ' on the parent of ' || p_layer,
                                                          NULL,NULL,p_release,psql2);

               END IF;

               EXECUTE IMMEDIATE psql2 INTO nesting_valz(i) USING oidz(i);

            END LOOP;

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                   'Completed geting nesters for ' || oidz.COUNT || ' ' || p_layer,
                                                   NULL,NULL,p_release,psql2);

         END IF;


         ------------------------------
         --now divide the logic (again)
         ------------------------------


         IF nesting_layer_initial = 1
         THEN

            --different logic from below, hopefully we dont use this initial style too much
            --the hierarchical layer we are building here is actually gonna be level 0 in the topo
            --It is being built directly on faces

            --1) Get each record for the layer we are working on. Check, see above
            --2) Find the nesting value back in the bench for this record/oid. Also check
            --3) Find matches in the face feature table + benchmark join for nest
            --4) Use constructors on the results

            --select b.face_id from
            --z609in_face b,
            --tab10st09.county a
            --where a.statefp = '09'
            --and a.oid = b.county

            --doesnt change within the loop
            psql3 := 'SELECT b.face_id FROM '
                  || p_face_table || ' b, '
                  || nesting_table || ' a '
                  || 'WHERE '
                  ||  layer_hierarchical.nesting_layer_field || ' = :p1 AND '  --already a aliased
                  || 'a.' || nesting_key_col || ' = b.' || layer_hierarchical.nesting_layer || ' ';   --the nesting layer is the face col

            FOR i IN 1 .. oidz.COUNT
            LOOP


               IF i = 1
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                         'Heres our face fetch for ' || oidz.COUNT || ' ' || p_layer,
                                                         NULL,NULL,p_release,psql3);

               END IF;


               --How many could there be?
               --Lets do it the exadata way
               EXECUTE IMMEDIATE psql3 BULK COLLECT INTO facez USING nesting_valz(i);

               prim_ids := SDO_TOPO_OBJECT_ARRAY();
               prim_ids.EXTEND(facez.COUNT);

               FOR j IN 1 .. facez.COUNT
               LOOP

                  prim_ids(j) := SDO_TOPO_OBJECT(facez(j), 3);

               END LOOP;

               --TIDY
               facez.DELETE;

               --Put this set of faces into an array of arrays
               --where the index of the outer array matches the oid in this loop
               prim_ids_array_kount := prim_ids_array_kount + 1;
               prim_ids_array(prim_ids_array_kount) := prim_ids;

               --Delete the temp array
               prim_ids.DELETE;

               IF i = 1
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                         'Completed face fetch and array for first of ' || oidz.COUNT || ' ' || p_layer,
                                                         NULL,NULL,p_release,psql3);

               END IF;

            END LOOP;

         ELSE

            --select a.topogeom.tg_layer_id, a.topogeom.tg_id from z699tm_fsl150v a
            --where a.oid_superior = '20790540092527'

            psql3 := 'SELECT a.topogeom.tg_layer_id, a.topogeom.tg_id '
                  || 'FROM '
                  || p_output_topology || '_FSL' || layer_hierarchical.nesting_layer || 'V a '
                  || 'WHERE '
                  || 'a.oid_superior = :p1 ';

            FOR i IN 1 .. oidz.COUNT
            LOOP

               IF i = 1
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                         'Heres our child tgl_id fetch for ' || oidz.COUNT || ' ' || p_layer,
                                                         NULL,NULL,p_release,psql3);

               END IF;

               EXECUTE IMMEDIATE psql3 BULK COLLECT INTO tgl_idz,
                                                         tg_idz USING oidz(i);


               --make a single sdo_tgl_object array first
               child_ids := SDO_TGL_OBJECT_ARRAY();
               child_ids.EXTEND(tgl_idz.COUNT);

               FOR j IN 1 .. tgl_idz.COUNT
               LOOP

                  child_ids(j) := SDO_TGL_OBJECT(tgl_idz(j), tg_idz(j));

               END LOOP;

               --TIDY
               tgl_idz.DELETE;
               tg_idz.DELETE;

               --Put this set of tgl_objects into an array of arrays
               --where the index of the outer array matches the oid in this loop
               child_ids_array_kount := child_ids_array_kount + 1;
               child_ids_array(child_ids_array_kount) := child_ids;

               --Delete the temp array now that its tucked away
               child_ids.DELETE;

               IF i = 1
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                         'Completed child tgl_id fetch and array for first of ' || oidz.COUNT || ' ' || p_layer,
                                                         NULL,NULL,p_release,psql3);

               END IF;

            END LOOP;

            --just in case
            nesting_valz.DELETE;


         END IF;  --end if on are we built directly on faces or higher


         ---------------------------------------------
         --now split the logic again for topo creation
         ---------------------------------------------


         IF nesting_layer_initial = 1
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                   'Constructor call for ' || oidz.COUNT || ' ' || p_layer,
                                                   NULL,NULL,p_release,psql4);

            --initial nester uses constructor and face_ids
            --could switch this to GZ_OUTPUT.UPDATE_TOPOGEOM but I kinda like the FORALL option

            FORALL ii IN 1 .. oidz.COUNT
               EXECUTE IMMEDIATE psql4 USING p_output_topology,
                                             3,
                                             tg_layer_id,
                                             prim_ids_array(ii),
                                             oidz(ii);

            COMMIT;

            --reset these for next roll through
            prim_ids_array.DELETE;
            prim_ids_array_kount := 0;

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                   'Completed constructor call for ' || oidz.COUNT || ' ' || p_layer,
                                                   NULL,NULL,p_release,psql4);

         ELSE

            --higher uses UPDATE Using Constructor with SDO_TGL_OBJECT_ARRAY

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                   'Update topogeom call for ' || oidz.COUNT || ' ' || p_layer,
                                                    NULL,NULL,p_release);

            FOR i IN 1 .. oidz.COUNT
            LOOP

               GZ_OUTPUT.UPDATE_TOPOGEOM(p_output_topology,
                                         p_output_topology ||  '_FSL' || p_layer || 'V',
                                         oidz(i),                                          --the record we are making topogeom
                                         3,                                                --topo type
                                         tg_layer_id,                                      --THIS layer id
                                         NULL,                                             --No topo objects
                                         child_ids_array(i),                               --A single SDO_TGL_OBJECT_ARRAY pointing to child layer
                                         'OID_BASE');
               --internally commits

            END LOOP;

            --reset these for next roll through
            child_ids_array.DELETE;
            child_ids_array_kount := 0;

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                   'Completed update topogeom call for ' || oidz.COUNT || ' ' || p_layer,
                                                    NULL,NULL,p_release);



         END IF;


      END LOOP;  --end loop over outer cursor

      CLOSE my_cursor;


      --What to check?  All topogeom vals should be populated I think

      psql := 'SELECT COUNT(*) FROM '
           || p_output_topology || '_FSL' || p_layer || 'V a '
           || 'WHERE '
           || 'a.topogeom.tg_id IS NULL ';

      --could be empty, the entire table
      EXECUTE IMMEDIATE psql INTO kount;

      IF kount > 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                'ERROR for ' || p_layer || '. '|| kount || ' records in '
                                                || p_output_topology || '_FSL' || p_layer || 'V missing topogeom',
                                                NULL,NULL,p_release);

         RAISE_APPLICATION_ERROR(-20001,'Doh, ' || kount || ' records in ' || p_output_topology || '_FSL' || p_layer || 'V missing topogeom');


      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_HIERARCHICAL_TOPOGEOM',NULL,
                                                'Complete for ' || p_layer || '. All records in '
                                                || p_output_topology || '_FSL' || p_layer || 'V have topogeoms',
                                                NULL,NULL,p_release);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('UPDATE_HIERARCHICAL_TOPOGEOM: peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------


   END UPDATE_HIERARCHICAL_TOPOGEOM;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE UPDATE_AGGREGATE_TOPOGEOM (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_face_table         IN VARCHAR2
   )
   AS

      --Matt! 6/4/12
      --This is a bit of a mess
      --Would like to combine with update_subset_topogeom

      --For aggregate layers
      --Update topogeom for already populated oids
      --Using constructors or create_feature

      psql                 VARCHAR2(4000);
      psql2                VARCHAR2(4000);
      layer_aggregate      GZ_TYPES.GZ_LAYERS_AGGREGATE_REC;
      sourcez              GZ_TYPES.stringarray;
      my_cursor            SYS_REFCURSOR;
      oidz                 GZ_TYPES.stringarray;
      facez                GZ_TYPES.stringarray;
      tg_layer_id          NUMBER;
      id_kount             PLS_INTEGER := 0;
      prim_kount           PLS_INTEGER := 0;
      kurrent_kount        PLS_INTEGER := 0;
      current_id           NUMBER;
      --
      TYPE my_group_rec    IS RECORD (
                           feat_id  NUMBER,
                           prim_ids GZ_TYPES.stringarray
      );
      TYPE my_group_arr    IS TABLE of my_group_rec INDEX BY PLS_INTEGER;
      my_groups            my_group_arr;
      --
      TYPE stupidrec       IS RECORD (
                           feat_id NUMBER,
                           prim_ids SDO_TOPO_OBJECT_ARRAY );
      TYPE stupid_arr      IS TABLE OF stupidrec INDEX BY PLS_INTEGER;
      stupid_array         stupid_arr;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('UPDATE_AGGREGATE_TOPOGEOM: starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      IF p_layer_type NOT IN ('AGGREGATE')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Wut iz layur tipe ' || p_layer_type);

      END IF;

      layer_aggregate := GZ_OUTPUT.GET_AGGREGATE_LAYER(p_release,
                                                       p_layer);

      sourcez := GZ_BUSINESS_UTILS.SPLIT(layer_aggregate.source,',');

      IF sourcez.COUNT <> 2
      THEN

         --better have checked this somewhere else bub
         RAISE_APPLICATION_ERROR(-20001, 'Aggregate layer ' || p_layer || ' has ' || sourcez.COUNT || ' sources ');

      END IF;

      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_output_topology,
                                                  p_output_topology ||  '_FSL' || p_layer || 'V',
                                                  'TOPOGEOM',
                                                  'POLYGON');


      --This is pretty easy
      --we dont much care which of the deep sources we are aggregating, incplace, cdp whatever
      --there are just faces and matches in our fsl oids

      FOR iii IN 1 .. sourcez.COUNT
      LOOP

         IF GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                       p_gen_project_id,
                                       sourcez(iii))
         THEN

            --initial layer, oids right on the face table

            psql := 'SELECT a.oid_base, f.face_id  '
                 || 'FROM '
                 || p_output_topology || '_FSL' || p_layer || 'V a, '
                 || p_face_table || ' f '
                 || 'WHERE '
                 || 'a.oid_base = f.' || sourcez(iii) || ' ' --must include since we may have limited
                 || 'ORDER BY a.oid_base ';                  --the universe in previous step with a where clause

         ELSE

            --some other layer, pop faces out of the topogeom

            psql := 'SELECT a.oid_base, t.topo_id '
                 || 'FROM '
                 || p_output_topology || '_FSL' || sourcez(iii) || 'V a, '
                 || p_output_topology || '_FSL' || p_layer || 'V b, '
                 || 'TABLE(a.topogeom.get_topo_elements()) t '
                 || 'WHERE '
                 || 'a.oid_base = b.oid_base '
                 || 'ORDER BY a.oid_base ';

         END IF;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_AGGREGATE_TOPOGEOM',NULL,
                                                'Opening cursor to collect ' || sourcez(iii) || ' face ids for ' || p_layer,
                                                NULL,NULL,p_release,psql);

         OPEN my_cursor FOR psql;

         LOOP

            FETCH my_cursor BULK COLLECT INTO oidz, facez LIMIT 1000;
            EXIT WHEN oidz.COUNT = 0;

            FOR i in 1 .. oidz.COUNT
            LOOP

               --Have to set this up first so the stupid array can get EXTENDED
               --with the correct count
               --Theres SQL to do this, return soon

               IF i = 1
               THEN

                   --counter and loop cleanup
                   id_kount := 1;
                   prim_kount := 1;
                   current_id := oidz(i);
                   my_groups.DELETE;

                   my_groups(id_kount).feat_id := oidz(i);
                   my_groups(id_kount).prim_ids(prim_kount) := facez(i);

               ELSIF oidz(i) = current_id
               THEN

                  prim_kount := prim_kount + 1;

                  my_groups(id_kount).prim_ids(prim_kount) := facez(i);

               ELSIF oidz(i) <> current_id
               THEN

                  id_kount := id_kount + 1;
                  prim_kount := 1;
                  current_id := oidz(i);

                  my_groups(id_kount).feat_id := oidz(i);
                  my_groups(id_kount).prim_ids(prim_kount) := facez(i);

               ELSE

                  RAISE_APPLICATION_ERROR(-20001,'woops');

               END IF;

            END LOOP;

            FOR j IN 1 .. my_groups.COUNT
            LOOP

               IF j = 1
               THEN

                  --cleanup here for re loops
                  kurrent_kount := 0;
                  stupid_array.DELETE;

               END IF;

               kurrent_kount := kurrent_kount + 1;

               --place the feature id in the outer record id
               stupid_array(kurrent_kount).feat_id := my_groups(j).feat_id;
               --stupid extend the nested object
               stupid_array(kurrent_kount).prim_ids := SDO_TOPO_OBJECT_ARRAY();
               stupid_array(kurrent_kount).prim_ids.EXTEND(my_groups(j).prim_ids.COUNT);

               --stupid objects
               FOR jj IN 1 .. my_groups(j).prim_ids.COUNT
               LOOP

                  --prim_ids: SDO_TOPO_OBJECT_ARRAY is VARRAY (1000000) of SDO_TOPO_OBJECT
                  stupid_array(kurrent_kount).prim_ids(jj) := SDO_TOPO_OBJECT(my_groups(j).prim_ids(jj), 3);

               END LOOP;

            END LOOP;


            --Update this batch
            --Note that there may be overlaps on base_oids between the outer cursor BULK COLLECTs
            --This is ok, just another few face_id adds to the same oid_base


            psql2 := 'UPDATE ' || p_output_topology ||  '_FSL' || p_layer || 'V a '
                  || 'SET '
                  || 'a.topogeom = SDO_TOPO_GEOMETRY(:p1,:p2,:p3,:p4,NULL) '  --must have explicit null delete
                  || 'WHERE a.oid_base = :p5 ';

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_AGGREGATE_TOPOGEOM',NULL,
                                                   'Calling constructor for ' || stupid_array.COUNT || ' records on ' || p_layer,
                                                   NULL,NULL,p_release,psql2);

            FORALL ii IN 1 .. stupid_array.COUNT
               EXECUTE IMMEDIATE psql2
               USING p_output_topology,
                     3,
                     tg_layer_id,
                     stupid_array(ii).prim_ids,
                     stupid_array(ii).feat_id;

            COMMIT;


         END LOOP;

         CLOSE my_cursor;

      END LOOP;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_AGGREGATE_TOPOGEOM',NULL,
                                             'Complete for ' || p_layer,
                                             NULL,NULL,p_release);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('UPDATE_AGGREGATE_TOPOGEOM: peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END UPDATE_AGGREGATE_TOPOGEOM;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE UPDATE_SPLIT_TOPOGEOM (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_layer_type         IN VARCHAR2,
      p_face_table         IN VARCHAR2
   )
   AS

      --Matt! 6/15/12
      --8/17/12 updated for splits of splits
      --8/17/12 update was not correct, need to use geo_ids to uniquely ID splits of splits
      --12/04/12 Update to add a third id, the geo_id.  This mess should use ONLY geo_id. Come back to this

      psql                    VARCHAR2(4000);
      psql2                   VARCHAR2(4000);
      layer_split             GZ_TYPES.GZ_LAYERS_SPLIT_REC;
      my_cursor               SYS_REFCURSOR;
      oidz                    GZ_TYPES.stringarray;
      oidz_superior           GZ_TYPES.stringarray;
      facez                   GZ_TYPES.stringarray;
      geo_idz                 GZ_TYPES.stringarray;
      tg_layer_id             NUMBER;
      id_kount                PLS_INTEGER := 0;
      prim_kount              PLS_INTEGER := 0;
      kurrent_kount           PLS_INTEGER := 0;
      current_id              NUMBER;
      current_id2             NUMBER;
      current_id3             VARCHAR2(4000);  --freakin mess
      --
      TYPE my_group_rec       IS RECORD (
                              feat_id  NUMBER,
                              feat_id2 NUMBER,
                              feat_id3 VARCHAR2(4000),   --geo_id
                              prim_ids GZ_TYPES.stringarray
      );
      TYPE my_group_arr       IS TABLE of my_group_rec INDEX BY PLS_INTEGER;
      my_groups               my_group_arr;
      --
      TYPE stupidrec          IS RECORD (
                              feat_id NUMBER,
                              feat_id2 NUMBER,
                              feat_id3 VARCHAR2(4000),  --geo_id
                              prim_ids SDO_TOPO_OBJECT_ARRAY );
      TYPE stupid_arr         IS TABLE OF stupidrec INDEX BY PLS_INTEGER;
      stupid_array            stupid_arr;

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('UPDATE_SPLIT_TOPOGEOM: starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      layer_split := GZ_OUTPUT.GET_SPLIT_LAYER(p_release,
                                               p_layer);

      tg_layer_id := GZ_TOPO_UTIL.GET_TG_LAYER_ID(p_output_topology,
                                                  p_output_topology ||  '_FSL' || p_layer || 'V',
                                                  'TOPOGEOM',
                                                  'POLYGON');


      --splits are always at level 0
      --we have to break the sources down to faces

      --do the standard base layer first.  Have an oid_base value at this point

      psql := 'SELECT a.oid_base, t.face_id, a.oid_superior, a.geo_id '
           || 'FROM '
           || p_output_topology || '_FSL' || p_layer || 'V a, '
           || 'TABLE(a.split_primitives) t '
           || 'WHERE a.oid_base IS NOT NULL '
           || 'ORDER by a.oid_base, a.oid_superior ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_SPLIT_TOPOGEOM',NULL,
                                             'Opening cursor to collect split_primitives face ids for ' || p_layer,
                                              NULL,NULL,p_release,psql);

      OPEN my_cursor FOR psql;

      LOOP

         FETCH my_cursor BULK COLLECT INTO oidz, facez, oidz_superior, geo_idz LIMIT 1000;
         EXIT WHEN oidz.COUNT = 0;

         FOR i in 1 .. oidz.COUNT
         LOOP

            --Have to set this up first so the stupid array can get EXTENDED
            --with the correct count
            --Theres SQL to do this, return soon

            IF i = 1
            THEN

                --counter and loop cleanup
                id_kount := 1;
                prim_kount := 1;
                current_id := oidz(i);
                current_id2 := oidz_superior(i);
                current_id3 := geo_idz(i);
                my_groups.DELETE;

                my_groups(id_kount).feat_id := oidz(i);
                my_groups(id_kount).feat_id2 := oidz_superior(i);
                my_groups(id_kount).feat_id3 := geo_idz(i);
                my_groups(id_kount).prim_ids(prim_kount) := facez(i);

            ELSIF oidz(i) = current_id
            AND oidz_superior(i) = current_id2
            AND geo_idz(i) = current_id3
            THEN

               prim_kount := prim_kount + 1;

               my_groups(id_kount).prim_ids(prim_kount) := facez(i);

            ELSIF oidz(i) <> current_id
            OR oidz_superior(i) <> current_id2
            OR geo_idz(i) <> current_id3
            THEN

               id_kount := id_kount + 1;
               prim_kount := 1;
               current_id := oidz(i);
               current_id2 := oidz_superior(i);
               current_id3 := geo_idz(i);

               my_groups(id_kount).feat_id := oidz(i);
               my_groups(id_kount).feat_id2 := oidz_superior(i);
               my_groups(id_kount).feat_id3 := geo_idz(i);
               my_groups(id_kount).prim_ids(prim_kount) := facez(i);

            ELSE

               RAISE_APPLICATION_ERROR(-20001,'woops');

            END IF;

         END LOOP;

         FOR j IN 1 .. my_groups.COUNT
         LOOP

            IF j = 1
            THEN

               --cleanup here for re loops
               kurrent_kount := 0;
               stupid_array.DELETE;

            END IF;

            kurrent_kount := kurrent_kount + 1;

            --place the feature id in the outer record id
            stupid_array(kurrent_kount).feat_id := my_groups(j).feat_id;
            stupid_array(kurrent_kount).feat_id2 := my_groups(j).feat_id2;
            stupid_array(kurrent_kount).feat_id3 := my_groups(j).feat_id3;
            --stupid extend the nested object
            stupid_array(kurrent_kount).prim_ids := SDO_TOPO_OBJECT_ARRAY();
            stupid_array(kurrent_kount).prim_ids.EXTEND(my_groups(j).prim_ids.COUNT);

            --stupid objects
            FOR jj IN 1 .. my_groups(j).prim_ids.COUNT
            LOOP

               --prim_ids: SDO_TOPO_OBJECT_ARRAY is VARRAY (1000000) of SDO_TOPO_OBJECT
               stupid_array(kurrent_kount).prim_ids(jj) := SDO_TOPO_OBJECT(my_groups(j).prim_ids(jj), 3);

            END LOOP;

         END LOOP;


         --Update this batch
         --Note that there may be overlaps between the outer cursor BULK COLLECTs
         --This is ok, just another few face_id adds to the same oid_base


         psql2 := 'UPDATE ' || p_output_topology ||  '_FSL' || p_layer || 'V a '
               || 'SET '
               || 'a.topogeom = SDO_TOPO_GEOMETRY(:p1,:p2,:p3,:p4,NULL) '  --must have null
               || 'WHERE a.oid_base = :p5 AND '
               || 'a.oid_superior = :p6 AND '
               || 'a.geo_id = :p7 ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_SPLIT_TOPOGEOM',NULL,
                                                'Calling constructor for ' || stupid_array.COUNT || ' records on ' || p_layer,
                                                NULL,NULL,p_release,psql2);


         FORALL ii IN 1 .. stupid_array.COUNT
            EXECUTE IMMEDIATE psql2
            USING p_output_topology,
                  3,
                  tg_layer_id,
                  stupid_array(ii).prim_ids,
                  stupid_array(ii).feat_id,
                  stupid_array(ii).feat_id2,
                  stupid_array(ii).feat_id3;

         COMMIT;



      END LOOP;

      CLOSE my_cursor;
      --These should all happen at the start of loops, but to be safe
      oidz.DELETE;
      oidz_superior.DELETE;
      facez.DELETE;
      stupid_array.DELETE;

      --do remainders if requested
      --remainders have an oid_superior but no oid_base

      IF layer_split.create_remainders = 'Y'
      THEN

         psql := 'SELECT a.oid_superior, t.face_id '
              || 'FROM '
              || p_output_topology || '_FSL' || p_layer || 'V a, '
              || 'TABLE(a.split_primitives) t '
              || 'WHERE a.oid_base IS NULL '
              || 'ORDER by a.oid_superior ';

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_SPLIT_TOPOGEOM',NULL,
                                                'Opening cursor to collect REMAINDER split_primitives face ids for ' || p_layer,
                                                 NULL,NULL,p_release,psql);

         OPEN my_cursor FOR psql;

         LOOP

            FETCH my_cursor BULK COLLECT INTO oidz, facez LIMIT 1000;
            EXIT WHEN oidz.COUNT = 0;

            FOR i in 1 .. oidz.COUNT
            LOOP

               --Have to set this up first so the stupid array can get EXTENDED
               --with the correct count
               --Theres SQL to do this, return soon

               IF i = 1
               THEN

                   --counter and loop cleanup
                   id_kount := 1;
                   prim_kount := 1;
                   current_id := oidz(i);
                   my_groups.DELETE;

                   my_groups(id_kount).feat_id := oidz(i);
                   my_groups(id_kount).prim_ids(prim_kount) := facez(i);

               ELSIF oidz(i) = current_id
               THEN

                  prim_kount := prim_kount + 1;

                  my_groups(id_kount).prim_ids(prim_kount) := facez(i);

               ELSIF oidz(i) <> current_id
               THEN

                  id_kount := id_kount + 1;
                  prim_kount := 1;
                  current_id := oidz(i);

                  my_groups(id_kount).feat_id := oidz(i);
                  my_groups(id_kount).prim_ids(prim_kount) := facez(i);

               ELSE

                  RAISE_APPLICATION_ERROR(-20001,'woops');

               END IF;

            END LOOP;

            FOR j IN 1 .. my_groups.COUNT
            LOOP

               IF j = 1
               THEN

                  --cleanup here for re loops
                  kurrent_kount := 0;
                  stupid_array.DELETE;

               END IF;

               kurrent_kount := kurrent_kount + 1;

               --place the feature id in the outer record id
               stupid_array(kurrent_kount).feat_id := my_groups(j).feat_id;
               --stupid extend the nested object
               stupid_array(kurrent_kount).prim_ids := SDO_TOPO_OBJECT_ARRAY();

               stupid_array(kurrent_kount).prim_ids.EXTEND(my_groups(j).prim_ids.COUNT);

               --stupid objects
               FOR jj IN 1 .. my_groups(j).prim_ids.COUNT
               LOOP

                  --prim_ids: SDO_TOPO_OBJECT_ARRAY is VARRAY (1000000) of SDO_TOPO_OBJECT
                  stupid_array(kurrent_kount).prim_ids(jj) := SDO_TOPO_OBJECT(my_groups(j).prim_ids(jj), 3);

               END LOOP;

            END LOOP;


            --Update this batch
            --Note that there may be overlaps on base_oids between the outer cursor BULK COLLECTs
            --This is ok, just another few face_id adds to the same oid_base



            psql2 := 'UPDATE ' || p_output_topology ||  '_FSL' || p_layer || 'V a '
                  || 'SET '
                  || 'a.topogeom = SDO_TOPO_GEOMETRY(:p1,:p2,:p3,:p4,NULL) '   --must have null delete
                  || 'WHERE '
                  || 'a.oid_superior = :p5 AND '
                  || 'a.oid_base IS NULL ';     -- <-- !!!

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_SPLIT_TOPOGEOM',NULL,
                                                   'Calling constructor for ' || stupid_array.COUNT || ' remainder records on ' || p_layer,
                                                   NULL,NULL,p_release,psql2);


            FORALL ii IN 1 .. stupid_array.COUNT
               EXECUTE IMMEDIATE psql2
               USING p_output_topology,
                     3,
                     tg_layer_id,
                     stupid_array(ii).prim_ids,
                     stupid_array(ii).feat_id;

            COMMIT;


         END LOOP;

         CLOSE my_cursor;

      END IF;



      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_SPLIT_TOPOGEOM',NULL,
                                             'Complete for ' || p_layer,
                                             NULL,NULL,p_release);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('UPDATE_SPLIT_TOPOGEOM: peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

   END UPDATE_SPLIT_TOPOGEOM;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION BUILD_LAYER_TOPOGEOM (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_face_table         IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/7/12
      --This is the core of the output module
      --Switches layer types off to appropriate lower level code


      output               VARCHAR2(4000) := '0';
      layer_type           VARCHAR2(32);
      psql                 VARCHAR2(4000);



   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_LAYER_TOPOGEOM: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'BUILD_LAYER_TOPOGEOM',NULL,
                                             'STARTING for ' || p_layer,NULL,NULL,p_release);

      layer_type := GZ_OUTPUT.GET_LAYER_TYPE(p_output_topology,
                                             p_layer);

      IF layer_type = 'INITIAL'
      OR layer_type = 'SUBSET'
      THEN

         --insert universe of oids
         GZ_OUTPUT.INSERT_SUBSET_OIDS(p_release,
                                      p_gen_project_id,
                                      p_source_schema,
                                      p_output_topology,
                                      p_layer,
                                      layer_type,
                                      p_face_table);

         --then pop topogeom
         GZ_OUTPUT.UPDATE_SUBSET_TOPOGEOM(p_release,
                                          p_gen_project_id,
                                          p_source_schema,
                                          p_output_topology,
                                          p_layer,
                                          layer_type,
                                          p_face_table);


      ELSIF layer_type = 'HIERARCHICAL'
      THEN

         GZ_OUTPUT.INSERT_HIERARCHICAL_OIDS(p_release,
                                            p_gen_project_id,
                                            p_source_schema,
                                            p_output_topology,
                                            p_layer,
                                            layer_type,
                                            p_face_table);

         GZ_OUTPUT.UPDATE_HIERARCHICAL_TOPOGEOM(p_release,
                                                p_gen_project_id,
                                                p_source_schema,
                                                p_output_topology,
                                                p_layer,
                                                layer_type,
                                                p_face_table);

      ELSIF layer_type = 'AGGREGATE'
      THEN

         GZ_OUTPUT.INSERT_AGGREGATE_OIDS(p_release,
                                         p_gen_project_id,
                                         p_source_schema,
                                         p_output_topology,
                                         p_layer,
                                         layer_type,
                                         p_face_table);

         GZ_OUTPUT.UPDATE_AGGREGATE_TOPOGEOM(p_release,
                                             p_gen_project_id,
                                             p_source_schema,
                                             p_output_topology,
                                             p_layer,
                                             layer_type,
                                             p_face_table);

      ELSIF layer_type = 'SPLIT'
      THEN

         GZ_OUTPUT.INSERT_SPLIT_OIDS(p_release,
                                     p_gen_project_id,
                                     p_source_schema,
                                     p_output_topology,
                                     p_layer,
                                     layer_type,
                                     p_face_table);

         --splits is zpecial
         GZ_OUTPUT.PROCESS_SPLITS(p_release,
                                  p_gen_project_id,
                                  p_source_schema,
                                  p_output_topology,
                                  p_layer,
                                  layer_type,
                                  p_face_table,
                                  'Y'); --future flag to indicate post clip

         --then a simple update based on faces
         GZ_OUTPUT.UPDATE_SPLIT_TOPOGEOM(p_release,
                                         p_gen_project_id,
                                         p_source_schema,
                                         p_output_topology,
                                         p_layer,
                                         layer_type,
                                         p_face_table);

      ELSE

         --DONT FORGET WHERECLAUSE limitation for other layers

         RAISE_APPLICATION_ERROR(-20001,'Cant handle layer type ' || layer_type || ' yet ');

      END IF;



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('BUILD_LAYER_TOPOGEOM: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'BUILD_LAYER_TOPOGEOM',NULL,
                                             'Complete for ' || p_layer,NULL,NULL,p_release);

      RETURN output;

   END BUILD_LAYER_TOPOGEOM;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE POPULATE_REMAINDER_ATTRIBUTES (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2
   )
   AS

      --Matt! 6/18/12
      --If

      layer_split          GZ_TYPES.GZ_LAYERS_SPLIT_REC;
      psql                 VARCHAR2(4000);


   BEGIN

      layer_split := GZ_OUTPUT.GET_SPLIT_LAYER(p_release,
                                               p_layer);

      IF layer_split.create_remainders = 'N'
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_REMAINDER_ATTRIBUTES',NULL,
                                                'No remainders requested for ' || p_layer,NULL,NULL,p_release);

      ELSE

         IF layer_split.remainder_code IS NOT NULL
         THEN

            IF layer_split.remainder_code_field IS NULL
            THEN

               RAISE_APPLICATION_ERROR(-20001,'Sorry, I shoulda caught this sooner.  Remainder code field missing for ' || p_layer);

            END IF;

            psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V a '
                 || 'SET a.' || layer_split.remainder_code_field || ' = :p1 '
                 || 'WHERE a.oid_base IS NULL ';

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_REMAINDER_ATTRIBUTES',NULL,
                                                   'Populating remainder code field for ' || p_layer,
                                                   NULL,NULL,p_release,psql);

            EXECUTE IMMEDIATE psql USING layer_split.remainder_code;
            COMMIT;

         END IF;


         IF layer_split.remainder_name_prefix IS NOT NULL
         THEN

            --spec says somewhere
            --Remainder records will need the name populated from the split / remainder table
            --and the SUPERIOR layer's completely adjusted name

            --assuming name column is always "name"

            --ex
            --update z609in_fsl155v a
            --set a.name = (SELECT 'Remainder of ' || b.name from z609in_fsl050v b where b.oid_base = a.oid_superior)
            --where a.oid_base IS NULL

            psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V a '
                 || 'SET a.name = '
                 || '(SELECT ''' || layer_split.remainder_name_prefix || ''' || b.name '
                 || 'FROM ' || p_output_topology || '_FSL' || layer_split.superior_layer || 'V b '
                 || 'WHERE b.oid_base = a.oid_superior) '
                 || 'WHERE a.oid_base IS NULL ';

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_REMAINDER_ATTRIBUTES',NULL,
                                                   'Populating remainder name for ' || p_layer,
                                                   NULL,NULL,p_release,psql);

            EXECUTE IMMEDIATE psql;
            COMMIT;

         END IF;


      END IF;

   END POPULATE_REMAINDER_ATTRIBUTES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION POPULATE_LAYER_ATTRIBUTES (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,
      p_face_table         IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/9/12
      --our work table has correct spatial topogeoms
      --but is a wild west of oids and sources
      --We can only use the table itself at this point to figure out where its rows came from

      --1/29/13 - Changed cannibal deep split attribute sql to use geo_id only
      --          Allows split remainders - these weirdos join current base to deep superior oid

      --! 1/8/14 - Fixed bug mangling the update statement when final columns in the list arent to be updated


      output               VARCHAR2(4000) := '0';
      layer_type           VARCHAR2(32);
      psql                 VARCHAR2(4000);
      psql2                VARCHAR2(4000);
      psql_set             VARCHAR2(4000);
      psql_select          VARCHAR2(4000);
      fieldz               GZ_TYPES.stringarray;
      crosswalk_hash       GZ_TYPES.stringhash;
      fieldz_superior      GZ_TYPES.stringarray;
      fieldz_not_superior  GZ_TYPES.stringarray;
      fieldz_deep_superior GZ_TYPES.stringarray;
      cursor_sql           VARCHAR2(4000);
      update_sets          GZ_TYPES.stringarray;
      source_n_key         GZ_TYPES.stringarray;
      source_superior      VARCHAR2(64);
      layer_split          GZ_TYPES.GZ_LAYERS_SPLIT_REC;
      superior_source_key  VARCHAR2(32);
      kount                PLS_INTEGER;
      remainder_kount      PLS_INTEGER;
      update_a_column      PLS_INTEGER := 0;


   BEGIN

      IF layer_type NOT IN ('INITIAL','SUBSET','HIERARCHICAL','AGGREGATE','SPLIT')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Unknown layer type ' || layer_type);

      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_LAYER_ATTRIBUTES: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      --check that we have some records for this layer
      psql := 'SELECT COUNT(*) FROM '
           || p_output_topology || '_FSL' || p_layer || 'V a '
           || 'WHERE rownum = 1 ';

      EXECUTE IMMEDIATE psql INTO kount;

      IF kount = 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_LAYER_ATTRIBUTES',NULL,
                                                     'No records, exiting for ' || p_layer,NULL,NULL,p_release);

         RETURN output;

      END IF;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_LAYER_ATTRIBUTES',NULL,
                                                  'STARTING for ' || p_layer,NULL,NULL,p_release);

      layer_type := GZ_OUTPUT.GET_LAYER_TYPE(p_output_topology,
                                             p_layer);


      fieldz := GZ_OUTPUT.GET_FIELDS(p_release,
                                     p_layer);


      crosswalk_hash := GZ_OUTPUT.GET_CROSSWALK_HASH(p_release,
                                                     'BENCH_FIELD');

      --simple sample
      --update Z609IN_FSL050V z
      --set (z.state, z.county, z.name, z.lsad) =
      --( select a.statefp, a.countyfp, a.name, a.lsad from tab10st09.county a
      --where z.oid_base = a.oid)
      --WHERE z.source_base = 'TAB10ST09.COUNTY' and
      --z.key_base = 'oid'
      --better to put whereclause inside the parens?


      --put this on the outside of the SQL builder so we can fudge with it
      --based on the source
      cursor_sql := 'SELECT DISTINCT a.source_base || '','' || a.key_base '
                 || 'FROM ' || p_output_topology || '_FSL' || p_layer || 'V a ';

      IF layer_type = 'SPLIT'
      THEN

         --get remainders out of here if this is a split layer
         cursor_sql := cursor_sql || 'WHERE a.source_base IS NOT NULL ';

      END IF;

      EXECUTE IMMEDIATE cursor_sql BULK COLLECT INTO update_sets; --no limit, shouldnt be more than a few

      FOR i IN 1 .. update_sets.COUNT
      LOOP

         source_n_key := GZ_BUSINESS_UTILS.SPLIT(update_sets(i),',');

         IF source_n_key.COUNT <> 2
         THEN

            RAISE_APPLICATION_ERROR(-20001,'Wooops');

         END IF;


         IF layer_type = 'SPLIT'
         AND i = 1  --otherwise we lose superiors on loop 2
         THEN

            --splits come from the base table if the column (currently matched in the hash) exists

            --return the fields that dont exist in the base layer
            --required assumption - if the base layer has multiple sources (ex INCPLACE + CDP)
            --   the available fields in the base are the same. Wont work any other way

            --this guy only returns fields with crosswalk matches, non-benchmarked never get in play
            fieldz_superior := GZ_OUTPUT.GET_MISSING_FIELDS(p_layer,
                                                            source_n_key(1),  --ex tab10.county
                                                            fieldz,
                                                            crosswalk_hash);

            --subtract the superior-only columns from our list that we will process right below
            fieldz := GZ_OUTPUT.STRINGARRAY_SUBTRACT(fieldz,
                                                     fieldz_superior);

         END IF;

         --start the universal part of the update statement
         --use Z aliasing for my table for funz

         psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V z '
              || 'SET ( ';

         FOR i IN 1 .. fieldz.COUNT
         LOOP

            IF crosswalk_hash.EXISTS(fieldz(i))  --skip Geo_id or some other weirdos.  No corresponding bench_field, so not in xwalk
            THEN

               IF update_a_column = 0
               THEN

                  --test that we actually got in here once, both for comma management in this loop
                  --and for tracking below of a simple layer that has no field except geo_id
                  update_a_column := 1;

                               --z.state
                  psql_set := 'z.' || fieldz(i) || ' ';

                                 --a.statefp
                  psql_select := 'a.' || crosswalk_hash(fieldz(i)) || ' ';

               ELSE

                  --start loop comma style. Gangster

                                -- z.state , z.county
                  psql_set := psql_set || ', z.' || fieldz(i) || ' ';

                                    -- a.statefp , a.countyfp
                  psql_select := psql_select || ', a.' || crosswalk_hash(fieldz(i)) || ' ';

               END IF;

            END IF;

         END LOOP;

         IF update_a_column = 1
         THEN

            --assemble the full update statement

            --Full table scans are the exadata exaway
            --Plus, pretty sure that even a BIG table in the output world is gonna be something like county
            --Just a few thousand rows

            psql2 := psql || psql_set || ' ) = ( SELECT '
                          || psql_select
                          || 'FROM ' || source_n_key(1) || ' a '  -- <-- includes schema DOT
                          || 'WHERE z.oid_base = a.' || source_n_key(2) || ') '
                          || 'WHERE z.source_base = :p1 AND '
                          || 'z.key_base = :p2 ';

         END IF;

         IF  layer_type = 'SPLIT'
         THEN

            --splits only do not update remainders in here
            psql2 := psql2 || 'AND z.oid_base IS NOT NULL ';

         END IF;

         IF update_a_column = 1
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_LAYER_ATTRIBUTES',NULL,
                                                   i || ': Calling attribute update SQL for ' || p_layer
                                                   || ' using ' || source_n_key(1) || ',' || source_n_key(2),
                                                   NULL,NULL,p_release,psql2);

            EXECUTE IMMEDIATE psql2 USING source_n_key(1),
                                          source_n_key(2);
            COMMIT;

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_LAYER_ATTRIBUTES',NULL,
                                                   i || ': Skipping attribute update SQL for ' || p_layer
                                                   || ' since there are no columns to update from the benchmark',
                                                   NULL,NULL,p_release);

         END IF;

         --reset sql parts in case of a second loop, like aggregate layers
         psql        := '';
         psql_set    := '';
         psql_select := '';
         psql2       := '';
         update_a_column := 0;

      END LOOP;


      IF layer_type = 'SPLIT'
      AND fieldz_superior.COUNT > 0
      THEN

         --fieldz_superior is truly fields that come from the superior layer
         --doesnt include any "false" fields without crosswalk matches

         --reset
         update_a_column := 0;

         layer_split := GZ_OUTPUT.GET_SPLIT_LAYER(p_release,
                                                  p_layer);

         --process superior columns if we have any
         --There should just be one superior source
         --this next SQL will error if for some reason it collects more than 1 superior
         --could also get it from the deep_source, but I like this check

         psql := 'SELECT DISTINCT a.source_superior '
              || 'FROM ' || p_output_topology || '_FSL' || p_layer || 'V a '
              || 'WHERE a.oid_base IS NOT NULL '; --standard splits

         EXECUTE IMMEDIATE psql INTO source_superior;

         IF NOT GZ_OUTPUT.LAYER_IS_INITIAL(p_release,
                                           p_gen_project_id,
                                           layer_split.base_layer)
         AND GZ_OUTPUT.GET_LAYER_TYPE(p_output_topology, layer_split.base_layer) = 'SPLIT'  --note that this errors
         THEN                                                                               --if an initial layer hits it

            --sometimes we have complex split layers who have a
            --base layer input that is another split layer
            --in that case some of these presumed fieldz_superior columns may actually be
            --superior columns from the base input layer, ie available to us directly only
            --on the split layer itself, too hard to derive from back in the benchmark
            --pull any of these rare birds out now

            fieldz_deep_superior := GZ_OUTPUT.GET_MISSING_FIELDS(p_layer,  --not used
                                                                 source_superior,  --ex tab10.county
                                                                 fieldz_superior,
                                                                 crosswalk_hash);

            --subtract the deep superior columns from our list that we will process right below
            fieldz_superior := GZ_OUTPUT.STRINGARRAY_SUBTRACT(fieldz_superior,
                                                              fieldz_deep_superior);

         END IF;

         superior_source_key   := GZ_OUTPUT.GET_DEEP_SOURCE (p_release,
                                                             p_gen_project_id,
                                                             p_output_topology,
                                                             layer_split.superior_layer,
                                                             'KEY');

         psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V z '
              || 'SET ( ';

         FOR i IN 1 .. fieldz_superior.COUNT
         LOOP

            IF crosswalk_hash.EXISTS(fieldz_superior(i))  --should always EXIST, fieldz_superior should always have xwalk values
            THEN

               IF update_a_column = 0
               THEN

                  --test that we actually got in here once, this time only for comma management in this loop
                  update_a_column := 1;

                               --z.state
                  psql_set := 'z.' || fieldz_superior(i) || ' ';

                                 --a.statefp
                  psql_select := 'a.' || crosswalk_hash(fieldz_superior(i)) || ' ';

               ELSE

                  --start commas
                  -- z.state , z.county
                  psql_set := psql_set || ', z.' || fieldz_superior(i) || ' ';

                                    -- a.statefp , a.countyfp
                  psql_select := psql_select || ', a.' || crosswalk_hash(fieldz_superior(i)) || ' ';

               END IF;

            ELSE

               RAISE_APPLICATION_ERROR(-20001, 'Shouldnt hit this, how can ' || fieldz_deep_superior(i)
                                            || ' not have a crosswalk value? ');

            END IF;

         END LOOP;


         IF update_a_column = 1
         THEN

            --assemble the full update statement

            --Full table scans are the exadata exaway
            --Plus, pretty sure that even a BIG table in the output world is gonna be something like county
            --Just a few thousand rows

            psql2 := psql || psql_set || ' ) = ( SELECT '
                          || psql_select
                          || 'FROM ' || source_superior || ' a '  -- <-- includes schema DOT
                          || 'WHERE z.oid_superior = a.' || superior_source_key || ') '
                          || 'WHERE z.source_superior = :p1 AND '
                          || 'z.oid_base IS NOT NULL ';



            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_LAYER_ATTRIBUTES',NULL,
                                                   ' Calling superior attribute update SQL for ' || p_layer
                                                   || ' using ' || source_superior || ',' || superior_source_key,
                                                   NULL,NULL,p_release,psql2);

            EXECUTE IMMEDIATE psql2 USING source_superior;
            COMMIT;

         ELSE

            RAISE_APPLICATION_ERROR(-20001, 'This shouldnt be possible based on the logic in get_missing_fields '
                                         || 'We got ' || fieldz_superior.COUNT || ' superior fields without a match in the crosswalk?');

         END IF;

         IF fieldz_deep_superior.COUNT > 0
         THEN

            --special rare processing for an input base layer that is itself a split layer
            --the only place we can get some columns is from the layer itself
            --not off in the source benchmark
            --dont use the crosswalk here.  The source layer already has the output version of the column name


            /*UPDATE Z699IN_FSL080V z
               SET (z.cousub) =
                      (SELECT a.cousub
                         FROM Z699IN_FSL070V a
                        WHERE z.oid_base = a.oid_base AND z.geo_id = a.geo_id)
               WHERE z.source_superior = 'ACS12.TRACT'
               and z.oid_base is not null
            */

            psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V z '
                 || 'SET ( ';

            FOR i IN 1 .. fieldz_deep_superior.COUNT
            LOOP

               IF crosswalk_hash.EXISTS(fieldz_deep_superior(i))  --should always have a Xwalk
               THEN

                  IF i <> fieldz_deep_superior.COUNT
                  THEN

                     psql := psql || 'z.' || fieldz_deep_superior(i) || ',';

                  ELSE

                     psql := psql || 'z.' || fieldz_deep_superior(i) || ' ) = ( SELECT ';

                  END IF;

               ELSE

                  RAISE_APPLICATION_ERROR(-20001, 'Shouldnt hit this, how can ' || fieldz_deep_superior(i)
                                               || ' not have a crosswalk value? ');

               END IF;

            END LOOP;


            FOR i IN 1 .. fieldz_deep_superior.COUNT
            LOOP

               IF crosswalk_hash.EXISTS(fieldz_deep_superior(i))   --this should always EXIST in here
               THEN

                  IF i <> fieldz_deep_superior.COUNT
                  THEN

                     psql := psql || 'a.' || fieldz_deep_superior(i) || ',';  --not thru crosswalk, already
                                                                              --crosswalked on the output layer
                  ELSE

                     psql := psql || 'a.' || fieldz_deep_superior(i) || ' ';

                  END IF;

               ELSE

                  --repeat.  Double logically impossible to reach code
                  RAISE_APPLICATION_ERROR(-20001, 'Shouldnt hit this, how can ' || fieldz_deep_superior(i)
                                               || ' not have a crosswalk value? ');

               END IF;

            END LOOP;

            --Full table scans are the exadata exaway
            --Plus, pretty sure that even a BIG table in the output world is gonna be something like county
            --Just a few thousand rows
            psql2 := psql || 'FROM ' || p_output_topology || '_FSL' || layer_split.base_layer || 'V a '  -- <-- no schema, ours
                          || 'WHERE z.geo_id = a.geo_id) '  --must join on geo_id too
                          || 'WHERE z.source_superior = :p1 AND '  --not sure if this is necessary
                          || 'z.oid_base IS NOT NULL ';

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_LAYER_ATTRIBUTES',NULL,
                                                   ' Calling cannibal split layer update for ' || p_layer
                                                   || ' using ' || source_superior || ',' || superior_source_key,
                                                   NULL,NULL,p_release,psql2);

            EXECUTE IMMEDIATE psql2 USING source_superior;
            COMMIT;

         END IF; --if we have splits of splits

      END IF;  --If we are split and have some columns not found on the base



      IF layer_type = 'SPLIT'
      THEN

         --process remainder codes and whatnot

         --just because remainders are requested doesnt nec mean they exist


         layer_split := GZ_OUTPUT.GET_SPLIT_LAYER(p_release,
                                                  p_layer);

         --get the full list of fields again for this layer
         --we may have divided them into base and superior above
         fieldz := GZ_OUTPUT.GET_FIELDS(p_release,
                                        p_layer);



         IF layer_split.create_remainders = 'Y'
         THEN

            psql := 'SELECT COUNT(*) '
                 || 'FROM ' || p_output_topology || '_FSL' || p_layer || 'V a '
                 || 'WHERE a.oid_base IS NULL ';

            EXECUTE IMMEDIATE psql INTO remainder_kount;

            IF remainder_kount = 0
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_LAYER_ATTRIBUTES',NULL,
                                                      'No remainders actually exist, exiting ' || p_layer,
                                                      NULL,NULL,p_release,psql2);

            ELSE

               superior_source_key := GZ_OUTPUT.GET_DEEP_SOURCE (p_release,
                                                                 p_gen_project_id,
                                                                 p_output_topology,
                                                                 layer_split.superior_layer,
                                                                 'KEY');

               psql := 'SELECT DISTINCT a.source_superior '
                    || 'FROM ' || p_output_topology || '_FSL' || p_layer || 'V a '
                    || 'WHERE a.oid_base IS NULL '; --remainders only

               --should just be one result
               EXECUTE IMMEDIATE psql INTO source_superior;

               --these are columns with Xwalk matches that are "missing" from the superior source list
               --ie base columns
               fieldz_not_superior := GZ_OUTPUT.GET_MISSING_FIELDS(p_layer,
                                                                   source_superior,  --ex tab10.county
                                                                   fieldz,
                                                                   crosswalk_hash);

               --subtract the base-only columns from our list that we will process right below
               --this result may include garbage columns like geo_id or "false" columns without benchmark matches
               fieldz := GZ_OUTPUT.STRINGARRAY_SUBTRACT(fieldz,
                                                        fieldz_not_superior);


               --first, superior-only remainders get all standard attributes from the superior source?
               --Im not really sure about this

               psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V z '
                    || 'SET ( ';

               update_a_column := 0;

               FOR i IN 1 .. fieldz.COUNT
               LOOP

                  IF crosswalk_hash.EXISTS(fieldz(i))  --skip Geo_id or other "false" columns not in bench
                  THEN

                     IF layer_split.remainder_code_field IS NOT NULL
                     THEN

                        --if not null can compare right here

                        IF  fieldz(i) <> layer_split.remainder_code_field --special handling for this in POPULATE_REMAINDER_ATTRIBUTES sub
                        THEN

                           IF update_a_column = 0
                           THEN

                              --test that we actually got in here once
                              update_a_column := 1;

                                         --z.state
                              psql_set := 'z.' || fieldz(i) || ' ';

                                          --a.statefp
                              psql_select := 'a.' || crosswalk_hash(fieldz(i)) || ' ';

                           ELSE

                              --commas at start, brain

                                          -- z.state , z.county
                              psql_set := psql_set || ', z.' || fieldz(i) || ' ';

                                           -- a.statefp , a.countyfp
                              psql_select := psql_select || ', a.' || crosswalk_hash(fieldz(i)) || ' ';


                           END IF;

                        END IF;

                     ELSE --layer_split.remainder_code is null

                        --ick (code dupe) if null cant compare. Dont worry about it, doesnt exist at all. Poof

                        IF update_a_column = 0
                        THEN

                           --test that we actually got in here once
                           update_a_column := 1;

                                      --z.state
                           psql_set := 'z.' || fieldz(i) || ' ';

                                       --a.statefp
                           psql_select := 'a.' || crosswalk_hash(fieldz(i)) || ' ';

                        ELSE

                           --commas at start, brain

                                       -- z.state , z.county
                           psql_set := psql_set || ', z.' || fieldz(i) || ' ';

                                        -- a.statefp , a.countyfp
                           psql_select := psql_select || ', a.' || crosswalk_hash(fieldz(i)) || ' ';

                        END IF;

                     END IF;

                  END IF;

               END LOOP;

               IF update_a_column = 1
               THEN

                  --assemble the full update statement

                  --Full table scans are the exadata exaway
                  --Plus, pretty sure that even a BIG table in the output world is gonna be something like county
                  --Just a few thousand rows

                  psql2 := psql || psql_set || ' ) = ( SELECT '
                          || psql_select
                          || 'FROM ' || source_superior || ' a '  -- <-- includes schema DOT
                          || 'WHERE z.oid_superior = a.' || superior_source_key || ') '
                          || 'WHERE z.source_superior = :p1 AND '
                          || 'z.oid_base IS NULL ';


                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_LAYER_ATTRIBUTES',NULL,
                                                         ' Calling remainder attribute update SQL for ' || p_layer,
                                                         NULL,NULL,p_release,psql2);

                  EXECUTE IMMEDIATE psql2 USING source_superior;
                  COMMIT;

               END IF;

               GZ_OUTPUT.POPULATE_REMAINDER_ATTRIBUTES(p_release,
                                                       p_gen_project_id,
                                                       p_source_schema,
                                                       p_source_topology,
                                                       p_output_topology,
                                                       p_layer);

            END IF; --end if we even have remainders in existence for this table

         END IF;  --end if create_remainders is true

      END IF;  --end if layer_type = SPLIT

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_LAYER_ATTRIBUTES: Complete');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_LAYER_ATTRIBUTES',NULL,
                                             'Complete for ' || p_layer,NULL,NULL,p_release);

      RETURN output;

   END POPULATE_LAYER_ATTRIBUTES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION ADD_STATE_TO_AIA_NAME (
      p_oid                IN NUMBER,
      p_name               IN VARCHAR2,
      p_source_tab         IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt!  5/16/12
      --Wrapper to more complex ADD_TO_NAME_BASED_ON_SOURCE below
      --Meant to simplify parameter table entry for our most common update usage

      --Stephanie says: Check the aiannhfsr flag on the aiannh table,
      --see if it is a state or federal reservation.
      --If state, add " (state)" to the end of the TIGER name.

      output            VARCHAR2(4000);

   BEGIN

      output := GZ_OUTPUT.ADD_TO_NAME_BASED_ON_SOURCE(p_oid,
                                                      p_name,
                                                      p_source_tab,
                                                      NULL,
                                                      NULL,
                                                      ' (state)',
                                                      'END',
                                                      'aiannhfsr',
                                                      'S');

      RETURN output;

   END ADD_STATE_TO_AIA_NAME;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION ADD_TO_NAME_BASED_ON_SOURCE (
      p_oid                IN NUMBER,
      p_name               IN VARCHAR2,
      p_source_tab         IN VARCHAR2,
      p_alt_oid            IN NUMBER DEFAULT NULL,
      p_alt_source_tab     IN VARCHAR2 DEFAULT NULL,
      p_text_to_add        IN VARCHAR2 DEFAULT ' (state)',  --Defaults shown just for easy thinkin
      p_side_to_add        IN VARCHAR2 DEFAULT 'END',
      p_source_col         IN VARCHAR2 DEFAULT 'aiannhfsr',
      p_add_when_source    IN VARCHAR2 DEFAULT 'S',
      p_source_pkc         IN VARCHAR2 DEFAULT 'oid'
   ) RETURN VARCHAR2
   AS

      --Matt! 5/15/12
      --Generically: When the input has a column with a special value, add some special text to the output name
      --See GZ_OUTPUT.ADD_STATE_TO_AIA_NAME for the simpler wrapper we usually use

      --Here's a ridiculous example that changes the county name of Hartford to "special! Hartford" based on its area
      --update z609in_050 a set a.name =
      --GZ_OUTPUT.ADD_TO_NAME_BASED_ON_SOURCE(a.oid_base,a.name,a.source_base,NULL,NULL,'special! ','START','arealand','1903891914')



      --sample plain sql
      --update z609in_250 a set a.name =
      --  GZ_OUTPUT.ADD_TO_NAME_BASED_ON_SOURCE(a.oid_base, a.name, a.source_base)

      --more complex sql
      --update z609in_250 a set a.name =
      --  GZ_OUTPUT.ADD_TO_NAME_BASED_ON_SOURCE(a.oid_base, a.name, a.source_base, a.oid_superior, a.source_superior)

      psql              VARCHAR2(4000);
      aiannhflag        VARCHAR2(4000);


   BEGIN

      IF p_side_to_add NOT IN ('START','END')
      THEN

         RAISE_APPLICATION_ERROR(-20001,'Sorry, what side is ' || p_side_to_add || '?');

      END IF;

      psql := 'SELECT a.' || p_source_col || ' '
           || 'FROM ' || p_source_tab || ' a '
           || 'WHERE '
           || 'a.' || p_source_pkc || ' = ' || p_oid;

      --sample: select a.aiannhfsr from tab10st09.aiannh a where a.oid = 25120350008106

      EXECUTE IMMEDIATE psql INTO aiannhflag;

      IF aiannhflag = p_add_when_source
      THEN

         IF p_side_to_add = 'END'
         THEN

            RETURN p_name || p_text_to_add;

         ELSE

            RETURN p_text_to_add || p_name;

         END IF;

      ELSIF (aiannhflag IS NOT NULL AND p_alt_source_tab IS NULL)
      OR (aiannhflag IS NULL AND p_alt_source_tab IS NULL)
      THEN

         --typically aiannhfsr = R
         --or maybe couldnt find a matching oid but we dont have an alternate source table to go to

         RETURN p_name;

      ELSIF aiannhflag IS NULL
      AND p_alt_source_tab IS NOT NULL
      THEN

         psql := 'SELECT a.' || p_source_col || ' '
              || 'FROM ' || p_alt_source_tab || ' a '
              || 'WHERE '
              || 'a.' || p_source_pkc || ' = ' || p_alt_oid;

         --sample: select a.aiannhfsr from tab10st09.aiannh a where a.oid = 25120350008106

          EXECUTE IMMEDIATE psql INTO aiannhflag;

          IF aiannhflag = p_add_when_source
          THEN

             IF p_side_to_add = 'END'
             THEN

                RETURN p_name || p_text_to_add;

             ELSE

                RETURN p_text_to_add || p_name;

             END IF;

          ELSE

             RETURN p_name;

          END IF;


      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'Unexpected input combo');

      END IF;


   END ADD_TO_NAME_BASED_ON_SOURCE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_CDB_LSAD_ABB (
      p_lsad               IN VARCHAR2,
      p_cdb_lsad_table     IN VARCHAR2   --ex acs13cdb.lut_lsad
   ) RETURN VARCHAR2 DETERMINISTIC
   AS

      --Matt! 2/3/14
      --Attempt to allow us to avoid having the lut_lsad live in the GZ schema
      --Of course now the CDB name will have to change in our param tables with each release

      --sample usage in gz_layers_fields.update_fields
      --set a.lsad = GZ_OUTPUT.GET_CDB_LSAD_ABB(a.lsad, 'ACS13CDB.LUT_LSAD')

      --or a tester
      --select GZ_OUTPUT.GET_CDB_LSAD_ABB('M1','ACS13CDB.LUT_LSAD') from dual

   BEGIN

      RETURN GZ_OUTPUT.GET_RELATED_VALUE_FROM_LUT(p_lsad,
                                                  p_cdb_lsad_table,
                                                  'standard',
                                                  'lsad',
                                                  'Y');           --allow nulls.  00 usually

   END GET_CDB_LSAD_ABB;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_STANDARD_LSAD_ABB (
      p_lsad               IN VARCHAR2,
      p_release            IN VARCHAR2 DEFAULT NULL,
      p_release_col        IN VARCHAR2 DEFAULT 'RELEASE'
   ) RETURN VARCHAR2 DETERMINISTIC
   AS

      --Matt! 5/16/12
      --simplified wrapper with our standard usage of GET_RELATED_VALUE_FROM_LUT
      --intended to be used with a local copy of the LUT_LSAD table. If you dont have one,
      --   see GET_CDB_LSAD_ABB above

      --Sample parameter in gz_layers_fields.update_fields -- your local LUT_LSAD table has no RELEASE column
      --a.lsad = GZ_OUTPUT.GET_STANDARD_LSAD_ABB(a.lsad)

      --or new syntax with RELEASE in the lut_lsad table
      --a.lsad = GZ_OUTPUT.GET_STANDARD_LSAD_ABB(a.lsad, 'ACS12')

      --Turns Hartford into "Hartford County"

      --Stephanie says:
      --when passed an LSAD - returns the standard
      --abbreviation from a table or view called
      --"LUT_LSAD" in the user's schema (we should make
      --that table name a variable or have another way
      --to refer to the LUT_LSAD table in a different
      --schema, I am not sure how to make it easy to
      --control.  Maybe we could get the "release" from a
      --different part of the program and then refer to the
      --dadsgen lut_lsad view which could hold multiple
      --benchmark's worth.)

      output            VARCHAR2(4000);


   BEGIN

      output := GZ_OUTPUT.GET_RELATED_VALUE_FROM_LUT(p_lsad,
                                                     'LUT_LSAD', --schema?
                                                     'standard',
                                                     'lsad',
                                                     'Y',           --allow nulls.  00 usually
                                                     p_release,
                                                     p_release_col);

      RETURN output;

   END GET_STANDARD_LSAD_ABB;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_RELATED_VALUE_FROM_LUT (
      p_value              IN VARCHAR2,
      p_lut_table          IN VARCHAR2,
      p_lut_col_we_want    IN VARCHAR2,
      p_lut_col_we_match   IN VARCHAR2,
      p_allow_null         IN VARCHAR2 DEFAULT 'N', --must decline coverage
      p_value2             IN VARCHAR2 DEFAULT NULL,
      p_lut_col_we_match2  IN VARCHAR2 DEFAULT NULL
   ) RETURN VARCHAR2
   AS

      --Matt! 5/16/12
      --See GZ_OUTPUT.GET_STANDARD_LSAD_ABB for simpler and more likely usage

      --Generic LUT lookup function
      --Based on some column in one of our output tables
      --Get related values from an LUT

      output               VARCHAR2(4000);
      psql                 VARCHAR2(4000);

   BEGIN

      psql := 'SELECT a.' || p_lut_col_we_want || ' '
           || 'FROM '
           || p_lut_table || ' a '
           || 'WHERE a.' || p_lut_col_we_match || ' = :p1 ';

      IF p_value2 IS NULL
      THEN

         EXECUTE IMMEDIATE psql INTO output USING p_value;

      ELSE

         psql := psql || 'AND '
                      || 'a.' || p_lut_col_we_match2 || ' = :p2 ';

         EXECUTE IMMEDIATE psql INTO output USING p_value,
                                                  p_value2;

      END IF;


      IF output IS NOT NULL
      THEN

         RETURN output;

      ELSIF output IS NULL
      AND p_allow_null = 'Y'
      THEN

         RETURN NULL;

      ELSIF output IS NULL
      AND p_allow_null = 'N'
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Didnt LU anything from LUT ' || p_lut_table || ' with value ' || p_value);

      ELSIF output IS NULL
      THEN

         RAISE_APPLICATION_ERROR(-20001, 'Weird allow null option ' || p_allow_null);

      END IF;


   END GET_RELATED_VALUE_FROM_LUT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION REPLACE_APOSTROPHES (p_string VARCHAR2)
      RETURN VARCHAR2
   AS
      /*
        Program Name:  replace_apostrophe
        Author:  Stephanie Spahlinger
        Creation Date: 20140210 (COPIED FROM CAMPS CR_METADATA)

        Usage:
           Call this program from inside another PL/SQL program.  One argument is
           required:
              p_string - the string you want to replace apostrophes in

        Purpose:
           Removes apostophes and replaces them with escaped apostophes
           (' becomes '') to accomodate the need to escape apostrophe
           characters in some functions

           Should probably use q'^^' quotes instead, but I don't really know how.

        Dependencies:
           None

        Modification History:

      */

      v_newstring   VARCHAR2 (4000);

   BEGIN

      -- Replace apostrophes in the given string with escaped apostrophes
      -- This is used by the PUBL name creator (GET_PUBL_NAME) to prep
      -- the string for use in REGEX REPLACE.

      -- search and replace "'" with "''" in string

      v_newstring :=
         REPLACE (p_string, '''', UNISTR ('\0027') || UNISTR ('\0027'));

      RETURN v_newstring;

   END REPLACE_APOSTROPHES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GET_PUBL_NAME (pLsad                IN VARCHAR2,
                           pBaseName            IN VARCHAR2,
                           pLUT_LSAD            IN VARCHAR2 DEFAULT 'LUT_LSAD',
                           pType                IN VARCHAR2 DEFAULT 'publ',
                           p_release            IN VARCHAR2 DEFAULT NULL,
                           p_release_col        IN VARCHAR2 DEFAULT 'RELEASE')
      RETURN VARCHAR2
   AS
      /**

        Program Name:  get_publ_name
        Author:  Stephanie Spahlinger
        Creation Date: February, 2014 based on CAMPS version of same for
                       public cartographic boundary files.

        Usage:
          There are two required parameters and two optional parameters.

          REQUIRED parameters:

                 pLsad - the two digit LSAD for the entity as a string
                         ('06' for county, etc.  Must be a string!)

                 pBaseName - the base name of the entity to append to the Lsad
                             (usully from the NAME field in TIGER)

          OPTIONAL parameters:

                 pLUT_LSAD - the name of the LSAD look up table to use.
                             This table is created with each benchmark
                             and stored in the CPB_LSAD schema or somewhere
                             in the cartographic DB.
                             If you leave this parameter off, it will assume
                             you have a table or view called LUT_LSAD in your
                             schema.

                 pType - the type of name to return.
                         legal values are:
                         'publ' - this is the default, it returns
                                         the full publication name
                         'stnd' - this returns the standard name
                         'shrt' - this returns the short name
                         if the user enters an illegal value, an error is
                         raised.

                  p_release  - the "release" name.  This is used and required to
                               get a unique record when more than one release
                               is stored in the referenced LUT_LSAD table
                               or the "release" column in the LUT_LSAD is filled in.

                  p_release_col  - the column name in the LUT_LSAD table where
                                   the "release" value is stored (see above).
                                   This is used to get a unique
                                   record when more than one release is stored
                                   in the referenced LUT_LSAD table.
                                   If the column name is something other than
                                   "release," you must pass a value for this
                                   parameter.

        Purpose:
          The purpose of this program is to return the full publication name or
          standard or short name variants for a single entity.  This is also
          called the NAMELSAD translation.

        Dependencies:
          This program relies on an LUT_LSAD table (the benhcmark-tied
          snapshot of MTFNS info with easy to substitute name styles)

          This fucntion uses GET_RELATED_VALUE_FROM_LUT to obtain the default
          publication name string we need to insert the base name into.

      */

      vPubName     VARCHAR2 (4000);
      vNameType    VARCHAR2 (4000);
      vType        VARCHAR2(4000) := lower(pType);
      vfull_name_format  VARCHAR2 (4000); -- to hold the publication name format,
                                                          -- like '<NAME> County'
      vBasename   VARCHAR2 (4000);      -- to hold adjusted name (to deal with
                                                       -- apostophe's in names
   BEGIN
      IF vType NOT IN ('publ', 'stnd', 'shrt')
      THEN
         DBMS_OUTPUT.put_line (
               'GET_PUBL_NAME:  WARNING!  No publication name generated, '
            || 'Name type requested must be ''publ'',''stnd'',''shrt''.  '
            || 'User entered '
            || pType
            || '.');
         RAISE_APPLICATION_ERROR (
            -20001,
            'GET_PUBL_NAME: Name type requested must be ''publ'',''stnd'',''shrt''.');
      END IF;

      -- Adjust basename by escaping apostrophes.  If the name
      -- contains an apostrophe, it will not work in the REGEX replace line
      -- below unless we do this.

      -- Could probably do this with q'^^' quotes, but not sure how.  This
      -- works though.

      vBasename := REPLACE_APOSTROPHES (pBasename);

      CASE
         WHEN (vType = 'publ')
         THEN
            vNameType := 'publ_name';
         WHEN (vType = 'stnd')
         THEN
            vNameType := 'stnd_name';
         WHEN (vType = 'shrt')
         THEN
            vNameType := 'shrt_name';
      END CASE;

      -- Concept of query ...
      -- PubName := replace "<NAME>" with pBasename in PubSuf;

      -- get full name format string from LUT.
      vfull_name_format :=  get_related_value_from_LUT(pLsad,pLUT_LSAD , vNameType, 'LSAD', 'Y', p_release, p_release_col);

      -- replace <NAME> in full name string with the name passed in.
      -- PubName := replace "<NAME>" with pBasename in publ_name or stnd_name or shrt_name;

      vPubName := regexp_replace(vfull_name_format,'<NAME>',pBaseName);

      RETURN vPubName;

   END GET_PUBL_NAME;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION DROP_POSTAL_ABB (
      p_name               IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/16/12
      --Got this from Stephanie, who in turn I think got it from Jay Spurlin
      --gz_output.drop_postal_abb('Ironwood, MI--WI')
      -- return -> Ironwood

      v_retval          VARCHAR2(4000);

   BEGIN
           -- Original from Jay, works for all CBSAs.
           -- He couldn't get UPPER reg exp to work, so he's using the whole alphabet.
           --   v_retval :=
           --          REGEXP_REPLACE (
           --             REGEXP_REPLACE (
           --                p_name,
           --                ', [ABCDEFGHIJKLMNOPQRSTUVWXYZ][ABCDEFGHIJKLMNOPQRSTUVWXYZ]',
           --                ''),
           --             '-[ABCDEFGHIJKLMNOPQRSTUVWXYZ][ABCDEFGHIJKLMNOPQRSTUVWXYZ]',
           --             '');

           -- updated to work for all UAs (drop trailing '-' if leftover)
           -- to handle UAs like 'Ironwood, MI--WI'

           v_retval :=
              REGEXP_REPLACE (
                 REGEXP_REPLACE (
                    REGEXP_REPLACE (
                       p_name,
                       ', [ABCDEFGHIJKLMNOPQRSTUVWXYZ][ABCDEFGHIJKLMNOPQRSTUVWXYZ]',
                       ''),
                    '-[ABCDEFGHIJKLMNOPQRSTUVWXYZ][ABCDEFGHIJKLMNOPQRSTUVWXYZ]',
                    ''),
                 '-+$',
                 '');
           RETURN v_retval;

   END DROP_POSTAL_ABB;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION FORCE_SUPERIOR_ATTRIBUTE (
      p_source_superior       IN VARCHAR2,
      p_oid_superior          IN VARCHAR2,
      p_fsl_col               IN VARCHAR2 DEFAULT 'CBSA',
      p_bench_col             IN VARCHAR2 DEFAULT 'CBSAFP',
      p_bench_key             IN VARCHAR2 DEFAULT 'OID'
   ) RETURN VARCHAR2
   AS

      --Matt! 8/23/12
      --This is a lot of smoke and mirrors to deal with the output layer 321
      --And the oh so special case of Holland Michigan where you cant get the cbsafp
      --  from the base layer incplace source, since Holland lists just one but in reality has 2
      --Go to the overlapping superior record instead and pull down to the layer

      --Note that this isnt efficient compared to a single update
      --do not use this mess for a giant table
      --Should be fine for layer 321 with maybe 1000 records


      --Expected usage, 100 pctof the time, at least for now
      --SET a.cbsa = GZ_OUTPUT.FORCE_SUPERIOR(a.source_superior,a.oid_superior)
      --SET a.cbsa = GZ_OUTPUT.FORCE_SUPERIOR(a.source_superior,a.oid_superior,'CBSA','CBSAFP','OID')

      --Or in standalone testing format
      --select GZ_OUTPUT.FORCE_SUPERIOR_ATTRIBUTE('ACS12.CBSA','26290345863994','CBSA','CBSAFP','OID') from dual
      -->43780

      output         VARCHAR2(4000);
      psql           VARCHAR2(4000);

   BEGIN

      psql := 'SELECT s.' || p_bench_col || ' '
           || 'FROM '
           || p_source_superior || ' s '
           || 'WHERE '
           || 's.' || p_bench_key || ' = :p1 ';

      BEGIN

         EXECUTE IMMEDIATE psql INTO output USING p_oid_superior;

      EXCEPTION
      WHEN OTHERS
      THEN

         RAISE_APPLICATION_ERROR(-20001,DBMS_UTILITY.format_error_backtrace || ' on ' || psql
                                       || ' ' || p_oid_superior);

      END;

      RETURN output;


   END FORCE_SUPERIOR_ATTRIBUTE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION COLUMN_IS_NESTED_ON_SOURCE (
      p_oid                   IN VARCHAR2,
      p_column_value          IN VARCHAR2,
      p_source_table          IN VARCHAR2,
      p_source_column         IN VARCHAR2,
      p_source_pkc_col        IN VARCHAR2 DEFAULT 'OID'
   ) RETURN NUMBER
   AS

      --Matt! 8/24/12
      --Intended to weakly parameterize the removal of
      -- bits of principal cities that fall outside of their CBSAs

      --sample usage in gz_layers_fields.update_fields
      --DELETE is special kluged in token
      --<DELETE> GZ_OUTPUT.COLUMN_IS_NESTED_ON_SOURCE(a.oid_base, a.cbsa, a.source_base, 'pccbsacoll', a.key_base) = 0

      --standalone examples

      --the part of Midland MI that is inside its Midland CBSA
      --select GZ_OUTPUT.COLUMN_IS_NESTED_ON_SOURCE('2789057313490','33220','ACS12.INCPLACE','pccbsacoll','OID') from dual
      -- return 1

      --The part of Midland MI that sticks out into Bay City CBSA
      --select GZ_OUTPUT.COLUMN_IS_NESTED_ON_SOURCE('2789057313490','13020','ACS12.INCPLACE','pccbsacoll','OID') from dual
      --return 0

      output      NUMBER;
      psql        VARCHAR2(4000);

   BEGIN

      /*select count(1) from ACS12.INCPLACE a,
        table(a.pccbsacoll) t
        where a.oid = '2789057313490'
        and t.column_value = '13020'
      */

      --could probably parameterize the nested table-ness
      psql := 'SELECT COUNT(1) '
           || 'FROM ' || p_source_table || ' a, '
           || 'TABLE(a.' || p_source_column || ') t '
           || 'WHERE '
           || 'a.' || p_source_pkc_col || ' = :p1 AND '
           || 't.column_value = :p2 ';

      EXECUTE IMMEDIATE psql INTO output USING p_oid,
                                               p_column_value;

      IF (output = 1 OR output = 0)
      THEN

         RETURN output;

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'May be ok, but this is unexpected. Got '
                                     || output || ' on oid, column_value ' || p_oid || ',' || p_column_value
                                     || ' with sql ' ||  psql);

      END IF;

   END COLUMN_IS_NESTED_ON_SOURCE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

    FUNCTION COUSUB_ESTIMATES_UNIV (
      p_oid                          IN VARCHAR2, --a.oid_base
      p_source_table            IN VARCHAR2, --a.source_base
      p_source_pkc_col         IN VARCHAR2 DEFAULT 'OID' --a.key_base
   ) RETURN NUMBER
   AS

      --Suzanne (and Matt!) 10/03/12
      --Intended to limit county subdivisions to the estimates universe only as spec'd in SEP_40500_CRD V1.3 August 7, 2012

      --sample usage in gz_layers_fields.update_fields
      --DELETE is special kluged in token
      --<DELETE> GZ_OUTPUT.COUSUB_ESTIMATES_UNIV(a.oid_base, a.source_base, a.key_base) = 0

      --standalone examples

      --if the first query = 0 then see if it will be kept given results of 2nd query
      --select GZ_OUTPUT.COUSUB_ESTIMATES_UNIV('2765092139848','ACS12.COUSUB','OID') from dual
      -- return 1

      --if results of second query are also 0 then remove the record from the universe
      --select GZ_OUTPUT.COUSUB_ESTIMATES_UNIV('2765092139848','ACS12.COUSUB','OID') from dual
      --return 0

      output      NUMBER;
      psql        VARCHAR2(4000);

   BEGIN

        --Include if:
        -- FIPS State code = ¿55¿ and FIPS County code = ¿079¿ (Milwaukee County, Wisconsin) or
        -- FIPS State code = ¿25¿ and FIPS County code = ¿025¿ (Suffolk County, Massachusetts)
        -- Functional Status code of county subdivision = ¿A¿, ¿B¿ or ¿C¿,

      psql := 'SELECT COUNT(1) '
           || 'FROM ' || p_source_table || ' a '
           || 'WHERE (((a.statefp = ''55'' AND a.countyfp = ''079'') '
           || 'OR (a.statefp = ''25'' AND a.countyfp = ''025'')) '
           || 'OR (a.funcstat IN (''A'',''B'',''C''))) '
           || 'AND ' || p_source_pkc_col || ' = :p1 ';

      EXECUTE IMMEDIATE psql INTO output USING p_oid;

      IF (output = 1) --If result is a 1, then keep it no matter what so just kick out and return a 1 now.

      THEN

               RETURN output;

      ELSIF (output = 0) --If result is a 0, then do next test to see if result saves this record from being removed.

      THEN

            --Include if:
            -- Contained within a county with at least one county subdivision with a Functional Status of ¿A¿, ¿B¿ or ¿C¿

               psql := 'SELECT COUNT(1) '
                || 'FROM ' || p_source_table || ' a '
                || 'WHERE a.' || p_source_pkc_col || ' = :p1 '
                || 'AND a.statefp||a.countyfp IN '
                || '(SELECT DISTINCT (a.statefp||a.countyfp) '
                || 'FROM ' || p_source_table || ' a '
                || 'WHERE a.vintage = ''90'' '
                || 'AND a.funcstat IN (''A'',''B'',''C'')) ';

           EXECUTE IMMEDIATE psql INTO output USING p_oid;

                 IF (output = 1 OR output = 0)

                        THEN

                        RETURN output;

                  ELSE

                        RAISE_APPLICATION_ERROR(-20001,'This is unexpected. Got '
                                     || output || ' on oid, column_value ' || p_oid || ' with sql ' ||  psql);

                  END IF;

      ELSE

                        RAISE_APPLICATION_ERROR(-20001,'This is unexpected. Got '
                                     || output || ' on oid, column_value ' || p_oid || ' with sql ' ||  psql);

      END IF;

   END COUSUB_ESTIMATES_UNIV;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE DELETE_SOME_REMAINDERS (
      p_table                 IN VARCHAR2,
      p_base_count            IN NUMBER,
      p_base_count_inequality IN VARCHAR2 DEFAULT '=',
      p_b_oid_column          IN VARCHAR2 DEFAULT 'OID_BASE',
      p_s_oid_column          IN VARCHAR2 DEFAULT 'OID_SUPERIOR'
   ) AS

      --Matt! and Suzanne 10/03/12
      --For some of the ACS12 part 2 layers theres a requirement to delete remainders
      --   that have just one other base associated with them.  Like for the Place/County
      --   split layer only include remainders for Counties that contain more than one place

      --This cant be done with a simple DELETE... SQL statement since it requires deleting
      --from the same table that contains the information needed to determine what to delete
      --yields the famous MUTATING table error

      --This procedure operates on the entire FSL table

      --standalone examples

      --GZ_OUTPUT.DELETE_SOME_REMAINDERS('Z699IN_FSL071V',1,'=');

      psql              VARCHAR2(4000);
      dead_remainders   GZ_TYPES.stringarray;

   BEGIN

      /*select oid_superior from Z699IN_FSL071V
         where oid_base IS NULL
         and oid_superior IN (
         select oid_superior from Z699IN_FSL071V
         where oid_base IS NOT NULL
         group by oid_superior
         having ( count(oid_superior) ) = 1)
       */

      psql := 'SELECT ' || p_s_oid_column || ' FROM '
           || p_table || ' '
           || 'WHERE '
           || p_b_oid_column || ' IS NULL AND '
           || p_s_oid_column || ' IN '
           || '(SELECT ' || p_s_oid_column || ' FROM ' || p_table || ' '
           || 'WHERE ' || p_b_oid_column || ' IS NOT NULL '
           || 'GROUP BY ' ||  p_s_oid_column || ' '
           || 'HAVING ( COUNT(' || p_s_oid_column || ') ) ' || p_base_count_inequality || ' :p1 )';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO dead_remainders USING p_base_count;

      psql := 'DELETE FROM ' || p_table || ' '
           || 'WHERE '
           || p_b_oid_column || ' IS NULL AND '
           || p_s_oid_column || ' = :p1 ';

      FORALL ii IN 1 .. dead_remainders.COUNT
        EXECUTE IMMEDIATE psql USING dead_remainders(ii);

      COMMIT;


   END DELETE_SOME_REMAINDERS;


   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE ADD_LAYER_TO_FACE (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2,  --The layer producing the values, like 050
      p_add_to_face        IN VARCHAR2,  --STATE
      p_face_table         IN VARCHAR2
   )
   AS

      --Matt! 5/11/12
      --10/17/13 Parameterized column length definition

      psql                 VARCHAR2(4000);
      oidz                 GZ_TYPES.stringarray;
      valz                 GZ_TYPES.stringarray;
      facez                GZ_TYPES.stringarray;
      TYPE numarray IS TABLE OF NUMBER
      INDEX BY PLS_INTEGER;
      facez_num            numarray;
      my_cursor            SYS_REFCURSOR;
      psql2                VARCHAR2(4000);
      dbug                 PLS_INTEGER := 0;
      kount                PLS_INTEGER;
      lengths              GZ_TYPES.stringarray;



   BEGIN

      --first the DDL
      --its technically possible to add the all digit layer (Ex 040) as the column
      --but tis way bad practice
      --hafta use "040" (double quotes) when referring to it

      --alter table z609in_face add STATE VARCHAR2(4000)
      --face feature table isnt part of this module
      --so on reruns of this module the column may still be present
      --if user already has an input layer of the same name we are in trouble

      --return a single length, as a character
      lengths := GZ_BUSINESS_UTILS.GET_REFERENCE_FACE_FIELDS(p_release,
                                                             p_gen_project_id,
                                                             'ATTRIBUTE',
                                                             'REFERENCE_FACE_FIELDS',
                                                             'FIELD_LENGTH',
                                                             p_add_to_face);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'ADD_LAYER_TO_FACE',NULL,
                                                  'Adding column ' || p_add_to_face || ' VARCHAR2(' || lengths(1) || ')'
                                                 || ' to ' || p_face_table,
                                                 NULL,NULL,p_release,psql);

      GZ_OUTPUT.ADD_COLUMN_TO_TABLE(p_face_table,
                                    p_add_to_face,
                                    'VARCHAR2(' || lengths(1) || ')');


      --bust out the primitive faces for our layer
      --and populate the face feature table with corresponding base_oids

      --get topo elements shorthand, revisit later if performance is a problem
      --sample
      --UPDATE z609in_face a
      --SET a.Z609IN_040 =
      --   (SELECT b.state
      --     FROM z609in_040 b, TABLE (b.topogeom.get_topo_elements ()) t
      --    WHERE a.face_id = t.topo_id)

      /*Too slow for big topos

      psql := 'UPDATE ' || p_face_table || ' a '
           || 'SET '
           || 'a.' || p_add_to_face || ' = '
           || '(SELECT b.' || p_add_to_face || ' FROM '
           || p_output_topology || '_FSL' || p_layer || 'V b, '
           || 'TABLE(b.topogeom.get_topo_elements()) t '
           || 'WHERE a.face_id = t.topo_id) ';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'ADD_LAYER_TO_FACE',NULL,
                                             'Updating column ' ||p_add_to_face
                                             || ' on ' || p_output_topology || '_FACE',
                                             NULL,NULL,p_release,psql);
                                             */

      psql := 'SELECT a.oid_base, a.' || p_add_to_face || ' '
           || 'FROM ' ||  p_output_topology || '_FSL' || p_layer || 'V a '
           || 'ORDER BY a.oid_base ';

      OPEN my_cursor FOR psql;

      LOOP

         FETCH my_cursor BULK COLLECT INTO oidz, valz LIMIT 10;
         EXIT WHEN oidz.COUNT = 0;

         FOR i IN 1 .. oidz.COUNT
         LOOP

            facez := GZ_TOPO_UTIL.GZ_GET_FSL_FACES(p_output_topology || '_FSL' || p_layer || 'V',
                                                   oidz(i),
                                                   'OID_BASE',
                                                   'TOPOGEOM');

            FOR jj IN 1 .. facez.COUNT
            LOOP

               facez_num(jj) := TO_NUMBER(facez(jj));

            END LOOP;

            facez.DELETE;

            psql2 := 'UPDATE ' || p_face_table || ' a '
                  || 'SET a.' || p_add_to_face || ' = :p1 '
                  || 'WHERE a.face_id = :p2 ';


            IF i = 1
            OR dbug = 1
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'ADD_LAYER_TO_FACE',NULL,
                                             'Updating column ' ||p_add_to_face
                                             || ' on ' || p_output_topology || '_FACE (current val ' || valz(i) || ')',
                                             NULL,NULL,p_release,psql2);

            END IF;

            FORALL ii IN 1 .. facez_num.COUNT
               EXECUTE IMMEDIATE psql2 USING valz(i),
                                             facez_num(ii);

            COMMIT;
            facez_num.DELETE;

         END LOOP;


      END LOOP;

      --check

      psql := 'SELECT COUNT(*) FROM '
            || p_face_table || ' '
            || 'WHERE ' || p_add_to_face || ' IS NULL ';

      EXECUTE IMMEDIATE psql INTO kount;

      IF kount = 0
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'ADD_LAYER_TO_FACE',NULL,
                                                'Success, all ' || p_add_to_face || ' on ' || p_face_table || ' have values ',
                                                 NULL,NULL,p_release,psql);

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'ADD_LAYER_TO_FACE',NULL,
                                                'Oops, ' || kount || ' ' || p_add_to_face || ' on ' || p_face_table || ' are NULL ',
                                                 NULL,NULL,p_release,psql);

         RAISE_APPLICATION_ERROR(-20001,'Oops, ' || kount || ' ' || p_add_to_face || ' on ' || p_face_table || ' are NULL ');


      END IF;


   END ADD_LAYER_TO_FACE;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE UPDATE_FIELDS (
      p_release            IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2
   )
   AS

      --Matt! 5/11/12

      psql                 VARCHAR2(4000);
      update_fields        GZ_TYPES.stringhash;
      update_string        VARCHAR2(4000);
      update_delimiter     VARCHAR2(1);
      update_statements    GZ_TYPES.stringarray;



   BEGIN

      --single row stringhash (set a.xxx = yyy | set a.bbbb = ccc) ( | )
      update_fields := GZ_OUTPUT.GET_UPDATE_FIELDS(p_release,
                                                   p_layer);

      update_string := update_fields.FIRST;

      IF update_string IS NOT NULL
      THEN

         IF update_fields(update_string) IS NOT NULL
         THEN

            --multiple statements split up

            IF update_fields(update_string) IN ('|','?')
            THEN

               --special special handling for pipe, question delimiters doubling as metacharacters. Maybe more
               update_statements := GZ_BUSINESS_UTILS.SPLIT(update_string,'\' || update_fields(update_string));

            ELSE

               update_statements := GZ_BUSINESS_UTILS.SPLIT(update_string,update_fields(update_string));

            END IF;

         ELSE

            --no delimiter, single statement
            update_statements(1) := update_string;

         END IF;


         FOR i IN 1 .. update_statements.COUNT
         LOOP

            IF TRIM(update_statements(i)) NOT LIKE '<DELETE>%'
            AND TRIM(update_statements(i)) NOT LIKE '<PROCEDURE>%'
            THEN

               --SOP. Intended use of the update_fields parameter

               psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V a '
                     || update_statements(i) || ' ';

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SPECIAL_HANDLING',NULL,
                                                      'Calling update stmt on ' || p_output_topology || '_FSL' || p_layer || 'V',
                                                      NULL,NULL,p_release,psql);

               EXECUTE IMMEDIATE psql;
               COMMIT;

            ELSIF TRIM(update_statements(i)) LIKE '<DELETE>%'
            THEN

               --special tacked on handling for layer 321 and the need to DELETE principle city parts
               --that slop outside of their CBSAs
               --WHERE GZ_OUTPUT.COLUMN_IS_NESTED_ON_SOURCE(a.oid_base, a.cbsa, a.source_base, 'pccbsacoll', a.key_base) = 0

               --pull out the <DELETE> token
               update_statements(i) := REGEXP_REPLACE(update_statements(i), '<DELETE>','');

               psql := 'DELETE FROM ' || p_output_topology || '_FSL' || p_layer || 'V a '
                    || 'WHERE ' || update_statements(i) || ' ';

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SPECIAL_HANDLING',NULL,
                                                      'Calling special DELETE stmt on ' || p_output_topology || '_FSL' || p_layer || 'V',
                                                      NULL,NULL,p_release,psql);

               EXECUTE IMMEDIATE psql;
               COMMIT;

            ELSIF TRIM(update_statements(i)) LIKE '<PROCEDURE>%'
            THEN

               --everything is special
               --this means call some generic procedure for some bizarre thing that has to happen to this wacky layer

               -- ex: GZ_OUTPUT.DELETE_SOME_REMAINDERS(<TABLE>,1,'=')
               --The procedure will probably have to be passed the name of the table we are dealing with

               --pull out the <PROCEDURE> token
               update_statements(i) := REGEXP_REPLACE(update_statements(i), '<PROCEDURE>','');

               --replace the <TABLE> token if its in there
               update_statements(i) := REGEXP_REPLACE(update_statements(i),
                                                      '<TABLE>',
                                                      '''' ||  p_output_topology || '_FSL' || p_layer || 'V''');

               psql := 'BEGIN '
                    || ' ' || update_statements(i) || '; '
                    || 'END; ';

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SPECIAL_HANDLING',NULL,
                                                      'Calling special PROCEDURE stmt on ' || p_output_topology || '_FSL' || p_layer || 'V',
                                                      NULL,NULL,p_release,psql);

               EXECUTE IMMEDIATE psql;
               COMMIT;



            ELSE

               RAISE_APPLICATION_ERROR(-20001,'Fiddlesticks');

            END IF;

         END LOOP;

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_FIELDS',NULL,
                                                'No update stmt for ' || p_output_topology || '_FSL' || p_layer || 'V',
                                                NULL,NULL,p_release);

      END IF;

   END UPDATE_FIELDS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE UPDATE_GEO_ID (
      p_release            IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_layer              IN VARCHAR2
   )
   AS

      --Matt! 5/11/12

      psql              VARCHAR2(4000);
      gv                VARCHAR2(4000);
      gc                VARCHAR2(4000);
      separator         VARCHAR2(4000);
      geocodes          VARCHAR2(4000);
      geocode_arr       GZ_TYPES.stringarray;



   BEGIN

      --get rid of the temporary split base geo_ids we may have added to the work table

      psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V v '
           || 'SET v.geo_id = NULL ';

      EXECUTE IMMEDIATE psql;
      COMMIT;

      psql := 'SELECT a.gv, a.gc, a.separator, a.geocodes '
           || 'FROM GZ_LAYERS_GEOID a '
           || 'WHERE '
           || 'a.sum_lev = :p1 AND '
           || 'a.release = :p2 ';

      EXECUTE IMMEDIATE psql INTO gv, gc, separator, geocodes USING UPPER(p_layer),
                                                                    UPPER(p_release);

      --sample
      --update z609in_050 a
      --set a.geo_id = '050' || '00' || '00' || 'US' || a.state || a.county


      psql := 'UPDATE ' || p_output_topology || '_FSL' || p_layer || 'V a '
           || 'SET '
           || 'a.geo_id = :p1 || :p2 || :p3 || :p4 ';

      geocode_arr := GZ_BUSINESS_UTILS.SPLIT(geocodes, ',');

      FOR i IN 1 .. geocode_arr.COUNT
      LOOP

         psql := psql || ' || a.' || geocode_arr(i);

      END LOOP;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_GEO_ID',NULL,
                                             'Calling geoid update stmt on ' || p_output_topology || '_FSL' || p_layer || 'V',
                                             NULL,NULL,p_release,psql);

      EXECUTE IMMEDIATE psql USING p_layer, gv, gc, separator;
      COMMIT;


      --CHEAP CHEAP CHEAP says the chicken

      psql := 'ALTER TABLE ' || p_output_topology || '_FSL' || p_layer || 'V '
              || 'ADD ('
              || '   CONSTRAINT ' || p_output_topology || '_FSL' || p_layer || 'VPKC '
              || '      PRIMARY KEY(GEO_ID) '
              || ')';

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'UPDATE_GEO_ID',NULL,
                                             'Making geo_id the primary key for ' || p_output_topology || '_FSL' || p_layer || 'V',
                                             NULL,NULL,p_release,psql);

      EXECUTE IMMEDIATE psql;


   END UPDATE_GEO_ID;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION SPECIAL_HANDLING (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_source_schema      IN VARCHAR2,
      p_source_topology    IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_single_layer       IN VARCHAR2,
      p_face_table         IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/10/12
      --Special update
      --  execute the updates in the order they are entered in this field as some updates rely on the results of previous ones
      --GEOID pop
      --add_to_face


      output               VARCHAR2(4000) := '0';
      psql                 VARCHAR2(4000);
      layerz               GZ_TYPES.GZ_LAYERS_OUT_INFO;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('SPECIAL_HANDLING: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SPECIAL_HANDLING',NULL,
                                             'STARTING for ' || p_single_layer|| ' '  || p_output_topology,NULL,NULL,p_release);


      IF p_single_layer IS NULL
      THEN

         layerz := GZ_OUTPUT.GET_LAYERS_OUT_INFO(p_output_topology);

      ELSE

         --SOP
         layerz := GZ_OUTPUT.GET_LAYERS_OUT_INFO(p_output_topology, NULL, p_single_layer);

      END IF;

      FOR i IN 1 .. layerz.COUNT
      LOOP

         --special update for most layers

         GZ_OUTPUT.UPDATE_FIELDS(p_release,
                                 p_output_topology,
                                 layerz(i).layer);


         --populate geoid for all layers

         GZ_OUTPUT.UPDATE_GEO_ID(p_release,
                                 p_output_topology,
                                 layerz(i).layer);

         IF layerz(i).add_to_face IS NOT NULL
         THEN


            GZ_OUTPUT.ADD_LAYER_TO_FACE(p_release,
                                        p_gen_project_id,
                                        p_output_topology,
                                        layerz(i).layer,
                                        layerz(i).add_to_face,
                                        p_face_table);

         END IF;


      END LOOP;



      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('SPECIAL_HANDLING: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SPECIAL_HANDLING',NULL,
                                             'Complete for ' || p_output_topology,NULL,NULL,p_release);

      RETURN output;


   END SPECIAL_HANDLING;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION POPULATE_MEASUREMENTS (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_output_topology       IN VARCHAR2,
      p_single_layer          IN VARCHAR2,
      p_face_table            IN VARCHAR2,
      p_prcs_slivers          IN VARCHAR2 DEFAULT 'N',
      p_sliver_restart_flag   IN VARCHAR2 DEFAULT 'N',
      p_sliver_width          IN NUMBER DEFAULT NULL,
      p_segment_length        IN NUMBER DEFAULT NULL,
      p_expendable_review     IN VARCHAR2 DEFAULT 'N',
      p_reshape_review        IN VARCHAR2 DEFAULT 'Y',
      p_tolerance             IN NUMBER DEFAULT .05,
      p_srid                  IN NUMBER DEFAULT 8265,  --This is implicitly a "to" srid if diff
      p_restart_flag          IN VARCHAR2 DEFAULT 'N',
      p_topofix_edge          IN VARCHAR2 DEFAULT 'Y',
      p_topofix_2edge         IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa            IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2
   AS


      --Matt! 5/11/12
      --M@! Adding sliver processing August 2013
      --    Reminder to future: Sliver processing cant occur before this point
      --                        since it needs to know about expendability and
      --                        extinction for fully built and registered
      --                        feature tables
      --    Also added fix_edge and fix_face (check_face_tab)
      --! 12/3/13 added p_close_edge and default to N

      --Return values
      --0   - success
      --1|% - success with warnings, continue running to the end of output build
      --      this includes any fix_edge, fix_face, or coastal_sliver processing
      --      where something didnt get fixed as requested
      --2|% - fail, stop processing


      output               VARCHAR2(4000) := '0';
      psql                 VARCHAR2(4000);
      layerz               GZ_TYPES.GZ_LAYERS_OUT_INFO;
      kount                PLS_INTEGER;
      topo_srid            NUMBER;
      sliver_output        VARCHAR2(4000);
      edgefix_val          VARCHAR2(1);
      topofix_val          VARCHAR2(1);

   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                  'STARTING for ' || p_output_topology,NULL,NULL,p_release);

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                  'Gathering stats on ' || p_output_topology,NULL,NULL,p_release);

      --doesnt really belong here
      --but all the tables are pretty well populated, and may save us some slowness ahead
      GZ_TOPO_UTIL.GATHER_TOPO_STATS(p_output_topology);

      topo_srid := GZ_TOPO_UTIL.GET_SDO_TOPO_METADATA_NUM(p_output_topology,
                                                          p_face_table,
                                                          'srid');

      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      --Coastal Slivers--------------------------------------------------------
      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--


      IF p_prcs_slivers = 'Y'
      AND (p_sliver_restart_flag = 'N' OR p_sliver_restart_flag IS NULL)
      THEN

         --PROCESS SLIVERS with no SDO in the mix
         --can currently handle Z9 here too

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                     'Calling coastal sliver processing with no sdogeometry processing');

         BEGIN

            sliver_output := GZ_SLIVER.PROCESS_COASTAL_SLIVERS(p_release,
                                                               p_gen_project_id,
                                                               p_output_topology,
                                                               'OUTPUT',
                                                               p_sliver_width,
                                                               p_segment_length,                                                               
                                                               'N',
                                                               p_expendable_review,
                                                               p_reshape_review,
                                                               'N'); --no, dont deal with sdogeometry. Will calculate it in the module next

         EXCEPTION
         WHEN OTHERS
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                       'Coastal sliver processing bombed',
                                                       p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            output := '2| ' || ' Coastal sliver processing bombed ' || CHR(10) || output;

         END;

         IF sliver_output IS NOT NULL
         THEN

            --possible errors here are failed input checks
            --or unable to fix all slivers

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                       'Coastal sliver processing returned with an error message',
                                                       p_error_msg => sliver_output);

            output := '1| ' || sliver_output || CHR(10) || output;

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                        'Coastal sliver processing completed with no known errors');

         END IF;

      END IF;

      IF (p_sliver_restart_flag = 'N' OR p_sliver_restart_flag IS NULL)  --dont do any fixing on pure sliver restarts
      THEN

         IF p_topofix_edge = 'Y'
         THEN

            --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
            --Fix Edge---------------------------------------------------------------
            --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--

            --By now, with coastal slivers removed, should be down to a very rare straggling bad edge
            --Fix_edge does not update any sdogeometry

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                        'Call gz_fix_edge');

            --return char 1 for some still bad, 0 for all good

            edgefix_val := GZ_TOPOFIX.GZ_FIX_EDGE(p_release || '_' || p_gen_project_id,
                                                  p_output_topology,
                                                  'OUTPUT', --log type
                                                  'Y', --hold univeral
                                                  p_tolerance,
                                                  NULL, --no insistence on loop counts. Continue as long as there is progress
                                                  p_topofix_2edge); --check close edge, mad slow if Y

            IF edgefix_val = '0'
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'GZ_FIX_EDGE',NULL,
                                                           'gz_fix_edge success');

            ELSIF edgefix_val = '1'
            AND p_topofix_qa = 'Y'
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'GZ_FIX_EDGE',NULL,
                                                           'GZ_FIX_EDGE not successful, we will fail the job to be safe');

               IF output LIKE '2%'
               THEN

                  output := output || CHR(10) || ' Also failed to fix all edges in gz_fix_edge';

               ELSE

                  output := '1| ' || ' Failed to fix all edges in gz_fix_edge' || CHR(10) || output;

               END IF;


            ELSIF edgefix_val = '1'
            AND p_topofix_qa = 'N'
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'GZ_FIX_EDGE success',NULL,
                                                           'GZ_FIX_EDGE not successful, but we wont fail since topofix QA is N');


            ELSE

                RAISE_APPLICATION_ERROR(-20001,'Unknown edgefix result');

            END IF;

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                        'Skipping fix_edge since topofix_edge is ' || p_topofix_edge);

         END IF;


         --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         --Fix Face---------------------------------------------------------------
         --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--

         --always recalculate all face values
         --fix edge may have changed something, and just in case of any outside the workflow fiddling

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                     'Recalculate sdogeometry for all ' || p_face_table || ' faces');

         GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_face_table,
                                                   'FACE_ID',
                                                   'SDOGEOMETRY',
                                                   'ALL',
                                                   p_tolerance,
                                                   NULL);  --no TO srid. Stay in whatever get_geometry returns for face


         --fix face does update face sdo when it makes changes

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                     'Call fix face (CHECK_FACE_TAB)');

         --will update QC for all that it can't fix
         --and update sdo for those it does fix
         --Returns '0' for success, '1' for something bad
         --QC 1 is 13349, 13356 or some other validate_Geometry_with_context error
         --QC 2 is NULL sdogeometry
         --QC 3 is non-2003 gtype

         BEGIN

            topofix_val := GZ_TOPOFIX.CHECK_FACE_TAB(p_release || '_' || p_gen_project_id,
                                                     p_output_topology,
                                                     p_face_table,
                                                     'FACE_ID',
                                                     'OUTPUT',
                                                     p_tolerance,
                                                     topo_srid);

         EXCEPTION
         WHEN OTHERS
         THEN

            --can occasionally bomb.  For some reason I have the input verification checks
            --(most likely face$ -- feature face mismatch) in unprotected raise_application_errors

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                        'Fix face bombed, see error--> ',
                                                        p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

            topofix_val := 1;

         END;

         IF topofix_val <> '0'
         AND p_topofix_qa = 'Y'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                        'We cant get a valid geometry in ' || p_face_table || ' for some of our faces');

            IF output LIKE '2%'
            THEN

               output := output || CHR(10) || ' Also we cant get a valid geometry in ' || p_face_table || ' for some of our faces';

            ELSE

               output := '1| ' || ' We cant get a valid geometry in ' || p_face_table || ' for some of our faces ' || CHR(10) || output;

            END IF;

         ELSIF topofix_val <> '0'
         AND p_topofix_qa = 'N'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                        'We cant get a valid geometry in ' || p_face_table || ' for some of our faces '
                                                     || 'but wont fail since topofix QA is N');

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                        'Super: We have a valid geometry in ' || p_face_table || ' for all of our faces');

         END IF;

      END IF;

      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      --Populate Measurements on FSLS------------------------------------------
      --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--

      IF (p_sliver_restart_flag = 'N' OR p_sliver_restart_flag IS NULL)
      THEN

         IF p_single_layer IS NULL
         THEN

            --SOP
            layerz := GZ_OUTPUT.GET_LAYERS_OUT_INFO(p_output_topology);

         ELSE

            --single layer processing
            layerz := GZ_OUTPUT.GET_LAYERS_OUT_INFO(p_output_topology, NULL, p_single_layer);

         END IF;

         FOR i IN 1 .. layerz.COUNT
         LOOP

            --hmm no tiles. This does the entire table
            --Better return to this

            --make sure we have recs

            psql := 'SELECT count(*) '
                 || 'FROM ' || p_output_topology || '_FSL' ||  layerz(i).layer || 'V '
                 || 'WHERE rownum = 1';

            EXECUTE IMMEDIATE psql INTO kount;

            IF kount = 1
            AND (p_restart_flag = 'N' OR NOT (GZ_OUTPUT.IS_LAYER_THROUGH_MODULE(p_output_topology,
                                                                                layerz(i).layer,
                                                                                7)
                ))
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                      'Calc ' || p_output_topology || '_FSL' || layerz(i).layer || 'V sdogeometry ');


               IF p_srid IS NOT NULL
               AND topo_srid = p_srid
               THEN

                  GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_FSL' || layerz(i).layer || 'V',
                                                        'GEO_ID',
                                                        'SDOGEOMETRY',
                                                        'ALL',
                                                         p_tolerance);
                                                         --do not call with srid, will transform 8265 to 8265 and make some messes

                  --seems polite to do this no matter what
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                         'Index ' || p_output_topology || '_FSL' || layerz(i).layer || 'V sdogeometry ');

                  GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(p_output_topology || '_FSL' || layerz(i).layer || 'V',
                                                 'SDOGEOMETRY',
                                                 p_srid,
                                                 p_tolerance);

               ELSIF p_srid IS NOT NULL
               AND topo_srid <> p_srid
               THEN

                  --never even tried this

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                         'Calling with intent to transform to ' || p_srid || '. You are a titan of bravery ');

                  GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_FSL' || layerz(i).layer || 'V',
                                                        'GEO_ID',
                                                        'SDOGEOMETRY',
                                                        'ALL',
                                                         p_tolerance,
                                                         p_srid); --transformation

                                                                        --seems polite to do this no matter what
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                         'Index ' || p_output_topology || '_FSL' || layerz(i).layer || 'V sdogeometry ');

                  GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(p_output_topology || '_FSL' || layerz(i).layer || 'V',
                                                 'SDOGEOMETRY',
                                                 p_srid,
                                                 p_tolerance);

               ELSIF p_srid IS NULL
               THEN

                  --this is our special input to indicate Z9 translated albers
                  --ultimately headed to 1000082

                  --SOP first, make sdo in 8265
                  GZ_BUSINESS_UTILS.GZ_POPULATE_MEASUREMENTS(p_output_topology || '_FSL' || layerz(i).layer || 'V',
                                                        'GEO_ID',
                                                        'SDOGEOMETRY',
                                                        'ALL',
                                                         p_tolerance);

                  --call GZ_PROJECTION
                  --Trying to get a handle on the DDL required for gz_projection
                  --Have pre-created the 4 tables and 1 sequence, and will reuse them on each trip thru this loop

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                         'Calling GZ_PROJECTION.PROJECT_2007_TO_ALBERS');

                  GZ_PROJECTION.PROJECT_2007_TO_ALBERS(p_output_topology,
                                                       p_output_topology || '_FSL' || layerz(i).layer || 'V',
                                                       'SDOGEOMETRY',
                                                       'GEO_ID',                             --unique key col
                                                       SYS_CONTEXT('USERENV','CURRENT_SCHEMA'),
                                                       p_output_topology || '_OUT_Z9OUT',    --final table, holds the output
                                                       'NEW_GEOMETRY',                       --default, column name
                                                       NULL,                                 --exclude.  No clue
                                                       'FALSE',                              --key is varchar, Sidey sez "ignored"
                                                       'OUTPUT',                             --log type
                                                       p_output_topology || '_OUT_SEQ',      --sequence
                                                       p_output_topology || '_OUT_2003',     --2003 work table
                                                       p_output_topology || '_OUT_AREA',     --area work table
                                                       p_output_topology || '_OUT_Z9TMP'     --temp table, same format as _OUT_Z9OUT
                                                       );

                  --move normal 8265 geometry over to empty qa_geometry column
                  psql := 'UPDATE ' ||  p_output_topology || '_FSL' || layerz(i).layer || 'V v '
                       || 'SET '
                       || 'v.qa_sdogeometry = v.sdogeometry, '
                       || 'v.sdogeometry = NULL ';

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                         'Shifting sdogeometry to qa_sdogeometry',
                                                          NULL,NULL,p_release,psql);

                  EXECUTE IMMEDIATE psql;
                  COMMIT;

                  --update from temp table to layer table
                  psql := 'UPDATE ' ||  p_output_topology || '_FSL' || layerz(i).layer || 'V v '
                       || 'SET v.sdogeometry = ('
                       || 'SELECT z.new_geometry '
                       || 'FROM ' || p_output_topology || '_OUT_Z9OUT z '
                       || 'WHERE v.geo_id = z.geo_id) ';

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                         'Move translated geometry from temp table to layer table',
                                                          NULL,NULL,p_release,psql);

                  EXECUTE IMMEDIATE psql;
                  COMMIT;

                  --verify that we have geoms in all
                  psql := 'SELECT COUNT(*) FROM '
                       || p_output_topology || '_FSL' || layerz(i).layer || 'V v '
                       || 'WHERE v.sdogeometry IS NULL ';

                  EXECUTE IMMEDIATE psql INTO kount;

                  IF kount > 0
                  THEN

                     GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                            'Error: We have ' || kount || ' NULL sdogeometries in ' ||
                                                            p_output_topology || '_FSL' || layerz(i).layer || 'V v ',
                                                            NULL,NULL,p_release,psql);

                     RAISE_APPLICATION_ERROR(-20001,'We have ' || kount || ' NULL sdogeometries in ' ||
                                                     p_output_topology || '_FSL' || layerz(i).layer || 'V v ');

                  ELSIF kount = 0
                  THEN

                     --clean out the work tables
                     --so we can at least reuse them for the next layer
                     psql := 'DELETE FROM ' || p_output_topology || '_OUT_Z9OUT';
                     EXECUTE IMMEDIATE psql;
                     COMMIT;

                     psql := 'DELETE FROM ' || p_output_topology || '_OUT_2003';
                     EXECUTE IMMEDIATE psql;
                     COMMIT;

                     psql := 'DELETE FROM ' || p_output_topology || '_OUT_AREA';
                     EXECUTE IMMEDIATE psql;
                     COMMIT;

                     psql := 'DELETE FROM ' || p_output_topology || '_OUT_Z9TMP';
                     EXECUTE IMMEDIATE psql;
                     COMMIT;

                  END IF;

                  --seems polite to do this no matter what
                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                         'Index ' || p_output_topology || '_FSL' || layerz(i).layer || 'V sdogeometry ');

                  GZ_GEOM_UTILS.ADD_SPATIAL_INDEX(p_output_topology || '_FSL' || layerz(i).layer || 'V',
                                                 'SDOGEOMETRY',
                                                 1000082, --hard coded
                                                 p_tolerance);


               END IF;


            ELSE

               IF kount = 0
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                         'No records. Skipping calc ' || p_output_topology || '_FSL' || layerz(i).layer || 'V sdogeometry ');

               ELSE

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                         'Restart. Skipping previously calculated ' || p_output_topology || '_FSL' || layerz(i).layer || 'V sdogeometry ');


               END IF;

            END IF;


         END LOOP;

      ELSIF p_sliver_restart_flag = 'Y'
      AND p_prcs_slivers = 'Y'
      THEN

         --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
         --Coastal Sliver Restart ------------------------------------------------
         --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--

         --this is a restart solely for the purpose of processing slivers after interactive review
         --SDO is in the mix, will have to manage it in the sub
         --can NOT currently handle Z9 in this fashion

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                     'Calling coastal sliver restart with sdogeometry processing');

         IF p_srid IS NOT NULL
         THEN

            BEGIN

               sliver_output := GZ_SLIVER.PROCESS_COASTAL_SLIVERS(p_release,
                                                                  p_gen_project_id,
                                                                  p_output_topology,
                                                                  'OUTPUT',
                                                                  p_sliver_width,
                                                                  p_segment_length,
                                                                  'Y',  --yes restart
                                                                  p_expendable_review,
                                                                  p_reshape_review,
                                                                  'Y');  --Yes, maintain feature sdo
                                                                  
            EXCEPTION
            WHEN OTHERS
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                          'Coastal sliver processing bombed',
                                                          p_error_msg => SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);

               output := '2| ' || ' Coastal sliver processing bombed ' || CHR(10) || output;

            END;

         ELSE

            --make it obvious in case I or someone sets this up accidentally
            RAISE_APPLICATION_ERROR(-20001, 'Cant process Z9 on post-sliver review restarts');

         END IF;

         IF sliver_output IS NOT NULL
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                       'Coastal sliver restart processing returned with an error message',
                                                       p_error_msg => sliver_output);

            output := '1| ' || sliver_output || CHR(10) || output;

         ELSE

           GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                       'Coastal sliver restart processing completed with no known errors');

         END IF;

      ELSE

         RAISE_APPLICATION_ERROR(-20001, 'sliver_restart_flag is ' || p_sliver_restart_flag || ' and process slivers is '
                                         || p_prcs_slivers || '. What gives?');
      END IF;

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('POPULATE_MEASUREMENTS: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                  'Complete for ' || p_output_topology,NULL,NULL,p_release);

      RETURN output;

   END POPULATE_MEASUREMENTS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   PROCEDURE DROP_WORK_COLUMNS (
      p_table              IN VARCHAR2
   )
   AS

      --Matt! 5/11/12

      psql              VARCHAR2(4000);
      work_cols         GZ_TYPES.stringarray;

   BEGIN

      work_cols := GZ_OUTPUT.LEGAL_WORK_COLUMNS();

      --alter table z609in_040 drop (oid_base, oid_superior);

      psql := 'ALTER TABLE ' || p_table || ' DROP (';

      FOR i IN 1 .. work_cols.COUNT
      LOOP

         IF i <> work_cols.COUNT
         THEN

            psql := psql || work_cols(i) || ', ';

         ELSE

            psql := psql || work_cols(i) || ') ';

         END IF;

      END LOOP;

      BEGIN

         EXECUTE IMMEDIATE psql;

      EXCEPTION
      WHEN OTHERS THEN

         RAISE_APPLICATION_ERROR(-20001,SQLERRM || ' on DDL: ' || psql);

      END;


   END DROP_WORK_COLUMNS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION PREPARE_FOR_SHAPES (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_single_layer       IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_drop_work_tables   IN VARCHAR2
   ) RETURN VARCHAR2
   AS


      --Matt! 5/11/12
      --Not sure what will go in here
      --Drop my working columns for sure
      --Create views possibly?


      output               VARCHAR2(4000) := '0';
      psql                 VARCHAR2(4000);
      layerz               GZ_TYPES.GZ_LAYERS_OUT_INFO;


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('PREPARE_FOR_SHAPES: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PREPARE_FOR_SHAPES',NULL,
                                             'STARTING for ' || p_output_topology,NULL,NULL,p_release);


      IF p_single_layer IS NULL
      THEN

         --SOP
         layerz := GZ_OUTPUT.GET_LAYERS_OUT_INFO(p_output_topology);

      ELSE

         --single layer processing
         layerz := GZ_OUTPUT.GET_LAYERS_OUT_INFO(p_output_topology, NULL, p_single_layer);

      END IF;

      IF p_drop_work_tables = 'Y'  --default
      THEN

         FOR i IN 1 .. layerz.COUNT
         LOOP

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PREPARE_FOR_SHAPES',NULL,
                                                'Dropping work columns on ' || p_output_topology || '_FSL' || layerz(i).layer || 'V',
                                                 NULL,NULL,p_release);


            GZ_OUTPUT.DROP_WORK_COLUMNS(p_output_topology || '_FSL' || layerz(i).layer || 'V');


         END LOOP;

      END IF;


      --Thats all for now I guess

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('PREPARE_FOR_SHAPES: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PREPARE_FOR_SHAPES',NULL,
                                             'Complete for ' || p_output_topology,NULL,NULL,p_release);

      RETURN output;

   END PREPARE_FOR_SHAPES;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   FUNCTION VALIDATE_LAYER_SDO (
      p_table_name         IN VARCHAR2,
      p_pkc_col            IN VARCHAR2,
      p_qc_col             IN VARCHAR2,
      p_sdo_col            IN VARCHAR2,
      p_tolerance          IN NUMBER
   ) RETURN NUMBER
   AS

      --Matt! 5/15/12

      psql                 VARCHAR2(4000);
      output               NUMBER := 0;
      geo_idz              GZ_TYPES.stringarray;
      returncodez          GZ_TYPES.stringarray;

   BEGIN

      --one SQL
      --case will return first found in the order

      psql := 'SELECT a.' || p_pkc_col || ', '
           || 'CASE '
           || 'WHEN sdo_geom.validate_geometry_with_context(a.' || p_sdo_col || ', :p1) <> :p2 THEN :p3 '
           || 'WHEN (a.' || p_sdo_col || '.sdo_gtype IS NULL) THEN :p4 '
           || 'WHEN (a.' || p_sdo_col || '.sdo_gtype <> :p5 AND a.' || p_sdo_col || '.sdo_gtype <> :p6) THEN :p7 '
           || 'END '
           || 'FROM ' || p_table_name || ' a '
           || 'WHERE '
           || '(a.' || p_sdo_col || '.sdo_gtype <> :p8 AND a.' || p_sdo_col || '.sdo_gtype <> :p9) OR '
           || '(a.' || p_sdo_col || '.sdo_gtype IS NULL) OR '
           || 'sdo_geom.validate_geometry_with_context(a.' || p_sdo_col || ', :p10) <> :p11 ';

      EXECUTE IMMEDIATE psql BULK COLLECT INTO geo_idz,
                                               returncodez USING p_tolerance, 'TRUE', 1,
                                                                 2,
                                                                 2003, 2007, 3,
                                                                 2003, 2007,
                                                                 p_tolerance, 'TRUE';

      IF geo_idz.COUNT > 0
      THEN

         GZ_OUTPUT.ADD_COLUMN_TO_TABLE(p_table_name,
                                       p_qc_col,
                                       'NUMBER',
                                       'Y');  --replace col if it exists

         --These QC values match other module qc vals on the face table
         -- 1 - Invalid at tolerance
         -- 2 - NULL sdogeometry
         -- 3 - Gtype bad

         psql := 'UPDATE ' || p_table_name || ' a '
              || 'SET '
              || 'a.' || p_qc_col || ' = :p1 '
              || 'WHERE a.' || p_pkc_col || ' = :p2 ';

         FORALL ii IN 1 .. geo_idz.COUNT
            EXECUTE IMMEDIATE psql USING returncodez(ii),
                                         geo_idz(ii);

         COMMIT;

      END IF;

      IF geo_idz.COUNT = 0
      THEN

         RETURN 0;

      ELSE

         RETURN geo_idz.COUNT;

      END IF;


   END VALIDATE_LAYER_SDO;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Private--------------------------------------------------------------------------------

   PROCEDURE WRITE_SUMMARY_REPORT (
      p_topology           IN VARCHAR2,
      layers               IN GZ_TYPES.GZ_LAYERS_OUT_INFO
   )
   AS

     psql               VARCHAR2(4000);
     kount              PLS_INTEGER;

   BEGIN

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_topology,'VALIDATE_OUTPUT',NULL,
                                             '***RECORD COUNT FOR EACH LAYER***');

      --just in case this gets captured and someone prefers to look at the ticker tape
      dbms_output.put_line('OUTPUT ' || p_topology || ': ***RECORD COUNT FOR EACH LAYER***');


      FOR i IN 1 .. layers.COUNT
      LOOP

         psql := 'SELECT COUNT(*) FROM '
              || p_topology || '_FSL' || layers(i).layer || 'V';

         EXECUTE IMMEDIATE psql INTO kount;

         IF kount > 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_topology,'VALIDATE_OUTPUT',NULL,
                                                   kount || ' -- ' ||  layers(i).layer);

            dbms_output.put_line('   ' || kount || ' -- ' ||  layers(i).layer);

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_topology,'VALIDATE_OUTPUT',NULL,
                                                   'Warning: ' || kount || ' -- ' ||  layers(i).layer);

            dbms_output.put_line('   Warning: ' || kount || ' -- ' ||  layers(i).layer);

         END IF;


      END LOOP;

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_topology,'VALIDATE_OUTPUT',NULL,
                                             '***RECORD COUNT FOR EACH LAYER***');

      dbms_output.put_line('OUTPUT ' || p_topology || ': ***RECORD COUNT FOR EACH LAYER***');

   END WRITE_SUMMARY_REPORT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION VALIDATE_OUTPUT (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_single_layer       IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_tile_kount         IN NUMBER DEFAULT 10,
      p_tolerance          IN NUMBER DEFAULT .05,
      p_validate_topo      IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2
   AS


      --Matt! 5/11/12
      --! Added overlap check 1/25/13

      --Validate (and possibly fix) sdo for each layer
      --   If invalid add a QC col and populate
      --   Cant reuse existing topofix, it is face only
      --Check each layer for spatial overlaps
      --Validate topology
      --Write record count report to log



      output               VARCHAR2(4000) := '0';
      psql                 VARCHAR2(4000);
      layerz               GZ_TYPES.GZ_LAYERS_OUT_INFO;
      retval               NUMBER;
      tiles                GZ_TYPES.geomarray;
      validstr             VARCHAR2(4000);


   BEGIN

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VALIDATE_OUTPUT: Starting');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                             'STARTING for ' || p_output_topology,NULL,NULL,p_release);


      ------------------
      --Layer validation
      ------------------

      IF p_single_layer IS NULL
      THEN

         --SOP
         layerz := GZ_OUTPUT.GET_LAYERS_OUT_INFO(p_output_topology);

      ELSE

         --single layer processing
         layerz := GZ_OUTPUT.GET_LAYERS_OUT_INFO(p_output_topology, NULL, p_single_layer);

      END IF;

      FOR i IN 1 .. layerz.COUNT
      LOOP

         --return invalid feature count
         retval := GZ_OUTPUT.VALIDATE_LAYER_SDO(p_output_topology || '_FSL' || layerz(i).layer || 'V',
                                                'GEO_ID',      --pkc
                                                'QC',          --col to add and update
                                                'SDOGEOMETRY', -- sdo col to check
                                                p_tolerance);

         IF retval <> 0
         THEN


            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                   'UH OH, ' || retval || ' bad sdo in ' || p_output_topology || '_FSL' || layerz(i).layer ||'V',
                                                   NULL,NULL,p_release);

            output := output || ' | Bad sdo in table ' || p_output_topology || '_FSL' || layerz(i).layer || 'V';

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                   'All sdo in ' || p_output_topology || '_FSL' || layerz(i).layer || 'V is valid ',
                                                   NULL,NULL,p_release);


         END IF;


         --check for overlap
         retval := GZ_TOPO_UTIL.GZ_COUNT_FSL_OVERLAPS(p_output_topology,
                                                      p_output_topology || '_FSL' || layerz(i).layer || 'V',
                                                      'TOPOGEOM');

         IF retval <> 0
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                   'UH OH, ' || retval || ' spatial overlaps in ' || p_output_topology || '_FSL' || layerz(i).layer ||'V',
                                                   NULL,NULL,p_release);

            output := output || ' | Spatial overlap in table ' || p_output_topology || '_FSL' || layerz(i).layer || 'V';

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                   'All topo in ' || p_output_topology || '_FSL' || layerz(i).layer || 'V is non-overlapping ',
                                                   NULL,NULL,p_release);


         END IF;


      END LOOP;


      ------------------
      --Topo validation
      ------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                             'Calling topo tune up on ' || p_output_topology,NULL,NULL,p_release);

      GZ_TOPO_UTIL.GZ_TOPO_TUNE_UP(p_output_topology);

      IF p_validate_topo = 'Y'
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                'Getting tiles to use in validating ' || p_output_topology,NULL,NULL,p_release);

         --try to find us some tiles
         tiles := GZ_OUTPUT.GET_OUTPUT_TILES(p_output_topology,
                                             p_release,
                                             p_gen_project_id,
                                             p_tile_kount,
                                             p_face_table);


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                'STARTING topo validation for ' || p_output_topology,NULL,NULL,p_release);

         BEGIN

            validstr := GZ_TOPO_UTIL.VALIDATE_TOPOLOGY_TILE(p_output_topology,
                                                            tiles,
                                                            p_log_type => 'OUTPUT');

         EXCEPTION
         WHEN OTHERS
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                   'Boo: ' || p_output_topology || ' is not valid or validation threw an error');

            --this is a FAIL but we'll continue
            output := output || ' VALIDATE_TOPOLOGY_TILE threw ' || SQLERRM || ' |';

         END;

         IF validstr = 'TRUE'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                   'Sweet: ' || p_output_topology || ' is valid ');

         END IF;

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                     'Skipping topo validation since validate_topo is ' || p_validate_topo);

      END IF;


      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                  'Gathering stats on ' || p_output_topology,NULL,NULL,p_release);

      --also did this periodically and at the start of special handling
      GZ_TOPO_UTIL.GATHER_TOPO_STATS(p_output_topology);


      ------------------
      --Reports
      ------------------

      --Some day there may be an entity check of some sort here
      --Not doing one makes me nervous
      --For now lets write a report to the log

      GZ_OUTPUT.WRITE_SUMMARY_REPORT(p_output_topology,
                                     layerz);

      ----------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      DBMS_APPLICATION_INFO.SET_ACTION('');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('VALIDATE_OUTPUT: Peace out');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      ----------------------------------------------------------------------------------

      GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                             'Complete for ' || p_output_topology,NULL,NULL,p_release);

      RETURN output;

   END VALIDATE_OUTPUT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION TIDY_EXIT (
      p_release            IN VARCHAR2,
      p_gen_project_id     IN VARCHAR2,
      p_output_topology    IN VARCHAR2,
      p_drop_work_tables   IN VARCHAR2,
      p_single_layer       IN VARCHAR2,
      p_face_table         IN VARCHAR2,
      p_fixes_retval       IN VARCHAR2
   ) RETURN VARCHAR2
   AS

      --Matt! 5/15/12

      output            VARCHAR2(4000) := '0';

   BEGIN

      IF p_drop_work_tables = 'Y'
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'TIDY_EXIT',NULL,
                                                     'Dropping work tables ');

         BEGIN

            GZ_BUSINESS_UTILS.DROP_MODULE_WORK_TABLES(p_output_topology,
                                                       'OUT',  --accidentally named my only work table topo_layers_out_info instead of OUTPUT *
                                                       'N');   --no drop tracking, how else we write?
                                                               --* now also topo_layers_out_sup layers_out_bas + topo_layers_out_help


         EXCEPTION
         WHEN OTHERS
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'TIDY_EXIT',NULL,
                                                   'Error dropping work tables: ' || SQLERRM);
         END;


         BEGIN

            --only time we should have sequences is Z9, Sideys one sequence in gz_projection
            --but call this anyway

            GZ_BUSINESS_UTILS.DROP_MODULE_SEQUENCES(p_output_topology,
                                                    'OUT');



         EXCEPTION
         WHEN OTHERS
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'TIDY_EXIT',NULL,
                                                        'Error dropping sequences: ' || SQLERRM);
         END;

      ELSE

        GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'TIDY_EXIT',NULL,
                                                    'NOT Dropping work tables ');


      END IF;

      --no allowance in here for failure right now
      --would fail out earlier, esp in module 10

      --what else is tidy?

      IF p_fixes_retval IS NULL
      THEN

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'GENERALIZATION_OUTPUT',NULL,
                                                     'WOOT: Returning success code 0');

         RETURN '0';

      ELSE

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'GENERALIZATION_OUTPUT',NULL,
                                                     'Complete but fail in output module due to fix/sliver failure ' || p_fixes_retval);

         RETURN '1';

      END IF;

   END TIDY_EXIT;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   --Public---------------------------------------------------------------------------------

   FUNCTION GENERALIZATION_OUTPUT (
      p_release               IN VARCHAR2,
      p_gen_project_id        IN VARCHAR2,
      p_source_schema         IN VARCHAR2,
      p_source_topology       IN VARCHAR2,
      p_output_topology       IN VARCHAR2,
      p_modules               IN VARCHAR2 DEFAULT 'YYYYYYYYYY',
      p_restart_flag          IN VARCHAR2 DEFAULT 'N',
      p_single_layer          IN VARCHAR2 DEFAULT NULL,
      p_tile_kount            IN NUMBER DEFAULT 10,
      p_prcs_slivers          IN VARCHAR2 DEFAULT 'N',
      p_sliver_restart_flag   IN VARCHAR2 DEFAULT 'N',
      p_sliver_width          IN NUMBER DEFAULT NULL,
      p_segment_length        IN NUMBER DEFAULT NULL,
      p_expendable_review     IN VARCHAR2 DEFAULT 'N',
      p_reshape_review        IN VARCHAR2 DEFAULT 'Y',
      p_srid                  IN NUMBER DEFAULT 8265,
      p_tolerance             IN NUMBER DEFAULT .05,
      p_drop_work_tables      IN VARCHAR2 DEFAULT 'Y',
      p_validate_topo         IN VARCHAR2 DEFAULT 'Y',
      p_fix_edge              IN VARCHAR2 DEFAULT 'Y',
      p_fix_2edge             IN VARCHAR2 DEFAULT 'N',
      p_topofix_qa            IN VARCHAR2 DEFAULT 'Y'
   ) RETURN VARCHAR2
   AS

      --Matt! Started 5/3/12

      --Sample call:
      --declare
      --retval varchar2(4000);
      --begin
      --retval := GZ_OUTPUT.GENERALIZATION_OUTPUT('A12','Z6','TAB10ST09','MT','Z609IN');
      --end;

      psql                 VARCHAR2(4000);
      retval               VARCHAR2(4000) := '1';  --set to fail, must set to pass in each module
      fixes_retval         VARCHAR2(4000);
      stack                VARCHAR2(4000);
      errm                 VARCHAR2(8000) := 'ERROR:'; --default for line 1 in log error message, if no SQLERRM
      layer_try            PLS_INTEGER := 0;
      allowed_tries        PLS_INTEGER := 5;
      layers_processing    GZ_TYPES.stringarray;
      skip_layer           PLS_INTEGER := 0;
      face_table           VARCHAR2(32);
      drop_work_tables     VARCHAR2(1);
      tile_kount           NUMBER;
      layer_start          TIMESTAMP;
      layer_end            TIMESTAMP;
      topofix_qa           VARCHAR2(1);


   BEGIN

      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 10');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_OUTPUT: Let the logging begin');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      IF p_drop_work_tables IS NULL
      THEN

         drop_work_tables := 'Y';

      ELSE

         drop_work_tables := p_drop_work_tables;

      END IF;

      IF p_tile_kount IS NULL
      THEN

         tile_kount := 10;

      ELSE

         tile_kount := p_tile_kount;

      END IF;

      IF p_topofix_qa IS NULL
      OR p_topofix_qa = 'N'
      THEN

         topofix_qa := 'N';

      ELSIF p_topofix_qa = 'Y'
      THEN

         topofix_qa := 'Y';

      ELSE

         RAISE_APPLICATION_ERROR(-20001,'Unknown topofix_qa value of ' || p_topofix_qa);

      END IF;

      IF p_restart_flag = 'N'
      THEN

         BEGIN

            --We are starting from the beginning for this job
            GZ_OUTPUT.START_OUTPUT_LOGGING(p_release,
                                           p_gen_project_id,
                                           p_output_topology);

         EXCEPTION
         WHEN OTHERS
         THEN
            --send special exception to generic handler at the end
            RAISE_APPLICATION_ERROR(-20002,'Failed before we could even create a log.  Check the basics. Backtrace- '
                                          || SQLERRM || chr(10) || DBMS_UTILITY.format_error_backtrace);
         END;

         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SET_UP',NULL,
                                                'Inputs are (' || p_release || ',' || p_gen_project_id || ','
                                                || p_source_schema || ',' || p_source_topology || ','
                                                || p_output_topology || ',' || p_modules || ',' || p_restart_flag || ','
                                                || p_single_layer || ',' || ',' || p_tile_kount || ','
                                                || p_prcs_slivers || ',' || p_sliver_restart_flag || ',' || p_sliver_width || ','
                                                || p_segment_length || ',' || p_expendable_review || ',' || p_reshape_review || ','
                                                ||  p_srid || ',' || p_tolerance || ','
                                                || drop_work_tables || ',' || p_validate_topo || ','
                                                || p_fix_edge || ',' || p_fix_2edge || ','
                                                || p_topofix_qa || ')');

      END IF;


      --probably more here...


      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 20');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_OUTPUT: Verify Inputs');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      IF substr(p_modules,1,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_OUTPUT.VERIFY_INPUTS(p_release,
                                              p_gen_project_id,
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

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VERIFY_INPUTS',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology || '. '
                                                   || 'VERIFY_INPUTS returned ' || substr(retval,1,3500),
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VERIFY_INPUTS',NULL,
                                                   'Complete for ' || p_output_topology);

         END IF;

      END IF;

      --get this one time only.  Checked that this works in verification
      face_table := GZ_OUTPUT.GET_FACE_TABLE(p_release,
                                             p_gen_project_id,
                                             p_output_topology);


      ---------------------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      DBMS_APPLICATION_INFO.SET_ACTION('Step 30');
      DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_OUTPUT: Set Up and Clean Down');
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
      ---------------------------------------------------------------------------------------

      IF substr(p_modules,2,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_OUTPUT.SET_UP(p_release,
                                       p_gen_project_id,
                                       p_source_schema,
                                       p_source_topology,
                                       p_output_topology,
                                       face_table,
                                       p_srid,
                                       p_restart_flag,
                                       p_single_layer);

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

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SET_UP',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology || '. '
                                                   || 'SET_UP returned ' || retval,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || stack , 1, 4000) );

            RETURN '1';


         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SET_UP',NULL,
                                                   'Complete for ' || p_output_topology);

         END IF;

      END IF;


      --------------------------
      --PROCESS BY LAYER, MODULE
      --MODULES 3,4,5,6
      --------------------------

      IF substr(p_modules,3,1) = 'Y'
      OR substr(p_modules,4,1) = 'Y'
      OR substr(p_modules,5,1) = 'Y'
      OR substr(p_modules,6,1) = 'Y'
      THEN


         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'GENERALIZATION_OUTPUT',NULL,
                                                'Diverting into layer-based processing ');


         --want a consistent order

         psql := 'SELECT a.layer '
              || 'FROM ' || p_output_topology || '_layers_out_info a '
              || 'WHERE a.module_status = :p1 '
              || 'ORDER BY a.layer_sequence, a.layer ';

         EXECUTE IMMEDIATE psql BULK COLLECT INTO layers_processing USING 2;

      END IF;


      LOOP

         EXIT WHEN layers_processing.COUNT = 0;

         IF layer_try > allowed_tries
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'GENERALIZATION_OUTPUT',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology || '. '
                                                   || 'Out of tries on try ' || layer_try,
                                                   NULL,NULL,NULL,NULL,NULL,NULL);

            RETURN '1';

         ELSE

            layer_try := layer_try + 1;

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'GENERALIZATION_OUTPUT',NULL,
                                                   'Processing ' || layers_processing.COUNT || ' layers on try loop ' || layer_try );


         END IF;


         --Now loop over the layers on this try

         FOR i IN 1 .. layers_processing.COUNT
         LOOP

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'GENERALIZATION_OUTPUT',NULL,
                                                   'Processing layer ' || layers_processing(i) || ' through modules 3,4,5' );

            ---------------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
            DBMS_APPLICATION_INFO.SET_ACTION('Step 40');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_OUTPUT: Create and register layer');
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
            ---------------------------------------------------------------------------------------

            IF substr(p_modules,3,1) = 'Y'
            THEN

               BEGIN

                  --assume fail
                  retval := '1';

                  --set this guy up too, assume success
                  skip_layer := 0;

                  retval := GZ_OUTPUT.CREATE_LAYER(p_release,
                                                   p_gen_project_id,
                                                   p_output_topology,
                                                   layers_processing(i),
                                                   face_table);


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


               IF retval = '-1'
               THEN

                  --special returncode means that its too soon for this layer
                  --its child layer is not built yet
                  skip_layer := 1;

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                                         'Layer ' || layers_processing(i) || ' must be skipped until children are born ');

               ELSIF retval != '0'
               THEN

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                                         'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology || '. '
                                                         || 'CREATE_LAYER returned ' || retval,
                                                         NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || stack , 1, 4000) );

                  RETURN '1';


               ELSE

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'CREATE_LAYER',NULL,
                                                         'Layer ' || layers_processing(i) || ' complete for ' || p_output_topology);

                  GZ_OUTPUT.SET_LAYER_INFO_MODULE(p_output_topology, 3, layers_processing(i));

               END IF;

            END IF;


            ---------------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
            DBMS_APPLICATION_INFO.SET_ACTION('Step 50');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_OUTPUT: Build Layer Topogeom');
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
            ---------------------------------------------------------------------------------------

            IF substr(p_modules,4,1) = 'Y'
            AND skip_layer = 0
            THEN

               --time starts here, including the skip_layer above is too messy for me
               layer_start := SYSTIMESTAMP;

               BEGIN

                  --assume fail
                  retval := '1';

                  retval := GZ_OUTPUT.BUILD_LAYER_TOPOGEOM(p_release,
                                                           p_gen_project_id,
                                                           p_source_schema,
                                                           p_source_topology,
                                                           p_output_topology,
                                                           layers_processing(i),
                                                           face_table);


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

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'BUILD_LAYER_TOPOGEOM',NULL,
                                                         'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology || '. '
                                                         || 'BUILD_LAYER_TOPOGEOM returned ' || retval,
                                                         NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || stack , 1, 4000) );

                  RETURN '1';


               ELSE

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'BUILD_LAYER_TOPOGEOM',NULL,
                                                         'Layer ' || layers_processing(i) || ' complete for ' || p_output_topology);

                  GZ_OUTPUT.SET_LAYER_INFO_MODULE(p_output_topology, 4, layers_processing(i));

               END IF;

            END IF;


            ---------------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
            DBMS_APPLICATION_INFO.SET_ACTION('Step 60');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_OUTPUT: Populate Attributes');
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
            ---------------------------------------------------------------------------------------

            IF substr(p_modules,5,1) = 'Y'
            AND skip_layer = 0
            THEN

               BEGIN

                  --assume fail
                  retval := '1';

                  retval := GZ_OUTPUT.POPULATE_LAYER_ATTRIBUTES(p_release,
                                                                p_gen_project_id,
                                                                p_source_schema,
                                                                p_source_topology,
                                                                p_output_topology,
                                                                layers_processing(i),
                                                                face_table);


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

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_LAYER_ATTRIBUTES',NULL,
                                                         'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology || '. '
                                                         || 'POPULATE_LAYER_ATTRIBUTES returned ' || retval,
                                                         NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || stack , 1, 4000) );

                  RETURN '1';


               ELSE

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_LAYER_ATTRIBUTES',NULL,
                                                         'Layer ' || layers_processing(i) || ' complete for ' || p_output_topology);

                  GZ_OUTPUT.SET_LAYER_INFO_MODULE(p_output_topology, 5, layers_processing(i));

               END IF;

            END IF;

            IF substr(p_modules,6,1) = 'Y'
            AND skip_layer = 0
            THEN

               BEGIN

                  --assume fail
                  retval := '1';

                  retval := GZ_OUTPUT.SPECIAL_HANDLING(p_release,
                                                       p_gen_project_id,
                                                       p_source_schema,
                                                       p_source_topology,
                                                       p_output_topology,
                                                       layers_processing(i),
                                                       face_table);


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

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SPECIAL_HANDLING',NULL,
                                                         'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology || '. '
                                                         || 'SPECIAL_HANDLING returned ' || retval,
                                                         NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || stack , 1, 4000) );

                  RETURN '1';

               ELSE

                  GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'SPECIAL_HANDLING',NULL,
                                                          'Layer ' || layers_processing(i) || ' complete for ' || p_output_topology);

                  GZ_OUTPUT.SET_LAYER_INFO_MODULE(p_output_topology, 6, layers_processing(i));

                  layer_end := SYSTIMESTAMP;

               END IF;

            END IF;


            --Stuff to do that isnt part of a module but should happen betwen layers

            --1. Gather stats on all feature and $ tables. We may have just added 100 million records to rel$
            --   But on every layer seems like overkill.  Lets do every other layer

            IF MOD(i,2) = 0
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'GATHER TOPO STATS',NULL,
                                                      'Layer ' || layers_processing(i) || ' (every other layer)');

               GZ_TOPO_UTIL.GATHER_TOPO_STATS(UPPER(p_output_topology));

            END IF;


            --2. LOG layer times in (semi) easy to use records
            IF layer_start IS NOT NULL
            AND layer_end IS NOT NULL
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PERFORMANCE QUARRY QUERY',NULL,
                                                      layers_processing(i), layer_start, layer_end);

            END IF;

            layer_start := NULL;
            layer_end := NULL;


         END LOOP; --loop over layers on this try



         --get another bucket of layers
         EXECUTE IMMEDIATE psql BULK COLLECT INTO layers_processing USING 2;

      END LOOP; --Loop over layer tries




      IF substr(p_modules,7,1) = 'Y'
      THEN

         BEGIN

            --assume worst case fail
            retval := '2';

             --Return values
             --0   - success
             --1|% - success with warnings, continue running to the end of output build
             --      this includes any fix_edge, fix_face, or coastal_sliver processing
             --      where something didnt get fixed as requested
             --2|% - fail, stop processing

            retval := GZ_OUTPUT.POPULATE_MEASUREMENTS(p_release,
                                                      p_gen_project_id,
                                                      p_output_topology,
                                                      p_single_layer,
                                                      face_table,
                                                      p_prcs_slivers,
                                                      p_sliver_restart_flag,
                                                      p_sliver_width,
                                                      p_segment_length,
                                                      p_expendable_review,
                                                      p_reshape_review,
                                                      p_tolerance,
                                                      p_srid,
                                                      p_restart_flag,
                                                      p_fix_edge,
                                                      p_fix_2edge,
                                                      topofix_qa);


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


         IF retval LIKE '2%'
         THEN

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology || '. '
                                                   || 'POPULATE_MEASUREMENTS returned ' || retval,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || stack , 1, 4000) );

            RETURN '1';

         ELSE

            IF retval LIKE '0%'
            THEN

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                           'Complete for ' || p_output_topology);

            ELSIF retval LIKE '1%'
            THEN

               fixes_retval := retval;

               GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'POPULATE_MEASUREMENTS',NULL,
                                                           'Complete with non-fatal error ' || retval || ' for ' || p_output_topology);

            END IF;

            IF p_sliver_restart_flag <> 'Y' OR p_sliver_restart_flag IS NULL
            THEN

               GZ_OUTPUT.SET_LAYER_INFO_MODULE(p_output_topology, 7, p_single_layer);

            END IF;

         END IF;

      END IF;


      IF substr(p_modules,8,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_OUTPUT.PREPARE_FOR_SHAPES(p_release,
                                                   p_gen_project_id,
                                                   p_output_topology,
                                                   p_single_layer,
                                                   face_table,
                                                   drop_work_tables);


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

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PREPARE_FOR_SHAPES',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology || '. '
                                                   || 'PREPARE_FOR_SHAPES returned ' || retval,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || stack , 1, 4000) );

            RETURN '1';

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'PREPARE_FOR_SHAPES',NULL,
                                                    'Complete for ' || p_output_topology);

            GZ_OUTPUT.SET_LAYER_INFO_MODULE(p_output_topology, 8, p_single_layer);

         END IF;

      END IF;

      IF substr(p_modules,9,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_OUTPUT.VALIDATE_OUTPUT(p_release,
                                                p_gen_project_id,
                                                p_output_topology,
                                                p_single_layer,
                                                face_table,
                                                tile_kount,
                                                p_tolerance,
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

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                   'UNRECOVERABLE ERROR: Ending processing for ' || p_output_topology || '. '
                                                   || 'VALIDATE_OUTPUT returned ' || retval,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || stack , 1, 4000) );

            --IF topo is invalid or we have invalid geoms
            --Just skip out now without running module 10, which drops work tables mainly
            RETURN '1';

         ELSE

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'VALIDATE_OUTPUT',NULL,
                                                    'Complete for ' || p_output_topology);


            GZ_OUTPUT.SET_LAYER_INFO_MODULE(p_output_topology, 9, p_single_layer);

         END IF;

      END IF;

      IF substr(p_modules,10,1) = 'Y'
      THEN

         BEGIN

            --assume fail
            retval := '1';

            retval := GZ_OUTPUT.TIDY_EXIT(p_release,
                                          p_gen_project_id,
                                          p_output_topology,
                                          drop_work_tables,
                                          p_single_layer,
                                          face_table,
                                          fixes_retval);


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

            GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'TIDY_EXIT',NULL,
                                                   'Complete but failing output processing for ' || p_output_topology || '. '
                                                   || 'TIDY_EXIT returned ' || retval,
                                                   NULL,NULL,NULL,NULL,NULL,substr(errm || chr(10) || stack , 1, 4000) );

            RETURN '1';

         ELSE

            --final logging is insidy tidy

            --Dont do this since we just dropped it
            --GZ_OUTPUT.SET_LAYER_INFO_MODULE(p_output_topology, 10, p_single_layer);

            ---------------------------------------------------------------------------------------
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
            DBMS_APPLICATION_INFO.SET_ACTION('');
            DBMS_APPLICATION_INFO.SET_CLIENT_INFO('GENERALIZATION_OUTPUT: Peace out');
            --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-------
            ---------------------------------------------------------------------------------------

            --EXIT
            RETURN retval;

         END IF;

      END IF;


      --Below is the generic unhandled exception area
      --usually a totally flubbed up input near the very beginning of the job


   EXCEPTION
   WHEN OTHERS
   THEN

      IF SQLCODE = -20002  --my made up code
      THEN

         errm := SQLERRM || DBMS_UTILITY.format_error_backtrace;
         RAISE_APPLICATION_ERROR(-20001, 'Doh! Unhandled exception.  We dont even have a log to write to. '
                                          || chr(10) || ' Heres the backtrace: '
                                          || chr(10) || errm);

      ELSE

         -- Word up to "good practice"

         errm := SQLERRM || DBMS_UTILITY.format_error_backtrace;
         GZ_BUSINESS_UTILS.GEN_EXTENDED_TRACKING_LOG('OUTPUT',p_output_topology,'EXCEPTION HANDLER',NULL,
                                                'UNRECOVERABLE ERROR: OUTPUT caught this exception and has no clue ',
                                                 NULL,NULL,NULL,NULL,NULL,substr(errm, 1, 4000) );

         RETURN '1';


      END IF;


   END GENERALIZATION_OUTPUT;


END GZ_OUTPUT;

/