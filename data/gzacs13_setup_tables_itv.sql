-- ITV setup SQL for ACS13 generalized file processing.

--------------------------------------------------------------------------------
-- Alters the GZ_ENTITY_TABLE to limit the universe to the 9
-- test deck states.
-- 02, 04, 09, 10, 11, 15, 25, 44, 72
--------------------------------------------------------------------------------

DELETE FROM gz_entity_table
      WHERE release = 'ACS13'
            AND entity_code NOT IN
                   ('02',
                    '04',
                    '09',
                    '10',
                    '11',
                    '15',
                    '25',
                    '44',
                    '72',
                    '99');

COMMIT;

--------------------------------------------------------------------------------
-- Update tile counts to account for fewer states in run.
--------------------------------------------------------------------------------

UPDATE gz_build_source_parameters
   SET tile_count = 500
 WHERE gen_project_id NOT IN ('Z6', 'V6');

COMMIT;

UPDATE gz_build_source_parameters
   SET tile_count = 1000
 WHERE gen_project_id IN ('Z6', 'V6');

COMMIT;

--------------------------------------------------------------------------------
-- Alter the build parameters to build only the states in the test deck.
-- Set up the SDO filter on the gz_build_source_parameters table for all
-- resoltuions.
--------------------------------------------------------------------------------

DECLARE
   psql     VARCHAR2 (4000);
   shapes   gz_types.geomarray;
   output   SDO_GEOMETRY;
BEGIN
   psql :=
      'select sdogeometry from acs13.state where vintage = ''90'' and statefp IN (''09'',''25'',''44'')';

   EXECUTE IMMEDIATE psql BULK COLLECT INTO shapes;

   FOR i IN 1 .. shapes.COUNT
   LOOP
      IF i = 1
      THEN
         output := shapes (i);
      ELSE
         output := SDO_GEOM.sdo_union (output, shapes (i), .05);
      END IF;
   END LOOP;

   psql :=
      'select sdogeometry from acs13.state where vintage = ''90'' and statefp IN (''02'',''04'',''10'',''11'',''15'',''72'')';

   EXECUTE IMMEDIATE psql BULK COLLECT INTO shapes;

   FOR i IN 1 .. shapes.COUNT
   LOOP
      output := SDO_UTIL.append (output, shapes (i));
   END LOOP;

   EXECUTE IMMEDIATE 'update gz_build_source_parameters set sdo_filter = :p1'
      USING output;

   COMMIT;
END;
/

--------------------------------------------------------------------------------
-- Populate the setup tables
--------------------------------------------------------------------------------

BEGIN
   gz_workflow.insert_new_jobs ('ACS13', 'Z6');
   gz_workflow.insert_new_jobs ('ACS13', 'V6');
   gz_workflow.insert_new_jobs ('ACS13', 'Z8');
   gz_workflow.insert_new_jobs ('ACS13', 'V8');
   gz_workflow.insert_new_jobs ('ACS13', 'Z9');
END;
/

--------------------------------------------------------------------------------
-- Turn off shapefile creation for step 1.
--------------------------------------------------------------------------------

UPDATE gz_job_setup
   SET shapefile = 'N'
 WHERE step = 1;

COMMIT;

QUIT
