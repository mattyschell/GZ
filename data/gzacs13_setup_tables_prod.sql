-- Production setup SQL for ACS13 generalized file processing.

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

--------------------------------------------------------------------------------
-- Set stop points for known manual feature work.
--------------------------------------------------------------------------------

UPDATE gz_job_setup
   SET qa_status = 'F', comments = 'QA pre-set to F to allow for manual adjustment. '
 WHERE jobid IN
          ('ACS13_Z6_ST09_2',
           'ACS13_Z6_ST17_2',
           'ACS13_Z6_ST26_2',
           'ACS13_Z6_ST55_2');

COMMIT;

QUIT


