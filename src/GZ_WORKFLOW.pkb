CREATE OR REPLACE PACKAGE BODY GZ_WORKFLOW
AS

   PROCEDURE Update_WorkFlow_Status (pJobID    IN VARCHAR2,
                                     pModule   IN VARCHAR2,
                                     pAction   IN VARCHAR2,
                                     pStatus   IN VARCHAR2)
   AS
      PRAGMA AUTONOMOUS_TRANSACTION;

      APP_INFO_MODULE           VARCHAR2 (48);
      APP_INFO_ACTION           VARCHAR2 (32);
      SQL1                      VARCHAR2 (1000);
      JOB_SETUP_STATUS_COL_NM   VARCHAR2 (30);
      MOD_SETUP_TBL_NM          VARCHAR2 (30);

      START_TIME                TIMESTAMP;
      END_TIME                  TIMESTAMP;

      vNote                     VARCHAR2 (400);
   BEGIN
      APP_INFO_MODULE := 'Begin Update WorkFlow Status';
      APP_INFO_ACTION := pJobID || ': ' || pModule || '-' || pAction;
      DBMS_APPLICATION_INFO.SET_MODULE (APP_INFO_MODULE, APP_INFO_ACTION);

      CASE pModule
         WHEN 'PP' THEN
            JOB_SETUP_STATUS_COL_NM := 'PRE_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_PRE_SETUP';
         WHEN 'TB' THEN
            JOB_SETUP_STATUS_COL_NM := 'TOPOBUILD_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_TOPOBUILD_SETUP';
         WHEN 'BS' THEN
            JOB_SETUP_STATUS_COL_NM := 'TOPOBUILD_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_BUILD_SOURCE_SETUP';
         WHEN 'CL' THEN
            JOB_SETUP_STATUS_COL_NM := 'CLIP_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_CLIP_SETUP';
         WHEN 'LS' THEN
            JOB_SETUP_STATUS_COL_NM := 'LINESIM_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_LINESIM_SETUP';
         WHEN 'SP' THEN
            JOB_SETUP_STATUS_COL_NM := 'SMPOLY_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_SMPOLY_SETUP';
         WHEN 'MRG' THEN
            JOB_SETUP_STATUS_COL_NM := 'MERGE_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_MERGE_SETUP';
         WHEN 'FSL' THEN
            JOB_SETUP_STATUS_COL_NM := 'FSLBUILD_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_FSLBUILD_SETUP';
         WHEN 'OUT' THEN
            JOB_SETUP_STATUS_COL_NM := 'FSLBUILD_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_OUTPUT_SETUP';
         WHEN 'CA' THEN
            JOB_SETUP_STATUS_COL_NM := 'CLIP_ATTR_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_CLIP_SETUP';
         WHEN 'QA' THEN
            JOB_SETUP_STATUS_COL_NM := 'QA_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_QA_SETUP';
         WHEN 'SHP' THEN
            JOB_SETUP_STATUS_COL_NM := 'SHAPEFILE_STATUS';
            MOD_SETUP_TBL_NM := 'GZ_SHAPEFILE_SETUP';
         ELSE
            vNote := 'JobID: ' || pJobID || ' Invalid Module name: ' || pModule;

            RAISE_APPLICATION_ERROR (-20001, vNote);
      END CASE;

      SQL1 := 'Update GZ_JOB_SETUP SET '
         || JOB_SETUP_STATUS_COL_NM
         || ' = :1, CURR_MOD = :2 WHERE JOBID = :3';

      EXECUTE IMMEDIATE SQL1 USING pStatus, pModule, pJobID;

      IF pStatus = 'F' THEN
         SQL1 := 'Update GZ_JOB_SETUP SET RUN_STATUS = :1, END_TIME = :2 WHERE JOBID = :3';
         EXECUTE IMMEDIATE SQL1 USING pStatus, SYSTIMESTAMP, pJobID;
         COMMIT;
      END IF;

      IF (pACTION = 'START') AND (pModule <> 'PP') THEN
         SQL1 :=  'UPDATE '
            || MOD_SETUP_TBL_NM
            || ' SET STATUS = :1, START_TIME = :2, END_TIME = NULL '
            || '  WHERE JOBID = :3';
         EXECUTE IMMEDIATE SQL1 USING pStatus, SYSTIMESTAMP, pJobID;
         COMMIT;
      ELSIF (pModule <> 'PP') THEN
         SQL1 := 'UPDATE '
            || MOD_SETUP_TBL_NM
            || ' SET STATUS = :1, END_TIME = :2  WHERE JOBID = :3';
         EXECUTE IMMEDIATE SQL1 USING pStatus, SYSTIMESTAMP, pJobID;
         COMMIT;
      END IF;

      COMMIT;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
         RAISE_APPLICATION_ERROR (-20001, SQLERRM);
   END Update_WorkFlow_Status;

   PROCEDURE Generalize(pJobID IN VARCHAR2)
   AS

      --Matt! 12/17/12 Made pre setup executions CLOB-friendly
      --               and cleaned up some of the logic
      --!  11/22/13 Tidied some pre processing workflow and error handling
      --M! 12/30/13 Rearranged the topo validation and topofix deck chairs again

      JOB_REC       GZ_TYPES.GZ_JOB_SETUP_REC;
      BUILDSOURCE_REC GZ_TYPES.GZ_BUILD_SOURCE_SETUP_REC;
      TOPOBUILD_REC GZ_TYPES.GZ_TOPOBUILD_SETUP_REC;
      CLIP_REC      GZ_TYPES.GZ_CLIP_SETUP_REC;
      SMPOLY_REC    GZ_TYPES.GZ_SMPOLY_SETUP_REC;
      LINESIM_REC   GZ_TYPES.GZ_LINESIM_SETUP_REC;
      MERGE_REC     GZ_TYPES.GZ_MERGE_SETUP_REC;
      FSLBUILD_REC  GZ_TYPES.GZ_FSLBUILD_SETUP_REC;
      QA_REC        GZ_TYPES.GZ_QA_SETUP_REC;
      OUTPUT_REC        GZ_TYPES.GZ_OUTPUT_SETUP_REC;

      SQL1 VARCHAR2(2000);
      SQL2 VARCHAR2(2000);

      vGenSchema VARCHAR2(30);
      vRelease VARCHAR2(10);
      vTopoTag VARCHAR2(10);
      vProjectID VARCHAR2(4);
      vGenClipTab VARCHAR2(32);
      vRunStatus VARCHAR2(20);
      vState_TBL  VARCHAR2(32);

      vCurrModule VARCHAR2(5) := NULL;
      vPrevModule VARCHAR2(5) := NULL;
      vStep INTEGER;
      vDependency INTEGER;

      -- PreProcess
      vStatementl  CLOB;

      -- BuildSource
      vSourceSchema  VARCHAR2(30);
      vSourceTOPO     VARCHAR2(30);
      vOutTOPO          VARCHAR2(30);
      vTileCount          NUMBER;
      vSDO_Filter        SDO_GEOMETRY;
      vModules           VARCHAR2(20);
      vRSFlag             VARCHAR2(1);
      vSRID                NUMBER;
      vTolerance        NUMBER;
      vSDigits             NUMBER;
      vDropTab           VARCHAR2(1);

      -- TopoBuild
      vUniverseTbl  VARCHAR2(30);
      vHierarchyTbl VARCHAR2(30);
      vStateFP      VARCHAR2(2);
      vSkipEdgesTable VARCHAR2(32);
      vTCleanup     VARCHAR2(1);
      vFeatMBRonly  VARCHAR2(10);
      vTopoValidation VARCHAR2(1);
      vBuildOption INTEGER;
      vDeploy       VARCHAR2(32);

      -- Clip
      vClipJobID   VARCHAR2(20);
      vClipMask    VARCHAR2(32);
      vClipModules VARCHAR2(20);
      vTopoIn      VARCHAR2(32);
      vTopoOut     VARCHAR2(32);
      vClipDropTables VARCHAR2(1) := 'Y';
      vClipTransferAtts     VARCHAR2(1) := 'Y';

      -- SmPoly
      vSPModules       VARCHAR2(32);
      vStateOutlineTbl VARCHAR2(30);
      vGeoIDColumn     VARCHAR2(32);
      vRunId           VARCHAR2(5);
      vSCleanUp         VARCHAR2(1);
      vTopoBk VARCHAR2(5);

      -- LineSim
      vLSModules VARCHAR2(32);

      -- FSL Build
      vFSLFaceTblExt VARCHAR2(30);
      --vHasMirrors    VARCHAR2(1);
      vProjectZ9     VARCHAR2(1);
      vStateEdgeTbl  VARCHAR2(30);
      vDropTbls      VARCHAR2(1);
      vTopoType VARCHAR2(2);

      -- Merge
      vMRG_TopoInTbl   VARCHAR2(30);
      vMRG_TopLayerTbl VARCHAR2(30);
      vMRG_FaceTblExt  VARCHAR2(20);
      vMRG_Modules     VARCHAR2(10);
      vRestartFlg      VARCHAR2(1);
      returnVal        NUMBER;

      NumOfTopo2Mrg INTEGER;
      NumOfTopoDone INTEGER;

      FaceOutputTbl VARCHAR2(20);
      --Topo_Tbl VARCHAR2(30);

      -- QA
      vGenTopology     VARCHAR2(30);
      vGenSdoGeomCol   VARCHAR2(30);
      vUnGenSchema     VARCHAR2(30);
      vUnGenTopology   VARCHAR2(30);
      vUnGenSdoGeomCol VARCHAR2(30);
      vOutPutTableSuffix VARCHAR2(20);
      vtarget_scale    NUMBER;

      vCompareGenColumn VARCHAR2(20);
      vAreaCheck NUMBER;
      vShapeCheck NUMBER;

      vEntireTopology  VARCHAR2(1);
      vGenTableName    VARCHAR2(30);
      vUnGenTableName  VARCHAR2(30);

      vMissingRecTbl VARCHAR2(30);
      vMissingRecKey VARCHAR2(30);
      vMissingRecFeatTbl VARCHAR2(30);


      APP_INFO_MODULE VARCHAR2(48);
      APP_INFO_ACTION VARCHAR2(32);
      APP_INFO_CLIENT_INFO VARCHAR2(64);

      vNote VARCHAR2(4000);
      vSQLCode NUMBER;
      vSQLErrMsg VARCHAR2(1000);

      vPPStatus VARCHAR2(1);
      vTBStatus VARCHAR2(1);
      vCLStatus VARCHAR2(1);
      vSPStatus VARCHAR2(1);
      vLSStatus VARCHAR2(1);
      vMRGStatus VARCHAR2(1);
      vFSLBuildStatus VARCHAR2(1);
      vQAStatus VARCHAR2(1);
      vOUTStatus VARCHAR2(1);
      --vSHPStatus VARCHAR2(1);

      vSPTrackingTable VARCHAR2(30);
      vSPDEITable VARCHAR2(30);
      vLSTrackingTable VARCHAR2(30);
      vQATrackingTable VARCHAR2(30);

      rcdCount INTEGER;
      IsBuildSource    NUMBER :=1 ;

      TYPE RefCursorType IS REF CURSOR;

      Cur1         RefCursorType;
      retval   VARCHAR2 (4000);

   BEGIN

      ------------------------------------------------------------------------------
      -- Tag the batch job
      APP_INFO_MODULE := 'GZ_WORKFLOW.GENERALIZE-'||pJobID;
      APP_INFO_ACTION  := 'Initializing';
      DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
      ------------------------------------------------------------------------------

      -- Select Job Record from GZ_JOB_SETUP
      SQL1 := 'SELECT * FROM GZ_JOB_SETUP WHERE JOBID = :1';
      EXECUTE IMMEDIATE SQL1 into JOB_REC USING pJobID; -- 'ACS11_Z6_ST99_1'

      vGenSchema := USER;
      vRelease := JOB_REC.RELEASE;
      vTopoTag := JOB_REC.TOPO_TAG;
      vProjectID := JOB_REC.GEN_PROJECT_ID;
      vRunStatus := JOB_REC.RUN_STATUS;
      vGenClipTab := JOB_REC.GEN_CLIP_TABLE;
      vState_TBL := JOB_REC.STATE_TABLE;
      vCurrModule := JOB_REC.CURR_MOD;
      vStep := JOB_REC.STEP;
      vDependency := JOB_REC.DEPENDENCY;

      DBMS_OUTPUT.PUT_LINE('JOBID = ' || JOB_REC.JOBID);
      DBMS_OUTPUT.PUT_LINE('vGenSchema = ' || vGenSchema);
      DBMS_OUTPUT.PUT_LINE('vProjectID = ' || vProjectID);

      DBMS_OUTPUT.PUT_LINE('The following modules will be processed');


      IF (JOB_REC.PRE = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('PRE-PROCESS');
      END IF;
      IF (JOB_REC.TOPOBUILD = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('TOPO BUILD');
      END IF;
      IF (JOB_REC.CLIP = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('CLIP');
      END IF;
      IF (JOB_REC.SMPOLY = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('SMPOLY');
      END IF;
      IF (JOB_REC.LINESIM = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('LINESIM');
      END IF;
      IF (JOB_REC.MERGE = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('MERGE');
      END IF;
      IF (JOB_REC.FSLBUILD = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('FSLBUILD');
      END IF;
      IF (JOB_REC.QA = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('QA');
      END IF;
      IF (JOB_REC.SHAPEFILE = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('SHAPEFILE');
      END IF;

      SQL1 := 'SELECT COUNT(1) FROM  GZ_BUILD_SOURCE_PARAMETERS
                     WHERE  release = :1 and gen_project_id = :2';
      EXECUTE IMMEDIATE SQL1 INTO IsBuildSource USING vRelease, vProjectID;
      COMMIT;

      SQL1 := 'Update GZ_JOB_SETUP SET RUN_STATUS = :1, START_TIME = :2, END_TIME = :3 WHERE JOBID = :4';
      EXECUTE IMMEDIATE SQL1 USING 'I', SYSTIMESTAMP, '', pJobID;
      COMMIT;

      -- Check if the current job can run;  Previous step must complete for the current one to run.
      IF (vDependency IS NOT NULL) THEN
         SQL1 := 'Select count(jobid) from gz_job_setup where release = :1 and gen_project_id = :2 and step = :3 and run_status not in (:4, :5)';
         Execute Immediate SQL1 into rcdCount Using vRelease, vProjectID, vDependency, 'S', 'M';

         IF rcdCount > 0 THEN
            vNote := 'All dependent jobs did not complete successfully.  Aborting';
            SQL1 := 'Update GZ_JOB_SETUP SET RUN_STATUS = :1, END_TIME = :2 WHERE JOBID = :3';
            EXECUTE IMMEDIATE SQL1 USING 'F', SYSTIMESTAMP, pJobID;
            RAISE_APPLICATION_ERROR(-20001, vNote);
         END IF;

      END IF;

      IF (JOB_REC.PRE = 'Y') THEN

         APP_INFO_ACTION  := 'Begin Pre-Process';
         DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
         vCurrModule := 'PP';

         IF (JOB_REC.PRE_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the PP process');

         ELSE
            CASE JOB_REC.PRE_STATUS
               WHEN 'F' THEN
                  vNote := 'Current PP Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GOTO EndPP;

               WHEN 'I' THEN
                  vNote := 'Current PP Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GOTO EndPP;

               ELSE
                  vNote := 'Unable to recognize current PP Status ' || JOB_REC.TOPOBUILD_STATUS;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;

         DBMS_OUTPUT.PUT_LINE('PROCESSING PRE_PROCESS');
         BEGIN
            UPDATE_WORKFLOW_STATUS (pJobID,  vCurrModule, 'START', 'I');

            APP_INFO_ACTION := 'Begin PP';                   -- Begin || procedure name
            DBMS_APPLICATION_INFO.SET_MODULE (APP_INFO_MODULE, APP_INFO_ACTION);

            SQL1 := 'Select statement, step From GZ_PRE_SETUP Where JOBID = '''
               || pJobID || ''' AND status IS NULL ORDER BY step';

            OPEN Cur1 FOR SQL1;
            LOOP
               FETCH Cur1
               INTO vStatementl, vStep;  --vstatementl variable is a CLOB
               EXIT WHEN Cur1%NOTFOUND;

               BEGIN
                  SQL2 := 'Update GZ_PRE_SETUP SET STATUS = :1, start_time = :2,
                                       end_time = NULL
                                       WHERE JOBID = :3 AND step = :4';

                  EXECUTE IMMEDIATE SQL2
                     USING 'I', SYSTIMESTAMP, pJobID, vStep;

                  /* What this for?  Disallows mixed status flags to exist, like previous PREs with Y
                     I guess the idea was to avoid a total bomb from leaving the other higher steps in a nebulous state?
                     Leaving it here in case Im in here some day trying to fix whatever this prevented from happening
                  SQL2 := 'Update GZ_PRE_SETUP SET STATUS = NULL, start_time = NULL,
                                      end_time = NULL
                                       WHERE JOBID = :1 AND step > :2';
                  EXECUTE IMMEDIATE SQL2 USING pJobID, vStep;
                  COMMIT;
                  */

                  GZ_BUSINESS_UTILS.DBMS_SQL_HELPER('BEGIN ' || vStatementl || ' END;');

                  SQL2 := 'Update GZ_PRE_SETUP SET STATUS = :1, end_time = :2
                                       WHERE JOBID = :3 AND step = :4';
                  EXECUTE IMMEDIATE SQL2
                     USING 'S', SYSTIMESTAMP, pJobID, vStep;
                  COMMIT;

                  IF vPPStatus IS NULL
                  THEN

                     --this status is the overall pre-process status for all pp steps in the job
                     --if status is F for one step dont reset to S on a later good one
                     vPPStatus := 'S';

                  END IF;

               EXCEPTION
                  WHEN OTHERS THEN
                     vSQLErrMsg := SQLERRM;
                     vPPStatus := 'F'; --any failure causes the overall ppstatus to be F
                     vNote := 'Pre-Process Failed ' || vSQLErrMsg;
                     DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
                     DBMS_OUTPUT. PUT_LINE (
                           'Pre-Process statement (' || vStatementl || ') in step ' || vStep);

                     --dont forget to F the individual PPpproblem
                     SQL2 := 'Update GZ_PRE_SETUP SET STATUS = :1, end_time = :2 '
                          || 'WHERE JOBID = :3 AND step = :4';

                     EXECUTE IMMEDIATE SQL2 USING 'F', SYSTIMESTAMP, pJobID, vStep;
                     COMMIT;

               END;
            END LOOP;
            COMMIT;
            CLOSE Cur1;

            IF vPPStatus IS NULL
            THEN

               --didn't get anything from the cursor to create a vPPStatus
               --this cant be intentional
               --means gz_job_setup.pre_status said to run
               --but nothing found in gz_pre_setup with the appropriate jobid and null status
               vPPStatus := 'F';
               vNote := 'No GZ_PRE_SETUP records found with jobid ' || pJobID || ' and a NULL status';

            END IF;

         END;

         CASE vPPStatus
            WHEN 'S' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vPPStatus);
               APP_INFO_ACTION  := 'End PP';
               DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            WHEN 'F' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vPPStatus);

               RAISE_APPLICATION_ERROR(-20001, vNote);

            ELSE
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');
               RAISE_APPLICATION_ERROR(-20001, 'status is ' || vPPStatus || SQLERRM);

         END CASE;

        <<EndPP>>
        NULL;
      END IF;

      IF (JOB_REC.TOPOBUILD = 'A') THEN
         APP_INFO_ACTION  := 'Begin Build Source';
         DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
         vCurrModule := 'BS';
         -- Check if Current Module can run
         -- Now check CURRENT_MODULE and RUN_STATUS
         --    CASE vRunStatus

         IF (JOB_REC.TOPOBUILD_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the BS process');
         ELSE
            CASE JOB_REC.TOPOBUILD_STATUS
               WHEN 'F' THEN
                  vNote := 'Current BS Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GoTo EndBS;

               WHEN 'I' THEN
                  vNote := 'Current BS Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GoTo EndBS;

               ELSE
                  vNote := 'Unable to recognize current BS Status ' || JOB_REC.TOPOBUILD_STATUS;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;

         DBMS_OUTPUT.PUT_LINE('PROCESSING BUILD SOURCE');
         SQL1 := 'Select * From GZ_BUILD_SOURCE_SETUP Where JOBID = :1';
         EXECUTE IMMEDIATE SQL1 into BUILDSOURCE_REC USING pJobID;
         DBMS_OUTPUT.PUT_LINE('BUILDSOURCE_REC.JOBID = ' || BUILDSOURCE_REC.JOBID);

         vSourceSchema:= BUILDSOURCE_REC.SOURCE_SCHEMA;
         vSourceTOPO := BUILDSOURCE_REC.SOURCE_TOPOLOGY;
         vOutTOPO := BUILDSOURCE_REC.OUTPUT_TOPOLOGY;
         vTileCount := BUILDSOURCE_REC.TILE_COUNT;
         vSDO_Filter := BUILDSOURCE_REC.SDO_FILTER;
         vModules := BUILDSOURCE_REC.MODULES;
         vRSFlag := BUILDSOURCE_REC.RESTART_FLAG;
         vSRID := BUILDSOURCE_REC.SRID;
         vTolerance := BUILDSOURCE_REC.TOLERANCE;
         vSDigits := BUILDSOURCE_REC.SNAPPING_DIGITS;
         vDropTab := BUILDSOURCE_REC.DROP_TABLES;

         BEGIN
            UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'START', 'I');
            APP_INFO_ACTION  := 'Begin BS'; -- Begin || procedure name
            DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

             retval := gz_build_source.generalization_topo_build (USER,
                                                                  vRelease,
                                                                  vProjectID,
                                                                  vSourceSchema,
                                                                  vSourceTOPO,
                                                                  vOutTOPO,
                                                                  vTileCount,
                                                                  vSDO_Filter,
                                                                  vModules,
                                                                  vRSFlag,
                                                                  vSRID,
                                                                  vTolerance,
                                                                  vSDigits,
                                                                  vDropTab,
                                                                  buildsource_rec.validate_topo,
                                                                  buildsource_rec.topofix_edge,
                                                                  buildsource_rec.topofix_2edge,
                                                                  buildsource_rec.topofix_qa);


         IF (retval = '0' ) THEN
              vTBStatus := 'S';
         ELSE
              vTBStatus := 'F';
         END IF;
         -- Update TopoBuild_Status

         EXCEPTION
            WHEN Others THEN
               vSQLErrMsg := SQLERRM;
               vTBStatus := 'F';
               vNote := 'Build Source Failed ' || vSQLErrMsg;
               DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
               DBMS_OUTPUT.PUT_LINE('gz_build_source.generalization_topo_build (' || USER || ',' || vProjectID || ','|| vSourceSchema ||','
                                                                                  || vSourceTOPO ||',' || vOutTOPO ||',' || vTileCount
                                                                                  ||',vSDO_Filter,' || vModules ||','|| vRSFlag ||','
                                                                                  || vSRID||',' || vTolerance||',' || vSDigits||','
                                                                                  || vDropTab||','|| buildsource_rec.validate_topo || ','
                                                                                  || buildsource_rec.topofix_edge || ',' 
                                                                                  || buildsource_rec.topofix_2edge || ','
                                                                                  || buildsource_rec.topofix_qa || ');');

         END;

         CASE vTBStatus
            WHEN 'S' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vTBStatus);
               APP_INFO_ACTION  := 'End BS';
               DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            WHEN 'F' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vTBStatus);

               RAISE_APPLICATION_ERROR(-20001, vNote);

            ELSE
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');
               RAISE_APPLICATION_ERROR(-20001, SQLERRM);

         END CASE;

         -- Update tracking info
         -- DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

        <<EndBS>>
        NULL;
      ELSIF (JOB_REC.TOPOBUILD = 'T') THEN

         APP_INFO_ACTION  := 'Begin Topo Build';
         DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
         vCurrModule := 'TB';

         If (JOB_REC.TOPOBUILD_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the TB process');
         ELSE
            CASE JOB_REC.TOPOBUILD_STATUS
               WHEN 'F' THEN
                  vNote := 'Current TB Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GoTo EndTB;

               WHEN 'I' THEN
                  vNote := 'Current TB Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GoTo EndTB;

               ELSE
                  vNote := 'Unable to recognize current TB Status ' || JOB_REC.TOPOBUILD_STATUS;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;

         DBMS_OUTPUT.PUT_LINE('PROCESSING TOPO BUILD');
         SQL1 := 'Select * From GZ_TOPOBUILD_SETUP Where JOBID = :1';
         EXECUTE IMMEDIATE SQL1 into TOPOBUILD_REC USING pJobID; -- 'ACS11_Z6_ST99_1'
         DBMS_OUTPUT.PUT_LINE('TOPOBUILD_REC.JOBID = ' || TOPOBUILD_REC.JOBID);

         vTopoOut := TOPOBUILD_REC.Topo_Out;
         vUniverseTbl := TOPOBUILD_REC.UNIVERSE_TABLE;
         vHierarchyTbl := TOPOBUILD_REC.HIERARCHY_TABLE;
         vStateFP := TOPOBUILD_REC.STATEFP;
         vTCleanup := TOPOBUILD_REC.CLEANUP;
         vFeatMBRonly := TOPOBUILD_REC.FEATURE_MBR_ONLY ;
         vTopoValidation := TOPOBUILD_REC.TOPO_VALIDATION;
         vBuildOption := TOPOBUILD_REC.BUILD_OPTION;
         vDeploy := USER;

         BEGIN
            UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'START', 'I');
            APP_INFO_ACTION  := 'Begin TB'; -- Begin || procedure name
            DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            gz_topo_build.execution_script (vTopoOut, vUniverseTbl , vHierarchyTbl  , vStateFP, vState_tbl, vFeatMBRonly,    vTopoValidation, vBuildOption, vTCleanup, vRelease, vDeploy);
            rcdCount := FIX_TOPO_FACE (vTopoOut, vRelease, vProjectID);

            IF rcdCount > 0 THEN
               vNote := 'There are '||rcdCount||' of face_id without geoid set|.  Aborting';
               SQL1 := 'Update GZ_JOB_SETUP SET RUN_STATUS = :1, END_TIME = :2 WHERE JOBID = :3';
               EXECUTE IMMEDIATE SQL1 USING 'F', SYSTIMESTAMP, pJobID;
               vTBStatus := 'F';
               RAISE_APPLICATION_ERROR(-20001, vNote);
            ELSE
               vTBStatus := 'S';
            END IF;


            vTBStatus := 'S';
            -- Update TopoBuild_Status

         EXCEPTION
            WHEN Others THEN
                vSQLErrMsg := SQLERRM;
               vTBStatus := 'F';
               vNote := 'Topo Build Failed ' || vSQLErrMsg;
               DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
               DBMS_OUTPUT.PUT_LINE('gz_topo_build.execution_script (' || vTopoOut || ',' || vUniverseTbl || ','|| vHierarchyTbl ||',' || vStateFP ||',' || vState_tbl ||',' || vFeatMBRonly
                                                                                                       ||',' || vTopoValidation ||',' || vBuildOption ||',' || vTCleanup ||',' || vRelease ||',' || vDeploy||');');

         END;

         CASE vTBStatus
            WHEN 'S' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vTBStatus);
               APP_INFO_ACTION  := 'End TB';
               DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            WHEN 'F' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vTBStatus);

               RAISE_APPLICATION_ERROR(-20001, vNote);

            ELSE
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');
               RAISE_APPLICATION_ERROR(-20001, SQLERRM);

         END CASE;

        <<EndTB>>
        NULL;
      END IF;

      IF (JOB_REC.CLIP = 'Y') THEN

         DBMS_OUTPUT.PUT_LINE('PROCESSING CLIP');

         vPrevModule := vCurrModule;
         vCurrModule := 'CL';

         If (JOB_REC.CLIP_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the CL process');
         ELSE
            CASE JOB_REC.CLIP_STATUS
               WHEN 'F' THEN
                  vNote := 'Current CL Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GoTo EndCL;

               WHEN 'I' THEN
                  vNote := 'Current CL Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GoTo EndCL;

               ELSE
                  vNote := 'Unable to recognize current CL Status ' || JOB_REC.CLIP_STATUS;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;

         SQL1 := 'SELECT * FROM GZ_CLIP_SETUP WHERE JOBID = :1';
         EXECUTE IMMEDIATE SQL1 into CLIP_REC USING pJobID; -- 'ACS11_Z6_ST99_1'
         DBMS_OUTPUT.PUT_LINE('CLIP_REC.JOBID = ' || CLIP_REC.JOBID);

         vClipMask := CLIP_REC.GEN_CLIP_MASK;
         vClipJobID := CLIP_REC.CL_JOBID;
         vTopoIn := CLIP_REC.TOPO_IN;
         vTopoOut := CLIP_REC.TOPO_OUT;
         vClipModules := CLIP_REC.GEN_CLIP_MODULES;
         vClipDropTables := CLIP_REC.DROP_TABLES;
         vClipTransferAtts := CLIP_REC.TRANSFER_ATTS;

         Begin

            UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'START', 'I');

            APP_INFO_ACTION  := 'Begin CL';
            DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            returnVal := GZ_CLIP.GENERALIZATION_CLIP(vGenSchema,
                                                     vRelease,
                                                     vProjectID,
                                                     vClipJobID,
                                                     vGenClipTab,
                                                     vClipMask,
                                                     vTopoOut,
                                                     vTopoIn,
                                                     vClipDropTables,
                                                     vClipTransferAtts,
                                                     'N',
                                                     vClipModules,
                                                     clip_rec.validate_topo,
                                                     clip_rec.topofix_edge,
                                                     clip_rec.topofix_2edge,
                                                     clip_rec.topofix_qa);

            dbms_output.put_line('returnVal = ' || returnVal);

            -- Check for success
            If (returnVal = 0) Then
                vCLStatus := 'S';
            else
                vCLStatus := 'F';
            end if;

         EXCEPTION
            WHEN Others THEN
               vSQLErrMsg := SQLERRM;
               returnVal := 1;
               vCLStatus := 'F';
               vNote := 'Clip Failed ' || vSQLErrMsg;
               DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
               DBMS_OUTPUT.PUT_LINE('GZ_CLIP.GENERALIZATION_CLIP(' || vGenSchema ||',' || vRelease ||',' || vProjectID ||',' || vClipJobID ||',' || vGenClipTab ||',' ||vClipMask ||','
                                                                   ||vTopoOut ||',' ||vTopoIn ||',' ||vClipDropTables ||',' ||vClipTransferAtts
                                                                   || ', N,' || vClipModules || ',' || clip_rec.validate_topo || ','
                                                                   || clip_rec.topofix_edge || ',' || clip_rec.topofix_2edge || ','
                                                                   || clip_rec.topofix_qa || ');');
         END;

         CASE vCLStatus
            WHEN 'S' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vCLStatus);
               APP_INFO_ACTION  := 'End CL';
               DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            WHEN 'F' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vCLStatus);

               RAISE_APPLICATION_ERROR(-20001, vNote);

            ELSE
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');
               RAISE_APPLICATION_ERROR(-20001, SQLERRM);

         END CASE;

        <<EndCL>>
        NULL;
      END IF;

      IF (JOB_REC.SMPOLY = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('PROCESSING SMPOLY');

         vPrevModule := vCurrModule;
         vCurrModule := 'SP';

         If (JOB_REC.SMPOLY_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the SP process');
         ELSE
            CASE JOB_REC.SMPOLY_STATUS
               WHEN 'F' THEN
                  vNote := 'Current SP Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GoTo EndSP;

               WHEN 'I' THEN
                  vNote := 'Current SP Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GoTo EndSP;

               ELSE
                  vNote := 'Unable to recognize current SP Status ' || JOB_REC.SMPOLY_STATUS;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;

         SQL1 := 'SELECT * FROM GZ_SMPOLY_SETUP WHERE JOBID = :1';
         EXECUTE IMMEDIATE SQL1 into SMPOLY_REC USING pJobID; -- 'ACS11_Z6_ST99_1'
         DBMS_OUTPUT.PUT_LINE('SMPOLY_REC.JOBID = ' || SMPOLY_REC.JOBID);

         -- vGenSchema, vProjectID
         vTopoOut := SMPOLY_REC.TOPO_OUT;
         vTopoIn := SMPOLY_REC.TOPO_IN;
         vTopoBk := 'BK';
         vSPModules := SMPOLY_REC.SP_MODULES;
         vStateFP := SMPOLY_REC.STATEFP;
         vGeoIDColumn := SMPOLY_REC.GEOID_COL;
         vRunId := SMPOLY_REC.RUNID;
         vSCleanUp :=  SMPOLY_REC.CLEANUP;

         BEGIN
            UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'START', 'I');

            APP_INFO_ACTION  := 'Begin SP';
            DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            gz_smpoly.generalization_sp_removal(vGenSchema,
                                                vProjectID,
                                                pJobID,
                                                vTopoOut,
                                                vSPModules,
                                                vTopoIn,
                                                vTopoOut || vTopoBk,
                                                vState_TBL,
                                                vStateFP,
                                                vGeoIDColumn,
                                                vRunId,
                                                vSCleanUp,
                                                smpoly_rec.validate_topo,
                                                smpoly_rec.topofix_edge,
                                                smpoly_rec.topofix_2edge,
                                                smpoly_rec.topofix_qa);

            vSPTrackingTable := vTopoOut || '_SP_TRACKING';

            vSPStatus := 'S';

            SQL1 := 'Select count(*) from ' || vSPTrackingTable;
            SQL1 := SQL1 || ' where error_msg = :1';
            Execute Immediate SQL1 into rcdCount using 'Complete - Successful';

            IF rcdCount <> 1 THEN

               DBMS_OUTPUT.PUT_LINE(vSPTrackingTable || ' did not complete successfully.  rcdCount: ' || rcdCount);
               vSPStatus := 'F';
            END IF;

            vSPDEITable := vTopoOut || '_' || vRunId || 'DEI';
            SQL1 := 'Select count(*) from ' || vSPDEITable;
            SQL1 := SQL1 || ' where done = :1';
            Execute Immediate SQL1 into rcdCount using 'N';

            IF rcdCount > 0 THEN

               DBMS_OUTPUT.PUT_LINE(vSPDEITable || ' Not all small polygons are removed. Remaining rcdCount: ' || rcdCount);
               vSPStatus := 'F';
            END IF;


         EXCEPTION
            WHEN Others THEN
               vSQLErrMsg := SQLERRM;
               vSPStatus := 'F';
               vNote := 'SMPoly Failed ' || vSQLErrMsg;
               DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
               DBMS_OUTPUT.PUT_LINE('gz_smpoly.generalization_sp_removal(' || vGenSchema || ',' || vProjectID || ',' || pJobID || ','
                                                                              || vTopoOut || ',' ||  vSPModules || ',' ||vTopoIn || ','
                                                                              || vTopoOut || vTopoBk || ',' || vState_TBL || ','
                                                                              || vStateFP || ',' ||vGeoIDColumn || ',' || vRunId || ','
                                                                              || vSCleanUp || ',' || smpoly_rec.validate_topo || ','
                                                                              || smpoly_rec.topofix_edge || ',' || smpoly_rec.topofix_2edge || ','
                                                                              || smpoly_rec.topofix_qa || ');');

         END;

         UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vSPStatus);

         CASE vSPStatus
            WHEN 'S' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vSPStatus);
               APP_INFO_ACTION  := 'End SP';
               DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            WHEN 'F' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vSPStatus);

               RAISE_APPLICATION_ERROR(-20001, vNote);

            ELSE
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');

               RAISE_APPLICATION_ERROR(-20001, SQLERRM);

         END CASE;

        <<EndSP>>
        NULL;
      END IF;

      IF (JOB_REC.LINESIM = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('PROCESSING LINESIM');

         vPrevModule := vCurrModule;
         vCurrModule := 'LS';

         IF (JOB_REC.LINESIM_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the LS process');
         ELSE
            CASE JOB_REC.LINESIM_STATUS
               WHEN 'F' THEN
                  vNote := 'Current LS Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GoTo EndLS;

               WHEN 'I' THEN
                  vNote := 'Current LS Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GoTo EndLS;

               ELSE
                  vNote := 'Unable to recognize current LS Status ' || JOB_REC.LINESIM;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;

         SQL1 := 'Select * From GZ_LINESIM_SETUP Where JOBID = :1';
         EXECUTE IMMEDIATE SQL1 into LINESIM_REC USING pJobID; -- 'ACS11_Z6_ST99_1'
         DBMS_OUTPUT.PUT_LINE('LINESIM_REC.JOBID = ' || LINESIM_REC.JOBID);

         -- vGenSchema, vProjectID
         vTopoOut := LINESIM_REC.TOPO_OUT;
         vTopoIn := LINESIM_REC.TOPO_IN;
         vTopoBk := 'BK';
         vLSModules := LINESIM_REC.LS_MODULES;
         vStateFP := LINESIM_REC.STATEFP;
         vSkipEdgesTable := LINESIM_REC.SKIP_EDGES_TABLE;


         BEGIN
            UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'START', 'I');

            APP_INFO_ACTION  := 'Begin LS';
            DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            GZ_LINESIM.LINE_SIM (vGenSchema,
                                 vProjectID,
                                 pJobID,
                                 vTopoOut,
                                 vLSModules,
                                 vTopoIn,
                                 vTopoOut || vTopoBk,
                                 vState_TBL,
                                 vStateFP,
                                 vSkipEdgesTable,
                                 linesim_rec.validate_topo,
                                 linesim_rec.topofix_edge,
                                 linesim_rec.topofix_2edge,
                                 linesim_rec.topofix_qa);

            vLSStatus := 'S';
            vLSTrackingTable := vTopoOut || '_LS_TRACKING';

            SQL1 := 'Select count(*) from ' || vLSTrackingTable;
            SQL1 := SQL1 || ' where error_msg = :1';
            Execute Immediate SQL1 into rcdCount using 'Complete - Successful';

            IF rcdCount <> 1 THEN

               DBMS_OUTPUT.PUT_LINE(vLSTrackingTable || ' did not complete successfully.  rcdCount: ' || rcdCount);
               vLSStatus := 'F';
            END IF;

         EXCEPTION
            WHEN Others THEN
               vSQLErrMsg := SQLERRM;
               vLSStatus := 'F';
               vNote := 'LineSim Failed ' || vSQLErrMsg;
               DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
               DBMS_OUTPUT.PUT_LINE('GZ_LINESIM.LINE_SIM (' || vGenSchema || ',' || vProjectID || ',' || pJobID ||',' || vTopoOut || ','
                                                               || vLSModules || ',' || vTopoIn  || ',' || vTopoOut
                                                               || vTopoBk || ',' || vState_TBL || ',' || vStateFP|| ','
                                                               || vSkipEdgesTable || ',' || linesim_rec.validate_topo || ','
                                                               || linesim_rec.topofix_edge || ',' || linesim_rec.topofix_2edge || ','
                                                               || linesim_rec.topofix_qa || ');');
                                                               
         END;

         CASE vLSStatus
            WHEN 'S' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vLSStatus);
               APP_INFO_ACTION  := 'End LS';
               DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            WHEN 'F' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vLSStatus);

                RAISE_APPLICATION_ERROR(-20001, vNote);

               ELSE
                  UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM);

         END CASE;

        <<EndLS>>
        NULL;
      END IF;

      IF (JOB_REC.MERGE = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('PROCESSING MERGE');

         vPrevModule := vCurrModule;
         vCurrModule := 'MRG';
         -- Check if Current Module can run

         If (JOB_REC.MERGE_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the MRG process');
         ELSE
            CASE JOB_REC.MERGE_STATUS
               WHEN 'F' THEN
                  vNote := 'Current MRG Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GoTo EndMRG;

               WHEN 'I' THEN
                  vNote := 'Current MRG Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GoTo EndMRG;

               ELSE
                  vNote := 'Unable to recognize current MRG Status ' || JOB_REC.MERGE_STATUS;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;


         UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'START', 'I');

         APP_INFO_ACTION  := 'Preparing Topo tbl';
         DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

         SQL1 := 'Select * From GZ_MERGE_SETUP Where JOBID = :1';
         EXECUTE IMMEDIATE SQL1 into MERGE_REC USING pJobID; -- 'ACS11_Z6_ST99_1'
         DBMS_OUTPUT.PUT_LINE('MERGE_REC.JOBID = ' || MERGE_REC.JOBID);

         -- vGenSchema, vProjectID
         vMRG_TopoInTbl := MERGE_REC.TOPO_IN_TBL;
         vTopoOut := MERGE_REC.TOPO_OUT;
         vMRG_Modules := MERGE_REC.MRG_MODULES;
         vMRG_FaceTblExt := MERGE_REC.FACE_OUT_TBL_EXT;
         vRestartFlg := MERGE_REC.RESTART;

         vRelease := JOB_REC.RELEASE;

         SQL1 := 'Select count(jobid) from gz_job_setup where '
              || 'release = :1 and gen_project_id = :2 and '
              || 'step = (Select dependency from gz_job_setup where jobid = :3) and use_for_merge = :4';
         DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
         Execute Immediate SQL1 into NumOfTopo2Mrg Using vRelease, vProjectID, pJobID, 'Y';
         DBMS_OUTPUT.PUT_LINE('NumOfTopo2Mrg: ' || NumOfTopo2Mrg);

         -- Get NumOfTopoDone  where run_status is 'S' or 'M'
         SQL1 := 'SELECT count(jobid) FROM gz_job_setup WHERE '
              || 'release = :1 AND gen_project_id = :2 AND '
              || 'step = (SELECT dependency FROM gz_job_setup WHERE jobid = :3) AND use_for_merge = :4 AND run_status IN (:5,:6)';
         DBMS_OUTPUT.PUT_LINE('SQL1: ' || SQL1);
         Execute Immediate SQL1 into NumOfTopoDone Using vRelease, vProjectID, pJobID, 'Y', 'S', 'M';
         DBMS_OUTPUT.PUT_LINE('NumOfTopoDone: ' || NumOfTopoDone);

         -- Is NumOfTopo2Mrg = NumOfTopoDone?
         IF (NumOfTopo2Mrg > 0) and (NumOfTopo2Mrg = NumOfTopoDone) THEN
            SQL1 := 'Select FACE_OUTPUT_TABLE from GEN_CLIP_PARAMETERS Where GEN_PROJECT_ID = :1 AND RELEASE = :2';
            Execute Immediate SQL1 into FaceOutputTbl using vProjectID, vRelease;

            -- Create vMRG_TopoInTbl  (drop if it exists)
            SQL1 := 'Select count(*) from user_tables where table_name = :1';
            Execute Immediate SQL1 into rcdCount Using vMRG_TopoInTbl;

            IF rcdCount = 1 THEN

               SQL1 := 'Drop table ' || vMRG_TopoInTbl || ' Purge';
               Execute Immediate SQL1;

            END IF;

            SQL1 :=         'Create table ' || vMRG_TopoInTbl || ' as ';
            SQL1 := SQL1 || 'Select topology topo_name, table_name face_table';
            SQL1 := SQL1 || '  from user_sdo_topo_info  ';
            SQL1 := SQL1 || ' where topology in (Select topo_tag || ''LS'' ';
            SQL1 := SQL1 || '                      from gz_job_setup ';
            SQL1 := SQL1 || '                     where release = ''' || vRelease || '''';
            SQL1 := SQL1 || '                       and gen_project_id = ''' || vProjectID || '''';
            SQL1 := SQL1 || '                       and step = (Select dependency from gz_job_setup ';
            SQL1 := SQL1 || '                                    where jobid = ''' || pJobID || ''')';
            SQL1 := SQL1 || '                       and use_for_merge = ''Y'' )';
            SQL1 := SQL1 || '   and table_name like ''%' || FaceOutputTbl || '''';

            Execute Immediate SQL1;

            -- Count rcds in vMRG_TopoInTbl it should match the NumOfTopo2Mrg else raise error
            SQL1 := 'Select count(*) from ' || vMRG_TopoInTbl;
            Execute Immediate SQL1 into rcdCount;

            If rcdCount <> NumOfTopo2Mrg Then
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');
               RAISE_APPLICATION_ERROR(-20001,'Check topology count.  Cannot proceed with Topo Merge');
            End If;

         ELSE
            UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');
            RAISE_APPLICATION_ERROR(-20001,'Cannot proceed with Topo Merge');
         END IF;

         BEGIN

            APP_INFO_ACTION  := 'Begin MRG';
            DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            returnval := GZ_TOPO_MERGE.GENERALIZATION_TOPO_MERGE(vGenSchema,
                                                                 pJobID,
                                                                 vRelease,
                                                                 vProjectID,
                                                                 vMRG_TopoInTbl,
                                                                 vTopoOut,
                                                                 vMRG_FaceTblExt,
                                                                 vMRG_Modules,
                                                                 vRestartFlg,
                                                                 merge_rec.validate_topo,
                                                                 merge_rec.topofix_edge,
                                                                 merge_rec.topofix_2edge,
                                                                 merge_rec.topofix_qa);

            IF returnVal = 0 THEN
               vMRGStatus := 'S';
            ELSE
               vMRGStatus := 'F';
            END IF;

         EXCEPTION
            WHEN Others THEN
            
               vSQLErrMsg := SQLERRM;
               vMRGStatus := 'F';
               
               vNote := 'Topo Merge Failed ' || vSQLErrMsg;
               
               DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
               
               dbms_output.put_line('GZ_TOPO_MERGE.GENERALIZATION_TOPO_MERGE('
                                   || vGenSchema||','||pJobID||','||vRelease||','||vProjectID||','||vMRG_TopoInTbl||','
                                   || vTopoOut||','||MERGE_REC.FACE_OUT_TBL_EXT||','||vMRG_Modules||','||vRestartFlg||','
                                   || merge_rec.validate_topo || ',' || merge_rec.topofix_edge || ','
                                   || merge_rec.topofix_2edge || ',' || merge_rec.topofix_qa || ');');
         END;

         CASE vMRGStatus
            WHEN 'S' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vMRGStatus);
               APP_INFO_ACTION  := 'End MRG';
               DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            WHEN 'F' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vMRGStatus);

               RAISE_APPLICATION_ERROR(-20001, SQLERRM);

            ELSE
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');

               RAISE_APPLICATION_ERROR(-20001, SQLERRM);

         END CASE;

        <<EndMRG>>
        NULL;
      END IF;

      IF (JOB_REC.FSLBUILD = 'A') THEN

         APP_INFO_ACTION  := 'Begin Output';
         DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
         vCurrModule := 'OUT';

         IF (JOB_REC.FSLBUILD_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the OUT process');
         ELSE
            CASE JOB_REC.FSLBUILD_STATUS
               WHEN 'F' THEN
                  vNote := 'Current OUT Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GoTo EndOUT;

               WHEN 'I' THEN
                  vNote := 'Current OUT Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GoTo EndOUT;

               ELSE
                  vNote := 'Unable to recognize current OUT Status ' || JOB_REC.FSLBUILD_STATUS;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;

         DBMS_OUTPUT.PUT_LINE('PROCESSING ALTERNATE TOPOLOGY OUTPUT');
         SQL1 := 'SELECT * FROM GZ_OUTPUT_SETUP WHERE JOBID = :1';
         EXECUTE IMMEDIATE SQL1 into OUTPUT_REC USING pJobID;
         DBMS_OUTPUT.PUT_LINE('OUTPUT_REC.JOBID = ' || OUTPUT_REC.JOBID);

         vSourceSchema:= OUTPUT_REC.SOURCE_SCHEMA;
         vSourceTOPO := OUTPUT_REC.SOURCE_TOPOLOGY;
         vOutTOPO := OUTPUT_REC.OUTPUT_TOPOLOGY;
         vModules := OUTPUT_REC.MODULES;
         vRSFlag := OUTPUT_REC.RESTART_FLAG;
         vSRID := OUTPUT_REC.SRID;
         vTolerance := OUTPUT_REC.TOLERANCE;
         vDropTab := OUTPUT_REC.DROP_TABLES;
         vTileCount := OUTPUT_REC.TILE_COUNT;

         BEGIN
            UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'START', 'I');
            APP_INFO_ACTION  := 'Begin OUT';
            DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

             retval := GZ_OUTPUT.GENERALIZATION_OUTPUT(vRelease,
                                                       vProjectID,
                                                       vSourceSchema,
                                                       vSourceTOPO,
                                                       vOutTOPO,
                                                       vModules,
                                                       vRSFlag,
                                                       OUTPUT_REC.SINGLE_LAYER,
                                                       vTileCount,
                                                       output_rec.prcs_slivers,
                                                       output_rec.sliver_restart_flag,
                                                       output_rec.sliver_width,
                                                       output_rec.segment_length,
                                                       output_rec.expendable_review,
                                                       output_rec.reshape_review,
                                                       vSRID,
                                                       vTolerance,
                                                       vDropTab,
                                                       output_rec.validate_topo,
                                                       output_rec.topofix_edge,
                                                       output_rec.topofix_2edge,
                                                       output_rec.topofix_qa);

         IF (retval = '0' ) THEN
              vOUTStatus := 'S';
         ELSE
              vOUTStatus := 'F';
         END IF;
         -- Update TopoBuild_Status

         EXCEPTION
            WHEN Others THEN
               vSQLErrMsg := SQLERRM;
               vOUTStatus := 'F';
               vNote := 'ALTERNATE TOPOLOGY OUTPUT Failed ' || vSQLErrMsg;
               DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
               DBMS_OUTPUT.PUT_LINE('GZ_OUTPUT.GENERALIZATION_OUTPUT ('
                     || vRelease || ',' || vProjectID || ','|| vSourceSchema ||',' || vSourceTOPO ||','
                     || vOutTOPO ||',' || vModules ||',' || vRSFlag ||',' ||OUTPUT_REC.SINGLE_LAYER||','
                     || vTileCount || ',' || output_rec.prcs_slivers||','||output_rec.sliver_restart_flag||','
                     ||output_rec.sliver_width||','||output_rec.segment_length||','||output_rec.expendable_review||','
                     ||output_rec.reshape_review||','||vSRID||','||vTolerance||','||vDropTab
                     || output_rec.validate_topo || ',' || output_rec.topofix_edge || ','
                     || output_rec.topofix_2edge || ',' || output_rec.topofix_qa || ');');

         END;

         CASE vOUTStatus
            WHEN 'S' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vOUTStatus);
               APP_INFO_ACTION  := 'End OUT';
               DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            WHEN 'F' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vOUTStatus);

               RAISE_APPLICATION_ERROR(-20001, vNote);

            ELSE
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');
               RAISE_APPLICATION_ERROR(-20001, SQLERRM);

         END CASE;

         <<EndOUT>>
         NULL;
      ELSIF (JOB_REC.FSLBUILD = 'T') THEN
         APP_INFO_ACTION  := 'Begin FSL Build';
         DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

         vPrevModule := vCurrModule;
         vCurrModule := 'FSL';

         IF (JOB_REC.FSLBUILD_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the FSLBuild process');
         ELSE
            CASE JOB_REC.FSLBUILD_STATUS
               WHEN 'F' THEN
                  vNote := 'Current FSLBuild Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GoTo EndFSL;

               WHEN 'I' THEN
                  vNote := 'Current FSL Build Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GoTo EndFSL;

               ELSE
                  vNote := 'Unable to recognize current FSL Build Status ' || JOB_REC.FSLBUILD_STATUS;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;

         DBMS_OUTPUT.PUT_LINE('PROCESSING FSLBUILD');

         SQL1 := 'Select * From GZ_FSLBUILD_SETUP Where JOBID = :1';
         EXECUTE IMMEDIATE SQL1 into FSLBUILD_REC USING pJobID; -- 'ACS11_Z6_ST99_1'
         DBMS_OUTPUT.PUT_LINE('FSLBUILD_REC.JOBID = ' || FSLBUILD_REC.JOBID);

         -- vGenSchema, vProjectID
         vTopoIn := FSLBUILD_REC.TOPO_IN;
         vUnGenTopology := FSLBUILD_REC.UNGEN_TOPO;
         vFSLFaceTblExt := FSLBUILD_REC.FACE_TABLE_EXT;
         vUniverseTbl := FSLBUILD_REC.UNIVERSE_TABLE;
         vHierarchyTbl := FSLBUILD_REC.HIERARCHY_TABLE;
         -- vHasMirrors := FSLBUILD_REC.HAS_MIRRORS;
         vProjectZ9 := FSLBUILD_REC.PROJECT_Z9;
         vRelease := FSLBUILD_REC.RELEASE;
         vStateEdgeTbl := FSLBUILD_REC.STATE_EDGES_TABLE;
         vDropTbls := FSLBUILD_REC.DROP_TABLES;  -- Where do I use this?

         BEGIN
            UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'START', 'I');

            APP_INFO_ACTION  := 'Begin FSLBUILD';
            DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);
            --GZ_FSL.FSL_BUILD(   pSchema, pTopo_In, pFace_Tab_Ext,pTopo_Universe,pTopo_Hierarchy, pHas_Mirrors, pProject_Id, pProject_Z9, pRelease, pSt_Edges_Tbl);

            IF vUnGenTopology IS NOT NULL THEN
               vTopoType := 'UG';

               DBMS_OUTPUT.PUT_LINE('Building FSLs on UnGenTopology: ' || vUnGenTopology);
               DBMS_OUTPUT.PUT_LINE('GZ_FSL.FSL_BUILD('|| vGenSchema ||','|| vUnGenTopology ||','|| vFSLFaceTblExt ||','|| vUniverseTbl ||','||vHierarchyTbl ||','|| vProjectID ||','|| vProjectZ9 ||','|| vRelease ||','|| vStateEdgeTbl);
               -- FSL_BUILD(   pSchema IN VARCHAR2,    pTopo_In IN VARCHAR2,   pFace_Tab_Ext IN VARCHAR2,   pTopo_Universe IN VARCHAR2,   pTopo_Hierarchy IN VARCHAR2,   pHas_Mirrors IN VARCHAR2,   pProject_Id IN VARCHAR2,   pProject_Z9 IN VARCHAR2,   pRelease In VARCHAR2,   pSt_Edges_Tbl in VARCHAR2   )
               GZ_FSL.FSL_BUILD( vGenSchema, vUnGenTopology, vTopoType, vFSLFaceTblExt, vUniverseTbl,vHierarchyTbl, vProjectID, vProjectZ9, vRelease, vStateEdgeTbl, pJobID);

            END IF;

            IF vTopoIn IS NOT NULL THEN
               vTopoType := 'G';

               DBMS_OUTPUT.PUT_LINE('Building FSLs on Topology: ' || vTopoIn);
               DBMS_OUTPUT.PUT_LINE('GZ_FSL.FSL_BUILD('|| vGenSchema ||','|| vTopoIn ||','|| vFSLFaceTblExt ||','|| vUniverseTbl ||','||vHierarchyTbl ||','|| vProjectID ||','|| vProjectZ9 ||','|| vRelease ||','|| vStateEdgeTbl || ';');
               GZ_FSL.FSL_BUILD( vGenSchema, vTopoIn, vTopoType, vFSLFaceTblExt, vUniverseTbl,vHierarchyTbl, vProjectID, vProjectZ9, vRelease, vStateEdgeTbl, pJobID);

            END IF;

            vFSLBuildStatus := 'S';

         EXCEPTION
            WHEN Others THEN
               vSQLErrMsg := SQLERRM;
               vFSLBuildStatus := 'F';
               vNote := 'FSL Build Failed ' || vSQLErrMsg;
               DBMS_OUTPUT.PUT_LINE(vNote);
               DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
              IF vTopoType = 'UG' THEN
                  DBMS_OUTPUT.PUT_LINE('Ungeneralized topology');
                  DBMS_OUTPUT.PUT_LINE('GZ_FSL.FSL_BUILD( ' || vGenSchema ||',' || vUnGenTopology ||',' || vTopoType ||','
                                                                                          || vFSLFaceTblExt ||',' || vUniverseTbl ||',' ||vHierarchyTbl ||','
                                                                                          || vProjectID ||',' || vProjectZ9 ||',' || vRelease
                                                                                          ||',' || vStateEdgeTbl ||',' || pJobID ||');');

              ELSIF vTopoType = 'G' THEN
                  DBMS_OUTPUT.PUT_LINE('Generalized topology');
                  DBMS_OUTPUT.PUT_LINE('GZ_FSL.FSL_BUILD( ' || vGenSchema || ',' || vTopoIn || ',' ||  vTopoType
                                                                                        || ',' ||  vFSLFaceTblExt || ',' || vUniverseTbl || ',' || vHierarchyTbl
                                                                                        || ',' ||  vProjectID || ',' ||  vProjectZ9 || ',' ||  vRelease
                                                                                        || ',' ||  vStateEdgeTbl || ',' ||  pJobID || ');');

              ELSE
                  DBMS_OUTPUT.PUT_LINE('Bad Topology Type');
              END IF;

         END;

         CASE vFSLBuildStatus
            WHEN 'S' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vFSLBuildStatus);
               APP_INFO_ACTION  := 'End FSLBuild';
               DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            WHEN 'F' THEN
               vNote := 'FSLBUILD Failed';
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vFSLBuildStatus);

               RAISE_APPLICATION_ERROR(-20001, vNote);

            ELSE
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');

               RAISE_APPLICATION_ERROR(-20001, SQLERRM);

         END CASE;

        <<EndFSL>>
        NULL;
      END IF;

      IF (JOB_REC.CLIP_ATTR = 'Y') THEN

         DBMS_OUTPUT.PUT_LINE('PROCESSING CLIP ATTRIBUTE');

         vPrevModule := vCurrModule;
         vCurrModule := 'CA';

         IF (JOB_REC.CLIP_ATTR_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the CA process');
         ELSE
            CASE JOB_REC.CLIP_ATTR_STATUS
               WHEN 'F' THEN
                  vNote := 'Current CA Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GoTo EndCA;

               WHEN 'I' THEN
                  vNote := 'Current CA Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GoTo EndCA;

               ELSE
                  vNote := 'Unable to recognize current CA Status ' || JOB_REC.CLIP_STATUS;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;

         SQL1 := 'SELECT * FROM GZ_CLIP_SETUP WHERE JOBID = :1';
         EXECUTE IMMEDIATE SQL1 into CLIP_REC USING pJobID; -- 'ACS11_Z6_ST99_1'
         DBMS_OUTPUT.PUT_LINE('CLIP_REC.JOBID = ' || CLIP_REC.JOBID);

         vClipMask := CLIP_REC.GEN_CLIP_MASK;
         vClipJobID := CLIP_REC.CL_JOBID;
         vTopoIn := CLIP_REC.TOPO_IN;
         vTopoOut := CLIP_REC.TOPO_OUT;
         vClipModules := CLIP_REC.GEN_CLIP_MODULES;
         vClipDropTables := CLIP_REC.DROP_TABLES;
         vClipTransferAtts := CLIP_REC.TRANSFER_ATTS;

         Begin

            UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'START', 'I');

            APP_INFO_ACTION  := 'Begin CA';
            DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            returnVal := GZ_CLIP.GENERALIZATION_CLIP(vGenSchema, 
                                                     vRelease, 
                                                     vProjectID, 
                                                     vClipJobID, 
                                                     vGenClipTab, 
                                                     vClipMask, 
                                                     vTopoOut, 
                                                     vTopoIn, 
                                                     vClipDropTables, 
                                                     vClipTransferAtts, 
                                                     'N', 
                                                     vClipModules,
                                                     clip_rec.validate_topo,
                                                     clip_rec.topofix_edge,
                                                     clip_rec.topofix_2edge,
                                                     clip_rec.topofix_qa);

            dbms_output.put_line('returnVal = ' || returnVal);

            -- Check for success
            If (returnVal = 0) Then
                vCLStatus := 'S';
            else
                vCLStatus := 'F';
            end if;

         EXCEPTION
            WHEN Others THEN
               vSQLErrMsg := SQLERRM;
               returnVal := 1;
               vCLStatus := 'F';
               vNote := 'Clip Failed ' || vSQLErrMsg;
               DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
               DBMS_OUTPUT.PUT_LINE('GZ_CLIP.GENERALIZATION_CLIP(' || vGenSchema || ',' || vRelease || ',' || vProjectID || ',' 
                                                                   || vClipJobID || ',' || vGenClipTab || ',' ||vClipMask || ','
                                                                   ||vTopoOut || ',' ||vTopoIn || ',' ||vClipDropTables || ',' 
                                                                   ||vClipTransferAtts || ', N, ' || vClipModules || ','
                                                                   || clip_rec.validate_topo || ',' || clip_rec.topofix_edge || ','
                                                                   || clip_rec.topofix_2edge || ',' || clip_rec.topofix_qa || ');');
         END;

         CASE vCLStatus
            WHEN 'S' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vCLStatus);
               APP_INFO_ACTION  := 'End CA';
               DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            WHEN 'F' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vCLStatus);

               RAISE_APPLICATION_ERROR(-20001, vNote);

            ELSE
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');
               RAISE_APPLICATION_ERROR(-20001, SQLERRM);

         END CASE;

        <<EndCA>>
        NULL;
      END IF;

      IF (JOB_REC.QA = 'Y') THEN
         DBMS_OUTPUT.PUT_LINE('PROCESSING QA');
         vPrevModule := vCurrModule;
         vCurrModule := 'QA';
         -- Check if Current Module can run

         If (JOB_REC.QA_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the QA process');
         ELSE
            CASE JOB_REC.QA_STATUS
               WHEN 'F' THEN
                  vNote := 'Current QA Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GoTo EndQA;

               WHEN 'I' THEN
                  vNote := 'Current QA Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GoTo EndQA;

               ELSE
                  vNote := 'Unable to recognize current QA Status ' || JOB_REC.QA;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;

         SQL1 := 'SELECT * FROM GZ_QA_SETUP WHERE JOBID = :1';
         EXECUTE IMMEDIATE SQL1 into QA_REC USING pJobID; -- 'ACS11_Z6_ST99_1'
         DBMS_OUTPUT.PUT_LINE('QA_REC.JOBID = ' || QA_REC.JOBID);

         vGenSchema := USER;
         vGenTopology :=  QA_REC.GEN_TOPO;
         vGenSdoGeomCol := QA_REC.GEN_SDOGEOM_COL;
         vUnGenSchema := QA_REC.UNGEN_SCHEMA;
         vUnGenTopology := QA_REC.UNGEN_TOPO;
         vUnGenSdoGeomCol :=  QA_REC.UNGEN_SDOGEOM_COL;
         vOutPutTableSuffix := QA_REC.OUT_TBL_SUFFIX;
         vtarget_scale := QA_REC.TARGET_SCALE;

         vCompareGenColumn := QA_REC.COMPARE_GEN_COLUMN;
         vAreaCheck := QA_REC.AREA_CHECK;
         vShapeCheck := QA_REC.SHAPE_CHECK;

         vEntireTopology := QA_REC.ENTIRE_TOPO;
         vGenTableName := QA_REC.GEN_TBL_NAME;
         vUnGenTableName := QA_REC.UNGEN_TBL_NAME;

         vMissingRecTbl := QA_REC.MISSING_REC_TBL;
         vMissingRecKey := QA_REC.MISSING_REC_KEY;
         vMissingRecFeatTbl := QA_REC.MISSING_REC_FEAT_TBL;

         BEGIN

            UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'START', 'I');

            APP_INFO_ACTION  := 'Begin QA';
            DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            -------   GZ_QA.Build_QA_TBLS_VIEWS on hold for now ------------

            -- GZ_QA.Build_QA_TBLS_VIEWS(vGenSchema,
            --                            vGenTopology,
            --                            vGenSdoGeomCol,
            --                            vUnGenSchema,
            --                            vUnGenTopology,
            --                            vUnGenSdoGeomCol,
            --                            vOutPutTableSuffix,
            --                            vtarget_scale,
            --                            vCompareGenColumn,
            --                            vAreaCheck,
            --                            vShapeCheck,
            --                            vEntireTopology,
            --                            vGenTableName,
            --                            vUnGenTableName,
            --                            vMissingRecTbl,
            --                            vMissingRecKey,
            --                            vMissingRecFeatTbl);

            -------   GZ_QA.Build_QA_TBLS_VIEWS on hold for now ------------

            -- just run most basic checks on job...

            retval := GZ_QA_BASICS.GENERALIZATION_RUN_BASIC_QA(pJobID,
                                                               vGenTopology,
                                                               vUnGenTopology,
                                                               vGenTableName,
                                                               vUnGenTableName);

            IF (retval = '0' ) THEN
                 vQAStatus := 'S';
            ELSE
                 vQAStatus := 'F';
            END IF;

         EXCEPTION
            WHEN Others THEN
               vSQLErrMsg := SQLERRM;
               vQAStatus := 'F';
               vNote := 'QA Module Failed ' || vSQLErrMsg;
               DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
               DBMS_OUTPUT.put_line (
                  'GZ_QA_BASICS.GENERALIZATION_RUN_BASIC_QA('
                  ||pJobID||','||vGenTopology||','
                  ||vUnGenTopology||','||vGenTableName||','
                  ||vUnGenTableName||');');

               -------   GZ_QA.Build_QA_TBLS_VIEWS on hold for now ------------

               --DBMS_OUTPUT.put_line ('GZ_QA.Build_QA_TBLS_VIEWS(' ||vGenSchema || ',' || vGenTopology || ',' || vGenSdoGeomCol
               --                                                                                      || ',' || vUnGenSchema || ',' || vUnGenTopology || ',' || vUnGenSdoGeomCol
               --                                                                                       || ',' || vOutPutTableSuffix || ',' ||vtarget_scale || ',' || vCompareGenColumn
               --                                                                                       || ',' || vAreaCheck || ',' || vShapeCheck || ',' || vEntireTopology
               --                                                                                       || ',' ||vGenTableName || ',' || vUnGenTableName || ',' ||vMissingRecTbl
               --                                                                                       || ',' || vMissingRecKey || ',' || vMissingRecFeatTbl || ');');

               -------   GZ_QA.Build_QA_TBLS_VIEWS on hold for now ------------

         END;

         UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vQAStatus);

         CASE vQAStatus
            WHEN 'S' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vQAStatus);
               APP_INFO_ACTION  := 'End QA';
               DBMS_APPLICATION_INFO.SET_MODULE(APP_INFO_MODULE,APP_INFO_ACTION);

            WHEN 'F' THEN
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', vQAStatus);

               RAISE_APPLICATION_ERROR(-20001, vNote);

            ELSE
               UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'END', 'F');

               RAISE_APPLICATION_ERROR(-20001, SQLERRM);

         END CASE;

        <<EndQA>>
        NULL;
      END IF;

      IF (JOB_REC.SHAPEFILE = 'Y') THEN
         vPrevModule := vCurrModule;
         vCurrModule := 'SHP';
         -- Check if Current Module can run
         DBMS_OUTPUT.PUT_LINE('PROCESSING SHAPEFILES');

         IF (JOB_REC.SHAPEFILE_STATUS IS NULL) THEN
            DBMS_OUTPUT.PUT_LINE('Current status is NULL, go ahead and start the SHAPEFILE process');
         ELSE
            CASE JOB_REC.SHAPEFILE_STATUS
               WHEN 'F' THEN
                  vNote := 'Current SHAPEFILE Status is Fail';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'S' THEN
                  GoTo EndSHAPEFILE;

               WHEN 'I' THEN
                  vNote := 'Current SHAPEFILE Status is already running';
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);

               WHEN 'M' THEN
                  GoTo EndSHAPEFILE;

               WHEN 'R' THEN
                  -- ShapeFile status is set to R(eady).... Go ahead
                  GoTo EndSHAPEFILE;

               ELSE
                  vNote := 'Unable to recognize current QA Status ' || JOB_REC.QA;
                  DBMS_OUTPUT.PUT_LINE(vNote);

                  RAISE_APPLICATION_ERROR(-20001, vNote);
            END CASE;
         END IF;

        UPDATE_WORKFLOW_STATUS(pJobID, vCurrModule, 'START', 'R');

        <<EndSHAPEFILE>>
        NULL;
      ELSE
         SQL1 := 'Update GZ_JOB_SETUP SET RUN_STATUS = :1, END_TIME = :2 WHERE JOBID = :3';
         EXECUTE IMMEDIATE SQL1 USING 'S', SYSTIMESTAMP, pJobID;

         COMMIT;
      END IF;


   EXCEPTION
    WHEN Others THEN
           DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
           SQL1 := 'Update GZ_JOB_SETUP SET RUN_STATUS = :1, END_TIME = :2 WHERE JOBID = :3';
           EXECUTE IMMEDIATE SQL1 USING 'F', SYSTIMESTAMP, pJobID;
           COMMIT;
           RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' ' || DBMS_UTILITY.format_error_backtrace);
   END Generalize;

  PROCEDURE Create_Table(pSchemaName VARCHAR2, pTableName VARCHAR2, pGzType VARCHAR2, pDropFlag VARCHAR2 DEFAULT 'N')
  AS
     functionName VARCHAR2(40) := 'NEW_' ||pTableName;
     returnType VARCHAR2(40) := pGzType;

     sql1 VARCHAR2(4000);
     psql VARCHAR2(4000);

  BEGIN
   -- 1.  Create a pipelined function that returns the required nested-table type from GZ_TYPES package

   psql := 'Create or Replace Function ' || functionName || ' Return ' || returnType || ' PIPELINED ';
   psql := psql || ' AS ';
   psql := psql || ' Begin ';
   psql := psql || '    NULL; ';
   psql := psql || ' End ' || functionName || ';';

   --dbms_output.put_line(psql);

   Execute immediate psql;

   --dbms_output.put_line('Created function');

   -- 2. Create the table and grant select to public (should grant select to GZ role)
   psql := 'CREATE TABLE ' || pTableName || ' ';
   psql := psql || ' NOPARALLEL NOLOGGING AS '
                || 'SELECT * FROM TABLE(' || pSchemaName || '.' || functionName ||') ';
   BEGIN
      EXECUTE IMMEDIATE psql;
   EXCEPTION
      WHEN OTHERS
      THEN
         IF SQLCODE = -60
         OR SQLCODE = -18014
         THEN
            --ORA-00060: deadlock detected while waiting for resource
            -- Matt says it is usually bogus, just heavy production
            -- let's try again
            EXECUTE IMMEDIATE psql;
         ELSIF SQLCODE = -955
         THEN
            --table already exists
            If pDropFlag = 'Y'
            Then
               --dbms_output.put_line('Table ' || pTableName || ' already exists.  Let''s drop and try to create the table');
               EXECUTE IMMEDIATE 'DROP TABLE ' || UPPER(pTableName) || ' PURGE';
               EXECUTE IMMEDIATE psql;
            Else
               dbms_output.put_line('Table ' || pTableName || ' already exists.  Please set the drop flag to Y and rerun to recreate the table');
            End If;
         ELSE
            RAISE;
         END IF;
   END;

   EXECUTE IMMEDIATE 'GRANT SELECT ON ' || pTableName || ' TO "PUBLIC" ';




   -- 3.  Drop the Pipelined function created in step 1

   psql := 'Drop function ' || functionName;

   Execute immediate psql;
   --dbms_output.put_line('Dropped function');



  END;

   PROCEDURE Insert_New_jobs (
      pRel              IN VARCHAR2,
      pPrj              IN VARCHAR2,
      pSchema           IN VARCHAR2 DEFAULT USER,
      ptopobuildtype    IN VARCHAR2 DEFAULT 'A'
   )
   AS

      --Matt! Added ptopobuildtype input 8/28/12
      --Matt! Added skip_edge_table population to GZ_LINESIM_SETUP
      --Matt! 6/7/13 Added gz_output_parameters tile_count switcheroo for nation vs state
      --Matt! 6/13/13 Added topofix_qa default Ys and Ns
      --Matt! 7/31/13 New coastal sliver parameters
      --Stephanie! 8/6/2013 Updated QA to run by default for step 2 and updated QA_SETUP table face feature table names
      --M! 8/16/13 Switched topofix_qa defaults for Linesim to N
      --M! 12/30/13: All 6 modules input: validate_topo, topofix_edge, topofix_2edge, topofix_qa

      vRelease                 VARCHAR2 (10) := pRel;                 -- 'ACS11';
      vProjectID               VARCHAR2 (4) := pPrj;                     -- 'Z8';
      vSchema                  VARCHAR2 (30) := UPPER (pSchema);

      vJobPrmRec               GZ_TYPES.GZ_JOB_PARAMETERS_REC;

      vClipPrmRec              GZ_TYPES.GEN_CLIP_PARAMETERS_REC;
      vSMPolyPrmRec            GZ_TYPES.SMALL_POLYGON_PARAMETERS_REC;
      vLineSimPrmRec           GZ_TYPES.LINE_SIM_PARAMETERS_REC;
      vFSLBuildPrmRec          GZ_TYPES.FSLBUILD_PARAMETERS_REC;
      vQAPrmRec                GZ_TYPES.QA_PARAMETERS_REC;
      vMergePrmRec             GZ_TYPES.GEN_TOPO_MERGE_PARAMETERS_REC;
      vShapeFilePrmRec         GZ_TYPES.SHAPEFILE_PARAMETERS_REC;

      vEntityTableRec          GZ_TYPES.GZ_ENTITY_TABLE_REC;
      vEntityTableNullRec      GZ_TYPES.GZ_ENTITY_TABLE_REC;
      vEntityTableTab          GZ_TYPES.GZ_ENTITY_TABLE;

      vBuildSourcePrmRec       GZ_TYPES.GZ_BUILD_SOURCE_PARAMETERS_REC;
      vBuildSourcePrmNullRec   GZ_TYPES.GZ_BUILD_SOURCE_PARAMETERS_REC;

      vOutputPrmRec            GZ_TYPES.GZ_OUTPUT_PARAMETERS_REC;
      vOutputPrmNullRec        GZ_TYPES.GZ_OUTPUT_PARAMETERS_REC;

      SQL1                     VARCHAR2 (2000);
      tab_col                  VARCHAR2 (2000);
      vJOBID                   VARCHAR2 (32);
      tblName                  VARCHAR2 (30);
      Error_Desc               VARCHAR2 (4000);
      vStep                    NUMBER (1);

      prcsClip                 VARCHAR2 (1) := 'N';
      prcsShapeFiles           VARCHAR2 (1) := 'N';
      vUSE_FOR_MERGE           VARCHAR2 (1);

      vTOPO_IN                 VARCHAR2 (20);
      vTRANSFER_ATTS           VARCHAR2 (1);
      vFACE_TABLE_EXT          VARCHAR2 (30);
      vPROJECT_Z9              VARCHAR2 (1);
      vTOP_LAYER_TABLE         VARCHAR2 (30);
      vFACE_OUT_TBL_EXT        VARCHAR2 (20);
      vTOPOBUILDTYPE           VARCHAR2 (1) := UPPER(ptopobuildtype);
      vTopoSCHEMA              VARCHAR2 (30);
      vSHP_UNIT_REQUEST        VARCHAR2 (100);
      process_slivers          VARCHAR2 (1);

      vQAdefault               VARCHAR2 (1) := 'N';

   BEGIN

      BEGIN
         SQL1 := 'Select * from GZ_JOB_PARAMETERS '
            || ' Where release = : 1 and gen_project_id = :2';

         EXECUTE IMMEDIATE SQL1 INTO vJobPrmRec USING vRelease, vProjectID;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            RAISE_APPLICATION_ERROR (  -20001, 'Job parameters not found for release '
               || vRelease || ' and project id ' || vProjectID);
         WHEN TOO_MANY_ROWS THEN
            RAISE_APPLICATION_ERROR ( -20001,
                  'Found more than one Job parameters records for release '
               || vRelease || ' and project id ' || vProjectID);
         WHEN OTHERS THEN
            Error_Desc:= SQLERRM;
            DBMS_OUTPUT.put_line (Error_Desc);
            RAISE_APPLICATION_ERROR(-20001,'There is error in GZ_JOB_PARAMETERS.');
      END;

      --------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      -- Step 1 GZ Process
      -- Depends on
      -- BENCH_MARK_PRCS_UNIT => TOPOBUILD_PRCS_UNIT and
      -- SHAPE_FILE_PRCS_UNIT (Could be S => S, N => S, N => N or N => B) 3
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      --------------------------------------------------------------------------

      vStep := 1;

      BEGIN
         tblName := 'GZ_BUILD_SOURCE_PARAMETERS';

         EXECUTE IMMEDIATE 'SELECT * FROM GZ_BUILD_SOURCE_PARAMETERS
          WHERE release = :1 AND gen_project_id = :2 AND rownum = 1 '
            INTO vBuildSourcePrmRec
            USING vRelease, vProjectID;
          vTOPOBUILDTYPE := 'A';

         EXECUTE IMMEDIATE 'SELECT * FROM GZ_OUTPUT_PARAMETERS
          WHERE release = :1 AND gen_project_id = :2 AND rownum = 1 '
            INTO vOutputPrmRec
            USING vRelease, vProjectID;

      EXCEPTION
         --WHEN NO_DATA_FOUND THEN
         --   RAISE_APPLICATION_ERROR(-20001,'Parameters not found for project id ' || vJobPrmRec.gen_project_id );
         --WHEN TOO_MANY_ROWS THEN
         --   RAISE_APPLICATION_ERROR(-20001,'Found more than one parameters records for project id ' || vJobPrmRec.gen_project_id );
         WHEN OTHERS THEN
            vTOPOBUILDTYPE := 'T';
            DBMS_OUTPUT. PUT_LINE (  'Using GZ_BUILD_SOURCE_PARAMETERS  for project id ' || vProjectID);
      END;

      DBMS_OUTPUT.PUT_LINE ('Processing Step: ' || vStep);

      SQL1 := 'SELECT * FROM GZ_ENTITY_TABLE '
         || 'WHERE release = :1 '
         || 'AND gen_project_id = :2 '
         || 'AND Entity_type = :3';

      EXECUTE IMMEDIATE SQL1
         BULK COLLECT INTO vEntityTableTab
         USING vRelease, vProjectID,
               vJobPrmRec.TOPOBUILD_PRCS_UNIT;

      IF vEntityTableTab.COUNT = 0 THEN
         RAISE_APPLICATION_ERROR ( -20001, 'No GZ_ENTITY_TABLE records found for release ' || vRelease
            || ' project id ' ||vProjectID || ' and TopoBuild_Prcs_Unit '
            || vJobPrmRec.TOPOBUILD_PRCS_UNIT);
      END IF;

      IF (vJobPrmRec.TOPOBUILD_PRCS_UNIT = 'N' OR vJobPrmRec.TOPOBUILD_PRCS_UNIT = 'B')
       AND vJobPrmRec.GZ_PRCS_UNIT = 'S' THEN
         prcsClip := 'Y';
      END IF;

      IF vJobPrmRec.SHAPE_FILE_PRCS_UNIT = 'B'  THEN
         prcsShapeFiles := 'Y';
         vSHP_UNIT_REQUEST := 'BOTH';
      ELSIF vJobPrmRec.SHAPE_FILE_PRCS_UNIT = 'N'  THEN
         prcsShapeFiles := 'Y';
         vSHP_UNIT_REQUEST := 'BOTH';
      ELSIF vJobPrmRec.SHAPE_FILE_PRCS_UNIT = 'S'  THEN
         prcsShapeFiles := 'Y';
         vSHP_UNIT_REQUEST := 'STATE';
      ELSE
         prcsShapeFiles := 'N';
      END IF;

      BEGIN
         -- Clip
         tblName := 'GEN_CLIP_PARAMETERS';

         EXECUTE IMMEDIATE 'SELECT * FROM GEN_CLIP_PARAMETERS
          WHERE release = :1 AND gen_project_id = :2'
            INTO vClipPrmRec
            USING vRelease, vProjectID;
      EXCEPTION
         --WHEN NO_DATA_FOUND THEN
         --   RAISE_APPLICATION_ERROR(-20001,'Parameters not found for project id ' || vJobPrmRec.gen_project_id );
         --WHEN TOO_MANY_ROWS THEN
         --   RAISE_APPLICATION_ERROR(-20001,'Found more than one parameters records for project id ' || vJobPrmRec.gen_project_id );
         WHEN OTHERS THEN
            --   RAISE;
            Error_Desc:= SQLERRM;
            DBMS_OUTPUT.put_line (Error_Desc);
            DBMS_OUTPUT. PUT_LINE (  'Verify Parameter table ' || tblName || ' for project id ' || vProjectID);
      END;

      BEGIN
         tblName := 'FSLBUILD_PARAMETERS';

         EXECUTE IMMEDIATE 'SELECT * FROM FSLBUILD_PARAMETERS
          WHERE release = :1 AND gen_project_id = :2'
            INTO vFSLBuildPrmRec
            USING vRelease, vProjectID;
      EXCEPTION
         --WHEN NO_DATA_FOUND THEN
         --   RAISE_APPLICATION_ERROR(-20001,'Parameters not found for project id ' || vJobPrmRec.gen_project_id );
         --WHEN TOO_MANY_ROWS THEN
         --   RAISE_APPLICATION_ERROR(-20001,'Found more than one parameters records for project id ' || vJobPrmRec.gen_project_id );
         WHEN OTHERS THEN
            --   RAISE;
            DBMS_OUTPUT. PUT_LINE ( 'Verify Parameter table ' || tblName || ' for project id ' || vProjectID);
      END;


      --+++++++++++++++++++++++++++++++++++++++++++++--
      -------------------------------------------------
      --actual step 1 setup table inserts start here
      -------------------------------------------------
      --+++++++++++++++++++++++++++++++++++++++++++++--

      IF vJobPrmRec.TOPOBUILD_PRCS_UNIT = 'S' 
      AND vJobPrmRec.GZ_PRCS_UNIT = 'S' 
      THEN

         --STATE processing unit for step 1
         --this code is NOT SOP, probably doesnt really even work any more
         --see the ELSE for a second set of inserts that are standard

         BEGIN
         
            tblName := 'SMALL_POLYGON_PARAMETERS';

            EXECUTE IMMEDIATE 'Select *
              From SMALL_POLYGON_PARAMETERS
             Where release = :1 AND gen_project_id = :2'
               INTO vSMPolyPrmRec
               USING vRelease, vProjectID;
         EXCEPTION
            --WHEN NO_DATA_FOUND THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Parameters not found for project id ' || vJobPrmRec.gen_project_id );
            --WHEN TOO_MANY_ROWS THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Found more than one parameters records for project id ' || vJobPrmRec.gen_project_id );
            WHEN OTHERS THEN
               --   RAISE;
               Error_Desc:= SQLERRM;
               DBMS_OUTPUT.put_line (Error_Desc);

               DBMS_OUTPUT. PUT_LINE (  'Verify Parameter table ' || tblName || ' for project id ' || vProjectID);
         END;

         BEGIN
            tblName := 'LINE_SIM_PARAMETERS';

            EXECUTE IMMEDIATE 'SELECT * FROM LINE_SIM_PARAMETERS
             WHERE  release = :1 AND gen_project_id = :2'
               INTO vLineSimPrmRec
               USING vRelease, vProjectID;
         EXCEPTION
            --WHEN NO_DATA_FOUND THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Parameters not found for project id ' || vJobPrmRec.gen_project_id );
            --WHEN TOO_MANY_ROWS THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Found more than one parameters records for project id ' || vJobPrmRec.gen_project_id );
            WHEN OTHERS THEN
               RAISE;
               Error_Desc:= SQLERRM;
               DBMS_OUTPUT.put_line (Error_Desc);
               DBMS_OUTPUT. PUT_LINE (  'Verify Parameter table ' || tblName || ' for project id ' || vProjectID);
         END;

         BEGIN
            tblName := 'QA_PARAMETERS';

            EXECUTE IMMEDIATE 'SELECT * FROM QA_PARAMETERS
             WHERE release = :1 AND gen_project_id = :2'
               INTO vQAPrmRec
               USING vRelease, vProjectID;
         EXCEPTION
            --WHEN NO_DATA_FOUND THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Parameters not found for project id ' || vJobPrmRec.gen_project_id );
            --WHEN TOO_MANY_ROWS THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Found more than one parameters records for project id ' || vJobPrmRec.gen_project_id );
            WHEN OTHERS THEN
               --   RAISE;
               Error_Desc:= SQLERRM;
               DBMS_OUTPUT.put_line (Error_Desc);
               DBMS_OUTPUT. PUT_LINE ( 'Verify Parameter table ' || tblName || ' for project id ' || vProjectID);
         END;

         BEGIN
            tblName := 'SHAPEFILE_PARAMETERS';

            EXECUTE IMMEDIATE 'SELECT * FROM SHAPEFILE_PARAMETERS
             WHERE  release = :1 AND gen_project_id = :2'
               INTO vShapeFilePrmRec
               USING vRelease, vProjectID;
         EXCEPTION
            --WHEN NO_DATA_FOUND THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Parameters not found for project id ' || vJobPrmRec.gen_project_id );
            --WHEN TOO_MANY_ROWS THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Found more than one parameters records for project id ' || vJobPrmRec.gen_project_id );
            WHEN OTHERS THEN
               --   RAISE;
               Error_Desc:= SQLERRM;
               DBMS_OUTPUT.put_line (Error_Desc);
               DBMS_OUTPUT. PUT_LINE ( 'Verify Parameter table ' || tblName || ' for project id ' || vProjectID);
         END;



         FOR i IN vEntityTableTab.FIRST .. vEntityTableTab.LAST
         LOOP
         
            DBMS_OUTPUT.PUT_LINE ('ENTITY: ' || vEntityTableTab (i).ENTITY);
            vJOBID := vRelease || '_' || vProjectID||'_' ||vEntityTableTab (i).ENTITY || '_'|| vStep;

            -- Turn on QA by default for step 2 jobs
               IF vStep = 2
               THEN
                  vQAdefault := 'Y';
               ELSE
                  vQAdefault := 'N';
               END IF;

            -- Insert into GZ_Job_Setup and GZ_TOPOBUILD_SETUP
            tab_col := 'JOBID, DESCR, RELEASE, gen_project_id, ENTITY, STEP, USE_FOR_MERGE,
                            SRC_TYPE, PRD_TYPE, TOPO_TAG, TOPOBUILD, CLIP, SMPOLY, LINESIM, MERGE,
                            FSLBUILD, CLIP_ATTR, QA, SHAPEFILE, STATE_TABLE, TARGET_SCALE, GEN_CLIP_TABLE';
                            
            SQL1 :=  'INSERT INTO GZ_JOB_SETUP(' || tab_col|| ')  VALUES '
                  || '(:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18, :19, :20, :21, :22)';
                  
            EXECUTE IMMEDIATE SQL1
               USING vJOBID, 'Job record for ' || vJOBID,
                     vRelease, vProjectID,  vEntityTableTab (i).ENTITY,
                     vStep,  'N',
                     vJobPrmRec.BENCH_MARK_PRCS_UNIT,
                     vJobPrmRec.FINAL_TOPO_PRCS_UNIT,
                     vProjectID || vEntityTableTab (i).ENTITY_CODE,
                     vTOPOBUILDTYPE, 'Y', 'Y',  'Y', 'N', vTOPOBUILDTYPE, 'N',  vQAdefault, prcsShapeFiles,
                     vJobPrmRec.STATE_TABLE, vJobPrmRec.TARGET_SCALE, vJobPrmRec.GEN_CLIP_TABLE;

            IF (vTOPOBUILDTYPE = 'A' ) 
            THEN
            
               tab_col := 'JOBID, SOURCE_SCHEMA, SOURCE_TOPOLOGY, '
                       || 'OUTPUT_TOPOLOGY, TILE_COUNT, SDO_FILTER, MODULES, '
                       || 'RESTART_FLAG,  SRID, TOLERANCE, SNAPPING_DIGITS, DROP_TABLES, '
                       || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';
                               
               EXECUTE IMMEDIATE   'Insert into GZ_BUILD_SOURCE_SETUP (' || tab_col|| ')  '
                                || 'VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16)'
                  USING vJOBID, 
                        vJobPrmRec.SOURCE_SCHEMA||vEntityTableTab(i).ENTITY,
                        vJobPrmRec.SOURCE_TOPOLOGY,
                        vProjectID || vEntityTableTab (i).ENTITY_CODE || 'IN',
                        vBuildSourcePrmRec.TILE_COUNT,
                        vBuildSourcePrmRec.SDO_FILTER,
                        vBuildSourcePrmRec.MODULES,
                        vBuildSourcePrmRec.RESTART_FLAG,
                        vBuildSourcePrmRec.SRID,
                        vBuildSourcePrmRec.TOLERANCE,
                        vBuildSourcePrmRec.SNAPPING_DIGITS,
                        vBuildSourcePrmRec.DROP_TABLES,
                        'Y', --validate topo
                        'Y', --topofix edge
                        'N', --topofix_2edge
                        'Y'; --topofix QA, dont expect inputs to break bad
                        
            ELSE
               
               --classic topo build.  Decided its wrong to leave this fossil in here since it 
               --implicitly suggests code is being maintained on various updates

               RAISE_APPLICATION_ERROR(-20001, 'Klassic topo build based on topobuild type ' ||  vTOPOBUILDTYPE
                                               || ' is no longer supported.');
                                              

            END IF;

            -- Clip columns here arent in table order
            tab_col := 'JOBID, GEN_CLIP_MASK, CL_JOBID, TOPO_IN, '
                    || 'TOPO_OUT, DROP_TABLES, TRANSFER_ATTS, GEN_CLIP_MODULES, '
                    || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';

            EXECUTE IMMEDIATE  'Insert into GZ_CLIP_SETUP ('|| tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12)'
               USING vJOBID,
                     vEntityTableTab(i).ENTITY_CODE,
                     vEntityTableTab (i).ENTITY_CODE || 'CL', vTOPO_IN,
                     vProjectID|| vEntityTableTab (i).ENTITY_CODE|| 'CL',
                     'Y',
                     vTRANSFER_ATTS,
                     vClipPrmRec.GEN_CLIP_MODULES,
                     'Y', --validate topo
                     'Y', --topofix_edge
                     'N', --topofix_2edge
                     'N'; --topofix_qa

            -- SMPoly
            tab_col := 'JOBID, SP_MODULES, TOPO_IN, TOPO_OUT, '
                    || 'STATEFP, GEOID_COL, RUNID, CLEANUP, '
                    || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';

            EXECUTE IMMEDIATE   'Insert into GZ_SMPOLY_SETUP ('|| tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12)'
               USING vJOBID,
                     vSMPolyPrmRec.SP_MODULES,
                     vProjectID || vEntityTableTab (i).ENTITY_CODE || 'CL',
                      vProjectID|| vEntityTableTab (i).ENTITY_CODE|| 'SP',
                     vEntityTableTab(i).ENTITY_CODE,
                     'GEOID',
                     1,
                     'N',
                     'Y', --validate topo
                     'Y', --topofix_edge
                     'N', --topofix_2edge
                     'N'; --topofix_qa

            -- LineSim
            tab_col := 'JOBID, LS_MODULES, TOPO_IN, TOPO_OUT, STATEFP, SKIP_EDGES_TABLE, '
                    || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';

            EXECUTE IMMEDIATE   'Insert into GZ_LINESIM_SETUP ('|| tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10)'
               USING vJOBID,
                     vLineSimPrmRec.LS_MODULES,
                     vProjectID || vEntityTableTab (i).ENTITY_CODE || 'SP',
                     vProjectID || vEntityTableTab (i).ENTITY_CODE || 'LS',
                     vEntityTableTab (i).ENTITY_CODE,
                     vLineSimPrmRec.skip_edges_table,
                     'Y', --validate topo
                     'Y', --topofix single edges
                     'Y', --topofix pairs of edges. Reduced coordinates and parallel state jobs are the one spot for this
                     'Y'; --topofix QA. End of the line here, could have shapefiles coming from this topo
                     

            IF (vTOPOBUILDTYPE = 'A' ) THEN
   
               --step1 alternative output build after a step1 state-based topo build
               --This is some far-fetched workflow ish here

               tab_col := 'JOBID, SOURCE_SCHEMA, SOURCE_TOPOLOGY, OUTPUT_TOPOLOGY, '
                       || 'MODULES, RESTART_FLAG, SINGLE_LAYER, SRID, TOLERANCE, DROP_TABLES,TILE_COUNT, '
                       || 'PRCS_SLIVERS, SLIVER_WIDTH, SEGMENT_LENGTH, EXPENDABLE_REVIEW, RESHAPE_REVIEW, '
                       || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA '; 

               IF vEntityTableTab(i).entity_type = 'S'
               THEN

                  IF vOutputPrmRec.SLIVER_PRCS_UNIT IN ('S','B')
                  THEN
                     process_slivers := 'Y';
                  ELSE
                     process_slivers := 'N';
                  END IF;

                  --state tile count
                  EXECUTE IMMEDIATE   'Insert into GZ_OUTPUT_SETUP (' || tab_col|| ')  '
                                   || ' VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20)'
                  USING vJOBID,  
                        vJobPrmRec.SOURCE_SCHEMA||vEntityTableTab(i).ENTITY,
                        vJobPrmRec.SOURCE_TOPOLOGY, 
                        vProjectID || vEntityTableTab (i).ENTITY_CODE || 'IN',
                        vOutputPrmRec.MODULES, 
                        vOutputPrmRec.RESTART_FLAG,
                        vOutputPrmRec.SINGLE_LAYER, 
                        vOutputPrmRec.SRID,
                        vOutputPrmRec.TOLERANCE, 
                        vOutputPrmRec.DROP_TABLES,
                        vOutputPrmRec.TILE_COUNT_STATE,
                        process_slivers,
                        vOutputPrmRec.SLIVER_WIDTH, 
                        vOutputPrmRec.SEGMENT_LENGTH, 
                        'N',  --expendable review
                        'Y',  --reshape review
                        'Y',  --validate topo
                        'Y',  --topofix_edge
                        'N',  --topofix_2edge
                        'Y';  --topofix qa, could lead directly to shapefiles here

               ELSIF vEntityTableTab(i).entity_type = 'N'
               THEN

                  IF vOutputPrmRec.SLIVER_PRCS_UNIT IN ('N','B')
                  THEN
                     process_slivers := 'Y';
                  ELSE
                     process_slivers := 'N';
                  END IF;

                  --nation tile count
                  EXECUTE IMMEDIATE   'Insert into GZ_OUTPUT_SETUP (' || tab_col|| ')  '
                                   || ' VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20)'
                  USING vJOBID,  
                        vJobPrmRec.SOURCE_SCHEMA||vEntityTableTab(i).ENTITY,
                        vJobPrmRec.SOURCE_TOPOLOGY, 
                        vProjectID || vEntityTableTab (i).ENTITY_CODE || 'IN',
                        vOutputPrmRec.MODULES, 
                        vOutputPrmRec.RESTART_FLAG,
                        vOutputPrmRec.SINGLE_LAYER, 
                        vOutputPrmRec.SRID,
                        vOutputPrmRec.TOLERANCE, 
                        vOutputPrmRec.DROP_TABLES,
                        vOutputPrmRec.TILE_COUNT_NATION,
                        process_slivers,
                        vOutputPrmRec.SLIVER_WIDTH, 
                        vOutputPrmRec.SEGMENT_LENGTH, 
                        'N',  --expendable review
                        'Y',  --reshape review
                        'Y',  --validate topo
                        'Y',  --topofix_edge
                        'N',  --topofix_2edge
                        'Y';  --topofix qa, could lead directly to shapefiles here

               ELSE

                  RAISE_APPLICATION_ERROR(-20001,'GZ_ENTITY_TABLE entity ' || vEntityTableTab(i).entity
                                              || ' entity_type is ' || vEntityTableTab(i).entity_type || '. Im confused');

               END IF;

            ELSE
            
               --classic fsl build.  Decided its wrong to leave this fossil in here since it 
               --implicitly suggests code is being maintained on various updates

               RAISE_APPLICATION_ERROR(-20001, 'Klassic FSL build based on topobuild type ' ||  vTOPOBUILDTYPE
                                               || ' is no longer supported.');
                                               
            END IF;
            

            -- QA
            tab_col :='JOBID, GEN_TOPO, GEN_SDOGEOM_COL, UNGEN_SCHEMA,
                            UNGEN_TOPO, UNGEN_SDOGEOM_COL, OUT_TBL_SUFFIX,
                            COMPARE_GEN_COLUMN, AREA_CHECK,
                            SHAPE_CHECK, ENTIRE_TOPO, GEN_TBL_NAME,
                            UNGEN_TBL_NAME, MISSING_REC_TBL,MISSING_REC_KEY,
                            TARGET_SCALE ';

            EXECUTE IMMEDIATE 'Insert into GZ_QA_SETUP (' || tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16)'
               USING vJOBID, 
                     vProjectID || vEntityTableTab (i).ENTITY_CODE || 'LS', 
                     'SDOGEOMETRY',
                     vSchema, 
                     vProjectID || vEntityTableTab (i).ENTITY_CODE || 'CL', 
                     'SDOGEOMETRY', 
                     'QA_TBL',
                     vQAPrmRec.COMPARE_GEN_COLUMN,
                     vQAPrmRec.AREA_CHECK, 
                     vQAPrmRec.SHAPE_CHECK, 
                     'Y',
                     vProjectID|| vEntityTableTab (i).ENTITY_CODE|| 'LS_CLIP_FACE',
                     vProjectID|| vEntityTableTab (i).ENTITY_CODE|| 'CL_CLIP_FACE',
                     vProjectID|| vEntityTableTab (i).ENTITY_CODE|| '_MISSING',
                     'GEO_ID',
                     vJobPrmRec.TARGET_SCALE;

            IF prcsShapeFiles = 'Y' 
            THEN
            
               -- Insert into GZ_SHAPEFILE_SETUP only if SF_PU = TB_PU
               
               BEGIN
               
                  SQL1 :=  'Select * from SHAPEFILE_PARAMETERS ' || ' Where release = :1 AND gen_project_id = :1';

                  EXECUTE IMMEDIATE SQL1 INTO vShapeFilePrmRec USING vRelease, vProjectID;
                  
               EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                     Error_Desc:= SQLERRM;
                     DBMS_OUTPUT.put_line (Error_Desc);

                     --RAISE_APPLICATION_ERROR ( -20001, 'Shapefile_parameters not found for project id ' || vProjectID);
                  WHEN TOO_MANY_ROWS THEN
                     Error_Desc:= SQLERRM;
                     DBMS_OUTPUT.put_line (Error_Desc);

                     --RAISE_APPLICATION_ERROR ( -20001, 'Found more than one Job parameters records for project id ' || vProjectID);
                  WHEN OTHERS THEN
                     Error_Desc:= SQLERRM;
                     DBMS_OUTPUT.put_line (Error_Desc);
                     --RAISE_APPLICATION_ERROR ( -20001, 'There is error at SHAPEFILE_PARAMETERS. ');
                     
               END;

                tab_col := 'JOBID, TOPO_NAME, FSL_STRING, YEAR, GEN_LEVEL,
                               GENCODE, PROJECTION, STATE_TOPO_FLAG,
                               DEL_CPG_FLAG, SHP_UNIT_REQUEST';

               EXECUTE IMMEDIATE   'Insert into GZ_SHAPEFILE_SETUP ('|| tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9, :10)'
                  USING vJOBID,
                        vProjectID || vEntityTableTab (i).ENTITY_CODE || 'IN',
                        vShapeFilePrmRec.FSL_STRING, 
                        vShapeFilePrmRec.YEAR,
                        vShapeFilePrmRec.GEN_LEVEL, 
                        vShapeFilePrmRec.GENCODE,
                        vShapeFilePrmRec.PROJECTION,
                        vShapeFilePrmRec.STATE_TOPO_FLAG,
                        vShapeFilePrmRec.DEL_CPG_FLAG, 
                        vSHP_UNIT_REQUEST;
                        
            END IF;
            
         END LOOP;

      ELSE  --implicitly I think: vJobPrmRec.TOPOBUILD_PRCS_UNIT = 'N' AND vJobPrmRec.GZ_PRCS_UNIT = 'S'

         --Standard step 1 processing
         --Just a couple of ST99-type entities doing topo build, shapefiles, etc

         FOR i IN vEntityTableTab.FIRST .. vEntityTableTab.LAST
         LOOP

            DBMS_OUTPUT.PUT_LINE ('ENTITY: ' || vEntityTableTab (i).ENTITY);
            vJOBID := vRelease || '_' || vProjectID||'_' ||vEntityTableTab (i).ENTITY || '_'|| vStep;

            -- Turn on QA by default for step 2 jobs
            IF vStep = 2
            THEN
               vQAdefault := 'Y';
            ELSE
               vQAdefault := 'N';
            END IF;

            -- Insert into GZ_Job_Setup and GZ_TOPOBUILD_SETUP
            
            tab_col := 'JOBID, DESCR, RELEASE, gen_project_id, ENTITY, STEP, USE_FOR_MERGE,
                            SRC_TYPE, PRD_TYPE, TOPO_TAG, TOPOBUILD, CLIP, SMPOLY, LINESIM,
                            MERGE, FSLBUILD, CLIP_ATTR, QA, SHAPEFILE,STATE_TABLE,TARGET_SCALE,GEN_CLIP_TABLE';
                            
            SQL1 :=  'Insert into GZ_JOB_SETUP(' || tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20,:21, :22)';

            EXECUTE IMMEDIATE SQL1
               USING vJOBID, 
                     'Job record for ' || vJOBID,
                     vRelease, vProjectID,  
                     vEntityTableTab(i).ENTITY,
                     vStep,  
                     'N',
                     vJobPrmRec.BENCH_MARK_PRCS_UNIT,
                     vJobPrmRec.FINAL_TOPO_PRCS_UNIT,
                     vProjectID || vEntityTableTab(i).ENTITY_CODE,
                     vTOPOBUILDTYPE, 
                     'N', 
                     'N',  
                     'N', 
                     'N', 
                     vTOPOBUILDTYPE, 
                     'Y',  
                     vQAdefault, 
                     prcsShapeFiles,
                     vJobPrmRec.STATE_TABLE, 
                     vJobPrmRec.TARGET_SCALE, 
                     vJobPrmRec.GEN_CLIP_TABLE;

            IF (vJobPrmRec.TOPOBUILD_PRCS_UNIT = 'N') 
            OR (vJobPrmRec.TOPOBUILD_PRCS_UNIT = 'B')
            THEN
            
               vTopoSCHEMA:= vJobPrmRec.SOURCE_SCHEMA;
               
            ELSE
            
                vTopoSCHEMA:= vJobPrmRec.SOURCE_SCHEMA||vEntityTableTab (i).ENTITY;
                
            END IF;

            IF (vTOPOBUILDTYPE = 'A' ) 
            THEN
            
               tab_col := 'JOBID, SOURCE_SCHEMA, SOURCE_TOPOLOGY, '
                       || 'OUTPUT_TOPOLOGY, TILE_COUNT, SDO_FILTER, MODULES, '
                       || 'RESTART_FLAG,  SRID, TOLERANCE, SNAPPING_DIGITS, DROP_TABLES, '
                       || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';
                       
               EXECUTE IMMEDIATE   'Insert into GZ_BUILD_SOURCE_SETUP (' || tab_col|| ')  '
                                || 'VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16)'
                  USING vJOBID,  vTopoSCHEMA,
                        vJobPrmRec.SOURCE_TOPOLOGY,
                        vProjectID || vEntityTableTab (i).ENTITY_CODE || 'IN',
                        vBuildSourcePrmRec.TILE_COUNT,
                        vBuildSourcePrmRec.SDO_FILTER,
                        vBuildSourcePrmRec.MODULES,
                        vBuildSourcePrmRec.RESTART_FLAG,
                        vBuildSourcePrmRec.SRID,
                        vBuildSourcePrmRec.TOLERANCE,
                        vBuildSourcePrmRec.SNAPPING_DIGITS,
                        vBuildSourcePrmRec.DROP_TABLES,
                        'Y', --validate_topo
                        'Y', --topofix_edge
                        'N', --topofix_2edge
                        'Y'; --topofix_qa. Dont expect problems on initial build
                        
            ELSE
            
               --classic fsl build.  Decided its wrong to leave this fossil in here since it 
               --implicitly suggests code is being maintained on various updates

               RAISE_APPLICATION_ERROR(-20001, 'Klassic fsl build based on topobuild type ' ||  vTOPOBUILDTYPE
                                               || ' is no longer supported.');

            END IF;

            -- Insert into OUTPUT SETUP Table
            
            IF (vTOPOBUILDTYPE = 'A' ) 
            THEN
            
               tab_col := 'JOBID, SOURCE_SCHEMA, SOURCE_TOPOLOGY, OUTPUT_TOPOLOGY, '
                       || 'MODULES, RESTART_FLAG, SINGLE_LAYER, SRID, TOLERANCE, DROP_TABLES, TILE_COUNT, '
                       || 'PRCS_SLIVERS, SLIVER_WIDTH, SEGMENT_LENGTH, EXPENDABLE_REVIEW, RESHAPE_REVIEW, '
                       || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';

               IF vEntityTableTab(i).entity_type = 'S'   -- oops, dont think this is really possible in this section of the code
               THEN

                  IF vOutputPrmRec.SLIVER_PRCS_UNIT IN ('S','B')
                  THEN
                     process_slivers := 'Y';
                  ELSE
                     process_slivers := 'N';
                  END IF;

                  --state tile count - dont think this is really possible in this section of the code
                  EXECUTE IMMEDIATE   'Insert into GZ_OUTPUT_SETUP (' || tab_col|| ') '
                                   || ' VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20)'
                  USING vJOBID,  
                        vTopoSCHEMA,
                        vJobPrmRec.SOURCE_TOPOLOGY, 
                        vProjectID || vEntityTableTab (i).ENTITY_CODE || 'IN',
                        vOutputPrmRec.MODULES, 
                        vOutputPrmRec.RESTART_FLAG,
                        vOutputPrmRec.SINGLE_LAYER, 
                        vOutputPrmRec.SRID,
                        vOutputPrmRec.TOLERANCE, 
                        vOutputPrmRec.DROP_TABLES,
                        vOutputPrmRec.TILE_COUNT_STATE,
                        process_slivers,
                        vOutputPrmRec.SLIVER_WIDTH, 
                        vOutputPrmRec.SEGMENT_LENGTH,
                        'N', --expendable review
                        'Y', --reshape review
                        'Y', --validate topo
                        'Y', --topofix edge
                        'N', --topofix 2edge
                        'Y'; --topofix qa, could be on the cusp of output shapefiles

               ELSIF vEntityTableTab(i).entity_type = 'N'  --THIS IS THE REAL ONE HERE
               THEN

                  IF vOutputPrmRec.SLIVER_PRCS_UNIT IN ('N','B')
                  THEN
                     process_slivers := 'Y';
                  ELSE
                     process_slivers := 'N';
                  END IF;

                  --nation tile count, standard
                  EXECUTE IMMEDIATE   'Insert into GZ_OUTPUT_SETUP (' || tab_col|| ') '
                                   || ' VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20)'
                  USING vJOBID,  
                        vTopoSCHEMA,
                        vJobPrmRec.SOURCE_TOPOLOGY, 
                        vProjectID || vEntityTableTab(i).ENTITY_CODE || 'IN',
                        vOutputPrmRec.MODULES, 
                        vOutputPrmRec.RESTART_FLAG,
                        vOutputPrmRec.SINGLE_LAYER, 
                        vOutputPrmRec.SRID,
                        vOutputPrmRec.TOLERANCE, 
                        vOutputPrmRec.DROP_TABLES,
                        vOutputPrmRec.TILE_COUNT_NATION,
                        process_slivers,
                        vOutputPrmRec.SLIVER_WIDTH, 
                        vOutputPrmRec.SEGMENT_LENGTH, 
                        'N', --expendable review
                        'Y', --reshape review
                        'Y', --validate topo
                        'Y', --topofix edge
                        'N', --topofix 2edge
                        'Y'; --topofix qa, could be on the cusp of output shapefiles
                        
               ELSE

                  RAISE_APPLICATION_ERROR(-20001,'GZ_ENTITY_TABLE entity ' || vEntityTableTab(i).entity
                                              || ' entity_type is ' || vEntityTableTab(i).entity_type || '. Im confused');

               END IF;

            ELSE
            
               --classic fsl build.  Decided its wrong to leave this fossil in here since it 
               --implicitly suggests code is being maintained on various updates

               RAISE_APPLICATION_ERROR(-20001, 'Klassic fsl build based on topobuild type ' ||  vTOPOBUILDTYPE
                                               || ' is no longer supported.');
                                               
            END IF;

            IF prcsShapeFiles = 'Y' THEN

               BEGIN
                  SQL1 :=  'Select * from SHAPEFILE_PARAMETERS ' || ' Where release = :1 AND gen_project_id = :2';

                  EXECUTE IMMEDIATE SQL1 INTO vShapeFilePrmRec USING vRelease, vProjectID;
               EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                     RAISE_APPLICATION_ERROR ( -20001, 'Shapefile_parameters not found for project id ' || vProjectID);
                     Error_Desc:= SQLERRM;
                     DBMS_OUTPUT.put_line (Error_Desc);
                  WHEN TOO_MANY_ROWS THEN
                     Error_Desc:= SQLERRM;
                     DBMS_OUTPUT.put_line (Error_Desc);
                     RAISE_APPLICATION_ERROR ( -20001, 'Found more than one Job parameters records for project id ' || vProjectID);
                  WHEN OTHERS THEN
                     Error_Desc:= SQLERRM;
                     DBMS_OUTPUT.put_line (Error_Desc);
                     RAISE;
               END;

               tab_col := 'JOBID, TOPO_NAME, FSL_STRING, YEAR, GEN_LEVEL,
                               GENCODE, PROJECTION, STATE_TOPO_FLAG,
                               DEL_CPG_FLAG, SHP_UNIT_REQUEST';

               EXECUTE IMMEDIATE   'Insert into GZ_SHAPEFILE_SETUP ('|| tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9, :10)'
                  USING vJOBID,vProjectID || vEntityTableTab (i).ENTITY_CODE || 'IN',
                        vShapeFilePrmRec.FSL_STRING, vShapeFilePrmRec.YEAR,
                        vShapeFilePrmRec.GEN_LEVEL, vShapeFilePrmRec.GENCODE,
                        vShapeFilePrmRec.PROJECTION,vShapeFilePrmRec.STATE_TOPO_FLAG,
                        vShapeFilePrmRec.DEL_CPG_FLAG, vSHP_UNIT_REQUEST;
            END IF;

            IF prcsClip = 'Y' 
            THEN
            
               --this is transfer_attributes = O(nly) on ST 99 step 1s
               
               -- Insert into GZ_CLIP_SETUP only if BM_PU = N and TB_PU = S  (what does this mean?)
               
               tab_col := 'JOBID, GEN_CLIP_MASK, CL_JOBID, TOPO_OUT, '
                       || 'DROP_TABLES, TRANSFER_ATTS, GEN_CLIP_MODULES, '
                       || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';

               EXECUTE IMMEDIATE   'Insert into GZ_CLIP_SETUP (' || tab_col || ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11)'
                  USING vJOBID,
                        vEntityTableTab(i).ENTITY_CODE,
                        vEntityTableTab(i).ENTITY_CODE || 'CL',
                        vProjectID || vEntityTableTab (i).ENTITY_CODE || 'CL',
                        'Y',
                        'O',
                        vClipPrmRec.GEN_CLIP_MODULES,
                        'N', --validate_topo        clip module always ignores these
                        'N', --topofix_edge         parameters on transfer atts Only
                        'N', --topofix_2edge        just sticking the values in here 
                        'N'; --topofix_qa           for consistency and obviousness
                        
            END IF;
            
         END LOOP;
         
      END IF;


      --------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      -- Step 2 GZ Process
      -- Init variables
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      --------------------------------------------------------------------------

      vEntityTableRec := vEntityTableNullRec;

      SQL1 := NULL;
      vStep := 2;

      IF NOT (vJobPrmRec.TOPOBUILD_PRCS_UNIT = 'S' AND vJobPrmRec.GZ_PRCS_UNIT = 'S') 
      THEN
      
         DBMS_OUTPUT.PUT_LINE ('Processing Step: ' || vStep);

         SQL1 :=  'Select * from GZ_ENTITY_TABLE '
            || 'Where release = :1 '
            || 'And gen_project_id = :2 '
            || 'And Entity_type = :3';

         EXECUTE IMMEDIATE SQL1
            BULK COLLECT INTO vEntityTableTab
            USING vRelease,  vProjectID,
                  vJobPrmRec.GZ_PRCS_UNIT;

         IF vEntityTableTab.COUNT = 0 
         THEN
         
            RAISE_APPLICATION_ERROR ( -20001,  'No GZ_ENTITY_TABLE records found for release ' || vRelease
               || ' project id ' || vProjectID || ' and GZ_Prcs_Unit ' || vJobPrmRec.GZ_PRCS_UNIT);
               
         END IF;

         BEGIN
         
            tblName := 'SMALL_POLYGON_PARAMETERS';

            EXECUTE IMMEDIATE 'Select *
              From SMALL_POLYGON_PARAMETERS
             Where release = :1 AND gen_project_id = :2'
               INTO vSMPolyPrmRec
               USING vRelease, vProjectID;
         EXCEPTION
            --WHEN NO_DATA_FOUND THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Parameters not found for project id ' || vJobPrmRec.gen_project_id );
            --WHEN TOO_MANY_ROWS THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Found more than one parameters records for project id ' || vJobPrmRec.gen_project_id );
            WHEN OTHERS THEN
               --   RAISE;
               Error_Desc:= SQLERRM;
               DBMS_OUTPUT.put_line (Error_Desc);
               DBMS_OUTPUT. PUT_LINE (  'Verify Parameter table ' || tblName || ' for project id ' || vProjectID);
         END;

         BEGIN
         
            tblName := 'LINE_SIM_PARAMETERS';

            EXECUTE IMMEDIATE 'Select *
              From LINE_SIM_PARAMETERS
             Where release = :1 AND gen_project_id = :2 '
               INTO vLineSimPrmRec
               USING vRelease, vProjectID;
         EXCEPTION
            --WHEN NO_DATA_FOUND THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Parameters not found for project id ' || vJobPrmRec.gen_project_id );
            --WHEN TOO_MANY_ROWS THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Found more than one parameters records for project id ' || vJobPrmRec.gen_project_id );
            WHEN OTHERS THEN
               RAISE;
               Error_Desc:= SQLERRM;
               DBMS_OUTPUT.put_line (Error_Desc);
               DBMS_OUTPUT. PUT_LINE (  'Verify Parameter table ' || tblName || ' for project id ' || vProjectID);
         END;

         BEGIN
         
            tblName := 'QA_PARAMETERS';

            EXECUTE IMMEDIATE 'Select *
              From QA_PARAMETERS
             Where release = :1 AND gen_project_id = :2'
               INTO vQAPrmRec
               USING vRelease, vProjectID;
         EXCEPTION
            --WHEN NO_DATA_FOUND THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Parameters not found for project id ' || vJobPrmRec.gen_project_id );
            --WHEN TOO_MANY_ROWS THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Found more than one parameters records for project id ' || vJobPrmRec.gen_project_id );
            WHEN OTHERS THEN
               --   RAISE;
               Error_Desc:= SQLERRM;
               DBMS_OUTPUT.put_line (Error_Desc);
               DBMS_OUTPUT. PUT_LINE ( 'Verify Parameter table ' || tblName || ' for project id ' || vProjectID);
         END;

         BEGIN
         
            tblName := 'SHAPEFILE_PARAMETERS';

            EXECUTE IMMEDIATE 'Select *
              From SHAPEFILE_PARAMETERS
             Where release = :1 AND gen_project_id = :2'
               INTO vShapeFilePrmRec
               USING vRelease, vProjectID;
         EXCEPTION
            --WHEN NO_DATA_FOUND THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Parameters not found for project id ' || vJobPrmRec.gen_project_id );
            --WHEN TOO_MANY_ROWS THEN
            --   RAISE_APPLICATION_ERROR(-20001,'Found more than one parameters records for project id ' || vJobPrmRec.gen_project_id );
            WHEN OTHERS THEN
               --   RAISE;
               Error_Desc:= SQLERRM;
               DBMS_OUTPUT.put_line (Error_Desc);
               DBMS_OUTPUT. PUT_LINE ( 'Verify Parameter table ' || tblName || ' for project id ' || vProjectID);
         END;

         IF vJobPrmRec.SHAPE_FILE_PRCS_UNIT = 'B'  
         THEN
         
            prcsShapeFiles := 'Y';
            vSHP_UNIT_REQUEST := 'STATE';
            
         ELSIF vJobPrmRec.SHAPE_FILE_PRCS_UNIT = 'S'  
         THEN
         
            prcsShapeFiles := 'Y';
            vSHP_UNIT_REQUEST := 'STATE';
            
         ELSE
         
            prcsShapeFiles := 'N';
            
         END IF;

         IF vJobPrmRec.GZ_PRCS_UNIT = 'S' 
         AND vJobPrmRec.FINAL_TOPO_PRCS_UNIT = 'N' 
         THEN
         
            vUSE_FOR_MERGE := 'Y';
            
         ELSE
         
            vUSE_FOR_MERGE := 'N';
            
         END IF;

         FOR i IN vEntityTableTab.FIRST .. vEntityTableTab.LAST
         LOOP
         
            DBMS_OUTPUT.PUT_LINE ('ENTITY: ' || vEntityTableTab (i).ENTITY);
            vJOBID := vRelease || '_' || vProjectID||'_' ||vEntityTableTab (i).ENTITY || '_'|| vStep;

            -- Turn on QA by default for step 2 jobs
            IF vStep = 2
            THEN
               vQAdefault := 'Y';
            ELSE
               vQAdefault := 'N';
            END IF;

            tab_col :=  'JOBID, DESCR, RELEASE, gen_project_id, ENTITY, STEP, DEPENDENCY, USE_FOR_MERGE,
                            SRC_TYPE, PRD_TYPE, TOPO_TAG, TOPOBUILD, CLIP, SMPOLY, LINESIM,
                            MERGE, FSLBUILD, CLIP_ATTR, QA, SHAPEFILE,STATE_TABLE,TARGET_SCALE, GEN_CLIP_TABLE';

            EXECUTE IMMEDIATE 'Insert into GZ_JOB_SETUP (' || tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20,:21,:22,:23)'
               USING vJOBID, 'Job record for ' || vJOBID,
                     vRelease,  vProjectID, vEntityTableTab (i).ENTITY,
                     vStep, 1, vUSE_FOR_MERGE, vJobPrmRec.BENCH_MARK_PRCS_UNIT,
                     vJobPrmRec.FINAL_TOPO_PRCS_UNIT,
                     vProjectID || vEntityTableTab (i).ENTITY_CODE,
                     'N', 'Y', 'Y', 'Y', 'N',  vTOPOBUILDTYPE, 'N', vQAdefault, prcsShapeFiles,
                     vJobPrmRec.STATE_TABLE, vJobPrmRec.TARGET_SCALE, vJobPrmRec.GEN_CLIP_TABLE;

            -- Clip
            IF (vJobPrmRec.TOPOBUILD_PRCS_UNIT = 'N' OR vJobPrmRec.TOPOBUILD_PRCS_UNIT = 'B')
            AND vJobPrmRec.GZ_PRCS_UNIT = 'S' 
            THEN
            
               vTOPO_IN := NULL;
               vTRANSFER_ATTS := 'N';
               
            ELSE
            
               vTOPO_IN := vProjectID || vEntityTableTab (i).ENTITY_CODE || 'IN';
               vTRANSFER_ATTS := 'Y';
               
            END IF;

            --not in table order
            tab_col := 'JOBID, GEN_CLIP_MASK, CL_JOBID, TOPO_IN, '
                    || 'TOPO_OUT, DROP_TABLES, TRANSFER_ATTS, GEN_CLIP_MODULES, '
                    || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';

            EXECUTE IMMEDIATE   'Insert into GZ_CLIP_SETUP ('|| tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12)'
               USING vJOBID,
                     vEntityTableTab(i).ENTITY_CODE,
                     vEntityTableTab(i).ENTITY_CODE || 'CL', vTOPO_IN,
                     vProjectID|| vEntityTableTab (i).ENTITY_CODE|| 'CL',
                     'Y',
                     vTRANSFER_ATTS,
                     vClipPrmRec.GEN_CLIP_MODULES,
                     'Y', --validate topo
                     'Y', --topofix edge
                     'N', --topofix 2edge
                     'N'; --topofix qa

            -- SMPoly
            tab_col := 'JOBID, SP_MODULES, TOPO_IN, TOPO_OUT, '
                    || 'STATEFP, GEOID_COL, RUNID, CLEANUP, '
                    || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';

            EXECUTE IMMEDIATE   'Insert into GZ_SMPOLY_SETUP ('|| tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12)'
               USING vJOBID,
                     vSMPolyPrmRec.SP_MODULES,
                     vProjectID || vEntityTableTab(i).ENTITY_CODE || 'CL',
                     vProjectID|| vEntityTableTab(i).ENTITY_CODE|| 'SP',
                     vEntityTableTab(i).ENTITY_CODE,
                     'GEOID',
                     1,
                     'N',
                     'Y', --validate topo
                     'Y', --topofix edge
                     'N', --topofix 2edge
                     'N'; --topofix qa

            -- LineSim

            tab_col := 'JOBID, LS_MODULES, TOPO_IN, TOPO_OUT, STATEFP, SKIP_EDGES_TABLE, '
                    || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';

            EXECUTE IMMEDIATE   'Insert into GZ_LINESIM_SETUP ('|| tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10)'
               USING vJOBID,
                     vLineSimPrmRec.LS_MODULES,
                     vProjectID || vEntityTableTab (i).ENTITY_CODE || 'SP',
                     vProjectID || vEntityTableTab (i).ENTITY_CODE || 'LS',
                     vEntityTableTab(i).ENTITY_CODE,
                     vLineSimPrmRec.skip_edges_table,
                     'Y', --validate topo
                     'Y', --topofix single edges
                     'Y', --topofix pairs of edges. Reduced coordinates and parallel state jobs are the one spot for this
                     'Y'; --topofix QA. End of the line here, could have shapefiles coming from this topo

            -- Insert into OUTPUT SETUP Table
            IF (vTOPOBUILDTYPE = 'A' ) THEN
               tab_col := 'JOBID, SOURCE_SCHEMA, SOURCE_TOPOLOGY, OUTPUT_TOPOLOGY, '
                       || 'MODULES, RESTART_FLAG, SINGLE_LAYER, SRID, TOLERANCE, DROP_TABLES,TILE_COUNT, '
                       || 'PRCS_SLIVERS, SLIVER_WIDTH, SEGMENT_LENGTH, EXPENDABLE_REVIEW, RESHAPE_REVIEW, '
                       || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';                                

               IF vEntityTableTab(i).entity_type = 'S'
               THEN

                  IF vOutputPrmRec.SLIVER_PRCS_UNIT IN ('S','B')
                  THEN
                     process_slivers := 'Y';
                  ELSE
                     process_slivers := 'N';
                  END IF;

                  --state tile count, SOP
                  
                  EXECUTE IMMEDIATE   'Insert into GZ_OUTPUT_SETUP (' || tab_col|| ')  '
                                   || ' VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20)'
                  USING vJOBID,  
                        vTopoSCHEMA, 
                        vJobPrmRec.SOURCE_TOPOLOGY,
                        vProjectID || vEntityTableTab(i).ENTITY_CODE || 'LS',
                        vOutputPrmRec.MODULES, 
                        vOutputPrmRec.RESTART_FLAG,
                        vOutputPrmRec.SINGLE_LAYER, 
                        vOutputPrmRec.SRID,
                        vOutputPrmRec.TOLERANCE, 
                        vOutputPrmRec.DROP_TABLES,
                        vOutputPrmRec.TILE_COUNT_STATE,  --here
                        process_slivers,
                        vOutputPrmRec.SLIVER_WIDTH, 
                        vOutputPrmRec.SEGMENT_LENGTH,
                        'N', --expendable review
                        'Y', --reshape review
                        'Y', --validate topo
                        'Y', --topofix edge
                        'N', --topofix 2edge
                        'Y'; --topofix qa, could be on the cusp of output shapefiles                        

               ELSIF vEntityTableTab(i).entity_type = 'N'
               THEN

                  IF vOutputPrmRec.SLIVER_PRCS_UNIT IN ('N','B')
                  THEN
                     process_slivers := 'Y';
                  ELSE
                     process_slivers := 'N';
                  END IF;

                  --nation tile count, pretty much always NA here in step 2 inserts
                  EXECUTE IMMEDIATE   'Insert into GZ_OUTPUT_SETUP (' || tab_col|| ')  '
                                   || ' VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20)'
                  USING vJOBID,  
                        vTopoSCHEMA, 
                        vJobPrmRec.SOURCE_TOPOLOGY,
                        vProjectID || vEntityTableTab(i).ENTITY_CODE || 'LS',
                        vOutputPrmRec.MODULES, 
                        vOutputPrmRec.RESTART_FLAG,
                        vOutputPrmRec.SINGLE_LAYER, 
                        vOutputPrmRec.SRID,
                        vOutputPrmRec.TOLERANCE, 
                        vOutputPrmRec.DROP_TABLES,
                        vOutputPrmRec.TILE_COUNT_NATION,  --here
                        process_slivers,
                        vOutputPrmRec.SLIVER_WIDTH, 
                        vOutputPrmRec.SEGMENT_LENGTH,
                        'N', --expendable review
                        'Y', --reshape review
                        'Y', --validate topo
                        'Y', --topofix edge
                        'N', --topofix 2edge
                        'Y'; --topofix qa, could be on the cusp of output shapefiles                 

               ELSE

                  RAISE_APPLICATION_ERROR(-20001,'GZ_ENTITY_TABLE entity ' || vEntityTableTab(i).entity
                                              || ' entity_type is ' || vEntityTableTab(i).entity_type || '. Im confused');

               END IF;

               --why this here?
               tab_col := 'JOBID, SOURCE_SCHEMA, SOURCE_TOPOLOGY, OUTPUT_TOPOLOGY,
                                MODULES, RESTART_FLAG, SINGLE_LAYER, SRID, TOLERANCE, DROP_TABLES,TILE_COUNT';

            ELSE
            
               --classic fsl build.  Decided its wrong to leave this fossil in here since it 
               --implicitly suggests code is being maintained on various updates

               RAISE_APPLICATION_ERROR(-20001, 'Klassic fsl build based on topobuild type ' ||  vTOPOBUILDTYPE
                                               || ' is no longer supported.');
                                               
            END IF;

            -- QA
            tab_col :='JOBID, GEN_TOPO, GEN_SDOGEOM_COL, UNGEN_SCHEMA,
                            UNGEN_TOPO, UNGEN_SDOGEOM_COL, OUT_TBL_SUFFIX,
                            COMPARE_GEN_COLUMN, AREA_CHECK,
                            SHAPE_CHECK, ENTIRE_TOPO, GEN_TBL_NAME,
                            UNGEN_TBL_NAME,MISSING_REC_TBL,MISSING_REC_KEY,TARGET_SCALE ';

            EXECUTE IMMEDIATE 'Insert into GZ_QA_SETUP (' || tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16)'
               USING vJOBID, vProjectID || vEntityTableTab (i).ENTITY_CODE || 'LS', 'SDOGEOMETRY',
                     vSchema, vProjectID || vEntityTableTab (i).ENTITY_CODE || 'CL', 'SDOGEOMETRY', 'QA_TBL',
                     vQAPrmRec.COMPARE_GEN_COLUMN, vQAPrmRec.AREA_CHECK,
                     vQAPrmRec.SHAPE_CHECK, 'Y',
                     vProjectID|| vEntityTableTab (i).ENTITY_CODE|| 'LS_CLIP_FACE',
                     vProjectID|| vEntityTableTab (i).ENTITY_CODE|| 'CL_CLIP_FACE',
                     vProjectID|| vEntityTableTab (i).ENTITY_CODE|| '_MISSING','GEO_ID',
                     vJobPrmRec.TARGET_SCALE;

            -- SHP
            IF prcsShapeFiles = 'Y' THEN
               tab_col := 'JOBID, UNGEN_TOPO_NAME, TOPO_NAME, QA_STRING,
                               FSL_STRING, YEAR, GEN_LEVEL,
                               GENCODE, PROJECTION, QA_UNGEN_OUT_DIR, QA_OUT_DIR,
                               STATE_TOPO_FLAG, DEL_CPG_FLAG, SHP_UNIT_REQUEST';

               EXECUTE IMMEDIATE 'Insert into GZ_SHAPEFILE_SETUP (' || tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13, :14)'
                  USING vJOBID, vProjectID || vEntityTableTab (i).ENTITY_CODE || 'CL',  vProjectID || vEntityTableTab (i).ENTITY_CODE || 'LS',
                        vShapeFilePrmRec.QA_STRING, vShapeFilePrmRec.FSL_STRING,
                        vShapeFilePrmRec.YEAR,  vShapeFilePrmRec.GEN_LEVEL,
                        vShapeFilePrmRec.GENCODE, vShapeFilePrmRec.PROJECTION,
                        vShapeFilePrmRec.QA_UNGEN_OUT_DIR,  vShapeFilePrmRec.QA_OUT_DIR,
                        vShapeFilePrmRec.STATE_TOPO_FLAG, vShapeFilePrmRec.DEL_CPG_FLAG,
                        vSHP_UNIT_REQUEST;
            END IF;
            
         END LOOP;
         
      END IF;

      --------------------------------------------------------------------------
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      -- Step 3 Merge and final output + SHP 
      -- Depends on
      -- GZ_PRCS_UNIT => FINAL_TOPO_PRCS_UNIT and SHAPE_FILE_PRCS_UNIT (Could be S => N)
      -- Only Case: S => N
      --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
      --------------------------------------------------------------------------

      -- Insert into GZ_JOB_SETUP (MRG, FSL, QA, decode(SHAPE_FILE_PRCS_UNIT) for SHP)

      SQL1 := 'Select * from GZ_ENTITY_TABLE '
         || 'Where release = :1 '
         || 'And gen_project_id = :2 '
         || 'And Entity_type = :3';

      EXECUTE IMMEDIATE SQL1
         BULK COLLECT INTO vEntityTableTab
         USING vRelease, vProjectID,
               vJobPrmRec.FINAL_TOPO_PRCS_UNIT;

      IF vEntityTableTab.COUNT = 1
         AND (vJobPrmRec.FINAL_TOPO_PRCS_UNIT = 'N'
              OR vJobPrmRec.FINAL_TOPO_PRCS_UNIT = 'B')
         AND vJobPrmRec.GZ_PRCS_UNIT = 'S'
      THEN

         IF vJobPrmRec.SHAPE_FILE_PRCS_UNIT = 'B'  THEN
            prcsShapeFiles := 'Y';
            vSHP_UNIT_REQUEST := 'NATION';
         ELSIF vJobPrmRec.SHAPE_FILE_PRCS_UNIT = 'N'  THEN
            prcsShapeFiles := 'Y';
            vSHP_UNIT_REQUEST := 'BOTH';
         ELSIF vJobPrmRec.SHAPE_FILE_PRCS_UNIT = 'S'  THEN
            prcsShapeFiles := 'Y';
            vSHP_UNIT_REQUEST := 'STATE';
         ELSE
            prcsShapeFiles := 'N';
         END IF;

         DBMS_OUTPUT.PUT_LINE ('ENTITY: ' || vEntityTableTab (1).ENTITY);


         SQL1 := NULL;
         vStep := 3;
         vJOBID := vRelease|| '_' || vProjectID|| '_'|| vEntityTableTab (1).ENTITY|| '_'|| vStep;
         tab_col := 'JOBID, DESCR, RELEASE, gen_project_id, ENTITY, STEP, DEPENDENCY, USE_FOR_MERGE,
                         SRC_TYPE, PRD_TYPE, TOPO_TAG, TOPOBUILD, CLIP, SMPOLY, LINESIM,
                         MERGE, FSLBUILD, CLIP_ATTR, QA, SHAPEFILE,STATE_TABLE,TARGET_SCALE,GEN_CLIP_TABLE ';

         EXECUTE IMMEDIATE 'Insert into GZ_JOB_SETUP (' || tab_col || ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20,:21,:22, :23)'
            USING vJOBID, 'Job record for ' || vJOBID, vRelease,
                  vProjectID, 'ST99', vStep, 2, 'N', vJobPrmRec.TOPOBUILD_PRCS_UNIT,
                  vJobPrmRec.FINAL_TOPO_PRCS_UNIT,
                  vProjectID || vEntityTableTab (1).ENTITY_CODE,
                  'N', 'N', 'N', 'N', 'Y',  vTOPOBUILDTYPE, 'N', 'N',  'Y',
                     vJobPrmRec.STATE_TABLE, vJobPrmRec.TARGET_SCALE, vJobPrmRec.GEN_CLIP_TABLE;

         -- Merge
         BEGIN
            EXECUTE IMMEDIATE 'Select GEN_MERGE_TOP_LAYER, GEN_MERGE_FACE_OUT
              From GEN_MERGE_PARAMETERS
             Where release = :1 AND gen_project_id = :2'
               INTO vTOP_LAYER_TABLE, vFACE_OUT_TBL_EXT
               USING vRelease, vProjectID;
         EXCEPTION
            WHEN OTHERS THEN
               Error_Desc:= SQLERRM;
               DBMS_OUTPUT.put_line (Error_Desc);
               DBMS_OUTPUT. PUT_LINE (  'Using default values of FSL040 and MERGE_FACE instead');
               vTOP_LAYER_TABLE := 'FSL040';
               vFACE_OUT_TBL_EXT := 'MERGE_FACE';
         END;

         tab_col := 'JOBID, TOPO_IN_TBL, TOPO_OUT, TOP_LAYER_TABLE, '
                 || 'FACE_OUT_TBL_EXT, MRG_MODULES, RESTART, '
                 || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';

         EXECUTE IMMEDIATE   'Insert into GZ_MERGE_SETUP ('|| tab_col|| ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11)'
            USING vJOBID,
                  vProjectID || '_MERGE_TOPO_IN',
                  vProjectID || vEntityTableTab (1).ENTITY_CODE || 'TM',
                  vTOP_LAYER_TABLE,
                  vFACE_OUT_TBL_EXT,
                  'YYYYYYYYYY',
                  'N', --restart flag
                  'Y', --validate topo
                  'Y', --topofix edge
                  'N', --topofix 2edge
                  'Y'; --topofix QA

         -- Insert into FSL SETUP Table
         IF (vTOPOBUILDTYPE = 'A' ) THEN
            tab_col := 'JOBID, SOURCE_SCHEMA, SOURCE_TOPOLOGY, OUTPUT_TOPOLOGY, '
                    || 'MODULES, RESTART_FLAG, SINGLE_LAYER, SRID, TOLERANCE, DROP_TABLES,TILE_COUNT, '
                    || 'PRCS_SLIVERS, SLIVER_WIDTH, SEGMENT_LENGTH, EXPENDABLE_REVIEW, RESHAPE_REVIEW, '
                    || 'VALIDATE_TOPO, TOPOFIX_EDGE, TOPOFIX_2EDGE, TOPOFIX_QA ';


            IF vJobPrmRec.FINAL_TOPO_PRCS_UNIT = 'S'  --pretty rare. Possibly extinct
            THEN

               IF vOutputPrmRec.SLIVER_PRCS_UNIT IN ('S','B')
               THEN
                  process_slivers := 'Y';
               ELSE
                  process_slivers := 'N';
               END IF;

               --state tile count
               EXECUTE IMMEDIATE   'Insert into GZ_OUTPUT_SETUP (' || tab_col|| ')  '
                                || 'VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20)'
               USING vJOBID,  
                     vJobPrmRec.SOURCE_SCHEMA, 
                     vJobPrmRec.SOURCE_TOPOLOGY,
                     vProjectID || vEntityTableTab (1).ENTITY_CODE || 'TM',
                     vOutputPrmRec.MODULES, 
                     vOutputPrmRec.RESTART_FLAG,
                     vOutputPrmRec.SINGLE_LAYER, 
                     vOutputPrmRec.SRID,
                     vOutputPrmRec.TOLERANCE, 
                     vOutputPrmRec.DROP_TABLES,
                     vOutputPrmRec.TILE_COUNT_STATE,
                     process_slivers,
                     vOutputPrmRec.SLIVER_WIDTH, 
                     vOutputPrmRec.SEGMENT_LENGTH,
                     'N', --expendable review
                     'Y', --reshape review
                     'Y', --validate topo
                     'Y', --topofix edge
                     'N', --topofix 2edge
                     'Y'; --topofix qa

            ELSIF vJobPrmRec.FINAL_TOPO_PRCS_UNIT = 'N'  --SOP
            THEN

               IF vOutputPrmRec.SLIVER_PRCS_UNIT IN ('N','B')
               THEN
                  process_slivers := 'Y';
               ELSE
                  process_slivers := 'N';
               END IF;

               --nation tile count
               EXECUTE IMMEDIATE   'Insert into GZ_OUTPUT_SETUP (' || tab_col|| ')  '
                                || 'VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20)'
               USING vJOBID,  
                     vJobPrmRec.SOURCE_SCHEMA, 
                     vJobPrmRec.SOURCE_TOPOLOGY,
                     vProjectID || vEntityTableTab(1).ENTITY_CODE || 'TM',
                     vOutputPrmRec.MODULES, 
                     vOutputPrmRec.RESTART_FLAG,
                     vOutputPrmRec.SINGLE_LAYER, 
                     vOutputPrmRec.SRID,
                     vOutputPrmRec.TOLERANCE, 
                     vOutputPrmRec.DROP_TABLES,
                     vOutputPrmRec.TILE_COUNT_NATION,
                     process_slivers,
                     vOutputPrmRec.SLIVER_WIDTH, 
                     vOutputPrmRec.SEGMENT_LENGTH,
                     'N', --expendable review
                     'Y', --reshape review
                     'Y', --validate topo
                     'Y', --topofix edge
                     'N', --topofix 2edge
                     'Y'; --topofix qa
                     
            ELSE

               RAISE_APPLICATION_ERROR(-20001,'GZ_JOB_PARAMETERS entity FINAL_TOPO_PRCS_UNIT is ' ||vJobPrmRec.FINAL_TOPO_PRCS_UNIT || '?');

            END IF;

         ELSE

            --classic fsl build.  Decided its wrong to leave this fossil in here since it 
            --implicitly suggests code is being maintained on various updates

             RAISE_APPLICATION_ERROR(-20001, 'Klassic fsl build based on topobuild type ' ||  vTOPOBUILDTYPE
                                          || ' is no longer supported.');
                                               
         END IF;


         -- QA
         tab_col := 'JOBID, GEN_TOPO, GEN_SDOGEOM_COL, UNGEN_SCHEMA,
                         UNGEN_TOPO, UNGEN_SDOGEOM_COL, OUT_TBL_SUFFIX,
                         COMPARE_GEN_COLUMN, AREA_CHECK,
                         SHAPE_CHECK, ENTIRE_TOPO, GEN_TBL_NAME,
                         UNGEN_TBL_NAME,MISSING_REC_TBL,MISSING_REC_KEY,TARGET_SCALE ';

         EXECUTE IMMEDIATE 'Insert into GZ_QA_SETUP (' || tab_col || ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9, :10,:11,:12,:13,:14,:15,:16)'
            USING vJOBID, vProjectID || vEntityTableTab (1).ENTITY_CODE || 'TM', 'SDOGEOMETRY',  vSchema,
                  vProjectID || vEntityTableTab (1).ENTITY_CODE || 'CL',
                  'SDOGEOMETRY', 'QA_TBL', vQAPrmRec.COMPARE_GEN_COLUMN,
                  vQAPrmRec.AREA_CHECK, vQAPrmRec.SHAPE_CHECK, 'Y',
                  vProjectID|| vEntityTableTab (1).ENTITY_CODE|| 'LS_CLIP_FACE',
                  vProjectID|| vEntityTableTab (1).ENTITY_CODE|| 'CL_CLIP_FACE',
                  vProjectID || vEntityTableTab (1).ENTITY_CODE || '_MISSING',
                  'GEO_ID', vJobPrmRec.TARGET_SCALE;

         -- SHP
         IF prcsShapeFiles = 'Y' THEN

            tab_col := 'JOBID, TOPO_NAME, QA_STRING,  FSL_STRING, YEAR, GEN_LEVEL,
                         GENCODE, PROJECTION, QA_UNGEN_OUT_DIR, QA_OUT_DIR,
                         STATE_TOPO_FLAG, DEL_CPG_FLAG, SHP_UNIT_REQUEST';

            EXECUTE IMMEDIATE 'Insert into GZ_SHAPEFILE_SETUP (' || tab_col
                             || ')  VALUES (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12, :13)'
               USING vJOBID, vProjectID || vEntityTableTab (1).ENTITY_CODE || 'TM', vShapeFilePrmRec.QA_STRING,
                     vShapeFilePrmRec.FSL_STRING, vShapeFilePrmRec.YEAR,
                     vShapeFilePrmRec.GEN_LEVEL,  vShapeFilePrmRec.GENCODE,
                     vShapeFilePrmRec.PROJECTION,  vShapeFilePrmRec.QA_UNGEN_OUT_DIR,
                     vShapeFilePrmRec.QA_OUT_DIR, vShapeFilePrmRec.STATE_TOPO_FLAG,
                     vShapeFilePrmRec.DEL_CPG_FLAG, vSHP_UNIT_REQUEST;
         END IF;
      ELSE
         DBMS_OUTPUT.PUT_LINE ('Skipping Step 3. vEntityTableTab.Count: ' || vEntityTableTab.COUNT);
      END IF;

      vEntityTableTab.Delete;
      COMMIT;


   END Insert_New_jobs;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

   PROCEDURE DELETE_NEW_JOBS (
      p_release          IN VARCHAR2,
      p_gen_project_id   IN VARCHAR2
   )
   AS

      --Matt! 6/12/13
      --This is poorly thought out
      --Assumes all jobs are defined like <release>_<project id>_xxxx
      --And it just gets setup table names from the gz_types hard coded list

      --example
      -- begin
      --    GZ_WORKFLOW.DELETE_NEW_JOBS('ACS122','Z6');
      -- end;

      all_tabs          GZ_TYPES.stringarray;
      psql              VARCHAR2(4000);
      bad_deletes       VARCHAR2(4000);

   BEGIN

      all_tabs := GZ_TYPES.LEGAL_GZ_TABLES();

      FOR i IN 1 .. all_tabs.COUNT
      LOOP

         IF all_tabs(i) LIKE ('%SETUP')
         THEN

            psql := 'DELETE FROM ' || all_tabs(i) || ' a '
                 || 'WHERE a.jobid LIKE :p1 ';

            --utilities + verbose = 4 the best
            dbms_output.put_line(psql || chr(10) || UPPER(p_release) || '_' || UPPER(p_gen_project_id) || '_%');

            BEGIN

               EXECUTE IMMEDIATE psql USING UPPER(p_release) || '_' || UPPER(p_gen_project_id) || '_%';
               COMMIT;

            EXCEPTION
            WHEN OTHERS
            THEN

               IF SQLCODE = -942 --ORA-00942: table or view does not exist
               THEN

                  bad_deletes := bad_deletes || '| ' || all_tabs(i) || ' doesnt exist!';

               ELSE

                  RAISE_APPLICATION_ERROR(-20001, SQLERRM || ' on ' || psql || ' using '
                                                  || UPPER(p_release) || '_' || UPPER(p_gen_project_id) || '_%');

               END IF;

            END;

         END IF;

         IF LENGTH(bad_deletes) > 0
         THEN

            RAISE_APPLICATION_ERROR(-20001, 'Deleted as much as possible but... ' || bad_deletes);

         END IF;

      END LOOP;

   END DELETE_NEW_JOBS;

   -----------------------------------------------------------------------------------------
   --+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   -----------------------------------------------------------------------------------------

  PROCEDURE Validate_Job(pJobID IN VARCHAR2)
    AS

  BEGIN

     NULL;

  END;

  PROCEDURE Delete_Job(pJobID IN VARCHAR2)
    AS

  BEGIN

     NULL;

  END;

  PROCEDURE Get_Status(pRel IN VARCHAR2, pPrj IN VARCHAR2)
    AS

  BEGIN

     NULL;

  END;

  PROCEDURE Get_Status(pJobID IN VARCHAR2)
    AS

  BEGIN

     NULL;

  END;

  PROCEDURE Copy_Project(pSrc_Prj IN VARCHAR2, pTgt_Prj IN VARCHAR2)  AS

  BEGIN

     NULL;

  END;

  PROCEDURE Copy_Job_Prm(pSrc_Rel IN VARCHAR2, pSrc_Prj IN VARCHAR2, pTgt_Rel IN VARCHAR2, pTgt_Prj IN VARCHAR2)
    AS

  BEGIN

     NULL;

  END;

  PROCEDURE Create_Empty_Prm_Tbls  AS
     PRM_TBLS MDSYS.SDO_STRING_ARRAY := MDSYS.SDO_STRING_ARRAY();

     --Reminder to future selves
     --The standard (to me) method for creating GZ parameter tables is confusingly in two parts
     --Best entry point is gz_business_utils.CREATE_GZ_TABLES
     --   It will first create all parameter tables that have standalone procedures like create_gen_clip_parameters
     --   Then it will call THIS HERE PROCEDURE to create more.  There are overlaps between the two lists
     --   However, THIS HERE PROCEDURE calls GZ_WORKFLOW.CREATE_TABLE which won't drop an existing table
     --   So the standalone procedures trump anything in here.
     --   Also, THIS HERE PROCEDURE is nice because it adds some universal support objects (triggers, keys) to all parameter tables,
     --     regardless of whether the table already exists, and whether the procedure coded back in the day is aware of this stuff
     --   This is all rather annoying and confusing and should be tidied some day

  BEGIN
     PRM_TBLS.extend(13);
     PRM_TBLS(1) := 'GZ_JOB_PARAMETERS';
     PRM_TBLS(2) := 'GEN_CLIP_PARAMETERS';
     PRM_TBLS(3) := 'SMALL_POLYGON_PARAMETERS';
     PRM_TBLS(4) := 'LINE_SIM_PARAMETERS';
     PRM_TBLS(5) := 'FSLBUILD_PARAMETERS';
     PRM_TBLS(6) := 'QA_PARAMETERS';
     PRM_TBLS(7) := 'GEN_TOPO_MERGE_PARAMETERS';
     PRM_TBLS(8) := 'GZ_SHAPEFILE_PARAMETERS';
     PRM_TBLS(9) := 'SHAPEFILE_PARAMETERS';
     PRM_TBLS(10) := 'GZ_BUILD_SOURCE_PARAMETERS';
     PRM_TBLS(11) := 'GZ_ENTITY_TABLE';
     PRM_TBLS(12) := 'GZ_LAYERS_IN';
     PRM_TBLS(13) := 'GZ_OUTPUT_PARAMETERS';

     FOR i in 1..PRM_TBLS.LAST LOOP

        -- dbms_output.put_line(user || '.' || PRM_TBLS(i) || ' GZ_TYPES.' || PRM_TBLS(i));

        GZ_WORKFLOW.CREATE_TABLE(USER, PRM_TBLS(i), 'GZ_TYPES.' || PRM_TBLS(i) , 'N');
        GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS', PRM_TBLS(i));

        --Matt is a control freak on all parameter and setup tables
        --This sub will add, for now, just primary keys and triggers
        --Note the tricksy plan to run this support stuff even on existing tables.  Existing tables
        --   get ignored in the create_table code just above

        GZ_WORKFLOW.CREATE_PRM_TABLE_SUPPORT(PRM_TBLS(i));


     END LOOP;

     PRM_TBLS.DELETE;
  END Create_Empty_Prm_Tbls;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE CREATE_SETUP_TABLE_SUPPORT (
      setup_tbl            IN VARCHAR2
   )
   AS

      --Matt! 12/17/12
      --! 6/27/13 made user and date last modified trigger on all setup tables
      --Control freak attempt to lock down the setup tables


      key_list       VARCHAR2(4000);

   BEGIN

      --Primary keys are always best practice yo
      --Why did our sunbaes not include them?

      IF setup_tbl = 'GZ_PRE_SETUP'
      THEN

         --pre table is a little different, can have multiple steps per jobid
         key_list := 'JOBID, STEP';

      ELSE

         key_list := 'JOBID';

      END IF;


      BEGIN

         EXECUTE IMMEDIATE 'ALTER TABLE ' || setup_tbl || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || setup_tbl || 'PKC '
              || '      PRIMARY KEY(' || key_list || ') '
              || ')';

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -2260
         THEN

            --ORA-02260: table can have only one primary key (get this when pkc already exists)
            --is like rubber to us
            NULL;

         ELSE

            RAISE_APPLICATION_ERROR(-20001,SQLERRM);

         END IF;

      END;


      --all tables get user last modified and date last modified

      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || setup_tbl || 'TRG '
                  || 'BEFORE INSERT OR UPDATE ON ' || setup_tbl || ' '
                  || 'FOR EACH ROW '
                  || 'BEGIN '
                  || '   :NEW.date_last_modified := CURRENT_DATE; '
                  || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                  || 'END;';



   END CREATE_SETUP_TABLE_SUPPORT;

    ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE CREATE_PRM_TABLE_SUPPORT (
      prm_tbl              IN VARCHAR2
   )
   AS

      --Matt! 6/27/13
      --Control freak attempt to lock down the "parameter" tables

      --all parameter tables have at least these two columns to make composite pkc
      key_list       VARCHAR2(4000) := 'RELEASE, GEN_PROJECT_ID';
      xtended_keys   GZ_TYPES.stringhash;

   BEGIN

      --some tables have an additional column
      xtended_keys := GZ_TYPES.LEGAL_GZ_XTENDED_KEYS();

      IF xtended_keys.EXISTS(prm_tbl)
      THEN

         key_list := key_list || ',' || xtended_keys(prm_tbl);

      END IF;

      --Primary keys are best practice yo

      BEGIN

         EXECUTE IMMEDIATE 'ALTER TABLE ' || prm_tbl || ' '
              || 'ADD ('
              || '   CONSTRAINT ' || prm_tbl || 'PKC '
              || '      PRIMARY KEY(' || key_list || ') '
              || ')';

      EXCEPTION
      WHEN OTHERS
      THEN

         IF SQLCODE = -2260
         THEN

            --ORA-02260: table can have only one primary key (get this when pkc already exists)
            --is like rubber to us
            NULL;

         ELSE

            RAISE_APPLICATION_ERROR(-20001,SQLERRM);

         END IF;

      END;


      --everyone gets this
      EXECUTE IMMEDIATE 'CREATE OR REPLACE TRIGGER ' || prm_tbl || 'TRG '
                     || 'BEFORE INSERT OR UPDATE ON ' || prm_tbl || ' '
                     || 'FOR EACH ROW '
                     || 'BEGIN '
                     || '   :NEW.date_last_modified := CURRENT_DATE; '
                     || '   :NEW.user_last_modified := SUBSTR(USER,1,32); '
                     || 'END;';


   END CREATE_PRM_TABLE_SUPPORT;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   PROCEDURE Create_Empty_Setup_Tbls
   AS

      --Matt! 12/17/12  Added call to new setup table support procedure

      --In my typical usage create_empty_setup_tbls is called
      --   from CREATE_GZ_TABLES, which is called from gz_build.pl

      SETUP_TBLS MDSYS.SDO_STRING_ARRAY := MDSYS.SDO_STRING_ARRAY();


   BEGIN

      SETUP_TBLS.extend(12);
      SETUP_TBLS(1) := 'GZ_JOB_SETUP';
      SETUP_TBLS(2) := 'GZ_TOPOBUILD_SETUP';
      SETUP_TBLS(3) := 'GZ_CLIP_SETUP';
      SETUP_TBLS(4) := 'GZ_SMPOLY_SETUP';
      SETUP_TBLS(5) := 'GZ_LINESIM_SETUP';
      SETUP_TBLS(6) := 'GZ_MERGE_SETUP';
      SETUP_TBLS(7) := 'GZ_FSLBUILD_SETUP';
      SETUP_TBLS(8) := 'GZ_QA_SETUP';
      SETUP_TBLS(9) := 'GZ_SHAPEFILE_SETUP';
      SETUP_TBLS(10) := 'GZ_PRE_SETUP';
      SETUP_TBLS(11) := 'GZ_BUILD_SOURCE_SETUP';
      SETUP_TBLS(12) := 'GZ_OUTPUT_SETUP';

      FOR i in 1..SETUP_TBLS.LAST LOOP
         -- dbms_output.put_line(user || '.' || PRM_TBLS(i) || ' GZ_TYPES.' || PRM_TBLS(i));

         GZ_WORKFLOW.CREATE_TABLE(USER, SETUP_TBLS(i), 'GZ_TYPES.' || SETUP_TBLS(i) , 'N');
         GZ_BUSINESS_UTILS.GZ_PRIV_GRANTER('REFERENCE_SCHEMAS', SETUP_TBLS(i));

         --Matt is a control freak on user input tables
         --This sub will add primary keys, indexes, and other control freak stuff as currently whimmed
         --Note the tricksy plan to run this support stuff even on existing tables.  Existing tables
         --   get ignored in the create_table code just above

         GZ_WORKFLOW.CREATE_SETUP_TABLE_SUPPORT(SETUP_TBLS(i));

      END LOOP;


      SETUP_TBLS.DELETE;

   END Create_Empty_Setup_Tbls;

   ------------------------------------------------------------------------------------
   --++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++--
   ------------------------------------------------------------------------------------

   FUNCTION GET_SHAPEFILE_PRCS_STR (pJobID IN VARCHAR2) RETURN VARCHAR2
   AS
      SF_STATUS            VARCHAR2 (1);
      RELEASE              VARCHAR2 (64);
      GEN_PROJECT_ID      VARCHAR2 (2);
      STEP                  NUMBER;
      IS_QA                  VARCHAR2 (1);
      SQL1                 VARCHAR2 (1000);
      SHAPEFILE_PRCS_STR   VARCHAR2 (1000);
      pQA_UNGEN_OUT_DIR    VARCHAR2 (200);
      pQA_OUT_DIR          VARCHAR2 (200);
   BEGIN
      -- 1. Check to see if we are ready to create shapefiles
      SQL1 := ' Select SHAPEFILE_STATUS, QA, RELEASE, GEN_PROJECT_ID, STEP From GZ_JOB_SETUP Where jobid = :1';
      EXECUTE IMMEDIATE SQL1 INTO SF_STATUS, IS_QA, RELEASE, GEN_PROJECT_ID, STEP USING pJobID;
      DBMS_OUTPUT.PUT_LINE ('SF_STATUS: ' || SF_STATUS);

      IF SF_STATUS = 'R'  THEN
         -- 2. If we are ready to create shape files then return the shape file arguments required for perl module
         SQL1 := 'SELECT QA_UNGEN_OUT_DIR, QA_OUT_DIR FROM gz_shapefile_setup  Where jobid = :1 and status = :2';

         EXECUTE IMMEDIATE SQL1 INTO pQA_UNGEN_OUT_DIR, pQA_OUT_DIR USING pJobID, 'R';

         IF (SUBSTR (pQA_UNGEN_OUT_DIR, 1, 1) = '/') THEN
            pQA_UNGEN_OUT_DIR := SUBSTR (pQA_UNGEN_OUT_DIR, 2);
         END IF;

         IF (SUBSTR (pQA_OUT_DIR, 1, 1) = '/') THEN
            pQA_OUT_DIR := SUBSTR (pQA_OUT_DIR, 2);
         END IF;

         SQL1 :='Select ''SF_STATUS=0;RELEASE='' || ''' || RELEASE || ''' ||'';GEN_PROJECT_ID='' || ''' || GEN_PROJECT_ID || ''' ||'';STEP='' || ''' || STEP || ''' ||'';DATABASE='' ||ORA_DATABASE_NAME || '';GEN_SCHEMA='' || USER
                         || '';QA_UNGEN_TABLE_PREFIX='' ||UNGEN_TOPO_NAME|| ''_'' || FSL_STRING
                         || '';QA_TABLE_PREFIX='' ||TOPO_NAME|| ''_'' || QA_STRING || ''_'' || FSL_STRING
                         || '';PRD_TABLE_PREFIX='' ||TOPO_NAME || ''_'' || FSL_STRING || '';QA=''||''' || IS_QA || '''
                         || '';YEAR='' || YEAR || '';GEN_LEVEL='' ||GEN_LEVEL||'';GENCODE='' || GENCODE || '';PROJECTION='' || PROJECTION
                         || '';QA_UNGEN_OUT_DIR='' || ''' || pQA_UNGEN_OUT_DIR || ''' || '';QA_OUT_DIR='' || ''' || pQA_OUT_DIR || '''
                         || '';STATE_TOPO_FLAG='' || STATE_TOPO_FLAG || '';DEL_CPG_FLAG='' || DEL_CPG_FLAG  || '';SHP_UNIT_REQUEST='' || SHP_UNIT_REQUEST
                          from gz_shapefile_setup  Where jobid = :1 and status = :2';

         --DBMS_OUTPUT.PUT_LINE(SQL1);
         EXECUTE IMMEDIATE SQL1 INTO SHAPEFILE_PRCS_STR USING pJobID, 'R';
      --DBMS_OUTPUT.PUT_LINE('SHAPEFILE_PRCS_STR: ' || SHAPEFILE_PRCS_STR);

      ELSIF (SF_STATUS = 'S') OR (SF_STATUS = 'M') THEN
          SHAPEFILE_PRCS_STR := 'SF_STATUS=3';
      ELSIF SF_STATUS IS NULL THEN
          SHAPEFILE_PRCS_STR := 'SF_STATUS=2';
      ELSE
         SHAPEFILE_PRCS_STR := 'SF_STATUS=1';
      END IF;

      RETURN SHAPEFILE_PRCS_STR;
   END GET_SHAPEFILE_PRCS_STR;

   PROCEDURE VERIFY_SETUP_TABLE (p_jobid IN VARCHAR2, p_correct IN VARCHAR2 DEFAULT 'Y')
   AS
      TYPE RefCursorType IS REF CURSOR;
      Cur1            RefCursorType;
      SQL1           VARCHAR2(2000);
      SQL2           VARCHAR2(2000);
      vTAB           VARCHAR2(32);
      vreleaseC    VARCHAR2(32);
      vprojIDC      VARCHAR2(32);
      vreleaseV    VARCHAR2(10);
      vprojIDV      VARCHAR2(4);
      vreleaseT    VARCHAR2(10);
      vprojIDT      VARCHAR2(4);
      vCount        NUMBER;
      vGood         NUMBER := 1;

   BEGIN
      SQL1 := 'SELECT table_name FROM USER_TABLES WHERE table_name LIKE ''GZ_%_SETUP''';

      OPEN Cur1 FOR SQL1;
      LOOP
         FETCH Cur1 INTO vTAB;
         EXIT WHEN Cur1%NOTFOUND;

         BEGIN
            SQL2 := 'SELECT column_name FROM user_tab_cols
                          WHERE table_name = :1 AND column_name = :2';

            EXECUTE IMMEDIATE SQL2 INTO vreleaseC
               USING vTAB, 'RELEASE';
         EXCEPTION
            WHEN OTHERS THEN
              vreleaseC := NULL;
         END;

         BEGIN
            SQL2 := 'SELECT column_name FROM user_tab_cols
                          WHERE table_name = :1 AND column_name LIKE :2';

            EXECUTE IMMEDIATE SQL2 INTO vprojIDC
               USING vTAB, '%gen_project_id';
         EXCEPTION
            WHEN OTHERS THEN
              vprojIDC := NULL;
         END;

         SQL2 := 'SELECT count(1) FROM '||vTAB
                 || '  WHERE jobid = :1';
         EXECUTE IMMEDIATE SQL2 INTO vCount  USING p_jobid;
         IF (vCount = 0) THEN
            vprojIDC := NULL;
            vreleaseC := NULL;
         END IF;

         BEGIN
            IF (vreleaseC IS NOT NULL) AND (vprojIDC IS NOT NULL) THEN
               SQL2 := 'SELECT '||vreleaseC||', '|| vprojIDC
                     ||'    FROM '||vTAB
                     ||'  WHERE jobid = :1';
               EXECUTE IMMEDIATE SQL2 INTO vreleaseV, vprojIDV USING p_jobid;
               vreleaseT := substr(p_jobid, 0, instr(p_jobid, '_')-1) ;
               vprojIDT := substr(p_jobid, instr(p_jobid, '_')+1,instr(p_jobid, '_', 1, 2)- instr(p_jobid, '_')-1);

               IF (vreleaseV <> vreleaseT) OR (vprojIDV <> vprojIDT) THEN
                  DBMS_OUTPUT.PUT_LINE('In '||vTAB ||', the release and/or gen_project_id does not match the jobid: ' || p_jobid);
                  IF (p_correct = 'Y')  THEN
                     SQL2 := 'UPDATE '||vTAB
                           ||'    SET '||vreleaseC||' = :1, '
                           ||vprojIDC ||' =:2 '
                           ||'  WHERE jobid = :3';
                     EXECUTE IMMEDIATE SQL2 USING vreleaseT, vprojIDT, p_jobid;
                     COMMIt;
                  END IF;
               END IF;
            ELSIF  (vreleaseC IS NOT NULL) THEN
               SQL2 := 'SELECT '||vreleaseC
                     ||'    FROM '||vTAB
                     ||'  WHERE jobid = :1';
               EXECUTE IMMEDIATE SQL2 INTO vreleaseV USING p_jobid;
               vreleaseT := substr(p_jobid, 0, instr(p_jobid, '_')-1) ;

               IF (vreleaseV <> vreleaseT) THEN
                  DBMS_OUTPUT.PUT_LINE('In '||vTAB ||', the release does not match the jobid: ' || p_jobid);
                  IF (p_correct = 'Y')  THEN
                     SQL2 := 'UPDATE '||vTAB
                           ||'    SET '||vreleaseC||' = :1 '
                           ||'  WHERE jobid = :2';
                     EXECUTE IMMEDIATE SQL2 USING vreleaseT, p_jobid;
                     COMMIt;
                  END IF;
               END IF;
            ELSIF  (vprojIDC IS NOT NULL) THEN
               SQL2 := 'SELECT '|| vprojIDC
                     ||'    FROM '||vTAB
                     ||'  WHERE jobid = :1';
               EXECUTE IMMEDIATE SQL2 INTO  vprojIDV USING p_jobid;
               vprojIDT := substr(p_jobid, instr(p_jobid, '_')+1,instr(p_jobid, '_', 1, 2)- instr(p_jobid, '_')-1);

               IF  (vprojIDV <> vprojIDT) THEN
                  DBMS_OUTPUT.PUT_LINE('In '||vTAB ||', the gen_project_id does not match the jobid: ' || p_jobid);
                  IF (p_correct = 'Y')  THEN
                     SQL2 := 'UPDATE '||vTAB
                           ||'    SET '|| vprojIDC ||' =:1 '
                           ||'  WHERE jobid = :2';
                     EXECUTE IMMEDIATE SQL2 USING vprojIDT, p_jobid;
                     COMMIt;
                  END IF;
               END IF;
            END IF;

         EXCEPTION
            WHEN OTHERS THEN
                vGood := 0;
               DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
               DBMS_OUTPUT.put_line (vTAB||': JobID='||p_jobid||', release='||vreleaseV||', projectID='||vprojIDV);
               DBMS_OUTPUT.put_line ( 'VERIFY_SETUP_TABLE Failed ' || SQLERRM);
         END;
      END LOOP;
      COMMIT;
      CLOSE Cur1;

      IF (vGood = 1) THEN
         DBMS_OUTPUT.put_line ( 'Release and ProjectID value match JobID in all setup tables!');
      END IF;
   END VERIFY_SETUP_TABLE;

   FUNCTION FIX_TOPO_FACE (
      pTopology        VARCHAR2,
      pRelease        VARCHAR2,
      pProjectID        VARCHAR2)
      RETURN NUMBER
   AS
      Topology    VARCHAR2 (30) := UPPER (pTopology);
      vArea_tolerance      NUMBER;
      vProjectID     VARCHAR2 (4)  :=  UPPER (pProjectID);
      vRelease     VARCHAR2 (10)  :=  UPPER (pRelease);

      face_ids    MDSYS.SDO_LIST_TYPE := MDSYS.SDO_LIST_TYPE ();
      face_id     NUMBER;
      face_id1     NUMBER;
      face_id2     NUMBER;
      length1     NUMBER;
      length2     NUMBER;

      sql_stmt    VARCHAR2 (4000);
      sql_stmt1    VARCHAR2 (4000);
      sql_stmt2    VARCHAR2 (4000);
      vTab         VARCHAR2 (32);

      vcols         VARCHAR2 (500);
      vcol         VARCHAR2 (32);
      vcol1         VARCHAR2 (32);
      vCount      NUMBER :=0;
      vCount1      NUMBER;
      my_cursor            SYS_REFCURSOR;
      vRetVal      NUMBER := 0;
      vTolerance  NUMBER := 0.05;

   BEGIN
      sql_stmt := 'SELECT field FROM reference_face_fields
                          WHERE UPPER(gen_project_id) = :1
                              AND UPPER(release) = :2
                              AND UPPER(field_use) = ''ATTRIBUTE''
                              AND UPPER(field) <> ''GEOID''';
      vcols := 'geoid';
      OPEN my_cursor FOR sql_stmt USING  vProjectID,vRelease;
      LOOP
         FETCH my_cursor INTO vcol;

         -- exit as soon as we have no data
         EXIT WHEN my_cursor%NOTFOUND;
         vCount := vCount + 1;
         vcols := vcols ||','||vcol;
      END LOOP;
      CLOSE my_cursor;
      my_cursor := NULL;

      IF (vCount > 0) THEN
         sql_stmt := ' SELECT sdo_tolerance '
            || ' FROM user_sdo_geom_metadata m, TABLE (m.diminfo) t '
            || ' WHERE m.table_name = :1 '
            || '     AND  m.column_name = :2'
            || '     AND ROWNUM = 1';
         EXECUTE IMMEDIATE sql_stmt INTO vTolerance
            USING Topology||'_EDGE$','GEOMETRY';


         sql_stmt := 'SELECT area_tolerance FROM small_polygon_parameters
                             WHERE UPPER(gen_project_id) = :1
                                 AND UPPER(release) = :2';
         EXECUTE IMMEDIATE sql_stmt INTO vArea_tolerance
               USING vProjectID, vRelease;

         sql_stmt := 'SELECT face_feature_mask_col FROM gen_clip_parameters
                             WHERE UPPER(gen_project_id) = :1
                                 AND UPPER(release) = :2';
         EXECUTE IMMEDIATE sql_stmt INTO vcol1
               USING vProjectID, vRelease;

         sql_stmt := 'SELECT face_id FROM '||Topology||'_face
                             WHERE '||vcol1||' IS NULL
                                 AND areatotal < :1';
         EXECUTE IMMEDIATE sql_stmt BULK COLLECT INTO face_ids
               USING vArea_tolerance;

         vTab := Topology||'_EDGE$';

         sql_stmt1 := 'SELECT right_face_id face_id, SDO_GEOM.SDO_LENGTH (a.geometry, :1) length FROM '
                        || vTab ||' a
                           WHERE SDO_GEOM.SDO_LENGTH (a.geometry, :2) =
                              (SELECT MAX (SDO_GEOM.SDO_LENGTH (b.geometry, :3))
                                  FROM '|| vTab ||'  b,
                                            TABLE (SDO_TOPO.get_face_boundary (:4, :5)) c
                                WHERE     b.edge_id = ABS (c.COLUMN_VALUE)
                                     AND a.edge_id = b.edge_id
                                     AND (left_face_id = :6 AND right_face_id > 0))
                               AND ROWNUM = 1';

         sql_stmt2 := 'SELECT left_face_id face_id, SDO_GEOM.SDO_LENGTH (a.geometry, :1) length FROM '
                          || vTab ||' a
                              WHERE SDO_GEOM.SDO_LENGTH (a.geometry, :2) =
                                 (SELECT MAX (SDO_GEOM.SDO_LENGTH (b.geometry, :3))
                                     FROM '|| vTab ||'  b,
                                               TABLE (SDO_TOPO.get_face_boundary (:3, :5)) c
                                   WHERE     b.edge_id = ABS (c.COLUMN_VALUE)
                                        AND a.edge_id = b.edge_id
                                        AND (right_face_id = :6 AND left_face_id > 0))
                                  AND ROWNUM = 1';

         FOR i IN 1 .. face_ids.COUNT
         LOOP

            face_id1 := face_ids(i);
            face_id2 := -1;
            face_id := -1;

            sql_stmt := 'SELECT COUNT(1) FROM ('||sql_stmt1||')';
            EXECUTE IMMEDIATE sql_stmt INTO vCount1
                   USING vTolerance, vTolerance, vTolerance,
                            Topology, face_ids(i), face_ids(i);
            IF (vCount1 > 0) THEN
               EXECUTE IMMEDIATE sql_stmt1 INTO face_id1, length1
                     USING vTolerance, vTolerance,vTolerance,
                               Topology, face_ids(i), face_ids(i);
            END IF;

            sql_stmt := 'SELECT COUNT(1) FROM ('||sql_stmt2||')';
            EXECUTE IMMEDIATE sql_stmt INTO vCount1
                   USING vTolerance, vTolerance, vTolerance,
                            Topology, face_ids(i), face_ids(i);
            IF (vCount1 > 0) THEN
               EXECUTE IMMEDIATE sql_stmt2 INTO face_id2, length2
                     USING vTolerance, vTolerance, vTolerance,
                               Topology, face_ids(i), face_ids(i);
            END IF;

            IF (face_id1 > 0) AND  (face_id2 > 0)  THEN
               IF (length1 >  length2) THEN
                   face_id :=  face_id1;
               ELSe
                  face_id :=  face_id2;
               END IF;
            ELSIF (face_id1 > 0)  THEN
               face_id :=  face_id1;
            ELSIF (face_id2 > 0)  THEN
               face_id :=  face_id2;
            END IF;

             IF (face_id >  0) THEN
               sql_stmt := 'UPDATE '||Topology||'_face  '
                        || 'SET ('||vcols||') = (SELECT '||vcols
                        || '                               FROM '||Topology||'_face  '
                        || '                               WHERE face_id = :1)
                             WHERE face_id = :2';
               EXECUTE IMMEDIATE sql_stmt  USING face_id, face_ids(i);
               COMMIT;
            END IF;
         END LOOP;

         sql_stmt := 'SELECT COUNT(1) FROM '||Topology||'_face
                             WHERE '||vcol1||' IS NULL
                                 AND areatotal < :1';
         EXECUTE IMMEDIATE sql_stmt INTO vRetVal
               USING vArea_tolerance;
      END IF;
      RETURN vRetVal;
   EXCEPTION
      WHEN OTHERS
      THEN
         DBMS_OUTPUT.put_line (SQLCODE || ' ' || SQLERRM);
         DBMS_OUTPUT.put_line (DBMS_UTILITY.format_error_backtrace);
   END FIX_TOPO_FACE;

END GZ_WORKFLOW;
/
