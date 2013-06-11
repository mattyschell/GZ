/* SQL INSTALL SCRIPT FOR GENERALIZATION*/
set serveroutput ON size 1000000 format word_wrapped;
set feedback on;
set pagesize 0;
set define off;
set scan off;
timing start;
spool "/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/schel010_install_log.txt";
begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_BUILD_SOURCE.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_BUILD_SOURCE.pks";
/
show errors;
GRANT EXECUTE ON GZ_BUILD_SOURCE TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_CLIP.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_CLIP.pks";
/
show errors;
GRANT EXECUTE ON GZ_CLIP TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_FSL.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_FSL.pks";
/
show errors;
GRANT EXECUTE ON GZ_FSL TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_FSL_NAME_LSAD_UPDATE.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_FSL_NAME_LSAD_UPDATE.pks";
/
show errors;
GRANT EXECUTE ON GZ_FSL_NAME_LSAD_UPDATE TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_GEODESIC.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_GEODESIC.pks";
/
show errors;
GRANT EXECUTE ON GZ_GEODESIC TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_LINESIM.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_LINESIM.pks";
/
show errors;
GRANT EXECUTE ON GZ_LINESIM TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_MATH.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_MATH.pks";
/
show errors;
GRANT EXECUTE ON GZ_MATH TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_METADATA.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_METADATA.pks";
/
show errors;
GRANT EXECUTE ON GZ_METADATA TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_OUTPUT.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_OUTPUT.pks";
/
show errors;
GRANT EXECUTE ON GZ_OUTPUT TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_PROJECTION.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_PROJECTION.pks";
/
show errors;
GRANT EXECUTE ON GZ_PROJECTION TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_QA.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_QA.pks";
/
show errors;
GRANT EXECUTE ON GZ_QA TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_SMPOLY.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_SMPOLY.pks";
/
show errors;
GRANT EXECUTE ON GZ_SMPOLY TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_SUPER.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_SUPER.pks";
/
show errors;
GRANT EXECUTE ON GZ_SUPER TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_BUILD.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_BUILD.pks";
/
show errors;
GRANT EXECUTE ON GZ_TOPO_BUILD TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_EDIT.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_EDIT.pks";
/
show errors;
GRANT EXECUTE ON GZ_TOPO_EDIT TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_HELPER.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_HELPER.pks";
/
show errors;
GRANT EXECUTE ON GZ_TOPO_HELPER TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_MERGE.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_MERGE.pks";
/
show errors;
GRANT EXECUTE ON GZ_TOPO_MERGE TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_UTIL.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_UTIL.pks";
/
show errors;
GRANT EXECUTE ON GZ_TOPO_UTIL TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPOFIX.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPOFIX.pks";
/
show errors;
GRANT EXECUTE ON GZ_TOPOFIX TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TYPES.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TYPES.pks";
/
show errors;
GRANT EXECUTE ON GZ_TYPES TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_UTIL_ZONE.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_UTIL_ZONE.pks";
/
show errors;
GRANT EXECUTE ON GZ_UTIL_ZONE TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_UTILITIES.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_UTILITIES.pks";
/
show errors;
GRANT EXECUTE ON GZ_UTILITIES TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_WORKFLOW.pks...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_WORKFLOW.pks";
/
show errors;
GRANT EXECUTE ON GZ_WORKFLOW TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_BUILD_SOURCE.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_BUILD_SOURCE.pkb";
/
show errors;
GRANT EXECUTE ON GZ_BUILD_SOURCE TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_CLIP.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_CLIP.pkb";
/
show errors;
GRANT EXECUTE ON GZ_CLIP TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_FSL.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_FSL.pkb";
/
show errors;
GRANT EXECUTE ON GZ_FSL TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_FSL_NAME_LSAD_UPDATE.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_FSL_NAME_LSAD_UPDATE.pkb";
/
show errors;
GRANT EXECUTE ON GZ_FSL_NAME_LSAD_UPDATE TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_GEODESIC.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_GEODESIC.pkb";
/
show errors;
GRANT EXECUTE ON GZ_GEODESIC TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_LINESIM.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_LINESIM.pkb";
/
show errors;
GRANT EXECUTE ON GZ_LINESIM TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_MATH.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_MATH.pkb";
/
show errors;
GRANT EXECUTE ON GZ_MATH TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_METADATA.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_METADATA.pkb";
/
show errors;
GRANT EXECUTE ON GZ_METADATA TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_OUTPUT.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_OUTPUT.pkb";
/
show errors;
GRANT EXECUTE ON GZ_OUTPUT TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_PROJECTION.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_PROJECTION.pkb";
/
show errors;
GRANT EXECUTE ON GZ_PROJECTION TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_QA.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_QA.pkb";
/
show errors;
GRANT EXECUTE ON GZ_QA TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_SMPOLY.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_SMPOLY.pkb";
/
show errors;
GRANT EXECUTE ON GZ_SMPOLY TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_SUPER.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_SUPER.pkb";
/
show errors;
GRANT EXECUTE ON GZ_SUPER TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_BUILD.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_BUILD.pkb";
/
show errors;
GRANT EXECUTE ON GZ_TOPO_BUILD TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_EDIT.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_EDIT.pkb";
/
show errors;
GRANT EXECUTE ON GZ_TOPO_EDIT TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_HELPER.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_HELPER.pkb";
/
show errors;
GRANT EXECUTE ON GZ_TOPO_HELPER TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_MERGE.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_MERGE.pkb";
/
show errors;
GRANT EXECUTE ON GZ_TOPO_MERGE TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_UTIL.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPO_UTIL.pkb";
/
show errors;
GRANT EXECUTE ON GZ_TOPO_UTIL TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPOFIX.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TOPOFIX.pkb";
/
show errors;
GRANT EXECUTE ON GZ_TOPOFIX TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TYPES.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_TYPES.pkb";
/
show errors;
GRANT EXECUTE ON GZ_TYPES TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_UTIL_ZONE.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_UTIL_ZONE.pkb";
/
show errors;
GRANT EXECUTE ON GZ_UTIL_ZONE TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_UTILITIES.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_UTILITIES.pkb";
/
show errors;
GRANT EXECUTE ON GZ_UTILITIES TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_WORKFLOW.pkb...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_WORKFLOW.pkb";
/
show errors;
GRANT EXECUTE ON GZ_WORKFLOW TO "PUBLIC";

timing show;

begin 
  DBMS_OUTPUT.PUT_LINE('Installing /mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_PRIV_GRANTER.prc...');
end;
/
@"/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/GZ_PRIV_GRANTER.prc";
/
show errors;
GRANT EXECUTE ON GZ_PRIV_GRANTER TO "PUBLIC";

timing show;

commit;
timing stop;
spool off;
exit;
