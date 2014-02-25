/* CREATE GZ PARAMETER TABLES */
set serveroutput on;
spool "/mtdata004/mapping/gz/dev/devbnch/schel010/generalization/src/gzcpb1_install_tables_log.txt";
set termout off;
set feedback on;
BEGIN
   GZ_BUSINESS_UTILS.BACKUP_GZ_TABLES();
END;
/
BEGIN
   GZ_BUSINESS_UTILS.CREATE_GZ_TABLES('Y','Y');
END;
/
BEGIN
   GZ_BUSINESS_UTILS.COPY_GZ_TABLES('schel010');
END;
/
show errors;
exit;
