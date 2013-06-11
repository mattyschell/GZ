#!/bin/ksh
#############################################################################
#
#      Script: run_gz.ksh
#      Author: Sreeni Karpurapu
#        Date: 08/26/2011
#
#############################################################################
#
# Description:
#
# Dependencies:
#
#############################################################################
#
#  Modification History
#     Ver  Date        Author          Comments
#     1.0  08/26/2011  Karpurapu       Initial Version
#

Version=1.0

echo "Current Version is ${Version}"

START_DTTM=`date +%Y-%b-%d_%H:%M:%S`


# define log directory
#GZ_LOG_DIR=/home/karpu001/log

GZ_BIN=$( cd -P -- "$(dirname -- "$(command -v -- "$0")")" && pwd -P )
echo "GZ_BIN: ${GZ_BIN}"

GZ_DATA=${GZ_BIN}/../data
echo "GZ_DATA: ${GZ_DATA}"

if [ $# -eq 7 ]
then
   # User Authentication mode
   GZ_USER_ID=$1
   GZ_PWD=$2
   GZ_INST=$3
   GZ_JOBID=$4
   GZ_DIR=$5
   FME_LOGFILE_DIR=$6
   FME_APP_DIR=$7
   AUTH_MODE=USER

   # Convert GZ_INST to uppercase
   GZ_INST=`echo ${GZ_INST} | awk '{print toupper($0)}'`
  
   # Convert GZ_JOBID to uppercase
   GZ_JOBID=`echo ${GZ_JOBID} | awk '{print toupper($0)}'`

   GZ_CONN_STR=${GZ_USER_ID}/${GZ_PWD}@${GZ_INST}

#   GZ_OUT_FILE=${GZ_LOG_DIR}/${4}_${START_DTTM}.out
#   GZ_ERR_FILE=${GZ_LOG_DIR}/${4}_${START_DTTM}.err

elif [ $# -eq 6 ]
then
   # User Authentication mode
   GZ_USER_ID=$1
   GZ_PWD=$2
   GZ_INST=$3
   GZ_JOBID=$4
   GZ_DIR=$5
   FME_LOGFILE_DIR=$6
   AUTH_MODE=USER

   # Convert GZ_INST to uppercase
   GZ_INST=`echo ${GZ_INST} | awk '{print toupper($0)}'`
  
   # Convert GZ_JOBID to uppercase
   GZ_JOBID=`echo ${GZ_JOBID} | awk '{print toupper($0)}'`

   GZ_CONN_STR=${GZ_USER_ID}/${GZ_PWD}@${GZ_INST}

elif [ $# -eq 5 ]
then
   # Wallet mode
   echo "Wallet Mode"
   GZ_WALLET=$1
   GZ_JOBID=$2
   GZ_DIR=$3
   FME_LOGFILE_DIR=$4
   FME_APP_DIR=$5
   
   AUTH_MODE=WALLET

   # Convert GZ_JOBID to uppercase
   GZ_JOBID=`echo ${GZ_JOBID} | awk '{print toupper($0)}'`

   GZ_CONN_STR="/@${GZ_WALLET}"

#   GZ_OUT_FILE=${GZ_LOG_DIR}/${2}_${START_DTTM}.out
#   GZ_ERR_FILE=${GZ_LOG_DIR}/${2}_${START_DTTM}.err

elif [ $# -eq 4 ]
then
   # Wallet mode
   echo "Wallet Mode"
   GZ_WALLET=$1
   GZ_JOBID=$2
   GZ_DIR=$3
   FME_LOGFILE_DIR=$4
   
   AUTH_MODE=WALLET

   # Convert GZ_JOBID to uppercase
   GZ_JOBID=`echo ${GZ_JOBID} | awk '{print toupper($0)}'`

   GZ_CONN_STR="/@${GZ_WALLET}"

#   GZ_OUT_FILE=${GZ_LOG_DIR}/${2}_${START_DTTM}.out
#   GZ_ERR_FILE=${GZ_LOG_DIR}/${2}_${START_DTTM}.err

else
   # Invalid 
   print "\nInvalid number of arguments --$#;\n"
   print "\nPlease use seven arguments for User Authentication Mode (userid, pwd, instance, jobid and p_directory, fme_logfile_dir and fme_app_dir)\n"
   print "or six arguments for User Authentication Mode (userid, pwd, instance, jobid and p_directory and fme_logfile_dir)\n"
   print "or five arguments for User Authentication Mode (wallet, jobid, p_directory, fme_logfile_dir and fme_app_dir)\n"
   print "or four arguments for User Authentication Mode (wallet, jobid, p_directory and fme_logfile_dir)\n"
   exit 1

fi

update_sf_status(){
   echo "***************************************************"
   echo "GZ_JOBID: $GZ_JOBID SHAPEFILE_STATUS: $GZ_SF_STATUS"
   echo "***************************************************"
   ######### Update GZ_SHAPEFILE_SETUP.STATUS and GZ_JOB_SETUP.SHAPEFILE_STATUS etc ##########
   sqlplus -s -l ${GZ_CONN_STR} <<endsql
      set serveroutput on size unlimited
      execute dbms_output.enable(buffer_size => NULL)
      whenever sqlerror exit 1
      set define off

      update gz_job_setup 
         set shapefile_status = '${GZ_SF_STATUS}', run_status = '${GZ_SF_STATUS}',
             end_time = TO_TIMESTAMP ('`date +"%Y%m%d%H%M%S"`', 'YYYYMMDDHH24MISS')
       where jobid = '${GZ_JOBID}';

      update gz_shapefile_setup
         set status = '${GZ_SF_STATUS}', end_time = TO_TIMESTAMP ('`date +"%Y%m%d%H%M%S"`', 'YYYYMMDDHH24MISS') 
       where jobid ='${GZ_JOBID}';

      Commit;

endsql


}


#replace_prj_file() {
#
#      echo "PRJ_ERR: ${PRJ_ERR}"
#      if [ ${PRJ_ERR} == 'N' ]
#      then
#         echo "Change directory to ${QA_SF_DIR}"
#         cd ${QA_SF_DIR}
#         for file in `find . -name '*.prj'`; do
#            ls -l ${file}
#            echo "rm -f ${file}"
#            echo "cp ${GZ_DATA}/${PRJ_FILE} ${file}"
#         done
#      fi
#
#}

   echo "GZ_USER_ID: $GZ_USER_ID"
   #echo "GZ_PWD: $GZ_PWD"
   echo "GZ_WALLET: $GZ_WALLET"
   echo "GZ_INST: $GZ_INST"
   echo "GZ_JOBID: $GZ_JOBID"
#   echo "GZ_OUT_FILE: $GZ_OUT_FILE"
#   echo "GZ_ERR_FILE: $GZ_ERR_FILE"

#if [ -e ${GZ_LOG_DIR} ]
#then
#   print "\nLog directory ${GZ_LOG_DIR} exists"
#   # Now see if you have permission to write to it
#   touch ${GZ_LOG_DIR}/testfile
#   if [ -f ${GZ_LOG_DIR}/testfile ]
#   then
#      echo "Successfully created a testfile in ${GZ_LOG_DIR}"
#      rm -f ${GZ_LOG_DIR}/testfile
#   else
#      echo "Unable to write to ${GZ_LOG_DIR}"
#      exit 1
#   fi
#
#else
#   print "\n Log directory ${GZ_LOG_DIR} does not exist"
#   print "Please create the directory and restart this shell script again"
#   exit 1
#
#fi

sqlplus -s -l ${GZ_CONN_STR} <<endsql
set serveroutput on size unlimited
execute dbms_output.enable(buffer_size => NULL)
whenever sqlerror exit 1
set define off

Select user from dual;

DECLARE
  JOBID VARCHAR2(20) := '${GZ_JOBID}';

BEGIN

   -- Add code to check if GZ_JOBID is valid
   dbms_output.put_line('JOBID: ' || JOBID);
   
   --exec GZ_WORKFLOW.GENERALIZE('ACS09_Z6_ST99_1')
   GZ_WORKFLOW.GENERALIZE(JOBID);

    
--EXCEPTION


END;
/

endsql

retVal=$?

if [ ${retVal} -ne 0 ]
then
   echo "Problem with sqlplus session.  Exit with status ${retVal}"
   exit ${retVal}
fi


# The following is for shapefile processing


SHAPEFILE_STRING=`sqlplus -s -l ${GZ_CONN_STR} <<endsql
set echo off pages 0 head off
set lines 1000
Select NVL(GZ_WORKFLOW.GET_SHAPEFILE_PRCS_STR('${GZ_JOBID}'),'SF_STATUS=1') from dual;
endsql`

echo "SHAPEFILE_STRING: $SHAPEFILE_STRING"


IFS=";"
set -A SF_PRM_ARR ${SHAPEFILE_STRING}
unset IFS

for tmpStr in ${SF_PRM_ARR[*]};
do
  echo $tmpStr
  export `echo $tmpStr`
done

export GZ_DIR=${GZ_DIR%/}/

export QA_UNGEN_OUT_DIR=${GZ_DIR}${QA_UNGEN_OUT_DIR}
export QA_OUT_DIR=${GZ_DIR}${QA_OUT_DIR}

export PRD_OUT_DIR=${GZ_DIR%/}
cd ${GZ_BIN}/../data
export MAPPING_FILE_DIR=`pwd`/

echo QA_UNGEN_OUT_DIR=${QA_UNGEN_OUT_DIR}
echo QA_OUT_DIR=${QA_OUT_DIR}
echo PRD_OUT_DIR=${PRD_OUT_DIR}
echo MAPPING_FILE_DIR=${MAPPING_FILE_DIR}
echo SHP_UNIT_REQUEST=${SHP_UNIT_REQUEST}
echo RELEASE=${RELEASE}
echo GEN_PROJECT_ID=${GEN_PROJECT_ID}

# SF_STATUS will be set to 0 if we have to create shape files.  Otherwise it will be set to 1
if [ ${SF_STATUS} == 1 ]
then

     echo "Set GZ_SF_STATUS to F - GZ_WORKFLOW.GET_SHAPEFILE_PRCS_STR failed to run properly"
     GZ_SF_STATUS=F
     update_sf_status
     exit 1     
fi

if [ ${SF_STATUS} == 0 ]
then

   print "\n\nStart Metadata Module\n"

   if [ ${AUTH_MODE} == 'WALLET' ]
   then
      # If AUTH_MODE is WALLET, make GZ_PWD=$GZ_CONN_STR
      GZ_PWD=${GZ_CONN_STR}
      echo "Using Wallet Mode to create shapefiles ${GZ_PWD}"       
      cd ${GZ_BIN}
      perl gz_shp_metadata.pl -j ${GZ_JOBID} -s ${GEN_SCHEMA} -p ${GZ_PWD} -l ${FME_LOGFILE_DIR}
   fi
   
   if [ ${AUTH_MODE} == 'USER' ]
   then
   	  cd ${GZ_BIN}   
      perl gz_shp_metadata.pl -j ${GZ_JOBID} -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -l ${FME_LOGFILE_DIR}
   fi
	 
   print "\n\nEnd Metadata Module\n"
   	 
   if [ ${QA} == 'Y' ]
   then

      print "\n\nCreate QA shapefiles now\n"
      # Change directory to bin.  This will enable relative usage for data directory
      cd ${GZ_BIN}


      if [ $# -eq 7 ]
      then
         #echo "perl gz_shp_creator_2010.pl -a ${FME_APP_DIR} -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${QA_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR}"
         perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -a ${FME_APP_DIR} -i {$GZ_JOBID} -h 'QA' -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${QA_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}
      elif [ $# -eq 5 ]
      then
         #echo "perl gz_shp_creator_2010.pl -a ${FME_APP_DIR} -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${QA_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR}"
         perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -a ${FME_APP_DIR} -i {$GZ_JOBID} -h 'QA' -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${QA_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}
      elif [ $# -eq 6 ]
      then
         #echo "perl gz_shp_creator_2010.pl -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${QA_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR}"
         perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -i {$GZ_JOBID} -h 'QA' -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${QA_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}
      else
         #echo "perl gz_shp_creator_2010.pl -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${QA_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR}"
         perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -i {$GZ_JOBID} -h 'QA' -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${QA_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}

      fi

      retVal1=$?

      #Let's run the replace prj files as an adhoc/on-request job
      #if [ ${retVal1} -eq 0 ]
      #then 
      #   QA_SF_DIR=${QA_OUT_DIR}
      #   replace_prj_file
      #fi
 

      print "\n\nCreate UNGEN QA shapefiles now\n"
      # Change directory to bin.  This will enable relative usage for data directory
      cd ${GZ_BIN}
      UNGEN_LEVEL=z1
      UNGEN_CODE=d

      if [ $# -eq 7 ] 
      then
         #echo "perl gz_shp_creator_2010.pl -a ${FME_APP_DIR} -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_UNGEN_TABLE_PREFIX} -y ${YEAR} -n ${UNGEN_LEVEL} -c ${UNGEN_CODE} -j ${PROJECTION} -o ${QA_UNGEN_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR}"
         perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -a ${FME_APP_DIR} -i {$GZ_JOBID} -h 'UNGEN' -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_UNGEN_TABLE_PREFIX} -y ${YEAR} -n ${UNGEN_LEVEL} -c ${UNGEN_CODE} -j ${PROJECTION} -o ${QA_UNGEN_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}
      elif [ $# -eq 5 ]
      then
         #echo "perl gz_shp_creator_2010.pl -a ${FME_APP_DIR} -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_UNGEN_TABLE_PREFIX} -y ${YEAR} -n ${UNGEN_LEVEL} -c ${UNGEN_CODE} -j ${PROJECTION} -o ${QA_UNGEN_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR}"
         perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -a ${FME_APP_DIR} -i {$GZ_JOBID} -h 'UNGEN' -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_UNGEN_TABLE_PREFIX} -y ${YEAR} -n ${UNGEN_LEVEL} -c ${UNGEN_CODE} -j ${PROJECTION} -o ${QA_UNGEN_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}
      elif [ $# -eq 6 ]
      then
         #echo "perl gz_shp_creator_2010.pl -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_UNGEN_TABLE_PREFIX} -y ${YEAR} -n ${UNGEN_LEVEL} -c ${UNGEN_CODE} -j ${PROJECTION} -o ${QA_UNGEN_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR}"
         perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -i {$GZ_JOBID} -h 'UNGEN' -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_UNGEN_TABLE_PREFIX} -y ${YEAR} -n ${UNGEN_LEVEL} -c ${UNGEN_CODE} -j ${PROJECTION} -o ${QA_UNGEN_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}

      else
         #echo "perl gz_shp_creator_2010.pl -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_UNGEN_TABLE_PREFIX} -y ${YEAR} -n ${UNGEN_LEVEL} -c ${UNGEN_CODE} -j ${PROJECTION} -o ${QA_UNGEN_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR}"
         perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -i {$GZ_JOBID} -h 'UNGEN' -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${QA_UNGEN_TABLE_PREFIX} -y ${YEAR} -n ${UNGEN_LEVEL} -c ${UNGEN_CODE} -j ${PROJECTION} -o ${QA_UNGEN_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}

      fi


      retVal2=$?

      #Let's run the replace prj files as an adhoc/on-request job
      #if [ ${retVal2} -eq 0 ]
      #then 
      #   QA_SF_DIR=${QA_UNGEN_OUT_DIR}
      #   replace_prj_file
      #fi


   else
      print "\n\nQA flag is set to N.  Skipping QA shapefile creation\n"
      retVal1=0
      retVal2=0

   fi

   print "\n\nCreate production shapefiles now\n\n"
   # Change directory to bin.  This will enable relative usage for data directory
   cd ${GZ_BIN}

   if [ $# -eq 7 ]
   then
      #echo "perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -a ${FME_APP_DIR} -i ${GZ_JOBID} -h 'PRD' -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${PRD_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${PRD_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}"
      perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -a ${FME_APP_DIR} -i ${GZ_JOBID} -h 'PRD' -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${PRD_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${PRD_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}
   elif [ $# -eq 5 ]
   then
      #echo "perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -a ${FME_APP_DIR} -i {${GZ_JOBID} -h 'PRD' -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${PRD_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${PRD_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}"
      perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -a ${FME_APP_DIR} -i ${GZ_JOBID} -h 'PRD' -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${PRD_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${PRD_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}
   elif [ $# -eq 6 ]
   then
      #echo "perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -i ${GZ_JOBID} -h 'PRD' -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${PRD_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${PRD_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}"
      perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -i ${GZ_JOBID} -h 'PRD' -d ${GZ_INST} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${PRD_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${PRD_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}
   else
      #echo "perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -i ${GZ_JOBID} -h 'PRD' -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${PRD_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${PRD_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}"
      perl gz_shp_creator_2010.pl -z ${RELEASE} -x ${GEN_PROJECT_ID} -i ${GZ_JOBID} -h 'PRD' -d ${DATABASE} -s ${GEN_SCHEMA} -p ${GZ_PWD} -t ${PRD_TABLE_PREFIX} -y ${YEAR} -n ${GEN_LEVEL} -c ${GENCODE} -j ${PROJECTION} -o ${PRD_OUT_DIR} -f ${STATE_TOPO_FLAG} -e ${DEL_CPG_FLAG} -m ${MAPPING_FILE_DIR} -l ${FME_LOGFILE_DIR} -r ${SHP_UNIT_REQUEST}

   fi


   retVal3=$?

   echo "*** retVal1: ${retVal1} retVal2: ${retVal2} retVal3: ${retVal3} ***"

   if [ ${retVal1} -eq 0 ] && [ ${retVal2} -eq 0 ] && [ ${retVal3} -eq 0 ]
   then
     echo "Set GZ_SF_STATUS to S"
     GZ_SF_STATUS=S
     update_sf_status
   else
     echo "Set GZ_SF_STATUS to F"
     GZ_SF_STATUS=F
     update_sf_status
     exit 1

   fi

elif [ ${SF_STATUS} == 2 ]
then
   echo "Skipping shapefile creation"
elif [ ${SF_STATUS} == 3 ]
then
     echo "Shapefiles have already been created. Skipping shapefile creation"
     GZ_SF_STATUS=S
     update_sf_status
     
else
   echo "Cannot set the env variables"
   GZ_SF_STATUS=F
   update_sf_status
   exit 1
   
fi

print "\n\n\n*****  Successfully completed GZ_WORKFLOW.GENERALIZE($GZ_JOBID) *****\n\n\n"
#


