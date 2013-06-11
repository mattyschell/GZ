#!/bin/ksh

GZ_BIN=$( cd -P -- "$(dirname -- "$(command -v -- "$0")")" && pwd -P )
echo "GZ_BIN: ${GZ_BIN}"

if [ $# -eq 6 ]
then
   # User Authentication mode
   GZ_USER_ID=$1
   GZ_PWD=$2
   GZ_INST=$3
   GZ_SRC=$4
   GZ_SRC_TOPO=$5
   GZ_TGT_TOPO=$6
   AUTH_MODE=USER

   # Convert GZ_INST to uppercase
   GZ_INST=`echo ${GZ_INST} | awk '{print toupper($0)}'`
  
   GZ_CONN_STR=${GZ_USER_ID}/${GZ_PWD}@${GZ_INST}

elif [ $# -eq 4 ]
then
   # Wallet mode
   echo "Wallet Mode"
   GZ_WALLET=$1
   GZ_SRC=$2
   GZ_SRC_TOPO=$3
   GZ_TGT_TOPO=$4
   AUTH_MODE=WALLET

   GZ_CONN_STR="/@${GZ_WALLET}"

else
   # Invalid 
   print "\nInvalid number of arguments --$#;\n"
   print "\nPlease use six arguments for User Authentication Mode (userid, pwd, instance, src_schema, src_topo, tgt_topo)\n"
   print "or four arguments for Wallet Authentication Mode (wallet, src_schema, src_topo, tgt_topo)\n"
   exit 1

fi

sqlplus -s -l ${GZ_CONN_STR} <<endsql
exec gz_topo_util.copy_topology('${GZ_SRC}', '${GZ_SRC_TOPO}', USER, '${GZ_TGT_TOPO}', 'Y', 'Y', 'Y');
exit;
endsql

retVal=$?

if [ ${retVal} -ne 0 ]
then

   echo "Problem with sqlplus session.  Exit with status ${retVal}"
   exit ${retVal}
else
   echo "Successfully copy topology.  Exit with status ${retVal}"
fi