#!/bin/bash

set -e

: ${1?"Usage:  babi.sh /path/to/babi/tasks/en-valid [qaNN_valid.txt]"}

VALID_DIR=$(readlink -f $1)
SCRIPT=$(readlink -f $0)
SCRIPT_PATH=`dirname ${SCRIPT}`
TESTCASE=${2:-qa1_valid.txt}

PASSING="qa1 qa2 qa3 qa5 qa6 qa7 qa8 qa9 qa11 qa12 qa13 qa14 qa16"

if [ ! -f "${VALID_DIR}/qa1_valid.txt" ]
then
    echo "Invalid path:  ${VALID_DIR} does not contain qa1_valid.txt"
    exit -1
fi

pushd ${SCRIPT_PATH}/../../..

run_one() {
    echo
    echo "Starting test:  ${TESTCASE}"
    echo
    sbt "runMain com.lingeringsocket.shlurd.platonic.SpcInterpretTester \
       ${SCRIPT_PATH}/../resources/expect/babi-qa-beliefs.txt " \
        < "${VALID_DIR}/${TESTCASE}"
}

if [ ! -z $2 ]
then
    TESTCASE=$2
    run_one
else
    for TESTNAME in ${PASSING}; do
        TESTCASE="${TESTNAME}_valid.txt"
        run_one
    done
fi

popd
