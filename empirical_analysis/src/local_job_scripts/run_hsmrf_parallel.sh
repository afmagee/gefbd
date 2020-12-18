#!/bin/bash

TMP_OUTPUT_DIR="output_tmp"

rm -rf ${TMP_OUTPUT_DIR}
mkdir ${TMP_OUTPUT_DIR}

parallel -j8 --eta < src/hsmrf_commands.txt

#rm -rf ${TMP_OUTPUT_DIR}
rm history.txt
