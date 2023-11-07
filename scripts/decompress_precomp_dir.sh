#!/bin/sh

ARCHIVE_DIR=btyd_precompute


for PRECOMP_DIR in `ls ${ARCHIVE_DIR}/`
do

  tar xvfp ${ARCHIVE_DIR}/${PRECOMP_DIR}
#  COMPRESS_FILE=precompute_${PRECOMP_DIR}.tar.bz2

#  tar cvfp ${ARCHIVE_DIR}/${COMPRESS_FILE} -I /usr/bin/pbzip2 precompute/${PRECOMP_DIR}/
done