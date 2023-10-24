#!/bin/sh

ARCHIVE_DIR=btyd_precompute

mkdir ${ARCHIVE_DIR}

for PRECOMP_DIR in `ls precompute`
do

  COMPRESS_FILE=precompute_${PRECOMP_DIR}.tar.bz2

  tar cvfp ${ARCHIVE_DIR}/${COMPRESS_FILE} -I /usr/bin/pbzip2 precompute/${PRECOMP_DIR}/
done