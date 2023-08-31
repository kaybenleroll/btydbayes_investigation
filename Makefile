### repo variables
PROJECT_USER=kaybenleroll
PROJECT_NAME=btydbayes-investigation
PROJECT_TAG=latest

IMAGE_TAG=${PROJECT_USER}/${PROJECT_NAME}:${PROJECT_TAG}

DOCKER_USER=rstudio
DOCKER_PASS=CHANGEME
DOCKER_UID=$(shell id -u)
DOCKER_GID=$(shell id -g)

RSTUDIO_PORT=8787

PROJECT_FOLDER=btydwork


### Set GITHUB_USER with 'gh config set gh_user <<user>>'
GITHUB_USER=$(shell gh config get gh_user)

CONTAINER_NAME=btyd-work

### Project build targets
.SUFFIXES: .qmd .html .dot .png

QMD_FILES  := $(wildcard *.qmd)
HTML_FILES := $(patsubst %.qmd,%.html,$(QMD_FILES))

all-html: $(HTML_FILES)

.qmd.html:
	echo "TIMESTAMP:" `date` "- Rendering script $<"  >> output.log 2>&1
	quarto render $< --to html >> output.log 2>&1
	echo "TIMESTAMP:" `date` "- Finished $*.html"         >> output.log 2>&1


.dot.png:
	dot -Tpng -o$*.png $<

full_deps.dot:
	makefile2graph all-html > full_deps.dot

depgraph: full_deps.png


exploring_shortsynth_data.html: generate_transaction_datasets.html
exploring_longsynth_data.html: generate_transaction_datasets.html
exploring_online_retail_transactions.html: retrieve_retail_data.html
exploring_cdnow_dataset.html: retrieve_retail_data.html

initial_pnbd_models.html: exploring_shortsynth_data.html
construct_longsynth_fixed_pnbd_models.html: exploring_longsynth_data.html
construct_onlineretail_fixed_pnbd_models.html: exploring_online_retail_transactions.html
construct_cdnow_fixed_pnbd_models.html: exploring_cdnow_dataset.html

construct_shortsynth_onehier_pnbd_models.html: initial_pnbd_models.html
construct_longsynth_onehier_pnbd_models.html: construct_longsynth_fixed_pnbd_models.html
construct_onlineretail_onehier_pnbd_models.html: construct_onlineretail_fixed_pnbd_models.html
construct_cdnow_onehier_pnbd_models.html: construct_cdnow_fixed_pnbd_models.html

construct_shortsynth_twohier_pnbd_models.html: construct_shortsynth_onehier_pnbd_models.html
construct_longsynth_twohier_pnbd_models.html: construct_longsynth_onehier_pnbd_models.html
construct_onlineretail_twohier_pnbd_models.html: construct_onlineretail_onehier_pnbd_models.html
construct_cdnow_twohier_pnbd_models.html: construct_cdnow_onehier_pnbd_models.html


mrproper: clean-cache clean-data clean-html clean-precompute clean-models
	rm -fv data/*.xlsx
	rm -fv *.dot
	rm -fv output.log

clean-data:
	rm -fv data/*.rds
	rm -fv data/*.csv

clean-html:
	rm -fv *.html

clean-cache:
	rm -rfv *_cache
	rm -rfv *_files

clean-precompute:
	rm -rfv precompute/*

clean-models:
	rm -fv stan_models/*


### Docker targets
docker-build-image: Dockerfile
	docker build -t ${IMAGE_TAG} \
	  --build-arg BUILD_DATE=$(shell date -u +'%Y-%m-%dT%H:%M:%SZ') \
	  -f Dockerfile .

docker-show-context:
	docker build -f build/context.dockerfile -t context-image .
	docker run --rm -it context-image find /tmp/build
	docker rmi test:latest

docker-run:
	docker run --rm -d \
	  -p ${RSTUDIO_PORT}:8787 \
	  -e USER=${DOCKER_USER} \
	  -e PASSWORD=${DOCKER_PASS} \
	  -e USERID=${DOCKER_UID} \
	  -e GROUPID=${DOCKER_GID} \
	  -v "${PWD}":"/home/${DOCKER_USER}/${PROJECT_FOLDER}":rw \
	  --name ${CONTAINER_NAME} \
	  ${IMAGE_TAG}

docker-fix-permissions:
	docker exec ${CONTAINER_NAME} bash -c "chown -R ${DOCKER_USER}:${DOCKER_USER} /home/${DOCKER_USER}"


docker-bash:
	docker exec -it -u ${DOCKER_USER} ${CONTAINER_NAME} bash

docker-stop:
	docker stop ${CONTAINER_NAME}

docker-rm:
	docker rm ${CONTAINER_NAME}

docker-start:
	docker start ${CONTAINER_NAME}

docker-clean: docker-stop-all
	docker rm $(shell docker ps -q -a)

docker-pull:
	docker pull ${IMAGE_TAG}
