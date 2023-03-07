#! /bin/bash

docker-compose -f deploy/dockerephemeral/docker-compose.yaml stop
pkill -9 gundeck
pkill -9 galley
pkill -9 brig
pkill -9 cargohold
pkill -9 spar
pkill -9 cannon
pkill -9 federator
pkill -9 python3
pkill -9 nginz
pkill -9 nginx


( ./deploy/dockerephemeral/run.sh 2>&1 | grep -v demo ) &
sleep 30s
make db-migrate
( ./services/start-services-only.sh 2>&1 | grep -v demo ) &
sleep 5s
for X in $(seq 1 5)
do
  docker-compose -f deploy/dockerephemeral/docker-compose.yaml stop redis redis-node-{1,2,3,4,5,6} redis-cluster
  sleep 5s
  cd services/gundeck
  for F in $(seq 1 100)
  do
    timeout 0.1 ../../dist/gundeck-integration -s gundeck.integration.yaml -i ../integration.yaml
  done
  cd ../..
  sleep 30s
  docker-compose -f deploy/dockerephemeral/docker-compose.yaml start redis redis-node-{1,2,3,4,5,6} redis-cluster
  sleep 15s
done
pkill gundeck
pkill galley
pkill brig
pkill cargohold
pkill spar
pkill cannon
pkill federator
pkill python3
pkill nginz
pkill nginx
docker-compose -f deploy/dockerephemeral/docker-compose.yaml stop

cd services/gundeck
sleep 5s
eventlog2html --colour-scheme tableau10 -x robust gundeck.eventlog

