#!/bin/sh
set -eu
image=kierdavis/site:latest
podman load $image < $(nix-build --no-out-link)
podman push $image
kubectl -n kier rollout restart deploy/website
