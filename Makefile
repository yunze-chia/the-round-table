include .env

IMAGE = $(REGISTRY)/the-round-table

push:
	docker build -t $(IMAGE) .
	docker push $(IMAGE)
