
build: messaging_api.capnp
	capnp compile -o ocaml -I /home/cjj39/go/src/zombiezen.com/go/capnproto2/std messaging_api.capnp
	capnp compile -I /home/cjj39/go/src/zombiezen.com/go/capnproto2/std -ogo messaging_api.capnp

