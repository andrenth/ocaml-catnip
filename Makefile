default:
	jbuilder build @install

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	rm -rf _build

examples:
	jbuilder build examples/free_api.exe

.PHONY: default install uninstall reinstall clean examples
