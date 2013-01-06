NAME := blaise
FILES := AUTHORS prettyprinter.ml eval.ml blaise.ml lexer.ml Makefile parser.ml\
	lex/parser.mly lex/lexer.mll
LOGIN := delaro_q
FOLDER := delaro_q-mpoc2012
TARBALL := delaro_q-mpoc2012.tar.bz2

all: ${FILES}
	ocamlbuild ${NAME}.native
	cp ${NAME}.native ${NAME}
	ocamlbuild -clean

tarball: ${FILES}
	@mkdir ${FOLDER}
	@for file in ${FILES}; do cp $$file ${FOLDER}; done
	@tar -cjf ${TARBALL} ${FOLDER}
	@rm -rf ${FOLDER}
	@echo "Tarball created"

clean:
	ocamlbuild -clean
	rm -rf ${FOLDER}
	rm ${NAME}
	rm ${TARBALL}

blaise: all
