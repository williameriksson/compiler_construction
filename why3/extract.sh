#rm -r ../tmp/*
why3 -L . extract -D ocaml64 imp_ex.mlw -o ../tmp/
why3 -L . extract -D ocaml64 vm_ex.mlw -o ../tmp/
why3 -L . extract -D ocaml64 compiler.mlw -o ../tmp/
why3 -L . extract -D ocaml64 listmap.mlw -o ../tmp/
why3 -L . extract -D ocaml64 ast_opt.mlw -o ../tmp/
#rm -r ../cimp/ocaml/extract/*
cp ../tmp/* ../cimp/ocaml/extract/
