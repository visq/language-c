TESTS=analysis_enum \
      analysis_ext_decls \
      analysis_local_decls \
      analysis_type_check attributes \
      builtins \
      bug5_dos_newline bugn6_empty_file bug21_sem_typedef \
      bug22_file_permission_cpp bug30_preserve_int_repr bug31_pp_if_else \
      bug20130805_nopos \
      bug20160302_int128 \
      bug20160314_noreturn \
      bug20160729_C_include_stack \
      bug20160911_builtin_bswap \
      iec_60559 \
      parse_dg

#bug20140111_utf8
all: tests
tests:  $(TESTS:=.runtest)
prepare: make_examples make_test
make_examples: build_lib
	make -C ../../examples
build_lib:
	cd ../.. && cabal build
make_test: build_lib
	make -C ../
clean: $(TESTS:=.runclean)
	make -C ../../examples clean
%.runtest: | prepare
	make -C $* clean all
%.runclean:
	make -C $* clean

